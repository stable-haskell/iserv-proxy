#!/usr/bin/env python3
# Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
# SPDX-License-Identifier: BSD-3-Clause
"""Test piped interpreter ownership without GHC or an emulator (POSIX only).

The controller sends ResolveObjs and Shutdown directly. Each case has separate
startup and cleanup deadlines, and kills its owned processes even on failure.
Use --child-python to run a native interpreter when the proxy runs under Rosetta.
"""

import argparse
import contextlib
import json
import os
from pathlib import Path
import select
import signal
import struct
import subprocess
import sys
import tempfile
import time


STARTUP_TIMEOUT = 30
CLEANUP_TIMEOUT = 12
CASES = (
    "normal", "slow_shutdown", "exit_idle", "success_idle",
    "exit_after_request", "signal_after_request", "exit_with_descendant",
    "close_stdout_ignore_term", "ghc_disconnect", "interrupt", "terminate",
    "terminate_during_shutdown", "interrupt_during_shutdown",
    "late_terminate", "late_interrupt", "shutdown_failure", "fd_isolation",
    "large_write", "blocked_write",
)
SUCCESS_CASES = {"normal", "slow_shutdown", "fd_isolation", "large_write"}
NO_RESPONSE_CASES = {
    "exit_after_request", "signal_after_request", "exit_with_descendant",
    "close_stdout_ignore_term",
}


def wait_for_file(path, process):
    deadline = time.monotonic() + STARTUP_TIMEOUT
    while not path.exists():
        assert process.poll() is None, f"Proxy exited before {path.name}"
        assert time.monotonic() < deadline, f"Timed out waiting for {path.name}"
        time.sleep(0.01)


def owned_processes(directory):
    path = directory / "processes"
    if not path.exists():
        return []
    return [(int(pid), int(group), role) for pid, group, role in
            (line.split() for line in path.read_text().splitlines())]


def process_exists(pid, include_zombies=False):
    try:
        os.kill(pid, 0)
        status = Path(f"/proc/{pid}/status")
        if not include_zombies and status.exists():
            return "\nState:\tZ" not in status.read_text()
        return True
    except (ProcessLookupError, FileNotFoundError):
        return False


def assert_processes_cleaned(directory):
    # The proxy must reap its direct child, not just kill it. Linux PID 1 may
    # retain an orphaned descendant as a zombie; that process is no longer live.
    deadline = time.monotonic() + 1
    while True:
        survivors = [pid for pid, _, role in owned_processes(directory)
                     if process_exists(pid, include_zombies=(role == "interpreter"))]
        if not survivors:
            return
        assert time.monotonic() < deadline, (
            f"Interpreter processes survived or were not reaped: {survivors}"
        )
        time.sleep(0.01)


def cleanup_fixture(process, directory):
    # Kill recorded children first, so even a broken proxy can reap its child.
    for pid, group, _ in owned_processes(directory):
        if not process_exists(pid):
            continue
        with contextlib.suppress(ProcessLookupError):
            if group != process.pid and group != os.getpgrp():
                os.killpg(group, signal.SIGKILL)
            else:
                os.kill(pid, signal.SIGKILL)
    try:
        process.wait(timeout=2)
    except subprocess.TimeoutExpired:
        with contextlib.suppress(ProcessLookupError):
            os.killpg(process.pid, signal.SIGKILL)
        process.wait(timeout=3)


def drive_protocol(process, directory, read_local, write_local, scenario):
    wait_for_file(directory / "ready", process)
    if scenario in {"exit_idle", "success_idle"}:
        return

    if scenario in {"large_write", "blocked_write"}:
        # LoadObj encodes a Binary String; Done and its unit result need no
        # GHC response. ResolveObjs below confirms that forwarding finished.
        payload = b"a" * (1024 * 1024)
        request = memoryview(b"\x06" + struct.pack(">q", len(payload)) + payload)
        while request:
            request = request[os.write(write_local, request):]
        if scenario == "blocked_write":
            wait_for_file(directory / "write-blocked", process)
            time.sleep(0.2)
            os.kill(process.pid, signal.SIGTERM)
            return

    os.write(write_local, b"\x0a")
    if scenario in NO_RESPONSE_CASES:
        return
    ready, _, _ = select.select([read_local], [], [], STARTUP_TIMEOUT)
    assert ready, "ResolveObjs response timed out"
    assert os.read(read_local, 1) == b"\x01", "Incorrect ResolveObjs response"

    if scenario == "ghc_disconnect":
        os.close(write_local)
        return
    if scenario in {"interrupt", "terminate"}:
        os.kill(process.pid, signal.SIGINT if scenario == "interrupt" else signal.SIGTERM)
        return

    os.write(write_local, b"\x00")
    if scenario in {
        "terminate_during_shutdown", "interrupt_during_shutdown",
        "late_terminate", "late_interrupt",
    }:
        wait_for_file(directory / "shutdown-started", process)
        # Let the proxy enter its graceful wait. The late cases then exit zero,
        # proving cancellation remains a failure even if the child succeeds.
        time.sleep(0.2)
        cancellation = signal.SIGINT if scenario in {
            "interrupt_during_shutdown", "late_interrupt",
        } else signal.SIGTERM
        os.kill(process.pid, cancellation)


def assert_result(process, directory, scenario, stderr, expected_fds):
    assert_processes_cleaned(directory)
    assert (process.returncode == 0) == (scenario in SUCCESS_CASES), (
        f"Unexpected proxy exit {process.returncode}"
    )
    expected_status = {
        "exit_idle": "139", "exit_after_request": "139",
        "exit_with_descendant": "139", "signal_after_request": "-11",
        "success_idle": "ExitSuccess", "shutdown_failure": "23",
    }.get(scenario)
    if expected_status is not None:
        assert expected_status in stderr, f"Lost interpreter status {expected_status}"
    if scenario == "slow_shutdown":
        assert (directory / "shutdown-completed").exists(), (
            "Graceful Shutdown was terminated before completion"
        )
    if scenario == "fd_isolation":
        observed = json.loads((directory / "descriptors.json").read_text())
        assert all(observed[fd] != identity for fd, identity in expected_fds.items()), (
            f"Interpreter inherited GHC or unrelated pipe descriptors: {observed}"
        )


def run_case(proxy, child_python, scenario):
    with tempfile.TemporaryDirectory(prefix="iserv-proxy-lifecycle-") as temporary:
        directory = Path(temporary)
        read_local, write_proxy = os.pipe()
        read_proxy, write_local = os.pipe()
        extra_read, extra_write = os.pipe()
        descriptors = (write_proxy, read_proxy, extra_write)
        expected_fds = {}
        for descriptor in descriptors:
            status = os.fstat(descriptor)
            expected_fds[str(descriptor)] = [status.st_dev, status.st_ino]
        command = [
            str(proxy), str(write_proxy), str(read_proxy), "--pipe",
            child_python, str(Path(__file__).resolve().with_name("fake-interpreter.py")),
            scenario, str(directory), *(str(fd) for fd in descriptors), "-v",
        ]
        # Files avoid an unrelated stdout/stderr reader hanging on descendants.
        with (directory / "stdout").open("wb") as stdout, (directory / "stderr").open("wb") as stderr:
            process = subprocess.Popen(
                command, pass_fds=descriptors, start_new_session=True,
                stdout=stdout, stderr=stderr,
            )
        for descriptor in (write_proxy, read_proxy, extra_read, extra_write):
            os.close(descriptor)

        start = time.monotonic()
        try:
            drive_protocol(process, directory, read_local, write_local, scenario)
            process.wait(timeout=CLEANUP_TIMEOUT)
            elapsed = time.monotonic() - start
            stderr = (directory / "stderr").read_text(errors="replace")
            assert_result(process, directory, scenario, stderr, expected_fds)
            print(f"PASS {scenario}: exit={process.returncode}, {elapsed:.3f}s", flush=True)
        except (AssertionError, subprocess.TimeoutExpired) as error:
            cleanup_fixture(process, directory)
            stderr = (directory / "stderr").read_text(errors="replace")
            raise AssertionError(f"{scenario}: {error}; stderr tail={stderr[-4000:]!r}") from error
        finally:
            cleanup_fixture(process, directory)
            os.close(read_local)
            with contextlib.suppress(OSError):
                os.close(write_local)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", type=Path, required=True)
    parser.add_argument("--child-python", default=sys.executable)
    parser.add_argument("--case", choices=CASES, action="append", dest="cases")
    parser.add_argument("--repeat", type=int, default=1)
    args = parser.parse_args()
    if os.name != "posix":
        parser.error("These fixtures require POSIX processes and descriptors")
    if args.repeat < 1:
        parser.error("--repeat must be positive")

    def interrupted(signum, _frame):
        # An external timeout must still run each fixture's finally cleanup.
        raise SystemExit(128 + signum)

    signal.signal(signal.SIGTERM, interrupted)
    failed = False
    for _ in range(args.repeat):
        for scenario in args.cases or CASES:
            try:
                run_case(args.proxy.resolve(), args.child_python, scenario)
            except AssertionError as error:
                failed = True
                print(f"FAIL {error}", file=sys.stderr, flush=True)
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
