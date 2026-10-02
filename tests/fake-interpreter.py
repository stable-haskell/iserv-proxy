#!/usr/bin/env python3
# Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
# SPDX-License-Identifier: BSD-3-Clause
"""The two-message interpreter used by lifecycle.py."""

import argparse
import json
import os
from pathlib import Path
import resource
import signal
import struct
import time


def record_process(directory, role):
    with (directory / "processes").open("a") as stream:
        stream.write(f"{os.getpid()} {os.getpgrp()} {role}\n")


def wait_forever():
    while True:
        signal.pause()


def read_exact(size):
    data = bytearray()
    while len(data) < size:
        chunk = os.read(0, size - len(data))
        assert chunk, "Unexpected end of LoadObj request"
        data.extend(chunk)
    return data


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("scenario")
    parser.add_argument("directory", type=Path)
    parser.add_argument("descriptors", nargs="*", type=int)
    parser.add_argument("-v", action="store_true")
    args = parser.parse_args()
    scenario = args.scenario
    directory = args.directory

    if scenario in {
        "close_stdout_ignore_term", "ghc_disconnect", "interrupt", "terminate",
        "terminate_during_shutdown", "interrupt_during_shutdown",
        "late_terminate", "late_interrupt", "blocked_write",
    }:
        signal.signal(signal.SIGTERM, signal.SIG_IGN)

    if scenario == "fd_isolation":
        descriptors = {}
        for descriptor in args.descriptors:
            try:
                status = os.fstat(descriptor)
                descriptors[str(descriptor)] = [status.st_dev, status.st_ino]
            except OSError:
                descriptors[str(descriptor)] = None
        (directory / "descriptors.json").write_text(json.dumps(descriptors))

    record_process(directory, "interpreter")
    (directory / "ready").touch()
    if scenario in {"exit_idle", "success_idle"}:
        os._exit(139 if scenario == "exit_idle" else 0)

    while True:
        request = os.read(0, 1)
        if request == b"\x06":  # LoadObj followed by a Binary String.
            if scenario == "blocked_write":
                (directory / "write-blocked").touch()
                wait_forever()  # Keep stdin open with a pending proxy write.
            size, = struct.unpack(">q", read_exact(8))
            assert read_exact(size) == b"a" * size
            os.write(1, b"\x02")  # Proxy Done, followed by a unit result.
        elif request == b"\x0a":  # ResolveObjs returns a Binary Bool.
            if scenario == "exit_after_request":
                os._exit(139)
            if scenario == "signal_after_request":
                resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
                os.kill(os.getpid(), signal.SIGSEGV)
            if scenario == "exit_with_descendant":
                if os.fork() == 0:
                    signal.signal(signal.SIGTERM, signal.SIG_IGN)
                    record_process(directory, "descendant")
                    (directory / "descendant-ready").touch()
                    wait_forever()  # Keep the interpreter's stdout open.
                while not (directory / "descendant-ready").exists():
                    time.sleep(0.01)
                os._exit(139)
            if scenario == "close_stdout_ignore_term":
                os.close(1)
                wait_forever()
            os.write(1, b"\x01")
        elif request == b"\x00":  # Shutdown returns no response bytes.
            (directory / "shutdown-started").touch()
            if scenario in {"terminate_during_shutdown", "interrupt_during_shutdown"}:
                wait_forever()
            if scenario in {"slow_shutdown", "late_terminate", "late_interrupt"}:
                time.sleep(1)
            (directory / "shutdown-completed").touch()
            os._exit(23 if scenario == "shutdown_failure" else 0)
        elif request == b"":
            if scenario in {"ghc_disconnect", "interrupt", "terminate"}:
                wait_forever()
            os._exit(0)
        else:
            raise AssertionError(f"Unknown request: {request!r}")


if __name__ == "__main__":
    main()
