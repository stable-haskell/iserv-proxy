# Proxy lifecycle notes

- Use `make help`; keep local builds and tests under `TIMEOUT`.
- Test both threaded and non-threaded RTS builds for process changes.
- Poll `getProcessExitCode` for progress on a non-threaded RTS.
- Test cancellation during successful slow Shutdown as well as forced cleanup.
  A later SIGKILL can conceal lost cancellation.
- Keep the protocol on the thread targeted by GHC's signal handlers.
- Abort and reap the child before closing pipes: closing flushes pending
  builder writes and can block while a child retains stdin without reading.
- Pipe mode owns the interpreter process group on POSIX; TCP mode owns no child.
