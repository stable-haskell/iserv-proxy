#ifdef __ANDROID__
#define BIONIC_IOCTL_NO_SIGNEDNESS_OVERLOAD
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <assert.h>
#include <math.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/epoll.h>
// #include <sys/eventfd.h>
// #include <fcntl.h> // this includes too many overloaded items.
#include <string.h>
#include <ctype.h>
#include <wchar.h>
#include <utime.h>
#include <poll.h>
#include <time.h>
#include <dirent.h>
#include <netdb.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/auxv.h>
#include <sys/random.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>
#include <sys/uio.h>
#include <termios.h>

// fnctl stubs, see above
extern void open(void);
extern void openat(void);
extern void creat(void);
extern void eventfd(void);
extern void eventfd_write(void);

extern void futimes(void);
extern void lutimes(void);
extern void statx(void);

extern void __stack_chk_fail(void);
extern void __vsprintf_chk(void);
extern void __open_2(void);
extern void __memcpy_chk(void);
extern void __memset_chk(void);
extern void __memmove_chk(void);
// GCC stuff
extern void __addtf3(void);
extern void __divtf3(void);
extern void __extenddftf2(void);
extern void __fixtfsi(void);
extern void __floatditf(void);
extern void __floatsitf(void);
extern void __getf2(void);
extern void __gttf2(void);
extern void __lttf2(void);
extern void __multf3(void);
extern void __subtf3(void);
extern void __trunctfdf2(void);

extern void flock(void);
extern void times(void);

extern void uname(void);
extern void sem_post(void);
extern void __gnu_strerror_r(void);
extern void async_safe_format_buffer(void);
extern void sem_trywait(void);
extern void sem_wait(void);
extern void sem_unlink(void);
extern void sem_close(void);
extern void sem_open(void);
extern void sem_getvalue(void);
extern void dlclose(void);
extern void dlerror(void);
extern void dlsym(void);
extern void dlopen(void);
extern void fallocate(void);
extern void posix_fallocate(void);
extern void __fadvise64(void);
extern void posix_fadvise(void);
extern void async_safe_fatal_no_abort(void);
extern void __vsnprintf_chk(void);
extern void openpty(void);
extern void __getpriority(void);
extern void * _Unwind_Resume;
extern void __emutls_get_address(void);
//extern void __sF(void);
extern void __sflush(void);
extern void __eqtf2(void);
extern void __ldtoa(void);
extern void __vfprintf(void);
//extern void _Unwind_SetGR(void);
//extern void _Unwind_SetIP(void);
//extern void _Unwind_GetLanguageSpecificData(void);
//extern void _Unwind_GetIP(void);
//extern void _Unwind_GetRegionStart(void);
void * __gxx_personality_v0;
extern void __cxa_guard_acquire(void);
extern void __cxa_guard_release(void);
extern void __strncpy_chk(void);

#define MISSING_FUN(f) void (f)(void) { printf("Unknown call to `%s'\n", #f); exit(1); }

MISSING_FUN(c_format_unix_time)
MISSING_FUN(c_format_unix_time_gmt)
MISSING_FUN(c_parse_unix_time)
MISSING_FUN(c_parse_unix_time_gmt)
MISSING_FUN(abort_message)
//MISSING_FUN(__cxa_unexpected_handler)
MISSING_FUN(__cxa_begin_catch)
MISSING_FUN(_ZSt9terminatev)
//MISSING_FUN(_ZN10__cxxabiv130__aligned_malloc_with_fallbackEm)
//MISSING_FUN(_ZN10__cxxabiv119__getExceptionClassEPK17_Unwind_Exception)
//MISSING_FUN(_ZSt11__terminatePFvvE)
// MISSINF_FUN(__gxx_personality_v0)
// Symbols absent from the aarch64 Android NDK's bionic / libc++
// as shipped with our current toolchain.  The dynamic linker
// would otherwise fail to load this library when it sees the
// unresolved references via `SYM(...)` below.  These stubs are
// never expected to be invoked at runtime; if a real call ever
// lands here we `exit(1)` with a diagnostic so the caller is
// visible instead of silently corrupting state.  Mirrors the
// armv7a-android stubs added earlier.
MISSING_FUN(__cxa_guard_acquire)
MISSING_FUN(__cxa_guard_release)
MISSING_FUN(__fadvise64)
MISSING_FUN(__getpriority)
MISSING_FUN(__ldtoa)
MISSING_FUN(__sflush)
MISSING_FUN(__vfprintf)
MISSING_FUN(async_safe_fatal_no_abort)
MISSING_FUN(async_safe_format_buffer)

typedef void SymbolAddr;
typedef char SymbolName;

typedef enum _SymStrength {
    STRENGTH_NORMAL,
    STRENGTH_WEAK,
    STRENGTH_STRONG,
} SymStrength;

typedef enum _SymType {
    SYM_TYPE_CODE = 1 << 0, /* the symbol is a function and can be relocated via a jump island */
    SYM_TYPE_DATA = 1 << 1, /* the symbol is data */
    SYM_TYPE_INDIRECT_DATA = 1 << 2, /* see Note [_iob_func symbol] */
    SYM_TYPE_DUP_DISCARD = 1 << 3, /* the symbol is a symbol in a BFD import library
                                      however if a duplicate is found with a mismatching
                                      SymType then discard this one.  */
} SymType;

typedef struct _RtsSymbolVal {
    const SymbolName* lbl;
    SymbolAddr* addr;
    SymStrength strength;
    SymType type;
} RtsSymbolVal;

#define SYM(x) { #x, (void*)(&x), STRENGTH_NORMAL, 1 }
typedef mode_t (*umask_func_ptr_t)(mode_t);

RtsSymbolVal my_iserv_syms[] = {
    // arpa/inet.h
    SYM(htons),
    SYM(ntohs),
    SYM(htonl),
    SYM(ntohl),
    // sys/random.h
    SYM(getentropy),
    SYM(getrandom),
    // sys/auxv.h
    SYM(getauxval),
    // sys/mman.h
    SYM(madvise),SYM(mlock),SYM(mmap),SYM(mprotect),SYM(munmap),
    SYM(mremap),
    SYM(munlock),
    // select.h
    SYM(__FD_SET_chk),
    // sys/socket
    SYM(accept),SYM(bind),SYM(connect),SYM(getsockopt),SYM(listen),
    SYM(setsockopt),SYM(socket),SYM(getsockname),SYM(select),
    SYM(getpeername),SYM(__cmsg_nxthdr),SYM(recv),SYM(recvfrom),
    SYM(recvmsg),SYM(send),SYM(sendmsg),SYM(sendto),SYM(writev),
    SYM(accept4),SYM(socketpair),
    // pthread.h
    SYM(pthread_equal),SYM(pthread_getspecific),SYM(pthread_key_create),
    SYM(pthread_key_delete),SYM(pthread_once),SYM(pthread_rwlock_destroy),
    SYM(pthread_rwlock_init),SYM(pthread_rwlock_rdlock),SYM(pthread_rwlock_unlock),
    SYM(pthread_rwlock_wrlock),SYM(pthread_self),SYM(pthread_setspecific),
    SYM(pthread_create),SYM(pthread_join),SYM(pthread_mutex_destroy),
    SYM(pthread_mutex_init),SYM(pthread_mutex_lock),SYM(pthread_mutex_trylock),
    SYM(pthread_mutex_unlock),SYM(pthread_mutexattr_destroy),
    SYM(pthread_mutexattr_init),SYM(pthread_mutexattr_settype),
    // chk.h
    SYM(__read_chk),SYM(__write_chk),
    // netdb.h
    SYM(freeaddrinfo),SYM(gai_strerror),SYM(getaddrinfo),SYM(getnameinfo),
    SYM(gethostbyname),
    // dirent.h
    SYM(readdir_r),SYM(readdir),
    SYM(opendir),SYM(closedir),
    // time.h
    SYM(clock),SYM(gmtime_r),
    // sys/time.h
    SYM(gettimeofday),SYM(clock_getres),SYM(clock_gettime),SYM(localtime_r),SYM(tzset),
    // unistd.h
    SYM(readlink),
    SYM(rename),
    SYM(rmdir),
    SYM(chown),
    SYM(realpath),
    SYM(fchdir),
    SYM(fdopendir),
    SYM(rewinddir),
    SYM(futimens),SYM(futimes),SYM(lutimes),
    SYM(mknod),
    SYM(lchown),
    SYM(symlink),
    SYM(endgrent),SYM(endpwent),
    SYM(pathconf),
    SYM(truncate),
    SYM(utimensat),
    SYM(statx),
    SYM(seekdir),
    SYM(telldir),
    SYM(clearenv),
    SYM(chdir),
    SYM(sleep),
    SYM(stdout),
    SYM(strftime),
    SYM(utimes),
    SYM(times),
    SYM(setenv),
    SYM(fpathconf),
    SYM(exit),
    SYM(environ),
    SYM(ftruncate),
    SYM(getenv),
    SYM(putenv),
    SYM(unsetenv),
    SYM(read),
    SYM(write),
    SYM(isatty),
    SYM(link),
    SYM(pipe),
    SYM(unlink),
    SYM(execv),SYM(execve),SYM(execvp),SYM(execvpe),
    SYM(syscall),SYM(sysconf),
    // errno.h
    SYM(__errno),
    // math.h
    SYM(sinhf), SYM(sinh), SYM(sinf), SYM(sin),
    SYM(coshf), SYM(cosh), SYM(cosf), SYM(cos),
    SYM(atanhf), SYM(atanh), SYM(atanf), SYM(atan),
    SYM(asinhf), SYM(asinh), SYM(asinf), SYM(asin),
    SYM(acoshf), SYM(acosh), SYM(acosf), SYM(acos),
    SYM(log1pf), SYM(log1p), SYM(logf), SYM(log),
    SYM(expm1f), SYM(expm1),
    SYM(expf), SYM(exp),
    SYM(ldexp),
    SYM(powf), SYM(pow),
    SYM(sqrtf), SYM(sqrt),
    SYM(tanhf), SYM(tanh), SYM(tanf), SYM(tan),
    // assert.h
    SYM(__assert2),
    // signal.h
    SYM(signal),SYM(sigaction),
    SYM(raise), SYM(sigaddset), SYM(sigemptyset), SYM(sigprocmask),
    // sys/eventfd.h
    SYM(eventfd), SYM(eventfd_write),
    // sys/stat.h
    SYM(fstat),
    SYM(lstat),
    SYM(stat),
    SYM(chmod),
    SYM(mkfifo),
    // SYM(umask),
    { "umask", (umask_func_ptr_t)(&umask), STRENGTH_NORMAL, 1 },
    // sys/wait.h
    SYM(waitpid),
    // sym/epoll.h
    SYM(epoll_create), SYM(epoll_ctl), SYM(epoll_wait),
    // poll.h
    SYM(poll),
    // fcntl.h
    SYM(open), SYM(creat), SYM(fcntl), SYM(ioctl),
    SYM(openat),SYM(__open_2),
    // string.h
    SYM(strerror),
    SYM(strerror_r),
    SYM(strcmp),
    SYM(memchr),SYM(strcpy),SYM(strchr),SYM(strncpy),SYM(strrchr),
    SYM(strcat),SYM(strncmp),SYM(strdup),
    SYM(strtoul),SYM(strspn),SYM(strtol),SYM(strstr),SYM(strcspn),
    SYM(strpbrk),
    SYM(__strncpy_chk),SYM(__strncpy_chk2),SYM(__memcpy_chk),
    // ctype.h
    SYM(__ctype_get_mb_cur_max),
    // wchar.h
    SYM(mbrtowc), SYM(wcrtomb),
    // stdlib.h
    SYM(qsort),
    // unistd.h
    SYM(access), SYM(close), SYM(dup), SYM(dup2), SYM(fork), SYM(getpid),
    SYM(lseek),
    // utime.h
    SYM(utime),SYM(time),
    // ...
    SYM(fileno),
    SYM(__vsprintf_chk),
    SYM(__strlen_chk),
    SYM(__strchr_chk),
    SYM(__memset_chk),
    SYM(__memmove_chk),
    SYM(__memchr_chk),
    SYM(__stack_chk_fail),
    SYM(memmove),
    SYM(memcmp),
    SYM(memcpy),
    SYM(memset),
    SYM(stderr),
    SYM(realloc),
    SYM(calloc),
    SYM(malloc),
    SYM(free),
    SYM(fprintf),
    SYM(vfprintf),
    SYM(fopen), SYM(fclose),
    SYM(flock),
    SYM(getegid),SYM(getgid),
    SYM(getpwent),
    SYM(getgrent),
    SYM(getgroups),
    SYM(getlogin),
    SYM(getuid),
    SYM(getgrgid_r),SYM(getgrnam_r),SYM(getpwnam_r),SYM(getpwuid_r),
    SYM(setegid),SYM(seteuid),SYM(setgid),SYM(setgrent),SYM(setgroups),
    SYM(setpwent),SYM(setuid),
    SYM(fread),
    SYM(abort),
    SYM(strlen),
    SYM(fwrite),
    SYM(feof),
    SYM(ferror),
    SYM(fflush),
    SYM(fgets),SYM(fputc),SYM(fputs),
    SYM(puts),
    SYM(fseek),SYM(ftell),
    SYM(sscanf),
    SYM(shutdown),
    SYM(atoi),
    SYM(stdin),
    SYM(atexit),
    SYM(usleep),
    SYM(fchmod),
    SYM(fchown),
    SYM(fsync),
    SYM(getcwd),
    SYM(geteuid),
    SYM(localtime),
    SYM(lseek64),
    SYM(mkdir),
    SYM(mktime),
    SYM(fdopen),
    SYM(c_format_unix_time),
    SYM(c_format_unix_time_gmt),
    SYM(c_parse_unix_time),
    SYM(c_parse_unix_time_gmt),
    SYM(__addtf3),
    SYM(__divtf3),
    SYM(__extenddftf2),
    SYM(__fixtfsi),
    SYM(__floatditf),
    SYM(__floatsitf),
    SYM(__getf2),
    SYM(__gttf2),
    SYM(__lttf2),
    SYM(__multf3),
    SYM(__subtf3),
    SYM(__trunctfdf2),
    SYM(getrusage),
    SYM(tcsetattr),
    SYM(tcgetattr),
    SYM(setrlimit),
    SYM(getrlimit),
    SYM(uname),
    SYM(fdatasync),
    SYM(nanosleep),
    SYM(kill),
    SYM(killpg),
    SYM(sigpending),
    SYM(sigsuspend),
    SYM(setitimer),
    SYM(alarm),
    SYM(sigismember),
    SYM(sigfillset),
    SYM(sigdelset),
    SYM(__gnu_strerror_r),
    SYM(async_safe_format_buffer),
    SYM(android_get_application_target_sdk_version),
    SYM(sem_post),
    SYM(sem_trywait),
    SYM(sem_wait),
    SYM(sem_unlink),
    SYM(sem_close),
    SYM(sem_open),
    SYM(sem_getvalue),
    SYM(dlclose),
    SYM(dlerror),
    SYM(dlsym),
    SYM(dlopen),
    SYM(fallocate),
    SYM(posix_fallocate),
    SYM(__fadvise64),
    SYM(posix_fadvise),
    SYM(async_safe_fatal_no_abort),
    SYM(arc4random_buf),
    SYM(mkdtemp),
    SYM(mkstemps),
    SYM(mkstemp),
    SYM(__vsnprintf_chk),
    SYM(ttyname),
    SYM(openpty),
    SYM(ptsname),
    SYM(ctermid),
    SYM(getppid),
    SYM(getpgid),
    SYM(getpgrp),
    SYM(setsid),
    SYM(setpgid),
    SYM(setpriority),
    SYM(__getpriority),
    SYM(getpriority),
    SYM(tcgetpgrp),
    SYM(tcsetpgrp),
    SYM(tcflow),
    SYM(tcflush),
    SYM(tcdrain),
    SYM(tcsendbreak),
    SYM(cfsetospeed),
    SYM(cfgetospeed),
    SYM(cfsetispeed),
    SYM(cfgetispeed),
    SYM(_Unwind_Resume),
//    SYM(posix_memalign),
    SYM(__emutls_get_address),
//    SYM(__sF),
    SYM(__sflush),
    SYM(__eqtf2),
    SYM(__ldtoa),
    SYM(__vfprintf),
    SYM(vasprintf),
    SYM(abort_message),
//    SYM(__cxa_unexpected_handler),
    SYM(_ZSt9terminatev),
//    SYM(_ZN10__cxxabiv130__aligned_malloc_with_fallbackEm),
    SYM(__cxa_begin_catch),
//    SYM(_Unwind_SetGR),
//    SYM(_Unwind_SetIP),
//    SYM(_Unwind_GetLanguageSpecificData),
//    SYM(_Unwind_GetIP),
//    SYM(_Unwind_GetRegionStart),
//    SYM(_ZN10__cxxabiv119__getExceptionClassEPK17_Unwind_Exception),
//    SYM(_ZSt11__terminatePFvvE),
    SYM(__gxx_personality_v0),
    SYM(__fwrite_chk),
    SYM(timezone),
    SYM(sendmsg),
    SYM(sendmmsg),
    SYM(recvmmsg),
    SYM(__strrchr_chk),
//    SYM(__loader_add_thread_local_dtor),
//    SYM(__cxa_thread_finalize),
    SYM(pthread_exit),
    SYM(__poll_chk),
    SYM(pthread_cond_wait),
    SYM(__cxa_guard_acquire),
    SYM(__cxa_guard_release),
    SYM(pthread_cond_signal),
    SYM(pthread_cond_broadcast),
    SYM(pthread_cond_init),
    SYM(pthread_cond_timedwait),
    SYM(pthread_cond_destroy),
    SYM(pthread_attr_init),
    SYM(pthread_attr_setdetachstate),
    SYM(pthread_attr_destroy),
    SYM(nice),
    SYM(unlinkat),
    SYM(fchmodat),
    SYM(fstatat),
    SYM(floor),
    { 0, 0, STRENGTH_NORMAL, 1 } /* sentinel */
};

RtsSymbolVal* iserv_syms() {
    return my_iserv_syms;
}

// Name used in GHC >=9.12
RtsSymbolVal* rtsExtraSyms() {
    return my_iserv_syms;
}
#endif
