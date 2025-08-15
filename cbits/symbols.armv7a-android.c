#if defined(__ANDROID__) && defined(__arm__)
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

extern void __aeabi_idiv(void);
extern void __aeabi_idivmod(void);
extern void __aeabi_memcpy(void);
extern void __aeabi_memclr(void);
extern void __aeabi_memclr4(void);
extern void __aeabi_ldivmod(void);
extern void __aeabi_memcpy4(void);
extern void __aeabi_memmove(void);
extern void __aeabi_memset4(void);
extern void __aeabi_uidiv(void);
extern void __aeabi_uidivmod(void);
extern void __aeabi_uldivmod(void);
extern void __aeabi_d2f(void);
extern void __aeabi_d2iz(void);
extern void __aeabi_dadd(void);
extern void __aeabi_dcmpeq(void);
extern void __aeabi_dcmpge(void);
extern void __aeabi_dcmpgt(void);
extern void __aeabi_dcmple(void);
extern void __aeabi_dcmplt(void);
extern void __aeabi_ddiv(void);
extern void __aeabi_dmul(void);
extern void __aeabi_f2d(void);
extern void __aeabi_fcmpeq(void);
extern void __aeabi_fcmpge(void);
extern void __aeabi_fcmpgt(void);
extern void __aeabi_fcmple(void);
extern void __aeabi_i2d(void);

extern void eventfd_write(void);
extern void ioctl(void);
extern void close(void);
extern void epoll_wait(void);
// extern void expf(void);
extern void getpid(void);
extern void isatty(void);
extern void waitpid(void);

extern void dup2(void);
extern void epoll_ctl(void);
extern void ftruncate64(void);
extern void link(void);
extern void pipe(void);

extern void __assert2(void);
extern void dup(void);
extern void epoll_create(void);
extern void eventfd(void);
extern void fork(void);
extern void unlink(void);
extern void creat(void);
extern void fcntl(void);
extern void open(void);
extern void access(void);
extern void environ(void);
extern void lseek(void);
extern void lseek64(void);
extern void umask(void);
extern void poll(void);
extern void raise(void);
extern void mbrtowc(void);
extern void sigprocmask(void);
extern void sigaddset(void);
extern void sigemptyset(void);
extern void stat(void);
extern void utime(void);
extern void chmod(void);
extern void mkfifo(void);
extern void wcrtomb(void);
extern void lstat(void);
extern void fstat(void);
extern void write(void);
extern void read(void);
extern void __stack_chk_fail(void);
extern void __stack_chk_guard(void);
extern void __aeabi_memclr8(void);
extern void __aeabi_memcpy8(void);
extern void __aeabi_memmove4(void);
extern void __write_chk(void);
extern void time(void);
extern void uname(void);
extern void tzset(void);
extern void sysconf(void);
extern void socket(void);
extern void socketpair(void);
extern void readdir(void);
extern void readdir_r(void);
extern void pthread_rwlock_init(void);
extern void __memset_chk(void);
extern void __memcpy_chk(void);
extern void gmtime_r(void);
extern void getauxval(void);
extern void clock_gettime(void);
extern void __aeabi_memset4(void);
extern void __aeabi_memset8(void);
extern void __aeabi_memset(void);
extern void __vsprintf_chk(void);
extern void usleep(void);
extern void setsockopt(void);
extern void __read_chk(void);
extern void opendir(void);
extern void mmap(void);
extern void pthread_rwlock_rdlock(void);
extern void __memmove_chk(void);
extern void localtime_r(void);
extern void getsockopt(void);
extern void getnameinfo(void);
extern void getentropy(void);
extern void clock_getres(void);
extern void __aeabi_d2lz(void);
extern void __aeabi_l2d(void);
extern void __aeabi_d2ulz(void);
extern void syscall(void);
extern void shutdown(void);
extern void pthread_rwlock_wrlock(void);
extern void mprotect(void);
extern void gettimeofday(void);
extern void gethostbyname(void);
extern void gai_strerror(void);
extern void fsync(void);
extern void connect(void);
extern void closedir(void);
extern void bind(void);
extern void sleep(void);
extern void pthread_rwlock_unlock(void);
extern void __open_2(void);
extern void mlock(void);
extern void listen(void);
extern void freeaddrinfo(void);
extern void clock(void);
extern void __aeabi_ul2d(void);
extern void accept(void);
extern void strftime(void);
extern void pthread_rwlock_destroy(void);
extern void madvise(void);
extern void getsockname(void);
extern void getaddrinfo(void);
extern void sigaction(void);
extern void pthread_once(void);
extern void munmap(void);
extern void __FD_SET_chk(void);
extern void __cmsg_nxthdr(void);
extern void pthread_key_create(void);
extern void select(void);
extern void sendmsg(void);
extern void signal(void);
extern void pthread_getspecific(void);
extern void pthread_setspecific(void);
extern void munlock(void);
extern void pthread_key_delete(void);
extern void pthread_key_create(void);
extern void pthread_mutexattr_init(void);
extern void pthread_self(void);
extern void pthread_mutexattr_settype(void);
extern void pthread_equal(void);
extern void pthread_mutex_init(void);
extern void pthread_mutexattr_destroy(void);
extern void pthread_mutex_destroy(void);
extern void pthread_mutex_lock(void);
extern void pthread_mutex_trylock(void);
extern void pthread_mutex_unlock(void);
extern void utimes(void);
extern void pthread_join(void);
extern void __aeabi_l2f(void);
extern void pthread_create(void);
extern void localtime(void);
extern void getcwd(void);
extern void fchmod(void);
extern void mkdir(void);
extern void rmdir(void);
extern void fchown(void);
extern void geteuid(void);
extern void mmap64(void);
extern void mremap(void);
extern void readlink(void);
extern void flock(void);
extern void times(void);
extern void getrusage(void);
extern void openat(void);
extern void futimes(void);
extern void lutimes(void);
extern void dlclose(void);
extern void dlerror(void);
extern void dlsym(void);
extern void dlopen(void);
extern void execv(void);
extern void execve(void);
extern void execvp(void);
extern void execvpe(void);
extern void setrlimit(void);
extern void sigpending(void);
extern void getrlimit(void);
extern void fallocate(void);
extern void rewinddir(void);
extern void seekdir(void);
extern void telldir(void);
void * __gxx_personality_v0;
void * _Unwind_Resume;
extern void __atomic_load_4(void);
extern void __atomic_store_4(void);
extern void pthread_cond_wait(void);
extern void pthread_cond_broadcast(void);
extern void __cxa_guard_acquire(void);
extern void __cxa_guard_release(void);
extern void getpeername(void);
extern void mktime(void);
extern void __poll_chk(void);
extern void pthread_attr_destroy(void);
extern void pthread_attr_init(void);
extern void pthread_attr_setdetachstate(void);
extern void pthread_cond_destroy(void);
extern void pthread_cond_init(void);
extern void pthread_cond_signal(void);
extern void pthread_cond_timedwait(void);
extern void pthread_exit(void);
extern void recvfrom(void);
extern void recvmmsg(void);
extern void sendmmsg(void);
extern void sendto(void);
extern void timezone(void);
extern void fdatasync(void);
extern void fdopendir(void);
extern void nanosleep(void);
extern void sigsuspend(void);
extern void __assert(void);
extern void _Z21__libc_shared_globalsv(void);
extern void __strcpy_chk(void);
extern void __cxa_thread_finalize(void);
extern void kill(void);
extern void sigismember(void);
extern void __vsnprintf_chk(void);
extern void setitimer(void);
extern void sigfillset(void);
extern void __readlink_chk(void);
extern void __libc_stdio_cleanup(void);
extern void sigdelset(void);
extern void writev(void);
extern void prctl(void);
extern void getuid(void);
extern void __fsetxattr(void);
extern void setxattr(void);
extern void setuid(void);
extern void setresuid(void);
extern void setgid(void);
extern void setresgid(void);
extern void getegid(void);
extern void getgid(void);
extern void setgroups(void);
extern void getgroups(void);
extern void chdir(void);
extern void __fstatfs64(void);
extern void __statfs64(void);
extern void fchownat(void);
extern void symlinkat(void);
extern void truncate64(void);
extern void mknodat(void);
extern void fallocate64(void);
extern void __arm_fadvise64_64(void);
extern void setsid(void);
extern void _exit(int) __attribute__((noreturn));
extern void fchdir(void);
extern void utimensat(void);
extern void getppid(void);
extern void getpgid(void);
extern void setpgid(void);
extern void setpriority(void);
extern void __getpriority(void);
extern void __emutls_get_address(void);
extern void __cxa_guard_abort(void);
extern void killpg(void);
extern void alarm(void);
extern void sem_post(void);
extern void sem_trywait(void);
extern void sem_wait(void);
extern void sem_unlink(void);
extern void sem_close(void);
extern void sem_open(void);
extern void sem_getvalue(void);
extern void getlogin(void);
extern void getgrent(void);
extern void setgrent(void);
extern void endgrent(void);
extern void getpwent(void);
extern void setpwent(void);
extern void endpwent(void);
extern void seteuid(void);
extern void setegid(void);
extern void getpwnam_r(void);
extern void getpwuid_r(void);
extern void getgrnam_r(void);
extern void getgrgid_r(void);
extern void pathconf(void);
extern void chown(void);
extern void lchown(void);
extern void symlink(void);
extern void mknod(void);
extern void posix_fallocate64(void);
extern void posix_fadvise64(void);
extern void ttyname(void);
extern void openpty(void);
extern void fpathconf(void);
extern void futimens(void);
extern void getpgrp(void);
extern void getpriority(void);
extern void nice(void);
extern void tcgetpgrp(void);
extern void tcsetpgrp(void);
extern void unlinkat(void);
extern void fchmodat(void);
extern void fstatat(void);
extern void __strncpy_chk(void);

#define MISSING_FUN(f) void (f)(void) { printf("Unknown call to %s\n", #f); exit(1); }

MISSING_FUN(c_format_unix_time)
MISSING_FUN(c_format_unix_time_gmt)
MISSING_FUN(c_parse_unix_time)
MISSING_FUN(c_parse_unix_time_gmt)
MISSING_FUN(__cxa_begin_catch)
MISSING_FUN(_ZSt9terminatev)
MISSING_FUN(__loader_add_thread_local_dtor)
MISSING_FUN(__loader_remove_thread_local_dtor)
MISSING_FUN(__clang_call_terminate)

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

// work around built-ins being referenced from libraries, and us not being able to &<built-in>; instead
// wrap the built-in into a function.
//int32_t ___sync_val_compare_and_swap_1(volatile int32_t* ptr, int32_t oldval, int32_t newval) {
//  return __sync_val_compare_and_swap_1(ptr, oldval, newval);
//}

#define SYM(x) { #x, (void*)(&x), STRENGTH_NORMAL, 1 }
typedef mode_t (*umask_func_ptr_t)(mode_t);

RtsSymbolVal my_iserv_syms[] = {
    // { "__sync_val_compare_and_swap_1", (void*)(&___sync_val_compare_and_swap_1), STRENGTH_NORMAL, 1 },
    SYM(strlen),
    SYM(__aeabi_idiv),
    SYM(__aeabi_idivmod),
    SYM(__aeabi_memcpy),
    SYM(__aeabi_memclr),
    SYM(__aeabi_memclr4),
    SYM(__aeabi_ldivmod),
    SYM(__aeabi_memcpy4),
    SYM(__aeabi_memmove),
    SYM(__aeabi_memset4),
    SYM(__aeabi_uidiv),
    SYM(__aeabi_uidivmod),
    SYM(__aeabi_uldivmod),
    SYM(calloc),
    SYM(malloc),
    SYM(free),
    SYM(realloc),
    SYM(abort),
    SYM(close),
    SYM(epoll_wait),
    SYM(__errno),
    SYM(eventfd_write),
    SYM(expf),
    SYM(fprintf),
    SYM(getenv),
    SYM(getpid),
    SYM(ioctl),
    SYM(isatty),
    SYM(memcmp),
    SYM(poll),
    SYM(raise),
    SYM(strcmp),
    SYM(strerror),
    SYM(setenv),
    SYM(unsetenv),
    SYM(waitpid),
    SYM(dup2),
    SYM(epoll_ctl),
    SYM(ftruncate64),
    SYM(ldexp),
    SYM(link),
    SYM(log1pf),
    SYM(mbrtowc),
    SYM(pipe),
    SYM(putenv),
    SYM(sigprocmask),
    SYM(stderr),
    SYM(__assert2),
    SYM(dup),
    SYM(epoll_create),
    SYM(eventfd),
    SYM(expm1f),
    SYM(fork),
    SYM(sigaddset),
    SYM(stat),
    SYM(wcrtomb),
    SYM(__ctype_get_mb_cur_max),
    SYM(fstat),
    SYM(logf),
    SYM(sigemptyset),
    SYM(unlink),
    SYM(creat),
    SYM(lstat),
    SYM(mkfifo),
    SYM(qsort),
    SYM(sinf),
    SYM(chmod),
    SYM(cosf),
    SYM(fcntl),
    SYM(open),
    SYM(access),
    SYM(environ),
    SYM(tanf),
    SYM(utime),
    SYM(asinf),
    SYM(lseek),
    SYM(lseek64),
    SYM(umask),
    SYM(acosf),
    SYM(write),
    SYM(atanf),
    SYM(read),
    SYM(sinhf),
    SYM(coshf),
    SYM(tanhf),
    SYM(asinhf),
    SYM(acoshf),
    SYM(atanhf),
    SYM(powf),
    SYM(exp),
    SYM(log1p),
    SYM(expm1),
    SYM(log),
    SYM(sin),
    SYM(cos),
    SYM(tan),
    SYM(asin),
    SYM(acos),
    SYM(atan),
    SYM(sinh),
    SYM(cosh),
    SYM(tanh),
    SYM(asinh),
    SYM(acosh),
    SYM(atanh),
    SYM(pow),
    SYM(__aeabi_memclr8),
    SYM(__aeabi_memcpy8),
    SYM(__aeabi_memmove4),
    SYM(__aeabi_memset),
    SYM(__aeabi_memset8),
    SYM(clock_gettime),
    SYM(fopen),
    SYM(fwrite),
    SYM(getauxval),
    SYM(gmtime_r),
    SYM(memchr),
    SYM(__memcpy_chk),
    SYM(memset),
    SYM(__memset_chk),
    SYM(pthread_rwlock_init),
    SYM(readdir),
    SYM(readdir_r),
    SYM(socket),
    SYM(socketpair),
    SYM(__stack_chk_fail),
    SYM(__stack_chk_guard),
    SYM(strchr),
    SYM(__strchr_chk),
    SYM(strcpy),
    SYM(strdup),
    SYM(__strlen_chk),
    SYM(strncmp),
    SYM(strncpy),
    SYM(__strncpy_chk),
    SYM(__strncpy_chk2),
    SYM(strpbrk),
    SYM(strrchr),
    SYM(strspn),
    SYM(strstr),
    SYM(strtoul),
    SYM(sysconf),
    SYM(time),
    SYM(tzset),
    SYM(uname),
    SYM(vfprintf),
    SYM(__write_chk),
    SYM(__aeabi_d2lz),
    SYM(__aeabi_l2d),
    SYM(atexit),
    SYM(atoi),
    SYM(clock_getres),
    SYM(fclose),
    SYM(fileno),
    SYM(fread),
    SYM(fseek),
    SYM(getentropy),
    SYM(getnameinfo),
    SYM(getsockopt),
    SYM(localtime_r),
    SYM(__memmove_chk),
    SYM(__memchr_chk),
    SYM(mmap),
    SYM(opendir),
    SYM(pthread_rwlock_rdlock),
    SYM(__read_chk),
    SYM(setsockopt),
    SYM(sscanf),
    SYM(strcspn),
    SYM(strtok),
    SYM(strtol),
    SYM(usleep),
    SYM(__vsprintf_chk),
    SYM(__aeabi_d2ulz),
    SYM(bind),
    SYM(closedir),
    SYM(connect),
    SYM(ferror),
    SYM(fsync),
    SYM(ftell),
    SYM(gai_strerror),
    SYM(gethostbyname),
    SYM(gettimeofday),
    SYM(mprotect),
    SYM(pthread_rwlock_wrlock),
    SYM(shutdown),
    SYM(stdin),
    SYM(syscall),
    SYM(accept),
    SYM(__aeabi_ul2d),
    SYM(clock),
    SYM(fgets),
    SYM(fputs),
    SYM(freeaddrinfo),
    SYM(listen),
    SYM(mlock),
    SYM(__open_2),
    SYM(pthread_rwlock_unlock),
    SYM(sleep),
    SYM(strcat),
    SYM(feof),
    SYM(fflush),
    SYM(getaddrinfo),
    SYM(getsockname),
    SYM(madvise),
    SYM(perror),
    SYM(pthread_rwlock_destroy),
    SYM(strftime),
    SYM(__cmsg_nxthdr),
    SYM(__FD_SET_chk),
    SYM(fputc),
    SYM(munmap),
    SYM(pthread_once),
    SYM(sigaction),
    SYM(pthread_key_create),
    SYM(select),
    SYM(sendmsg),
    SYM(signal),
    SYM(stdout),
    SYM(pthread_getspecific),
    SYM(rename),
    SYM(munlock),
    SYM(pthread_setspecific),
    SYM(pthread_key_delete),
    SYM(pthread_mutexattr_init),
    SYM(pthread_mutexattr_settype),
    SYM(pthread_self),
    SYM(pthread_equal),
    SYM(pthread_mutex_init),
    SYM(pthread_mutexattr_destroy),
    SYM(pthread_mutex_destroy),
    SYM(pthread_mutex_lock),
    SYM(pthread_mutex_trylock),
    SYM(pthread_mutex_unlock),
    SYM(utimes),
    SYM(pthread_join),
    SYM(__aeabi_l2f),
    SYM(pthread_create),
    SYM(localtime),
    SYM(getcwd),
    SYM(fchmod),
    SYM(mkdir),
    SYM(rmdir),
    SYM(fchown),
    SYM(geteuid),
    SYM(mmap64),
    SYM(mremap),
    SYM(readlink),
    SYM(times),
    SYM(strerror_r),
    SYM(flock),
    SYM(puts),
    SYM(getrusage),
    SYM(__aeabi_memclr8),
    SYM(openat),
    SYM(futimes),
    SYM(lutimes),
    SYM(android_get_application_target_sdk_version),
    SYM(dlclose),
    SYM(dlerror),
    SYM(dlsym),
    SYM(dlopen),
    SYM(execv),
    SYM(execve),
    SYM(execvp),
    SYM(execvpe),
    SYM(setrlimit),
    SYM(sigpending),
    SYM(getrlimit),
    SYM(fallocate),
    SYM(rewinddir),
    SYM(seekdir),
    SYM(telldir),
    SYM(__gxx_personality_v0),
    SYM(__cxa_begin_catch),
    SYM(_ZSt9terminatev),
    SYM(_Unwind_Resume),
    SYM(__aeabi_d2f),
    SYM(__aeabi_d2iz),
    SYM(__aeabi_dadd),
    SYM(__aeabi_dcmpeq),
    SYM(__aeabi_dcmpge),
    SYM(__aeabi_dcmpgt),
    SYM(__aeabi_dcmple),
    SYM(__aeabi_dcmplt),
    SYM(__aeabi_ddiv),
    SYM(__aeabi_dmul),
    SYM(__aeabi_f2d),
    SYM(__aeabi_fcmpeq),
    SYM(__aeabi_fcmpge),
    SYM(__aeabi_fcmpgt),
    SYM(__aeabi_fcmple),
    SYM(__aeabi_i2d),
    SYM(__atomic_load_4),
    SYM(__atomic_store_4),
    SYM(pthread_cond_wait),
    SYM(vasprintf),
    SYM(pthread_cond_broadcast),
    SYM(__cxa_guard_acquire),
    SYM(ceil),
    SYM(__cxa_guard_release),
    SYM(getpeername),
    SYM(mktime),
    SYM(__poll_chk),
    SYM(pthread_attr_destroy),
    SYM(pthread_attr_init),
    SYM(pthread_attr_setdetachstate),
    SYM(pthread_cond_destroy),
    SYM(pthread_cond_init),
    SYM(pthread_cond_signal),
    SYM(pthread_cond_timedwait),
    SYM(pthread_exit),
    SYM(recvfrom),
    SYM(recvmmsg),
    SYM(sendmmsg),
    SYM(sendto),
    SYM(timezone),
    SYM(fdatasync),
    SYM(fdopendir),
    SYM(nanosleep),
    SYM(sigsuspend),
    SYM(__assert),
    SYM(__strlcpy_chk),
    SYM(_Z21__libc_shared_globalsv),
    SYM(__strcpy_chk),
    SYM(getline),
    SYM(__loader_add_thread_local_dtor),
    SYM(__loader_remove_thread_local_dtor),
    SYM(__cxa_thread_finalize),
    SYM(kill),
    SYM(sigismember),
    SYM(__vsnprintf_chk),
    SYM(ctermid),
    SYM(setitimer),
    SYM(sigfillset),
    SYM(__readlink_chk),
    SYM(__libc_stdio_cleanup),
    SYM(sigdelset),
    SYM(writev),
    SYM(prctl),
    SYM(getuid),
    SYM(__fsetxattr),
    SYM(setxattr),
    SYM(setuid),
    SYM(setresuid),
    SYM(setgid),
    SYM(setresgid),
    SYM(getegid),
    SYM(getgid),
    SYM(setgroups),
    SYM(getgroups),
    SYM(chdir),
    SYM(__fstatfs64),
    SYM(__statfs64),
    SYM(fchownat),
    SYM(symlinkat),
    SYM(truncate64),
    SYM(mknodat),
    SYM(fallocate64),
    SYM(__arm_fadvise64_64),
    SYM(setsid),
    SYM(_exit),
    SYM(fchdir),
    SYM(utimensat),
    SYM(getppid),
    SYM(getpgid),
    SYM(setpgid),
    SYM(setpriority),
    SYM(__getpriority),
    SYM(__emutls_get_address),
    SYM(__cxa_guard_abort),
    SYM(__clang_call_terminate),
    SYM(killpg),
    SYM(alarm),
    SYM(sem_post),
    SYM(sem_trywait),
    SYM(sem_wait),
    SYM(sem_unlink),
    SYM(sem_close),
    SYM(sem_open),
    SYM(sem_getvalue),
    SYM(getlogin),
    SYM(getgrent),
    SYM(setgrent),
    SYM(endgrent),
    SYM(getpwent),
    SYM(setpwent),
    SYM(endpwent),
    SYM(seteuid),
    SYM(setegid),
    SYM(getpwnam_r),
    SYM(getpwuid_r),
    SYM(getgrnam_r),
    SYM(getgrgid_r),
    SYM(pathconf),
    SYM(chown),
    SYM(lchown),
    SYM(symlink),
    SYM(mknod),
    SYM(clearenv),
    SYM(posix_fallocate64),
    SYM(posix_fadvise64),
    SYM(mkdtemp),
    SYM(mkstemps),
    SYM(mkstemp),
    SYM(ttyname),
    SYM(openpty),
    SYM(ptsname),
    SYM(fpathconf),
    SYM(futimens),
    SYM(getpgrp),
    SYM(exit),
    SYM(getpriority),
    SYM(nice),
    SYM(tcgetpgrp),
    SYM(tcsetpgrp),
    SYM(realpath),
    SYM(unlinkat),
    SYM(fchmodat),
    SYM(fstatat),
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
