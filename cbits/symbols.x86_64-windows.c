#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define MISSING_FUN(f) void (f)(void) { printf("Unknown call to `%s'\n", #f); exit(1); }

/* mingw-w64 14.0.0 added an `_assert` member to `libmingwex.a` (13.0.0 had
   none, so `_assert` came from the UCRT import library).  That member calls
   the CRT's assert through `__imp___msvcrt_assert`, and wine's ucrtbase does
   not export that alias by name -- `GetProcAddress` returns NULL for it, so
   the whole archive fails to resolve and every unit that needs a symbol from
   it (ghc-bignum, ...) fails to load.  Hand the linker the indirection
   directly; same shape as GHC's Note [_iob_func symbol].

   Do not point this at `_assert`: with libmingwex's member linked in that is
   the caller, and it would recurse. */
static void
iserv_msvcrt_assert (const char *expr, const char *file, unsigned int line)
{
    fprintf (stderr, "Assertion failed: %s, file %s, line %u\n",
             expr ? expr : "(null)", file ? file : "(null)", line);
    abort ();
}

static const void *iserv_imp_msvcrt_assert = (const void *) &iserv_msvcrt_assert;

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

RtsSymbolVal my_iserv_syms[] = {
    SYM(acos),
    SYM(acosf),
    SYM(acosh),
    SYM(acoshf),
    SYM(asin),
    SYM(asinf),
    SYM(asinh),
    SYM(asinhf),
    SYM(atan),
    SYM(atanf),
    SYM(atanh),
    SYM(atanhf),
    SYM(cos),
    SYM(cosf),
    SYM(cosh),
    SYM(coshf),
    SYM(erf),
    SYM(erfc),
    SYM(erfcf),
    SYM(erff),
    SYM(exp),
    SYM(expf),
    SYM(expm1),
    SYM(expm1f),
    SYM(log),
    SYM(log1p),
    SYM(log1pf),
    SYM(logf),
    SYM(memcmp),
    SYM(memcpy),
    SYM(memmove),
    SYM(memset),
    SYM(pow),
    SYM(powf),
    SYM(sin),
    SYM(sinf),
    SYM(sinh),
    SYM(sinhf),
    SYM(strlen),
    SYM(tan),
    SYM(tanf),
    SYM(tanh),
    SYM(tanhf),
    /* POSIX-named CRT aliases.  wine's ucrtbase implements them but only
       exports the underscore spellings (`_fileno`, `_stricmp`, `_strnicmp`),
       so `GetProcAddress` cannot find these and any object referencing them
       fails to load: `fileno` is needed by libmingwex's `_assert` member (see
       above), `strncasecmp` by unix-time.  Taking their address here is fine:
       the static link resolves it through mingw-w64's import library. */
    SYM(fileno),
    SYM(strcasecmp),
    SYM(strncasecmp),
    { "__imp___msvcrt_assert", (void*)&iserv_imp_msvcrt_assert,
      STRENGTH_NORMAL, SYM_TYPE_INDIRECT_DATA },
    { 0, 0, STRENGTH_NORMAL, 1 } /* sentinel */
};

RtsSymbolVal* iserv_syms() {
    return my_iserv_syms;
}

// Name used in GHC >=9.12
RtsSymbolVal* rtsExtraSyms() {
    return my_iserv_syms;
}
