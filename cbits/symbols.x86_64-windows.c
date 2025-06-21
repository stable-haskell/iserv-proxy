#include <string.h>
#include <math.h>

#define MISSING_FUN(f) void (f)(void) { printf("Unknown call to `%s'\n", #f); exit(1); }

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
    { 0, 0, STRENGTH_NORMAL, 1 } /* sentinel */
};

RtsSymbolVal* iserv_syms() {
    return my_iserv_syms;
}

// Name used in GHC >=9.12
RtsSymbolVal* rtsExtraSyms() {
    return my_iserv_syms;
}
