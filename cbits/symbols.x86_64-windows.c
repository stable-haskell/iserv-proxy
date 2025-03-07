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
    SYM(log1pf), SYM(log1p), SYM(logf), SYM(log),
    { 0, 0, STRENGTH_NORMAL, 1 } /* sentinel */
};

RtsSymbolVal* iserv_syms() {
    return my_iserv_syms;
}
