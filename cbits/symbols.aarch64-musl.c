#define SYM(s) extern void s(void);
#include "symbols.aarch64-musl.h"
#undef SYM

extern void exit(int);
extern void printf(const char*, ...);

#define MISSING_FUN(f) void (f)(void) { printf("Unknown call to `%s'\n", #f); exit(1); }

MISSING_FUN(c_format_unix_time)
MISSING_FUN(c_format_unix_time_gmt)
MISSING_FUN(c_parse_unix_time)
MISSING_FUN(c_parse_unix_time_gmt)

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

#define SYM(x) { #x, (void*)(&x), STRENGTH_NORMAL, 1 },
// typedef mode_t (*umask_func_ptr_t)(mode_t);

RtsSymbolVal my_iserv_syms[] = {
#include "symbols.aarch64-musl.h"
    { 0, 0, STRENGTH_NORMAL, 1 } /* sentinel */
};

RtsSymbolVal* iserv_syms() {
    return my_iserv_syms;
}