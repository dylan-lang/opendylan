#ifndef LLVM_RUNTIME_H_
#define LLVM_RUNTIME_H_

#include <stddef.h>
#include <inttypes.h>

typedef void                   *dylan_value;
typedef void                   *D;
typedef char                   *DBSTR;
typedef const char             *DCBSTR;
typedef intptr_t                DSINT;
typedef uintptr_t               DUINT;
typedef uintptr_t               DBOOL;
typedef uintptr_t               DADDR;
typedef uintptr_t               DWORD;
typedef float                   DSFLT;
typedef double                  DDFLT;

#define ITAG 1
#define I(n) ((dylan_value) (((intptr_t)(n) << 2) | ITAG))

#include OPEN_DYLAN_RUNTIME_HEADER

#endif // LLVM_RUNTIME_H_
