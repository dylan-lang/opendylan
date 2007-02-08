
#ifndef MY_BOOL_H
#define MY_BOOL_H

#ifdef _AIX
#include <sys/types.h>
#define boolean boolean_t
#else
typedef int boolean;
#endif

#ifndef TRUE

#define TRUE 1
#define FALSE 0

#endif

#endif
