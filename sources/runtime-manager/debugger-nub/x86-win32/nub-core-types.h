/* ********************************************************************** */
/* ** nub-core-types.h                                                 ** */
/* ** Basic type aliases to improve code documentation in the nub.     ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                              ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                     ** */
/* **            All Rights Reserved.                                  ** */
/* ********************************************************************** */


typedef char           INT8;
typedef short          INT16;
#if !defined(_BASETSD_H_)
typedef long           INT32;
typedef void*          INT64;
#endif
typedef short          UNICODE;
typedef void*          NUBHANDLE;
typedef int            NUBINT;
typedef void*          NUB;
typedef void*          SERVER;
typedef int            NUB_INDEX;
typedef int            NUB_ERROR;
typedef void*          NUBPROCESS;
typedef void*          NUBTHREAD;
typedef void*          NUBLIBRARY;
typedef float          FLOAT;
typedef double         DOUBLE;
typedef void*          NUBFRAME;
typedef void*          TARGET_ADDRESS;
typedef int            TARGET_INT;

/*
Many pointers used in the remote debugger nub are completely opaque.
They must not be dereferenced by the client - only by the server.
MIDL does not have any in-built semantics for this, so we need to
define a small set of specific "remote handle" datatypes that can
be exchanged for pointers at the level of the server, perhaps
purely by casting them.
*/

typedef unsigned long  RNUBHANDLE;
typedef unsigned long  RNUB;
typedef unsigned long  RSERVER;
typedef unsigned long  RNUBPROCESS;
typedef unsigned long  RNUBTHREAD;
typedef unsigned long  RNUBLIBRARY;
typedef unsigned long  RTARGET_ADDRESS;

#define LOCALIZE_RNUBHANDLE(x) ((NUBHANDLE) (x))
#define REMOTIZE_NUBHANDLE(x) ((RNUBHANDLE) (x))
#define LOCALIZE_RNUB(x) ((NUB) (x))
#define REMOTIZE_NUB(x) ((RNUB) (x))
#define LOCALIZE_RSERVER(x) ((SERVER) (x))
#define REMOTIZE_SERVER(x) ((RSERVER) (x))
#define LOCALIZE_RNUBPROCESS(x) ((NUBPROCESS) (x))
#define REMOTIZE_NUBPROCESS(x) ((RNUBPROCESS) (x))
#define LOCALIZE_RNUBTHREAD(x) ((NUBTHREAD) (x))
#define REMOTIZE_NUBTHREAD(x) ((RNUBTHREAD) (x))
#define LOCALIZE_RNUBLIBRARY(x) ((NUBLIBRARY) (x))
#define REMOTIZE_NUBLIBRARY(x) ((RNUBLIBRARY) (x))
#define LOCALIZE_RTARGET_ADDRESS(x) ((TARGET_ADDRESS) (x))
#define REMOTIZE_TARGET_ADDRESS(x) ((RTARGET_ADDRESS) (x))
#define LOCALIZE_RTARGET_ADDRESS_ARRAY(x) ((TARGET_ADDRESS*) (x))
#define REMOTIZE_TARGET_ADDRESS_ARRAY(x) ((RTARGET_ADDRESS*) (x))

typedef unsigned long FLAG;

