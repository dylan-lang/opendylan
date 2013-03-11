/* impl.h.mpsw3: HARLEQUIN MEMORY POOL SYSTEM C INTERFACE, WINDOWS PART
 *
 * Copyright (C) 1998 Functional Objects, Inc.  All rights reserved.
 *
 * .readership: customers, MPS developers.
 * .sources: design.mps.interface.c.
 */


#undef mps_tramp /* Override generic version */

typedef void *(*mps_tramp_t)(void *, size_t);

#define mps_tramp(r_o, f, p, s) \
    { \
    void **_r_o = (r_o); \
    mps_tramp_t _f = (f); \
    void *_p = (p); \
    size_t _s = (s); \
    *_r_o = (*_f)(_p, _s); \
    } \


