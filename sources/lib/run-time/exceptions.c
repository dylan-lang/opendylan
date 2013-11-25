#include <assert.h>
#include <signal.h>
#include "mm.h"

#if defined(OPEN_DYLAN_PLATFORM_LINUX) && defined(OPEN_DYLAN_ARCH_X86)
#include "x86-linux-exceptions.c"
#elif defined(OPEN_DYLAN_PLATFORM_FREEBSD) && defined(OPEN_DYLAN_ARCH_X86)
#include "x86-freebsd-exceptions.c"
#elif defined(OPEN_DYLAN_PLATFORM_DARWIN) && defined(OPEN_DYLAN_ARCH_X86)
#include "x86-darwin-exceptions.c"
#elif defined(OPEN_DYLAN_PLATFORM_WINDOWS) && defined(OPEN_DYLAN_ARCH_X86)
#include "x86-windows-exceptions.c"
#else
#define EXCEPTION_PREAMBLE()
#define EXCEPTION_POSTAMBLE()
#endif

#if defined(GC_USE_BOEHM) || defined(GC_USE_MALLOC)
#undef mps_tramp /* Override generic version */

typedef void *(*mps_tramp_t)(void *, size_t);

#define mps_tramp(r_o, f, p, s) \
    { \
    void **_r_o = (r_o); \
    mps_tramp_t _f = (f); \
    void *_p = (p); \
    size_t _s = (s); \
    *_r_o = (*_f)(_p, _s); \
    }
#endif

/* Support for foreign call-ins */
extern void *dylan_callin_internal(void *arg_base, size_t s);


MMError dylan_init_thread(void **rReturn, void *(*f)(void *, size_t), void *p, size_t s)
{
  EXCEPTION_PREAMBLE()

  gc_teb_t gc_teb = current_gc_teb();

  gc_teb->gc_teb_inside_tramp = 1;

  /* Go for it! */
  mps_tramp(rReturn, f, p, s);

  gc_teb->gc_teb_inside_tramp = 0;

  EXCEPTION_POSTAMBLE()

  return MMSUCCESS;
}


void *dylan_callin_handler(void *arg_base, size_t s)
{
  void *res;

  EXCEPTION_PREAMBLE()

  gc_teb_t gc_teb = current_gc_teb();

  int was_inside = gc_teb->gc_teb_inside_tramp;
  gc_teb->gc_teb_inside_tramp = 1;

  /* Go for it! */
  mps_tramp(&res, dylan_callin_internal, arg_base, s);

  gc_teb->gc_teb_inside_tramp = was_inside;

  EXCEPTION_POSTAMBLE()

  return res;
}

