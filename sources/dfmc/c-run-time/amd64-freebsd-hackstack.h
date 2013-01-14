#include <sys/mman.h>
#include <setjmp.h>
#include <gc/gc.h>

#define freebsd_hack_stack(orig, fn)		     \
void orig ()					     \
{						     \
    void fn();					     \
    static char *stack = NULL;			     \
    enum {stack_size = 16 * 1024 * 1024};	     \
    static jmp_buf env;				     \
    char *tos;					     \
    char *saved_GC_stackbottom;			     \
						     \
    stack = mmap(NULL, stack_size + getpagesize(),   \
		 PROT_READ | PROT_WRITE,	     \
		 MAP_ANON, -1, 0);		     \
    if (stack == NULL)				     \
      abort();					     \
						     \
    mprotect(stack, getpagesize(), PROT_NONE);	     \
    tos = stack + stack_size + getpagesize();	     \
						     \
    if (setjmp(env) == 0) {			     \
    __asm__("movq %%rax, %%rsp"			     \
	    ::"a"(tos));			     \
    saved_GC_stackbottom = GC_stackbottom;	     \
    GC_stackbottom = tos;			     \
    fn ();					     \
    longjmp(env, 1);				     \
    } else {					     \
    GC_stackbottom = saved_GC_stackbottom;	     \
    munmap(stack, stack_size + getpagesize());	     \
    stack = NULL;				     \
    }						     \
}

#include FREEBSD_HACKSTACK
