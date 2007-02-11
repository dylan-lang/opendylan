#include <stdio.h>
#define __USE_GNU
#include <dlfcn.h>

int getebp () {
    int ebp;
    asm("mov (%%ebp), %0"
        :"=r"(ebp));
    return ebp;
};

void walkstack() {
  int ebp = getebp();
  int eip;
  int rc;
  Dl_info info;

  while(ebp) {
    eip = *((int*)ebp + 1);
    rc = dladdr((void*)eip, &info);
    if (!rc||(!info.dli_sname && !info.dli_fname)) {
      printf("0x%x (unknown)\n");
    } else {
      if (!info.dli_sname) {
        printf("0x%x (%s)\n", eip, info.dli_fname);
      } else {
        printf("%s+%i (%s)\n",
               info.dli_sname,
	       eip - (int)info.dli_saddr,
	       info.dli_fname);
      }
    }
    ebp = *((int*)ebp);
  }
}
