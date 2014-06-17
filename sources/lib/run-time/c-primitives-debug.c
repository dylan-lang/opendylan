#include "run-time.h"
#include <stdio.h>

extern dylan_object KPfalseVKi;
extern dylan_object KPtrueVKi;

extern void dylan_format (char*, D, D);

static void do_debug_message (D string, D arguments) {
  char error_output[8192];
  error_output[0] = 0;

  dylan_format(error_output, string, arguments);
#ifdef OPEN_DYLAN_PLATFORM_WINDOWS
  {
    #define $STD_OUTPUT_HANDLE (unsigned long)-11
    #define $INVALID_HANDLE_VALUE (void*)-1
    extern void* __stdcall GetStdHandle(unsigned long);
    extern BOOL __stdcall WriteFile(void*, char*, unsigned long, unsigned long*, void*);
    extern void __stdcall OutputDebugStringA(char*);
    void* stdoutHandle = GetStdHandle($STD_OUTPUT_HANDLE);
    put_char('\n', error_output);
    if ((stdoutHandle != $INVALID_HANDLE_VALUE) && (stdoutHandle != (void*)0)) {
      unsigned long nBytes = strlen(error_output);
      WriteFile(stdoutHandle, error_output, nBytes, &nBytes, (void*)0);
    }
    OutputDebugStringA(error_output);
  }
#else
  fputs(error_output, stderr);
  fputs("\n", stderr); /* Adds a terminating newline */
  fflush(stderr);
#endif
}

void primitive_invoke_debugger (D string, D arguments) {
  do_debug_message(string, arguments);
  primitive_break();
}

D primitive_inside_debuggerQ (void) {
  return DFALSE;
}

void primitive_debug_message (D string, D arguments) {
  do_debug_message(string, arguments);
}

