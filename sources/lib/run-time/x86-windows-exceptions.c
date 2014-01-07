/* Windows exception handling */

/* Support for handling exceptions in Dylan (other than MM traps) */
/* Currently, we just handle stack overflows arithmetic exceptions  */

extern int inside_dylan_ffi_barrier();
extern void dylan_stack_overflow_handler(PVOID base_address, int size, DWORD protection);
extern void dylan_integer_overflow_handler();
extern void dylan_integer_divide_0_handler();
extern void dylan_float_divide_0_handler();
extern void dylan_float_invalid_handler();
extern void dylan_float_overflow_handler();
extern void dylan_float_underflow_handler();


PVOID current_stack_pointer ()
{
  PVOID stack_ptr;
  __asm
  {
    mov  stack_ptr, esp
  };
  return(stack_ptr);
}


#define VPAGESIZE 0x1000

void call_dylan_stack_overflow_handler ()
{
  MEMORY_BASIC_INFORMATION memBuf;
  PVOID stack_ptr = current_stack_pointer();
  int res = VirtualQuery(stack_ptr, &memBuf, sizeof(memBuf));

  PVOID baseAddress    = memBuf.BaseAddress;     // base address of region
  PVOID allocationBase = memBuf.AllocationBase;  // allocation base address
  DWORD protect        = memBuf.Protect;         // current access protection

  dylan_stack_overflow_handler(baseAddress, VPAGESIZE, PAGE_GUARD + protect);

}


/* Establish the stack overflow filter outside the MPS handler because
   it has less requirement for efficiency */
#define EXCEPTION_PREAMBLE() \
  __try {

#define EXCEPTION_POSTAMBLE() \
  } \
  __except (DylanExceptionFilter(GetExceptionInformation())) { }


LONG DylanExceptionFilter (LPEXCEPTION_POINTERS info)
{

  LPEXCEPTION_RECORD er = info->ExceptionRecord;

  if (inside_dylan_ffi_barrier() == 0) {
    return(EXCEPTION_CONTINUE_SEARCH);
  }

  switch (er->ExceptionCode)
  {
  case EXCEPTION_STACK_OVERFLOW:
    {
      // On a stack overflow, the filter calls into Dylan to signal
      // an error, via dylan_signal_overflow_handler. The dylan
      // code will arrange to re-establish the guard protection on
      // the appropriate page of the stack (probably during the
      // rewind when recovering from the error). Before calling the
      // handler, we do a check to ensure that there is sufficient
      // spare stack space after the guard to allow the handler itself
      // to run.

      MEMORY_BASIC_INFORMATION memBuf;
      PVOID stack_ptr = current_stack_pointer();
      int res = VirtualQuery(stack_ptr, &memBuf, sizeof(memBuf));

      PVOID baseAddress    = memBuf.BaseAddress;    // base address of region
      PVOID allocationBase = memBuf.AllocationBase; // allocation base addr

      if (((int)baseAddress - (int)allocationBase) >= (2 * VPAGESIZE)) {
        // There's enough space past the guard to invoke the Dylan handler.
        // Rather than attempt a long-jump within the filter (by simply
        // calling the Dylan handler) we destructively modify the execution
        // context, so that when Windows continues from the exception, it
        // actually continues in the Dylan handler calling code instead.
        // This handler will never return - instead it will ultimately NLX

        info->ContextRecord->Eip = (unsigned long) &call_dylan_stack_overflow_handler;
        return(EXCEPTION_CONTINUE_EXECUTION);
      } else {
        return(EXCEPTION_CONTINUE_SEARCH);
      }
    }
  case EXCEPTION_INT_OVERFLOW:
    { info->ContextRecord->Eip = (unsigned long) &dylan_integer_overflow_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_INT_DIVIDE_BY_ZERO:
    { info->ContextRecord->Eip = (unsigned long) &dylan_integer_divide_0_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_FLT_DIVIDE_BY_ZERO:
    { info->ContextRecord->Eip = (unsigned long) &dylan_float_divide_0_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_FLT_INVALID_OPERATION:
    { info->ContextRecord->Eip = (unsigned long) &dylan_float_invalid_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_FLT_OVERFLOW:
    { info->ContextRecord->Eip = (unsigned long) &dylan_float_overflow_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  case EXCEPTION_FLT_UNDERFLOW:
    { info->ContextRecord->Eip = (unsigned long) &dylan_float_underflow_handler;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
  /*
  case  DBG_CONTROL_C:
    { dylan_keyboard_interruptQ = TRUE;
      return(EXCEPTION_CONTINUE_EXECUTION);
    }
   */

  default:
    return(EXCEPTION_CONTINUE_SEARCH);
  }
}
