/* ********************************************************************** */
/* ** register_access.c                                                ** */
/* ** Describes the register set for the processor on which this       ** */
/* ** instance of the debugger nub is running, and implements access   ** */
/* ** to those registers.                                              ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

typedef struct _REGISTER_NAME {
  int            NameLength;
  char          *Name;
  int            CodeviewEnumeration;
} REGISTER_NAME;

static REGISTER_NAME register_names[] = {
  {16, "<Not a register>", 0},
  {3, "EAX", 17},
  {3, "EDX", 19},
  {3, "ECX", 18},
  {3, "EBX", 20},
  {3, "EBP", 22},
  {3, "ESI", 23},
  {3, "EDI", 24},
  {3, "ESP", 21},
  {3, "EIP", 33},
  {3, "EFL", 34},
  {2, "CS",  26},
  {2, "DS",  28},
  {2, "SS",  27},
  {2, "ES",  25},
  {2, "FS",  29},
  {2, "GS",  30},
  {6, "FLT(0)", 128},
  {6, "FLT(1)", 129},
  {6, "FLT(2)", 130},
  {6, "FLT(3)", 131},
  {6, "FLT(4)", 132},
  {6, "FLT(5)", 133},
  {6, "FLT(6)", 134},
  {6, "FLT(7)", 135}
};


// The following is taken from Chapter 6 of "Codeview Symbolic Debug Information
// Specification" (Microsoft Developer Network Library)

static NUB_INDEX register_codeview_indices[] =
{
  /* 000 */ NUB_REGISTER_ILLEGAL,
  /* 001 */ NUB_REGISTER_EAX,
  /* 002 */ NUB_REGISTER_ECX,
  /* 003 */ NUB_REGISTER_EDX,
  /* 004 */ NUB_REGISTER_EBX,
  /* 005 */ NUB_REGISTER_EAX,
  /* 006 */ NUB_REGISTER_ECX,
  /* 007 */ NUB_REGISTER_EDX,
  /* 008 */ NUB_REGISTER_EBX,
  /* 009 */ NUB_REGISTER_EAX,
  /* 010 */ NUB_REGISTER_ECX,
  /* 011 */ NUB_REGISTER_EDX,
  /* 012 */ NUB_REGISTER_EBX,
  /* 013 */ NUB_REGISTER_ESP,
  /* 014 */ NUB_REGISTER_EBP,
  /* 015 */ NUB_REGISTER_ESI,
  /* 016 */ NUB_REGISTER_EDI,
  /* 017 */ NUB_REGISTER_EAX,
  /* 018 */ NUB_REGISTER_ECX,
  /* 019 */ NUB_REGISTER_EDX,
  /* 020 */ NUB_REGISTER_EBX,
  /* 021 */ NUB_REGISTER_ESP,
  /* 022 */ NUB_REGISTER_EBP,
  /* 023 */ NUB_REGISTER_ESI,
  /* 024 */ NUB_REGISTER_EDI,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL,
  NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL, NUB_REGISTER_ILLEGAL
};


void nub_all_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  (*first) = (NUB_INDEX) 1;
  (*last) = (NUB_INDEX) 24;
}


void nub_general_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  (*first) = (NUB_INDEX) 1;
  (*last) = (NUB_INDEX) 7;
}


void nub_special_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  (*first) = (NUB_INDEX) 8;
  (*last) = (NUB_INDEX) 16;
}


void nub_floating_registers
  (NUB nub, NUB_INDEX *first, NUB_INDEX *last)
{
  (*first) = (NUB_INDEX) 17;
  (*last) = (NUB_INDEX) 24;
}


NUB_INDEX map_cv_register_index (BYTE index)
{
  if ((index > 0) && (index < 25))
    return (register_codeview_indices[index]);
  else
    return (NUB_REGISTER_EAX);
}


void register_index_from_codeview_enumerate
  (WORD cvr, NUB_INDEX *hireg, NUB_INDEX *loreg)
{
  BYTE   hi_index = (BYTE) (cvr >> 8);
  BYTE   lo_index = (BYTE) (cvr & 0x00ff);

  (*hireg) = map_cv_register_index(hi_index);
  (*loreg) = map_cv_register_index(lo_index);
}


// Accessors for registers.


NUBINT nub_get_register_enumeration_code
  (NUB nub, NUB_INDEX i)
{
  return(register_names[(int) i].CodeviewEnumeration);
}


NUBINT nub_get_register_name_length
  (NUB nub, NUB_INDEX i)
{
  return ((NUBINT) register_names[(int) i].NameLength);
}


void nub_get_register_name
  (NUB nub, NUB_INDEX i, NUBINT buf_size, char *buf)
{
  char  *name = register_names[(int) i].Name;
  int   limit = (int) buf_size;
  int   j = 0;

  while ((j < limit) && (name[j] != '\0')) {
    buf[j] = name[j];
    j++;
  }

  if (j < limit) buf[j] = '\0';
}


// Headers for sign conversion functions in memory_access.c



// Floating-point register conversion functions.

void extended_float_to_single_float 
  (void *ext_float,
   FLOAT *single_float);

void extended_float_to_double_float 
  (void *ext_float,
   DOUBLE *double_float);

void single_float_to_extended_float 
  (FLOAT *single_float,
   void *ext_float);

void double_float_to_extended_float 
  (DOUBLE *double_float,
   void *ext_float);


void extended_float_to_single_float 
  (void *ext_float,
   FLOAT *single_float)
{
  __asm
  {
    mov        eax, ext_float
    fld        TBYTE PTR [eax]           ; Load the extended arg
    mov        eax, single_float
    fstp       DWORD PTR [eax]           ; Store as single.
  }
}

void extended_float_to_double_float 
  (void *ext_float,
   DOUBLE *double_float)
{
  __asm
  {
    mov         eax, ext_float
    fld         TBYTE PTR [eax]          ; Load the extended arg
    mov         eax, double_float
    fstp        QWORD PTR [eax]          ; Store as double precision.
  }
}


void single_float_to_extended_float 
  (FLOAT *single_float,
   void *ext_float)
{
  __asm
  {
    mov        eax, single_float
    fld        DWORD PTR [eax]           ; Load the single
    mov        eax, ext_float
    fstp       TBYTE PTR [eax]           ; Store as extended.
  }
}
 

void double_float_to_extended_float 
  (DOUBLE *double_float,
   void *ext_float)
{
  __asm
  {
    mov        eax, double_float
    fld        QWORD PTR [eax]           ; Load the double
    mov        eax, ext_float
    fstp       TBYTE PTR [eax]           ; Store as extended.
  }
}


// RAW REGISTER TRANSACTIONS
// The debugger nub register access functions are written in terms of
// these...

DWORD register_get32
  (LPDBGPROCESS process, 
   LPDBGTHREAD thread, 
   NUB_INDEX reg, 
   NUB_ERROR *error_code)
{
  CONTEXT           context;
  BOOL              status;

  context.ContextFlags = CONTEXT_FULL | CONTEXT_FLOATING_POINT;
  status = GetThreadContext(thread->ThreadHandle, &context);

  if (!status) {
    (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
    return ((DWORD) 0);
  }
  else {
    switch (reg) {
    case NUB_REGISTER_EAX:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Eax);
      break;

    case NUB_REGISTER_EDX:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Edx);
      break;

    case NUB_REGISTER_ECX:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Ecx);
      break;

    case NUB_REGISTER_EBX:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Ebx);
      break;

    case NUB_REGISTER_EBP:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Ebp);
      break;

    case NUB_REGISTER_ESI:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Esi);
      break;

    case NUB_REGISTER_EDI:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Edi);
      break;

    case NUB_REGISTER_ESP:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Esp);
      break;

    case NUB_REGISTER_EIP:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.Eip);
      break;

    case NUB_REGISTER_EFL:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      return (context.EFlags);
      break;

    case NUB_REGISTER_CS:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
       return ((DWORD) context.SegCs);
       break;

    case NUB_REGISTER_DS:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
       return ((DWORD) context.SegCs);
       break;

    case NUB_REGISTER_SS:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
       return ((DWORD) context.SegSs);
       break;

    case NUB_REGISTER_ES:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
       return ((DWORD) context.SegEs);
       break;

    case NUB_REGISTER_FS:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
       return ((DWORD) context.SegFs);
       break;

    case NUB_REGISTER_GS:
      (*error_code) = (NUB_ERROR) ACCESS_OK;
       return ((DWORD) context.SegGs);
       break;

    case NUB_REGISTER_FLT0:
    case NUB_REGISTER_FLT1:
    case NUB_REGISTER_FLT2:
    case NUB_REGISTER_FLT3:
    case NUB_REGISTER_FLT4:
    case NUB_REGISTER_FLT5:
    case NUB_REGISTER_FLT6:
    case NUB_REGISTER_FLT7:
      // Can only do float transactions to these registers
      (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
      return ((DWORD) 0);
      break;

    }
  }
}


void register_put32
  (LPDBGPROCESS process, 
   LPDBGTHREAD thread, NUB_INDEX reg, 
   DWORD value, NUB_ERROR *error_code)
{
  CONTEXT           context;
  BOOL              status;

  context.ContextFlags = CONTEXT_FULL | CONTEXT_FLOATING_POINT;
  status = GetThreadContext(thread->ThreadHandle, &context);

  if (!status) {
    (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
  }
  else {
    switch (reg) {
    case NUB_REGISTER_EAX:
      context.Eax = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_EDX:
      context.Edx = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_ECX:
      context.Ecx = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_EBX:
      context.Ebx = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_EBP:
      context.Ebp = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_ESI:
      context.Esi = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_EDI:
      context.Edi = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_ESP:
      context.Esp = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_EIP:
      context.Eip = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_EFL:
      context.EFlags = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_CS:
      context.SegCs = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_DS:
      context.SegDs = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_SS:
      context.SegSs = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_ES:
      context.SegEs = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FS:
      context.SegFs = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_GS:
      context.SegGs = value;
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT0:
    case NUB_REGISTER_FLT1:
    case NUB_REGISTER_FLT2:
    case NUB_REGISTER_FLT3:
    case NUB_REGISTER_FLT4:
    case NUB_REGISTER_FLT5:
    case NUB_REGISTER_FLT6:
    case NUB_REGISTER_FLT7:
      (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
      break;
    }
    status = SetThreadContext(thread->ThreadHandle, &context);
    if (!status)
      (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
  }
}


void read_nth_float (BYTE *area_base, BYTE *value, BYTE n)
{
  BYTE     i;
  for (i = 0; i < 10; i++) value[i] = area_base[(n * 10) + i];
}

void store_nth_float (BYTE *area_base, BYTE *value, BYTE n)
{
  BYTE     i;
  for (i = 0; i < 10; i++) area_base[(n * 10) + i] = value[i];
}

void register_getfloat
  (LPDBGPROCESS process, 
   LPDBGTHREAD thread, NUB_INDEX reg, 
   void* value, NUB_ERROR *error_code)
{
  CONTEXT           context;
  BOOL              status;

  context.ContextFlags = CONTEXT_FULL | CONTEXT_FLOATING_POINT;
  status = GetThreadContext(thread->ThreadHandle, &context);

  if (!status) {
    (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
  }
  else {
    switch (reg) {
    case NUB_REGISTER_EAX:
    case NUB_REGISTER_EDX:
    case NUB_REGISTER_ECX:
    case NUB_REGISTER_EBX:
    case NUB_REGISTER_EBP:
    case NUB_REGISTER_ESI:
    case NUB_REGISTER_EDI:
    case NUB_REGISTER_ESP:
    case NUB_REGISTER_EIP:
    case NUB_REGISTER_EFL:
    case NUB_REGISTER_CS:
    case NUB_REGISTER_DS:
    case NUB_REGISTER_SS:
    case NUB_REGISTER_ES:
    case NUB_REGISTER_FS:
    case NUB_REGISTER_GS:
      (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
      return;

    case NUB_REGISTER_FLT0:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 0);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT1:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 1);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT2:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 2);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT3:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 3);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT4:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 4);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT5:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 5);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT6:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 6);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT7:
      read_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 7);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;
    }
  }
}


void register_putfloat
  (LPDBGPROCESS process, 
   LPDBGTHREAD thread, NUB_INDEX reg, 
   void* value, NUB_ERROR *error_code)
{
  CONTEXT           context;
  BOOL              status;

  context.ContextFlags = CONTEXT_FULL | CONTEXT_FLOATING_POINT;
  status = GetThreadContext(thread->ThreadHandle, &context);

  if (!status) {
    (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
  }
  else {
    switch (reg) {
    case NUB_REGISTER_EAX:
    case NUB_REGISTER_EDX:
    case NUB_REGISTER_ECX:
    case NUB_REGISTER_EBX:
    case NUB_REGISTER_EBP:
    case NUB_REGISTER_ESI:
    case NUB_REGISTER_EDI:
    case NUB_REGISTER_ESP:
    case NUB_REGISTER_EIP:
    case NUB_REGISTER_EFL:
    case NUB_REGISTER_CS:
    case NUB_REGISTER_DS:
    case NUB_REGISTER_SS:
    case NUB_REGISTER_ES:
    case NUB_REGISTER_FS:
    case NUB_REGISTER_GS:
      (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
      return;

    case NUB_REGISTER_FLT0:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 0);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT1:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 1);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT2:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 2);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT3:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 3);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT4:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 4);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT5:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 5);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT6:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 6);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;

    case NUB_REGISTER_FLT7:
      store_nth_float(context.FloatSave.RegisterArea, (BYTE*) value, 7);
      (*error_code) = (NUB_ERROR) ACCESS_OK;
      break;
    }
    status = SetThreadContext(thread->ThreadHandle, &context);
    if (!status)
      (*error_code) = (NUB_ERROR) ACCESS_VIOLATION_ERROR;
  }
}


TARGET_ADDRESS read_value_from_process_register
  (NUB nub, NUBTHREAD thread, NUB_INDEX reg, NUB_ERROR *error)
{
  TARGET_ADDRESS result;
  (*error) = (NUB_ERROR) 0;
  result = (TARGET_ADDRESS)
    register_get32((LPDBGPROCESS) nub, (LPDBGTHREAD) thread, reg, error);
  return (result);
}


TARGET_ADDRESS read_value_from_process_register_in_stack_frame
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX reg, NUB_INDEX stackframe,
   NUB_ERROR *error)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    thread = (LPDBGTHREAD) nubthread;
  TARGET_ADDRESS result;
  
  (*error) = ACCESS_OK;

  switch (reg) {

  case NUB_REGISTER_EBP:
    result = (TARGET_ADDRESS) thread->StackTraceFramePointers[stackframe];
    break;

  case NUB_REGISTER_ESP:
    result = (TARGET_ADDRESS) thread->StackTraceStackPointers[stackframe];
    break;

  case NUB_REGISTER_EIP:
    result = (TARGET_ADDRESS) thread->StackTraceInstrPointers[stackframe];
    break;

  default:
    result = read_value_from_process_register(nub, nubthread, reg, error);
    break;
  }
  return(result);  
}


void write_value_to_process_register
  (NUB nub, NUBTHREAD thread, NUB_INDEX reg, 
   TARGET_ADDRESS val, NUB_ERROR *error)
{
  register_put32((LPDBGPROCESS) nub,
                 (LPDBGTHREAD) thread,
                 reg,
                 (DWORD) val,
                 error);
}


FLOAT read_single_float_from_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX reg, NUB_ERROR *error)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    thread = (LPDBGTHREAD) nubthread;
  BYTE           holder[10];
  FLOAT          value;

  register_getfloat(process, thread, reg, (void*) holder, error);
  extended_float_to_single_float((void*) holder, &value);
  return(value);
}


void write_single_float_to_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX reg, FLOAT value, NUB_ERROR *error)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    thread = (LPDBGTHREAD) nubthread;
  BYTE           holder[10];

  single_float_to_extended_float(&value, (void*) holder);
  register_putfloat(process, thread, reg, (void*) holder, error);
}

DOUBLE read_double_float_from_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX reg, NUB_ERROR *error)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    thread = (LPDBGTHREAD) nubthread;
  BYTE           holder[10];
  DOUBLE         value;

  register_getfloat(process, thread, reg, (void*) holder, error);
  extended_float_to_double_float((void*) holder, &value);
  return(value);
}


void write_double_float_to_process_register
  (NUB nub, NUBTHREAD nubthread, NUB_INDEX reg, DOUBLE value, 
   NUB_ERROR *error)
{
  LPDBGPROCESS   process = (LPDBGPROCESS) nub;
  LPDBGTHREAD    thread = (LPDBGTHREAD) nubthread;
  BYTE           holder[10];

  double_float_to_extended_float(&value, (void*) holder);
  register_putfloat(process, thread, reg, (void*) holder, error);
}
