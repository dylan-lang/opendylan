/* ************************************************************************* */
/* ** dylan-extensions.h                                                  ** */
/* ** A debugger nub extension unit that knows about Dylan code.          ** */
/* ** ------------------------------------------------------------------- ** */
/* ** Author: Paul Howard    Copyright: (c) 1996 Functional Objects, Inc. ** */
/* **                                   All Rights Reserved.              ** */
/* ************************************************************************* */


typedef struct _BIND_EXIT_FRAME {
  DWORD        MVPointer;
  DWORD        VectorWrapper;
  DWORD        VectorSize;
  DWORD        VectorContents[8];
  DWORD        StoredUnwindProtectFrame;
  DWORD        ParentFramePointer;
  DWORD        ContinuationAddress;
} BIND_EXIT_FRAME;

typedef struct _UNWIND_PROTECT_FRAME {
  DWORD        PreviousUnwindProtectFrame;
  DWORD        ParentFramePointer;
  DWORD        CleanupCodeAddress;
} UNWIND_PROTECT_FRAME;

typedef struct _DYLAN_CALLING_CONVENTION {
  int          FirstRegisterArg;
  int          LastRegisterArg;
  int          RegisterIndices[8];
  int          DylanFunctionRegister;
} DYLAN_CALLING_CONVENTION;

typedef struct _GENERIC_THREAD_LOCAL_STORAGE_AREA {

  /* Offset 0x00 */ DWORD  Offset00;
  /* Offset 0x04 */ DWORD  Offset04;
  /* Offset 0x08 */ DWORD  Offset08;
  /* Offset 0x0C */ DWORD  Offset0C;
  /* Offset 0x10 */ DWORD  Offset10;
  /* Offset 0x14 */ DWORD  ThreadEnvironmentBlock;

} GENERIC_THREAD_LOCAL_STORAGE_AREA;

typedef struct _DYLAN_THREAD_ENVIRONMENT_BLOCK {

  /* Offset 0x00 */ DWORD  DynamicEnvironment;
  /* Offset 0x04 */ DWORD  ThreadLocalVariablesVector;
  /* Offset 0x08 */ DWORD  DylanThreadObject;
  /* Offset 0x0C */ DWORD  Nothing;
  /* Offset 0x10 */ DWORD  CurrentHandlers;
  /* Offset 0x14 */ DWORD  Reserved1;
  /* Offset 0x18 */ DWORD  Reserved2;
  /* Offset 0x1C */ DWORD  Reserved3;
  /* Offset 0x20 */ DWORD  MVCount;
  /* Offset 0x24 */ DWORD  MVArea[64];

} DYLAN_THREAD_ENVIRONMENT_BLOCK;

