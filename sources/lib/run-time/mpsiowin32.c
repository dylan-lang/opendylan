/* impl.c.mpsiowin32: HARLEQUIN MEMORY POOL SYSTEM I/O IMPLEMENTATION (WIN32)
 *
 * Copyright (C) 1997 Functional Objects, Inc. All rights reserved.
 *
 */

#include <windows.h>

#include "mpsio.h"

HANDLE ioFile = NULL;

mps_res_t mps_io_create(mps_io_t *mps_io_r)
{
  HANDLE f;

  if(ioFile != NULL) /* See impl.c.event.trans.log */
    return MPS_RES_LIMIT; /* Cannot currently open more than one log */

  f = CreateFile("mpsio.log", GENERIC_WRITE, 0, 0, OPEN_ALWAYS, 0, 0);
  if(f == INVALID_HANDLE_VALUE)
    return MPS_RES_IO;
  
  *mps_io_r = (mps_io_t)f;
  ioFile = f;
  return MPS_RES_OK;
}

void mps_io_destroy(mps_io_t mps_io)
{
  HANDLE f = (HANDLE)mps_io;
  ioFile = NULL; /* Should check f == ioFile */
  (void)CloseHandle(f);
}

mps_res_t mps_io_write(mps_io_t mps_io, void *mps_buf, size_t mps_size)
{
  HANDLE f = (HANDLE)mps_io; /* Should check f == ioFile */
  size_t n;

  BOOL res = WriteFile(f, mps_buf, mps_size, &n, 0);
  if(res == 0 || (n != mps_size))
    return MPS_RES_IO;
  
  return MPS_RES_OK;
}

mps_res_t mps_io_flush(mps_io_t mps_io)
{
  return MPS_RES_OK;
}


