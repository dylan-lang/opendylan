/* ********************************************************************* */
/* ** windows-spy-interfaces.c                                        ** */
/* ** Implementations of C Spy functions, callable by the debugger.   ** */
/* ** --------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                             ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                    ** */
/* **            All Rights Reserved                                  ** */
/* ********************************************************************* */

#include <windows.h>
#include "spy-interfaces.h"

SPY_INTERFACE int spy_load_extension_component (char *name)
{
  HANDLE  extension = LoadLibrary(name);
  if (extension == NULL)
    return(SPY_LOAD_EXTENSION_COMPONENT_FAILED);
  else
    return(SPY_LOAD_EXTENSION_COMPONENT_SUCCEEDED);
}
