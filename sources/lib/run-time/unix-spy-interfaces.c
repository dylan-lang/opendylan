/* ********************************************************************* */
/* ** spy-interfaces.c                                                ** */
/* ** Implementations of C Spy functions, callable by the debugger.   ** */
/* ** --------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                             ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                    ** */
/* **            All Rights Reserved                                  ** */
/* ********************************************************************* */

#define _GNU_SOURCE

#include <stdlib.h>
#include <dlfcn.h>
#include "unix-types.h"
#include "spy-interfaces.h"


SPY_INTERFACE int spy_load_extension_component (char *name)
{
  void * extension = dlopen(name, RTLD_NOW | RTLD_GLOBAL);
  if (extension == NULL) {
    return(SPY_LOAD_EXTENSION_COMPONENT_FAILED);
  } else {
    return(SPY_LOAD_EXTENSION_COMPONENT_SUCCEEDED);
  }
}

