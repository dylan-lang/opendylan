/* ********************************************************************* */
/* ** spy-interfaces.h                                                ** */
/* ** Prototypes for C Spy functions, callable by the debugger.       ** */
/* ** --------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                             ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                    ** */
/* **            All Rights Reserved                                  ** */
/* ********************************************************************* */

#define SPY_INTERFACE

/*
  NB: The following constants must match the definitions of
     $spy-load-extension-component-failed AND
     $spy-load-extension-component-succeeded
  in D-runtime-manager-debugger-manager!spy-catalogue.dylan
*/

#define SPY_LOAD_EXTENSION_COMPONENT_FAILED 0
#define SPY_LOAD_EXTENSION_COMPONENT_SUCCEEDED 1

SPY_INTERFACE int spy_load_extension_component (char *name);
/* Loads a named DLL into the runtime */

