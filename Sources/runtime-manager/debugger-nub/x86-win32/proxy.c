/* ************************************************************************ */
/* ** proxy.c                                                            ** */
/* ** The API for the proxy debugger nub, linked to the environment      ** */
/* ** along with the local debugger nub.  (All versions).                ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard, Copyright: (c) 1996 Functional Objects, Inc.   ** */
/* **                                 All Rights Reserved                ** */
/* ************************************************************************ */

#include "nub-core.h"

NUB create_and_debug_process 
  (SERVER dummy, 
   char *module_name, 
   char *arguments,
   NUBINT path_count, char **paths,
   NUBINT lib_path_count, char **lib_paths,
   char *workdir,
   NUBINT create_shell);

NUB debug_active_process
  (SERVER dummy,
   NUBPROCESS nubprocess,
   NUBINT symbol_path_count,
   char **symbol_paths,
   char *system_JIT_information);

/*
  The following three functions ALWAYS run in the local debugger nub, and
  are hence part of the "proxy" file.
*/


NUB open_local_tether
  (char *command, char *arguments, 
   NUBINT sym_path_count, char **paths,
   NUBINT lib_path_count, char **lib_paths,
   char *working_directory,
   NUBINT create_shell, NUBINT *success)
{
  NUB tether = create_and_debug_process 
                 (0, 
                  command, 
                  arguments, 
                  sym_path_count, paths, 
                  lib_path_count, lib_paths,
                  working_directory,
                  create_shell);

  if (tether == NULL) {
    (*success) = (NUBINT) 0;
    return (NULL);
  }
  else {
    (*success) = (NUBINT) 1;
    return (tether);
  }
}


NUB attach_local_tether
  (NUBPROCESS process,
   NUBINT symbol_path_count, 
   char **symbol_paths,
   char *system_JIT_information,
   NUBINT *success)
{
  NUB  tether = debug_active_process
                   ((SERVER) 0, 
                     process, 
                     symbol_path_count, 
                     symbol_paths,
                     system_JIT_information);

  if (tether == NULL) {
    (*success) = (NUBINT) 0;
    return (NULL);
  }
  else {
    (*success) = (NUBINT) 1;
    return (tether);
  }
}

PROCESSD  global_attachment_descriptor;

NUB attach_remote_tether
  (NUBPROCESS process,
   NUBINT name_length, unsigned char *name_string,
   NUBINT id_length, unsigned char *id_string,
   unsigned long actual_id,
   NUBINT symbol_path_count, 
   char **symbol_paths,
   char *system_JIT_information,
   NUBINT *success)
{
  int i;

  /* Fill in the Global Attachment Descriptor. */

  global_attachment_descriptor.ProcessNameLength = name_length;
  global_attachment_descriptor.ProcessIDLength = id_length;
  for (i = 0; i < name_length; i++)
    global_attachment_descriptor.ProcessName[i] = name_string[i];
  for (i = 0; i < id_length; i++)
    global_attachment_descriptor.ProcessID[i] = id_string[i];
  global_attachment_descriptor.ActualProcessID = (DWORD) actual_id;

  return
    (attach_local_tether
     ((NUBPROCESS) &global_attachment_descriptor,
      symbol_path_count, symbol_paths,
      system_JIT_information, success));

}
