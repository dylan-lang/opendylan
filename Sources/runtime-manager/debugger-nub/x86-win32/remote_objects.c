/* ********************************************************************** */
/* ** remote_objects.c                                                 ** */
/* ** Functions for describing remote objects to the access path.      ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved.             ** */
/* ********************************************************************** */

#include "nub-core.h"

// TODO:
// I think nub_all_libraries and nub_all_threads are fast becoming
// redundant. The access-path implementation doesn't actually make
// use of them any more!

void nub_all_libraries 
  (NUB nub, 
   NUB_INDEX *first, 
   NUB_INDEX *last)
{
  LPDBGPROCESS    process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY    this_library = process->LibraryList;
  LPDBGLIBRARY    last_library = process->LibraryList;
  int             count;

  // Clean up the nub's list of libraries, to filter out DLL images
  // that have been unloaded since the last call.

  while (this_library != NULL) {

    if (!this_library->Valid) {
      // Drop a node out of the list
      last_library->Next = this_library->Next;
      free(this_library);
      this_library = last_library->Next;
    }
    else {
      // This one lives on.
      last_library = this_library;
      this_library = this_library->Next;
    }
  }

  this_library = process->LibraryList;

  (*first) = (NUB_INDEX) 1;
  count = 0;

  while (this_library != NULL) {

    if (!this_library->Valid) {
      // Internal error case
    }

    count++;
    this_library = this_library->Next;
  }
  (*last) = (NUB_INDEX) (count);
}

void nub_all_library_sections
  (NUB nub, NUBLIBRARY library, NUB_INDEX *first, NUB_INDEX *last)
{
}

NUBINT nub_get_library_section_name_length
  (NUB nub, NUBLIBRARY library, NUB_INDEX index)
{
  return(0);			/* TEMPORARY? */
}

void nub_get_library_section_details
  (NUB nub, NUBLIBRARY library, NUB_INDEX index,
   TARGET_ADDRESS *section_start, TARGET_ADDRESS *section_end)
{
}

void nub_get_library_section_name
  (NUB nub, NUBLIBRARY library, NUB_INDEX index, NUBINT buf_size, char *buf)
{
}

TARGET_ADDRESS nub_get_library_base_address
  (NUB nub, NUBLIBRARY library)
{
  LPDBGPROCESS           process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY           module = (LPDBGLIBRARY) library;

  return((TARGET_ADDRESS) module->ImageInformation.ImageBase);
}

void nub_get_library_version 
  (NUB nub, 
   NUBLIBRARY library, 
   NUBINT *major, 
   NUBINT *minor)
{
  LPDBGPROCESS            process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY            module = (LPDBGLIBRARY) library;

  (*major) = 
    (NUBINT) 
      (module->ImageInformation.OptionalHeader.WindowsNTFields.UserMajor);

  (*minor) = 
    (NUBINT) 
      (module->ImageInformation.OptionalHeader.WindowsNTFields.UserMinor);

}


NUBINT nub_get_library_undecorated_name_length
    (NUB nub, NUBLIBRARY library)
{
  LPDBGPROCESS             process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY             module = (LPDBGLIBRARY) library;
  BYTE                     i = 0;
  BYTE                     j = 0;
  char                    *name;

  if (module->DefaultImageName[0] != '\0') {
    name = module->DefaultImageName;
    while(name[j + 1] != '\0') j++;
    while((j > 0) && (name[j - 1] != '\\') && (name[j - 1] != '/')) j--;
    while((name[j] != '.') && (name[j] != '\0')) {
      j++;
      i++;
    }
    return((NUBINT) i);
  }
  else if (module->DebugMap != NULL) {
    name = (char*) module->DebugMap->ImageFileName;
    while(name[j + 1] != '\0') j++;
    while((j > 0) && (name[j - 1] != '\\') && (name[j - 1] != '/')) j--;
    while((name[j] != '.') && (name[j] != '\0')) {
      j++;
      i++;
    }
    return((NUBINT) i);
  }
  else if ((module->SymbolHandlerWorking) &&
           (module->ImagehlpModuleStruct.ModuleName[0] != '\0')) {
    name = module->ImagehlpModuleStruct.ModuleName;
    while(name[i] != '\0') i++;
    return((NUBINT) i);
  }
  else {
    return((NUBINT) 7);
  }
}


void nub_get_library_undecorated_name
  (NUB nub, NUBLIBRARY library, NUBINT buf_size, char *buf)
{
  LPDBGPROCESS             process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY             module = (LPDBGLIBRARY) library;
  BYTE                     i = 0;
  BYTE                     j = 0;
  char                    *name;

  if (module->DefaultImageName[0] != '\0') {
    name = module->DefaultImageName;
    while(name[j + 1] != '\0') j++;
    while((j > 0) && (name[j - 1] != '\\') && (name[j - 1] != '/')) j--;
    while((name[j] != '.') && (name[j] != '\0')) {
      buf[i] = name[j];
      j++;
      i++;
    }
  }
  else if (module->DebugMap != NULL) {
    name = (char*) module->DebugMap->ImageFileName;
    while(name[j + 1] != '\0') j++;
    while((j > 0) && (name[j - 1] != '\\') && (name[j - 1] != '/')) j--;
    while((name[j] != '.') && (name[j] != '\0')) {
      buf[i] = name[j];
      j++;
      i++;
    }
  }
  else if ((module->SymbolHandlerWorking) &&
           (module->ImagehlpModuleStruct.ModuleName[0] != '\0')) {
    name = module->ImagehlpModuleStruct.ModuleName;
    while(name[i] != '\0') {
      buf[i] = name[i];
      i++;
    }
  }
  else {
    name = "unknown";
    while(name[i] != '\0') {
      buf[i] = name[i];
      i++;
    }
  }
  if (((NUBINT) i) < buf_size) buf[i] = '\0';
}


void nub_all_threads 
  (NUB nub, 
   NUB_INDEX *first, 
   NUB_INDEX *last)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  LPDBGTHREAD this_thread = process->ThreadList;
  LPDBGTHREAD last_thread = process->ThreadList;
  int count;

  // The first thing to do is "clean up" the nub's list of threads,
  // filtering out all those that have died. At the moment, I'm
  // dangerously ignoring the possibility that the primary thread
  // is not the most long-lived.

  while (this_thread != NULL) {

    if (!this_thread->Valid) {
      // Drop a node out of the list.
      last_thread->Next = this_thread->Next;
      free(this_thread);
      this_thread = last_thread->Next;
    }
    else {
      // This one lives on.
      last_thread = this_thread;
      this_thread = this_thread->Next;
    }
  }

  this_thread = process->ThreadList; // Rewind to scan the list again.

  (*first) = (NUB_INDEX) 1;
  count = 0;

  while (this_thread != NULL)
  {
    if (!this_thread->Valid) {
      // This is an internal error case
    }
    count++;
    this_thread = this_thread->Next;
  }

  // This loop will exit with count being the number of threads.
  // The index on the last thread is one less than this.

  (*last) = (NUB_INDEX) (count);
}


NUBTHREAD nub_thread_handle 
  (NUB nub, 
   NUB_INDEX thread)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  return (thread_descriptor_from_index(nub, thread));
}


NUBLIBRARY nub_library_handle 
  (NUB nub, 
   NUB_INDEX library)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  return ((NUBLIBRARY) library_descriptor_from_index (process, library));
}


NUBINT nub_get_thread_name_length 
  (NUB nub, 
   NUBTHREAD thread)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  LPDBGTHREAD Cthread = (LPDBGTHREAD) thread;
  int length;
  char *name;

  // Point name to the base of the name string.

  name = (char*) Cthread->ThreadName;

  // Count the characters...

  length = 0;
  while (name[length] != '\0') length ++;

  return ((NUBINT) length);
}


void nub_get_thread_name (
  NUB nub, 
  NUBTHREAD thread, 
  NUBINT buf_size,
  char *buf)
{
  LPDBGPROCESS process = (LPDBGPROCESS) nub;
  int          limit = (int) buf_size;
  LPDBGTHREAD  Cthread = (LPDBGTHREAD) thread;
  int          i = 0;
  char         *name = Cthread->ThreadName;

  // Copy characters...

  while ((name[i] != '\0') && (i < limit)) {
    buf[i] = name[i];
    i++;
  }

  // Null-terminate if necessary.

  if (i < limit) {
    buf[i] = '\0';
  }
}


NUBINT nub_thread_state 
  (NUB nub, 
   NUBTHREAD thread)
{
  return ((NUBINT) 0);
}


NUBINT nub_thread_os_priority 
  (NUB nub, 
   NUBTHREAD thread)
{
  LPDBGPROCESS        process = (LPDBGPROCESS) nub;
  LPDBGTHREAD         threadC = (LPDBGTHREAD) thread;
  int                 code = GetThreadPriority(threadC->ThreadHandle);
  
  if (code == THREAD_PRIORITY_ERROR_RETURN) code = 0;

  return ((NUBINT) code);
}


NUBINT nub_get_library_filename_length 
  (NUB nub, 
   NUBLIBRARY library)
{
  LPDBGPROCESS        process = (LPDBGPROCESS) nub;
  int                 i;
  char                *name;
  LPDBGLIBRARY        libraryC = (LPDBGLIBRARY) library;

  i = 0;
  name = libraryC->DefaultImageName;
  while (name[i] != '\0') i++;
  return ((NUBINT) i);
}

void nub_get_library_filename 
  (NUB nub, 
   NUBLIBRARY library, 
   NUBINT buf_size,
   char *buf)
{
  LPDBGPROCESS         process = (LPDBGPROCESS) nub;
  int                  limit, i;
  char                 *name;
  LPDBGLIBRARY         libraryC = (LPDBGLIBRARY) library;

  i = 0;

  name = libraryC->DefaultImageName;
  limit = (int) buf_size;

  while ((name[i] != '\0') && (i < limit)) { 
    buf[i] = name[i];
    i++;
  }

  if (i < limit) buf[i] = '\0';
}


NUBINT nub_get_thread_cpu_time
  (NUB nub,
   NUBTHREAD thread)
{
  LPDBGPROCESS         process = (LPDBGPROCESS) nub;
  LPDBGTHREAD          Cthread = (LPDBGTHREAD) thread;

  return ((NUBINT) (*get_os_thread_cpu_time)(process, Cthread));
}

