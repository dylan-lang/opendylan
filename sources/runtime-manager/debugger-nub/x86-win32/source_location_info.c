/* ********************************************************************** */
/* ** source_location_info.c                                           ** */
/* ** Storage and retrieval of NUB source location information.        ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

/*

  NOTE:
  Source location functionality has been hurriedly added to the
  Devel milestone requirements. It was a bit of a scramble to get
  this together, and it isn't ideal.

  Like symbolic lookup, this stuff only works if CodeView information
  is present in the loaded image files.

*/


NUBINT nub_source_location_address 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX i)
{
  LPDBGPROCESS             process = (LPDBGPROCESS) nub;
  PIMAGE_DEBUG_INFORMATION info;
  SL_LOOKUP_TABLE          *locations = (SL_LOOKUP_TABLE*) table;
  int                      this_location = (int) i;
  DWORD                    address;

  if (locations == NULL) {
    return (0);
  }

  info = locations->Module->DebugMap;
  address = locations->Locations[this_location].Offset;

  return ((NUBINT) address);
}


NUBINT nub_source_location_linenumber 
  (NUB nub, 
   NUBHANDLE table,
   NUB_INDEX i)
{
  LPDBGPROCESS             process = (LPDBGPROCESS) nub;
  PIMAGE_DEBUG_INFORMATION info;
  SL_LOOKUP_TABLE          *locations = (SL_LOOKUP_TABLE*) table;
  int                      this_location = (int) i;
  WORD                     linenumber;

  if (locations == NULL) {
    return ((NUBINT) 0);
  }

  info = locations->Module->DebugMap;
  linenumber = locations->Locations[this_location].LineNumber;

  return ((NUBINT) linenumber);
}


NUBINT nub_source_location_filename_length 
  (NUB nub, 
   NUBHANDLE table)
{
  SL_LOOKUP_TABLE        *locations = (SL_LOOKUP_TABLE*) table;

  if (locations == NULL)
    return ((NUBINT) 0);
  else
    return ((NUBINT) (locations->FilenameLength));
}


void nub_source_location_filename 
  (NUB nub, 
   NUBHANDLE table,
   NUBINT buf_size, 
   char *buf)
{
  SL_LOOKUP_TABLE         *locations = (SL_LOOKUP_TABLE*) table;
  int                     limit = (int) buf_size;
  int                     i = 0;
  char                    *name;

  if (locations == NULL)
    return;

  name = locations->Filename;

  // Copy characters...

  while ((name[i] != '\0') && (i < limit)) {
    buf[i] = name[i];
    i++;
  }

  // Null-terminate if necessary.

  if (i < limit)
    buf[i] = '\0';
}


void nub_dispose_source_locations 
  (NUB nub, 
   NUBHANDLE table)
{
  SL_LOOKUP_TABLE            *locations = (SL_LOOKUP_TABLE*) table;

  if (locations == NULL)
    return;
  else
    free (locations);
}


NUBINT nub_number_of_source_locations 
  (NUB nub, 
   NUBHANDLE table)
{
  SL_LOOKUP_TABLE            *locations = (SL_LOOKUP_TABLE*) table;

  if (locations == NULL)
    return ((NUBINT) 0);
  else
    return ((NUBINT) (locations->NumberOfEntries));
}


NUBHANDLE nub_fetch_source_locations 
  (NUB nub,
   TARGET_ADDRESS start,
   TARGET_ADDRESS end)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY      module = library_descriptor_from_address (process,
                                                          (DWORD) start);
  LPDBGLIBRARY      endmod = library_descriptor_from_address (process,
                                                          (DWORD) end);

  if (module == NULL) {
    return (NULL);
  }
  else if (module != endmod) {

    // This implementation currently doesn't work if the two
    // addresses are in different modules. This is not really
    // acceptable, but it shouldn't happen in practice since there
    // is no way that a symbol's definition can split across DLLs.

    return (NULL);
  }

  ensure_debug_information_for_library(process, module);

  switch (module->DebugType) {

  case CODEVIEW_IMAGE:
    return ((NUBHANDLE)
            source_locations_from_debug_map (process, module,
                                            (DWORD) start,
                                            (DWORD) end));
    break;

  default:
    return (NULL);
    break;
  }
}


TARGET_ADDRESS nub_resolve_source_location
  (NUB nub,
   NUBLIBRARY library,
   char *filename,
   NUBINT linenumber,
   NUBINT column,
   NUBINT *valid,
   NUBINT *path,
   NUBHANDLE *search,
   NUBINT *is_exact)
{
   LPDBGPROCESS           process = (LPDBGPROCESS) nub;
   LPDBGLIBRARY           module = (LPDBGLIBRARY) library;
   WORD                   line = (WORD) linenumber;
   WORD                   col = (WORD) column;
   DWORD                  address;
   BOOL                   exact;

   ensure_debug_information_for_library(process, module);
   
   switch (module->DebugType) {

   case CODEVIEW_IMAGE:
     if (resolve_source_location_in_debug_map
            (process, module, filename, line, col, &address, &exact)) {
       (*valid) = 1;
       (*path) = 0;
       if (exact)
         (*is_exact) = 1;
       else
         (*is_exact) = 0;
       return((TARGET_ADDRESS) address);
     }
     else {
       (*valid) = 0;
       (*path) = 0;
       (*is_exact) = 0;
       return((TARGET_ADDRESS) 0);
     }
     break;

   default:
     (*valid) = 0;
     (*path) = 0;
     (*is_exact) = 0;
     return((TARGET_ADDRESS) 0);
   }
}


NUBHANDLE nub_add_source_search_path
  (NUB nub, char *name)
{
  return ((NUBHANDLE) 0);
}


void nub_remove_source_search_path
  (NUB nub, char *name)
{
}


void nub_clear_source_search_paths
  (NUB nub)
{
}
