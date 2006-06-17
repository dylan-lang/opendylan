/* ********************************************************************** */
/* ** symbol_lookup.c                                                  ** */
/* ** Functions for looking up remote symbols in the application.      ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved              ** */
/* ********************************************************************** */

#include "nub-core.h"

/*
  Every loaded library has a NUBLIBRARY descriptor, which stores a
  code indicating what kind of debug information is available for that
  library. These routines make no assumptions about the availablity
  of debug information, but they find out what is available, and
  dispatch their arguments out to a specific routine for unpicking that
  kind of information.
*/


SYMBOL_LOOKUP_ENTRY *find_entry (LOOKUP_TABLE *table, DWORD index);
// Finds the actual entry in the lookup table. Note that the "index" is only
// a pseudo-index into the table. Since the table is made up of small arrays
// chained together, some calculations have to be done to get the table entry.


BOOL search_boundary_cache
  (BOUNDARY_CACHE *cache, DWORD addr, DWORD *lower, DWORD *upper);
// Attempts to find a pair of cached function boundaries that enclose
// the supplied address in the given cache. If it is found, the function
// returns TRUE, and the output parameters lower and upper will be
// filled in with the boundary. Otherwise, FALSE is returned.


void add_boundary_cache
  (BOUNDARY_CACHE *cache, DWORD addr, DWORD lower, DWORD upper);
// Adds a pair of function boundaries enclosing "addr" to the specified
// cache.

char out[1024];

#define debug_me(x, y) \
  sprintf(out, x, y); \
  debugger_message(out, 0, 0);

BOOL CALLBACK nub_enumerate_symbol_callback
  (PSYMBOL_INFO pSymInfo,
   ULONG        SymbolSize,
   PVOID        UserContext)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) UserContext;

  process->SymbolBuffer.Info = *pSymInfo;
//  strcpy(process->SymbolBuffer.Info.Name, pSymInfo->Name);
  
  process->SymbolBufferValid = TRUE;

  debug_me("Found symbol: %s", pSymInfo->Name);

  return TRUE;
}


NUBINT
  nub_closest_symbol
     (NUB nub,
      TARGET_ADDRESS location,
      NUBLIBRARY *library,
      TARGET_ADDRESS *actual_address,
      NUBINT *offset,
      NUBINT *name_length,
      NUBINT *type,
      NUBINT *is_function,
      TARGET_ADDRESS *debug_start,
      TARGET_ADDRESS *debug_end,
      NUBINT *language,
      TARGET_ADDRESS *final
  )
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  BOOL              status;
  DWORD64           dw_disp;
  LPDBGLIBRARY      module 
                      = library_descriptor_from_address (process,
                                                         (DWORD) location);


  debug_me("nub_closest_symbol for address 0x%x", location);

  ensure_debug_information_for_library(process, module);

  switch (module->DebugType) {
  
  // We're going to use mapped CV stuff in preference to the symbol
  // handler, purely because it allows us access to more data!

  case CODEVIEW_IMAGE:
    if (closest_symbol_in_debug_map
          (process, module, location,
           name_length, actual_address, type, is_function, debug_start,
           debug_end, language, final)) {
      (*offset) = (NUBINT) ((DWORD) location - (DWORD) (*actual_address));
      (*library) = (NUBLIBRARY) module;
      return ((NUBINT) 1);
    }
    else {
      return ((NUBINT) 0);
    }
    break;

  case COFF_IMAGE:
    if (closest_symbol_in_coff_map
          (process, module, location,
           name_length, actual_address, type, is_function, debug_start,
           debug_end, language, final)) {
      (*offset) = (NUBINT) ((DWORD) location - (DWORD) (*actual_address));
      (*library) = (NUBLIBRARY) module;
      return ((NUBINT) 1);
    }
    else {
      return ((NUBINT) 0);
    }
    break;

  default:
    // If the symbol handler is working, try to use DbgHelp to lookup the
    // symbol.

    process->SymbolBuffer.Info.MaxNameLen = 256;

    debug_me("Searching symbol", 0);

    status =
      SymFromAddr(process->ProcessHandle,
	         (DWORD64) location,
                 &dw_disp,
                 (SYMBOL_INFO*) &(process->SymbolBuffer.Info));
    if (status) {
      BYTE    i = 0;
      debug_me("Located a symbol: %s", process->SymbolBuffer.Info.Name);
      //printf("Located the symbol: ");
      (*actual_address) = (TARGET_ADDRESS) process->SymbolBuffer.Info.Address;
      (*offset)
	= (NUBINT) dw_disp;
      // TODO: Something better here.
      // When using DbgHelp's symbol handler, we have no real way to
      // identify the programming language that defined the symbol.
      // This is a slight hack.
      // In fact, there's a corresponding function in the high-level code...
      i = strlen(process->SymbolBuffer.Info.Name);
      if ((i > 0) &&
          (process->SymbolBuffer.Info.Name[0] == 'K') &&
          (process->SymbolBuffer.Info.Name[i - 1] == 'I'))
        (*language) = DYLAN_LANGUAGE;
      else
        (*language) = C_LANGUAGE;
      (*debug_start) = (*actual_address);
      (*debug_end) = 
        (TARGET_ADDRESS) 
          (process->SymbolBuffer.Info.Address
	   + process->SymbolBuffer.Info.Size);
      (*final) = (*debug_end);
      (*library) = (NUBLIBRARY) module;
      (*name_length) = (NUBINT) i;
      (*is_function) = (process->SymbolBuffer.Info.Tag == SymTagFunction);
      process->NameCache = process->SymbolBuffer.Info.Name;
      return ((NUBINT) 1);
    }
    else {
      if (status) {
        debug_me("no symbol found for addr\n",0 );
      } else {
	debug_me("Error %x\n", GetLastError());
      }
      return((NUBINT) 0);
    }
  }
}

void nub_closest_symbol_name
  (NUB nub, NUBINT buf_size, char *buf)
{
  LPDBGPROCESS     process = (LPDBGPROCESS) nub;
  int              limit = (int) buf_size;
  char             *name = process->NameCache;
  int              i = 0;

  debug_me("nub_closest_symbol_name",0);

  while (i < limit) {
    buf[i] = name[i];
    i++;
  }
  if (i < limit) buf[i] = '\0';
}

NUBINT nub_find_symbol_in_library 
  (NUB nub, 
   NUBLIBRARY library,
   NUBINT name_length, char *name,
   TARGET_ADDRESS *address,
   NUBINT *type,
   NUBINT *is_function,
   TARGET_ADDRESS *debug_start,
   TARGET_ADDRESS *debug_end,
   NUBINT *language,
   TARGET_ADDRESS *last_address)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY      module = (LPDBGLIBRARY) library;
  BOOL              status;
  char              extended_name[300];
  int               i = 0;
  int               j = 0;

  debug_me("nub_find_symbol_in_library",0);

  ensure_debug_information_for_library(process, module);

  switch (module->DebugType) {

  case CODEVIEW_IMAGE:
    if (find_symbol_in_debug_map 
         (process, module, (BYTE) name_length,
          name, address, type,
          is_function, debug_start, debug_end, language, last_address))
      return ((NUBINT) 1);
    else
      return ((NUBINT) 0);
  
    break;

  case COFF_IMAGE:
    if (find_symbol_in_coff_map 
         (process, module, (BYTE) name_length,
          name, address, type,
          is_function, debug_start, debug_end, language, last_address))
      return ((NUBINT) 1);
    else
      return ((NUBINT) 0);
  
    break;

  default:
    // If the symbol handler is working, try to use DbgHelp to lookup the
    // symbol.

    if (!(module->SymbolHandlerWorking))
      return((NUBINT) 0);

    // We are going to extend the name with the "module!" prefix so
    // that the search is restricted to this one library, as per the
    // semantics of this API.
    // Just do it quickly inline!

    while((module->ImagehlpModuleStruct.ModuleName[j] != '\0') &&
          (j < 32)) {
      extended_name[i] = module->ImagehlpModuleStruct.ModuleName[j];
      i++; j++;
    }
    extended_name[i] = '!'; i++;
    for (j = 0; j < name_length; j++) {
      extended_name[i] = name[j];
      i++;
    }
    extended_name[i] = '\0';

    // Fill in the symbol buffer information.

    process->SymbolBufferValid = FALSE;

    debug_me("Enumerating symbols for %s", extended_name);

    status =
      SymEnumSymbols(process->ProcessHandle,
		     module->ImageInformation.ImageBase,
		     extended_name,
		     nub_enumerate_symbol_callback,
		     process);
    if (status && process->SymbolBufferValid) {
      (*address) = (TARGET_ADDRESS) process->SymbolBuffer.Info.Address;
      // TODO: Something better here.
      // When using DbgHelp's symbol handler, we have no real way to
      // identify the programming language that defined the symbol.
      // This is a slight hack.
      if ((name[0] == 'K')  && (name[name_length - 1] == 'I'))
        (*language) = DYLAN_LANGUAGE;
      else
        (*language) = C_LANGUAGE;
      (*debug_start) = (*address);
      (*debug_end) = 
        (TARGET_ADDRESS) 
          (process->SymbolBuffer.Info.Address + process->SymbolBuffer.Info.Size);
      (*last_address) = (*debug_end);
      (*is_function) = (process->SymbolBuffer.Info.Tag == SymTagFunction);
      process->NameCache = process->SymbolBuffer.Info.Name;
      return ((NUBINT) 1);
    }
    else {
      return((NUBINT) 0);
    }
  }
}


void nub_do_static_symbols 
  (NUB nub, 
   NUBLIBRARY library, 
   NUB_INDEX *first,
   NUB_INDEX *last, 
   NUBHANDLE *lookups)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY      module = (LPDBGLIBRARY) library;
  LOOKUP_TABLE      *table;
  DWORD             dwFirst, dwLast;

  debug_me("nub_do_static_symbols",0);

  switch (module->DebugType) {

  case CODEVIEW_IMAGE:
    table = static_symbols_from_debug_map 
              (process, module, &dwFirst, &dwLast);
    break;

  case NONE:
  case COFF_IMAGE:
  case CODEVIEW_PDB:
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
    break;

  default:
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
  }

  (*first) = (NUB_INDEX) dwFirst;
  (*last) = (NUB_INDEX) dwLast;
  (*lookups) = (NUBHANDLE) table;
}


void nub_do_global_symbols 
  (NUB nub, 
   NUBLIBRARY library, 
   NUB_INDEX *first,
   NUB_INDEX *last, 
   NUBHANDLE *lookups)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY      module = (LPDBGLIBRARY) library;
  LOOKUP_TABLE      *table;
  DWORD             dwFirst, dwLast;

  debug_me("nub_do_global_symbols",0);

  switch (module->DebugType) {

  case CODEVIEW_IMAGE:
    table = global_symbols_from_debug_map 
              (process, module, &dwFirst, &dwLast);
    break;

  case NONE:
  case COFF_IMAGE:
  case CODEVIEW_PDB:
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
    break;

  default:
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
  }

  (*first) = (NUB_INDEX) dwFirst;
  (*last) = (NUB_INDEX) dwLast;
  (*lookups) = (NUBHANDLE) table;
}


void nub_do_exported_symbols 
  (NUB nub, 
   NUBLIBRARY library, 
   NUB_INDEX *first,
   NUB_INDEX *last, 
   NUBHANDLE *lookups)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY      module = (LPDBGLIBRARY) library;
  LOOKUP_TABLE      *table;
  DWORD             dwFirst, dwLast;

  debug_me("nub_do_exported_symbols",0);

  switch (module->DebugType) {

  case CODEVIEW_IMAGE:
    table = exported_symbols_from_debug_map 
              (process, module, &dwFirst, &dwLast);
    break;

  case NONE:
  case COFF_IMAGE:
  case CODEVIEW_PDB:
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
    break;

  default:
    dwFirst = 1;
    dwLast = 0;
    table = NULL;
  }

  (*first) = (NUB_INDEX) dwFirst;
  (*last) = (NUB_INDEX) dwLast;
  (*lookups) = (NUBHANDLE) table;
}


NUBINT nub_nearest_symbols 
  (NUB nub, 
   TARGET_ADDRESS addr, 
   NUBLIBRARY *lib, 
   NUBHANDLE *lookups)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  LPDBGLIBRARY      module 
                      = library_descriptor_from_address (process,
                                                         (DWORD) addr);
  LOOKUP_TABLE      *table;

  debug_me("nub_nearest_symbols",0);

  if (module == NULL) {
    (*lookups) = NULL;
    return ((NUBINT) 0);
  }

  ensure_debug_information_for_library(process, module);
  
  (*lib) = (NUBLIBRARY) module;

  switch (module->DebugType) {

  case CODEVIEW_IMAGE:
    table = nearest_symbols_in_debug_map 
              (process, module, (DWORD) addr);
    (*lookups) = (NUBHANDLE) table;
    return ((NUBINT) 1);
    break;

  default:
    return ((NUBINT) 0);
    break;
  }
}


void nub_function_bounding_addresses 
  (NUB nub, 
   TARGET_ADDRESS addr, 
   TARGET_ADDRESS *lower,
   TARGET_ADDRESS *upper)
{
  LPDBGPROCESS      process = (LPDBGPROCESS) nub;
  DWORD             dwAd = (DWORD) addr;
  DWORD             dwHi, dwLo;
  LPDBGLIBRARY      module 
                      = library_descriptor_from_address (process,
                                                         (DWORD) addr);
  BOOL                   status = FALSE;

  debug_me("nub_function_bounding_addresses",0);

  ensure_debug_information_for_library(process, module);

  // The first place to look is the cache.

  if (search_boundary_cache(&(module->BoundaryCache), dwAd, &dwHi, &dwLo)) {
    (*lower) = (TARGET_ADDRESS) dwLo;
    (*upper) = (TARGET_ADDRESS) dwHi;
    return;
  }

  // If the symbol handler is working, then try using it to obtain the
  // symbol, and hence its range.

  if (process->SymbolHandlerWorking) {
    process->SymbolBufferValid = FALSE;

    status =
      SymEnumSymbolsForAddr(process->ProcessHandle,
			    (DWORD64) addr,
			    nub_enumerate_symbol_callback,
			    process);
  }

  if (status) {
    // DbgHelp found the symbol, so unpick the details.
    (*lower) = (TARGET_ADDRESS) process->SymbolBuffer.Info.Address;
    (*upper)
      = (TARGET_ADDRESS) (process->SymbolBuffer.Info.Address
			  + process->SymbolBuffer.Info.Size - 1);
  }
  else {
    // DbgHelp did not come through for us, so defer to bruteforce
    // methods.

    debugger_message("Tried to lookup symbols for %= and failed!\n", addr, 0);

    module = library_descriptor_from_address (process, (DWORD) addr);

    if (module == NULL) {
      (*lower) = addr;
      (*upper) = addr;
      return;
    }
  
    switch (module->DebugType) {

    case CODEVIEW_IMAGE:
      function_bounding_addresses_in_debug_map 
         (process, module, (DWORD) addr, lower, upper);
      break;

    default:
      (*lower) = addr;
      (*upper) = addr;
      break;
    }
  }

  add_boundary_cache(&(module->BoundaryCache), (DWORD) addr, 
                     (DWORD) (*lower),
                     (DWORD) (*upper));
}


NUBINT nub_get_lexical_variable_name_length 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);

  debug_me("nub_lexical_variable_name_length",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    // This is the only one we deal with for now.
    return ((NUBINT) lexical_name_length_from_debug_map 
                       ((LOOKUP_TABLE*) table, sym));
    break;

  case MAPPED_COFF_SYMBOL:
    return ((NUBINT) 0);
    break;

  default:
    return ((NUBINT) 0);
  }
}


NUBINT nub_lookup_symbol_name_length 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index)
{

  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);
  debug_me("nub_lookup_symbol_name_length",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    // This is the only one we deal with for now!
    return ((NUBINT) symbol_name_length_from_debug_map 
                       ((LOOKUP_TABLE*) table, sym));
    break;

  case MAPPED_COFF_SYMBOL:
    return ((NUBINT) 0);
    break;

  default:
    return ((NUBINT) 0);
  }
}


NUBINT nub_lookup_symbol_language_code
  (NUB nub,
   NUBHANDLE table,
   NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);
  debug_me("nub_lookup_symbol_language_code",0);

  return ((NUBINT) (sym->LanguageCode));
}


void nub_get_lexical_variable_name 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index,
   NUBINT buf_size, 
   char *buf)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);
  debug_me("nub_get_lexical_variable_name",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    lexical_name_from_debug_map ((LOOKUP_TABLE*) table, sym,
                                 (DWORD) buf_size, buf);
    break;

  default:
    break;
  }
}

// This function is redundant. It just happened to speed things up
// slightly with the old compiler.

char *nub_get_lexical_variable_name_fast 
  (NUB nub, 
   NUBHANDLE table,
   NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);

  debug_me("nub_get_lexical_variable_name_fast",0);

  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    lexical_name_from_debug_map ((LOOKUP_TABLE*) table, sym,
                                 (DWORD) GLOBAL_STRING_SPACE_SIZE,
                                 global_string_space);
    return (global_string_space);
    break;

  default:
    return ("**NUB SYMBOL LOOKUP ERROR**");
    break;
  }
}


void nub_lookup_symbol_name 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index,
   NUBINT buf_size, 
   char *buf)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);
  debug_me("nub_lookup_symbol_name",0);

  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    // This is the only one we deal with for now!
    symbol_name_from_debug_map (table, sym, (DWORD) buf_size, buf);
    break;

  default:
    break;

  }
}


char* nub_lookup_symbol_name_fast 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index)
{

  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);
  debug_me("nub_lookup_symbol_name_fast",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    // This is the only one we deal with for now!
    symbol_name_from_debug_map (table, sym, (DWORD) GLOBAL_STRING_SPACE_SIZE, 
                                global_string_space);
    return (global_string_space);
    break;

  default:
    return ("**NUB SYM LOOKUP ERROR**");
    break;
  }
}


TARGET_ADDRESS nub_lexical_variable_address 
  (NUB nub, 
   TARGET_ADDRESS frame, 
   NUBHANDLE table,
   NUB_INDEX index,
   NUBINT *needs_register_lookup,
   NUB_INDEX *register_index_high,
   NUB_INDEX *register_index_low,
   NUBINT *is_argument)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);
  DWORD               FP = (DWORD) frame;
  DWORD               offset;
  DWORD               addr;

  debug_me("nub_lexical_variable_address",0);

  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    offset = lexical_address_from_debug_map
                (table, sym, needs_register_lookup,
                 register_index_high, register_index_low);
    addr = FP + offset;
    if (addr > FP)
      (*is_argument) = (NUBINT) 1;
    else
      (*is_argument) = (NUBINT) 0;
    return ((TARGET_ADDRESS) addr);
    break;

  default:
    return ((TARGET_ADDRESS) 0);
  }
}


TARGET_ADDRESS nub_lookup_symbol_address 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);

  debug_me("nub_lookup_symbol_address",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    return ((TARGET_ADDRESS) symbol_address_from_debug_map (table, sym));
    break;

  default:
    return ((TARGET_ADDRESS) NULL);
  }
}


TARGET_ADDRESS nub_lookup_function_debug_start 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);

  debug_me("nub_lookup_function_debug_start",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    return ((TARGET_ADDRESS) function_debug_start_from_debug_map (table, sym));
    break;

  default:
    return ((TARGET_ADDRESS) NULL);
  }
}


TARGET_ADDRESS nub_lookup_function_debug_end 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);

  debug_me("nub_lookup_function_debug_end",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    return ((TARGET_ADDRESS) function_debug_end_from_debug_map (table, sym));
    break;

  case MAPPED_COFF_SYMBOL:
    return ((TARGET_ADDRESS) NULL);
    break;

  default:
    return ((TARGET_ADDRESS) NULL);
  }
}


TARGET_ADDRESS nub_lookup_function_end 
  (NUB nub, 
   NUBHANDLE table, 
   NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);

  debug_me("nub_lookup_function_end",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    return ((TARGET_ADDRESS) function_end_from_debug_map (table, sym));
    break;

  case MAPPED_COFF_SYMBOL:
    return ((TARGET_ADDRESS) NULL);
    break;

  default:
    return ((TARGET_ADDRESS) NULL);
  }
}


NUBINT nub_symbol_is_function (NUB nub, NUBHANDLE table, NUB_INDEX index)
{
  SYMBOL_LOOKUP_ENTRY *sym = find_entry ((LOOKUP_TABLE*) table,
                                         (DWORD) index);

  debug_me("nub_symbol_is_function",0);


  switch (sym->LookupType) {

  case MAPPED_CODEVIEW_SYMBOL:
    return ((NUBINT) symbol_is_function_from_debug_map (table, sym));
    break;

  default:
    return ((NUBINT) 0);
    break;
  }
}


void nub_dispose_lookups (NUB nub, NUBHANDLE table)
{
  LOOKUP_TABLE         *lookups = (LOOKUP_TABLE*) table;
  LOOKUP_TABLE_SEGMENT *segment;
  LOOKUP_TABLE_SEGMENT *old_segment;

  if (lookups == NULL) return; // No table was allocated, 
                               // so nothing to dispose.

  segment = lookups->FirstSegment;
  old_segment = segment;

  while (segment != NULL) {
    old_segment = segment;
    segment = segment->NextSegment;
    free(old_segment);
  }
  free (lookups);
}


SYMBOL_LOOKUP_ENTRY *find_entry (LOOKUP_TABLE *table, DWORD index)
{
  LOOKUP_TABLE_SEGMENT *this_segment = table->FirstSegment;
  DWORD                seg = 0;
  DWORD                count = 0;
  DWORD                seg_size = (DWORD) LOOKUP_TABLE_SEGMENT_SIZE;
  DWORD                partition;

  count = ((index - 1) / seg_size);
  partition = ((index - 1) % seg_size);

  while (count < seg) 
    this_segment = this_segment->NextSegment;

  return (&(this_segment->SegmentEntries[partition]));
}


LOOKUP_TABLE *new_lookup_table (LPDBGPROCESS process, LPDBGLIBRARY module)
{
  LOOKUP_TABLE *table = (LOOKUP_TABLE*) malloc (sizeof(LOOKUP_TABLE));

  table->Process = process;
  table->Module = module;

  // Allocate one segment now - this is the minimum number of segments
  // that the table can hold in order to be meaningful!

  table->FirstSegment =
    (LOOKUP_TABLE_SEGMENT*) malloc (sizeof(LOOKUP_TABLE_SEGMENT));

  table->FirstSegment->NextSegment = NULL;
  table->LastSegment = table->FirstSegment;
  table->LastEntry = 0;
  return (table);
}


void add_lookup_table_entry 
  (LOOKUP_TABLE *table, BYTE lookup_type, void *pointer, BYTE lang)
{
  // If this addition requires us to add a new segment to the lookup table,
  // then allocate the new segment, link to the list, and make sure that the
  // last entry is reset to zero so we start filling this table from the
  // beginning.

  if (table->LastEntry == LOOKUP_TABLE_SEGMENT_SIZE) {
    table->LastSegment->NextSegment = malloc (sizeof(LOOKUP_TABLE_SEGMENT));
    table->LastSegment = table->LastSegment->NextSegment;
    table->LastEntry = 0;
    table->LastSegment->NextSegment = NULL;
  }

  // Now we can add the entry

  table->LastSegment->SegmentEntries[table->LastEntry].LookupType 
    = lookup_type;
  table->LastSegment->SegmentEntries[table->LastEntry].Pointer 
    = pointer;
  table->LastSegment->SegmentEntries[table->LastEntry].LanguageCode
    = lang;

  // Make sure the next entry goes into the next slot.
  table->LastEntry ++;
}


BOOL search_boundary_cache
  (BOUNDARY_CACHE *cache, DWORD addr, DWORD *lower, DWORD *upper)
{
  DWORD      i;

  if (cache->IndexNextEntry == 0)
    return(FALSE);

  for (i = 0; i < cache->IndexNextEntry; i++) {
    if (cache->EntryTable[i].AddrAtLastHit == addr) {
      cache->EntryTable[i].HitCount++;
      (*lower) = cache->EntryTable[i].LowerBound;
      (*upper) = cache->EntryTable[i].UpperBound;
      return(TRUE);
    }
    else if ((cache->EntryTable[i].LowerBound <= addr) &&
             (addr <= cache->EntryTable[i].UpperBound)) {
      cache->EntryTable[i].HitCount++;
      (*lower) = cache->EntryTable[i].LowerBound;
      (*upper) = cache->EntryTable[i].UpperBound;
      cache->EntryTable[i].AddrAtLastHit = addr;
      return(TRUE);
    }
  }
  return(FALSE);
}


void add_boundary_cache
  (BOUNDARY_CACHE *cache, DWORD addr, DWORD lower, DWORD upper)
{
  DWORD      i;

  // If this is not a useful entry, just return.
  if (upper == lower)
    return;

  // If the boundary cache still has places in it, just insert this
  // new entry.

  if (!cache->CacheFull) {
    i = cache->IndexNextEntry;
    (cache->IndexNextEntry)++;
    (cache->IndexLastAddition) = i;
    cache->EntryTable[i].HitCount = 0;
    cache->EntryTable[i].AddrAtLastHit = addr;
    cache->EntryTable[i].LowerBound = lower;
    cache->EntryTable[i].UpperBound = upper;
    if (cache->IndexNextEntry >= MAX_BOUNDARY_CACHE_ENTRIES) {
      cache->CacheFull = TRUE;
    }
  }
  else {
    // The cache is full, so we will have to throw out an entry.
    // Find the entry with the lowest hit count?
    DWORD    lowest_count = cache->EntryTable[0].HitCount;
    DWORD    candidate_index = 0;
    for (i = 0; i < MAX_BOUNDARY_CACHE_ENTRIES; i++) {
      if (cache->EntryTable[i].HitCount < lowest_count) {
        lowest_count = cache->EntryTable[i].HitCount;
        candidate_index = i;
      }
    }
    i = candidate_index;
    (cache->IndexLastAddition) = i;
    cache->EntryTable[i].HitCount = 0;
    cache->EntryTable[i].AddrAtLastHit = addr;
    cache->EntryTable[i].LowerBound = lower;
    cache->EntryTable[i].UpperBound = upper;
  }
}
