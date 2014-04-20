/* ************************************************************************ */
/* ** coff_map.c                                                         ** */
/* ** Functions for obtaining and navigating memory-mapped debug         ** */
/* ** information in COFF format.                                        ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard, Copyright: 1996-2000 Functional Objects, Inc. ** */
/* **                                 All Rights Reserved.               ** */
/* ************************************************************************ */

#include "nub-core.h"

// TODO: This is not ideal.
//       For the most part, the symbolic-lookup functionality of the debugger
//       nub is not supposed to know anything about the dylan language, or
//       the functional objects implementation of it.
//       When there is more time, this implementation should be changed
//       so that a higher level can inform the debugger nub of a number of
//       symbols that should be ignored when doing by-address lookups.
//       It is also incorrect that we are only doing this for COFF symbol
//       information - we should really be doing it for CodeView also.

#define NUMBER_OF_EXPLICIT_NON_DATA_SYMBOLS 3

typedef struct _EXPLICIT_NON_DATA_SYMBOL {
  BYTE       NameLength;
  char      *Name;
} EXPLICIT_NON_DATA_SYMBOL;

BOOL is_non_data_record (COFF_SYMBOL_ENTRY *x, BYTE *strings);

static EXPLICIT_NON_DATA_SYMBOL explicit_non_data_symbols[] = {
  {29,  "_dylan_object_file_objs_start"},
  {29,  "_dylan_object_file_vars_start"},
  {26,  "_dylan_untraced_objs_start"}
};

int section_number_called (char *target, IMAGE_INFORMATION *info)
{
  int this_section;
  int j;

  for (this_section = 0;
       this_section < (info->CoffHeader).NumberOfSections;
       this_section++) {
    char *name = (info->SectionHeaders)[this_section].SectionName;
    BOOL match = TRUE;

    for (j = 0; j < 8; j++) {
      if ((name[j] == '\0') && (target[j] == '\0') && match)
        return (this_section);
      else if (name[j] != target[j])
        match = FALSE;
    }
    if (match) return (this_section);
  }
  return (SECTION_NOT_FOUND);
}

void pull_image_header (LPDBGLIBRARY module)
{
  IMAGE_INFORMATION   *info = &(module->ImageInformation);
  DWORD                bytes_transferred;
  DWORD                image_signature_pointer;
  HANDLE               file = info->ImageFileHandle;

  // First, set sensitive fields in the image info structure to
  // null values, so we never try to examine crud.

  (info->CoffHeader).NumberOfSections = 0;
  (info->NumberOfDebugDirectories) = 0;
  (info->NumberOfSubSectionDirEntries) = 0;
  (info->NumberOfCoffSymbols) = 0;
  (info->CoffSymbolTable) = NULL;
  (info->CoffStringTable) = NULL;
  (info->CoffTableExplicitlyAllocated) = FALSE;

  // Navigate through header stuff to get the image signature.

  SetFilePointer (file, (LONG) LOCATION_OF_SIGNATURE_POINTER,
                  NULL, FILE_BEGIN);

  ReadFile (file, &image_signature_pointer, sizeof(DWORD),
            &bytes_transferred, NULL);

  SetFilePointer (file, (LONG) image_signature_pointer, NULL,
                  FILE_BEGIN);

  // Read the signature, the COFF header, and the optional header.

  ReadFile (file, &(info->ImageSignature), sizeof(IMAGE_SIGNATURE),
            &bytes_transferred, NULL);

  ReadFile (file, &(info->CoffHeader), sizeof(COFF_HEADER),
       	    &bytes_transferred, NULL);

  // Copy the symbol count into the top level descriptor.

  (info->NumberOfCoffSymbols) = (info->CoffHeader).NumberOfSymbols;

  ReadFile (file, &(info->OptionalHeader),
            (DWORD) (info->CoffHeader).OptionalHeaderSize,
            &bytes_transferred, NULL);
}


void pull_image_information (LPDBGLIBRARY module)
{
  IMAGE_INFORMATION   *info = &(module->ImageInformation);
  DWORD                bytes_transferred;
  DWORD                string_table_length;
  int                  num_sections;
  int                  num_extra_sections = 0;
  int                  num_debug_dirs;
  DWORD                rva_debug_dirs;
  int                  debug_section;
  int                  rdata_section;
  HANDLE               file = info->ImageFileHandle;

  // Allocate space for the section headers and read them.

  num_sections = (int) (info->CoffHeader).NumberOfSections;
  (info->SectionHeaders) =
     (COFF_SECTION_HEADER*) 
       malloc (num_sections * sizeof(COFF_SECTION_HEADER));
	
  ReadFile (file, info->SectionHeaders,
            (DWORD) num_sections * sizeof(COFF_SECTION_HEADER),
            &bytes_transferred, NULL);

  // Now read in the debug directory entries.

  num_debug_dirs = 
    ((int) (info->OptionalHeader).DataDirectories.Debug.Size) /
            sizeof(DEBUG_DIRECTORY);

  (info->NumberOfDebugDirectories) = num_debug_dirs;

  rva_debug_dirs = (info->OptionalHeader).DataDirectories.Debug.RVA;

  // For now, just truncate any that won't fit in the default space.

  if (num_debug_dirs > MAX_DEBUG_DIRECTORIES)
    num_debug_dirs = MAX_DEBUG_DIRECTORIES;

  // Look for named sections that directories may appear in.
  // The obvious one is a ".debug" section, but it is documented
  // that Microsoft executables store them in the ".rdata" section.

  debug_section = section_number_called(".debug", info);
  rdata_section = section_number_called(".rdata", info);

  if (debug_section != SECTION_NOT_FOUND) {
    // We need to find a file pointer to the debug directories.
    // We have their RVA, and also the RVA (and file pointer) of
    // the raw data for the section. Therefore, we calculate the
    // file pointer by subtracting the header RVA from the debug
    // directory RVA. The result of this, added to the file pointer
    // of the section's raw data, gives the file pointer for the
    // debug directories.

    // You see? Quite simple!

    DWORD rva_header = (info->SectionHeaders)[debug_section].RVA;
    DWORD raw_data_offset = rva_debug_dirs - rva_header;
    DWORD file_pointer = 
           (info->SectionHeaders)[debug_section].PointerToRawData
            + raw_data_offset;
  
    // Set the file pointer to the calculated position.

    SetFilePointer(file, (LONG) file_pointer, NULL, FILE_BEGIN);

    // Read the debug directories.

    ReadFile(file, info->DebugDirectories,
             (DWORD) num_debug_dirs * sizeof(DEBUG_DIRECTORY),
             &bytes_transferred,
             NULL);
  }
  else if (rdata_section != SECTION_NOT_FOUND) {
    // We need to find a file pointer to the debug directories.
    // We have their RVA, and also the RVA (and file pointer) of
    // the raw data for the section. Therefore, we calculate the
    // file pointer by subtracting the header RVA from the debug
    // directory RVA. The result of this, added to the file pointer
    // of the section's raw data, gives the file pointer for the
    // debug directories.

    // You see? Quite simple!

    DWORD rva_header = (info->SectionHeaders)[rdata_section].RVA;
    DWORD raw_data_offset = rva_debug_dirs - rva_header;
    DWORD file_pointer = 
           (info->SectionHeaders)[rdata_section].PointerToRawData
            + raw_data_offset;
  
    // Set the file pointer to the calculated position.

    SetFilePointer(file, (LONG) file_pointer, NULL, FILE_BEGIN);

    // Read the debug directories.

    ReadFile(file, info->DebugDirectories,
             (DWORD) num_debug_dirs * sizeof(DEBUG_DIRECTORY),
             &bytes_transferred,
             NULL);

  }
  else {
    // Just do a brute-force search of the sections, and try to find
    // one whose RVA matches that of the directory table.

    DWORD rva_header, raw_data_offset, file_pointer, rva_end;
    int hit_section, candidate_section;
    BOOL section_found = FALSE;

    candidate_section = 0;

    while ((candidate_section < info->CoffHeader.NumberOfSections) &&
           !section_found) {

      rva_header = (info->SectionHeaders)[candidate_section].RVA;
      rva_end = 
        rva_header + 
         (info->SectionHeaders)[candidate_section].SizeOfRawData - 1;

      if ((rva_header <= rva_debug_dirs) &&
          (rva_debug_dirs <= rva_end)) {
        hit_section = candidate_section;
        section_found = TRUE;
      }
      else {
        candidate_section++;
      }
    }

    // If we found the dirs in a section, then read them.
 
    if (section_found) {

      raw_data_offset = rva_debug_dirs - rva_header;
      file_pointer =
         (info->SectionHeaders)[hit_section].PointerToRawData
             + raw_data_offset;

      // Set the file pointer to the calculated position.

      SetFilePointer(file, (LONG) file_pointer, NULL, FILE_BEGIN);

      // Read the debug directories.

      ReadFile(file, info->DebugDirectories,
               (DWORD) num_debug_dirs * sizeof(DEBUG_DIRECTORY),
               &bytes_transferred,
               NULL);

    }
  }
  (info->IPCacheIndex) = 1;

  // Now read the COFF symbol table itself, as long as there is one, and
  // that we _need_ to. (There is no need to store all COFF symbols if we
  // already have all the codeview).

  // There is a special case where the COFF symbol table was already
  // embedded in the structure returned by MapDebugInformation (in
  // DbgHelp). If this is true, then the module's DebugType field will
  // already hold the value COFF_IMAGE, and we must do nothing here.

  if (module->DebugType == COFF_IMAGE) {
    // This case intentionally left blank. :-)
  }
  else if ((info->CoffHeader).PointerToSymbolTable == 0) {
    // If there's no COFF symbol table to read from the image, we're
    // screwed on this path so just return.
    (info->NumberOfCoffSymbols) = 0;
    (info->CoffSymbolTable) = NULL;
    (info->CoffStringTable) = NULL;
  }
  else if (module->DebugType != NONE) {
    // Don't read a COFF table from the image if we have some other kind of
    // debugging information already available to us. (COFF is basically
    // a "last resort").
    (info->NumberOfCoffSymbols) = 0;
    (info->CoffSymbolTable) = NULL;
    (info->CoffStringTable) = NULL;
  }
  else {

    module->DebugType = COFF_IMAGE;
    info->CoffTableExplicitlyAllocated = TRUE;

    SetFilePointer(file, (LONG) ((info->CoffHeader).PointerToSymbolTable),
                   NULL, FILE_BEGIN);

    (info->CoffSymbolTable) = 
       (BYTE*) malloc ((info->NumberOfCoffSymbols) * SIZEOF_SYMBOL_ENTRY);

    ReadFile(file,
             info->CoffSymbolTable,
             (info->NumberOfCoffSymbols) * SIZEOF_SYMBOL_ENTRY,
             &bytes_transferred,
             NULL);

    SetFilePointer(file, (LONG) ((info->CoffHeader).PointerToSymbolTable),
                   NULL, FILE_BEGIN);
    SetFilePointer(file, 
                   (LONG) ((info->NumberOfCoffSymbols) * SIZEOF_SYMBOL_ENTRY),
                   NULL, 
                   FILE_CURRENT);

    // The Length of the string table.

    ReadFile(file,
             &string_table_length,
             sizeof(DWORD),
             &bytes_transferred,
             NULL);

    (info->CoffStringTable) = 
      (char*) malloc(string_table_length * sizeof(char));

    // And the COFF strings table itself.

    ReadFile(file,
             info->CoffStringTable,
             string_table_length,
             &bytes_transferred,
             NULL);

  }
}


// release_library_coff_map
// Disposes of all storage and resources allocated to hold the COFF
// symbol table for an image file.

void release_library_coff_map(LPDBGPROCESS process, LPDBGLIBRARY module)
{
  if (module->ImageInformation.CoffSymbolTable != NULL) {
    if (module->ImageInformation.CoffTableExplicitlyAllocated)
      free(module->ImageInformation.CoffSymbolTable);
    module->ImageInformation.CoffSymbolTable = NULL;
  }
  if (module->ImageInformation.CoffStringTable != NULL) {
    if (module->ImageInformation.CoffTableExplicitlyAllocated)
      free(module->ImageInformation.CoffStringTable);
    module->ImageInformation.CoffStringTable = NULL;
  }
}

BOOL find_symbol_in_coff_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module, 
   BYTE name_length, 
   char *name, 
   TARGET_ADDRESS *addr, SYMBOL_CLASSIFICATION *type,
   NUBINT *is_function,
   TARGET_ADDRESS *debug_start,
   TARGET_ADDRESS *debug_end,
   NUBINT *language,
   TARGET_ADDRESS *last_address)
{

  DWORD             i = 0;
  DWORD             limit = module->ImageInformation.NumberOfCoffSymbols;
  BYTE              *strings = module->ImageInformation.CoffStringTable;
  DWORD             symbol_base;
  DWORD             ds, de, fin;
  COFF_SYMBOL_ENTRY *this_symbol;

  // Initialize the return values.

  (*addr) = (TARGET_ADDRESS) NULL;
  (*type) = 0;
  (*is_function) = 0;
  (*debug_start) = (TARGET_ADDRESS) NULL;
  (*debug_end) = (TARGET_ADDRESS) NULL;
  (*language) = 0;
  (*last_address) = (TARGET_ADDRESS) NULL;

  // If there is no COFF symbol table loaded for this image, then just
  // take an early negative exit.

  if (module->ImageInformation.CoffSymbolTable == NULL)
    return (FALSE);

  // Otherwise, iterate over all standard COFF symbols in the table,
  // skipping auxiliary definitions. Compare names exhaustively until
  // we find a match, or can prove there are no matches.

  symbol_base = (DWORD) (module->ImageInformation.CoffSymbolTable);

  while (i < limit) {
    this_symbol = (COFF_SYMBOL_ENTRY*) (symbol_base + i * SIZEOF_SYMBOL_ENTRY);
    if (compare_coff_names(this_symbol, strings, name_length, name)) {

      // This isn't CodeView, so the symbol's language has to be decided
      // in a very simple-minded manor. Assume anything that doesn't start
      // with an underscore to be a dylan symbol, and anything that does
      // to be a C symbol.

      if (name[0] == '_')
        (*language) = (NUBINT) 0; 
      else
        (*language) = (NUBINT) 9;

      // Calculate the address
      (*addr) = (TARGET_ADDRESS) calculate_coff_address(module, this_symbol);

      // If it's a function, fill in the other optional values
      if (calculate_function_debug_offsets
                (module, this_symbol, &ds, &de, &fin)) {
        (*is_function) = (NUBINT) 1;
        (*debug_start) = (TARGET_ADDRESS) ds;
        (*debug_end) = (TARGET_ADDRESS) de;
        (*last_address) = (TARGET_ADDRESS) fin;
      }

      // And exit positive.
      return (TRUE);
    }
    else {
      i += 1 + (DWORD) (this_symbol->NumberOfAuxSymbols);
    }
  }  
  return (FALSE);
}


LOOKUP_TABLE *nearest_symbols_in_coff_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD IP)
{
  return(NULL);
}


BOOL closest_symbol_in_coff_map
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   TARGET_ADDRESS address,
   NUBINT *name_length,
   TARGET_ADDRESS *actual_address, SYMBOL_CLASSIFICATION *type,
   NUBINT *is_function,
   TARGET_ADDRESS *debug_start,
   TARGET_ADDRESS *debug_end,
   NUBINT *language,
   TARGET_ADDRESS *last_address)
{
  DWORD               target = (DWORD) address;
  COFF_SYMBOL_ENTRY   *this_symbol;
  COFF_SYMBOL_ENTRY   *closest_so_far = NULL;
  DWORD               closest_address = 0;
  DWORD               i = 0;
  DWORD               this_address;
  DWORD               limit = module->ImageInformation.NumberOfCoffSymbols;
  BYTE                *strings = module->ImageInformation.CoffStringTable;
  DWORD               symbol_base;
  DWORD               ds, de, fin;
  BOOL                keep_going = TRUE;

  // Initialize the return values.

  (*actual_address) = (TARGET_ADDRESS) NULL;
  (*type) = 0;
  (*is_function) = 0;
  (*debug_start) = (TARGET_ADDRESS) NULL;
  (*debug_end) = (TARGET_ADDRESS) NULL;
  (*language) = 0;
  (*last_address) = (TARGET_ADDRESS) NULL;
  (*name_length) = 0;

  // If there is no COFF symbol table loaded for this image, then just
  // take an early negative exit.

  if (module->ImageInformation.CoffSymbolTable == NULL)
    return (FALSE);

  // Otherwise, iterate over all standard COFF symbols in the table,
  // skipping auxiliary definitions. Compare names exhaustively until
  // we find a match, or can prove there are no matches.

  symbol_base = (DWORD) (module->ImageInformation.CoffSymbolTable);

  while ((i < limit) && (keep_going)) {
    this_symbol = (COFF_SYMBOL_ENTRY*) (symbol_base + i * SIZEOF_SYMBOL_ENTRY);
    if (is_non_data_record(this_symbol, strings)) {
      i += 1 + (DWORD) (this_symbol->NumberOfAuxSymbols);
      // Ignore this symbol
      continue;
    }
    this_address = calculate_coff_address(module, this_symbol);
    if (this_address == target) {
      closest_so_far = this_symbol;
      closest_address = this_address;
      keep_going = FALSE;
    }
    else if ((this_address < target) && (this_address > closest_address)) {
      closest_so_far = this_symbol;
      closest_address = this_address;
    }
    i += 1 + (DWORD) (this_symbol->NumberOfAuxSymbols);
  }

  if (closest_so_far != NULL) {
    (*actual_address) = (TARGET_ADDRESS) closest_address;

    process->NameCache = 
      calculate_coff_symbol_name_pointer(closest_so_far, strings);
    (*name_length) =
      calculate_coff_symbol_name_length(closest_so_far, strings);

    if (process->NameCache[0] == '_')
      (*language) = (NUBINT) 0;
    else
      (*language) = (NUBINT) 9;

    // If it's a function, fill in the other optional values
    if (calculate_function_debug_offsets
              (module, this_symbol, &ds, &de, &fin)) {
      (*is_function) = (NUBINT) 1;
      (*debug_start) = (TARGET_ADDRESS) ds;
      (*debug_end) = (TARGET_ADDRESS) de;
      (*last_address) = (TARGET_ADDRESS) fin;
    }

    // And exit positive.
    return (TRUE);
  }
  else {
    return (FALSE);
  }
}


/////
///// Accessor utilities (on COFF_SYMBOL_ENTRY)
/////


COFF_SYMBOL_ENTRY *following_coff_symbol (COFF_SYMBOL_ENTRY *x)
{
  return ((COFF_SYMBOL_ENTRY *) (((DWORD) x) + SIZEOF_SYMBOL_ENTRY));
}

COFF_SYMBOL_ENTRY *preceding_coff_symbol (COFF_SYMBOL_ENTRY *x)
{
  return ((COFF_SYMBOL_ENTRY *) (((DWORD) x) - SIZEOF_SYMBOL_ENTRY));
}

COFF_SYMBOL_ENTRY *following_standard_coff_symbol (COFF_SYMBOL_ENTRY *x)
{
  DWORD  aux = (DWORD) (x->NumberOfAuxSymbols);
  return ((COFF_SYMBOL_ENTRY *) (((DWORD) x) + SIZEOF_SYMBOL_ENTRY * aux));
}

BOOL is_function_defining_record (COFF_SYMBOL_ENTRY *x)
{
  return ((x->Type == COFF_SYMBOL_TYPE_FUNCTION) && (x->StorageClass == 0x2));
}

BOOL is_non_data_record (COFF_SYMBOL_ENTRY *x, BYTE *strings)
{
  if (x->Name.Name[0] == '.') {
    return(TRUE);
  }
  else {
    DWORD  i = 0;
    BYTE   l = 0;
    char  *n = NULL;
    for (i = 0; i < NUMBER_OF_EXPLICIT_NON_DATA_SYMBOLS; i++) {
      l = explicit_non_data_symbols[i].NameLength;
      n = explicit_non_data_symbols[i].Name;
      if (compare_coff_names(x, strings, l, n)) {
        return(TRUE);
      }
    }
    return(FALSE);
  }
}

BOOL is_begin_function_record (COFF_SYMBOL_ENTRY *x)
{
  return ((x->Name.Name[0] == '.') &&
          (x->Name.Name[1] == 'b') &&
          (x->Name.Name[2] == 'f') &&
          (x->Name.Name[3] == '\0'));
}

BOOL is_end_function_record (COFF_SYMBOL_ENTRY *x)
{
  return ((x->Name.Name[0] == '.') &&
          (x->Name.Name[1] == 'e') &&
          (x->Name.Name[2] == 'f') &&
          (x->Name.Name[3] == '\0'));
}

BOOL is_lines_in_function_record (COFF_SYMBOL_ENTRY *x)
{
  return ((x->Name.Name[0] == '.') &&
          (x->Name.Name[1] == 'l') &&
          (x->Name.Name[2] == 'f') &&
          (x->Name.Name[3] == '\0'));
}

/////
///// Calculations on sections, offsets and base addresses.
/////

BOOL calculate_function_debug_offsets
    (LPDBGLIBRARY module, COFF_SYMBOL_ENTRY *entry,
     DWORD *function_debug_start, DWORD *function_debug_end,
     DWORD *function_final)
{
  // Initially, set safe values.
  (*function_debug_start) = calculate_coff_address(module, entry);
  (*function_debug_end) = (*function_debug_start);
  (*function_final) = (*function_debug_start);

  if (is_function_defining_record(entry)) {
    COFF_SYMBOL_ENTRY *fdaux = following_coff_symbol(entry);
    COFF_SYMBOL_ENTRY *bf = following_coff_symbol(fdaux);
    COFF_SYMBOL_ENTRY *bfaux = following_coff_symbol(bf);
    COFF_SYMBOL_ENTRY *lf = following_coff_symbol(bfaux);
    COFF_SYMBOL_ENTRY *ef = following_coff_symbol(lf);
    COFF_SYMBOL_ENTRY *efaux = following_coff_symbol(ef);

    // Sanity check that we're looking at the right kinds of records.
    if (is_begin_function_record(bf) &&
        is_end_function_record(ef) &&
        is_lines_in_function_record(lf)) {
      (*function_debug_end) = 
        (*function_debug_start) + 
          ((COFF_AUX_FUNCTION_DEFINITION*) fdaux)->TotalSize;
      (*function_final) = (*function_debug_end);
    }
    return (TRUE);
  }
  else {
    return (FALSE);
  }
}

char* calculate_coff_symbol_name_pointer
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings)
{
  if (symbol->Name.Pointer.NullDWord == 0x00000000) {
    DWORD string_table_offset = symbol->Name.Pointer.StringTableOffset;
    DWORD table_base = (DWORD) strings;
    char  *name = (char*) (table_base + (string_table_offset - 4));
    return(name);
  }
  else {
    char *name = symbol->Name.Name;
    return(name);
  }
}

int calculate_coff_symbol_name_length
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings)
{
  int length = 0;
  if (symbol->Name.Pointer.NullDWord == 0x00000000) {
    DWORD string_table_offset = symbol->Name.Pointer.StringTableOffset;
    DWORD table_base = (DWORD) strings;
    char  *name = (char*) (table_base + (string_table_offset - 4));
    while (name[length] != '\0') length++;
  }
  else {
    char *name = symbol->Name.Name;
    while ((length < 8) && (name[length] != '\0')) length++;
  }
  return(length);
}

void copy_coff_symbol_name_into_buffer
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings, int bufsize, char *buf)
{
  int length = 0;
  if (symbol->Name.Pointer.NullDWord == 0x00000000) {
    DWORD string_table_offset = symbol->Name.Pointer.StringTableOffset;
    DWORD table_base = (DWORD) strings;
    char  *name = (char*) (table_base + (string_table_offset - 4));
    while ((name[length] != '\0') && (length < bufsize)) {
      buf[length] = name[length];
      length++;
    }
  }
  else {
    char *name = symbol->Name.Name;
    while ((length < 8) && (name[length] != '\0') && (length < bufsize)) {
      buf[length] = name[length];
      length++;
    }
  }
}

DWORD calculate_coff_address
    (LPDBGLIBRARY module, COFF_SYMBOL_ENTRY *x)
{
  return(calculate_segmented_address(module, x->SectionNumber, x->Value));
}

DWORD calculate_segmented_address
    (LPDBGLIBRARY module, WORD segment, DWORD offset)
{
  DWORD image_preferred_base = 
    module->ImageInformation.OptionalHeader.WindowsNTFields.ImageBase;
  DWORD image_loaded_base =
    module->ImageInformation.ImageBase;
  WORD image_section_count =
    module->ImageInformation.CoffHeader.NumberOfSections;

  // COFF Symbol tables, once they are created inside IMAGE files,
  // are stored with addresses relative to the base address of the
  // image. The exception is the GNU linker, which actually stores
  // absolute addresses. This is slightly anti-social of it, since
  // if the DLL is re-based at load time, the address in the table
  // becomes invalid. This calculation tries to detect this, and
  // make a correction.

  // The standard case (COFF tables produced by the Microsoft linker)
  // is where the loaded base just has to be added to the offset as
  // stored in the table.

  if (offset >= image_preferred_base) { // The offset is already the address
    if (image_preferred_base == image_loaded_base) {
      // No rebasing occurred, so the offset is already the correct
      // 32-bit address.
      return(offset);
    }
    else {
      // The image was rebased, therefore the pre-added base is wrong, and
      // has to be subtracted out. The actual base is then added, of course.
      return(offset - image_preferred_base + image_loaded_base);
    }
  }
  else {
    // This is the more standard case, where the offset is indeed just an
    // offset, so the image base has to be added
    return(offset + image_loaded_base);
  }
}

/////
///// Iterator utilities (may not be useful!)
/////

// iterate_coff_symbols
// Applies a callback function to all symbols in a module's COFF symbol
// table.
// The callback function takes the following arguments:

//    LPDBGPROCESS         The process descriptor for the debugee
//    LPDBGLIBRARY         The DLL descriptor for the module's image.
//    DWORD                An unsigned index of the COFF symbol.
//    COFF_SYMBOL_ENTRY*   A pointer to the COFF symbol record itself.
//    BYTE*                A pointer to the COFF string table.

// and should return an INT, which should be one of these constants:
//    COFF_ITERATOR_NEXT_SYMBOL     
//    COFF_ITERATOR_NEXT_STANDARD_SYMBOL
//    COFF_ITERATOR_ABORT

// (The iteration will also abort if the callback result matches none of these)

void iterate_coff_symbols
    (LPDBGPROCESS process, LPDBGLIBRARY module,
     int (*f) (LPDBGPROCESS, LPDBGLIBRARY, DWORD, COFF_SYMBOL_ENTRY*, BYTE*))
{
  if (module->ImageInformation.CoffSymbolTable != NULL) {
    DWORD                i = 0;
    COFF_SYMBOL_ENTRY   *this_symbol;
    BOOL                 still_going = TRUE;
    BYTE                *strings = module->ImageInformation.CoffStringTable;
    DWORD                symbol_base = 
       (DWORD) module->ImageInformation.CoffSymbolTable;
    int                  callback_result;

    while (still_going && 
           (i < (module->ImageInformation.NumberOfCoffSymbols))) {
      this_symbol = (COFF_SYMBOL_ENTRY*) 
        (symbol_base + (i * SIZEOF_SYMBOL_ENTRY));
      callback_result = (*f) (process, module, i, this_symbol, strings);
      switch (callback_result) {
        case COFF_ITERATOR_NEXT_SYMBOL:
          i++;
        case COFF_ITERATOR_NEXT_STANDARD_SYMBOL:
          i += (DWORD) (this_symbol->NumberOfAuxSymbols + 1);
        default:
          i++;
          still_going = FALSE;
      }
    }
  }
}


/////
///// Low-level comparisons on names and addresses.
/////

BOOL compare_coff_names
  (COFF_SYMBOL_ENTRY *symbol, BYTE *strings, BYTE length, char *candidate)
{
  if ((length < 1) || (symbol == NULL) || (strings == NULL)) {
    return (FALSE);
  }
  else if (symbol->Name.Pointer.NullDWord == 0x00000000) {
    DWORD string_table_offset = symbol->Name.Pointer.StringTableOffset;
    DWORD table_base = (DWORD) strings;
    char *name = (char*) (table_base + (string_table_offset - 4));
    BYTE i = 0;
    BYTE j = 0;
    BYTE imax = length - 1;
    if ((name[i] == '_') && (candidate[j] != '_'))
      {i++; imax++;}
    while (i <= imax) {
      if (name[i] == '\0') return (FALSE);
      else if (name[i] != candidate[j]) return (FALSE);
      else {i++; j++;}
    }
    if (name[i] == '\0') return (TRUE); else return (FALSE);
  }
  else if (length > 8) { // The COFF symbol has a short name!
                         // If the candidate name is longer than 8 chars,
                         // then we can't possibly have a match.
    return (FALSE);
  }
  else {
    BYTE  i = 0;
    BYTE  j = 0;
    BYTE  imax = length - 1;
    char *name = symbol->Name.Name;
    if ((name[i] == '_') && (candidate[j] != '_'))
      {i++; imax++;}
    while (i <= imax) {
      if (name[i] == '\0') return (FALSE);
      else if (name[i] != candidate[j]) return (FALSE);
      else {i++; j++;}
    }
    if ((name[i] == '\0') || (i == 8)) return (TRUE); else return (FALSE);
  }
}

/*

/////
///// DEBUG CODE
/////

void print_coff_symbol_name
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings)
{
  if (symbol->Name.Pointer.NullDWord == 0x00000000) {
    DWORD string_table_offset = symbol->Name.Pointer.StringTableOffset;
    DWORD table_base = (DWORD) strings;
    char  *name = (char*) (table_base + (string_table_offset - 4));
    printf("%s", name);
  }
  else {
    DWORD i = 0;
    char *name = symbol->Name.Name;
    while ((i < 8) && (name[i] != '\0')) printf("%c", name[i++]);
  }
}

void debug_print_coff_symbols
    (LPDBGLIBRARY module)
{
  if (module->ImageInformation.CoffSymbolTable == NULL) {
    printf("No symbol table available for this DLL.\n");
  }
  else {
    DWORD                i = 0;
    COFF_SYMBOL_ENTRY   *this_symbol;
    BYTE                *strings = module->ImageInformation.CoffStringTable;
    DWORD                symbol_base = 
       (DWORD) module->ImageInformation.CoffSymbolTable;
    printf("------ COFF Symbol Table Contents As Read from File.\n\n");
    while (i < (module->ImageInformation.NumberOfCoffSymbols)) {
      this_symbol = (COFF_SYMBOL_ENTRY*) 
        (symbol_base + (i * SIZEOF_SYMBOL_ENTRY));
      printf("  Symbol[%4d] = ", i);
      print_coff_symbol_name(this_symbol, strings);
      printf("\n");
      printf ("Type = 0x%x, Section = %d, Offset = 0x%x, Storage = 0x%x\n",
             this_symbol->Type, this_symbol->SectionNumber,
             this_symbol->Value, this_symbol->StorageClass);
      i += (DWORD) (this_symbol->NumberOfAuxSymbols + 1);
    }
    printf("------------------------------------------------------------\n\n");
  }
}

*/
