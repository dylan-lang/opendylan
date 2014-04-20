/* ********************************************************************** */
/* ** debug_map.c                                                      ** */
/* ** Functions for obtaining and navigating memory-mapped debug       ** */
/* ** information.                                                     ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* **                                 All Rights Reserved.             ** */
/* ********************************************************************** */

#include "nub-core.h"

LOOKUP_TABLE *new_lookup_table(LPDBGPROCESS process, LPDBGLIBRARY module);
void add_lookup_table_entry (LOOKUP_TABLE *table, BYTE lookup_type, void *pointer, BYTE lang);

void test_print_type_information (LPDBGPROCESS process, LPDBGLIBRARY module);
// Will soon vanish.

char *readable_text_for_leaf_code (WORD code);
// This header should go elsewhere.

DWORD cv_symbol_address 
  (LPDBGPROCESS process, LPDBGLIBRARY module, 
   DWORD FP, CV_HEADER *sym);
// This function does its best to find the address of the symbol's
// definition regardless of the symbol type. This will return 0 if the
// symbol does not have a sensible address (eg, it is a compile flag
// symbol). If the symbol is frame-pointer relative, the supplied FP
// argument is consulted to find the address, otherwise FP is ignored,
// and you can safely pass a value of 0.

DWORD size_of_subsection_prolog (WORD subsection_type);
// Returns the number of bytes that precede "useful" information in
// subsection formats. These "prologs" contain data that might be useful
// in the future, but we aren't interested in them for the time being.

BOOL cv_sym_encloses_IP32 
  (LPDBGLIBRARY module, CV_HEADER *sym,
   DWORD IP);

BOOL cv_public_sym_encloses_IP32 
  (PIMAGE_DEBUG_INFORMATION info,
   CV_HEADER *last_symH, CV_HEADER *symH,
   DWORD IP);

BOOL should_be_in_all_symbols_scan (CV_HEADER *sym);
// Returns TRUE if the given CodeView symbols is of interest for a
// call to do-all-symbols (and related) - ie, it's a variable or procedure
// as opposed to a compiler flag or object file name.

int equal_symbolic_names (BYTE l1, BYTE l2, char *name1, char *name2);
// Returns non-zero if the two length-prefixed names are equal

int equal_cv_names (CV_HEADER *rec, BYTE l, char *name);
// Returns non-zero if the name of the symbolic record is equal to the
// given length-prefixed name.

void release_library_coff_map(LPDBGPROCESS process, LPDBGLIBRARY module);
// Imported from coff_map.c.
// Disposes of all memory that was allocated to hold COFF symbol and string
// tables in the image.

DWORD number_of_codeview_subsections (PIMAGE_DEBUG_INFORMATION info)
{
  SS_DIR_HEADER *ss_dir_header;
  BASE_POINTER  *base_pointer;

  if (info == NULL) 
    return (0);
  base_pointer = (BASE_POINTER*) (info->CodeViewSymbols);
  if (base_pointer == NULL) return (0);
  ss_dir_header = (SS_DIR_HEADER*) (CVMAP(info, base_pointer->lfoBase));

  return (ss_dir_header->cDir);
}


WORD codeview_subsection_type (PIMAGE_DEBUG_INFORMATION info, DWORD subsection)
{
  SS_DIR_HEADER     *ss_dir_header;
  SS_DIR_ENTRY      *ss_dir_entry;
  BASE_POINTER      *base_pointer;

  if (info == NULL) return (0);
  base_pointer = (BASE_POINTER*) (info->CodeViewSymbols);
  if (base_pointer == NULL) return (0);
  ss_dir_header = (SS_DIR_HEADER*) (CVMAP(info, 0));
  ss_dir_entry 
    = (SS_DIR_ENTRY*) 
        (CVMAP(info, (base_pointer->lfoBase + sizeof(SS_DIR_HEADER))));

  return (ss_dir_entry[subsection].subsection);
}


DWORD codeview_subsection_offset 
  (PIMAGE_DEBUG_INFORMATION info, DWORD subsection)
{
  SS_DIR_HEADER     *ss_dir_header;
  SS_DIR_ENTRY      *ss_dir_entry;
  BASE_POINTER      *base_pointer;

   if (info == NULL) return (0);
   base_pointer = (BASE_POINTER*) (info->CodeViewSymbols);
   if (base_pointer == NULL) return (0);
   ss_dir_header = (SS_DIR_HEADER*) (CVMAP(info, base_pointer->lfoBase));
   ss_dir_entry 
     = (SS_DIR_ENTRY*) 
         (CVMAP(info, (base_pointer->lfoBase + sizeof(SS_DIR_HEADER))));

 return (ss_dir_entry[subsection].lfo);
}


DWORD codeview_subsection_size 
   (PIMAGE_DEBUG_INFORMATION info, DWORD subsection)
{
  SS_DIR_HEADER     *ss_dir_header;
  SS_DIR_ENTRY      *ss_dir_entry;
  BASE_POINTER      *base_pointer;

  if (info == NULL) return (0);
  base_pointer = (BASE_POINTER*) (info->CodeViewSymbols);
  if (base_pointer == NULL) return (0);
  ss_dir_header = (SS_DIR_HEADER*) (CVMAP(info, base_pointer->lfoBase));
  ss_dir_entry 
    = (SS_DIR_ENTRY*) 
        (CVMAP(info, (base_pointer->lfoBase + sizeof(SS_DIR_HEADER))));

  return (ss_dir_entry[subsection].cb);
}


BOOL codeview_present_in_image (PIMAGE_DEBUG_INFORMATION info)
{
  BASE_POINTER      *base_pointer;
  char              *sig;

  if (info == NULL) return (FALSE);
  base_pointer = (BASE_POINTER*) (info->CodeViewSymbols);
  if (base_pointer == NULL) return (FALSE);
  sig = (base_pointer->DebugSignature);

  if (info->SizeOfCodeViewSymbols == 0)
    return (FALSE);
 
  if ((sig[0] == 'N') && (sig[1] == 'B') &&
      (sig[2] == '0') && (sig[3] == '9'))
    return (TRUE);
  else if ((sig[0] == 'N') && (sig[1] == 'B') &&
           (sig[2] == '1') && (sig[3] == '1'))
    return(TRUE);
  else
    return (FALSE);
}

DWORD size_of_subsection_prolog (WORD subsection_type)
{
  switch (subsection_type) {

  case SST_ALIGN_SYM:
  case SST_SYMBOLS:
  case SST_PUBLIC_SYM:
    return (sizeof(DWORD));
    break;

  case SST_GLOBAL_SYM:
  case SST_GLOBAL_PUB:
  case SST_STATIC_SYM:
    return (sizeof(HASH_INFO_RECORD));
    break;

  default:
    return (0);
  }
}


BOOL codeview_present_in_pdb (PIMAGE_DEBUG_INFORMATION info)
{
  BASE_POINTER  *base_pointer;
  char          *sig;

  if (info == NULL) return (FALSE);
  base_pointer = (BASE_POINTER*) (info->CodeViewSymbols);
  if (base_pointer == NULL) return (FALSE);
  sig = (base_pointer->DebugSignature);

  // If there is no mapped codeview information at all, then it doesn't
  // matter where it is.

  if (info->SizeOfCodeViewSymbols == 0)
    return (FALSE);
 
  // Informal strategy here. The generated signature always seems to
  // be "NB10" when a pdb has been used, except that the Visual Studio .NET
  // linker uses the signature "RSDS".

  if ((sig[0] == 'N') && (sig[1] == 'B') &&
       (sig[2] == '1') && (sig[3] == '0'))
    return (TRUE);
  else if ((sig[0] == 'R') && (sig[1] == 'S') &&
       (sig[2] == 'D') && (sig[3] == 'S'))
    return (TRUE);
  else
    return (FALSE);
}


BOOL coff_table_present_in_image (PIMAGE_DEBUG_INFORMATION info)
{
  if (info->SizeOfCoffSymbols == 0)
    return (FALSE);
  else
    return (TRUE);
}

void per_source_table_info_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD subsection,
   WORD *num_files)
{
  PIMAGE_DEBUG_INFORMATION      info = module->DebugMap;

  // Get the offset and subsection type.

  DWORD subsection_offset = codeview_subsection_offset(info, subsection);

  WORD  subsection_type = codeview_subsection_type(info, subsection);

  // Point us into the map for this subsection.

  DWORD source_table_base = CVMAP(info, subsection_offset);

  // Mapping of per-source-table data

  WORD *cFile = (WORD*) source_table_base;

  WORD *cSeg = (WORD*) (source_table_base + sizeof(WORD));

  DWORD *baseSrcFile = (DWORD*) (source_table_base + 2 * sizeof(WORD));

  OFFSET_PAIR *start_end = (OFFSET_PAIR*) (source_table_base +
                                           2 * sizeof(WORD) +
                                          (*cFile) * sizeof(DWORD));

  WORD *seg = (WORD*) (source_table_base +
                       2 * sizeof(WORD) +
                       (*cFile) * sizeof(DWORD) +
                       (*cSeg) * sizeof(OFFSET_PAIR));

  // First, a sanity check to make sure we're not trying to delve into anything
  // that isn't a sstSrcModule section.

  if (subsection_type != SST_SRC_MODULE) {
    // This is an error.
    return;
  }

  (*num_files) = (*cFile);

}


void per_file_info_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module,
   DWORD subsection, WORD file,
   WORD *num_segments, BYTE *name_length,
   char **name_ptr)
{
  PIMAGE_DEBUG_INFORMATION      info = module->DebugMap;

  // Get the offset and subsection type.

  DWORD subsection_offset = codeview_subsection_offset(info, subsection);

  WORD  subsection_type = codeview_subsection_type(info, subsection);

  // Point us into the map for this subsection.

  DWORD source_table_base = CVMAP(info, subsection_offset);

  // Mapping of per-source-table data

  WORD *cFile = (WORD*) source_table_base;

  WORD *cSeg = (WORD*) (source_table_base + sizeof(WORD));

  DWORD *baseSrcFile = (DWORD*) (source_table_base + 2 * sizeof(WORD));

  OFFSET_PAIR *start_end = (OFFSET_PAIR*) (source_table_base +
                                           2 * sizeof(WORD) +
                                           (*cFile) * sizeof(DWORD));

  WORD *seg = (WORD*) (source_table_base +
                       2 * sizeof(WORD) +
                       (*cFile) * sizeof(DWORD) +
                       (*cSeg) * sizeof(OFFSET_PAIR));

  // Mapping of per-file-table data for the required file.

  DWORD file_table_base = source_table_base + baseSrcFile[file];

  WORD *cSegFile = (WORD*) file_table_base;

  WORD *pad = (WORD*) (file_table_base + sizeof(WORD));

  DWORD *baseSrcLn = (DWORD*) (file_table_base + 2 * sizeof(WORD));

  OFFSET_PAIR *start_endFile = (OFFSET_PAIR*) (file_table_base +
                                               2 * sizeof(WORD) +
                                               (*cSegFile) * sizeof(DWORD));

  BYTE *cbName = (BYTE*) (file_table_base +
                          2 * sizeof(WORD) +
                          (*cSegFile) * sizeof(DWORD) +
                          (*cSegFile) * sizeof(OFFSET_PAIR));

  char *name = (char *) (file_table_base +
                         2 * sizeof(WORD) +
                         (*cSegFile) * sizeof(DWORD) +
                         (*cSegFile) * sizeof(OFFSET_PAIR) +
                         sizeof(BYTE));

 // First, a sanity check to make sure we're not trying to delve into anything
 // that isn't a sstSrcModule section.

  if (subsection_type != SST_SRC_MODULE) {
    // This is an error.
    return;
  }

  (*num_segments) = (*cSegFile);
  (*name_length) = (*cbName);
  (*name_ptr) = name;
}


void per_segment_info_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module,
   DWORD subsection, WORD file, WORD segment,
   WORD *segment_index, WORD *num_pairs)
{
  PIMAGE_DEBUG_INFORMATION      info = module->DebugMap;

  // Get the offset and subsection type.

  DWORD subsection_offset = codeview_subsection_offset(info, subsection);

  WORD  subsection_type = codeview_subsection_type(info, subsection);

  // Point us into the map for this subsection.

  DWORD source_table_base = CVMAP(info, subsection_offset);

  // Mapping of per-source-table data

  WORD *cFile = (WORD*) source_table_base;

  WORD *cSeg = (WORD*) (source_table_base + sizeof(WORD));

  DWORD *baseSrcFile = (DWORD*) (source_table_base + 2 * sizeof(WORD));

  OFFSET_PAIR *start_end = (OFFSET_PAIR*) (source_table_base +
                                           2 * sizeof(WORD) +
                                           (*cFile) * sizeof(DWORD));

  WORD *seg = (WORD*) (source_table_base +
                       2 * sizeof(WORD) +
                       (*cFile) * sizeof(DWORD) +
                       (*cSeg) * sizeof(OFFSET_PAIR));

  // Mapping of per-file-table data for the required file.

  DWORD file_table_base = source_table_base + baseSrcFile[file];

  WORD *cSegFile = (WORD*) file_table_base;

  WORD *pad = (WORD*) (file_table_base + sizeof(WORD));

  DWORD *baseSrcLn = (DWORD*) (file_table_base + 2 * sizeof(WORD));

  OFFSET_PAIR *start_endFile = (OFFSET_PAIR*) (file_table_base +
                                               2 * sizeof(WORD) +
                                               (*cSegFile) * sizeof(DWORD));

 BYTE *cbName = (BYTE*) (file_table_base +
                         2 * sizeof(WORD) +
                         (*cSegFile) * sizeof(DWORD) +
                         (*cSegFile) * sizeof(OFFSET_PAIR));

 char *name = (char *) (file_table_base +
                        2 * sizeof(WORD) +
                        (*cSegFile) * sizeof(DWORD) +
                        (*cSegFile) * sizeof(OFFSET_PAIR) +
                        sizeof(BYTE));

  // Mapping of per-segment-table data for the required segment.

  DWORD segment_table_base = source_table_base + baseSrcLn[segment];

  WORD *seg_index = (WORD*) segment_table_base;

  WORD *cPair = (WORD*) (segment_table_base + sizeof(WORD));

  DWORD *offsets = (DWORD*) (segment_table_base + 2 * sizeof(WORD));

  WORD *linenumbers = (WORD*) (segment_table_base + 2 *sizeof(WORD) +
                               (*cPair) * sizeof(DWORD));

  // First, a sanity check to make sure we're not trying to delve into anything
  // that isn't a sstSrcModule section.

  if (subsection_type != SST_SRC_MODULE) {
    // This is an error.
    return;
  }

  (*segment_index) = (*seg_index);
  (*num_pairs) = (*cPair);
}

void per_pair_info_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module,
   DWORD subsection, WORD file, WORD segment,
   WORD pair, DWORD *offset, WORD *linenumber)
{
  PIMAGE_DEBUG_INFORMATION      info = module->DebugMap;

  // Get the offset and subsection type.

  DWORD subsection_offset = codeview_subsection_offset(info, subsection);

  WORD  subsection_type = codeview_subsection_type(info, subsection);

  // Point us into the map for this subsection.

  DWORD source_table_base = CVMAP(info, subsection_offset);

  // Mapping of per-source-table data

  WORD *cFile = (WORD*) source_table_base;

  WORD *cSeg = (WORD*) (source_table_base + sizeof(WORD));

  DWORD *baseSrcFile = (DWORD*) (source_table_base + 2 * sizeof(WORD));

  OFFSET_PAIR *start_end = (OFFSET_PAIR*) (source_table_base +
                                           2 * sizeof(WORD) +
                                           (*cFile) * sizeof(DWORD));

  WORD *seg = (WORD*) (source_table_base +
                       2 * sizeof(WORD) +
                       (*cFile) * sizeof(DWORD) +
                       (*cSeg) * sizeof(OFFSET_PAIR));

  // Mapping of per-file-table data for the required file.

  DWORD file_table_base = source_table_base + baseSrcFile[file];

  WORD *cSegFile = (WORD*) file_table_base;

  WORD *pad = (WORD*) (file_table_base + sizeof(WORD));

  DWORD *baseSrcLn = (DWORD*) (file_table_base + 2 * sizeof(WORD));

  OFFSET_PAIR *start_endFile = (OFFSET_PAIR*) (file_table_base +
                                               2 * sizeof(WORD) +
                                               (*cSegFile) * sizeof(DWORD));

   BYTE *cbName = (BYTE*) (file_table_base +
                           2 * sizeof(WORD) +
                           (*cSegFile) * sizeof(DWORD) +
                           (*cSegFile) * sizeof(OFFSET_PAIR));

  char *name = (char *) (file_table_base +
                         2 * sizeof(WORD) +
                         (*cSegFile) * sizeof(DWORD) +
                         (*cSegFile) * sizeof(OFFSET_PAIR) +
                         sizeof(BYTE));

  // Mapping of per-segment-table data for the required segment.

  DWORD segment_table_base = source_table_base + baseSrcLn[segment];

  WORD *seg_index = (WORD*) segment_table_base;

  WORD *cPair = (WORD*) (segment_table_base + sizeof(WORD));

  DWORD *offsets = (DWORD*) (segment_table_base + 2 * sizeof(WORD));

  WORD *linenumbers = (WORD*) (segment_table_base + 2 *sizeof(WORD) +
                               (*cPair) * sizeof(DWORD));

  // First, a sanity check to make sure we're not trying to delve into anything
  // that isn't a sstSrcModule section.

  if (subsection_type != SST_SRC_MODULE) {
    // This is an error.
    return;
  }

  // We've already done all the hard work of mapping array pointers
  // into place. Now we just do an array access to return the required data.

  (*offset) = offsets[pair];
  (*linenumber) = linenumbers[pair];
}

// This was just pinched from a temporary "unpicker" testing
// algorithm. Could really do with a whole new implementation...

SL_LOOKUP_TABLE *source_locations_from_debug_map
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD start, DWORD end)
{
  DWORD                    max_subsections;
  DWORD                    this_subsection;
  DWORD                    loc_index = 0;
  WORD                     subsection_type;
  PIMAGE_DEBUG_INFORMATION info = module->DebugMap;
  SL_LOOKUP_TABLE          *locations = 
             (SL_LOOKUP_TABLE*) malloc (sizeof (SL_LOOKUP_TABLE));

  // Initialize information in the lookup table.
  (locations->NumberOfEntries) = 0;
  (locations->Process) = process;
  (locations->Module) = module;
  (locations->BaseAddress) = start;

//  printf ("Gathering source locations between 0x%x and 0x%x\n",
//          start, end);

  // Find out how many debug-info sections there are.
  max_subsections = number_of_codeview_subsections (info);

  // Get all relevant information from SST_SRC_MODULE subsections...

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++) {

    subsection_type =
      codeview_subsection_type (info, this_subsection);

    if (subsection_type == SST_SRC_MODULE) {

      WORD num_files, file;

//    printf ("  Subsection %d is a SST_SRC_MODULE table\n", this_subsection);
      per_source_table_info_from_debug_map 
        (process, module, this_subsection, &num_files);

//    printf ("    Examining %d source file entries\n", num_files);
      for (file = 0; file < num_files; file++) {

        WORD    num_segments, segment;
    
        per_file_info_from_debug_map 
          (process, module, this_subsection,
           file, &num_segments, 
           &(locations->FilenameLength),
           &(locations->Filename));

//      printf ("      Examining %d segment table entries\n", num_segments);

        for (segment = 0; segment < num_segments; segment++) {

          WORD segment_index;
          WORD num_pairs, pair;
          DWORD base;
          DWORD first_offset, last_offset;
          WORD first_line, last_line;

          per_segment_info_from_debug_map 
            (process, module, this_subsection, file,
             segment, &segment_index, &num_pairs);

          // "base" holds the absolute address of the first code
          // location in this segment descriptor. Each pair
          // descriptor has an address which is offset from this
          // base, and needs to have this value added onto it
          // (since the addresses that we are comparing against
          // are absolute!)

          base = (module->ImageInformation).ImageBase +
                 info->Sections[segment_index - 1].VirtualAddress;

          // Get the first and last pair to find out if this is
          // any good to us at all. If so, fill up the locations
          // table with all the relevant pairs, and take an early
          // exit from the function.

          per_pair_info_from_debug_map 
            (process, module, this_subsection, file,
             segment, 0,
             &first_offset,
             &first_line);

          per_pair_info_from_debug_map 
            (process, module, this_subsection, file,
              segment, (WORD) (num_pairs - 1),
              &last_offset,
              &last_line);

          if ((((base + first_offset) <= start) &&
              (end <= (base + last_offset))) ||
             ((start <= (base + first_offset)) &&
              ((base + last_offset) <= end))) {

            (locations->SegmentAddress) = base;
            (locations->SegmentIndex) = segment_index;

            for (pair = 0; pair < num_pairs; pair++) {

              DWORD   offset;
              WORD    linenumber;

              per_pair_info_from_debug_map 
                (process, module, this_subsection, file,
                 segment, pair,
                 &offset,
                 &linenumber);

              // Pick up this source location if it is relevant.

              if ((start <= (base + offset)) &&
                  ((base + offset) < end)) {

                locations->Locations[loc_index].Offset = 
                     (base + offset) - start;
                locations->Locations[loc_index].LineNumber = linenumber;
                (locations->NumberOfEntries) ++;
                loc_index++;
                if (loc_index >= MAX_SOURCE_LOCATIONS) {
                  OutputDebugString("Warning: Truncating source locations.");
                  return(locations);
                }
              }
            }
            if ((locations->NumberOfEntries) == 0) {
              free(locations);
              locations = NULL;
            }
            return(locations);
          }
        }
      }
    }
  }
  if ((locations->NumberOfEntries) == 0) {
   free (locations);
   locations = NULL;
  }
  return (locations);
}


LOOKUP_TABLE *all_lexicals_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module,DWORD frame_pointer,
   DWORD IP, DWORD *first, DWORD *last)
{
  LOOKUP_TABLE  *table;
  CV_HEADER     *scope_start;
  DWORD         num_lexicals;
  BYTE          lang;

  (*first) = 1;
  (*last) = 0;

  // Create a new lookup table.

  table = new_lookup_table(process, module);

  // Attempt to find the function from mapped debug information.

  scope_start = IP32_to_cv_sym_from_debug_map (process, module, IP, &lang);

  // Find out how many lexical variables are visible within the scope.

  num_lexicals 
    = pull_lexicals_within_scope (process, module, scope_start,
                                  table, IP);

  // Return the constructed table.

  (*last) = num_lexicals;
  return (table);
}


DWORD pull_lexicals_within_scope 
  (LPDBGPROCESS process, LPDBGLIBRARY module, CV_HEADER *scope_start,
   LOOKUP_TABLE *table, DWORD IP)
{
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;
  DWORD                     scope_start_base = (DWORD) scope_start;
  DWORD                     map_pointer;
  DWORD                     counter = 0;
  DWORD                     level = 0;
  CV_HEADER                 *this_header;
  BOOL                      need_to_keep_looking = TRUE;

  if (scope_start == NULL)
    return (0);

  // Careful, we might not have lexical scope information anyway. Need to
  // take an early exit if this symbol doesn't open a scope.

  if ((scope_start->Index != CV_S_GPROC32) &&
      (scope_start->Index != CV_S_GPROC32_NEW) &&
      (scope_start->Index != CV_S_LPROC32) &&
      (scope_start->Index != CV_S_LPROC32_NEW) &&
      (scope_start->Index != CV_S_GPROC16) &&
      (scope_start->Index != CV_S_LPROC16))

    return (0);

  map_pointer = scope_start_base + (scope_start->Length) + sizeof(WORD);
  this_header = (CV_HEADER*) map_pointer;

  // this_header now points at the CV symbol that follows the scope
  // opening symbol.

  while (need_to_keep_looking) {

    switch (this_header->Index) {

    case CV_S_END:
      // We may have descended into deeper lexical levels. Only stop
      // the search when we read the CV_END record for the outer
      // scope.

      if (level > 0) {
        level--;
        map_pointer = (DWORD) this_header;
        map_pointer += this_header->Length + sizeof(WORD);
        this_header = (CV_HEADER*) map_pointer;
      }
      else {
        need_to_keep_looking = FALSE;
      }
      break;

    case CV_S_BPREL16:
    case CV_S_BPREL32:
    case CV_S_BPREL32_NEW:
    case CV_S_REGISTER:
    case CV_S_REGISTER_NEW:
      counter++;
      add_lookup_table_entry 
        (table, MAPPED_CODEVIEW_SYMBOL, this_header, 0);
      map_pointer = (DWORD) this_header;
      map_pointer += this_header->Length + sizeof(WORD);
      this_header = (CV_HEADER*) map_pointer;
      break;

    case CV_S_LPROC32:
    case CV_S_LPROC32_NEW:
    case CV_S_GPROC32:
    case CV_S_GPROC32_NEW:
    case CV_S_BLOCK32:
      // We're opening a new scope. The lexicals described within this
      // scope are only of interest if the IP falls within it.
      // If it does not, skip over this scope (including any child
      // scopes).

      if (cv_sym_encloses_IP32(module, this_header, IP)) {
        level++;
        map_pointer = (DWORD) this_header;
        map_pointer += this_header->Length + sizeof(WORD);
        this_header = (CV_HEADER*) map_pointer;
      }
      else {
        // We need to skip this inner scope, because it just isn't
        // _in_ scope!
        // #@! We should be able to do this skip in one go, but
        // the pEnd field doesn't provide an offset _from_ this
        // symbol. We shouldn't loose too much using this loop,
        // though.

        BOOL   skipping = TRUE;
        DWORD  temp_level = 0;

        // Skip over the initial scope-opener, and then keep
        // skipping symbols until we reach the matching CV_END
        // symbol.

        map_pointer = (DWORD) this_header;
        map_pointer += this_header->Length + sizeof(WORD);
        this_header = (CV_HEADER*) map_pointer;

        while (skipping) {

          switch (this_header->Index) {

          case CV_S_LPROC32:
          case CV_S_LPROC32_NEW:
          case CV_S_GPROC32:
          case CV_S_GPROC32_NEW:
          case CV_S_BLOCK32:
            temp_level++;
            map_pointer = (DWORD) this_header;
            map_pointer += this_header->Length + sizeof(WORD);
            this_header = (CV_HEADER*) map_pointer;
            break;

          case CV_S_END:
            if (temp_level > 0) 
              temp_level--;
            else
              skipping = FALSE;

            map_pointer = (DWORD) this_header;
            map_pointer += this_header->Length + sizeof(WORD);
            this_header = (CV_HEADER*) map_pointer;
            break;

          default:
            map_pointer = (DWORD) this_header;
            map_pointer += this_header->Length + sizeof(WORD);
            this_header = (CV_HEADER*) map_pointer;
            break;

          }
        }
      }
      break;

    default:
      map_pointer = (DWORD) this_header;
      map_pointer += this_header->Length + sizeof(WORD);
      this_header = (CV_HEADER*) map_pointer;
      break;
    }
  }
  return (counter);
}


LOOKUP_TABLE *static_symbols_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD *first, DWORD *last)
{
  LOOKUP_TABLE              *table = new_lookup_table (process, module);
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;
  DWORD                     this_subsection;
  DWORD                     max_symbols;
  DWORD                     this_symbol;
  CV_HEADER                 *this_record;
  WORD                      subsection_type;
  DWORD                     max_subsections;

  // First of all, find out basic navigation info for the debug map.

  (*first) = 1;
  (*last) = 0;

  max_subsections = number_of_codeview_subsections (info);

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++) {

    subsection_type = 
      codeview_subsection_type (info, this_subsection);

    switch (subsection_type) {

    case SST_SYMBOLS:
    case SST_ALIGN_SYM:
    case SST_STATIC_SYM:
      max_symbols 
        = last_symbol_in_subsection (info, this_subsection);

      for (this_symbol = 0; this_symbol < max_symbols; this_symbol++) {
        // Pull the record for this symbol out of the debug map.
        this_record = 
          get_cv_sym_from_debug_map (process, module,
                                     this_subsection, this_symbol);

        // See if it's of interest to us, and add it to the
        // growing lookup table if it is.

        if ((this_record != NULL) &&
            (should_be_in_all_symbols_scan (this_record))) {

          add_lookup_table_entry (table, MAPPED_CODEVIEW_SYMBOL,
                                  (void*) this_record, 0);
          (*last) ++;
        }
      }
      break;

    default:
      break;
    }
  }
  return (table);
}

BOOL should_be_in_all_symbols_scan (CV_HEADER *sym)
{
  // This function decides whether a symbolic record in Codeview actually
  // is a symbol (ie, some sort of program identifier) as opposed to
  // some auxiliary definition, such as a compiler flag.

  switch (sym->Index) {

  case CV_S_LDATA16:
  case CV_S_GDATA16:
  case CV_S_PUB16:
  case CV_S_LPROC16:
  case CV_S_GPROC16:
  case CV_S_LDATA32:
  case CV_S_LDATA32_NEW:
  case CV_S_GDATA32:
  case CV_S_GDATA32_NEW:
  case CV_S_PUB32:
  case CV_S_PUB32_NEW:
  case CV_S_LPROC32:
  case CV_S_LPROC32_NEW:
  case CV_S_GPROC32:
  case CV_S_GPROC32_NEW:
    return (TRUE);
    break;

  default:
    return (FALSE);
    break;
  }
}

LOOKUP_TABLE *global_symbols_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD *first, DWORD *last)
{
  LOOKUP_TABLE              *table = new_lookup_table (process, module);
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;
  DWORD                     this_subsection;
  DWORD                     max_symbols;
  DWORD                     this_symbol;
  CV_HEADER                 *this_record;
  WORD                      subsection_type;
  DWORD                     max_subsections;

  // First of all, find out basic navigation info for the debug map.

  max_subsections = number_of_codeview_subsections (info);

  (*first) = 1;
  (*last) = 0;

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++) {

    subsection_type = 
      codeview_subsection_type (info, this_subsection);

    switch (subsection_type) {

    case SST_GLOBAL_SYM:
    case SST_GLOBAL_PUB:
      max_symbols = last_symbol_in_subsection (info, this_subsection);
      for (this_symbol = 0; this_symbol < max_symbols; this_symbol++) {
        // Pull the record for this symbol out of the debug map.

        this_record = 
          get_cv_sym_from_debug_map (process, module,
                                     this_subsection, this_symbol);

        // See if it's of interest to us, and add it to the
        // growing lookup table if it is.

        if ((this_record != NULL) &&
            (should_be_in_all_symbols_scan (this_record))) {

          add_lookup_table_entry (table, MAPPED_CODEVIEW_SYMBOL,
                                  (void*) this_record, 0);
          (*last) ++;
        }
      }
      break;

    default:
      break;
    }
  }
  return (table);
}


// TODO: This function always fails to find any symbols. Need to
// add code to look at the exported names in the debug map.

LOOKUP_TABLE *exported_symbols_from_debug_map 
   (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD *first, DWORD *last)
{
  LOOKUP_TABLE              *table = new_lookup_table (process, module);
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;
  DWORD                     this_subsection;
  WORD                      subsection_type;
  DWORD                     max_subsections;

  // First of all, find out basic navigation info for the debug map.

  max_subsections = number_of_codeview_subsections (info);

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++) {

    subsection_type = 
      codeview_subsection_type (info, this_subsection);

    switch (subsection_type) {

    case SST_SYMBOLS:
    case SST_ALIGN_SYM:
    case SST_GLOBAL_SYM:
    case SST_STATIC_SYM:
      break;

    case SST_PUBLIC_SYM:
    case SST_GLOBAL_PUB:
      break;

    default:
      break;
    }
  }
  (*first) = 1;
  (*last) = 0;
  return (table);
}

DWORD cv_symbol_debug_start_offset (CV_HEADER *sym, BOOL *valid)
{
  (*valid) = FALSE;

  // Paranoia R us
  if (sym == NULL) return (0);

  // Return offsets for any function symbols

  switch (sym->Index) {

  case CV_S_LPROC16:
    (*valid) = TRUE;
    return ((DWORD) (((CV_LPROC16*) sym)->DebugStart));
    break;

  case CV_S_GPROC16:
    (*valid) = TRUE;
    return ((DWORD) (((CV_GPROC16*) sym)->DebugStart));
    break;

  case CV_S_LPROC32:
    (*valid) = TRUE;
    return (((CV_GPROC32*) sym)->DebugStart);
    break;

  case CV_S_LPROC32_NEW:
    (*valid) = TRUE;
    return (((CV_GPROC32_NEW*) sym)->DebugStart);
    break;

  case CV_S_GPROC32:
    (*valid) = TRUE;
    return (((CV_GPROC32*) sym)->DebugStart);
    break;

  case CV_S_GPROC32_NEW:
    (*valid) = TRUE;
    return (((CV_GPROC32_NEW*) sym)->DebugStart);
    break;

   default:
     return (0);
  }
}

DWORD cv_symbol_debug_end_offset (CV_HEADER *sym, BOOL *valid)
{
  (*valid) = FALSE;

  // Paranoia R us
  if (sym == NULL) return (0);

  // Return offsets for any function symbols

  switch (sym->Index) {

  case CV_S_LPROC16:
    (*valid) = TRUE;
    return ((DWORD) (((CV_LPROC16*) sym)->DebugEnd));
    break;

  case CV_S_GPROC16:
    (*valid) = TRUE;
    return ((DWORD) (((CV_GPROC16*) sym)->DebugEnd));
    break;

  case CV_S_LPROC32:
    (*valid) = TRUE;
    return (((CV_GPROC32*) sym)->DebugEnd);
    break;

  case CV_S_LPROC32_NEW:
    (*valid) = TRUE;
    return (((CV_GPROC32_NEW*) sym)->DebugEnd);
    break;

  case CV_S_GPROC32:
    (*valid) = TRUE;
    return (((CV_GPROC32*) sym)->DebugEnd);
    break;

  case CV_S_GPROC32_NEW:
    (*valid) = TRUE;
    return (((CV_GPROC32_NEW*) sym)->DebugEnd);
    break;

  default:
    return (0);
  }
}

DWORD cv_symbol_address 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD FP, CV_HEADER *sym)
{
  int                       frame_offset;
  PIMAGE_DEBUG_INFORMATION   info = module->DebugMap;
  DWORD                      base = (module->ImageInformation).ImageBase;
  DWORD                      offset;
  WORD                       segment;

  // First a bug-out check.
  if (sym == NULL) return (0);

  switch (sym->Index) {

  case CV_S_BPREL16:
    frame_offset = (int) ((CV_BPREL16*)sym)->Offset;
    if (frame_offset < 0)
      return (FP - (DWORD) (-frame_offset));
    else
      return (FP + (DWORD) frame_offset);
    break;

  case CV_S_LDATA16:
    offset = (DWORD) ((CV_LDATA16*) sym)->Offset;
    segment = ((CV_LDATA16*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_GDATA16:
    offset = (DWORD) ((CV_GDATA16*) sym)->Offset;
    segment = ((CV_GDATA16*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_PUB16:
    offset = (DWORD) ((CV_PUB16*) sym)->Offset;
    segment = ((CV_PUB16*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_LPROC16:
    offset = (DWORD) ((CV_LPROC16*) sym)->Offset;
    segment = ((CV_LPROC16*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_GPROC16:
    offset = (DWORD) ((CV_GPROC16*) sym)->Offset;
    segment = ((CV_GPROC16*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_BPREL32:
    return (FP + ((CV_BPREL32*) sym)->Offset);
    break;

  case CV_S_BPREL32_NEW:
    return (FP + ((CV_BPREL32_NEW*) sym)->Offset);
    break;

  case CV_S_LDATA32:
    offset = ((CV_LDATA32*) sym)->Offset;
    segment = ((CV_LDATA32*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_LDATA32_NEW:
    offset = ((CV_LDATA32_NEW*) sym)->Offset;
    segment = ((CV_LDATA32_NEW*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_GDATA32:
    offset = ((CV_GDATA32*) sym)->Offset;
    segment = ((CV_GDATA32*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_GDATA32_NEW:
    offset = ((CV_GDATA32_NEW*) sym)->Offset;
    segment = ((CV_GDATA32_NEW*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_PUB32:
    offset = ((CV_PUB32*) sym)->Offset;
    segment = ((CV_PUB32*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_PUB32_NEW:
    offset = ((CV_PUB32_NEW*) sym)->Offset;
    segment = ((CV_PUB32_NEW*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_LPROC32:
    offset = ((CV_LPROC32*) sym)->Offset;
    segment = ((CV_LPROC32*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_LPROC32_NEW:
    offset = ((CV_LPROC32_NEW*) sym)->Offset;
    segment = ((CV_LPROC32_NEW*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_GPROC32:
    offset = ((CV_GPROC32*) sym)->Offset;
    segment = ((CV_GPROC32*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  case CV_S_GPROC32_NEW:
    offset = ((CV_GPROC32_NEW*) sym)->Offset;
    segment = ((CV_GPROC32_NEW*) sym)->Segment;
    return (base + offset + info->Sections[segment - 1].VirtualAddress);
    break;

  default:
    return (0);
  }
}


CV_HEADER *IP32_to_cv_sym_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD IP, BYTE *lang)
{
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;
  DWORD                     this_subsection;
  DWORD                     max_symbols;
  DWORD                     this_symbol;
  DWORD                     closest_symbol_address = 0;
  DWORD                     symbol_address;
  CV_HEADER                 *closest_symbol = NULL;
  CV_HEADER                 *this_record = NULL;
  WORD                      subsection_type;
  DWORD                     max_subsections;

  // We are going to return the CV symbol for the function whose code
  // contains the given instruction pointer. This will hopefully be
  // some kind of lexical-scope-start record, but in the absence of
  // such records, we must be prepared to return another kind of
  // symbol, maybe a CV_S_PUB32 (public symbol).

  // First of all, find out basic navigation info for the debug map.

  if (module->DebugType != CODEVIEW_IMAGE) return (NULL);

  max_subsections = number_of_codeview_subsections (info);

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++) {

    subsection_type = 
      codeview_subsection_type (info, this_subsection);

    switch (subsection_type) {

    case SST_SYMBOLS:
    case SST_ALIGN_SYM:
    case SST_GLOBAL_SYM:
    case SST_STATIC_SYM: 
      max_symbols = last_symbol_in_subsection (info, this_subsection);
      for (this_symbol = 0; this_symbol < max_symbols; this_symbol++) {

        // Pull the record for this symbol out of the debug map.

        this_record = 
          get_cv_sym_from_debug_map (process, module,
                                     this_subsection, this_symbol);

        // If this is a compiler flag, update the language

        if ((this_record != NULL) &&
            (this_record->Index == CV_S_COMPILE)) {
          CV_COMPILE_FLAG *flag = (CV_COMPILE_FLAG*) this_record;
          //print_cv_symbol_record(this_record);
          (*lang) = flag->Flags[1];
        }

        // See if it's the one we want.

        if ((this_record != NULL) &&
            (cv_sym_encloses_IP32 (module, this_record, IP))) {
          return (this_record);
        }

        // If we didn't take that exit, then keep recording
        // the closest candidate symbol.

        symbol_address = cv_symbol_address (process, module,
                                             0, this_record);

        if ((symbol_address > closest_symbol_address) &&
            (symbol_address <= IP)) {
          closest_symbol_address = symbol_address;
          closest_symbol = this_record;
        }
      }
      break;

    case SST_PUBLIC_SYM:
    case SST_GLOBAL_PUB:   
      max_symbols = last_symbol_in_subsection (info, this_subsection);

      for (this_symbol = 0; this_symbol < max_symbols; this_symbol++) {

        // Pull the record for this symbol out of the debug map.

        this_record = 
          get_cv_sym_from_debug_map (process, module,
                                     this_subsection, this_symbol);

        // If this is a compiler flag, update the language.

        if ((this_record != NULL) &&
            (this_record->Index == CV_S_COMPILE)) {
          CV_COMPILE_FLAG *flag = (CV_COMPILE_FLAG*) this_record;
          //print_cv_symbol_record(this_record);
          (*lang) = flag->Flags[1];
        }

        // Keep recording the closest symbol.

        symbol_address = cv_symbol_address (process, module,
                                            0, this_record);

        if ((symbol_address > closest_symbol_address) &&
            (symbol_address <= IP)) {
          closest_symbol_address = symbol_address;
          closest_symbol = this_record;
        }
      }
      break;

    default:
      break;
    }
  }
  // Return the closest symbol we found during the entire scan.
  return (closest_symbol);
}


BOOL cv_public_sym_encloses_IP32 
  (PIMAGE_DEBUG_INFORMATION info, CV_HEADER *last_symH, CV_HEADER *symH,
   DWORD IP)
{
  DWORD segment_rva_sym, segment_rva_last_sym;
  DWORD addr_sym, addr_last_sym;
  CV_PUB32 *last_sym, *sym;

  if ((last_symH == NULL) || (symH == NULL))
    return (FALSE);

  if ((last_symH->Index != CV_S_PUB32) || (symH->Index != CV_S_PUB32))
    return (FALSE);

  last_sym = (CV_PUB32*) last_symH;
  sym = (CV_PUB32*) symH;

  // Calculate the addresses of the two symbols.

  segment_rva_sym 
    = info->Sections[sym->Segment - 1].VirtualAddress;
  segment_rva_last_sym 
    = info->Sections[last_sym->Segment - 1].VirtualAddress;

  addr_sym 
    = info->ImageBase + segment_rva_sym + sym->Offset;
  addr_last_sym 
    = info->ImageBase + segment_rva_last_sym + last_sym->Offset;

  // Now see if this is the symbol we want.

  if ((addr_last_sym <= IP) && (IP < addr_sym))
    return (TRUE);
  else
    return (FALSE);
}

BOOL cv_sym_encloses_IP32 
  (LPDBGLIBRARY module, CV_HEADER *sym, DWORD IP)
{
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;

  if (sym == NULL)
    return (FALSE);

  // If this symbol is a codeview record beginning a lexical scope,
  // find out if it encloses the given instruction pointer.

  switch (sym->Index) {

    case CV_S_LPROC32:
    case CV_S_GPROC32:
      {
        CV_GPROC32 *rec = (CV_GPROC32*) sym;

        // The actual beginning and end addresses of this procedure are
        // found by combining the segment/offset address given in the
        // codeview record with the rva of the segment and the base of
        // the image. Basically, the code for procedure P starts at:

        // Base Of Image + RVA Of Segment for P + Offset for P

        DWORD segment_rva 
          = info->Sections[rec->Segment - 1].VirtualAddress;
        DWORD code_start 
          = (module->ImageInformation).ImageBase + segment_rva + rec->Offset;
        DWORD code_end 
          = code_start + rec->ProcLength - 1;

        if ((code_start <= IP) && (IP <= code_end)) {
          return (TRUE);
        }
        else {
          return (FALSE);
        }
      }
      break;

    case CV_S_LPROC32_NEW:
    case CV_S_GPROC32_NEW:
      {
        CV_GPROC32_NEW *rec = (CV_GPROC32_NEW*) sym;

        // The actual beginning and end addresses of this procedure are
        // found by combining the segment/offset address given in the
        // codeview record with the rva of the segment and the base of
        // the image. Basically, the code for procedure P starts at:

        // Base Of Image + RVA Of Segment for P + Offset for P

        DWORD segment_rva 
          = info->Sections[rec->Segment - 1].VirtualAddress;
        DWORD code_start 
          = (module->ImageInformation).ImageBase + segment_rva + rec->Offset;
        DWORD code_end 
          = code_start + rec->ProcLength - 1;

        if ((code_start <= IP) && (IP <= code_end)) {
          return (TRUE);
        }
        else {
          return (FALSE);
        }
      }
      break;

    case CV_S_BLOCK32:
      {
        CV_BLOCK32 *rec = (CV_BLOCK32*) sym;

        // Find the addresses in the same way as for procedures.

        DWORD segment_rva 
          = info->Sections[rec->Segment - 1].VirtualAddress;
        DWORD code_start 
          = (module->ImageInformation).ImageBase + segment_rva + rec->Offset;
        DWORD code_end 
          = code_start + rec->Length - 1;

        if ((code_start <= IP) && (IP <= code_end)) {
          return (TRUE);
        }
        else {
          return (FALSE);
        }
      }
      break;

    default:
      return (FALSE);
  }
}

int equal_symbolic_names (BYTE l1, BYTE l2, char *name1, char *name2)
{
  BYTE i1 = 0;
  BYTE i2 = 0;

  if ((l1 == (l2 + 1)) && (name1[i1] == '_')) {
    i1++;
    l1--;
  }
  else if ((l2 == (l1 + 1)) && (name2[i2] == '_')) {
    i2++;
    l2--;
  }
 
  if (l1 != l2)
    return (0);
  else {
    while (i1 < l1) {
      if (name1[i1] != name2[i2])
        return (0);
      else {
        i1++; i2++;
      }
    }
    return (1);
  }
}

int equal_cv_names (CV_HEADER *rec, BYTE l, char *name)
{

  // Make sure we're not just looking at rubbish.
  if (rec == NULL) return (0);

  switch (rec->Index) {

  case CV_S_OBJNAME:
//  print_cv_symbol_record(rec);
//  printf("\n");
    return(0);
    break;

  case CV_S_LDATA16:
    return (equal_symbolic_names (l, ((CV_LDATA16*) rec)->NameLength,
                                  name, ((CV_LDATA16*) rec)->Name));
    break;

  case CV_S_GDATA16:
    return (equal_symbolic_names (l, ((CV_GDATA16*) rec)->NameLength,
                                  name, ((CV_GDATA16*) rec)->Name));
    break;

  case CV_S_PUB16:
    return (equal_symbolic_names (l, ((CV_PUB16*) rec)->NameLength,
                                  name, ((CV_PUB16*) rec)->Name));
    break;

  case CV_S_LPROC16:
    return (equal_symbolic_names (l, ((CV_LPROC16*) rec)->NameLength,
                                  name, ((CV_LPROC16*) rec)->Name));
    break;

  case CV_S_GPROC16:
    return (equal_symbolic_names (l, ((CV_GPROC16*) rec)->NameLength,
                                  name, ((CV_GPROC16*) rec)->Name));
    break; 

  case CV_S_LDATA32:
    return (equal_symbolic_names (l, ((CV_LDATA32*) rec)->NameLength,
                                  name, ((CV_LDATA32*) rec)->Name));
    break;

  case CV_S_LDATA32_NEW:
    return (equal_symbolic_names (l, ((CV_LDATA32_NEW*) rec)->NameLength,
                                  name, ((CV_LDATA32_NEW*) rec)->Name));
    break;

  case CV_S_GDATA32:
    return (equal_symbolic_names (l, ((CV_GDATA32*) rec)->NameLength,
                                  name, ((CV_GDATA32*) rec)->Name));
    break;

  case CV_S_GDATA32_NEW:
    return (equal_symbolic_names (l, ((CV_GDATA32_NEW*) rec)->NameLength,
                                  name, ((CV_GDATA32_NEW*) rec)->Name));
    break;

  case CV_S_PUB32:
    return (equal_symbolic_names (l, ((CV_PUB32*) rec)->NameLength,
                                  name, ((CV_PUB32*) rec)->Name));
    break;

  case CV_S_PUB32_NEW:
    return (equal_symbolic_names (l, ((CV_PUB32_NEW*) rec)->NameLength,
                                  name, ((CV_PUB32_NEW*) rec)->Name));
    break;

  case CV_S_LPROC32:
    return (equal_symbolic_names (l, ((CV_LPROC32*) rec)->NameLength,
                                  name, ((CV_LPROC32*) rec)->Name));
    break;

  case CV_S_LPROC32_NEW:
    return (equal_symbolic_names (l, ((CV_LPROC32_NEW*) rec)->NameLength,
                                  name, ((CV_LPROC32_NEW*) rec)->Name));
    break;

  case CV_S_GPROC32:
    return (equal_symbolic_names (l, ((CV_GPROC32*) rec)->NameLength,
                                  name, ((CV_GPROC32*) rec)->Name));
    break; 

  case CV_S_GPROC32_NEW:
    return (equal_symbolic_names (l, ((CV_GPROC32_NEW*) rec)->NameLength,
                                  name, ((CV_GPROC32_NEW*) rec)->Name));
    break; 

  default:
//  printf ("Failed to compare names. Type was: %x\n", rec->Index);
    return (0);
    break;
  }
}

BOOL cv_get_function_offsets 
  (CV_HEADER *sym, DWORD *ds_offset, DWORD *de_offset, DWORD *e_offset)
{
  if (sym == NULL) 
    return (FALSE);

  switch (sym->Index) {

  case CV_S_LPROC16:
    (*ds_offset) = (DWORD) (((CV_LPROC16*)sym)->DebugStart);
    (*de_offset) = (DWORD) (((CV_LPROC16*)sym)->DebugEnd);
    (*e_offset)  = (DWORD) (((CV_LPROC16*)sym)->ProcLength);
    return (TRUE);
    break;

  case CV_S_GPROC16:
    (*ds_offset) = (DWORD) (((CV_GPROC16*)sym)->DebugStart);
    (*de_offset) = (DWORD) (((CV_GPROC16*)sym)->DebugEnd);
    (*e_offset)  = (DWORD) (((CV_GPROC16*)sym)->ProcLength);
    return (TRUE);
    break;

  case CV_S_LPROC32:
    (*ds_offset) = ((CV_LPROC32*)sym)->DebugStart;
    (*de_offset) = ((CV_LPROC32*)sym)->DebugEnd;
    (*e_offset)  = ((CV_LPROC32*)sym)->ProcLength;
    return (TRUE);
    break;

  case CV_S_LPROC32_NEW:
    (*ds_offset) = ((CV_LPROC32_NEW*)sym)->DebugStart;
    (*de_offset) = ((CV_LPROC32_NEW*)sym)->DebugEnd;
    (*e_offset)  = ((CV_LPROC32_NEW*)sym)->ProcLength;
    return (TRUE);
    break;

  case CV_S_GPROC32:
    (*ds_offset) = ((CV_GPROC32*)sym)->DebugStart;
    (*de_offset) = ((CV_GPROC32*)sym)->DebugEnd;
    (*e_offset)  = ((CV_GPROC32*)sym)->ProcLength;
    return (TRUE);
    break;

  case CV_S_GPROC32_NEW:
    (*ds_offset) = ((CV_GPROC32_NEW*)sym)->DebugStart;
    (*de_offset) = ((CV_GPROC32_NEW*)sym)->DebugEnd;
    (*e_offset)  = ((CV_GPROC32_NEW*)sym)->ProcLength;
    return (TRUE);
    break;

  default:
    return (FALSE);
  }
}

BOOL closest_symbol_in_debug_map
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
   CV_HEADER               *nearest_symbol;
   DWORD                    dword_address;
   BYTE                     byte_language;
   BOOL                     symbol_is_function;
   DWORD                    offset_to_frame_on;
   DWORD                    offset_to_frame_off;
   DWORD                    offset_to_function_end;

   // For convenience, declare a variable of each important codeview
   // record type.

   CV_LDATA16               *ldata16;
   CV_GDATA16               *gdata16;
   CV_PUB16                 *pub16;
   CV_LPROC16               *lproc16;
   CV_GPROC16               *gproc16;
   CV_LDATA32               *ldata32;
   CV_GDATA32               *gdata32;
   CV_PUB32                 *pub32;
   CV_LPROC32               *lproc32;
   CV_GPROC32               *gproc32;
   CV_LDATA32_NEW           *ldata32new;
   CV_GDATA32_NEW           *gdata32new;
   CV_PUB32_NEW             *pub32new;
   CV_LPROC32_NEW           *lproc32new;
   CV_GPROC32_NEW           *gproc32new;

   // Just get the nearest symbol using a lower-level function. Hopefully,
   // we can speed up IP32_to_cv_sym_from_debug_map.
   
   nearest_symbol = 
     IP32_to_cv_sym_from_debug_map(process,
                                   module,
                                   (DWORD) address,
                                   &byte_language);

   // If that did not succeed, return all failure values and go no further.

   if (nearest_symbol == NULL) {
     (*name_length) = (NUBINT) 0;
     (*actual_address) = (TARGET_ADDRESS) 0;
     (*is_function) = (NUBINT) 0;
     (*debug_start) = (TARGET_ADDRESS) 0;
     (*debug_end) = (TARGET_ADDRESS) 0;
     (*last_address) = (TARGET_ADDRESS) 0;
     return(FALSE);
   }

   // Otherwise, get the actual address of the symbol definition.

   dword_address = cv_symbol_address(process, module, 0, nearest_symbol);
   (*actual_address) = (TARGET_ADDRESS) dword_address;

   // Try to find out whether this symbol is a function, and what the
   // various important offsets are.

   symbol_is_function =
     cv_get_function_offsets(nearest_symbol,
                             &offset_to_frame_on,
                             &offset_to_frame_off,
                             &offset_to_function_end);

   if (symbol_is_function) {
     (*is_function) = (NUBINT) 1;
     (*debug_start) = (TARGET_ADDRESS) (dword_address + offset_to_frame_on);
     (*debug_end) = (TARGET_ADDRESS) (dword_address + offset_to_frame_off);
     (*last_address) = 
       (TARGET_ADDRESS) (dword_address + offset_to_function_end - 1);
   }
   else {
     (*is_function) = (NUBINT) 0;
     (*debug_start) = (TARGET_ADDRESS) 0;
     (*debug_end) = (TARGET_ADDRESS) 0;
     (*last_address) = (TARGET_ADDRESS) 0;
   }

   // Now fill in the name data, depending on what kind of symbol this is.

   switch (nearest_symbol->Index) {
   case CV_S_LDATA16:
     ldata16 = (CV_LDATA16*) nearest_symbol;
     (*name_length) = (NUBINT) (ldata16->NameLength);
     process->NameCache = ldata16->Name;
     break;

   case CV_S_GDATA16:
     gdata16 = (CV_GDATA16*) nearest_symbol;
     (*name_length) = (NUBINT) (gdata16->NameLength);
     process->NameCache = gdata16->Name;
     break;

   case CV_S_PUB16:
     pub16 = (CV_PUB16*) nearest_symbol;
     (*name_length) = (NUBINT) (pub16->NameLength);
     process->NameCache = pub16->Name;
     break;

   case CV_S_LPROC16:
     lproc16 = (CV_LPROC16*) nearest_symbol;
     (*name_length) = (NUBINT) (lproc16->NameLength);
     process->NameCache = lproc16->Name;
     break;

   case CV_S_GPROC16:
     gproc16 = (CV_GPROC16*) nearest_symbol;
     (*name_length) = (NUBINT) (gproc16->NameLength);
     process->NameCache = gproc16->Name;
     break;

   case CV_S_LDATA32:
     ldata32 = (CV_LDATA32*) nearest_symbol;
     (*name_length) = (NUBINT) (ldata32->NameLength);
     process->NameCache = ldata32->Name;
     break;

   case CV_S_LDATA32_NEW:
     ldata32new = (CV_LDATA32_NEW*) nearest_symbol;
     (*name_length) = (NUBINT) (ldata32new->NameLength);
     process->NameCache = ldata32new->Name;
     break;

   case CV_S_GDATA32:
     gdata32 = (CV_GDATA32*) nearest_symbol;
     (*name_length) = (NUBINT) (gdata32->NameLength);
     process->NameCache = gdata32->Name;
     break;

   case CV_S_GDATA32_NEW:
     gdata32new = (CV_GDATA32_NEW*) nearest_symbol;
     (*name_length) = (NUBINT) (gdata32new->NameLength);
     process->NameCache = gdata32new->Name;
     break;

   case CV_S_PUB32:
     pub32 = (CV_PUB32*) nearest_symbol;
     (*name_length) = (NUBINT) (pub32->NameLength);
     process->NameCache = pub32->Name;
     break;

   case CV_S_PUB32_NEW:
     pub32new = (CV_PUB32_NEW*) nearest_symbol;
     (*name_length) = (NUBINT) (pub32new->NameLength);
     process->NameCache = pub32new->Name;
     break;

   case CV_S_LPROC32:
     lproc32 = (CV_LPROC32*) nearest_symbol;
     (*name_length) = (NUBINT) (lproc32->NameLength);
     process->NameCache = lproc32->Name;
     break;

   case CV_S_LPROC32_NEW:
     lproc32new = (CV_LPROC32_NEW*) nearest_symbol;
     (*name_length) = (NUBINT) (lproc32new->NameLength);
     process->NameCache = lproc32new->Name;
     break;

   case CV_S_GPROC32:
     gproc32 = (CV_GPROC32*) nearest_symbol;
     (*name_length) = (NUBINT) (gproc32->NameLength);
     process->NameCache = gproc32->Name;
     break;

   case CV_S_GPROC32_NEW:
     gproc32new = (CV_GPROC32_NEW*) nearest_symbol;
     (*name_length) = (NUBINT) (gproc32new->NameLength);
     process->NameCache = gproc32new->Name;
     break;

   default:
     (*name_length) = (NUBINT) 40;
     process->NameCache = "NAME_SEARCH_ERROR";
   }
   (*language) = (NUBINT) byte_language;

   return(TRUE);
}

BOOL find_symbol_in_debug_map 
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
  PIMAGE_DEBUG_INFORMATION info = module->DebugMap;
  DWORD                    max_subsections, this_subsection;
  WORD                     subsection_type;
  DWORD                    max_symbols, this_symbol;
  CV_HEADER                *this_record;

  // Debug counters

  int                      stats, pubs;

  // Iterate over all symbolic records in this library and try to find
  // one that matches the given symbolic name.

  stats = 0;
  pubs = 0;

  max_subsections = number_of_codeview_subsections (info);

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++)
  {
    subsection_type = 
      codeview_subsection_type (info, this_subsection);

    switch (subsection_type) {

    case SST_SYMBOLS:
    case SST_ALIGN_SYM:
    case SST_STATIC_SYM:
      (*type) = STATIC_SYMBOL;
      max_symbols = last_symbol_in_subsection (info, this_subsection);
//    printf ("Searching %d symbols in subsection %x\n", max_symbols, 
//            subsection_type);
      for (this_symbol = 0; this_symbol < max_symbols; this_symbol++) {

        // Get the record.

        this_record =
          get_cv_sym_from_debug_map (process, module, this_subsection,
                                     this_symbol);

        // One more static symbol searched.

        stats++;

        // If this is a compiler record, then update the language
        // for the symbol.

        if ((this_record != NULL) && (this_record->Index == CV_S_COMPILE)) {
          CV_COMPILE_FLAG *cvflag = (CV_COMPILE_FLAG*) this_record;
          BYTE lang = cvflag->Flags[1];  // Is that the right one??
          //print_cv_symbol_record(this_record);
          (*language) = (NUBINT) lang;
        }

        // Find out if the names match.

        if (equal_cv_names (this_record, name_length, name)) {
          DWORD sym_addr;
          DWORD ds_offset;
          DWORD de_offset;
          DWORD e_offset;
          BOOL  func;

          sym_addr 
            = cv_symbol_address (process, module, 0, this_record);
          func 
            = cv_get_function_offsets (this_record,
                                       &ds_offset, &de_offset, &e_offset);
          if (func) {
            (*is_function) = (NUBINT) 1;
            (*addr) = (TARGET_ADDRESS) sym_addr;
            (*debug_start) = (TARGET_ADDRESS) (sym_addr + ds_offset);
            (*debug_end) = (TARGET_ADDRESS) (sym_addr + de_offset);
            (*last_address) = (TARGET_ADDRESS) (sym_addr + e_offset - 1);
          }
          else {
            (*is_function) = (NUBINT) 0;
            (*addr) = (TARGET_ADDRESS) sym_addr;
            (*debug_start) = (*addr);
            (*debug_end) = (*addr);
          }
//        printf("FOUND! Having searched %d statics and %d globals.\n",
//               stats, pubs);
          return (TRUE);
        }
      } 
      break;

    case SST_GLOBAL_SYM:
    case SST_GLOBAL_PUB:
      (*type) = GLOBAL_SYMBOL;
      max_symbols = last_symbol_in_subsection (info, this_subsection);
//    printf ("Searching %d symbols in subsection %x\n", max_symbols, 
//            subsection_type);
      for (this_symbol = 0; 
           this_symbol < max_symbols; 
           this_symbol++) {

        // Get the record.

        this_record =
          get_cv_sym_from_debug_map (process, module, this_subsection,
                                     this_symbol);

        // Another global

        pubs++;

        // Find out if the names match.

        if (equal_cv_names (this_record, name_length, name)) {
          DWORD sym_addr;
          DWORD ds_offset;
          DWORD de_offset;
          DWORD e_offset;
          BOOL  func;

          sym_addr 
            = cv_symbol_address (process, module, 0, this_record);
          func 
            = cv_get_function_offsets (this_record,
                                       &ds_offset, &de_offset, &e_offset);
          if (func) {
            (*is_function) = (NUBINT) 1;
            (*addr) = (TARGET_ADDRESS) sym_addr;
            (*debug_start) = (TARGET_ADDRESS) (sym_addr + ds_offset);
            (*debug_end) = (TARGET_ADDRESS) (sym_addr + de_offset);
            (*last_address) = (TARGET_ADDRESS) (sym_addr + e_offset);
          }
          else {
            (*is_function) = (NUBINT) 0;
            (*addr) = (TARGET_ADDRESS) sym_addr;
            (*debug_start) = (*addr);
            (*debug_end) = (*addr);
          }
//        printf("FOUND! Having searched %d statics and %d globals.\n",
//               stats, pubs);
          return (TRUE);
        }
      }
      break;

    default:
      break;
    }
  }
  // Found nothing, so return nothing.
//printf("Not found. Searched %d statics and %d globals.\n", stats, pubs);
  return (FALSE);
}


LOOKUP_TABLE *nearest_symbols_in_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD IP)
{
  CV_HEADER                 *nearest_cv, *previous_cv, *next_cv;
  DWORD                     nearest_address, previous_address, next_address;
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;
  DWORD                     max_subsections, this_subsection;
  DWORD                     max_symbols, this_symbol;
  CV_HEADER                 *this_record;
  WORD                      subsection_type;
  BYTE                      lang_nearest = 0;
  BYTE                      lang_prev = 0;
  BYTE                      lang_next = 0;
  DWORD                     sym_address;
  LOOKUP_TABLE              *table = new_lookup_table (process, module);

  // The strategy to find the nearest symbols is as follows. First,
  // obtain the nearest symbol using the same algorithm as is already
  // in place for finding the function corresponding to an address...

  nearest_cv 
    = IP32_to_cv_sym_from_debug_map (process, module, IP, &lang_nearest);

  nearest_address = cv_symbol_address (process, module, 0, nearest_cv);

  // Now we make a SECOND pass through the map, keeping records for the
  // closest symbols above and below the symbol we just found.

  previous_cv = next_cv = NULL;

  max_subsections = number_of_codeview_subsections (info);

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++) {

    subsection_type =
      codeview_subsection_type (info, this_subsection);

    switch (subsection_type) {

    case SST_SYMBOLS:
    case SST_ALIGN_SYM:
    case SST_GLOBAL_SYM:
    case SST_STATIC_SYM:
    case SST_PUBLIC_SYM:
    case SST_GLOBAL_PUB:
      max_symbols = last_symbol_in_subsection (info, this_subsection);
      for (this_symbol = 0; 
           this_symbol < max_symbols; 
           this_symbol++) {

         BYTE current_language = 0;

         // Get the record for this symbol.

         this_record =
           get_cv_sym_from_debug_map (process, module,
                                      this_subsection, this_symbol);

         // If this is a compiler record, update the current language

         if ((this_record != NULL) && (this_record->Index == CV_S_COMPILE)) {
            CV_COMPILE_FLAG *flag = (CV_COMPILE_FLAG*) this_record;
            //print_cv_symbol_record(this_record);
            current_language = flag->Flags[1];
	  }

         sym_address = cv_symbol_address (process, module, 0, this_record);

         // The update algorithm...

         if (sym_address < nearest_address) {
           if ((previous_cv == NULL) || (sym_address > previous_address)) {
             previous_cv = this_record;
             previous_address = sym_address;
             lang_prev = current_language;
           }
         }
         else if (sym_address > nearest_address) {
           if ((next_cv == NULL) || (sym_address < next_address)) {
             next_cv = this_record;
             next_address = sym_address;
             lang_next = current_language;
           }
         }
      }
    }
  }

  // We only want three entries in the lookup table - closest, previous, next.

  add_lookup_table_entry 
    (table, MAPPED_CODEVIEW_SYMBOL, (void*) nearest_cv, lang_nearest);
  add_lookup_table_entry 
    (table, MAPPED_CODEVIEW_SYMBOL, (void*) previous_cv, lang_prev);
  add_lookup_table_entry 
    (table, MAPPED_CODEVIEW_SYMBOL, (void*) next_cv, lang_next);

  return (table);
}


void function_bounding_addresses_in_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD IP,
   TARGET_ADDRESS *lower, TARGET_ADDRESS *upper)
{
  CV_HEADER                 *nearest_cv, *previous_cv, *next_cv;
  DWORD                     nearest_address, previous_address, next_address;
  PIMAGE_DEBUG_INFORMATION  info = module->DebugMap;
  DWORD                     max_subsections, this_subsection;
  DWORD                     max_symbols, this_symbol;
  CV_HEADER                 *this_record;
  WORD                      subsection_type;
  BYTE                      lang_nearest = 0;
  BYTE                      lang_prev = 0;
  BYTE                      lang_next = 0;
  DWORD                     sym_address;

  // The strategy to find the nearest symbols is as follows. First,
  // obtain the nearest symbol using the same algorithm as is already
  // in place for finding the function corresponding to an address...

  nearest_address = previous_address = next_address = IP;

  nearest_cv 
    = IP32_to_cv_sym_from_debug_map (process, module, IP, &lang_nearest);

  if (nearest_cv == NULL) {
    (*lower) = (TARGET_ADDRESS) nearest_address;
    (*upper) = (TARGET_ADDRESS) nearest_address;
    return;
  }

  nearest_address = cv_symbol_address (process, module, 0, nearest_cv);

  // Now we make a SECOND pass through the map, keeping records for the
  // closest symbols above and below the symbol we just found.

  previous_cv = next_cv = NULL;

  max_subsections = number_of_codeview_subsections (info);

  for (this_subsection = 0; 
       this_subsection < max_subsections; 
       this_subsection++) {

    subsection_type =
      codeview_subsection_type (info, this_subsection);

    switch (subsection_type) {

    case SST_SYMBOLS:
    case SST_ALIGN_SYM:
    case SST_GLOBAL_SYM:
    case SST_STATIC_SYM:
    case SST_PUBLIC_SYM:
    case SST_GLOBAL_PUB:
      max_symbols = last_symbol_in_subsection (info, this_subsection);
      for (this_symbol = 0; 
           this_symbol < max_symbols; 
           this_symbol++) {

         // Get the record for this symbol.

         this_record =
           get_cv_sym_from_debug_map (process, module,
                                      this_subsection, this_symbol);

         sym_address = cv_symbol_address (process, module, 0, this_record);

         // The update algorithm...

         if (sym_address < nearest_address) {
           if ((previous_cv == NULL) || (sym_address > previous_address)) {
             previous_cv = this_record;
             previous_address = sym_address;
           }
         }
         else if (sym_address > nearest_address) {
           if ((next_cv == NULL) || (sym_address < next_address)) {
             next_cv = this_record;
             next_address = sym_address;
           }
         }
      }
    }
  }
  (*lower) = (TARGET_ADDRESS) nearest_address;
  (*upper) = (TARGET_ADDRESS) next_address;
}


CV_HEADER *get_cv_sym_from_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD subsection, DWORD symbol)
{
  PIMAGE_DEBUG_INFORMATION info = module->DebugMap;
 
  // The easiest case is when we're after the symbol that has already
  // been cached.

  if ((module->Subsection == subsection) && 
      (module->Symbol == symbol) &&
      (module->SymbolPointer != NULL))
    return (module->SymbolPointer);

  // The next (and most probable) case is where the cached symbol is
  // a predecessor of the symbol we want. (In fact, it will probably be
  // the direct predecessor of the symbol we want, so this should be
  // quite efficient).

  if ((module->Subsection == subsection) && 
      (module->Symbol < symbol) &&
      (module->SymbolPointer != NULL)) {

    CV_HEADER          *this_header = module->SymbolPointer;
    DWORD              this_symbol = module->Symbol;

    // Chain through until we get to the symbol we want.

    while (this_symbol < symbol) {
      DWORD  map_pointer = (DWORD) this_header;
      map_pointer += (DWORD) this_header->Length + sizeof(WORD);
      this_header = (CV_HEADER*) map_pointer;
      this_symbol++;
    }

    // Update the cache before returning.
    module->Symbol = this_symbol;
    module->SymbolPointer = this_header;
    return (this_header);
  }

  // The next case is when the cache is for the wrong subsection. We have
  // to update it to point to the required subsection, and then start chaining
  // through from the first symbol. Again, note that in the usual case the
  // first symbol will be the required one if we're changing subsections,
  // so there won't be any chaining to do.

  if ((module->Subsection != subsection) || 
      (module->Symbol > symbol) ||
      (module->SymbolPointer == NULL)) {

    DWORD     subsection_lfo 
      = codeview_subsection_offset(info, subsection);
    WORD      subsection_type 
      = codeview_subsection_type(info, subsection);
    CV_HEADER *this_header;
    DWORD     this_symbol = 0;
    DWORD     map_pointer;

    map_pointer = CVMAP(info, subsection_lfo);
    map_pointer += size_of_subsection_prolog (subsection_type);
    this_header = (CV_HEADER*) map_pointer;

    // Now start chaining as before.

    while (this_symbol < symbol) {
      map_pointer += (DWORD) this_header->Length + sizeof(WORD);
      this_symbol++;
      this_header = (CV_HEADER*) map_pointer;
    }

    // Update the cache before returning.
    module->Symbol = this_symbol;
    module->SymbolPointer = this_header;
    module->Subsection = subsection;
    return (this_header);
  }

  return NULL;
}


DWORD last_symbol_in_subsection 
  (PIMAGE_DEBUG_INFORMATION info, DWORD subsection)
{
  DWORD             map_pointer;
  DWORD             subsection_offset;
  DWORD             total_bytes = 0;
  CV_HEADER         *this_header;
  DWORD             this_symbol = 0;
  WORD              subsection_type;
  DWORD             subsection_size;
  HASH_INFO_RECORD  *hash_info;

  // Obtain the offset to the subsection, its type, and its size from the
  // subsection directory.

  subsection_offset = codeview_subsection_offset (info, subsection);
  subsection_type = codeview_subsection_type (info, subsection);
  subsection_size = codeview_subsection_size (info, subsection);

  // Point us at the subsection.

  map_pointer = CVMAP(info, subsection_offset);

  // We need to find the number of symbols in the subsection. How we do
  // this depends upon the subsection type. The three cases are:

  // 1. This is not a symbol subsection at all (in which case just return
  //    zero.

  // 2. This is a linear symbol subsection, in which case we count the
  //    symbols linearly, keeping track of the number of bytes read.

  // 3. This is a hashed symbol subsection. We read the hashing info to
  //    obtain the size of the actual symbol records, but then simply
  //    search the records linearly as in (2). (There is obvious scope for
  //    improvement if we could actually use the hash info properly).


  switch (subsection_type) {

  case SST_SYMBOLS:
  case SST_ALIGN_SYM:
  case SST_PUBLIC_SYM:

    // Skip the DWORD signature on the symbol table.

    map_pointer += sizeof(DWORD);

    // Now point our first header record into the map.

    this_header = (CV_HEADER*) map_pointer;

    while (total_bytes < subsection_size) { 
      total_bytes += this_header->Length + sizeof(WORD);
      map_pointer += this_header->Length + sizeof(WORD);
      this_header = (CV_HEADER*) map_pointer;
      this_symbol++;
    }
    break;

  case SST_GLOBAL_SYM:
  case SST_GLOBAL_PUB:
  case SST_STATIC_SYM:
    hash_info = (HASH_INFO_RECORD*) map_pointer;
    subsection_size = hash_info->cbSymbol;
    map_pointer += sizeof(HASH_INFO_RECORD);

    // Now point our first header record into the map.

    this_header = (CV_HEADER*) map_pointer;

    while (total_bytes < subsection_size) {
      total_bytes += this_header->Length + sizeof(WORD);
      map_pointer += this_header->Length + sizeof(WORD);
      this_header = (CV_HEADER*) map_pointer;
      this_symbol++;
    }
    break;

  default:
    this_symbol = 0;
  }
  return (this_symbol);
}

void create_library_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module)
{
  HANDLE file_with_debug_info;
  DWORD base = module->ImageInformation.ImageBase;
  char extended_path[256];
  char original_path[256];
  char dbg_without_path[256];
  char buffer[256];
  char result[256];
  char original_extension[8];
  char *sep = ";";
  char full_search_order[1024];

  full_search_order[0] = '\0';
  extended_path[0] = '\0';

  // Generate the symbol file name by using the default image name
  // for the module, and replacing the extension with DBG.

  override_file_extension
    (module->DefaultImageName, "DBG", buffer, original_extension);
  override_file_path(buffer, NULL, dbg_without_path, original_path);

  extend_path_in_place(extended_path, original_path);
  extend_path_in_place(extended_path, "Symbols\\");
  extend_path_in_place(extended_path, original_extension);

  extend_path_in_place(full_search_order, original_path);
  extend_path_in_place(full_search_order, sep);
  extend_path_in_place(full_search_order, extended_path);
  extend_path_in_place(full_search_order, sep);
  extend_path_in_place(full_search_order, process->SymbolPaths);

  // Attempt to find a symbol (.DBG) file.

  file_with_debug_info
     = FindDebugInfoFile(dbg_without_path,
                         full_search_order,
                         result);

  // If this fails, then use the PE file itself as the repository of
  // debugging information.

  if (file_with_debug_info == NULL) {
    file_with_debug_info = module->ImageInformation.ImageFileHandle;
  }
  else {
    module->SymbolFile = file_with_debug_info;
  }

  module->DebugMap =
    MapDebugInformation (module->ImageInformation.ImageFileHandle, 
                         dbg_without_path,
                         full_search_order,
                         base);

  module->DebugType = NONE;

  if (module->DebugMap == NULL)
    return;

  // See to initializing cache data.

  module->Subsection = 0;
  module->Symbol = 0;
  module->SymbolPointer = NULL;

  // Find out (and store) the type(s) of debug information that we can
  // get hold of.

  if (codeview_present_in_image (module->DebugMap))
    module->DebugType = CODEVIEW_IMAGE;

  else if (codeview_present_in_pdb (module->DebugMap))
    module->DebugType = CODEVIEW_PDB;

  else if (coff_table_present_in_image (module->DebugMap))
    module->DebugType = COFF_IMAGE;
}


DWORD lexical_name_length_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_BPREL16:
    return ((DWORD) ((CV_BPREL16*) header)->NameLength);
    break;

  case CV_S_BPREL32:
    return ((DWORD) ((CV_BPREL32*) header)->NameLength);
    break;

  case CV_S_BPREL32_NEW:
    return ((DWORD) ((CV_BPREL32_NEW*) header)->NameLength);
    break;

  case CV_S_REGISTER:
    return ((DWORD) ((CV_REGISTER*) header)->NameLength);
    break;

  case CV_S_REGISTER_NEW:
    return ((DWORD) ((CV_REGISTER_NEW*) header)->NameLength);
    break;

  default:
    return (0);
  }
}

DWORD symbol_name_length_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_LDATA16:
    return ((DWORD) ((CV_LDATA16*) header)->NameLength);
    break;

  case CV_S_GDATA16:
    return ((DWORD) ((CV_GDATA16*) header)->NameLength);
    break;

  case CV_S_PUB16:
    return ((DWORD) ((CV_PUB16*) header)->NameLength);
    break;

  case CV_S_LPROC16:
    return ((DWORD) ((CV_LPROC16*) header)->NameLength);
    break;

  case CV_S_GPROC16:
    return ((DWORD) ((CV_GPROC16*) header)->NameLength);
    break;

  case CV_S_LDATA32:
    return ((DWORD) ((CV_LDATA32*) header)->NameLength);
    break;

  case CV_S_LDATA32_NEW:
    return ((DWORD) ((CV_LDATA32_NEW*) header)->NameLength);
    break;

  case CV_S_GDATA32:
    return ((DWORD) ((CV_GDATA32*) header)->NameLength);
    break;

  case CV_S_GDATA32_NEW:
    return ((DWORD) ((CV_GDATA32_NEW*) header)->NameLength);
    break;

  case CV_S_PUB32:
    return ((DWORD) ((CV_PUB32*) header)->NameLength);
    break;

  case CV_S_PUB32_NEW:
    return ((DWORD) ((CV_PUB32_NEW*) header)->NameLength);
    break;

  case CV_S_LPROC32:
    return ((DWORD) ((CV_LPROC32*) header)->NameLength);
    break;

  case CV_S_LPROC32_NEW:
    return ((DWORD) ((CV_LPROC32_NEW*) header)->NameLength);
    break;

  case CV_S_GPROC32:
    return ((DWORD) ((CV_GPROC32*) header)->NameLength);
    break;

  case CV_S_GPROC32_NEW:
    return ((DWORD) ((CV_GPROC32_NEW*) header)->NameLength);
    break;

  default:
    return (0);
    break;
  }
}


void lexical_name_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym, DWORD buf_size, char *buf)
{
  CV_HEADER *header = (CV_HEADER*) (sym->Pointer);
  char      *name = NULL;

  buf[0] = '\0';
  if (header == NULL) return;

  switch (header->Index) {

  case CV_S_BPREL16:
    name = ((CV_BPREL16*) header)->Name;
    break;

  case CV_S_BPREL32:
    name = ((CV_BPREL32*) header)->Name;
    break;

  case CV_S_BPREL32_NEW:
    name = ((CV_BPREL32_NEW*) header)->Name;
    break;

  case CV_S_REGISTER:
    name = ((CV_REGISTER*) header)->Name;
    break;

  case CV_S_REGISTER_NEW:
    name = ((CV_REGISTER_NEW*) header)->Name;
    break;

  default:
    break;
  }

  if (name != NULL) {
    DWORD length = lexical_name_length_from_debug_map (table, sym);
    DWORD i = 0;

    // Copy the name into the buffer from the mapped codeview
    // information.

    while ((i < buf_size) && (i < length)) {
      buf[i] = name[i];
      i++;
    }

    // Nul-terminate the buffer if there is room.
    if (i < buf_size) buf[i] = '\0';
  }
}


void symbol_name_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym, DWORD buf_size, char *buf)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);
  char      *name = NULL;

  buf[0] = '\0';
  if (header == NULL) return;

  switch (header->Index) {

  case CV_S_LDATA16:
    name = ((CV_LDATA16*) header)->Name;
    break;

  case CV_S_GDATA16:
    name = ((CV_GDATA16*) header)->Name;
    break;

  case CV_S_PUB16:
    name = ((CV_PUB16*) header)->Name;
    break;

  case CV_S_LPROC16:
    name = ((CV_LPROC16*) header)->Name;
    break;

  case CV_S_GPROC16:
    name = ((CV_GPROC16*) header)->Name;
    break;

  case CV_S_LDATA32:
    name = ((CV_LDATA32*) header)->Name;
    break;

  case CV_S_LDATA32_NEW:
    name = ((CV_LDATA32_NEW*) header)->Name;
    break;

  case CV_S_GDATA32:
    name = ((CV_GDATA32*) header)->Name;
    break;

  case CV_S_GDATA32_NEW:
    name = ((CV_GDATA32_NEW*) header)->Name;
    break;

  case CV_S_PUB32:
    name = ((CV_PUB32*) header)->Name;
    break;

  case CV_S_PUB32_NEW:
    name = ((CV_PUB32_NEW*) header)->Name;
    break;

  case CV_S_LPROC32:
    name = ((CV_LPROC32*) header)->Name;
    break;

  case CV_S_LPROC32_NEW:
    name = ((CV_LPROC32_NEW*) header)->Name;
    break;

  case CV_S_GPROC32:
    name = ((CV_GPROC32*) header)->Name;
    break;

  case CV_S_GPROC32_NEW:
    name = ((CV_GPROC32_NEW*) header)->Name;
    break;

  default:
    break;
  }

  if (name != NULL) {
    DWORD length = symbol_name_length_from_debug_map (table, sym);
    DWORD i = 0;

    // Copy the name into the buffer from the mapped codeview
    // information.

    while ((i < buf_size) && (i < length)) {
      buf[i] = name[i];
      i++;
    }

    // Nul-terminate the buffer if there is room.
    if (i < buf_size) buf[i] = '\0';
  }
}


DWORD lexical_address_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym,
   NUBINT *needs_register_lookup,
   NUB_INDEX *hi, NUB_INDEX *lo)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);
  CV_REGISTER *register_record;
  CV_REGISTER_NEW *register_record_new;

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_BPREL16:
    (*needs_register_lookup) = (NUBINT) 0;
    return ((DWORD) ((CV_BPREL16*) header)->Offset);
    break;

  case CV_S_BPREL32:
    (*needs_register_lookup) = (NUBINT) 0;
    return ((DWORD) ((CV_BPREL32*) header)->Offset);
    break;

  case CV_S_BPREL32_NEW:
    (*needs_register_lookup) = (NUBINT) 0;
    return ((DWORD) ((CV_BPREL32_NEW*) header)->Offset);
    break;

  case CV_S_REGISTER:
    register_record = (CV_REGISTER*) header;
    (*needs_register_lookup) = (NUBINT) 1;
    register_index_from_codeview_enumerate(register_record->Register, hi, lo);
    return ((DWORD) 0);
    break;

  case CV_S_REGISTER_NEW:
    register_record_new = (CV_REGISTER_NEW*) header;
    (*needs_register_lookup) = (NUBINT) 1;
    register_index_from_codeview_enumerate
       (register_record_new->Register, hi, lo);
    return ((DWORD) 0);
    break;

  default:
    return (0);
  }
}


DWORD symbol_address_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);
  WORD      segment = 0;
  DWORD     offset = 0;
  DWORD     segment_rva;

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_LDATA16:
    segment = ((CV_LDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_LDATA16*) header)->Offset;
    break;

  case CV_S_GDATA16:
    segment = ((CV_GDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_GDATA16*) header)->Offset;
    break;

  case CV_S_PUB16:
    segment = ((CV_PUB16*) header)->Segment;
    offset  = (DWORD) ((CV_PUB16*) header)->Offset;
    break;

  case CV_S_LPROC16:
    segment = ((CV_LPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_LPROC16*) header)->Offset;
    break;

  case CV_S_GPROC16:
    segment = ((CV_GPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_GPROC16*) header)->Offset;
    break;

  case CV_S_LDATA32:
    segment = ((CV_LDATA32*) header)->Segment;
    offset  = ((CV_LDATA32*) header)->Offset;
    break;

  case CV_S_LDATA32_NEW:
    segment = ((CV_LDATA32_NEW*) header)->Segment;
    offset  = ((CV_LDATA32_NEW*) header)->Offset;
    break;

  case CV_S_GDATA32:
    segment = ((CV_GDATA32*) header)->Segment;
    offset  = ((CV_GDATA32*) header)->Offset;
    break;

  case CV_S_GDATA32_NEW:
    segment = ((CV_GDATA32_NEW*) header)->Segment;
    offset  = ((CV_GDATA32_NEW*) header)->Offset;
    break;

  case CV_S_PUB32:
    segment = ((CV_PUB32*) header)->Segment;
    offset  = ((CV_PUB32*) header)->Offset;
    break;

  case CV_S_PUB32_NEW:
    segment = ((CV_PUB32_NEW*) header)->Segment;
    offset  = ((CV_PUB32_NEW*) header)->Offset;
    break;

  case CV_S_LPROC32:
    segment = ((CV_LPROC32*) header)->Segment;
    offset  = ((CV_LPROC32*) header)->Offset;
    break;

  case CV_S_LPROC32_NEW:
    segment = ((CV_LPROC32_NEW*) header)->Segment;
    offset  = ((CV_LPROC32_NEW*) header)->Offset;
    break;

  case CV_S_GPROC32:
    segment = ((CV_GPROC32*) header)->Segment;
    offset  = ((CV_GPROC32*) header)->Offset;
    break;

  case CV_S_GPROC32_NEW:
    segment = ((CV_GPROC32_NEW*) header)->Segment;
    offset  = ((CV_GPROC32_NEW*) header)->Offset;
    break;

  default:
    break;

  }

  if (segment != 0) {

    segment_rva 
        = table->Module->ImageInformation.ImageBase +
          table->Module->DebugMap->Sections[segment - 1].VirtualAddress;

    return (segment_rva + offset);
  }
  else {
    return (0);
  }
}


int symbol_is_function_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym)
{
  CV_HEADER *header = (CV_HEADER*) (sym->Pointer);

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_LPROC16:
  case CV_S_GPROC16:
  case CV_S_LPROC32:
  case CV_S_LPROC32_NEW:
  case CV_S_GPROC32:
  case CV_S_GPROC32_NEW:
    return (1);
    break;

  default:
    return (0);
    break;
  }
}

DWORD function_debug_start_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);
  WORD      segment = 0;
  DWORD     offset = 0;
  DWORD     debug_start_offset = 0;
  DWORD     segment_rva;

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_LDATA16:
    segment = ((CV_LDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_LDATA16*) header)->Offset;
    break;

  case CV_S_GDATA16:
    segment = ((CV_GDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_GDATA16*) header)->Offset;
    break;

  case CV_S_PUB16:
    segment = ((CV_PUB16*) header)->Segment;
    offset  = (DWORD) ((CV_PUB16*) header)->Offset;
    break;

  case CV_S_LPROC16:
    segment = ((CV_LPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_LPROC16*) header)->Offset;
    debug_start_offset = (DWORD) ((CV_LPROC16*) header)->DebugStart;
    break;

  case CV_S_GPROC16:
    segment = ((CV_GPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_GPROC16*) header)->Offset;
    debug_start_offset = (DWORD) ((CV_LPROC16*) header)->DebugStart;
    break;

  case CV_S_LDATA32:
    segment = ((CV_LDATA32*) header)->Segment;
    offset  = ((CV_LDATA32*) header)->Offset;
    break;

  case CV_S_LDATA32_NEW:
    segment = ((CV_LDATA32_NEW*) header)->Segment;
    offset  = ((CV_LDATA32_NEW*) header)->Offset;
    break;

  case CV_S_GDATA32:
    segment = ((CV_GDATA32*) header)->Segment;
    offset  = ((CV_GDATA32*) header)->Offset;
    break;

  case CV_S_GDATA32_NEW:
    segment = ((CV_GDATA32_NEW*) header)->Segment;
    offset  = ((CV_GDATA32_NEW*) header)->Offset;
    break;

  case CV_S_PUB32:
    segment = ((CV_PUB32*) header)->Segment;
    offset  = ((CV_PUB32*) header)->Offset;
    break;

  case CV_S_PUB32_NEW:
    segment = ((CV_PUB32_NEW*) header)->Segment;
    offset  = ((CV_PUB32_NEW*) header)->Offset;
    break;

  case CV_S_LPROC32:
    segment = ((CV_LPROC32*) header)->Segment;
    offset  = ((CV_LPROC32*) header)->Offset;
    debug_start_offset = ((CV_LPROC32*) header)->DebugStart;
    break;

  case CV_S_LPROC32_NEW:
    segment = ((CV_LPROC32_NEW*) header)->Segment;
    offset  = ((CV_LPROC32_NEW*) header)->Offset;
    debug_start_offset = ((CV_LPROC32_NEW*) header)->DebugStart;
    break;

  case CV_S_GPROC32:
    segment = ((CV_GPROC32*) header)->Segment;
    offset  = ((CV_GPROC32*) header)->Offset;
    debug_start_offset = ((CV_LPROC32*) header)->DebugStart;
    break;

  case CV_S_GPROC32_NEW:
    segment = ((CV_GPROC32_NEW*) header)->Segment;
    offset  = ((CV_GPROC32_NEW*) header)->Offset;
    debug_start_offset = ((CV_LPROC32_NEW*) header)->DebugStart;
    break;

  default:
    break;
  }

  if (segment != 0) {

    segment_rva 
      = table->Module->ImageInformation.ImageBase +
        table->Module->DebugMap->Sections[segment - 1].VirtualAddress;

    return (segment_rva + offset + debug_start_offset);
  }
  else {
    return (0);
  }
}


DWORD function_debug_end_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);
  WORD      segment = 0;
  DWORD     offset = 0;
  DWORD     debug_end_offset = 0;
  DWORD     segment_rva;

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_LDATA16:
    segment = ((CV_LDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_LDATA16*) header)->Offset;
    break;

  case CV_S_GDATA16:
    segment = ((CV_GDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_GDATA16*) header)->Offset;
    break;

  case CV_S_PUB16:
    segment = ((CV_PUB16*) header)->Segment;
    offset  = (DWORD) ((CV_PUB16*) header)->Offset;
    break;

  case CV_S_LPROC16:
    segment = ((CV_LPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_LPROC16*) header)->Offset;
    debug_end_offset = (DWORD) ((CV_LPROC16*) header)->DebugEnd;
    break;

  case CV_S_GPROC16:
    segment = ((CV_GPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_GPROC16*) header)->Offset;
    debug_end_offset = (DWORD) ((CV_LPROC16*) header)->DebugEnd;
    break;

  case CV_S_LDATA32:
    segment = ((CV_LDATA32*) header)->Segment;
    offset  = ((CV_LDATA32*) header)->Offset;
    break;

  case CV_S_LDATA32_NEW:
    segment = ((CV_LDATA32_NEW*) header)->Segment;
    offset  = ((CV_LDATA32_NEW*) header)->Offset;
    break;

  case CV_S_GDATA32:
    segment = ((CV_GDATA32*) header)->Segment;
    offset  = ((CV_GDATA32*) header)->Offset;
    break;

  case CV_S_GDATA32_NEW:
    segment = ((CV_GDATA32_NEW*) header)->Segment;
    offset  = ((CV_GDATA32_NEW*) header)->Offset;
    break;

  case CV_S_PUB32:
    segment = ((CV_PUB32*) header)->Segment;
    offset  = ((CV_PUB32*) header)->Offset;
    break;

  case CV_S_PUB32_NEW:
    segment = ((CV_PUB32_NEW*) header)->Segment;
    offset  = ((CV_PUB32_NEW*) header)->Offset;
    break;

  case CV_S_LPROC32:
    segment = ((CV_LPROC32*) header)->Segment;
    offset  = ((CV_LPROC32*) header)->Offset;
    debug_end_offset = ((CV_LPROC32*) header)->DebugEnd;
    break;

  case CV_S_LPROC32_NEW:
    segment = ((CV_LPROC32_NEW*) header)->Segment;
    offset  = ((CV_LPROC32_NEW*) header)->Offset;
    debug_end_offset = ((CV_LPROC32_NEW*) header)->DebugEnd;
    break;

  case CV_S_GPROC32:
    segment = ((CV_GPROC32*) header)->Segment;
    offset  = ((CV_GPROC32*) header)->Offset;
    debug_end_offset = ((CV_LPROC32*) header)->DebugEnd;
    break;

  case CV_S_GPROC32_NEW:
    segment = ((CV_GPROC32_NEW*) header)->Segment;
    offset  = ((CV_GPROC32_NEW*) header)->Offset;
    debug_end_offset = ((CV_LPROC32_NEW*) header)->DebugEnd;
    break;

  default:
    break;
  }

  if (segment != 0) {

    segment_rva 
      = table->Module->ImageInformation.ImageBase +
        table->Module->DebugMap->Sections[segment - 1].VirtualAddress;

    return (segment_rva + offset + debug_end_offset);
  }
  else {
    return (0);
  }
}


DWORD function_end_from_debug_map 
  (LOOKUP_TABLE *table, SYMBOL_LOOKUP_ENTRY *sym)
{
  CV_HEADER *header = (CV_HEADER *) (sym->Pointer);
  WORD      segment = 0;
  DWORD     offset = 0;
  DWORD     end_offset = 0;
  DWORD     segment_rva;

  if (header == NULL) return (0);

  switch (header->Index) {

  case CV_S_LDATA16:
    segment = ((CV_LDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_LDATA16*) header)->Offset;
    break;

  case CV_S_GDATA16:
    segment = ((CV_GDATA16*) header)->Segment;
    offset  = (DWORD) ((CV_GDATA16*) header)->Offset;
    break;

  case CV_S_PUB16:
    segment = ((CV_PUB16*) header)->Segment;
    offset  = (DWORD) ((CV_PUB16*) header)->Offset;
    break;

  case CV_S_LPROC16:
    segment = ((CV_LPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_LPROC16*) header)->Offset;
    end_offset = (DWORD) ((CV_LPROC16*) header)->ProcLength;
    break;

  case CV_S_GPROC16:
    segment = ((CV_GPROC16*) header)->Segment;
    offset  = (DWORD) ((CV_GPROC16*) header)->Offset;
    end_offset = (DWORD) ((CV_LPROC16*) header)->ProcLength;
    break;

  case CV_S_LDATA32:
    segment = ((CV_LDATA32*) header)->Segment;
    offset  = ((CV_LDATA32*) header)->Offset;
    break;

  case CV_S_LDATA32_NEW:
    segment = ((CV_LDATA32_NEW*) header)->Segment;
    offset  = ((CV_LDATA32_NEW*) header)->Offset;
    break;

  case CV_S_GDATA32:
    segment = ((CV_GDATA32*) header)->Segment;
    offset  = ((CV_GDATA32*) header)->Offset;
    break;

  case CV_S_GDATA32_NEW:
    segment = ((CV_GDATA32_NEW*) header)->Segment;
    offset  = ((CV_GDATA32_NEW*) header)->Offset;
    break;

  case CV_S_PUB32:
    segment = ((CV_PUB32*) header)->Segment;
    offset  = ((CV_PUB32*) header)->Offset;
    break;

  case CV_S_PUB32_NEW:
    segment = ((CV_PUB32_NEW*) header)->Segment;
    offset  = ((CV_PUB32_NEW*) header)->Offset;
    break;

  case CV_S_LPROC32:
    segment = ((CV_LPROC32*) header)->Segment;
    offset  = ((CV_LPROC32*) header)->Offset;
    end_offset = ((CV_LPROC32*) header)->ProcLength;
    break;

  case CV_S_LPROC32_NEW:
    segment = ((CV_LPROC32_NEW*) header)->Segment;
    offset  = ((CV_LPROC32_NEW*) header)->Offset;
    end_offset = ((CV_LPROC32_NEW*) header)->ProcLength;
    break;

  case CV_S_GPROC32:
    segment = ((CV_GPROC32*) header)->Segment;
    offset  = ((CV_GPROC32*) header)->Offset;
    end_offset = ((CV_LPROC32*) header)->ProcLength;
    break;

  case CV_S_GPROC32_NEW:
    segment = ((CV_GPROC32_NEW*) header)->Segment;
    offset  = ((CV_GPROC32_NEW*) header)->Offset;
    end_offset = ((CV_LPROC32_NEW*) header)->ProcLength;
    break;

  default:
    break;
  }

  if (segment != 0) {

    segment_rva 
      = table->Module->ImageInformation.ImageBase +
        table->Module->DebugMap->Sections[segment - 1].VirtualAddress;

    return (segment_rva + offset + end_offset - 1);
  }
  else {
    return (0);
  }
}


BOOL is_exported_name_in_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, char *name)
{
  return (FALSE);  // Quick hack for now.
}


BOOL file_names_match_in_debug_map
  (char *filename_wanted, char *this_filename, BYTE name_length)
{
  BYTE    i = (name_length - 1);
  BYTE    j = 0;
  BOOL    done = FALSE;

  // The name stored in the debugging information might have path
  // information. We will eventually establish a protocol for dealing
  // with paths, but for now we are just going to compare leaf filenames.
  // So, get to the index in "this_filename" that we want by scanning
  // right to left.

  while ((i > 0) && (this_filename[i - 1] != '/')
                 && (this_filename[i - 1] != '\\'))
    i--;

  // Compare characters.

  while (!done) {
    if (filename_wanted[j] == '\0')
      return (i == name_length);
    else if (i == name_length)
      return (FALSE);
    else if (filename_wanted[j] != this_filename[i])
      return (FALSE);
    else {
      i++;
      j++;
    }
  }
  return(TRUE);
}

WORD absolute_difference(WORD x, WORD y)
{
  if (x > y) 
    return (x - y);
  else
    return (y - x);
}

BOOL is_closer_linenumber (WORD this_one, WORD than_this_one, WORD to_this_one)
{
  return ((absolute_difference(this_one, to_this_one)) <
          (absolute_difference(than_this_one, to_this_one)));
}


BOOL resolve_source_location_in_debug_map
  (LPDBGPROCESS process, LPDBGLIBRARY module,
   char *filename, WORD line, WORD column, DWORD *address, BOOL *exact)
{
  DWORD                      max_subsections;
  DWORD                      this_subsection;
  WORD                       subsection_type;
  PIMAGE_DEBUG_INFORMATION   info = module->DebugMap;

  // The usual bug-out check.

  if (info == NULL)
    return(FALSE);

  // Iterate through all subsections.

  max_subsections = number_of_codeview_subsections(info);
  for (this_subsection = 0;
       this_subsection < max_subsections;
       this_subsection++) {

    subsection_type =
      codeview_subsection_type(info, this_subsection);

    if (subsection_type == SST_SRC_MODULE) {

      // This is a source code location map. We can iterate through the
      // various file entries until we find one that matches our name.

      WORD     num_files;
      WORD     file;

      per_source_table_info_from_debug_map
        (process, module, this_subsection, &num_files);

      // Iterate over files.

      for (file = 0; file < num_files; file++) {
        WORD          num_segments;
        WORD          segment;
        BYTE          name_length;
        char         *name;

        per_file_info_from_debug_map
          (process, module, this_subsection, file, &num_segments,
           &name_length, &name);

        // Check for the match.
        // TODO: Decent handling of pathnames, when available.
        
        if (file_names_match_in_debug_map(filename, name, name_length)) {

          WORD       closest_line;
          DWORD      closest_offset;
          WORD       closest_segment_index;
          WORD       first_num_pairs;

          // Start by pulling out whatever the first linenumber record
          // happens to be, and assume that to be the closest.

          per_segment_info_from_debug_map 
            (process, module, this_subsection, file,
             0, &closest_segment_index, &first_num_pairs);

          per_pair_info_from_debug_map
            (process, module, this_subsection, file,
             0, 0, &closest_offset, &closest_line);

          // Now begin iterating through all segments, and all pairs.
          // Keep updating the "closest" information. If we get an
          // exact match, though, take an early exit!

          for (segment = 0; segment < num_segments; segment++) {

            WORD segment_index;
            WORD num_pairs, pair;
            DWORD offset;
            WORD linenumber;
            DWORD base;

            per_segment_info_from_debug_map 
              (process, module, this_subsection, file,
               0, &segment_index, &num_pairs);

            // "base" holds the absolute address of the first code
            // location in this segment descriptor. Each pair
            // descriptor has an address which is offset from this
            // base, and needs to have this value added onto it

            base = module->ImageInformation.ImageBase +
                   info->Sections[segment_index - 1].VirtualAddress;

            // Now iterate over each linenumber/offset pair. We cannot
            // assume increasing linenumber ordering! We have to look at
            // every single entry! However, if we find an _exact_ match,
            // we can take an early exit.

            for (pair = 0; pair < num_pairs; pair++) {
              per_pair_info_from_debug_map
                (process, module, this_subsection, file,
                 segment, pair, &offset, &linenumber);
              if (linenumber == line) {
                (*address) = base + offset;
                (*exact) = TRUE;
                return(TRUE);
              }
              else if (is_closer_linenumber(linenumber, closest_line, line)) {
                closest_offset = offset;
                closest_line = linenumber;
                closest_segment_index = segment_index;
              }
            }
          }

          // All segments searched. Assume this to be it.
          (*address) = module->ImageInformation.ImageBase +
                       info->Sections[closest_segment_index - 1].VirtualAddress
                       + closest_offset;
          (*exact) = FALSE;
          return(TRUE);
        }
      }
    }
  }
  return(FALSE);
}            

void release_library_debug_map (LPDBGPROCESS process, LPDBGLIBRARY module)
{
  // Simple make the API call. (It allocated the memory, so it
  // should be responsible for releasing it).

  if (module->DebugMap != NULL) {
    UnmapDebugInformation (module->DebugMap);
    module->DebugMap = NULL;
  }

  // If we opened a handle on a DBG file, then close it.

  if (module->SymbolFile != NULL)
    CloseHandle(module->SymbolFile);

  // If DbgHelp has loaded up any symbols for us in this module. Unload
  // them now.

  if (module->SymbolHandlerWorking) {
    SymUnloadModule64(process->ProcessHandle,
		      (DWORD64) module->ImageInformation.ImageBase);
    module->SymbolHandlerWorking = FALSE;
  }
}


void finished_with_process (LPDBGPROCESS process)
{
  about_to_kill (process);
  free (process);
}


void about_to_kill (LPDBGPROCESS process)
{
  LPDBGTHREAD   thread = process->ThreadList;
  LPDBGTHREAD   old_thread;
  LPDEBUG_POINT debug_point = process->DebugPointList;
  LPDEBUG_POINT old_debug_point;
  LPDBGLIBRARY  library = process->LibraryList;
  LPDBGLIBRARY  old_library;

  // Dispose the storage for thread descriptors, and close their handles.

  while (thread != NULL) {
    old_thread = thread;
    thread = thread->Next;
    /* Closing the thread handles is a nice idea - but fails
    if (old_thread->Valid)
      CloseHandle (old_thread->ThreadHandle);
    */
    free (old_thread);
  }

  // Dispose the storage for library image descriptors, and free up their
  // debug maps.

  while (library != NULL) {
    old_library = library;
    library = library->Next;
    release_library_debug_map (process, old_library);
    release_library_coff_map (process, old_library);
    CloseHandle(old_library->ImageInformation.ImageFileHandle);
    free (old_library);
  }

  // Dispose the storage for debug point descriptors.

  while (debug_point != NULL) {
    old_debug_point = debug_point;
    debug_point = debug_point->Next;
    free (old_debug_point);
  } 

  if (process->SymbolHandlerWorking)
    SymCleanup(process->ProcessHandle);

  // Close our handle on the process.
  CloseHandle (process->ProcessHandle);
}

/*

void test_print_type_information (LPDBGPROCESS process, LPDBGLIBRARY module)
{
 PIMAGE_DEBUG_INFORMATION info = module->DebugMap;
 CV_GLOBAL_TYPE_TABLE     *type_base =
                           global_type_table_base(info);
 DWORD  i;
 DWORD  record_base;

 // Great big hack!
 // This function is not part of the nub.

 if (type_base == NULL) {
  printf ("Could not locate a global type table.\n");
  return;
 }

 printf ("Global types subsection mapped in at %x\n", type_base);
 printf ("First Reserved Byte = %x\n", type_base->Reserved1);
 printf ("Second Reserved Byte = %x\n", type_base->Reserved2);
 printf ("Third Reserved Byte = %x\n", type_base->Reserved3);
 printf ("Signature Byte = %x\n", type_base->Signature);
 printf ("Number of Records = %d\n", type_base->NumberOfTypeStrings);
 printf ("Offset to first Record = %x\n", type_base->TypeStringOffset[0]);
 record_base = ((DWORD) type_base + sizeof(DWORD) + sizeof(DWORD) +
                     (sizeof(DWORD) * type_base->NumberOfTypeStrings));
 printf ("\n");
 for (i = 0; i < (type_base->NumberOfTypeStrings); i++) {
  DWORD type_rec = record_base + type_base->TypeStringOffset[i];
  WORD  *init_ptr = (WORD*) type_rec;
  WORD  *lf_ptr = (WORD*) (type_rec + sizeof(WORD));
  printf ("Record %d is size %d :: %s\n", i, (*init_ptr),
         readable_text_for_leaf_code(*lf_ptr));

  // Have a go at unpicking field lists.

  if ((*lf_ptr) == CVT_LF_FIELDLIST) {

   // The documentation about the layout of field lists is
   // really pretty poor. I'm assuming that the LF_FIELDLIST
   // is followed immediately by another leaf index
   // (almost certainly an LF_MEMBER) and its associated data.
   // There is then some arbitrary amount of padding. There
   // is also the possibility that the last item in the list
   // is an LF_INDEX, indexing another FIELDLIST that is to
   // be viewed as a continuation of this one. 
   // Phew, what a scorcher!

   WORD *first_item_leaf = lf_ptr + 1;
   WORD *current_leaf = first_item_leaf;
   DWORD current_field = (DWORD) first_item_leaf;

   while ((*current_leaf) == CVT_LF_MEMBER) {
    CV_TYPE_DATA_MEMBER *cvt = (CV_TYPE_DATA_MEMBER*) current_field;
    CV_TYPE_NUMERIC_LEAF *lfOffset =
     (CV_TYPE_NUMERIC_LEAF*) &(cvt->OffsetAndName);
    DWORD leaf_size = byte_size_of_numeric_leaf(lfOffset);
    DWORD dwnameptr = ((DWORD) lfOffset) + leaf_size;
    DWORD dwstringptr = dwnameptr + sizeof(BYTE);
    DWORD total_length;
    DWORD next_current;
    BYTE *nameptr = (BYTE*) dwnameptr;
    BYTE *checker_byte;
    char *stringptr = (char*) dwstringptr;
    printf ("      MEMBER: ");
    print_length_prefixed_string (*nameptr, stringptr);
    printf ("\n");
    total_length = sizeof(WORD) + // The leaf code
                sizeof(WORD) + // Member type index
          sizeof(WORD) + // The attribute
          leaf_size +    // The numeric leaf for the offset
          sizeof(BYTE) + // The name length prefix
          sizeof(char) * (*nameptr); // The name
    next_current = current_field + total_length;
    checker_byte = (BYTE*) next_current;
    if ((*checker_byte) > 0xf0)
     next_current += ((*checker_byte) & 0x0f);
    current_field = next_current;
    current_leaf = (WORD*) current_field;
   }
  }
  console_input("Phewt");

 }
}


char *readable_text_for_leaf_code (WORD code)
{
 switch (code) {

 case CVT_LF_MODIFIER:
  return ("Modified type");
  break;

 case CVT_LF_POINTER:
  return ("General pointer type");
  break;

 case CVT_LF_ARRAY:
  return ("General array type");
  break;

 case CVT_LF_CLASS:
  return ("Class type");
  break;

 case CVT_LF_STRUCTURE:
  return ("Structure type");
  break;

 case CVT_LF_UNION:
  return ("Union type");
  break;

 case CVT_LF_ENUM:
  return ("Enumeration type");
  break;

 case CVT_LF_PROCEDURE:
  return ("Procedure type");
  break;

 case CVT_LF_MFUNCTION:
  return ("Member function type");
  break;

 case CVT_LF_VTSHAPE:
  return ("Virtual table shape type");
  break;

 case CVT_LF_COBOL0:
 case CVT_LF_COBOL1:
  return ("Reserved COBOL type");
  break;

 case CVT_LF_BARRAY:
  return ("Basic array type");
  break;

 case CVT_LF_LABEL:
  return ("Label type");
  break;

 case CVT_LF_NULL:
  return ("Null type");
  break;

 case CVT_LF_NOTTRAN:
  return ("Untranslated type");
  break;

 case CVT_LF_DIMARRAY:
  return ("Dimensioned array type");
  break;

 case CVT_LF_VFTPATH:
  return ("Virtual Function Table path type");
  break;

 case CVT_LF_PRECOMP:
  return ("Precompiled type");
  break;

 case CVT_LF_ENDPRECOMP:
  return ("End of precompiled type");
  break;

 case CVT_LF_OEM:
  return ("Generic OEM type");
  break;

 case CVT_LF_TYPESERVER:
  return ("Server type");
  break;

 case CVT_LF_SKIP:
  return ("Skip record");
  break;

 case CVT_LF_ARGLIST:
  return ("Argument list");
  break;

 case CVT_LF_DEFARG:
  return ("Default argument entry");
  break;

 case CVT_LF_LIST:
  return ("Arbitrary list entry");
  break;

 case CVT_LF_FIELDLIST:
  return ("Field list");
  break;

 case CVT_LF_DERIVED:
  return ("Derivation class list");
  break;

 case CVT_LF_BITFIELD:
  return ("Bitfield type description");
  break;

 case CVT_LF_METHODLIST:
  return ("Method list");
  break;

 case CVT_LF_DIMCONU:
  return ("Dimensioned array with constant upper bound");
  break;

 case CVT_LF_DIMCONLU:
  return ("Dimensioned array with constant upper and lower bounds");
  break;

 case CVT_LF_DIMVARU:
  return ("Dimensioned array with variable upper bound");
  break;

 case CVT_LF_DIMVARLU:
  return ("Dimensioned array with variable upper and lower bounds");
  break;

 case CVT_LF_REFSYM:
  return ("Referenced CodeView symbol copy");
  break;

 case CVT_LF_BCLASS:
  return ("Base class");
  break;

 case CVT_LF_VBCLASS:
  return ("Direct virtual base class");
  break;

 case CVT_LF_IVBCLASS:
  return ("Indirect virtual base class");
  break;

 case CVT_LF_ENUMERATE:
  return ("Enumeration entry");
  break;

 case CVT_LF_FRIENDFCN:
  return ("Friend function entry");
  break;

 case CVT_LF_INDEX:
  return ("******** INDIRECTION *******");
  break;

 case CVT_LF_MEMBER:
  return ("Member of struct/class/union");
  break;

 case CVT_LF_STMEMBER:
  return ("Static member entry");
  break;

 case CVT_LF_METHOD:
  return ("Overloaded method entry");
  break;

 case CVT_LF_NESTTYPE:
  return ("Nested type index");
  break;

 case CVT_LF_VFUNCTAB:
  return ("Virtual function table");
  break;

 case CVT_LF_FRIENDCLS:
  return ("Friend class entry");
  break;

 case CVT_LF_ONEMETHOD:
  return ("Non-overloaded method entry");
  break;

 case CVT_LF_VFUNCOFF:
  return ("Virtual function offset entry");
  break;

 default:
  return ("------------------- ????? -------------------");
  break;

 }
}
*/
