/* ************************************************************************ */
/* ** debug_map.h                                                        ** */
/* ** Functions for unpicking the symbolic debug map provided by the     ** */
/* ** imagehelp routines                                                 ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard  Copyright: (c) 1996 Functional Objects, Inc.  ** */
/* **                                 All Rights Reserved                ** */
/* ************************************************************************ */


DWORD number_of_codeview_subsections 
  (PIMAGE_DEBUG_INFORMATION info);
// If CodeView debug information has been mapped, this function returns
// the number of subsections that it is divided into. (See Chapter 7
// of CodeView format spec for information about subsections).
// Subsequent lookup functions are often done on a per-subsection
// basis, so the number of subsections is important. Subsections are
// indexed from zero.

WORD codeview_subsection_type 
  (PIMAGE_DEBUG_INFORMATION info, DWORD subsection);
// Returns the type of the given CodeView subsection, eg SST_ALIGN_SYM

DWORD codeview_subsection_offset 
  (PIMAGE_DEBUG_INFORMATION info, DWORD subsection);
// Returns the address (relative to the base of the debug map) for the raw
// data in the subsection.

DWORD codeview_subsection_size 
  (PIMAGE_DEBUG_INFORMATION info, DWORD subsection);
// Returns the size of the raw data for the subsection.

BOOL codeview_present_in_image 
  (PIMAGE_DEBUG_INFORMATION info);
// Returns TRUE if codeview has been mapped in from the image file, else
// returns FALSE. A value of FALSE indicates that either no CV was present,
// or it was dumped in a PDB.

BOOL codeview_present_in_pdb 
  (PIMAGE_DEBUG_INFORMATION info);
// Returns TRUE if the function detects that CodeView information was dumped
// into a PDB file.

CV_HEADER *get_cv_sym_from_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   DWORD subsection, 
   DWORD symbol);
// Returns a pointer to the header struct of the given symbol in the given
// subsection. This function can be used to locate any symbol. Note that
// the returned pointer can be subsequently typecasted into any CodeView
// record type. To find out which type to cast into, examine the ->Index
// field of the header.

DWORD last_symbol_in_subsection 
  (PIMAGE_DEBUG_INFORMATION info, 
   DWORD subsection);
// Returns the last valid symbol index for the subsection. Note that symbols
// are indexed from zero, so zero is not a failure result. If the subsection
// is not a symbol table, this function should not be called with it.

void per_source_table_info_from_debug_map 
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   DWORD subsection,
   WORD *num_files);
// If the subsection is an sstSrcModule subsection (which is what we use to
// get linenumber information with CodeView data), this passes back the
// number of source files described by the subsection.

void per_file_info_from_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   DWORD subsection, 
   WORD file,
   WORD *num_segments, 
   BYTE *name_length,
   char **name_ptr);
// If the subsection is an sstSrcModule subsection, this passes back the
// information for the requested source file. (Source files are indexed
// from zero). It passes back the number of code segments that the source
// file contributes to, the byte-count for the string giving its name, and
// a character pointer to the name itself.

void per_segment_info_from_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   DWORD subsection, 
   WORD file, 
   WORD segment,
   WORD *segment_index, 
   WORD *num_pairs);
// If the subsection is an sstSrcModule subsection, this passes back the
// information for the requested segment within the given source file. It
// passes back the index of the segment that is being described, and the
// number of offset/linenumber pairs that the debug map holds for this
// segment.

void per_pair_info_from_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   DWORD subsection, 
   WORD file, 
   WORD segment,
   WORD pair, 
   DWORD *offset, 
   WORD *linenumber);
// The lowest level of detail. This passes back a single offset/linenumber
// pair.

CV_HEADER *IP32_to_cv_sym_from_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   DWORD IP,
   BYTE *lang);
// Searches the entire debug map for the given module, and attempts to find
// a symbol corresponding to the function whose code encloses the given
// instruction pointer (IP). Ideally, this will be a symbol that opens a
// lexical scope (eg CV_S_GPROC32), but in the absence of detailed lexical
// scope linkage, this function will return the closest symbol.

CV_HEADER *lexical_sym_from_scope_start 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   CV_HEADER *scope_start, 
   DWORD lex);
// Returns the lexical symbol indexed by "lex". This function searches
// within the lexical scope opened by "scope_start", jumping over inner
// scopes, and returns a pointer to the "lex"th frame-pointer-relative
// symbol, or NULL if no such symbol exists. Note, zero is a valid index.

DWORD pull_lexicals_within_scope 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   CV_HEADER *scope_start,
   LOOKUP_TABLE *table,
   DWORD IP);
// Returns the number of frame-pointer-relative symbols that are contained
// within the given scope.

void create_library_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module);
// Attempts to map debug information for the module.

void release_library_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module);
// Disposes mapped debug information for the module.

void finished_with_process 
  (LPDBGPROCESS process);
// Disposes of all memory that was allocated for the process. This function
// should be called before all legal exits from the debugger nub in order to
// prevent memory leaks.

void about_to_kill 
  (LPDBGPROCESS process);
// Is almost identical to the above function, but does not dispose of the
// top-level process descriptor. Therefore, this function should be used
// when killing the debugee with the intention of restarting it.

void print_cv_symbol_record 
  (CV_HEADER *header);
// A function for testing.


DWORD symbol_name_length_from_debug_map 
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym);
// Returns the length of the symbol's name, given that it is a mapped
// CodeView symbol.

void symbol_name_from_debug_map 
  (LOOKUP_TABLE *table, 
   SYMBOL_LOOKUP_ENTRY *sym,
   DWORD buf_size, 
   char *buf);
// Copies the symbol's name into "buf", given that it is a mapped CodeView
// symbol. The string space is null-terminated/truncated as necessary.

DWORD symbol_address_from_debug_map 
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym);
// Returns the address (in the linear address space of the running application)
// of the symbol, given that it is a mapped CodeView symbol.

BOOL is_exported_name_in_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   char *name);
// Returns TRUE if the named symbol is in the export table for this module,
// else returns false. The "name" must contain the entire symbol name, and
// be null-terminated.

LOOKUP_TABLE *all_lexicals_from_debug_map 
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   DWORD frame_pointer,
   DWORD IP,
   DWORD *first,
   DWORD *last);
// Scans for all lexical variables that are currently visible in the
// given stack frame, and generates a lookup table of them.

LOOKUP_TABLE *static_symbols_from_debug_map 
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   DWORD *first, 
   DWORD *last);
// Scans for all static symbols in the given module, and generates a
// quick-lookup table for them.

LOOKUP_TABLE *global_symbols_from_debug_map 
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   DWORD *first, 
   DWORD *last);
// Scans for all symbols that are global in the given module, and
// constructs a quick-lookup table for them.

LOOKUP_TABLE *exported_symbols_from_debug_map 
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   DWORD *first, 
   DWORD *last);
// Scans for all symbols that are exported from the given module, and
// constructs a quick-lookup table for them.

DWORD lexical_name_length_from_debug_map 
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym);
// Returns the length of the lexical's name, given that it is a mapped
// CodeView symbol.

void lexical_name_from_debug_map 
  (LOOKUP_TABLE *table, 
   SYMBOL_LOOKUP_ENTRY *sym,
   DWORD buf_size, 
   char *buf);
// Copies the lexical's name into "buf", given that it is a mapped CodeView
// symbol. The string space is null-terminated/truncated as necessary.

DWORD lexical_address_from_debug_map 
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym,
   NUBINT *needs_register_lookup,
   NUB_INDEX *hi_register,
   NUB_INDEX *lo_register);
// Returns the address (in the linear address space of the running application)
// of the lexical, given that it is a mapped CodeView symbol. Note that
// this only returns the frame offset.

BOOL find_symbol_in_debug_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module,
   BYTE name_length, char *name,
   TARGET_ADDRESS *addr,
   SYMBOL_CLASSIFICATION *type,
   NUBINT *is_function,
   TARGET_ADDRESS *debug_start,
   TARGET_ADDRESS *debug_end,
   NUBINT *language,
   TARGET_ADDRESS *last_address);
// Looks for a named symbol in the debug map for the given library. Returns
// TRUE if the symbol is found, as well as filling in it's address and it's
// type (STATIC, GLOBAL or EXPORTED)

BOOL closest_symbol_in_debug_map
  (LPDBGPROCESS process, LPDBGLIBRARY module,
   TARGET_ADDRESS address,
   NUBINT *name_length,
   TARGET_ADDRESS *actual_address, SYMBOL_CLASSIFICATION *type,
   NUBINT *is_function,
   TARGET_ADDRESS *debug_start,
   TARGET_ADDRESS *debug_end,
   NUBINT *language,
   TARGET_ADDRESS *last_address);

int symbol_is_function_from_debug_map 
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym);
// Returns non-zero if the given symbol is a function.

DWORD function_end_from_debug_map
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym);
// Given that the symbol is a function, returns the last valid address
// within the execution space of that function. This should be the address
// shortly after (or perhaps identical to) the debug-end of the function.

DWORD function_debug_start_from_debug_map 
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym);
// Given that the symbol is a function, returns the address at
// which the function's stack frame has been set up.

DWORD function_debug_end_from_debug_map 
  (LOOKUP_TABLE *table,
   SYMBOL_LOOKUP_ENTRY *sym);
// Given that the symbol is  function, returns the last valid address
// within the stack frame.

void function_bounding_addresses_in_debug_map 
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   DWORD IP,
   TARGET_ADDRESS *lower,
   TARGET_ADDRESS *upper);

LOOKUP_TABLE *nearest_symbols_in_debug_map 
  (LPDBGPROCESS process, 
   LPDBGLIBRARY module,
   DWORD IP);
// Finds the three nearest symbols to IP and stores them in a lookup table.

SL_LOOKUP_TABLE *source_locations_from_debug_map
   (LPDBGPROCESS process,
    LPDBGLIBRARY module,
    DWORD start, 
    DWORD end);
// Builds a table of source locations between the start and end addresses.
// The addresses should be flat 32-bit, not segment rva offsets or
// any other flowery stuff.

BOOL resolve_source_location_in_debug_map
  (LPDBGPROCESS process,
   LPDBGLIBRARY module,
   char *filename,
   WORD line,
   WORD column,
   DWORD *address,
   BOOL *exact);
