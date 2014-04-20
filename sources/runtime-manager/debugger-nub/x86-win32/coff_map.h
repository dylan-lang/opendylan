/* ************************************************************************ */
/* ** coff_map.h                                                         ** */
/* ** Functions for unpicking the symbolic debug map provided by the     ** */
/* ** COFF symbol table.                                                 ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard  Copyright: (c) 1996 Functional Objects, Inc.  ** */
/* **                                 All Rights Reserved                ** */
/* ************************************************************************ */

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
   TARGET_ADDRESS *last_address);


LOOKUP_TABLE *nearest_symbols_in_coff_map 
  (LPDBGPROCESS process, LPDBGLIBRARY module, DWORD IP);


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
   TARGET_ADDRESS *last_address);

COFF_SYMBOL_ENTRY *following_coff_symbol (COFF_SYMBOL_ENTRY *x);

COFF_SYMBOL_ENTRY *preceding_coff_symbol (COFF_SYMBOL_ENTRY *x);

COFF_SYMBOL_ENTRY *following_standard_coff_symbol (COFF_SYMBOL_ENTRY *x);

BOOL is_function_defining_record (COFF_SYMBOL_ENTRY *x);

BOOL is_begin_function_record (COFF_SYMBOL_ENTRY *x);

BOOL is_end_function_record (COFF_SYMBOL_ENTRY *x);

BOOL is_lines_in_function_record (COFF_SYMBOL_ENTRY *x);

BOOL calculate_function_debug_offsets
    (LPDBGLIBRARY module, COFF_SYMBOL_ENTRY *entry,
     DWORD *function_debug_start, DWORD *function_debug_end,
     DWORD *function_final);

int calculate_coff_symbol_name_length
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings);

char* calculate_coff_symbol_name_pointer
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings);

void copy_coff_symbol_name_into_buffer
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings, int bufsize, char *buf);

DWORD calculate_coff_address
    (LPDBGLIBRARY module, COFF_SYMBOL_ENTRY *x);

DWORD calculate_segmented_address
    (LPDBGLIBRARY module, WORD segment, DWORD offset);

void iterate_coff_symbols
    (LPDBGPROCESS process, LPDBGLIBRARY module,
     int (*f) (LPDBGPROCESS, LPDBGLIBRARY, DWORD, COFF_SYMBOL_ENTRY*, BYTE*));

BOOL compare_coff_names
  (COFF_SYMBOL_ENTRY *symbol, BYTE *strings, BYTE length, char *candidate);

void print_coff_symbol_name
    (COFF_SYMBOL_ENTRY *symbol, BYTE *strings);

void debug_print_coff_symbols
    (LPDBGLIBRARY module);


