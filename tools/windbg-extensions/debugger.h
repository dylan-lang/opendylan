#include <windows.h>
#include <string.h>
#include <windbgkd.h>
#include <ntsdexts.h>
#include <wdbgexts.h>

#include <stdio.h>
#include <string.h>

#ifndef DEBUGGERDEFD 
#define DEBUGGERDEFD
#define boolean int
#define true 1
#define false 0
#define CORE_ADDR ULONG
extern void fprintf_filtered (FILE *stream, char *format_string, ...);
extern void fputs_filtered (char *string, FILE *stream);
extern int read_memory (CORE_ADDR addr, void* buffer, int size);
extern int target_read_memory (CORE_ADDR addr, void* buffer, int size);
extern void error (char *format_string, ...);
#define builtin_type_void void
#define TYPE_POINTER_TYPE(type) type *
#define TYPE_LENGTH(type) sizeof(type)
extern char *find_closest_symbolic_name(CORE_ADDR target, CORE_ADDR *closest);
#endif
