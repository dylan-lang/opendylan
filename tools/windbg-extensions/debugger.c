#include "dylan.h"
#include "wdbgexts.h"
#include <stdio.h>
#include <stdarg.h>

char filter_buffer[1000];

void fprintf_filtered (FILE *stream, char *format_string, ...) {
  va_list args;
  va_start(args, format_string);
  vsprintf(filter_buffer, format_string, args);
  va_end(args);
  dprintf("%s", filter_buffer);
}

void fputs_filtered (char *string, FILE* stream) {
  dprintf("%s", string);
}

int target_read_memory (CORE_ADDR addr, void* buffer, int size) {
  int size_read;

  ReadMemory(addr, buffer, size, &size_read);
  return (size_read == size ? 0 : -1);
}

int read_memory (CORE_ADDR addr, void* buffer, int size) {
  return(target_read_memory(addr, buffer, size));
}

char buffer[MAX_NAME_SIZE];

char *find_closest_symbolic_name(CORE_ADDR target, CORE_ADDR *closest) {
  int displacement;

  GetSymbol(target, buffer, &displacement);
  *closest = target + displacement;
  return(buffer);
}

void error (char *format_string, ...) {
  va_list args;
  va_start(args, format_string);
  vsprintf(filter_buffer, format_string, args);
  va_end(args);
  dprintf("%s", filter_buffer);
}


