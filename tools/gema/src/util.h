
/* $Id: util.h,v 1.1 2004/03/12 00:42:09 cgay Exp $ */

/*
 * $Log: util.h,v $
 * Revision 1.1  2004/03/12 00:42:09  cgay
 * Initial revision
 *
 * Revision 1.6  1995/07/04 23:43:33  gray
 * When on Macintosh, use ':' as the directory delimiter.
 *
 * Revision 1.5 1995/05/22 02:52:28 gray
 * Add recognition of Windows NT - treat the same as MS-DOS.
 *
 * Revision 1.4 1995/05/08 03:19:46 gray
 * Define `MSDOS' if `__MSDOS__' is defined.
 */

#ifndef UTIL_H
#define UTIL_H

#include <stdlib.h>

#ifndef MSDOS
#if defined(__MSDOS__) || defined(_MSDOS) || defined(_WIN32)
#define MSDOS 1
#endif
#endif

typedef enum {
  MemoryPatterns, MemoryStream, MemoryInputBuf, MemoryOutputBuf,
  MemoryVar, MemoryPath, MemoryRegexp, MemoryDispatch
} Memory_Kinds;

/* allocate memory space; does not return unless succesful */
void* allocate( size_t size, Memory_Kinds what );

char* str_dup( const char* x ); /* return new copy of string */
char* str_dup_len( const char* x, int len );

/* directory delimiter character */
#if defined(MSDOS)
#define DirDelim '\\'
#elif defined(MACOS)
#define DirDelim ':'
#else /* assume Unix */
#define DirDelim '/'
#endif

const char*
pathname_name_and_type(const char* path);

const char*
pathname_type(const char* path);

char*
pathname_merge_directory( const char* path, const char* dir );

int
is_absolute_pathname(const char* path);

const char*
relative_pathname(const char* relative_to, const char* file_path);

const char*
canonicalize_path(const char* path);

#ifdef MSDOS
#define HAS_STRICMP
#endif

#ifndef HAS_STRICMP
/* unless already provided in the compiler's library */
int stricmp (const char* s1, const char* s2);
#endif

#endif

