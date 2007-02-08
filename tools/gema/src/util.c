
/* $Id: util.c,v 1.3 1995/02/27 23:29:14 gray Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "main.h"

void* allocate( size_t size, Memory_Kinds what ) {
  /* note: the `what' argument is not used yet, but may be used someday to
     collect statistics on memory usage. */
  void* result;
  result = malloc(size);
  if ( result == NULL ) {
      fprintf(stderr, "Out of memory; aborting.\n");
      exit((int)EXS_MEM);
    }
  return result;
}

char* str_dup_len( const char* x, int len ) {
  char* r;
  r = allocate( len+1, MemoryVar);
  memcpy(r,x,len);
  r[len] = '\0';
  return r;
}

char* str_dup( const char* x ) {
  if ( x == NULL )
    return NULL;
  return str_dup_len(x, strlen(x));
}

const char*
pathname_name_and_type(const char* path) {
  const char* d = strrchr(path,DirDelim);
  if ( d == NULL )
    return path;
  else return d+1;
}

const char*
pathname_type(const char* path) {
  const char* d = strrchr(path,'.');
  if ( d == NULL )
    return NULL;
  else return d+1;
}

#if 0  /* not currently needed */
char*
pathname_merge_directory( const char* path, const char* dir ) {
  int dlen, len;
  char* result;
  dlen = strlen(dir);
  len = dlen + strlen(path) + 2;
  result = allocate( len, MemoryPath);
  strcpy(result, dir);
  if ( result[dlen-1] == DirDelim )
    dlen--;
  else result[dlen] = DirDelim;
  strcpy(result+dlen+1, path);
  return result;
}
#endif

int
is_absolute_pathname(const char* path) {
  const char* p = path;
#ifdef MSDOS
  if ( p[1] == ':' )
    p += 2;  /* strip off drive letter */
#endif
  return p[0] == DirDelim;
}

const char*
relative_pathname(const char* relative_to, const char* file_path) {
  /* If the two file pathnames are in the same directory, then return
     just the name and type portion of the second pathname.
     Otherwise return the whole second argument. */
  const char* file_name;
  const char* rel_name;
  int dir_len;
  file_name = pathname_name_and_type(file_path);
  rel_name = pathname_name_and_type(relative_to);
  dir_len = rel_name - relative_to;
  if ( dir_len == ( file_name - file_path ) &&
       strncmp(relative_to, file_path, dir_len) == 0 )
    return file_name;
  else return file_path;
}

const char*
canonicalize_path(const char* path) {
  if ( path[0] == '.' && path[1] == DirDelim )
    return path+2;	/*   "./foo" -> "foo"   */
  else return path;
}

#ifndef HAS_STRICMP
#include <ctype.h>

int stricmp (const char* s1, const char* s2){
  /* case-insensitive string comparison 
     for platforms that do not provide stricmp
   */
  int c1, c2;
  const unsigned char* u1 = (const unsigned char*)s1;
  const unsigned char* u2 = (const unsigned char*)s2;
  for ( ; ; ) {
    c1 = *u1++;
    c2 = *u2++;
    if ( c1 != c2 ) {
      c1 = toupper(c1);
      c2 = toupper(c2);
      if ( c1 != c2 )
	return c1 - c2;
    }
    if ( c1 == '\0' )
      return 0;
  }
}
#endif
