/* character stream functions */

/* $Id: cstream.c,v 1.12 2002/02/17 01:17:58 gray Exp $ */

/*********************************************************************
  This file is part of "gema", the general-purpose macro translator,
  written by David N. Gray <dgray@acm.org> in 1994 and 1995.
  Adapted for the Macintosh by David A. Mundie <mundie@anthus.com>.
  You may do whatever you like with this, so long as you retain
  an acknowledgment of the original source.
 *********************************************************************/

/*
 * $Log: cstream.c,v $
 * Revision 1.12  2002/02/17 01:17:58  gray
 * Prevent a reference outside of array -- fix contributed by Alex Karahalios.
 *
 * Revision 1.11  2002/02/17  00:29:39  gray
 * On Windows 95 and later, append ".bak" like on Unix instead of replacing
 * the extension like on MS-DOS.
 *
 * Revision 1.10  2001/12/15  20:22:01  gray
 * Update directory check to be more portable.
 * Clean up compiler warnings.
 *
 * Revision 1.9  1996/04/08  05:09:02  gray
 * Fix `extend_buffer' to correctly handle binary files on MS-DOS.
 * Modify `open_output_file' to facilitate better error message when input and
 * output files are the same and non-existent.
 *
 * Revision 1.8  1995/09/29  04:08:41  gray
 * Fix to work with "gcc" and SunOS library.
 *
 * Revision 1.7  1995/08/20  05:29:32  gray
 * Restore missing space in error message.
 *
 * Revision 1.6  1995/07/27  02:49:08  gray
 * For Macintosh, new function `get_info_block' in lieu of `stat'.
 *
 * Revision 1.5  1995/07/04  23:39:03  gray
 * For Macintosh, conditionalize out use of unsupported `stat'.
 *
 * Revision 1.4 1995/05/08 04:32:58 gray
 * If output file path is actually a directory, reports error instead of
 * renaming the directory.
 * For efficiency, use free list for input streams and output buffers.
 */

#if defined(_QC) || defined(_MSC_VER) /* Microsoft C or Quick C */
#pragma check_stack(off)
#endif

#include "cstream.h"
#include "util.h"
#include "main.h"
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#if defined(MACOS)
#include <Files.h>
#include <Strings.h>
#else
/* for the `stat' struct: */
#include <sys/types.h>
#include <sys/stat.h>
#endif

#ifndef SEEK_SET
/* hack for pre-ANSI header files */
#define SEEK_SET 0
/* hack for pre-ANSI library (such as "gcc" with SunOS library) */
#define memmove memcpy
#endif

static void
free_buffer( unsigned char* start, unsigned char* bufend );


/* ============    Input Streams ==============  */

struct input_stream_struct
{
  FILE* fs;
  MarkBuf* first_mark;
  unsigned char* start;
  unsigned char* end;
  const unsigned char* next;
  unsigned char* bufend;
  int peek_char;
  int cur_char;
  const char* pathname;
  unsigned long line;
  unsigned short column;
};

#define is_string_stream(s) (s->next!=NULL)
#define is_file_stream(s) (s->fs!=NULL)

#define NoMark NULL
#define BufSize 4000

static CIStream in_free_list = NULL;

static CIStream
allocate_input_stream(void) {
  if ( in_free_list != NULL ) {
    CIStream s = in_free_list;
    in_free_list = (CIStream)(void*)s->next;
    return s;
  }
  return (CIStream)
    allocate( sizeof(struct input_stream_struct), MemoryStream );
}

static boolean
extend_buffer ( CIStream s ) {
	if ( s->first_mark == NoMark ) { /* don't need buffer anymore */
	  s->cur_char = cis_prevch(s);
	  free( s->start );
	  s->start = NULL; s->next = NULL; s->bufend = NULL;
	  s->peek_char = -1;
	}
	else if ( s->end < (s->bufend-2) ) { /* read more into buffer */
	  if ( s->fs == stdin && ! binary ) {
	    /* for interactive input, don't read more than 1 line at a time */
	    unsigned char* r;
	    r = (unsigned char*)
	      fgets((char*)s->end, (s->bufend - s->end), s->fs);
	    if ( r != NULL )
	      s->end = r + strlen((char*)r);
	    else return FALSE;
	  }
	  else {
	    unsigned char* bp;
	    size_t nread, maxread;
	    bp = s->end;
	    maxread = s->bufend - bp;
	    if ( maxread > 128 )
	      maxread = 128;
	    nread = fread((char*)bp, 1, maxread, s->fs);
	    if ( nread > 0 )
	      s->end = bp + nread;
	    else return FALSE;
	  }
	}
       else { /* need to expand the buffer */
	 unsigned char* x;
	 size_t n;
	 size_t newsize;
	 n = s->end - s->start;
	 newsize = (s->bufend - s->start) + BufSize;
	 x = realloc( s->start, newsize );
	 if ( x != NULL ) { /* expanded OK */
	   s->start = x;
	   s->end = x + n;
	   s->next = s->end;
	   s->bufend = x + newsize;
	 }
	 else {
	   fputs("Out of memory for expanding input buffer.\n",stderr);
	   exit(EXS_MEM);
	 }
       }
   return TRUE;
}

/* read one character or EOF */
int cis_getch(CIStream s){
  int ch;
  if ( is_string_stream(s) ) {
    if ( s->next >= s->end ) {
      if ( is_file_stream(s) ) { /* buffered file stream */
	if ( extend_buffer(s) )
	  return cis_getch(s);
      }
      return EOF;
    }
    if ( s->next == s->start || s->next[-1] == '\n' ) {
      s->line++;
      s->column = 1;
    }
    else s->column++;
    return *s->next++;
  }
  else {
    if ( s->cur_char == '\n' ) {
      s->line++;
      s->column = 1;
    }
    else s->column++;
    if ( s->peek_char >= 0 ) {
      ch = s->peek_char;
      s->peek_char = -1;
    } else {
      ch = getc(s->fs);
      if ( ch == EOF && !feof(s->fs) ) {
	perror(s->pathname? s->pathname : "stdin");
	if ( keep_going )
	  exit_status = EXS_INPUT;
	else exit(EXS_INPUT);
	}
    }
    s->cur_char = ch;
    return ch;
  }
}

/* peek ahead to the next character to be read */
int cis_peek(CIStream s){
  if ( is_string_stream(s) ) {
    if ( s->next >= s->end ) {
      if ( is_file_stream(s) ) { /* buffered file stream */
	if ( extend_buffer(s) )
	  return cis_peek(s);
      }
      return EOF;
    }
    else return *s->next;
  }
  else {
    if ( s->peek_char < 0 )
      s->peek_char = getc(s->fs);
    return s->peek_char;
  }
}

/* return the previous character read */
int cis_prevch(CIStream s){
  if ( is_string_stream(s) ) {
    if ( s->next <= s->start )
      return EOF;
    else return s->next[-1];
  }
  else return s->cur_char;
}

unsigned long cis_line(CIStream s) {
  return s==NULL? (unsigned long)0 : s->line;
}

unsigned int cis_column(CIStream s) {
  return s==NULL? 0 : s->column;
}

/* remember current position */
void cis_mark(CIStream s,MarkBuf* mb){
  long m;
  if ( is_string_stream(s) )
    m = s->next - s->start;
  else {
#ifndef MSDOS /* seek is too slow on MS-DOS */
    if ( s->pathname == NULL ) /* can't trust rewinding a pipe */
      m = -1;
    else m = ftell(s->fs);
    if ( m < 0 ) /* seek is not supported; need to buffer */
#endif
     {
      unsigned char* r;
      r = (unsigned char*) allocate( BufSize, MemoryInputBuf );
      s->start = r;
      if ( s->cur_char != EOF )
	*r++ = (unsigned char)s->cur_char;
      s->next = r;
      if ( s->peek_char >= 0 )
	*r++ = (unsigned char)s->peek_char;
      s->end = r;
      s->bufend = s->start + BufSize;
      m = s->next - s->start;
    }
  }
  if ( s->first_mark == NoMark )
    s->first_mark = mb;
  else assert( m >= s->first_mark->position );
  mb->position = m;
  mb->line = s->line;
  mb->column = s->column;
  mb->prevch = s->cur_char;
  mb->peekch = s->peek_char;
}

void cis_release(CIStream s, MarkBuf* mb) {
  if ( mb == s->first_mark ) {
      s->first_mark = NoMark;
      if ( is_string_stream(s) && is_file_stream(s) ) {
	size_t len = s->end - s->next;
	size_t done = s->next - s->start;
	if ( len*4 < done && done > 100 ) {
	  unsigned char* n;

	  memmove( s->start, s->next-1, len+1 );
	  n = s->start+1;
	  s->next = n;
	  s->end = n + len;
	  *s->end = '\0';
	}
      }
  }
  else assert( s->first_mark != NoMark );
}

/* restore to remembered position; returns 0 if OK */
int cis_restore(CIStream s, MarkBuf* mb){
  int status;
  long pos;
  pos = mb->position;
  if ( is_string_stream(s) ) {
    s->next = s->start + pos;
    if ( s->next < s->start || s->next > s->end ) {
      fprintf(stderr, "cis_restore invalid position %ld\n", pos);
      exit(EXS_INPUT);
    }
    status = 0;
  }
  else {
    status = fseek(s->fs, pos, SEEK_SET);
  }
  cis_release(s, mb);
  s->line = mb->line;
  s->column = mb->column;
  s->cur_char = mb->prevch;
  s->peek_char = mb->peekch;
  return status;
}

/* reset to read from the beginning */
void cis_rewind(CIStream s){
  if ( is_string_stream(s) )
    s->next = s->start;
  else {
    s->peek_char = -1;
    s->cur_char = EOF;
    fseek(s->fs, 0, SEEK_SET);
  }
}

/* create stream */
CIStream make_file_input_stream(FILE* f, const char* path){
  register CIStream s = allocate_input_stream();
  s->fs = f;
  s->pathname = str_dup(path);
  s->first_mark = NoMark;
  s->peek_char = -1;
  s->cur_char = EOF;
  s->line = 1;
  s->column = 0;
  s->next = NULL;
  s->start = NULL;
  s->end = NULL;
  s->bufend = NULL;
  return s;
}

CIStream
open_input_file( const char* pathname, boolean binary )
{
  FILE* infs;
  if ( pathname[0] == '-' && pathname[1] == '\0' )
    return stdin_stream;
  infs = fopen( pathname, binary? "rb" : "r" );
  if ( infs == NULL ) {
    input_error(input_stream, EXS_INPUT, "Can't open file for reading:\n");
    perror(pathname);
    if ( keep_going )
      return NULL;
    else exit(EXS_INPUT);
  }
  return make_file_input_stream(infs,pathname);
}

const char* cis_pathname(CIStream s){
  return s==NULL? "" : s->pathname;
}

boolean cis_is_file(CIStream s) {
  return is_file_stream(s);
}

#ifdef MACOS
static CInfoPBRec myblock;

OSErr get_info_block(const char* pathname) {
	static WDPBRec mywd;
	OSErr e;

	e = PBHGetVol(&mywd, 0);
	if (e) return e;
	pathname = c2pstr(pathname);
	myblock.hFileInfo.ioNamePtr = pathname;
	myblock.hFileInfo.ioVRefNum = mywd.ioVRefNum;
	myblock.hFileInfo.ioDirID = mywd.ioWDDirID;
	e=PBGetCatInfo(&myblock, 0);
	pathname = p2cstr(pathname);
	return e;
}
#endif

time_t cis_mod_time(CIStream s) {
  if ( s != NULL && s->fs != NULL && s->pathname != NULL ) {
#ifdef MACOS
    if (get_info_block(s->pathname)==0)
      return myblock.hFileInfo.ioFlMdDat;
    else return 0;
#else
    struct stat sbuf;
    fstat( fileno(s->fs), &sbuf );
    return sbuf.st_mtime;
#endif
  }
  else return 0;
}

char probe_pathname(const char* pathname) {
#ifdef MACOS
    if (get_info_block(pathname)==0) {
      if (myblock.hFileInfo.ioFlAttrib & 16)
	return 'D'; /* Directory */
      else return 'F';        /* File */
    } else return 'X'; /* Unexpected */
#else
    struct stat sbuf;
    if ( stat( pathname, &sbuf ) == 0 ) {
      int isdir;
#ifdef S_ISDIR  /* POSIX */
      isdir = S_ISDIR(sbuf.st_mode);
#else	       /* older way */
      isdir = sbuf.st_mode & S_IFDIR;
#endif
      if ( isdir )
	return 'D'; /* directory */
      else if ( sbuf.st_mode & S_IFREG )
	return 'F'; /* file */
      else if ( sbuf.st_mode & S_IFCHR )
	return 'V'; /* device */
      else return 'X'; /* unexpected mode */
    }
    else return 'U'; /* undefined */
#endif
}

CIStream make_string_input_stream(const char* x, size_t length,
				boolean copy){
  register CIStream s = allocate_input_stream();
  if ( length == 0 )
    length = strlen(x);
  s->fs = NULL;
  s->pathname = NULL;
  s->first_mark = NoMark;
  s->start = (unsigned char*)x;
  s->bufend = NULL;
  if ( copy ) {
    size_t alen = length+1;
    s->start = allocate(alen, MemoryInputBuf);
    memcpy(s->start, x, alen);
    s->bufend = s->start + alen;
  }
  s->next = s->start;
  s->end = s->start + length;
  s->line = 0;
  s->column = 0;
  return s;
}

CIStream clone_input_stream( CIStream in ) {
  register CIStream s = allocate_input_stream();
  assert ( is_string_stream(in) );
  *s = *in;
  s->first_mark = NoMark;
  s->bufend = NULL;
  return s;
}

char* cis_whole_string ( CIStream s ) {
  if ( s == NULL )
    return "";
  else {
    assert( is_string_stream(s) );
    assert( *s->end == '\0' );
    return (char*)s->start;
  }
}

/* number of characters in input buffer stream */
unsigned cis_length( CIStream s ){
  if ( s == NULL )
    return 0;
  else {
    assert( is_string_stream(s) );
    return s->end - s->start;
  }
}

/* close stream */
void cis_close(CIStream s){
  if ( s == NULL || s == stdin_stream )
    return;
  if ( is_file_stream(s) ) {
    fclose(s->fs);
    s->fs = NULL;
    if ( s->pathname != NULL ) {
      free((char*)s->pathname);
      s->pathname = NULL;
    }
  }
  s->next = NULL;
  if ( s->bufend != NULL ) {
    free_buffer(s->start, s->bufend);
    s->start = NULL;
    s->bufend = NULL;
  }

  /* instead of:  free(s);  */
  s->next = (unsigned char*)(void*)in_free_list;
  in_free_list = s;
}

void input_error( CIStream s, Exit_States code, const char* format, ... ) {
  va_list args;
  va_start(args,format);
  if ( code > exit_status )
    exit_status = (Exit_States)code;
  if ( s != NULL ) {
    if ( !is_file_stream(s) )
      s = input_stream;
    if ( s != NULL && is_file_stream(s) ) {
      const char* path = cis_pathname(s);
      if ( path != NULL )
#if defined(MACOS)
	/* For MPW, this makes the output "self-referential I/O" - users can
		execute the error message as a command, and it takes them to
		the point in the file where the error occurred.
		-- D.A.M. 7/21/95 */
	fprintf(stderr, "File \"%s\"; line %ld # ",
#else
	fprintf(stderr, "File \"%s\" line %ld: ",
#endif
		pathname_name_and_type(path),
		cis_line(s) );
    }
  }
  vfprintf(stderr, format, args);
  va_end(args);
}


/* ============    Output Streams ==============  */

struct output_stream_struct {
  FILE* fs;   /* the file descriptor */
  int lastch; /* last character written */
  int column;
  unsigned char* start;
  unsigned char* next;
  unsigned char* bufend;
  const char* pathname;
};

#define OutBufSize 512
static unsigned char* free_buf_list = NULL;

static COStream out_free_list = NULL;

static COStream
allocate_output_stream(void) {
  if ( out_free_list != NULL ) {
    COStream s = out_free_list;
    out_free_list = (COStream)(void*)s->next;
    return s;
  }
  return (COStream)
    allocate( sizeof(struct output_stream_struct), MemoryStream );
}

static void
free_output_stream(COStream s) {
  s->next = (unsigned char*)(void*)out_free_list;
  out_free_list = s;
}

static void
expand_output_buffer ( COStream s, int need ) {
   unsigned char* x;
   size_t n;
   size_t more;
   size_t oldsize;
   size_t newsize;

   oldsize = s->bufend - s->start;
   more = OutBufSize + (oldsize >> 2) ;
   if ( (size_t)need >= more )
     more = need + OutBufSize;
   newsize = oldsize + more;
   n = s->next - s->start;
   x = realloc( s->start, newsize );
   if ( x != NULL ) { /* expanded OK */
     s->start = x;
     s->next = x + n;
     s->bufend = x + newsize;
   }
   else {
     fputs("Out of memory for expanding output buffer.\n",stderr);
     exit(EXS_MEM);
   }
}

/* write one character */
void cos_putch(COStream s, int c){
  if ( is_string_stream(s) ) {
    if ( s->next >= s->bufend )
      expand_output_buffer ( s, 1 );
    *s->next++ = c;
  }
  else {
    if ( putc(c,s->fs) == EOF ) {
      fprintf(stderr, "Error writing to output file:\n");
      perror(s->pathname);
      exit(EXS_OUTPUT);
    }
    s->lastch = c;
    if ( c == '\n' )
      s->column = 1;
    else s->column++;
  }
}

/* write some number of spaces */
void cos_spaces(COStream s, int n) {
  int i;
  for ( i = n ; i > 0 ; i-- )
    cos_putch(s,' ');
}

/* write null-terminated string */
void cos_puts(COStream s, const char* x) {
  int len;
  if ( x == NULL )
    return;
  len = strlen(x);
  if ( len > 0 ) {
    if ( is_string_stream(s) )
      cos_put_len(s, x, len);
    else {
      int n;
      const char* p;
      fputs(x,s->fs);
      p = x+len-1;
      s->lastch = *p;
      for ( n = 0 ; p >= x ; p-- ) {
	if ( *p == '\n' ) {
	  s->column = 1;
	  break;
	}
	else n++;
      }
      s->column += n;
    }
  }
}

/* write arbitrary data */
void cos_put_len(COStream s, const char* x, size_t len) {
  if ( len > 0 ) {
    if ( is_string_stream(s) ) {
      if ( s->next + len >= s->bufend )
	expand_output_buffer ( s, len );
      memcpy( s->next, x, len+1 );
      s->next += len;
    }
    else {
      int n;
      const char* p;
      p = x;
      for ( n = len ; n > 0 ; n-- )
	cos_putch(s,*p++);
    }
  }
}

/* return the last character written, or EOF if nothing written yet */
int cos_prevch(COStream s) {
  if ( is_string_stream(s) )
    return s->next <= s->start ? EOF : s->next[-1];
  else return s->lastch;
}

unsigned int cos_column(COStream s) {
  if ( s == NULL )
    return 0;
  else if ( is_string_stream(s) ) {
    int n;
    const unsigned char* p;
    n = 1;
    for ( p = s->next-1 ; p >= s->start ; p-- ) {
      if ( *p == '\n' )
	break;
      else n++;
    }
    return n;
  }
  else return s->column;
}

/* start new line if not already at start of line */
void cos_freshline( COStream s ) {
  int lastch;
  lastch = cos_prevch(s);
  if ( lastch != '\n' && lastch != EOF )
    cos_putch(s,'\n');
}

#if 0  /* don't think this is needed */
/* remember current position */
CSMark cos_mark(COStream s){
  return (CSMark) ftell(s->fs);
}

/* restore to remembered position; returns 0 if OK */
int cos_restore(COStream s, CSMark pos){
  return fseek(s->fs, pos, SEEK_SET);
}
#endif

/* create stream */
COStream make_file_output_stream(FILE* f, const char* path){
  register COStream s = allocate_output_stream();
  s->fs = f;
  s->pathname = str_dup(path);
  s->lastch = EOF;
  s->column = 1;
  s->start = NULL;
  s->next = NULL;
  s->bufend = NULL;
  return s;
}

char* backup_suffix = ".bak";

char* current_backup = NULL;
static COStream current_output = NULL;

COStream
open_output_file( const char* pathname, boolean binary )
{
  FILE* outfs;
  if ( pathname[0] == '-' && pathname[1] == '\0' )
    return stdout_stream;
  if ( probe_pathname(pathname) == 'D' ) {
    /* avoid renaming a directory */
    input_error(input_stream, EXS_OUTPUT,
  	        "Output path \"%s\" is not a file.\n", pathname);
    if ( keep_going )
      return NULL;
    else exit(EXS_OUTPUT);
  }
  if ( current_backup != NULL ) {
    free(current_backup);
    current_backup = NULL;
  }
  if ( backup_suffix[0] != '\0' ) {
    CIStream bakpath;
    COStream outbuf;
    const char* backup_pathname;
    outbuf = make_buffer_output_stream();
#if defined(MSDOS) && !defined(_WIN32)
    /* on MS-DOS (8.3 filenames), replace any previous extension */
    merge_pathnames( outbuf, FALSE, NULL, pathname, backup_suffix);
#else
    /* on Unix or Win32, append ".bak" to the file name */
    cos_puts(outbuf,pathname);
    cos_puts(outbuf,backup_suffix);
#endif
    bakpath = convert_output_to_input( outbuf );
    backup_pathname = cis_whole_string(bakpath);
    remove(backup_pathname);
    if ( rename(pathname,backup_pathname) == 0 ||
	 probe_pathname(backup_pathname) == 'U' ) {
      current_backup = str_dup(backup_pathname);
    }
    cis_close(bakpath);
  }

  outfs = fopen( pathname, binary? "wb" : "w" );
  if ( outfs == NULL ) {
    input_error(input_stream, EXS_OUTPUT, "Can't open output file:\n");
    perror(pathname);
    if ( ! keep_going ) {
      exit(EXS_OUTPUT);
    }
    return NULL;
  }
  else {
    current_output = make_file_output_stream(outfs,pathname);
    return current_output;
  }
}

COStream make_buffer_output_stream(){
  register COStream s = allocate_output_stream();
  s->fs = NULL;
  s->pathname = NULL;
  s->lastch = EOF;
  s->column = 0;
  if ( free_buf_list != NULL ) {
    s->start = free_buf_list;
    free_buf_list = *(unsigned char**)free_buf_list;
  }
  else
    s->start = (unsigned char*) allocate( OutBufSize, MemoryOutputBuf );
  s->next = s->start;
  s->bufend = s->start + OutBufSize;
  return s;
}

const char* cos_pathname(COStream s){
  return s==NULL? "" : s->pathname;
}

/* number of characters written so far to output buffer stream */
unsigned cis_out_length( COStream s ){
  if ( is_file_stream(s) )
    return (unsigned)ftell(s->fs);
  else return s->next - s->start;
}

static void
free_buffer( unsigned char* start, unsigned char* bufend ) {
  if ( bufend != NULL ) {
    if ( (bufend - start) == OutBufSize ) {
      *(unsigned char**)start = free_buf_list;
      free_buf_list = start;
    }
    else
      free( start );
  }
}

/* close stream */
void cos_close(COStream s){
  if ( s == NULL || s == stdout_stream )
    return;
  if ( is_file_stream(s) ) {
    fclose(s->fs);
    s->fs = NULL;
  }
  if ( is_string_stream(s) ) {
    free_buffer(s->start, s->bufend);
    s->start = NULL;
    s->next = NULL;
  }
  if ( s->pathname != NULL ) {
      free((char*)s->pathname);
      s->pathname = NULL;
    }
  if ( s == current_output )
    current_output = NULL;
  free_output_stream(s);
}

CIStream convert_output_to_input( COStream out ) {
  size_t len, buflen;
  unsigned char* buf;
  CIStream in;

  assert( is_string_stream(out) && !is_file_stream(out) );
  len = out->next - out->start;
  buflen = out->bufend - out->start;
  if ( free_buf_list == NULL && buflen == OutBufSize && len < buflen )
    buf = out->start;
  else {
    buflen = len+1;
    buf = realloc( out->start, buflen ); /* release unused space */
  }
  buf[len] = '\0';
  in = make_string_input_stream((char*)buf, len, FALSE);
  in->bufend = buf + buflen; /* cause free when input stream is closed */
  out->start = NULL;
  out->next = NULL;
  free_output_stream(out);
  return in;
}

void cos_copy_input_stream(COStream out, CIStream in) {
  if ( in != NULL ) {
    if ( is_file_stream(in) ) {
      int ch;
      while ( (ch = cis_getch(in)) != EOF )
	cos_putch(out,ch);
    }
    else {
      cos_put_len(out, (const char*)in->next, in->end - in->next);
      in->next = in->end;
    }
  }
}

