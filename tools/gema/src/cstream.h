/* character stream functions */

/* $Id: cstream.h,v 1.4 1996/04/08 05:05:37 gray Exp $ */

/*********************************************************************
  This file is part of "gema", the general-purpose macro translator,
  written by David N. Gray <dgray@acm.org> in 1994 and 1995.
  You may do whatever you like with this, so long as you retain
  an acknowledgment of the original source.
 *********************************************************************/

#ifndef CSTREAM_H
#define CSTREAM_H

#include <stdio.h> /* for FILE */
#include <time.h>  /* for time_t */

#include "bool.h"
#include "main.h" /* for Exit_States */

/* ============    Input Streams ==============  */

typedef struct input_stream_struct* CIStream;

typedef struct {
  long position;
  unsigned long line;
  unsigned column;
  short int prevch;
  short int peekch;
} MarkBuf;

/* read one character */
int cis_getch(CIStream s);

/* return next or previous character */
int cis_peek(CIStream s);
int cis_prevch(CIStream s);

/* remember current position */
void cis_mark(CIStream s, MarkBuf* b);

/* restore to remembered position; returns 0 if OK */
int cis_restore(CIStream s, MarkBuf* b);

/* release saved position (will not be returning to it) */
void cis_release(CIStream s, MarkBuf* b);

void cis_rewind(CIStream s);

void skip_whitespace( CIStream s );

/* create stream */
CIStream make_file_input_stream(FILE* f, const char* path);
CIStream make_string_input_stream(const char* start, size_t length,
				boolean copy);
CIStream clone_input_stream( CIStream in );

CIStream
open_input_file( const char* pathname, boolean binary );

/* return name of a file stream, or NULL */
const char* cis_pathname(CIStream s);

/* return current line and column number (first is 1) */
unsigned long cis_line(CIStream s);
unsigned int cis_column(CIStream s);

/* distinguishes the input file from internal streams */
boolean cis_is_file(CIStream s);

/* return modification time of file; 0 if not applicable */
time_t cis_mod_time(CIStream s);

/* test whether a file exists; return 'U', 'F', 'D' or 'V' */
char probe_pathname(const char* pathname);

/* close stream */
void cis_close(CIStream s);

/* report a syntax or semantic error in the input */
void input_error( CIStream s, Exit_States code, const char* format, ... );

extern CIStream input_stream; /* used only for error message location */

/* ============    Output Streams ==============  */

typedef struct output_stream_struct* COStream;

/* write one character */
void cos_putch(COStream s, int c);

/* write some number of spaces */
void cos_spaces(COStream s, int n);

/* write null-terminated string */
void cos_puts(COStream s, const char* x);

/* write arbitrary data */
void cos_put_len(COStream s, const char* x, size_t len);

/* start new line if not already at start of line */
void cos_freshline( COStream s );

/* return the last character written, or EOF if nothing written yet */
int cos_prevch(COStream s);

/* create stream */
COStream make_file_output_stream (FILE* f, const char* path);

COStream
open_output_file( const char* pathname, boolean binary );

extern char* backup_suffix;
extern char* current_backup;

/* return name of a file stream, or NULL */
const char* cos_pathname(COStream s);

/* close stream */
void cos_close(COStream s);

/* create buffer to hold output and then read it back in */
COStream make_buffer_output_stream(void);
CIStream convert_output_to_input( COStream out );
char* cis_whole_string ( CIStream in );
unsigned cis_length( CIStream in );

/* column number (1 when at beginning of line) */
unsigned int cos_column(COStream s);

/* number of characters written so far to output buffer stream */
unsigned cis_out_length( COStream out );

/* copy input stream to output stream */
void cos_copy_input_stream(COStream out, CIStream in);

 /* ==========  utilities  ===========  */

void
merge_pathnames( COStream out, boolean just_dir,
		 const char* dpath, const char* npath, const char* tpath );

void
expand_wildcard ( const char* file_spec, COStream out );

extern COStream stdout_stream;
extern CIStream stdin_stream;

#endif
