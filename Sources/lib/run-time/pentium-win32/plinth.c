/* impl.c.mpsliban: HARLEQUIN MEMORY POOL SYSTEM LIBRARY INTERFACE (DYLAN)
 *
 *
 * PURPOSE
 *
 * .purpose: The purpose of this code is
 *   1. to connect the MPS Library Interface to the Dylan runtime
 *      with no dependency on the C runtime.
 */

#include "mpslib.h"
#include <windows.h>
#include <stdio.h>
#include <time.h>

/* First some simple stream buffer management */

#define BUFFER_SIZE 200
#define BUFFER_LIMIT (BUFFER_SIZE + 1)

typedef struct message_buffer_s  *message_buffer_t;

typedef struct message_buffer_s {
  int   buf_index;
  char  buf_data [BUFFER_LIMIT];
} message_buffer_s;


static message_buffer_s err_buf_struct = {0, ""};
static message_buffer_s out_buf_struct = {0, ""};

static message_buffer_t err_buf = &err_buf_struct;
static message_buffer_t out_buf = &out_buf_struct;

extern BOOL dylan_streamQ;
extern char *dylan_buffer;
extern int  dylan_buffer_pos;
extern int  dylan_buffer_size;

int buffer_add (message_buffer_t buf, char c)
{
  int res = 0;
  if (dylan_streamQ)
    {
      if (dylan_buffer_pos < dylan_buffer_size)
	dylan_buffer[dylan_buffer_pos++] = c;
      else {
	/* HACK -- delete last line in dylan buffer on overflow */
	int pos = dylan_buffer_pos - 1;

	if (dylan_buffer[pos] != '\n') {
	  while (dylan_buffer[--pos] != '\n');
	  while (pos < dylan_buffer_pos)
	    dylan_buffer[++pos] = ' ';
	  dylan_buffer[pos - 1] = '\n'; }
      }
    }
  else
    {
      buf->buf_data[buf->buf_index++] = c;
      if ((c == '\n') || (c == '\0') || (buf->buf_index >= BUFFER_SIZE))
	res = buf->buf_index;
    };
  return(res);
}

char *buffer_contents (message_buffer_t buf)
{
  buf->buf_data[buf->buf_index] = '\0';
  return(buf->buf_data);
}

void buffer_reset (message_buffer_t buf)
{
  buf->buf_index = 0;
}


static HANDLE current_log_file = 0;


HANDLE ensure_log_file (void)
{
  if (current_log_file == 0)
    {
      current_log_file = CreateFile("dylan-runtime.log", 
				    GENERIC_WRITE, 
				    FILE_SHARE_READ, 
				    0, 
				    OPEN_ALWAYS, 
				    FILE_ATTRIBUTE_NORMAL,
				    0);
    }
  return(current_log_file);
}

void ensure_log_file_closed (void)
{
  if (current_log_file != 0)
    {
      if (CloseHandle(current_log_file))
	current_log_file = 0;
    }
}  


void plinth_flush_string_to_file (char *string, int length)
{
  int written;
  HANDLE log_file = ensure_log_file();
  if (log_file != 0)
    WriteFile(log_file, string, length, &written, 0);
}

void plinth_flush_string_to_debugger (char *string, int length)
{
  OutputDebugString(string);
}

void plinth_flush_string (char *string, int length)
{
  plinth_flush_string_to_debugger(string, length);
  plinth_flush_string_to_file(string, length);
}

int plinth_putc (message_buffer_t buf, int c)
{
  int num_to_flush = buffer_add(buf, (char)c);
  if (num_to_flush > 0)
    {
      plinth_flush_string(buffer_contents(buf), num_to_flush);
      buffer_reset(buf);
    };
  return(c);
}

/* Now the plinth implementation itself */


void mps_lib_abort(void)
{ 
  ensure_log_file_closed();
  DebugBreak();
}

int mps_lib_get_EOF(void)
{
  return 0;
}

mps_lib_FILE *mps_lib_get_stderr(void)
{
  return (mps_lib_FILE *)err_buf;
}

mps_lib_FILE *mps_lib_get_stdout(void)
{
  return (mps_lib_FILE *)out_buf;
}

int mps_lib_fputc(int c, mps_lib_FILE *stream)
{
  return plinth_putc((message_buffer_t)stream, c);
}

int mps_lib_fputs(const char *s, mps_lib_FILE *stream)
{
  int i = 0;
  char c;
  while (c = s[i++])
    {
      plinth_putc((message_buffer_t)stream, c);
    };
  return 1;
}

int mps_lib_fputs_(const char *s, int end, mps_lib_FILE *stream)
{
  int i = 0;
  char c;
  while ((i < end) && (c = s[i++]))
    {
      plinth_putc((message_buffer_t)stream, c);
    };
  return 1;
}

void mps_lib_assert_fail(const char *message)
{
  mps_lib_FILE *err = mps_lib_get_stderr();
  mps_lib_fputs("\nMPS ASSERTION FAILURE: ", err);
  mps_lib_fputs(message, err);
  mps_lib_fputs("\n", err);
  mps_lib_abort();
}

mps_clock_t mps_clock(void)
{
  return (unsigned long)clock();
}

mps_clock_t mps_clocks_per_sec(void)
{
  return (unsigned long)CLOCKS_PER_SEC;
}


unsigned long mps_lib_telemetry_control(void)
{
  return 0;
}

void *mps_lib_memset( void *dest, int c, size_t count )
{
  return memset(dest, c, count);
}


void *mps_lib_memcpy( void *dest, const void *src, size_t count )
{
  return memcpy(dest, src, count);
}


int mps_lib_memcmp(const void *s1, const void *s2, size_t n)
{
  return memcmp(s1, s2, n);
}
