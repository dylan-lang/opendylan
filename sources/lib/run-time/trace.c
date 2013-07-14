
#include "trace.h"

#include <stdlib.h>
#include <string.h>

#ifdef OPENDYLAN_CRT_TRACE

/**
 * STDIO stream for trace output
 */
FILE *trace_stream;

/**
 * True if the stream is owned by us and must be closed
 */
DBOOL trace_close;

/**
 * True if we should flush after each message
 */
DBOOL trace_flush;

/**
 * Per-category enable flags
 */
DBOOL trace_enable[_TRACE_MAX];

/**
 * Per-category name
 */
const char *trace_names[_TRACE_MAX] = TRACE_CATEGORY_NAMES;

/**
 * Close the trace stream, if appropriate
 */
static void
maybe_close(void)
{
  if (trace_stream && trace_close) {
    fclose(trace_stream);
  }
  trace_stream = NULL;
  trace_close = 0;
}

/**
 * Set up tracing to the given stdio stream
 *
 * We assume the stream is not ours, so we do not close it.
 */
static void
trace_to_stdio(FILE *stream)
{
  maybe_close();
  trace_stream = stream;
  trace_close = 0;
}

/**
 * Set up tracing to the given file
 */
static void
trace_to_file(const char *fn)
{
  maybe_close();
  trace_stream = fopen(fn, "a");
  trace_close = 1;
}

/**
 * Process a single configuration directive
 */
static void
trace_token(const char *token)
{
  int i;
  if (!strcmp(token, "all")) {
    for (i = 0; i < _TRACE_MAX; i++) {
      trace_enable[i] = 1;
    }
  } else if (!strncmp(token, "file=", 5)) {
    trace_to_file(token + 5);
  } else if (!strcmp(token, "stderr")) {
    trace_to_stdio(stderr);
  } else if (!strcmp(token, "stdout")) {
    trace_to_stdio(stdout);
  } else if (!strcmp(token, "flush")) {
    trace_flush = 1;
  } else if (!strcmp(token, "noflush")) {
    trace_flush = 0;
  } else {
    for (i = 0; i < _TRACE_MAX; i++) {
      if (!strcmp(token, trace_names[i])) {
        trace_enable[i] = 1;
      }
    }
  }
}

/**
 * Initialize the tracing system
 */
void
trace_init(void)
{
  int i;
  char *save;
  char *config;
  const char *token;

  // initialize to defaults
  trace_to_stdio(stderr);
  trace_flush = 1;

  // disable all categories
  for (i = 0; i < _TRACE_MAX; i++) {
    trace_enable[i] = 0;
  }

  // configure from environment
  config = getenv("OPEN_DYLAN_CRT_TRACE");
  if (config) {
    config = strdup(config);
    token = strtok_r(config, ":", &save);
    while (token) {
      trace_token(token);
      token = strtok_r(NULL, ":", &save);
    }
    free(config);
  }
}

/**
 * Called before each trace message
 *
 * Prints the message header.
 */
void
trace_prologue(unsigned category)
{
  flockfile(trace_stream);
  fprintf(trace_stream, "[%p] [%s] ", get_teb(), trace_names[category]);
}

/**
 * Called after each trace message
 *
 * Flushes the output stream, if configured.
 */
void
trace_epilogue(void)
{
  fprintf(trace_stream, "\n");
  if (trace_flush) {
    fflush(trace_stream);
  }
  funlockfile(trace_stream);
}

#endif
