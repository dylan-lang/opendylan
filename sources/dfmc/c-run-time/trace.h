/*
 * CRT tracing facility
 *
 * This is a simple printf-based tracing facality for use
 * in debugging c-run-time. It can be compiled out completely.
 *
 * Tracing is organized in categories, each of which can
 * be enabled or disabled individually.
 *
 * Traces can be sent to stderr, stdout or a file,
 * each with flushing enabled or disabled.
 *
 * The subsystem can be configured through the environment
 * variable OPEN_DYLAN_CRT_TRACE using the following
 * directives, separated by colons (:).
 *
 * Category directives:
 *  all     - enable all categories
 *  runtime - trace runtime lifecycle
 *  threads - trace thread lifecycle
 *  nlx     - trace non-local exits
 *  tlv     - trace thread-local variables
 *
 * Output directives (last takes effect):
 *  stderr   - output trace on stderr
 *  stdout   - output trace on stdout
 *  file=XXX - output trace to file XXX
 *
 * Flushing directives (last takes effect):
 *  flush   - flush after each message
 *  noflush - do not flush after messages
 *
 */
#ifndef OPENDYLAN_CRT_TRACE_H
#define OPENDYLAN_CRT_TRACE_H

#include "run-time.h"

#include <stdio.h>

/* Undefine this to disable the trace facility */
#define OPENDYLAN_CRT_TRACE

/* Enumeration of trace categories */
enum {
  TRACE_RUNTIME,
  TRACE_THREADS,
  TRACE_NLX,
  TRACE_TLV,
  _TRACE_MAX
};

/* Names of trace categories */
#define TRACE_CATEGORY_NAMES \
  {                          \
    "runtime",               \
    "threads",               \
    "nlx",                   \
    "tlv",                   \
  }

/* Aliases for trace categories */
#define trace_runtime(...) trace(TRACE_RUNTIME, __VA_ARGS__)
#define trace_threads(...) trace(TRACE_THREADS, __VA_ARGS__)
#define trace_nlx(...) trace(TRACE_NLX, __VA_ARGS__)
#define trace_tlv(...) trace(TRACE_TLV, __VA_ARGS__)


/* Select dummy implementation if tracing not enabled */
#ifndef OPENDYLAN_CRT_TRACE

#define trace_init()
#define trace(category, ...)

#else /* OPENDYLAN_CRT_TRACE */

void trace_init(void);

/* Internal functions */
void trace_prologue(unsigned category);
void trace_epilogue(void);

/* Internal variables */
extern FILE       *trace_stream;
extern DBOOL       trace_close;
extern DBOOL       trace_flush;
extern DBOOL       trace_enable[_TRACE_MAX];
extern const char *trace_names[_TRACE_MAX];

/**
 * Submit a trace message in CATEGORY
 *
 * Second argument must be a format string.
 */
#define trace(category, ...)             \
  if(trace_enable[category]) {           \
    trace_prologue(category);            \
    fprintf(trace_stream, __VA_ARGS__);  \
    trace_epilogue();                    \
  }

#endif /* OPENDYLAN_CRT_TRACE */

#endif /* !OPENDYLAN_CRT_TRACE_H */
