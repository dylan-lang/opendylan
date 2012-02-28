
#ifndef OPENDYLAN_CRT_TRACE_H
#define OPENDYLAN_CRT_TRACE_H

#include "run-time.h"

#include <stdio.h>

#define OPENDYLAN_CRT_TRACE

enum {
  TRACE_RUNTIME,
  TRACE_THREADS,
  TRACE_NLX,
  TRACE_TLV,
  _TRACE_MAX
};

#define TRACE_CATEGORY_NAMES \
  {                          \
    "runtime",               \
    "threads",               \
    "nlx",                   \
    "tlv",                   \
  }

#define trace_runtime(...) trace(TRACE_RUNTIME, __VA_ARGS__)
#define trace_threads(...) trace(TRACE_THREADS, __VA_ARGS__)
#define trace_nlx(...) trace(TRACE_NLX, __VA_ARGS__)
#define trace_tlv(...) trace(TRACE_TLV, __VA_ARGS__)


#ifndef OPENDYLAN_CRT_TRACE

#define trace_init()

#define trace(category, ...)

#else /* OPENDYLAN_CRT_TRACE */

void trace_init(void);
void trace_prologue(unsigned category);
void trace_epilogue(void);

extern FILE       *trace_stream;
extern DBOOL       trace_close;
extern DBOOL       trace_flush;
extern DBOOL       trace_enable[_TRACE_MAX];
extern const char *trace_names[_TRACE_MAX];

#define trace(category, ...)             \
  if(trace_enable[category]) {           \
    trace_prologue(category);            \
    fprintf(trace_stream, __VA_ARGS__);  \
    trace_epilogue();                    \
  }

#endif /* OPENDYLAN_CRT_TRACE */

#endif /* !OPENDYLAN_CRT_TRACE_H */
