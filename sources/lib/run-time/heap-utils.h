/* heap-utils.h
 *
 * General utilities for displaying heap statistics
 */


#ifndef heap_utils_h
#define heap_utils_h


#include "mps.h"        /* MPS Interface */
#include "mpslib.h"     /* plinth interface */

#ifdef OPEN_DYLAN_PLATFORM_UNIX
#include "unix-types.h"
#else
#include "windows-types.h"
#endif


/* top-level interface functions */

extern void display_heap_stats_order_1 (void);

extern void display_heap_stats_order_2 (void);

extern void display_reference_trails (void);

extern void display_wrapper_breakpoints();


/* control of filtering for order_2 stats */

extern BOOL add_filter (void *wrapper);

extern void clear_filters (void);


/* control of aggregation classes for order_2 stats */

typedef struct ag_class_s *ag_class_t;

extern ag_class_t create_aggregation_class (char* name);

extern BOOL map_aggregation_class (void *wrapper, ag_class_t agclass);

extern void clear_aggregation_classes (void);


/* mid-level interface for maintaing wrapper stats */


#include "wrapper-stats.h"

#define STAT_SIZE 10000

extern wrapper_stats_s wrapper_stats[STAT_SIZE];

extern void clear_wrapper_stats ();

extern void display_wrapper_stats ();

extern void add_stat_for_object (void *object, void* wrapper, int size);

extern void set_wrapper_breakpoint (void *wrapper, int count);

extern void clear_wrapper_breakpoint (void *wrapper);

extern void check_wrapper_breakpoint (void *wrapper, int size);

extern BOOL check_wrapper_breakpoint_for_objectQ;

extern int wrapper_breaks_cursor;


/* General implementation support */


extern void report_message (char* message);

extern void report_error (char* message);

extern void report_break (char* message);

extern int object_repeated_size (void *object, int fixed_offset);

extern int size_of_object (void *object, void* wrapper);

/* tracer function should return FALSE to abort tracing further fields */
typedef BOOL (*object_tracer_t)(mps_addr_t object, mps_addr_t parent,
                                int parent_size, void *env);

extern int trace_object (mps_addr_t parent, object_tracer_t fn, void* env);


extern char* class_name_from_wrapper (void* wrapper);

extern void display_integer (int integer, mps_lib_FILE *stream);

extern void display_hex_address (void *address, mps_lib_FILE *stream);

extern void display_padding_for_string(char *string, char pad,
                                       int field, mps_lib_FILE *stream);

extern void *alloc_obj(size_t size);

extern void free_obj(void *obj, size_t size);

/* some variables defined elsewhere in the runtime */

extern mps_arena_t arena;

extern int mps_lib_fputs_(const char *s, int end, mps_lib_FILE *stream);

extern void mps_lib_abort(void);


/* class_name_size
 *   a reasonable constant width for displaying Dylan class names
 */
#define class_name_size 45



/* The hash table interface */


typedef struct table_s *table_t;

extern BOOL table_create(table_t *tableReturn, size_t length);
extern void table_destroy(table_t table);
extern BOOL table_define(table_t table, void *key, void *value);
extern BOOL table_redefine(table_t table, void *key, void *value);
extern BOOL table_lookup(void **valueReturn, table_t table, void *key);
extern BOOL table_remove(table_t table, void *key);
extern size_t table_count(table_t table);
extern void table_map(table_t table, void(*fun)(void *key, void *value));




#endif /* heap_utils_h */
