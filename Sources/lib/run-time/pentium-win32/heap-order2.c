#include "heap-utils.h"
#include <assert.h>


/****  support for filtering ****/

static table_t filter_table = NULL;  /* table of filters, keyed by wrapper */


BOOL add_filter (void *wrapper)
{
  BOOL res;
  res = table_define(filter_table, wrapper, wrapper);
  return res;
}

void clear_filters (void)
{
  BOOL res;
  if (filter_table != NULL) {
    table_destroy(filter_table);
  }
  res = table_create(&filter_table, 16);
  assert(res == TRUE);
}

static BOOL class_is_filtered (void *wrapper)
{
  void *val;
  return table_lookup(&val, filter_table, wrapper);
}

/* @@@@@ HACK TO TEST FILTERING */

/* set these variables from a debugger to wrapper values to test filters */
void *o2_default_filter_1 = NULL; 
void *o2_default_filter_2 = NULL;
void *o2_default_filter_3 = NULL;


static void add_default_filter (void *wrapper)
{
  if (wrapper != NULL) {
    BOOL res;
    res = add_filter(wrapper);
    assert(res == TRUE);
  }
}

static void add_default_filters (void)
{
  add_default_filter(o2_default_filter_1);
  add_default_filter(o2_default_filter_2);
  add_default_filter(o2_default_filter_3);
}

/* @@@@ END OF HACK */


void maybe_initialize_filters (void)
{
  if (filter_table == NULL) {
    clear_filters();
    add_default_filters();
  }
}


/**** support for aggregation ****/

static table_t aggregation_table = NULL;  /* table of aggregations */

static table_t wrapper_table = NULL;  /* table of wrapper -> aggregations */


/* root_aggregation -- magic aggregation class for roots
 *
 * special aggregation class for references from GC roots.
 */

static ag_class_t root_aggregation = NULL; 


typedef struct ag_class_s {
  union {
    void *wrapper_struct; /* for normal wrappers */
    char* name; /* name for agregation class */
  } the;
} ag_class_s;


static ag_class_t wrapper_aggregation_class (void *wrapper)
{
  ag_class_t agclass;
  if (table_lookup(&agclass, wrapper_table, wrapper)) {
    return agclass;
  } else {
    return (ag_class_t)wrapper;
  }
}

static BOOL aggregated_class (ag_class_t agclass)
{
  void *val;
  return table_lookup(&val, aggregation_table, agclass);
}

static char *aggregated_class_name (ag_class_t agclass)
{
  if (aggregated_class(agclass)) {
    return agclass->the.name;
  } else {
    return class_name_from_wrapper(&agclass->the.wrapper_struct);
  }
}

ag_class_t create_aggregation_class (char* name)
{
  BOOL res;
  ag_class_t agclass = alloc_obj(sizeof(ag_class_s));
  agclass->the.name = name;
  res = table_define(aggregation_table, agclass, agclass);
  assert(res == TRUE);
  return agclass;
}

BOOL map_aggregation_class (void *wrapper, ag_class_t agclass)
{
  BOOL res;
  res = table_define(wrapper_table, wrapper, agclass);
  return res;
}


static void clear_aggregation_entry (void *key, void *value)
{
  ag_class_t agclass = key;
  free_obj(agclass, sizeof(ag_class_s));
}

void clear_aggregation_classes (void)
{
  BOOL res;
  if (aggregation_table != NULL) {
    table_map(aggregation_table, clear_aggregation_entry);
    table_destroy(aggregation_table);
  }
  res = table_create(&aggregation_table, 16);
  assert(res == TRUE);

  if (wrapper_table != NULL) {
    table_destroy(wrapper_table);
  }
  res = table_create(&wrapper_table, 16);
  assert(res == TRUE);

  root_aggregation = create_aggregation_class("GC-ROOT");
}

/* @@@@@ HACK TO TEST AGGREGATIONS */

/* set these variables from a debugger to wrapper values to test aggregations */
void *o2_default_aggregation_1 = NULL; 
void *o2_default_aggregation_2 = NULL;
void *o2_default_aggregation_3 = NULL;

static ag_class_t collection_aggregation = NULL; 



static void add_default_aggregation (void *wrapper)
{
  if (wrapper != NULL) {
    BOOL res;
    res = map_aggregation_class(wrapper, collection_aggregation);
    assert(res == TRUE);
  }
}

static void add_default_aggregations (void)
{
  if (collection_aggregation == NULL) {
    collection_aggregation = create_aggregation_class("HACK-COLLECTION");
  }
  add_default_aggregation(o2_default_aggregation_1);
  add_default_aggregation(o2_default_aggregation_2);
  add_default_aggregation(o2_default_aggregation_3);
}

/* @@@@ END OF HACK */


void maybe_initialize_aggregations (void)
{
  if (aggregation_table == NULL) {
    clear_aggregation_classes();
    add_default_aggregations();
  }
}




/**** basic data structures ****/



/* stats_table -- The root of the order2 stats data
 *
 * This is a table keyed by aggregation class (or wrapper).
 * Values in the table are of type stats_t, and have non-NULL
 * table fileds for 2nd order data.
 */

static table_t stats_table = NULL;  /* root of order2 stats data */



typedef struct stats_s *stats_t;

typedef struct stats_s {
  int  usage_count; /* number of instances found */
  int  usage_size;  /* total size of instances found */
  table_t table;    /* table breaking down objects referenced by class */
} stats_s;


static stats_t stats_create (void)
{
  stats_t stats = alloc_obj(sizeof(stats_s));
  stats->usage_count = 0;
  stats->usage_size = 0;
  stats->table = NULL;
  return stats;
}

static void stats_destroy (stats_t stats)
{
  free_obj(stats, sizeof(stats_s));
}

static void stats_update (stats_t stats, int size)
{
  stats->usage_count += 1;
  stats->usage_size += size;
}

static stats_t order2_stats_create (void)
{
  stats_t o2stats = stats_create();
  BOOL res = table_create(&o2stats->table, 16);
  assert(res == TRUE);
  return o2stats;
}

static stats_t leaf_stats_create (void)
{
  return stats_create();
}



/* seen_table -- Internal data for order2 stats
 *
 * This table is keyed by objects to indicate whether and object
 * has been previously encountered.
 */

static table_t seen_table = NULL;  /* table of objects seen so far */


static void create_seen_table (void)
{
  BOOL res;
  res = table_create(&seen_table, 256 * 1024);
  assert(res == TRUE);
}

static void destroy_seen_table (void)
{
  table_destroy(seen_table);
}

static void record_object_seen (void *object)
{
  void *val;
  if (!table_lookup(&val, seen_table, object)) {
    BOOL res;
    res = table_define(seen_table, object, object);
    assert(res == TRUE);
  }
}

static BOOL object_is_seen (void *object)
{
  void *val;
  return table_lookup(&val, seen_table, object);
}



/* Clearing up the tables */

static void clear_leaf_entry (void *key, void *value)
{
  stats_t lstats = value;
  assert(lstats->table == NULL);
  stats_destroy(lstats);
}

static void clear_order2_entry (void *key, void *value)
{
  stats_t o2stats = value;
  table_map(o2stats->table, clear_leaf_entry);
  stats_destroy(o2stats);
}

static void clear_order2_stats (void)
{
  BOOL res;
  if (stats_table != NULL) {
    table_map(stats_table, clear_order2_entry);
    table_destroy(stats_table);
  }
  res = table_create(&stats_table, 1024);
  assert(res == TRUE);
  maybe_initialize_filters();
  maybe_initialize_aggregations();
}

static BOOL add_stats_for_object (mps_addr_t object, mps_addr_t parent, 
                                  int parent_size, void *pclass)
{
  void *owrapper;
  if (object && ((int)object & 3) == 0) {
    /* this is probably a heap object, but check the wrapper */
    owrapper = *(void **)object;
    if (owrapper && ((int)owrapper & 3) == 0) {
      void **wwrapper = *(void ***)owrapper; /* wrapper wrapper */
      if (!wwrapper || (((int)wwrapper & 3) != 0) || 
          (wwrapper != *wwrapper)) {
        /* not a valid wrapper wrapper. do nothing */
      } else if (object_is_seen(object)) {
        /* we've previously encountered this object. do nothing */
      } else if (class_is_filtered(owrapper)) {
        record_object_seen(object); /* mark it seen to avoid cycles */
        /* trace through all references to filtered objects */
        trace_object(object, add_stats_for_object, pclass);
      } else {
        int osize = size_of_object(object, owrapper);
        ag_class_t oclass = wrapper_aggregation_class(owrapper);
        stats_t o2stats;
        stats_t lstats;

        record_object_seen(object);
        
        if (!table_lookup(&o2stats, stats_table, oclass)) {
          BOOL res;
          o2stats = order2_stats_create();
          res = table_define(stats_table, oclass, o2stats);
          assert(res == TRUE);
        }
        stats_update(o2stats, osize);
        
        if (!table_lookup(&lstats, o2stats->table, pclass)) {
          BOOL res;
          lstats = leaf_stats_create();
          res = table_define(o2stats->table, pclass, lstats);
          assert(res == TRUE);
        }
        stats_update(lstats, osize);
      }
    }
  }
  return TRUE;
}
  
static void record_order_2_root (mps_addr_t *objectref, mps_root_t root,
                                 void *p1, size_t p2)
{
  mps_addr_t object = *objectref;
  add_stats_for_object(object, NULL, 0, root_aggregation);
}

static void record_order_2_object (mps_addr_t object, mps_fmt_t format, 
                                   mps_pool_t pool, void *p1, size_t p2)
{
  void *wrapper = *(void **)object;
  if (wrapper && ((int)wrapper & 3) == 0) {
    /* ignore filtered objects at this stage */
    if (!class_is_filtered(wrapper)) {
      /* Now find all the children referenced from this parent */
      ag_class_t agclass = wrapper_aggregation_class(wrapper);
      /* use the aggregation class as the environment for the tace */
      trace_object(object, add_stats_for_object, agclass);
    }
  }
}

static int sort_criterion_for_index (stats_t stats)
{
  return stats->usage_size;
}


/* global variables used while iterating over tables */

static int largest_found = -1;
static int limiting_value = -1;
static int wanted_size = -1;
static int margin_indent = -1;
static mps_lib_FILE *std_stream;

static void display_stats_in_table (table_t table, int indent);

static void check_if_biggest_stat (void *key, void *value)
{
  stats_t stats = value;
  int count = sort_criterion_for_index(stats);
  if ((count < limiting_value) && (count > largest_found)) {
      largest_found = count;
  }
}

static int biggest_below_value (table_t table, int value)
{
  largest_found = -1;
  limiting_value = value;
  table_map(table, check_if_biggest_stat);
  return largest_found;
}


static void display_stat_line (char *message, int count, int size)
{
  int i;
  int name_size = class_name_size - margin_indent;
  if (margin_indent == 0) {
    mps_lib_fputc('\n', std_stream);
  } else {
    for (i = 0; i < margin_indent; i++) {
      mps_lib_fputc(' ', std_stream);
    }
  }
  mps_lib_fputs_(message, name_size, std_stream);
  display_padding_for_string(message, ' ', name_size, std_stream);
  display_integer(count, std_stream);
  mps_lib_fputc(' ', std_stream);
  display_integer(size, std_stream);
  mps_lib_fputc('\n', std_stream);
}

static void display_one_stat (ag_class_t agclass, stats_t stat)
{
  char *class_name = aggregated_class_name(agclass);
  display_stat_line(class_name, stat->usage_count, stat->usage_size);
  if (stat->table != NULL) {
    display_stats_in_table(stat->table, margin_indent + 4);
  }
}

static void display_if_correct_size (void *key, void *value)
{
  stats_t stat = value;
  ag_class_t agclass = (ag_class_t)key;
  if (sort_criterion_for_index(stat) == wanted_size) {
    display_one_stat(agclass, stat);
  }
}

static void display_stats_of_size (table_t table, int size)
{
  int old_wanted_size = wanted_size;
  wanted_size = size;
  table_map(table, display_if_correct_size);
  wanted_size = old_wanted_size;
}

static void display_stats_in_table (table_t table, int indent)
{
#define very_big 0x7fffffff
  int old_margin_indent = margin_indent;
  int largest;
  margin_indent = indent;
  for (largest = biggest_below_value(table, very_big); 
       largest >= 0; 
       largest = biggest_below_value(table, largest)) {
    display_stats_of_size(table, largest);
  }
  margin_indent = old_margin_indent;
}

static void display_order2_stats (void)
{
  char *message = "Start of order 2 heap statistics";
  std_stream = mps_lib_get_stdout();
  mps_lib_fputc('\n', std_stream);
  mps_lib_fputs(message, std_stream);
  display_padding_for_string(message, ' ', class_name_size, std_stream);
  mps_lib_fputs("   (count)     (size)", std_stream);
  mps_lib_fputs("\n\n", std_stream);
  display_stats_in_table(stats_table, 0);
  mps_lib_fputs("End of order 2 heap statistics\n\n", std_stream);
}



void display_heap_stats_order_2 (void)
{
  clear_order2_stats();
  create_seen_table();
  mps_arena_formatted_objects_walk(space, &record_order_2_object, 0, 0);
  mps_arena_roots_walk(space, &record_order_2_root, 0, 0);
  display_order2_stats();
  destroy_seen_table();
}
