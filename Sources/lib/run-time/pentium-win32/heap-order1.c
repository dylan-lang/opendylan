
/* Support for finding the count and size for each class (wrapper) 
 * in the heap .
 */

#include "heap-utils.h"

wrapper_stats_s wrapper_stats[STAT_SIZE];

static int wrapper_cursor;
static int wrapper_preassignments;


/*
extern void *KLpairGYdylanVdylanW;
extern void *KLsimple_object_vectorGYdylanVdylanW;
*/

static void preassign_wrapper (void* wrapper, int index)
{
  wrapper_stats[wrapper_cursor].wrapper_address = wrapper;
  wrapper_cursor++;
  wrapper_preassignments++;
}

void clear_wrapper_stats (void)
{
  int i;
  wrapper_cursor = 0;
  wrapper_preassignments = 0;
  for (i = 0; i < STAT_SIZE; i++) {
    wrapper_stats[i].wrapper_address = (void *)0;
    wrapper_stats[i].usage_count = 0;
    wrapper_stats[i].usage_size = 0;
  }
  /*
  pre_assign_wrapper(&KLpairGYdylanVdylanW);
  pre_assign_wrapper(&KLsimple_object_vectorGYdylanVdylanW);
  */
}

static int default_index_for_wrapper (void *wrapper)
{
  int i;
  for (i = wrapper_preassignments; i < wrapper_cursor; i++) {
    if (wrapper_stats[i].wrapper_address == wrapper)
      return(i);
  }
  if (wrapper_cursor < STAT_SIZE) {
    int cursor = wrapper_cursor;
    wrapper_stats[cursor].wrapper_address = wrapper;
    wrapper_cursor++;
    return(cursor);
  }
  report_error("Too many different classes encountered while walking the heap");
  return(STAT_SIZE - 1);
}


static int index_for_wrapper (void *wrapper)
{
  /*
  if (wrapper == &KLpairGYdylanVdylanW)
    return(0);
  else if (wrapper == &KLsimple_object_vectorGYdylanVdylanW)
    return(1);
  else
  */
    return default_index_for_wrapper(wrapper);
}

void add_stat_for_object (void *object, void* wrapper, int size)
{
  int index = index_for_wrapper(wrapper);
  wrapper_stats[index].usage_count += 1;
  wrapper_stats[index].usage_size += size;
}

static void record_order_1_object (mps_addr_t object, mps_fmt_t format, 
                                   mps_pool_t pool, void *p1, size_t p2)
{
  void *wrapper = *(void **)object;
  if (wrapper && ((int)wrapper & 3) == 0) {
    add_stat_for_object(object, wrapper, size_of_object(object, wrapper));
  }
}

static int sort_criterion_for_index (int index)
{
  return wrapper_stats[index].usage_size;
}

static int biggest_below_value (int value)
{
  int i;
  int largest_found = -1;
  for (i = 0; i < wrapper_cursor; i++) {
    int count = sort_criterion_for_index(i);
    if ((count < value) && (count > largest_found)) {
      largest_found = count;
    }
  }
  return largest_found;
}


static void display_stat_line(char *message, 
                              int count, int size, mps_lib_FILE *stream)
{
  mps_lib_fputs_(message, class_name_size, stream);
  display_padding_for_string(message, ' ', class_name_size, stream);
  display_integer(count, stream);
  mps_lib_fputc(' ', stream);
  display_integer(size, stream);
  mps_lib_fputc('\n', stream);
}

static void display_one_wrapper(int index, mps_lib_FILE *stream)
{
  wrapper_stats_t stat = wrapper_stats + index;
  char *class_name = class_name_from_wrapper(stat->wrapper_address);
  display_stat_line(class_name, stat->usage_count, stat->usage_size, stream);
}


static void display_wrappers_of_size(int size, mps_lib_FILE *stream)
{
  int i;
  for (i = 0; i < wrapper_cursor; i++) {
    int count = sort_criterion_for_index(i);
    if (count == size) {
      display_one_wrapper(i, stream);
    }
  }
}

static void display_totals(mps_lib_FILE *stream)
{
  int i;
  int tot_size = 0;
  int tot_count = 0;
  for (i = 0; i < wrapper_cursor; i++) {
    tot_size += wrapper_stats[i].usage_size;
    tot_count += wrapper_stats[i].usage_count;
  }
  display_stat_line("TOTAL:", tot_count, tot_size, stream);
}


#define very_big 0x7fffffff

void display_wrapper_stats (void)
{
  int largest;
  mps_lib_FILE *stream = mps_lib_get_stdout();
  char *message = "Start of heap statistics";
  mps_lib_fputc('\n', stream);
  mps_lib_fputs(message, stream);
  display_padding_for_string(message, ' ', class_name_size, stream);
  mps_lib_fputs("   (count)     (size)", stream);
  mps_lib_fputs("\n\n", stream);
  display_totals(stream);
  mps_lib_fputc('\n', stream);
  for (largest = biggest_below_value(very_big); 
       largest >= 0; 
       largest = biggest_below_value(largest)) {
    display_wrappers_of_size(largest, stream);
  }
  mps_lib_fputs("End of heap statistics\n\n", stream);
}


void display_heap_stats_order_1 (void)
{
  clear_wrapper_stats();
  mps_arena_formatted_objects_walk(arena, &record_order_1_object, 0, 0);
  display_wrapper_stats();
}
