


/* controlling variables */

/* check_heap_stats_order_1  -- controls 1st order heap breakdown display */
int check_heap_stats_order_1 = 1;

/* check_heap_stats_order_2  -- controls 2nd order heap breakdown display */
int check_heap_stats_order_2 = 1;

/* check_reference_trails  -- controls tracing reference trails */
int check_reference_trails = 1; 


#include "heap-utils.h"


void display_stats_for_memory_usage ()
{
  if (check_heap_stats_order_1) {
    display_heap_stats_order_1();
  }
  if (check_heap_stats_order_2) {
    display_heap_stats_order_2();
  }
  if (check_reference_trails) {
    display_reference_trails();
  }
}



#include "break.c"


void display_wrapper_breakpoints()
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  if (wrapper_breaks_cursor >= 0)
    {
      int i;
      mps_lib_fputs("Object allocation breakpoints\n\n", stream);
      mps_lib_fputs("   (class-name)                                    (count)\n\n", stream);
      for (i = 0; i < wrapper_breaks_cursor + 1; i++) {
	wrapper_stats_t wrapper_record = wrapper_breaks + i;
	char *class_name = class_name_from_wrapper(wrapper_record->wrapper_address);

	mps_lib_fputs_(class_name, class_name_size, stream);
	display_padding_for_string(class_name, ' ', class_name_size, stream);
	display_integer(wrapper_record->usage_size, stream);
	mps_lib_fputc('\n', stream);
      }
    }
  else
    mps_lib_fputs("No active object allocation breakpoints\n\n", stream);
}
