
/* Support Breakpoints on class allocation */


#define MAX_BREAKS 100

wrapper_stats_s wrapper_breaks[MAX_BREAKS];

int wrapper_breaks_cursor = -1;

define_CRITICAL_SECTION(class_breakpoint_lock);

unsigned int class_breakpoints_pending = 0;
HANDLE class_breakpoint_events[2];

int index_for_wrapper_breaks (void *wrapper)
{
  int i;
  for (i = 0; i < wrapper_breaks_cursor + 1; i++) {
    if (wrapper_breaks[i].wrapper_address == wrapper)
      return(i);
  }
  return(-1);
}

void set_wrapper_breakpoint (void *wrapper, int count)
{
  int index = index_for_wrapper_breaks(wrapper);
  if (index < 0)
    {
      wrapper_stats_t wrapper_record = wrapper_breaks + wrapper_breaks_cursor + 1;
      ++wrapper_breaks_cursor;
      wrapper_record->wrapper_address = wrapper;
      wrapper_record->usage_size = count;
      wrapper_record->usage_count = 0;
    }
  else
    {
      wrapper_stats_t wrapper_record = wrapper_breaks + index;
      wrapper_record->usage_size = count;
    }
}

void clear_wrapper_breakpoint (void *wrapper)
{
  if (wrapper == NULL)
    wrapper_breaks_cursor = -1;
  else {
    int index = index_for_wrapper_breaks(wrapper);
    if (index >= 0)
      {
	int i;
	for (i = index; i < wrapper_breaks_cursor; i++) {
	  wrapper_stats_t wrapper_record1 = wrapper_breaks + i;
	  wrapper_stats_t wrapper_record2 = wrapper_breaks + i + 1;
	  
	  wrapper_record1->wrapper_address = wrapper_record2->wrapper_address;
	  wrapper_record1->usage_size = wrapper_record2->usage_size;
	  wrapper_record1->usage_count = wrapper_record2->usage_count;
	};
	--wrapper_breaks_cursor;
      }
  }
}


static
__inline
void *wrapper_to_class(void *wrapper)
{
  void *iclass = ((void**)wrapper)[1];
  void *class  = ((void**)iclass)[2];

  return class;
}

extern void *class_allocation_break(char *string, void *class, int count, int size);

void signal_wrapper_breakpoint (void *wrapper, int count, int size)
{
  class_allocation_break("Break on allocating instance of class",
			 wrapper_to_class(wrapper), count, size);
}

BOOL check_wrapper_breakpoint_for_objectQ = FALSE;

void check_wrapper_breakpoint (void *wrapper, int size)
{
  enter_CRITICAL_SECTION(&class_breakpoint_lock);

  while (class_breakpoints_pending) {
    set_EVENT(class_breakpoint_events[0]);
    if (wait_for_EVENT(class_breakpoint_events[1], INFINITE) != EVENT_WAIT_SUCCESS) {
      // MSG0("check_wrapper_breakpoint: error waiting for class breakpoint event\n");
    };
  }

  if (check_wrapper_breakpoint_for_objectQ) {
    signal_wrapper_breakpoint(wrapper, 1, size);
  }
  else
    if (wrapper_breaks_cursor >= 0)
      {
	int index = index_for_wrapper_breaks(wrapper);
	if (index >= 0)
	  {
	    wrapper_stats_t wrapper_record = wrapper_breaks + index;
	    
	    wrapper_record->usage_count += 1;
	    if (wrapper_record->usage_count >= wrapper_record->usage_size) {
	      signal_wrapper_breakpoint(wrapper, wrapper_record->usage_count, size);
	      wrapper_record->usage_count = 0;
	    }
	  }
      };

  leave_CRITICAL_SECTION(&class_breakpoint_lock);

}

