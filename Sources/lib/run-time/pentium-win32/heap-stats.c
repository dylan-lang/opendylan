
#include "mps.h"        /* MPS Interface */
#include "mpslib.h"     /* plinth interface */
#include <windows.h>


extern mps_space_t space;

/* controlling variables */

int check_wrapper_stats = 1;    /* if true, the display a heap breakdown 
				   on each forced GC */



int check_reference_paths = 1;  /* if true, then permit the tracing of
				   object reference trails on on each forced
				   GC, for any rquested object/wrapper */


int prompt_before_checking_references = 0;  /* if true then prompt for another
					      object/wrapper on each forced GC
					      */

int prompt_after_checking_references = 1;  /* if true then prompt for another
					      object/wrapper after following
					      a trail */

int depth_limit = 0;          /* set this from the debugger to permit
				 roots to be found at depths greater than
				 the depth of the first found roots */

int trail_limit = 4;           /* set this from the debugger to limit
                                  the number of trails which can be reported
                                  at any depth depth */

int cold_trail_limit = 1;     /* set this from the debugger to limit
				 the number of trails which can be reported
				 when a trail goes cold */

int object_report_limit = 4;  /* set this from the debugger to limit
				 the number of objects which get reported
				 during processing for each depth */

void *object_to_follow = 0;   /* set this from a debugger if you want 
				 to trace to an object */

void *wrapper_to_follow = 0;  /* set this from a debugger for tracing 
				 to the first instance found with this 
				 wrapper */




/*
extern void *KLpairGYdylanVdylanW;
extern void *KLsimple_object_vectorGYdylanVdylanW;
*/

extern int mps_lib_fputs_(const char *s, int end, mps_lib_FILE *stream);
extern void mps_lib_abort(void);

void report_message (char* message)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();  
  mps_lib_fputs(message, stream);
}

void report_error (char* message)
{
  mps_lib_FILE *stream = mps_lib_get_stderr();  
  mps_lib_fputs("\nError:\n", stream);
  mps_lib_fputs(message, stream);
  mps_lib_fputc('\n', stream);
  mps_lib_abort();
}


void report_break (char* message)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();  
  mps_lib_fputs("Break to debugger:\n    ", stream);
  mps_lib_fputs(message, stream);
  mps_lib_fputc('\n', stream);
  DebugBreak();
}


/* Support for finding the count and size for each class (wrapper) in the heap */



typedef struct wrapper_stats_s *wrapper_stats_t;

typedef struct wrapper_stats_s {
  void *wrapper_address;
  int  usage_count;
  int  usage_size;
} wrapper_stats_s;

#define STAT_SIZE 10000

wrapper_stats_s wrapper_stats[STAT_SIZE];

int wrapper_cursor;
int wrapper_preassignments;


void preassign_wrapper (void* wrapper, int index)
{
  wrapper_stats[wrapper_cursor].wrapper_address = wrapper;
  wrapper_cursor++;
  wrapper_preassignments++;
}

void clear_wrapper_stats ()
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

int default_index_for_wrapper (void *wrapper)
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


int index_for_wrapper (void *wrapper)
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

int wrapper_fixed_length_in_words(void* wrapper)
{
  int wf = ((int*)wrapper)[3];
  int fl = wf >> 2;
  return fl + 1; /* 1 for the header itself */
}

int wrapper_vector_scaling_in_bytes(void* wrapper)
{
  int wv = ((int*)wrapper)[4];
  int vf = wv & 7;
  switch (vf) {
  case 7: 
    return(0);
  case 4: 
  case 5: 
    return(1);  /* assume all non-word vectors are byte sized */
  default: 
    return(4);
  }
}

int object_repeated_size (void *object, int fixed_offset)
{
  int tagged_size = ((int*)object)[fixed_offset];
  return tagged_size >> 2;
}

int size_of_object (void *object, void* wrapper)
{
  int fixed = wrapper_fixed_length_in_words(wrapper);
  int vec_scale = wrapper_vector_scaling_in_bytes(wrapper);
  if (vec_scale) {
    return (4 * fixed)
           + 4 /* for the size slot */
           + (vec_scale * object_repeated_size(object, fixed));
  } else {
    return 4 * fixed;
  }
}

int traceable_word_size_of_object (void *object, void* wrapper)
{
  int fixed = wrapper_fixed_length_in_words(wrapper);
  int vec_scale = wrapper_vector_scaling_in_bytes(wrapper);
  if (vec_scale == 4) {
    return fixed
           + 1 /* for the size slot */
           + object_repeated_size(object, fixed);
  } else {
    return fixed;
  }
}

void record_an_object (mps_addr_t object, mps_fmt_t format, mps_pool_t pool, 
		       void *p1, size_t p2)
{
  void *wrapper = *(void **)object;
  if (wrapper && ((int)wrapper & 3) == 0) {
    add_stat_for_object(object, wrapper, size_of_object(object, wrapper));
  }
}

int sort_criterion_for_index (int index)
{
  return wrapper_stats[index].usage_size;
}

int biggest_below_value (int value)
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


__inline
void *wrapper_class(void *wrapper)
{
  void *iclass = ((void**)wrapper)[1];
  void *class  = ((void**)iclass)[2];

  return class;
}

char* class_name_from_wrapper (void* wrapper)
{
  void *class = wrapper_class(wrapper);
  char *name = (char*)((void**)class)[2];
  return (name + 8);
}

void display_integer (int integer, mps_lib_FILE *stream)
{  /* This is naieve. Assume no more than 7 digits */
  int remainder = integer;
  int leading = 1;
  int power;
  if (integer == 0) {
    /* special case needs the leading zero */
      mps_lib_fputs("       0", stream);
      return;
  }
  for (power = 10000000; power > 0; power = power / 10) {
    int exponent = (int)(log10(power));
    int digit = remainder / power;
    remainder = remainder % power;
    if (digit == 0) {
      mps_lib_fputc(leading ? ' ' : '0', stream);
    } else {
      leading = 0;
      mps_lib_fputc('0' + digit, stream);
    };
    if ((exponent == 6) || (exponent == 3))
      if (digit == 0) {
	mps_lib_fputc(leading ? ' ' : ',', stream);
      } else
	mps_lib_fputc(',', stream);
  }
}

void display_hex_address (void *address, mps_lib_FILE *stream)
{  
  unsigned int integer = (unsigned int)address;
  unsigned int remainder = integer;
  int leading = 1;
  unsigned int power;
  for (power = 0x10000000; power > 0; power = power / 0x10) {
    unsigned int digit = remainder / power;
    remainder = remainder % power;
    if (digit == 0) {
      mps_lib_fputc(leading ? ' ' : '0', stream);
    } else if (digit > 9) {
      leading = 0;
      mps_lib_fputc('A' + digit - 10, stream);
    } else {
      leading = 0;
      mps_lib_fputc('0' + digit, stream);
    }
  }
}



int padding_for_string(char *string, int length)
{
  int padding = length;
  char *remaining = string;
  while (*remaining) {
    remaining++;
    padding--;
  }
  return padding;
}

void display_padding_for_string(char *string, char pad, int field, mps_lib_FILE *stream)
{
  int i;
  int padding = padding_for_string(string, field);
  for (i = 0; i < padding; i++)
    mps_lib_fputc(' ', stream);
}

#define class_name_size 45


void display_stat_line (char *message, int count, int size, mps_lib_FILE *stream)
{
  mps_lib_fputs_(message, class_name_size, stream);
  display_padding_for_string(message, ' ', class_name_size, stream);
  display_integer(count, stream);
  mps_lib_fputc(' ', stream);
  display_integer(size, stream);
  mps_lib_fputc('\n', stream);
}

void display_one_wrapper(int index, mps_lib_FILE *stream)
{
  wrapper_stats_t stat = wrapper_stats + index;
  char *class_name = class_name_from_wrapper(stat->wrapper_address);
  display_stat_line(class_name, stat->usage_count, stat->usage_size, stream);
}


void display_wrappers_of_size(int size, mps_lib_FILE *stream)
{
  int i;
  for (i = 0; i < wrapper_cursor; i++) {
    int count = sort_criterion_for_index(i);
    if (count == size) {
      display_one_wrapper(i, stream);
    }
  }
}

void display_totals(mps_lib_FILE *stream)
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

void display_wrapper_stats ()
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



/* Following reference paths via a reverse trace */

/* Strategy:
 * 1. Find all objects directly referenced from the roots of the GC
 * 2. Start with a set of "parent" objects we want to track.
 * 3. Look to see if any object in the set is directly referenced from a root
 * 4. Walk the heap looking for all objects which reference objects in the set.
 * 5. Record those objects as "children" of the parents - but only if they're new.
 * 6. Consider next generation, by treating the children as next set
 * 7. Repeat from step 3
 *
 */

typedef struct obj_rec_st *obj_rec_p;

typedef struct obj_rec_st {
  obj_rec_p next;     /* Next recorded object with this hash value */
  obj_rec_p parent;   /* parent object that this object references  */
  mps_addr_t object;  /* The Dylan object this records */
} obj_rec_st;



#define TABLE_SIZE 0x1000
#define TABLE_MASK (TABLE_SIZE - 1)
#define TABLE_SHIFT 3

typedef obj_rec_p *obj_table;


int hash_val(mps_addr_t obj)
{
  return ((int)obj >> TABLE_SHIFT) & TABLE_MASK;
}


obj_rec_p root_table[TABLE_SIZE] = {NULL};      /* hash table of known roots */
obj_rec_p processed_table[TABLE_SIZE] = {NULL}; /* hash table of processed objects */
obj_rec_p current_table[TABLE_SIZE] = {NULL};   /* hash table of current set of objects */
obj_rec_p child_table[TABLE_SIZE] = {NULL};     /* hash table of child objects */


HANDLE process_heap = 0;

obj_rec_p alloc_obj_rec(mps_addr_t obj)
{
  obj_rec_p new;
  if (process_heap == 0) {
    process_heap = GetProcessHeap();
  }
  new = HeapAlloc(process_heap, 0, sizeof(obj_rec_st));
  new->object = obj;
  new->next = NULL;
  new->parent = NULL;
  return new;
}

void free_obj_rec(obj_rec_p or)
{
  HeapFree(process_heap, 0, or);
}

void free_table(obj_table table)
{
  int i;
  for (i = 0; i < TABLE_SIZE; i++) {
    obj_rec_p next = table[i];
    while (next != NULL) {
      obj_rec_p this = next;
      next = next->next;
      free_obj_rec(this);
    }
    table[i] = NULL;
  }
}


void add_to_table(obj_table table, obj_rec_p objrec)
{
  int hash = hash_val(objrec->object);
  objrec->next = table[hash];
  table[hash] = objrec;
}


obj_rec_p find_obj_in_table(obj_table table, mps_addr_t obj)
{
  int hash = hash_val(obj);
  obj_rec_p current = table[hash];
  while (current != NULL) {
    if (current->object == obj) {
      return current;
    }
    current = current->next;
  }
  return NULL;
}

/* fold the src table into the dest - clearing src in the process */
void merge_tables(obj_table dest, obj_table src)
{
  int i;
  for (i = 0; i < TABLE_SIZE;  i++) {
    obj_rec_p dchain = dest[i];
    obj_rec_p schain = src[i];
    if (schain != NULL) {
      dest[i] = schain;
      src[i] = NULL;
      if (dchain != NULL) {
        while (schain->next != NULL) {
          schain = schain->next;
        }
        schain->next = dchain;
      }
    } 
  }
}

int size_of_table(obj_table table)
{
  int i;
  int size = 0;
  for (i = 0; i < TABLE_SIZE; i++) {
    obj_rec_p current = table[i];
    while (current != NULL) {
      size++;
      current = current->next;
    }
  }
  return size;
}


obj_rec_p add_new_to_table(obj_table table, mps_addr_t obj)
{
  obj_rec_p new = alloc_obj_rec(obj);
  add_to_table(table, new);
  return new;
}

obj_rec_p new_obj_rec(mps_addr_t obj)
{
  return add_new_to_table(child_table, obj);
}

obj_rec_p new_root(mps_addr_t obj, mps_addr_t root)
{
  obj_rec_p new = add_new_to_table(root_table, obj);
  obj_rec_p parent = alloc_obj_rec(root);
  new->parent = parent;
  return new;
}


void clear_object_reference_paths ()
{
  free_table(root_table);
  free_table(processed_table);
  free_table(current_table);
  free_table(child_table);
}



void display_object_reference(void* object, mps_lib_FILE *stream)
{
  void *wrapper = ((void**)object)[0];
  char *class_name = class_name_from_wrapper(wrapper);
  mps_lib_fputs("    ", stream);
  mps_lib_fputs(class_name, stream);
  display_padding_for_string(class_name, ' ', class_name_size, stream);
  display_hex_address(object, stream);
  mps_lib_fputc('\n', stream);
}

void display_reference_paths (obj_rec_p or, mps_lib_FILE *stream)
{
  if (or != 0) {
    display_reference_paths(or->parent, stream);
    display_object_reference(or->object, stream);
  }
}

void display_cold_trail_intro (int gensize)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nFailed to find any objects referenced from a root.\n", stream);  
  mps_lib_fputs("Hence all detected objects are 'floating garbage'.\n", stream);  
  mps_lib_fputs("There were ", stream);  
  display_integer(gensize, stream);
  mps_lib_fputs(" objects encountered while processing the last depth.\n", stream);  
  mps_lib_fputs("Here are some cold trails:\n", stream);  
}

void display_cold_trail_details (obj_rec_p obj)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nStart of a cold trail of object references\n", stream);  
  display_reference_paths(obj, stream);
  mps_lib_fputs("End of cold trail\n", stream);
}

void display_trail_details (obj_rec_p obj, obj_rec_p root)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nStart of a trail of object references\n", stream);  
  display_reference_paths(obj, stream);
  mps_lib_fputs("Referenced from a root at address                     ", stream);  
  display_hex_address(root->parent->object, stream);
  mps_lib_fputc('\n', stream);

  mps_lib_fputs("End of trail\n", stream);
}

void display_generation_details (int gennum, int gensize, obj_table table)
{
  int i;
  int count = 0;
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("Processing depth ", stream);
  display_integer(gennum, stream);  
  mps_lib_fputs(" containing ", stream);
  display_integer(gensize, stream);  
  mps_lib_fputs(" objects:\n", stream);
  for (i = 0; i < TABLE_SIZE; i++) {
    obj_rec_p current = table[i];
    while (current != NULL) {
      count++;
      if (count > object_report_limit) {
        mps_lib_fputs("    To display more objects, ", stream);
        mps_lib_fputs("use the debugger to set the variable at address: ", stream);  
        display_hex_address(&object_report_limit, stream);
        mps_lib_fputc('\n', stream);
        return;
      } else {
        display_object_reference(current->object, stream);
      }
      current = current->next;
    }
  }
}

void display_reference_variables_addresses ()
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nTo find a trail of references to an object, \n", stream);
  mps_lib_fputs("use the debugger to set one of the following variables:\n", stream);  
  mps_lib_fputs("      object_to_follow  - set address ", stream);
  display_hex_address(&object_to_follow, stream);
  mps_lib_fputc('\n', stream);
  mps_lib_fputs("      wrapper_to_follow - set address ", stream);  
  display_hex_address(&wrapper_to_follow, stream);
  mps_lib_fputc('\n', stream);
}

void display_gen_limit_addresses (int gennum)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nTo find deeper trails of references to an object, \n", stream);
  mps_lib_fputs("use the debugger to set the following variable\n", stream);  
  mps_lib_fputs("to a depth limit greater than ", stream);  
  display_integer(gennum, stream);  
  mps_lib_fputs(":\n", stream);
  mps_lib_fputs("      depth_limit  - set address ", stream);
  display_hex_address(&depth_limit, stream);
  mps_lib_fputc('\n', stream);
}

void display_trail_limit_addresses (int rootnum)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nTo display more trails of references at this depth, \n", stream);
  mps_lib_fputs("use the debugger to set the following variable\n", stream);  
  mps_lib_fputs("to a trail limit greater than ", stream);  
  display_integer(rootnum, stream);  
  mps_lib_fputs(":\n", stream);
  mps_lib_fputs("      trail_limit  - set address ", stream);
  display_hex_address(&trail_limit, stream);
  mps_lib_fputc('\n', stream);
}

void display_cold_trail_limit_addresses (int reported)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nTo display more cold trails, \n", stream);
  mps_lib_fputs("use the debugger to set the following variable\n", stream);  
  mps_lib_fputs("to a trail limit greater than ", stream);  
  display_integer(reported, stream);  
  mps_lib_fputs(":\n", stream);
  mps_lib_fputs("      cold_trail_limit  - set address ", stream);
  display_hex_address(&cold_trail_limit, stream);
  mps_lib_fputc('\n', stream);
}

void add_target_object (mps_addr_t obj)
{
  new_obj_rec(obj);
}

void add_target_object_of_wrapper (mps_addr_t object, 
				   mps_fmt_t format, mps_pool_t pool, 
				   void *wrapper, int p2)
{
  void **found = (void**)object;
  void *found_wrapper = *found;
  if (found_wrapper == wrapper) {
    add_target_object(object);
  }
}


void add_target_objects_from_wrapper (void *wrapper)
{
  mps_arena_formatted_objects_walk(space, &add_target_object_of_wrapper, wrapper, 0);
}



obj_rec_p object_in_roots(mps_addr_t object)
{
  return find_obj_in_table(root_table, object);
}

obj_rec_p object_in_processed_set(mps_addr_t object)
{
  return find_obj_in_table(processed_table, object);
}

obj_rec_p object_in_current_set(mps_addr_t object)
{
  return find_obj_in_table(current_table, object);
}


int size_of_current_set()
{
  return size_of_table(current_table);
}

void advance_generation ()
{
  /* merge the current into the processed set */
  /* then merge the children to make them thecurrent set */
  merge_tables(processed_table, current_table);
  merge_tables(current_table, child_table);
}



void look_for_reference (mps_addr_t object, mps_fmt_t format, mps_pool_t pool, 
			 void *p1, int p2)
{
  mps_addr_t *refs = (mps_addr_t*)object;
  mps_addr_t wrapper = *refs;
  int size;
  int i;
  if ((wrapper == NULL) || ((int)wrapper & 3) != 0) {
    return; /* not a proper object */
  }
  size = traceable_word_size_of_object(object, wrapper);
  for (i = 1; i < size; i++) {
    obj_rec_p refrec = object_in_current_set(refs[i]);
    if (refrec != NULL) {
      /* Have found a ref to the current gen */
      if (object_in_processed_set(object)
          || object_in_current_set(object)) {
	/* Found a reference from a known parent or sibling. */
	/* Since the trail to the parent has already been followed, */
	/* we can be sure that this cannot have any effect */
	/* on whether a child object is a root */
	return;
      } else {
	/* Found an "interesting" reference, so find or make an */
	/* obj_rec for the reference source, and record the references */
        obj_rec_p this_or = new_obj_rec(object);
        this_or->parent = refrec;
        return;
      }
    }
  }
}


/* traverse the heap looking for any objects which reference this generation */
void process_generation ()
{
  mps_arena_formatted_objects_walk(space, &look_for_reference, NULL, 0);
}



int display_roots_in_current_set ()
{
  int rootnum = 0;
  int i;
  for (i = 0; i < TABLE_SIZE; i++) {
    obj_rec_p current = current_table[i];
    while (current != NULL) {
      obj_rec_p refrec = object_in_roots(current->object);
      if (refrec != NULL) {
        rootnum++;
        if (trail_limit == rootnum) {
          display_trail_limit_addresses(rootnum); /* prompt to increase limit */
          report_break("Set the trail limit to show more if required\n");
        }
        if (trail_limit > rootnum) {
          display_trail_details(current, refrec);
        }
      }
      current = current->next;
    }
  }
  return rootnum;
}

void display_cold_trail()
{
  int size = size_of_current_set();
  int reported = 0;
  int i;
  if (size > 0) {
    /* don't display for null entry sets */
    display_cold_trail_intro(size);
    for (i = 0; i < TABLE_SIZE; i++) {
      obj_rec_p current = current_table[i];
      while (current != NULL) {
        if (cold_trail_limit == reported) {
          /* prompt to increase limit */
          display_cold_trail_limit_addresses(reported); 
          report_break("Set the cold trail limit to show more if required\n");
        }
        if (cold_trail_limit > reported) {
          display_cold_trail_details(current);
        }
        reported++;
        current = current->next;
      }
    }
  }
}

void advance_through_generations ()
{
  int gennum = 0;
  while (TRUE) {
    int roots;
    int gensize;
    gensize = size_of_table(child_table);
    if (gensize == 0) {
      /* failed to find a trail from a root */
      /* We must have etected a clique kept alive by GC conservatism */
      display_cold_trail();
      return;
    }
    display_generation_details(gennum, gensize, child_table);
    advance_generation();
    roots = display_roots_in_current_set();
    if (roots > 0) {
      /* check to see if we really want to continue */
      if (depth_limit <= gennum) {
	display_gen_limit_addresses(gennum); /* prompt to increase limit */
	report_break("Set the depth limit to trace further if required\n");
	if (depth_limit <= gennum) {
	  return;
	}
      }
    }
    process_generation(gennum); /* find children for this generation */
    gennum++;
  }
}
    
void record_a_root (mps_addr_t *objectref, mps_root_t root,
                    void *p1, size_t p2)
{
  void **object = (void **)(*objectref);
  void *wrapper = *object;
  if (wrapper && ((int)wrapper & 3) == 0) {
    obj_rec_p rootrec = object_in_roots(object);
    if (rootrec == NULL) {
      /* Have found a new root - so add to the set */
      new_root(object, objectref);
    }
  }
}


void find_all_roots()
{
  mps_arena_roots_walk(space, &record_a_root, 0, 0);
}

int follow_reference_paths ()
{
  while (object_to_follow || wrapper_to_follow) {
    clear_object_reference_paths();
    find_all_roots();
    if (wrapper_to_follow) {
      report_message("\nTracing paths to all objects with selected wrapper ...\n");
      add_target_objects_from_wrapper(wrapper_to_follow);
    } else if (object_to_follow) {
      report_message("\nTracing paths to selected object ...\n");
      add_target_object(object_to_follow);
    }
    advance_through_generations();

    object_to_follow = 0;
    wrapper_to_follow = 0;
    if (prompt_after_checking_references) {  
      display_reference_variables_addresses();
      report_break("Set a new object or wrapper to trace if required\n");
    } else {
      return 0;
    }
  }
  return 0;
}


void display_stats_for_memory_usage ()
{
  if (check_wrapper_stats) {
    clear_wrapper_stats();
    mps_arena_formatted_objects_walk(space, &record_an_object, 0, 0);
    display_wrapper_stats();
  }
  if (check_reference_paths) {
    if (object_to_follow || wrapper_to_follow) {
      follow_reference_paths();
    } else {
      display_reference_variables_addresses();
    }
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
