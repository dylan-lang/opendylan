#include "heap-utils.h"

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



/* Controlling variables */

/* if true then prompt for another  object/wrapper on each forced GC */
static int prompt_before_checking_references = 0;  

/* if true then prompt for another object/wrapper after following a trail */
static int prompt_after_checking_references = 1; 

/* set this from the debugger to permit roots to be found at depths
   greater than the depth of the first found roots .
*/
static int depth_limit = 0;

/* set this from the debugger to limit the number of trails which can
   be reported at any depth depth.
*/
static int trail_limit = 4;

/* set this from the debugger to limit the number of trails which can
   be reported when a trail goes cold.
*/
static int cold_trail_limit = 1;

/* set this from the debugger to limit the number of objects which get
   reported during processing for each depth.
*/
static int object_report_limit = 4;

/* set this from a debugger if you want to trace to an object */
static void *object_to_follow = 0;

/* set this from a debugger for tracing to the first instance found
   with this wrapper.
*/
static void *wrapper_to_follow = 0;



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


static int hash_val(mps_addr_t obj)
{
  return ((int)obj >> TABLE_SHIFT) & TABLE_MASK;
}


static obj_rec_p root_table[TABLE_SIZE] = {NULL};      /* hash table of known roots */
static obj_rec_p processed_table[TABLE_SIZE] = {NULL}; /* hash table of processed objects */
static obj_rec_p current_table[TABLE_SIZE] = {NULL};   /* hash table of current set of objects */
static obj_rec_p child_table[TABLE_SIZE] = {NULL};     /* hash table of child objects */


static obj_rec_p alloc_obj_rec(mps_addr_t obj)
{
  obj_rec_p new = alloc_obj(sizeof(obj_rec_st));
  new->object = obj;
  new->next = NULL;
  new->parent = NULL;
  return new;
}

static void free_obj_rec(obj_rec_p or)
{
  free_obj(or, sizeof(obj_rec_st));
}

static void free_table(obj_table table)
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


static void add_to_table(obj_table table, obj_rec_p objrec)
{
  int hash = hash_val(objrec->object);
  objrec->next = table[hash];
  table[hash] = objrec;
}


static obj_rec_p find_obj_in_table(obj_table table, mps_addr_t obj)
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
static void merge_tables(obj_table dest, obj_table src)
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

static int size_of_table(obj_table table)
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


static obj_rec_p add_new_to_table(obj_table table, mps_addr_t obj)
{
  obj_rec_p new = alloc_obj_rec(obj);
  add_to_table(table, new);
  return new;
}

static obj_rec_p new_obj_rec(mps_addr_t obj)
{
  return add_new_to_table(child_table, obj);
}

static obj_rec_p new_root(mps_addr_t obj, mps_addr_t root)
{
  obj_rec_p new = add_new_to_table(root_table, obj);
  obj_rec_p parent = alloc_obj_rec(root);
  new->parent = parent;
  return new;
}


static void clear_object_reference_paths (void)
{
  free_table(root_table);
  free_table(processed_table);
  free_table(current_table);
  free_table(child_table);
}



static void display_object_reference(void* object, mps_lib_FILE *stream)
{
  void *wrapper = ((void**)object)[0];
  char *class_name = class_name_from_wrapper(wrapper);
  mps_lib_fputs("    ", stream);
  mps_lib_fputs(class_name, stream);
  display_padding_for_string(class_name, ' ', class_name_size, stream);
  display_hex_address(object, stream);
  mps_lib_fputc('\n', stream);
}

static void display_reference_paths (obj_rec_p or, mps_lib_FILE *stream)
{
  if (or != 0) {
    display_reference_paths(or->parent, stream);
    display_object_reference(or->object, stream);
  }
}

static void display_cold_trail_intro (int gensize)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nFailed to find any objects referenced from a root.\n", stream);  
  mps_lib_fputs("Hence all detected objects are 'floating garbage'.\n", stream);  
  mps_lib_fputs("There were ", stream);  
  display_integer(gensize, stream);
  mps_lib_fputs(" objects encountered while processing the last depth.\n", stream);  
  mps_lib_fputs("Here are some cold trails:\n", stream);  
}

static void display_cold_trail_details (obj_rec_p obj)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nStart of a cold trail of object references\n", stream);  
  display_reference_paths(obj, stream);
  mps_lib_fputs("End of cold trail\n", stream);
}

static void display_trail_details (obj_rec_p obj, obj_rec_p root)
{
  mps_lib_FILE *stream = mps_lib_get_stdout();
  mps_lib_fputs("\nStart of a trail of object references\n", stream);  
  display_reference_paths(obj, stream);
  mps_lib_fputs("Referenced from a root at address                     ", stream);  
  display_hex_address(root->parent->object, stream);
  mps_lib_fputc('\n', stream);

  mps_lib_fputs("End of trail\n", stream);
}

static void display_generation_details (int gennum, int gensize, obj_table table)
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

static void display_reference_variables_addresses (void)
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

static void display_gen_limit_addresses (int gennum)
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

static void display_trail_limit_addresses (int rootnum)
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

static void display_cold_trail_limit_addresses (int reported)
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

static void add_target_object (mps_addr_t obj)
{
  new_obj_rec(obj);
}

static void add_target_object_of_wrapper (mps_addr_t object, 
                                          mps_fmt_t format, mps_pool_t pool, 
                                          void *wrapper, int p2)
{
  void **found = (void**)object;
  void *found_wrapper = *found;
  if (found_wrapper == wrapper) {
    add_target_object(object);
  }
}


static void add_target_objects_from_wrapper (void *wrapper)
{
  mps_arena_formatted_objects_walk(space, &add_target_object_of_wrapper, wrapper, 0);
}



static obj_rec_p object_in_roots(mps_addr_t object)
{
  return find_obj_in_table(root_table, object);
}

static obj_rec_p object_in_processed_set(mps_addr_t object)
{
  return find_obj_in_table(processed_table, object);
}

static obj_rec_p object_in_current_set(mps_addr_t object)
{
  return find_obj_in_table(current_table, object);
}


static int size_of_current_set()
{
  return size_of_table(current_table);
}

static void advance_generation (void)
{
  /* merge the current into the processed set */
  /* then merge the children to make them thecurrent set */
  merge_tables(processed_table, current_table);
  merge_tables(current_table, child_table);
}



static BOOL trace_reference(mps_addr_t object, mps_addr_t parent,
                            int parent_size, void * env)
{
  obj_rec_p refrec = object_in_current_set(object);
  if (refrec != NULL) {
    /* Have found a ref to the current gen */
    if (object_in_processed_set(parent)
        || object_in_current_set(parent)) {
      /* Found a reference from a known parent or sibling. */
      /* Since the trail to the parent has already been followed, */
      /* we can be sure that this cannot have any effect */
      /* on whether a child object is a root */
      return FALSE;
    } else {
      /* Found an "interesting" reference, so find or make an */
      /* obj_rec for the reference source, and record the references */
      obj_rec_p this_or = new_obj_rec(parent);
      this_or->parent = refrec;
      return FALSE;
    }
  }
  return TRUE;
}


static void look_for_reference(mps_addr_t object, mps_fmt_t format, 
                               mps_pool_t pool, 
                               void *p1, int p2)
{
  trace_object(object, trace_reference, NULL);
}


/* traverse the heap looking for any objects which reference this generation */
static void process_generation (void)
{
  mps_arena_formatted_objects_walk(space, &look_for_reference, NULL, 0);
}



static int display_roots_in_current_set (void)
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

static void display_cold_trail()
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

static void advance_through_generations (void)
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
    process_generation(); /* find children for this generation */
    gennum++;
  }
}
    
static void record_a_root (mps_addr_t *objectref, mps_root_t root,
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


/* Avoid inlining function calls from now on. 
 * It might confuse the roots walk, by putting extra stuff on the stack.
 */
#pragma inline_depth(0)

/* The following function "cleans" below the stack pointer,
 * avoiding any palimpsest problem, whereby spurious "old" 
 * values turn up
 */

static int recurse_to_clean_the_stack(int count, int dummy1, int dummy2, int dummy3)
{
  if (count > 0) {
    int res = recurse_to_clean_the_stack(count-1, dummy1, dummy2, dummy3);
    return res + dummy1;
  } else {
    return count;
  }
}

static void find_all_roots()
{
  recurse_to_clean_the_stack(200, 0, 0, 0);
  mps_arena_roots_walk(space, &record_a_root, 0, 0);
}

static void find_trails()
{
  if (wrapper_to_follow) {
    report_message("\nTracing paths to all objects with selected wrapper ...\n");
    add_target_objects_from_wrapper(wrapper_to_follow);
  } else if (object_to_follow) {
    report_message("\nTracing paths to selected object ...\n");
    add_target_object(object_to_follow);
  }
  advance_through_generations();
}

static int follow_reference_paths (void)
{
  while (object_to_follow || wrapper_to_follow) {
    find_all_roots();
    find_trails();

    object_to_follow = 0;
    wrapper_to_follow = 0;
    if (prompt_after_checking_references) {  
      display_reference_variables_addresses();
      clear_object_reference_paths();
      report_break("Set a new object or wrapper to trace if required\n");
    } else {
      clear_object_reference_paths();
      return 0;
    }
  }
  return 0;
}


void display_reference_trails (void)
{
  if (object_to_follow || wrapper_to_follow) {
    follow_reference_paths();
  } else {
    display_reference_variables_addresses();
  }
}
