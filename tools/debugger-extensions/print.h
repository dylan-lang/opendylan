#ifndef PRINTDEFD
#define PRINTDEFD

extern void change_module_from_mangled_name(char* name, char** library_name, char** module_name);


#define MAX_VALUES 10000

extern CORE_ADDR values[MAX_VALUES];
extern int values_index;
extern int max_values_index;

extern int add_value (CORE_ADDR value);

extern int dylan_print_depth;
extern int dylan_print_length;
extern char *dylan_current_library;
extern char *dylan_current_module;

#define INFINITE_PRINT_LENGTH 1000000
#define INFINITE_PRINT_DEPTH  1000000
#define DEFAULT_PRINT_LENGTH 10
#define DEFAULT_PRINT_DEPTH  3

void print_dylan_object (FILE* stream, CORE_ADDR val, boolean verbose_p, int print_depth);
void describe_dylan_object (FILE* stream, CORE_ADDR val, boolean verbose_p, int print_depth);

extern void
describe_dylan_object
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth);

extern void
print_dylan_object 
  (FILE* stream, CORE_ADDR instance, boolean verbose_p, int print_depth);
  
extern int dylan_backtrace;

#endif
