#ifndef MANGLERDEFD
#define MANGLERDEFD
extern char * mangle_dylan_name (char *name, char *mangled, boolean localp);
extern char * demangle_dylan_name (char *name, char *demangled, boolean localp);
extern boolean global_dylan_name_p (char *name);
extern boolean dylan_primitive_p (char *filename);

extern boolean string_equal (char *s1, char *s2);

extern char *
demangle_dylan_name_exploded 
  (char *name, 
   char *library_name, char *module_name, char *variable_name, char *demangled,
   boolean localp);
   
extern boolean global_dylan_mangled_name_p (char *name);
extern char* demangle_dylan_dylan_name(char* name, char* demangled_name);
extern char*
demangle_dylan_name_in_library_module
    (char* name, char* demangled_name, char* library_name, char* module_name);

   
#endif
