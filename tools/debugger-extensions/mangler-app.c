#include <stdio.h>
#include <ctype.h>
#include <string.h>
	
typedef int boolean;
#define error exit
#define false 0
#define true 1
	
char *dylan_current_library = "dylan";
char *dylan_current_module  = "dylan";

/* #include "dylan.h" */

#define MangledLibrarySeparator 	'V'
#define MangledModuleSeparator		'Y'
#define NamespaceSeparator		':'
#define SlotSeparator			','
#define SlotSeparatorString		","
#define MangledSlotSeparator		'H'
#define MangledSlotSeparatorString	"H"

/* 
   GLOBAL-DYLAN-MANGLED-NAME-P 
   IS THIS MANGLED NAME A MODULE VARIABLE
   EXCLUDES VARIABLE NAMES ASSOCIATED WITH DYLAN SYMBOLS
*/

boolean
global_dylan_name_info_with_separator
    (char *name, boolean *library_p, int *library_index,
                 boolean *module_p, int *module_index,
		 char librarySeparator, char moduleSeparator) 
{
  char* mod_separator = strchr(name, moduleSeparator);
  char* lib_separator = strrchr(name, librarySeparator);

  if (mod_separator) {
    if (mod_separator != lib_separator) {
      *library_p = true;  *library_index = (int)(lib_separator - name);
      *module_p = true;   *module_index = (int)(mod_separator - name);
    } else {
      *module_p = true;   *module_index = (int)(mod_separator - name);
      *library_p = false; *library_index = *module_index;
    }
  } else {
    *library_p = false; *library_index = strlen(name); /* USEFUL DEFAULTS */
    *module_p = false;  *module_index  = strlen(name);
  }
  return(*library_p && *module_p);
}

boolean
global_dylan_mangled_name_info 
    (char *name, boolean *library_p, int *library_index,
                 boolean *module_p, int *module_index) 
{
  return(global_dylan_name_info_with_separator
           (name, library_p, library_index, module_p, module_index, 
	    MangledLibrarySeparator, MangledModuleSeparator));
}

boolean
global_dylan_mangled_name_p (char *name) {
  boolean module_p, library_p; int module_index, library_index;
  return(global_dylan_mangled_name_info
           (name, &library_p, &library_index, &module_p, &module_index));
}

/* 
   GLOBAL-DYLAN-NAME-P 
   IS THIS MANGLED NAME A MODULE VARIABLE
   EXCLUDES VARIABLE NAMES ASSOCIATED WITH DYLAN SYMBOLS
*/

boolean
global_dylan_name_info 
    (char *name, boolean *library_p, int *library_index,
                 boolean *module_p, int *module_index) 
{
  return(global_dylan_name_info_with_separator
           (name, library_p, library_index, module_p, module_index, 
	    NamespaceSeparator, NamespaceSeparator));
}

boolean
global_dylan_name_p (char *name) {
  boolean library_p; int library_index;
  boolean module_p;  int module_index;
  global_dylan_name_info(name, &library_p, &library_index, &module_p, &module_index);
  return(module_p);
}

/*
 * NAME DEMANGLING
 */

#include <ctype.h>

void
demangle_dylan_char (char *name, int *src_i, char *demangled, int *dst_i)
{
  int si = *src_i; 
  int di = *dst_i; 
  int len = strlen(name);
  char c = name[si];

  if (c == '_') {
    si++;
    demangled[di++] = '-';
  } else if (c == 'M') {
    demangled[di++] = '#';
    si++;
    while (si < len && (name[si] >= '0') && (name[si] <= '9')) 
      demangled[di++] = name[si++];
  } else if (c == 'I') {
    demangled[di++] = name[si++];
  } else if (c == 'W') {
    demangled[di++] = name[si++];
  } else if (c == 'Z') {
      int code, n;
      if (sscanf(&name[si], "Z%dZ", &code) != 1)
        error("Malformed demangled symbol %s @ %d\n", name, si);
      si = (si + 1) + (strchr(&name[si+1], 'Z') - &name[si+1]) + 1;
      demangled[di++] = (char)code;
  } else if (isupper(c)) {
      si++;
      
           if (c == 'X') demangled[di++] = '!'; /* exclamationPoint */
      else if (c == 'D') demangled[di++] = '$'; /* dollarSign */
      else if (c == 'P') demangled[di++] = '%'; /* percentSign */
      else if (c == 'T') demangled[di++] = '*'; /* times */
      else if (c == 'S') demangled[di++] = '/'; /* slash */
      else if (c == 'L') demangled[di++] = '<'; /* lessThan */
      else if (c == 'G') demangled[di++] = '>'; /* greaterThan */
      else if (c == 'Q') demangled[di++] = '?'; /* questionMark */
      else if (c == 'A') demangled[di++] = '+'; /* plus */
      else if (c == 'B') demangled[di++] = '&'; /* ampersand */
      else if (c == 'C') demangled[di++] = '^'; /* carot */
      else if (c == 'U') demangled[di++] = '_'; /* underscore */
      else if (c == 'O') demangled[di++] = '@'; /* atSign */
      else if (c == 'E') demangled[di++] = '='; /* equals */
      else if (c == 'N') demangled[di++] = '~'; /* squiggle */
      else
	error ("Unknown dylan demangle symbol: %s @ %d", name, si);
  } else if (islower(c) || isdigit(c)) {
    demangled[di++] = name[si++];
  }
  *src_i = si;
  *dst_i = di;
}

void demangle_dylan_string_into
    (char *src, int *src_i, char *dst, int *dst_i, int len, char stop) {
  for (; *src_i < len && (src[*src_i] != stop);)
    demangle_dylan_char(src, src_i, dst, dst_i);
}

int demangle_dylan_string
    (char *src, int *src_i, char *dst, int len, char stop) {
  int tmp_i = 0;
  demangle_dylan_string_into(src, src_i, dst, &tmp_i, len, stop);
  dst[tmp_i] = '\0';
}

void demangle_dylan_string_into_both
    (char *src, int *src_i, 
     char *dst, int *dst_i, char *target, int len, char stop) {
  demangle_dylan_string(src, src_i, target, len, stop);
  strcpy(&dst[*dst_i], target);
  *dst_i += strlen(target);
}

char *
demangle_dylan_name_exploded 
  (char *name, 
   char *library_name, char *module_name, char *variable_name, char *demangled,
   boolean localp)
{
  if (localp || global_dylan_mangled_name_p(name)) {
    int src_i, tmp_i, i;
    int dst_i = 0;
    int len = strlen(name);

    src_i = 0;

    /* HANDLE CONSTANTS with K prefix */

    demangle_dylan_string_into_both
      (name, &src_i, demangled, &dst_i, variable_name, len, MangledModuleSeparator); 

    if (!localp) {
      src_i++; demangled[dst_i++] = NamespaceSeparator;
      demangle_dylan_string_into_both
        (name, &src_i, demangled, &dst_i, module_name, len, MangledLibrarySeparator); 
      if (!strcmp(module_name, dylan_current_module))
        dst_i -= strlen(module_name) + 1;
	
      src_i++; demangled[dst_i++] = NamespaceSeparator;
      demangle_dylan_string_into_both
        (name, &src_i, demangled, &dst_i, library_name, len, NULL); 
      if (!strcmp(library_name, dylan_current_library))
        dst_i -= strlen(library_name) + 1;

      demangled[dst_i] = '\0';
    }
    
    /* translate known variables */
    
         if (!strcmp(variable_name, "%empty-list"))
      strcpy(demangled, "#()");
    else if (!strcmp(variable_name, "%false"))
      strcpy(demangled, "#f");
    else if (!strcmp(variable_name, "%true"))
      strcpy(demangled, "#t");
    else if (!strcmp(variable_name, "%bs-empty"))
      strcpy(demangled, "\"\"");
    else if (!strcmp(variable_name, "%sv-empty"))
      strcpy(demangled, "#[]");
  } else
    strcpy(demangled, name);
    
  return(demangled);
}

void
split_string_at (char *dst1, char *dst2, char *src, int offset)
{
  strncpy(dst1, src, offset);  
  dst1[offset] = '\0';
  strcpy(dst2, &src[offset + 1]);  
}

char *
demangle_dylan_name (char *name, char *demangled, boolean localp)
{
  char* slot_separator = strchr(name, MangledSlotSeparator);
  if (slot_separator) {
    char library_name[1000], module_name[1000], variable_name[1000];
    char getter_name[1000], class_name[1000];
    split_string_at
      (getter_name, class_name, name, (int)(slot_separator - name));
    demangle_dylan_name_exploded
      (getter_name, library_name, module_name, variable_name, demangled, 0);
    strcat(demangled, SlotSeparatorString);
    demangle_dylan_name_exploded
      (class_name, library_name, module_name, variable_name, 
       &demangled[strlen(demangled)], 0);
  } else if (name[0] == 'J') {
    char variable_name[1000];
    int src_i = 1;
    strcpy(variable_name, "#\"");
    demangle_dylan_string
      (&name[1], &src_i, &variable_name[2], strlen(name), NULL);
    strcat(variable_name, "\"");
  } else if (localp || global_dylan_mangled_name_p(name)) {
    char module_name[1000], library_name[1000], variable_name[1000];
    
    demangle_dylan_name_exploded
      (name, library_name, module_name, variable_name, demangled, localp);
  } else
    strcpy(demangled, name);
    
  return(demangled);
}

char*
demangle_dylan_name_in_library_module
    (char* name, char* demangled_name, char* library_name, char* module_name) {
  char *saved_module = dylan_current_module; char *saved_library = dylan_current_library;
  dylan_current_library = library_name; dylan_current_module = module_name;
  
  demangle_dylan_name(name, demangled_name, false);
    
  dylan_current_module = saved_module; dylan_current_library = saved_library;
  return(demangled_name);
}
	 
char*
demangle_dylan_dylan_name(char* name, char* demangled_name) {
  demangle_dylan_name_in_library_module(name, demangled_name, "dylan", "dylan");
}
	 
char*
copy_string(char* str) {
  char* new_str = (char*)malloc(strlen(str)+1);
  strcpy(new_str, str);
  return(new_str);
}

void
change_module_from_mangled_name
    (char* name, char** old_module_name, char** old_library_name) {
    /*
  char demangled[1000], module_name[1000], library_name[1000], variable_name[1000];
  *old_library_name = dylan_current_library;
  *old_module_name = dylan_current_module;
  demangle_dylan_name_exploded
    (name, library_name, module_name, variable_name, demangled, localp);
  dylan_current_library = copy_string(library_name);
  dylan_current_module = copy_string(module_name);
  */
}

/*
 * NAME MANGLING
 */

int
mangle_dylan_char (char *name, int *src_i, char* mangled, int *dst_i)
{
  int len = strlen(name);
  int  si = *src_i;
  int  di = *dst_i;
  char  c = name[si++];

  switch (c) {
  case '-': mangled[di++] = '_'; break;
  case '!': mangled[di++] = 'X'; break;
  case '$': mangled[di++] = 'D'; break;
  case '%': mangled[di++] = 'P'; break;
  case '*': mangled[di++] = 'T'; break;
  case '/': mangled[di++] = 'S'; break;
  case '<': mangled[di++] = 'L'; break;
  case '>': mangled[di++] = 'G'; break;
  case '?': mangled[di++] = 'Q'; break;
  case '+': mangled[di++] = 'A'; break;
  case '&': mangled[di++] = 'B'; break;
  case '^': mangled[di++] = 'C'; break;
  case '_': mangled[di++] = 'U'; break;
  case '@': mangled[di++] = 'O'; break;
  case '=': mangled[di++] = 'E'; break;
  case '~': mangled[di++] = 'N'; break;
  case 'I': mangled[di++] = 'I'; break;
  case 'W': mangled[di++] = 'W'; break;
  case '#': mangled[di++] = 'M'; 
    while (di < len && isdigit(name[si])) 
      mangled[di++] = name[si++];
    break;            
  default:  
    if (islower(c) || isdigit(c))
      mangled[di++] = c;
    else
      di += sprintf(&mangled[di], "Z%dZ", (int)c);
  }
  *src_i = si;
  *dst_i = di;
  return(c == '#' || c == ',');
}

char *
mangle_dylan_string 
  (char *name, int *src_i, int max_src_i, char *mangled, int *dst_i)
{
  for (; *src_i < max_src_i; )
    if (mangle_dylan_char (name, src_i, mangled, dst_i))
      return(name);
  return(name);
}

char *
mangle_global_dylan_name (char *name, char *mangled)
{
  int src_i = 0, dst_i = 0;
  int len = strlen(name);
  boolean library_p, module_p;
  int library_separator_index, module_separator_index;

  global_dylan_name_info
    (name, &library_p, &library_separator_index, 
	   &module_p,  &module_separator_index);

  mangle_dylan_string
    (name, &src_i, module_separator_index, mangled, &dst_i);

  mangled[dst_i++] = MangledModuleSeparator; 

  if (module_p && module_separator_index != 0) {
    src_i++;
    mangle_dylan_string
      (name, &src_i, library_separator_index, mangled, &dst_i);
  } else {
    int tmp_i = 0;
    mangle_dylan_string
      (dylan_current_module, &tmp_i, strlen(dylan_current_module),
       mangled, &dst_i);
    if (module_p && module_separator_index == 0)
      src_i++; /* skip colon prefix */
  }

  mangled[dst_i++] = MangledLibrarySeparator; 

  if (library_p) {
    src_i++;
    mangle_dylan_string
      (name, &src_i, len, mangled, &dst_i);
  } else {
    int tmp_i = 0;
    mangle_dylan_string
      (dylan_current_library, &tmp_i, strlen(dylan_current_library),
       mangled, &dst_i);
  }

  mangled[dst_i] = '\0';
  return(mangled);
}

char *
mangle_dylan_name (char *name, char *mangled, boolean localp)
{
  int len = strlen(name);
  char* slot_separator = strchr(name, SlotSeparator);
       if (!strcmp(name, "#()"))
    mangle_dylan_name("%empty-list:dylan:dylan", mangled, localp);
  else if (!strcmp(name, "#f"))
    mangle_dylan_name("%false:dylan:dylan", mangled, localp);
  else if (!strcmp(name, "#t"))
    mangle_dylan_name("%true:dylan:dylan", mangled, localp);
  else if (!strcmp(name, "\"\""))
    mangle_dylan_name("%bs-empty:dylan:dylan", mangled, localp);
  else if (!strcmp(name, "#[]"))
    mangle_dylan_name("%sv-empty:dylan:dylan", mangled, localp);
  else if (!strncmp(name, "#\"", 2) && name[len - 1] == '\"') {
    int src_i = 2; int dst_i = 1;
    mangled[0] = 'J';
    mangle_dylan_string(name, &src_i, len - 1, mangled, &dst_i);
  } else if (slot_separator) {
    char getter_name[1000], class_name[1000];
    split_string_at
      (getter_name, class_name, name, (int)(slot_separator - name));
    mangle_global_dylan_name(getter_name, mangled);
    strcat(mangled, MangledSlotSeparatorString);
    mangle_global_dylan_name(class_name, &mangled[strlen(mangled)]);
  } else if (!localp) {
    mangle_global_dylan_name(name, mangled);
  } else
    strcpy(mangled, name);
    
  return(mangled);
}

main ()
{
  char s[100], d[100];

  printf("MANGLE? ");
  gets(s);
  if (!strcmp(s, "y")) {
    for (;;) {
      printf("M? ");
      gets(s);
      printf("%s => ", mangle_dylan_name(s, d, 0));
      printf("%s => ", demangle_dylan_name(d, s, 0));
      printf("%s\n", mangle_dylan_name(s, d, 0));
    }
  } else {
    for (;;) {
      printf("D? ");
      gets(s);
      printf("%s => ", demangle_dylan_name(s, d, 0));
      printf("%s => ", mangle_dylan_name(d, s, 0));
      printf("%s\n", demangle_dylan_name(s, d, 0));
    }
  }
}		

