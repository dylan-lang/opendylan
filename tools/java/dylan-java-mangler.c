
#include <stdio.h>

typedef int boolean ;

#define true 1
#define false 0



char * dylan_current_module ;
char * dylan_current_library ;


#include "dylan.h"

/* #include "dylan.h" */


#define MangledLocalSuffix		'_'
#define LocalSuffix			'-'
/* #define MangledLibrarySeparator 	'V' */
/* #define MangledModuleSeparator	'Y' */
#define MangledLibrarySeparator 	'.'
#define MangledModuleSeparator	        '.'
#define NamespaceSeparator		':'
#define SlotSeparator			','
#define SlotSeparatorString		","
#define MangledSlotSeparator		'H'
#define MangledSlotSeparatorString	"H"


extern char * strchr (const char * str, int ch) ;

/* 
   GLOBAL-DYLAN-MANGLED-NAME-P 
   IS THIS MANGLED NAME A MODULE VARIABLE
   EXCLUDES VARIABLE NAMES ASSOCIATED WITH DYLAN SYMBOLS
*/

boolean global_dylan_name_info_with_separator
  (char * name,
   boolean * library_p,
   int * library_index,
   boolean * module_p, 
   int * module_index,
   char librarySeparator,
   char moduleSeparator) 
{
  char * mod_separator = strchr  (name, moduleSeparator);
  char * lib_separator = strrchr (name, librarySeparator);

  if (mod_separator) 
    {
      if (mod_separator != lib_separator) 
	{
	  *library_p = true;  *library_index = (int)(lib_separator - name);
	  *module_p = true;   *module_index = (int)(mod_separator - name);
	}
      else 
	{
	  *module_p = true;   *module_index = (int)(mod_separator - name);
	  *library_p = false; *library_index = strlen(name); /* USEFUL DEFAULT */
	}
    } 
  else
    {
      *library_p = false; *library_index = strlen(name); /* USEFUL DEFAULTS */
      *module_p = false;  *module_index  = strlen(name);
    }
  return (*library_p && *module_p);
}



boolean global_dylan_mangled_name_info 
  (char * name,
   boolean * library_p,
   int * library_index,
   boolean * module_p,
   int * module_index) 
{
  return (global_dylan_name_info_with_separator
           (name, library_p, library_index, module_p, module_index, 
	    MangledLibrarySeparator, MangledModuleSeparator));
}

boolean global_dylan_mangled_name_p (char *name) 
{
  boolean module_p, library_p;
  int module_index, library_index;
  return (global_dylan_mangled_name_info
           (name, &library_p, &library_index, &module_p, &module_index));
}

/* 
   GLOBAL-DYLAN-NAME-P 
   IS THIS MANGLED NAME A MODULE VARIABLE
   EXCLUDES VARIABLE NAMES ASSOCIATED WITH DYLAN SYMBOLS
*/

boolean global_dylan_name_info 
  (char * name,
   boolean * library_p,
   int * library_index,
   boolean * module_p,
   int * module_index) 
{
  return (global_dylan_name_info_with_separator
           (name, library_p, library_index, module_p, module_index, 
	    NamespaceSeparator, NamespaceSeparator));
}

boolean global_dylan_name_p (char * name) 
{
  boolean library_p; 
  int library_index;
  boolean module_p;
  int module_index;
  global_dylan_name_info (name, &library_p, &library_index, &module_p, &module_index);
  return (module_p);
}

/*
 * NAME DEMANGLING
 */

#include <ctype.h>




void split_string_at (char * dst1, char * dst2, char * src, int offset)
{
  strncpy (dst1, src, offset);  
  dst1[offset] = '\0';
  strcpy (dst2, &src[offset+1]);  
}



char * copy_string (char * str) 
{
  char * new_str = (char*) malloc(strlen(str)+1);
  strcpy (new_str, str);
  return (new_str);
}

void change_module_from_mangled_name
  (char * name, char ** old_module_name, char ** old_library_name) 
{
    /*
  char demangled[1000] ;
  char module_name[1000] ;
  char library_name[1000] ;
  char variable_name[1000] ;
  *old_library_name = dylan_current_library;
  *old_module_name  = dylan_current_module;
  demangle_dylan_name_exploded
    (name, library_name, module_name, variable_name, demangled, localp);
  dylan_current_library = copy_string (library_name);
  dylan_current_module = copy_string (module_name);
  */
}

/*
 * NAME MANGLING
 */

int mangle_dylan_char (char * name, int * src_i, char * mangled, int * dst_i)
{
  int len = strlen(name);
  int  si = *src_i;
  int  di = *dst_i;
  char  c = name[si++];

  if (c >= 'A' && c <= 'Z')  /* markt, I know all Dylan names case insensitive! */
    c = c-'A'+'a' ;

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
      { sprintf(&mangled[di], "Z%dZ", (int)c);
	di += 3 ;
	if (c > 9)
	  { di ++ ;
	    if (c > 99)
	      di ++ ;
	  }
      }
  }
  *src_i = si;
  *dst_i = di;
  return (c == '#' || c == ',');
}

char * mangle_dylan_string (char * name, int * src_i, int max_src_i, char * mangled, int * dst_i)
{
  for (; *src_i < max_src_i; )
    if (mangle_dylan_char (name, src_i, mangled, dst_i))
      return(name);
  return(name);
}

char * ___mangle_global_dylan_name (char * name, char * mangled)
{
  int src_i = 0 ;
  int dst_i = 0;
  int len = strlen(name);
  boolean library_p ;
  boolean module_p;
  int library_separator_index ;
  int module_separator_index ;
  
  global_dylan_name_info
    (name, &library_p, &library_separator_index, 
	   &module_p,  &module_separator_index);

  mangle_dylan_string
    (name, &src_i, module_separator_index, mangled, &dst_i);

  mangled [dst_i++] = MangledModuleSeparator; 

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


char * mangle_global_dylan_name (char * name, char * mangled)
{
  int src_i = 0 ;
  int dst_i = 0;
  int len = strlen(name);
  boolean library_p ; 
  boolean module_p ;
  int library_separator_index ;
  int module_separator_index ;
  
  global_dylan_name_info
    (name, &library_p, &library_separator_index, 
	   &module_p,  &module_separator_index);


  if (library_p) 
    {
      src_i = library_separator_index + 1 ;
      mangle_dylan_string
	(name, &src_i, len, mangled, &dst_i);
    }
  else
    {
      int tmp_i = 0;
      mangle_dylan_string
	(dylan_current_library, &tmp_i, strlen(dylan_current_library),
	 mangled, &dst_i);
    }

  mangled[dst_i++] = MangledLibrarySeparator; 

  if (module_p && module_separator_index != 0) 
    {
      src_i = module_separator_index + 1;
      mangle_dylan_string
	(name, &src_i, library_separator_index, mangled, &dst_i);
    } 
  else
    {
      int tmp_i = 0;
      mangle_dylan_string
	(dylan_current_module, &tmp_i, strlen(dylan_current_module),
	 mangled, &dst_i);
      if (module_p && module_separator_index == 0)
	src_i++; /* skip colon prefix */
    }

  mangled[dst_i++] = MangledModuleSeparator; 

  src_i = 0 ;
  mangle_dylan_string
    (name, &src_i, module_separator_index, mangled, &dst_i);

  mangled[dst_i] = '\0';
  return(mangled);
}

char * mangle_local_dylan_name (char * name, char * mangled)
{
  int src_i = 0 ;
  int dst_i = 0 ;

  mangle_dylan_string (name, &src_i, strlen(name), mangled, &dst_i);

  mangled[dst_i++] = MangledLocalSuffix; 
  mangled[dst_i]   = '\0';
  return(mangled);
}
  
char * mangle_dylan_name (char * name, char * mangled, boolean localp)
{
  int len = strlen(name);
  char* slot_separator = strchr(name, SlotSeparator);
  if (!strcmp(name, "#()"))
    { mangle_dylan_name("%empty-list:dylan:dylan", mangled, localp); }
  else if (!strcmp(name, "#f"))
    { mangle_dylan_name("%false:dylan:dylan", mangled, localp); }
  else if (!strcmp(name, "#t"))
    { mangle_dylan_name("%true:dylan:dylan", mangled, localp); }
  else if (!strcmp(name, "\"\""))
    { mangle_dylan_name("%bs-empty:dylan:dylan", mangled, localp); }
  else if (!strcmp(name, "#[]"))
    { mangle_dylan_name("%sv-empty:dylan:dylan", mangled, localp); }
  else if (!strncmp(name, "#\"", 2) && name[len - 1] == '\"') 
    {
      int src_i = 2; int dst_i = 1;
      mangled[0] = 'J';
      mangle_dylan_string(name, &src_i, len - 1, mangled, &dst_i);
    }
  else if (slot_separator) 
    {
      char getter_name[1000], class_name[1000];
      split_string_at
	(getter_name, class_name, name, (int)(slot_separator - name));
      mangle_global_dylan_name(getter_name, mangled);
      strcat(mangled, MangledSlotSeparatorString);
      mangle_global_dylan_name(class_name, &mangled[strlen(mangled)]);
    }
  else if (!localp) 
    {
      mangle_global_dylan_name(name, mangled);
    }
  else 
    { mangle_local_dylan_name(name, mangled); }
    
  return (mangled);
}


char * default_prefix ;
int    default_prefix_len ;

void set_default_prefix (char * string)
{
  default_prefix = string ;
  default_prefix_len = strlen (string) ;
}


char * my_malloc (int size)
{
  char * result = (char *) malloc (size) ;
  if (!result)
    { fprintf (stderr, "malloc failed\n") ;
      exit (100) ;
    }
  return (result) ;
}

unsigned long code_hash (char * string)
{
  unsigned long result = 0 ;
  int  n ;
  for (n = 0 ; n < strlen (string) ; n++)
    {
      result = ((result << 5) | (result >> 27)) ^ (string[n] & 0xff) ;
    }
  return (result & 0x3fffffff) ;
}


typedef struct cell
{
  char * string ;
  struct cell * next ;
} cell ;

#define java_keyword_size 113

cell * java_keywords [java_keyword_size] ;

void add_ent (char * string)
{
  int  len = strlen (string) ;
  int n ;
  char * copy = (char *) my_malloc (len+1) ;
  for (n = 0 ; n <= len ; n++)
    copy[n] = string[n] ;
  n = code_hash (copy) % java_keyword_size ;
  { cell * new_ent = (cell *) my_malloc (sizeof (cell)) ;
    new_ent -> string = copy ;
    new_ent -> next   = java_keywords [n] ;
    java_keywords [n] = new_ent ;
  }
}

void  add_ents ()
{
  int n ;
  for (n = 0 ; n < java_keyword_size ; n++)
    java_keywords [n] = (cell *) 0 ;
  add_ent ("abstract") ;
  add_ent ("boolean") ;
  add_ent ("break") ;
  add_ent ("byte") ;
  add_ent ("case") ;
  add_ent ("cast") ;
  add_ent ("catch") ;
  add_ent ("char") ;
  add_ent ("class") ;
  add_ent ("const") ;
  add_ent ("continue") ;
  add_ent ("default") ;
  add_ent ("do") ;
  add_ent ("double") ;
  add_ent ("else") ;
  add_ent ("extends") ;
  add_ent ("final") ;
  add_ent ("finally") ;
  add_ent ("float") ;
  add_ent ("for") ;
  add_ent ("future") ;
  add_ent ("generic") ;
  add_ent ("goto") ;
  add_ent ("if") ;
  add_ent ("implements") ;
  add_ent ("import") ;
  add_ent ("inner") ;
  add_ent ("instanceof") ;
  add_ent ("int") ;
  add_ent ("interface") ;
  add_ent ("long") ;
  add_ent ("native") ;
  add_ent ("new") ;
  add_ent ("null") ;
  add_ent ("operator") ;
  add_ent ("outer") ;
  add_ent ("package") ;
  add_ent ("private") ;
  add_ent ("protected") ;
  add_ent ("public") ;
  add_ent ("rest") ;
  add_ent ("return") ;
  add_ent ("short") ;
  add_ent ("static") ;
  add_ent ("super") ;
  add_ent ("switch") ;
  add_ent ("synchronized") ;
  add_ent ("this") ;
  add_ent ("throw") ;
  add_ent ("throws") ;
  add_ent ("transient") ;
  add_ent ("try") ;
  add_ent ("var") ;
  add_ent ("void") ;
  add_ent ("volatile") ;
  add_ent ("while") ;

  /*
  { int n;
    for (n = 0 ; n < java_keyword_size ; n++)
      { int len = 0 ;
	cell * ent = java_keywords [n] ;
	while (ent)
	  { len ++ ;
	    ent = ent->next ;
	  }
	fprintf (stderr, "len of %d :  %d\n", n, len) ;
      }
  }
  */
}


boolean java_keyword_p (char * string)
{ 
  int  hash_code  = code_hash (string) % java_keyword_size ;
  cell * ent = java_keywords [hash_code] ;
  while (ent)
    { if (!strcmp(ent->string,string)) return (true) ;
      ent = ent->next ;
    }
  return (false) ;
}

void check_for_keyword_clash (char * buffer)
{
  if (java_keyword_p (buffer))
    { int n ;
      int len = strlen (buffer) ;
	  for (n = len ; n >= 0 ; n--)
	    buffer[n+2] = buffer [n] ;
      buffer [0] = 'D' ;
      buffer [1] = '_' ;
    }
}

int  find_last_part (char * buffer)
{
  int i, n = strlen (buffer) ;
  for (i = n ; i > 0 ; i--)
    {
      if (buffer[i-1] == MangledLibrarySeparator)
	return (i) ;
    }
  return (0) ;
}

char * my_copy (char * name, char * buffer) 
{
  char c = *name++ ;
  { *buffer = c ;
    if (c == 0)
      return (buffer);
    buffer++;
    c = *name++ ;
  }
}
char * my_mangle (char * name, char * buffer) 
{
  mangle_dylan_name (name, buffer, 0) ;
  if (!strncmp (buffer, default_prefix, default_prefix_len))
    { buffer += default_prefix_len ; }
  check_for_keyword_clash (buffer + find_last_part (buffer)) ;
  return (buffer) ;
}

char * my_mangle_last_part (char * name, char * buffer) 
{
  mangle_dylan_name (name, buffer, 0) ;
  buffer += find_last_part (buffer) ;
  check_for_keyword_clash (buffer) ;
  return (buffer) ;
}


/* very simple filter to substitute the mangled name 
   for "foo" when the string "`foo'" appears in the input stream
 */
void  mangly (FILE * input, FILE * output)
{
  while (1)
    {
      int ch = getc (input) ;
      if (ch == EOF)
	return ;
      if (ch == '`')
	{
	  int  i = 0 ;
	  char buffer [1000] ;
	  char mangled [1000] ;
	  char * result ;
	  ch = getc (input) ;
	  while (ch != '\'')
	    {
	      if (ch == EOF)
		return ;
	      buffer [i++] = ch ;
	      ch = getc (input) ;
	    }
	  buffer [i] = 0 ;
	  result = my_mangle (&buffer[0], &mangled[0]) ;
	  for (i = 0 ; result [i] ;i++)
	    putc (result [i], output) ;
	}
/* enable mangling of string contents
      else if (ch == '"')
	{
	  putc (ch, output) ;
	  ch = getc (input) ;
	  while (ch != '"')
	    {
	      if (ch == EOF) return ;
	      putc (ch, output) ;
	      ch = getc (input) ;
	    }
	  putc (ch, output) ;
	}
*/      else
	{ putc (ch, output) ; }
    }
}

main (int argc, char ** argv, char ** envp)
{
  int n ;
  int status = 0 ;
  int mangle_filename ;
  char buffer [1000] ;
  dylan_current_library = "dylan" ;   /* set up the defaults */
  dylan_current_module  = "dylan" ;
  set_default_prefix ("dylan.dylan.") ;
  add_ents () ;

/*  for (n = 1 ; n < argc; n++)
    { 
      mangle_dylan_name (argv[n], &buffer[0], 0) ;
      printf ("%s  ->  %s\n", argv[n], &buffer[0]) ;
    } */


  for (n = 1 ; n < argc ; n++)
    {
      char input_name [300] ;
      char output_name [300] ;
      char buffer [1000] ;
      char * outname;
      int mi = 0, mo = 0 ;
      int i ;
      char c ;

/*      output_name [mo++] = '<' ; */
      for (i = 0 ; (c = argv[n][i]) ; i++)
	{ input_name [mi++] = c ;
	  output_name [mo++] = c ;
	}

      mangle_filename = 1 ;
      if ((mi > 2) &&
	  (input_name [mi-2] == '.'))
	{ if (input_name [mi-1] == 'J')
	    { mi -= 2 ; mo -= 2 ; mangle_filename = 1 ; }
	  if (input_name [mi-1] == 'j')
	    { mi -= 2 ; mo -= 2 ; mangle_filename = 0 ; }
	}

/*      output_name [mo++] = '>' ; */
      output_name [mo] = 0 ;

      if (mangle_filename)
	{ outname = my_mangle_last_part (&output_name[0], &buffer[0]) ; }
      else
	{ outname = my_copy (&output_name[0], &buffer[0]) ; }


      input_name [mi++]  = '.' ;
      input_name [mi++]  = mangle_filename ? 'J' : 'j' ;
      input_name [mi]    = 0 ;

      mo = strlen (outname) ;

      outname [mo++] = '.' ;
      outname [mo++] = 'j' ;
      outname [mo++] = 'a' ;
      outname [mo++] = 'v' ;
      outname [mo++] = 'a' ;
      outname [mo]   = 0 ;

      { FILE * input = fopen (&input_name[0], "r") ;
	FILE * output ;
	if (input)
	  {
	    output = fopen (outname, "w") ;
	    if (output)
	      {      
		fprintf (stderr, "mangling %s to %s:\n", &input_name[0], outname) ;
		mangly (input, output) ;
		fclose (input) ;
		fclose (output) ;
	      }
	    else
	      { status = 1 ;
		fprintf (stderr, "cannot open %s for output\n", outname) ;
		fclose (input) ;
	      }
	  }
	else
	  { status = 1 ;
	    fprintf (stderr, "cannot open %s for input\n", &input_name[0]) ;
	  }
      }

    }
  fprintf (stderr, "Done.\n") ;
  exit (status) ;
}


int error (char * string, int a1, int a2)
{
  fprintf (stderr, string, a1, a2) ;
  exit (11) ;
}
