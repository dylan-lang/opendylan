#include <stdio.h>


typedef unsigned short unicode_char ;

typedef struct CON
{
  int type ;
} CON ;

typedef struct ucon
{
  int type ;
  unsigned long len ;
  unicode_char * unicode ;
} ucon ;

typedef struct icon
{
  int type ;
  long number ;
} icon ;

typedef struct fcon
{
  int type ;
  float number ;
} fcon ;

typedef struct lcon
{
  int type ;
  unsigned long lsw ;
  long          msw ;
} lcon ;

typedef struct dcon
{
  int type ;
  double number ;
} dcon ;

typedef struct ccon
{
  int type ;
  int name ;
} ccon ;

typedef struct scon 
{
  int type ;
  int val ;
} scon ;

typedef struct slotcon
{
  int type ;
  int class ;
  int nat ;
} slotcon ;

typedef struct natcon
{
  int type ;
  int name ;
  int sig ;
} natcon ;

#define False 0
#define  True 1
typedef int Boolean ;


static FILE * output = NULL;

static int errors = 0 ;

static FILE * input = NULL ;
static unsigned long input_pos = 0 ;
static unsigned long input_length = 0xffffffffL ;
static char * input_name = 0 ;

static void err (char * string)
{
  errors++ ;
  fprintf (stderr, "+ Error at char 0x%x in %s: %s\n", input_pos, input_name, string) ;
  if (output != NULL) fflush (output) ;
  exit (4) ;
}

static void err1 (char * string, int arg)
{
  errors++ ;
  fprintf (stderr, "+ Error: ") ;
  fprintf (stderr, string, arg) ;
  fprintf (stderr, "\n") ;
  if (output != NULL) fflush (output) ;
  exit (4) ;
}

static void err2 (char * string, int arg, int arg2)
{
  errors++ ;
  fprintf (stderr, "+ Error: ") ;
  fprintf (stderr, string, arg, arg2) ;
  fprintf (stderr, "\n") ;
  if (output != NULL) fflush (output) ;
  exit (4) ;
}

extern void * malloc (int) ;

static char * grab (int size)
{
  char * result = (char *) malloc (size);
  if (!result)
    err ("ran out of memory") ;
  return (result) ;
}


static unsigned int get1u ()
{
  int  res = getc (input) ;
  if (res == EOF || input_pos >= input_length)
    err2 ("unexpected EOF at char 0x%x in %s", input_pos, (int)(input_name ? input_name : "<stdin>")) ;
  input_pos ++ ;
  return ((unsigned int) res) ;
}

static unsigned int get2u ()
{
  int  hi = get1u () ;
  int  lo = get1u () ;
  return ((hi << 8) | lo) ;
}
static unsigned int get2u_le ()
{
  int  lo = get1u () ;
  int  hi = get1u () ;
  return ((hi << 8) | lo) ;
}

static unsigned long get4u ()
{
  unsigned long hi = get2u () ;
  unsigned long lo = get2u () ;
  return ((hi << 16) | lo) ;
}
static unsigned long get4u_le ()
{
  unsigned long lo = get2u_le () ;
  unsigned long hi = get2u_le () ;
  return ((hi << 16) | lo) ;
}

static long get4 ()
{
  long hi = (long) get2u () ;
  long lo = (long) get2u () ;
  return ((hi << 16) | lo) ;
}



static unsigned long parse_utf8 (unsigned long len, unicode_char * buf)
{
  unsigned long f = 0 ;
  unsigned long t = 0 ;
  while (f < len)
    {
      int  val = get1u () ;
      f++ ;
      if (val == EOF)
	err ("unexpected EOF during UTF8 string") ;
      if (val < 0x80)
	{
	  buf[t++] = val ;
	}
      else if ((val & 0xe0) == 0xc0)
	{
	  int next = get1u () ; f++ ;
	  if (next == EOF) err ("unexpected EOF during UTF8 string") ;
	  if ((next & 0xc0) != 0x80) err ("bad utf8") ;
	  val = ((val & 0x1f) << 6) | (next & 0x3f) ;
	  buf[t++] = val ;
	}
      else if ((val & 0xf0) == 0xe0)
	{
	  int  next1 = get1u () ;
	  int  next2 = get1u () ;
	  f += 2 ;
	  if (next1 == EOF) err ("unexpected EOF during UTF8 string") ;
	  if ((next1 & 0xc0) != 0x80) err ("bad utf8") ;
	  if (next2 == EOF) err ("unexpected EOF during UTF8 string") ;
	  if ((next2 & 0xc0) != 0x80) err ("bad utf8") ;
	  val = ((val & 0xf) << 12) | ((next1 & 0x3f) << 6) | (next2 & 0x3f) ;
	  buf[t++] = val ;
	}
      else 
	err ("Bad utf8");
      if (t > len)
        err ("parse_utf8 overran") ;
    }
  if (f != len) err ("bad utf8 ending") ;
  return (t) ;
}

static int show_constants_pool = 0 ;

static CON * parse_constant (int * size)
{
  int ch = get1u () ;
  int len ;
  long  i ;
  float f ;
  double d ;
  unsigned long lsw ;
  double * dp = &d;
  *size = 1 ;
  switch (ch)
    {
    case EOF:
      err ("premature end of file in constants pool") ; break ;
    case 1:
      len = get2u () ;
      { unicode_char * buffer = (unicode_char *) grab (len * sizeof (unicode_char)) ;
	ucon *  str = (ucon *) grab (sizeof (ucon)) ;
	str->type    = 'U' ;
	str->len = parse_utf8 (len, buffer) ;
	str->unicode = buffer ;
	return ((CON *) str) ;
      }
    case 2:
      len = get2u () ;
      { unicode_char * buffer = (unicode_char *) grab (len * sizeof (unicode_char)) ;
	ucon *  str = (ucon *) grab (sizeof (ucon)) ;
	str->type    = 'U' ;
	str->len     = len ;
	str->unicode = buffer ;
	for (i = 0 ; i < len ; i++)
	  buffer [i]  =  get2u () ;
	return ((CON *) str) ;
      }
    case 3:
      i = get4 () ;
      { icon * inty = (icon *) grab (sizeof (icon)) ;
	inty->type   =  'I' ;
	inty->number  =  i ;
	return ((CON *) inty) ;
      }
    case 4:
      i = get4 () ;
      * ((long *) dp) = i ;
      f = * ((float *) dp) ;
      { fcon * flt = (fcon *) grab (sizeof (fcon)) ;
	flt->type   =  'F' ;
	flt->number  =  f ;
	return ((CON *) flt) ;
      }
    case 5:
      *size = 2 ;
      i = get4 () ;
      lsw = get4u () ;
      { lcon * lng = (lcon *) grab (sizeof (lcon)) ;
	lng->type   =  'L' ;
	lng->lsw  =  lsw ;
	lng->msw  =  i ;
	return ((CON *) lng) ;
      }
    case 6:
      * size = 2 ;
      i = get4 () ;
      lsw = get4u () ;
      { long * tp = (long *) dp ;
	tp[0] = i ;
	tp[1] = (long) lsw ;
      }
      d = *dp ;
      { dcon * db = (dcon *) grab (sizeof (dcon)) ;
	db->type   =  'D' ;
	db->number  =  d ;
	return ((CON *) db) ;
      }
    case 7:
      { ccon * cls = (ccon *) grab (sizeof (ccon)) ;
	cls->type   =  'c' ;
	cls->name   =  get2u () ;
	return ((CON *) cls) ;
      }
    case 8:
      { scon * str = (scon *) grab (sizeof (scon)) ;
	str->type   =  'S' ;
	str->val    =  get2u () ;
	return ((CON *) str) ;
      }
    case 9:
    case 10:
    case 11:
      { slotcon * slot = (slotcon *) grab (sizeof (slotcon)) ;
	slot->type  =  (ch == 9 ? 'f' :
			ch == 10 ? 'm' :
			ch == 11 ? 'i' : '?') ;
	slot->class = get2u () ;
	slot->nat   = get2u () ;
	return ((CON *) slot) ;
      }
    case 12:
      { natcon * nat = (natcon *) grab (sizeof (natcon)) ;
	nat->type  =  'n' ;
	nat->name  =  get2u () ;
	nat->sig   =  get2u () ;
	return ((CON *) nat) ;
      }
    default:
      fprintf (stderr, "constant typebyte 0x%x\n", ch) ;
      err ("illegal typebyte for constant in constants pool") ;
    }
  return ((CON *) 0) ;
}


static void print_int_char (int val)
{
  fprintf (output, "%d", val) ;
  if ((val > 0x1f) && (val < 0x7f))
    fprintf (output, "  \'%c\'", val) ;
}


static int report_bad_sig (int from, int len, unicode_char * str)
{
  int i ;
  fprintf (output, "<BAD SIG ");
  for (i = 0 ; i < from ; i++)
    putc ((char) str[i], output) ;
  fprintf (output, " | ") ;
  for (i = from ; i < len ; i++)
    putc ((char) str[i], output) ;
  fprintf (output, ">") ;
  return (len) ;
}

static Boolean dylan_mode_set = False ;
static Boolean dylan_java_aware_mode = False ;

static int demangler (int ch)
{
  if (ch == '/') ch = '.' ;  /* canonicalize class names for readability */
  if (dylan_java_aware_mode)
    {
      int j ;
      for (j = 0 ; j < 17 ; j++)
        if (ch == "_JXDPTSLGQABCUOENM"[j]) 
          { ch =  "-;!$%*/<>?+&^_@=~#"[j] ; break ; }
    }
  return ch ;
}

static int id (int ch)
{ return ch ; }


static int parse_signature_internal (int from, int len, unicode_char * str)
{
  if (len <= from)
    return (report_bad_sig (len, len, str)) ;
  switch (str [from])
    {
    case '[':
      {
        int res = parse_signature_internal (from+1, len, str) ;
        fprintf (output, "[]") ;
        return res ;
      }

    case 'L':
      {
        int end = from+1 ;
        while (end < len)
          {
            if (str [end] == ';')
              {
                int i ;
                for (i = from+1 ; i < end ; i++)
                  {
                    unicode_char ch = str [i] ;
                    putc (demangler (ch), output) ;
                  }
                return end+1 ;
              }
            end ++ ;
          }
        return (report_bad_sig (from, len, str)) ;
      }

    case '(':
      {
        int  prev = ++from ;
        Boolean  first = True ;
        fprintf (output, "(") ;
        while (True)
          {
            if (from >= len)
              return (report_bad_sig (len, len, str)) ;
            if (str [from] == ')')
              {
                fprintf (output, dylan_java_aware_mode ? ") => " : ")") ;
                return (parse_signature_internal (from+1, len, str)) ;
              }
            else
              {
                if (!first)
                  fprintf (output, dylan_java_aware_mode ? " X " : ", ") ;
                prev = from ;
                from = parse_signature_internal (from, len, str) ;
              }
            first = False ;
          }
      }

    case 'Z':
      fprintf (output, "boolean") ; 
      return (from+1);
    case 'B':
      fprintf (output, "byte") ; 
      return (from+1);
    case 'C':
      fprintf (output, "char") ; 
      return (from+1);
    case 'S':
      fprintf (output, "short") ; 
      return (from+1);
    case 'I':
      fprintf (output, "int") ; 
      return (from+1);
    case 'F':
      fprintf (output, "float") ; 
      return (from+1);
    case 'J':
      fprintf (output, "long") ; 
      return (from+1);
    case 'D':
      fprintf (output, "double") ; 
      return (from+1);
    case 'V':
      fprintf (output, dylan_java_aware_mode ? "()" : "void") ; 
      return (from+1);
    
    default:
      return (report_bad_sig (from, len, str)) ;
    }
}

static void parse_signature (ucon * str)
{ parse_signature_internal (0, str->len, str->unicode) ; }


static void print_unicode (ucon * str, Boolean demangling)
{
  int n ;
  int len = str->len ;
  unicode_char * chars = str->unicode ;
  for (n = 0 ; n < len ; n++)
    putc ((demangling ? demangler : id) (chars [n]), output) ;
}


     

static CON ** the_conpool ;
static int the_conpool_len ;
static Boolean constants_with_names = True ;
static Boolean print_as_sig = False ;

static void print_constant_internal (CON ** conpool, int i, Boolean demangling) ;

static void print_constant (CON ** conpool, int i)
{ print_constant_internal (conpool, i, True) ; }

static void print_constant_internal (CON ** conpool, int i, Boolean demangling)
{
  if (i <= 0 || i >= the_conpool_len)
    { fprintf (output, "<BADCON %d>", i) ; }
  else
    {
      CON * ent = conpool [i] ;
      int type  = ent->type ;
      switch (type)
	{
	case 'U' :
	  { ucon * u = (ucon *) ent ;
            if (!constants_with_names)
              fprintf (output, "    ") ;
            if (print_as_sig)
              { parse_signature (u); }
            else
              { print_unicode (u, demangling) ; }
	  }
	  break ;
	case 'S' :
          {
            int utf = ((scon *) ent)->val ;
            if (constants_with_names)
              {
                fprintf (output, "\"") ;
                print_constant_internal (conpool, utf, False) ; // no demangling
                fprintf (output, "\"") ; 
              }
            else
              fprintf (output, "String (%d)", utf) ; 
          }
          break ;

	case 'I' :
	  print_int_char (((icon *)ent)->number) ; 
	  break ;
	case 'L' :
	  fprintf (output, "Long!") ; break ;
	case 'F' :
	  fprintf (output, "%ff", ((fcon *)ent)->number) ; break ;
	case 'D' :
	  fprintf (output, "%lf", ((dcon *)ent)->number) ; break ;

	case 'c' :
          {
            int utf = ((scon *) ent)->val ;
            if (constants_with_names)
              {
                fprintf (output, "{") ;
                print_constant (conpool, utf) ; 
                fprintf (output, "}") ; 
              }
            else
              fprintf (output, "Class (%d)", utf) ; 
          }
          break ;

	case 'f' :
	case 'm' :
	case 'i' :
          fprintf (output, constants_with_names ? ""    : "Field (") ;
          print_constant (conpool, ((slotcon *) ent)->class) ; 
          fprintf (output, constants_with_names ? " . " : ", ") ;
          print_constant (conpool, ((slotcon *) ent)->nat) ;
          fprintf (output, constants_with_names ? ""    : ")") ;
	  break ;

	case 'n' :
          {
            int  nam = ((natcon *) ent)->name ;
            int  sig = ((natcon *) ent)->sig ;
            if (constants_with_names)
              {
                print_constant (conpool, nam) ; 
                fprintf (output, dylan_java_aware_mode ? " :: " : "        ; ") ;
                { 
                  Boolean old = print_as_sig ;
                  print_as_sig = True ;
                  print_constant (conpool, sig) ;
                  print_as_sig = old ;
                }
              }
            else
              fprintf (output, "NameType (%d, %d)", nam, sig) ;
          }
	  break ;
      
	default:
	  fprintf (output, "yukky constant!");

	}
    }
}

static int double_sized (CON * ent)
{
  int type = ent->type ;
  switch (type)
    {
    case 'L' :
    case 'D' :
      return True ;

    default:
      return False ;
    }
}

static int print_constant_type (CON ** conpool, int i)
{
  if (i <= 0 || i >= the_conpool_len)
    { fprintf (output, "?") ; }
  else
    {
      CON * ent = conpool [i] ;
      int type = ent->type ;
      switch (type)
	{
	case 'S' :
	  fprintf (output, "_string "); return (1) ;

	case 'I' :
	  fprintf (output, "_int ") ; return (1) ;
	case 'L' :
	  fprintf (output, "_long ") ; return (2) ;
	case 'F' :
	  fprintf (output, "_float ") ; return (1) ;
	case 'D' :
	  fprintf (output, "_double ") ; return (2) ;

	case 'c' :
	  fprintf (output, "_class ") ; return (1 | 0x100) ;
	case 'U' :
	  fprintf (output, "_Utf8 ") ; return (1 | 0x100) ;
	default:
	  fprintf (output, "_yuk(%c)!", type); return (1 | 0x100) ;

	}
    }
}





static CON ** parse_constants (int len)
{
  CON ** conpool = (CON **) grab (len * sizeof (CON *)) ;
  int n = 1;
  int constant_size ;
  the_conpool = conpool ;
  the_conpool_len = len ;
  while (n < len)
    { 
      conpool [n]  =  parse_constant (&constant_size);
      n += constant_size ;
    }
  if (n != len)
    err ("badly sized constants pool") ;

  return (conpool) ;
}

static void maybe_show_constants (CON ** conpool, int len)
{
  if (show_constants_pool)
    {
      Boolean old = constants_with_names ;
      int n = 1 ;
      constants_with_names = False ;
      fprintf (output, "class constants pool:\n\n") ;
      while (n < len)
	{ 
          fprintf (output, "constant %3d: ", n) ;
          print_constant (conpool, n) ;
          fprintf (output, "\n") ;
          n +=  (double_sized (conpool[n]) ? 2 : 1) ;
	}
      fprintf (output, "\f\n");
      fflush (output);
      constants_with_names = old ;
    }
}


static void  parse_magic ()
{
  unsigned long  magic = get4u () ;
  if (magic != 0xcafebabe)
    err ("wrong magic number for Java class file") ;
}

static Boolean alpha_java_mode = False ;

static void  parse_versions ()
{ 
  int minor = get2u () ;
  int major = get2u () ;
  if ((major != 0x2d) || (minor < 2) || (minor > 3))
    err ("wrong version numbers for Java 1.0, 1.1 or 1.2 class file") ;
  alpha_java_mode = (minor == 2) ;
}


static void  parse_implements (CON ** conpool)
{
  int  iface = get2u () ;
  fprintf (output, " implements ") ;
  print_constant (conpool, iface) ;
}

static void  parse_attribute (CON ** conpool) ;

static void  describe_slot_access (int access)
{
  if ((access & ~0x00df) != 0)
    err ("bad slot access") ;
  if ((access & 0x0001) != 0)    fprintf (output, "public ") ;
  if ((access & 0x0002) != 0)    fprintf (output, "private ") ;
  if ((access & 0x0004) != 0)    fprintf (output, "protected ") ;
  if ((access & 0x0008) != 0)    fprintf (output, "static ") ;
  if ((access & 0x0010) != 0)    fprintf (output, "final ") ;
  if ((access & 0x0040) != 0)    fprintf (output, "volatile ") ;
  if ((access & 0x0080) != 0)    fprintf (output, "transient ") ;
}

static void  describe_method_access (int access)
{
  if ((access & ~0x053f) != 0)
    err ("bad method access") ;
  if ((access & 0x0001) != 0)    fprintf (output, "public ") ;
  if ((access & 0x0002) != 0)    fprintf (output, "private ") ;
  if ((access & 0x0004) != 0)    fprintf (output, "protected ") ;
  if ((access & 0x0008) != 0)    fprintf (output, "static ") ;
  if ((access & 0x0010) != 0)    fprintf (output, "final ") ;
  if ((access & 0x0020) != 0)    fprintf (output, "synchronized ") ;
  if ((access & 0x0100) != 0)    fprintf (output, "native ") ;
  if ((access & 0x0400) != 0)    fprintf (output, "abstract ") ;
}

static void  describe_class_access (int access)
{
  if ((access & ~0x0631) != 0)
    err ("bad class access") ;
  if ((access & 0x0001) != 0)    fprintf (output, "public ") ;
  if ((access & 0x0010) != 0)    fprintf (output, "final ") ;
  if ((access & 0x0020) != 0)    fprintf (output, "super ") ;
  if ((access & 0x0200) != 0)    fprintf (output, "interface ") ;
  if ((access & 0x0400) != 0)    fprintf (output, "abstract ") ;
}


static Boolean  unicode_byte_equal (int len, unicode_char * unicode, unsigned char * bytes)
{
  int n ;
  for (n = 0 ; n < len ; n++)
    {
      unsigned int byte = bytes[n] ;
      if ((byte == 0) ||
	  (byte != unicode [n]))
	return (False);
    }
  return (bytes[len] == 0) ;
}

static Boolean unicode_streq (ucon * unicode, unsigned char * bytes)
{ return (unicode_byte_equal (unicode->len, unicode->unicode, bytes)) ; }



static Boolean has_dylan_prefix (CON ** conpool, int i)
{
  if (i <= 0 || i >= the_conpool_len)
    err ("utf index outside constants pool for a class?") ;
  {
    CON * ent = conpool [i] ;
    int type  = ent->type ;
    if (type != 'c')
      err ("non class entry for this") ;
    {
      int utf = ((scon *) ent)->val ;
      if (utf <= 0 || utf >= the_conpool_len)
        err ("utf index outside constants pool for a class?") ;
      {
        ucon * u = (ucon *) (conpool [utf]) ;
        return (u->len > 6 &&
                unicode_byte_equal (6, u->unicode, "dylan/")) ;
      }
    }
  }
}



static void  generic_parse_slot (int methodp, CON ** conpool)
{
  int name, type, n ;
  Boolean old ;
  fprintf (output, "  ") ;
  if (methodp)
    { 
      describe_method_access (get2u ()) ;
      fprintf (output, "method ") ;
    }
  else
    { describe_slot_access (get2u ()) ; }
  fprintf (output, " ") ;
  name  = get2u () ;
  type  = get2u () ;
  if (dylan_java_aware_mode)
    {
      print_constant (conpool, name) ;
      fprintf (output, " :: ") ;
      old = print_as_sig ;
      print_as_sig = True ;
      print_constant (conpool, type) ;
      print_as_sig = old ;
    }
  else
    {
      old = print_as_sig ;
      print_as_sig = True ;
      print_constant (conpool, type) ;
      print_as_sig = old ;
      fprintf (output, " ") ;
      print_constant (conpool, name) ;
    }
  fprintf (output, " ;\n\n") ;
  {
    int  att_count = get2u () ;
    for (n = 0 ; n < att_count ; n++)
      parse_attribute (conpool) ;  
  }
}

static void  parse_slot (CON ** conpool)
{ generic_parse_slot (0, conpool) ; }
     
static void  parse_method (CON ** conpool)
{ generic_parse_slot (1, conpool) ; }


#define NEW  0
#define H_OF_HL  1
#define L_OF_HL  2
#define H_OF_SHORT 3
#define L_OF_SHORT 4
#define BYTE 5
#define TYPED_BYTE_CON 6
#define L_OF_LOCAL 7
#define H_OF_LOCAL 8
#define BYTE_LOC 9
#define IINC1 10
#define IINC2 11
#define INTER_H 12
#define INTER_L 13
#define INTER_N 14
#define INTER_DUM 15
#define H_OF_PCREL 16
#define L_OF_PCREL 17
#define TYPED_H_OF_HL  18
#define TYPED_L_OF_HL  19
#define TYPED_H_OF_HL_2  20
#define TYPED_L_OF_HL_2  21
#define H_OF_HLHL 22
#define L_OF_HLHL 23

#define LOOKUPSWITCH_ALIGN   24
#define LOOKUPSWITCH_DEFAULT 25
#define LOOKUPSWITCH_COUNT   26
#define LOOKUPSWITCH_KEY     27
#define LOOKUPSWITCH_VALUE   28

#define TABLESWITCH_ALIGN    29
#define TABLESWITCH_DEFAULT  30
#define TABLESWITCH_LO       31
#define TABLESWITCH_HI       32
#define TABLESWITCH_VALUE    33

#define IINC1W 34
#define IINC2W 35
#define BYTE_LOCW 36

static int state = NEW ;
static int wide = False ;
static int partial = 0 ;

static int switch_default ;
static int switch_count ;
static int switch_value ;
static int look_key ;
static int table_lo ;
static int table_hi ;
static int opstart ;
static int methend ;

static int pass2 ;
static char * label_flags ;

static void output_branch_dest (int value, Boolean nl, char * which)
{
  if (value < 0 || value >= methend)
    {
      fprintf (stderr, "out of range branch from %d:%d by %d in %s\n", opstart, methend, value, which) ;
      value = 0 ;
    }
  if (pass2)
    { fprintf (output, (nl ? "%d\n" : "%d"), value) ; }
  else
    { label_flags[value] = True ; }
}

static void output_switch_arm (int key, int value, Boolean defaultp)
{
  int dest = value + opstart ;
  if (dest < 0 || dest >= methend)
    {
      fprintf (stderr, "out of range switch from %d:%d by %d\n", opstart, methend, dest) ;
      dest = 0 ;
    }
  if (pass2)
    {
      if (defaultp)
	{ fprintf (output, "           default: ") ;
	  output_branch_dest (dest, False, "switch-default");
	}
      else
	{ fprintf (output, "             %5d: ", key) ;
	  output_branch_dest (dest, False, "switch-arm") ; 
	  if ((key > 0x1f) && (key < 0x7f))
	    fprintf (output, "  \'%c\'", key) ;
	}
      putc ('\n', output) ;
    }
  else
    {
      label_flags [dest] = True ;
    }
}


static void record_dest (int value)
{
  if (value == methend)
    return ;
  if (value < 0 || value >= methend)
    {
      fprintf (stderr, "out of range dest from %d:%d by %d\n", opstart, methend, value) ;
      value = 0 ;
    }
  label_flags[value] = True ;
}

#define next_aligned(_p) (((_p)&3)==3)

#define p2printf  if (pass2) fprintf


static void handle_bytecode (int code, CON ** conpool, int pc, unsigned char * the_code_vector)
{ 
  if (label_flags [pc] && (state != NEW || wide))
    { fprintf (output, "<<illegal-jump-dest %d>>", pc) ; }

  switch (state)
    {
    case IINC1:
      if (wide)
	{
	  partial = code ;
	  state = IINC1W ;
	}
      else
	{
	  partial = code ;
	  if (pass2) fprintf (output, "loc%d += ", partial);
	  state = IINC2 ;
	}
      break;
    case IINC1W:
      partial = (partial << 8) | code ;
      if (pass2) fprintf (output, "loc%d += ", partial);
      state = IINC2 ;
      break;

    case IINC2:
      partial = ((code & 0x80)==0 ? code : code - 0x100) ;
      if (wide)
	{ state = IINC2W ; }
      else
	{
	  if (pass2) fprintf (output, "%d\n", partial) ;
	  state = NEW;
	}
      break;
    case IINC2W:
      partial = (partial << 8) | code ;
      if (pass2) fprintf (output, "%d\n", partial) ;
      state = NEW; wide = False ;
      break;

    case H_OF_HLHL:
      partial = code ;
      state = L_OF_HLHL ;
      break;
    case L_OF_HLHL:
      partial = (partial << 8) | code ;
      state = H_OF_HL ;
      if (pass2) print_constant (conpool, partial) ;
      if (pass2) fprintf (output, ", ") ;
      partial = 0 ;
      break;
    case H_OF_HL:
      partial = code ;
      state = L_OF_HL ;
      break;
    case L_OF_HL:
      partial = (partial << 8) | code ;
      state = NEW ;
      if (pass2) print_constant (conpool, partial) ;
      if (pass2) fprintf (output, "\n") ;
      partial = 0 ;
      break;
    case INTER_H:
      partial = code ;
      state = INTER_L ;
      break;
    case INTER_L:
      partial = (partial << 8) | code ;
      state = INTER_N ;
      if (pass2) print_constant (conpool, partial) ;
      partial = 0 ;
      break;
    case INTER_N:
      partial = code ;
      if (pass2) fprintf (output, "(%d args)\n", partial) ;
      state = INTER_DUM;
      break;
    case INTER_DUM:
      state = NEW ;
      break;
    case TYPED_BYTE_CON:
      partial =  code ;
      state = NEW ;
      if (pass2)
	{ 
	  if (1 != print_constant_type (conpool, partial))
	    fprintf (output, "<YUK>") ;
	  print_constant (conpool, partial) ;
	  fprintf (output, "\n") ;
	}
      partial = 0 ;
      break;
    case H_OF_SHORT:
      partial = ((code & 0x80)==0 ? code : code | 0xffffff00) ;
      state = L_OF_SHORT ;
      break ;
    case L_OF_SHORT:
      partial = (partial << 8) | code ;
      if (pass2)
	{ print_int_char (partial);
	  putc ('\n', output) ;
	}
      state = NEW ;
      break;
    case H_OF_PCREL:
      partial = ((code & 0x80)==0 ? code : code | 0xffffff00) ;
      state = L_OF_PCREL ;
      break ;
    case L_OF_PCREL:
      partial = (partial << 8) | code ;
      output_branch_dest (partial + pc-2, True, "pc-rel instruction") ;
      state = NEW ;
      break;
    case BYTE:
      partial = ((code & 0x80)==0 ? code : code | 0xffffff00) ;
      if (pass2) 
	{ print_int_char (partial) ;
	  fprintf (output, "\n");
	}
      state = NEW ;
      break;
    case H_OF_LOCAL:
      partial = code ;
      state = L_OF_LOCAL ;
      break ;
    case L_OF_LOCAL:
      partial = (partial << 8) | code ;
      if (pass2) fprintf (output, "loc%d\n", partial);
      state = NEW ;
      break;
    case BYTE_LOC:
      partial = code ;
      if (wide)
	{ state = BYTE_LOCW; }
      else
	{
	  if (pass2) fprintf (output, "loc%d\n", partial);
	  state = NEW ;
	}
      break;
    case BYTE_LOCW:
      partial = (partial << 8) | code ;
      if (pass2) fprintf (output, "loc%d\n", partial);
      state = NEW ; wide = False ;
      break;

    case TYPED_H_OF_HL:
      partial = code ;
      state = L_OF_HL ;
      break;
    case TYPED_L_OF_HL:
      partial = (partial << 8) | code ;
      state = NEW ;
      if (pass2) 
	{
	  if (1 != print_constant_type (conpool, partial))
	    fprintf (output, "<YUK>") ;
	  print_constant (conpool, partial) ;
	  fprintf (output, "\n") ;
	}
      partial = 0 ;
      break;
    case TYPED_H_OF_HL_2:
      partial = code ;
      state = L_OF_HL ;
      break;
    case TYPED_L_OF_HL_2:
      partial = (partial << 8) | code ;
      state = NEW ;
      if (pass2)
	{
	  if (2 != print_constant_type (conpool, partial))
	    fprintf (output, "<YUK>") ;
	  print_constant (conpool, partial) ;
	  fprintf (output, "\n") ;
	}
      partial = 0 ;
      break;

    case LOOKUPSWITCH_ALIGN:
      if (next_aligned (pc))
	{ partial = 0 ;
	  state = LOOKUPSWITCH_DEFAULT ;
	}
      break ;
    case LOOKUPSWITCH_DEFAULT:
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ switch_default = partial ;
	  partial = 0 ;
	  state = LOOKUPSWITCH_COUNT ;
	}
      break ;
    case LOOKUPSWITCH_COUNT:  
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ switch_count = partial ;
	  partial = 0 ;
	  state = LOOKUPSWITCH_KEY ;
	}
      break ;
    case LOOKUPSWITCH_KEY:    
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ look_key = partial ;
	  partial = 0 ;
	  state = LOOKUPSWITCH_VALUE ;
	}
      break ;
    case LOOKUPSWITCH_VALUE:
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ switch_value = partial ;
	  output_switch_arm (look_key, switch_value, False) ;
	  partial = 0 ;
	  switch_count -- ;
	  if (switch_count == 0)
	    { 
	      state = NEW ; 
	      output_switch_arm (0, switch_default, True) ;
	    }
	  else
	    { state = LOOKUPSWITCH_KEY ; }
	}
      break ;


    case TABLESWITCH_ALIGN:
      if (next_aligned (pc))
	{ partial = 0 ;
	  state = TABLESWITCH_DEFAULT ;
	}
      break ;
    case TABLESWITCH_DEFAULT:
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ switch_default = partial ;
	  partial = 0 ;
	  state = TABLESWITCH_LO ;
	}
      break ;
    case TABLESWITCH_LO:      
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ table_lo = partial ;
	  partial = 0 ;
	  state = TABLESWITCH_HI ;
	}
      break ;
    case TABLESWITCH_HI:      
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ table_hi = partial ;
	  switch_count = table_hi - table_lo + 1 ;
	  partial = 0 ;
	  state = TABLESWITCH_VALUE ;
	}
      break ;
    case TABLESWITCH_VALUE:   
      partial = (partial << 8) | code ;
      if (next_aligned (pc))
	{ switch_value = partial ;
	  output_switch_arm (table_lo, switch_value, False) ;
	  partial = 0 ;
	  table_lo ++ ;
	  switch_count -- ;
	  if (switch_count == 0)
	    { 
	      state = NEW ; 
	      output_switch_arm (0, switch_default, True) ;
	    }
	}
      break ;


    case NEW:
      if (wide)
	{
	  if ((code != 0x84) &&
	      (code < 0x15 || code > 0x19) &&
	      (code < 0x36 || code > 0x3A))
	    {
	      fprintf (stderr, "Bad wide opcode %2x\n", code) ;
	      wide = False ;
	    }
	}
      if (pass2)
	{ 
	  if (label_flags[pc])
	    putc ('\n', output) ;
	  if (wide)
	    { fprintf (output, "%2x wide_", code) ; }
	  else if (code == 0xc4)
	    { fprintf (output, "    %3d: %2x", pc, code) ; }
          else
	    { fprintf (output, "    %3d: %2x   ", pc, code) ; }
	}
      opstart = pc ;
      switch (code)
	{
	case 0x00: p2printf (output, "nop\n") ; break;
	case 0x01: p2printf (output, "push null\n") ; break;
	case 0x02: 
	case 0x03:
	case 0x04:
	case 0x05:
	case 0x06:
	case 0x07:
	case 0x08: p2printf (output, "push_int %d\n", code-0x03); break ;
	case 0x09:
	case 0x0A: p2printf (output, "push_long %d\n", code-0x09) ; break ;
	case 0x0B:
	case 0x0C:
	case 0x0D: p2printf (output, "push_float %d.0\n", code-0x0B); break;
	case 0x0E:
	case 0x0F: p2printf (output, "push_double %d.0\n", code-0x0E); break;
	case 0x10: p2printf (output, "push_int "); state = BYTE ; break;
	case 0x11: p2printf (output, "push_int "); state = H_OF_SHORT ; break;
	case 0x12: p2printf (output, "pushcon"); state = TYPED_BYTE_CON ; break;
	case 0x13: p2printf (output, "pushcon "); state = TYPED_H_OF_HL ; break;
	case 0x14: p2printf (output, "pushcon "); state = TYPED_H_OF_HL_2 ; break;
	case 0x15: p2printf (output, "push_int "); state = BYTE_LOC ; break;
	case 0x16: p2printf (output, "push_long "); state = BYTE_LOC ; break;
	case 0x17: p2printf (output, "push_float "); state = BYTE_LOC ; break ;
	case 0x18: p2printf (output, "push_double "); state = BYTE_LOC ; break;
	case 0x19: p2printf (output, "push "); state = BYTE_LOC ; break ;        
	case 0x1A:          
	case 0x1B:
	case 0x1C:
	case 0x1D: p2printf (output, "push_int loc%d\n", code-0x1A); break;
	case 0x1E:
	case 0x1F:  
	case 0x20:  
	case 0x21: p2printf (output, "push_long loc%d\n", code-0x1E); break;
	case 0x22:
	case 0x23:
	case 0x24:
	case 0x25: p2printf (output, "push_float loc%d\n", code-0x22); break;
	case 0x26:
	case 0x27:
	case 0x28:
	case 0x29: p2printf (output, "push2_double loc%d\n", code-0x26); break;
	case 0x2A:
	case 0x2B:
	case 0x2C:
	case 0x2D: p2printf (output, "push loc%d\n", code-0x2A); break;            
	case 0x2E: p2printf (output, "load_int\n"); break;
	case 0x2F: p2printf (output, "load_long\n"); break;
	case 0x30: p2printf (output, "load_float\n"); break;
	case 0x31: p2printf (output, "load_double\n"); break;
	case 0x32: p2printf (output, "load\n"); break;
	case 0x33: p2printf (output, "load_byte\n"); break;
	case 0x34: p2printf (output, "load_char\n"); break;
	case 0x35: p2printf (output, "load_short\n"); break; 
	case 0x36: p2printf (output, "pop_int "); state = BYTE_LOC ; break;   
	case 0x37: p2printf (output, "pop_long "); state = BYTE_LOC ; break; 
	case 0x38: p2printf (output, "pop_float "); state = BYTE_LOC ; break ;
	case 0x39: p2printf (output, "pop_double "); state = BYTE_LOC ; break;
	case 0x3A: p2printf (output, "pop "); state = BYTE_LOC ; break ;        
	case 0x3B:                                                           
	case 0x3C:                                                           
	case 0x3D:                                                           
	case 0x3E: p2printf (output, "pop_int loc%d\n", code-0x3B); break;      
	case 0x3F:                                                           
	case 0x40:                                                           
	case 0x41:                                                           
	case 0x42: p2printf (output, "pop_long loc%d\n", code-0x3F); break;    
	case 0x43:                                                           
	case 0x44:                                                           
	case 0x45:                                                           
	case 0x46: p2printf (output, "pop_float loc%d\n", code-0x43); break;    
	case 0x47:                                                           
	case 0x48:                                                           
	case 0x49:                                                           
	case 0x4A: p2printf (output, "pop_double loc%d\n", code-0x47); break;  
	case 0x4B:                                                           
	case 0x4C:                                                           
	case 0x4D:                                                           
	case 0x4E: p2printf (output, "pop loc%d\n", code-0x4B); break;            
	case 0x4F: p2printf (output, "store_int\n"); break;   
	case 0x50: p2printf (output, "store_long\n"); break;  
	case 0x51: p2printf (output, "store_float\n"); break; 
	case 0x52: p2printf (output, "store_double\n"); break;
	case 0x53: p2printf (output, "store\n"); break;         
	case 0x54: p2printf (output, "store_byte\n"); break;  
	case 0x55: p2printf (output, "store_char\n"); break;  
	case 0x56: p2printf (output, "store_short\n"); break; 
	case 0x57: p2printf (output, "pop\n"); break;
	case 0x58: p2printf (output, "pop2\n"); break;
	case 0x59: p2printf (output, "dup\n"); break;
	case 0x5A: p2printf (output, "dup_x1\n"); break;
	case 0x5B: p2printf (output, "dup_x2\n"); break;
	case 0x5C: p2printf (output, "dup2\n"); break;
	case 0x5D: p2printf (output, "dup2_x1\n"); break;
	case 0x5E: p2printf (output, "dup2_x2\n"); break;
	case 0x5F: p2printf (output, "swap\n"); break;
	case 0x60: p2printf (output, "add_int\n"); break;
	case 0x61: p2printf (output, "add_long\n"); break;
	case 0x62: p2printf (output, "add_float\n"); break;
	case 0x63: p2printf (output, "add_double\n"); break;
	case 0x64: p2printf (output, "sub_int\n"); break;
	case 0x65: p2printf (output, "sub_long\n"); break;
	case 0x66: p2printf (output, "sub_float\n"); break;
	case 0x67: p2printf (output, "sub_double\n"); break;
	case 0x68: p2printf (output, "mul_int\n"); break;
	case 0x69: p2printf (output, "mul_long\n"); break;
	case 0x6A: p2printf (output, "mul_float\n"); break;
	case 0x6B: p2printf (output, "mul_double\n"); break;
	case 0x6C: p2printf (output, "div_int\n"); break;
	case 0x6D: p2printf (output, "div_long\n"); break;
	case 0x6E: p2printf (output, "div_float\n"); break;
	case 0x6F: p2printf (output, "div_double\n"); break;
	case 0x70: p2printf (output, "mod_int\n"); break;
	case 0x71: p2printf (output, "mod_long\n"); break;
	case 0x72: p2printf (output, "mod_float\n"); break;
	case 0x73: p2printf (output, "mod_double\n"); break;
	case 0x74: p2printf (output, "neg_int\n"); break;
	case 0x75: p2printf (output, "neg_long\n"); break;
	case 0x76: p2printf (output, "neg_float\n"); break;
	case 0x77: p2printf (output, "neg_double\n"); break;
	case 0x78: p2printf (output, "shl_int\n"); break;
	case 0x79: p2printf (output, "shl_long\n"); break;
	case 0x7A: p2printf (output, "shr_int\n"); break;
	case 0x7B: p2printf (output, "shr_long\n"); break;
	case 0x7C: p2printf (output, "ushr_int\n"); break;
	case 0x7D: p2printf (output, "ushr_long\n"); break;
	case 0x7E: p2printf (output, "and_int\n"); break;
	case 0x7F: p2printf (output, "and_long\n"); break;
	case 0x80: p2printf (output, "or_int\n"); break;
	case 0x81: p2printf (output, "or_long\n"); break;
	case 0x82: p2printf (output, "xor_int\n"); break;
	case 0x83: p2printf (output, "xor_long\n"); break;
	case 0x84: p2printf (output, "inc_int "); state = IINC1 ; break ;
	case 0x85: p2printf (output, "int2long\n"); break;
	case 0x86: p2printf (output, "int2float\n"); break;
	case 0x87: p2printf (output, "int2double\n"); break;
	case 0x88: p2printf (output, "long2int\n"); break;
	case 0x89: p2printf (output, "long2float\n"); break;
	case 0x8A: p2printf (output, "long2double\n"); break;
	case 0x8B: p2printf (output, "float2int\n"); break;
	case 0x8C: p2printf (output, "float2long\n"); break;
	case 0x8D: p2printf (output, "float2double\n"); break;
	case 0x8E: p2printf (output, "double2int\n"); break;
	case 0x8F: p2printf (output, "double2long\n"); break;
	case 0x90: p2printf (output, "double2float\n"); break;
	case 0x91: p2printf (output, "int2byte\n"); break;
	case 0x92: p2printf (output, "int2char\n"); break;
	case 0x93: p2printf (output, "int2short\n"); break;
	case 0x94: p2printf (output, "cmp_long\n"); break;
	case 0x95: p2printf (output, "cmpl_float\n"); break;
	case 0x96: p2printf (output, "cmpg_float\n"); break;
	case 0x97: p2printf (output, "cmpl_double\n"); break;
	case 0x98: p2printf (output, "cmpg_double\n"); break;
	case 0x99: p2printf (output, "ifeq_int "); state = H_OF_PCREL ; break;
	case 0x9A: p2printf (output, "ifne_int "); state = H_OF_PCREL ; break;
	case 0x9B: p2printf (output, "iflt_int "); state = H_OF_PCREL ; break;
	case 0x9C: p2printf (output, "ifge_int "); state = H_OF_PCREL ; break;
	case 0x9D: p2printf (output, "ifgt_int "); state = H_OF_PCREL ; break;
	case 0x9E: p2printf (output, "ifle_int "); state = H_OF_PCREL ; break;
	case 0x9F: p2printf (output, "if_cmpeq_int "); state = H_OF_PCREL ; break;
	case 0xA0: p2printf (output, "if_cmpne_int "); state = H_OF_PCREL ; break;
	case 0xA1: p2printf (output, "if_cmplt_int "); state = H_OF_PCREL ; break;
	case 0xA2: p2printf (output, "if_cmpge_int "); state = H_OF_PCREL ; break;
	case 0xA3: p2printf (output, "if_cmpgt_int "); state = H_OF_PCREL ; break;
	case 0xA4: p2printf (output, "if_cmple_int "); state = H_OF_PCREL ; break;
	case 0xA5: p2printf (output, "if_cmpeq "); state = H_OF_PCREL ; break;
	case 0xA6: p2printf (output, "if_cmpne "); state = H_OF_PCREL ; break;
	case 0xA7: p2printf (output, "goto "); state = H_OF_PCREL ; record_dest (pc+3) ; break;
	case 0xA8: p2printf (output, "jsr "); state = H_OF_PCREL ;  record_dest (pc+3) ;  break;
	case 0xA9: p2printf (output, "ret "); state = BYTE_LOC ; break;
	case 0xAA: p2printf (output, "tableswitch\n") ; 
	  if (next_aligned(pc))
	    { partial = 0 ;
	      state = TABLESWITCH_DEFAULT ;
	    }
	  else
	    state = TABLESWITCH_ALIGN ;
	  break ;
	case 0xAB: p2printf (output, "lookupswitch\n") ; 
	  opstart = pc ;
	  if (next_aligned(pc))
	    { partial = 0 ;
	      state = LOOKUPSWITCH_DEFAULT ;
	    }
	  else
	    state = LOOKUPSWITCH_ALIGN ;
	  break ;
	case 0xAC: p2printf (output, "return_int\n\n");  break;
	case 0xAD: p2printf (output, "return_long\n\n"); break;
	case 0xAE: p2printf (output, "return_float\n\n"); break;
	case 0xAF: p2printf (output, "return_double\n\n"); break;
	case 0xB0: p2printf (output, "return\n\n"); break;
	case 0xB1: p2printf (output, "return_void\n\n"); break;
	case 0xB2: p2printf (output, "getstatic "); state = H_OF_HL ; break;
	case 0xB3: p2printf (output, "putstatic "); state = H_OF_HL ; break;
	case 0xB4: p2printf (output, "getfield ") ; state = H_OF_HL ; break;
	case 0xB5: p2printf (output, "putfield "); state = H_OF_HL ; break ;
	case 0xB6: p2printf (output, "invokevirtual "); state = H_OF_HL ; break ;
	case 0xB7: p2printf (output, alpha_java_mode ? "invokenonvirtual " : "invokespecial "); state = H_OF_HL ; break;
	case 0xB8: p2printf (output, "invokestatic "); state = H_OF_HL ; break;
	case 0xB9: p2printf (output, "invokeinterface ") ; state = INTER_H ; break ;
	case 0xBA: p2printf (output, alpha_java_mode ? "newfromname " : "OBSOLETE newfromname ") ; break ;
	case 0xBB: p2printf (output, "new "); state = H_OF_HL ; break;
	case 0xBC: p2printf (output, "new_prim_array "); state = BYTE ; break;
	case 0xBD: p2printf (output, "newarray ") ; state = H_OF_HL; break;
	case 0xBE: p2printf (output, "arraylength\n") ; break;
	case 0xBF: p2printf (output, "throw\n\n"); break;
	case 0xC0: p2printf (output, "checkcast ") ; state = H_OF_HL ; break;
	case 0xC1: p2printf (output, "instanceof ") ; state = H_OF_HL; break;
	case 0xC2: p2printf (output, "monitorenter\n") ; break ;
	case 0xC3: p2printf (output, "monitorexit\n"); break;
	case 0xC4: wide = True ; state = NEW ; break;
	case 0xC5: p2printf (output, "multianewarray!!\n"); break ;  /* takes h l d */
	case 0xC6: p2printf (output, "ifnull ") ; state = H_OF_PCREL ; break ;
	case 0xC7: p2printf (output, "ifnonnull ") ; state = H_OF_PCREL ; break ;
	case 0xC8: p2printf (output, "goto_w!!\n\n") ; break;
	case 0xC9: p2printf (output, "jsr_w\n"); break;

	  /* the bedworth extensions */
	case 0xCA: p2printf (output, "mkfun ") ; state = H_OF_HL ; break ;
	case 0xCB: p2printf (output, "mkclosure\n") ; break ;
	case 0xCC: p2printf (output, "invokefun\n") ; break ;
	case 0xCD: p2printf (output, "emptyclosure\n") ; break ;
	case 0xCE: p2printf (output, "acload ") ; state = H_OF_HLHL ; break ;
	case 0xCF: p2printf (output, "acstore ") ; state = H_OF_HLHL ; break ;

	case 0xD0: p2printf (output, "restarg\n") ; break ;
	case 0xD1: p2printf (output, "initinteger\n") ; break ;
	case 0xD2: p2printf (output, "printstack ") ; state = H_OF_SHORT ; break ;
	case 0xD3: p2printf (output, "i2id\n") ; break ;
	case 0xD4: p2printf (output, "id2i\n") ; break ;
	case 0xD5: p2printf (output, "breakpoint\n"); break; /* was CA for standard */

	case 0xEE: p2printf (output, "applyfunction ") ; state = BYTE ; break ;
	case 0xEF: p2printf (output, "invokefunction ") ; state = BYTE ; break ;


	case 0xFE: p2printf (output, "software\n"); break;
	case 0xFF: p2printf (output, "hardware\n"); break;
	default: 
	  printf ("unexpected opcode value 0x%x\n", code) ;
	  printf ("trail is pc=%x, %x %x %x %x [%x] %x %x %x %x\n", pc,
		  the_code_vector[pc-4],
		  the_code_vector[pc-3],
		  the_code_vector[pc-2],
		  the_code_vector[pc-1],
		  the_code_vector[pc],
		  the_code_vector[pc+1],
		  the_code_vector[pc+2],
		  the_code_vector[pc+3],
		  the_code_vector[pc+4]) ;
	  err ("bad opcode") ;
	}
      break;
    default:
      err ("dejava internal loop state problem") ;
    }
}

static long bytecode_size ;

static void  parse_code_attribute (unsigned long size, CON ** conpool)
{
  unsigned int n ;
  if (size < (unsigned long)(alpha_java_mode ? 4 : 8))
    {
      fprintf (stderr, "code attrib appears to be %x long\n", size) ;
      err ("code attribute too small") ;
    }
  { 
    int  maxstack  = alpha_java_mode ? get1u() : get2u() ;
    int  maxlocals = alpha_java_mode ? get1u() : get2u() ;
    unsigned long codelen = alpha_java_mode ? get2u() : get4u() ;

    bytecode_size += codelen ;

    size -= (alpha_java_mode ? 4 : 8) ;
    if (size < codelen+2)
      err ("code attribute too small") ;
/*fflush (stdout) ;
  fprintf (stderr, "code attribute size=%d, codelen=%d\n", size, codelen);*/
    fprintf (output, "%d locals, %d stack\n", maxlocals, maxstack) ;
    { 
      unsigned int pc ;
      unsigned char * the_code =  (unsigned char *) grab (codelen * sizeof (unsigned char)) ;
      label_flags  =  (char *) grab (codelen * sizeof (char)) ;
      for (pc = 0 ; pc < codelen ; pc++)
	{ the_code[pc] = get1u () ;
/*fprintf (stderr, "%3d: %2x\n", pc, the_code [pc]);*/
	  label_flags[pc] = False ;
	}
      pass2 = False ;
      methend = codelen ;

      state = NEW ; wide = False ;
      for (pc = 0 ; pc < codelen ; pc++)
	handle_bytecode (the_code[pc], conpool, pc, the_code) ;

      state = NEW ; wide = False ;
      pass2 = True ;
      for (pc = 0 ; pc < codelen ; pc++)
	handle_bytecode (the_code[pc], conpool, pc, the_code) ;
    }
    fprintf (output, "\n\n") ;
    size -= codelen ;
    { 
      unsigned int excep_count = get2u () ;
      if (size < excep_count*8+2)
	err ("code attribute too small") ;

      for (n = 0 ; n < excep_count ; n++)
	{ 
          int  range_start = get2u () ;
	  int  range_end   = get2u () ;
	  int  handler_pc  = get2u () ;
	  int  class_no    = get2u () ;

	  if (class_no)
	    { 
              fprintf (output, "  catch (") ;
	      print_constant (conpool, class_no) ;
	      fprintf (output, " e)");
	    }
	  else
	    { fprintf (output, "  protected") ; }

	  fprintf (output, " code from %d--%d, handler @ %d\n", range_start, range_end, handler_pc) ; 

	}
      fprintf (output, "\n") ;
    }
    { 
      unsigned int att_count = get2u () ;
      for (n = 0 ; n < att_count ; n++)
	parse_attribute (conpool) ;
    }
  }
}


static void parse_random_attribute (ucon * namecon, unsigned long size)
{
  unsigned long n ;
  fprintf (output, " Unhandled \"") ;
  print_unicode (namecon, False) ;
  fprintf (output, "\" attribute (%d bytes)\n\n", size) ;
  for (n = 0 ; n < size ; n++)
    get1u ();
}

static void parse_sourcefile_attribute (ucon * namecon, unsigned long size, CON ** conpool)
{
  if (size == 2)
    {
      int  filename = get2u () ;
      fprintf (output, "  SourceFile = ") ;
      print_constant_internal (conpool, filename, False) ;
      fprintf (output, "\n\n") ;
    }
  else
    {
      fprintf (stderr, "bad SourceFile attribute?\n") ;
      parse_random_attribute (namecon, size) ; 
    }
}

static void parse_constantvalue_attribute (ucon * namecon, unsigned long size, CON ** conpool)
{
  if (size == 2)
    {
      int  index = get2u () ;
      fprintf (output, "    ConstantValue is ") ;
      print_constant_internal (conpool, index, False) ;
      fprintf (output, "\n\n") ;
    }
  else
    { 
      fprintf (stderr, "bad ConstantValue attribute?\n") ;
      parse_random_attribute (namecon, size) ; 
    }
}

static void parse_linenumbertable_attribute (ucon * namecon, unsigned long size, CON ** conpool)
{
  unsigned long  i;
  unsigned long  count = get2u () ;
  size -= 2; 
  if (count * 4 != size)
    {
      fprintf (stderr, "bad LineNumberTable attribute?\n") ;
    }
  for (i = 0 ; i < count ; i++)
    {
      int  start_pc = get2u () ;
      int  line_number = get2u () ;
      fprintf (output, "    %d = line %d\n", start_pc, line_number) ;
      size -= 4 ;
    }
  fprintf (output, "\n") ;
  while (size > 0) 
    { get1u () ; size--; }
}

static void parse_localvariabletable_attribute (ucon * namecon, unsigned long size, CON ** conpool)
{
  unsigned long i;
  unsigned long count = get2u () ;
  size -= 2; 
  if (count * 10 != size)
    {
      fprintf (stderr, "bad LocalVariableTable attribute?\n") ;
    }
  for (i = 0 ; i < count ; i++)
    {
      int  start_pc = get2u () ;
      int  length = get2u () ;
      int  name_index = get2u () ;
      int  desc_index = get2u () ;
      int  index = get2u () ;
      fprintf (output, "    loc%d from %d..%d  is  ", index, start_pc, start_pc+length) ;
      print_constant_internal (conpool, name_index, False) ;
      fprintf (output, "    ; ")  ; 
      print_constant_internal (conpool, desc_index, False) ;
      fprintf (output, "\n")  ; 
      size -= 10 ;
    }
  while (size > 0) 
    { get1u () ; size--; }
}

static void parse_exceptions_attribute (ucon * namecon, unsigned long size, CON ** conpool)
{
  unsigned long  i;
  unsigned long  count = get2u () ;
  size -= 2; 
  if (count * 2 != size)
    {
      fprintf (stderr, "bad Exceptions attribute?\n") ;
    }
  for (i = 0 ; i < count ; i++)
    {
      int  index = get2u () ;
      if (i == 0)
	fprintf (output, "    throws ") ;
      print_constant_internal (conpool, index, False) ;
      fprintf (output, (i == count-1) ? " ;\n\n" : ", ") ;
      size -= 2 ;
    }
  while (size > 0) 
    { get1u () ; size--; }
}

static void  parse_attribute (CON ** conpool)
{
  unsigned long size ;
  int name ;
  name = get2u () ;
  size = get4u () ;
  { CON * name_con = conpool [name] ;
    if (name_con->type != 'U')
      err ("attribute not named by a unicode name") ;
    {
      ucon * namecon = (ucon *) name_con ;
      if (unicode_streq (namecon, "Code"))
	{ parse_code_attribute (size, conpool) ; }
      else if (unicode_streq (namecon, "SourceFile"))
        { parse_sourcefile_attribute (namecon, size, conpool) ; }
      else if (unicode_streq (namecon, "ConstantValue"))
        { parse_constantvalue_attribute (namecon, size, conpool) ; }
      else if (unicode_streq (namecon, "LineNumberTable"))
	{ parse_linenumbertable_attribute (namecon, size, conpool) ; }
      else if (unicode_streq (namecon, "LocalVariableTable"))
	{ parse_localvariabletable_attribute (namecon, size, conpool) ; }
      else if (unicode_streq (namecon, "Exceptions"))
	{ parse_exceptions_attribute (namecon, size, conpool) ; }
      else
        { parse_random_attribute (namecon, size) ; }
    }
  }
}

static Boolean verbose_mode = False ;
static Boolean quiet_mode = False ;

void parse_class () ;

void  parse_class_file (char * inname, char * outname)
{

  /* setup for a standard file */
  input_pos = 0 ;
  input_length = 0xffffffffL ;
  
  bytecode_size = 0 ;
  input_name = inname ;
  input = stdin ;
  output = stdout ;
  if (inname)
    input = fopen (inname, "rb") ;
  if (input == NULL)
    {
      errors ++ ;
      if (!quiet_mode)
        fprintf (stderr, "+ failed to open %s for input\n", inname) ;
      return ;
    }
  if (outname)
    output = fopen (outname, "wb") ;
  if (output == NULL)
    {
      if (input != stdin)
        fclose (input) ;
      errors ++ ;
      if (!quiet_mode)
        fprintf (stderr, "+ failed to open %s for output\n", outname) ;
      return ;
    }
  parse_class() ;

  if (inname)  { fclose (input) ;  input  = NULL ; }
  if (outname) { fclose (output) ; output = NULL ; }
}
      
void parse_class ()
{
  int    ncon, i ;
  unsigned long    prev_pos ;
  long    constants_size ;
  long    slots_size ;
  long    methods_size ;
  long    attributes_size ;
  CON ** conpool ;

  parse_magic () ;
  parse_versions () ;
  ncon = get2u () ;

  prev_pos = input_pos ;
  conpool = parse_constants (ncon) ;
  constants_size = input_pos - prev_pos ;

  describe_class_access (get2u ()) ;
  { 
    int  this = get2u () ;
    int  super = get2u () ;
    Boolean  has_dylan = has_dylan_prefix (conpool, this) ;
    if (!dylan_mode_set)
      dylan_java_aware_mode = has_dylan ;
    if (alpha_java_mode)
      {
        fprintf (output, "1.0alpha3 ") ;
        fprintf (stderr, "   [ 1.0alpha3 class! ]\n") ;
      }
    fprintf (output, has_dylan ? "dylan class " : "class ") ;
    print_constant (conpool, this) ;
      
    if (super)
      { fprintf (output, " extends ") ;
	print_constant (conpool, super) ;
      }
  }

  { 
    int int_count = get2u () ;
    int n ;
    for (n = 0 ; n < int_count ; n++)
      parse_implements (conpool) ;
    fprintf (output, "\n{\n") ;
  }

  prev_pos = input_pos ;
  { 
    int slot_count = get2u () ;
    int n ;
    for (n = 0 ; n < slot_count ; n++)
      parse_slot (conpool) ;
  }
  slots_size = input_pos - prev_pos ;

  prev_pos = input_pos ;
  { 
    int method_count = get2u () ;
    int n ;
    for (n = 0 ; n < method_count ; n++)
      parse_method (conpool) ;
  }
  methods_size = input_pos - prev_pos ;

  prev_pos = input_pos ;
  { 
    int attr_count = get2u () ;
    int n ;
    for (n = 0 ; n < attr_count ; n++)
      parse_attribute (conpool) ;
  }
  attributes_size = input_pos - prev_pos ;
  for (i = 0 ; i < (verbose_mode ? 2 : 1) ; i++)
    { FILE * file = i==0 ? output : stderr ;
      fprintf (file, "  %d bytes of constants pool\n", constants_size) ;
      fprintf (file, "  %d bytes of slots data\n", slots_size) ;
      fprintf (file, "  %d bytes of methods data (%d bytes code)\n", methods_size, bytecode_size) ;
      fprintf (file, "  %d bytes of attributes data\n", attributes_size) ;
    }
  fprintf (output, "}\n\n\f\n") ;
  maybe_show_constants (conpool, ncon) ;
}

void dejava_from_zip_file (char * class_fname) ;

int main (int argc, char ** argv, char ** envp)
{
  int arg = 1 ;
  char  class_fname [100] ;
  char  list_fname [100] ;
  while (arg < argc && argv[arg][0] == '-')
    {
      char * opt = argv[arg++] ;
      while (*++opt != 0)
        {
          switch (*opt)
            {
            case 'c' : show_constants_pool = True ; break ;
            case 'd' : dylan_java_aware_mode = True ; dylan_mode_set = True ; break ;
            case 'q' : quiet_mode = True ;   verbose_mode = False ; break ;
            case 'v' : verbose_mode = True ; quiet_mode   = False ; break ;
            default:   fprintf (stderr, "ignoring unknown option %s\n", argv[arg]) ; break ;
            }
        }
    }

  if (arg < argc)
    {
      int report_progress = (argc - arg > 1) && !quiet_mode ;
      while (arg < argc)
	{
	  int n = 0 ;
	  while (argv[arg][n])
	    { class_fname[n] = argv[arg][n] ;
	      list_fname[n] = argv[arg][n] ;
	      n++;
	    }
	  if (n > 4 &&
	      class_fname [n-4] == '.' &&
	      ((class_fname [n-3] == 'j' &&
		class_fname [n-2] == 'a' &&
		class_fname [n-1] == 'r') ||
	       (class_fname [n-3] == 'z' &&
		class_fname [n-2] == 'i' &&
		class_fname [n-1] == 'p')))
	    {
	      class_fname [n] = 0 ;
	      dejava_from_zip_file (class_fname) ;
	    }
	  else if (n > 6 &&
		   class_fname [n-6] == '.' &&
		   class_fname [n-5] == 'c' &&
		   class_fname [n-4] == 'l' &&
		   class_fname [n-3] == 'a' &&
		   class_fname [n-2] == 's' &&
		   class_fname [n-1] == 's')
	    {
	      n -= 6 ;
	      class_fname[n] = '.' ;  list_fname[n] = '.' ;  n++ ;
	      class_fname[n] = 'c' ;  list_fname[n] = 'l' ;  n++ ;
	      class_fname[n] = 'l' ;  list_fname[n] = 'i' ;  n++ ;
	      class_fname[n] = 'a' ;  list_fname[n] = 's' ;  n++ ;
	      class_fname[n] = 's' ;  list_fname[n] = 't' ;  n++ ;
	      class_fname[n] = 's' ;  list_fname[n] = 0 ;  n++ ;
	      class_fname[n] = 0 ;

	      if (report_progress)
		fprintf (stderr, "%s  =>  %s\n", &class_fname[0], &list_fname[0]) ;
	      parse_class_file (&class_fname[0], &list_fname[0]) ;
	    }

	  else
	    err1 ("bad filename, not .zip/.jar/.class: %s", (int)class_fname) ;

	  arg++ ;
	}
    }
  else
    parse_class_file (0, 0) ;

  exit (errors ? 1 : 0) ;
  return (errors ? 1 : 0) ;  /* keep M$ C happy */
}


void iterate_over_zip_file (char * zipfile_name, FILE * zip_arch, int fn)
{
  unsigned long pos, end_pos ;
  unsigned int i ;
  unsigned int n = 0 ;
  unsigned char b[4] ;

  input = zip_arch ;
  
  fseek (zip_arch, -400, SEEK_END) ;  /* -22, but vary-length comment at end! (doh!) */
  end_pos = ftell (zip_arch) + 400 ;
  for (i = 0 ; i<4 ; i++)
    {
      b[i] = get1u() ;
      n++ ;
    }
  while (!(b[n&3]     == 'P' &&
	   b[(n+1)&3] == 'K' &&
	   b[(n+2)&3] ==  5 &&
	   b[(n+3)&3] ==  6))
    {
      int ch = getc (zip_arch) ;
      if (ch == EOF)
	err1 ("failed to parse .zip file end record (comment very long?)", 0) ;
      b[n&3] = (unsigned char) ch ;
      n++ ;
    }

  pos = ftell (zip_arch) ;

  {
    unsigned short  this_disk = get2u_le () ;
    unsigned short  cen_disk  = get2u_le () ;
    unsigned short  cen_disk_count = get2u_le () ;
    unsigned short  cen_count = get2u_le () ;
    if (this_disk != 0 ||
	cen_disk  != 0 ||
	cen_disk_count != cen_count)
      err1 ("multi-disk .zip file - can't cope", 0) ;
    {
      unsigned long cen_size = get4u_le () ;
      unsigned long cen_offset = get4u_le () ;
      unsigned long comment_length = get2u_le () ;
      if (end_pos - comment_length != (unsigned long) ftell (zip_arch))
	fprintf (stderr, "dodgy length on ZIP file comment\n") ;
      fprintf (stderr, "zip file comment for %s: ", zipfile_name) ;
      for (n = 0 ; n < comment_length ; n++)
	fputc (get1u (), stderr) ;
      fprintf (stderr, "\n") ;

      /* now move to the table */
      for (n = 0 ; n < cen_count ; n++)
	{
	  fseek (zip_arch, cen_offset, SEEK_SET) ;
	  {
	    char fname [500] ;
	    unsigned long m ;
	    unsigned long sig = get4u_le () ;
	    unsigned short ver_made   = get2u_le () ;
	    unsigned short ver_ext    = get2u_le () ;
	    unsigned short flags      = get2u_le () ;
	    unsigned short comp_meth  = get2u_le () ;
	    unsigned long dos_timestamp  = get4u_le () ;
	    unsigned long crc_32     = get4u_le () ;
	    unsigned long comp_size  = get4u_le () ;
	    unsigned long file_size  = get4u_le () ;
	    unsigned short fname_len = get2u_le () ;
	    unsigned short extra_len = get2u_le () ;
	    unsigned short comm_len = get2u_le () ;
	    unsigned short disk_no = get2u_le () ;
	    unsigned short internal_attr = get2u_le () ;
	    unsigned long  external_attr = get4u_le () ;
	    unsigned long  file_offset = get4u_le () ;
	    if (sig != 0x02014b50)
	      err ("bad CEN signature in .zip file") ;
	    if (ver_ext < 10)
	      err ("bad .zip file, older than version 1.0") ;
	    if (flags & 9 != 0)
	      err ("can't handle encrypted/instream .zip files") ;
	    if (comp_meth != 0)
	      err ("can't handle compressed .zip files") ;
	    for (m = 0 ; m < fname_len ; m++)
	      fname [m] = get1u() ;
	    fname [fname_len] = 0 ;
	    /*
	    fprintf (stderr, "faking read of %s (offset %x, len %x, crc %x)\n", &fname[0], file_offset, file_size, crc_32) ;
	    */
	    
	    if (fname_len > 6 &&
		fname [fname_len-6] == '.' &&
		fname [fname_len-5] == 'c' &&
		fname [fname_len-4] == 'l' &&
		fname [fname_len-3] == 'a' &&
		fname [fname_len-2] == 's' &&
		fname [fname_len-1] == 's')
	      {
		fseek (zip_arch, file_offset, SEEK_SET) ;
		{
		  char output_name [500] ;
		  int j ;
		  unsigned long sig = get4u_le () ;
		  unsigned short ver_ext    = get2u_le () ;
		  unsigned short flags      = get2u_le () ;
		  unsigned short comp_meth  = get2u_le () ;
		  unsigned long loc_dos_timestamp  = get4u_le () ;
		  unsigned long loc_crc_32     = get4u_le () ;
		  unsigned long loc_comp_size  = get4u_le () ;
		  unsigned long loc_file_size  = get4u_le () ;
		  unsigned short fname_len = get2u_le () ;
		  unsigned short extra_len = get2u_le () ;
		  unsigned long  true_file_offset = file_offset + 30 + fname_len + extra_len ;
		  if (sig != 0x04034b50)
		    err ("bad LOC signature in .zip file") ;
		  if (ver_ext < 10)
		    err ("bad .zip file, older than version 1.0") ;
		  if (flags & 9 != 0)
		    err ("can't handle encrypted/instream .zip files") ;
		  if (comp_meth != 0)
		    err ("can't handle compressed .zip files") ;

		  /* can now actually read the entry */

		  fseek (zip_arch, true_file_offset, SEEK_SET) ;
		  input_name = &fname[0] ;
		  input_pos  = 0 ;
		  input_length = file_size ;
		  for (j = 0 ; j < fname_len - 5 ; j++)
		    output_name [j] = fname [j] ;
		  output_name [j++] = 'l' ;
		  output_name [j++] = 'i' ;
		  output_name [j++] = 's' ;
		  output_name [j++] = 't' ;
		  output_name [j++] = 0 ;
		  output = fopen (&output_name[0], "wb") ;
		  if (!output)
		    err1 ("cannot open %s for writing", (int) &output_name[0]) ;
		  parse_class () ;
		  fclose (output) ;
		  input_name = "zip file" ;
		  input_length = 0xffffffffL ;

		  if (!quiet_mode)
		    fprintf (stderr, "%s  =>  %s\n", zipfile_name, &output_name[0]) ;

		}
	      }
	    else
	      {
		fprintf (stderr, "skipping non .class file %s in zip\n", &fname[0]) ;
	      }
	      
	    cen_offset += (46 + fname_len + extra_len + comm_len) ;
	  }
	}
    }
  }
}


#include <errno.h>

void dejava_from_zip_file (char * class_fname) 
{
  FILE * zip_arch = fopen (class_fname, "rb") ;
  if (!zip_arch)
    err2 ("failed to open file %s : %d", (int) class_fname, errno) ;
  
  iterate_over_zip_file (class_fname, zip_arch, 0 /*extract_zip_entry*/) ;
}


/* eof */
