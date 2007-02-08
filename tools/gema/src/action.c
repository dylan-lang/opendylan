
/* execute the action for a matched template */

/* $Id: action.c,v 1.15 2004/12/29 00:10:15 dngray Exp $ */

/*********************************************************************
  This file is part of "gema", the general-purpose macro translator,
  written by David N. Gray <dgray@acm.org> in 1994 and 1995.
  You may do whatever you like with this, so long as you retain
  an acknowledgment of the original source.
 *********************************************************************/

/*
 * $Log: action.c,v $
 * Revision 1.15  2004/12/29 00:10:15  dngray
 * Extend range of @radix by using strtoul instead of strtol.
 * (Suggested by Alex Karahalios.)
 *
 * Revision 1.14  2004/09/18 22:57:05  dngray
 * Allow MAX_DOMAINS to be larger than 255
 * (merged changes contributed by Alex Karahalios).
 *
 * Revision 1.13  2003/12/01 18:58:16  gray
 * Fix a flaw in @set-syntax that affects optimized Microsoft compile.
 *
 * Revision 1.12  2003/11/02  00:03:56  gray
 * Add trace printout of domain call.
 *
 * Revision 1.11  2001/12/31  01:35:22  gray
 * Flush stdout before invoking @shell command.
 *
 * Revision 1.10  2001/12/15  20:21:06  gray
 * Clean up compiler warnings.
 *
 * Revision 1.9  1996/04/08  05:04:50  gray
 * Fix @wrap to do nothing if the argument is empty, and to not wrap when the
 * current column is less than the margin.
 *
 * Revision 1.8  1995/08/06  02:28:55  gray
 * Ignore implicit "\I" when expanding template as "$0" so that it doesn't
 * introduce spaces that were not present in the original text.
 *
 * Revision 1.7  1995/07/04  23:36:22  gray
 * Use separate segment on Macintosh -- from David A. Mundie
 *
 * Revision 1.6  1995/06/12 02:58:25 gray
 * Add OP_OUTCOL and OP_GET_SWITCH.
 *
 * Revision 1.5  1995/05/08 03:13:42 gray
 * Add @expand-wild
 */

#ifdef MACOS
#pragma segment action
#endif

#include "pattern.h"
#include "util.h"
#include "var.h"
#include "patimp.h"
#include <ctype.h>  /* for isalnum */
#include <string.h>
#include <time.h>
#include <assert.h>
#include <stdlib.h>	/* for system */
#include "main.h"
#ifndef MSDOS
#include <locale.h>	/* for setlocale */
#endif

int wrap_column = 80;
char* wrap_indent = NULL;
static unsigned wrap_indent_length = 0;

static void
close_output(const char* pathname);

static boolean
put_var( COStream out, const char* name, boolean default_value ) {
  size_t length;
  const char* value;
  value = get_var( name, default_value, &length );
  if ( value != NULL ) {
    cos_put_len(out, value, length);
    return TRUE;
  }
  else return FALSE;
}

static void
put_number( COStream out, long number ) {
  char buf[20];
  sprintf(buf,"%ld", number);
  cos_puts(out, buf);
}

#if defined(_QC) || defined(_MSC_VER) /* Microsoft C or Quick C */
#pragma check_stack(on)
#endif

static void
put_datime( COStream out, const time_t *t ) {
  char* x = ctime(t);
  int len = strlen(x);
  cos_put_len(out, x,  x[len-1] == '\n' ? len-1 : len);
}

static CIStream
function_operand( const unsigned char** asp, CIStream* args ) {
  const unsigned char* ap;
  ap = *asp;
  if ( ap[2] == PT_SEPARATOR ) {
  if ( ap[0] == PT_PUT_ARG ) {
	/* special case when operand is $n and nothing else */
 	CIStream arg = args[ ap[1] - 1 ];
	*asp = ap+3;
	cis_rewind(arg);
  	return clone_input_stream( arg );
      } /* end PUT_PUT_ARG */
  if ( ap[0] == PT_VAR1 ) {
    /* special case for variable to avoid copying the value */
    char vname[2];
    size_t len;
    const char* value;
    vname[0] = ap[1];
    vname[1] = '\0';
    value = get_var(vname, TRUE, &len);
    if ( value != NULL ) {
      *asp = ap+3;
      return make_string_input_stream(value, len, FALSE);
    }
  }
  }
  if ( ap[0] == PT_OP ) {
   if ( ap[1] == OP_READ ) {
    CIStream path;
    ap += 2;
    path = function_operand( &ap, args );
    if ( *ap == PT_SEPARATOR ) {
      /* special case when operand is "@include{...}" and nothing else. */
      const char* pathname;
      CIStream istream;
      pathname = cis_whole_string(path);
      if ( strcmp(canonicalize_path(cos_pathname(output_stream)),
    		  canonicalize_path(pathname))==0 ) {
	/* Using same file for translation input and output --
	   actually read the backup file instead. */
	if ( current_backup != NULL )
	  pathname = current_backup;
	else input_error( input_stream, EXS_INPUT,
			  "input and output files same but no backup\n" );
      }
      else
	close_output(pathname);
      istream = open_input_file(pathname, binary );
      if ( istream == NULL )
	istream = make_string_input_stream("", 0, FALSE);
      *asp = ap + 1;
      cis_close(path);
      return istream;
    }
    cis_close(path);
  } /* end OP_READ */
#if 0	/* changed my mind; use @read{-} instead of of @stdin{}   */
   else if ( ap[1] == OP_STDIN && ap[2] == PT_SEPARATOR ) {
     *asp = ap + 3;
    return stdin_stream;
   }
#endif
  } /* end PT_OP */
  {
  /* else general case */
    COStream outbuf;
    outbuf = make_buffer_output_stream();
    *asp = do_action( *asp, args, outbuf );
    return convert_output_to_input( outbuf );
    }
}

static long
numeric_operand( const unsigned char** asp, CIStream* args ) {
  long result;
  char* end;
  const char* sp;
  sp = (const char*) *asp;
  result = strtol( sp, &end, 10 );
  if ( *end == PT_SEPARATOR && end > sp ) {
    /* short-cut for constant number */
    *asp = (const unsigned char*)end+1;
  }
  else { /* general case */
    CIStream arg;
    char* string;
    arg = function_operand( asp, args );
    string = cis_whole_string(arg);
    result = strtol( string, &end, 10 );
    while ( isspace(*end) )
	end++;
    if ( *end != '\0' )
      input_error( input_stream, EXS_NUM,
    		   "Non-numeric operand: \"%.99s\"\n", string);
    cis_close(arg);
  }
  return result;
}

struct outfile_struct {
    COStream out;
    struct outfile_struct * next;
};
static struct outfile_struct * outfiles = NULL;

static COStream
find_output_file(const char* pathname, boolean make) {
  struct outfile_struct * os;
  COStream altout;

  for ( os = outfiles ; ; os = os->next ) {
    if ( os == NULL ) {
      if ( !make )
	return NULL;
      altout = open_output_file( pathname, binary );
      if ( altout == NULL )
	return make_buffer_output_stream();
      os = allocate(sizeof(struct outfile_struct),MemoryStream);
      os->next = outfiles;
      os->out = altout;
      outfiles = os;
      break;
    }
    if ( strcmp(pathname, cos_pathname(os->out)) == 0 ) {
      altout = os->out;
      break;
    }
  }
  return altout;
}

static void
close_output(const char* pathname) {
  COStream outs = find_output_file(pathname, FALSE);
  if ( outs != NULL ) {
    struct outfile_struct * xx;
    if ( outfiles->out == outs ) {
      xx = outfiles;
      outfiles = xx->next;
    }
    else {
      struct outfile_struct * os;
      for ( os = outfiles ; ; os = os->next ) {
	xx = os->next;
	if ( xx->out == outs ) {
	  os->next = xx->next;
	  break;
	}
      }
    }
    cos_close(outs);
    free(xx);
  }
}

void
merge_pathnames( COStream out, boolean just_dir,
		 const char* dpath, const char* npath, const char* tpath ) {
  /* create a pathname by taking the directory from the first argument,
     the name from the second, and the extension from the third. */
  const char* dname;
  const char* nname;
  const char* ntype;
  const char* p;
  ntype = pathname_type(npath);
  if ( is_absolute_pathname(npath) || dpath == NULL || dpath[0] == '\0' )
    nname = npath;
  else {
    nname = pathname_name_and_type(npath);
    if ( just_dir ) {
      cos_puts(out,dpath);
      if ( cos_prevch(out) != DirDelim )
	cos_putch(out,DirDelim);
    }
    else {
      dname = pathname_name_and_type(dpath);
      for ( p = dpath ; p < dname ; p++ )
	cos_putch(out,*p);
    }
  }
  if ( ntype == NULL )
    cos_puts(out,nname);
  else
    for ( p = nname ; p < ntype ; p++ )
      cos_putch(out,*p);
  p = NULL;
  if ( tpath != NULL ) {
    p = pathname_type(tpath);
    if ( p == NULL && tpath[0] != '\0' && strchr(tpath,DirDelim)==NULL )
      p = tpath;
  }
  if ( p == NULL )
    p = ntype;
  if ( p != NULL ) {
    if ( cos_prevch(out) != '.' )
      cos_putch(out,'.');
    cos_puts(out, p);
  }
}

static const unsigned char*
skip_action( const unsigned char* action) {
  const unsigned char* as;
  unsigned char ac;

  as = action;
    for ( ; ; ) {
      ac = *as++;
      switch (ac) {
      case PT_END: return as-1;
      case PT_SEPARATOR: return as;

      case PT_PUT_ARG:
      case PT_VAR1:
      case PT_AUX:
      case PT_QUOTE:
	as++;

      case PT_ONE_OPT:
      case PT_LINE:
      case PT_MATCHED_TEXT:
	break;

      case PT_DOMAIN:
#if MAX_DOMAINS < 256
	as = skip_action( as+1 );
#else
        as = skip_action( as+2 );	/* +2 since domains take up 2 bytes */
#endif
	break;
      case PT_OP: {
	int n;
	for ( n = fnnargs[*as++] ; n > 0 ; n-- ) {
	  assert ( *as != PT_END );
	  as = skip_action(as);
	}
	break;
      }

#ifdef PT_ARG_DELIM
      case PT_ARG_DELIM:
#endif
      case PT_WORD_DELIM:
      case PT_ID_DELIM:
      case PT_SPACE:
      case PT_SKIP_WHITE_SPACE:
	break;
      default:
	assert( !is_operator(ac) );
	break;
      } /* end switch ac */
    } /* end for */
} /* end skip_action */

static const unsigned char*
do_cmp( int cmp, const unsigned char* start, CIStream* args, COStream out) {
  const unsigned char* as;
  as = start;
  if ( cmp >= 0 ) {
    as = skip_action(as);
    if ( cmp > 0 )
      as = skip_action(as);
  }
  as = do_action( as, args, out );
  if ( cmp <= 0 ) {
    as = skip_action(as);
    if ( cmp < 0 )
      as = skip_action(as);
  }
  return as;
} /* end do_cmp */

const unsigned char*
do_action( const unsigned char* action, CIStream* args, COStream out) {
  const unsigned char* as;
  unsigned char ac;
  int argn = 0;

  as = action;
  if ( as != NULL )
    for ( ; ; ) {
      ac = *as++;
      switch (ac) {
      case PT_END: return as-1;
      case PT_SEPARATOR: return as;
      case PT_PUT_ARG: {
 	CIStream arg = args[ (*as++) - 1 ];
	cis_rewind(arg);
	cos_copy_input_stream(out,arg);
	break;
      }
      case PT_ONE_OPT:
	cos_putch(out,arg_char);
	break;

      case PT_DOMAIN: {
	CIStream inbuf;
	Pattern save_rule = current_rule;
#if MAX_DOMAINS < 256
	int domain = *as++ - 1;
#else
	/* Get domain index as 14 bit little endian number */
        int domain = ((unsigned char)*as++)&0x7f;
        domain = ((((unsigned char)*as++)&0x7f)<<7) | domain;
#endif
        if ( as[0] == PT_VAR1 ||
             ( as[0] == PT_OP &&
	       ( as[1] == OP_VAR || as[1] == OP_VAR_DFLT ) ) ) {
	  /* for safety, copy the variable's value in case it is
	     changed during translation.  */
	  COStream outbuf;
	  outbuf = make_buffer_output_stream();
	  as = do_action( as, args, outbuf );
	  inbuf = convert_output_to_input( outbuf );
	}
	else /* optimized operand access */
	  inbuf = function_operand( &as, args );
#ifdef TRACE
	if ( trace_switch ) {
	  int n;
	  fprintf( stderr, "%12ld,%2d ",
		   cis_line(input_stream), cis_column(input_stream));
	  for ( n = trace_indent ; n > 0 ; n-- )
	    fputc(' ',stderr);
	  if ( cis_is_file(inbuf) ) {
	    const char* inpath = cis_pathname(inbuf);
	    if ( inpath == NULL )
	      inpath = "-";
	    fprintf( stderr, "@%s{@read{%s}}\n",
		     domains[domain]->name, inpath);
	  }
	  else 
	    fprintf( stderr, "@%s{%.60s}\n",
		     domains[domain]->name, cis_whole_string(inbuf));
	  ++trace_indent;
	}
#endif
	if ( !translate( inbuf, domains[domain], out, NULL ) &&
	     cis_is_file(inbuf) && exit_status < EXS_FAIL )
	  exit_status = EXS_FAIL;
#ifdef TRACE
	if ( trace_switch ) {
	  --trace_indent;
	}
#endif
	current_rule = save_rule;
	cis_close(inbuf);
	break;
      }

      case PT_VAR1: {
	char vname[2];
	vname[0] = *as++;
	vname[1] = '\0';
	put_var(out, vname, FALSE);
	break;
      }

      case PT_LINE:
	cos_freshline(out);
	break;

      case PT_MATCHED_TEXT:
      	do_action( current_rule->pattern, args, out );
	break;

      case PT_SPECIAL_ARG:
#if MAX_DOMAINS >= 256 /* advance one more since  2 bytes for domain index */
      case PT_RECUR:
#endif
	as++;
      case PT_REGEXP:
#if MAX_DOMAINS < 256
      case PT_RECUR:
#endif
	as++;
      case PT_MATCH_ANY:
      case PT_MATCH_ONE: {
	/* these will be encountered only when replaying the template as $0 */
 	CIStream arg = args[ argn++ ];
	cis_rewind(arg);
	cos_copy_input_stream(out,arg);
	break;
      	}

      case PT_AUX:
	as++;
	break;

      case PT_OP: {
	CIStream inbuf = NULL;
	enum Operators ac;
	ac = (enum Operators)*as++;
	switch(ac){
	case OP_UNDEFINE:
	case OP_DEFINE: {
	  inbuf = function_operand( &as, args );
	  read_patterns(inbuf, "", ac==OP_UNDEFINE);
	  break;
	}

	case OP_SUBST: {
	  int d;
	  CIStream arg;
	  Pattern save_rule = current_rule;
	  arg = function_operand( &as, args );
	  d = read_patterns(arg," temp ",FALSE);
	  inbuf = function_operand( &as, args );
	  translate ( inbuf, domains[d], out, NULL );
	  current_rule = save_rule;
	  delete_domain(d);
	  cis_close(arg);
	  break;
	}

	case OP_VAR: {
	  inbuf = function_operand( &as, args );
	  put_var(out, cis_whole_string(inbuf), FALSE );
	  break;
	}
	case OP_VAR_DFLT: {
	  inbuf = function_operand( &as, args ); /* variable name */
	  if ( put_var(out, cis_whole_string(inbuf), TRUE ) )
	    as = skip_action(as); /* skip default value */
	  else as = do_action( as, args, out ); /* output default */
	  break;
	}

	case OP_SET: {
	  CIStream name;
	  name = function_operand( &as, args );
	  inbuf = function_operand( &as, args );
	  set_var( cis_whole_string(name),
		   cis_whole_string(inbuf), cis_length(inbuf) );
	  cis_close(name);
	  break;
	}

	case OP_BIND: {
	  CIStream name;
	  name = function_operand( &as, args );
	  inbuf = function_operand( &as, args );
	  bind_var( cis_whole_string(name),
		    cis_whole_string(inbuf), cis_length(inbuf) );
	  cis_close(name);
	  break;
	}
	case OP_UNBIND: {
	  CIStream name;
	  name = function_operand( &as, args );
	  unbind_var( cis_whole_string(name) );
	  cis_close(name);
	  break;
	}

	case OP_APPEND: {
	  CIStream name;
	  name = function_operand( &as, args );
	  inbuf = function_operand( &as, args );
	  append_var( cis_whole_string(name),
		      cis_whole_string(inbuf), cis_length(inbuf) );
	  cis_close(name);
	  break;
	}

	case OP_INCR:
	case OP_DECR: {
	  CIStream name;
	  name = function_operand( &as, args );
	  incr_var( cis_whole_string(name), ac==OP_DECR? -1 : 1 );
	  cis_close(name);
	  break;
	}

	case OP_GETENV:
	case OP_GETENV_DEFAULT: {
	  CIStream dbuf = NULL;
	  char* value;
	  inbuf = function_operand( &as, args );
	  if ( ac == OP_GETENV_DEFAULT )
	    dbuf = function_operand( &as, args );
	  value = getenv(cis_whole_string(inbuf));
	  if ( value == NULL )
	    cos_copy_input_stream(out, dbuf);
	  else cos_puts(out, value);
	  cis_close(dbuf);
	  break;
	}

	case OP_ERR: {
	  static COStream err_stream = NULL;
	  if ( err_stream == NULL )
	    err_stream = make_file_output_stream(stderr,"stderr");
	  as = do_action( as, args, err_stream );
	  break;
	}

	case OP_OUT: {
	  as = do_action( as, args, output_stream );
	  break;
	}

       case OP_PATH:
       case OP_FILE: {
	  const char* path = cis_pathname(input_stream);
	  if ( path != NULL ) {
	    if ( ac == OP_FILE )
	      path = pathname_name_and_type(path);
	    cos_puts(out, path);
	  }
	  break;
       }
       case OP_OUTFILE: {
	 const char* opath;
	 opath = cos_pathname(out);
	 if ( opath == NULL )
	   opath = cos_pathname(output_stream);
	 cos_puts(out, opath);
	 break;
       }

       case OP_LINE: {
	 put_number(out, cis_line(input_stream));
	 break;
       }
       case OP_COL: {
	 put_number(out, cis_column(input_stream));
	 break;
       }
       case OP_OUTCOL: {
	 put_number(out, cos_column(output_stream));
	 break;
       }
       case OP_HELP:
	 usage();
	 break;
       case OP_VERSION:
	 cos_puts(out, Version);
	 break;

       case OP_DATE:
       case OP_TIME: {
	 time_t now;
	 struct tm* ts;
	 char tbuf [12];
	 now = time(NULL);
	 ts = localtime(&now);
	 if ( ac == OP_TIME )
	   sprintf(tbuf, "%02d:%02d:%02d",
		   ts->tm_hour, ts->tm_min, ts->tm_sec);
	 else sprintf(tbuf, "%02d/%02d/%d",
		      ts->tm_mon + 1, ts->tm_mday, 1900 + ts->tm_year);
	 cos_puts(out, tbuf);
	 break;
       }
       case OP_DATIME: {
	 time_t now;
	 now = time(NULL);
	 put_datime( out, &now );
	 break;
       }
       case OP_MODTIME: {
	 time_t mtime;
	 mtime = cis_mod_time(input_stream);
	 if ( mtime != 0 )
	   put_datime( out, &mtime );
	 break;
       }
       case OP_PROBE: {
       	 inbuf = function_operand( &as, args );
       	 cos_putch(out, probe_pathname(cis_whole_string(inbuf)));
	 break;
       }

       case OP_READ: {
       	 const char* pathname;
	 CIStream in;
       	 inbuf = function_operand( &as, args );
       	 pathname = cis_whole_string(inbuf);
	 close_output(pathname);
	 in = open_input_file(pathname,binary);
	 cos_copy_input_stream(out, in);
	 cis_close(in);
	 break;
	}

       case OP_WRITE: {
	 COStream oldout;
       	 const char* pathname;
	 oldout = output_stream;
       	 inbuf = function_operand( &as, args );
       	 pathname = cis_whole_string(inbuf);
	 output_stream = find_output_file(pathname,TRUE);
	 as = do_action( as, args, output_stream );
	 output_stream = oldout;
	 break;
	}

	case OP_CLOSE: {
       	 inbuf = function_operand( &as, args );
	 close_output(cis_whole_string(inbuf));
	 break;
	}

	case OP_COMBINEPATH:
	case OP_MERGEPATH: {
	  CIStream dir;
	  CIStream name;
	  CIStream typ;
	  dir = function_operand( &as, args );
	  name = function_operand( &as, args );
	  typ = function_operand( &as, args );
	  merge_pathnames( out, ac==OP_COMBINEPATH, cis_whole_string(dir),
				cis_whole_string(name),
				cis_whole_string(typ) );
	  cis_close(dir);
	  cis_close(name);
	  cis_close(typ);
	  break;
	}
	case OP_RELPATH: {
	  CIStream dir;
	  dir = function_operand( &as, args );
	  inbuf = function_operand( &as, args );
	  cos_puts( out, relative_pathname(cis_whole_string(dir),
					   cis_whole_string(inbuf)) );
	  cis_close(dir);
	  break;
	}
	case OP_EXP_WILD: {
       	 inbuf = function_operand( &as, args );
	 expand_wildcard ( cis_whole_string(inbuf), out );
	 break;
	}

	case OP_ADD:
	case OP_SUB:
	case OP_MUL:
	case OP_DIV:
	case OP_MOD:
	case OP_AND:
	case OP_OR: {
	  long x,y,z;
	  x = numeric_operand( &as, args );
	  y = numeric_operand( &as, args );
	  switch(ac){
	    case OP_ADD: z = x + y; break;
	    case OP_SUB: z = x - y; break;
	    case OP_MUL: z = x * y; break;
	    case OP_DIV: z = x / y; break;
	    case OP_MOD: z = x % y; break;
	    case OP_AND: z = x & y; break;
	    case OP_OR:  z = x | y; break;
	    default: /* can't happen; just to avoid compiler warning */
	      assert(FALSE);
	      z = 0;
	      break;
	  }
	  put_number(out,z);
	  break;
	}
	case OP_NOT:
	  put_number(out, ~ numeric_operand( &as, args ) );
	  break;

	case OP_RADIX: {
	  int from, to;
	  unsigned long value;
	  char* string;
	  char* end;
	  const char* fmt;
	  char buf[24]; /* enough for 64 bits in octal */
	  from = (int)numeric_operand( &as, args );
	  to = (int)numeric_operand( &as, args );
	  inbuf = function_operand( &as, args );
	  string = cis_whole_string(inbuf);
	  value = strtoul( string, &end, from );
	  if ( *end != '\0' )
	    input_error ( input_stream, EXS_NUM,
		"Invalid argument for radix %d conversion: \"%.99s\"\n",
			from, string);
	  if ( to == 8 )
	    fmt = "%lo";
	  else if ( to == 16 )
	    fmt = "%lX";
	  else {
	    if ( to != 10 )
	      input_error ( input_stream, EXS_NUM,
	    		    "Unsupported radix: %d\n", to);
	    while ( isspace(string[0]) )
	      string++;
	    fmt = (string[0]=='-') ? "%ld" : "%lu";
	  }
	  sprintf(buf, fmt, value);
	  cos_puts(out, buf);
	  break;
	}

	case OP_STR_CMP:
	case OP_STRI_CMP: {	/* string comparison */
	  CIStream x = function_operand( &as, args );
	  CIStream y = function_operand( &as, args );
	  const char* xs = cis_whole_string(x);
	  const char* ys = cis_whole_string(y);
	  int cmp;
	  cmp = ac == OP_STRI_CMP ? stricmp(xs, ys) : strcmp(xs, ys);
	  cis_close(x); cis_close(y);
	  as = do_cmp( cmp, as, args, out);
	  break;
	}
	case OP_NUM_CMP: {	/* numeric comparison */
	  long x = numeric_operand( &as, args );
	  long y = numeric_operand( &as, args );
	  int cmp;
	  if ( x < y )
	    cmp = -1;
	  else if ( x == y )
	    cmp = 0;
	  else cmp = 1;
	  as = do_cmp( cmp, as, args, out);
	  break;
	}

	case OP_LENGTH: {
	  inbuf = function_operand( &as, args );
	  put_number(out, cis_length(inbuf));
	  break;
	}

	case OP_TAB: {
	  int col;
	  col = (int)numeric_operand( &as, args );
	  cos_spaces(out, col - (int)cos_column(out));
	  break;
	}

	case OP_WRAP: {
	  unsigned length;
	  unsigned col;
	  inbuf = function_operand( &as, args );
	  length = cis_length(inbuf);
	  col = cos_column(out);
	  if ( ( ((int)(col + length)) > wrap_column &&
		 col > wrap_indent_length ) ||
	       ( col <= 1 && length > 0 ) ) {
	    cos_freshline(out);
	    cos_puts(out, wrap_indent);
	    skip_whitespace(inbuf);
	  }
	  cos_copy_input_stream(out, inbuf);
	  break;
	}

	case OP_SET_WRAP: {
	  wrap_column = (int)numeric_operand( &as, args );
  	  inbuf = function_operand( &as, args );
	  if ( wrap_indent != NULL )
	    free(wrap_indent);
	  wrap_indent_length = cis_length(inbuf);
	  wrap_indent = str_dup_len( cis_whole_string(inbuf),
				     wrap_indent_length );
	  break;
	}

	case OP_RIGHT:
	case OP_LEFT:
	case OP_CENTER: { /* justify value in fixed-length field */
	  int field_length, string_length, left_pad, right_pad;
	  field_length = (int)numeric_operand( &as, args );
	  inbuf = function_operand( &as, args );
	  string_length = cis_length(inbuf);
	  left_pad = field_length - string_length;
	  right_pad = 0;
	  if ( left_pad < 0 )
	    left_pad = 0;
	  if ( ac == OP_LEFT ) {
	    right_pad = left_pad;
	    left_pad = 0;
	  }
	  else if ( ac == OP_CENTER ) {
	    left_pad = left_pad / 2;
	    right_pad = field_length - string_length - left_pad;
	  }
	  cos_spaces(out, left_pad);
	  cos_copy_input_stream(out, inbuf);
	  cos_spaces(out, right_pad);
	  break;
	}

	case OP_FILL_RIGHT:
	case OP_FILL_LEFT:
	case OP_FILL_CENTER: { /* justify value in fixed-length field */
	  int field_length, string_length, left_pad, right_pad;
	  CIStream background;
	  int i;
	  background = function_operand( &as, args );
	  field_length = cis_length(background);
	  inbuf = function_operand( &as, args );
	  string_length = cis_length(inbuf);
	  left_pad = field_length - string_length;
	  right_pad = 0;
	  if ( left_pad < 0 )
	    left_pad = 0;
	  if ( ac == OP_FILL_LEFT ) {
	    right_pad = left_pad;
	    left_pad = 0;
	  }
	  else if ( ac == OP_FILL_CENTER ) {
	    left_pad = left_pad / 2;
	    right_pad = field_length - string_length - left_pad;
	  } else assert( ac == OP_FILL_RIGHT );
	  for ( i = left_pad ; i > 0 ; i-- )
	    cos_putch(out, cis_getch(background));
	  cos_copy_input_stream(out, inbuf);
	  if ( right_pad > 0 ) {
	    for ( i = string_length ; i > 0 ; i-- )
	      (void)cis_getch(background);
	    cos_copy_input_stream(out, background);
	  }
	  cis_close(background);
	  break;
	}

	case OP_SUBSTRING: {
	  int skip_length, result_length, string_length;
	  skip_length = (int)numeric_operand( &as, args );
	  result_length = (int)numeric_operand( &as, args );
	  inbuf = function_operand( &as, args );
	  string_length = cis_length(inbuf);
	  if ( skip_length <= string_length ) {
	    if ( skip_length < 0 )
	      skip_length = 0;
	    if ( (skip_length + result_length) > string_length )
	      result_length = string_length - skip_length;
	    cos_put_len(out, cis_whole_string(inbuf) + skip_length,
			result_length);
	  }
	  break;
	}

	case OP_DOWNCASE:
	case OP_UPCASE: {
	  int cc;
	  inbuf = function_operand( &as, args );
	  while ( (cc = cis_getch(inbuf)) != EOF )
	    cos_putch(out, ac==OP_DOWNCASE ? tolower(cc) : toupper(cc) );
	  break;
	}

	case OP_CHARINT:
	  inbuf = function_operand( &as, args );
	  put_number(out, cis_getch(inbuf));
	  break;
	case OP_INTCHAR:
	  cos_putch(out, (char)numeric_operand( &as, args ));
	  break;

	case OP_REVERSE: {
	  int len;
	  const char* start;
	  const char* ip;
	  inbuf = function_operand( &as, args );
	  len = cis_length(inbuf);
	  start = cis_whole_string(inbuf);
	  for ( ip = start+len-1 ; ip >= start ; ip-- )
	    cos_putch(out, *ip);
	  break;
	}

	case OP_SHELL: {
	  const char* command;
	  inbuf = function_operand( &as, args );
	  command = cis_whole_string(inbuf);
	  fflush(stdout);
	  if ( system( command ) < 0 ) {
	    input_error ( input_stream, EXS_SHELL,
	  		 "Failed shell command \"%.20s...\":\n", command );
	    perror("system");
	  }
	  break;
	}

	case OP_EXIT:
	  translation_status = Translate_Exited;
	  break;

	case OP_FAIL:
	  translation_status = Translate_Failed;
	  break;

	case OP_END_OR_FAIL:
	  /* ideally this should be testing whether the input stream
	     has been advanced, but that is not so easy. */
	  translation_status =
	    ( cis_out_length(out) == 0 )? Translate_Failed : Translate_Exited;
	  break;

	case OP_EXIT_STATUS:
	  exit_status = (Exit_States)(int)numeric_operand( &as, args );
	  break;

	case OP_ABORT:
	  exit((int)(exit_status > EXS_FAIL ? exit_status : EXS_FAIL ));
	  break;

	case OP_GET_SWITCH:
	case OP_SET_SWITCH: {
	  const char* name;
	  int* valpt;
	  inbuf = function_operand( &as, args );
	  name = cis_whole_string(inbuf);
	  valpt = find_switch(name);
	  if ( valpt == NULL ) {
	    input_error(input_stream, EXS_UNDEF,
	  		"Undefined switch name \"%.99s\"\n", name );
	    if ( ac == OP_SET_SWITCH )
	      (void)numeric_operand( &as, args );
	  }
	  else {
	    if ( ac == OP_SET_SWITCH )
	      *valpt = (int)numeric_operand( &as, args );
	    else
	      put_number( out, *valpt );
	  }
	  break;
	}

	case OP_SET_PARM: {
	  const char* name;
	  CIStream val;
	  inbuf = function_operand( &as, args );
	  name = cis_whole_string(inbuf);
	  val = function_operand( &as, args );
	  if ( !set_parm( name, cis_whole_string(val) ) )
	    input_error(input_stream, EXS_UNDEF,
			"Undefined parameter name \"%.99s\"\n", name );
	  cis_close(val);
	  break;
	}

	case OP_SYNTAX: {
	  const char* type;
	  const char* charset;
	  CIStream val;
	  inbuf = function_operand( &as, args );
	  val = function_operand( &as, args );
	  charset = cis_whole_string(val);
	  for ( type = cis_whole_string(inbuf) ; *type != '\0' ; type++ ) {
	    const char* chars;
	    char c[2];
	    if ( type[1] == '\0' )
	      chars = charset;
	    else {
	      c[0] = *charset++;
	      c[1] = '\0';
	      chars = c;
	    }
	    if ( !set_syntax(type[0], chars) )
	      input_error(input_stream, EXS_UNDEF,
			  "Undefined syntax type \"%.99s\"\n", type );
	  }
	  cis_close(val);
	  break;
	}

	case OP_DEFAULT_SYNTAX:
	  initialize_syntax();
	  break;

#ifndef MSDOS
       case OP_LOCALE: {
	 const char* lname;
	 inbuf = function_operand( &as, args );
	 lname = cis_whole_string(inbuf);
	 if ( setlocale(LC_ALL, lname) == NULL )
	   input_error(input_stream, EXS_UNDEF,
		       "Undefined locale \"%.99s\"\n", lname );
	 break;
       }
#endif

	case OP_REPEAT: {
	  long n = numeric_operand( &as, args );
	  if ( n <= 0 )
  	    as = skip_action(as);
	  else {
	    const unsigned char* start = as;
	    for ( ; n > 0 ; n-- )
	      as = do_action( start, args, out );
	  }
	  break;
	}

	case OP_QUOTE: {
	  inbuf = function_operand( &as, args );
	  quoted_copy( inbuf, out );
	  break;
	}
	default:
	  fprintf(stderr, "Undefined op in action: %d\n", (int)ac);
	  break;

	} /* end switch on ops */
	cis_close(inbuf);
	break;
      } /* end PT_OP */

      case PT_WORD_DELIM:
      case PT_ID_DELIM:
	/* Ignore if in expansion of "$0" */
	if ( current_rule == NULL || action != current_rule->pattern ) {
	  /* output a space if needed as a delimiter */
	  int prevch = cos_prevch(out);
	  if ( prevch != EOF )
	    if ( ac == PT_ID_DELIM ? isident(prevch) : isalnum(prevch) )
	      cos_putch(out,' ');
	}
	break;
#if 0   /* not needed now */
      case PT_ARG_DELIM:
	if ( cos_prevch(out) != Arg_Delim )
	  cos_putch(out,Arg_Delim);
	break;
#endif

      case PT_SPACE: {
	/* output a space if the last character is not white space */
	int prevch = cos_prevch(out);
	if ( !isspace(prevch) )
	  cos_putch(out,' ');
	break;
      }

      case PT_SKIP_WHITE_SPACE:
	break;

      case PT_QUOTE:		/* use next character literally */
	ac = *as++;
	/* and fall-through */
      default:
	cos_putch(out, ac);
      } /* end switch ac */
    } /* end for */
  /* can't ever get here, but return to avoid Gnu compiler warning. */
  return as;
}
