
/* pattern matching */

/* $Id: match.c,v 1.1 2004/03/12 00:42:09 cgay Exp $ */

/*********************************************************************
  This file is part of "gema", the general-purpose macro translator,
  written by David N. Gray <dgray@acm.org> in 1994 and 1995.
  You may do whatever you like with this, so long as you retain
  an acknowledgment of the original source.
 *********************************************************************/

/* $Log: match.c,v $
 * Revision 1.1  2004/03/12 00:42:09  cgay
 * Initial revision
 *
/* Revision 1.17  1996/04/21 00:34:48  gray
/* Use new `goal_state' structure to pass additional context information for
/* goal match; this allows access to previous arguments and the correct local
/* mode for case sensitivity.  New function `match_to_stream' combines common
/* code for matching on variables and previous arguments; fixed to respect
/* case-insensitive option.  Also support case-insensitive comparison for the
/* terminator of a recognizer or recursive argument.  Don't automatically skip
/* whitespace at the beginning of a template.  For domain with default rule
/* ``=@fail'', don't match an empty string if delimiter found immediately.
/* Minor improvement of trace messages.
/*
 * Revision 1.16  1995/09/29  04:09:44  gray
 * Fix `trace' format on MS-DOS.
 *
 * Revision 1.15  1995/08/27  20:46:03  gray
 * Fix for "\J" preceding empty argument with "-w" but not "-t".
 *
 * Revision 1.14  1995/08/20  05:38:21  gray
 * Fix handling of empty optional argument in argument terminator.
 * Add trace messages for matched recognizer and failed "*".
 *
 * Revision 1.13  1995/08/13  05:33:35  gray
 * Fix regression on "*" at end of template.
 *
 * Revision 1.12  1995/08/07  03:22:39  gray
 * Fix to not match on "\Z" after "@end" (regression in previous changes).
 *
 * Revision 1.11  1995/08/06  02:24:31  gray
 * Fix bug on "\A" (regression in previous version).
 * Allow "\Z" to match goal string of recursive argument.
 * Add "<J>" (match lower case) and "<K>" (match upper case).
 *
 * Revision 1.10  1995/07/27  05:27:27  gray
 * If template does not advance the input stream, continue looking for the
 * next match instead of repeating the same match.  Fix for "\B" followed by
 * argument instead of literal.  Extend `filechars' for Macintosh.
 *
 * Revision 1.9 1995/06/18 23:20:21 gray
 * Some refinement of trace.
 *
 * Revision 1.8 1995/06/12 03:03:57 gray
 * Added experimental trace option.
 * Fix to not skip whitespace when testing for delimiter of a "*" argument.
 *
 * Revision 1.7 1995/05/22 02:51:31 gray
 * A goal of "\S" should not terminate a predefined-recognizer that has not
 * yet accepted any characters and is not optional.
 *
 * Revision 1.6 1995/05/08 03:19:05 gray
 * Fixed memory leak on failed "*" argument in line mode.
 */

#include "pattern.h"
#include "util.h"
#include "patimp.h"
#include "var.h"	/* for get_var */
#include <ctype.h>  /* for isalnum, isspace */
#include <string.h>
#include <assert.h>

boolean case_insensitive = FALSE;
boolean ignore_whitespace = FALSE;

int MAX_ARG_LEN = 4096;

int arg_char;

CIStream input_stream = NULL; /* used only for error message location */

enum Translation_Status translation_status;

char* idchars = "_";
char* filechars = "./-_~#@%+="
#if defined(MSDOS)
		":\\"
#elif defined(MACOS)
  "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89"
  "\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b"
  "\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad"
  "\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf"
  "\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1"
  "\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3"
  "\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5"
  "\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"
#endif
		;

boolean
isident( int ch ) { /* is the character an identifier constituent? */
  return isalnum(ch) ||
    ( strchr(idchars,ch) != NULL && ch != 0 );
}

struct goal_state {
  const unsigned char* template;
  CIStream* args;
  int options;
};

#if defined(_QC) || defined(_MSC_VER) /* Microsoft C or Quick C */
#pragma check_stack(on)
#endif

static CIStream
match_regexp ( CIStream s, int regex_num ) {
      int lc;
      unsigned char* end_match;
      MarkBuf begin;
      unsigned char buf[1024];
      unsigned char* tp;
      int tc;

      lc = cis_prevch(s);
      cis_mark(s, &begin);
      for ( tp = buf ; tp < buf+1023 ; ) {
	tc = cis_getch(s);
	if ( tc == EOF )
	  break;
	*tp++ = (unsigned char)tc;
      	if ( tc == '\n' )
	  break;
      }
      *tp = '\0';
      end_match = regex_match( regex_num, buf, (lc == '\n' || lc == EOF ));
      cis_restore(s, &begin);
      if ( end_match == NULL )
	return NULL;
      else {
	*end_match = '\0';
	for ( tp = buf ; tp < end_match ; tp++ )
	  (void)cis_getch(s);
	return make_string_input_stream((char*)buf, end_match-buf, TRUE);
      }
}

static unsigned
get_goal_char( const unsigned char* ps ) {
  /* returns goal character, or ENDOP if no goal, or a value that
     satisfies ISOP for an operator. */
  return get_template_element(&ps,TRUE);
}

/* option bits for try_pattern */
#define MatchSwallow 1
#define MatchLine 2
#define MatchNoCase 4
#define MatchArgDelim 8

struct mark_struct {
  CIStream in;
  boolean marked;
  MarkBuf start;
};

static int
getch_marked(struct mark_struct* ps) {
  if ( !ps->marked ) {
    cis_mark(ps->in, &ps->start);
    ps->marked = TRUE;
  }
  return cis_getch(ps->in);
}

#ifdef TRACE
/* Compile with -DTRACE to enable use of the -trace option */
boolean trace_switch = FALSE;

#include <stdarg.h>
#include <limits.h>
static int trace_indent = 0;
static struct { int level; long line; int column; int ch; int domain; }
	trace_enter = {INT_MAX,0,0,0,-1};
static char*
show_char(int ch) {
  static char buf[6];
  int b2;
  buf[0] = '\'';
  buf[1] = '\\';
  buf[3] = '\'';
  buf[4] = '\0';
  switch(ch){
  case EOF: return "EOF";
  case '\n': b2 = 'n'; break;
#if '\r' != '\n'
  case '\r': b2 = 'r'; break;
#endif
  case '\t': b2 = 't'; break;
  case '\f': b2 = 'f'; break;
  case '\b': b2 = 'b'; break;
  default: 
    if ( (ch >> 3) == 0 )
      b2 = '0' + ch;
    else { buf[1] = ch; b2 = '\''; buf[3] = '\0'; }
    break;
  }
  buf[2] = b2;
  return buf;
}

typedef enum { FAIL, OK } trace_kinds;

static void
trace( struct mark_struct * mp, trace_kinds kind, const char* format, ... ) {
  va_list args;
  va_start(args,format);
  if ( trace_switch ) {
    int n;
    if ( trace_indent > trace_enter.level ) {
      fprintf( stderr, "%12ld,%2d ", trace_enter.line, trace_enter.column);
      for ( n = trace_enter.level ; n > 0 ; n-- )
	fputc(' ',stderr);
      fprintf( stderr, "Try <%s> at %s\n",
	       domains[trace_enter.domain]->name,
	       show_char(trace_enter.ch) );
    }
    trace_enter.level = INT_MAX;
    if ( mp->in != input_stream )
      fprintf( stderr, "%16s", "");
    else {
      if ( mp->marked )
	fprintf( stderr, "%4ld,%2d:", mp->start.line, mp->start.column );
      else fprintf( stderr, "%8s","");
      fprintf( stderr, "%4ld,%2d ", cis_line(mp->in), cis_column(mp->in));
    }
    for ( n = trace_indent ; n > 0 ; n-- )
      fputc(' ',stderr);
    if ( kind == FAIL ) fprintf( stderr, "Failed ");
    vfprintf(stderr, format, args);
    if ( kind == FAIL ) fprintf( stderr, " at %s\n",
				 show_char(cis_peek(mp->in)) );
  }
  va_end(args);
}

#define TRACE_FAILURE(string) if(trace_switch) trace(&marker,FAIL,string)

#else
#define trace_switch FALSE
#define TRACE_FAILURE(string)
#endif

static int
match_to_stream(struct mark_struct* ps, CIStream arg, int local_options) {
  int ac, kc;
  CIStream in;

  in = ps->in;
  while ( ( ac = cis_getch(arg) ) != EOF ) {
loop1:
    kc = cis_peek(in);
    if ( ac == kc ||
	 ( (local_options & MatchNoCase) && toupper(ac) == toupper(kc) ) )
      (void)getch_marked(ps);
    else if ( ignore_whitespace && isspace(kc) &&
	      !( kc == '\n' && (local_options & MatchLine) ) &&
	      cis_prevch(arg) == EOF && ps->marked ) {
      (void)cis_getch(in);
      goto loop1;
    }
    else return FALSE;
  }
  return TRUE;
}

boolean
try_pattern( CIStream in, const unsigned char* patstring, CIStream* next_arg,
	     CIStream* all_args, int options, Goal goal ) {
  const unsigned char* ps;
  int ic, pc;
  struct mark_struct marker;
  MarkBuf end_position;
  boolean end_position_marked;
  boolean match;
  int local_options;

  local_options = options;
  marker.marked = FALSE;
  marker.in = in;
  end_position_marked = FALSE;
  for ( ps = patstring ; ; ps++ ) {
    pc = *ps;
    switch(pc){
    case PT_END:
      goto success;
    case PT_MATCH_ANY: {
      COStream outbuf;
      int limit;
      Goal use_goal;
      if ( next_arg == NULL ) /* matching only up to first argument */
	goto success;
      assert( next_arg[0] == NULL );
      next_arg[1] = NULL;
      outbuf = make_buffer_output_stream();
      use_goal = (ps[1] == PT_END) ? goal : NULL;
      for ( limit = MAX_ARG_LEN ; limit > 0 ; limit-- ) {
      	ic = cis_peek(in);
	if ( ic == EOF ) {
	  if ( ps[1] == PT_END || ps[1] == PT_AUX ) {
	    *next_arg++ = convert_output_to_input(outbuf);
	    goto continue_match;
	  }
	  else break;
	}
	if ( use_goal != NULL ) {
	  assert( ! use_goal->options & MatchSwallow );
	  if ( try_pattern( in, use_goal->template, NULL, use_goal->args,
			    (use_goal->options | MatchArgDelim), goal ) &&
	       use_goal->template[0] != PT_END ) {
	    next_arg[0] = convert_output_to_input(outbuf);
	    goto success;
	  }
	}
	else
	if ( try_pattern( in, ps+1, next_arg+1, all_args,
			  (local_options | (MatchArgDelim|MatchSwallow)),
			  goal ) &&
	     /* (the simpler test is being done second only because it is
		 the much less common case.) */
	     ps[1] != PT_END ) {
	  next_arg[0] = convert_output_to_input(outbuf);
	  goto success;
	}
	if ( ic == '\n' && (local_options & MatchLine) ) {
	  TRACE_FAILURE( "*" );
	  goto failed_any;
	}
	else {
	  int xc;
	  xc = getch_marked(&marker);
#ifndef NDEBUG
	  if ( xc != ic )
	    input_error(in, EXS_INPUT, __FILE__
	        " line %d: ic = '%c', xc = '%c'\n",
	         __LINE__, ic, xc );
#endif
	  cos_putch(outbuf, (char)xc );
	}
      }
    failed_any:
      cos_close(outbuf);
      goto failure;
    } /* end PT_MATCH_ANY */

    case PT_MATCH_ONE: {  /* "?" argument (general case) */
      char str[2];
      if ( next_arg == NULL ) /* matching only up to first argument */
	goto success;
      ic = getch_marked(&marker);
      if ( ic == EOF || ( ic == '\n' && (local_options & MatchLine) ) )
	goto failure;
      str[0] = ic;
      str[1] = '\0';
      *next_arg++ = make_string_input_stream(str, 1, TRUE);
      *next_arg = NULL;
      break;
    } /* end PT_MATCH_ONE */

    case PT_ONE_OPT: {  /* "?" argument (optimized special case) */
      assert( next_arg != NULL );
      arg_char = cis_getch(in);
      if ( arg_char == EOF )
	goto failure;
      break;
    } /* end PT_ONE_OPT */

    case PT_RECUR: { /* argument recursively translated */
      COStream outbuf;
      int domain;
      struct goal_state goal_info;
      Goal new_goal;
      if ( next_arg == NULL ) /* matching only up to first argument */
	goto success;
      assert( next_arg[0] == NULL );
      outbuf = make_buffer_output_stream();
      domain = *++ps - 1;
      if ( !marker.marked ) {
	cis_mark(in,&marker.start);
	marker.marked = TRUE;
      }
#ifdef TRACE
      if ( trace_switch ) {
	++trace_indent;
	if ( trace_indent < trace_enter.level ) {
	  trace_enter.level = trace_indent;
	  trace_enter.line = cis_line(in);
	  trace_enter.column = cis_column(in);
	  trace_enter.ch = cis_peek(in);
	  trace_enter.domain = domain;
	}
      }
#endif
      if ( ps[1] == PT_END )
	new_goal = goal;
      else {
	goal_info.template = ps+1;
	goal_info.args = all_args;
	goal_info.options = local_options & ~ MatchSwallow;
	new_goal = &goal_info;
      }
      if ( translate ( in, domains[domain], outbuf, new_goal ) ) {
	*next_arg++ = convert_output_to_input( outbuf );
	*next_arg = NULL;
#ifdef TRACE
	if ( trace_switch ) {
	  trace ( &marker, OK, "Matched <%s> as \"%.60s\"\n",
		   domains[domain]->name, cis_whole_string(next_arg[-1]) );
	  --trace_indent;
	}
#endif
	}
      else {
#ifdef TRACE
	if ( trace_switch ) {
	  trace( &marker, FAIL, "<%s>", domains[domain]->name );
	  --trace_indent;
	}
#endif
	cos_close(outbuf);
	goto failure;
      }
      break;
    } /* end PT_RECUR */

    case PT_SPECIAL_ARG: {
      COStream outbuf;
      int kind;
      int parms;
      int num_wanted;
      int num_found;
      boolean optional;
      boolean inverse;
      unsigned goal_char, alt_goal_char;
      struct goal_state terminator_data;
      Goal terminator;

      num_found = 0;
      parms = *++ps;
      optional = parms & 0x40;
      inverse = parms & 0x80;
      kind = ('A'-1) + (parms & 0x3F);
      num_wanted = (*++ps) - 1;
      goal_char = get_goal_char(ps+1);
      if ( goal_char == ENDOP && goal != NULL ) {
	goal_char = get_goal_char(goal->template);
	terminator = goal;
      }
      else {
	terminator_data.template = ps+1;
	terminator_data.args = all_args;
	terminator_data.options = local_options;
	terminator = &terminator_data;
      }
      if ( num_wanted >= 0xFE )
	num_wanted = -1;
      else if ( !optional )
	goal_char = ENDOP;
      alt_goal_char = goal_char;
      if ( (terminator->options & MatchNoCase) && !ISOP(goal_char) ) {
	goal_char = tolower(goal_char);
	alt_goal_char = toupper(goal_char);
      }
      outbuf = next_arg != NULL ? make_buffer_output_stream() : NULL;
      for ( ; ; ) {
	boolean ok;
	ic = cis_peek(in);
	switch(kind) {
	case 'O': if ( ic > '7' ) {			/* octal digits */
		    ok = FALSE;
		    break;
		}
	  /* else fall-through */
	case 'D': ok = isdigit(ic); break;		/* digits */
	case 'W': if ( num_found > 0 &&			/* word */
		       ( ic == '\'' || ic == '-' ) ) {
		    ok = TRUE;
		    break;
		}
	  /* else fall-through */
	case 'L': ok = isalpha(ic); break;		/* letters */
	case 'J': ok = islower(ic); break;		/* lower case */
	case 'K': ok = isupper(ic); break;		/* upper case */
	case 'A': ok = isalnum(ic); break;		/* alphanumerics */
	case 'I': ok = isident(ic); break;		/* identifier */
	case 'G': ok = isgraph(ic); break;		/* graphic char */
	case 'C': ok = iscntrl(ic); break;		/* control char */
	case 'F': ok = isalnum(ic) ||			/* file pathname */
		( strchr(filechars,ic)!=NULL && ic!=0 );
		break;
	case 'S': ok = isspace(ic); break;		/* white space */
	case 'X': ok = isxdigit(ic); break;		/* hex digits */
	case 'N': ok = isdigit(ic) || 			/* number */
			( ic=='.' && cos_prevch(outbuf) != '.' ) ||
			( num_found == 0 && ( ic=='-' || ic=='+' ) );
		if ( !ok && num_found == 1 &&
		     strchr("+-.", cos_prevch(outbuf)) != NULL ) {
		  cos_close(outbuf);
		  goto failure;
		  }
		break;
	case 'Y': ok = ispunct(ic) &&			/* punctuation */
			 !( isspace(ic) | isident(ic) );
		break;
	case 'P': ok = isprint(ic); break;		/* printing char */
	case 'T':
	case 'V': ok = (isprint(ic) | isspace(ic));	/* valid text */
		break;
	case 'U': ok = ic != EOF;			/* anything */
		break;
	default:
		fprintf(stderr, "Undefined arg type: <%c>\n", kind);
		ok = FALSE;
	}
	if ( inverse && ic != EOF )
	  ok = !ok;
	if(ok) {
	  if ( ic == '\n' && (local_options & MatchLine) )
	    break;
	  if ( next_arg == NULL ) /* matching only up to first argument */
	    goto success;
	  if ( ( ((unsigned)ic) == goal_char ||
		 ((unsigned)ic) == alt_goal_char ||
	         ( goal_char == UPOP(PT_SPACE) && isspace(ic) &&
		   ( optional || isspace(cos_prevch(outbuf)) ) ) ) &&
	       try_pattern( in, terminator->template, NULL, terminator->args,
			    (terminator->options & MatchNoCase), goal ) )
	    /* would be valid constituent except that it appears in the
	       template as a terminator. */
	    break;
	  num_found++;
	  if ( num_found > num_wanted && num_wanted >= 0 ) {
	    if ( num_wanted == 0 )
	      cos_putch(outbuf, ic);
	    break;
	  }
	  cos_putch(outbuf, getch_marked(&marker));
	} /* end ok */
	else if ( ignore_whitespace && isspace(ic) && num_found == 0 &&
		  !( ic == '\n' && (local_options & MatchLine) ) &&
		  ( kind=='Y' || kind=='C' || !isident(cis_prevch(in)) ) &&
		  marker.marked /* not beginning of template */ ) {
	  (void)getch_marked(&marker);
	  continue;
	}
	else break;
      } /* end for */
      if ( ( num_found < num_wanted ||  /* not enough characters found */
	     num_found == 0		/* no valid characters found */
	   ) && !optional )
       {
#ifdef TRACE
	if ( trace_switch )
	  trace( &marker, FAIL, "<%s%c>", (inverse? "-" : ""), kind );
#endif
	cos_close(outbuf);
      	goto failure;
      }
      else {
	if ( next_arg == NULL )
	  /* matching only up to first argument; here for empty optional arg */
	  goto success;
	else {
	  *next_arg++ = convert_output_to_input(outbuf);
	  *next_arg = NULL;
#ifdef TRACE
	  if ( trace_switch )
	    trace ( &marker, OK, "Matched <%s%c> as \"%.60s\"\n",
		    (inverse? "-" : ""), (optional? tolower(kind) : kind),
		    cis_whole_string(next_arg[-1]) );
#endif
	}
	break;
      }
    } /* end PT_SPECIAL_ARG */

    case PT_REGEXP: {
       CIStream value;
       if ( next_arg == NULL ) /* matching only up to first argument */
	 goto success;
       value = match_regexp ( in, *++ps -1 );
       if ( value == NULL ) {
	 TRACE_FAILURE( "regular expression" );
	 goto failure;
       }
       *next_arg++ = value;
       *next_arg = NULL;
       break;
    }

    case PT_PUT_ARG: { /* match against value of previous argument */
	CIStream arg;
	if ( all_args == NULL ) /* matching only up to first argument */
	  goto success;
	arg = all_args[ (*++ps) - 1 ];
	if ( arg == NULL ) {
	  if ( next_arg == NULL )
	    goto success;
	  else goto failure;
	}
	cis_rewind(arg);
	if ( match_to_stream(&marker, arg, local_options) )
	  break;
	else goto failure;
    }

    case PT_VAR1: {
	char vname[2];
	const char* value;
	size_t length;
	CIStream arg;
	boolean ok;
	vname[0] = *++ps;
	vname[1] = '\0';
    	value = get_var(vname, FALSE, &length);
	arg = make_string_input_stream(value, length, FALSE);
	ok = match_to_stream(&marker, arg, local_options);
	cis_close(arg);
	if ( ok )
	  break;
	else goto failure;
      }

    case PT_SPACE: /* at least one space required */
      if ( !isspace( cis_peek(in) ) ) {
	if ( isspace( cis_prevch(in) ) &&
	     ( marker.marked || next_arg == all_args ) )
	  break;
	else {
	  TRACE_FAILURE( "\\S" );
	  goto failure;
	}
      }
      /* and fall through for optional additional space */
    case PT_SKIP_WHITE_SPACE: {  /* optional white space */
      int x;
      for ( ; ; ) {
	x = cis_peek(in);
	if ( x == EOF || !isspace(x) )
	  break;
	if ( x == '\n' &&
	     ( (local_options & MatchLine) || ps[1] == PT_LINE ||
      	       ps[1] == '\n' ||
	       ( ps > patstring &&
	         ( ps[-1] == PT_LINE || ps[-1] == '\n' ) ) ) )
	  break;
	(void)getch_marked(&marker);
      }
      break;
    }
    case PT_WORD_DELIM: { /* word delimiter */
      if ( !isalnum( cis_prevch(in) ) )
	break;
      if ( !isalnum(cis_peek(in)) )
	break;
      TRACE_FAILURE( "\\X" );
      goto failure;
    }
    case PT_ID_DELIM: { /* identifier delimiter */
      if ( !isident( cis_prevch(in) ) )
	break;
      if ( !isident(cis_peek(in)) )
	break;
      TRACE_FAILURE( "\\I" );
      goto failure;
    }
#if 0 	/* changed my mind */
    case PT_ARG_DELIM: { /* command line argument delimiter */
      while ( cis_peek(in) == Arg_Delim )
	(void)getch_marked(&marker);
      ic = cis_prevch(in);
      if ( ic == Arg_Delim || ic == EOF || cis_peek(in) == EOF )
	break;
      else goto failure;
    }
#endif
   case PT_LINE: { /* at beginning or end of line */
      int np;
      ic = cis_prevch(in);
      if ( ic == '\n' || ic == EOF )
	break;
      np = ps[1];
      if ( np != PT_SKIP_WHITE_SPACE && np != PT_SPACE &&
   	   np != PT_LINE && !isspace(np) ) {
	ic = cis_peek(in);
	if ( ic == EOF )
	  break;
	if ( ic == '\n' ) {
	  if ( !is_operator(np) ) /* accept newline if not end of template */
	    (void)getch_marked(&marker);
	  break;
	}
      }
      TRACE_FAILURE( "\\N" );
      goto failure;
    }

   case PT_AUX: {
     unsigned char ec;
     ec = *++ps;
     switch(ec) {
#if 0	/* superseded by separate beginning and end operators */
       case PTX_MATCH_EOF: { /* at beginning or end of file */
	if ( cis_is_file(in) ) {
	  if ( cis_prevch(in) == EOF )
	    break;
	  if ( cis_peek(in) == EOF )
	    break;
	}
	goto failure;
      }
#endif
      case PTX_ONE_LINE:
	local_options |= MatchLine;
	break;
      case PTX_NO_CASE:
	local_options |= MatchNoCase;
	break;
      case PTX_BEGIN_FILE:
	if ( !cis_is_file(in) )
	  goto failure;
	/* else fall-through */
      case PTX_INIT: /* beginning of input data */
	/* when this is at the beginning of a template, we wouldn't have
	   gotten here unless it was already known to be true. */
	if ( ps-1 == patstring || cis_prevch(in) == EOF )
	  break;
	else goto failure;
      case PTX_END_FILE:
	if ( !cis_is_file(in) )
	  goto failure;
	/* else fall-through */
      case PTX_FINAL: /* end of input data */
	/* when this is at the beginning of a template, we wouldn't have
	   gotten here unless it was already known to be true. */
	if ( ps-1 == patstring || cis_peek(in) == EOF )
	  break;
	else if ( goal != NULL && ps[0] == PTX_FINAL &&
		  try_pattern( in, goal->template, NULL, goal->args,
			       (goal->options & ~MatchSwallow), NULL) )
	  break;
    	else goto failure;
      case PTX_POSITION: /* leave input stream here after match */
	if ( ps[1] != PT_END ) {
	  if ( !marker.marked ) {
	    cis_mark(in,&marker.start);
	    marker.marked = TRUE;
	  }
	  cis_mark(in, &end_position);
	  end_position_marked = TRUE;
	}
	break;
      case PTX_NO_GOAL:
	if ( !(local_options & MatchSwallow) ) {
	  /* when doing look-ahead for argument delimiter */
	  assert( next_arg == NULL );
	  if ( marker.marked ) /* some text matched, consider it sufficient. */
	    goto success;
	  else goto failure; /* don't terminate the argument yet */
	}
	break;
      case PTX_JOIN:
	if ( ignore_whitespace ) {
	  ic = cis_peek(in);
	  if ( isspace(ic) && ic != ps[1] &&
	       ( !is_operator(ps[1]) || ps[1] == PT_SPECIAL_ARG ) ) {
	    TRACE_FAILURE( "\\J" );
	    goto failure; /* prevent the space from being ignored */
	  }
	}
     	break;
#ifndef NDEBUG
      default:
	input_error( in, EXS_FAIL, "Undefined aux op in template: %d\n", ec);
	break;
#endif
     } /* end switch ec */
     break;
   } /* end PT_AUX */

    case PT_QUOTE: /* take next character literally */
      pc = *++ps;
      /* and fall-through */
    default: {
again:
      ic = cis_peek(in);
      if ( ic != pc &&
	   ( !(local_options & MatchNoCase) || toupper(ic) != toupper(pc) ) ){
	if ( ignore_whitespace && isspace(ic) &&
	     ( !isident(pc) || !isident(cis_prevch(in)) ) &&
	     marker.marked /* don't skip before beginning of template */ &&
	     !( ic == '\n' && (local_options & MatchLine) ) ) {
	  (void)getch_marked(&marker);
	  goto again;
	}
#ifdef TRACE
	if ( trace_switch &&
	     (local_options & (MatchArgDelim|MatchSwallow))==MatchSwallow )
	  trace( &marker, FAIL, show_char(pc) );
#endif
	goto failure;
      }
      else (void)getch_marked(&marker);
      } /* end default */
    } /* end switch pc */
  continue_match: ;
  } /* end for pattern string */
 failure:
  match = FALSE;
  goto quit;
 success:
  match = TRUE;
  if ( !marker.marked && ( options & MatchSwallow ) &&
       next_arg == all_args &&
       translation_status == Translate_Complete && 
       patstring[0] != PT_ONE_OPT && patstring[0] != PT_MATCH_ANY )
    /* matched without advancing the input stream */
    translation_status = Translate_Continue;
 quit:
  if ( marker.marked ) {
    if ( match && ( options & MatchSwallow ) ) {
      if ( end_position_marked ) {
	if ( end_position.position == marker.start.position &&
	     next_arg == all_args &&
	     translation_status == Translate_Complete )
	  /* matched without advancing the input stream */
	  translation_status = Translate_Continue;
	cis_restore(in,&end_position);
      }
      /* else leave the input stream at the end of the matched text */
      cis_release(in,&marker.start);
    }
    else {
      if ( end_position_marked )
	cis_release(in,&end_position);
      cis_restore(in,&marker.start); /* restore input to previous position */
    }
  }
  return match;
}

static int global_options;
Pattern current_rule = NULL;

static boolean
try_match( CIStream in, Pattern pat, COStream out, Goal goal )
{
  boolean result;
  varp varmark;
  CIStream args[MAX_ARG_NUM+1];

  args[0] = NULL;
  varmark = first_bound_var;
  if ( ! try_pattern( in, pat->pattern, &args[0], &args[0],
		      global_options, goal ) ) {
    if ( varmark != first_bound_var )
      prune_vars(varmark); /* undo variables bound within failed match */
    result = FALSE;
  }
  else {
    const unsigned char* as;
    enum Translation_Status save = translation_status;
    translation_status = Translate_Complete;
    /* pattern matches, so perform the specified action. */
    current_rule = pat;
    as = do_action( pat->action, args, out );
    assert( pat->action == NULL || *as == PT_END );
    result = TRUE;
    if ( translation_status <= Translate_Continue )
      translation_status = save;
  }
  {
    CIStream * argp;
    for ( argp = &args[0] ; *argp != NULL ; argp++ )
      cis_close(*argp); /* de-allocate argument buffer */
  }
  return result;
}

static boolean
try_list( CIStream in, Patterns p, COStream out, Goal goal ) {
  Pattern pat;
  boolean result = FALSE;
  for ( pat = p->head ; pat != NULL ; pat = pat->next ) {
    if ( translation_status > Translate_Continue )
      return translation_status != Translate_Failed;
    if ( try_match( in, pat, out, goal ) ) {
      if ( translation_status != Translate_Continue )
	return TRUE;
      result = TRUE;
    }
  }
  return result;
}

static boolean
try_patterns( int ch, CIStream in, MarkBuf* start, Patterns p, COStream out,
	      Goal goal ) {
    MarkBuf mark;
    if ( p->dispatch != NULL ) {
      Patterns sub;
      sub = p->dispatch[ dispatch_index(ch) ];
      if ( sub != NULL ) {
	if ( sub->dispatch != NULL ) {
	  int xc;
	  if ( start == NULL ) {
	    start = &mark;
	    cis_mark(in, start);
	  }
	  xc = cis_getch(in);
	  assert ( ch == xc );
	  if ( try_patterns( cis_peek(in), in, start, sub, out, goal ) )
	    return TRUE;
	  else start = NULL;
	}
	else {
	  if ( start != NULL ) {
	    cis_restore(in, start);
	    start = NULL;
	  }
	  if ( try_list( in, sub, out, goal ) ) {
	    if ( translation_status == Translate_Continue )
	      translation_status = Translate_Complete;
	    else
	      return TRUE;
	  }
	}
      }
    } /* end p->dispatch */
    if ( start != NULL ) {
      assert ( start != &mark );
      cis_restore(in, start);
    }
    if ( try_list( in, p, out, goal ) ) {
      if ( translation_status == Translate_Continue )
	translation_status = Translate_Complete;
      else
	return TRUE;
    }
    return FALSE;
}

static int domains_checked = 1;

boolean translate ( CIStream in, Domain domainpt, COStream out,
		    Goal goal_info ) {
  int ch, ch2;
  const unsigned char* goal;
  unsigned goal_char, alt_goal_char;
  enum Translation_Status save_fail;
  CIStream save_input;
  boolean no_match = TRUE;
  boolean discard = FALSE;
  boolean beginning = TRUE;

  save_input = input_stream;
  if ( save_input == NULL || cis_pathname(in) != NULL ||
       ( cis_is_file(in) && ! cis_is_file(save_input) ) )
    input_stream = in;
  for ( ; domains_checked < ndomains ; domains_checked++ ) {
    Domain dp = domains[domains_checked];
    if ( dp->patterns.head == NULL && dp->patterns.dispatch == NULL &&
  	 dp->name[0] != '\0' && dp->init_and_final_patterns == NULL )
      fprintf(stderr, "Domain name \"%s\" is referenced but not defined.\n",
		dp->name);
  }
  global_options = MatchSwallow;
  if ( line_mode )
    global_options |= MatchLine;
  if ( case_insensitive )
    global_options |= MatchNoCase;
  if ( goal_info == NULL ) {
    goal = NULL;
    goal_char = ENDOP;
  }
  else {
    goal = goal_info->template;
    goal_char = get_goal_char(goal);
    alt_goal_char = goal_char;
    if ( (goal_info->options & MatchNoCase) && !ISOP(goal_char) ) {
      goal_char = tolower(goal_char);
      alt_goal_char = toupper(goal_char);
    }
  }
  save_fail = translation_status;
  translation_status = Translate_Complete;
  if ( discard_unmatched && domainpt->name[0] == '\0' )
    discard = TRUE;
  {	/* do any initialization actions */
  Pattern pat;
  for ( pat = domainpt->init_and_final_patterns ; pat != NULL ; pat = pat->next ) {
    const unsigned char* ps = pat->pattern;
    assert( ps[0] == PT_AUX );
    if ( ps[1] == PTX_INIT ||
	 ( ps[1] == PTX_BEGIN_FILE && cis_prevch(in) == EOF ) )
      if ( try_match( in, pat, out, goal_info ) ) {
	no_match = FALSE;
	if ( translation_status == Translate_Continue )
	  translation_status = Translate_Complete;
	else if ( translation_status == Translate_Complete )
	  break;
	else {
	  boolean result = translation_status != Translate_Failed;
	  translation_status = save_fail;
	  input_stream = save_input;
	  return result;
	}
      }
  }
  }

  for ( ; translation_status == Translate_Complete ; ) {
    Domain idp;
    ch = cis_peek(in);
    if ( ch == EOF )
      break;  /* done */
    else if ( goal_char != ENDOP ) {
      if ( ((unsigned)ch) == goal_char || ((unsigned)ch) == alt_goal_char ||
    	   (goal_char == UPOP(PT_SPACE) && isspace(ch) &&
	    !( beginning && ignore_whitespace ) ) ) {
	if ( ( goal_char == goal[0] &&
	       (goal[1] == PT_END || goal[1] == PT_MATCH_ANY ||
		goal[1] == PT_MATCH_ONE || goal[1] == PT_RECUR ) )
	     /* short-cut for single-character delimiter */
	     || /* use general pattern matching */
	     try_pattern( in, goal, NULL, goal_info->args,
			  goal_info->options, NULL ) ) {
	  Pattern tailpat;
	    /* For a domain whose default rule is ``=@fail'',
	       an argument should not match an empty string just
	       because the terminator is found before starting. */
	  if( !( beginning && NULL != ( tailpat = domainpt->patterns.tail ) &&
		 tailpat->pattern[0] == PT_END && /* empty template */
		 tailpat->action[0] == PT_OP &&
		 tailpat->action[1] == OP_FAIL ) )
	    break;
	}
      }
      beginning = FALSE;
    }
    for ( idp = domainpt ; idp != NULL ; idp = idp->inherits )
      if ( try_patterns( ch, in, NULL, &idp->patterns, out, goal_info ) ) {
	/* match found */
	if ( translation_status != Translate_Complete ) {
	  boolean result = translation_status != Translate_Failed;
	  translation_status = save_fail;
	  input_stream = save_input;
	  return result;
	}
	else goto next_char;
      }
    ch2 = cis_getch(in);
#ifndef NDEBUG
    if( ch2 != ch )
      input_error(in, EXS_INPUT, __FILE__
      " line %d : ch2 = '%c', ch = '%c'\n", __LINE__, ch2, ch);
#endif
    if ( !discard )
      cos_putch(out, (char)ch2);
next_char: ;
  }  /* end for */

  {		/* do any finalization actions */
  Pattern pat;
  for ( pat = domainpt->init_and_final_patterns ; pat != NULL ; pat = pat->next ) {
    const unsigned char* ps = pat->pattern;
    if ( ps[1] == PTX_FINAL ||
	 ( ps[1] == PTX_END_FILE && cis_peek(in) == EOF) )
      if ( try_match( in, pat, out, goal_info ) )
	no_match = FALSE;
  }
  if ( no_match && cis_prevch(in) == EOF && cis_peek(in) == EOF ) {
    for ( pat = domainpt->patterns.head ; pat != NULL ; pat = pat->next ) {
      if ( pat->pattern[0] == PT_END ) {
	/* empty template matches empty input stream */
	current_rule = pat;
	do_action( pat->action, NULL, out );
	break;
      }
    }
  }
  }
  translation_status = save_fail;
  input_stream = save_input;
  return TRUE;  /* indicate success */
}
