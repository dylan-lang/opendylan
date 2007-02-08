
/* read pattern definitions */

/* $Id: read.c,v 1.20 2004/09/18 22:57:06 dngray Exp $ */

/*********************************************************************
  This file is part of "gema", the general-purpose macro translator,
  written by David N. Gray <dgray@acm.org> in 1994 and 1995.
  You may do whatever you like with this, so long as you retain
  an acknowledgment of the original source.
 *********************************************************************/

/* 
 * $Log: read.c,v $
 * Revision 1.20  2004/09/18 22:57:06  dngray
 * Allow MAX_DOMAINS to be larger than 255
 * (merged changes contributed by Alex Karahalios).
 *
 * Revision 1.19  2001/12/15  20:22:44  gray
 * Modify use of hex character constants to work-around a compiler bug on
 * DEC Alpha OSF1.  Clean up compiler warnings..
 *
 * Revision 1.18  2001/09/30  23:10:20  gray
 * Fix uninitialized variable in skip_comment.
 *
 * Revision 1.17  1996/04/08  05:29:56  gray
 * Fixed initialization of `fnnargs' so that ${varname} always works even when
 * @var has never been used.  Fixed interaction of comment and continuation
 * lines.  If the first line of a pattern file begins with "#!", ignore that
 * line so that pattern files can be made directly executable.  When '=' is
 * missing, discard incomplete rule to avoid other errors.  Warn when template
 * begins with recursive argument in same domain (will just overflow stack).
 *
 * Revision 1.16  1995/08/27  21:03:46  gray
 * Fix handling of space between identifiers with "-t" or "-w".
 * Fix to not be prevented from using dispatch table when template begins with
 * "\I" or when upper/lower case difference for key.
 *
 * Revision 1.15  1995/08/20  05:34:40  gray
 * Treat variable in template as literal for argument terminator.
 * Fix to not free patterns installed in multiple places.
 * Fix bug in dispatch table setup.
 *
 * Revision 1.14  1995/08/13  05:35:49  gray
 * New macro `char_kind' to ensure uniform handling of EOF -- fixes crash on
 * EOF at end of inherited domain rule.
 *
 * Revision 1.13  1995/08/06  02:26:47  gray
 * Fix bug on "\A" (regression in previous version).
 * Add "<J>" (match lower case) and "<K>" (match upper case).
 * Support @set-syntax{M;\'} for quoting a string.
 *
 * Revision 1.12  1995/07/27  05:31:34  gray
 * Fix a couple of problems with spaces with "-w".
 *
 * Revision 1.11  1995/07/27  03:00:45  gray
 * Fix handling of "\B" followed by an argument instead of a literal.
 *
 * Revision 1.10 1995/06/12 03:05:17 gray
 * Fixed bug in handling of rule domain encloses in angle brackets
 * ("<...>:...").  Add new functions @get-switch and @out-column.
 *
 * Revision 1.9 1995/05/30 05:06:21 gray
 * Fix occasional spurious warning about escape sequence in action
 * continued on second line.
 *
 * Revision 1.8 1995/05/08 03:13:46 gray
 * Add @expand-wild
 */

#if defined(_QC) || defined(_MSC_VER) /* Microsoft C or Quick C */
#pragma check_stack(off)
#endif

#include "pattern.h"
#include "util.h"
#include "patimp.h"
#include <ctype.h>  /* for isalnum */
#include <string.h>
#include <assert.h>
#include "reg-expr.h"
#include "main.h"  /* for EXS_SYNTAX */
#include "var.h"	/* for get_var */

boolean line_mode = FALSE;
boolean token_mode = FALSE;

boolean discard_unmatched = FALSE;

boolean debug_switch = FALSE;

enum char_kinds {
  PI_LITERAL,	/* character represents itself */
  PI_LIT_CTRL,	/* control character that represents itself but has
		  the same code as a pattern operator */
  PI_CR,	/* Carriage Return */
  PI_ARG,	/* template argument -- wild card match */
  PI_1ARG,	/* template argument -- match any one character */
  PI_RARG,	/* template argument with recursive translation */
  PI_SEP,	/* separates template and action */
  PI_PUT,	/* outputs an argument or variable in an action */
  PI_QUOTE,	/* take next character literally */
  PI_ESC,	/* escape character */
  PI_CTRL,	/* combines with next character for ASCII control */
  PI_SPACE,	/* matches one or more white space characters */
  PI_BEGIN_ARG,	/* begin argument list */
  PI_ARG_SEP,	/* separates arguments of action operators */
  PI_END_ARG,	/* end argument list */
  PI_END,	/* end of pattern */
  PI_DOMAIN,	/* terminates domain name preceding pattern */
  PI_BEGIN_DOMAIN_ARG,
  PI_END_DOMAIN_ARG,
  PI_END_REGEXP,
  PI_BEGIN_REGEXP,
  PI_CHAR_OP, 	/* makes following character special */
  PI_OP,	/* introduces named command */
  PI_COMMENT,	/* rest of line is a comment */
  PI_ABBREV_DOMAIN, /* single-character recursive argument domain */
  PI_IGNORE,	/* character that is completely ignored */
  PI_IGNORED_SPACE, /* ignored unless needed as delimiter */
  PI_QUOTE_STRING, /* take characters literally until matching quote */
  PI_EOF,	/* end of file */
  Num_Char_Kinds	/* must be last element of enumeration */
};

#define NUMCHAR 256
static unsigned char default_syntax_chars[Num_Char_Kinds+1] =
		 ".\1\r*?#=$\\\\^ {;};:<>//@@!\0\0\0\0\0";
static unsigned char syntax_chars[Num_Char_Kinds+1];

static char char_table[NUMCHAR+1] = {
#if EOF == (-1)
 /* -1: */ PI_EOF,
#endif
 /* 00: */ PI_LIT_CTRL,
 /* 01: */ PI_LIT_CTRL,
 /* 02: */ PI_LIT_CTRL,
 /* 03: */ PI_LIT_CTRL,
 /* 04: */ PI_LIT_CTRL,
 /* 05: */ PI_LIT_CTRL,
 /* 06: */ PI_LIT_CTRL,
 /* 07: */ PI_LIT_CTRL,
 /* 08: */ PI_LITERAL, /* ASCII BS */
 /* 09: */ PI_LITERAL, /* ASCII HT */
 /* 0A: */ PI_LITERAL, /* ASCII LF */
 /* 0B: */ PI_LITERAL,  /* VT */
 /* 0C: */ PI_LITERAL,  /* FF */
#if defined(MSDOS) || '\r' == '\n'
 /* 0D: */ PI_LITERAL,  /* CR */
#else
 /* 0D: */ PI_CR,  	/* CR */
#endif
 /* 0E: */ PI_LIT_CTRL,
 /* 0F: */ PI_LIT_CTRL,
 /* 10: */ PI_LIT_CTRL,
 /* 11: */ PI_LIT_CTRL,
 /* 12: */ PI_LIT_CTRL,
 /* 13: */ PI_LIT_CTRL,
 /* 14: */ PI_LIT_CTRL,
 /* 15: */ PI_LITERAL,  /* EBCDIC NL */
 /* 16: */ PI_LIT_CTRL,
 /* 17: */ PI_LIT_CTRL,
 /* 18: */ PI_LIT_CTRL,
 /* 19: */ PI_LIT_CTRL,
 /* 1A: */ PI_LIT_CTRL,
 /* 1B: */ PI_LITERAL,  /* ASCII ESC */
 /* 1C: */ PI_LIT_CTRL,
 /* 1D: */ PI_LIT_CTRL,
 /* 1E: */ PI_LIT_CTRL,
 /* 1F: */ PI_LIT_CTRL,
 /* 20: */ PI_LITERAL };

#if EOF == (-1)
#define char_kind(ch) ((enum char_kinds)((char_table+1)[ch]))
#define set_char_kind(ch,k) char_table[(ch)+1] = (k)
#else
/* I don't know of any implementation where EOF is not -1, but the ANSI
   standard does not require it. */
static enum char_kinds
char_kind(int ch){
  return ( ch == EOF ? PI_EOF : (enum char_kinds) char_table[ch] );
}
#define set_char_kind(ch,k) char_table[ch] = (k)
#endif

boolean is_operator(int x) {
  return char_kind(x) == PI_LIT_CTRL;
}

static enum char_kinds
default_char_kind( int pc ) {
  const char* x;
      x = strrchr((const char*)default_syntax_chars,(char)pc);
      if ( x == NULL )
	return PI_LITERAL;
      else
	return (enum char_kinds)(x - (const char*)default_syntax_chars);
}

boolean set_syntax( int type, const char* char_set ) {
  enum char_kinds k;
  const char* s;
  switch(toupper(type)) {
    case 'L': k = PI_LITERAL; break;
    case 'Q': k = PI_QUOTE; break;
    case 'M': k = PI_QUOTE_STRING; break;
    case 'E': k = PI_ESC; break;
    case 'C': k = PI_COMMENT; break;
    case 'A': k = PI_ARG_SEP; break;
    case 'T': k = PI_END; break;
    case 'F': k = PI_OP; break;
    case 'I': k = PI_IGNORE; break;
    case 'S': k = PI_IGNORED_SPACE; break;
    case 'D': k = PI_ABBREV_DOMAIN; break;
    case 'K': k = PI_CHAR_OP; break;
    default:
       k = char_kind(type);
       if ( k <= PI_LIT_CTRL ) {
	 k = default_char_kind(type);
	   if ( k <= PI_LIT_CTRL )
	     return FALSE;
       }
       break;
  } /* end switch */
  for ( s = char_set ; s[0] != '\0' ; s++ ) {
    unsigned int ch = *(const unsigned char*)s;
    unsigned char* scp = &syntax_chars[ char_kind(ch) ];
    if ( *scp == ch )
      *scp = '\0';
    set_char_kind(ch,k);
    syntax_chars[k] = ch;
  }
  return TRUE;
}

#if 0 /* old way */
/* special characters in external pattern definitions: */
#define PI_ARG '*'      /* template argument -- wild card match */
#define PI_1ARG '?'     /* template argument -- match any one character */
#define PI_RARG '#'     /* template argument with recursive translation */
#define PI_SEP '='      /* separates template and action */
#define PI_PUT '$'      /* outputs an argument or variable in an action */
#define PI_QUOTE '\\'   /* take next character literally */
#define PI_ESC '\\'     /* escape character */
#define PI_CTRL '^'     /* combines with next character for ASCII control */
#define PI_SPACE ' '    /* matches one or more white space characters */
#define PI_END '\n'     /* end of pattern */
#define PI_ALTEND ';'   /* end of pattern */
#define PI_BEGIN_ARG '{' /* begin argument list */
#define PI_ARG_SEP ';'  /* separates arguments of action operators */
#define PI_END_ARG '}'  /* end argument list */
#define PI_DOMAIN ':'   /* terminates domain name preceding pattern */
#define PI_BEGIN_DOMAIN_ARG '<'
#define PI_END_DOMAIN_ARG '>'
#define PI_BEGIN_REGEXP '/'
#define PI_END_REGEXP '/'
#define PI_OP '@'       /* introduces named command */
#define PI_COMMENT '!'  /* rest of line is a comment */
#endif

int ndomains = 0;
#if MAX_DOMAINS > (1<<14)
  #error "MAX_DOMAINS is too large; must fit in 14 bits."
#endif
Domain domains[MAX_DOMAINS] = { NULL };

char* trim_name( unsigned char* x ) {
  unsigned char* s;
  unsigned char* end;
  s = x;
  while ( isspace(s[0]) || s[0] == PT_SPACE || s[0] == PT_ID_DELIM )
    s++;
  if ( s[0] != PT_RECUR ) {
  end = s + strlen((const char*)s);
  while ( end > s &&
	( isspace(end[-1]) || end[-1] == PT_SPACE || end[-1] == PT_ID_DELIM ) )
    end--;
  end[0] = '\0';
  }
  return (char*)s;
}

static int find_domain( const char* name ) {
  int i;
  for ( i = ndomains ; i > 0 ; ) {
    i--;
    if ( case_insensitive ) {
      if( stricmp(name, domain_name(i)) == 0 )
	return i;
    } else if ( strcmp(name, domain_name(i)) == 0 )
	return i;
  }
  if ( ndomains >= MAX_DOMAINS ) {
    fprintf(stderr,"More than %d domain names; aborting.\n", MAX_DOMAINS);
    exit((int)EXS_SYNTAX);
    return -1; /* just to avoid warning from SGI compiler */
  }
  else {
    Patterns p;
    Domain dp;
    i = ndomains++;
    dp = (Domain)allocate(sizeof(struct domain_struct), MemoryPatterns);
    p = &dp->patterns;
    domains[i] = dp;
    dp->name = str_dup(name);
    dp->inherits = NULL;
    p->head = NULL;
    p->tail = NULL;
    p->dispatch = NULL;
    dp->init_and_final_patterns = NULL;
    return i;
  }
}

Domain get_domain( const char* name ) {
  return domains[ find_domain(name) ];
}

static void
delete_pattern ( Pattern p ) {
    free((char*)p->pattern);
    free((char*)p->action);
    free(p);
}

static void
delete_patterns ( Patterns ps ) {
  Pattern p;
  Pattern n;
  if ( ps->dispatch != NULL ) {
    int i;
    Patterns xp;
    for ( i = 0 ; i < DISPATCH_SIZE ; i++ ) {
      xp = ps->dispatch[i];
      if ( xp != NULL ) {
	delete_patterns(xp);
	free(xp);
      }
    }
    free(ps->dispatch);
    ps->dispatch = NULL;
  }
  for ( p = ps->head ; p != NULL ; ) {
    n = p->next;
    delete_pattern(p);
    p = n;
  }
  ps->head = NULL;
  ps->tail = NULL;
}

void delete_domain(int n) {
  Domain dp;
  assert( n < ndomains );
  dp = domains[n];
  delete_patterns( &dp->patterns );
  while ( dp->init_and_final_patterns != NULL ) {
    Pattern p = dp->init_and_final_patterns;
    dp->init_and_final_patterns = p->next;
    delete_pattern(p);
  }
  if ( n == ndomains-1 ) {
    ndomains--;
    free((char*)dp->name);
    free(dp);
  }
}

void quoted_copy( CIStream in, COStream out ) {
  /* copy the input stream to the output stream, quoting any
     characters that have special meaning in patterns. */
  int qc;
  int quote = (int)syntax_chars[PI_QUOTE];
  for ( ; ; ) {
  	qc = cis_getch(in);
  	if ( char_kind(qc) != PI_LITERAL ) {
	  if ( qc == EOF )
	    break;
  	  cos_putch(out, quote);
	  if ( qc == '\n' )
	    qc = 'n';
	}
  	cos_putch(out, qc);
  }
}


const char* safe_string(const unsigned char* s) {
  if ( s == NULL )
    return "";
  else return (char*)s;
}

static void
describe_character(enum char_kinds chartype, const char* description ) {
  int ch = (int)syntax_chars[(int)chartype];
  if ( ch != '\0' )
    fprintf(stderr, "\t'%c' %s\n", ch, description );
}

void pattern_help( FILE* f ) {
  int i;
  assert( f == stderr );
  fprintf(f, "Pattern syntax:\n\t<template>%c<replacement>\n",
	syntax_chars[PI_SEP]);
  fprintf(f, "Text matching <template> is replaced by <replacement>.\n");
  fprintf(f, "Patterns are separated by a newline or '%c'.\n",
	syntax_chars[PI_END]);
  fputs("Special characters within a template:\n", f);
  describe_character( PI_ARG,  "argument - match any number of characters" );
  describe_character( PI_1ARG, "argument - match any one character" );
  describe_character( PI_RARG, "argument, recursively translated" );
  describe_character( PI_BEGIN_REGEXP, "regular expression delimiter" );
  fputs("Special characters within the replacement:\n", f );
  describe_character( PI_PUT, "followed by digit, insert numbered argument" );
  describe_character( PI_OP, "prefixes name of function to call" );
  fputs("Within both template and replacement:\n", f);
  describe_character( PI_ESC, "is an escape character." );
  describe_character( PI_CTRL, "adds the Control key to the following character" );
  fputs( "Following are all of the characters with special meaning:\n\t", f);
#if 0
  for ( i = PI_ARG ; i < PI_EOF ; i++ ) {
    int ch = (int)syntax_chars[i];
    if ( isprint(ch) && strchr((const char*)syntax_chars+1+i, ch)==NULL ) {
      fputc( ch, f );
      fputc( ' ', f );
    }
  }
#else
  for ( i = 0 ; i < NUMCHAR ; i++ ) {
    enum char_kinds kind = char_kind(i);
    if ( kind > PI_CR && isgraph(i) ) {
      fputc( i, f );
      fputc( ' ', f );
    }
  }
#endif
  fputs( "\nSee the man page for further details.\n", f );
}

void
skip_whitespace( CIStream s ) {
  while ( isspace(cis_peek(s)) )
    (void)cis_getch(s);
}

static unsigned char*
escaped_char( int ch, unsigned char* bp, CIStream s ) {
      int nc;
      int pc;

      nc = cis_getch(s);
      if ( syntax_chars[PI_ESC] != ch && char_kind(ch) == PI_QUOTE )
	pc = nc;
      else
      switch(nc) {
	/* control characters */
#ifndef MSDOS
#if '\r' != '\n'
	case '\r':
	  if ( cis_peek(s) != '\n' ) {
	    pc = nc;
	    break;
	  }
	  /* else fall-through to ignore redundant CR in MS-DOS files */
#endif
#endif
	case '\n': /* ignore new line and leading space on next line */
	  	skip_whitespace(s);
      		return bp;
	case 'n': pc = '\n'; break;
	case 't': pc = '\t'; break;
	case 'a': pc = '\a'; break;
	case 'b': pc = '\b'; break;
	case 'f': pc = '\f'; break;
	case 'r': pc = '\r'; break;
	case 'v': pc = '\v'; break;
	case 's': pc = ' '; break;
#if 'A' == 0x41
	case 'e': pc = ((char)0x1B) ; break; /* ASCII Escape */
	case 'd': pc = ((char)0x7F) ; break; /* ASCII Delete */
	case 'c': {	/* control */
	  int xc;
	  xc = cis_getch(s);
      	  pc = toupper(xc) ^ 0x40;
	  break;
	}
#elif 'A' == 0xC1
	case 'e': pc = ((char)0x27) ; break; /* EBCDIC Escape */
	case 'd': pc = ((char)0x07) ; break; /* EBCDIC Delete */
#endif
#if 0	/* not needed */
		/* the following two are the same in ASCI and EBCDIC */
	case 'o': pc = ((char)0x0E) ; break; /* shift out */
	case 'i': pc = ((char)0x0F) ; break; /* shift in */
#endif
	case 'x': {
	  char cbuf[4];
	  char* endp;
	  cbuf[0] = (char)cis_getch(s);
	  cbuf[1] = (char)cis_getch(s);
	  cbuf[2] = '\0';
	  pc = (int)strtol(cbuf,&endp,16);
	  if ( *endp != '\0' )
	    input_error(s, EXS_SYNTAX,
			"Invalid escape sequence in pattern:  \"%cx%s\"\n",
	                ch, cbuf);
	  break;
	}

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	  pc = nc - '0';
	  while ( isdigit(cis_peek(s)) )
	    pc = (pc << 3) + cis_getch(s) - '0';
	  break;

	/* template match operators */
	case 'S': pc = PT_SPACE; goto store;
	case 'W': pc = PT_SKIP_WHITE_SPACE; goto store;
	case 'I': pc = PT_ID_DELIM ; goto store;
	case 'X': pc = PT_WORD_DELIM ; goto store;
#if 0		/* changed my mind */
	case 'V': pc = PT_ARG_DELIM ; goto store;
#endif
	case 'N': pc = PT_LINE ; goto store;
	case 'B': pc = PTX_BEGIN_FILE ; goto aux;
	case 'E': pc = PTX_END_FILE ; goto aux;
	case 'L': pc = PTX_ONE_LINE ; goto aux;
	case 'C': pc = PTX_NO_CASE ; goto aux;
	case 'A': pc = PTX_INIT;  goto aux;
	case 'Z': pc = PTX_FINAL; goto aux;
	case 'P': pc = PTX_POSITION; goto aux;
	case 'G': pc = PTX_NO_GOAL; goto aux;
	case 'J': pc = PTX_JOIN; goto aux;

	/* quoting literal character */
	default:
	  if ( isalnum(nc) ||
      	       ( ch != '\\' && ch != syntax_chars[PI_QUOTE] ) )
	    input_error(s, EXS_SYNTAX,
      		"Invalid escape sequence in pattern:  \"%c%c\"\n",
	        ch, nc);
	  pc = nc;
	  break;
      } /* end switch */
    if ( is_operator(pc) )
      *bp++ = PT_QUOTE;
    goto store;
aux:
    *bp++ = PT_AUX;
store:
    *bp = pc;
    return bp+1;
}

struct regex_struct {
	const char* source;
	unsigned char* compiled;
};

static struct regex_struct * regex_table = NULL;

static int last_regex = -1;

#define MAX_NUM_REGEXP 255

int intern_regexp( unsigned char* exp, CIStream in ) {
  int i;
  if ( regex_table == NULL )
    regex_table = (struct regex_struct*)
		allocate( MAX_NUM_REGEXP * sizeof(struct regex_struct*),
		MemoryRegexp);
  for ( i = last_regex ; i >= 0 ; i-- )
    if ( strcmp( (const char*)exp, regex_table[i].source )==0 )
      return i;
  if ( last_regex >= (MAX_NUM_REGEXP-1) ) {
    input_error(in, EXS_SYNTAX, "More than %d unique regular expressions.\n",
		MAX_NUM_REGEXP);
    exit(exit_status);
    return -1; /* just to avoid warning from SGI compiler */
  }
  else {
    struct regex_struct* p;
    unsigned char* msg;
    size_t bufsize;
    bufsize = 200;
    p = regex_table + ++last_regex;
    p->source = str_dup((const char*)exp);
    p->compiled = allocate ( bufsize, MemoryRegexp);
    for ( ; ; ) {
      msg = regexp_comp(exp, p->compiled, bufsize);
      if ( msg == NULL ) /* OK */
	break;
      else if ( msg == regexp_dfa_buffer_too_short ) {
	bufsize = bufsize * 2;
	p->compiled = realloc( p->compiled, bufsize );
	if ( p->compiled == NULL ) {
	  input_error(in, EXS_MEM, "Out of memory for regular expression.\n");
	  exit(exit_status);
	}
	continue;
      }
      else {
	input_error(in, EXS_SYNTAX, "Error in regular expression: %s\n", msg);
	if ( ! keep_going ) {
	  exit(exit_status);
	}
	return intern_regexp( (unsigned char*)"\1", in );
      }
    }
    return last_regex;
  }
}

unsigned char*
regex_match(int regex_num, const unsigned char* text, boolean start_of_line) {
  /* if the regular expression interned as number `regex_num' matches the
     beginning of `text', return the position of the end of the match,
     else return NULL. */
  boolean match;
  assert( regex_num >= 0 && regex_num <= last_regex );
  match = regexp_exec( (unsigned char*)text, start_of_line, FALSE,
			regex_table[regex_num].compiled );
  if ( !match )
    return NULL;
  else return regexp_eopat[0];
}

static int
regexp_key( int regex_num ) {
  const char* s;
  int ch;
  s = regex_table[regex_num].source;
  if ( s[0] == '^' )
    s++;
  if ( s[0] == '\\' && s[1] == '<' )
    s += 2;
  ch = s[0];
  if ( strchr(".*+\\[]", ch )==NULL && !is_operator(ch) )
    return ch;
  else return PT_REGEXP;
}

static struct action_ops {
  const char* name;
  unsigned char code;
  unsigned char nargs;
} action_operators[] =
  { { "abort", OP_ABORT, 0 },
    { "add", OP_ADD, 2 },
    { "and", OP_AND, 2 },
    { "append", OP_APPEND, 2 },
    { "bind", OP_BIND, 2 },
    { "center", OP_CENTER, 2 },
    { "char-int", OP_CHARINT, 1 },
    { "close", OP_CLOSE, 1 },
    { "cmpi", OP_STRI_CMP, 5 }, /* compare strings, case insensitive */
    { "cmpn", OP_NUM_CMP, 5 },  /* compare numbers */
    { "cmps", OP_STR_CMP, 5 },  /* compare strings, case sensitive */
    { "column", OP_COL, 0 },
    { "date", OP_DATE, 0 },
    { "datime", OP_DATIME, 0 },
    { "decr", OP_DECR, 1 },
    { "define", OP_DEFINE, 1 },
    { "div", OP_DIV, 2 },
    { "downcase", OP_DOWNCASE, 1 },
    { "end", OP_EXIT, 0 },
    { "err", OP_ERR, 1},
    { "exit-status", OP_EXIT_STATUS, 1 },
    { "expand-wild", OP_EXP_WILD, 1 },
    { "fail", OP_FAIL, 0 },
    { "file", OP_FILE, 0 },
    { "file-time", OP_MODTIME, 0 },
    { "fill-center", OP_FILL_CENTER, 2 },
    { "fill-left", OP_FILL_LEFT, 2 },
    { "fill-right", OP_FILL_RIGHT, 2 },
    { "getenv", OP_GETENV, 1 },
    { "getenv", OP_GETENV_DEFAULT, 2 },
    { "get-switch", OP_GET_SWITCH, 1 },
    { "incr", OP_INCR, 1 },
    { "inpath", OP_PATH, 0 },
    { "int-char", OP_INTCHAR, 1 },
    { "line", OP_LINE, 0 },
    { "left", OP_LEFT, 2 },
    { "length", OP_LENGTH, 1 },
    { "makepath", OP_COMBINEPATH, 3 },
    { "mergepath", OP_MERGEPATH, 3 },
    { "mul", OP_MUL, 2 },
    { "mod", OP_MOD, 2 },
    { "not", OP_NOT, 1 },
    { "or", OP_OR, 2 },
    { "out", OP_OUT, 1},
    { "outpath", OP_OUTFILE, 0 },
    { "out-column", OP_OUTCOL, 0 },
    { "push", OP_BIND, 2 },
    { "pop", OP_UNBIND, 1 },
    { "probe", OP_PROBE, 1 },
    { "quote", OP_QUOTE, 1 },
    { "radix", OP_RADIX, 3 },
    { "read", OP_READ, 1 },
    { "relpath", OP_RELPATH, 2 },
    { "relative-path", OP_RELPATH, 2 },
    { "repeat", OP_REPEAT, 2 },
    { "reset-syntax", OP_DEFAULT_SYNTAX, 0 },
    { "reverse", OP_REVERSE, 1 },
    { "right", OP_RIGHT, 2 },
    { "set", OP_SET, 2 },
#ifndef MSDOS
    { "set-locale", OP_LOCALE, 1 },
#endif
    { "set-switch", OP_SET_SWITCH, 2 },
    { "set-syntax", OP_SYNTAX, 2 },  /*  @set-syntax{type;charset}  */
    { "set-parm", OP_SET_PARM, 2 },
    { "set-wrap", OP_SET_WRAP, 2 },
    { "shell", OP_SHELL, 1},
    { "show-help", OP_HELP, 0},
    { "sub", OP_SUB, 2 },
    { "subst", OP_SUBST, 2 },	/*  @subst{patterns;operand}  */
    { "substring", OP_SUBSTRING, 3 }, /*  @substring{skip,length,string}  */
    { "tab", OP_TAB, 1 },
    { "terminate", OP_END_OR_FAIL, 0 },
    { "time", OP_TIME, 0 },
    { "unbind", OP_UNBIND, 1 },
    { "undefine", OP_UNDEFINE, 1 },
    { "upcase", OP_UPCASE, 1 },
    { "var", OP_VAR, 1 },
    { "var", OP_VAR_DFLT, 2 },
    { "version", OP_VERSION, 0 },
    { "wrap", OP_WRAP, 1 },
    { "write", OP_WRITE, 2 },
    { NULL, 0, 0 }
  };

/* number of arguments for each built-in function: */
unsigned char fnnargs[OP_last_op] = { 0 };

void initialize_syntax(void) {
  int i;
  memcpy(syntax_chars, default_syntax_chars, sizeof(syntax_chars));
  for ( i = 0x20 ; i < NUMCHAR ; i++ )
    set_char_kind(i,PI_LITERAL);
  set_char_kind('\n',PI_END);
  for ( i = 0 ; i < Num_Char_Kinds ; i++ ) {
    unsigned int ch = syntax_chars[i];
    if ( ch != '\0' )
      set_char_kind(ch,i);
  }
  {
    const struct action_ops * tp;
    for ( tp = &action_operators[0] ; tp->name != NULL ; tp++ )
      fnnargs[tp->code] = tp->nargs;
  }
}

#if 0 /* not needed after all */
static int
end_delim(int c) {
  /* if the argument is an opening delimiter, return the corresponding
     closing delimiter, else NUL. */
  const char* k;
  for ( k = "(){}[]`'''""<>" ; *k != '\0' ; k += 2 )
    if ( k[0] == c )
      return k[1];
  return '\0';
}
#endif

static unsigned char*
read_action( CIStream s, unsigned char* bp, int nargs,
	     unsigned char* arg_keys );

int
read_put( CIStream s, unsigned char** app, int nargs,
	  unsigned char* arg_keys ) {
  int ch;
  int pc;
  unsigned char* ap;
  ap = *app;
  ch = cis_getch(s);
  if ( isdigit(ch) ) {  /* "$1" is argument */
    pc = ch - '0';
    if ( pc == 0 ) /* $0 is special case */
      pc = PT_MATCHED_TEXT;
    else if ( pc < 1 || pc > nargs ) {
      pc = syntax_chars[PI_PUT];
      input_error(s, EXS_SYNTAX,
    		  "Invalid argument number: \"%c%c\"\n", pc, ch);
    }
    else *ap++ = PT_PUT_ARG;
  }			/* "$x" is single-letter variable */
  else if ( isalpha(ch) ) {
    *ap++ = PT_VAR1;
    pc = ch;
  }
  else if ( char_kind(ch) == PI_BEGIN_ARG && arg_keys != NULL ) {
	/* "${varname}" */
    int xc;
    unsigned char* ap1;
    *ap++ = PT_OP;
    *ap++ = OP_VAR;
    ap1 = ap;
    ap = read_action( s, ap,  nargs, arg_keys );
    pc = PT_SEPARATOR;
    xc = cis_prevch(s);
    if ( xc == syntax_chars[PI_ARG_SEP] || char_kind(xc) == PI_ARG_SEP ) {
      (*app)[1] = OP_VAR_DFLT;
      *ap++ = PT_SEPARATOR;
      ap = read_action( s, ap,  nargs, arg_keys );
      xc = cis_prevch(s);
    }
    else {
      int n;
      char* end;
      *ap = '\0';
      n = (int)strtol((char*)ap1,&end,10);
      if ( n > 0 && end == (char*)ap && n <= nargs ) {
	/* an argument number instead of a variable */
	ap1[-2] = PT_PUT_ARG;
	ap = ap1-1;
	pc = n;
      }
    }
    if ( char_kind(xc) != PI_END_ARG )
      input_error(s, EXS_SYNTAX, "Missing \"%c\" for \"%c%c\"\n",
		  syntax_chars[PI_END_ARG], syntax_chars[PI_PUT],
  		  syntax_chars[PI_BEGIN_ARG]);
  }
  else {
    input_error(s, EXS_SYNTAX, "Invalid variable reference: \"%c%c\"\n",
		syntax_chars[PI_PUT], ch);
    *ap++ = syntax_chars[PI_PUT];
    pc = ch;
  }
  *app = ap;
  return pc;
}

static boolean 
skip_comment( CIStream s ) {
  int ch;
  ch = 0;
  for ( ; ; ) {
    int nc;
    nc = cis_getch(s);
    if ( nc == '\n' || nc == EOF )
      break;
    else ch = nc;
  }
  if ( char_kind(ch) == PI_ESC ) {
    /* line ends in "\"; continue with the next line. */
    /* ignore new line and leading space on next line */
    skip_whitespace(s);
    return FALSE;
  }
  else return TRUE;
}

/* maximum length of template or action: */
#define BUFSIZE 1200

static unsigned char*
read_action( CIStream s, unsigned char* bp, int nargs,
	     unsigned char* arg_keys ) {
  unsigned char* ap;
  enum char_kinds kind;
    for ( ap = bp ; ; ) {
      int pc; /* code for action */
      int ch; /* character read */

      ch = cis_getch(s);
      pc = ch; /* just to avoid warning from Gnu compiler */
      kind = char_kind(ch);
dispatch:
      switch ( kind ) {
      case PI_COMMENT: /* ignore rest of line */
	if( !skip_comment(s) )
	  continue;
	/* else fall-through to end the line */
      case PI_END:
      case PI_ARG_SEP:
      case PI_END_ARG:
      case PI_EOF:
	*ap = PT_END;
	return ap;

      case PI_BEGIN_ARG:
	input_error(s, EXS_SYNTAX, "Unexpected '%c' encountered.\n", ch);
	pc = ch;
	break;

      case PI_PUT:	/* argument or variable */
	pc = read_put( s, &ap, nargs, arg_keys );
	break;

    case PI_ARG:
    case PI_RARG:
    case PI_ABBREV_DOMAIN:
    case PI_1ARG: {
	int ai;
	pc = ch;
	for ( ai = 1 ; ai <= nargs ; ai++ )
	  if ( (int)arg_keys[ai] == ch ) {
	    *ap++ = PT_PUT_ARG;
	    pc = ai;
	    arg_keys[ai] = '\0';
	    break;
	  }
	if ( pc == ch && nargs > 0  )
	  input_error(s, EXS_OK, "More '%c' in action than in template.\n", ch);
	break;
	}

    case PI_CHAR_OP:
charop: {
      pc = cis_getch(s);
      kind = default_char_kind(pc);
      if ( kind <= PI_LIT_CTRL ) {
	input_error( s, EXS_SYNTAX, "Undefined control sequence: \"%c%c\"\n",
		ch, pc);
	break;
      }
      else {
	ch = pc;
	goto dispatch;
      }
    }

    case PI_OP: {
	  int xc;
	  unsigned char* xp;
	  unsigned char* apcode;
	  *ap = (unsigned char)ch;
	  for ( xp = ap+1 ; ; xp++ ) {
	    xc = cis_peek(s);
	    if ( !isalnum(xc) && xc != '_' && xc != '-' ) {
	      const struct action_ops * tp;
	      char* name;
	      if ( xp == ap+1 && ch == syntax_chars[PI_CHAR_OP] &&
	           char_kind(xc) != PI_BEGIN_ARG )
		goto charop;
	      *xp = '\0';
	      name = trim_name(ap+1);
	      for ( tp = &action_operators[0] ; ; tp++ ) {
	        if ( tp->name == NULL ) {
#if 1
		  int domain = find_domain(name);
	          *ap++ = PT_DOMAIN;
#if MAX_DOMAINS < 256
		  *ap++ = (unsigned char)(domain + 1);
#else
                  *ap++ = (unsigned char)((domain & 0x7f)|0x80);
                  *ap++ = (unsigned char)(((domain>>7) & 0x7f)|0x80);
#endif
		  if ( char_kind(xc) == PI_BEGIN_ARG ) {
		    int term_kind;
		    int term_char;
		    (void)cis_getch(s);
		read_arg:
		    ap = read_action( s, ap,  nargs, arg_keys );
		    term_char = cis_prevch(s);
		    term_kind = char_kind(term_char);
		    if ( term_kind != PI_END_ARG ) {
		      if ( term_kind == PI_ARG_SEP ||
			   term_char == syntax_chars[PI_ARG_SEP] ) {
		        input_error(s, EXS_SYNTAX,
		  	  "Arg separator \"%c\" in domain call \"%c%s%c\"\n",
				  term_char, ch,
		  		  domain_name(domain), xc);
			*ap++ = term_char;
			goto read_arg;
		      }
		      else input_error(s, EXS_SYNTAX,
		  		  "Missing \"%c\" for \"%c%s%c\"\n",
				  syntax_chars[PI_END_ARG], ch,
		  		  domain_name(domain), xc);
		    }
		  }
		  else input_error(s, EXS_SYNTAX,
	        		   "Error: missing '%c' after \"%c%s\"\n",
				   syntax_chars[PI_BEGIN_ARG],
	        		   ch, domain_name(domain));
	          pc = PT_SEPARATOR;
#else
	          input_error(s, EXS_SYNTAX,
	        	      "Error: undefined operator: \"%c%s\"\n",
	                      ch, name);
	          ap = xp;
	          pc = ' ';
#endif
	          goto store;
	        }
	        if ( stricmp(name,tp->name) == 0 ) {
		  unsigned n = 0;
	          *ap++ = PT_OP;
		  apcode = ap;
	          *ap++ = tp->code;
		  if ( char_kind(xc) == PI_BEGIN_ARG ) {
		    (void)cis_getch(s);
		    for ( ; ; ) {
		      ap = read_action( s, ap,  nargs, arg_keys );
		      *ap++ = PT_SEPARATOR;
		      n++;
		      xc = cis_prevch(s);
		      if ( xc != syntax_chars[PI_ARG_SEP] &&
		    	   char_kind(xc) != PI_ARG_SEP ) {
			if ( char_kind(xc) != PI_END_ARG )
		      	  input_error(s, EXS_SYNTAX,
		      		"Missing \"%c\" for \"%c%s%c\"\n",
				syntax_chars[PI_END_ARG], ch, tp->name,
		      		syntax_chars[PI_BEGIN_ARG]);
		    	break;
		      }
		    }
		  }
		  if ( n != tp->nargs ) {
		    /* @fn{} could mean no args or one empty arg. */
		    if ( n == 1 && tp->nargs == 0 && ap == apcode+2 ){
		      assert( ap[-1] == PT_SEPARATOR );
		      ap--;
		    }
		    else {
	              const struct action_ops * xtp;
		      for ( xtp = tp+1; ; xtp++ ) {
	                if ( xtp->name == NULL ) {
		          input_error(s, EXS_SYNTAX,
	                    "wrong number of arguments for %c%s:"
			    " expected %d, found %d\n", ch, tp->name,
		  	    tp->nargs, n);
	                  break;
	                }
	                else if ( xtp->nargs == n &&
		      		stricmp(tp->name,xtp->name) == 0 ) {
		      	  *apcode = xtp->code;
			  break;
		      	}
	             }
	            }
		  }
	          break;
	        }
	      }
	      goto end_op;
	    } /* end !isalpha */
	    *xp = (unsigned char)cis_getch(s);
	  } /* end for xp */
      end_op:
	  continue;
	} /* end PI_OP */

      case PI_IGNORED_SPACE:
      	while ( char_kind(cis_peek(s)) == PI_IGNORED_SPACE )
	  (void)cis_getch(s);
	if ( ap > bp && isident(ap[-1]) && isident(cis_peek(s)) )
	  pc = ' ';
	else if ( ap <= bp || ap[-1] != PT_ID_DELIM )
	  pc = PT_ID_DELIM;
	else continue;
	break;
      case PI_SPACE:
	*ap++ = PT_SPACE;
      	while ( cis_peek(s) == ch ) /* extra spaces are literals */
	  *ap++ = cis_getch(s);
	continue;

      case PI_QUOTE:
      case PI_ESC: {
	 unsigned char* sp;
	 sp = ap;  *sp = '\0';
	 ap = escaped_char(ch,ap,s);
	 if ( *sp == PT_AUX )
	   input_error(s, EXS_OK,
      		"Escape sequence \"%c%c\" is not meaningful in an action.\n",
      		ch, cis_prevch(s) );
	 continue;
       }

      case PI_QUOTE_STRING:
	for ( ; ; ) {
	  pc = cis_getch(s);
	  if ( pc == ch )
	    break;
	  if ( pc == '\n' ) {
	    input_error(s, EXS_SYNTAX, "Unmatched %c\n", ch );
	    break;
	  }
	  *ap++ = (unsigned char)pc;
	}
	continue;
      
#ifndef MSDOS
#if '\r' != '\n'
      case PI_CR:
	if ( cis_peek(s) == '\n' )
	  /* ignore redundant CR in files copied from MS-DOS to UNIX */
	  continue;
	else pc = ch;
	break;
#endif
#endif
      case PI_CTRL:  /* control character */
	 ch = cis_getch(s);
	 ch = toupper(ch) ^ 0x40;
	 /* and fall-through */
    case PI_DOMAIN:		/* This group of characters are */
    case PI_SEP:		/* special in templates but not */
    case PI_BEGIN_DOMAIN_ARG:	/* in actions. */
    case PI_END_DOMAIN_ARG:
    case PI_BEGIN_REGEXP:
    case PI_END_REGEXP:
	 if ( !is_operator(ch) ) {
	   pc = ch;
	   break;
	 } /* else fall-through */
    case PI_LIT_CTRL:
       /* quote characters that might be mistaken for internal control */
       *ap++ = PT_QUOTE;
       /* and fall-through */
    case PI_LITERAL:
       pc = ch;
       break;
    case PI_IGNORE:
       continue;
    case Num_Char_Kinds: /* just to avoid compiler warning */
#ifndef NDEBUG
    default:
	input_error(s, EXS_FAIL, "Invalid char kind %d for '%c'\n", kind, ch);
#endif
	pc = ch;
	break;
      } /* end switch(kind) */
   store:
      if ( ap >= &bp[BUFSIZE-10] ) {
	input_error(s, EXS_SYNTAX, "Action is too long.\n");
	ap = &bp[BUFSIZE/2];
      }
      *ap++ = (unsigned char)pc;
    } /* end for */
} /* end read_action */

static unsigned char*
read_actions( CIStream s, unsigned char* pbuf, int nargs,
	      unsigned char* arg_keys ) {
    unsigned char* ap;
    int prevch;
    ap = read_action(s,pbuf,nargs,arg_keys);
    prevch = cis_prevch(s);
    while ( char_kind(prevch) == PI_END_ARG && cis_peek(s) != EOF ) {
      input_error(s, EXS_SYNTAX, "Extraneous '%c'\n", prevch);
      *ap++ = prevch;
      ap = read_action(s,ap,nargs,arg_keys);
    }
    *ap++ = PT_END;
    if ( PT_END != '\0' )
	*ap++ = '\0';
    return ap;
}

static Pattern
read_pattern ( CIStream s, int * domainpt, int default_domain,
		boolean undef ){
  int ch;  /* character read */
  enum char_kinds kind; /* kind of character read */
  int pc;  /* pattern character */
  unsigned char pbuf [BUFSIZE];
  unsigned char* bp;
  unsigned char* start_bp;
  unsigned char* prev_bp;
  size_t plen;
  Pattern pt;
  unsigned char* astring;
  unsigned char* pstring;
  int nargs;
  unsigned char arg_keys[MAX_ARG_NUM+2];
  boolean domain_seen;

top:
  if ( cis_prevch(s) == '\n' )
    *domainpt = default_domain;
  nargs = 0;
  bp = pbuf;
  start_bp = (unsigned char*)"\0";
  domain_seen = FALSE;
  if ( cis_peek(s) == '\f' )
    (void)cis_getch(s);

  while ( char_kind(cis_peek(s)) == PI_OP ) { /* immediate action */
      (void)read_actions(s,pbuf,0,NULL);
      do_action( pbuf, NULL, output_stream);
    }

  /* read template to match */
  for ( ; ; ) {
    prev_bp = start_bp;
    start_bp = bp;
    ch = cis_getch(s);
    pc = ch; /* just to avoid warning from Gnu compiler */
    kind = char_kind(ch);
dispatch:
    switch (kind) {
    case PI_COMMENT: /* ignore rest of line */
      if ( pbuf[0] == PT_RECUR && cis_column(s) == 2 && cis_line(s) == 1 )
	/* As a special case, if the first line begins with "#!", then treat
	   the whole line as a comment on the assumption that this is an
	   executable file using "gema" instead of a shell.  For example:
			#!/usr/local/bin/gema -f
	   Note that this wouldn't be meaningful as a pattern anyway. */
	bp = pbuf;
      if( !skip_comment(s) )
	continue;
      /* else fall-through to end line */
    case PI_END:
      if ( bp == pbuf ) /* empty pattern; continue reading */
	goto top;
      /* else fall-through */
    case PI_EOF:
      if ( bp != pbuf && !undef ) {
	input_error(s, EXS_SYNTAX, "Missing '%c'\n", syntax_chars[PI_SEP]);
	bp = pbuf; /* discard incomplete rule to avoid other errors */
      }
      /* and fall-through */
    case PI_SEP:
      goto done;
#ifndef MSDOS
#if '\r' != '\n'
    case PI_CR:
      if ( cis_peek(s) == '\n' )
	/* ignore redundant CR in files copied from MS-DOS to UNIX */
	continue;
      else pc = ch;
      break;
#endif
#endif

    case PI_DOMAIN:  /* explicit domain name */
      if ( domain_seen ) {
	if ( bp == pbuf ) {
	  /* double colon indicates domain inheritance */
	  char* iname;
	  unsigned char* xp;
	  Domain dp = domains[*domainpt];

	  for ( xp = bp ; ; xp++ ) {
	    enum char_kinds xc = char_kind(cis_peek(s));
	    if ( xc == PI_END || xc == PI_COMMENT || xc == PI_EOF )
	      break;
	    else *xp = cis_getch(s);
	  }
	  *xp = '\0';
	  iname = trim_name(bp);
	  if ( dp->inherits != NULL ) {
	    input_error(s, EXS_SYNTAX,
	  		"Domain \"%s\" already inherits from \"%s\".\n",
	  		dp->name, dp->inherits->name );
	  }
	  else dp->inherits = get_domain(iname);
	  goto top;
	}
	else {
	  input_error(s, EXS_OK, "More than one '%c'\n", ch);
      	  pc = ch;
	}
	break;
      }
      else {
	char* name;
	*bp = '\0';
	name = trim_name(pbuf);
	if ( domain_name(*domainpt)[0] == ' ' )
	  /* only the domain " temp " created by OP_SUBST can have a
	     leading space */
	  input_error(input_stream, EXS_SYNTAX,
		"Domain change \"%s%c\" not allowed in temporary pattern.\n",
      		name, ch);
	else
#if MAX_DOMAINS < 256
	*domainpt = name[0] == PT_RECUR ? name[1]-1 : find_domain( name );
#else
	*domainpt = name[0] == PT_RECUR ? (unsigned int)(name[1]&0x7f) | (unsigned int)((name[2]&0x7f)<<7) : find_domain( name );
#endif
	bp = pbuf;
	domain_seen = TRUE;
	continue;
      }

    case PI_OP:
      if ( syntax_chars[PI_CHAR_OP] != ch ) {
	  input_error(s, EXS_OK, "'%c' not valid in template\n", ch);
	  pc = ch;
	  break;
      }
	/* else fall through when same character used for both */
   case PI_CHAR_OP: {
      pc = cis_getch(s);
      kind = default_char_kind(pc);
      if ( kind > PI_LIT_CTRL ) {
	  ch = pc;
	  goto dispatch;
      	}
      input_error( s, EXS_SYNTAX, "Undefined control sequence: \"%c%c\"\n",
		   ch, pc);
      break;
    }

    case PI_ARG:
    case PI_RARG:
    case PI_ABBREV_DOMAIN:
    case PI_1ARG:
    case PI_BEGIN_DOMAIN_ARG:
    case PI_BEGIN_REGEXP:
      if ( nargs >= MAX_ARG_NUM ) {
	*bp = '\0';
	if ( nargs == MAX_ARG_NUM )
	  input_error(s, EXS_SYNTAX, "Pattern has more than %d arguments.\n",
      		      MAX_ARG_NUM);
	pc = ch;
      }
      else {
	nargs++;
	arg_keys[nargs] = (unsigned char)ch;
	if ( kind == PI_RARG ) {
	  int domain = *domainpt;
	  *bp++ = PT_RECUR;
#if MAX_DOMAINS < 256
	  pc = domain + 1;
#else
	  /* Save low byte of domain index */
	  *bp++ = (unsigned char)((domain &0x7f)|0x80);
	  pc = ((domain>>7) & 0x7f)|0x80;
#endif
	}
	else if ( kind == PI_ABBREV_DOMAIN ) {
	  int domain;
	  char aname[2];
	  aname[0] = ch;
	  aname[1] = '\0';
	  domain = find_domain(aname);
	  *bp++ = PT_RECUR;
#if MAX_DOMAINS < 256
	  pc = 1 + domain;
#else
	  /* Save low byte of domain index */
	  *bp++ = (unsigned char)((domain &0x7f)|0x80);
	  pc = (((unsigned int)domain>>7) & 0x7f)|0x80;
#endif
	}
	else if ( kind == PI_BEGIN_DOMAIN_ARG ) {
	  int xc;
	  unsigned char* xp;
	  for ( xp = bp ; ; xp++ ) {
	    enum char_kinds xk;
	    xc = cis_getch(s);
	    xk = char_kind(xc);
	    if ( xk == PI_END_DOMAIN_ARG ||
		 syntax_chars[PI_END_DOMAIN_ARG] == xc ||
		 ( xk == PI_CHAR_OP &&
		   cis_peek(s)==(int)default_syntax_chars[PI_END_DOMAIN_ARG] &&
		   cis_getch(s) ) ) {
	      int inverse;
	      const char* dname;
	      const char* rname;
	      *xp = '\0';
	      inverse = 0;
	      dname = trim_name(bp);
	      rname = dname;
	      /* can't make up my mind which to use here: */
	      if ( strchr("^/~-!", rname[0]) != NULL && rname[0] != '\0' ) {
	      	rname++;
		inverse = 0x80;
	      }
	      if ( ( rname[1] == '\0'||
		     ( isdigit(rname[1]) &&
	               ( rname[2] == '\0' ||
	    		 ( isdigit(rname[2]) && rname[3] == '\0' ) ) ) ) &&
	    	   isalpha(rname[0]) &&
	    	   strchr("DLJKAWIFGCSOXNPTUVY",toupper(rname[0])) != NULL ) {
		/* built-in domains with single-letter names */
		unsigned parms;
		parms = inverse | (toupper(rname[0]) - ('A'-1));
		if ( islower(rname[0]) )
		  parms |= 0x40;
		if ( rname[1] == '\0' )
		  pc = 0xFF;
		else pc = 1 + atoi(rname+1);
		*bp++ = PT_SPECIAL_ARG;
		*bp++ = parms;
	      }
	      else {  /* user-defined domain */
		int domain = find_domain( dname );
	        *bp++ = PT_RECUR;
#if MAX_DOMAINS < 256
	        pc = 1 + domain;
#else
		/* Save low byte of domain index */
		*bp++ = (unsigned char)((domain & 0x7f)|0x80);
		pc = ((domain>>7) & 0x7f)|0x80;
#endif
	      }
	      break;
	    }
	    else if ( xc == EOF || xk == PI_END || xk == PI_SEP ) {
	      input_error(s, EXS_SYNTAX, "Error: unmatched '%c'\n", ch);
	      bp = xp;
	      goto done;
	    }
	    *xp = (unsigned char)xc;
	  }
	} /* end PI_BEGIN_DOMAIN_ARG */
	else if ( kind == PI_BEGIN_REGEXP ) {
	  int xc;
	  unsigned char* xp;
	  boolean op_found = FALSE;
	  pc = ch;
	  for ( xp = bp ; ; ) {
	    xc = cis_getch(s);
	    if ( xc == '\\' ) { /* not PI_ESC because this is regexp syntax */
	      *xp++ = xc;
	      xc = cis_getch(s);
	      if ( strchr("<>()", xc) != NULL )
	        op_found = TRUE;
	    }
	    else {
	    if ( syntax_chars[PI_END_REGEXP] == xc ||
	    	 char_kind(xc) == PI_END_REGEXP ||
		 ( char_kind(xc) == PI_CHAR_OP &&
		   cis_peek(s) == (int)default_syntax_chars[PI_END_REGEXP] &&
		   cis_getch(s) ) ) {
	        *xp = '\0';
		if ( ! op_found )
		  input_error(s, EXS_OK, "You probably intended \"\\%c\""
			" instead of a regular expression.\n", ch);
	        pc = intern_regexp( bp, s ) + 1;
 	        *bp++ = PT_REGEXP;
	        break;
	    }
	    if ( xc == '\n' || xc == EOF ) {
	      input_error(s, EXS_SYNTAX, "Unterminated regular expression.\n");
	      pc = PT_END;
	      break;
	    }
	    if ( strchr(".+*[^$|", xc) != NULL )
	      op_found = TRUE;
	    }
	    *xp++ = xc;
	  } /* end for xp */
	} /* end regexp */
	else pc = kind == PI_ARG ? PT_MATCH_ANY : PT_MATCH_ONE;
      }
      break;
    case PI_END_DOMAIN_ARG:
    case PI_END_REGEXP:
      input_error(s, EXS_OK, "Warning: unmatched '%c'\n", ch);
      pc = ch;
      break;

    case PI_PUT:	/* argument or variable */
      pc = read_put( s, &bp, nargs, NULL );
      break;

    case PI_QUOTE:
    case PI_ESC: {
       bp = escaped_char(ch,bp,s);
       if ( start_bp > pbuf ) {
	 if ( *start_bp == PT_AUX ) {
    	   if ( start_bp[1] == PTX_INIT || start_bp[1] == PTX_BEGIN_FILE )
	     input_error(s, EXS_SYNTAX,
    		"\"%c%c\" only meaningful at beginning of template.\n",
    		ch, cis_prevch(s) );
	   if ( start_bp[1] == PTX_JOIN && *prev_bp == PT_ID_DELIM ) {
	     bp = prev_bp;
	   }
	 }
	 else if ( *start_bp == *prev_bp && *prev_bp == PT_ID_DELIM )
	   /* delete redundancy */
	   bp = start_bp;
       }
       continue;
    }

    case PI_QUOTE_STRING:
      for ( ; ; ) {
	pc = cis_getch(s);
	if ( pc == ch )
	  break;
	if ( pc == '\n' ) {
	  input_error(s, EXS_SYNTAX, "Unmatched %c\n", ch );
	  break;
	}
	*bp++ = (unsigned char)pc;
      }
      continue;

    case PI_IGNORED_SPACE: {
	int nextch;
      	while ( char_kind(nextch = cis_peek(s)) == PI_IGNORED_SPACE )
	  (void)cis_getch(s);
	if ( !( ( *prev_bp == PT_ID_DELIM || isident(*prev_bp) ) &&
		isident(nextch) ) ) {
	  if ( char_kind(nextch) != PI_LITERAL &&
	       *prev_bp != PT_SKIP_WHITE_SPACE) {
	    pc = PT_SKIP_WHITE_SPACE;
	    break;
	  }
	  else continue;
	}
	/* else fall-through */
    }
    case PI_SPACE:
    	pc  = PT_SPACE;
	break;
    case PI_CTRL:  /* control character */
       pc = cis_getch(s);
       pc = toupper(pc) ^ 0x40;
       if ( is_operator(pc) )
	 /* quote characters that might be mistaken for an internal control */
	 *bp++ = PT_QUOTE;
       break;
    case PI_LIT_CTRL:
       *bp++ = PT_QUOTE;
       /* and fall-through */
    case PI_BEGIN_ARG: /* not special in this context */
    case PI_END_ARG:   /* not special in this context */
    case PI_ARG_SEP:   /* not special in this context */
    case PI_LITERAL:
       if ( token_mode && isident(ch) ) {
	 enum char_kinds peek_kind;
       	 if ( is_operator(*prev_bp) && *prev_bp != PT_ID_DELIM ) {
	   if ( prev_bp[0] == PT_AUX && prev_bp[1] == PTX_JOIN )
	     bp = prev_bp;
	   else *bp++ = PT_ID_DELIM;
	   start_bp = bp;
    	   }
	 peek_kind = char_kind(cis_peek(s));
	 if ( peek_kind != PI_LITERAL && peek_kind != PI_SPACE ) {
	  *bp++ = ch;
	  start_bp = bp;
	  pc = PT_ID_DELIM;
	  break;
	}
       } /* end token_mode */
       pc = ch;
       break;
    case PI_IGNORE:
       continue;
    case Num_Char_Kinds: ; /* just to avoid compiler warning */
#ifndef NDEBUG
    default:
	input_error(s, EXS_FAIL, "Invalid char kind %d for '%c'\n",
		    kind, ch);
#endif
    }
    *bp++ = (unsigned char)pc;
    if ( bp > &pbuf[BUFSIZE-10] ) {
      input_error(s, EXS_SYNTAX, "Template is too long.\n");
      bp = &pbuf[BUFSIZE/2];
    }
  } /* end for */
 done:
  if ( bp == pbuf && kind != PI_SEP )
    return NULL;
  *bp++ = PT_END;
  if ( PT_END != '\0' )
    *bp++ = '\0';
  pt = (Pattern) allocate(sizeof(*pt), MemoryPatterns);
  plen = bp - pbuf;
  pstring = (unsigned char*)allocate(plen, MemoryPatterns);
  memcpy(pstring,pbuf,plen);
  astring = NULL;
  pt->next = NULL;
 {
  unsigned char* ap;
  ap = pbuf;
  if ( undef )
    *ap++ = PT_UNDEF;
  if ( kind == PI_SEP ) {
    /* read action template */
    ap = read_actions(s,ap,nargs,arg_keys);
  }
  else *ap++ = PT_END;
  if ( pstring[0] == PT_MATCH_ONE && pstring[1] == PT_END ) {
    /* optimize special case when template is just "?" */
    unsigned char* zp;
    pstring[0] = PT_ONE_OPT;
    for ( zp = pbuf ; *zp != PT_END ; zp++ )
      if ( zp[0] == PT_PUT_ARG && zp[1] == 1 ) {
      	zp[0] = PT_ONE_OPT;
      	strcpy((char*)zp+1,(char*)zp+2);
      }
  }
  if ( pbuf[0] != PT_END ) {
      int len;
      len = ap - pbuf;
      astring = (unsigned char*)allocate(len,MemoryPatterns);
      memcpy(astring,pbuf,len);
  }
 }
 pt->pattern = pstring;
 pt->action = astring;
#ifndef NDEBUG
  if ( debug_switch ) {
    fprintf(stderr, "%s: \"%s\" -> \"%s\"\n",
	    safe_string((const unsigned char*)domain_name(*domainpt)),
	    safe_string(pt->pattern), safe_string(pt->action));
  }
#endif
  return pt;
}

unsigned
get_template_element( const unsigned char** ap, boolean for_goal ) {
  unsigned result;
  unsigned char ch;
  const unsigned char* a = *ap;
again:
    ch = *a++;
    switch(ch){
      case PT_END:
	result = ENDOP;
	break;
      case PT_AUX:
	if ( *a++ == PTX_NO_GOAL && for_goal ) {
	  result = ENDOP;
	  break;
	}
	else goto again;
      case PT_LINE:
	if ( for_goal && *a == PT_END ) {
	  result = '\n';
	  break;
	}
	else goto again;
      case PT_QUOTE:
	result = *a++;
	break;
      case PT_VAR1:
	if ( for_goal ) {
	  char vname[2];
	  const char* value;
	  size_t length;
	  vname[0] = *a++;
	  vname[1] = '\0';
	  value =  get_var(vname,TRUE,&length);
	  if ( value != NULL ) {
	    if ( length == 0 )
	      goto again;
	    else {
	      result = value[0];
	      break;
	    }
	  }
	}
	/* else fall-through */
      case PT_RECUR:
      case PT_REGEXP:
      case PT_PUT_ARG:
	result = UPOP(ch) | *a++;
	break;
      case PT_SPECIAL_ARG:
	result = UPOP(ch) | *a;
	a += 2;
	break;
      case PT_ID_DELIM:
      case PT_WORD_DELIM:
	*ap = a;
	result = get_template_element(ap,for_goal);
	if ( result != ENDOP )
	  return result;
	result = UPOP(ch);
	break;
      case PT_SKIP_WHITE_SPACE:
	goto again;
      default:
	result = ch;
	if ( is_operator(ch) )
	  result = UPOP(result);
	break;
    } /* end switch */

  *ap = a;
  return result;
}

int
compare_specificity( const unsigned char* a, const unsigned char* b ) {
/* Given two templates (in internal representation), return > 0 if a is
   more specific than b, or < 0 if a is less specific, or 0 if they should
   remain in the same order in which they were defined.
   The two templates are compared (ignoring certain operators that don't
   matter for this purpose) until the first difference is found.  At that
   point, a literal is most specific, the end of the template is least
   specific, and arguments are intermediate.
*/
  const unsigned char* xp;
  const unsigned char* yp;
  int count = 0;
  for ( xp = a, yp = b ; ; ) {
    unsigned xc, yc;

    xc = get_template_element( &xp, FALSE );
    yc = get_template_element( &yp, FALSE );

    if ( xc != yc ) {
      if ( xc == ENDOP )
	return -1; /* a less specific than b */
      if ( yc == ENDOP )
	return 1; /* a more specific than b */
      if ( ISOP(yc) ) {
	if ( !ISOP(xc) )
	  return count;  /* a more specific than b (unless at beginning) */
	else if ( *xp == PT_END && yc == UPOP(PT_MATCH_ANY) )
	  return 1;  /* trailing "*" is always least specific */
      }
      else if ( ISOP(xc) )
	return -count;  /* a less specific than b (unless at beginning) */
      return 0;
    }
    else if ( xc == ENDOP )
      return 0;
    count++;
  }
}

static void
chain_pattern( Pattern pat, Patterns patset, int key ) {
  boolean undef = pat->action != NULL && pat->action[0] == PT_UNDEF;
     if ( patset->tail == NULL ) {
     	if ( undef ) {
	  delete_pattern(pat);
	  return;
     	}
	patset->head = pat;
     }
     else {
	Pattern old;
	Pattern prev;
	assert ( patset->tail->next == NULL );
	prev = NULL;
	for ( old = patset->head ; old != NULL ; old = old->next ) {
	  int cmp;
	  cmp = compare_specificity( pat->pattern, old->pattern );
	  if ( cmp > 0 && !undef ) {
	    /* new pattern more specific than old, needs to go before it. */
	    pat->next = old;
	    if ( prev == NULL )
	      patset->head = pat;
	    else prev->next = pat;
	    return;
	  }
	  else if ( cmp == 0 && strcmp((const char*)old->pattern,
				       (const char*)pat->pattern) == 0 ) {
	    /* identical template */
	    if ( undef ) {
	      if ( pat->action[1] == PT_END ||
       		   strcmp((const char*)old->action,
			  (const char*)pat->action+1) == 0 ) {
		/* undefine the old macro */
		if ( prev == NULL )
		  patset->head = old->next;
		else prev->next = old->next;
		if ( old == patset->tail )
		  patset->tail = prev;
		delete_pattern(old);
		delete_pattern(pat);
		return;
	      }
	    } /* end PT_UNDEF */
	    else { /* newer definition replaces older */
	    if ( old->action != pat->action ) {
	      /* Have to be careful not to delete an action that might
		 have been installed in more than one place as the result of
		 a "\W" or "\S" at the beginning of the template.
		 This crude hack favors memory leaks over corrupted data.*/
	      if ( !isspace(key) )
		free((char*)old->action);
	      old->action = pat->action;
	    }
	    if ( old->pattern != pat->pattern )
	      if ( !isspace(key) )
	      free((char*)pat->pattern);
	    free(pat);
	    return;
	    } /* end replace */
	  }
	  prev = old;
	} /* end for */
     	if ( undef ) {
	  delete_pattern(pat);
	  return;
     	}
	patset->tail->next = pat;
     }
    patset->tail = pat;
}

static Pattern
copy_pattern( Pattern opat ) {
  /* share the template and action pointers, but need separate `next' cell. */
  Pattern pt;
  pt = (Pattern) allocate(sizeof(*pt), MemoryPatterns);
  pt->pattern = opat->pattern;
  pt->action = opat->action;
  pt->next = NULL;
  return pt;
}

static void
install_pattern( const unsigned char* template, Pattern pat,
	     	 Patterns patset );

boolean
literal_key( const unsigned char* ps );

static void
install_pattern_for_key( unsigned char key, const unsigned char* rest,
	     		Pattern pat, Patterns patset ) {
  int index;
  Patterns sub;
  const unsigned char* more;

  index = dispatch_index(key);
  if ( patset->dispatch == NULL ) {
    int i;
    patset->dispatch = (Patterns*) allocate( DISPATCH_SIZE * sizeof(Patterns),
	                                MemoryDispatch );
    for ( i = 0 ; i < DISPATCH_SIZE ; i++ )
      patset->dispatch[i] = NULL;
  }
  sub = patset->dispatch[index];
  if ( sub == NULL ) {
    sub = (Patterns)allocate( sizeof(struct patterns_struct), MemoryDispatch );
    patset->dispatch[index] = sub;
    sub->dispatch = NULL;
    sub->head = pat;
    sub->tail = pat;
  }
  else if ( sub->dispatch != NULL )
    install_pattern( rest, pat, sub );
  else if ( sub->head != sub->tail &&
	    ( (more=pat->pattern), (key==get_template_element(&more,FALSE))) &&
	    more == rest && /* not already at second level */
	    ( literal_key(more) || 
	      ( (more=sub->head->pattern) , get_template_element(&more,FALSE) ,
		literal_key(more) ) ) ) {
    /* already have at least two entries and adding a third,
       and at least one of them has a literal following the initial key;
       change to use a second-level dispatch table. */
    Pattern old;
    old = sub->head;
    sub->head = NULL;
    sub->tail = NULL;
    while ( old != NULL ) {
      Pattern next;
      unsigned op;
      const unsigned char* old_template;
      next = old->next;
      old->next = NULL;
      old_template = old->pattern;
      op = get_template_element(&old_template,FALSE);
      if ( op == key || ( op < 0xFF && toupper(op) == toupper(key) ) )
	install_pattern( old_template, old, sub );
      else chain_pattern( old, sub, key );
      old = next;
    }
    install_pattern( rest, pat, sub );
  }
  else chain_pattern( pat, sub, key );
}

static void
install_pattern( const unsigned char* template, Pattern pat,
	     	 Patterns patset ) {
  unsigned char key;
  const unsigned char* rest;

  rest = template;
  key = *rest++;
  while ( key == PT_AUX ) {
    key = rest[1];
    rest += 2;
  }
  while ( key == PT_ID_DELIM || key == PT_WORD_DELIM ||
	  key == PT_LINE )
    key = *rest++;
  if ( key == PT_SKIP_WHITE_SPACE || key == PT_SPACE ) {
      install_pattern_for_key( ' ', rest, copy_pattern(pat), patset );
      install_pattern_for_key( '\t', rest, copy_pattern(pat), patset );
#if '\r' != '\n'
      install_pattern_for_key( '\r', rest, copy_pattern(pat), patset );
#endif
      if ( pat->pattern[0] != PT_LINE && rest[0] != PT_LINE &&
  	   !line_mode && !(pat->pattern[0] == PT_AUX &&
  		           pat->pattern[1] == PTX_ONE_LINE ) ) {
	install_pattern_for_key( '\n', rest, copy_pattern(pat), patset );
	install_pattern_for_key( '\f', rest, copy_pattern(pat), patset );
      }
      if ( key == PT_SPACE )
	return;
      key = *rest++;
  }
  if ( key == PT_REGEXP )
    key = regexp_key( rest[0]-1 );
  if ( key == PT_QUOTE )
    key = *rest++;
  else if ( is_operator(key) ) {
    if ( key == PT_RECUR
#if MAX_DOMAINS < 256
	 && patset == &domains[rest[0]-1]->patterns
#else
	 && patset == &domains[(unsigned int)(rest[0]&0x7f)
			      |(unsigned int)((rest[1]&0x7f)<<7)]->patterns
#endif
	 )
      /* template beginning with recursive arg in same domain is
	 going to get stack overflow */
      input_error(input_stream, EXS_SYNTAX, "Unbounded recursion.\n");
    chain_pattern( pat, patset, key );
    return;
  }
  install_pattern_for_key( key, rest, pat, patset );
#if 0 /* upper and lower case now have same dispatch key anyway */
  if ( case_insensitive && isalpha(key) ) {
    if ( islower(key) )
      key = toupper(key);
    else key = tolower(key);
    install_pattern_for_key( key, rest, copy_pattern(pat), patset );
  }
#endif
}

boolean
literal_key( const unsigned char* ps ) {
  /* returns true if the template begins with a literal instead of
     an argument. */
  return !ISOP(get_template_element(&ps,FALSE));
}

int read_patterns ( CIStream s, const char* default_domain,
		    boolean undef ){
  Pattern pat;
  int domain, top;
  CIStream save_input;
  save_input = input_stream;
  if ( input_stream == NULL || cis_pathname(s) != NULL )
    input_stream = s;
  top = find_domain(default_domain);
  domain = top;
  while ( NULL != (pat = read_pattern(s, &domain, top, undef)) ) {
    const unsigned char* ps = pat->pattern;
    if ( ps[0] == PT_AUX &&
  	 ( ps[1] == PTX_INIT || ps[1] == PTX_BEGIN_FILE ||
	   ps[1] == PTX_FINAL || ps[1] == PTX_END_FILE ) ) {
      Pattern* opp = &domains[domain]->init_and_final_patterns;
      while ( *opp != NULL )
	opp = &(*opp)->next;
      *opp = pat;
    }
    else install_pattern( ps, pat, &domains[domain]->patterns );
   } /* end while */
  input_stream = save_input;
  return top;
}
