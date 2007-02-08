
/* pattern implementation internal declarations */

/* $Id: patimp.h,v 1.10 2004/09/18 22:58:58 dngray Exp $ */

/*********************************************************************
  This file is part of "gema", the general-purpose macro translator,
  written by David N. Gray <dgray@acm.org> in 1994 and 1995.
  You may do whatever you like with this, so long as you retain
  an acknowledgment of the original source.
 *********************************************************************/

#include "pattern.h"

/* special characters used internally: */
  /* in both templates and actions: */
#define PT_END '\0'			/* end of template or action */
#define PT_QUOTE '\1'			/* use next character literally */
#define PT_SPACE '\2'			/* match at least one white space */
#define PT_LINE '\3'			/* match beginning or end of line */

  /* pattern matching operations in templates: */

#define PT_MATCH_ANY '\4'		/* wild card match argument */
#define PT_RECUR '\5'			/* recursively translated argument */
#define PT_SKIP_WHITE_SPACE '\6'	/* skip optional white space */
#define PT_SPECIAL_ARG	'\7'		/* predefined recognition domains */
	/* note: skip over codes 08..0D for whitspace control */
#define PT_WORD_DELIM ((char)0x0E)	/* word delimiter */
#define PT_ID_DELIM ((char)0x0F)	/* identifier delimiter */
#define PT_AUX ((char)0x10)		/* extended opcode PTX_... */
#define PT_REGEXP ((char)0x11)		/* argument as regular expression */
#define PT_MATCH_ONE ((char)0x12)	/* match a single character */
#if 0
#define PT_ARG_DELIM ((char)0x13)	/* command line argument delimiter */
#endif

  /* operations in actions: */
#define PT_PUT_ARG ((char)0x14)		/* output argument value */
	/* skip over ((char)0x15) for EBCDIC NewLine */
#define PT_DOMAIN ((char)0x16)	/* translate argument in another domain */
#define PT_SEPARATOR ((char)0x17)	/* separates arguments */
#define PT_OP ((char)0x18)		/* operator prefix, followed by OP_... */
#define PT_VAR1 ((char)0x19)		/* value of variable with 1 letter name */
#define PT_MATCHED_TEXT ((char)0x1A)	/* output all the matched text */
	/* note: skip over ((char)0x1B) for Escape character */

#define PT_ONE_OPT ((char)0x1C)	/* optimized "?" argument */

  /* special flag used during undefinition: */
#define PT_UNDEF ((char)0x1F)		/* undefine the pattern */

  /* extended template opcodes following PT_AUX: */
#define PTX_INIT '\1'		/* beginning-of-domain processing */
#define PTX_FINAL '\2'		/* end-of-domain processing */
#define PTX_BEGIN_FILE '\3'	/* beginning-of-file processing */
#define PTX_END_FILE '\4'	/* end-of-file processing */
#define PTX_ONE_LINE '\5'	/* set line mode for this template */
#define PTX_NO_CASE '\6'	/* case insensitive mode for this template */
#define PTX_POSITION '\7'	/* mark ending position of input stream */
#define PTX_NO_GOAL ((char)0x08)/* don't use rest as argument terminator */
#define PTX_JOIN ((char)0x09)	/* concatenate (override token mode) */

enum Operators {
 OP_NONE, /* skip 0 */
	/* variables: */
 OP_VAR,		/* value of variable */
 OP_SET,		/* set variable */
 OP_INCR,		/* increment variable */
 OP_DECR,		/* decrement variable */
 OP_VAR_DFLT,		/* variable with default if not defined */
 OP_BIND,		/* bind a variable */
 OP_UNBIND,		/* unbind a variable */
 OP_APPEND,		/* append to variable */

	/* match termination: */
 OP_EXIT,		/* end translation */
 OP_FAIL,		/* translation fails */
 OP_END_OR_FAIL,	/* end if any match, else fail */

	/* string operations: */
 OP_QUOTE,		/* quote characters for OP_DEFINE */
 OP_LEFT,		/* left justify */
 OP_RIGHT,		/* right justify */
 OP_CENTER,		/* center string in field */
 OP_FILL_LEFT,		/* left justify in background string */
 OP_FILL_RIGHT,		/* right justify in background string */
 OP_FILL_CENTER,	/* center string in background string */
 OP_STR_CMP,		/* string comparison, case sensitive */
 OP_STRI_CMP,		/* string comparison, case insensitive */
 OP_SUBST,		/* substitution using temporary pattern */
 OP_REVERSE,		/* reverse string */
 OP_LENGTH,		/* length of string */
 OP_UPCASE,		/* convert to upper case */
 OP_DOWNCASE,		/* convert to lower case */
 OP_SUBSTRING,		/* substring */

	/* characters: */
 OP_CHARINT,		/* numeric code of character argument */
 OP_INTCHAR,		/* construct character with given code */

	/* files: */
 OP_ERR,		/* write message to stderr */
 OP_OUT,		/* write directly to current output file */
 OP_READ,		/* read from another file */
 OP_FILE,		/* name of input file */
 OP_PATH,		/* pathname of input file */
 OP_LINE,		/* current line number in input file */
 OP_COL,		/* current column number in input file */
 OP_WRITE,		/* write to alternate file */
 OP_CLOSE,		/* close alternate output file */
 OP_MODTIME,		/* modification time of input file */
 OP_OUTFILE,		/* name of output file */
 OP_OUTCOL,		/* current column number in output file */
 OP_MERGEPATH,		/* merge pathnames, dir of 1st, name of 2nd */
 OP_COMBINEPATH,	/* args are directory, name, and type */
 OP_RELPATH,		/* relative pathname (2nd arg in dir of 1st) */
 OP_PROBE,		/* test whether pathname is defined */
 OP_EXP_WILD,		/* expand wild card file name */

	/* output formatting: */
 OP_TAB,		/* skip to particular output column */
 OP_WRAP,		/* output argument after newline if needed */
 OP_SET_WRAP,		/* set wrap column and indent string */

	/* miscelaneous: */
 OP_DEFINE,		/* define patterns */
 OP_UNDEFINE,		/* undefine patterns */
 OP_DATE,		/* current date */
 OP_TIME,		/* current time */
 OP_DATIME,		/* current date and time */
 OP_GETENV,		/* get environment variable */
 OP_GETENV_DEFAULT, 	/* get env var with default value */
 OP_SHELL,		/* execute a shell command */
 OP_EXIT_STATUS,	/* set program termination code */
 OP_SET_SWITCH,		/* set numeric program option */
 OP_GET_SWITCH,		/* get value of numeric program option */
 OP_SET_PARM,		/* set program parameter with string value */
 OP_SYNTAX,		/* change syntax type of characters */
 OP_DEFAULT_SYNTAX,	/* restore default syntax table */
 OP_LOCALE,		/* set internationalization locale */
 OP_ABORT,		/* stop immediately */
 OP_HELP,		/* display command usage message */
 OP_REPEAT,		/* repeated execution */
 OP_VERSION,		/* display program version */

	/* arithmetic: */
 OP_ADD,		/* addition */
 OP_SUB,		/* subtraction */
 OP_MUL,		/* multiplication */
 OP_DIV,		/* division */
 OP_MOD,		/* modulus */
 OP_AND,		/* bitwise and */
 OP_OR,			/* bitwise or */
 OP_NOT,		/* bitwise inverse */
 OP_NUM_CMP,		/* numeric comparison */
 OP_RADIX,		/* radix conversion */

 OP_last_op
};

boolean is_operator(int x);
boolean isident( int ch ); /* is the character an identifier constituent? */

/* maximum number of arguments in a template: */
#define MAX_ARG_NUM 20

struct pattern_struct {
  const unsigned char* pattern;
  const unsigned char* action;
  struct pattern_struct * next;
};

#define DISPATCH_SIZE 0x60

#define dispatch_index(ch) (((unsigned)toupper(ch)) % DISPATCH_SIZE)

struct patterns_struct {
  struct patterns_struct** dispatch;
  struct pattern_struct* head;
  struct pattern_struct* tail;
};

typedef struct pattern_struct * Pattern;

struct domain_struct {
  struct patterns_struct patterns;
  struct pattern_struct* init_and_final_patterns;
  const char* name;
  Domain inherits;
};

/* Maximum number of domains.  This may be modified as needed. */
#ifndef MAX_DOMAINS
#define MAX_DOMAINS 240
#endif

extern int ndomains;

extern Domain domains[MAX_DOMAINS];
#define domain_name(n) domains[n]->name

void delete_domain(int n);

extern enum Translation_Status
  { Translate_Complete, Translate_Continue,Translate_Exited, Translate_Failed }
  translation_status;

const unsigned char*
do_action( const unsigned char* action, CIStream* args, COStream out);

extern Pattern current_rule;

extern int arg_char; /* value of optimized "?" argument */

extern unsigned char fnnargs[OP_last_op];

void quoted_copy( CIStream in, COStream out );

int intern_regexp( unsigned char* exp, CIStream in );

unsigned char*
regex_match(int regex_num, const unsigned char* text, boolean start_of_line);

unsigned
get_template_element( const unsigned char** ap, boolean for_goal );
/* the following macros concern the result value of the above function */
#define UPOP(op) (op << 8)
#define ISOP(x) (x & 0xFF00)
#define ENDOP 0x4000
