
#ifndef MAIN_H
#define MAIN_H
#include "bool.h"

extern boolean keep_going;

extern boolean binary;

typedef enum {
EXS_OK,		/* nothing wrong */
EXS_1,		/* (reserved for user) */
EXS_FAIL,	/* failed match signaled by "@fail" or "@abort" */
EXS_ARG,	/* undefined command line argument */
EXS_SYNTAX,	/* syntax error in pattern definitions */
EXS_UNDEF,	/* use of undefined name during translation
		   (domain, variable, switch, parameter, syntax, locale) */
EXS_NUM,	/* invalid numeric operand */
EXS_SHELL,	/* can't execute shell command */
EXS_INPUT,	/* I/O error on input file */
EXS_OUTPUT,	/* I/O error on output file */
EXS_MEM 	/* out of memory */
} Exit_States;

extern Exit_States exit_status;

extern const char what_string[];
#define Version what_string+4

int*
find_switch(const char* arg);

boolean
set_parm(const char* name, const char* value);

void usage(void);

#endif
