
/* variables */

/* $Id: var.c,v 1.1 2004/03/12 00:42:09 cgay Exp $ */

/*********************************************************************
  This file is part of "gema", the general-purpose macro translator,
  written by David N. Gray <dgray@acm.org> in 1994 and 1995.
  You may do whatever you like with this, so long as you retain
  an acknowledgment of the original source.
 *********************************************************************/

#include "var.h"
#include "util.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>	/* for isdigit */
#include "cstream.h"  /* for input_error */
#include "main.h"  /* for EXS_MEM */

struct var_struct {
  const char* name;
  char* value;
  struct var_struct* next;
  unsigned length;
};

varp first_bound_var = NULL;
static varp last_bound_var = NULL;

static varp first_global_var = NULL;

static varp find_var ( const char* name ) {
  varp x;
  for ( x = first_bound_var ; x != NULL ; x = x->next )
    if ( strcmp(name, x->name) == 0 )
      return x;
  return NULL;
}

const char* set_var( const char* name, const char* value, size_t len ) {
  varp x;
  x = find_var(name);
  if ( x != NULL ) {
    if ( len != x->length ) {
      free(x->value);
      x->value = str_dup_len(value,len);
    }
    else strcpy(x->value, value);
  }
  else {
    x = (varp)allocate(sizeof(struct var_struct), MemoryVar);
    x->next = first_global_var;
    x->name = str_dup(name);
    x->value = str_dup_len(value,len);
    first_global_var = x;
    if ( last_bound_var != NULL ) {
      assert ( last_bound_var->next == x->next );
      last_bound_var->next = x;
    }
    else first_bound_var = x;
  }
  x->length = len;
  return x->value;
}

void bind_var( const char* name, const char* value, size_t len ) {
  varp x;
  x = (varp)allocate(sizeof(struct var_struct), MemoryVar);
  x->next = first_bound_var;
  x->name = str_dup(name);
  x->value = str_dup_len(value,len);
  first_bound_var = x;
  if ( last_bound_var == NULL )
    last_bound_var = x;
  x->length = len;
}

static void
delete_var( varp x ) {
      free(x->value);
      free((char*)x->name);
      free(x);
}

void unbind_var( const char* name ) {
  varp x, prev;
  prev = NULL;
  for ( x = first_bound_var ; x != first_global_var ; x = x->next ) {
    if ( strcmp(name, x->name) == 0 ) {
      if ( x == last_bound_var )
	last_bound_var = prev;
      if ( prev == NULL )
	first_bound_var = x->next;
      else prev->next = x->next;
      delete_var(x);
      return;
    }
    prev = x;
  }
  input_error( input_stream, EXS_OK, "No binding for @unbind{%s}\n", name);
}

const char* get_var( const char* name, boolean undefined_ok, size_t* lenpt ) {
  varp x;
  x = find_var(name);
  if ( x != NULL ) {
    *lenpt = x->length;
    return x->value;
  }
  else if ( undefined_ok )
    return NULL;
  else {
    input_error( input_stream, EXS_UNDEF,
  		"Undefined variable: \"%s\"\n", name);
    *lenpt = strlen(name);
    return name;
  }
}

void incr_var( const char* name, int amount ){
  const char* val;
  size_t length;
  val = get_var(name,FALSE,&length);
  if ( val == name ) /* error */
    return;
  else if ( strlen(val) < 86 ) {
    char buf[90];
    long n;
    char* end;
    char* bp = buf;
    const char* start;
    for ( start = val ; ; start++ ) {
      int ch = *start;
      if ( isdigit(ch) || ch == '-' || ch == '+' ) { /* increment number */
      	n = strtol( start, &end, 10 );
      	sprintf(bp,"%ld%s", n+amount, end);
	break;
      }
      else *bp++ = ch;
      if ( ch == '\0' ) { /* no number found */
	if ( bp > buf && isalpha(bp[-2]) ) { /* increment alphabetic counter */
	  char tc, nc;
	  tc = bp[-2];
	  nc = (char)(tc + amount);
	  if ( isalpha(nc) )
	    bp[-2] = nc;
	  else if ( amount == 1 && tolower(tc) == 'z' ) {
	      char a = (char)(tc - ('z'-'a'));
	      if ( bp == buf+2 || !isalpha(bp[-3]) ) {
		bp[-2] = a;
		bp[-1] = a;
		bp[0] = '\0';
	      }
	      else {
		bp[-3] = bp[-3] + 1;
		bp[-2] = a;
	      }
	  }
	  else goto no_good;
	  break;
	}
	else goto no_good;
      }
    } /* end for */
   set_var(name,buf,strlen(buf));
   return;
  } /* end defined value not too long */
no_good:
  input_error( input_stream, EXS_NUM,
		"Can't increment variable \"%.99s\" with value \"%.99s\"\n",
		name, val);
}

const char* append_var( const char* name, const char* value, size_t val_len ) {
  varp x;
  x = find_var(name);
  if ( x == NULL )
    return set_var( name, value, val_len );
  else {
    size_t old_len;
    size_t new_len;
    old_len = x->length;
    new_len = old_len + val_len;
    x->value = realloc(x->value, new_len+1);
    if ( x->value == NULL ) {
      fprintf(stderr, "Out of memory for variable \"%s\"; aborting.\n",
    		name);
      exit((int)EXS_MEM);
    }
    strcpy( x->value + old_len, value );
    x->length = new_len;
    return x->value;
  }
}

/* crude first approximation at an "undo" facility: */

void prune_vars(varp old) {
  varp x;
  while ( first_bound_var != old &&
	  first_bound_var != first_global_var ) {
    x = first_bound_var;
    first_bound_var = x->next;
    if ( x == last_bound_var )
      last_bound_var = NULL;
    delete_var(x);
  }
}

#if 0
/* beginning of more comprehensive mechanism, not yet used: */
static VarMark* oldest_mark = NULL;
static VarMark* newest_mark = NULL;

void mark_vars(VarMark* m) {
  m->first_var = first_bound_var;
  m->previous_mark = newest_mark;
  if ( oldest_mark == NULL )
    oldest_mark = m;
  newest_mark = m;
}

void commit_vars(VarMark* m) {
  assert( m == newest_mark );
  newest_mark = m->previous_mark;
  if ( m == oldest_mark )
    oldest_mark = NULL;
}

void restore_vars(VarMark* m) {
  varp x;
  assert( m == newest_mark );
  while ( first_bound_var != m->first_var &&
	  first_bound_var != first_global_var ) {
    x = first_bound_var;
    first_bound_var = x->next;
    delete_var(x);
  }
  newest_mark = m->previous_mark;
  if ( m == oldest_mark )
    oldest_mark = NULL;
}
#endif
