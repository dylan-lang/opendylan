#ifndef VAR_H
#define VAR_H
#include <stddef.h>

#include "bool.h"

const char* set_var( const char* name, const char* value, size_t len );

const char* get_var( const char* name, boolean undefined_ok, size_t* lenpt );

void incr_var( const char* name, int amount );

const char* append_var( const char* name, const char* value, size_t len );

void bind_var( const char* name, const char* value, size_t len );

void unbind_var( const char* name );

typedef struct var_mark_struct{
  struct var_struct * first_var;
  struct var_mark_struct * previous_mark;
} VarMark;

void mark_vars(VarMark* m);
void commit_vars(VarMark* m);
void restore_vars(VarMark* m);

typedef struct var_struct * varp;

extern varp first_bound_var;
void prune_vars(varp old);

#endif







