#include <stddef.h>
#include <stdlib.h>

/* For ssize_t definition (POSIX extension) */
#ifndef OPEN_DYLAN_PLATFORM_WINDOWS
#include <sys/types.h>
#endif

unsigned char incr_unsigned_char(unsigned char i)
{
  return i + 1;
}

signed char incr_signed_char(signed char i)
{
  return i + 1;
}

unsigned short incr_unsigned_short(unsigned short i)
{
  return i + 1;
}

signed short incr_signed_short(signed short i)
{
  return i + 1;
}

size_t incr_size_t(size_t s)
{
  return s + 1;
}

ssize_t incr_ssize_t(ssize_t s)
{
  return s + 1;
}

// for c-function-indirect test

int a_function_0(void)
{
  return 5;
}

int (*gimme_a_function_0(void))(void)
{
  return a_function_0;
}

int a_function_1(int a)
{
  return a;
}

int (*gimme_a_function_1())(int a)
{
  return &a_function_1;
}

int a_function_2(int a, int b)
{
  return a + b;
}

int (*gimme_a_function_2(void))(int a, int b)
{
  return &a_function_2;
}

// for bug 217

typedef void *SQLHENV;

short SQLAllocEnv(SQLHENV *hp)
{
  *hp = (SQLHENV) malloc(16);
  return 37;
}

short SQLFreeEnv(SQLHENV h)
{
  free(h);
  return 37;
}

// for bug 321

typedef union Tunion_test1
{
  unsigned short union_1;
  signed short union_2;
  unsigned char union_3;
  signed char union_4;
} union_test;

typedef struct Tunion_test2
{
  char struct_pad_1;
  union_test union_struct_1;
  char struct_pad_2;
} union_test_2;


unsigned short union_tester_1(union_test_2 *arg)
{
  return arg->union_struct_1.union_1;
}

void union_tester_1_setter(unsigned short new, union_test_2 *arg)
{
  arg->union_struct_1.union_1 = new;
}

signed short union_tester_2(union_test_2 *arg)
{
  return arg->union_struct_1.union_2;
}

void union_tester_2_setter(signed short new, union_test_2 *arg)
{
  arg->union_struct_1.union_2 = new;
}

unsigned char union_tester_3(union_test_2 *arg)
{
  return arg->union_struct_1.union_3;
}

void union_tester_3_setter(unsigned char new, union_test_2 *arg)
{
  arg->union_struct_1.union_3 = new;
}

signed char union_tester_4(union_test_2 *arg)
{
  return arg->union_struct_1.union_4;
}

void union_tester_4_setter(signed char new, union_test_2 *arg)
{
  arg->union_struct_1.union_4 = new;
}

// for bug 313

int mix_it_up(int *a)
{
  int val = *a;

  *a = val + val;
  return val + 1;
}

