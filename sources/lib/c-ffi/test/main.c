#include <stddef.h>
/* For ssize_t definition (POSIX extension) */
#ifndef OPEN_DYLAN_PLATFORM_WINDOWS
#include <sys/types.h>
#endif

#include "main.h"

unsigned long foo = 0xffff;
unsigned long failure_count = 0;
unsigned long test_count = 0;

void run_tests_from_c()
{
  printf("running c-ffi tests\n");
  run_c_tests();
  printf("running c-ffi tests from C\n");
  printf("finished running c-ffi tests from C\n");
  printf("running c-ffi tests from dylan\n");
  run_dylan_tests();
  printf("finished running c-ffi tests from dylan\n");
  printf("done c-ffi tests\n");
}

void petes_printf(char *str, long a1, long a2)
{
  printf(str, a1, a2);
}


void simplest_foreign_function()
{
  printf("calling simplest_foreign_function\n");
}

void ffi_no_result(unsigned short i)
{
  printf("calling ffi_no_result %d\n");
}

unsigned short ffi_no_parameters()
{
  printf("calling ffi_no_parameters\n");
  return(0);
}

unsigned char ffi1(unsigned char* p3)
{
  printf("calling ffi1\n");
  *p3 = 'A';
  return('B');
}

/*
 * check struct sizes
 */

int struct_minimal_size()
{
  printf("   minimal size: %d\n", sizeof(minimal));
  return sizeof(minimal);
}

int struct_maximal_size()
{
  printf("   maximal size: %d\n", sizeof(maximal));
  return sizeof(maximal);
}




/*
 * run the tests from C
 */


void run_c_tests()
{
  int i;
  signed long target = 1;

  printf("calling call_me_too\n");
  call_me_too();
  printf("called call_me_too\n");

  {
    minimal p6;
    unsigned char p3 = 3;
    unsigned char p4 = 4;
    unsigned char p5 = 5;
    unsigned char retval;

    p6.slot_1 = 171717;
    retval = call_me_more(1, 2, &p3, &p4, &p5, &p6);
    RUN_TEST((retval == 4),
               "return value from interesting callback: %d should be %d",
               retval, 4);
    RUN_TEST((p3 == 1),
               "output parameter 3 for interesting callback: %d should be %d",
               p3, 1);
    RUN_TEST((p4 == 2),
               "output parameter 4 for interesting callback: %d should be %d",
               p4, 2);
    RUN_TEST((p5 == 4),
             "output parameter 5 for interesting callback: %d should be %d",
             p5, 4);
    RUN_TEST((p6.slot_1 == 0xfffff00),
             "parameter 6 for interesting callback: 0x%x should be 0x%x",
             p6.slot_1, 0xfffff00);
  }

  for (i = 0; i < sizeof(signed long) * 8; i++, target = target << 1)
    {
      RUN_TEST((dylan_int_identity(target) == target),
               "signed callback dylan identity on %d",
               target, 0);
    }
  target = -1;
  for (i = 0; i < sizeof(signed long) * 8; i++, target = target << 1)
    {
      RUN_TEST((dylan_int_identity(target) == target),
               "negative signed callback identity on %d",
               target, 0);
    }
  RUN_TEST((dylan_always_one() == 1),
           "signed constant one callback", 0, 0);
}

size_t incr_size_t(size_t s)
{
  return (s + 1);
}

ssize_t incr_ssize_t(ssize_t s)
{
  return (s + 1);
}

unsigned long incr_unsigned_long(unsigned long i)
{
  return(i + 1);
}

signed long incr_signed_long(signed long i)
{
  return(i + 1);
}

unsigned int incr_unsigned_int(unsigned int i)
{
  return(i + 1);
}

signed int incr_signed_int(signed int i)
{
  return(i + 1);
}

unsigned short incr_unsigned_short(unsigned short i)
{
  return(i + 1);
}

signed short incr_signed_short(signed short i)
{
  return(i + 1);
}

unsigned char incr_unsigned_char(unsigned char i)
{
  return(i + 1);
}

signed char incr_signed_char(signed char i)
{
  return(i + 1);
}

char incr_char(char i)
{
  return(i + 1);
}

#define HENV void*

int SQLAllocEnv(HENV *h)
{
  *h = (HENV)malloc(16);
  return(37);
}

/* for bug 321 */

unsigned short union_tester_1(union_test_2 *arg)
{
  return(arg->union_struct_1.union_1);
}

void union_tester_1_setter(unsigned short new, union_test_2 *arg)
{
  arg->union_struct_1.union_1 = new;
}

signed short union_tester_2(union_test_2 *arg)
{
  return(arg->union_struct_1.union_2);
}

void union_tester_2_setter(signed short new, union_test_2 *arg)
{
  arg->union_struct_1.union_2 = new;
}

unsigned char union_tester_3(union_test_2 *arg)
{
  return(arg->union_struct_1.union_3);
}

void union_tester_3_setter(unsigned char new, union_test_2 *arg)
{
  arg->union_struct_1.union_3 = new;
}

signed char union_tester_4(union_test_2 *arg)
{
  return(arg->union_struct_1.union_4);
}

void union_tester_4_setter(signed char new, union_test_2 *arg)
{
  arg->union_struct_1.union_4 = new;
}

/* for bug 313 */

int mix_it_up(int *a)
{
  int val = *a;

  *a = val + val;
  return(val + 1);
}

/* for c-function-indirect test */

int a_function(int a)
{
  return(a);
}

int (*gimme_a_function())()
{
  return(a_function);
}


/* for bug 394  (bitfield slots in structs) */

unsigned short return_code_slot (DDEACK* dde)
{
  return(dde->bAppReturnCode);
}

unsigned short return_code_slot_set (unsigned short v, DDEACK* dde)
{
  dde->bAppReturnCode = v;
  return(dde->bAppReturnCode);
}


unsigned short reserved_slot (DDEACK* dde)
{
  return(dde->reserved);
}

unsigned short reserved_slot_set (unsigned short v, DDEACK* dde)
{
  dde->reserved = v;
  return(dde->reserved);
}


unsigned short busy_slot (DDEACK* dde)
{
  return(dde->fBusy);
}

unsigned short busy_slot_set (unsigned short v, DDEACK* dde)
{
  dde->fBusy = v;
  return(dde->fBusy);
}


unsigned short ack_slot (DDEACK* dde)
{
  return(dde->fAck);
}

unsigned short ack_slot_set (unsigned short v, DDEACK* dde)
{
  dde->fAck = v;
  return(dde->fAck);
}

/* ---
 * for struct by value
 */
unsigned short Vreturn_code_slot (DDEACK dde)
{
  return(dde.bAppReturnCode);
}

unsigned short Vreturn_code_slot_set (unsigned short v, DDEACK dde)
{
  dde.bAppReturnCode = v;
  return(dde.bAppReturnCode);
}


unsigned short Vreserved_slot (DDEACK dde)
{
  return(dde.reserved);
}

unsigned short Vreserved_slot_set (unsigned short v, DDEACK dde)
{
  dde.reserved = v;
  return(dde.reserved);
}


unsigned short Vbusy_slot (DDEACK dde)
{
  return(dde.fBusy);
}

unsigned short Vbusy_slot_set (unsigned short v, DDEACK dde)
{
  dde.fBusy = v;
  return(dde.fBusy);
}


unsigned short Vack_slot (DDEACK dde)
{
  return(dde.fAck);
}

unsigned short Vack_slot_set (unsigned short v, DDEACK dde)
{
  dde.fAck = v;
  return(dde.fAck);
}

long OMGW_width(OLEMENUGROUPWIDTHS strct, int index)
{
  return(strct.width[index]);
}

struct NVGcolor {
  struct {
    float r,g,b,a;
  };
};
typedef struct NVGcolor NVGcolor;

void process_nvg_color(NVGcolor color)
{
}
