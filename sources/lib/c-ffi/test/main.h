#include <stdio.h>
#include <stdlib.h>

void run_c_tests(void);


typedef struct sub
{
  unsigned long slot_1;
  unsigned long *slot_2;
} minimal;

typedef struct subminiature {
  char windows_dont_allow_empty_structs;
} really_minimal;

typedef struct max
{
  minimal max_1;
  unsigned char max_2;
  minimal max_3[7];
  /*
  unsigned max_4:4;
  unsigned max_5:12;
  */
  unsigned char max_6;
  unsigned char *max_7;
} maximal;
  

void simplest_foreign_function(void);
void ffi_no_result(unsigned short);
unsigned short ffi_no_parameters(void);
extern unsigned char ffi1(/* unsigned long p1, */
			  /* unsigned char p2, */
			  unsigned char *p3 /* , */
			  /* unsigned char *p4, */
			  /* minimal* p5, */
			  /* minimal* p6 */
			  );
extern void run_dylan_tests(void);


extern void call_me_too(void);
extern signed long dylan_int_identity(signed long);
extern signed long dylan_always_one(void);



extern unsigned char call_me_more(unsigned long p1,
				  unsigned char p2,
				  unsigned char* p3,
				  unsigned char* p4,
				  unsigned char* p5,
				  minimal* p6);

extern unsigned long foo;

extern unsigned long failure_count;
extern unsigned long test_count;

#define report_losing_test(str, v1, v2) \
  (printf("### losing test: " str "\n", (v1), (v2)), failure_count++)

#define RUN_TEST(expr, string, v1, v2) \
  (((expr)? 1 : (report_losing_test(string, (v1),(v2))), 1), test_count++)



/* for bug 321 */

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


/* for bug 394 */

    typedef struct {
     unsigned short bAppReturnCode:8,
       reserved:6,
       fBusy:1,
       fAck:1;
    } DDEACK;


/* for struct by value tests */


typedef struct {
  long width[6];
} OLEMENUGROUPWIDTHS;

typedef struct {
  void *obj_handle;
} test_dylan_object;

