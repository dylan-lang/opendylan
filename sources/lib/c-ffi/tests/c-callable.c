extern void simple_callable(void);

typedef struct sub
{
  unsigned long slot_1;
  unsigned long *slot_2;
} minimal;

extern unsigned char more_interesting_callable
    (unsigned long p1,
     unsigned char p2,
     unsigned char* p3,
     unsigned char* p4,
     unsigned char* p5,
     minimal* p6);

extern void dylan_check(const char *description, long want, long got);

extern signed long dylan_int_identity(signed long);
extern signed long dylan_always_one(void);

void callback_tests()
{
  int i;

  simple_callable();

  {
    minimal p6;
    unsigned char p3 = 3;
    unsigned char p4 = 4;
    unsigned char p5 = 5;
    unsigned char retval;

    p6.slot_1 = 171717;
    retval = more_interesting_callable(1, 2, &p3, &p4, &p5, &p6);
    dylan_check("return value from interesting callback", 4, retval);
    dylan_check("output parameter 3 for interesting callback", 1, p3);
    dylan_check("output parameter 4 for interesting callback", 2, p4);
    dylan_check("output parameter 5 for interesting callback", 4, p5);
    dylan_check("parameter 6 for interesting callback", 0xfffff00, p6.slot_1);
  }

  signed long target = 1;
  for (i = 0; i < sizeof(signed long) * 8; i++, target = target << 1) {
    dylan_check("signed callback dylan identity",
                target, dylan_int_identity(target));
  }
  target = -1;
  for (i = 0; i < sizeof(signed long) * 8; i++, target = target << 1) {
    dylan_check("negative signed callback dylan identity",
                target, dylan_int_identity(target));
  }
  dylan_check("signed constant one callback", 1, dylan_always_one());
}
