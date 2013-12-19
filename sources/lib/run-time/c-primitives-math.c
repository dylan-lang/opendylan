#include "run-time.h"

#include <math.h>
#include <stdlib.h>

#define ignore(x) (void)x

#if defined(__x86_64__)
#define NO_LONGLONG 1
#define LONG_BIT 64
#define WORD_BIT 32
#define LOHALF(x) (x & 0x00000000FFFFFFFFL)
#define HIHALF(x) (((unsigned long)x & 0xFFFFFFFF00000000L) >> 32)
#else
#define LONG_BIT 32
#endif

/* DYLAN CONSTANTS */
extern OBJECT KPfalseVKi;
extern OBJECT KPtrueVKi;

/* NUMBERS */

extern D MV2_(D, D);
extern D MV3_(D, D, D);

#define MV2(x,y) return((DMINT)MV2_((D)(x), (D)(y)))
#define MV2U(x,y) return((DUMINT)MV2_((D)(x), (D)(y)))
#define MV3(x,y,z) return((DMINT)MV3_((D)(x), (D)(y), (D)(z)))

extern Wrapper KLmachine_wordGVKeW;
extern Wrapper KLdouble_integerGVKeW;
extern Wrapper KLsingle_floatGVKdW;
extern Wrapper KLdouble_floatGVKdW;

typedef union {
  UINT32 i;
  FLT f;
} INTFLT;

D primitive_raw_as_single_float(DSFLT x) {
  return(primitive_allocate_filled
           (2, &KLsingle_floatGVKdW, 1,
            (D)primitive_cast_single_float_as_machine_word(x), 0, 0));
}

DMINT primitive_single_float_as_double_integer(DSFLT f) {
#ifdef NO_LONGLONG
  DMINT i = (DMINT)f;
  MV2((DMINT)i, (i < 0) ? (DMINT)-1 : (DMINT)0);
#else
  DLMINT i = (DLMINT)f;
  MV2((DMINT)i, (DMINT)(i >> LONG_BIT));
#endif
}

DSFLT primitive_double_integer_as_single_float(DMINT low, DMINT high) {
#ifdef NO_LONGLONG
  DSFLT fl = (DSFLT)(DUMINT)low;
  DSFLT fh = (DSFLT)((high < 0) ? 0 - high : high);
  DSFLT f = fl + fh * pow(2.0, (DDFLT)LONG_BIT);
  return((high < 0) ? 0.0 - f : f);
#else
  DLMINT i = ((DLMINT)high << LONG_BIT) | (DLMINT)(DUMINT)low;
  return((DSFLT)i);
#endif
}

DUMINT primitive_cast_single_float_as_machine_word(DSFLT x) {
  INTFLT intflt; intflt.f = x; return(intflt.i);
}

DSFLT primitive_cast_machine_word_as_single_float(DUMINT x) {
  INTFLT intflt; intflt.i = x; return(intflt.f);
}

typedef union {
  UINT64 i;
  DFLT f;
} INTDFLT;

D primitive_raw_as_double_float(DDFLT x) {
  D f = primitive_allocate_filled(3, &KLdouble_floatGVKdW, 0, (D)0, 0, 0);
  ((DDF)f)->data = x;
  return(f);
}

DMINT primitive_double_float_as_double_integer(DDFLT f) {
#ifdef NO_LONGLONG
  DMINT i = (DMINT)f;
  MV2((DMINT)i, (i < 0) ? (DMINT)-1 : (DMINT)0);
#else
  DLMINT i = (DLMINT)f;
  MV2((DMINT)i, (DMINT)(i >> LONG_BIT));
#endif
}

DDFLT primitive_double_integer_as_double_float(DMINT low, DMINT high) {
#ifdef NO_LONGLONG
  DDFLT fl = (DDFLT)(DUMINT)low;
  DDFLT fh = (DDFLT)((high < 0) ? 0 - high : high);
  DDFLT f = fl + fh * pow(2.0, (DDFLT)LONG_BIT);
  return((high < 0) ? 0.0 - f : f);
#else
  DLMINT i = ((DLMINT)high << LONG_BIT) | (DLMINT)(DUMINT)low;
  return((DDFLT)i);
#endif
}

DUMINT primitive_cast_double_float_as_machine_words(DDFLT x) {
  INTDFLT intflt;
  intflt.f = x;
#ifdef NO_LONGLONG
  MV2U((DUMINT)intflt.i, 0);
#else
  MV2U((DUMINT)intflt.i, (DUMINT)(intflt.i >> LONG_BIT));
#endif
}

DDFLT primitive_cast_machine_words_as_double_float(DUMINT low, DUMINT high) {
  INTDFLT intflt;
#ifdef NO_LONGLONG
  ignore(high);
  intflt.i = (DULMINT)low;
#else
  intflt.i = ((DULMINT)high << LONG_BIT) | (DULMINT)low;
#endif
  return(intflt.f);
}

/* MACHINE-WORD primitives */

D primitive_wrap_machine_word(DMINT x) {
  return(primitive_allocate_filled
           (2, &KLmachine_wordGVKeW, 1, (D)x, 0, 0));
}

/*---*** NOTE: This is wrong!  It should make a <double-integer> */
D primitive_wrap_abstract_integer(DMINT x) {
  if (R(I(x)) != x) {
    return(primitive_wrap_machine_word(x));
  } else {
    return(primitive_box_integer(x));
  }
}

/*---*** NOTE: This is wrong!  It should make a <double-integer> */
D primitive_wrap_unsigned_abstract_integer(DMINT x) {
  if (R(I(x)) != x) {
    return(primitive_wrap_machine_word(x));
  } else {
    return(primitive_box_integer(x));
  }
}

/*---*** NOTE: This is wrong!  It should unwrap a <double-integer> */
DMINT primitive_unwrap_abstract_integer(D x) {
  if (BOOLASRAW(primitive_integerQ(x))) {
    return(primitive_unbox_integer(x));
  } else {
    return(primitive_unwrap_machine_word(x));
  }
}

/*---*** NOTE: Here's the correct implementation of the above three functions */
#ifdef NOTYET
#define HIGH_BITS 0xC0000000L
#define HIGH_BITS_AND_SIGN 0xE0000000L

D primitive_wrap_abstract_integer(DMINT x) {
  DUMINT hs = (DUMINT)x & HIGH_BITS_AND_SIGN;
  if (hs != 0 && hs != HIGH_BITS_AND_SIGN) {
    xd = primitive_allocate_filled
           (3, &KLdouble_integerGVKeW, 2, (D)0, 0, 0);
    (DBI)xd->low = (DUMINT)x;
    /* Propagate the sign of x through the high word of the <double-integer> */
    (DBI)xd->high = (x < 0) ? -1 : 0;
    return(xd);
  } else {
    return(I(x));
  }
}

D primitive_wrap_unsigned_abstract_integer(DMINT x) {
  if ((DUMINT)x & HIGH_BITS != 0) {
    D xd = primitive_allocate_filled
             (3, &KLdouble_integerGVKeW, 2, (D)0, 0, 0);
    /* When x is treated as an unsigned value, the high word of the
       resulting <double-integer> will always be 0 */
    (DBI)xd->low = (DUMINT)x;
    return(xd);
  } else {
    return(I(x));
  }
}

DMINT primitive_unwrap_abstract_integer(D x) {
  if (BOOLASRAW(primitive_integerQ(x))) {
    return(R(x));
  } else {
    /* Native runtime will signal overflow if (DBI)x->high != 0 | != 1
       (See page 3 of "Integer and Machine integer primitives") */
    return((DBI)x->low);
  }
}
#endif

DMINT primitive_machine_word_divide(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  MV2((DMINT)z.quot, (DMINT)z.rem);
}

DMINT primitive_machine_word_floorS_quotient(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem > 0) : (z.rem < 0))) {
    z.quot--;
    z.rem += y;
  }
  return((DMINT)z.quot);
}
DMINT primitive_machine_word_floorS_remainder(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem > 0) : (z.rem < 0))) {
    z.quot--;
    z.rem += y;
  }
  return((DMINT)z.rem);
}
DMINT primitive_machine_word_floorS(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem > 0) : (z.rem < 0))) {
    z.quot--;
    z.rem += y;
  }
  MV2((DMINT)z.quot, (DMINT)z.rem);
}

DMINT primitive_machine_word_ceilingS_quotient(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem < 0) : (z.rem > 0))) {
    z.quot++;
    z.rem -= y;
  }
  return((DMINT)z.quot);
}
DMINT primitive_machine_word_ceilingS_remainder(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem < 0) : (z.rem > 0))) {
    z.quot++;
    z.rem -= y;
  }
  return((DMINT)z.rem);
}
DMINT primitive_machine_word_ceilingS(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem < 0) : (z.rem > 0))) {
    z.quot++;
    z.rem -= y;
  }
  MV2((DMINT)z.quot, (DMINT)z.rem);
}

DMINT primitive_machine_word_roundS_quotient(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  long threshold = labs(y) / 2;
  if ((z.rem > threshold) || ((z.rem == threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot--; z.rem += y; }
    else { z.quot++; z.rem -= y; }
  }
  else if ((z.rem < -threshold) || ((z.rem == -threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot++; z.rem -= y; }
    else { z.quot--; z.rem += y; }
  }
  return((DMINT)z.quot);
}
DMINT primitive_machine_word_roundS_remainder(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  long threshold = labs(y) / 2;
  if ((z.rem > threshold) || ((z.rem == threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot--; z.rem += y; }
    else { z.quot++; z.rem -= y; }
  }
  else if ((z.rem < -threshold) || ((z.rem == -threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot++; z.rem -= y; }
    else { z.quot--; z.rem += y; }
  }
  return((DMINT)z.rem);
}
DMINT primitive_machine_word_roundS(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  long threshold = labs(y) / 2;
  if ((z.rem > threshold) || ((z.rem == threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot--; z.rem += y; }
    else { z.quot++; z.rem -= y; }
  }
  else if ((z.rem < -threshold) || ((z.rem == -threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot++; z.rem -= y; }
    else { z.quot--; z.rem += y; }
  }
  MV2((DMINT)z.quot, (DMINT)z.rem);
}

DMINT primitive_machine_word_truncateS_quotient(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  return((DMINT)z.quot);
}
DMINT primitive_machine_word_truncateS_remainder(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  return((DMINT)z.rem);
}
DMINT primitive_machine_word_truncateS(DMINT x, DMINT y) {
  ldiv_t z = ldiv(x, y);
  MV2((DMINT)z.quot, (DMINT)z.rem);
}

static void divide_double (DMINT xl, DMINT xh, DMINT y, DMINT* q, DMINT* r) {
#ifdef NO_LONGLONG
  DMINT dividend = ((DMINT)LOHALF(xh) << WORD_BIT) + (DMINT)LOHALF(xl);
  DMINT divisor = y;
  *q = dividend / divisor;
  *r = dividend % divisor;
  if (xh < 0 && xl > 0) {
    *q = -(*q);
    *r = -(*r);
  }
#else
  DLMINT dividend = ((DLMINT)xh << LONG_BIT) | (DLMINT)(DUMINT)xl;
  DLMINT divisor = (DLMINT)y;
  *q = (DMINT)(dividend / divisor);
  *r = (DMINT)(dividend % divisor);
#endif
  return;
}

static void unsigned_divide_double (DMINT xl, DMINT xh, DMINT y, DUMINT* q, DUMINT* r) {
#ifdef NO_LONGLONG
  ignore(xh);
  DUMINT dividend = ((DUMINT)LOHALF(xh) << WORD_BIT) + (DUMINT)LOHALF(xl);
  DUMINT divisor = (DUMINT)y;
  *q = dividend / divisor;
  *r = dividend % divisor;
#else
  DULMINT dividend = ((DULMINT)(DUMINT)xh << LONG_BIT) | (DULMINT)(DUMINT)xl;
  DULMINT divisor = (DULMINT)(DUMINT)y;
  *q = (DUMINT)(dividend / divisor);
  *r = (DUMINT)(dividend % divisor);
#endif
  return;
}

DMINT primitive_machine_word_double_floorS_quotient(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r > 0) : (r < 0))) {
    q--;
    r += y;
  }
  return(q);
}
DMINT primitive_machine_word_double_floorS_remainder(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r > 0) : (r < 0))) {
    q--;
    r += y;
  }
  return(r);
}
DMINT primitive_machine_word_double_floorS(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r > 0) : (r < 0))) {
    q--;
    r += y;
  }
  MV2(q, r);
}

DMINT primitive_machine_word_double_ceilingS_quotient(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r < 0) : (r > 0))) {
    q++;
    r -= y;
  }
  return(q);
}
DMINT primitive_machine_word_double_ceilingS_remainder(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r < 0) : (r > 0))) {
    q++;
    r -= y;
  }
  return(r);
}
DMINT primitive_machine_word_double_ceilingS(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r < 0) : (r > 0))) {
    q++;
    r -= y;
  }
  MV2(q, r);
}

DMINT primitive_machine_word_double_roundS_quotient(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  long threshold = labs(y) / 2;
  divide_double(xl, xh, y, &q, &r);
  if ((r > threshold) || ((r == threshold) && (q & 1))) {
    if (y < 0) { q--; r += y; }
    else { q++; r -= y; }
  }
  else if ((r < -threshold) || ((r == -threshold) && (q & 1))) {
    if (y < 0) { q++; r -= y; }
    else { q--; r += y; }
  }
  return(q);
}
DMINT primitive_machine_word_double_roundS_remainder(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  long threshold = labs(y) / 2;
  divide_double(xl, xh, y, &q, &r);
  if ((r > threshold) || ((r == threshold) && (q & 1))) {
    if (y < 0) { q--; r += y; }
    else { q++; r -= y; }
  }
  else if ((r < -threshold) || ((r == -threshold) && (q & 1))) {
    if (y < 0) { q++; r -= y; }
    else { q--; r += y; }
  }
  return(r);
}
DMINT primitive_machine_word_double_roundS(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  long threshold = labs(y) / 2;
  divide_double(xl, xh, y, &q, &r);
  if ((r > threshold) || ((r == threshold) && (q & 1))) {
    if (y < 0) { q--; r += y; }
    else { q++; r -= y; }
  }
  else if ((r < -threshold) || ((r == -threshold) && (q & 1))) {
    if (y < 0) { q++; r -= y; }
    else { q--; r += y; }
  }
  MV2(q, r);
}

DMINT primitive_machine_word_double_truncateS_quotient(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  return(q);
}
DMINT primitive_machine_word_double_truncateS_remainder(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  return(r);
}
DMINT primitive_machine_word_double_truncateS(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  MV2(q, r);
}

DMINT primitive_machine_word_double_divide_quotient(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  return(q);
}
DMINT primitive_machine_word_double_divide_remainder(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  return(r);
}
DMINT primitive_machine_word_double_divide(DMINT xl, DMINT xh, DMINT y) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  MV2(q, r);
}

#ifndef OPEN_DYLAN_COMPILER_GCC_LIKE
DMINT primitive_machine_word_count_low_zeros(DMINT x) {
  if (x == 0) return(DMINT)(primitive_word_size() * 8);
  DMINT mask4 = (DMINT)0xF;
  int index = (int)(mask4 & x);
  int count = 0;
  static int t[16] = { 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0 };

  /* scan for a non-zero low nibble. */
  for (; index == 0; count += 4, x >>= 4, index = (int)(mask4 & x)) {}
  return(DMINT)(count + t[index]);
}

DMINT primitive_machine_word_count_high_zeros(DMINT x) {
  if (x == 0) return(DMINT)(primitive_word_size() * 8);
  DUMINT ux = (DUMINT)x;
  DUMINT mask4 = ((DUMINT)0xF) << (primitive_word_size() * 8 - 4);
  DUMINT uindex = mask4 & ux;
  int count = 0;
  static int t[16] = { 4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 };

  /* scan for non-zero high nibble. */
  for (; uindex == (DUMINT)0; count += 4, ux <<= 4, uindex = mask4 & ux) {}
  int index = (int)(uindex >> (primitive_word_size() * 8 - 4));
  return(DMINT)(count + t[index]);
}
#endif

static void multiply_double (DMINT x, DMINT y, DUMINT* zl, DUMINT* zh) {
#ifdef NO_LONGLONG
  DUMINT xl = (DUMINT)LOHALF(x);
  DUMINT xh = (DUMINT)HIHALF(x);
  DUMINT yl = (DUMINT)LOHALF(y);
  DUMINT yh = (DUMINT)HIHALF(y);
  DUMINT zll = xl * yl;
  DUMINT zlh = xl * yh;
  DUMINT zhl = xh * yl;
  DUMINT zhh = xh * yh;
  *zl = zll + ((LOHALF(zlh) + LOHALF(zhl)) << WORD_BIT);
  *zh = zhh + HIHALF(zlh) + HIHALF(zhl);
#else
  DULMINT z = (DULMINT)(DUMINT)x * (DULMINT)(DUMINT)y;
  *zl = (DUMINT)z;
  *zh = (DUMINT)(z >> LONG_BIT);
#endif
  return;
}

DMINT primitive_machine_word_add_with_overflow(DMINT x, DMINT y) {
  DMINT r = (DMINT)((DUMINT)x + (DUMINT)y);
  /* Overflow if signs of inputs are the same but different from sign of result ... */
  MV2(r, RAWASBOOL(((x ^ y) >= 0) && ((r ^ x) < 0)));
}
DMINT primitive_machine_word_subtract_with_overflow(DMINT x, DMINT y) {
  DMINT r = (DMINT)((DUMINT)x - (DUMINT)y);
  /* Overflow if signs of inputs differ and sign of result isn't sign of X ... */
  MV2(r, RAWASBOOL(((x ^ y) < 0) && ((r ^ x) < 0)));
}
DMINT primitive_machine_word_multiply_with_overflow(DMINT x, DMINT y) {
  DUMINT rl, rh;
  multiply_double(x, y, &rl, &rh);
  /* Overflow if sign of result is wrong or ? ... */
  MV3(rl, rh, RAWASBOOL(((x ^ y) < 0) ? ((DMINT)rh >= 0) : ((DMINT)rh < 0)));
}
DMINT primitive_machine_word_negative_with_overflow(DMINT x) {
  DMINT r = - x;
  /* Overflow if input was negative and result is negative or zero ... */
  MV2(r, RAWASBOOL(x < 0 && r <= 0));
}
DMINT primitive_machine_word_abs_with_overflow(DMINT x) {
  DMINT r = labs(x);
  /* Overflow if input was negative and result is negative or zero ... */
  MV2(r, RAWASBOOL(x < 0 && r <= 0));
}
DMINT primitive_machine_word_shift_left_with_overflow(DMINT x, DMINT y) {
  MV2(primitive_machine_word_shift_left_low(x, y), 0);
}

DMINT primitive_machine_word_multiply_high(DMINT x, DMINT y) {
  DUMINT zl, zh;
  multiply_double(x, y, &zl, &zh);
  return((DMINT)zh);
}
DMINT primitive_machine_word_multiply_lowShigh(DMINT x, DMINT y) {
  DUMINT zl, zh;
  multiply_double(x, y, &zl, &zh);
  MV2((DMINT)zl, (DMINT)zh);
}
DMINT primitive_machine_word_multiply_low_with_overflow(DMINT x, DMINT y) {
  DMINT r = x * y;
  /* Overflow if result has wrong sign or is smaller than inputs ... */
  MV2(r, RAWASBOOL((((x ^ y) < 0) ? r >= 0 : r < 0) || (labs(r) < labs(x)) || (labs(r) < labs(y))));
}

DMINT primitive_machine_word_unsigned_add_with_carry(DMINT x, DMINT y) {
  DUMINT ux = (DUMINT)x;
  DUMINT uy = (DUMINT)y;
  DUMINT uz = ux + uy;
  DUMINT bbc = ((ux & 1) && (uy & 1)) ? (DUMINT)1 : (DUMINT)0;
  MV2(uz, ((DMINT)((ux >> 1) + (uy >> 1) + bbc) < 0) ? (DMINT)1 : (DMINT)0);
}
DMINT primitive_machine_word_unsigned_subtract_with_borrow(DMINT x, DMINT y) {
  DUMINT ux = (DUMINT)x;
  DUMINT uy = (DUMINT)y;
  DUMINT uz = ux - uy;
  MV2(uz, (uy > ux) ? (DMINT)1 : (DMINT)0);
}
DMINT primitive_machine_word_unsigned_multiply_high(DMINT x, DMINT y) {
  DUMINT zl, zh;
  multiply_double(x, y, &zl, &zh);
  return((DMINT)zh);
}
DMINT primitive_machine_word_unsigned_multiply(DMINT x, DMINT y) {
  DUMINT zl, zh;
  multiply_double(x, y, &zl, &zh);
  MV2(zl, zh);
}
DMINT primitive_machine_word_unsigned_divide(DMINT x, DMINT y) {
  DUMINT q, r;
  unsigned_divide_double(x, (x < 0) ? -1 : 0, y, &q, &r);
  MV2(q, r);
}
DMINT primitive_machine_word_unsigned_rotate_left(DMINT x, DMINT y) {
  DUMINT low = (DUMINT)x >> (LONG_BIT - y);
  DUMINT high = (DUMINT)x << y;
  return((DMINT)(low | high));
}
DMINT primitive_machine_word_unsigned_rotate_right(DMINT x, DMINT y) {
  DUMINT high = (DUMINT)x << (LONG_BIT - y);
  DUMINT low = (DUMINT)x >> y;
  return((DMINT)(low | high));
}

DMINT primitive_machine_word_unsigned_double_divide_quotient(DMINT xl, DMINT xh, DMINT y) {
  DUMINT q, r;
  unsigned_divide_double(xl, xh, y, &q, &r);
  return((DMINT)q);
}
DMINT primitive_machine_word_unsigned_double_divide_remainder(DMINT xl, DMINT xh, DMINT y) {
  DUMINT q, r;
  unsigned_divide_double(xl, xh, y, &q, &r);
  return((DMINT)r);
}
DMINT primitive_machine_word_unsigned_double_divide(DMINT xl, DMINT xh, DMINT y) {
  DUMINT q, r;
  unsigned_divide_double(xl, xh, y, &q, &r);
  MV2(q, r);
}

DMINT primitive_machine_word_unsigned_shift_left_high(DMINT x, DMINT y) {
  return((DMINT)((DUMINT)x >> (LONG_BIT - y)));
}
DMINT primitive_machine_word_unsigned_double_shift_left_high(DMINT xl, DMINT xh, DMINT y) {
  DUMINT lowpart = (DUMINT)xl >> (LONG_BIT - y);
  DUMINT highpart = (DUMINT)xh << y;
  return((DMINT)(lowpart | highpart));
}
DMINT primitive_machine_word_unsigned_double_shift_left(DMINT xl, DMINT xh, DMINT y) {
  DUMINT lowpart = (DUMINT)xl >> (LONG_BIT - y);
  DUMINT highpart = (DUMINT)xh << y;
  MV2((DUMINT)xl <<  y, lowpart | highpart);
}
DMINT primitive_machine_word_unsigned_double_shift_right_low(DMINT xl, DMINT xh, DMINT y) {
  DUMINT lowpart = (DUMINT)xl >> y;
  DUMINT highpart = (DUMINT)xh << (LONG_BIT - y);
  return((DMINT)(lowpart | highpart));
}
DMINT primitive_machine_word_unsigned_double_shift_right_high(DMINT xl, DMINT xh, DMINT y) {
  ignore(xl);
  return((DMINT)((DUMINT)xh >> y));
}
DMINT primitive_machine_word_unsigned_double_shift_right(DMINT xl, DMINT xh, DMINT y) {
  DUMINT lowpart = (DUMINT)xl >> y;
  DUMINT highpart = (DUMINT)xh << (LONG_BIT - y);
  MV2(lowpart | highpart, (DUMINT)xh >> y);
}

/* additions to run-time.c specific to handling pass-by-reference of non-first
   return values of primitives   (gts,9/97) */

extern D MV2_byref_(D, DMINT*, DMINT);
extern D MV3_byref_(D, DMINT*, DMINT, DMINT*, DMINT);

#define MV2_byref(x,v,y) return((DMINT)MV2_byref_((D)(x), (DMINT*)(v), (DMINT)(y)))
#define MV2_byrefU(x,v,y) return((DUMINT)MV2_byref_((D)(x), (DMINT*)(v), (DMINT)(y)))
#define MV3_byref(x,v1,y,v2,z) return((DMINT)MV3_byref_((D)(x), (DMINT*)(v1), (DMINT)(y), (DMINT*)(v2), (DMINT)(z)))

DMINT primitive_single_float_as_double_integer_byref(DSFLT f, DMINT* v2) {
#ifdef NO_LONGLONG
  DMINT i = (DMINT)f;
  MV2_byref((DMINT)i, v2, (i < 0) ? (DMINT)-1 : (DMINT)0);
#else
  DLMINT i = (DLMINT)f;
  MV2_byref((DMINT)i, v2, (DMINT)(i >> LONG_BIT));
#endif
}

DMINT primitive_double_float_as_double_integer_byref(DDFLT f, DMINT* v2) {
#ifdef NO_LONGLONG
  DMINT i = (DMINT)f;
  MV2_byref((DMINT)i, v2, (i < 0) ? (DMINT)-1 : (DMINT)0);
#else
  DLMINT i = (DLMINT)f;
  MV2_byref((DMINT)i, v2, (DMINT)(i >> LONG_BIT));
#endif
}

DMINT primitive_cast_double_float_as_machine_words_byref(DDFLT x, DMINT* v2) {
  INTDFLT intflt;
  intflt.f = x;
#ifdef NO_LONGLONG
  MV2_byref((DMINT)intflt.i, v2, 0);
#else
  MV2_byref((DMINT)intflt.i, v2, (DMINT)(intflt.i >> LONG_BIT));
#endif
}

DMINT primitive_machine_word_divide_byref(DMINT x, DMINT y, DMINT* v2) {
  ldiv_t z = ldiv(x, y);
  MV2_byref((DMINT)z.quot, v2, (DMINT)z.rem);
}

DMINT primitive_machine_word_floorS_byref(DMINT x, DMINT y, DMINT* v2) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem > 0) : (z.rem < 0))) {
    z.quot--;
    z.rem += y;
  }
  MV2_byref((DMINT)z.quot, v2, (DMINT)z.rem);
}

DMINT primitive_machine_word_ceilingS_byref(DMINT x, DMINT y, DMINT* v2) {
  ldiv_t z = ldiv(x, y);
  if (z.rem && ((y < 0) ? (z.rem < 0) : (z.rem > 0))) {
    z.quot++;
    z.rem -= y;
  }
  MV2_byref((DMINT)z.quot, v2, (DMINT)z.rem);
}

DMINT primitive_machine_word_roundS_byref(DMINT x, DMINT y, DMINT* v2) {
  ldiv_t z = ldiv(x, y);
  long threshold = labs(y) / 2;
  if ((z.rem > threshold) || ((z.rem == threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot--; z.rem += y; }
    else { z.quot++; z.rem -= y; }
  }
  else if ((z.rem < -threshold) || ((z.rem == -threshold) && (z.quot & 1))) {
    if (y < 0) { z.quot++; z.rem -= y; }
    else { z.quot--; z.rem += y; }
  }
  MV2_byref((DMINT)z.quot, v2, (DMINT)z.rem);
}

DMINT primitive_machine_word_truncateS_byref(DMINT x, DMINT y, DMINT* v2) {
  ldiv_t z = ldiv(x, y);
  MV2_byref((DMINT)z.quot, v2, (DMINT)z.rem);
}

DMINT primitive_machine_word_double_floorS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r > 0) : (r < 0))) {
    q--;
    r += y;
  }
  MV2_byref(q, v2, r);
}

DMINT primitive_machine_word_double_ceilingS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  if (r && ((y < 0) ? (r < 0) : (r > 0))) {
    q++;
    r -= y;
  }
  MV2_byref(q, v2, r);
}

DMINT primitive_machine_word_double_roundS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DMINT q, r;
  long threshold = labs(y) / 2;
  divide_double(xl, xh, y, &q, &r);
  if ((r > threshold) || ((r == threshold) && (q & 1))) {
    if (y < 0) { q--; r += y; }
    else { q++; r -= y; }
  } else if ((r < -threshold) || ((r == -threshold) && (q & 1))) {
    if (y < 0) { q++; r -= y; }
    else { q--; r += y; }
  }
  MV2_byref(q, v2, r);
}

DMINT primitive_machine_word_double_truncateS_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  MV2_byref(q, v2, r);
}

DMINT primitive_machine_word_double_divide_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DMINT q, r;
  divide_double(xl, xh, y, &q, &r);
  MV2_byref(q, v2, r);
}

DMINT primitive_machine_word_add_with_overflow_byref(DMINT x, DMINT y, D* v2) {
  DMINT r = (DMINT)((DUMINT)x + (DUMINT)y);
  /* Overflow if signs of inputs are the same but different from sign of result ... */
  MV2_byref(r, v2, RAWASBOOL(((x ^ y) >= 0) && ((r ^ x) < 0)));
}

DMINT primitive_machine_word_subtract_with_overflow_byref(DMINT x, DMINT y, D* v2) {
  DMINT r = (DMINT)((DUMINT)x - (DUMINT)y);
  /* Overflow if signs of inputs differ and sign of result isn't sign of X ... */
  MV2_byref(r, v2, RAWASBOOL(((x ^ y) < 0) && ((r ^ x) < 0)));
}

DMINT primitive_machine_word_multiply_with_overflow_byref(DMINT x, DMINT y, DMINT* v2, D* v3) {
  DUMINT rl, rh;
  multiply_double(x, y, &rl, &rh);
  /* Overflow if sign of result is wrong or ? ... */
  MV3_byref(rl, v2, rh, v3, RAWASBOOL(((x ^ y) < 0) ? ((DMINT)rh >= 0) : ((DMINT)rh < 0)));
}

DMINT primitive_machine_word_negative_with_overflow_byref(DMINT x, D* v2) {
  DMINT r = - x;
  /* Overflow if input was negative and result is negative or zero ... */
  MV2_byref(r, v2, RAWASBOOL(x < 0 && r <= 0));
}

DMINT primitive_machine_word_abs_with_overflow_byref(DMINT x, D* v2) {
  DMINT r = labs(x);
  /* Overflow if input was negative and result is negative or zero ... */
  MV2_byref(r, v2, RAWASBOOL(x < 0 && r <= 0));
}

DMINT primitive_machine_word_shift_left_with_overflow_byref(DMINT x, DMINT y, DMINT* v2, D* v3) {
  /* was: MV2_byref(primitive_machine_word_shift_left_low(x, y), v2, 0); */
  MV3_byref(primitive_machine_word_shift_left_low(x, y), v2, 0, v3, RAWASBOOL(0));
}

DMINT primitive_machine_word_multiply_lowShigh_byref(DMINT x, DMINT y, DMINT* v2) {
  DUMINT zl, zh;
  multiply_double(x, y, &zl, &zh);
  MV2_byref((DMINT)zl, v2, (DMINT)zh);
}

DMINT primitive_machine_word_multiply_low_with_overflow_byref(DMINT x, DMINT y, D* v2) {
  DMINT r = x * y;
  /* Overflow if result has wrong sign or is smaller than inputs ... */
  MV2_byref(r, v2, RAWASBOOL((((x ^ y) < 0) ? r >= 0 : r < 0) || (labs(r) < labs(x)) || (labs(r) < labs(y))));
}

DMINT primitive_machine_word_unsigned_add_with_carry_byref(DMINT x, DMINT y, DMINT* v2) {
  DUMINT ux = (DUMINT)x;
  DUMINT uy = (DUMINT)y;
  DUMINT uz = ux + uy;
  DUMINT bbc = ((ux & 1) && (uy & 1)) ? (DUMINT)1 : (DUMINT)0;
  MV2_byref(uz, v2, ((DMINT)((ux >> 1) + (uy >> 1) + bbc) < 0) ? (DMINT)1 : (DMINT)0);
}

DMINT primitive_machine_word_unsigned_subtract_with_borrow_byref(DMINT x, DMINT y, DMINT* v2) {
  DUMINT ux = (DUMINT)x;
  DUMINT uy = (DUMINT)y;
  DUMINT uz = ux - uy;
  MV2_byref(uz, v2, (uy > ux) ? (DMINT)1 : (DMINT)0);
}

DMINT primitive_machine_word_unsigned_multiply_byref(DMINT x, DMINT y, DMINT* v2) {
  DUMINT zl, zh;
  multiply_double(x, y, &zl, &zh);
  MV2_byref(zl, v2, zh);
}

DMINT primitive_machine_word_unsigned_divide_byref(DMINT x, DMINT y, DMINT* v2) {
  DUMINT q, r;
  unsigned_divide_double(x, (x < 0) ? -1 : 0, y, &q, &r);
  MV2_byref(q, v2, r);
}

DMINT primitive_machine_word_unsigned_double_divide_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DUMINT q, r;
  unsigned_divide_double(xl, xh, y, &q, &r);
  MV2_byref(q, v2, r);
}

DMINT primitive_machine_word_unsigned_double_shift_left_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DUMINT lowpart = (DUMINT)xl >> (LONG_BIT - y);
  DUMINT highpart = (DUMINT)xh << y;
  MV2_byref((DUMINT)xl <<  y, v2, lowpart | highpart);
}

DMINT primitive_machine_word_unsigned_double_shift_right_byref(DMINT xl, DMINT xh, DMINT y, DMINT* v2) {
  DUMINT lowpart = (DUMINT)xl >> y;
  DUMINT highpart = (DUMINT)xh << (LONG_BIT - y);
  MV2_byref(lowpart | highpart, v2, (DUMINT)xh >> y);
}



D MV2_byref_ (D x, DMINT* v, DMINT y) {
  *v = y;
  return x;
}

D MV3_byref_ (D x, DMINT* v1, DMINT y, DMINT* v2, DMINT z) {
  *v1 = y;
  *v2 = z;
  return x;
}

