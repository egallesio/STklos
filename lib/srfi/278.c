
#include "stklos.h"
#include <math.h>
#include "278-incl.c"

// FIXME: should go to stklos.h?
#define TYPEOF(n)                (INTP(n)? tc_integer: STYPE(n))

void error_not_a_number(SCM x) {
  STk_error("bad number ~s", x);
}

void error_not_a_real_number(SCM x) {
  STk_error("bad real number ~s", x);
}

EXTERN_PRIMITIVE("negative?", negativep, subr1, (SCM n));
EXTERN_PRIMITIVE("truncate", truncate, subr1, (SCM x));


DEFINE_PRIMITIVE("conjugate", conjugate, subr1, (SCM z)) {
  if (COMPLEXP(z)) return STk_make_rectangular(COMPLEX_REAL(z),
                                           STk_mul2(MAKE_INT(-1), COMPLEX_IMAG(z)));
  if (INTP(z) || BIGNUMP(z) || REALP(z) || RATIONALP(z))
    return z;
  error_not_a_number(z);
  return STk_false; /* Never reached */
}

DEFINE_PRIMITIVE("round-away", round_away, subr1, (SCM x)) {
  switch (TYPEOF(x)) {
    case tc_real:     {
                      if (isinf(REAL_VAL(x)) || isnan(REAL_VAL(x))) return x;
                        return REAL_VAL(x) < 0
                          ? STk_double2real(trunc(REAL_VAL(x) - 0.5))
                          : STk_double2real(trunc(REAL_VAL(x) + 0.5));
                      }
    case tc_rational: return STk_negativep(x)
                               /* FIXME: calling div2 is too slow. We should be able to
                                  make a rational here... */
                               ? STk_truncate(STk_add2(x, STk_div2(MAKE_INT(1), MAKE_INT(2))))
                               : STk_truncate(STk_sub2(x, STk_div2(MAKE_INT(1), MAKE_INT(2))));
    case tc_bignum:
    case tc_integer:  return x;
    default:          error_not_a_real_number(x);
  }
  return STk_void; /* never reached */
}

DEFINE_PRIMITIVE("imaginary?", imaginaryp, subr1, (SCM obj)) {
  return MAKE_BOOLEAN(COMPLEXP(obj) && MAKE_INT(0) == COMPLEX_REAL(obj));
}

DEFINE_PRIMITIVE("ordered?", orderedp, subr2, (SCM x1, SCM x2)) {
  if (!NUMBERP(x1)) error_not_a_real_number(x1);
  if (!NUMBERP(x2)) error_not_a_real_number(x2);
  return MAKE_BOOLEAN(!((REALP(x1) && isnan(REAL_VAL(x1))) ||
                        (REALP(x2) && isnan(REAL_VAL(x2)))));
}

DEFINE_PRIMITIVE("unordered?", unorderedp, subr2, (SCM x1, SCM x2)) {
  return MAKE_BOOLEAN(STk_orderedp(x1, x2) == STk_false);
}

DEFINE_PRIMITIVE("sign-negative?", sign_negativep, subr1, (SCM x)) {
  switch (TYPEOF(x)) {
  case tc_integer:  return MAKE_BOOLEAN(INT_VAL(x) < 0);
  case tc_bignum:   return STk_negativep(x);
  case tc_rational: return STk_sign_negativep(RATIONAL_NUM(x));
  case tc_real:     return MAKE_BOOLEAN(signbit(REAL_VAL(x)));
  case tc_complex:  return (STk_sign_negativep(COMPLEX_REAL(x)) == STk_false) &&
                           (STk_sign_negativep(COMPLEX_REAL(x)) == STk_false)
                             ? STk_false : STk_true;
  default:          return STk_false;
  }
}

DEFINE_PRIMITIVE("!=", not_equalp, vsubr, (int argc, SCM *argv)) {
  /* No args   => false
     All equal => false
     Any NaN   => false
     Else      => true   */
  if (argc==0) return STk_false;

  SCM *ptr = argv;
  SCM *prev = argv;

  if (REALP(*ptr) && isnan(REAL_VAL(*ptr))) return STk_false;

  ptr--;

  for(int i=1; i<argc; i++) {
    if (REALP(*ptr) && isnan(REAL_VAL(*ptr))) return STk_false;
    // FIXME: numeq? do we consider 1 == 1.0 ?
    if (!STk_numeq2(*ptr, *prev)) return STk_true;
    prev = ptr;
    ptr--;
  }
  return STk_false;
}


/*===========================================================================* \
 *
 *  Module for SRFI-278
 *
\*===========================================================================*/

MODULE_ENTRY_START("srfi/278")
{
  SCM module =  STk_create_module(STk_intern("srfi/278"));


  ADD_PRIMITIVE_IN_MODULE(conjugate, module);
  ADD_PRIMITIVE_IN_MODULE(round_away, module);
  ADD_PRIMITIVE_IN_MODULE(imaginaryp, module);
  ADD_PRIMITIVE_IN_MODULE(orderedp, module);
  ADD_PRIMITIVE_IN_MODULE(unorderedp, module);
  ADD_PRIMITIVE_IN_MODULE(sign_negativep, module);
  ADD_PRIMITIVE_IN_MODULE(not_equalp, module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
