/*
 *  flonum.c         -- Implementation of (scheme flonum) aka SRFI-144
 *
 *  Copyright © 2020 Jeronimo Pellegrini - <j_p@aleph0.info>
 *
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 *  USA.
 *
 *            Author: Jeronimo Pellegrini [j_p@aleph0.info]
 *     Creation date: 03-Dec-2021 00:00 (jpellegrini)
 *
 */

#include <stklos.h>
#include <gmp.h>
#include <math.h>
#include <float.h>

#include "flonum-incl.c"

/* FIXME: will fail to compile if NAN is not supported.
   we should generate a NaN by some other means in that case. */
#ifdef NAN
static SCM SCM_NaN;
#endif

#ifdef Android
/* The lgamma_r function is not standard and not defined on Android.
 * However, it seems to be present in the linked libm. Declare it here.
 */
 double lgamma_r(double x, int *signp);
#endif

static inline SCM double2real(double x)
{
  SCM z;

  NEWCELL(z, real);
  REAL_VAL(z) = x;
  return z;
}


#if CONTROL_FX_PARAMETERS == 1

static void error_bad_fixnum1(SCM obj)
{
  STk_error("expected fixnum, found ~S", obj);
}

static void error_bad_flonum(SCM obj)
{
  STk_error("expected flonum, found ~S", obj);
}

static void error_bad_flonum2(SCM obj1, SCM obj2)
{
  STk_error("expected two flonums, found ~S and ~S", obj1, obj2);
}

static void error_bad_flonum3(SCM obj1, SCM obj2, SCM obj3)
{
  STk_error("expected flonum, found ~S, ~S and ~S", obj1, obj2, obj3);
}

#define ensure_fx(x) {        \
  if (!INTP(x))               \
    error_bad_fixnum1(x);     \
}

#define ensure_fl(x) {        \
  if (!REALP(x))              \
    error_bad_flonum(x);     \
}
#define ensure_fl2(x,y) {      \
  if (!(REALP(x) && REALP(y))) \
    error_bad_flonum2(x,y);    \
}
#define ensure_fl3(x,y,z) {                \
  if (!(REALP(x) && REALP(y) && REALP(z))) \
    error_bad_flonum3(x,y,z);              \
}
#else
#define ensure_fx(x)           {}
#define ensure_fl(x)           {}
#define ensure_fl2(x, y)       {}
#define ensure_fl3(x, y, z)    {}
#endif

/**************************\
 * MATHEMATICAL CONSTANTS *  DONE
\**************************/
/****************************\
 * IMPLEMENTATION CONSTANTS *  DONE
\****************************/

#define DEFCONST(name,val,module) STk_define_variable(STk_intern(name),val,module);
#define DEFLOCONST(name,val,module) STk_define_variable(STk_intern(name),double2real(val),module);

void STk_srfi_144_define_constants(SCM module) {
    DEFLOCONST("fl-e", M_E, module);
    DEFLOCONST("fl-1/e", 1.0/M_E, module);
    DEFLOCONST("fl-e-2",  7.389056098930650227230427, module);
    DEFLOCONST("fl-e-pi/4", exp(M_PI/4), module);
    DEFLOCONST("fl-log2-e",M_LOG2E, module);
    DEFLOCONST("fl-log10-e",M_LOG10E, module);
    DEFLOCONST("fl-log-2",M_LN2, module);
    DEFLOCONST("fl-1/log-2",1.0/M_LN2, module);
    DEFLOCONST("fl-log-3", log(3.0), module);
    DEFLOCONST("fl-log-pi", log(M_PI), module);
    DEFLOCONST("fl-log-10", M_LN10, module);
    DEFLOCONST("fl-1/log-10", 0.4342944819032518, module);
    DEFLOCONST("fl-pi", M_PI, module);
    DEFLOCONST("fl-1/pi", M_1_PI, module);
    DEFLOCONST("fl-2pi", 2.0*M_PI, module);
    DEFLOCONST("fl-pi/2", M_PI_2, module);
    DEFLOCONST("fl-pi/4", M_PI_4, module);
    DEFLOCONST("fl-pi-squared", M_PI*M_PI, module);
    DEFLOCONST("fl-degree", M_PI/180, module);
    DEFLOCONST("fl-2/pi", M_2_PI, module);
    DEFLOCONST("fl-2/sqrt-pi", M_2_SQRTPI, module);
    DEFLOCONST("fl-sqrt-2", M_SQRT2, module);
    DEFLOCONST("fl-sqrt-3", sqrt(3), module);
    DEFLOCONST("fl-sqrt-5", sqrt(5), module);
    DEFLOCONST("fl-sqrt-10", sqrt(10), module);
    DEFLOCONST("fl-1/sqrt-2", M_SQRT1_2, module);
    DEFLOCONST("fl-cbrt-2", pow(2.0,1/3.0), module);
    DEFLOCONST("fl-cbrt-3", pow(3.0,1/3.0), module);
    DEFLOCONST("fl-4thrt-2", sqrt(M_SQRT2), module);
    DEFLOCONST("fl-phi",(1.0+sqrt(5.0))/2.0, module);
    DEFLOCONST("fl-log-phi", 0.4812118250596034474977589134243684231352, module);
    DEFLOCONST("fl-1/log-phi", 2.0780869212350275376013226061177957677422, module);
    DEFLOCONST("fl-euler",0.5772156649015329, module);
    DEFLOCONST("fl-e-euler",exp(0.5772156649015329), module);
    DEFLOCONST("fl-sin-1",sin(1), module);
    DEFLOCONST("fl-cos-1",cos(1), module);
    DEFLOCONST("fl-gamma-1/2",1.77245385090551603, module);
    DEFLOCONST("fl-gamma-1/3",2.67893853470774763, module);
    DEFLOCONST("fl-gamma-2/3",tgamma(2.0/3.0), module);

    DEFLOCONST("fl-greatest", DBL_MAX, module);
    DEFLOCONST("fl-least", STk_dbl_true_min(), // (eventually computed)  DBL_TRUE_MIN
               module); 

    DEFLOCONST("fl-epsilon", DBL_EPSILON, module);
#ifdef FP_FAST_FMA
    DEFCONST("fl-fast-fl+*", STk_true, module);
#else
    DEFCONST("fl-fast-fl+*", STk_false, module);
#endif
    DEFLOCONST("%fl-integer-exponent-zero", FP_ILOGB0, module);
    DEFLOCONST("%fl-integer-exponent-nan",  FP_ILOGBNAN, module);
}



/****************\
 * CONSTRUCTORS *  DONE
\****************/

EXTERN_PRIMITIVE("exact->inexact", ex2inex, subr1, (SCM z));

DEFINE_PRIMITIVE("flonum", srfi_144_flonum, subr1, (SCM x))
{
    if (INTP(x) || BIGNUMP(x) || REALP(x) || RATIONALP(x))
        return STk_ex2inex(x);
    else
        return double2real(STk_NaN);
}

DEFINE_PRIMITIVE("fladjacent", srfi_144_fladjacent, subr2,(SCM x, SCM y))
{
    ensure_fl2(x,y);
    return double2real(nextafter(REAL_VAL(x),REAL_VAL(y)));
}


DEFINE_PRIMITIVE("flcopysign", srfi_144_copysign, subr2,(SCM x, SCM y))
{
    ensure_fl2(x,y);
    return double2real(copysign(REAL_VAL(x),REAL_VAL(y)));
}

DEFINE_PRIMITIVE("make-flonum", srfi_144_make_flonum, subr2,(SCM x, SCM n))
{
    ensure_fl(x);
    if (INTP(n)) {
        register int nn = INT_VAL(n);
        /* FIXME: INT_MAX, INT_MIN are device/compiler-dependent? */
        if ( (nn <= INT_MAX) && (nn >= INT_MIN) )
            return double2real(ldexp(REAL_VAL(x),(int)nn));
    }
    STk_error("");
    return STk_false; /* never reached */
}

/*************\
 * ACCESSORS *  DONE
\*************/

DEFINE_PRIMITIVE("flinteger-fraction", srfi_144_flinteger_fraction, subr1, (SCM x))
{
    ensure_fl(x);
    double i;
    double f = modf(REAL_VAL(x), &i);
    return STk_n_values(2,double2real(i),double2real(f));
}

DEFINE_PRIMITIVE("flexponent", srfi_144_flexponent, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(logb(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("%flinteger-exponent", srfi_144_flinteger_exponent, subr1, (SCM x))
{
    ensure_fl(x);
    /* FIXME: inefficient! */
    return (double2real((double)ilogb(REAL_VAL(x))));
}

DEFINE_PRIMITIVE("flnormalized-fraction-exponent", srfi_144_flnormalized_fraction_exponent, subr1, (SCM x))
{
    ensure_fl(x);
    int e;
    double f;
    if (isinf(REAL_VAL(x))) {
        f = 0.5 * ( signbit(REAL_VAL(x)) ? -1 : 1);
        e = 3 + round(log2(DBL_MAX));
    } else {
        f = frexp(REAL_VAL(x), &e);
    }
    return STk_n_values(2,double2real(f),MAKE_INT(e));
}

DEFINE_PRIMITIVE("flsign-bit", srfi_144_flsign_bit, subr1, (SCM x))
{
    ensure_fl(x);
    /* use a comparison to be sure that result is 0 or 1 (tcc returns 0 or 128) */
    return (MAKE_INT(signbit(REAL_VAL(x))!=0));
}


/**************\
 * PREDICATES *  DONE
\**************/

DEFINE_PRIMITIVE("flonum?",srfi_144_flonump,subr1,(SCM x))
{
    return MAKE_BOOLEAN(REALP(x));
}

DEFINE_PRIMITIVE("fl=?",srfi_144_fleqp,vsubr,(int argc, SCM *argv))
{
    if (argc==2) {
        ensure_fl2(*argv,*(argv-1));
        if (REAL_VAL(*argv) == REAL_VAL(*(argv-1)))
            return STk_true;
        else
            return STk_false;
    } else if (argc==3) {
        ensure_fl3(*argv,*(argv-1), *(argv-2));
        if ( (REAL_VAL(*argv) == REAL_VAL(*(argv-1))) &&
             (REAL_VAL(*(argv-1)) == REAL_VAL(*(argv-2))) )
            return STk_true;
        else
            return STk_false;
    } else {
        ensure_fl(*argv);
        for (register int i=0; i<argc-1; i++) {
            ensure_fl(*(argv-1));
            if (!(REAL_VAL(*argv) == REAL_VAL(*(argv-1))))
                return STk_false;
            argv--;
        }
        return STk_true;
    }
}

DEFINE_PRIMITIVE("fl<?",srfi_144_lessp,vsubr,(int argc, SCM *argv))
{
    if (argc==2) {
        ensure_fl2(*argv,*(argv-1));
        if (REAL_VAL(*argv)<REAL_VAL(*(argv-1)))
            return STk_true;
        else
            return STk_false;
    } else if (argc==3) {
        ensure_fl3(*argv,*(argv-1), *(argv-2));
        if ( (REAL_VAL(*argv) < REAL_VAL(*(argv-1))) &&
             (REAL_VAL(*(argv-1)) < REAL_VAL(*(argv-2))) )
            return STk_true;
        else
            return STk_false;
    } else {
        ensure_fl(*argv);
        for (register int i=0; i<argc-1; i++) {
            ensure_fl(*(argv-1));
            if (!(REAL_VAL(*argv) < REAL_VAL(*(argv-1))))
                return STk_false;
            argv--;
        }
        return STk_true;
    }
}

DEFINE_PRIMITIVE("fl>?",srfi_144_greaterp,vsubr,(int argc, SCM *argv))
{
    if (argc==2) {
        ensure_fl2(*argv,*(argv-1));
        if (REAL_VAL(*argv) > REAL_VAL(*(argv-1)))
            return STk_true;
        else
            return STk_false;
    } else if (argc==3) {
        ensure_fl3(*argv,*(argv-1), *(argv-2));
        if ( (REAL_VAL(*argv) > REAL_VAL(*(argv-1))) &&
             (REAL_VAL(*(argv-1)) > REAL_VAL(*(argv-2))) )
            return STk_true;
        else
            return STk_false;
    } else {
        ensure_fl(*argv);
        for (register int i=0; i<argc-1; i++) {
            ensure_fl(*(argv-1));
            if (!(REAL_VAL(*argv) > REAL_VAL(*(argv-1))))
                return STk_false;
            argv--;
        }
        return STk_true;
    }
}

DEFINE_PRIMITIVE("fl<=?",srfi_144_lesseqp,vsubr,(int argc, SCM *argv))
{
    if (argc==2) {
        ensure_fl2(*argv,*(argv-1));
        if (REAL_VAL(*argv)<=REAL_VAL(*(argv-1)))
            return STk_true;
        else
            return STk_false;
    } else if (argc==3) {
        ensure_fl3(*argv,*(argv-1), *(argv-2));
        if ( (REAL_VAL(*argv) <= REAL_VAL(*(argv-1))) &&
             (REAL_VAL(*(argv-1)) <= REAL_VAL(*(argv-2))) )
            return STk_true;
        else
            return STk_false;
    } else {
        ensure_fl(*argv);
        for (register int i=0; i<argc-1; i++) {
            ensure_fl(*(argv-1));
            if (!(REAL_VAL(*argv) <= REAL_VAL(*(argv-1))))
                return STk_false;
            argv--;
        }
        return STk_true;
    }
}

DEFINE_PRIMITIVE("fl>=?",srfi_144_greatereqp,vsubr,(int argc, SCM *argv))
{
    if (argc==2) {
        ensure_fl2(*argv,*(argv-1));
        if (REAL_VAL(*argv) >= REAL_VAL(*(argv-1)))
            return STk_true;
        else
            return STk_false;
    } else if (argc==3) {
        ensure_fl3(*argv,*(argv-1), *(argv-2));
        if ( (REAL_VAL(*argv) >= REAL_VAL(*(argv-1))) &&
             (REAL_VAL(*(argv-1)) >= REAL_VAL(*(argv-2))) )
            return STk_true;
        else
            return STk_false;
    } else {
        ensure_fl(*argv);
        for (register int i=0; i<argc-1; i++) {
            ensure_fl(*(argv-1));
            if (!(REAL_VAL(*argv) >= REAL_VAL(*(argv-1))))
                return STk_false;
            argv--;
        }
        return STk_true;
    }
}

DEFINE_PRIMITIVE("flunordered?",srfi_144_flunorderedp,subr2,(SCM x, SCM y))
{
    ensure_fl2(x,y);
    return MAKE_BOOLEAN( isnan(REAL_VAL(x))||isnan(REAL_VAL(y)) );
}


DEFINE_PRIMITIVE("flinteger?",srfi_144_flintegerp,subr1,(SCM x))
{
    ensure_fl(x);
    register double val = REAL_VAL(x);
    return MAKE_BOOLEAN( ( ! isinf(val) ) &&
                         (floor(val) == val));
}

DEFINE_PRIMITIVE("flzero?",srfi_144_flzerop,subr1,(SCM x))
{
    ensure_fl(x);
    return MAKE_BOOLEAN(FP_ZERO == fpclassify(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flpositive?",srfi_144_flpositivep,subr1,(SCM x))
{
    ensure_fl(x);
    return MAKE_BOOLEAN(REAL_VAL(x) > 0.0);
}
DEFINE_PRIMITIVE("flnegative?",srfi_144_flnegativep,subr1,(SCM x))
{
    ensure_fl(x);
    return MAKE_BOOLEAN(REAL_VAL(x) < 0.0);
}

DEFINE_PRIMITIVE("flodd?",srfi_144_floddp,subr1,(SCM x))
{
    ensure_fl(x);
    return MAKE_BOOLEAN(STk_real_isoddp(x));
}

DEFINE_PRIMITIVE("fleven?",srfi_144_flevenp,subr1,(SCM x))
{
    ensure_fl(x);
    return MAKE_BOOLEAN(!STk_real_isoddp(x));
}

DEFINE_PRIMITIVE("flfinite?",srfi_144_flfinitep,subr1,(SCM x))
{
    ensure_fl(x);
    return (!isinf(REAL_VAL(x)) && !isnan(REAL_VAL(x))) ? STk_true : STk_false;
}

DEFINE_PRIMITIVE("flinfinite?",srfi_144_flinfinitep,subr1,(SCM x))
{
    ensure_fl(x);
    return FP_INFINITE == fpclassify(REAL_VAL(x)) ? STk_true : STk_false;
}

DEFINE_PRIMITIVE("flnan?",srfi_144_flnanp,subr1,(SCM x))
{
    ensure_fl(x);
    return FP_NAN == fpclassify(REAL_VAL(x)) ? STk_true : STk_false;
}

DEFINE_PRIMITIVE("flnormalized?",srfi_144_flnormalizedp,subr1,(SCM x))
{
    ensure_fl(x);
    return isnormal(REAL_VAL(x)) ? STk_true : STk_false;
}

DEFINE_PRIMITIVE("fldenormalized?",srfi_144_fldenormalizedp,subr1,(SCM x))
{
    ensure_fl(x);
    return (FP_SUBNORMAL == fpclassify(REAL_VAL(x))) ? STk_true : STk_false;
}


/**************\
 * ARITHMETIC *   DONE
\**************/

DEFINE_PRIMITIVE("flmax",srfi_144_flmax,vsubr,(int argc, SCM *argv))
{
    if (argc==0) return double2real(-INFINITY);

    ensure_fl(*argv);

    if (argc==1) return *argv;

    if (argc==2) {
        ensure_fl(*(argv-1)); /* first arg was already checked */
        if (isnan(REAL_VAL(*argv))) return *(argv-1);
        if (isnan(REAL_VAL(*(argv-1)))) return *(argv);
        if (REAL_VAL(*argv) < REAL_VAL(*(argv-1)))
            return *(argv-1);
        else
            return *argv;
    } else {
        register double result = NAN;

        /* skip leading NaNs */
        while (argc > 0 && isnan(REAL_VAL(*argv))) {
            argv--;
            argc--;
        }

        /* if there are arguments left, we work on them. otherwise, return result,
           which is NaN */
        if (argc > 0) {
            /* first value, guaranteed to NOT be NaN */
            result = REAL_VAL(*argv--);
            argc--;

            while (argc > 0) {
                ensure_fl(*argv);
                if (!isnan(REAL_VAL(*argv))) {
                    if (REAL_VAL(*argv) > result)
                        result = REAL_VAL(*argv);
                }
                argc--;
                argv--;
            }
        }
        return double2real(result);
    }
}

DEFINE_PRIMITIVE("flmin",srfi_144_flmin,vsubr,(int argc, SCM *argv))
{
    if (argc==0) return double2real(INFINITY);

    ensure_fl(*argv);

    if (argc==1) return *argv;

    if (argc==2) {
        ensure_fl(*(argv-1)); /* first arg was already checked */
        if (isnan(REAL_VAL(*argv))) return *(argv-1);
        if (isnan(REAL_VAL(*(argv-1)))) return *(argv);
        if (REAL_VAL(*argv) > REAL_VAL(*(argv-1)))
            return *(argv-1);
        else
            return *argv;
    } else {
        register double result = NAN;

        /* skip leading NaNs */
        while (argc > 0 && isnan(REAL_VAL(*argv))) {
            argv--;
            argc--;
        }

        /* if there are arguments left, we work on them. otherwise, return result,
           which is NaN */
        if (argc > 0) {
            /* first value, guaranteed to NOT be NaN */
            result = REAL_VAL(*argv--);
            argc--;

            while (argc > 0) {
                ensure_fl(*argv);
                if (!isnan(REAL_VAL(*argv))) {
                    if (REAL_VAL(*argv) < result)
                        result = REAL_VAL(*argv);
                }
                argc--;
                argv--;
            }
        }
        return double2real(result);
    }
}


DEFINE_PRIMITIVE("fl+",srfi_144_sum,vsubr,(int argc, SCM *argv))
{
    if (argc==2) {
        ensure_fl2(*argv,*(argv-1));
        return double2real(REAL_VAL(*argv)+REAL_VAL(*(argv-1)));
    } else if (argc==3) {
        ensure_fl3(*argv,*(argv-1), *(argv-2));
        return double2real(REAL_VAL(*argv) +
                           REAL_VAL(*(argv-1)) +
                           REAL_VAL(*(argv-2)));
    } else {
        register double res = 0.0;
        for (register int i=0; i<argc; i++) {
            ensure_fl(*argv);
            res += REAL_VAL(*argv--);
        }
        return double2real(res);
    }
}

DEFINE_PRIMITIVE("fl*",srfi_144_mul,vsubr,(int argc, SCM *argv))
{
    register double res = 1.0;
    for (register int i=0; i<argc; i++) {
        ensure_fl(*argv);
        res *= REAL_VAL(*argv--);
    }
    return double2real(res);
}

DEFINE_PRIMITIVE("fl+*", srfi_144_sum_mul, subr3,(SCM x, SCM y, SCM z))
{
    ensure_fl3(x,y,z);
    return double2real(fma(REAL_VAL(x),REAL_VAL(y),REAL_VAL(z)));
}


DEFINE_PRIMITIVE("fl/",srfi_144_div,vsubr,(int argc, SCM *argv))
{
    if (argc==0) STk_error("expects at least one argument");
    ensure_fl(*argv);
    if (argc==1) return double2real(1.0 / REAL_VAL(*argv));

    register double res = REAL_VAL(*argv--);
    argc--;

    for (register int i=0; i<argc; i++) {
        ensure_fl(*argv);
        res /= REAL_VAL(*argv--);
    }
    return double2real(res);
}

DEFINE_PRIMITIVE("fl-",srfi_144_sub,vsubr,(int argc, SCM *argv))
{
    if (argc==0) STk_error("expects at least one argument");
    ensure_fl(*argv);
    if (argc==1) return double2real(-REAL_VAL(*argv));

    register double res = REAL_VAL(*argv--);
    argc--;

    for (register int i=0; i<argc; i++) {
        ensure_fl(*argv);
        res -= REAL_VAL(*argv--);
    }
    return double2real(res);

}

DEFINE_PRIMITIVE("flabs",srfi_144_flabs,subr1,(SCM x))
{
    ensure_fl(x);
    return double2real(fabs(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flabsdiff",srfi_144_flabsdiff,subr2,(SCM x, SCM y))
{
    ensure_fl2(x,y);
    return double2real(fabs(REAL_VAL(x) - REAL_VAL(y)));
}

DEFINE_PRIMITIVE("flposdiff",srfi_144_flposdiff,subr2,(SCM x, SCM y))
{
    ensure_fl2(x,y);
    return double2real(fdim(REAL_VAL(x),REAL_VAL(y)));
}

DEFINE_PRIMITIVE("flsgn",srfi_144_flsgn,subr1,(SCM x))
{
    ensure_fl(x);
    return double2real(copysign(1.0,REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flfloor",srfi_144_flfloor,subr1,(SCM x))
{
    ensure_fl(x);
    return double2real(floor(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flceiling",srfi_144_flceiling,subr1,(SCM x))
{
    ensure_fl(x);
    return double2real(ceil(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flround",srfi_144_flround,subr1,(SCM x))
{
    ensure_fl(x);
    return STk_false; /* FIXME */
}


DEFINE_PRIMITIVE("fltruncate",srfi_144_fltruncate,subr1,(SCM x))
{
    ensure_fl(x);
    return double2real(trunc(REAL_VAL(x)));
}

/****************************\
 * EXPONENTS AND LOGARITHMS *  DONE
\****************************/


DEFINE_PRIMITIVE("flexp", srfi_144_flexp, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(exp(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flexp2", srfi_144_flexp2, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(exp2(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flexp-1", srfi_144_flexp_1, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(expm1(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flsquare", srfi_144_flsquare, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(REAL_VAL(x)*REAL_VAL(x));
}

DEFINE_PRIMITIVE("flsqrt", srfi_144_flsqrt, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(sqrt(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flcbrt", srfi_144_flcbrt, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(cbrt(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flhypot", srfi_144_flhypot, subr2, (SCM x, SCM y))
{
    ensure_fl2(x,y);
    return double2real(hypot(REAL_VAL(x), REAL_VAL(y)));
}

DEFINE_PRIMITIVE("flexpt", srfi_144_flexpt, subr2, (SCM x, SCM y))
{
    ensure_fl2(x,y);
    return double2real(pow(REAL_VAL(x), REAL_VAL(y)));
}

DEFINE_PRIMITIVE("fllog", srfi_144_fllog, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(log(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("fllog1+", srfi_144_fllog1, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(log1p(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("fllog2", srfi_144_fllog2, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(log2(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("fllog10", srfi_144_fllog10, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(log10(REAL_VAL(x)));
}

/***************************\
 * TRIGONOMETRIC FUNCTIONS *  DONE
\***************************/

DEFINE_PRIMITIVE("flsin", srfi_144_flsin, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(sin(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flcos", srfi_144_flcos, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(cos(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("fltan", srfi_144_fltan, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(tan(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flasin", srfi_144_flasin, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(asin(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flacos", srfi_144_flacos, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(acos(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flatan", srfi_144_flatan, subr12, (SCM x, SCM y))
{
    ensure_fl(x);
    if (y) {
        ensure_fl(y);
        return double2real(atan2(REAL_VAL(x),REAL_VAL(y)));
    }
    else return double2real(atan(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flsinh", srfi_144_flsinh, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(sinh(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flcosh", srfi_144_flcosh, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(cosh(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("fltanh", srfi_144_fltanh, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(tanh(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flasinh", srfi_144_flasinh, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(asinh(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flacosh", srfi_144_flacosh, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(acosh(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flatanh", srfi_144_flatanh, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(atanh(REAL_VAL(x)));
}


/********************\
 * INTEGER DIVISION * DONE
\********************/

DEFINE_PRIMITIVE("flquotient", srfi_144_flquotient, subr2, (SCM x, SCM y))
{
    ensure_fl2(x,y);
    return double2real(trunc(REAL_VAL(x) / REAL_VAL(y)));
}

DEFINE_PRIMITIVE("flremainder", srfi_144_flremainder, subr2, (SCM x, SCM y))
{
    ensure_fl2(x,y);
    register double xx = REAL_VAL(x);
    register double yy = REAL_VAL(y);
    return(double2real(xx-yy*trunc(xx/yy)));
}


DEFINE_PRIMITIVE("flremquo", srfi_144_flremquo, subr2, (SCM x, SCM y))
{
    ensure_fl(x);
    int q;
    double r = remquo(REAL_VAL(x),REAL_VAL(y), &q);
    return STk_n_values(2,double2real(r),MAKE_INT(q));
}

/*********************\
 * SPECIAL FUNCTIONS *  DONE
\*********************/

DEFINE_PRIMITIVE("flgamma", srfi_144_flgamma, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(tgamma(REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flloggamma", srfi_144_flloggamma, subr1, (SCM x))
{
    ensure_fl(x);
    int s;
    register double g = lgamma_r(REAL_VAL(x), &s);
    return STk_n_values(2,double2real(g), double2real((double)s));
}

/* FIXME: if n is FIXNUM, return -inf.0 ? */

DEFINE_PRIMITIVE("flfirst-bessel", srfi_144_flfirst_bessel, subr2, (SCM n, SCM x))
{
    ensure_fx(n);
    ensure_fl(x);
    return double2real(jn(INT_VAL(n),REAL_VAL(x)));
}

DEFINE_PRIMITIVE("flsecond-bessel", srfi_144_flsecond_bessel, subr2, (SCM n, SCM x))
{
    ensure_fx(n);
    ensure_fl(x);
    return double2real(yn(INT_VAL(n),REAL_VAL(x)));
}


DEFINE_PRIMITIVE("flerf", srfi_144_flerf, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(erf(REAL_VAL(x)));
}
DEFINE_PRIMITIVE("flerfc", srfi_144_flerfc, subr1, (SCM x))
{
    ensure_fl(x);
    return double2real(erfc(REAL_VAL(x)));
}


/*********************\
 *  MODULE SRFI-144  *
\*********************/

MODULE_ENTRY_START("scheme/flonum")
{
  SCM module =  STk_create_module(STk_intern("scheme/flonum"));

  /* FIXME: this should exist outside of this SRFI... */
  SCM_NaN = double2real(NAN);

  /* CONSTANTS */
  STk_srfi_144_define_constants(module);

  /* CONSTRUCTORS */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flonum, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fladjacent, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_copysign, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_make_flonum, module);

  /* ACCESSORS */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flinteger_fraction, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flexponent, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flinteger_exponent, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flnormalized_fraction_exponent, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flsign_bit, module);

  /* PREDICATES */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flonump, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fleqp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_lessp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_greaterp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_lesseqp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_greatereqp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flunorderedp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flintegerp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flzerop, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flpositivep, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flnegativep, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_floddp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flevenp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flfinitep, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flinfinitep, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flnanp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flnormalizedp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fldenormalizedp, module);

  /* ARITHMETIC */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flmax, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flmin, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_sum, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_mul, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_div, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_sub, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_sum_mul, module);

  ADD_PRIMITIVE_IN_MODULE(srfi_144_flabs, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flabsdiff, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flposdiff, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flsgn, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flfloor, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flceiling, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fltruncate, module);

  /* EXPONENTS AND LOGARITHMS */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flexp, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flexp2, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flexp_1, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flsquare, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flsqrt, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flcbrt, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flhypot, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flexpt, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fllog, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fllog1, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fllog2, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fllog10, module);

  /* TRIGONOMETRIC FUNCTIONS */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flsin, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flcos, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fltan, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flasin, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flacos, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flatan, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flsinh, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flcosh, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_fltanh, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flasinh, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flacosh, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flatanh, module);

  /* INTEGER DIVISION */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flquotient, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flremainder, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flremquo, module);

  /* SPECIAL FUNCTIONS */
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flgamma, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flloggamma, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flfirst_bessel, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flsecond_bessel, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flerf, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_144_flerfc, module);

  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
