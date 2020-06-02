/*
 *
 *  o b j e c t . c                     -- Objects support
 *
 * Copyright © 1994-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 *
 *            Author: Erick Gallesio [eg@unice.fr]
 *    Creation date:  9-Feb-1994 15:56
 * Last file update: 30-May-2020 17:26 (eg)
 */

#include "stklos.h"
#include "object.h"
#include "struct.h"

#define GF_VAL(name)            (STk_lookup(STk_intern(name), \
                                            STk_current_module(), &unused, TRUE))
#define CALL_GF1(name,a)        (STk_C_apply(GF_VAL(name), 1, (a)))
#define CALL_GF2(name,a,b)      (STk_C_apply(GF_VAL(name), 2, (a), (b)))
#define CALL_GF3(name,a,b,c)    (STk_C_apply(GF_VAL(name), 3, (a), (b), (c)))
#define CALL_GF4(name,a,b,c,d)  (STk_C_apply(GF_VAL(name), 4, (a), (b), (c), (d)))

#define CLASS_REDEF(c)  INST_SLOT(c, S_redefined)


#define CLASSP(x)       (INSTANCEP(x) && SUBCLASSP(INST_CLASS_OF(x), Class))
#define GENERICP(x)     (INSTANCEP(x) && SUBCLASSP(INST_CLASS_OF(x), Generic))
#define METHODP(x)      (INSTANCEP(x) && SUBCLASSP(INST_CLASS_OF(x), Method))


#define SPEC_OF(x)      (INST_SLOT((x), S_specializers))




static SCM unused;      /* for the (useless here) reference returned by lookup */


static SCM make_instance(SCM classe, short size, short type);
static SCM compute_getters_n_setters(SCM slots);
EXTERN_PRIMITIVE("class-of", class_of, subr1, (SCM obj));



static SCM Top, Object, Class, Generic, Method, Simple_method, Accessor,
           Procedure_class, Entity_class;
static SCM Boolean, Char, Pair, Procedure, String, Symbol, Vector, Number,
           Liste, Null, Real, Complex, Rational, Integer, Keyword, Eof,
  Struct_type, Struct, Cond, Cond_type, Box, UnknownClass;


int STk_oo_initialized = FALSE;

/*=============================================================================*/
static void error_bad_instance(SCM obj)
{
  STk_error("~S is a bad object", obj);
}

static void error_bad_class(SCM obj)
{
  STk_error("~S is a bad class", obj);
}

static void error_bad_slot_name(SCM obj)
{
  STk_error("~S is a bad slot name", obj);
}

static void error_bad_index(SCM index, SCM obj)
{
  STk_error("bad index ~S for ~S", index, obj);
}

static void error_bad_generic(SCM gf)
{
  STk_error("bad generic function ~S", gf);
}

static void error_no_method(SCM gf)
{
  STk_error("no method associated to ~S generic", gf);
}

static void error_bad_method(SCM obj)
{
  STk_error("bad method ~S", obj);
}

static void error_bad_list(SCM obj)
{
  STk_error("~S is a bad list of parameters", obj);
}


/******************************************************************************
 *
 *   GGGG                FFFFF
 *  G                    F
 *  G  GG                FFF
 *  G   G                F
 *   GGG  E N E R I C    F    U N C T I O N S
 *
 * This implementation provides
 *      - generic functions (with class specializers)
 *      - multi-methods
 *      - next-method
 *      - a hard-coded MOP for standard gf, which can be overloaded for non-std gf
 *
 ******************************************************************************/

SCM STk_make_next_method(SCM gf, int argc, SCM *argv, SCM methods)
{
  SCM z, *p;
  int i;

  /* Copy parameters in the next-method object (in fact objects are not always
   * on the stack due to tail recursive code handling).
   */
  NEWCELL_WITH_LEN(z, next_method, NXT_MTHD_ALLOC_BYTES(argc));
  NXT_MTHD_GF(z)      = gf;
  NXT_MTHD_METHOD(z)  = CAR(methods);
  NXT_MTHD_METHODS(z) = CDR(methods);
  NXT_MTHD_ARGC(z)    = argc;

  for (p=NXT_MTHD_ARGV(z), i=0; i < argc; i++)
    p[i] = *argv--;

  return z;
}

DEFINE_PRIMITIVE("%set-next-method!", set_next_method, subr2,(SCM closure, SCM val))
{
  if (!CLOSUREP(closure)) STk_error("bad procedure ~S", closure);
  SET_NEXT_METHOD(closure, val);
  return STk_void;
}


#ifdef FIXME
// static PRIMITIVE next_method_exists(SCM next)
// {
//   if (NTYPEP(next, tc_next_method))
//     Err("next-method-exists: bad next method'", next);
//   return NULLP(NXT_MTHD_METHODS(next))? STk_false : STk_true;
// }
#endif

/******************************************************************************
 *
 * Protocol for calling a generic fumction
 * This protocol is roughly equivalent to (parameter are a little bit different
 * for efficiency reasons):
 *
 *      + apply-generic (gf args)
 *              + compute-applicable-methods (gf args ...)
 *                      + sort-applicable-methods (methods args)
 *              + apply-methods (gf methods args)
 *
 * apply-methods calls make-next-method to build the "continuation" of a a
 * method.  Applying a next-method will call apply-next-method which in
 * turn will call  apply again to call effectively the following method.
 *
 ******************************************************************************/
static int applicablep(SCM actual, SCM formal)
{
  register SCM ptr;

  /* We test that (memq formal (slot-ref actual 'cpl))
   * However, we don't call memq here since we already know that
   * the list is well formed
   */
  for (ptr=INST_SLOT(actual, S_cpl); !NULLP(ptr); ptr = CDR(ptr)) {
    if (CAR(ptr) == formal) return TRUE;
  }
  return FALSE;
}

static int more_specificp(SCM m1, SCM m2, SCM *targs)
{
  register SCM s1, s2;
  /*
   * Note:
   *   m1 and m2 can have != length (i.e. one can be one element longer than the
   * other when we have a dotted parameter list). For instance, with the call
   *   (M 1)
   * with
   *   (define-method M (a . l) ....)
   *   (define-method M (a) ....)
   *
   * we consider that the second method is more specific.
   *
   * BTW, targs is an array of types. We don't need it's size since
   * we already know that m1 and m2 are applicable (no risk to go past
   * the end of this array).
   *
   */
  for (s1=SPEC_OF(m1), s2=SPEC_OF(m2); ; targs++,s1=CDR(s1),s2=CDR(s2)) {
    if (NULLP(s1)) return 1;
    if (NULLP(s2)) return 0;

    /* Variadic specializers aren't proper lists. */
    if (!CONSP(s1)) return 0;
    if (!CONSP(s2)) return 1;

    if (CAR(s1) != CAR(s2)) {
      register SCM l, cs1 = CAR(s1), cs2 = CAR(s2);

      for (l = INST_SLOT(*targs, S_cpl);   ; l = CDR(l)) {
        if (cs1 == CAR(l)) return 1;
        if (cs2 == CAR(l)) return 0;
      }
      return 0;/* should not occur! */
    }
  }
  return 0; /* should not occur! */
}

#define BUFFSIZE 32             /* big enough for most uses */

static SCM sort_applicable_methods(SCM method_list, int size, SCM *targs)
{
  int i, j, incr;
  SCM *v, vector;
  SCM buffer[BUFFSIZE];
  SCM save = method_list;

  /* For reasonably sized method_lists we can try to avoid all the
   * consing and reorder the list in place...
   * This idea is due to David McClain <Dave_McClain@msn.com>
   */
  if (size <= BUFFSIZE) {
    for(i=0;  i < size; i++) {
      buffer[i]   = CAR(method_list);
      method_list = CDR(method_list);
    }
    vector = STk_nil;           /* for GCC */
    v      = buffer;
  }
  else {
    /* Too many elements in method_list to keep everything locally */
    vector = STk_makevect(size, save);   //FIXME:HORROR
    v      = VECTOR_DATA(vector);
    // CE CODE DOIT être avant la logique est faussse!!!!
  }

  /* Use a simple shell sort since it is generally faster than qsort on
   * small vectors (which is probably mostly the case when we have to
   * sort a list of applicable methods).
   */
  for (incr = size / 2; incr; incr /= 2) {
    for (i = incr; i < size; i++) {
      for (j = i-incr ;j >= 0; j -= incr) {
        if (more_specificp(v[j], v[j+incr], targs)) break;
        else {
          SCM tmp   = v[j+incr];
          v[j+incr] = v[j];
          v[j]      = tmp;
        }
      }
    }
  }

  if (size <= BUFFSIZE) {
    /* We did it in locally, so restore the original list (reordered) in-place */
    for(i=0, method_list=save; i < size; i++, v++) {
      CAR(method_list) = *v;
      method_list      = CDR(method_list);
    }
    return save;
  }
  /* If we are here, that's that we did it the hard way... */
  return STk_vector2list(vector);
}


SCM STk_compute_applicable_methods(SCM gf, int argc, SCM *argv, int find_method)
{
  int i, count = 0;
  SCM l, fl, applicable = STk_nil;
  SCM buffer[BUFFSIZE], *types, *p;
  SCM *save = argv;
  SCM tmp;

  if (NULLP(INST_SLOT(gf, S_methods)))
    /* generic function without associated method */
    return CALL_GF2("no-method", gf, STk_argv2list(argc, argv));

  /* Build the list of arguments types */
  if (argc >= BUFFSIZE) {
    tmp   = STk_makevect(argc, (SCM) NULL);
    types = p = VECTOR_DATA(tmp);
  }
  else
    types = p = buffer;

  for (i=0; i < argc; i++)
    *p++ = STk_class_of(*argv--);

  /* Build a list of all applicable methods */
  for (l = INST_SLOT(gf, S_methods); !NULLP(l); l = CDR(l)) {
    for (i=0, fl=SPEC_OF(CAR(l));  ; i++, fl=CDR(fl)) {
      if (INSTANCEP(fl) ||              /* We have a dotted argument list */
          (i >= argc && NULLP(fl))) {   /* both list exhausted */
        applicable = STk_cons(CAR(l), applicable);
        count     += 1;
        break;
      }
      if (i >= argc || NULLP(fl) || !applicablep(types[i], CAR(fl))) break;
    }
  }

  if (count == 0) {
    if (find_method) return STk_false;
    CALL_GF2("no-applicable-method", gf, STk_argv2list(argc, save));
    /* if we are here, it's because no-applicable-method didn't signal an error */
    return STk_nil;
  }
  return (count == 1) ? applicable :
                        sort_applicable_methods(applicable, count, types);
}


DEFINE_PRIMITIVE("find-method", find_method, vsubr, (int argc, SCM *argv))
{
  SCM gf;

  if (argc == 0) STk_error("no generic function given");

  gf = argv[0];
  if (!GENERICP(gf))                   error_bad_generic(gf);
  if (NULLP(INST_SLOT(gf, S_methods))) error_no_method(gf);

  return STk_compute_applicable_methods(gf, --argc, --argv, TRUE);
}


DEFINE_PRIMITIVE("%method-more-specific?", method_more_specificp, subr3,
                 (SCM m1, SCM m2, SCM targs))
{
  SCM l, v;
  int i, len;

  len = STk_int_length(targs);

  if (!METHODP(m1)) error_bad_method(m1);
  if (!METHODP(m2)) error_bad_method(m2);
  if (len < 0)      error_bad_list(targs);

  /* Verify that all the arguments of targs are classes and place them in a vector*/
  v = STk_makevect(len, (SCM) NULL);

  for (i=0, l=targs; !NULLP(l); i++, l=CDR(l)) {
    if (!CLASSP(CAR(l))) error_bad_list(targs);
    VECTOR_DATA(v)[i] = CAR(l);
  }
  return MAKE_BOOLEAN(more_specificp(m1, m2, VECTOR_DATA(v)));
}


/******************************************************************************
 *
 * Initializations
 *
 ******************************************************************************/

#ifdef FIXME
// static void define_extended_type_classes(void)
// {
//   /*
//    * This function is called when STklos is initialized. It performs
//    * the definition of classes for extended types defined before STklos
//    * loading
//    */
//   int i;
//
//   initialized = TRUE;
//   for (i = tc_start_extd; ; i++) {
//     char *name = STk_get_extended_name(i);
//
//     if (!name) return;
//     STk_register_extended_class(STk_make_extended_class(name), i);
//   }
// }
//
// static void add_primitive(char *name, int type, void *fct_ptr)
// {
//   STk_add_new_primitive(name, type, fct_ptr);
//   STk_export_symbol(STk_intern(name), STklos);
// }
#endif

/*===========================================================================*\
 *
 *                              S L O T   A C C E S S
 *
\*===========================================================================*/

static Inline SCM test_change_class(SCM obj)
{
  SCM classe = INST_CLASS_OF(obj);

  if (CLASS_REDEF(classe) != STk_false)
    CALL_GF3("change-object-class", obj, classe, CLASS_REDEF(classe));
  return classe;
}



static Inline SCM get_slot_value(SCM classe, SCM obj, SCM slot_name)
{
  SCM l;

  for (l=INST_ACCESSORS(obj); !NULLP(l); l=CDR(l)) {
    if (CAR(CAR(l)) == slot_name) {
      l = CDR(CDR(CAR(l)));
      /* Two cases here:
       *        - l is an integer (the offset of this slot in the slots vector)
       *        - otherwise (car l) is the getter function to apply
       */
      if (INTP(l))
        return INST_SLOT(obj, INT_VAL(l));
      else {    /* We must evaluate (apply (car l) (list obj)) */
        return STk_C_apply(CAR(l), 1, obj);
      }
    }
  }
  return CALL_GF3("slot-missing", classe, obj, slot_name);
}

static Inline SCM set_slot_value(SCM classe, SCM obj, SCM slot_name, SCM value)
{
  register SCM l;

  for (l=INST_ACCESSORS(obj); !NULLP(l); l=CDR(l)) {
    if (CAR(CAR(l)) == slot_name) {
      l = CDR(CDR(CAR(l)));
      /* Two cases here:
       *        - l is an integer (the offset of this slot in the slots vector)
       *        - otherwise (cadr l) is the setter function to apply
       */
      if (INTP(l))
        INST_SLOT(obj, INT_VAL(l)) = value;
      else {  /* We must evaluate (apply (cadr l) (list obj value)) */
        STk_C_apply(CAR(CDR(l)), 2, obj, value);
      }
      return STk_void;
    }
  }
  return CALL_GF4("slot-missing", classe, obj, slot_name, value);
}


DEFINE_PRIMITIVE("slot-ref", slot_ref, subr2, (SCM obj, SCM slot_name))
{
  SCM classe, res;

  if (!INSTANCEP(obj)) error_bad_instance(obj);

  classe = test_change_class(obj);
  res = get_slot_value(classe, obj, slot_name);

  return (res==STk_void) ? CALL_GF3("slot-unbound", classe, obj, slot_name): res;
}


DEFINE_PRIMITIVE("slot-set!", slot_set, subr3, (SCM obj, SCM slot_name, SCM value))
{
  SCM classe;

  if (!INSTANCEP(obj)) error_bad_instance(obj);

  classe = test_change_class(obj);
  return set_slot_value(classe, obj, slot_name, value);
}


DEFINE_PRIMITIVE("slot-ref-using-class", slot_ref_using_class, subr3,
                 (SCM classe, SCM obj, SCM slot_name))
{
  SCM res;

  if (!CLASSP(classe)) error_bad_class(classe);
  if (!INSTANCEP(obj)) error_bad_instance(obj);

  res = get_slot_value(classe, obj, slot_name);
  return (res==STk_void) ? CALL_GF3("slot-unbound", classe, obj, slot_name): res;
}

DEFINE_PRIMITIVE("slot-set-using-class!", slot_set_using_class, subr4,
                 (SCM klass, SCM object, SCM slot_name, SCM value))
{
  if (!CLASSP(klass))     error_bad_class(klass);
  if (!INSTANCEP(object)) error_bad_instance(object);
  return set_slot_value(klass, object, slot_name, value);
}


DEFINE_PRIMITIVE("%fast-slot-ref", fast_slot_ref, subr2, (SCM obj, SCM index))
{
  register long i = STk_integer_value(index);

  if (!INSTANCEP(obj))                    error_bad_instance(obj);
  if (i < 0 || i >= INST_NUMBER_OF_SLOTS(obj)) error_bad_index(index, obj);
  return INST_SLOT(obj, i);
}

DEFINE_PRIMITIVE("%fast-slot-set!", fast_slot_set, subr3,
                 (SCM obj, SCM index, SCM value))
{
  register long i = STk_integer_value(index);

  if (!INSTANCEP(obj))                    error_bad_instance(obj);
  if (i < 0 || i >= INST_NUMBER_OF_SLOTS(obj)) error_bad_index(index, obj);

  INST_SLOT(obj, i) = value;
  return STk_void;
}

DEFINE_PRIMITIVE("%slot-ref", undoc_slot_ref, subr2, (SCM obj, SCM slot_name))
{
  /* As slot-ref except that we can reference an unbound slot */
  SCM classe;

  if (!INSTANCEP(obj)) error_bad_instance(obj);

  classe = test_change_class(obj);
  return get_slot_value(classe, obj, slot_name);
}



/******************************************************************************
 *
 * initialize-object
 *
 ******************************************************************************/
// static void Inline set_slot_value_if_unbound
//                      (SCM classe, SCM obj, SCM slot_name, SCM val)
// {
// #ifdef FIXME
//    SCM old_val = get_slot_value(classe, obj, slot_name);
// 
//   //FIXMOI:  if (old_val == STk_void)
// #endif
//     set_slot_value(classe, obj, slot_name, val);
// }


DEFINE_PRIMITIVE("%initialize-object", initialize_obj, subr2,(SCM obj,SCM initargs))
{
  static char k_init_keyword[] = ":init-keyword";
  SCM tmp, get_n_set, slots;
  SCM init_keyword = STk_makekey(k_init_keyword);
  SCM classe       = INST_CLASS_OF(obj);


  if (!INSTANCEP(obj))                      error_bad_instance(obj);
  if (!CONSP(initargs) && !NULLP(initargs)) STk_error("bad initialization list",
                                                      initargs);

  get_n_set = INST_SLOT(classe, S_getters_n_setters);
  slots     = INST_SLOT(classe, S_slots);

  /* See for each slot how it must be initialized */
  for ( ; !NULLP(slots); get_n_set=CDR(get_n_set), slots=CDR(slots)) {
    SCM slot_name  = CAR(slots);
    SCM slot_value = STk_void;

    if (CONSP(slot_name)) {
      /* This slot admits (perhaps) to be initialized at creation time */
      tmp        = STk_key_get(CDR(slot_name), init_keyword, STk_void);
      slot_name  = CAR(slot_name);

      if (tmp != STk_void) {
        /* an initarg was provided for this slot */
#ifdef FIXME
        //VIRER
        //if (!KEYWORDP(tmp)) STk_error("initarg must be a keyword. It was", tmp);
#endif
        slot_value = STk_key_get(initargs, tmp, STk_void);
      }
    }

    if (slot_value != STk_void)
      /* set slot to provided value */
      STk_slot_set(obj, slot_name, slot_value);
    else {
      /* set slot to its :init-form if it exists */
      tmp = CAR(CDR(CAR(get_n_set)));
      if (tmp != STk_false)
        set_slot_value(classe, obj, slot_name, STk_C_apply(tmp, 1, obj));
    }
  }
  return obj;
}





static SCM test_slot_existence(SCM _UNUSED(classe), SCM obj, SCM slot_name)
{
  register SCM l;

  for (l=INST_ACCESSORS(obj); !NULLP(l); l=CDR(l))
    if (CAR(CAR(l)) == slot_name) return STk_true;

  return STk_false;
}


                /* ======================================== */


DEFINE_PRIMITIVE("slot-bound?", slot_boundp, subr2, (SCM obj, SCM slot_name))
{
  SCM classe;

  if (!INSTANCEP(obj))     error_bad_instance(obj);
  if (!SYMBOLP(slot_name)) error_bad_slot_name(obj);

  classe = test_change_class(obj);

  return MAKE_BOOLEAN(get_slot_value(classe, obj, slot_name) != STk_void);
}

DEFINE_PRIMITIVE("slot-exists?", slot_existsp, subr2, (SCM obj, SCM slot_name))
{
  SCM classe;

  if (!INSTANCEP(obj))     error_bad_instance(obj);
  if (!SYMBOLP(slot_name)) error_bad_slot_name(obj);

  classe = test_change_class(obj);

  return test_slot_existence(classe, obj, slot_name);
}


DEFINE_PRIMITIVE("slot-bound-using-class?", slot_boundp_using_class, subr3,
                 (SCM classe, SCM obj, SCM slot_name))
{
  if (!CLASSP(classe))     error_bad_class(classe);
  if (!INSTANCEP(obj))     error_bad_instance(obj);
  if (!SYMBOLP(slot_name)) error_bad_slot_name(slot_name);

  return MAKE_BOOLEAN(get_slot_value(classe, obj, slot_name) != STk_void);
}

DEFINE_PRIMITIVE("slot-exists-using-class?", slot_existsp_using_class, subr3,
                 (SCM classe, SCM obj, SCM slot_name))
{
  if (!CLASSP(classe))     error_bad_class(classe);
  if (!INSTANCEP(obj))     error_bad_instance(obj);
  if (!SYMBOLP(slot_name)) error_bad_slot_name(slot_name);

  return test_slot_existence(classe, obj, slot_name);
}



/*===========================================================================*\
 *
 *                              C L A S S E S
 *
\*===========================================================================*/

/*
 * basic-compute-cpl
 *      Basic version unable to manage multiple inheritance. Used only
 *      during boot phase
 */
static SCM basic_compute_cpl(SCM supers, SCM res)
{
  return NULLP(supers) ?
             STk_reverse(res) :
             basic_compute_cpl(INST_SLOT(CAR(supers), S_direct_supers),
                               STk_cons(CAR(supers), res));
}


/*
 * basic-compute-slots
 *      Basic version which does not handle slots which are inherited
 *      several time
 *
 */
static SCM basic_compute_slots(SCM slots, SCM cpl)
{
  for (cpl = CDR(cpl); !NULLP(cpl); cpl = CDR(cpl))
    slots = STk_append2(INST_SLOT(CAR(cpl), S_direct_slots), slots);

  return slots;
}

/*
 * basic_make_class
 *      Create a classe (basic version, unable to manage fancy features such
 *      as multiple inheritance or class redefinition). This version is used
 *      during boot only.
 */
static SCM basic_make_class(SCM classe, SCM name, SCM dsupers, SCM dslots)
{
  SCM tmp, z, cpl, slots, g_n_s;

  /* Allocate one instance */
  z = make_instance(classe, NUMBER_OF_CLASS_SLOTS, TYPE_INSTANCE);

  /* Initialize its slots */
  cpl   = basic_compute_cpl(dsupers, LIST1(z));
  slots = basic_compute_slots(dslots, cpl);
  g_n_s = compute_getters_n_setters(slots);

  INST_SLOT(z, S_name)              = name;
  INST_SLOT(z, S_direct_supers)     = dsupers;
  INST_SLOT(z, S_direct_slots)      = dslots;
  INST_SLOT(z, S_direct_subclasses) = STk_nil;
  INST_SLOT(z, S_direct_methods)    = STk_nil;
  INST_SLOT(z, S_cpl)               = cpl;
  INST_SLOT(z, S_slots)             = slots;
  INST_SLOT(z, S_nfields)           = MAKE_INT(STk_int_length(slots));
  INST_SLOT(z, S_getters_n_setters) = g_n_s;
  INST_SLOT(z, S_redefined)         = STk_false;

  /* Don't forget to set the accessors list of the object */
  INST_ACCESSORS(z) = INST_SLOT(classe, S_getters_n_setters);

  /* Add this class in the direct-subclasses slot of dsupers */
  for (tmp = dsupers; !NULLP(tmp); tmp = CDR(tmp)) {
    INST_SLOT(CAR(tmp), S_direct_subclasses) =
      STk_cons(z, INST_SLOT(CAR(tmp), S_direct_subclasses));
  }

  return z;
}



/*===========================================================================*\
 *
 *                              I N S T A N C E S
 *
\*===========================================================================*/


static SCM make_instance(SCM classe, short size, short type)
{
  SCM z;
  int i;

  NEWCELL(z, instance);
  INST_NUMBER_OF_SLOTS(z) = size;
  INST_TYPE(z)            = type;
  INST_CLASS_OF(z)        = classe;
  INST_ACCESSORS(z)       = classe? INST_SLOT(classe, S_getters_n_setters): STk_nil;
  INST_SLOTS(z)           = STk_must_malloc(size * sizeof(SCM));

  /* Set all the slots to unbound */
  for (i = 0; i < size; i++)
    INST_SLOT(z, i) = STk_void;

  return z;
}


/*
 * compute-getters-n-setters
 *      This version doesn't handle slot options. It serves only for booting
 *      classes and will be overloaded in Scheme.
 *
 */
static SCM compute_getters_n_setters(SCM slots)
{
  SCM res = STk_nil;
  int i   = 0;

  /* Build a kind of A-list which is something like
   *     ( .... (slot-name #f . 3) ... )
   * where #f is the slot initialization function and 3 is the offset of a slot
   * in a the vector of slots
   */
  for (  ; !NULLP(slots); slots = CDR(slots))
    res = STk_cons(STk_cons(CAR(slots),
                            STk_cons(STk_false, MAKE_INT(i++))),
                   res);
  return res;
}

/*
 * create_Top_Object_Class
 *      Creation of the base classes: <Top> <Object> and <Class>.
 */
static void create_Top_Object_Class(void)
{
  SCM tmp, slots_of_class = STk_cons(STk_intern("name"),
                            STk_cons(STk_intern("direct-supers"),
                            STk_cons(STk_intern("direct-slots"),
                            STk_cons(STk_intern("direct-subclasses"),
                            STk_cons(STk_intern("direct-methods"),
                            STk_cons(STk_intern("cpl"),
                            STk_cons(STk_intern("slots"),
                            STk_cons(STk_intern("nfields"),
                            STk_cons(STk_intern("getters-n-setters"),
                            STk_cons(STk_intern("redefined"),
                                     STk_nil))))))))));
  SCM current_module = STk_STklos_module;


  /* ========== Creation of the <Class> class  ========== */
  tmp   = STk_intern("<class>");
  Class = make_instance((SCM) NULL, NUMBER_OF_CLASS_SLOTS, TYPE_INSTANCE);


  INST_CLASS_OF(Class)  = Class;
  INST_ACCESSORS(Class) = compute_getters_n_setters(slots_of_class);

  INST_SLOT(Class, S_name)               = tmp;
  INST_SLOT(Class, S_direct_supers)      = STk_nil;  /* will be changed */
  INST_SLOT(Class, S_direct_slots)       = slots_of_class;
  INST_SLOT(Class, S_direct_subclasses)  = STk_nil;
  INST_SLOT(Class, S_direct_methods)     = STk_nil;
  INST_SLOT(Class, S_cpl)                = STk_nil;  /* will be changed */
  INST_SLOT(Class, S_slots)              = slots_of_class;
  INST_SLOT(Class, S_nfields)            = MAKE_INT(NUMBER_OF_CLASS_SLOTS);
  INST_SLOT(Class, S_getters_n_setters)  = INST_ACCESSORS(Class);
  INST_SLOT(Class, S_redefined)          = STk_false;

  STk_define_variable(tmp, Class, current_module);


  /* ========== Creation of the <Top> class  ========== */
  tmp = STk_intern("<top>");
  Top = basic_make_class(Class, tmp, STk_nil, STk_nil);

  STk_define_variable(tmp, Top, current_module);


  /* ========== Creation of the <Object> class  ========== */
  tmp    = STk_intern("<object>");
  Object = basic_make_class(Class, tmp, LIST1(Top), STk_nil);

  STk_define_variable(tmp, Object, current_module);

  /*
   * <top> <object> and <class> were partially initialized.
   * Correct them here
   */
  INST_SLOT(Object, S_direct_subclasses) = LIST1(Class);
  INST_SLOT(Class, S_direct_supers)      = LIST1(Object);
  INST_SLOT(Class, S_cpl)                = LIST3(Class, Object, Top);
}


static void mk_cls(SCM *var, char *name, SCM meta, SCM super, SCM slots)
{
   SCM tmp = STk_intern(name);

   *var = basic_make_class(meta, tmp, LIST1(super), slots);
   STk_define_variable(tmp, *var, STk_STklos_module);
}

static void make_standard_classes(void)
{
  SCM tmp1 = LIST3(STk_intern("generic-function"),
                   STk_intern("specializers"),
                   STk_intern("procedure"));
  SCM tmp2 = LIST3(STk_intern("name"),
                   STk_intern("methods"),
                   STk_intern("documentation"));

  /* Generic functions classes */
  mk_cls(&Procedure_class, "<procedure-class>", Class, Class,           STk_nil);
  mk_cls(&Entity_class,    "<entity-class>",    Class, Procedure_class, STk_nil);
  mk_cls(&Method,          "<method>",          Class, Object,          tmp1);
  mk_cls(&Simple_method,   "<simple-method>",   Class, Method,          STk_nil);
  mk_cls(&Accessor,        "<accessor-method>", Class, Simple_method,   STk_nil);
  mk_cls(&Generic,         "<generic>",         Entity_class, Object,   tmp2);

  /* Primitive types classes */
  mk_cls(&Boolean,      "<boolean>",    Class,           Top,       STk_nil);
  mk_cls(&Char,         "<char>",       Class,           Top,       STk_nil);
  mk_cls(&Liste,        "<list>",       Class,           Top,       STk_nil);
  mk_cls(&Pair,         "<pair>",       Class,           Liste,     STk_nil);
  mk_cls(&Null,         "<null>",       Class,           Liste,     STk_nil);
  mk_cls(&String,       "<string>",     Class,           Top,       STk_nil);
  mk_cls(&Symbol,       "<symbol>",     Class,           Top,       STk_nil);
  mk_cls(&Vector,       "<vector>",     Class,           Top,       STk_nil);
  mk_cls(&Number,       "<number>",     Class,           Top,       STk_nil);
  mk_cls(&Complex,      "<complex>",    Class,           Number,    STk_nil);
  mk_cls(&Real,         "<real>",       Class,           Complex,   STk_nil);
  mk_cls(&Rational,     "<rational>",   Class,           Real,      STk_nil);
  mk_cls(&Integer,      "<integer>",    Class,           Rational,  STk_nil);
  mk_cls(&Keyword,      "<keyword>",    Class,           Top,       STk_nil);
  mk_cls(&Eof,          "<eof>",        Class,           Top,       STk_nil);
  mk_cls(&Struct,       "<struct>",     Class,           Top,       STk_nil);
  mk_cls(&Struct_type,  "<struct-type>",Class,           Top,       STk_nil);
  mk_cls(&Cond,         "<condition>",  Class,           Top,       STk_nil);
  mk_cls(&Cond_type,    "<condition-type>",Class,        Top,       STk_nil);
  mk_cls(&Box,          "<ref>",        Class,           Top,       STk_nil);
  mk_cls(&UnknownClass, "<unknown>",    Class,           Top,       STk_nil);
  mk_cls(&Procedure,    "<procedure>",  Procedure_class, Top,       STk_nil);
}





/*===========================================================================*\
 *
 *                        U S E R   F U N C T I O N S
 *
\*===========================================================================*/

/* A simple make which handles only creation of gf, methods and classes
 * (no instances). Since this code will disappear when the object system
 * will be fully booted, no extra control are done.
 */
DEFINE_PRIMITIVE("%make", basic_make, subr3, (SCM classe, SCM kind, SCM l))
{
  SCM z;

  if (kind == STk_intern("generic")) {
    /* This is a <generic> */
    z = make_instance(classe,
                      STk_int_length(INST_SLOT(classe, S_slots)),
                      TYPE_GENERIC);
    INST_SLOT(z, S_name)          = l;
    INST_SLOT(z, S_methods)       = STk_nil;
    INST_SLOT(z, S_documentation) = STk_false;
  } else if (kind == STk_intern("method")) {
    /* This is a <method>, <simple-method> or <accessor-method> */
    z = make_instance(classe,
                      STk_int_length(INST_SLOT(classe, S_slots)),
                      TYPE_INSTANCE);
    INST_SLOT(z, S_generic_function) = CAR(l);
    INST_SLOT(z, S_specializers)     = CAR(CDR(l));
    INST_SLOT(z, S_procedure)        = CAR(CDR(CDR(l)));
  } else {
    /* This is a <class> */
    z = make_instance(classe,
                      STk_int_length(INST_SLOT(classe, S_slots)),
                      TYPE_INSTANCE);
    INST_SLOT(z, S_name)          = CAR(l);
    INST_SLOT(z, S_direct_supers) = CAR(CDR(l));
    INST_SLOT(z, S_direct_slots)  = CAR(CDR(CDR(l)));
  }
  return z;
}

/* ==== I N T R O S P E C T I O N ==== */

DEFINE_PRIMITIVE("instance?", instancep, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(INSTANCEP(obj));
}

DEFINE_PRIMITIVE("class?", classp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CLASSP(obj));
}

DEFINE_PRIMITIVE("generic?", genericp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(GENERICP(obj));
}

DEFINE_PRIMITIVE("method?", methodp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(METHODP(obj));
}

DEFINE_PRIMITIVE("class-of", class_of, subr1, (SCM obj))
{
  if (INSTANCEP(obj)) {
    test_change_class(obj);
    return INST_CLASS_OF(obj);
  }

  /* Not instances objects */
  if (SCONSTP(obj)) {
    /* Expression is a small constant */
    switch (AS_LONG(obj)) {
      case AS_LONG(STk_nil):    return Null;
      case AS_LONG(STk_false):
      case AS_LONG(STk_true):   return Boolean;
      case AS_LONG(STk_eof):    return Eof;
      default:                  return UnknownClass;
    }
  }

  if (INTP(obj))
    return Integer;

  if (CHARACTERP(obj))
    return Char;

  switch (BOXED_TYPE(obj)) {
    case tc_cons:       return Pair;
    case tc_string:     return String;
    case tc_symbol:     return Symbol;
    case tc_vector:     return Vector;
    case tc_real:       return Real;
    case tc_complex:    return Complex;
    case tc_rational:   return Rational;
    case tc_bignum:     return Integer;
    case tc_keyword:    return Keyword;
    case tc_struct_type:return (COND_TYPEP(obj)) ? Cond_type: Struct_type;
    case tc_struct:     return (CONDP(obj)) ? Cond : Struct;
    case tc_box:        return Box;
    default:
#ifdef FIXME
      //XX      if (EXTENDEDP(obj))
      //XX          return STk_extended_class_of(obj);
      //XX    else
#endif
                        return (STk_procedurep(obj) == STk_true)? Procedure :
                                                                  UnknownClass;
  }
}

/* ==== I N S T A N C E S */

/* %allocate-instance: the low level instance allocation primitive */
DEFINE_PRIMITIVE("%allocate-instance", allocate_instance, subr1, (SCM classe))
{
  int type;

  if (!CLASSP(classe)) error_bad_class(classe);

  type = (classe == Generic)       ? TYPE_GENERIC       :
         (classe == Accessor)      ? TYPE_ACCESSOR      :
         (classe == Simple_method) ? TYPE_SIMPLE_METHOD:
                                     TYPE_INSTANCE;
  return make_instance(classe, INT_VAL(INST_SLOT(classe, S_nfields)), type);
  // FIXME:HORROR
}



/******************************************************************************
 *
 * %modify-instance (used by change-class to modify in place)
 *
 ******************************************************************************/
DEFINE_PRIMITIVE("%modify-instance", modify_instance, subr2, (SCM old, SCM new))
{
  struct instance_obj tmp;

  if (!INSTANCEP(old) || !INSTANCEP(new))
    STk_error("both parameters must be instances");

  /* Exchange the data contained in old and new. We exchange rather than
   * scratch the old value with new to be correct with GC
   */
  tmp = *(struct instance_obj*) old;
  *(struct instance_obj *) old = *(struct instance_obj *) new;
  *(struct instance_obj *) new = tmp;
  return STk_void;
}




/* // FIXME: Remplacer ça par une C-var */
DEFINE_PRIMITIVE("%object-system-initialized", oo_init_done, subr0, (void))
{
  STk_oo_initialized = TRUE;
  return STk_void;
}


/*===========================================================================*\
 *
 *  Instance extended type definition
 *
\*===========================================================================*/

static void print_instance(SCM inst, SCM port, int mode)
{
  char *fct_name;
  SCM fct, res;

  fct_name = (mode == DSP_MODE) ? "display-object" : "write-object";
  fct      = STk_lookup(STk_intern(fct_name), STk_current_module(), &res, FALSE);

  if (fct == STk_void) {
    /* Do a default print */
    STk_fprintf(port, "#[instance %lx]", (unsigned long) inst);
  } else {
    /* Use the ad-hoc function */
    STk_C_apply(fct, 2, inst, port);
  }
}


static struct extended_type_descr xtype_instance = {
  "instance",
  print_instance
};

static struct extended_type_descr xtype_next_method = {
  "next-method",
  NULL,
};


/*=============================================================================*/

int STk_init_object(void)
{
  DEFINE_XTYPE(instance,    &xtype_instance);
  DEFINE_XTYPE(next_method, &xtype_next_method);

  /* Creation of the base classes */
  create_Top_Object_Class();
  make_standard_classes();


  /* Primitives */
  ADD_PRIMITIVE(basic_make);
  ADD_PRIMITIVE(allocate_instance);
  ADD_PRIMITIVE(instancep);
  ADD_PRIMITIVE(classp);
  ADD_PRIMITIVE(genericp);
  ADD_PRIMITIVE(methodp);
  ADD_PRIMITIVE(set_next_method);
  ADD_PRIMITIVE(find_method);
  ADD_PRIMITIVE(method_more_specificp);
  ADD_PRIMITIVE(class_of);
  ADD_PRIMITIVE(slot_ref);
  ADD_PRIMITIVE(slot_set);
  ADD_PRIMITIVE(slot_ref_using_class);
  ADD_PRIMITIVE(slot_set_using_class);
  ADD_PRIMITIVE(fast_slot_ref);
  ADD_PRIMITIVE(fast_slot_set);
  ADD_PRIMITIVE(undoc_slot_ref);

  ADD_PRIMITIVE(slot_boundp);
  ADD_PRIMITIVE(slot_existsp);
  ADD_PRIMITIVE(slot_boundp_using_class);
  ADD_PRIMITIVE(slot_existsp_using_class);

  ADD_PRIMITIVE(initialize_obj);
  ADD_PRIMITIVE(modify_instance);

  ADD_PRIMITIVE(oo_init_done);          /* //FIXME */
  return TRUE;
}
