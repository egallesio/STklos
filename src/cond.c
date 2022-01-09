/*                                                      -*- coding: utf-8 -*-
 * c o n d . c          -- Condition implementation
 *
 * Copyright © 2004-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
 *
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
 *           Author: Erick Gallesio [eg@essi.fr]
 *    Creation date: 22-May-2004 08:57 (eg)
 * Last file update: 27-Aug-2021 20:23 (eg)
 */

#include "stklos.h"
#include "struct.h"

static SCM root_condition, location_condition, serious_condition, error_condition;
SCM STk_message_condition;
SCM STk_err_mess_condition;
SCM STk_exit_condition;
SCM STk_posix_error_condition;


static void error_bad_type(SCM obj)
{
  STk_error("bad condition type ~S", obj);
}

static void error_bad_cond(SCM obj)
{
  STk_error("bad condition ~S", obj);
}

static void initialize_cond(SCM who, SCM from)
{
  SCM s;

  for (s = STRUCT_TYPE_SLOTS(STRUCT_TYPE(from)); !NULLP(s); s = CDR(s)) {
    STk_struct_set(who, CAR(CAR(s)), STk_int_struct_ref(from, CAR(CAR(s))));
  }
}

/* ----------------------------------------------------------------------
 *      is_a ...
 * ---------------------------------------------------------------------- */
static SCM is_a(SCM type, SCM t)
{
  if (type == t)
    return STk_true;
  else {
    SCM tmp, parent = STRUCT_TYPE_PARENT(type);

    /* We can have:
          - no parent ==> #f
          - parent is a condition type ==> (is-a? parent t)
          - A list ==> if (one of the list is a parent) #t else #f
    */

    if (parent == STk_false)
      return STk_false;
    if (!CONSP(parent))
      return is_a(parent, t);
    for (tmp = parent; !NULLP(tmp); tmp = CDR(tmp)) {
      if (is_a(CAR(tmp), t) == STk_true) return STk_true;
    }
    /* nobody is a t; retrun #f */
    return STk_false;
  }
}



/* ----------------------------------------------------------------------
 *      allocate_condition ...
 * ---------------------------------------------------------------------- */
static SCM allocate_condition(SCM type)
{
  int i, len;
  SCM z;

  len = STRUCT_TYPE_SIZE(type);
  NEWCELL_WITH_LEN(z, struct, sizeof(struct struct_obj) + (len-1) * sizeof(SCM));
  STRUCT_TYPE(z) = type;

  for (i = 0; i < len; i++) {
    STRUCT_SLOTS(z)[i] = STk_void;
  }

  SET_COND_FLAG(z);

  return z;
}


/* ======================================================================
 *
 *                     C O N D I T I O N   T Y P E S
 *
 * ======================================================================
 */

/*
<doc EXT make-condition-type
 * (make-condition-type id  parent  slot-names)
 *
 * |Make-condition-type| returns a new condition type. |Id| must be a symbol
 * that serves as a symbolic name for the condition type. |Parent| must itself
 * be a condition type. |Slot-names| must be a list of symbols. It identifies
 * the slots of the conditions associated with the condition type.
 *
doc>
*/
DEFINE_PRIMITIVE("make-condition-type", make_cond_type, subr3,
                 (SCM name, SCM parent, SCM slots))
{
  SCM z;

  if (STk_int_length(slots) < 0) STk_error("bad slot list ~S", slots);
  if (!COND_TYPEP(parent)) STk_error("bad condition type for parent ~S",parent);

  z = STk_make_struct_type(name, parent, slots);
  SET_COND_FLAG(z);

  return z;
}

/*
<doc EXT make-compound-condition-type
 * (make-compound-condition-type id ct1 ...)
 *
 * |Make-compound-condition-type| returns a new condition type, built
 * from the condition types |ct1|, ...
 * |Id| must be a symbol  that serves as a symbolic name for the
 * condition type. The slots names of the new condition type is the
 * union of the slots of conditions |ct1| ...
 * @l
 * NOTE: This function is not defined in ,(srfi 34).
doc>
*/
DEFINE_PRIMITIVE("make-compound-condition-type", make_comp_cond_type, subr2,
                 (SCM name, SCM parents))
{
  SCM z, tmp, l = STk_nil;

  /* Collect the slots of all the parents */
  if (STk_int_length(parents) < 0)
    STk_error("bad list of parents ~S", parents);

  for (tmp = parents; !NULLP(tmp); tmp = CDR(tmp)) {
    if (!COND_TYPEP(CAR(tmp)))
      STk_error("bad condition type for parent ~S", CAR(tmp));
    l = STk_append2(l, STk_st_slots(CAR(tmp)));
  }

  z = STk_make_struct_type(name, STk_false, l);
  STRUCT_TYPE_PARENT(z) = parents;      /* Here we cheat a little bit */
  SET_COND_FLAG(z);

  return z;
}


/*
<doc EXT condition-type?
 * (condition-type? obj)
 *
 * Returns |#t| if |obj| is a condition type, and |#f| otherwise
doc>
*/
DEFINE_PRIMITIVE("condition-type?", ctp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(COND_TYPEP(obj));
}

/* ======================================================================
 *      STk_defcond_type ...
 * ====================================================================== */
SCM STk_defcond_type(char *name, SCM parent, SCM slots, SCM module)
{
  SCM res, tmp = STk_intern(name);

  if (parent == STk_false) parent = root_condition;
  res = STk_make_cond_type(tmp, parent, slots);
  STk_define_variable(tmp, res, module);
  return res;
}


SCM STk_condition_type_is_a(SCM type, SCM t)
{
  if (!COND_TYPEP(type)) error_bad_type(type);
  if (!COND_TYPEP(t))    error_bad_type(t);
  return is_a(type, t);
}

/* ======================================================================
 *
 *                           C O N D I T I O N S
 *
 * ======================================================================
 */

/*
 * %allocate-condition is used by the condition syntax in srfi-35
 */
DEFINE_PRIMITIVE("%allocate-condition", alloc_cond,  subr1, (SCM type))
{
  if (!COND_TYPEP(type)) error_bad_type(type);
  return allocate_condition(type);
}


/*
<doc EXT make-condition
 * (make-condition type slot-name value  ...)
 *
 * |Make-condition| creates a condition value belonging condition type
 * |type|. The following arguments must be, in turn, a slot name and an
 * arbitrary value. There must be such a pair for each slot of |type| and
 * its direct and indirect supertypes. |Make-condition| returns the
 * condition value, with the argument values associated with their
 * respective slots.
 * @lisp
 * (let* ((ct (make-condition-type 'ct1 &condition '(a b)))
 *        (c  (make-condition ct 'b 2 'a 1)))
 *   (struct->list c))
 *      => ((a . 1) (b . 2))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("make-condition", make_cond,  vsubr, (int argc, SCM *argv))
{
  SCM z, type, slots;

  if (!argc) STk_error("no argument given");
  argc--;
  type = *argv--;

  if (!COND_TYPEP(type)) error_bad_type(type);
  slots = STk_st_slots(type);

  z = allocate_condition(type);
  while (argc) {
    if (argc == 1) STk_error("no value provided for ~S" , *argv);
    if (SYMBOLP(*argv)) {
      if (STk_memq(*argv, slots) != STk_false) {
        /* This is a valid slot initialize it */
        STk_struct_set(z, *argv, *(argv - 1));
        slots = STk_dremq(*argv, slots);
        argv -= 2;
        argc -= 2;
      }
      else
        STk_error("bad slot name ~S", *argv);
    }
    else
      STk_error("bad symbol ~S", *argv);
  }
  /* We have finished to process the user passed list */
  if (!NULLP(slots))
    STk_error("list of uninitialized slots for condition: ~S", slots);

  return z;
}

/*
<doc EXT condition?
 * (condition? obj)
 *
 * Returns |#t| if |obj| is a condition, and |#f| otherwise
doc>
*/
DEFINE_PRIMITIVE("condition?", condp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(CONDP(obj));
}



/*
<doc EXT make-compound-condition
 * (make-compound-condition condition0 condition1 ...)
 *
 * |Make-compound-condition| returns a compound condition belonging to
 * all condition types that the |conditioni| belong to.
 *
 * |Condition-ref|, when applied to a compound condition will return
 *  the value from the first of the |conditioni| that has such a slot.
doc>
*/
DEFINE_PRIMITIVE("make-compound-condition", make_comp_cond, vsubr,
                 (int argc, SCM *argv))
{
  static int counter = 0;
  SCM type, z, cts = STk_nil;
  char buff[100];
  int args = argc;

  /* build a list of all the condition types */
  while (argc--) {
    if (!CONDP(*argv)) error_bad_cond(*argv);
    cts = STk_cons(STRUCT_TYPE(*argv--), cts);
  }

  /* Create a new type and and instance of it for the compound condition */
  snprintf(buff, sizeof(buff), "&cct-%d", counter++);
  type = STk_make_comp_cond_type(STk_make_uninterned_symbol(buff),
                                 cts);
  z = allocate_condition(type);

  /* Rewind args ... */
  argc = args;

  /* ... and initialize new condition from the arguments */
  while(argc--) {
    initialize_cond(z, *++argv);
  }
  return z;
}


/*
<doc EXT condition-ref
 * (condition-ref condition slot-name)
 *
 * |Condition| must be a condition, and |slot-name| a symbol. Moreover,
 * |condition| must belong to a condition type which has a slot name called
 * |slot-name|, or one of its (direct or indirect) supertypes must have the
 * slot. |Condition-ref| returns the value associated with |slot-name|.
 * @lisp
 * (let* ((ct (make-condition-type 'ct1 &condition '(a b)))
 *        (c  (make-condition ct 'b 2 'a 1)))
 *   (condition-ref c 'b))
 *      => 2
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("condition-ref", condition_ref, subr2, (SCM c, SCM slot))
{
  if (!CONDP(c)) error_bad_cond(c);
  return STk_int_struct_ref(c, slot);
}


/*
<doc EXT condition-set!
 * (condition-set! condition slot-name obj)
 *
 * |Condition| must be a condition, and |slot-name| a symbol. Moreover,
 * |condition| must belong to a condition type which has a slot name called
 * |slot-name|, or one of its (direct or indirect) supertypes must have the
 * slot. |Condition-set!| change the value associated with |slot-name| to |obj|.
 * @l
 * NOTE: Whereas |condition-ref| is defined in ,(srfi 35),
 * |confition-set!| is not.
doc>
*/
DEFINE_PRIMITIVE("condition-set!", condition_set, subr3, (SCM c, SCM slot, SCM val))
{
  if (!CONDP(c)) error_bad_cond(c);
  return STk_int_struct_set(c, slot, val);
}


/*
<doc EXT condition-has-type?
 * (condition-has-type? condition condition-type)
 *
 * |Condition-has-type?| tests if |condition| belongs to |condition-type|.
 * It returns |#t| if any of condition 's types includes |condition-type|
 * either directly or as an ancestor and |#f| otherwise.
 * @lisp
 *  (let* ((ct1 (make-condition-type 'ct1 &condition '(a b)))
 *         (ct2 (make-condition-type 'ct2 ct1 '(c)))
 *         (ct3 (make-condition-type 'ct3 &condition '(x y z)))
 *         (c   (make-condition ct2 'a 1 'b 2 'c 3)))
 *   (list (condition-has-type? c ct1)
 *      (condition-has-type? c ct2)
 *      (condition-has-type? c ct3)))
 *     => (#t #t #f)
 * @end lisp
doc>
*/

DEFINE_PRIMITIVE("condition-has-type?", cond_has_typep, subr2, (SCM c, SCM t))
{
  if (!CONDP(c)) error_bad_cond(c);
  if (!COND_TYPEP(t)) error_bad_type(t);

  return is_a(STRUCT_TYPE(c), t);
}



/*
<doc EXT extract-condition
 * (extract-condition condition  condition-type)
 *
 * |Condition| must be a condition belonging to |condition-type|.
 * |Extract-condition| returns a condition of |condition-type|
 * with the slot values specified by |condition|. The new condition
 * is always allocated.
 * @lisp
 * (let* ((ct1 (make-condition-type 'ct1 &condition '(a b)))
 *        (ct2 (make-condition-type 'ct2 ct1 '(c)))
 *        (c2  (make-condition ct2 'a 1 ' b 2 'c 3))
 *        (c1  (extract-condition c2 ct1)))
 *   (list (condition-has-type? c1 ct2)
 *      (condition-has-type? c1 ct1)))
 *       => (#f #t)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("extract-condition", extract_cond, subr2, (SCM c, SCM t))
{
  SCM tmp, z;

  if (!CONDP(c)) error_bad_cond(c);
  if (!COND_TYPEP(t)) error_bad_type(t);
  if (is_a(STRUCT_TYPE(c), t) == STk_false)
    STk_error("condition ~S is not of type ~S", c, t);

  z = allocate_condition(t);
  for (tmp = STRUCT_TYPE_SLOTS(t); !NULLP(tmp); tmp = CDR(tmp)) {
    STk_struct_set(z, CAR(CAR(tmp)), STk_int_struct_ref(c, CAR(CAR(tmp))));
  }
  return z;
}



/* ======================================================================
 *
 *                           E X C E P T I O N S
 *
 * ======================================================================
 */
/*
<doc R7RS raise
 * (raise obj)
 *
 * Invokes the current exception handler on |obj|. The handler is called in
 * the dynamic environment of the call to |raise|, except that the current
 * exception handler is that in place for the call to |with-handler|
 * that installed the handler being called.
 *
 * @lisp
 * (with-handler (lambda (c)
 *              (format "value ~A was raised" c))
 *    (raise 'foo)
 *    (format #t "never printed\\n"))
 *           => "value foo was raised"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("raise", raise, subr1, (SCM obj))
{
  STk_raise_exception(obj);
  return STk_void;
}

SCM STk_make_C_cond(SCM type, int nargs, ...)
{
  va_list ap;
  SCM z = allocate_condition(type);
  int i;

  va_start(ap, nargs);
  for (i=0; i < nargs; i++) {
    STRUCT_SLOTS(z)[i] = va_arg(ap, SCM);
   }
  va_end(ap);

  return z;
}


/* ======================================================================
 *      Init ...
 * ====================================================================== */

#define DEFVAR(x, mod) STk_define_variable(STRUCT_TYPE_NAME(x), x, mod)

int STk_init_cond(void)
{
  SCM module = STk_STklos_module;

  /* Build the special value SRFI-35 &condition */
  NEWCELL(root_condition, struct_type);
  STRUCT_TYPE_SLOTS(root_condition)   = STk_nil;
  STRUCT_TYPE_SIZE(root_condition)    = 0;
  STRUCT_TYPE_NAME(root_condition)    = STk_intern("&condition");
  STRUCT_TYPE_PARENT(root_condition)  = STk_false;
  STRUCT_TYPE_PRINTER(root_condition) = STk_false;
  SET_COND_FLAG(root_condition);
  DEFVAR(root_condition, module);

  /* Build special-values SRFI-35 &message, &serious, &error */
  STk_message_condition = STk_defcond_type("&message", root_condition,
                                           LIST3(STk_intern("message"),
                                                 STk_intern("r7rs-msg"),
                                                 STk_intern("r7rs-irritants")),
                                           module);
  location_condition    = STk_defcond_type("&location", root_condition,
                                           LIST2(STk_intern("location"),
                                                 STk_intern("backtrace")),
                                           module);
  serious_condition     = STk_defcond_type("&serious", root_condition,
                                           STk_nil,
                                           module);
  error_condition       = STk_defcond_type("&error", serious_condition,
                                           LIST1(STk_intern("location")),
                                           module);

  /* Define STklos &error-message condition (used for error messages) */
  STk_err_mess_condition = STk_make_comp_cond_type(STk_intern("&error-message"),
                                                   LIST3(error_condition,
                                                         location_condition,
                                                         STk_message_condition));

  DEFVAR(STk_err_mess_condition, module);

  /* Define the exit-condition used for R7RS exit primitive */
  STk_exit_condition = STk_defcond_type("&exit-r7rs", root_condition,
                                        LIST1(STk_intern("retcode")),
                                        module);

  /* Define the &posix_error used for SRFI-170 */
  STk_posix_error_condition = STk_defcond_type("&posix-error",
                                               STk_err_mess_condition,
                                               LIST2(STk_intern("errname"),
                                                     STk_intern("errno")),
                                        module);

  /* Conditions types */
  ADD_PRIMITIVE(alloc_cond);
  ADD_PRIMITIVE(make_cond_type);
  ADD_PRIMITIVE(ctp);
  ADD_PRIMITIVE(make_comp_cond_type);

  /* Conditions */
  ADD_PRIMITIVE(make_cond);
  ADD_PRIMITIVE(make_comp_cond);
  ADD_PRIMITIVE(condp);
  ADD_PRIMITIVE(condition_ref);
  ADD_PRIMITIVE(condition_set);
  ADD_PRIMITIVE(cond_has_typep);
  ADD_PRIMITIVE(extract_cond);

  /* Exceptions */
  ADD_PRIMITIVE(raise);

  return TRUE;
}
