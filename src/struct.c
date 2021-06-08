/*
 * struct.c         -- Low level support for structures
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
 *    Creation date: 12-May-2004 17:26 (eg)
 * Last file update: 10-Apr-2021 18:47 (eg)
 */

#include "stklos.h"
#include "struct.h"


static void error_bad_type(SCM obj)
{
  STk_error("bad structure type ~S", obj);
}

static void error_bad_struct(SCM obj)
{
  STk_error("bad structure ~S", obj);
}

static void error_bad_slot(SCM slot, SCM obj)
{
  STk_error("bad slot name ~S for ~S", slot, obj);
}


SCM STk_int_struct_ref(SCM s, SCM slot)
{
  SCM index = STk_int_assq(slot, STRUCT_TYPE_SLOTS(STRUCT_TYPE(s)));

  if (!SYMBOLP(slot) || index == STk_false) error_bad_slot(slot, s);

  return STRUCT_SLOTS(s)[INT_VAL(CDR(index))];
}

SCM STk_int_struct_set(SCM s, SCM slot, SCM val)
{
  SCM index = STk_int_assq(slot, STRUCT_TYPE_SLOTS(STRUCT_TYPE(s)));

  if (!SYMBOLP(slot) || index == STk_false) error_bad_slot(slot, s);

  STRUCT_SLOTS(s)[INT_VAL(CDR(index))] = val;
  return STk_void;
}




/* ======================================================================
 *
 *                 S T R U C T U R E   T Y P E S
 *
 * ======================================================================
 */
/*
<doc EXT make-struct-type
 * (make-struct-type name parent slots)
 *
 * This form which is more general than |define-struct| permits to define a
 * new structure type whose name is |name|. Parent is the structure
 * type from which is the new structure type is a subtype (or |#f| is the
 * new structure-type has no super type). |Slots| is the list of the slot
 * names which constitute the structure tpe.
 * @l
 * When a structure type is s subtype of a previous type, its slots are added
 * to the ones of the super type.
doc>
*/
DEFINE_PRIMITIVE("make-struct-type", make_struct_type, subr3,
         (SCM name, SCM parent, SCM slots))
{
  SCM z, all_slots = STk_nil;
  int alloc = 0;

  if (STk_int_length(slots) < 0) STk_error("bad slot list ~S", slots);

  if (parent != STk_false) {
    if (!STRUCT_TYPEP(parent)) STk_error("bad structure type for parent ~S",parent);
    all_slots  = STRUCT_TYPE_SLOTS(parent);
    alloc      = STRUCT_TYPE_SIZE(parent);
  }

  for ( ; !NULLP(slots); slots = CDR(slots)) {
    if (STk_int_assq(CAR(slots), all_slots) == STk_false) {
      /* Add this slot to all_slots */
      all_slots = STk_cons(STk_cons(CAR(slots),
                     MAKE_INT(alloc++)),
                all_slots);
    }
  }

  /* all_slots is of the form ((f1 . 0) (f2 . 1))
   * The index is the slot-address
   */
  NEWCELL(z, struct_type);
  STRUCT_TYPE_SLOTS(z)   = all_slots;
  STRUCT_TYPE_SIZE(z)    = alloc;
  STRUCT_TYPE_NAME(z)    = name;
  STRUCT_TYPE_PARENT(z)  = parent;
  STRUCT_TYPE_PRINTER(z) = STk_false;
  return z;
}


/*
<doc EXT struct-type?
 * (struct-type? obj)
 *
 * Returns |#t| if |obj| is a structure type, otherwise returns |#f|.
 * @lisp
 * (let ((type (make-struct-type 'point #f '(x y))))
 *   (struct-type? type))         => #t
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("struct-type?", stp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(STRUCT_TYPEP(obj));
}


/*
<doc EXT struct-type-slots
 * (struct-type-slots structype)
 *
 * Returns the slots of the structure type |structype| as a list.
 * @lisp
 * (define point  (make-struct-type 'point #f '(x y)))
 * (define circle (make-struct-type 'circle point '(r)))
 * (struct-type-slots point)   => (x y)
 * (struct-type-slots circle)  => (x y r)
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("struct-type-slots", st_slots, subr1, (SCM obj))
{
  SCM tmp, res = STk_nil;

  if (!STRUCT_TYPEP(obj)) error_bad_type(obj);

  for (tmp = STRUCT_TYPE_SLOTS(obj); !NULLP(tmp); tmp = CDR(tmp))
    res = STk_cons(CAR(CAR(tmp)), res);

  return res;
}

/*
<doc EXT struct-type-parent
 * (struct-type-parent structype)
 *
 * Returns the super type of the structure type |structype|, if it exists
 * or |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("struct-type-parent", st_parent, subr1, (SCM obj))
{
  if (!STRUCT_TYPEP(obj)) error_bad_type(obj);
  return STRUCT_TYPE_PARENT(obj);
}

/*
<doc EXT struct-type-name
 * (struct-type-name structype)
 *
 * Returns the name associated to the structure type |structype|.
doc>
*/
DEFINE_PRIMITIVE("struct-type-name", st_name, subr1, (SCM obj))
{
  if (!STRUCT_TYPEP(obj)) error_bad_type(obj);
  return STRUCT_TYPE_NAME(obj);
}


/*
<doc EXT struct-type-change-writer!
 * (struct-type-change-writer! structype proc)
 *
 * Change the default writer associated to structures of type |structype| to
 * to the |proc| procedure. The |proc| procedure must accept 2 arguments
 * (the structure to write and the port wher the structure must be written
 * in that order). The value returned by |struct-type-change-writer!| is the
 * old writer associated to |structype|. To restore the standard wtructure
 * writer for |structype|, use the special value |#f|.
 *
 * @lisp
 * (define point (make-struct-type 'point #f '(x y)))
 *
 * (struct-type-change-writer!
 *   point
 *   (lambda (s port)
 *     (let ((type (struct-type s)))
 *       (format port "{~A" (struct-type-name type))
 *       ;; display the slots and their value
 *       (for-each (lambda (x)
 *        (format port " ~A=~S" x (struct-ref s x)))
 *      (struct-type-slots type))
 *       (format port "}"))))
 * (display (make-struct point 1 2)) @print{} {point x=1 y=2}
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("struct-type-change-writer!",
         st_change_writer, subr2, (SCM st, SCM proc))
{
  SCM res;

  if (!STRUCT_TYPEP(st)) error_bad_type(st);
  if (proc != STk_false && (STk_procedurep(proc) == STk_true) &&
      STk_proc_arity(proc) != MAKE_INT(2))
    STk_error("bad wrtiter procedure ~S", proc);

  res             = STRUCT_TYPE_PRINTER(st);
  STRUCT_TYPE_PRINTER(st) = proc;

  return res;
}


static char *get_struct_type_name(SCM st)
{
  SCM name = STRUCT_TYPE_NAME(st);

  return (SYMBOLP(name))? SYMBOL_PNAME(name) :
         (KEYWORDP(name))? KEYWORD_PNAME(name) :
         "";
}


static void print_struct_type(SCM expr, SCM port, int _UNUSED(mode))
{
  char buffer[1000];

  sprintf(buffer, "#[%s-type %s %ld]",
      COND_TYPEP(expr) ? "condition": "struct",
      get_struct_type_name(expr),
      (unsigned long) expr);
  STk_puts(buffer, port);
}


/* ======================================================================
 *
 *                      S T R U C T U R E S
 *
 * ======================================================================
 */

/*
<doc EXT make-struct
 * (make-struct structype expr ...)
 *
 * Returns a newly allocated instance of the structure type |structype|,
 * whose slots are initialized to |expr| ... If fewer |expr| than the number of
 * instances are given to |make-struct|, the remaining slots are inialized with
 * the special ,(emph "void") value.
doc>
*/
DEFINE_PRIMITIVE("make-struct", make_struct, vsubr, (int argc, SCM *argv))
{
  SCM z, type;
  int i, len;

  if (!argc) STk_error("no argument given");
  argc--;
  type = *argv--;

  if (!STRUCT_TYPEP(type)) error_bad_type(type);
  len = STRUCT_TYPE_SIZE(type);
  if (argc > len) STk_error("too much initializers for ~S", type);

  NEWCELL_WITH_LEN(z, struct, sizeof(struct struct_obj) + (len-1) * sizeof(SCM));

  STRUCT_TYPE(z) = type;
  for (i = 0; i < len; i++) {
    STRUCT_SLOTS(z)[i] = (argc-- > 0) ? *argv-- : STk_void;
  }
  return z;
}

/*
<doc EXT struct?
 * (struct? obj)
 *
 * Returns |#t| if |obj| is a structure, otherwise returns |#f|.
 * @lisp
 * (let* ((type (make-struct-type 'point #f '(x y)))
 *        (inst (make-struct type 1 2)))
 *   (struct? inst))         => #t
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("struct?", structp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(STRUCTP(obj));
}


/*
<doc EXT struct-type
 * (struct-type s)
 *
 * Returns the structure type of the |s| structure
doc>
*/
DEFINE_PRIMITIVE("struct-type", struct_type, subr1, (SCM s))
{
  if (!STRUCTP(s)) error_bad_struct(s);
  return STRUCT_TYPE(s);
}

/*
<doc EXT struct-ref
 * (struct-ref s slot-name)
 *
 * Returns the value associated to slot |slot-name| of the |s| structure.
 * @lisp
 * (define point  (make-struct-type 'point #f '(x y)))
 * (define circle (make-struct-type 'circle point '(r)))
 * (define p (make-struct point 1 2))
 * (define c (make-struct circle 10 20 30))
 * (struct-ref p 'y) => 2
 * (struct-ref c 'r) => 30
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("struct-ref", struct_ref, subr2, (SCM s, SCM slot))
{
  if (!STRUCTP(s)) error_bad_struct(s);
  return STk_int_struct_ref(s, slot);
}


/*
<doc EXT struct-set!
 * (struct-set! s slot-name value)
 *
 * Stores value in the to slot |slot-name| of the |s| structure. The value
 * returned by |struct-set!| is ,(emph "void").
 *
 * @lisp
 * (define point  (make-struct-type 'point #f '(x y)))
 * (define p (make-struct point 1 2))
 * (struct-ref p 'x) => 1
 * (struct-set! p 'x 0)
 * (struct-ref p 'x) => 0
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("struct-set!", struct_set, subr3, (SCM s, SCM slot, SCM val))
{
  if (!STRUCTP(s)) error_bad_struct(s);
  return STk_int_struct_set(s, slot, val);
}


/*
<doc EXT struct-is-a?
 * (struct-is-a? s structype)
 *
 * Return a boolean that indicates if the structure |s| is a of type |structype|.
 * Note that if |s| is an instance of a subtype of ,(emph "S"), it is considered
 * also as an instance of type ,(emph "S").
 *
 * @lisp
 * (define point  (make-struct-type 'point #f '(x y)))
 * (define circle (make-struct-type 'circle point '(r)))
 * (define p (make-struct point 1 2))
 * (define c (make-struct circle 10 20 30))
 * (struct-is-a? p point)   => #t
 * (struct-is-a? c point)   => #t
 * (struct-is-a? p circle)  => #f
 * (struct-is-a? c circle)  => #t
 * @end lisp
doc>
*/
static SCM is_a(SCM type, SCM t)
{
  if (type == t)
    return STk_true;
  else {
    if (STRUCT_TYPE_PARENT(type) != STk_false)
      return is_a(STRUCT_TYPE_PARENT(type), t);
    else
      return STk_false;
  }
}

DEFINE_PRIMITIVE("struct-is-a?", struct_isa, subr2, (SCM s, SCM t))
{
  if (!STRUCTP(s))      error_bad_struct(s);
  if (!STRUCT_TYPEP(t)) error_bad_type(t);
  if (CONDP(s))         return STk_false;
  return is_a(STRUCT_TYPE(s), t);
}


/*
<doc EXT struct->list
 * (struct->list s)
 *
 * Returns the content of structure |s| as an A-list whose keys are the
 * slots of the structure type of |s|.
 * @lisp
 * (define point  (make-struct-type 'point #f '(x y)))
 * (define p (make-struct point 1 2))
 * (struct->list p) => ((x . 1) (y . 2))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("struct->list", struct2list, subr1, (SCM s))
{
  SCM type, slots, l = STk_nil;
  int i = 0;

  if (!STRUCTP(s)) error_bad_struct(s);

  type = STRUCT_TYPE(s);
  i    = STRUCT_TYPE_SIZE(type) - 1;

  for (slots = STRUCT_TYPE_SLOTS(type); !NULLP(slots); slots=CDR(slots)) {
    l = STk_cons(STk_cons(CAR(CAR(slots)), STRUCT_SLOTS(s)[i--]),
         l);
  }
  return l;
}


/*
 * Fast getters & setters (used by define-struct)
 *
 */
DEFINE_PRIMITIVE("%fast-struct-ref", fast_struct_ref, subr4,
         (SCM s, SCM st, SCM who, SCM offset))
{
  if (!STRUCTP(s))
    STk_error("bad structure ~S in ~S", s, who);
  if (STRUCT_TYPE(s) != st)
    STk_error("bad ~S in ~S: ~S", STRUCT_TYPE_NAME(st), who, s);

  return STRUCT_SLOTS(s)[INT_VAL(offset)];
}

DEFINE_PRIMITIVE("%fast-struct-set!", fast_struct_set, subr5,
         (SCM s, SCM st, SCM who, SCM offset, SCM val))
{
  if (!STRUCTP(s))
        STk_error("bad structure ~S in setter of ~S", s, who);
  if (STRUCT_TYPE(s) != st)
    STk_error("bad ~S in setter of ~S: ~S", STRUCT_TYPE_NAME(st), who, s);

   STRUCT_SLOTS(s)[INT_VAL(offset)] = val;
   return STk_void;
}



static void print_struct(SCM expr, SCM port, int _UNUSED(mode))
{
  SCM type = STRUCT_TYPE(expr);
  char buffer[1000];

  if (STRUCT_TYPE_PRINTER(type) != STk_false) {
    /* we have a user defined writer */
    STk_C_apply(STRUCT_TYPE_PRINTER(type), 2, expr, port);
  } else {
    /* Use the default writer */
    sprintf(buffer, "#[%s %s %ld]",
        (CONDP(expr) ? "condition" : "struct"),
        get_struct_type_name(STRUCT_TYPE(expr)),
        (unsigned long) expr);
    STk_puts(buffer, port);
  }
}


/* ----------------------------------------------------------------------
 *  Initialize ...
 * ----------------------------------------------------------------------
 */
static struct extended_type_descr xtype_struct_type = {
  .name  = "struct-type",
  .print = print_struct_type
};

static struct extended_type_descr xtype_struct = {
  .name  = "struct",
  .print = print_struct
};


int STk_init_struct(void)
{
  DEFINE_XTYPE(struct_type, &xtype_struct_type);
  DEFINE_XTYPE(struct, &xtype_struct);

  /* Structure types */
  ADD_PRIMITIVE(make_struct_type);
  ADD_PRIMITIVE(stp);
  ADD_PRIMITIVE(st_slots);
  ADD_PRIMITIVE(st_name);
  ADD_PRIMITIVE(st_parent);
  ADD_PRIMITIVE(st_change_writer);

  /* Structures */
  ADD_PRIMITIVE(make_struct);
  ADD_PRIMITIVE(structp);
  ADD_PRIMITIVE(struct_type);
  ADD_PRIMITIVE(struct_ref);
  ADD_PRIMITIVE(struct_set);
  ADD_PRIMITIVE(struct_isa);
  ADD_PRIMITIVE(struct2list);
  ADD_PRIMITIVE(fast_struct_ref);
  ADD_PRIMITIVE(fast_struct_set);

  return TRUE;
}

