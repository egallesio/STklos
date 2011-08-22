/*
 * struct.h			-- Low level support for structures
 *
 * Copyright © 2004-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 * Last file update:  6-Jul-2007 17:17 (eg)
 */

/**** Structure types ****/
struct struct_type_obj {
  stk_header header;
  SCM slots;
  SCM name;
  SCM parent;
  SCM printer;
  int size;
};

#define STRUCT_TYPEP(x)		(BOXED_TYPE_EQ((x),tc_struct_type))
#define STRUCT_TYPE_SLOTS(x)	(((struct struct_type_obj *) (x))->slots)
#define STRUCT_TYPE_PARENT(x)	(((struct struct_type_obj *) (x))->parent)
#define STRUCT_TYPE_NAME(x)	(((struct struct_type_obj *) (x))->name)
#define STRUCT_TYPE_PRINTER(x)	(((struct struct_type_obj *) (x))->printer)
#define STRUCT_TYPE_SIZE(x)	(((struct struct_type_obj *) (x))->size)


/**** Structure instances ****/
struct struct_obj {
  stk_header header;
  SCM type;
  SCM slots[1];
};

#define STRUCTP(x)		(BOXED_TYPE_EQ((x),tc_struct))
#define STRUCT_SLOTS(x)		(((struct struct_obj *) (x))->slots)
#define STRUCT_TYPE(x)		(((struct struct_obj *) (x))->type)



EXTERN_PRIMITIVE("make-struct-type", make_struct_type, subr3,
		 (SCM name, SCM parent, SCM slots));
EXTERN_PRIMITIVE("struct-type-slots", st_slots, subr1, (SCM obj));
EXTERN_PRIMITIVE("struct-set!", struct_set, subr3, (SCM s, SCM slot, SCM val));
EXTERN_PRIMITIVE("make-struct", make_struct, vsubr, (int argc, SCM *argv));
EXTERN_PRIMITIVE("struct->list", struct2list, subr1, (SCM s));

SCM STk_int_struct_ref(SCM s, SCM slot);
SCM STk_int_struct_set(SCM s, SCM slot, SCM val);


/**** Conditions ****/
#define COND_FLAG		1
#define COND_TYPEP(x)		(STRUCT_TYPEP(x) && (BOXED_INFO(x) & COND_FLAG))
#define CONDP(x)		(STRUCTP(x) && (BOXED_INFO(x) & COND_FLAG))
#define SET_COND_FLAG(x)	(BOXED_INFO(x) |= COND_FLAG)
