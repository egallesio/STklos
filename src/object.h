/*
 *
 *  o b j e c t . h			-- Objects support
 *
 * Copyright © 1994-2010 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update:  1-Jan-2010 13:28 (eg)
 */


struct instance_obj {
  stk_header header;
  short number_of_slots;
  short type_of_instance;
  SCM classe;
  SCM accessors;
  SCM *slots;
};


#define TYPE_INSTANCE	    0x00
#define TYPE_GENERIC	    0x01
#define TYPE_SIMPLE_METHOD  0x02
#define TYPE_ACCESSOR	    0x04


#define INST_NUMBER_OF_SLOTS(x) (((struct instance_obj *) (x))->number_of_slots)
#define INST_TYPE(x)	    	(((struct instance_obj *) (x))->type_of_instance)
#define INST_CLASS_OF(x)	(((struct instance_obj *) (x))->classe)
#define INST_ACCESSORS(x)	(((struct instance_obj *) (x))->accessors)
#define INST_SLOTS(x)		(((struct instance_obj *) (x))->slots)
#define INST_SLOT(x, i)   	(INST_SLOTS(x)[i])

#define INSTANCEP(x)	    	(BOXED_TYPE_EQ((x), tc_instance))
#define PUREGENERICP(x)	    	(INST_TYPE(x) & TYPE_GENERIC)
#define SIMPLEMETHODP(x)    	(INST_TYPE(x) & TYPE_SIMPLE_METHOD)
#define ACCESSORP(x)	    	(INST_TYPE(x) & TYPE_ACCESSOR)
#define FASTMETHODP(x)	    	(INST_TYPE(x) & (TYPE_ACCESSOR|TYPE_SIMPLE_METHOD))

/*
 * Class slots have a fixed position which is given below
 *
 */
#define S_name 			0 	/* a symbol */
#define S_direct_supers 	1 	/* (class ...) */
#define S_direct_slots		2 	/* ((name . options) ...) */
#define S_direct_subclasses	3	/* (class ...) */
#define S_direct_methods	4	/* (methods ...) */
#define S_cpl			5 	/* (class ...) */
#define S_slots			6	/* ((name . options) ...) */
#define S_nfields		7	/* an integer */
#define S_getters_n_setters	8	/* ((slot getter setter) ...) */
#define S_redefined		9	/* the class to which class was redefined */
#define NUMBER_OF_CLASS_SLOTS	10


/*
 * Generic functions slots have a fixed position which is given below
 *
 */
#define S_methods		1	/* offset of methods slot in a <generic> */
#define S_documentation		2	/* offset of doc. slot in a <generic> */

/*
 * Methods slots have a fixed position which is given below
 *
 */
#define S_generic_function	0	/* offset of gf    slot in a <method> */
#define S_specializers		1	/* offset of spec. slot in a <method> */
#define S_procedure 		2	/* offset of proc. slot in a <method> */

#define SUBCLASSP(c1, c2)   	(STk_memq(c2, INST_SLOT(c1, S_cpl)) != STk_false)


/* Next-method is always the first variable of the surrounding environment
 * of the closure which implement method body.
 */
#define SET_NEXT_METHOD(closure, value) \
		FRAME_LOCAL(CLOSURE_ENV(closure),0) = (value)

EXTERN_PRIMITIVE("method?", methodp, subr1, (SCM obj));
EXTERN_PRIMITIVE("generic?", genericp, subr1, (SCM obj));

SCM STk_compute_applicable_methods(SCM gf, int argc, SCM *argv, int find_method);


/*===========================================================================*\
 *
 * 			N E X T - M E T H O D S   s t u f f
 *
\*===========================================================================*/
struct next_method_obj {
  stk_header header;
  SCM gf;
  SCM curr_method;
  SCM methods;
  int argc;
  SCM argv[1];
};

#define NXT_MTHD_GF(x)		(((struct next_method_obj *) (x))->gf)
#define NXT_MTHD_METHOD(x)	(((struct next_method_obj *) (x))->curr_method)
#define NXT_MTHD_METHODS(x)	(((struct next_method_obj *) (x))->methods)
#define NXT_MTHD_ARGC(x)	(((struct next_method_obj *) (x))->argc)
#define NXT_MTHD_ARGV(x)	(((struct next_method_obj *) (x))->argv)


#define NXT_MTHD_ALLOC_BYTES(len) \
     (sizeof(struct next_method_obj) + ((len)-1)*sizeof(SCM))



SCM STk_make_next_method(SCM gf, int argc, SCM *argv, SCM methods);

SCM STk_int_call_gf(char *name, SCM val, int nargs); /* FIXME: Utilisée encore? */


extern int STk_oo_initialized;			     /* FIXME: */
