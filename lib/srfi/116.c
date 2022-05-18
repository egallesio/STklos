/*
 * srfi-116.c   -- Implementation of SRFI-116: Immutable List Library
 *
 * Copyright © 2022 Jerônimo Pellegrini <j_p@aleph0.info>
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
 *           Author: Jerônimo Pellegrini [j_p@aleph0.info]
 *    Creation date: 01-May-2022 20:12
 * Last file update: 18-May-2022 16:13 (eg)
 */


#include <stklos.h>
#include "116-incl.c"

extern SCM STk_append2(SCM l1, SCM l2);
EXTERN_PRIMITIVE("append", append, vsubr, (int argc, SCM* argv));



/*              */
/* CONSTRUCTORS */
/*              */

DEFINE_PRIMITIVE("ipair",srfi_116_ipair,subr2, (SCM a, SCM d))
/*
<doc ipair
 * (ipair a d)
 *
 * Returns a newly allocated |ipair| whose |icar| is |a| and whose |icdr| is |d|.
 * The ipair is guaranteed to be different (in the sense of |eqv?|)
 * from every existing object.
doc>
 */
{
    SCM cons = STk_cons(a,d);
    BOXED_INFO(cons) |= CONS_CONST;
    return cons;
}

DEFINE_PRIMITIVE("ilist",srfi_116_ilist,vsubr, (int argc, SCM *argv))
/*
<doc ilist
 * (ilist obj ...)
 *
 * Returns a newly allocated ilist of its arguments.
 * @lisp
 *    (ilist 'a (+ 3 4) 'c)            =>  (a 7 c)
 *    (ilist)                          =>  ()
 * @end lisp
 * Being an ilist, its CAR, CDR and all sublists are immutable.
doc>
 */
{
  register SCM *tmp, l = STk_nil;

  for (tmp = argv-argc+1; tmp <= argv; tmp++) {
    l = STk_cons(*tmp, l);
    BOXED_INFO(l) |= CONS_CONST;
  }
  return l;
}

/*
<doc EXT ipair*
 * (ipair* obj ...)
 *
 * |ipair*| is like |ilist| except that the last argument to |ipair*| is
 * used as the ,(emph "cdr") of the last pair constructed.
 * @lisp
 *    (ipair* 1 2 3)        => (1 2 . 3)
 *    (ipair* 1 2 3 '(4 5)) => (1 2 3 4 5)
 *    (ipair*)              => ()
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("ipair*", srfi_116_ipair_star, vsubr, (int argc, SCM *argv))
{
  register SCM *tmp, l;

  if (argc == 0) return STk_nil;

  tmp = argv-argc+1;
  l   = *tmp;

  for (++tmp; tmp <= argv; tmp++) {
    l = STk_cons(*tmp, l);
    BOXED_INFO(l) |= CONS_CONST;
  }

  return l;
}

/*            */
/* PREDICATES */
/*            */

/*
<doc EXT ipair?
 * (ipair? obj)
 *
 * Returns true and only if |x| is a proper ilist — that is,
 * a ()-terminated ilist.
doc>
*/
DEFINE_PRIMITIVE("ipair?", srfi_116_ipairp, subr1, (SCM obj))
{
    return (CONSP(obj) && (BOXED_INFO(obj) & CONS_CONST))
        ? STk_true
        : STk_false;
}


/*           */
/* SELECTORS */
/*           */

DEFINE_PRIMITIVE("icar+icdr", srfi_116_icar_icdr, subr1, (SCM pair))
/*
<doc EXT icar+icdr
 * (icar+icdr ip)
 *
 * The fundamental ipair deconstructor. Returns two values:
 * the icar and the icdrif |ip|.
doc>
*/
{
    if (!CONSP(pair)) STk_error("bad ipair ~S", pair);
    return STk_n_values(2, CAR(pair), CDR(pair));
}


/*                                   */
/* MISCELLANEOUS: LENGTH, APPEND,    */
/* CONCATENATE, REVERSE, ZIP & COUNT */
/*                                   */

void
STk_lock_list(SCM l) {
    for(;CONSP(l); l = CDR(l))
        BOXED_INFO(l) |= CONS_CONST;
}

void
STk_unlock_list(SCM l) {
    for(;CONSP(l); l = CDR(l))
        BOXED_INFO(l) &= (~CONS_CONST);
}

void
STk_lock_tree(SCM l) {
    if (CONSP(l)) {
        BOXED_INFO(l) |= CONS_CONST;
        STk_lock_tree(CAR(l));
        STk_lock_tree(CDR(l));
    }
    return;
}

void
STk_unlock_tree(SCM l) {
    if (CONSP(l)) {
        BOXED_INFO(l) &= (~CONS_CONST);
        STk_lock_tree(CAR(l));
        STk_lock_tree(CDR(l));
    }
    return;
}

/*
<doc EXT iappend
 * (iappend ilist1 ...)
 *
 * Returns an ilist consisting of the elements of ilist1 followed by the elements of the other ilist parameters.
 *
 * @lisp
 * (iappend (iq x) (iq y))        =>  (x y)
 * (iappend (iq a) (iq b c d))    =>  (a b c d)
 * (iappend (iq a (b)) (iq (c)))  =>  (a (b) (c))
 * @end lisp
 *
 * The resulting ilist is always newly allocated, except that it
 * shares structure with the final ilisti argument. This last argument
 * may be any value at all; an improper ilist results if it is not a
 * proper ilist. All other arguments must be proper ilists.
 *
 * @lisp
 * (iappend (iq a b) (ipair 'c 'd))  =>  (a b c . d)
 * (iappend '() 'a)           =>  a
 * (iappend (iq x y))         =>  (x y)
 * (iappend)                  =>  ()
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("iappend", srfi_116_iappend, vsubr, (int argc, SCM *argv))
{
  SCM res;
  switch (argc) {
    case 0:  res = STk_nil;
             break;
    case 1:    res = *argv;
             break;
    case 2:  res = STk_append2(argv[0], argv[-1]);
             break;
    default: res = STk_append(argc, argv);
             break;
  }
  if (CONSP(res)) STk_lock_list(res);
  return res;
}

/*          */
/* DELETION */
/*          */


/*             */
/* REPLACEMENT */
/*             */

/*
<doc EXT replace-icar replace-icdr
 * (replace-icar ipair object)
 * (replace-icdr ipair object)
 *
 * |Replace-icar| returns an ipair with object in the icar field and
 * the icdr of ipair in the icdr field.
 *
 * |Replace-icdr| returns an ipair with object in the icdr field and
 * the icar of ipair in the icar field.
doc>
*/
DEFINE_PRIMITIVE("replace-icar", srfi_116_replace_icar, subr2, (SCM ipair, SCM obj))
{
    SCM res = STk_cons(obj,CDR(ipair));
    BOXED_INFO(res) |= CONS_CONST;
    return res;
}

DEFINE_PRIMITIVE("replace-icdr", srfi_116_replace_icdr, subr2, (SCM ipair, SCM obj))
{
    SCM res = STk_cons(CAR(ipair), obj);
    BOXED_INFO(res) |= CONS_CONST;
    return res;
}

/*            */
/* CONVERSION */
/*            */

/*
<doc EXT pair->ipair ipair->pair list->ilist ilist->list
 * (pair->ipair pair)
 * (ipair->pair ipair)
 *
 * These procedures, which are inverses, return an ipair and
 * a pair respectively that have the same (i)car and (i)cdr
 * fields as the argument.
doc>
 */
DEFINE_PRIMITIVE("ipair->pair",srfi_116_ipair_pair, subr1, (SCM ipair))
{
    /* Should we also check if the argument is constant?
       (Gauche doesn't) */
    if (!CONSP(ipair)) STk_error("bad ipair ~S", ipair);
    return STk_cons(CAR(ipair), CDR(ipair));
}

DEFINE_PRIMITIVE("pair->ipair",srfi_116_pair_ipair, subr1, (SCM pair))
{
    if (!CONSP(pair)) STk_error("bad cons ~S", pair);
    SCM ipair = STk_cons(CAR(pair), CDR(pair));
    BOXED_INFO(ipair) |= CONS_CONST;
    return ipair;
}

/*
<doc EXT list->ilist ilist->list
 * (list->ilist lst)
 * (ilist->list lst)
 *
 * These procedures return an ilist and a list respectively that have
 * the same elements as the argument. The tails of dotted (i)lists are
 * preserved in the result, which makes the procedures not inverses
 * when the tail of a dotted ilist is a list or vice versa. The empty
 * list is converted to itself.
 *
 * It is an error to apply list->ilist to a circular list.
doc>
 */
DEFINE_PRIMITIVE("list->ilist", srfi_116_list_ilist, subr1, (SCM l))
{
    SCM z = STk_list_copy(l);
    STk_lock_list(z);
    return z;
}

DEFINE_PRIMITIVE("ilist->list", srfi_116_ilist_list, subr1, (SCM l))
{
    SCM z = STk_list_copy(l);
    STk_unlock_list(z);
    return z;
}

/*
<doc EXT tree->itree itree->tree
 * (tree->itree object)
 * (itree->tree object)
 *
 * These procedures walk a tree of pairs or ipairs respectively and
 * make a deep copy of it, returning an isomorphic tree containing
 * ipairs or pairs respectively. The result may share structure with
 * the argument. If the argument is not of the expected type, it is
 * returned.

 * These procedures are not inverses in the general case. For example,
 * a pair of ipairs would be converted by |tree->itree| to an ipair of
 * ipairs, which if converted by |itree->tree| would produce a pair of
 * pairs.
doc>
*/
DEFINE_PRIMITIVE("tree->itree", srfi_116_tree_itree, subr1, (SCM l))
{
    SCM z = STk_list_copy(l);
    STk_lock_tree(z);
    return z;
}

DEFINE_PRIMITIVE("itree->tree", srfi_116_itree_tree, subr1, (SCM l))
{
    SCM z = STk_list_copy(l);
    STk_unlock_tree(z);
    return z;
}




/* EXTRA */

/*
<doc EXT list-lock+! list-lock!
 * (list-lock+! lst)
 * (list-lock! lst)
 *
 * Destructive versions of |list->ilist|: both procedures change their
 * argument so it will become an immutable list.
 * |List-lock+!| returns the list, while |list-lock!| returns |#void|.
doc>
*/
DEFINE_PRIMITIVE("list-lock+!", srfi_116_list_lock_plus, subr1, (SCM obj))
{
    if (!CONSP(obj)) STk_error("bad list ~S", obj);
    STk_lock_list(obj);
    return obj;
}

DEFINE_PRIMITIVE("list-lock!", srfi_116_list_lock, subr1, (SCM obj))
{
    if (!CONSP(obj)) STk_error("bad list ~S", obj);
    STk_lock_list(obj);
    return STk_void;
}

MODULE_ENTRY_START("srfi/116")
{
  SCM module =  STk_create_module(STk_intern("srfi/116"));

  ADD_PRIMITIVE_IN_MODULE(srfi_116_ipair,      module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_ipairp,     module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_ipair_star, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_ilist,      module);

  ADD_PRIMITIVE_IN_MODULE(srfi_116_ipair_pair, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_pair_ipair, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_ilist_list, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_list_ilist, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_itree_tree, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_tree_itree, module);

  ADD_PRIMITIVE_IN_MODULE(srfi_116_icar_icdr,    module);

  ADD_PRIMITIVE_IN_MODULE(srfi_116_iappend,      module);

  ADD_PRIMITIVE_IN_MODULE(srfi_116_replace_icar, module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_replace_icdr, module);

  ADD_PRIMITIVE_IN_MODULE(srfi_116_list_lock_plus,    module);
  ADD_PRIMITIVE_IN_MODULE(srfi_116_list_lock,    module);

  /* Export all the symbols we have just defined */
  STk_export_all_symbols(module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
