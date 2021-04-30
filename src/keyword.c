/*
 *
 * k e y w o r d . c                            -- Keywords management
 *
 * Copyright © 1993-2021 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 19-Nov-1993 16:12
 * Last file update: 30-Apr-2021 09:41 (eg)
 */

#include "stklos.h"
#include "hash.h"

static struct hash_table_obj keyword_table;     /* The keyword "obarray" */


/*===========================================================================*\
 *                              Utilities
\*===========================================================================*/

static void error_bad_keyword(SCM obj)
{
  STk_error("bad keyword ~S", obj);
}

static void error_bad_list(SCM obj)
{
  STk_error("bad list of keywords ~S", obj);
}

static void error_const_cell(SCM x)
{
  STk_error("changing the constant ~s is not allowed", x);
}

static SCM make_uninterned_keyword(char *name)
{
  SCM z;

  NEWCELL(z, keyword);
  SYMBOL_PNAME(z) = name;       /* already duplicated in STk_makekey */
  BOXED_INFO(z) |= STk_symbol_flags(name);
  return z;
}


SCM STk_makekey(char *token)
{
  SCM res;
  MUT_DECL(lck);

  MUT_LOCK(lck);
  res =  STk_hash_intern_symbol(&keyword_table,
                                STk_strdup(token),
                                make_uninterned_keyword);
  MUT_UNLOCK(lck);

  return res;
}

/*===========================================================================*\
 *
 *                              PRIMITIVES
 *
\*===========================================================================*/

/*
<doc EXT make-keyword
 * (make-keyword s)
 *
 * Builds a keyword from the given |s|. The parameter |s| must be a symbol
 * or a string.
 * @lisp
 * (make-keyword "test")    => :test
 * (make-keyword 'test)     => :test
 * (make-keyword ":hello")  => ::hello
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("make-keyword", make_keyword, subr1, (SCM str))
{
  char *s = "";

  if (STRINGP(str))
    s = STRING_CHARS(str);
  else if (SYMBOLP(str))
    s = SYMBOL_PNAME(str);
  else STk_error("~S is not a string or a symbol", str);

  return STk_makekey(s);
}


/*
<doc EXT keyword?
 * (keyword obj)
 *
 * Returns |#t| if |obj| is a keyword, otherwise returns |#f|.
 * @lisp
 * (keyword? 'foo)     => #f
 * (keyword? ':foo)    => #t
 * (keyword? 'foo:)    => #t
 * (keyword? :foo)     => #t
 * (keyword? foo:)     => #t
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("keyword?", keywordp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(KEYWORDP(obj));
}


/*
<doc EXT keyword->string
 * (keyword->string key)
 *
 * Returns the name of |key| as a string. The result does not contain a colon.
doc>
 */
DEFINE_PRIMITIVE("keyword->string", keyword2string, subr1, (SCM obj))
{
 SCM res;

 if (!KEYWORDP(obj)) error_bad_keyword(obj);
 res = STk_Cstring2string(KEYWORD_PNAME(obj));
 return res;
}


/*
<doc EXT  key-get
 * (key-get list key)
 * (key-get list key default)
 *
 * |List| must be a list of keywords and their respective values.
 * |key-get| scans the |list| and returns the value
 * associated with the given |key|. If  |key| does
 * not appear in an odd position in |list|, the specified
 * |default| is returned, or an error is raised if no |default| was
 * specified.
 * @lisp
 * (key-get '(:one 1 :two 2) :one)     => 1
 * (key-get '(:one 1 :two 2) :four #f) => #f
 * (key-get '(:one 1 :two 2) :four)    => error
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("key-get", key_get, subr23, (SCM l, SCM key, SCM dflt))
{
  SCM save = l;

  if (!KEYWORDP(key)) error_bad_keyword(key);
  if (CONSP(l) || NULLP(l)) {
    int i, len = STk_int_length(l);

    if (len< 0 || len&1) error_bad_list(save);

    for (i = 0; i < len; i+=2) {
      if (!KEYWORDP(CAR(l))) error_bad_keyword(CAR(l));
      if (strcmp(KEYWORD_PNAME(CAR(l)), KEYWORD_PNAME(key))==0) return CAR(CDR(l));
      l = CDR(CDR(l));
    }
    if (!dflt) STk_error("value ~S not found in list ~S", key, save);
    return dflt;
  } else {
    error_bad_list(key);
    return STk_void; /* never reached */
  }
}


/*
<doc EXT key-set!
 * (key-set! list key value)
 *
 * |List| must be a list of keywords and their respective values.
 * |key-set!| sets the value associated to |key| in the keyword list.
 * If the key is already present in |list|, the keyword list is
 * ,(emph "physically") changed.
 * @lisp
 * (let ((l (list :one 1 :two 2)))
 *   (set! l (key-set! l :three 3))
 *   (cons (key-get l :one)
 *         (key-get l :three)))            => (1 . 3)
 * @end lisp
doc>
  */
DEFINE_PRIMITIVE("key-set!", key_set, subr3, (SCM l, SCM key, SCM val))
{
  SCM save = l;

  if (!KEYWORDP(key)) error_bad_keyword(key);
  if (CONSP(l) || NULLP(l)) {
    int i, len = STk_int_length(l);

    if (len< 0 || len&1) error_bad_list(save);

    for (i = 0; i < len; i+=2) {
      if (!KEYWORDP(CAR(l))) error_bad_keyword(CAR(l));
      if (strcmp(KEYWORD_PNAME(CAR(l)), KEYWORD_PNAME(key))==0) {
        if (BOXED_INFO(l) & CONS_CONST) error_const_cell(l);
        CAR(CDR(l)) = val;
        return save;
      }
      l = CDR(CDR(l));
    }
    /* Key was not here. Add it to the end of the list to allow step/step build */
    return STk_dappend2(save, LIST2(key, val)); /* Test on constantness OK */
  } else {
    error_bad_list(l);
    return STk_void; /* never reached */
  }
}

/*
<doc EXT key-delete key-delete!
*  (key-delete  list key)
 * (key-delete! list key)
 *
 * |List| must be a list of keywords and their respective values.
 * |key-delete| remove the |key| and its associated value of the keyword
 * list. The key can be absent of the list.
 * ,(linebreak)
 * |key-delete!| does the
 * same job than |key-delete| by physically modifying its |list| argument.
 * @lisp
 * (key-delete '(:one 1 :two 2) :two)    => (:one 1)
 * (key-delete '(:one 1 :two 2) :three)  => (:one 1 :two 2)
 * @end lisp
doc>
  */
static SCM key_del(SCM l, SCM key)
{
  SCM prev=l, save = l;

  if (!KEYWORDP(key)) error_bad_keyword(key);
  if (CONSP(l) || NULLP(l)) {
    int i, len = STk_int_length(l);

    if (len< 0 || len&1) error_bad_list(save);

    for (i = 0; i < len; i+=2) {
      if (!KEYWORDP(CAR(l))) error_bad_keyword(CAR(l));
      if (strcmp(KEYWORD_PNAME(CAR(l)), KEYWORD_PNAME(key))==0) {
        if (BOXED_INFO(l) & CONS_CONST) error_const_cell(l);
        if (prev == l)
          return CDR(CDR(l));
        else {
          CDR(prev) = CDR(CDR(l));
          return save;
        }
      }
      prev = CDR(l); l = CDR(prev);
    }
    /* Key was not here */
    return save;
  } else {
    error_bad_list(l);
    return STk_void; /* never reached */
  }
}


DEFINE_PRIMITIVE("key-delete!", dkey_delete, subr2, (SCM l, SCM key))
{
  return key_del(l, key);
}

DEFINE_PRIMITIVE("key-delete", key_delete, subr2, (SCM l, SCM key))
{
  return key_del(STk_list_copy(l), key);
}


/*=============================================================================*/

int STk_init_keyword(void)
{
  /* Initialize the keyword hash table */
  STk_hashtable_init(&keyword_table, HASH_OBARRAY_FLAG);

  ADD_PRIMITIVE(make_keyword);
  ADD_PRIMITIVE(keywordp);
  ADD_PRIMITIVE(keyword2string);
  ADD_PRIMITIVE(key_get);
  ADD_PRIMITIVE(key_set);
  ADD_PRIMITIVE(dkey_delete);
  ADD_PRIMITIVE(key_delete);
  return TRUE;
}
