/*
 *
 * h a s h  . c			-- Hash Tables (mostly SRFI-69)
 *
 * Copyright © 1994-2011 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 *
 +=============================================================================
 ! This code is a rewriting of the file tclHash.c of the Tcl
 ! distribution.  Current writing is more modular, independent of Tcl
 ! and (I hope) simpler to use. Multi-word keys which exists in the
 ! Tcl implementation have been droped here. Furthermore, current
 ! implementation of the API also takes into account the fact that we
 ! have a GC approach. In fact, the code is very different, the only thing
 ! which is kept is the Tcl strategy for sizing and hashing
 ! The original code from which this file is derived was copyrighted
 ! as follow:
 !
 ! Copyright (c) 1991-1993 The Regents of the University of California.
 ! Copyright (c) 1994 Sun Microsystems, Inc.
 +=============================================================================
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 17-Jan-1994 17:49
 * Last file update: 27-Jul-2011 22:48 (eg)
 */

#include "stklos.h"
#include "hash.h"


/*
 * The following macro takes a preliminary integer hash value and
 * produces an index into a hash tables bucket list.  The idea is
 * to make it so that preliminary values that are arbitrarily similar
 * will end up in different buckets.  The hash function was taken
 * from a random-number generator.
 */

#define RANDOM_INDEX(ht, i) \
    (((((long) (i))*1103515245) >> HASH_SHIFT(ht)) & HASH_MASK(ht))

#define HASH_WORD(h1, h2)  ((((h1) << 4) + (h1)) ^ (h2))  /* Good repartition ? */



/*===========================================================================*\
 *
 *  		       H a s h i n g - f u n c t i o n s
 *
\*===========================================================================*/

static unsigned long hash_string(register char *string)
{
  register unsigned long result = 0;
  register int c;
  /*
   * I tried a zillion different hash functions and asked many other
   * people for advice.  Many people had their own favorite functions,
   * all different, but no-one had much idea why they were good ones.
   * I chose the one below (multiply by 9 and add new character)
   * because of the following reasons:
   *
   * 1. Multiplying by 10 is perfect for keys that are decimal strings,
   *    and multiplying by 9 is just about as good.
   * 2. Times-9 is (shift-left-3) plus (old).  This means that each
   *    character's bits hang around in the low-order bits of the
   *    hash value for ever, plus they spread fairly rapidly up to
   *    the high-order bits to fill out the hash value.  This seems
   *    works well both for decimal and non-decimal strings.
   */

  for (c = *string; c ; c = *string++) {
    result += (result<<3) + c;
  }
  return result;
}


static unsigned long hash_scheme_string(SCM str)
{					     /* The same one for Scheme strings */
  char *string;
  unsigned long result = 0;
  int i, l;

  if (!STRINGP(str)) STk_error("bad string key ~S", str);

  string = STRING_CHARS(str);
  for (i=0, l=STRING_SIZE(str); i < l ; i++) {
    result += (result<<3) + string[i];
  }
  return result;
}


/*
 * sxhash calculates a "universal" hash value à la CL sxhash  function
 *
 */
static unsigned long sxhash(SCM obj)
{
  unsigned long h;
  SCM tmp;
  int i;

  if (!BOXED_OBJP(obj)) return (AS_LONG(obj) >> 2);

  switch (BOXED_TYPE(obj)) {
    case tc_cons:       h = sxhash(CAR(obj));
      			for(tmp=CDR(obj); CONSP(tmp); tmp=CDR(tmp))
			  h = HASH_WORD(h, sxhash(CAR(tmp)));
			h = HASH_WORD(h, sxhash(tmp));
			return h;
    case tc_bignum:	return sxhash(STk_number2string(obj, MAKE_INT(16)));
    case tc_real:	return (unsigned long) REAL_VAL(obj);
    case tc_rational:   return HASH_WORD(sxhash(RATIONAL_NUM(obj)),
					 sxhash(RATIONAL_DEN(obj)));
    case tc_complex:    return HASH_WORD(sxhash(COMPLEX_REAL(obj)),
					 sxhash(COMPLEX_IMAG(obj)));
    case tc_symbol:     return hash_string(SYMBOL_PNAME(obj));
    case tc_keyword:	return hash_string(KEYWORD_PNAME(obj));
    case tc_string:	return hash_scheme_string(obj);
    case tc_vector:	h = 0;
			for (i=VECTOR_SIZE(obj)-1; i >= 0; i--)
			  h = HASH_WORD(h, sxhash(VECTOR_DATA(obj)[i]));
			return h;
    default:	        /* A complex type (STklos object, user defined type,
			 * hashtable...). In this case we return the type of the
			 * object. This is very  inneficient but it should be
			 * rare to use a  structured object as a key.
			 */
      			 return (unsigned long) BOXED_TYPE(obj);
  }
}

/*===========================================================================*\
 *
 * 				H a s h    S t a t s
 *
\*===========================================================================*/
#define _MAX_COUNT 5

static void hash_stats(struct hash_table_obj *h, SCM port)
{
  int i, j, more, len, count[_MAX_COUNT] = {0};

  STk_fprintf(port, "Hash table statistics\n");
  more = 0;
  for (i = 0; i < HASH_NBUCKETS(h); i++) {
    len = STk_int_length(HASH_BUCKETS(h)[i]);

    STk_fprintf(port, "%d: ", i);
    for (j = 0; j < len; j++) STk_putc('#', port);
    STk_putc('\n', port);

    if (len < _MAX_COUNT)
      count[len] += 1;
    else
      more += 1;
  }

  STk_fprintf(port, "  %d entries in table, %d buckets (ratio %g)\n",
	      HASH_NENTRIES(h), HASH_NBUCKETS(h),
	      (double) HASH_NENTRIES(h)/ HASH_NBUCKETS(h));
  STk_fprintf(port, "Repartition\n");

  for (i = 0; i < _MAX_COUNT; i++) {
    if (count[i])
      STk_fprintf(port, "  %d buckets with %d entries\n", count[i], i);
  }
  if (more)
    STk_fprintf(port, "  %d buckets with more than %d entries\n", more, _MAX_COUNT);
}


/*===========================================================================*\
 *
 * 				E n l a r g e   t a b l e
 *
\*===========================================================================*/

/* called when a table seems to be filled */
static void enlarge_table(register struct hash_table_obj *h)
{
  int old_size, i, index = 0;
  SCM *old_buckets;
  register SCM  tmp;

  old_size    = HASH_NBUCKETS(h);
  old_buckets = HASH_BUCKETS(h);


  /* Set up hashing constants for new array size. */
  HASH_NBUCKETS(h) *= 4;
  HASH_NEWSIZE(h)  *= 4;
  HASH_SHIFT(h)	   -= 2;
  HASH_MASK(h)	    = (HASH_MASK(h) << 2) + 3;

  /* Allocate and initialize the new bucket array. */
  HASH_BUCKETS(h)   = (SCM *)
    		STk_must_malloc((size_t) (HASH_NBUCKETS(h) * sizeof(SCM)));
  for (i = 0; i < HASH_NBUCKETS(h); i++) {
    HASH_BUCKETS(h)[i] = STk_nil;
  }

  /*  Rehash all of the existing entries into the new bucket array. */
  for (i = 0; i < old_size; i++) {
    for (tmp = old_buckets[i]; !NULLP(tmp); tmp = CDR(tmp)) {
      switch (BOXED_INFO(h)) {
	case HASH_OBARRAY_FLAG:
	  index = hash_string(SYMBOL_PNAME(CAR(tmp))) & HASH_MASK(h);
	  break;
	case HASH_VAR_FLAG:
	  index = hash_string(SYMBOL_PNAME(CAR(CAR(tmp)))) & HASH_MASK(h);
	  break;
	case HASH_SCM_FLAG: {
	  SCM key = CAR(CAR(tmp));
	  switch (HASH_TYPE(h)) {
	    case hash_eqp:
	      index = RANDOM_INDEX(h, key);
	      break;
	    case hash_stringp:
	      index = hash_scheme_string(key) & HASH_MASK(h);
	      break;
	    case hash_general:
	      index = RANDOM_INDEX(h, STk_integer_value(STk_C_apply(HASH_HASH(h),
								    1,
								    key)));
	      break;
	    default: ;
	  }
	}
	break;
      }
      /* Place the old value at new index */
      HASH_BUCKETS(h)[index] = STk_cons(CAR(tmp), HASH_BUCKETS(h)[index]);
    }
  }
  /* Let the GC free the old list */
  return;
}



/*===========================================================================*\
 *
 *  		       H a s h - t a b l e   c r e a t i o n
 *
\*===========================================================================*/

void STk_hashtable_init(struct hash_table_obj *h, int flag)
{
  /* Initialization of hash table. Only the system part is initialized here */
  BOXED_INFO(h)		   = flag;
  BOXED_TYPE(h)		   = tc_hash_table;
  HASH_BUCKETS(h) 	   = HASH_SBUCKETS(h);
  HASH_SBUCKETS(h)[0] 	   = STk_nil;
  HASH_SBUCKETS(h)[1] 	   = STk_nil;
  HASH_SBUCKETS(h)[2] 	   = STk_nil;
  HASH_SBUCKETS(h)[3] 	   = STk_nil;
  HASH_NBUCKETS(h) 	   = SMALL_HASH_TABLE;
  HASH_NENTRIES(h) 	   = 0;
  HASH_NEWSIZE(h) 	   = SMALL_HASH_TABLE * REBUILD_MULTIPLIER;
  HASH_SHIFT(h) 	   = 28;
  HASH_MASK(h) 		   = 3;
}



/*===========================================================================*\
 *
 *  	          O b a r r a y   h a s h t a b l e   f u n t i o n s
 *
 *
 * Keys are symbols or keywords. The value associated to a bucket is a list
 * of the  symbols (or keywords) with the same hash value
 *
\*===========================================================================*/

static Inline SCM hash_get_symbol(struct hash_table_obj *h, char *s, int *index)
{
  register SCM l;

  *index = hash_string(s) & HASH_MASK(h);

  for(l=HASH_BUCKETS(h)[*index]; !NULLP(l); l=CDR(l)) {
    if (strcmp(SYMBOL_PNAME(CAR(l)), s) == 0) return CAR(l);
  }
  return (SCM) NULL;
}


SCM STk_hash_intern_symbol(struct hash_table_obj *h, char *s, SCM (*create) (char*s))
{
  SCM z;
  int index;

  z = hash_get_symbol(h, s, &index);

  if (!z) {
    /* Enter the new symbol in table */
    z = create(s);
    HASH_BUCKETS(h)[index] = STk_cons(z, HASH_BUCKETS(h)[index]);
    HASH_NENTRIES(h) += 1;
    /* If the table has exceeded a decent size, rebuild it */
    if (HASH_NENTRIES(h) >= HASH_NEWSIZE(h)) enlarge_table(h);
  }
  return z;
}


/*===========================================================================*\
 *
 *  	       v a r i a b l e s    h a s h t a b l e   f u n t i o n s
 *
 *
 * Here variable are the variables defined in a module. Keys are symbols.
 * The value associated to a bucket is an A-list (symbol . value) of all the
 * symbols with the same hash value
 *
\*===========================================================================*/

SCM STk_hash_get_variable(struct hash_table_obj *h, SCM v, int *index)
{
  register SCM l;
  char *s = SYMBOL_PNAME(v);

  *index = hash_string(s) & HASH_MASK(h);

  for(l=HASH_BUCKETS(h)[*index]; !NULLP(l); l=CDR(l)) {
    if (strcmp(SYMBOL_PNAME(CAR(CAR(l))), s) == 0) return CAR(l);
  }
  return (SCM) NULL;
}


void STk_hash_set_variable(struct hash_table_obj *h, SCM v, SCM value)
{
  SCM z;
  int index;

  z = STk_hash_get_variable(h, v, &index);

  if (z) {
    /* Variable already exists. Change its value*/
    BOX_VALUE(CDR(z)) = value;
  } else {
    SCM z;

    /* Create a new box for this value */
    z = STk_make_box(value);

    /* Enter the new variable in table */
    HASH_BUCKETS(h)[index] = STk_cons(STk_cons(v, z),
				      HASH_BUCKETS(h)[index]);
    HASH_NENTRIES(h) += 1;
    /* If the table has exceeded a decent size, rebuild it */
    if (HASH_NENTRIES(h) >= HASH_NEWSIZE(h)) enlarge_table(h);
  }
}

void STk_hash_set_alias(struct hash_table_obj *h, SCM v, SCM value)
{
  SCM z;
  int index;

  z = STk_hash_get_variable(h, v, &index);

  if (z) {
    /* Variable already exists. Change its value*/
    CDR(z) = value;
  } else {
    /* Enter the new variable in table */
    HASH_BUCKETS(h)[index] = STk_cons(STk_cons(v, value),
				      HASH_BUCKETS(h)[index]);
    HASH_NENTRIES(h) += 1;
    /* If the table has exceeded a decent size, rebuild it */
    if (HASH_NENTRIES(h) >= HASH_NEWSIZE(h)) enlarge_table(h);
  }
}




/*===========================================================================*\
 *
 * 			Utilities on hash tables
 *
\*===========================================================================*/

/* Return the list of keys of a hash table */
SCM STk_hash_keys(struct hash_table_obj *h)
{
  int i, max;
  SCM tmp, res = STk_nil;

  for (i=0, max= HASH_NBUCKETS(h); i < max; i++) {
    for (tmp = HASH_BUCKETS(h)[i]; !NULLP(tmp); tmp = CDR(tmp)) {
      res = STk_cons(CAR(CAR(tmp)), res);
    }
  }
  return res;
}



/*===========================================================================*\
 *
 * 		H a s h - t a b l e s   i n   t h e
 *
 * 		      S c h e m e   W o r l d
 *
\*===========================================================================*/
static void error_bad_hash_table(SCM obj)
{
  STk_error("bad hash table ~S", obj);
}


static void error_bad_procedure(SCM obj)
{
  STk_error("bad procedure ~S", obj);
}

static void error_not_present(SCM key, SCM ht)
{
  STk_error("entry not defined for key ~S in table ~S", key, ht);
}


DEFINE_PRIMITIVE("%make-hash-table", make_hash, subr2, (SCM compar, SCM hashfct))
{
  SCM z;
  hash_type type = hash_general;

  if (STk_procedurep(compar) == STk_false)
    STk_error("bad comparison function ~S", compar);
  if (STk_procedurep(hashfct) == STk_false)
    STk_error("bad hash function ~S", hashfct);

  if (BOXED_TYPE(compar) == tc_subr2) {
    /* See if comparison function is 'eq?' or 'string?'.
     * If so, we implement the hash table in the most efficient way.
     */
    if (PRIMITIVE_FUNC(compar) == STk_eq)    type = hash_eqp;
    if (PRIMITIVE_FUNC(compar) == STk_streq) type = hash_stringp;
  }

  NEWCELL(z, hash_table);
  STk_hashtable_init((struct hash_table_obj *) z, HASH_SCM_FLAG);

  /* Finish initialization */
  HASH_TYPE(z)   = type;
  HASH_COMPAR(z) = compar;
  HASH_HASH(z)   = hashfct;

  return z;
}


/*
<doc EXT hash-table?
 * (hash-table? obj)
 *
 * Returns |#t| if |obj| is a hash table, returns |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("hash-table?", hashtablep, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(HASHP(obj));
}


/*
<doc EXT hash-table-size
 * (hash-table-size hash)
 *
 * Returns the number of entries in the |hash|.
doc>
*/
DEFINE_PRIMITIVE("hash-table-size", hash_table_size, subr1, (SCM ht))
{
  if(!HASHP(ht)) error_bad_hash_table(ht);
  return MAKE_INT(HASH_NENTRIES(ht));
}

/*
<doc EXT hash-table-equivalence-function
 * (hash-table-equivalence-function hash)
 *
 * Returns the equivalence predicate used for keys in |hash|.
doc>
*/
DEFINE_PRIMITIVE("hash-table-equivalence-function", hash_table_eqv_func,
		 subr1, (SCM ht))
{
  if(!HASHP(ht)) error_bad_hash_table(ht);
  return HASH_COMPAR(ht);
}


/*
<doc EXT hash-table-hash-function
 * (hash-table-hash-function hash)
 *
 * Returns the hash function used for keys in |hash|.
doc>
*/
DEFINE_PRIMITIVE("hash-table-hash-function", hash_table_hash_func,
		 subr1, (SCM ht))
{
  if(!HASHP(ht)) error_bad_hash_table(ht);
  return HASH_HASH(ht);
}



/*
<doc EXT hash-table-set!
 * (hash-table-set! hash key value)
 *
 * Enters an association between |key| and |value| in the|hash| table.
 * The value returned by |hash-table-set!| is ,(emph "void").
doc>
*/
DEFINE_PRIMITIVE("hash-table-set!", hash_set, subr3, (SCM ht, SCM key, SCM val))
{
  int index = 0;
  SCM func, l = STk_nil;

  if (!HASHP(ht)) error_bad_hash_table(ht);

  switch (HASH_TYPE(ht)) {
    case hash_eqp:
      index = RANDOM_INDEX(ht, key);
      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); l = CDR(l))
	if (CAR(CAR(l)) == key) break;
      break;

    case hash_stringp:
      index = hash_scheme_string(key) & HASH_MASK(ht);

      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); l = CDR(l))
	if (STk_streq(CAR(CAR(l)), key) != STk_false) break;
      break;
    case hash_general:
      func  = HASH_COMPAR(ht);
      index = RANDOM_INDEX(ht,
			   STk_integer_value(STk_C_apply(HASH_HASH(ht), 1, key)));

      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); l = CDR(l))
	if (STk_C_apply(func, 2, CAR(CAR(l)), key) != STk_false) break;
    default:  break;
  }

  /* When we are here, index is on the good bucket list. if l is null,
   * the item was not in the given list otherwise CAR(l) = (key . old-value)
   */
  if (NULLP(l)) {
    HASH_BUCKETS(ht)[index] = STk_cons(STk_cons(key, val), HASH_BUCKETS(ht)[index]);
    HASH_NENTRIES(ht) += 1;
    /* If the table has exceeded a decent size, rebuild it */
    if (HASH_NENTRIES(ht) >= HASH_NEWSIZE(ht))
      enlarge_table((struct hash_table_obj *) ht);
  }
  else
    CDR(CAR(l)) = val;

  return STk_void;
}


/*
<doc EXT hash-table-ref
 * (hash-table-ref hash key)
 * (hash-table-ref hash key thunk)
 *
 * Returns the value associated with |key| in the given |hash| table. If no
 * value has been associated with |key| in |hash|, the specified |thunk| is
 * called and its value is returned; otherwise an error is raised.
 * @lisp
 * (define h1 (make-hash-table))
 * (hash-table-set! h1 'foo (list 1 2 3))
 * (hash-table-ref  h1 'foo)                 =>  (1 2 3)
 * (hash-table-ref  h1 'bar
                       (lambda () 'absent))  =>  absent
 * (hash-table-ref  h1 'bar)                 =>  error
 * (hash-table-set! h1 '(a b c) 'present)
 * (hash-table-ref  h1 '(a b c)
                        (lambda () 'absent)) => absent
 *
 * (define h2 (make-hash-table equal?))
 * (hash-table-set! h2 '(a b c) 'present)
 * (hash-table-ref  h2 '(a b c))             => present
 * @end lisp
doc>
*/
static SCM Inline hash_table_search(SCM ht, SCM key)
{
  int index = 0;
  SCM func, l = STk_nil;

  if (!HASHP(ht)) error_bad_hash_table(ht);

  switch (HASH_TYPE(ht)) {
    case hash_eqp:
      index = RANDOM_INDEX(ht, key);
      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); l = CDR(l))
	if (CAR(CAR(l)) == key) return CDR(CAR(l));
      break;

    case hash_stringp:
      index = hash_scheme_string(key) & HASH_MASK(ht);

      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); l = CDR(l))
	if (STk_streq(CAR(CAR(l)), key) != STk_false) return CDR(CAR(l));
      break;

    case hash_general:
      func = HASH_COMPAR(ht);
      index = RANDOM_INDEX(ht,
			   STk_integer_value(STk_C_apply(HASH_HASH(ht), 1, key)));

      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); l = CDR(l))
	if (STk_C_apply(func, 2, CAR(CAR(l)), key) != STk_false) return CDR(CAR(l));
      break;
    default: break;
  }

  /* Not found */
  return NULL;
}

DEFINE_PRIMITIVE("hash-table-ref", hash_ref, subr23, (SCM ht, SCM key, SCM def))
{
  SCM res = hash_table_search(ht, key);

  if (res) return res;
  if (def) return STk_C_apply(def, 0);
  error_not_present(key, ht);
  return STk_void; 			/* never reached */
}

/*
<doc EXT hash-table-ref/default
 * (hash-table-ref/default hash key)
 *
 * This function is equivalent to
 * @lisp
 * (hash-table-ref hash key (lambda () default))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("hash-table-ref/default", hash_ref_default, subr3,
		 (SCM ht, SCM key, SCM def))
{
  SCM res = hash_table_search(ht, key);

  if (res) return res;
  if (def) return def;
  error_not_present(key,ht);
  return STk_void; 			/* never reached */
}

/*
<doc EXT hash-table-exists?
 * (hash-table-exists? hash key)
 *
 * Returns |#t| if there is any association of |key| in
 * |hash|. Returns |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("hash-table-exists?", hash_existp, subr2, (SCM ht, SCM key))
{
  return MAKE_BOOLEAN(hash_table_search(ht,key) != NULL);
}


/*
<doc EXT hash-table-delete!
 * (hash-table-delete! hash key)
 *
 * Deletes the entry for |key| in |hash|, if it exists. Result of
 * |hash-table-delete!| is ,(emph "void").
 *
 * @lisp
 * (define h (make-hash-table))
 * (hash-table-set! h 'foo (list 1 2 3))
 * (hash-table-ref h 'foo)                => (1 2 3)
 * (hash-table-delete! h 'foo)
 * (hash-table-ref h 'foo
                     (lambda () 'absent)  => absent
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("hash-table-delete!", hash_delete, subr2, (SCM ht, SCM key))
{
  int index = 0;
  SCM func, l, prev;

  if (!HASHP(ht)) error_bad_hash_table(ht);

  l = prev = STk_nil;

  switch (HASH_TYPE(ht)) {
    case hash_eqp:
      index = RANDOM_INDEX(ht, key);
      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); prev=l, l=CDR(l))
	if (CAR(CAR(l)) == key) break;
      break;

    case hash_stringp:
      index = hash_scheme_string(key) & HASH_MASK(ht);

      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l);  prev=l, l=CDR(l))
	if (STk_streq(CAR(CAR(l)), key) != STk_false) break;
      break;

    case hash_general:
      func = HASH_COMPAR(ht);
      index = RANDOM_INDEX(ht,
			   STk_integer_value(STk_C_apply(HASH_HASH(ht), 1, key)));

      for(l = HASH_BUCKETS(ht)[index]; !NULLP(l); prev=l, l=CDR(l))
	if (STk_C_apply(func, 2, CAR(CAR(l)), key) != STk_false) break;
      break;
    default: break;
  }

  if (!NULLP(l)) {     			/* found item */
    HASH_NENTRIES(ht) -= 1;
    if (NULLP(prev))
      HASH_BUCKETS(ht)[index] = CDR(l); /* somewhere in the chain */
    else
      CDR(prev) = CDR(l);		/* delete the first item of the chain */
  }
  return STk_void;
}



/*
<doc EXT hash-table-walk hash-table-for-each
 * (hash-table-for-each hash proc)
 * (hash-table-walk hash proc)
 *
 * |Proc| must be a procedure taking two arguments. |Hash-table-for-each|
 * calls |proc| on each key/value association in |hash|, with the key as
 * the first argument and the value as the second.  The value returned by
 * |hash-table-for-each| is ,(emph "void").
 *  ,(linebreak)
 * ,(bold "Note:") The order of application of |proc| is unspecified.
 *  ,(linebreak)
 * ,(bold "Note:") |hash-table-walk| is another name for |hash-table-for-each|
 * (this is the name used in ,(link-srfi 69)).
 *
 * @lisp
 * (let ((h   (make-hash-table))
 *       (sum 0))
 *   (hash-table-set! h 'foo 2)
 *   (hash-table-set! h 'bar 3)
 *   (hash-table-for-each h (lambda (key value)
 *                            (set! sum (+ sum value))))
 *   sum)           =>  5
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("hash-table-for-each", hash_for_each, subr2, (SCM ht, SCM proc))
{
  int i, n;
  SCM l = STk_nil;;

  if (!HASHP(ht)) 			 error_bad_hash_table(ht);
  if (STk_procedurep(proc) == STk_false) error_bad_procedure(proc);

  n = HASH_NBUCKETS(ht);
  for (i = 0; i < n; i++)
    for (l = HASH_BUCKETS(ht)[i]; !NULLP(l); l = CDR(l))
      STk_C_apply(proc, 2, CAR(CAR(l)), CDR(CAR(l)));
  return STk_void;
}


/*
<doc EXT hash-table-map
 * (hash-table-map hash proc)
 *
 * |Proc| must be a procedure taking two arguments. |Hash-table-map|
 * calls |proc| on each key/value association in |hash|, with the key as
 * the first argument and the value as the second.  The result of
 * |hash-table-map| is a list of the values returned by |proc|, in an
 * unspecified order.
 * ,(linebreak)
 * ,(bold "Note:") The order of application of |proc| is unspecified.
 * @lisp
 * (let ((h (make-hash-table)))
 *   (dotimes (i 5)
 *     (hash-table-set! h i (number->string i)))
 *   (hash-table-map h (lambda (key value)
 *                        (cons key value))))
 *              => ((3 . "3") (4 . "4") (0 . "0") (1 . "1") (2 . "2"))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("hash-table-map", hash_map, subr2, (SCM ht, SCM proc))
{
  int i, n;
  SCM res, l;

  if (!HASHP(ht)) 			 error_bad_hash_table(ht);
  if (STk_procedurep(proc) == STk_false) error_bad_procedure(proc);

  n = HASH_NBUCKETS(ht);
  l = res = STk_nil;

  for (i = 0; i < n; i++)
    for (l = HASH_BUCKETS(ht)[i]; !NULLP(l); l = CDR(l))
      res = STk_cons(STk_C_apply(proc, 2, CAR(CAR(l)), CDR(CAR(l))),
		     res);
  return res;
}

/*
<doc EXT hash-table-hash
 * (hash-table-hash obj)
 *
 * Computes a hash code for an object and returns this hash code as a
 * non negative integer. A property of |hash-table-hash| is that
 * @lisp
 * (equal? x y) => (equal? (hash-table-hash x) (hash-table-hash y)
 * @end lisp
 *
 * as the the Common Lisp |sxhash| function from which this procedure is
 * modeled.
doc>
*/
DEFINE_PRIMITIVE("hash-table-hash", hash_hash, subr1, (SCM obj))
{
  long int x = sxhash(obj);
  return STk_long2integer((x < 0) ? -x : x);
}


/*
<doc EXT hash-table-stats
 * (hash-table-stats hash)
 * (hash-table-stats hash port)
 *
 * Prints  overall information about |hash|, such as the number of entries
 * it contains, the number of buckets in its hash array, and the utilization
 * of the buckets. Informations are printed on |port|. If no |port| is given
 * to |hash-table-stats|, information are printed on the current output port
 * (see ,(ref :mark "current-output-port")).
doc>
*/
DEFINE_PRIMITIVE("hash-table-stats", hash_stats, subr12, (SCM ht, SCM port))
{
  if(!HASHP(ht)) error_bad_hash_table(ht);
  if (!port) port = STk_current_output_port();
  else if (!OPORTP(port)) STk_error("bad port ~S", port);

  hash_stats((struct hash_table_obj *) ht, port);
  return STk_void;
}


/*===========================================================================*\
 *
 * 	Initialization code
 *
\*===========================================================================*/

static struct extended_type_descr xtype_hash = { "hash-table", NULL };

int STk_init_hash(void)
{
  /* Define type for hash-tables */
  DEFINE_XTYPE(hash_table, &xtype_hash);

  /* Define primitives */
  ADD_PRIMITIVE(make_hash);
  ADD_PRIMITIVE(hashtablep);
  ADD_PRIMITIVE(hash_table_size);
  ADD_PRIMITIVE(hash_table_eqv_func);
  ADD_PRIMITIVE(hash_table_hash_func);

  ADD_PRIMITIVE(hash_set);
  ADD_PRIMITIVE(hash_ref);
  ADD_PRIMITIVE(hash_ref_default);
  ADD_PRIMITIVE(hash_existp);

  ADD_PRIMITIVE(hash_delete);

  ADD_PRIMITIVE(hash_for_each);
  ADD_PRIMITIVE(hash_map);

  ADD_PRIMITIVE(hash_hash);
  ADD_PRIMITIVE(hash_stats);

  return TRUE;
}
