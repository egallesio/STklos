/*
 *
 * h a s h  . h                 -- Hash Tables
 *
 * Copyright © 1994-2025 Erick Gallesio <eg@stklos.net>
 *
 +=============================================================================
 ! This code is a rewriting of the file tclHash.c of the Tcl
 ! distribution.  Current writing is more modular, independent of Tcl
 ! and (I hope) simpler to use. Multi-word keys which exists in the
 ! Tcl implementation have been droped here. Furthermore, current
 ! implementation of the API also takes into account the fact that we
 ! have a GC approach. The original code from which this file is
 ! derived was copyrighted as follow:
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
 */


#define SMALL_HASH_TABLE   4
#define REBUILD_MULTIPLIER 3    /* When there are this many entries per bucket, */
                                /* on average,  make the table larger           */

#define HASH_OBARRAY_FLAG   (1<<0)   /* Only for the symbol table        */
#define HASH_VAR_FLAG       (1<<1)   /* For modules (keys are symbols)   */
#define HASH_SCM_FLAG       (1<<2)   /* For Scheme hash tables           */
#define HASH_C_FLAG         (1<<3)   /* For C hash tables (used by read) */
#define HASH_CONST          (1<<4)   /* Constant hash table              */

typedef enum {hash_system, hash_eqp, hash_stringp, hash_general} hash_type;

struct hash_table_obj {
  stk_header header;
  SCM *buckets;
  SCM static_buckets[SMALL_HASH_TABLE];
  int num_buckets;
  int num_entries;
  int rebuild_size;
  int down_shift;
  int mask;
  /* Following slots are used only for "user" hash tables */
  hash_type type;
  SCM comparison;
  SCM hash_fct;
};

#define HASHP(o)                (BOXED_TYPE_EQ((o), tc_hash_table))
#define HASH_BUCKETS(h)         (((struct hash_table_obj *) (h))->buckets)
#define HASH_SBUCKETS(h)        (((struct hash_table_obj *) (h))->static_buckets)
#define HASH_NBUCKETS(h)        (((struct hash_table_obj *) (h))->num_buckets)
#define HASH_NENTRIES(h)        (((struct hash_table_obj *) (h))->num_entries)
#define HASH_NEWSIZE(h)         (((struct hash_table_obj *) (h))->rebuild_size)
#define HASH_SHIFT(h)           (((struct hash_table_obj *) (h))->down_shift)
#define HASH_MASK(h)            (((struct hash_table_obj *) (h))->mask)

#define HASH_CONSTP(h)          (BOXED_INFO(h) & HASH_CONST)

#define HASH_TYPE(h)            (((struct hash_table_obj *) (h))->type)
#define HASH_COMPAR(h)          (((struct hash_table_obj *) (h))->comparison)
#define HASH_HASH(h)            (((struct hash_table_obj *) (h))->hash_fct)


void STk_hashtable_init(struct hash_table_obj *h, int flag);



/*
 * Function for accessing symbol/keyword hash table. This function is of
 * little interest except for the obarrays. Don't use it but the
 * higher level interface instead.
 */
SCM STk_hash_intern_symbol(struct hash_table_obj *h, const char *s,
                           SCM (*create)(const char *s));

/*
 * Function for accessing module hash table. Don't use them but the
 * higher level interface instead.
 */
void STk_hash_define_variable(struct hash_table_obj *h, SCM v, SCM value);
SCM STk_hash_get_variable(struct hash_table_obj *h, SCM v);
void STk_hash_set_alias(struct hash_table_obj *h, SCM v, SCM value);

/*
 * Utilities on hash tables
 */
SCM STk_hash_keys(struct hash_table_obj *h);
SCM STk_make_basic_hash_table(void);

/*
 * Scheme interface
 */
EXTERN_PRIMITIVE("hash-table-ref/default", hash_ref_default, subr3,
                 (SCM ht, SCM key, SCM def));
EXTERN_PRIMITIVE("hash-table-set!", hash_set, subr3, (SCM ht, SCM key, SCM val));


/*
 * C hash tables (for internal use: keys are C strings and values are void*
 *
 * NOTE: Use only the functions defined below for these hash tables. Standard
 * hash table functions cannot deal with this sort of hash tables.
 */
SCM STk_make_C_hash_table(void);
void* STk_C_hash_get(struct hash_table_obj *ht, const char *key);
void STk_C_hash_set(struct hash_table_obj *ht, const char *key, void *val);
void STk_C_hash_delete(struct hash_table_obj *ht, const char *key);

int STk_init_hash(void);
