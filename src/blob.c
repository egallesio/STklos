/*
 * blob.c	-- Implementation of R7RS blobs
 *
 * Copyright © 2011 Erick Gallesio - Polytech'Nice-Sophia <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 18-Apr-2011 23:13 (eg)
 * Last file update: 20-Apr-2011 10:55 (eg)
 */

#include "stklos.h"
#include <stdint.h>

struct blob_obj {
  stk_header header;
  int len;
  uint8_t data[1];  /* will be sized to a different value when allocated */
};

#define BLOBP(o)		(BOXED_TYPE_EQ((o), tc_blob))
#define BLOB_LEN(p)		(((struct blob_obj *) (p))->len)
#define BLOB_DATA(p)		(((struct blob_obj *) (p))->data)


static void error_bad_blob(SCM obj)
{
  STk_error("bad blob ~S", obj);
}

static void error_bad_index(SCM blob, SCM index)
{
  STk_error("index ~S out of bounds for ~S", index, blob);
}

static void error_bad_value(SCM obj)
{
  STk_error("bad blob value ~S (mus be between 0 and %d)", obj, UINT8_MAX);
}


static void print_blob(SCM obj, SCM port, int mode)
{
  STk_fprintf(port, "#[blob len=%d @ %lx]", BLOB_LEN(obj), (unsigned long) obj);
}


static SCM make_blob(int len)
{
  SCM z;

  NEWCELL_ATOMIC(z, blob, sizeof(struct blob_obj) + len);
  BLOB_LEN(z) = len;
  memset(BLOB_DATA(z), (uint8_t) 0, len); /* rather than unspecified */
  return z;
}

/* ======================================================================*/
/*
<doc R7RS make-blob
 * (make-blob)
 *
 * Returns a newly allocated blob of k bytes. The initial
 * contents of each element is 0.
doc>
*/
DEFINE_PRIMITIVE("make-blob", make_blob, subr1, (SCM value))
{
  long int len = STk_integer_value(value);

  if (len < 0) STk_error("bad blob length ~S", value);

  return make_blob(len);
}

/*
<doc R7RS blob?
 * (blob? obj)
 *
 * Returns |#t| if |obj| is a blob and returns |#f| otherwise.
doc>
*/
DEFINE_PRIMITIVE("blob?", blobp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(BLOBP(obj));
}

/*
<doc R7RS blob-length
 * (blob-length blob)
 *
 * Returns the length of |blob| in bytes as an exact integer.
doc>
*/
DEFINE_PRIMITIVE("blob-length", blob_length, subr1, (SCM blob))
{
  if (!BLOBP(blob)) error_bad_blob(blob);

  return MAKE_INT(BLOB_LEN(blob));
}


/*
<doc R7RS blob-u8-ref
 * (blob-u8-ref blob k)
 *
 * Returns the byte at index |k| of |blob| as an exact integer in the
 * range [0..255].
doc>
*/
DEFINE_PRIMITIVE("blob-u8-ref", blob_u8_ref, subr2, (SCM blob, SCM index))
{
  long int ind = STk_integer_value(index);

  if (!BLOBP(blob)) error_bad_blob(blob);
  if (ind < 0 || ind >= BLOB_LEN(blob)) error_bad_index(blob, index);

  return MAKE_INT(BLOB_DATA(blob)[ind]);
}


/*
<doc R7RS blob-u8-set!
 * (blob-u8-ref blob k u8)
 *
 * Stores |u8| in  the byte at index |k| of |blob|. |u8| must be an exact integer
 * in the range [0..255].  The value returned by |blob-u8-set!| is ,(emph "void").
doc>
*/
DEFINE_PRIMITIVE("blob-u8-set!", blob_u8_set, subr3, (SCM blob, SCM index, SCM value))
{
  long int ind = STk_integer_value(index);
  long int val = STk_integer_value(value);


  if (!BLOBP(blob)) error_bad_blob(blob);
  if (ind < 0 || ind >= BLOB_LEN(blob)) error_bad_index(blob, index);
  if (val < 0 || val > UINT8_MAX) error_bad_value(value);

  BLOB_DATA(blob)[ind] = val;
  return STk_void;
}

/*
<doc R7RS blob-copy
 * (blob-copy blob)
 *
 * Returns a newly allocated blob containing the same bytes as |blob|.
doc>
*/
DEFINE_PRIMITIVE("blob-copy", blob_copy, subr1, (SCM blob))
{
  SCM z;

  if (!BLOBP(blob)) error_bad_blob(blob);
  z = make_blob(BLOB_LEN(blob));
  memcpy(BLOB_DATA(z), BLOB_DATA(blob), BLOB_LEN(blob));
  return z;
}


/*
<doc R7RS blob-copy!
 * (blob-copy! from to)
 *
 * Copy the bytes of blob |from- to blob |to|, which must not be shorter.
 * The value returned by |blob-copy!| is ,(emph "void").
doc>
*/
DEFINE_PRIMITIVE("blob-copy!", dblob_copy, subr2, (SCM from, SCM to))
{
  if (!BLOBP(from)) error_bad_blob(from);
  if (!BLOBP(to))   error_bad_blob(to);

  if (BLOB_LEN(to) < BLOB_LEN(from))
    STk_error("blob ~S is too long for copying it in ~S", from, to);

  memcpy(BLOB_DATA(to), BLOB_DATA(from), BLOB_LEN(from));
  return STk_void;
}

/*
<doc R7RS partial-blob
 * (partal-blob blob start end)
 *
 * Returns a newly allocated blob containing the bytes in |blob|
 * between |start| (inclusive) and |end| (exclusive).
doc>
*/
DEFINE_PRIMITIVE("partial-blob", partial_blob, subr3, (SCM blob, SCM start, SCM end))
{
  long int from = STk_integer_value(start);
  long int to   = STk_integer_value(end);
  SCM z;

  z = make_blob(to-from);
  memcpy(BLOB_DATA(z), BLOB_DATA(blob)+from, to-from);
  return z;
}


/*
<doc R7RS partial-blob-copy!
 * (partial-blob-copy! from start end to at)
 *
 * Copy the bytes of |blob| from between |start| and |end| to blob
 * |to|, starting at |at|. The order in which bytes are copied
 * is unspecified, except that if the source and destination
 * overlap, copying takes place as if the source is first copied
 * into a temporary blob and then into the destination.
 * The value returned by |partial-blob-copy!| is ,(emph "void").
doc>
*/
DEFINE_PRIMITIVE("partial-blob-copy!", partial_blob_copy, subr5,
		 (SCM from, SCM start, SCM end, SCM to, SCM at))
{
  long int istart = STk_integer_value(start);
  long int iend   = STk_integer_value(end);
  long int iat    = STk_integer_value(at);
  int len;

  if (!BLOBP(from)) error_bad_blob(from);
  if (!BLOBP(to))   error_bad_blob(to);

  len = iend-istart;
  if (iat + len > BLOB_LEN(to))
    STk_error("cannot copy %d bytes at index ~S of ~S", len, at, to);

  memmove(BLOB_DATA(to)+iat, BLOB_DATA(from)+ istart, len);

  return STk_void;
}

/*
<doc EXT blob->u8-list u8-list->blob
 * (blob->u8-list blob)
 * (u8-list->blob list)
 *
 * |blob->u8-list| returns a newly allocated list of the integers contained in
 * the elements of |blob|. |u8-list->blob| returns a newly created blob
 * initialized to the elements of the list |list|.
doc>
*/
DEFINE_PRIMITIVE("blob->u8-list", blob2u8list, subr1, (SCM blob))
{
  SCM l = STk_nil;
  uint8_t *start, *end;

  if (!BLOBP(blob)) error_bad_blob(blob);

  start = BLOB_DATA(blob);
  for (end = BLOB_DATA(blob) + BLOB_LEN(blob) - 1; end >= start; end--) {
    l = STk_cons(MAKE_INT(*end), l);
  }
  return l;
}


DEFINE_PRIMITIVE("u8-list->blob", u8list2blob, subr1, (SCM lst))
{
  SCM z;
  int i, len = STk_int_length(lst);

  if (len < 0) STk_error("bad list ~S", lst);

  z = make_blob(len);
  for (i = 0; i < len; i++) {
    long int val = STk_integer_value(CAR(lst));

    if (val < 0 || val > UINT8_MAX) error_bad_value(CAR(lst));
    BLOB_DATA(z)[i] = val;
    lst = CDR(lst);
  }
  return z;
}


/*===========================================================================*\
 *
 * 	Initialization code
 *
\*===========================================================================*/

static struct extended_type_descr xtype_blob = {"blob", print_blob};

int STk_init_blob(void)
{
  /* Define type for blobs */
  DEFINE_XTYPE(blob, &xtype_blob);

  /* Define primitives */
  ADD_PRIMITIVE(make_blob);
  ADD_PRIMITIVE(blobp);
  ADD_PRIMITIVE(blob_length);
  ADD_PRIMITIVE(blob_u8_ref);
  ADD_PRIMITIVE(blob_u8_set);
  ADD_PRIMITIVE(blob_copy);
  ADD_PRIMITIVE(dblob_copy);
  ADD_PRIMITIVE(partial_blob);
  ADD_PRIMITIVE(partial_blob_copy);
  ADD_PRIMITIVE(blob2u8list);
  ADD_PRIMITIVE(u8list2blob);
  return TRUE;
}
