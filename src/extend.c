/*
 *
 * e x t e n d . c			-- All the stuff dealing with
 *					   extended types
 *
 * Copyright © 1995-2006 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 15-Mar-1995 11:31
 * Last file update:  6-Aug-2006 22:09 (eg)
 */

#include "stklos.h"

/*
 * The array of extended type descriptors
 */
struct extended_type_descr *STk_xtypes[MAX_CELL_TYPES] = {NULL};

static int user_extended_type = tc_last_standard;


int STk_new_user_type(struct extended_type_descr *descr)
{
  MUT_DECL(lck);

  MUT_LOCK(lck);
  user_extended_type += 1;
  STk_xtypes[user_extended_type]= descr;
  MUT_UNLOCK(lck);

  return user_extended_type;
}


int STk_init_extend(void)
{
  return TRUE;
}
