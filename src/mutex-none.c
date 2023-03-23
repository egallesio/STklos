/*
 * mutex-none.c	-- Pthread Mutexes in Scheme
 *
 * Copyright © 2006-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *    Creation date: 16-Apr-2006 11:13 (eg)
 */


#include "stklos.h"
#include "thread-none.h"


int STk_init_mutexes(void)
{
  /* Mutexes primitives */
  FAKE_PRIMITIVE("make-mutex");
  FAKE_PRIMITIVE("mutex?");
  FAKE_PRIMITIVE("mutex-name");
  FAKE_PRIMITIVE("mutex-specific");
  FAKE_PRIMITIVE("mutex-specific-set!");
  FAKE_PRIMITIVE("mutex-state");
  FAKE_PRIMITIVE("%mutex-lock!");
  FAKE_PRIMITIVE("%mutex-unlock!");

  /* Condv primitives */
  FAKE_PRIMITIVE("make-condition-variable");
  FAKE_PRIMITIVE("condition-variable?");
  FAKE_PRIMITIVE("condition-variable-name");
  FAKE_PRIMITIVE("condition-variable-specific");
  FAKE_PRIMITIVE("condition-variable-specific-set!");
  FAKE_PRIMITIVE("condition-variable-signal!");
  FAKE_PRIMITIVE("condition-variable-broadcast!");
  return TRUE;
}
