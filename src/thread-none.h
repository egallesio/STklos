/*
 * nothread.h	-- Thread support for STklos
 *
 * Copyright © 2006 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *    Creation date:  4-Feb-2006 11:03 (eg)
 */
#ifndef _STK_THREAD_NONE_H
#define _STK_THREAD_NONE_H

extern  struct primitive_obj STk_o_threadno; 	/* A pseudo primitive which 	*/
						/* always fails 		*/

#define FAKE_PRIMITIVE(name) \
  STk_define_variable(STk_intern(name), &STk_o_threadno, STk_STklos_module)


#endif /* ! _STK_THREAD_NONE_H */
