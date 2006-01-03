/*
 * gtk-label.c	-- GTK+ labels
 * 
 * Copyright © 2000-2002 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 19-Aug-2000 12:07 (eg)
 * Last file update:  8-Mar-2002 14:03 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"


SIMPLE_WIDGET("%label",     label,  (gtk_label_new("")));
SIMPLE_WIDGET("%button",    button, (gtk_button_new()));
SIMPLE_WIDGET("%togglebut", toggle, (gtk_toggle_button_new()));
SIMPLE_WIDGET("%checkbut",  check,  (gtk_check_button_new()));

DEFINE_PRIMITIVE("%radiobut", radio, subr2, (SCM obj, SCM sibling))
{
  GtkWidget *w;
  
  if (sibling == STk_false) {
    /* First radiobutton of a group */
    w = gtk_radio_button_new_with_label(NULL, "");
  } else {
    /* Create a new radio button in the group of given sibling */
    if (!WIDGETP(sibling) || !GTK_IS_RADIO_BUTTON(WIDGET_ID(sibling)))
      STk_error("the given sibling (~S) is not a radiobutton", sibling);
    w = gtk_radio_button_new_with_label_from_widget(
      		GTK_RADIO_BUTTON(WIDGET_ID(sibling)), "");
  }
  return WIDGET_TO_SCM(w, obj);
}


void STk_init_gtk_label(void)
{
  ADD_PRIMITIVE(label);
  ADD_PRIMITIVE(button);
  ADD_PRIMITIVE(toggle);
  ADD_PRIMITIVE(check);
  ADD_PRIMITIVE(radio);
}

