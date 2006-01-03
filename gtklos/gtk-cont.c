/*
 * gtk-cont.c					-- GTk+ containers
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
 *    Creation date: 19-Aug-2000 11:56 (eg)
 * Last file update:  6-Apr-2002 14:04 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"

/*===========================================================================*\
 * 
 * Utilities
 *
\*===========================================================================*/

static void error_bad_integer(SCM obj, char *opt)
{
  STk_error("~S is a bad integer value for %S option", obj, opt);
}

static Inline long cont_integer_value(SCM obj, char *option)
{
  long val;
  
  val = STk_integer_value(obj);
  if (val==LONG_MIN) error_bad_integer(obj, option);
  return val;
}


/*===========================================================================*\
 * 
 * 			General Containers Functions
 *
\*===========================================================================*/

static void cont_children_helper(gpointer p, gpointer data)
{
  SCM *l     = (SCM*) data;
  SCM widget = (SCM) gtk_object_get_user_data(GTK_OBJECT(p));

  if (widget && widget != STk_void)
    *l = STk_cons(widget, *l);
}


DEFINE_PRIMITIVE("%cont-children", cont_children, subr1, (SCM w))
{
  GList *gl;
  SCM l = STk_nil;

  if (!WIDGETP(w)) STk_error_bad_widget(w);
  
  gl = gtk_container_children(GTK_CONTAINER(WIDGET_ID(w)));
  g_list_foreach(gl, cont_children_helper, &l);
  g_list_free(gl);

  return l;
}

DEFINE_PRIMITIVE("%cont-remove!", cont_remove, subr2, (SCM c, SCM w))
{
  if (!WIDGETP(c)) STk_error_bad_widget(c);
  if (!WIDGETP(w)) STk_error_bad_widget(w);

  gtk_widget_ref(WIDGET_ID(w));
  gtk_container_remove(GTK_CONTAINER(WIDGET_ID(c)), WIDGET_ID(w));
  return STk_void;
}


/*===========================================================================*\
 * 
 * 				  B O X 
 *
\*===========================================================================*/
SIMPLE_WIDGET("%hbox", hbox, (gtk_hbox_new(FALSE, 0)));
SIMPLE_WIDGET("%vbox", vbox, (gtk_vbox_new(FALSE, 0)));


/*===========================================================================*\
 * 
 * 				 P A C K E R
 *
\*===========================================================================*/
// SIMPLE_WIDGET("%packer", packer, (gtk_packer_new()));

/*===========================================================================*\
 * 
 * 				T A B L E
 *
\*===========================================================================*/
SIMPLE_WIDGET("%table", table, (gtk_table_new(2, 2, TRUE)));


/*===========================================================================*\
 * 
 * 				F R A M E
 *
\*===========================================================================*/
SIMPLE_WIDGET("%frame", frame, (gtk_frame_new("")));


/*===========================================================================*\
 * 
 * 				T O P L E V E L
 *
\*===========================================================================*/
SIMPLE_WIDGET("%toplevel", toplevel,  (gtk_window_new(GTK_WINDOW_TOPLEVEL)));
SIMPLE_WIDGET("%transient", transient,(gtk_window_new(GTK_WINDOW_POPUP)));

/*===========================================================================*\
 * 
 * 			S C R O L L E D - W I N D O W
 *
\*===========================================================================*/
SIMPLE_WIDGET("%scrolled-window", scrollwin, (gtk_scrolled_window_new(NULL, NULL)));


/*===========================================================================*\
 * 
 *				P A N E D
 *
\*===========================================================================*/
SIMPLE_WIDGET("%hpaned", hpaned, (gtk_hpaned_new()));
SIMPLE_WIDGET("%vpaned", vpaned, (gtk_vpaned_new()));

/*===========================================================================*\
 * 
 *		 	H A N D L E - W I N D O W
 *
\*===========================================================================*/
SIMPLE_WIDGET("%handle-window", handlewin, (gtk_handle_box_new()));



/* ====================================================================== */
void STk_init_gtk_cont(void)
{
  ADD_PRIMITIVE(hbox);			/* Constructors */
  ADD_PRIMITIVE(vbox);
  //  ADD_PRIMITIVE(packer);
  ADD_PRIMITIVE(table);
  ADD_PRIMITIVE(frame);

  ADD_PRIMITIVE(toplevel);
  ADD_PRIMITIVE(transient);
  ADD_PRIMITIVE(scrollwin);
  ADD_PRIMITIVE(hpaned);
  ADD_PRIMITIVE(vpaned);
  ADD_PRIMITIVE(handlewin);

  ADD_PRIMITIVE(cont_children);
  ADD_PRIMITIVE(cont_remove);
}
