/*
 * gtk-signal.c		-- GTK+ signal & callback management
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
 *    Creation date: 11-Oct-2000 23:42 (eg)
 * Last file update: 12-Feb-2002 16:55 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"

/*===========================================================================*\
 * 
 *		 	       %gtk-set-callback!
 *
 * GTK callbacks (at least the one uses in STklos)  are of two types: 
 *      - the ones with an associated event (they use the C call_handler3 
 *	function with the closure as data)
 * 	- the ones without an associated event (they use the C call_handler2 
 *	function with the closure as data)
 * 
 * In both cases, these closures takes one argument which is an event descriptor 
 * made from the object which wraps the widget and a GDK event (NULL if no 
 * event associated -- i.e. in call_handler2).
 *
 * gtk-set-callback! takes five parameters:
 *   - the widget on which the call back is set
 *   - the closure associated to the callback
 *   - the signal name (a string which correspond to a GTK arg)
 *   - a boolean "has_eventp" which indicates if the callback has an associated
 *     event or not (if it has one, the callback will be the call_handler3 C
 *     function otherwise, it will be the call_handler2 function)
 *   - "msk" contains the bits that must be added to the widget
 *     to take into account the given event.
 *
\*===========================================================================*/

static void call_handler2(GtkObject* w, gpointer data)
{
  SCM widget = (SCM) gtk_object_get_user_data(GTK_OBJECT(w));
  STk_C_apply((SCM) data, 1, STk_make_event(widget, NULL));
}

static void call_handler3(GtkObject* w, GdkEvent *event, gpointer data)
{
  SCM widget = (SCM) gtk_object_get_user_data(GTK_OBJECT(w));
  STk_C_apply((SCM) data, 1, STk_make_event(widget, event));
}


DEFINE_PRIMITIVE("%gtk-set-callback!", gtk_set_callback, subr5, 
		 (SCM widget, SCM closure, SCM signal, SCM has_eventp, SCM msk))
{
  int mask_value = STk_integer_value(msk);
  GtkSignalFunc handler;
  GtkWidget *w;

  if (!WIDGETP(widget))			  STk_error_bad_widget(widget);
  if (!STRINGP(signal))			  STk_error("bad signal name ~S", signal);
  if (STk_procedurep(closure)==STk_false) STk_error("bad procedure ~S", closure);

  handler = (has_eventp!=STk_false) ? call_handler3: (GtkSignalFunc)call_handler2;
  
  /* Do the GTK connection */
  gtk_signal_connect(GTK_OBJECT(WIDGET_ID(widget)), 
		     STRING_CHARS(signal), 
		      handler, 
		     (gpointer) closure);

  /* Eventually make the widget sensitive to this event */
  w = WIDGET_ID(widget);
  if (GTK_IS_WIDGET(w) && !GTK_WIDGET_NO_WINDOW(w)) 
    gtk_widget_add_events(w, mask_value);
  
  return STk_void;
}

/*===========================================================================*\
 * 
 *		 	       %gtk-reset-callback!
 *
 * gtk-rseset-callback! is quite similar to %gtk-set-callback!, the closure 
 * passed must be the old callback associated so that it can be deleted 
 * from the GTK event list.
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%gtk-reset-callback!", gtk_reset_callback, subr4, 
		 (SCM widget, SCM closure, SCM signal, SCM has_eventp))
{
  GtkSignalFunc handler;
 
  if (!WIDGETP(widget)) STk_error_bad_widget(widget);
  if (!STRINGP(signal)) STk_error("bad signal name ~S", signal);

  handler = (has_eventp!=STk_false) ? call_handler3: (GtkSignalFunc)call_handler2;
  
  /* Do a GTK disconnection */
  gtk_signal_disconnect_by_func(GTK_OBJECT(WIDGET_ID(widget)),
				handler,
				closure);
  return STk_void;
}


/*===========================================================================*\
 * 
 * 				Initializations
 *
\*===========================================================================*/
void STk_init_gtk_signal(void)
{
  ADD_PRIMITIVE(gtk_set_callback);
  ADD_PRIMITIVE(gtk_reset_callback);
}
