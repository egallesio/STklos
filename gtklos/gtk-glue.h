/*
 * gtk-glue.h				-- Connection to GTK+
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
 *    Creation date: 15-Aug-2000 18:41 (eg)
 * Last file update:  8-Apr-2002 22:20 (eg)
 */

#include <math.h>
#include "gtklosconf.h"
#ifdef HAVE_GNOME
#  include <gnome.h>
#else
#  include <gtk/gtk.h>
#endif

#define GTKLOS_VERSION "0.2"

/*===========================================================================*\
 * 
 * 		Definition of the widget type 
 *
\*===========================================================================*/

extern int STk_tc_widget;	/* The type_cell number associated to widgets */
extern SCM STk_gtk_module;	/* The Gtk module */


struct widget_obj {
  stk_header header;		/* Standard STklos header */
  GtkWidget *id;		/* Pointer to the GTK object */
  //  SCM signals;			/* Scheme signals associated to the widget */
  SCM prop_list;		/* A property list associated to the widget */
};

#define WIDGETP(w)	  (BOXED_TYPE_EQ((w),STk_tc_widget))/* and not tc-widget! */
#define WIDGET_ID(w)	  (((struct widget_obj *) (w))->id)
// #define WIDGET_SIGNALS(w) (((struct widget_obj *) (w))->signals)
#define WIDGET_PLIST(w)	  (((struct widget_obj *) (w))->prop_list)

/* This should be provided by GTK, and could cause problem some day. 
 * Anyway this works for now */
#define WIDGET_PARENT(w)  (((GtkWidget *) ((struct widget_obj *) (w))->id)->parent)




/*
 * 	Constructor for simple widgets 
 */
SCM STk_gtk_widget2scm(GtkObject *w, SCM scm_object);

#define WIDGET_TO_SCM(w, obj)	STk_gtk_widget2scm((GtkObject *) (w), (obj))

#define SIMPLE_WIDGET(sname, name, build_code)		\
    DEFINE_PRIMITIVE(sname, name, subr1, (SCM obj))	\
    {							\
      return WIDGET_TO_SCM((build_code), obj);		\
    }

/*
 * 	Signals
 */
SCM STk_make_callback(GtkSignalFunc fct);
void STk_set_signal(SCM widget, SCM signal, GtkSignalFunc callback, SCM closure);

SCM STk_get_signal(SCM w, SCM signal);


/*
 *	Events
 */ 
SCM STk_make_event(SCM receiver, GdkEvent *event);

/*
 *	Misc 
 */

#define DEF_AJUSTMENT 0.0, 0.0, 100.0, 0.5, 1.0, 0.0 /* default adjustment */

void STk_error_bad_widget(SCM obj);

void STk_init_gtk_signal(void);
void STk_init_gtk_cont(void);
void STk_init_gtk_label(void);
void STk_init_gtk_editable(void);
void STk_init_gtk_list(void);
void STk_init_gtk_misc(void);
void STk_init_gtk_event(void);
void STk_init_gtk_menu(void);
void STk_init_gtk_image(void);
#ifdef HAVE_GNOME
void STk_init_gtk_canvas(void);
#endif
