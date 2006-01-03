/*
 * gtk-menu.c	-- GTK+ Menu Management
 * 
 * Copyright © 2000-2003 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 27-Oct-2000 21:43 (eg)
 * Last file update: 11-Feb-2003 00:07 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"

static void error_bad_menu(SCM obj)
{
  STk_error("bad menu ~S", obj);
}

static void error_bad_control_code(SCM obj)
{
  STk_error("bad control code ~S", obj);
}

static void error_missing_parameter(SCM obj)
{
  STk_error("parameter missing for code ~S", obj);
}

static void error_bad_toolbar(SCM obj)
{
  STk_error("bad toolbar ~S", obj);
}

/*===========================================================================*\
 * 
 * 				   M E N U - I T E M
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%menu-item", menu_item, subr23, (SCM obj, SCM type, SCM sibling))
{
  SCM label, res;
  GtkWidget *menu_item = NULL; /* for gcc */
  GtkWidget *accel_label;

  switch (AS_LONG(type)) {
    case SCM_LONG(0): 					/* Normal Item */ 
      menu_item = gtk_menu_item_new(); 
      break;
    case SCM_LONG(1): 					/* Check Item */ 
      menu_item = gtk_check_menu_item_new(); 
      gtk_check_menu_item_set_show_toggle((GtkCheckMenuItem *)menu_item, TRUE);
      break;
    case SCM_LONG(2):					/* Radio Item */
      if (!sibling || sibling == STk_false)
	/* First radio button item of a group */
	menu_item = gtk_radio_menu_item_new(NULL);
      else {
	GSList *lst;
	/* Create a new radio button item in the group of given sibling */
	if (!WIDGETP(sibling) || !GTK_IS_RADIO_MENU_ITEM(WIDGET_ID(sibling)))
	  STk_error("the given sibling (~S) is not a radio menu item", sibling);
	lst = gtk_radio_menu_item_group((GtkRadioMenuItem *) WIDGET_ID(sibling));
	menu_item = gtk_radio_menu_item_new(lst);
      }
      gtk_check_menu_item_set_show_toggle((GtkCheckMenuItem *)menu_item, TRUE);
      break;
    default:
      error_bad_control_code(type);
  }

  accel_label = gtk_accel_label_new("");
  gtk_misc_set_alignment(GTK_MISC(accel_label), 0.0, 0.5);

  gtk_container_add(GTK_CONTAINER(menu_item), accel_label);
  gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(accel_label), menu_item);
  gtk_widget_show(accel_label);

  /* Enter the scheme world */
  label = WIDGET_TO_SCM(accel_label, STk_void);
  res   = WIDGET_TO_SCM(menu_item, obj);
  
  WIDGET_PLIST(res) = LIST2(STk_makekey(":label"), label);
  return res;
}

DEFINE_PRIMITIVE("%connect-menu", connect_menu, subr2, (SCM item, SCM menu))
{
  if (!WIDGETP(item) || !GTK_IS_MENU_ITEM(WIDGET_ID(item)))
    STk_error("bad menu item ~S", item);
  if (!WIDGETP(menu) || !GTK_IS_MENU(WIDGET_ID(menu)))
    error_bad_menu(menu);
  
  gtk_menu_item_set_submenu((GtkMenuItem *)WIDGET_ID(item), WIDGET_ID(menu));
  
  return STk_void;
}

DEFINE_PRIMITIVE("%menu-item-ctrl", menu_item_ctrl, subr23, 
		 (SCM item, SCM what, SCM val))
{
  if (!WIDGETP(item) || !GTK_IS_MENU_ITEM(WIDGET_ID(item)))
    STk_error("bad menu item ~S", item);
  
  switch (AS_LONG(what)) {
    case SCM_LONG(0): 				/* get value on a check */
      return MAKE_BOOLEAN(GTK_CHECK_MENU_ITEM(WIDGET_ID(item))->active);
    case SCM_LONG(1): 				/* set value on a check */
      if (val)
	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(WIDGET_ID(item)),
				       val != STk_false);
      else
	error_missing_parameter(what);
      break;
    case SCM_LONG(2):				/* Right justify item */ 
      gtk_menu_item_right_justify((GtkMenuItem *) WIDGET_ID(item));
      break;
    default: error_bad_control_code(what);
  }
  return STk_void;
}


/*===========================================================================*\
 * 
 * 			   M E N U - S E P A R A T O R
 *
\*===========================================================================*/
SIMPLE_WIDGET("%menu-separator", menu_separator, (gtk_menu_item_new()));

/*===========================================================================*\
 * 
 * 			   M E N U - T E A R - O F F
 *
\*===========================================================================*/
SIMPLE_WIDGET("%menu-tear-off", menu_tear_off, (gtk_tearoff_menu_item_new()));




/*===========================================================================*\
 * 
 * 				M E N U B A R
 *
\*===========================================================================*/
SIMPLE_WIDGET("%menubar", menubar, (gtk_menu_bar_new()));

DEFINE_PRIMITIVE("%menubar-add", menubar_add, subr2, (SCM mbar, SCM item))
{
  //FIXME:  Controler
  gtk_menu_bar_append(GTK_MENU_BAR(WIDGET_ID(mbar)), WIDGET_ID(item));
  return STk_void;
}


/*===========================================================================*\
 * 
 * 					M E N U
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%menu", menu, subr1, (SCM obj)) // Utiliser STk_gtk_widget2scm
{
  SCM z;
  GtkWidget *w = gtk_menu_new();
  int tc_widget = STk_tc_widget;

  if (w) {
    NEWCELL(z, widget);
    WIDGET_ID(z)      = w;
    //    WIDGET_SIGNALS(z) = STk_nil;
    WIDGET_PLIST(z)   = STk_nil;
    /* Add a pointer to the STklos object in the GTK widget */
    gtk_object_set_user_data(GTK_OBJECT(w), (gpointer) obj);
    ///* By default show window (this is the contary of GTK) */
    //if (GTK_IS_WIDGET(w)) gtk_widget_show(w);

    return z;
  }
  return STk_void;
}


DEFINE_PRIMITIVE("%menu-control", menu_control, subr23,(SCM w, SCM what, SCM param))
{
  GtkMenu *menu;

  if (!WIDGETP(w) || !GTK_IS_MENU(WIDGET_ID(w))) error_bad_menu(w);
  menu = (GtkMenu *) WIDGET_ID(w);

  switch (AS_LONG(what)) {
    case SCM_LONG(0): {
      /* Append a menu-item to the menu */
      if (param && (!WIDGETP(param) || !GTK_IS_MENU_ITEM(WIDGET_ID(param))))
	STk_error("cannot add ~S to menu ~S", param, w);
      gtk_menu_append(menu, WIDGET_ID(param));
      break;
    }
    case SCM_LONG(1): {
      /* Return title of menu when torn off*/
      gchar * title = gtk_object_get_data(GTK_OBJECT (menu), "gtk-menu-title");

      return STk_Cstring2string(title ? title : "");
    }
    case SCM_LONG(2): {
      /* Set torn off state */
      if (param)
	if (STRINGP(param)) {
	  gtk_menu_set_title(menu, (gchar*) STRING_CHARS(param));
	}
	else
	  STk_error("bad string for title ~S", param);
      else
	error_missing_parameter(what);
      break;
    }
    default: error_bad_control_code(what);
  }
  return STk_void;
}
    

/*===========================================================================*\
 * 
 * 				T O O L B A R 
 *
\*===========================================================================*/
static void toolbar_click(GtkObject* w, gpointer data)
{
  /* "data" is the closure to call. Its argument is the object which wraps "w" */
  //  STk_C_apply((SCM) data, 1, (SCM) gtk_object_get_user_data(GTK_OBJECT(w)));
  STk_C_apply((SCM) data, 0);
}


SIMPLE_WIDGET("%toolbar", toolbar, (gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,
						    GTK_TOOLBAR_ICONS)));

DEFINE_PRIMITIVE("%toolbar-add", toolbar_add, subr5, (SCM toolbar, SCM text,
						      SCM tooltip, SCM icon, 
						      SCM callback))
{
  char *tip_chars;

  if (!WIDGETP(toolbar) || !GTK_IS_TOOLBAR(WIDGET_ID(toolbar))) 
    error_bad_toolbar(toolbar);
  if (!STRINGP(text)) 
    STk_error("bad toolbar text ~S", text);
  if (!STRINGP(tooltip) && tooltip != STk_false) 
    STk_error("bad toolbar tooltip ~S", tooltip);
  if (!WIDGETP(icon)) /* FIXME: Test if it is a GTK_MISC? */
    STk_error("bad toolbar icon ~S", icon);
  if (STk_procedurep(callback) == STk_false)	/* FIXME: Test arity */
    STk_error("bad toolbar callback ~S", callback);
  
  /* Initialise tooltips */
  tip_chars = (tooltip == STk_false) ? NULL: STRING_CHARS(tooltip);

  /* Everything is correct now. Just add a toolbar item */
  return WIDGET_TO_SCM(
    		gtk_toolbar_append_item((GtkToolbar*) WIDGET_ID(toolbar),
					STRING_CHARS(text),
					tip_chars,
					NULL,
					WIDGET_ID(icon),
					toolbar_click,
					(gchar*) callback), 
		STk_void);
}

DEFINE_PRIMITIVE("%toolbar-add-space", toolbar_space, subr1, (SCM toolbar))
{
  if (!WIDGETP(toolbar) || !GTK_IS_TOOLBAR(WIDGET_ID(toolbar))) 
    error_bad_toolbar(toolbar);
  gtk_toolbar_append_space((GtkToolbar*) WIDGET_ID(toolbar));
  return STk_void;
}

/*===========================================================================*\
 * 
 * 				O P T I O N - M E N U 
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%opt-menu", opt_menu, subr1, (SCM obj))
{
  GtkWidget *menu, *w = gtk_option_menu_new();

  menu = gtk_option_menu_get_menu((GtkOptionMenu *) w);
  /* FIXME: STk_debug("menu = %x", menu); */

  WIDGET_PLIST(w) = LIST2(STk_makekey(":list"),
			  WIDGET_TO_SCM(menu, STk_void));
  return WIDGET_TO_SCM(w, obj);
}


DEFINE_PRIMITIVE("%opt-menu-add", opt_menu_add, subr2, (SCM obj, SCM menu))
{
  if (!WIDGETP(obj) || !GTK_IS_OPTION_MENU(WIDGET_ID(obj)))
    STk_error_bad_widget(obj);
  if (!WIDGETP(menu) || !GTK_IS_MENU(WIDGET_ID(menu)))
    STk_error_bad_widget(obj);

  gtk_option_menu_set_menu((GtkOptionMenu *) WIDGET_ID(obj), WIDGET_ID(menu));
  return STk_void;
}
  

void STk_init_gtk_menu(void)
{
  ADD_PRIMITIVE(menubar);
  ADD_PRIMITIVE(menubar_add);

  ADD_PRIMITIVE(menu);
  ADD_PRIMITIVE(menu_control);
  
  ADD_PRIMITIVE(menu_item);
  ADD_PRIMITIVE(connect_menu);
  ADD_PRIMITIVE(menu_item_ctrl);

  ADD_PRIMITIVE(menu_separator);
  ADD_PRIMITIVE(menu_tear_off);

  ADD_PRIMITIVE(toolbar);
  ADD_PRIMITIVE(toolbar_add);
  ADD_PRIMITIVE(toolbar_space);

  ADD_PRIMITIVE(opt_menu);
  ADD_PRIMITIVE(opt_menu_add);
}
