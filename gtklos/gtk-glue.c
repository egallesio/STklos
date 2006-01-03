/*
 * gtk-glue.c				-- Connection to GTK+
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
 *    Creation date: 24-May-2000 17:31 (eg)
 * Last file update:  8-May-2002 21:17 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"

static int tc_widget;	/* The type_cell number associated to widgets */
int STk_tc_widget;	/* this one is the exported version of the previous one */
SCM STk_gtk_module;	/* The Gtk module */

/*===========================================================================*\
 * 
 * 				Utilities 
 * 
\*===========================================================================*/

void STk_error_bad_widget(SCM obj)	/* this one is public, cause heavily used */
{
  STk_error("bad widget ~S", obj);
}

static void error_bad_option(SCM obj)
{
  STk_error("bad option ~S", obj);
}

static void error_bad_type(SCM obj)
{
  STk_error("bad type number ~S", obj);
}


/*
 * Type Management Utilities
 *
 */

static Inline SCM gtk_enum2scheme(GtkArg *arg)
{
  GtkEnumValue* v = gtk_type_enum_get_values(arg->type);

  return STk_intern(v[GTK_VALUE_ENUM(*arg)].value_nick);
}
  

static Inline void scheme2gtk_enum(GtkArg *arg, SCM val)
{
  GtkEnumValue* v = gtk_type_enum_get_values(arg->type);
  char *str       = SYMBOL_PNAME(val);
  int i;
  
  for (i = 0; v[i].value_nick; i++) {
    if (strcmp(v[i].value_nick, str) == 0) {
      GTK_VALUE_ENUM(*arg) = i;
      return;
    }
  }
  STk_error("~S is a bad value for widget slot %S (type must be %S)",
	    val, arg->name, gtk_type_name(arg->type));
}


static Inline char *pretty_name(SCM w)
{
  return gtk_type_name(GTK_OBJECT_TYPE(WIDGET_ID(w)));
}


/*===========================================================================*\
 * 
 * 		GTK Arguments getters and setters
 * 
\*===========================================================================*/

static Inline SCM get_arg(GtkArg *arg, SCM option)
{
  switch (GTK_FUNDAMENTAL_TYPE(arg->type)) {
    case GTK_TYPE_CHAR:	   return MAKE_CHARACTER(GTK_VALUE_CHAR(*arg));
    case GTK_TYPE_UCHAR:   return MAKE_CHARACTER(GTK_VALUE_UCHAR(*arg));
    case GTK_TYPE_BOOL:    return MAKE_BOOLEAN(GTK_VALUE_BOOL(*arg));
    case GTK_TYPE_INT:     return STk_long2integer((long) GTK_VALUE_INT(*arg));
    case GTK_TYPE_UINT:    return STk_ulong2integer((long) GTK_VALUE_UINT(*arg));
    case GTK_TYPE_LONG:
    case GTK_TYPE_FLAGS:   return STk_long2integer(GTK_VALUE_LONG(*arg));
    case GTK_TYPE_ULONG:   return STk_ulong2integer(GTK_VALUE_ULONG(*arg));
    case GTK_TYPE_STRING:  {
      			     char *s = GTK_VALUE_STRING(*arg);
			     return STk_Cstring2string(s? s: "");
    			   }
    case GTK_TYPE_FLOAT:   return STk_double2real((double) GTK_VALUE_FLOAT(*arg));
    case GTK_TYPE_DOUBLE:  return STk_double2real(GTK_VALUE_DOUBLE(*arg));
    case GTK_TYPE_ENUM:	   return gtk_enum2scheme(arg);
    case GTK_TYPE_OBJECT:  { 
      			     /* Return the STklos object (if possible) or #f  */
      			     GtkObject*	w = GTK_VALUE_OBJECT(*arg);
			     
			     if (w) {
			       SCM res = (SCM) gtk_object_get_user_data(w);
			       if (res) return res;
			     } 
			     return STk_false;
    			   }
    default: 	           STk_error("don't know how to convert type %d "
				     "yet for ~S option", 
				     GTK_FUNDAMENTAL_TYPE(arg->type), option);
  }
  return STk_void;	/* never reached */
}


static Inline void set_arg(GtkArg *arg, SCM option, SCM val)
{
  switch (GTK_FUNDAMENTAL_TYPE(arg->type)) {
    case GTK_TYPE_CHAR:
      if (CHARACTERP(val)) 
	{ GTK_VALUE_CHAR(*arg)= (char)CHARACTER_VAL(val); return;}
      break;
    case GTK_TYPE_UCHAR:
      if (CHARACTERP(val)) { GTK_VALUE_UCHAR(*arg) = CHARACTER_VAL(val); return; }
      break;
    case GTK_TYPE_BOOL:
      if (BOOLEANP(val)) { GTK_VALUE_BOOL(*arg) = (val == STk_true); return; }
      break;
    case GTK_TYPE_INT: {
      long int v = STk_integer_value(val);
      if (v != LONG_MIN) { GTK_VALUE_INT(*arg) = (int) v; return; }
      break;
    }
    case GTK_TYPE_UINT: {
      unsigned long v = STk_uinteger_value(val);
      if (v != ULONG_MAX) { GTK_VALUE_UINT(*arg) = (unsigned int) v; return; }
      break;
    }
    case GTK_TYPE_LONG: 
    case GTK_TYPE_FLAGS: {
      long v = STk_integer_value(val);
      if (v != (long) LONG_MIN) { GTK_VALUE_LONG(*arg) = v; return; }
      break;
    }
    case GTK_TYPE_ULONG: {
      unsigned long v = STk_uinteger_value(val);
      if (v != (unsigned long) LONG_MIN) { GTK_VALUE_ULONG(*arg) = v; return; }
      break;
    }
    case GTK_TYPE_STRING:
      if (STRINGP(val)) { GTK_VALUE_STRING(*arg) = STRING_CHARS(val); return; }
      break;
    case GTK_TYPE_FLOAT: {
      double v = STk_number2double(val);
      if (!isnan(v)) { GTK_VALUE_FLOAT(*arg) = (float) v; return; }
      break;
    }
    case GTK_TYPE_DOUBLE: {
      double v = STk_number2double(val);
      if (!isnan(v)) { GTK_VALUE_DOUBLE(*arg) = v; return; }
      break;
    }
    case GTK_TYPE_ENUM:
      if (STRINGP(val)) { 
	val = STk_string2symbol(val);
      }
      if (SYMBOLP(val)) { scheme2gtk_enum(arg, val); return; }
      break;
    case GTK_TYPE_OBJECT:
      if (WIDGETP(val)) 
	{ GTK_VALUE_OBJECT(*arg)=GTK_OBJECT(WIDGET_ID(val)); return; }
      break;
    default:
      STk_error("don't know how to convert type %d yet for ~S option (val= ~S)", 
		GTK_FUNDAMENTAL_TYPE(arg->type), option, val);
  }
  STk_error("~S is a bad value for widget slot %S", val, KEYWORD_PNAME(option));
}



/*
 * Simple widget constructor 
 *
 */

SCM STk_gtk_widget2scm(GtkObject *w, SCM scm_obj)
{
  SCM z;
  if (w) {
    NEWCELL(z, widget);
    WIDGET_ID(z)      = (GtkWidget *) w;
    WIDGET_PLIST(z)   = STk_nil;
    //    WIDGET_SCMOBJ(z)  = scm_obj;
    //    WIDGET_SIGNALS(z) = STk_nil;

    /* Add a pointer to the STklos object in the GTK widget */
    gtk_object_set_user_data(GTK_OBJECT(w), (gpointer) scm_obj);
    /* By default show window (this is the contary of GTK) */
    if (GTK_IS_WIDGET(w)) gtk_widget_show((GtkWidget *) w);
    return z;
  }
  return STk_void;
}


/*======================================================================*/

DEFINE_PRIMITIVE("%widget?", widgetp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(WIDGETP(obj));
}
 

DEFINE_PRIMITIVE("%widget-type", widget_type, subr1, (SCM obj))
{
  if (!WIDGETP(obj)) STk_error_bad_widget(obj);
  return STk_Cstring2string(pretty_name(obj));
}

DEFINE_PRIMITIVE("%widget-plist", widget_plist, subr23, (SCM obj, SCM key, SCM v))
{
  if (!WIDGETP(obj)) STk_error_bad_widget(obj);

  if (v) {
    /* Setting a new value for widget plist */
    WIDGET_PLIST(obj) = STk_key_set(WIDGET_PLIST(obj), key, v);
    return STk_void;
  } 

  /* Reading a value in widget plist */
  return STk_key_get(WIDGET_PLIST(obj), key, STk_void);
}

DEFINE_PRIMITIVE("%widget->object", widget2object, subr1, (SCM obj))
{
  SCM res;

  if (!WIDGETP(obj)) STk_error_bad_widget(obj);
  
  res = (SCM) gtk_object_get_user_data((GtkObject *) WIDGET_ID(obj));
  return (res && res != STk_void) ? res : STk_false;
}

  


DEFINE_PRIMITIVE("%set-widget-parent!", set_parent, subr2, (SCM w, SCM parent))
{
  GtkWidget *old_parent;

  if (!WIDGETP(w))      STk_error_bad_widget(w);
  if (!WIDGETP(parent)) STk_error_bad_widget(parent);
    
  old_parent = WIDGET_PARENT(w);
  if (old_parent) {
    /* Reparent window */
    gtk_widget_reparent(WIDGET_ID(w), WIDGET_ID(parent));
  } else {
    if (GTK_IS_SCROLLED_WINDOW(WIDGET_ID(parent))) {
      /* Gtk scrolled-windows need to call another function for adding a widget */
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(WIDGET_ID(parent)), 
					    WIDGET_ID(w));
    } 
    else
      /* Normal case: simply call gtk_container_add */
      gtk_container_add(GTK_CONTAINER(WIDGET_ID(parent)), WIDGET_ID(w));
  }
  return STk_void;
}



/*===========================================================================*\
 * 
 * 			Primitives to access widget "ARGS" 
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%gtk-arg-get", gtk_arg_get, subr2, (SCM widget, SCM option))
{
  GtkArg arg;

  if (!WIDGETP(widget))  STk_error_bad_widget(widget);
  if (!KEYWORDP(option)) error_bad_option(option);

  /* Build the request */
  arg.type = GTK_TYPE_STRING;
  arg.name = KEYWORD_PNAME(option);
  gtk_object_arg_get((GtkObject *) WIDGET_ID(widget), &arg, NULL);
  
  return get_arg(&arg, option);
}


DEFINE_PRIMITIVE("%gtk-arg-set!",gtk_arg_set, subr3,(SCM widget,SCM option,SCM val))
{
  GtkArg arg;

  if (!WIDGETP(widget))  STk_error_bad_widget(widget);
  if (!KEYWORDP(option)) error_bad_option(option);

  /* 
   * Build a "GET" request to obtain object type. Probably not very efficient, but
   * I don't see a better way for now 
   */
  arg.type = GTK_TYPE_STRING;
  arg.name = KEYWORD_PNAME(option);
  gtk_object_arg_get((GtkObject *) WIDGET_ID(widget), &arg, NULL);

  /* Set the new value and give it to GTK */
  set_arg(&arg, option, val);
  gtk_object_arg_set((GtkObject *) WIDGET_ID(widget), &arg, NULL);

  return STk_void;
}

DEFINE_PRIMITIVE("%gtk-arg-string-set!",gtk_arg_string_set, subr3,
		 (SCM widget,SCM option,SCM val))
{ /* special version of %gtk-arg-set whereval must be a string */
  GtkArg arg;

  if (!WIDGETP(widget))  STk_error_bad_widget(widget);
  if (!KEYWORDP(option)) error_bad_option(option);
  if (!STRINGP(val)) 	 STk_error("bad string ~S", val);

  arg.type = GTK_TYPE_STRING;
  arg.name = KEYWORD_PNAME(option);
  set_arg(&arg, option, val);
  gtk_object_arg_set((GtkObject *) WIDGET_ID(widget), &arg, NULL);

  return STk_void;
}

/*===========================================================================*\
 * 
 * 			Primitives to access childs "ARGS" 
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%child-get", child_get, subr3, (SCM cont, SCM widget, SCM option))
{
  GtkArg arg;

  if (!WIDGETP(cont))    STk_error_bad_widget(cont);
  if (!WIDGETP(widget))  STk_error_bad_widget(widget);
  if (!KEYWORDP(option)) error_bad_option(option);

  /* Build the request */
  arg.type = GTK_TYPE_STRING;
  arg.name = KEYWORD_PNAME(option);
  gtk_container_arg_get(GTK_CONTAINER(WIDGET_ID(cont)),
			WIDGET_ID(widget),
			&arg,
			NULL);
  
  return get_arg(&arg, option);
}


DEFINE_PRIMITIVE("%child-set!", child_set, subr4, 
		 (SCM cont, SCM widget, SCM option, SCM val))
{
  GtkArg arg;
  GtkWidget *c, *w;
  
  if (!WIDGETP(cont))    STk_error_bad_widget(cont);
  if (!WIDGETP(widget))  STk_error_bad_widget(widget);
  if (!KEYWORDP(option)) error_bad_option(option);

  c = WIDGET_ID(cont);
  w = WIDGET_ID(widget);
 
  /* 
   * Build a "GET" request to obtain object type. Probably not very efficient, but
   * I don't see a better way for now 
   */
  arg.type = GTK_TYPE_STRING;
  arg.name = KEYWORD_PNAME(option);
  gtk_container_arg_get(GTK_CONTAINER(c), w, &arg, NULL);

  /* Set the new value and give it to GTK */
  set_arg(&arg, option, val);
  gtk_container_arg_set(GTK_CONTAINER(c), w, &arg, NULL);

  return STk_void;
}



DEFINE_PRIMITIVE("%destroy", destroy, subr1, (SCM obj))
{
  if (!WIDGETP(obj)) STk_error_bad_widget(obj);
  gtk_object_destroy(GTK_OBJECT(WIDGET_ID(obj)));
  return STk_void;
}


/*===========================================================================*\
 * 
 *		 		Tooltips
 *
\*===========================================================================*/
static GtkTooltips* the_tooltips;    

DEFINE_PRIMITIVE("%tooltip", add_tooltip, subr12, (SCM widget, SCM text))
{
  if (!WIDGETP(widget)) STk_error_bad_widget(widget);

  if (text) {
    /* Setting a tooltip */
    if (!STRINGP(text))   STk_error("bad string for a tooltip ~S", text);
    
    gtk_tooltips_set_tip(GTK_TOOLTIPS(the_tooltips),
			 WIDGET_ID(widget),
			 STRING_CHARS(text),
			 "");
    return STk_void;
  } else {
    /* Reading a tooltip */
    GtkTooltipsData *data;
    
    data = gtk_tooltips_data_get(WIDGET_ID(widget));
    return (data) ? STk_Cstring2string(data->tip_text) : STk_false;
  }
}

DEFINE_PRIMITIVE("%tooltip-configure", tooltip_conf, subr12, (SCM what, SCM v))
{
  switch (AS_LONG(what)) {
    case SCM_LONG(0): gtk_tooltips_disable(the_tooltips); break;
    case SCM_LONG(1): gtk_tooltips_enable(the_tooltips); break;
    case SCM_LONG(2): {
      long delay = STk_integer_value(v);

      if (delay == LONG_MIN) STk_error("bad integer for delay ~S", v);
      gtk_tooltips_set_delay(the_tooltips, delay);
      break;
    }
    default: STk_error("bad code ~S", what);
  }
  return STk_void;
}


/*===========================================================================*\
 * 
 * 		       Debug functions
 * 
\*===========================================================================*/
#ifdef STK_DEBUG
DEFINE_PRIMITIVE("%gdebug", gtk_debug, subr1, (SCM w))
{
  STk_debug("GDEBUG ==>");
  STk_debug("%x", WIDGET_PARENT(w));
  return STk_void;
}
#endif
/*===========================================================================*\
 * 
 * 		       M O D U L E   i n i t i a l i z a t i o n  
 * 
\*===========================================================================*/

static void print_widget(SCM n, SCM port, int mode)
{
  char buffer[200];
  
  sprintf(buffer, "#[%s widget %lx]", pretty_name(n), (unsigned long) n);
  STk_puts(buffer, port);
}

static struct extended_type_descr xtype_widget = {
  "widget",
  print_widget
};

/* ====================================================================== */


MODULE_ENTRY_START("gtklos") 
{
  int argc = 0;
  
#ifdef HAVE_GNOME
  /* Initialize Gnome */
  static char *app_name[] = {"GTklos"};

  argc = 1;
  gnome_init(*app_name, GTKLOS_VERSION, argc, app_name);
#else
  /* Initialize GTk+. */
  gtk_init(&argc, NULL);
#endif

  /* Create the module gtk */
  STk_gtk_module = STk_create_module(STk_intern("gtk"));
  
  /* Create the new widget type and export it!*/
  DEFINE_USER_TYPE(tc_widget, &xtype_widget);
  STk_tc_widget = tc_widget;	/* because macros don't deal with "STk_" prefix */

  /* Add primitives */
  ADD_PRIMITIVE(widgetp);
  ADD_PRIMITIVE(widget_type);
  ADD_PRIMITIVE(widget_plist);
  ADD_PRIMITIVE(widget2object);
  ADD_PRIMITIVE(set_parent);

  ADD_PRIMITIVE(gtk_arg_get);
  ADD_PRIMITIVE(gtk_arg_set);
  ADD_PRIMITIVE(gtk_arg_string_set);

  ADD_PRIMITIVE(child_get);
  ADD_PRIMITIVE(child_set);


  ADD_PRIMITIVE(destroy);
  
  /* Tooltips stuff */
  the_tooltips = gtk_tooltips_new();
  ADD_PRIMITIVE(add_tooltip);
  ADD_PRIMITIVE(tooltip_conf);
#ifdef STK_DEBUG
  ADD_PRIMITIVE(gtk_debug);
#endif

  /* Initialize the rest of the library */
  STk_init_gtk_signal();
  STk_init_gtk_cont();
  STk_init_gtk_image();
  STk_init_gtk_label();
  STk_init_gtk_editable();
  STk_init_gtk_list();
  STk_init_gtk_menu();
  STk_init_gtk_misc();
#ifdef HAVE_GNOME
  STk_init_gtk_canvas(),
#endif
  STk_init_gtk_event();/* must be last */
}
MODULE_ENTRY_END
