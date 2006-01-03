/*
 * gtk-editable.c	-- GTK+ editable and descendants
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
 *    Creation date: 19-Aug-2000 12:09 (eg)
 * Last file update:  8-Apr-2002 18:02 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"

static GdkFont *fixed_font;


DEFINE_PRIMITIVE("%gtk-editable-get-or-set", editable_get_set, subr12,
		 (SCM w, SCM val))
{
  GtkEditable *e = NULL;
 
  if (!WIDGETP(w)) STk_error_bad_widget(w);

  if (GTK_IS_COMBO(WIDGET_ID(w))) 
    e = GTK_EDITABLE(GTK_COMBO(WIDGET_ID(w))->entry);
  else 
    if (GTK_IS_EDITABLE(WIDGET_ID(w))) 
      e = GTK_EDITABLE(WIDGET_ID(w));
    else
      STk_error_bad_widget(w);
  
  if (!val) {
    /* get entry value */
    return STk_Cstring2string(gtk_editable_get_chars(e, 0, -1));
  } else {
    /* set entry value */
    char *s = (char *)  STRING_CHARS(val);
    int pos = 0; 

    if (!STRINGP(val)) STk_error("bad string ~S", val);
    gtk_editable_delete_text(e, 0, -1);
    gtk_editable_insert_text(e, s, strlen(s), &pos);
    return STk_void;
  }
}

/*===========================================================================*\
 * 
 * 				  E N T R Y 
 *
\*===========================================================================*/
SIMPLE_WIDGET("%entry", entry, (gtk_entry_new()));

/*===========================================================================*\
 * 
 * 				C O M B O B O X
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%combobox", combobox, subr1, (SCM obj))
{ 
  GtkWidget *combo = gtk_combo_new();
  SCM res;

  res = WIDGET_TO_SCM(combo, obj);
  
  /* Keep a pointer on the entry and the listbox in the combo plist */
  WIDGET_PLIST(res) = LIST4(STk_makekey(":entry"), 
			    WIDGET_TO_SCM(GTK_COMBO(combo)->entry, obj),
			    STk_makekey(":listbox"),
			    WIDGET_TO_SCM(GTK_COMBO(combo)->list, obj));
  return res;
}


/*===========================================================================*\
 * 
 * 				  S P I N B O X 
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%spinbox", spinbox, subr1, (SCM obj))
{
  SCM adj, res;
  GtkObject *adjustment;
  GtkWidget *sb;
  
  /* Create a new ajustement */
  adjustment = gtk_adjustment_new(DEF_AJUSTMENT);
  adj 	     = WIDGET_TO_SCM(adjustment, obj);
  
  /* create the spinbox widget */
  sb  = gtk_spin_button_new((GtkAdjustment *) adjustment, 1.0, 2);
  res = WIDGET_TO_SCM(sb, obj);
  
  /* Keep a reference on the adjustement through the gauge */
  WIDGET_PLIST(res) = LIST2(STk_makekey(":adjustment"), adj);
  
  return res;
}

/*===========================================================================*\
 * 
 * 				  T E X T
 *
\*===========================================================================*/

DEFINE_PRIMITIVE("%text", text, subr1, (SCM obj))
{
  SCM res; 
  
  res = WIDGET_TO_SCM(gtk_text_new(NULL, NULL), obj);
  /* GTK text are non modifiable by default. Chage that */
  gtk_text_set_editable((GtkText *) WIDGET_ID(res), TRUE);

  /* This is the only way I've found to set the font to "fixed", insert 
   * a null string in the text widget with the "fixed" font!!!!!
   */
  gtk_text_insert((GtkText*) WIDGET_ID(res),
		  fixed_font, /* font */
		  NULL, /* foreground */
		  NULL, /* background */
		  "",
		  0);
  return res;
}

DEFINE_PRIMITIVE("%text-vadj", text_vadj, subr1, (SCM obj))
{
  if (!WIDGETP(obj) || !GTK_IS_TEXT(WIDGET_ID(obj)))
    STk_error("bad text widget ~S", obj);
  return WIDGET_TO_SCM(GTK_TEXT(WIDGET_ID(obj))->vadj, obj);
}

DEFINE_PRIMITIVE("%text-ctrl", text_ctrl, subr23, (SCM obj, SCM what, SCM val))
{
  if (!WIDGETP(obj) || !GTK_IS_TEXT(WIDGET_ID(obj)))
    STk_error_bad_widget(obj);

  switch (AS_LONG(what)) {
    case SCM_LONG(0): 		/* get point */
      return MAKE_INT(gtk_text_get_point((GtkText*) WIDGET_ID(obj)));
    case SCM_LONG(1): {		/* set point */
      int point; 
      
      if (!val) 	 STk_error("missing point value");
      point = STk_integer_value(val);
      if (point == LONG_MIN) STk_error("bad value for point ~S", val);
      gtk_text_set_point((GtkText*) WIDGET_ID(obj), point);
      break;
    }
    case SCM_LONG(2): 	/* freeze */
      gtk_text_freeze((GtkText*) WIDGET_ID(obj));
      break;
    case SCM_LONG(3): 	/* unfreeze */
      gtk_text_thaw((GtkText*) WIDGET_ID(obj));
      break;
    case SCM_LONG(4): 	/* Cut */
      gtk_editable_cut_clipboard(GTK_EDITABLE(WIDGET_ID(obj)));
      break;
    case SCM_LONG(5): 	/* Copy */
      gtk_editable_copy_clipboard(GTK_EDITABLE(WIDGET_ID(obj)));
      break;
    case SCM_LONG(6): 	/* Paste */
      gtk_editable_paste_clipboard(GTK_EDITABLE(WIDGET_ID(obj)));
      break;
    default:
      STk_error("bad code ~S", what);
  }
  return STk_void;
}


DEFINE_PRIMITIVE("%text-insert", text_insert, subr2, (SCM obj, SCM str))
{
  if (!WIDGETP(obj) || !GTK_IS_TEXT(WIDGET_ID(obj)))
    STk_error_bad_widget(obj);
  if (!STRINGP(str))
    STk_error("bad string ~S", str);

  gtk_text_insert((GtkText*) WIDGET_ID(obj),
		  fixed_font, /* font */
		  NULL, /* foreground */
		  NULL, /* background */
		  STRING_CHARS(str),
		  STRING_SIZE(str));
  return STk_void;
}

void STk_init_gtk_editable(void)
{
  //  fixed_font = gdk_font_load("-misc-fixed-medium-r-*-*-*-140-*-*-*-*-*-*");
  fixed_font = gdk_font_load("fixed");

  ADD_PRIMITIVE(editable_get_set);
  ADD_PRIMITIVE(entry);
  ADD_PRIMITIVE(spinbox);
  ADD_PRIMITIVE(text);
  ADD_PRIMITIVE(combobox);

  ADD_PRIMITIVE(text_vadj);
  ADD_PRIMITIVE(text_ctrl);
  ADD_PRIMITIVE(text_insert);
}
