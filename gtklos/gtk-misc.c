/*
 * gtk-misc.c	-- Misc GTK widgets
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
 *    Creation date: 25-Aug-2000 15:55 (eg)
 * Last file update: 14-Jun-2002 13:21 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"


static void error_bad_code(SCM obj)
{
  STk_error("bad code ~S", obj);
}


/*===========================================================================*\
 * 
 *		 		Adjustments
 *
\*===========================================================================*/
static void error_bad_adjustment(SCM adj)
{
  STk_error("bad adjustment ~S", adj);
}

DEFINE_PRIMITIVE("%adj-ctrl",adj_ctrl, subr23, (SCM w, SCM who, SCM value))
{
  GtkAdjustment *adj;
  
  if (!WIDGETP(w) || !GTK_IS_ADJUSTMENT(WIDGET_ID(w))) error_bad_adjustment(w);
  
  /* get back the adjustement used with this widget */
  adj = GTK_ADJUSTMENT(WIDGET_ID(w));

  if (value) {
    /* Set a new value */
    gfloat new_value = (gfloat) STk_number2double(value);
    
    if (isnan(new_value)) STk_error("bad value for adjustement: ~S", value);
    switch (AS_LONG(who)) {
      case SCM_LONG(0): adj->lower          = new_value; break;
      case SCM_LONG(1): adj->upper          = new_value; break;
      case SCM_LONG(2): adj->value          = new_value; break;
      case SCM_LONG(3): adj->step_increment = new_value; break;
      case SCM_LONG(4): adj->page_increment = new_value; break;
      case SCM_LONG(5): adj->page_size	    = new_value; break;
      default:          error_bad_code(who);
    }
    /* We change the value, even if value should be changed only for who == 2. 
     * Otherwise redisplays  are not always correct.
     */
    gtk_adjustment_value_changed(adj);
  } else {
    /* Get a value */
    switch (AS_LONG(who)) {
      case SCM_LONG(0): return STk_double2real((double) adj->lower);
      case SCM_LONG(1): return STk_double2real((double) adj->upper);
      case SCM_LONG(2): return STk_double2real((double) adj->value);
      case SCM_LONG(3): return STk_double2real((double) adj->step_increment);
      case SCM_LONG(4): return STk_double2real((double) adj->page_increment);
      case SCM_LONG(5): return STk_double2real((double) adj->page_size);
      default:		error_bad_code(who);
    }
  }
  return STk_void;
}

/*===========================================================================*\
 * 
 *		 		Scrollbar
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%scrollbar", scrollbar, subr3, (SCM obj, SCM horizontal, SCM adj))
{
  GtkWidget *scrollbar;

  //  if (!WIDGETP(adj) || GTK_IS_ADJUSTMENT(WIDGET_ID(adj))) error_bad_adjustment(adj);
  
  scrollbar = (horizontal != STk_false) ? 
    		gtk_hscrollbar_new((GtkAdjustment *) WIDGET_ID(adj)):
    		gtk_vscrollbar_new((GtkAdjustment *) WIDGET_ID(adj));
  
  return WIDGET_TO_SCM(scrollbar, obj);
}



/*===========================================================================*\
 * 
 *		 		Separators
 *
\*===========================================================================*/
SIMPLE_WIDGET("%hsepar", hsepar, (gtk_hseparator_new()));
SIMPLE_WIDGET("%vsepar", vsepar, (gtk_vseparator_new()));


/*===========================================================================*\
 * 
 *		 		Gauge
 *
\*===========================================================================*/

DEFINE_PRIMITIVE("%gauge", gauge, subr1, (SCM obj))
{
  SCM adj, res;
  GtkObject *adjustment;
  GtkWidget *pb;
  
  /* Create a new ajustement */
  adjustment = gtk_adjustment_new(DEF_AJUSTMENT);
  adj 	     = WIDGET_TO_SCM(adjustment, obj);
  
  /* create the progress bar */
  pb  = gtk_progress_bar_new_with_adjustment((GtkAdjustment *) adjustment);
  res = WIDGET_TO_SCM(pb, obj);
  
  /* Keep a reference on the adjustement through the gauge */
  WIDGET_PLIST(res) = LIST2(STk_makekey(":adjustment"), adj);
  
  return res;
}

DEFINE_PRIMITIVE("%gtk-gauge-format", gauge_format, subr2, (SCM w, SCM fmt))
{
  if (!WIDGETP(w))   STk_error_bad_widget(w);
  if (!STRINGP(fmt)) STk_error("bad format ~S", fmt);

  gtk_progress_set_format_string(GTK_PROGRESS(WIDGET_ID(w)), STRING_CHARS(fmt));

  return STk_void;
}

/*===========================================================================*\
 * 
 *		 		Scale
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%scale", scale, subr2, (SCM obj, SCM horizontal))
{
  SCM adj, res;
  GtkObject *adjustment;
  GtkWidget *scale;
  
  /* Create a new ajustement */
  adjustment = gtk_adjustment_new(DEF_AJUSTMENT);
  adj 	     = WIDGET_TO_SCM(adjustment, obj);

  /* create the scale */
  scale = (horizontal != STk_false) ? 
    		gtk_hscale_new((GtkAdjustment *) adjustment):
		gtk_vscale_new((GtkAdjustment *) adjustment);
  res = WIDGET_TO_SCM(scale, obj);
  
  /* Keep a reference on the adjustement through the gauge */
  WIDGET_PLIST(res) = LIST2(STk_makekey(":adjustment"), adj);
  
  return res;
}

/*===========================================================================*\
 * 
 *		 		Color-selector
 *
\*===========================================================================*/
// DEFINE_PRIMITIVE("%colorselector", color_selector, subr1, (SCM obj))
// {
//   gdouble col[4] = {1.0, 1.0, 1.0, 0.5};
//   GtkWidget* w = gtk_color_selection_new();
// 
//   gtk_color_selection_set_opacity((GtkColorSelection *) w, 1);
//   gtk_color_selection_set_color((GtkColorSelection *) w, col);
//   gtk_widget_realize(w);  
//   return STk_gtk_widget2scm(w, obj);
// }

SIMPLE_WIDGET("%colorselector", color_selector, (gtk_color_selection_new()));


static SCM color2scheme(gdouble *col)
{
  SCM res = STk_makevect(4, (SCM) NULL);
  SCM *v;
  
  v = VECTOR_DATA(res);
  v[0] = STk_double2real(col[0]);
  v[1] = STk_double2real(col[1]);
  v[2] = STk_double2real(col[2]);
  v[3] = STk_double2real(col[3]);
  return res;
}

static void scheme2color(SCM vect, gdouble *col)
{
  SCM *v;
  int i;
  double val;

  if (!VECTORP(vect) || !(VECTOR_SIZE(vect) == 4))
    STk_error("bad color specification ~S", vect);
  
  v = VECTOR_DATA(vect);
  for (i =0; i < 4; i++, v++, col++) {
    val = STk_number2double(*v);
    if (isnan(val)) STk_error("bad color component ~S", *v);
    *col = val;
  }
}

DEFINE_PRIMITIVE("%color-ctrl", color_ctrl, subr23, (SCM obj, SCM what, SCM val))
{
  gdouble col[4];

  if (!WIDGETP(obj) || !GTK_IS_COLOR_SELECTION(WIDGET_ID(obj)))
    STk_error_bad_widget(obj);

  switch (AS_LONG(what)) {
    case SCM_LONG(0): 		/* get color */
      gtk_color_selection_get_color((GtkColorSelection*) WIDGET_ID(obj), col);
      return color2scheme(col);
    case SCM_LONG(1):		/* set color */
      if (!val) STk_error("missing color");
      scheme2color(val, col);
      gtk_color_selection_set_color((GtkColorSelection*) WIDGET_ID(obj), col);
      break;
    case SCM_LONG(2):		/* getting opacity */
      return MAKE_BOOLEAN(((GtkColorSelection*) WIDGET_ID(obj))->use_opacity);
    case SCM_LONG(3):		/* setteing opacity */
      if (!val) STk_error("missing opacity");
      gtk_color_selection_set_opacity((GtkColorSelection*) WIDGET_ID(obj), 
				      val != STk_false);
      break;
    default:
     error_bad_code(what);
  }
  return STk_void;
}


/*===========================================================================*\
 * 
 *		 		File-selector
 *
\*===========================================================================*/

/* FileSelector is really a weird thing different from other selectors. 
 * We make a simple binding by default for OK and Cancel Buttons here 
 * and we will test it later in Scheme to do the real action.
 * This bindings sets 0 in the :button key of the widget plist if OK is 
 * pressed and 1 otherwise.
 */
static void action_ok(gpointer data)
{
  WIDGET_PLIST((SCM) data) = 
    STk_key_set(WIDGET_PLIST((SCM) data), STk_makekey(":button"), MAKE_INT(0));
}

static void action_cancel(gpointer data)
{
  WIDGET_PLIST((SCM) data) = 
    STk_key_set(WIDGET_PLIST((SCM) data), STk_makekey(":button"), MAKE_INT(1));
}

DEFINE_PRIMITIVE("%fileselector", file_selector, subr2, (SCM obj, SCM title))
{
  SCM res;
  GtkWidget *sel;

  if (!STRINGP(title))
    STk_error("bad title ~S", title);
  
  sel = gtk_file_selection_new(STRING_CHARS(title));
  res = WIDGET_TO_SCM(sel, obj);

  /* Connect the OK and Cancel buttons to our simple binding */
  gtk_signal_connect_object(GTK_OBJECT(((GtkFileSelection *) sel)->ok_button),
			    "clicked", 
			    GTK_SIGNAL_FUNC(action_ok),
			    (gpointer) res);
  gtk_signal_connect_object(GTK_OBJECT(((GtkFileSelection *) sel)->cancel_button),
  			    "clicked", 
			    GTK_SIGNAL_FUNC(action_cancel),
			    (gpointer) res);
  return res;
}

DEFINE_PRIMITIVE("%file-ctrl", file_ctrl, subr23, (SCM obj, SCM what, SCM val))
{
  if (!WIDGETP(obj) || !GTK_IS_FILE_SELECTION(WIDGET_ID(obj)))
    STk_error_bad_widget(obj);
  
  switch (AS_LONG(what)) {
    case SCM_LONG(0): {		/* get value */
      return STk_Cstring2string(gtk_file_selection_get_filename(
					(GtkFileSelection*) WIDGET_ID(obj)));
    }
    case SCM_LONG(1):		/* set font */
      if (!val) 	 STk_error("missing file");
      if (!STRINGP(val)) STk_error("bad file name ~S", val);
      gtk_file_selection_set_filename((GtkFileSelection*) WIDGET_ID(obj),
				       STRING_CHARS(val));
      break;
    default:
      error_bad_code(what);
  }
  return STk_void;
}


/*===========================================================================*\
 * 
 *		 		Font-selector
 *
\*===========================================================================*/


SIMPLE_WIDGET("%fontselector", font_selector, (gtk_font_selection_new()));

DEFINE_PRIMITIVE("%font-ctrl", font_ctrl, subr23, (SCM obj, SCM what, SCM val))
{
  if (!WIDGETP(obj) || !GTK_IS_FONT_SELECTION(WIDGET_ID(obj)))
    STk_error_bad_widget(obj);
  
  switch (AS_LONG(what)) {
    case SCM_LONG(0): {		/* get font */
      char *s=gtk_font_selection_get_font_name((GtkFontSelection*) WIDGET_ID(obj));
      
      return (s) ? STk_Cstring2string(s) : STk_false;
    }
    case SCM_LONG(1):		/* set font */
      if (!val) 	 STk_error("missing font");
      if (!STRINGP(val)) STk_error("bad font ~S", val);
      gtk_font_selection_set_font_name((GtkFontSelection*) WIDGET_ID(obj), 
				       STRING_CHARS(val));
      break;
    case SCM_LONG(2): {		/* get preview */
      char *s=   gtk_font_selection_get_preview_text(
					(GtkFontSelection*) WIDGET_ID(obj));
      
      return (s) ? STk_Cstring2string(s) : STk_false;
    }
    case SCM_LONG(3):		/* set preview */
      if (!val) 	 STk_error("missing preview text");
      if (!STRINGP(val)) STk_error("bad preview text ~S", val);
      gtk_font_selection_set_preview_text((GtkFontSelection*) WIDGET_ID(obj), 
					  STRING_CHARS(val));
      break;
    default:
      error_bad_code(what);
  }
  return STk_void;
}

/*===========================================================================*\
 * 
 *		 		Init
 *
\*===========================================================================*/
void STk_init_gtk_misc(void)
{
  ADD_PRIMITIVE(adj_ctrl);

  ADD_PRIMITIVE(scrollbar);
  ADD_PRIMITIVE(hsepar);
  ADD_PRIMITIVE(vsepar);
  ADD_PRIMITIVE(gauge);
  ADD_PRIMITIVE(gauge_format);
  ADD_PRIMITIVE(scale);
  ADD_PRIMITIVE(color_selector);
  ADD_PRIMITIVE(color_ctrl);
  ADD_PRIMITIVE(file_selector);
  ADD_PRIMITIVE(file_ctrl);
  ADD_PRIMITIVE(font_selector);
  ADD_PRIMITIVE(font_ctrl);
}
