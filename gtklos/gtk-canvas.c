/*
 * gtk-canvas.c	-- Stuff for managing GTklos canvases
 * 
 * Copyright © 2002-2003 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 17-Feb-2002 10:29 (eg)
 * Last file update: 11-Feb-2003 00:06 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"

#ifdef HAVE_GNOME

static void error_bad_canvas(SCM obj)
{
  STk_error("bad canvas ~S", obj);
}

static void error_bad_item(SCM obj)
{
  STk_error("bad canvas item ~S", obj);
}


static GtkType line_type, polygon_type, rectangle_type, ellipse_type;
static GtkType text_type, image_type, widget_type;



#define GNOME_CANVAS_ID(w)	((GnomeCanvasItem *) WIDGET_ID(w))


/*===========================================================================*\
 * 
 * 			 	C A N V A S
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%canvas", canvas, subr1, (SCM obj))
{
  GtkWidget   *w;
  GnomeCanvas *c;

  w = gnome_canvas_new();
  c = (GnomeCanvas *) w;

  /* Set the canvas size */
  gtk_widget_set_usize(w, 600, 450);
  gnome_canvas_set_scroll_region(c, 0.0, 0.0, 600.0, 450.0);
  gnome_canvas_set_pixels_per_unit(c, 1.0);

  /* Build a group for this canvas */
  /* TO BE DONE */

  return  WIDGET_TO_SCM(w, obj);  
}

/*===========================================================================*\
 * 
 * 			 C A N V A S - I T E M
 *
\*===========================================================================*/
static SCM make_canvas_item(SCM parent, SCM obj, GtkType type)
{
  GnomeCanvas *c;
  GnomeCanvasItem *item;
  
  if (!(WIDGETP(parent) && (GNOME_IS_CANVAS(WIDGET_ID(parent)))))
    error_bad_canvas(parent);

  c = GNOME_CANVAS(WIDGET_ID(parent));

  if (type == line_type || type == polygon_type) {
    char * s;
    /* Lines needs to have a set of points otherwise they do a segfault */
    GnomeCanvasPoints *points;

    points = gnome_canvas_points_new(2);
    points->coords[0] = 0.0; points->coords[1] = 0.0;
    points->coords[2] = 0.0; points->coords[3] = 0.0;

    s = (type == line_type) ? "fill_color" : "outline_color";

    item = gnome_canvas_item_new(gnome_canvas_root(c), 
				 type,
				 "points", 	points,
				 s, 		"black",
				 "width_units",	1.0,
				 NULL);

    gnome_canvas_points_free(points);
  } else if (type == text_type) {
    /* Text */
    item = gnome_canvas_item_new(gnome_canvas_root(c), 
				 type,
				 "fill_color", "red",
				 "font", "fixed",
				 NULL);
  } else if (type == image_type || type == widget_type) {
    item = gnome_canvas_item_new(gnome_canvas_root(c), 
				 type,
				 NULL);
  } else {
    /* Rectangle and Ellipse */
    item = gnome_canvas_item_new(gnome_canvas_root(c), 
				 type, 
				 "outline_color", "black",
				 "width_units", 1.0,
				 NULL);
  }
  return WIDGET_TO_SCM(GTK_OBJECT(item), obj);
}



DEFINE_PRIMITIVE("%canvas-visibility-set!", visibility, subr2,(SCM item, SCM value))
{
  if (!((WIDGETP(item) && GNOME_IS_CANVAS_ITEM(WIDGET_ID(item)))))
    error_bad_item(item);
  
  if (value == STk_false)
    gnome_canvas_item_hide(GNOME_CANVAS_ID(item));
  else
    gnome_canvas_item_show(GNOME_CANVAS_ID(item));
  return STk_void;
}

DEFINE_PRIMITIVE("%canvas-bb", bb, subr1,(SCM item))
{
  double x1, y1, x2, y2;

  if (!((WIDGETP(item) && GNOME_IS_CANVAS_ITEM(WIDGET_ID(item)))))
    error_bad_item(item);

  gnome_canvas_item_get_bounds(GNOME_CANVAS_ID(item), &x1, &y1, &x2, &y2);
  return LIST4(STk_double2real(x1), STk_double2real(y1),
	       STk_double2real(x2), STk_double2real(y2));
}

/*===========================================================================*\
 * 
 * 			 	R E C T A N G L E
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%make-rectangle", m_rect, subr2, (SCM parent, SCM obj))
{
  return make_canvas_item(parent, obj, rectangle_type);
}

/*===========================================================================*\
 * 
 * 			 	E L L I P S E
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%make-ellipse", m_ellipse, subr2, (SCM parent, SCM obj))
{
  return make_canvas_item(parent, obj, ellipse_type);
}

/*===========================================================================*\
 * 
 * 			 	L I N E
 *
\*===========================================================================*/
#define LINE_OR_POLY(item) (WIDGETP(item) && 				     \
                              (GNOME_IS_CANVAS_LINE(WIDGET_ID(item)) ||      \
			       GNOME_IS_CANVAS_POLYGON(WIDGET_ID(item))))

DEFINE_PRIMITIVE("%canvas-points", c_points, subr12, (SCM item, SCM lst))
{
  GtkArg arg;
  GnomeCanvasPoints *points;
  int i;

  if (!LINE_OR_POLY(item)) error_bad_item(item);

  arg.name = "points";
  arg.type = (GtkType) GTK_TYPE_GNOME_CANVAS_POINTS;
  
  if (lst) {					    /* Set! the points */
    points = gnome_canvas_points_new(STk_int_length(lst) / 2);
    
    i = 0;
    for(   ; CONSP(lst); lst = CDR(CDR(lst))) {
      points->coords[i++] = STk_number2double(CAR(lst));
      points->coords[i++] = STk_number2double(CAR(CDR(lst)));
    }

    /* Assign points using a GtkArg */
    arg.name = "points";
    arg.type = (GtkType) GTK_TYPE_GNOME_CANVAS_POINTS;
    GTK_VALUE_POINTER(arg) = (GtkObject *) points;
    gtk_object_arg_set((GtkObject *) WIDGET_ID(item), &arg, NULL);
    
    /* Free the list of points */
    gnome_canvas_points_free(points);
    
    return STk_void;
  } else {
    /* Get the points */
    gtk_object_arg_get((GtkObject *) WIDGET_ID(item), &arg, NULL);
    points = GTK_VALUE_POINTER(arg);

    lst = STk_nil;
    for(i = 2 * (points->num_points - 1); i >=0; i -= 2 ) {
      lst = STk_cons(STk_double2real(points->coords[i+1]), lst);
      lst = STk_cons(STk_double2real(points->coords[i]), lst);
    }
    
    /* Free the list of points */
    gnome_canvas_points_free(points);

    return lst;
  }
}

DEFINE_PRIMITIVE("%canvas-line-width!", c_line_width, subr2, (SCM item, SCM val))
{
  double value = STk_number2double(val);
  GtkArg arg;

  if (!LINE_OR_POLY(item)) error_bad_item(item);

  if (isnan(value)) STk_error("bad number ~S", val);

  arg.name = "width_units";
  arg.type = (GtkType) GTK_TYPE_DOUBLE;
  GTK_VALUE_DOUBLE(arg) = value;
  gtk_object_arg_set((GtkObject *) WIDGET_ID(item), &arg, NULL);

  return STk_void;
}


DEFINE_PRIMITIVE("%make-line", m_line, subr2, (SCM parent, SCM obj))
{
  return make_canvas_item(parent, obj, line_type);
}

/*===========================================================================*\
 * 
 * 			 	POLYGON
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%make-polygon", m_polygon, subr2, (SCM parent, SCM obj))
{
  return make_canvas_item(parent, obj, polygon_type);
}


/*===========================================================================*\
 * 
 * 			 	TEXT
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%make-text", m_text, subr2, (SCM parent, SCM obj))
{
  return make_canvas_item(parent, obj, text_type);
}


/*===========================================================================*\
 * 
 * 			 	IMAGE
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%make-canvas-image", m_image, subr2, (SCM parent, SCM obj))
{
  return make_canvas_item(parent, obj, image_type);
}

DEFINE_PRIMITIVE("%canvas-image-set!", canv_img_set, subr2, (SCM item, SCM image))
{
  if (!(WIDGETP(item) && GNOME_IS_CANVAS_IMAGE(WIDGET_ID(item))))
    error_bad_item(item);

  /* FIXME: STk_debug("On affecte l'image ~S à ~S", image, item); */
  return STk_void;
}

/*===========================================================================*\
 * 
 * 			 	WIDGET
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%make-canvas-widget", m_widget, subr2, (SCM parent, SCM obj))
{
  return make_canvas_item(parent, obj, widget_type);
}



/*===========================================================================*/
void STk_init_gtk_canvas(void)
{
  line_type      = gnome_canvas_line_get_type();
  polygon_type	 = gnome_canvas_polygon_get_type();
  rectangle_type = gnome_canvas_rect_get_type();
  ellipse_type	 = gnome_canvas_ellipse_get_type();
  text_type	 = gnome_canvas_text_get_type();
  image_type	 = gnome_canvas_image_get_type();
  widget_type    = gnome_canvas_widget_get_type();

  ADD_PRIMITIVE(canvas);
  ADD_PRIMITIVE(m_rect);
  ADD_PRIMITIVE(m_ellipse);
  ADD_PRIMITIVE(m_line);
  ADD_PRIMITIVE(m_polygon);
  ADD_PRIMITIVE(m_text);
  ADD_PRIMITIVE(m_image);
  ADD_PRIMITIVE(m_widget);

  ADD_PRIMITIVE(visibility);
  ADD_PRIMITIVE(bb);
  ADD_PRIMITIVE(c_points);
  ADD_PRIMITIVE(c_line_width);
  ADD_PRIMITIVE(canv_img_set);
}

#else
/* Some compilers cannot produces empty files. So ... */
static char x = ' ';
#endif
