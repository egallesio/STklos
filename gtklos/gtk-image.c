/*
 * gtk-image.c	-- Images Management for GTklos
 * 
 * Copyright © 2002 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 14-Feb-2002 00:00 (eg)
 * Last file update:  5-Apr-2002 17:07 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"




#ifdef HAVE_GNOME
/* Code stolen in Biglook */

static GtkWidget *gtk_pixbuf_to_gdk_pixmap(GdkPixbuf *pixbuf) 
{
  GtkWidget *pixmapwid;
  GdkPixmap *pixmap;
  GdkBitmap *mask;
    
  gdk_pixbuf_render_pixmap_and_mask(pixbuf, &pixmap, &mask, 1);
  pixmapwid = gtk_pixmap_new(pixmap, mask);
  gtk_object_set_data((GtkObject *) pixmapwid, "PIXBUF", pixbuf);

  gtk_object_ref((GtkObject *) pixmapwid);
  gtk_widget_show(pixmapwid);
  
  return pixmapwid;
}

#endif

static gchar **scm_string2argv(char *buffer) 
{
  char **line = NULL;
  char *runner = buffer;
  int i, size = 0, buflen = 0;
  char *tmp;
  
  /* Compute the image size */
  while (*runner) {
    buflen++;
    if (*runner == '"') size++;
    runner++;
  }
  size >>= 1;

  tmp = (char *) STk_must_malloc(buflen + 1);
  memcpy(tmp, buffer, buflen);
   
  /* Allocate the buffer */
  line = (char **) STk_must_malloc(sizeof(char *) * (2 + size));
  line[0]        = tmp;
  line[size + 1] = NULL;

  /* we start collecting the strings */
  runner = (char *) strtok(tmp, "\"");

  i = 1;
  while ((line[i++] = (char *) strtok(NULL, "\""))) {
    strtok( 0L, "\"" );
  }
  
  return line + 1;
}


static GtkWidget *fake_toplevel;

static gchar **scm_list2argv(SCM l)
{
  SCM tmp;
  gchar **res, **p;
  int len = STk_int_length(l);

  if (len < 0) 
    STk_error("bad list ~S", l);

  p = res = STk_must_malloc((len+1) * sizeof(gchar*));

  for (tmp = l; !NULLP(tmp); tmp = CDR(tmp)) {
    if (!STRINGP(CAR(tmp)))
      STk_error("bad data string for image ~S", CAR(tmp));
    *p++ = STRING_CHARS(CAR(tmp));
  }
  *p = NULL;
  return res;
}
  
/*===========================================================================*\
 * 
 *		 		gnome-available?
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("gnome-available?", gnome_available, subr0, (void))
{
#ifdef HAVE_GNOME
  return STk_true;
#else 
  return STk_false;
#endif
}


/*===========================================================================*\
 * 
 *		 		image-file
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%image-file", image_file, subr2, (SCM self, SCM filename))
{
  if (!STRINGP(filename))
    STk_error("bad file name for image ~S", filename);

  /* create the image */
#ifdef HAVE_GNOME
  {							/* Gnome version */
    GdkPixbuf *pixbuf;

    pixbuf = gdk_pixbuf_new_from_file(STRING_CHARS(filename));
    return WIDGET_TO_SCM(gtk_pixbuf_to_gdk_pixmap(pixbuf), self);
  }
#else
  {							/* GTK+ version */
    GdkPixmap *pixmap, *mask;
    GtkStyle  style;
    
    pixmap = gdk_pixmap_create_from_xpm(fake_toplevel->window,
					&mask,
					&style.bg[GTK_STATE_NORMAL],
					STRING_CHARS(filename));
    if (!pixmap) STk_error("cannot load image in file ~A", filename);
    
    return WIDGET_TO_SCM(gtk_pixmap_new(pixmap, mask), self);
  }
#endif
}

/*===========================================================================*\
 * 
 *		 		image-data
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%image-data", image_data, subr2, (SCM self, SCM data))
{
  GdkPixmap *pixmap, *mask;
  GtkStyle  style;
  gchar **argv = NULL;

  if (STRINGP(data)) {
    argv = scm_string2argv(STRING_CHARS(data));
  } else if (CONSP(data)) {
    argv = scm_list2argv(data);
  } else {
    STk_error("bad image data. It must be a string or a list");
  }
  
  /* create the image */
  pixmap = gdk_pixmap_create_from_xpm_d(fake_toplevel->window,
					&mask,
					&style.bg[GTK_STATE_NORMAL],
					argv);
  if (!pixmap) STk_error("cannot load from given data");
  
  return WIDGET_TO_SCM(gtk_pixmap_new(pixmap, mask), self);
}


/*===========================================================================*\
 * 
 *		 		initialization
 *
\*===========================================================================*/

void STk_init_gtk_image(void)
{
  /* When image pixmaps are read in a window which is not realized,
   * GTK issue a warning. To avoid this we create such images in a
   * fake window which is only accessible in this file
   */
  fake_toplevel = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_widget_realize(fake_toplevel);

  ADD_PRIMITIVE(image_file);
  ADD_PRIMITIVE(image_data);
  ADD_PRIMITIVE(gnome_available);
}
