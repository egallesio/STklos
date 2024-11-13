/*
 * gtklos.c              -- Various GTk+ wrappers for GTklos
 *
 * Copyright © 2007-2024 Erick Gallesio <eg@stklos.net>
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
 *           Author: Erick Gallesio [eg@essi.fr]
 *    Creation date: 11-Aug-2007 11:38 (eg)
 */


#include <math.h>               /* for isnan */
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>     /* For the keysyms macros */
#include <stklos.h>
#include "gtklos-config.h"

#include "gtklos-incl.c"

/* ======================================================================
 *
 *      stklos-gtklos Globals & Utilities
 *
 * ======================================================================
 */
static void error_set_property(SCM prop, SCM val, char *s)
{
  STk_error("Uncorrect type for property ~S (%s expected). Value was ~s",
            prop, s, val);
}

//XXX static void error_bad_widget(SCM obj)
//XXX {
//XXX   STk_error("bad widget ~S", obj);
//XXX }

static void error_bad_event(SCM obj)
{
  STk_error("bad event ~S", obj);
}

static void error_bad_integer(SCM obj)
{
  STk_error("bad integer ~S", obj);
}


/* ======================================================================
 *
 *      GValues ...
 *
 * ======================================================================
 */
static void init_gvalue(SCM object, SCM prop, GValue *value)
{
  GParamSpec *spec;

  spec = g_object_class_find_property(
                         G_OBJECT_GET_CLASS(G_OBJECT(CPOINTER_VALUE(object))),
                         STRING_CHARS(prop));
  if (!spec)
    STk_error("Object ~S doesn't have the property ~S", object, prop);
  g_value_init(value, spec->value_type);
}

static void init_gvalue_for_child(SCM container, SCM prop, GValue *value)
{
  GParamSpec* spec;

  spec = gtk_container_class_find_child_property(
                         G_OBJECT_GET_CLASS(CPOINTER_VALUE(container)),
                         STRING_CHARS(prop));
  if (!spec)
    STk_error("container ~S doesn't have the property ~S", container, prop);

  g_value_init(value, G_PARAM_SPEC_VALUE_TYPE(spec));
}


static SCM GValue2Scheme(GValue *value, SCM prop)
{
  SCM res = STk_void;

  switch (G_VALUE_TYPE(value)) {
    case G_TYPE_NONE:
      res = STk_void; break;
    case G_TYPE_CHAR:
      res = MAKE_CHARACTER((unsigned char) g_value_get_schar(value)); break;
    case G_TYPE_UCHAR:
      res = MAKE_CHARACTER((unsigned char) g_value_get_uchar(value)); break;
    case G_TYPE_BOOLEAN:
      res = MAKE_BOOLEAN(g_value_get_boolean(value)); break;
    case G_TYPE_INT:
      res = STk_long2integer((long) g_value_get_int(value)); break;
    case G_TYPE_UINT:
      res = STk_ulong2integer((long) g_value_get_uint(value));; break;
    case G_TYPE_LONG:
      res = STk_long2integer((long) g_value_get_long(value)); break;
    case G_TYPE_ULONG:
      res = STk_ulong2integer((long) g_value_get_ulong(value)); break;
    case G_TYPE_ENUM:
      res = STk_long2integer((long) g_value_get_enum(value));; break;
    case G_TYPE_FLOAT:
      res = STk_double2real((double) g_value_get_float(value)); break;
    case G_TYPE_DOUBLE:
      res = STk_double2real((double) g_value_get_double(value)); break;
    case G_TYPE_STRING: {
      char *s = (char *) g_value_get_string(value);

      res = STk_Cstring2string( (s) ? s : "");
      break;
    }
    case G_TYPE_INVALID:
    case G_TYPE_INTERFACE:
    case G_TYPE_INT64:
    case G_TYPE_UINT64:
    case G_TYPE_FLAGS:
    case G_TYPE_POINTER:
    case G_TYPE_BOXED:
    case G_TYPE_PARAM:
    case G_TYPE_OBJECT:
      STk_error("cannot convert property ~S to Scheme (%d)",
                prop, G_VALUE_TYPE(value));
      break; // for the compiler
    default:
      {
        /* Try to value objet to an int */
        GValue cast = {G_TYPE_INVALID};
        int n;

        g_value_init(&cast, G_TYPE_INT);

        n = g_value_transform(value, &cast);
        if (n) {
          /* STk_debug("Transformation possible"); */
          res = STk_long2integer((long) g_value_get_int(&cast));
          break;
        }
      }
      STk_error("unknown GObject type: %d",  G_VALUE_TYPE(value));
  }
  return res;
}

static void assign_GValue(GValue *value, SCM prop, SCM val)
{
  /*
  STk_debug("Setting property ~S to ~S (type = %s)\n",
     prop, val, G_VALUE_TYPE_NAME(value));
  */
  switch (G_VALUE_TYPE(value)) {
    case G_TYPE_NONE:
      break;
    case G_TYPE_CHAR:
      if (CHARACTERP(val))
        g_value_set_schar(value, (char) CHARACTER_VAL(val));
      else
        error_set_property(prop, val, "character");
      break;
    case G_TYPE_UCHAR:
      if (CHARACTERP(val))
        g_value_set_uchar(value, (unsigned char) CHARACTER_VAL(val));
      else
        error_set_property(prop, val, "character");
      break;
    case G_TYPE_BOOLEAN:
      g_value_set_boolean(value, val != STk_false);
      break;
    case G_TYPE_INT: {
      long v = STk_integer_value(val);

      if (v != LONG_MIN)
        g_value_set_int(value, (int) v);
      else
        error_set_property(prop, val, "integer");
      break;
    }
    case G_TYPE_UINT: {
      unsigned long v = STk_uinteger_value(val);

      if (v != ULONG_MAX)
        g_value_set_uint(value, (unsigned int) v);
      else
        error_set_property(prop, val, "unsigned integer");
      break;
    }
    case G_TYPE_LONG: {
      long v = STk_integer_value(val);

      if (v != LONG_MIN)
        g_value_set_long(value, (int) v);
      else
        error_set_property(prop, val, "long integer");
      break;
    }
    case G_TYPE_ULONG: {
      unsigned long v = STk_uinteger_value(val);

      if (v != ULONG_MAX)
        g_value_set_ulong(value, (unsigned long) v);
      else
        error_set_property(prop, val, "unsigned long integer");
      break;
    }
    case G_TYPE_ENUM: {
      long v = STk_integer_value(val);

      if (v != LONG_MIN)
        g_value_set_enum(value, (int) v);
      else
        error_set_property(prop, val, "enumeration");
      break;
    }
    case G_TYPE_FLOAT: {
      double d = STk_number2double(val);

      if (!isnan(d))
        g_value_set_float(value, (float) d);
      else
        error_set_property(prop, val, "real");
      break;
    }
    case G_TYPE_DOUBLE: {
      double d = STk_number2double(val);

      if (!isnan(d))
        g_value_set_double(value, d);
      else
        error_set_property(prop, val, "real");
      break;
    }
    case G_TYPE_STRING:
      if (STRINGP(val))
        g_value_set_string(value, STRING_CHARS(val));
      else
        error_set_property(prop, val, "string");
      break;

    case G_TYPE_INVALID:
    case G_TYPE_INTERFACE:
    case G_TYPE_INT64:
    case G_TYPE_UINT64:
    case G_TYPE_FLAGS:
    case G_TYPE_POINTER:
    case G_TYPE_BOXED:
    case G_TYPE_PARAM:
    case G_TYPE_OBJECT:
      STk_error("cannot convert Scheme object ~S for property ~S", val, prop);
      break; // for the compiler
    default:
      {
        /* Try to value objet to an int */
        GValue cast = {G_TYPE_INVALID};
        long v = STk_integer_value(val);
        int n;

        if (v != LONG_MIN) {
          g_value_init(&cast, G_TYPE_INT);
          g_value_set_int(&cast, (int) v);

          n = g_value_transform(&cast, value);
          if (!n) {
            STk_error("cannot transform value ~S in type %s for property ~S",
                      val, G_VALUE_TYPE_NAME(value), prop);
          }
          break;
        }
      }
      STk_error("unknown GObject type: %d",  G_VALUE_TYPE(value));
  }
}

/* ======================================================================
 *
 *      stklos-gtklos Primitives
 *
 * ======================================================================
 */


/* ----------------------------------------------------------------------
 *      %gtk-get-property ...
 * ---------------------------------------------------------------------- */
DEFINE_PRIMITIVE("%gtk-get-property", gtk_get_prop, subr2,(SCM object, SCM prop))
{
  GValue value = {G_TYPE_INVALID};
  SCM res = STk_void;

  if (! CPOINTERP(object)) STk_error("bad object ~S" ,object);
  if (! STRINGP(prop))     STk_error("bad property name ~S", prop);

  init_gvalue(object, prop, &value);

  /* the Gvalue is initialized with correct type . Fill it now */
  g_object_get_property(G_OBJECT(CPOINTER_VALUE(object)),
                        STRING_CHARS(prop),
                        &value);

  res = GValue2Scheme(&value, prop);
  /* Release ressources associated to value */
  g_value_unset(&value);
  return res;
}



/* ----------------------------------------------------------------------
 *      %gtk-set-property! ...
 * ---------------------------------------------------------------------- */
DEFINE_PRIMITIVE("%gtk-set-property!", gtk_set_prop, subr3,
                 (SCM object, SCM prop, SCM val))
{
  GValue value = {G_TYPE_INVALID};

  if (! CPOINTERP(object)) STk_error("bad object ~S" ,object);
  if (! STRINGP(prop))     STk_error("bad property name ~S", prop);

  init_gvalue(object, prop, &value);

  assign_GValue(&value, prop, val);
  g_object_set_property(G_OBJECT(CPOINTER_VALUE(object)),
                        STRING_CHARS(prop),
                        &value);

  /* Release ressources associated to value */
  g_value_unset(&value);
  return STk_void;
}

//XXX/* ----------------------------------------------------------------------
//XXX *      %gtk-get-width/height ...
//XXX * ---------------------------------------------------------------------- */
//XXXDEFINE_PRIMITIVE("%gtk-get-width", gtk_get_width, subr1, (SCM obj))
//XXX{
//XXX  GdkWindow *win;
//XXX
//XXX  if (!CPOINTERP(obj)) error_bad_widget(obj);
//XXX
//XXX  win = gtk_widget_get_window(GTK_WIDGET(CPOINTER_VALUE(obj)));
//XXX
//XXX  return MAKE_INT(gdk_window_get_width(win));
//XXX}
//XXX
//XXX
//XXXDEFINE_PRIMITIVE("%gtk-get-height", gtk_get_height, subr1, (SCM obj))
//XXX{
//XXX  GdkWindow *win;
//XXX
//XXX  if (!CPOINTERP(obj)) error_bad_widget(obj);
//XXX
//XXX  win = gtk_widget_get_window(GTK_WIDGET(CPOINTER_VALUE(obj)));
//XXX
//XXX  return MAKE_INT(gdk_window_get_height(win));
//XXX}


/* ----------------------------------------------------------------------
 *
 *      Containers  ...
 *
 * ---------------------------------------------------------------------- */
DEFINE_PRIMITIVE("%gtk-get-child-property", gtk_get_child_prop, subr3,
                 (SCM container, SCM object, SCM prop))
{
  GValue value = {G_TYPE_INVALID};
  SCM res = STk_void;

  if (! CPOINTERP(container)) STk_error("bad container ~S" , container);
  if (! CPOINTERP(object))    STk_error("bad object ~S" , object);
  if (! STRINGP(prop))        STk_error("bad property name ~S", prop);

  init_gvalue_for_child(container, prop, &value);

  /* the Gvalue is initialized with correct type . Fill it now */
  gtk_container_child_get_property(GTK_CONTAINER(CPOINTER_VALUE(container)),
                                   GTK_WIDGET(CPOINTER_VALUE(object)),
                                   STRING_CHARS(prop),
                                   &value);
  res = GValue2Scheme(&value, prop);
  g_value_unset(&value);
  return res;
}



//FIXME: /*
//FIXME:  * FIXME: This function is not exported. Should it be? Delete it?
//FIXME:  *
//FIXME:  */
//FIXME: DEFINE_PRIMITIVE("gtk-box-set-child-packing", box_set_packing, subr3,
//FIXME:                  (SCM box, SCM child, SCM value))
//FIXME: {
//FIXME:   long pad;
//FIXME:
//FIXME:
//FIXME:   if (!CPOINTERP(box)) error_bad_widget(box);
//FIXME:   if (!CPOINTERP(child)) error_bad_widget(child);
//FIXME:
//FIXME:   if (STk_int_length(value) != 4) STk_error("bad value ~S", value);
//FIXME:   pad = STk_integer_value(CAR(CDR(CDR(value))));
//FIXME:   if (pad == LONG_MIN) STk_error("bad padding value ~S", CAR(CDR(CDR(value))));
//FIXME:
//FIXME:   gtk_box_set_child_packing(GTK_BOX(CPOINTER_VALUE(box)),
//FIXME:                             GTK_WIDGET(CPOINTER_VALUE(child)),
//FIXME:                             (gboolean) 0, //(CAR(value) != STk_false),
//FIXME:                             (gboolean) ~0, // (CAR(CDR(value)) != STk_false),
//FIXME:                             (guint) pad,
//FIXME:                             ((CAR(CDR((CDR(CDR(value))))) == STk_true)?
//FIXME:                              GTK_PACK_START: GTK_PACK_END));
//FIXME:   return STk_void;
//FIXME: }
//FIXME:
//FIXME: /*
//FIXME:  * Children of a container computation
//FIXME:  */
//FIXME: static void cont_children_helper(gpointer p, gpointer data)
//FIXME: {
//FIXME:   SCM *l = (SCM*) data;
//FIXME:
//FIXME:   *l = STk_cons(STk_make_Cpointer(p, STk_void, STk_false), *l);
//FIXME: }
//FIXME:
//FIXME:
//FIXME:
//FIXME: DEFINE_PRIMITIVE("%container-children", cont_children, subr1, (SCM w))
//FIXME: {
//FIXME:   GList *gl;
//FIXME:   SCM l = STk_nil;
//FIXME:
//FIXME:   if (!CPOINTERP(w)) error_bad_widget(w);
//FIXME:
//FIXME:   gl = gtk_container_get_children(GTK_CONTAINER(CPOINTER_VALUE(w)));
//FIXME:   g_list_foreach(gl, cont_children_helper, &l);
//FIXME:   g_list_free(gl);
//FIXME:
//FIXME:   return l;
//FIXME: }

/* ----------------------------------------------------------------------
 *
 *      Events  ...
 *
 * ---------------------------------------------------------------------- */

static int tc_gtk_event;

struct gtk_event_obj {
  stk_header header;
  GdkEvent *ev;
};

#define GTK_EVENTP(p)         (BOXED_TYPE_EQ((p), tc_gtk_event))
#define GTK_EVENT_VALUE(p)    (((struct gtk_event_obj *) (p))->ev)

static struct extended_type_descr xtype_gtk_event = {
  .name  = "GTK-event",
};


static void gtk_event_finalizer(struct gtk_event_obj *o, void _UNUSED(*client_data))
{
  gdk_event_free(GTK_EVENT_VALUE(o));
}


DEFINE_PRIMITIVE("get-gtk-event", get_gtk_event, subr0, (void)) {
  GdkEvent *ev= gtk_get_current_event();

  if (!ev)
    return STk_false;
  else {
    SCM z;
    NEWCELL(z, gtk_event);
    GTK_EVENT_VALUE(z) = ev;
    STk_register_finalizer(z, gtk_event_finalizer);
    return z;
  }
}



DEFINE_PRIMITIVE("event-type", event_type, subr1, (SCM event))
{
  GdkEvent *ev;

  if (!GTK_EVENTP(event)) error_bad_event(event);
  ev = GTK_EVENT_VALUE(event);

  switch (((GdkEventAny *) ev)->type) {
    case GDK_NOTHING           : return STk_intern("NOTHING");
    case GDK_DELETE            : return STk_intern("DELETE");
    case GDK_DESTROY           : return STk_intern("DESTROY");
    case GDK_EXPOSE            : return STk_intern("EXPOSE");
    case GDK_MOTION_NOTIFY     : return STk_intern("MOTION");
    case GDK_BUTTON_PRESS      :
    case GDK_2BUTTON_PRESS     :
    case GDK_3BUTTON_PRESS     : return STk_intern("BUTTON-PRESS");
    case GDK_BUTTON_RELEASE    : return STk_intern("BUTTON-RELEASE");
    case GDK_KEY_PRESS         : return STk_intern("KEY-PRESS");
    case GDK_KEY_RELEASE       : return STk_intern("KEY-RELEASE");
    case GDK_ENTER_NOTIFY      : return STk_intern("ENTER");
    case GDK_LEAVE_NOTIFY      : return STk_intern("LEAVE");
    case GDK_FOCUS_CHANGE      : return STk_intern(((GdkEventFocus *) ev)->in ?
                                                      "FOCUS-IN" :
                                                      "FOCUS-OUT");
    case GDK_CONFIGURE         : return STk_intern("CONFIGURE");
    case GDK_MAP               : return STk_intern("MAP");
    case GDK_UNMAP             : return STk_intern("UNMAP");
    case GDK_PROPERTY_NOTIFY   : return STk_intern("PROPERTY");
    case GDK_SELECTION_CLEAR   : return STk_intern("SELECTION-CLEAR");
    case GDK_SELECTION_REQUEST : return STk_intern("SELECTION-REQUEST");
    case GDK_SELECTION_NOTIFY  : return STk_intern("SELECTION-NOTIFY");
    case GDK_PROXIMITY_IN      : return STk_intern("PROXIMITY-IN");
    case GDK_PROXIMITY_OUT     : return STk_intern("PROXIMITY-OUT");
    case GDK_DRAG_ENTER        : return STk_intern("DRAG-ENTER");
    case GDK_DRAG_LEAVE        : return STk_intern("DRAG-LEAVE");
    case GDK_DRAG_MOTION       : return STk_intern("DRAG-MOTION");
    case GDK_DRAG_STATUS       : return STk_intern("DRAG-STATUS");
    case GDK_DROP_START        : return STk_intern("DROP-START");
    case GDK_DROP_FINISHED     : return STk_intern("DROP-FINISHED");
    case GDK_CLIENT_EVENT      : return STk_intern("CLIENT-EVENT");
    case GDK_VISIBILITY_NOTIFY : return STk_intern("VISIBILITY");
    default:                     return STk_void;
  }
}


DEFINE_PRIMITIVE("event-x", event_x, subr1, (SCM event))
{
  GdkEvent *ev;
  int res;
  gdouble x;

  if (!GTK_EVENTP(event)) error_bad_event(event);
  ev = GTK_EVENT_VALUE(event);

  res = gdk_event_get_coords(ev, &x, NULL);
  return res ? STk_double2real(x): STk_false;
}


DEFINE_PRIMITIVE("event-y", event_y, subr1, (SCM event))
{
  GdkEvent *ev;
  int res;
  gdouble y;

  if (!GTK_EVENTP(event)) error_bad_event(event);
  ev = GTK_EVENT_VALUE(event);

  res = gdk_event_get_coords(ev, NULL, &y);
  return res ? STk_double2real(y): STk_false;
}


DEFINE_PRIMITIVE("event-char", event_char, subr1, (SCM event))
{
  GdkEvent *ev;
  guint keyval;

  if (!GTK_EVENTP(event)) error_bad_event(event);
  ev = GTK_EVENT_VALUE(event);

  switch (((GdkEventAny *) ev)->type) {
  case GDK_KEY_PRESS:
    keyval = ((GdkEventKey *)ev)->keyval;
    switch (keyval) {                  // Names are in <gdk/gdkkeysyms.h>
      case GDK_KEY_Return: return MAKE_CHARACTER('\n');
      case GDK_KEY_Tab:    return MAKE_CHARACTER('\t');
      default:             // keyval = (keyval < 0xff) ? keyval: 0;
                           return MAKE_CHARACTER(keyval);
    }
  default:
    return MAKE_CHARACTER(0);
  }
}


DEFINE_PRIMITIVE("event-keyval", event_keyval, subr1, (SCM event))
{
  GdkEvent *ev;
  guint16 keyval;
  gboolean res;

  if (!GTK_EVENTP(event)) error_bad_event(event);
  ev = GTK_EVENT_VALUE(event);

  res = gdk_event_get_keycode(ev, &keyval);
  return res ? MAKE_INT(keyval): STk_false;
}


DEFINE_PRIMITIVE("event-modifiers", event_modifiers,  subr1, (SCM event))
{
  GdkEvent *ev;
  SCM res = STk_nil;
  int modifiers;

  if (!GTK_EVENTP(event)) error_bad_event(event);
  ev = GTK_EVENT_VALUE(event);

  switch (((GdkEventAny *) ev)->type) {
      case GDK_ENTER_NOTIFY:
      case GDK_LEAVE_NOTIFY:
        modifiers = ((GdkEventCrossing *)ev)->state;
        break;
      case GDK_BUTTON_PRESS:
      case GDK_2BUTTON_PRESS:
      case GDK_3BUTTON_PRESS:
      case GDK_BUTTON_RELEASE:
         modifiers = ((GdkEventButton *) ev)->state;
         break;
      case GDK_MOTION_NOTIFY:
         modifiers = ((GdkEventMotion *) ev)->state;
         break;
      case GDK_KEY_PRESS:
         modifiers = ((GdkEventKey *) ev)->state;
         break;
      default:
         modifiers = 0;
   }

   if (modifiers & GDK_SHIFT_MASK)
      res = STk_cons(STk_intern("shift"), res);

   if (modifiers & GDK_LOCK_MASK)
      res = STk_cons(STk_intern("lock"), res);

   if (modifiers & GDK_CONTROL_MASK)
      res = STk_cons(STk_intern("control"), res);

   if (modifiers & GDK_MOD1_MASK)
      res = STk_cons(STk_intern("mod1"), res);

   if (modifiers & GDK_MOD2_MASK)
      res = STk_cons(STk_intern("mod2"), res);

   if (modifiers & GDK_MOD3_MASK)
      res = STk_cons(STk_intern("mod3"), res);

   if (modifiers & GDK_MOD4_MASK)
      res = STk_cons(STk_intern("mod4"), res);

   if (modifiers & GDK_MOD5_MASK)
      res = STk_cons(STk_intern("mod5"), res);

   return res;
}

DEFINE_PRIMITIVE("event-button", event_button, subr1, (SCM event))
{
  GdkEvent *ev;

  if (!GTK_EVENTP(event)) error_bad_event(event);
  ev = GTK_EVENT_VALUE(event);

  switch (((GdkEventAny *)ev)->type) {
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE: return MAKE_INT(((GdkEventButton *)ev)->button);
    case GDK_MOTION_NOTIFY: {
         int state = ((GdkEventMotion *)ev)->state;
         if( state & GDK_BUTTON1_MASK )
            return MAKE_INT(1);
         if( state & GDK_BUTTON2_MASK )
            return MAKE_INT(2);
         if( state & GDK_BUTTON3_MASK )
            return MAKE_INT(3);
         return MAKE_INT(0);
      }
    default: return MAKE_INT(0);
  }
}

/* ----------------------------------------------------------------------
 *
 *      Colors  ...
 *
 * ---------------------------------------------------------------------- */
DEFINE_PRIMITIVE("%string->color", string2color, subr1, (SCM str))
{
  GdkRGBA *color;

  if (!STRINGP(str)) STk_error("bad string ~S", str);

  color = STk_must_malloc_atomic(sizeof(GdkRGBA));
  if (!gdk_rgba_parse(color, STRING_CHARS(str)))
    STk_error("color ~S specification cannot be parsed ", str);
  return STk_make_Cpointer(color, STk_void, STk_false);
}


DEFINE_PRIMITIVE("%color->string", color2string, subr1, (SCM obj))
{
  if (!CPOINTERP(obj)) STk_error("bad color description ~S", obj);

  return STk_Cstring2string(gdk_rgba_to_string(CPOINTER_VALUE(obj)));
}

/* ----------------------------------------------------------------------
 *
 *      File chooser files   ...
 *
 * ---------------------------------------------------------------------- */
static void fcs_helper(gpointer p, gpointer data)
{
  SCM *l = (SCM*) data;

  *l = STk_cons(STk_Cstring2string((char*) p), *l);
  g_free(p);
}

DEFINE_PRIMITIVE("%file-chooser-files", file_chooser_files, subr1, (SCM obj))
{
  GtkFileChooser* choose;
  GSList *lst;
  SCM res = STk_nil;

  if (!CPOINTERP(obj)) STk_error("bad file chooser ~S", obj);

  choose = CPOINTER_VALUE(obj);
  lst = gtk_file_chooser_get_filenames(choose);
  g_slist_foreach(lst, fcs_helper, &res);
  g_slist_free(lst);

  return res;
}



/* ----------------------------------------------------------------------
 *
 * Interactive GTK support (when using readline/libedit)
 *
 * ---------------------------------------------------------------------- */
// readline provides a pointer variable (rl_event_hook) which permits to call
// the function it contains while there is no character ready to read.

static void stklos_interactive_gtk_loop(void) {
  if (gtk_events_pending())
    gtk_main_iteration_do(1);
}


DEFINE_PRIMITIVE("%flush-gtk-events", flush_gtk_event, subr0, (void))
{
  while (gtk_events_pending())
    gtk_main_iteration_do(0);
  return STk_void;
}

DEFINE_PRIMITIVE("%readline-idle-hook", rl_hook, subr1, (SCM ptr))
{
  unsigned long *f;

  if (!CPOINTERP(ptr))
    STk_error("bad pointer: ~S", ptr);

  f = (unsigned long*) CPOINTER_VALUE(ptr);
  *f = (unsigned long) stklos_interactive_gtk_loop;

  return STk_void;
}

/* ----------------------------------------------------------------------
 *
 *      Timeout  ...
 *
 * ---------------------------------------------------------------------- */
static gboolean do_timeout_call(gpointer proc)
{
  return STk_C_apply((SCM) proc, 0) != STk_false;
}


DEFINE_PRIMITIVE("timeout", timeout, subr2, (SCM delay, SCM proc))
{
  long val = STk_integer_value(delay);

  if (val == LONG_MIN) error_bad_integer(delay);
  if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);

  return STk_long2integer((long) g_timeout_add(val, do_timeout_call, proc));
}

DEFINE_PRIMITIVE("when-idle", when_idle, subr1, (SCM proc))
{
  if (STk_procedurep(proc) == STk_false) STk_error("bad procedure ~S", proc);

  return STk_long2integer((long) g_idle_add(do_timeout_call, proc));
}

DEFINE_PRIMITIVE("kill-idle", kill_idle, subr1, (SCM id))
{
  long val = STk_integer_value(id);

  if (val == LONG_MIN) error_bad_integer(id);
  return MAKE_BOOLEAN(g_source_remove(val));
}



#if HAVE_CANVAS == 1
/* ----------------------------------------------------------------------
 *
 *      Canvas support
 *
 * ______________________________________________________________________
 */

static SCM canvas_line;  // will contain the symbol "canvas-line"

static void error_bad_polyline(SCM obj)
{
  STk_error("bad Goo canvas polyline ~S", obj);
}

DEFINE_PRIMITIVE("%polyline-get-points", goocanv_get_points, subr1, (SCM line))
{
  SCM res = STk_nil;
  GooCanvasPoints* p;
  int i, len;

  if (!CPOINTERP(line) || CPOINTER_TYPE(line) != canvas_line)
    error_bad_polyline(line);

  g_object_get(G_OBJECT(CPOINTER_VALUE(line)), "points", &p, NULL);
  len = p->num_points * 2;
  for (i =  len-1 ; i >= 0; i--) {
    res = STk_cons(STk_double2real(p->coords[i]), res);
  }

  return res;
}

DEFINE_PRIMITIVE("%polyline-set-points!", goocanv_set_points, subr2, (SCM line, SCM lst))
{
  int i, len = STk_int_length(lst);
  GooCanvasPoints* p, *old;

  if (!CPOINTERP(line) || CPOINTER_TYPE(line) != canvas_line)
    STk_error("bad Goo canvas polyline ~S", line);

  if (len < 0) STk_error("bad list ~S", lst);
  if ((len % 2) != 0) STk_error("number of coordinates in ~S must be even", lst);

  /* Grab the old value to unref it later */
  g_object_get(G_OBJECT(CPOINTER_VALUE(line)), "points", &old, NULL);

  /* Allocate a new set of points */
  p = goo_canvas_points_new(len / 2);  // ref_count is set to 1

  for (i = 0; !NULLP(lst) ; lst = CDR(lst), i++) {
    double d = STk_number2double(CAR(lst));

    if (isnan(d)) STk_error("bad real number ~S", CAR(lst));
    p->coords[i] = d;
  }
  g_object_set(G_OBJECT(CPOINTER_VALUE(line)), "points", p, NULL);
  goo_canvas_points_unref(old); // to free it

  return STk_void;
}
#endif // HAVE_CANVAS

/* ======================================================================
 *
 *      Module stklos-gtklos starts here
 *
 * ======================================================================
 */


MODULE_ENTRY_START("stklos/gtklos") {

  SCM gtklos_module;

  /* Create a new module named "stklos/gtklos" */
  gtklos_module = STk_create_module(STk_intern("stklos/gtklos"));

   /* Create a new type for GTK events */
  tc_gtk_event = STk_new_user_type(&xtype_gtk_event);


  /* Add new primitives */
  ADD_PRIMITIVE_IN_MODULE(gtk_get_prop, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(gtk_set_prop, gtklos_module);


  ADD_PRIMITIVE_IN_MODULE(get_gtk_event, gtklos_module);


  ADD_PRIMITIVE_IN_MODULE(gtk_get_child_prop, gtklos_module);
  //XXX ADD_PRIMITIVE_IN_MODULE(gtk_get_width, gtklos_module);
  //XXX ADD_PRIMITIVE_IN_MODULE(gtk_get_height, gtklos_module);

//FIXME:   ADD_PRIMITIVE_IN_MODULE(box_set_packing, gtklos_module);
//FIXME:   ADD_PRIMITIVE_IN_MODULE(cont_children, gtklos_module);

  ADD_PRIMITIVE_IN_MODULE(event_type, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(event_x, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(event_y, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(event_char, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(event_keyval, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(event_modifiers, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(event_button, gtklos_module);

  ADD_PRIMITIVE_IN_MODULE(string2color, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(color2string, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(file_chooser_files, gtklos_module);

  //FIXME: ADD_PRIMITIVE_IN_MODULE(dialog_vbox, gtklos_module);

  ADD_PRIMITIVE_IN_MODULE(rl_hook, gtklos_module);

  ADD_PRIMITIVE_IN_MODULE(timeout, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(when_idle, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(kill_idle, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(flush_gtk_event, gtklos_module);
  
#if HAVE_CANVAS == 1
  /* Canvas support */
  canvas_line = STk_intern("canvas-line");
  ADD_PRIMITIVE_IN_MODULE(goocanv_get_points, gtklos_module);
  ADD_PRIMITIVE_IN_MODULE(goocanv_set_points, gtklos_module);
#endif

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);

} MODULE_ENTRY_END

DEFINE_MODULE_INFO
