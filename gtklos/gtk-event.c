/*
 * gtk-event.c	-- GTK + Event Management
 * 
 * Copyright © 2000-2005 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 21-Oct-2000 14:16 (eg)
 * Last file update: 15-Aug-2005 22:10 (eg)
 */

#include <signal.h>
#include <time.h>
#include "stklos.h"
#include "gtk-glue.h"
#include <gdk/gdkkeysyms.h>	/* For the keysyms macros */


static void error_bad_event(SCM event)
{
  STk_error("bad event", event);
}


/*===========================================================================*\
 * 
 * 				Event Loop Management
 * 
\*===========================================================================*/

static int input_handler_id;	/* the tag of the current input handler */
static int seen_control_C = 0;	/* will be one if a ^C is hit while in event loop */


static void seen_a_char(void)
{
  gtk_main_quit();
}


static void sigint_handler(int sig)
{
  /* This handler is effective only while managing GTK events */
  seen_control_C = 1;
  if (input_handler_id >= 0) gtk_main_quit();
}



DEFINE_PRIMITIVE("", gtk_idle, subr0, (void)) /* "anonymous" primitive !! */
{
  struct sigaction sigact, old_sigact;

  /* The normal handler for ^C cannot be used, since it makes the gtk-main 
   * reentrant. => We hae a special handler while in the GTk loop */
  sigfillset(&(sigact.sa_mask));
  sigact.sa_handler = sigint_handler;
  sigact.sa_flags   = 0;
  sigaction(SIGINT, &sigact, &old_sigact);
  
  /* Call the GTk+ event loop handler */
  seen_control_C = 0;
  gtk_main();
  
  /* Reset normal STklos handler */
  sigaction(SIGINT, &old_sigact, NULL);

  if (seen_control_C) /* propagate  */ old_sigact.sa_handler(SIGINT);
  
  /* old code:  if (gtk_events_pending()) gtk_main_iteration(); */
  return STk_void;
}


DEFINE_PRIMITIVE("%gtk-main", gtk_main, subr0, (void))
{
  /* to avoid premature termination when a script is completely read */
  gtk_main();
  return STk_void;
}

static void set_input_handler(void)
{
  /* Set a handler for stdin to interact cleanly with the event loop */
  input_handler_id = gdk_input_add(0, GDK_INPUT_READ | GDK_INPUT_EXCEPTION, 
				   (GdkInputFunction) seen_a_char, NULL);
}


/*===========================================================================*\
 * 
 *		 		After 
 *
\*===========================================================================*/

static SCM after_handlers = STk_nil;

static void register_proc(SCM arg)
{
  /* Register the function to avoid GC problems */
  after_handlers = STk_cons((SCM) arg, after_handlers);
}

static int apply_after(gpointer data)
{
  SCM tmp, prev; 

  if (data) {
    /* apply the function */
    STk_C_apply((SCM) data, 0);
    
    /* Unregister the function from the global list */
    for (prev=(SCM) NULL, tmp=after_handlers; !NULLP(tmp); prev=tmp, tmp=CDR(tmp)) {
      if (CAR(tmp) == (SCM) data) {
	if (prev)
	  CDR(prev) = CDR(tmp);
	else
	  after_handlers = CDR(tmp);
      }
    }
  }
  return 0;
}

//static void sigint_handler_on_wait(int sig)
//{
//  seen_control_C = 1;
//}


static void wait_ms(int ms)		/* For (after ms) without procedure */
{
  GTimeVal now, end;
  struct timespec ts;
  struct sigaction sigact, old_sigact;

  g_get_current_time(&now);
  end.tv_sec  = now.tv_sec  + ms/1000;
  end.tv_usec = now.tv_usec + (ms%1000)*1000;

  /* Unregister the character handler on standard input since it interacts
   * badly with our wait to manage the waiting 
   */
  gdk_input_remove(input_handler_id);
  input_handler_id = -1;

  
  sigfillset(&(sigact.sa_mask));
  sigact.sa_handler = sigint_handler;
  sigact.sa_flags   = 0;
  sigaction(SIGINT, &sigact, &old_sigact);
  
  seen_control_C = 0;
  
  for ( ; ; ) {
     /* if current time is > to end exit the loop */
    g_get_current_time(&now);
    if (now.tv_sec > end.tv_sec) break;
    else if (now.tv_sec == end.tv_sec)
      if (now.tv_usec >= end.tv_usec) break;
    
    /* If we have seen a ^C abort the loop too */
    if (seen_control_C) break;

    /* Otherwise treat events */
    while (gtk_events_pending())
      gtk_main_iteration();

    /* wait a little befor looping again */
    ts.tv_sec=0; ts.tv_nsec = 1000; /* 1 µs */
    nanosleep(&ts, NULL);
  }
  
  /* restore the input  handler */
  set_input_handler();

  sigaction(SIGINT, &old_sigact, NULL);
  if (seen_control_C) /* propagate  */ old_sigact.sa_handler(SIGINT);
}
  
  

DEFINE_PRIMITIVE("%after", after, subr23, (SCM what, SCM arg1, SCM arg2))
{
  /* WARNING: no control on the type of arguments. This is done in Scheme */
  switch (AS_LONG(what)) {
    case SCM_LONG(0): {				/* after ms action */
      long ms = STk_integer_value(arg1);

      register_proc(arg2);
      return MAKE_INT(gtk_timeout_add(ms, apply_after, (gpointer) arg2));
    }
    case SCM_LONG(1): {				/* after ms */	 //FIXME
      wait_ms(STk_integer_value(arg1));
      return STk_void;
    }
    case SCM_LONG(2): 				/* after 'idle ... */
      register_proc(arg1);
      return MAKE_INT(gtk_idle_add(apply_after, (gpointer) arg1));
    case SCM_LONG(3): 				/* after 'cancel # */
      gtk_timeout_remove(STk_integer_value(arg1));
      return STk_void;
  }
  return STk_void;
}



/*===========================================================================*\
 * 
 *		 		Update 
 *
\*===========================================================================*/

/*
<doc EXT update
 * (update)
 * 
 *  |Update| is used to bring the application ``up to date'' by
 *  entering the event loop repeated until all pending events have
 *  been processed. This procedure is useful in scripts where you are
 *  performing a long-running computation but you still want the
 *  application to respond to events such as user interactions.
doc>
 */
DEFINE_PRIMITIVE("update", update, subr0, (void))
{
  while (gtk_events_pending())
    gtk_main_iteration();
  return STk_void;
}

/*===========================================================================*\
 * 
 *		 		Low level Events
 *
\*===========================================================================*/

static int tc_event;	/* The type_cell number associated to GTK/GDK events */

struct event_obj {
  stk_header header;
  SCM receiver;
  GdkEvent *event_descr;
};

#define EVENTP(p) 	 	(BOXED_TYPE_EQ((p), tc_event))
#define EVENT_RECEIVER(p) 	(((struct event_obj *) (p))->receiver)
#define EVENT_DESCR(p)	 	(((struct event_obj *) (p))->event_descr)

static struct extended_type_descr xtype_event = {
  "gtk-event",
  NULL,
};


/* 
 * Build an event from a receiver and a GDK event descriptor 
 *
 */
SCM STk_make_event(SCM receiver, GdkEvent *event)
{
  SCM z;

  NEWCELL(z, event);
  EVENT_RECEIVER(z) = receiver;
  EVENT_DESCR(z)    = event;
  
  return z;
}


/* 
 * Event information getters 
 *
 */

DEFINE_PRIMITIVE("event-widget", event_widget, subr1, (SCM event))
{
  if (!EVENTP(event)) error_bad_event(event);
  return EVENT_RECEIVER(event);
}


DEFINE_PRIMITIVE("event-type", event_type, subr1, (SCM event))
{
  GdkEvent *ev;

  if (!EVENTP(event)) error_bad_event(event);
  ev = EVENT_DESCR(event);

  if (!ev) return STk_intern("NOTHING");
  switch (((GdkEventAny *) ev)->type) {
    case GDK_NOTHING           : return STk_intern("NOTHING");
    case GDK_DELETE            : return STk_intern("DELETE");
    case GDK_DESTROY           : return STk_intern("DESTROY");
    case GDK_EXPOSE            : return STk_intern("EXPOSE");
    case GDK_MOTION_NOTIFY     : return STk_intern("MOTION");
    case GDK_BUTTON_PRESS      : 
    case GDK_2BUTTON_PRESS     : 
    case GDK_3BUTTON_PRESS     : return STk_intern("PRESS");
    case GDK_BUTTON_RELEASE    : return STk_intern("RELEASE");
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
    case GDK_SELECTION_NOTIFY  : return STk_intern("SELECTION");
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
    case GDK_NO_EXPOSE         : return STk_intern("NO-EXPOSE");
    default: 			 return STk_void;
  }
}


DEFINE_PRIMITIVE("event-x", event_x, subr1, (SCM event))
{
  GdkEvent *ev;

  if (!EVENTP(event)) error_bad_event(event);
  ev = EVENT_DESCR(event);
  
  if(!ev) return MAKE_INT(-1);
   
  switch(((GdkEventAny *) ev)->type) {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:   return STk_double2real(((GdkEventCrossing *)  ev)->x);
    case GDK_BUTTON_PRESS: 
    case GDK_2BUTTON_PRESS: 
    case GDK_3BUTTON_PRESS: 
    case GDK_BUTTON_RELEASE: return STk_double2real(((GdkEventButton *)    ev)->x);
    case GDK_MOTION_NOTIFY:  return STk_double2real(((GdkEventMotion *)    ev)->x);
    case GDK_CONFIGURE:      return MAKE_INT(((GdkEventConfigure *) ev)->x);
    default:		     return MAKE_INT(-1);
  }
}


DEFINE_PRIMITIVE("event-y", event_y, subr1, (SCM event))
{
  GdkEvent *ev;

  if (!EVENTP(event)) error_bad_event(event);
  ev = EVENT_DESCR(event);
  
 if(!ev) return MAKE_INT(-1);
   
  switch(((GdkEventAny *) ev)->type) {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:   return STk_double2real(((GdkEventCrossing *)  ev)->y);
    case GDK_BUTTON_PRESS: 
    case GDK_2BUTTON_PRESS: 
    case GDK_3BUTTON_PRESS: 
    case GDK_BUTTON_RELEASE: return STk_double2real(((GdkEventButton *)    ev)->y);
    case GDK_MOTION_NOTIFY:  return STk_double2real(((GdkEventMotion *)    ev)->y);
    case GDK_CONFIGURE:      return MAKE_INT(((GdkEventConfigure *) ev)->y);
    default:		     return MAKE_INT(-1);
  }
}

DEFINE_PRIMITIVE("event-char", event_char, subr1, (SCM event))
{
  GdkEvent *ev;
  int keyval;
  
  if (!EVENTP(event)) error_bad_event(event);
  ev = EVENT_DESCR(event);
  
  if(!ev) return MAKE_CHARACTER(0);
  
  switch (((GdkEventAny *) ev)->type) {
    case GDK_KEY_PRESS:
      keyval = ((GdkEventKey *)ev)->keyval;
      switch (keyval) {
	case GDK_Return: return MAKE_CHARACTER('\n');
	case GDK_Tab:	 return MAKE_CHARACTER('\t');
	default:         keyval = (keyval < 0xff) ? keyval: 0;;
	  		 return MAKE_CHARACTER(keyval);
      }
    default: 
      return MAKE_CHARACTER(0);
  }
}

DEFINE_PRIMITIVE("event-keysym", event_keysym, subr1, (SCM event))
{
  GdkEvent *ev;
  int keyval;
  
  if (!EVENTP(event)) error_bad_event(event);
  ev = EVENT_DESCR(event);
  
  if(!ev) return STk_void;
  
  switch (((GdkEventAny *) ev)->type) {
    case GDK_KEY_PRESS: 
      switch (keyval = ((GdkEventKey *)ev)->keyval) {
	case GDK_Return: return MAKE_CHARACTER('\n');
	case GDK_Tab:	 return MAKE_CHARACTER('\t');
	default:         return MAKE_INT(keyval);
      }
    default: 
      return STk_void;
  }
}

DEFINE_PRIMITIVE("event-modifiers", event_modifiers,  subr1, (SCM event))
{
  GdkEvent *ev;
  SCM res = STk_nil;
  int modifiers;
  
  if (!EVENTP(event)) error_bad_event(event);
  ev = EVENT_DESCR(event);
  
  if(!ev) return STk_nil;

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
  
  if (!EVENTP(event)) error_bad_event(event);
  ev = EVENT_DESCR(event);
  
  if(!ev) return STk_void;
  
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


/*===========================================================================*\
 * 
 *				Event masks 
 * 
\*===========================================================================*/
#define defconst(name) STk_define_variable(STk_intern(#name),	\
					   MAKE_INT(name),	\
					   STk_gtk_module);
static void init_event_masks(void)
{
  defconst(GDK_EXPOSURE_MASK);
  defconst(GDK_POINTER_MOTION_MASK);
  defconst(GDK_POINTER_MOTION_HINT_MASK);
  defconst(GDK_BUTTON_MOTION_MASK);
  defconst(GDK_BUTTON1_MOTION_MASK);
  defconst(GDK_BUTTON2_MOTION_MASK);
  defconst(GDK_BUTTON3_MOTION_MASK);
  defconst(GDK_BUTTON_PRESS_MASK);
  defconst(GDK_BUTTON_RELEASE_MASK);
  defconst(GDK_KEY_PRESS_MASK);
  defconst(GDK_KEY_RELEASE_MASK);
  defconst(GDK_ENTER_NOTIFY_MASK);
  defconst(GDK_LEAVE_NOTIFY_MASK);
  defconst(GDK_FOCUS_CHANGE_MASK);
  defconst(GDK_STRUCTURE_MASK);
  defconst(GDK_PROPERTY_CHANGE_MASK);
  defconst(GDK_VISIBILITY_NOTIFY_MASK);
  defconst(GDK_PROXIMITY_IN_MASK);
  defconst(GDK_PROXIMITY_OUT_MASK);
  defconst(GDK_SUBSTRUCTURE_MASK);
  defconst(GDK_ALL_EVENTS_MASK);
}

// 
// DEFINE_PRIMITIVE("%set_events", set_events, subr2, (SCM widget, SCM value))
// {
//   long v = (gint) STk_integer_value(value);
// 
//   if (!WIDGETP(widget)) STk_error_bad_widget(widget);
//   if (v == LONG_MIN)    STk_error("bad value for event mask", value);
// 
//   gtk_widget_add_events(WIDGET_ID(widget), v);
// 
//   return STk_void;
// }


/*===========================================================================*\
 * 
 *		 		Initialization 
 *
\*===========================================================================*/
void STk_init_gtk_event(void)
{
  set_input_handler();
  init_event_masks();

  /* Define the new Scheme event type */
  DEFINE_USER_TYPE(tc_event, &xtype_event);

  /* Add the event loop */
  STk_add_port_idle(STk_stdin, THE_PRIMITIVE(gtk_idle));

  ADD_PRIMITIVE(gtk_main);
  ADD_PRIMITIVE(after);
  ADD_PRIMITIVE(update);

  /* Event information getters */
  ADD_PRIMITIVE(event_widget);
  ADD_PRIMITIVE(event_type);
  ADD_PRIMITIVE(event_x);
  ADD_PRIMITIVE(event_y);
  ADD_PRIMITIVE(event_char);
  ADD_PRIMITIVE(event_keysym);
  ADD_PRIMITIVE(event_modifiers);
  ADD_PRIMITIVE(event_button);
//  ADD_PRIMITIVE(set_events);
}
