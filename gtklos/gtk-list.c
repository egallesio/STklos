/*
 * gtk-list.c	-- GTK+ Listboxes
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
 *    Creation date: 11-Oct-2000 11:21 (eg)
 * Last file update:  5-Apr-2002 17:11 (eg)
 */

#include "stklos.h"
#include "gtk-glue.h"


static SCM listbox_keyword; 	/* contains the Scheme keyword :listbox */

/*
 * Find the listbox component of a GTklos listbox (a scrollled window) 
 */
static SCM get_listbox(SCM lb)
{
  if (WIDGETP(lb)) {
    SCM listbox = STk_key_get(WIDGET_PLIST(lb), listbox_keyword, STk_false);
    
    if (listbox != STk_false) return listbox;
  }
  STk_error("bad listbox ~S", lb);
  return STk_void;
}

/*
 *
 * The <listbox> constructor
 *
 */
DEFINE_PRIMITIVE("%listbox", listbox, subr1, (SCM obj))
{
  GtkWidget *scrolled_window, *list;
  SCM listbox, res;

  /* Create a scrolled window in which we will put the listbox */
  scrolled_window = gtk_scrolled_window_new(NULL, NULL);

  /* Create the listbox in the scrolled window */
  list    = gtk_list_new();
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), list);
  gtk_container_set_resize_mode(GTK_CONTAINER(scrolled_window), GTK_RESIZE_PARENT);

  /* The result is the scrolled window */
  listbox = WIDGET_TO_SCM(list, obj);
  res     = WIDGET_TO_SCM(scrolled_window, obj);  

  /* Keep a reference on the listbox through the scrolled_window */
  WIDGET_PLIST(res) = LIST2(listbox_keyword, listbox);

  return res;
}



DEFINE_PRIMITIVE("%set-list-items!", set_list_items, subr2, (SCM lb, SCM l))
{
  SCM tmp;
  GtkWidget *lst_item, *content, *listbox;
  int n;
  
  /* First pass on the list to verify that the elements are strings or widgets */
  if (!NULLP(l) && !CONSP(l)) STk_error("bad list ~S", l);

  for (tmp = l; !NULLP(tmp); tmp = CDR(tmp)) {
    if (!STRINGP(CAR(tmp)) && !WIDGETP(CAR(tmp)))
      STk_error("bad listbox element ~S", CAR(tmp));
  }

  listbox = WIDGET_ID(get_listbox(lb));

  /* We are now sure that the list is well formed => We can delete old value */
  gtk_list_clear_items ((GtkList*) listbox, 0, -1);

  /* Build the new list items */
  for (n=0, tmp = l; !NULLP(tmp); n++, tmp = CDR(tmp)) {
    if (STRINGP(CAR(tmp))) {
      lst_item = gtk_list_item_new_with_label(STRING_CHARS(CAR(tmp)));
    } else {
      content  = WIDGET_ID(CAR(tmp));
      lst_item = gtk_list_item_new();
      gtk_container_add(GTK_CONTAINER(lst_item), content);
      gtk_widget_show(content);
    }
    gtk_container_add(GTK_CONTAINER(listbox), lst_item);
    gtk_widget_show(lst_item);
    /* Keep the rank of this item in its internal data (used when querying 
     * selection)
     */
    gtk_object_set_user_data(GTK_OBJECT(lst_item), (gpointer) AS_LONG(n));
  }
  return STk_void;
}

DEFINE_PRIMITIVE("%select-list-item", select_list_item, subr2, (SCM lb, SCM item))
{
  int n = STk_integer_value(item);
  GtkList* listbox;

  if (n == LONG_MIN) STk_error("bad index ~S", item);

  listbox = (GtkList *) WIDGET_ID(get_listbox(lb));

  gtk_list_select_item(listbox, n);
  return STk_void;
}

DEFINE_PRIMITIVE("%unselect-all-list-items!",unselect_all_list_item, subr1,(SCM lb))
{
  gtk_list_unselect_all(GTK_LIST(WIDGET_ID(get_listbox(lb))));
  return STk_void;
}

DEFINE_PRIMITIVE("%listbox-selection-get",listbox_sel_get, subr1,(SCM lb))
{
  GList *dlist;
  SCM l = STk_nil;
  GtkList* listbox;

  listbox = (GtkList *) WIDGET_ID(get_listbox(lb));
  
  for (dlist=listbox->selection; dlist; dlist=dlist->next) {
    l = STk_cons(MAKE_INT((long) gtk_object_get_user_data(GTK_OBJECT(dlist->data))),
		 l);
  }
  return l;
}



/* ====================================================================== */

void STk_init_gtk_list(void)
{
  /* Retain in a static some keywords used here to avoid their reconstruction */
  listbox_keyword  = STk_makekey(":listbox");

  ADD_PRIMITIVE(listbox);
  ADD_PRIMITIVE(set_list_items);
  ADD_PRIMITIVE(select_list_item);
  ADD_PRIMITIVE(unselect_all_list_item);
  ADD_PRIMITIVE(listbox_sel_get);
}
