/*
 * dynload.c	-- Dynamic loading stuff
 * 
 * Copyright © 2000-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 23-Jan-1994 19:09
 * Last file update: 25-Jun-2007 15:20 (eg)
 *
 */

#include "stklos.h"

#define INIT_FUNC_NAME "STk_module_main"
#define INFO_FUNC_NAME "STk_module_info"

#if defined(DARWIN)
#  include "dynload-macos.c"
#  define HAVE_DLOPEN
#endif



#define INIT_FUNC_NAME_STRING(x) #x

#ifdef HAVE_DLOPEN
#  include <dlfcn.h>

#  ifdef RTLD_LAZY
#     define FLAG1 RTLD_LAZY
#  else
#     define FLAG1 1
#  endif

#  ifdef RTLD_GLOBAL
#    define FLAG2 RTLD_GLOBAL
#  else
#    define FLAG2 0
#  endif

#  define DYN_FLAG (FLAG1|FLAG2)

typedef void (*InitFunc)(void);
typedef SCM  (*InfoFunc)(void);



static SCM files_already_loaded = (SCM) NULL;
MUT_DECL(dynload_mutex);


static void initialize_dynload(void)
{
  void *handle;

  if ((handle = (void *) dlopen(NULL, DYN_FLAG)) == NULL)
    STk_error("cannot initialize dynamic loading system (%s)", dlerror());

  MUT_LOCK(dynload_mutex);
  files_already_loaded = LIST1(STk_cons(STk_Cstring2string(""), (SCM) handle));
  MUT_UNLOCK(dynload_mutex);
}


void *STk_find_external_function(char *path, char *fname, int error_if_absent)
{
  void *handle, *fct;
  SCM l;
  
  handle = fct = NULL;

  if (!files_already_loaded) 
    initialize_dynload();

  /* See if the file has already loaded. If so, use the old handle */
  for (l = files_already_loaded; !NULLP(l); l = CDR(l)) {
    /* An inline Assoc which knows that keys are well formed C strings */
    if (strcmp(STRING_CHARS(CAR(CAR(l))), path) == 0) {
      handle = (void *) CDR(CAR(l));
      break;
    }
  }

  if (!handle) {
    errno = 0;
    /* Not seen before => dynamically load the file and enter its handle in cache */
    if ((handle=(void *) dlopen(path, DYN_FLAG)) == NULL) {
      STk_error("cannot open object file %s (%s)", path, dlerror());
    }
    MUT_LOCK(dynload_mutex);
    files_already_loaded = STk_cons(STk_cons(STk_Cstring2string(path), (SCM) handle),
				    files_already_loaded);
    MUT_UNLOCK(dynload_mutex);
  }

  if ((fct = (void *) dlsym(handle, fname)) == NULL && error_if_absent) {
    STk_error("cannot find symbol `%s' in `%s'", fname, path);
  }
  return fct;
}

SCM STk_load_object_file(SCM f, char *path)
{
  InitFunc init_fct;

  /* Close the port since we don't need it */
  STk_close_port(f);

  init_fct = STk_find_external_function(path, INIT_FUNC_NAME, TRUE);
  init_fct();
  return STk_true;
}

SCM STk_info_object_file(char *path)
{
  InfoFunc info_fct;

  info_fct = STk_find_external_function(path, INFO_FUNC_NAME, FALSE);
  return (info_fct) ? info_fct() : STk_nil;
}
#endif /* HAVE_DLOPEN */
