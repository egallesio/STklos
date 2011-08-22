/*
 *
 * d y n l o a d . c			-- All the stuff dealing with
 *					   dynamic loading
 *
 * Copyright © 1993-2005 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 *
 *
 * Permission to use, copy, modify, distribute,and license this
 * software and its documentation for any purpose is hereby granted,
 * provided that existing copyright notices are retained in all
 * copies and that this notice is included verbatim in any
 * distributions.  No written agreement, license, or royalty fee is
 * required for any of the authorized uses.
 * This software is provided ``AS IS'' without express or implied
 * warranty.
 *
 *           Author: Erick Gallesio [eg@kaolin.unice.fr]
 *    Creation date: 23-Jan-1994 19:09
 * Last file update: 11-Jan-2005 16:00 (eg)
 *
 * Win32 DLL support by Steve Pruitt <steve@pruitt.net>
 *
 * 	- Added load_and_call functions for dynload for Win32 DLL support
 * 	- Added check for *shared-prefix* in load_object_file
 *
 */

/* Support for HPUX is due to Dipankar Gupta <dg@hplb.hpl.hp.com> */
/* Support for NETBSD is from Franke Ruediger (Ruediger.Franke@rz.tu-ilmenau.de) */
/* Support for FreeBsD is due to Amancio Hasty Jr (hasty@netcom.com) */
/* Support for Linux is inspired from Patrick Nguyen (pnguyen@elde.epfl.ch) */


#if defined(SUNOS4)||defined(SUNOS5)||defined(IRIX5)||defined(OSF1)||defined(UNIXWARE)
#  include <dlfcn.h>
#  define DLOPEN_COMPATIBLE
#endif

#if defined(LINUX_ELF)
#  include <dlfcn.h>
#  define DLOPEN_COMPATIBLE
#endif

#if defined(LINUX_DLD)
#  include <dld.h>
#endif

#if defined(NETBSD)
#  include <sys/types.h>
#  include <nlist.h>
#  include <link.h>
#  define dlerror() "error unknown" /* dlerror() isn't implemented in NetBSD 1.0 */
#  define DLOPEN_COMPATIBLE
#endif

#if defined(FREEBSD)
#  define dlerror() "error unknown" /* dlerror() isn't implemented in FreeBSD too */
#  define DLOPEN_COMPATIBLE
#endif

#ifdef HPUX
#  include <dl.h>
#endif

#ifdef RTLD_LAZY
#  define FLAG1 RTLD_LAZY
#else
#  define FLAG1 1
#endif
#ifdef RTLD_GLOBAL
#  define FLAG2 RTLD_GLOBAL
#else
#  define FLAG2 0
#endif

#define DYN_FLAG (FLAG1|FLAG2)

/*----------------------------------------------------------------------------*/

#if (defined(WIN32) && !defined(CYGWIN32) && defined(MSC_VER))
#  include <windows.h>
#  include "stk.h"
#  include "tclInt.h"
#  include "tclPort.h"
#else
#  include "stk.h"
#  ifdef USE_DYNLOAD
#     include <libstack.h>
#  endif
#endif


#ifdef USE_DYNLOAD
static SCM cache_files = NULL;

#if (defined(WIN32) && !defined(CYGWIN32) && defined(MSC_VER))

#include <libstack-Win32.h>

#define dlerror() "error unknown"

#define MAKE_STAT_PTR(p) (STk_make_Cpointer(ANONYMOUS_STAT_PTR_ID, (p), TRUE))
#define MAKE_DYN_PTR(p)  (STk_make_Cpointer(ANONYMOUS_DYN_PTR_ID,  (p), FALSE))

HINSTANCE TclWinLoadLibrary(char *path);

HINSTANCE tclInstance;

static void initialize_dynload(void)
{
  void *handle;

  handle = (void *) TclWinGetTclInstance();
  cache_files = LIST2(STk_makestring(""), MAKE_STAT_PTR(handle));
  STk_gc_protect(&cache_files);
}


static void *find_function(char *path, char *fname, int error_if_absent)
{
  void *handle, *fct;
  SCM l, str = STk_makestring(path);

  ENTER_SCM("dynload");

  if (cache_files == NULL) initialize_dynload();

  if ((l = STk_member(str, cache_files)) != STk_false) {
    /* This file has already been loaded. Find its handle */
    handle = EXTDATA(CAR(CDR(l)));
  }
  else {
    /* Dynamically load the file and enter its handle in cache */
    if ((handle = (void *) TclWinLoadLibrary(path)) == NULL)
      fprintf(stderr, "find_function: cannot open object file : %s", dlerror());
    cache_files = Cons(str, Cons(MAKE_STAT_PTR(handle), cache_files));
  }

  if ((fct = (void *) GetProcAddress(handle, fname)) == NULL && error_if_absent) {
    char msg[MAX_PATH_LENGTH];

    sprintf(msg, "cannot find symbol ``%s'' in object file", fname);
    Serror(msg, str);
  }

  return fct;
}


static void load_and_call(char *path, char *fct_name)
{
  void (*init_fct)();
  SCM str = STk_makestring(path);

  ENTER_SCM("dynload");

  /* Test if fct_name is already defined in the core interpreter */
  tclInstance = TclWinGetTclInstance();
  if ((init_fct = (void (*)()) GetProcAddress(tclInstance, fct_name)) != NULL)
    Serror("module is already (statically) loaded", str);

  if (cache_files == NULL) initialize_dynload();
  if (STk_member(str, cache_files) != STk_false) {
    Serror("module is already (dynamically) loaded", str);
  }

  init_fct = find_function(path, fct_name, TRUE);
  (*init_fct)();
}

#endif /* MSC_VER WIN32 */


#if defined(SUNOS4) || defined(SUNOS5) || defined(NETBSD) || defined(FREEBSD) || defined(IRIX5) || defined(OSF1) ||defined(LINUX_ELF) ||defined(UNIXWARE)

#define MAKE_STAT_PTR(p) (STk_make_Cpointer(ANONYMOUS_STAT_PTR_ID, (p), TRUE))
#define MAKE_DYN_PTR(p)  (STk_make_Cpointer(ANONYMOUS_DYN_PTR_ID,  (p), FALSE))


static void initialize_dynload(void)
{
  void *handle;

  if ((handle = (void *) dlopen(NULL, DYN_FLAG)) == NULL)
    Err("dynload: cannot initialize dynload.", STk_makestring(dlerror()));

  cache_files = LIST2(STk_makestring(""), MAKE_STAT_PTR(handle));
  STk_gc_protect(&cache_files);
}


static void *find_function(char *path, char *fname, int error_if_absent)
{
  void *handle, *fct;
  SCM l, str = STk_makestring(path);

  ENTER_SCM("dynload");

  if (cache_files == NULL) initialize_dynload();

  if ((l = STk_member(str, cache_files)) != STk_false) {
    /* This file has already been loaded. Find its handle */
    handle = EXTDATA(CAR(CDR(l)));
  }
  else {
    /* Dynamically load the file and enter its handle in cache */
    if ((handle=(void *) dlopen(path, DYN_FLAG)) == NULL)
      fprintf(stderr, "find_function: cannot open object file : %s", dlerror());
    cache_files = Cons(str, Cons(MAKE_STAT_PTR(handle), cache_files));
  }

  if ((fct = (void *) dlsym(handle, fname)) == NULL && error_if_absent) {
    char msg[MAX_PATH_LENGTH];

    sprintf(msg, "cannot find symbol ``%s'' in object file", fname);
    Serror(msg, str);
  }

  return fct;
}


static void load_and_call(char *path, char *fct_name)
{
  void (*init_fct)();
  SCM str = STk_makestring(path);

  ENTER_SCM("dynload");

  /* Test if fct_name is already defined in the core interpreter */
  if ((init_fct = find_function("", fct_name, FALSE)) != NULL)
    Serror("module is already (statically) loaded", str);

  if (STk_member(str, cache_files) != STk_false) {
    Serror("module is already (dynamically) loaded", str);
  }

  init_fct = find_function(path, fct_name, TRUE);
  (*init_fct)();
}
#endif

#if defined(LINUX_DLD)
/*
 * This code is for Linux, using the dld package. This code should not be used
 * anymore when ELF will be completely accepted under Linux. In the meanwhile...
 *
 * This code is inspired from a code sent by Patrick Nguyen pnguyen@elde.epfl.ch.
 *
 */
static SCM list_of_files = NULL;

static void load_and_call(char *path, char *fct_name)
{
  void *handle;
  void (*init_fct)();
  SCM str;
  static dld_already_initialized = FALSE;

  /* Try to avoid multiple loading */
  if (!list_of_files) {
    STk_gc_protect(&list_of_files);
    list_of_files = STk_nil;
  }
  str = STk_makestring(path);
  if (STk_member(str, list_of_files) != STk_false) {
    Err("Module is already (dynamically) loaded", str);
  }

  /* Dld must be initialized at first call */
  if(!dld_already_initialized) {
    if (dld_init (dld_find_executable (STk_Argv0)))
      dld_perror("dld: failed to init dld");
    else
      dld_already_initialized = TRUE;
  }

  /* Load file */
  if (dld_link(path)) dld_perror("dld: cannot link");

  /* And get a pointer on function "fct_name" */
  init_fct = (void (*) ()) dld_get_func(fct_name);
  if (init_fct) {
    /* When loading a function. It can induce some unresolved references
     * Most of the time, the unresolved references come from fucntions
     * which are in the libc but which are not used by the core interpreter.
     * For instance, if loaded module uses fork, which is not loaded in
     * the interpreter, we will have an unresolve reference for _fork.
     * To avoid this problem, we make again a link against standard libc
     * Note: in general situation is more complicated than this, unresolved
     * symbols could be elsewhere than the libc....
     * Those situation are not handled by current code, but I hope that ELF
     * will supplant this way of doing dynamic loading soon...
     */
    if (dld_function_executable_p (fct_name))
      /* Call the init code */
      (*init_fct) ();
    else {
      /* Function is notexecutable = we have unresolved references */
      if (dld_link("/usr/lib/libc.a") && dld_link("/usr/lib/libc.sa"))
	dld_perror("dld: cannot link");

      if (dld_function_executable_p (fct_name)) {
	init_fct = (void (*) ()) dld_get_func(fct_name);
	(*init_fct) ();
      }
      else {
	char **unresolved;
	extern int dld_undefined_sym_count;
	int i;
	Fprintf(STk_curr_eport, "dld: function %s not executable!\n", fct_name);
	Fprintf(STk_curr_eport, "Unresolved symbols are:\n");

	unresolved= dld_list_undefined_sym();
	for (i = 0; i < dld_undefined_sym_count; i++)
	  Fprintf(STk_curr_eport, "\t%s\n",unresolved[i]);
	free(unresolved);
	Err("dld: link aborted", STk_nil);
      }
    }
  }
  else {
    char msg[MAX_PATH_LENGTH];
    sprintf(msg, "Cannot find function \"%s\" in object file", fct_name);
    Err(msg, STk_nil);
  }

  list_of_files = Cons(str, list_of_files);
}
#endif /* LINUX_DLD */

#ifdef HPUX
static void load_and_call(char *path, char *fct_name)
{
  shl_t handle;
  void (*init_fct)();

  if ((handle = shl_load(path, BIND_IMMEDIATE | BIND_VERBOSE, 0L)) == NULL)
    Err("Cannot open file", STk_makestring(path));

  handle = NULL;
  if (shl_findsym(&handle, fct_name, TYPE_PROCEDURE, &init_fct) == -1) {
    char msg[MAX_PATH_LENGTH];

    sprintf(msg, "Cannot find function %s in object file", fct_name);
    Err(msg, STk_nil);
  }
  /* Call the init code */
  (*init_fct)();
}
#endif

void STk_load_object_file(char *path)
{
  char fct_name[1024], *p, *slash;

  /* Load the file as an object one */

  for (p = path, slash = p-1; *p; p++)		/* Find position of last '/' */
    if (*p == '/') slash = p;

#if defined(NETBSB)
  sprintf(fct_name, "_STk_init_%s", slash + 1);
#else
  sprintf(fct_name, "STk_init_%s", slash + 1);
#endif

  for (p = fct_name; *p; p++)			/* Delete suffix it it exists */
      if (*p == '.') { *p = '\0'; break; }

  load_and_call(path, fct_name);
}

#if defined(CYGWIN32)

#define MAKE_STAT_PTR(p) STk_nil
#define MAKE_DYN_PTR(p)  STk_nil


static void initialize_dynload(void)
{
  /* CYGWIN32 */
  Err("dynload: cannot initialize dynload.", STk_makestring(dlerror()));
}


static void load_and_call(char *path, char *fct_name)
{
  /* CYGWIN32 */
  Err("load-and-call: not yet implemented\n", STk_nil);
}

static void *find_function(char *path, char *fname, int error_if_absent)
{
  /* CYGWIN32 */
  Err("find-function: not yet implemented\n", STk_nil);
  return NULL;
}

#endif

/******************************************************************************
 *
 * FFI support
 *
 ******************************************************************************/
#ifndef UNKNOWN_ARCH

#  define EXT_VOID	    0
#  define EXT_CHAR	    1
#  define EXT_SHORT	    2
#  define EXT_USHORT	    3
#  define EXT_INT	    4
#  define EXT_UINT	    5
#  define EXT_LONG  	    6
#  define EXT_ULONG  	    7
#  define EXT_FLOAT 	    8
#  define EXT_DOUBLE	    9
#  define EXT_STAT_PTR 	    10
#  define EXT_DYN_PTR	    11
#  define EXT_STRING	    12
#  define EXT_BOOLEAN	    13

#  define C_INTERFACE          0
#  define ARGC_ARGV_INTERFACE  1	/* Not yet implemented */
#  define WINAPI_INTERFACE     2	/* Not yet implemented */


static void push_argument(char *proc_name, SCM value, SCM name, int type)
{
  int res;

  switch (type) {
    case EXT_VOID:   Serror("cannot push a void type argument", STk_nil); return;


    case EXT_CHAR:   if (INTP(value)) value = STk_integer2char(value);
    		     if (CHARP(value)) {
            	       res = push_char(CHAR(value));
		       break;
    		     }
    		     goto Error;

    case EXT_SHORT:  if (INTP(value)) {
                       res = push_short((short) STk_integer2long(value));
    		       break;
		     }
  		     goto Error;
    case EXT_USHORT: if (INTP(value)) {
                       res = push_short((unsigned short) STk_integer2ulong(value));
    		       break;
		     }
  		     goto Error;
    case EXT_BOOLEAN:res = push_int(value == STk_ntruth ? 0 : 1);
  		     break;
    case EXT_INT:    if (INTP(value)) {
                       res = push_int((int) STk_integer2long(value));
    		       break;
		     }
  		     goto Error;
    case EXT_UINT:   if (INTP(value)) {
                       res = push_int((unsigned int) STk_integer2ulong(value));
    		       break;
		     }
  		     goto Error;
    case EXT_ULONG:  if (INTP(value)) {
                       res = push_long(STk_integer2long(value));
    		       break;
		     }
  		     goto Error;
    case EXT_LONG:   if (INTP(value)) {
                       res = push_long(STk_integer2ulong(value));
    		       break;
		     }
  		     goto Error;

    case EXT_FLOAT:  if (INTP(value)) value = STk_exact2inexact(value);
      		     if (FLONUMP(value)) {
                       res = push_float((float) STk_real2double(value));
    		       break;
		     }
  		     goto Error;
    case EXT_DOUBLE: if (INTP(value)) value = STk_exact2inexact(value);
      		     if (FLONUMP(value)) {
                       res = push_double(STk_real2double(value));
    		       break;
		     }
  		     goto Error;

    case EXT_STRING:
    case EXT_STAT_PTR:
    case EXT_DYN_PTR: if (CPOINTERP(value)) {
      			res = push_ptr(EXTDATA(value));
			break;
    		      } else if (STRINGP(value)) {
			res = push_string(CHARS(value));
			break;
    		      } else if (value == STk_false) {
			res = push_ptr(NULL);
			break;
		      }
		      goto Error;
  }
  /* Verify that the value has been properly pushed */
  if (res == -1) {
    Serror("too many values pushed on the stack", STk_nil);
  }
  return;
Error:
  {
    char message[300];
    sprintf(message, "argument ``%s'' has a bad type", PNAME(name));
    Serror(message, value);
  }
}

static void push_list(char *proc_name, SCM l)
{
  if (!NULLP(l)) {
    SCM x = CAR(l);
    int type;

    if (CHARP(x))	  type = EXT_CHAR;
    else if (INTP(x)) 	  type = EXT_LONG;
    else if (FLONUMP(x))  type = EXT_DOUBLE;
    else if (STRINGP(x))  type = EXT_STRING;
    else if (BOOLEANP(x)) type = EXT_BOOLEAN;
    else
      Serror("not able to pass this argument (bad type)", x);
    push_argument(proc_name, x, STk_false, type);
    push_list(proc_name, CDR(l));
  }
}

static SCM call_function(void *fct, int rettype)
{
  switch (rettype) {
    case EXT_VOID:     call_ext_void((void (*) ())fct); return STk_unsepecified;

    case EXT_CHAR:     return STk_makechar(
				(unsigned char)
				call_ext_char((char (*)()) fct));

    case EXT_SHORT:    return STk_makeinteger(
				call_ext_short((short (*)()) fct));
    case EXT_USHORT:   return STk_makeunsigned(
				call_ext_short((unsigned short (*)()) fct));
    case EXT_INT:      return STk_makeinteger(
				call_ext_int((int (*)()) fct));
    case EXT_UINT:     return STk_makeunsigned(
				call_ext_int((unsigned int (*)()) fct));
    case EXT_LONG:     return STk_makeinteger(
				call_ext_long((long (*)()) fct));
    case EXT_ULONG:	return STk_makeinteger(
				call_ext_long((unsigned long (*)()) fct));

    case EXT_FLOAT:    return STk_makenumber(
				 (double) call_ext_float((float (*)()) fct));
    case EXT_DOUBLE:   return STk_makenumber(
				 call_ext_double((double (*)()) fct));

    case EXT_STAT_PTR: {
      			  void * p = call_ext_ptr((void * (*)()) fct);
			  return p ? MAKE_STAT_PTR(p): STk_false;
    		       }
    case EXT_DYN_PTR:  {
			  void * p = call_ext_ptr((void * (*)()) fct);
			  return p ? MAKE_DYN_PTR(p): STk_false;
		       }
    case EXT_STRING:   {
			  char *s = call_ext_string((char* (*)()) fct);
			  return s ? STk_embed_C_string(s) : STk_false;
		       }

    case EXT_BOOLEAN:  return (call_ext_bool((int (*) ()) fct) ? STk_true: STk_false);
  }
  return STk_unsepecified; /* never reached */
}

PRIMITIVE STk_external_existsp(SCM entry_name, SCM library)
{
  ENTER_PRIMITIVE("%external-exists?");

  if (NSTRINGP(entry_name)) Serror("bad string", entry_name);
  if (NSTRINGP(library))    Serror("bad string", library);

  return find_function(CHARS(library), CHARS(entry_name), FALSE) ? STk_true : STk_false;
}

PRIMITIVE STk_call_external(SCM l, int len)
{
  static char *proc_name = "%call-external";
  SCM libname, entryname, rettype, argnames, argtypes;
  char *c_entryname, *c_libname;
  int  c_rettype;

  if (len < 5) Serror("not enough arguments", l);

  libname   = CAR(l); l = CDR(l);
  entryname = CAR(l); l = CDR(l);
  rettype   = CAR(l); l = CDR(l);
  argnames  = CAR(l); l = CDR(l);
  argtypes  = CAR(l); l = CDR(l);

  /* make some conversions */
  if (!STRINGP(libname))   Serror("bad library name", libname);
  if (!STRINGP(entryname)) Serror("bad entry name", entryname);
  if (!INTEGERP(rettype))  Serror("bad return type", rettype);

  c_rettype   = STk_integer_value(rettype);
  c_entryname = CHARS(entryname);
  c_libname   = CHARS(libname);
  init_ext_call();

  /* Analyse arguments type and "push" them on stack */
  for ( ; ; ) {
    if (NULLP(argnames)) {
      if (!NULLP(l)) STk_procedure_error(c_entryname, "too many arguments", l);
      break;
    }
    if (NCONSP(argnames)) {
      /* We had a &rest */
      push_list(c_entryname, l);
      break;
    }
    if (NULLP(l))
      /* no actual arguments and list of names is not terminated */
      STk_procedure_error(c_entryname, "not enought arguments", argnames);

    /* Standard case */
    push_argument(c_entryname, CAR(l), CAR(argnames),
		  STk_integer_value_no_overflow(CAR(argtypes)));
    l 	     = CDR(l);
    argnames = CDR(argnames);
    argtypes = CDR(argtypes);
  }
  return call_function(find_function(c_libname, c_entryname, TRUE), c_rettype);
}


PRIMITIVE STk_cstring2string(SCM pointer)
{
  static char *proc_name = "c-string->string";
  char *str;

  if (STRINGP(pointer)) str = CHARS(pointer);
  else
    if (CPOINTERP(pointer)) str = (char *) EXTDATA(pointer);
    else
      Serror("bad strng or C pointer", pointer);

  return STk_makestring(str);
}

#else
/* Unknown architecture: no FFI */
static char *msg = "FFI support for this architecture does not exist yet. Sorry!";

PRIMITIVE STk_call_external(SCM l, int len)
{
  ENTER_PRIMITIVE("%call-external");
  Serror(msg, STk_nil);
}

PRIMITIVE STk_external_existsp(SCM entry_name, SCM library)
{
  ENTER_PRIMITIVE("%external-exists?");
  Serror(msg, STk_nil);
}

PRIMITIVE STk_cstring2string(SCM pointer)
{
  ENTER_PRIMITIVE("c-string->string");
  Serror(msg, STk_nil);
}
#endif



#else /* not DYNLOAD */
static char *msg = "FFI support for this architecture does not exist yet. Sorry!";

void STk_load_object_file(char *path)
{
  Err("load: Loading of object file is not defined on this architecture", STk_nil);
}

PRIMITIVE STk_call_external(SCM l, int len)
{
  ENTER_PRIMITIVE("%call-external");
  Serror(msg, STk_nil);
}

PRIMITIVE STk_external_existsp(SCM entry_name, SCM library)
{
  ENTER_PRIMITIVE("%external-exists?");
  Serror(msg, STk_nil);
}

PRIMITIVE STk_cstring2string(SCM pointer)
{
  ENTER_PRIMITIVE("c-string->string");
  Serror(msg, STk_nil);
}
#endif
