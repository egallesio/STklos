/*                                                      -*- coding: utf-8 -*-
 * m i s c . c          -- Misc. functions
 *
 * Copyright © 2000-2023 Erick Gallesio <eg@stklos.net>
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
 *    Creation date:  9-Jan-2000 12:50 (eg)
 */

#include <limits.h>
#include "stklos.h"
#include "gnu-getopt.h"
#include "git-info.h"

#ifdef STK_DEBUG
  #ifdef HAVE_BACKTRACE
     #include <execinfo.h>
  #endif

int STk_interactive_debug = 0;
#endif

#define BSIZEOF(t) ((int) (sizeof(t) * CHAR_BIT))

static void error_bad_string(SCM str)
{
  STk_error("bad string ~S", str);
}


char *STk_strdup(const char *s)
{
  /* Like standard strdup but with our allocator */
  char *res;
  register size_t len = strlen(s);

  res = STk_must_malloc_atomic(len + 1);
  memcpy(res, s, len+1);
  return res;
}


void STk_add_primitive(struct primitive_obj *o)
{
  SCM symbol;

  symbol = STk_intern(o->name);
  STk_define_variable(symbol, (SCM) o, STk_STklos_module);
}

void STk_add_primitive_in_module(struct primitive_obj *o, SCM module)
{
  SCM symbol;

  symbol = STk_intern(o->name);
  STk_define_variable(symbol, (SCM) o, module);
}




SCM STk_eval_C_string(const char *str, SCM module)
{
  SCM ref, eval = STk_lookup(STk_intern("eval-from-string"),
                             module,
                             &ref,
                             TRUE);
  return STk_C_apply(eval, 2, STk_Cstring2string(str), module);
}


SCM STk_read_from_C_string(const char *str)
{
  return STk_read(STk_open_C_string(str), STk_read_case_sensitive);
}


/*===========================================================================*\
 *
 * Primitives that don't feet anywhere else
 *
\*===========================================================================*/
/*
<doc EXT implementation-version version
 * (version)
 * (implementation-version)
 *
 * Returns a string identifying the current version of the system. A
 * version is constituted of two numbers separated by a point: the version
 * and the release numbers. Note that |implementation-version| corresponds
 * to the {{link-srfi 112}} name of this function.
doc>
 */
DEFINE_PRIMITIVE("version", version, subr0, (void))
{
  return STk_Cstring2string(FULL_VERSION);
}

DEFINE_PRIMITIVE("%stklos-configure", stklos_configure, subr0, (void))
{
  char buffer[2000];
  SCM z = STk_nil;

  /* The SRFI-176 c.version property */
  z = STk_append2(z, LIST2(STk_makekey("c-version"),
                           STk_Cstring2string(C_VERSION)));

  /* The SRFI-176 c.compile property */
  z = STk_append2(z, LIST2(STk_makekey("c-compile"),
                           STk_read_from_C_string(C_COMPILE)));

  /* The SRFI-176 c.link property */
  z = STk_append2(z, LIST2(STk_makekey("c-link"),
                           STk_read_from_C_string(C_LINK)));

  /* The SRFI-176 c.type-bits property */
  snprintf(buffer,
           sizeof(buffer),
           "(:c-type-bits ((char %d) (short %d) (int %d) (long %d) "
           "(float %d) (double %d) (pointer %d)))",
           BSIZEOF(char), BSIZEOF(short), BSIZEOF(int), BSIZEOF(long),
           BSIZEOF(float), BSIZEOF(double), BSIZEOF(SCM));
  z = STk_append2(z, STk_read_from_C_string(buffer));

  /* The SRFI-176 stklos.shlib.compile property */
  z = STk_append2(z, LIST2(STk_makekey("shlib-compile"),
                           STk_read_from_C_string(SHARED_LIB_COMPILE)));

  /* The SRFI-176 stklos.shlib.link property */
  z = STk_append2(z, LIST2(STk_makekey("shlib-link"),
                           STk_read_from_C_string(SHARED_LIB_LINK)));

  /* The SRFI-176 stklos.shlib.suffix property */
  z = STk_append2(z, LIST2(STk_makekey("shlib-suffix"),
                           STk_Cstring2string(SHARED_EXTENSION)));


  /* Add information gathered during configuration */
  z = STk_append2(z, STk_read_from_C_string(CONF_SUMMARY));

  return z;
}

DEFINE_PRIMITIVE("%stklos-git", stklos_git, subr0, (void))
{
  return STk_read_from_C_string(GIT_SUMMARY);
}

/*
<doc EXT void
 * (void)
 * (void arg1 ...)
 *
 * Returns the special *_void_* object. If arguments are passed to |void|,
 * they are evalued and simply ignored.
doc>
 */
DEFINE_PRIMITIVE("void", scheme_void, vsubr, (int _UNUSED(argc), SCM _UNUSED(*argv)))
{
  return STk_void;
}


/*
<doc EXT address-of
 * (address-of obj)
 *
 * Returns the address of the object |obj| as an integer.
doc>
*/
DEFINE_PRIMITIVE("address-of", address_of, subr1, (SCM object))
{
  char buffer[50];     /* should be sufficient for a while */

  snprintf(buffer, sizeof(buffer),
           "%lx", (unsigned long) object); /* not very efficient ... */
  return STk_Cstr2number(buffer, 16L);
}


/*===========================================================================*\
 *
 * GC stuff
 *
\*===========================================================================*/
static void GC_warning_handler(char *msg, GC_word arg)
{
  char buffer[512];

  if (strstr(msg, "cycle")) return;
  snprintf(buffer, sizeof(buffer), msg, arg);
  fprintf(stderr, "*** %s", buffer);
  if (strstr(msg, "Returning NULL")) STk_error("OUT of memory");
}



void STk_gc_init(void)
{
  GC_init();
  /* Consider a GC warning as errors. Abort as soon as we encounter one */
  GC_set_warn_proc(GC_warning_handler);
}


/*
<doc EXT gc
 * (gc)
 *
 * Force a garbage collection step.
doc>
*/
DEFINE_PRIMITIVE("gc", scheme_gc, subr0, (void))
{
  STk_gc();
  return STk_void;
}


/*===========================================================================*\
 *
 *                      Argument parsing
 *
\*===========================================================================*/
static int Argc;
static char * optstring;
static char **Argv;
static struct option *long_options;


DEFINE_PRIMITIVE("%initialize-getopt", init_getopt, subr3, (SCM argv, SCM s, SCM v))
{
  int i, len;

  STk_start_getopt_from_scheme();
  optind = 1;    /* Initialize optind, since it has already been used
                  * by ourselves before initializing the VM.
                  */

  /*
   * Argv processing
   */
  len = STk_int_length(argv);
  if (len < 0) STk_error("bad argument list ~S", argv);
  Argv = STk_must_malloc_atomic((len+1) * sizeof(char *));
  for (i = 0; i < len; i++) {
    if (!STRINGP(CAR(argv))) error_bad_string(CAR(argv));
    Argv[i] = STRING_CHARS(CAR(argv));
    argv    = CDR(argv);
  }
  Argv[len] = NULL;
  Argc      = len;

  /*
   * Optstring
   */
  if (!STRINGP(s)) error_bad_string(s);
  optstring = STRING_CHARS(s);

  /*
   * Option vector processing
   */
  if (!VECTORP(v)) STk_error("bad vector ~S", v);
  len = VECTOR_SIZE(v);
  /* If there is an else clause, last item of the vector is #f */
  if (VECTOR_DATA(v)[len-1] == STk_false) len -= 1;

  long_options = STk_must_malloc_atomic((len+1) * sizeof(struct option));

  /* Copy the values in v in the long_options array */
  for (i=0; i < len; i ++) {
    if (!STRINGP(CAR(VECTOR_DATA(v)[i]))) error_bad_string(CAR(VECTOR_DATA(v)[i]));

    long_options[i].name    = STRING_CHARS(CAR(VECTOR_DATA(v)[i]));
    long_options[i].has_arg = (CDR(VECTOR_DATA(v)[i]) == STk_false) ? no_argument
                                                                : required_argument;
    long_options[i].flag    = 0;
    long_options[i].val     = 0;
  }

  long_options[len].name = NULL; long_options[len].has_arg = 0;
  long_options[len].flag = NULL; long_options[len].val     = 0;

  return STk_void;
}

DEFINE_PRIMITIVE("%getopt", getopt, subr0, (void))
{
  int  n, longindex;

  n = getopt_long(Argc, Argv, optstring, long_options, &longindex);

  switch (n) {
    case -1:
      {
        /* We are at the end. Collect all the remaining parameters in a list */
        SCM l = STk_nil;
        while (optind < Argc)
          l = STk_cons(STk_Cstring2string(Argv[optind++]), l);

        return STk_cons(MAKE_INT(-1UL), STk_dreverse(l));
      }
    case '?': /* Error or argument missing */
    case ':': return STk_false;
    case 0  : /* Long option */
      {
        SCM str = (optarg)? STk_Cstring2string(optarg): STk_void;
        return STk_cons(MAKE_INT(longindex),str);
      }
    default: /* short option */
      {
        SCM str = (optarg)? STk_Cstring2string(optarg): STk_void;
        return STk_cons(MAKE_CHARACTER(n),  str);
      }
  }
}

/*===========================================================================*\
 *
 *                      HTML stuff
 *
\*===========================================================================*/

/*
static char URI_regexp[] =
  "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\\\?([^#]*))?(#(.*))?";
*/

/*
 * URI parsing (RFC2396)
 *
 */

/*
<doc EXT uri-parse
 * (uri-parse str)
 *
 * Parses the string |str| as an RFC-2396 URI and return a keyed list with the
 * following components
 *
 * - |scheme| : the scheme used as a string (defaults to |"file"|)
 * - |user|: the user information (generally expressed as |login:password|)
 * - |host| : the host as a string (defaults to "")
 * - |port| : the port as an integer (0 if no port specified)
 * - |path| : the path
 * - |query| : the qury part of the URI as a string (defaults to the
 *    empty string)
 * - |fragment| : the fragment of the URI as a string (defaults to the
 *   empty string)
 *
 * @lisp
 * (uri-parse "https://stklos.net")
{*    => (:scheme "https" :user "" :host "stklos.net" :port 443
 *         :path "/" :query "" :fragment "")
 *
 * (uri-parse "https://stklos.net:8080/a/file?x=1;y=2#end")
 *     => (:scheme "http" :user "" :host "stklos.net" :port 8080
 *         :path "/a/file" :query "x=1;y=2" :fragment "end")
 *
 * (uri-parse "http://foo:secret@stklos.net:2000/a/file")
 *     => (:scheme "http" :user "foo:secret" :host "stklos.net"
 *         :port 2000  :path "/a/file" :query "" :fragment "")
 *
 * (uri-parse "/a/file")
 *    => (:scheme "file" :user "" :host "" :port 0 :path "/a/file"
 *        :query "" :fragment "")
 *
 * (uri-parse "")
 *    => (:scheme "file"  :user "" :host "" :port 0 :path ""
 *        :query "" :fragment "")
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("uri-parse", uri_parse, subr1, (SCM url_str))
{
  SCM file, tmp, scheme, host, port, path, query, fragment, user;
  SCM *vect;
  char *url;

  if (!STRINGP(url_str)) error_bad_string(url_str);

  scheme = file = STk_Cstring2string("file");
  url    = STRING_CHARS(url_str);

  if (!strstr(url,"://")) {                     /* No :// => this is a file */
    port   = MAKE_INT(0);
    path   = url_str;
    host = query = fragment = user = STk_Cstring2string("");
  } else {                                      /* general URI */
    char *start;
    /* Scheme */
    for (start = url; *url && *url != ':'; url++) {
    }
    if (strncmp(url, "://", 3) != 0) goto Error;
    scheme = STk_makestring(url-start, start);
    STk_string_ddowncase(1, &scheme);
    if ((STk_equal(scheme, file) != STk_false) && (strncmp(url, ":///", 4) != 0))
      /* URI such as file://tmp/X produce host="tmp" and file "/X"
       * (as mozilla). It is incorrect, but this is a common mistake,
       * so we try to fix input here */
      url += 2;
    else
      url += 3;

    /* Host or user@host. Look forward to see if we have an '@'*/
    for (start = url; *url && *url != '/' && *url != '@'; url++) {
    }
    if (*url == '@') {
      /* We have a user */
      user = STk_makestring(url-start, start);
      /* read the host now */
      for (start = ++url; *url && *url != '/' && *url != ':'; url++) {
      }
      host = STk_makestring(url-start, start);
    } else {
      /* no user info: backtrack since we may have skipped a ":" */
      for (url = start; *url && *url != '/' && *url != ':'; url++) {
      }
      host = STk_makestring(url-start, start);
      user = STk_Cstring2string("");
    }

    /* Port */
    if (*url == ':') {
      url += 1;
      for (start = url; *url && *url != '/'; url++) {
      }
      tmp  = STk_makestring(url-start, start);
      port = STk_Cstr2number(STRING_CHARS(tmp), 10);
      if (port == STk_false)
        STk_error("bad port number in URL ~S", url_str);
    } else {
      char *scm = STRING_CHARS(scheme);

      if (strcmp(scm, "http") == 0)       port = MAKE_INT(80);
      else if (strcmp(scm, "https") == 0) port = MAKE_INT(443);
      else if (strcmp(scm, "ftp") == 0)   port = MAKE_INT(21);
      else port = MAKE_INT(0);
    }
    if (*url) url += 1;

    /* Path */
    if (*url) {
      for (start = url; *url && *url != '?' && *url != '#'; url++) {
      }
      path = STk_makestring(url-start+1, start-1);
    } else {
      path = STk_Cstring2string("/");
    }

    /* Query */
    if (*url == '?') {
      url += 1;
      for (start = url; *url && *url != '#'; url++) {
      }
      query = STk_makestring(url-start, start);
    } else
      query = STk_Cstring2string("");

    /* Fragment */
    fragment = STk_Cstring2string((*url == '#') ? url+1: "");
  }

  /* Build the result */
  tmp  = STk_makevect(14, STk_false);
  vect = VECTOR_DATA(tmp);

  vect[0]  = STk_makekey("scheme");   vect[1]  = scheme;
  vect[2]  = STk_makekey("user");     vect[3]  = user;
  vect[4]  = STk_makekey("host");     vect[5]  = host;
  vect[6]  = STk_makekey("port");     vect[7]  = port;
  vect[8]  = STk_makekey("path");     vect[9]  = path;
  vect[10] = STk_makekey("query");    vect[11]  = query;
  vect[12] = STk_makekey("fragment"); vect[13] = fragment;

  return STk_vector2list(tmp);
Error:
  STk_error("bad URL ~S", url_str);
  return STk_void;
}

/*
<doc EXT string->html
 * (string->html str)
 *
 * This primitive is a convenience function; it returns a string where
 * the HTML special chars are properly translated. It can easily be written
 * in Scheme, but this version is fast.
 * @lisp
 * (string->html "Just a <test>")
 *    => "Just a &lt;test&gt;"
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("string->html", str2html, subr1, (SCM str))
{
  char *s, *d;
  int i, len, size = 0;
  SCM z;

  if (!STRINGP(str)) error_bad_string(str);

  s   = STRING_CHARS(str);
  len = STRING_SIZE(str);

  /* Compute the size of the result string */
  for (size=0, i=len; i; size++, i--) {
    switch (*s++) {
      case '\'': size += 5; break;
      case '<':
      case '>': size += 3; break;
      case '&': size += 4; break;
    }
  }

  if (size == len)
    /* No special character in the string */
    return str;
  else {
    /* Build the result string */
    z = STk_makestring(size, NULL);
    d = STRING_CHARS(z);

    for (s = STRING_CHARS(str); len--; s++) {
      switch (*s) {
      case '\'':
        d[0]='&'; d[1]='q'; d[2]='u'; d[3]='o'; d[4]='t'; d[5]=';';
        d += 6;
        break;
      case '<':
        d[0]='&'; d[1]='l'; d[2]='t'; d[3]=';';
        d += 4;
        break;
      case '>':
        d[0]='&'; d[1]='g'; d[2]='t'; d[3]=';';
        d += 4;
        break;
      case '&':
        d[0]='&'; d[1]='a'; d[2]='m'; d[3]='p'; d[4]=';';
        d += 5;
        break;
      default:
        *d++ = *s;
      }
    }

    return z;
  }
}

/*
<doc EXT get-password
 * (get-password)
 *
 * This primitive permits to enter a password (character echoing
 * being turned off). The value returned by |get-password| is the entered
 * password as a string.
doc>
*/
DEFINE_PRIMITIVE("get-password", getpass, subr0, (void))
{
  char *s;
  SCM z;

  s = getpass("");
  if (!s) STk_error("terminal not available");

   z = STk_Cstring2string(s);
   memset(s, '\0', strlen(s)); /* more secure? */
   return z;
}


/*===========================================================================*\
 *
 *                      Debugging Code
 *
\*===========================================================================*/
#ifdef STK_DEBUG

#define BACKTRACE_SIZE 1024

DEFINE_PRIMITIVE("%%debug", set_debug, subr0, (void))
{
  STk_interactive_debug = !STk_interactive_debug;
  STk_debug("Debug mode %d", STk_interactive_debug);
  return STk_void;
}

DEFINE_PRIMITIVE("%test", test, subr1, (SCM s))
{
  /* A special place for doing tests */
  return STk_C_apply(s, 0);
}

DEFINE_PRIMITIVE("%c-backtrace", c_backtrace, subr0, (void))
{
# ifdef HAVE_BACKTRACE
  void *buffer[BACKTRACE_SIZE];
  int n;

  n = backtrace(buffer, BACKTRACE_SIZE);
  if (n >= BACKTRACE_SIZE) {
    STk_debug("***** Backtrace truncated to %d entries\n", n);
  }

  backtrace_symbols_fd(buffer, n, 2);
#else
  STk_debug("backtrace is not available on this system. Sorry.");
#endif
  return STk_void;
}
#endif

/*===========================================================================*\
 *
 *                              Initialization
 *
\*===========================================================================*/
int STk_init_misc(void)
{
  ADD_PRIMITIVE(version);
  ADD_PRIMITIVE(stklos_configure);
  ADD_PRIMITIVE(stklos_git);
  ADD_PRIMITIVE(scheme_void);
  ADD_PRIMITIVE(address_of);
  ADD_PRIMITIVE(scheme_gc);

  ADD_PRIMITIVE(init_getopt);
  ADD_PRIMITIVE(getopt);
  ADD_PRIMITIVE(getpass);

  ADD_PRIMITIVE(uri_parse);
  ADD_PRIMITIVE(str2html);

#ifdef STK_DEBUG
  ADD_PRIMITIVE(set_debug);
  ADD_PRIMITIVE(test);
  ADD_PRIMITIVE(c_backtrace);
#endif
  return TRUE;
}
