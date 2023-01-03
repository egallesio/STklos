/*
 * stklos.c     -- STklos interpreter main function
 *
 * Copyright © 1999-2023 Erick Gallesio <eg@stklos.net>
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
 *    Creation date: 28-Dec-1999 21:19 (eg)
 * Last file update:  3-Jan-2023 15:28 (eg)
 */

#include "stklos.h"
#include <langinfo.h>
#include "gnu-getopt.h"

#define ADD_OPTION(o, k)                                     do{\
  if (*o) options = STk_key_set(options,                        \
                                STk_makekey(k),                 \
                                STk_Cstring2string(o));         \
}while(0)

#define ADD_BOOL_OPTION(o, k)                        do{\
  options = STk_key_set(options,                        \
                        STk_makekey(k),                 \
                        MAKE_BOOLEAN(o));               \
}while(0)

#define ADD_INT_OPTION(o, k)                         do{\
  options = STk_key_set(options,                        \
                        STk_makekey(k),                 \
                        MAKE_INT(o));                   \
}while(0)

#define ADD_SCM_OPTION(o, k)                         do{\
  options = STk_key_set(options, STk_makekey(k),o);     \
}while(0)

/*=============================================================================
 *
 * Program arguments
 *
 *=============================================================================
 */

static char *boot_file    = "";
static char *program_file = "";
static char *load_file    = "";
static char *conf_dir     = "";
static char *sexpr        = "";
static int  vanilla       = 0;
static int  stack_size    = DEFAULT_STACK_SIZE;
static int  debug_mode    = 0;
static int  line_editor   = 1;
static int  srfi_176      = 0;
static char *script_file  = "";
static SCM  Idirs         = STk_nil;
static SCM  Adirs         = STk_nil;


static struct option long_options [] =
{
  {"version",           no_argument,       NULL, 'v'},
  {"file",              required_argument, NULL, 'f'},
  {"prepend-load-path", required_argument, NULL, 'I'},
  {"append-load-path",  required_argument, NULL, 'A'},
  {"load",              required_argument, NULL, 'l'},
  {"execute",           required_argument, NULL, 'e'},
  {"boot-file",         required_argument, NULL, 'b'},
  {"conf-dir",          required_argument, NULL, 'D'},
  {"no-init-file",      no_argument,       NULL, 'q'},
  {"interactive",       no_argument,       NULL, 'i'},
  {"no-line-editor",    no_argument,       NULL, 'n'},
  {"debug",             no_argument,       NULL, 'd'},
  {"stack-size",        required_argument, NULL, 's'},
  {"case-sensitive",    no_argument,       NULL, 'c'},
  {"case-insensitive",  no_argument,       NULL, 'z'},
  {"utf8-encoding",     required_argument, NULL, 'u'},
  {"help",              no_argument,       NULL, 'h'},
  {NULL,                0,                 NULL, 0  }     /* for Clang */
};

static void SimpleVersion(void)
{
  printf("stklos %s\n", VERSION);
  printf("For more information, use the -V option.\n");
}

static void Usage(FILE *stream)
{
  fprintf(stream, "stklos %s\n", VERSION);
  fprintf(stream, "Usage: stklos [option ...] [--] [arg ... ]");
  fprintf(stream, "\n"
"Possible options:\n"
"   -l file, --load=file            load 'file' before going interactive\n"
"   -f file, --file=file            use 'file' as program\n"
"   -e sexpr, --execute=sexpr       evaluate the given sexpr and exit\n"
"   -b file, --boot-file=file       use 'file' to boot the system\n"
"   -D dir, --conf-dir=dir          change configuration dir (default: ~/.stklos)\n"
"   -I dir, --prepend-load-path=dir prepend 'dir' to the load path list.\n"
"   -A dir, --append-load-path=dir  append 'dir' to the load path list.\n"
"   -q, --no-init-file              quiet: do not load the user init file\n"
"   -i, --interactive               interactive mode\n"
"   -n, --no-line-editor            don't use line editor\n"
"   -d, --debug                     add information to ease debugging\n"
"   -s, --stack-size=n              use a stack of size n (default %d)\n"
"   -c, --case-sensitive            be case sensitive by default\n"
"       --case-insensitive          be case insensitive by default\n"
"   -u, --utf8-encoding=yes|no      use/don't use UTF-8 encoding\n"
"   -v, --version                   show version and exit (simple)\n"
"   -V                              show version and exit (detailed, SRFI-176)\n"
"   -h, --help                      show this help and exit\n"
"All the arguments given after options are passed to the Scheme program.\n",
DEFAULT_STACK_SIZE);
}


static int process_program_arguments(int argc, char *argv[], int *pos)
{
  extern char *optarg;
  extern int optind;
  int c;

  // Retain the initial position of the first "--" (if any) BEFORE the getopt
  // function eventually displaces it to the left in the argv array (since it
  // reorders the arguments to place the options before parameters. 
  *pos = -1;
  for (int i =0; i < argc; i++) { /* Retain the position of a "--" in the arg list */
    if (strcmp(argv[i], "--") == 0) { *pos = i; break; }
  }


  for ( ; ; ) {
    c = getopt_long(argc, argv, "qidnvVhcf:l:e:b:s:D:I:A:u:", long_options, NULL);
    if (c == -1) break;

    switch (c) {
      case 'v': SimpleVersion(); exit(0);
      case 'V': srfi_176        = 1;                                    break;
      case 'I': Idirs = STk_cons(STk_Cstring2string(optarg), Idirs);    break;
      case 'A': Adirs = STk_cons(STk_Cstring2string(optarg), Adirs);    break;
      case 'f': program_file    = optarg;
                script_file     = STk_expand_file_name(optarg);         break;
      case 'l': load_file       = optarg;                               break;
      case 'e': sexpr           = optarg;                               break;
      case 'b': boot_file       = optarg;                               break;
      case 'D': conf_dir        = optarg;                               break;
      case 'i': STk_interactive = 1;                                    break;
      case 'n': line_editor     = 0;                                    break;
      case 'd': debug_mode++;                                           break;
      case 'q': vanilla         = 1;                                    break;
      case 's': stack_size      = atoi(optarg);                         break;
      case 'c': STk_read_case_sensitive = 1;                            break;
      case 'z': STk_read_case_sensitive = 0;                            break;
      case 'u': STk_use_utf8    = strspn(optarg, "yY1");                break;
      case 'h': Usage(stdout); exit(0);
      case '?': /* message error is printed by getopt */
                fprintf(stderr, "Try `%s --help' for more information\n", *argv);
                exit(1);
      default:  Usage(stderr); exit(1);
    }
  }
  return optind;
}

static void  build_scheme_args(int argc, char *argv[], char *argv0)
{
  SCM options, l = STk_nil;
  int i;

  for (i = argc-1; i >= 0; i--) {
    l = STk_cons(STk_Cstring2string(argv[i]), l);
  }
  options = LIST2(STk_makekey("argv"), l);
  ADD_OPTION("STklos",             "name");
  ADD_OPTION(argv0,                "program-name");
  ADD_OPTION(program_file,         "file");
  ADD_OPTION(load_file,            "load");
  ADD_OPTION(sexpr,                "sexpr");
  ADD_OPTION(conf_dir,             "conf-dir");
  ADD_BOOL_OPTION(srfi_176,        "srfi-176");
  ADD_BOOL_OPTION(vanilla,         "no-init-file");
  ADD_BOOL_OPTION(STk_interactive, "interactive");
  ADD_BOOL_OPTION(line_editor,     "line-editor");
  ADD_INT_OPTION(debug_mode,       "debug");
  ADD_BOOL_OPTION(STk_use_utf8,    "use-utf8");
  ADD_OPTION(script_file,          "script-file");
  ADD_SCM_OPTION(Idirs,            "prepend-dirs");
  ADD_SCM_OPTION(Adirs,            "append-dirs");

  STk_define_variable(STk_intern("*%system-state-plist*"), options,
                      STk_STklos_module);
}

int main(int argc, char *argv[])
{
  int ret;
  char *argv0 = *argv;
  int pos__ = -1;  // position of first "--"  in arguments

  /* Initialize the Garbage Collector */
#if (defined(__CYGWIN32__) &&  defined(GC_DLL)) || defined(_AIX)
# error GC problem
#endif
  STk_gc_init();

  /* Process command arguments */
  ret = process_program_arguments(argc, argv, &pos__);
  argc -= ret;
  argv += ret;

  if (!*program_file && argc) {
    // We have at least one argument. Use it as if we had a -f option
    // (except if a "--" was just before the first argument).
    // Hence, "stklos -- foo.stk 1 2" will not call foo.stk, whereas
    // "stklos foo.stk -- 1 2" will.
    if (pos__ + 1 != ret) {
      /* the "--" was not before the script name (before getopt reordering) */
      program_file = *argv++;
      argc-=1;
      script_file = STk_expand_file_name(program_file);
    }
  }

  /* See if we use UTF8 encoding */
  if (!setlocale(LC_ALL, "")) {
    fprintf(stderr, "Can't set the specified locale! "
            "Check LANG, LC_CTYPE, LC_ALL.\n");
    return 1;
  }

  if (STk_use_utf8 == -1) {
    /* user didn't force the encoding. Determine it from environment */
    STk_use_utf8 = (strcasecmp(nl_langinfo(CODESET), "UTF-8") == 0);
  }

  /* Hack: to give the illusion that there is no VM under the scene */
  if (*program_file) argv0 = program_file;

  /* Initialize the library */
  if (!STk_init_library(&argc, &argv, stack_size)) {
    fprintf(stderr, "cannot initialize the STklos library\nABORT\n");
    exit(1);
  }

  /* Place CLI arguments in the Scheme variable *%system-state-plist* */
  build_scheme_args(argc, argv, argv0);

  /* Boot the VM */
  if (*boot_file)
    /* We have a boot specified on the command line */
    ret = STk_load_boot(boot_file);
  else
    /* Use The boot in C file */
    ret = STk_boot_from_C();

  if (ret < 0) {
    fprintf(stderr, "cannot boot with \"%s\" file (code=%d)\n", boot_file, ret);
    exit(1);
  }

  STk_pre_exit(MAKE_INT(ret));
  return ret;
}
