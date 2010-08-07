/*
 * stklos.c	-- STklos interpreter main function
 * 
 * Copyright © 1999-2010 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 * Last file update:  7-Aug-2010 10:02 (eg)
 */

#include <stklos.h>
#include "gnu-getopt.h"


#define ADD_OPTION(o, k)  					\
  if (*o) options = STk_key_set(options, 			\
				STk_makekey(k), 		\
				STk_Cstring2string(o));

#define ADD_BOOL_OPTION(o, k)  				\
  options = STk_key_set(options, 			\
			STk_makekey(k), 		\
			MAKE_BOOLEAN(o));

#define ADD_INT_OPTION(o, k)  				\
  options = STk_key_set(options, 			\
			STk_makekey(k), 		\
			MAKE_INT(o));


/*=============================================================================
 *
 * Program arguments
 *
 *=============================================================================
 */

static char *boot_file    = "";
static char *program_file = "";
static char *load_file    = "";
static char *conf_dir	  = "";
static char *sexpr        = "";
static int  vanilla	  = 0;
static int  stack_size    = DEFAULT_STACK_SIZE;
static int  debug_mode    = 0;
static int  line_editor   = 1;

static struct option long_options [] = 
{
  {"version",      	no_argument,       NULL, 'v'},
  {"file",         	required_argument, NULL, 'f'},
  {"load",         	required_argument, NULL, 'l'},
  {"execute",      	required_argument, NULL, 'e'},
  {"boot-file",    	required_argument, NULL, 'b'},
  {"conf-dir",		required_argument, NULL, 'D'},
  {"no-init-file", 	no_argument,       NULL, 'q'},
  {"interactive",  	no_argument,	   NULL, 'i'},
  {"no-line-editor",  	no_argument,	   NULL, 'n'},
  {"debug",		no_argument,	   NULL, 'd'},
  {"stack-size",   	required_argument, NULL, 's'},
  {"case-sensitive",	no_argument,	   NULL, 'c'},
  {"help",         	no_argument,       NULL, 'h'},
  {NULL}
};

static void Usage(char *progname, int only_version)
{
  fprintf(stderr, "%s (version %s)\n", progname, VERSION);
  if (only_version) return;
  fprintf(stderr, "Usage: %s [option ...] [--] [arg ... ]", progname);
  fprintf(stderr, "\n"
"Possible options:\n"
"   -l file, --load=file        load 'file' before going interactive\n"
"   -f file, --file=file        use 'file' as program\n"
"   -e sexpr, --execute=sexpr   evaluate the given sexpr and exit\n"
"   -b file, --boot-file=file   use 'file' to boot the system\n"
"   -D dir, --conf-dir=dir      change configuration dir (default: ~/.stklos)\n"
"   -q, --no-init-file          quiet: do not load the user init file\n"
"   -i, --interactive           interactive mode\n"
"   -n, --no-line-editor        don't use line editor\n"
"   -d, --debug                 add informations to ease debugging\n"
"   -s, --stack-size=n          use a stack of size n (default %d)\n"
"   -c, --case-sensitive        be case sensitive (default is #f)\n"
"   -v, --version               print program version and exit\n"
"   -h, --help                  print this help and exit\n" 
"All the arguments given after options are passed to the Scheme program.\n",
DEFAULT_STACK_SIZE);
}


static int process_program_arguments(int argc, char *argv[])
{
  extern char *optarg;
  extern int optind;
  int c;

  for ( ; ; ) {
    c = getopt_long(argc, argv, "qidnvhcf:l:e:b:s:D:", long_options, NULL);
    if (c == -1) break;
    
    switch (c) {
      case 'v': Usage(*argv, 1); exit(0);
      case 'f': program_file    = optarg; 	break;
      case 'l': load_file       = optarg; 	break;
      case 'e': sexpr	        = optarg; 	break;
      case 'b': boot_file       = optarg; 	break;
      case 'D': conf_dir        = optarg;	break;
      case 'i': STk_interactive = 1;      	break;
      case 'n': line_editor     = 0;      	break;
      case 'd': debug_mode++;			break;
      case 'q': vanilla         = 1;	  	break;
      case 's': stack_size      = atoi(optarg); break;
      case 'c': STk_read_case_sensitive = 1;	break;
      case '?': /* message error is printed by getopt */
		fprintf(stderr, "Try `%s --help' for more information\n", *argv);
		exit(1);
      default:  Usage(*argv, 0); exit(c != 'h');
    }
  }
  return optind;
}

static void  build_scheme_args(int argc, char *argv[], char *argv0)
{
  SCM options, l = STk_nil;
  int i;

  for (i = argc-1; i >= 0; i--)
    l = STk_cons(STk_Cstring2string(argv[i]), l);

  options = LIST2(STk_makekey(":argv"), l);
  ADD_OPTION(argv0,	   	   ":program-name");
  ADD_OPTION(program_file, 	   ":file");
  ADD_OPTION(load_file,    	   ":load");
  ADD_OPTION(sexpr, 	   	   ":sexpr");
  ADD_OPTION(conf_dir,		   ":conf-dir");
  ADD_BOOL_OPTION(vanilla,	   ":no-init-file");
  ADD_BOOL_OPTION(STk_interactive, ":interactive");
  ADD_BOOL_OPTION(line_editor,     ":line-editor");
  ADD_INT_OPTION(debug_mode,   	   ":debug");

  STk_define_variable(STk_intern("*%program-args*"), options, 
		      STk_STklos_module);
}

int main(int argc, char *argv[])
{
  int ret;
  char *argv0 = *argv;

  /* Initialize the Garbage Collector */
#if (defined(__CYGWIN32__) &&  defined(GC_DLL)) || defined(_AIX)
# error GC problem
#endif
  STk_gc_init();

  /* Process command arguments */
  ret = process_program_arguments(argc, argv);
  argc -= ret;
  argv += ret;
  
  /* Hack: to give the illusion that there is no VM under the scene */
  if (*program_file) argv0 = program_file;

  /* Initialize the library */
  if (!STk_init_library(&argc, &argv, stack_size)) {
    fprintf(stderr, "cannot initialize the STklos library\nABORT\n");
    exit(1);
  }

  /* Place the interpreter arguments in the Scheme variable *%program-args* */
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
