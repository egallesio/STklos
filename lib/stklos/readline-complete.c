/*
 * readline-complete.c   -- tab-completion for readline
 *
 * Copyright © 2022 Jerônimo Pellegrini <j_p@aleph0.info>
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
 *           Author: Jerônimo Pellegrini [j_p@aleph0.info]
 *    Creation date: 09-May-2022 09:22
 * Last file update: 23-May-2022 17:55 (eg)
 */

#include <stklos.h>
#include "readline-complete-incl.c"

//
// Readline interface for completion
//
// When using completion we need some variables and function from
// "readline/readline.h" However, since we don't use autoconf for locating the
// installation directory of readline (or libedit), we just declare what we need
// here. It permits also to compile this file, without libredline or libedit.
//
// NOTE: STklos does not try to link the GNU readline library since on BSD
// system, it is generally not present (plus the complex interaction of LGPL
// and GPL). Hence, autoconf doesn't try to locate any editing library and,
// the REPL just tries to link against a line editing library,at runtime, if
// it finds one such lib.

/* Readline typedefs for the completion system */
typedef char *rl_compentry_func_t(const char *, int);
typedef char **rl_completion_func_t(const char *, int, int);

/* Readline function used */
extern char **rl_completion_matches(const char *, rl_compentry_func_t *);

/* Readline variables used */
extern char *rl_line_buffer;                      // The line buffer
extern int rl_attempted_completion_over;          // 1 to suppress filename completion
extern char *rl_completer_word_break_characters;  // word deparator. Normally "n\"\\'`@$>"
extern rl_completion_func_t *rl_attempted_completion_function; // Pointer on our completion func


SCM gen;

/*
  Calls the generator (gen) with two arguments:
  1. the text to be matched
  2. the state:
     - #f if this is the first call (and we want the first match from
       the list)
     - #t if this is a subsequent call (and we want one more match).
  This is how readline works.
 */
char *
generator(const char *text, int state) {
  SCM s = STk_C_apply(gen,2,
                      STk_Cstring2string((char *)text),
                      MAKE_BOOLEAN(state));
  if (s == STk_nil) return NULL;
  size_t size = STRING_SIZE(s);
  /* We MUST call malloc, and not use libgc, because readline will attempt
     to free the pointer (at least on FresBSD). */
  char *res = malloc(size+1);
  strncpy(res,STRING_CHARS(s), size);
  res[size]=0;
  return res;
}

/* STk_completion is the function passed to libreadline to do tab
   completion. Its arguments are
   - The partially typed string
   - The start and end positions, which we ignore.
   The actualy work is done by the generator function in this file,
   which in turn calls the Scheme procedure complete in
   readline-complete.stk. */
char **
STk_completion(const char *str, int _UNUSED(start), int _UNUSED(end)) {
    rl_attempted_completion_over = 1;
    return rl_completion_matches(str, generator);
}

/*
  %init-readline-completion-function will:
  1. set our C variable 'gen' to the STklos closure that is passed
     as the 'generator' argument;
  2. set the readline completion function to our C completion
     function STk_completion.
  It is called by the Scheme procedure 'init-readline-completion-function'.
 */
DEFINE_PRIMITIVE("%init-readline-completion-function",readline_init_completion,subr1,
         (SCM generator))
{
  gen = generator;
  /* The word break chars are by default " \t\n\"\\'`@$><=;|&{(".
     We adapt those here so they make sense in a Scheme environment.
     It's not perfect, though: we'll ignore variables names starting
     with \t, \n, ; etc, because if we matched them we'd have trouble
     with separating tokens. */
  rl_completer_word_break_characters = " \t\n'`;|(";

  rl_attempted_completion_function = STk_completion;
  return STk_void;
}

MODULE_ENTRY_START("stklos/readline-complete")
{
  SCM module =  STk_create_module(STk_intern("stklos/readline-complete"));

  ADD_PRIMITIVE_IN_MODULE(readline_init_completion, module);

  /* Execute Scheme code */
  STk_execute_C_bytecode(__module_consts, __module_code);
}
MODULE_ENTRY_END

DEFINE_MODULE_INFO
