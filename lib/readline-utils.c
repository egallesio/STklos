/*
 * readline-utils.c   -- Readline utils (tab-completion, option setting)
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
 */

#include "../src/stklos.h"

//
// Readline interface for completion
//
// When using completion we need some variables and functions from
// "readline/readline.h". However, since we don't use autoconf for locating the
// installation directory of readline (or libedit), we just declare what we need
// here. It also permits to compile this file without libreadline or libedit.
//
// NOTE: STklos does not try to link the GNU readline library since on BSD
// systems, it is generally not present (plus the complex interaction of LGPL
// and GPL). Hence, autoconf doesn't try to locate any editing library and
// the REPL just tries to link against a line editing library at runtime if it
// finds one.

/* Readline typedefs for the completion system */
typedef char *rl_compentry_func_t(const char *, int);
typedef char **rl_completion_func_t(const char *, int, int);

/* Readline function used */
extern char **rl_completion_matches(const char *, rl_compentry_func_t *);
extern int rl_parse_and_bind (char *);

/* Readline variables used */
extern char *rl_line_buffer;                    // The line buffer
extern int rl_attempted_completion_over;        // 1 to suppress filename completion
extern char *rl_completer_word_break_characters;// word separator. Normally "n\"\\'`@$>"
extern rl_completion_func_t
            *rl_attempted_completion_function; // Pointer to our completion func

extern char* rl_readline_name;                  // NOTE: both vars a are not
extern char* rl_basic_quote_characters;         // completion related (see below)

static SCM gen;                 // A pointer to the Scheme generator function


/*
  Calls the generator (gen) with two arguments:
  1. the text to be matched
  2. the state:
     - #f if this is the first call (and we want the first match from
       the list)
     - #t if this is a subsequent call (and we want one more match).
  This is how readline works.
 */
static char *
generator(const char *text, int state) {
  SCM s = STk_C_apply(gen,2,
                      STk_Cstring2string((char *)text),
                      MAKE_BOOLEAN(state));

  if (s == STk_nil) return NULL;

  /* We MUST call malloc, and not use libgc, because readline will attempt
     to free the pointer (at least on FresBSD). */
  size_t size = STRING_SIZE(s);
  char *res = malloc(size+1);
  if (res == NULL) return ""; // better return no completion than signaling an error.
  strncpy(res,STRING_CHARS(s), size);
  res[size]=0;
  return res;
}

/* scheme_completion is the function passed to libreadline to do tab
   completion. Its arguments are
   - The partially typed string
   - The start and end positions (end position is ignored here)
   The actual work is done by the generator function in this file,
   which in turn calls the Scheme procedure complete in
   readline.stk.
*/
static char **
scheme_completion(const char *str, int start, int _UNUSED(end)) {

  // We want path completion when we are after a double quote. If the character
  // preceding the first char of str is a '"' (we search it in the complete
  // line buffer), we inhibit path completion by setting
  // rl_attempted_completion_over to 0.
  rl_attempted_completion_over = (start == 0 ||
                                  rl_line_buffer[start-1] != '\"');

  return rl_completion_matches(str, generator);
}


/* ======================================================================
 *
 * Readline primitives
 *
 * ====================================================================== */
/*
  %init-readline-completion-function will:
  1. set our C variable 'gen' to the STklos closure that is passed
     as the 'generator' argument;
  2. set the readline completion function to our C completion
     function STk_completion.
  It is called by the Scheme procedure 'init-readline-completion-function'.
 */
DEFINE_PRIMITIVE("%init-readline-completion-function",readline_init_completion,
                 subr1, (SCM generator))
{
  gen = generator;
  /* The word break chars are by default " \t\n\"\\'`@$><=;|&{(".
     We adapt those here so they make sense in a Scheme environment.
     It's not perfect, though: we'll ignore variable names starting
     with \t, \n, ; etc., because if we matched them we'd have trouble
     with separating tokens. */
  rl_completer_word_break_characters = " \t\n\"'`;|(";
  rl_attempted_completion_function = scheme_completion;
  return STk_void;
}


DEFINE_PRIMITIVE("readline-set-option!",readline_set_option,subr2,
                 (SCM option, SCM value)) {
  if (!STRINGP(option)) STk_error("bad string ~s", option);
  if (!STRINGP(value)) STk_error("bad string ~s", value);
  if (STRING_SIZE(option) + STRING_SIZE(value) > 195)
    STk_error("option and value strings too long (max 195 bytes)");

  char s[201];
  snprintf(s,200,"set %s %s",STRING_CHARS(option), STRING_CHARS(value));
  int res = rl_parse_and_bind(s);
  return MAKE_BOOLEAN(res == 0);
}


MODULE_ENTRY_START("readline-utils")
{
  SCM module =  STk_STklos_module;   // FIXME: should be READLINE

  ADD_PRIMITIVE_IN_MODULE(readline_init_completion, module);
  ADD_PRIMITIVE_IN_MODULE(readline_set_option, module);

  // NOTE: the following assignments are not related to completion and should
  // not be here.  However since our FFI doesn't permit to read/set variables
  // and since this is the only file written in C interacting with readline,
  // we do this assignment here.


  rl_readline_name = "stklos";     // To permit STklos parts in the ~/ .inputrc file
  rl_basic_quote_characters = "\"";// Suppress the simple quote for parent flashing
}
MODULE_ENTRY_END

// DEFINE_MODULE_INFO
