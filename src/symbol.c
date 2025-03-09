/*
 *
 * s y m b o l . c                      -- Symbols management
 *
 * Copyright © 1993-2025 Erick Gallesio <eg@stklos.net>
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
 *    Creation date: 20-Nov-1993 12:12
 */

#include <ctype.h>
#include "stklos.h"
#include "hash.h"

/**** Static globals ****/
static struct hash_table_obj obarray;
static char valid_symbol_chars[]=
  "abcdefghijklmnopqrstuvwxyz0123456789+-.*/<=>!?:$%_&~^";


/**** Utilities ****/

static void error_bad_string(SCM str)
{
  STk_error("bad string ~S", str);
}

int STk_symbol_flags(register const char *s)
{
  if (!*s || (*s == '.' && !s[1])) {
    /* Special symbols || and |.| which always need bars */
    return SYMBOL_NEEDS_BARS;
  } else {
    int res = 0;

    if (s[0] == ':') res |= SYMBOL_NEEDS_BARS;   // seems to be a keyword

    for ( ;*s; s++) {
      if (isupper(*s)) {
        res |= SYMBOL_HAS_UPPER;
        continue;
      }
      if (!strchr(valid_symbol_chars, *s)) {
        res |= SYMBOL_NEEDS_BARS;
        break;
      }
    }

    if (s[-1] == ':') res |= SYMBOL_NEEDS_BARS; // seems to be a keyword
    return res;
  }
}


static inline SCM make_uninterned_symbol(const char *name)
{
  SCM z;

  NEWCELL(z, symbol);
  SYMBOL_PNAME(z) = STk_strdup(name);
  BOXED_INFO(z) |= STk_symbol_flags(name);
  return z;
}


SCM STk_make_uninterned_symbol(const char *name)
{
  SCM z = make_uninterned_symbol(name);
  BOXED_INFO(z) |= SYMBOL_UNINTERNED;
  return z;
}


SCM STk_intern(char *name)
{
  SCM res;
  MUT_DECL(obarray_mutex);

  MUT_LOCK(obarray_mutex);
  res =  STk_hash_intern_symbol(&obarray, name, make_uninterned_symbol);
  MUT_UNLOCK(obarray_mutex);
  return res;
}

/*
<doc EXT symbol-interned?
 * (symbol-interned? sym)
 *
 * Returns |#t| if |sym| is an interned symbol, amd |#f| if |sym| is an
 * uninterned symbol. When |sym| is not a symbol, an error is signalled.
 * @lisp
 * (symbol-interned? (gensym))      => #f
 * (symbol-interned? (gensym "x-")) => #f
 * (symbol-interned? 'a)            => #t
 * (symbol-interned? "x")           => error
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("symbol-interned?", symbol_interned_p, subr1, (SCM x))
{
  if (!SYMBOLP(x)) STk_error("bad symbol ~s", x);
  return MAKE_BOOLEAN(!(BOXED_INFO(x) & SYMBOL_UNINTERNED));
}

DEFINE_PRIMITIVE("symbol?", symbolp, subr1, (SCM x))
/*
<doc  symbol?
 * (symbol? obj)
 *
 * Returns |#t| if obj is a symbol, otherwise returns |#f|.
 * @lisp
 *    (symbol? 'foo)          =>  #t
 *    (symbol? (car '(a b)))  =>  #t
 *    (symbol? "bar")         =>  #f
 *    (symbol? 'nil)          =>  #t
 *    (symbol? '())           =>  #f
 *    (symbol? #f)            =>  #f
 *    (symbol? :key)          =>  #f
 * @end lisp
doc>
 */
{
  return MAKE_BOOLEAN(SYMBOLP(x));
}

/*
<doc  symbol->string
 * (symbol->string string)
 *
 * Returns the name of |symbol| as a string. If the symbol was part of an
 * object returned as the value of a literal expression or by a call to the
 * |read| procedure, and its name contains alphabetic characters, then the
 * string returned will contain characters in the implementation's preferred
 * standard case -- STklos prefers lower case. If the symbol was returned
 * by |string->symbol|, the case of characters in the string returned will be
 * the same as the case in the string that was passed to |string->symbol|. It
 * is an error to apply mutation procedures like |string-set!| to strings
 * returned by this procedure.
 * @lisp
 *    (symbol->string 'flying-fish)  =>  "flying-fish"
 *    (symbol->string 'Martin)       =>  "martin"
 *    (symbol->string (string->symbol "Malvina"))
 *                                   =>  "Malvina"
 * @end lisp
doc>
 */

DEFINE_PRIMITIVE("symbol->string", symbol2string, subr1, (SCM symbol))
{
  SCM str;
  const char *s;

  if (!SYMBOLP(symbol)) STk_error("bad symbol ~S", symbol);

  s   = SYMBOL_PNAME(symbol);
  str = STk_makestring(strlen(s), s);
  BOXED_INFO(str) |= STRING_CONST;   /* This string is a constant */
  return str;
}

/*
<doc  string->symbol
 * (string->symbol string)
 *
 * Returns the symbol whose name is |string|. This procedure can create
 * symbols with names containing special characters or letters in the
 * non-standard case, but it is usually a bad idea to create such symbols
 * because in some implementations of Scheme they cannot be read as themselves.
 *
 * @lisp
 *    (string->symbol "mISSISSIppi")                    =>  @pipemISSISSIppi@pipe
 *    (eq? 'bitBlt (string->symbol "bitBlt"))           =>  #f
 *    (eq? 'JollyWog
 *         (string->symbol
 *           (symbol->string 'JollyWog)))               =>  #t
 *    (string=? "K. Harper, M.D."
 *              (symbol->string
 *                (string->symbol "K. Harper, M.D.")))  =>  #t
 * @end lisp
 *
 * NOTE: The expression `(eq? 'mISSISSIppi 'mississippi)` returns `#f` if
 * {{stklos}} is running in case-sensitive mode (default), whereas it returns
 * `#t` otherwise.
doc>
 */
DEFINE_PRIMITIVE("string->symbol", string2symbol, subr1, (SCM string))
{
  if (!STRINGP(string)) error_bad_string(string);
  return STk_intern(STRING_CHARS(string));
}


/*
<doc EXT  string->uninterned-symbol
 * (string->unterned-symbol string)
 *
 * Returns the symbol whose print name is made from the characters of
 * |string|. This symbol is guaranteed to be _unique_ (i.e. not
 * |eq?| to any other symbol):
 * @lisp
 * (let ((ua (string->uninterned-symbol "a")))
 *   (list (eq? 'a ua)
 *         (eqv? 'a ua)
 *         (eq? ua (string->uninterned-symbol "a"))
 *         (eqv? ua (string->uninterned-symbol "a"))))
 *           => (#f #t #f #t)
 * @end lisp
doc>
 */
DEFINE_PRIMITIVE("string->uninterned-symbol", string2usymbol, subr1, (SCM string))
{
  if (!STRINGP(string)) error_bad_string(string);
  return STk_make_uninterned_symbol(STRING_CHARS(string));
}

int STk_init_symbol(void)
{
  STk_hashtable_init(&obarray, HASH_OBARRAY_FLAG);
  ADD_PRIMITIVE(symbolp);
  ADD_PRIMITIVE(symbol2string);
  ADD_PRIMITIVE(string2symbol);
  ADD_PRIMITIVE(string2usymbol);                /* + */
  ADD_PRIMITIVE(symbol_interned_p);
  return TRUE;
}
