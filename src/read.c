/*
 * r e a d  . c                         -- reading stuff
 *
 * Copyright © 1993-2020 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: ??-Oct-1993 ??:??
 * Last file update: 14-Dec-2020 17:09 (eg)
 *
 */

#include <ctype.h>
#include "stklos.h"


struct read_context {
  SCM cycles;
  SCM inner_refs;
  int comment_level;
  int case_significant;
  int constant;
};


static SCM read_srfi10(SCM port, SCM l);
static SCM read_rec(SCM port, struct read_context *ctx, int inlist);

static SCM sym_quote, sym_quasiquote, sym_unquote, sym_unquote_splicing, sym_dot;
static SCM sym_read_brace, sym_read_bracket, read_error;

int STk_read_case_sensitive = DEFAULT_CASE_SENSITIVE;

#define PLACEHOLDERP(x)         (CONSP(x) && (BOXED_INFO(x) & CONS_PLACEHOLDER))
#define PLACEHOLDER_VAL(x)      (CDR(x))

#define SYMBOL_VALUE(x,ref)     STk_lookup((x), STk_current_module(), &(ref), FALSE)

#define MAX_HEX_SEQ_LEN 20      /* Normally max value is 10FFFFF => 9 with '\0' */

/*===========================================================================*\
 *
 * Utilities
 *
\*===========================================================================*/
static void signal_error(SCM port, char *format, SCM param)
{
  STk_raise_exception(STk_make_C_cond(read_error,
                                      7,
                                      STk_false,
                                      STk_vm_bt(),
                                      STk_format_error(format, param),
                                      MAKE_INT(PORT_LINE(port)),    /* line */
                                      STk_false,                    /* column */
                                      MAKE_INT(PORT_POS(port)),     /* position */
                                      STk_false));                  /* span */
}

static void error_bad_srfi_207_escape(SCM port, char *tok)
{
    signal_error(port, "bad escape \\~a in string-notated bytevector", STk_Cstring2string(tok));
}

static void error_bad_srfi_207_char(SCM port, char *str, int pos)
{
      char message[100];

      snprintf(message, 100,
               "bad non-ASCII character in position %u of string \"~a\" used as string-notated bytevector",
               pos);
      signal_error(port, message, STk_Cstring2string(str));
}

static void error_token_too_large(SCM port, char *tok)
{
  tok[MAX_TOKEN_SIZE-1] = '\0'; /* truncate the name (should be sufficient !!) */
  signal_error(port, "token too large ~S", STk_Cstring2string(tok));
}

static void error_bad_sharp_syntax(SCM port, char *tok)
{
  signal_error(port, "bad sharp syntax in ~S", STk_Cstring2string(tok));
}

static void error_key_not_defined(SCM port, SCM key)
{
  signal_error(port, "key ``#~a='' not defined", key);
}

static void error_bad_dotted_list(SCM port)
{
  signal_error(port, "bad dotted list", STk_nil);
}

static void error_bad_inline_hexa_sequence(SCM port, char *str, int in_symbol)
{
  char message[100];

  snprintf(message, 100, "bad or unfinished hexa sequence `\\x%s' in a %s",
           str, (in_symbol ? "symbol": "string"));
  signal_error(port, message, STk_nil);

}

static void warning_parenthesis(SCM port)
{
  STk_warning("bad closing parenthesis on line %d of ~S", PORT_LINE(port), port);
}


static int flush_spaces(SCM port, char *message, SCM file)
{
  int c;

  for ( ; ; ) {
    switch (c = STk_getc(port)) {
      case EOF:  if (message) signal_error(port, message, file); else return(c);
                 break; /* for the compiler */
      case ';':  do
                   c = STk_getc(port);
                 while (c != '\n' && c != EOF);
                 continue;
    default:   if (!isspace((unsigned char) c)) return(c);
    }
  }
}


static int read_hex_sequence(SCM port, char* utf8_seq, int use_utf8) // ⇒ -1 if incorrect
{
  char buffer[MAX_HEX_SEQ_LEN];
  char *end;   /* normally max value is 10FFFFF */
  int c;
  unsigned i = 0;
  long int val;

  /* assert: current char is 'x' */
  do
    c = buffer[i++] = STk_getc(port);
  while ((i < MAX_HEX_SEQ_LEN - 1) && isxdigit(c) && (c != ';') && (c != EOF));
  buffer[i] = '\0';

  if (c != ';') {
    buffer[i-1] = '\0';
    STk_ungetc(c, port);
    goto bad_sequence;
  }
  else {
    val = strtol(buffer, &end, 16);

    if (val == LONG_MIN || val == LONG_MAX || *end != ';')
      goto bad_sequence;
    else
      if (use_utf8) {
        int len = STk_char2utf8(val, utf8_seq);

        if (len) return len;
      } else {
        if (0 <= val && val <= 0xFF) {
          *utf8_seq = (char) val;
          return 1;
        }
      }
  }
 bad_sequence:
  strcpy(utf8_seq, buffer); /* for a better error message */
  return -1;
}


static SCM read_list(SCM port, char delim, struct read_context *ctx)
/* Read a list ended by the `delim' char */
{
  static char *eof_seen = "end of file encountered on ~S";
  int c, line;
  SCM tmp, cell, cdr;

  line = PORT_LINE(port);
  c    = flush_spaces(port, eof_seen, port);

  if (c == delim) return(STk_nil);
Top:
  /* Read the car */
  STk_ungetc(c, port);
  tmp = read_rec(port, ctx, TRUE);

  /* See if we don't have a special car */
  if (tmp == STk_close_par) {
    /* We are here when reading a list such as (1 ..... #;XXX) */
    c = STk_getc(port);
    if (c == delim)
      return STk_nil;
    else {
      warning_parenthesis(port);
      c = delim;
      goto Top;
    }
  }

  if (tmp == STk_dot) {
    tmp = read_rec(port, ctx, TRUE);
    if (tmp == STk_close_par)
      error_bad_dotted_list(port);

    c = flush_spaces(port, eof_seen, port);
    if (c == '#') {
      SCM tmp;

      STk_ungetc(c, port);
      tmp = read_rec(port, ctx, TRUE);
      if (tmp != STk_close_par) warning_parenthesis(port);
      c = flush_spaces(port, eof_seen, port);
    }
    if (c != delim)
      STk_warning("missing close parenthesis (line %d)", PORT_LINE(port));
    return(tmp);
  }

  /* OK, read now the cdr */
  cdr = read_list(port, delim, ctx);
  if (cdr == STk_close_par)
    error_bad_dotted_list(port);

  if (ctx->constant) {
    /* Constant read uses extended cons instead of cons */
    cell = STk_econs(tmp, cdr, PORT_FNAME(port), line, PORT_POS(port));
    BOXED_INFO(cell) |= CONS_CONST;
  } else {
    cell = STk_cons(tmp, cdr);
  }

  return cell;
}


static int read_word(SCM port, int c, char *tok, int case_significant)
/* read an item whose 1st char is in c. Return its length */
{
  register int j = 0;
  int allchars   = 0;

  for( ; ; ) {
    allchars  ^= (c == '|');
    if (c != '|')
      tok[j++]  = (allchars || case_significant) ? c : tolower(c);

    if (c == '\\') {
      int k = j-1;

      c = STk_getc(port);
      switch (c) {
        case '\\': tok[k] = '\\'; break;     /*   \  */
        case '|' : tok[k] ='|';   break;     /*   |  */
        case 'a' : tok[k] ='\a';  break;     /* Bell */
        case 'b' : tok[k] ='\b';  break;     /* Bs   */
        case 'e' : tok[k] =0x1b;  break;     /* Esc  */
        case 'f' : tok[k] ='\f';  break;     /* FF   */
        case 'n' : tok[k] ='\n';  break;     /* Lf   */
        case 'r' : tok[k] ='\r';  break;     /* Cr   */
        case 't' : tok[k] ='\t';  break;     /* Tab  */
        case 'v' : tok[k] ='\v';  break;     /* VTab */
        case 'x': {
          /* This is an internal hexa sequence */
          char buffer[MAX_HEX_SEQ_LEN];
          int len = read_hex_sequence(port, buffer, STk_use_utf8);

          if (len < 0)
            error_bad_inline_hexa_sequence(port, buffer, 1); /* 1 = symbol */
          else {
            if (j + len >= MAX_TOKEN_SIZE-1) {
              tok[j] = '\0';
              error_token_too_large(port, tok);
            } else {
              memcpy(tok + j-1, buffer, len);
              j += len-1;
            }
          }
          break;
        }
        default:
          j -= 1; /* to delete the useless '\' */
          STk_ungetc(c, port);
      }
    }

    c = STk_getc(port);
    if (c == EOF) break;
    if (!allchars) {
      if (strchr("()[]{}'`,;\"\n\r \t\f", c)) {
        STk_ungetc(c, port);
        break;
      }
      //      if (isspace(c)) break; //FIXME:
    }
    if (j >= MAX_TOKEN_SIZE-1) error_token_too_large(port, tok);
  }

  tok[j] = '\0';
  return j;
}

static SCM read_token(SCM port, int c, int case_significant)
{
  char tok[MAX_TOKEN_SIZE];
  SCM z;
  int len;

  len = read_word(port, c, tok, case_significant);
  z   = STk_Cstr2number(tok, 10L);

  if (z == STk_false) {
    /* It is not a number */
    switch (*tok) {
      case ':': return STk_makekey(tok);
      case '#': if (len > 1) {
                  if (len == 2) {
                    if (tok[1] == 't' || tok[1] == 'T') return STk_true;
                    if (tok[1] == 'f' || tok[1] == 'F') return STk_false;
                  }
                  if (tok[1] == ':')
                    return STk_makekey(tok+1);
                  else if (strcasecmp(tok+1, "true") == 0)
                    return STk_true;
                  else if (strcasecmp(tok+1, "false") == 0)
                    return STk_false;
                  else if (strcasecmp(tok+1, "eof") == 0)
                    return STk_eof;
                  else if (strcasecmp(tok+1, "void") == 0)
                    return STk_void;
                }
                error_bad_sharp_syntax(port, tok);
                break; /* for the compiler */
      default : return (tok[len-1] == ':') ? STk_makekey(tok) : STk_intern(tok);
    }
  }
  /* Return the number read */
  return z;
}

static SCM read_char(SCM port, int c)
/* read a char (or a char name) item whose 1st char is in c */
{
  char tok[MAX_TOKEN_SIZE];
  register int j = 0;

  for( ; ; ) {
    tok[j++] = c;
    c = STk_getc(port);
    if (c == EOF || ((c <=0x80) && isspace((unsigned char)c)))
      /* (c < 0x80) is for MacOs */
      break;
    if (strchr("()[]'`,;\"", c)) {
      STk_ungetc(c, port);
      break;
    }
    if (j >= MAX_TOKEN_SIZE-1) error_token_too_large(port, tok);
  }
  tok[j] = '\0';

  /* convert the character contained in token in a character */
  return MAKE_CHARACTER(STk_string2char(tok));

}

static SCM read_address(SCM port)
{
  char *end, tok[MAX_TOKEN_SIZE] = "0";
  unsigned long address;

  read_word(port, 'x', tok+1, FALSE);
  address = strtoul(tok, &end, 16);
  if (*end)
    signal_error(port, "bad address specifier #p~a", STk_Cstring2string(tok+2));

  return (SCM) address;
}

static SCM read_here_string(SCM port)
{
  SCM eof_token = read_token(port, STk_getc(port), TRUE);
  SCM res, line;
  int first_line = TRUE;

  if (!SYMBOLP(eof_token)) STk_error("bad symbol for here string ~S", eof_token);

  /* skip the end of line */
  line = STk_read_line(port);
  if (!STRINGP(line) || (STRING_SIZE(line) != 0))
    STk_error("end of line expected after the ~S delimiter", eof_token);


  res = STk_open_output_string();
  while (1) {
    line = STk_read_line(port);
    if (line == STk_eof)
      STk_error("eof seen while reading an here-string");
    else
      if (strcmp(STRING_CHARS(line), SYMBOL_PNAME(eof_token)) == 0)
        break;
    /* Append the read string to the result */
    if (first_line)
      first_line = FALSE;
    else
      STk_putc('\n', res);
    STk_putstring(line, res);
  }

  return STk_get_output_string(res);
}


static SCM add_inner_references(SCM *obj, SCM to_correct) {
  /* *obj contains inner references that must be replaced later. Find them */
  if (PLACEHOLDERP(*obj)) {
    /* place it in the list of references to correct */
    if (STk_memq((SCM) obj, to_correct) == STk_false) {
      to_correct = STk_cons((SCM) obj, to_correct);
    }
  } else if (CONSP(*obj)) {
    to_correct = add_inner_references(&CAR(*obj), to_correct);
    to_correct = add_inner_references(&CDR(*obj), to_correct);
  } else if (VECTORP(*obj)) {
    int i, l = VECTOR_SIZE(*obj);
    SCM *p;

    for (i=0, p=VECTOR_DATA(*obj); i < l; i++, p++) {
      to_correct = add_inner_references(p, to_correct);
    }
  }
  return to_correct;
}


static void patch_references(SCM port, SCM l, SCM cycles)
{
  for ( ; !NULLP(l); l = CDR(l)) {
    SCM k, tmp;

    tmp = *((SCM *) CAR(l));
    k   = PLACEHOLDER_VAL(tmp);
    if ((tmp = STk_assq(k, cycles)) != STk_false) {
      *((SCM *) CAR(l)) = CDR(tmp);
    }
    else
      error_key_not_defined(port, k);
  }
}


static SCM read_cycle(SCM port, int c, struct read_context *ctx)
/* read a #xx# or #xx= cycle item whose 1st char is in c. */
{
  char buffer[MAX_TOKEN_SIZE];
  int  j = 0;
  SCM k, tmp, val;

  for( ; ; ) {
    buffer[j++] = c;
    c = STk_getc(port);
    if (c == EOF || !isdigit(c)) break;
    if (j >= MAX_TOKEN_SIZE-1) error_token_too_large(port, buffer);
  }
  buffer[j] = '\0';
  k = MAKE_INT(atoi(buffer));

  switch (c) {
    case '#': if ((tmp = STk_assq(k, ctx->cycles)) != STk_false) {
                val = CDR(tmp);
                if (PLACEHOLDERP(val))
                  CAR(val) = STk_true; /* Mark  the placeholder as read */
                return val;
              }
              else
                error_key_not_defined(port, k);
              break;
    case '=': {
                /* This is a little bit tricky here: We create a fake cell
                 * that serves as a place-holder. In some cases this is not
                 * useful (e.g. (#0=(1 2) 3 4 . #0#) ), but in some other
                 * cases such as
                 *    (#0=(1 2 . #0#) #0#)
                 * the first reference will use the placeholder cell, whereas
                 * the second one will be correct.
                 * We call here the function add_inner_references to capture
                 * the reference which are in the second case.
                 *
                 * At the end of the entire read (and only at the end to
                 * avoid  a long time calculation, or even infinite loops),
                 * the function "patch_references" will correct all the
                 * remaining references that must be modified.
                 */

                 /* create the fake cell */
                 SCM fake = STk_cons(STk_void, k);
                 BOXED_INFO(fake) |= CONS_PLACEHOLDER;

                 /* Add the couple (k . <fake-cell>) to the cycles list */
                 tmp         = STk_cons(k, fake);
                 ctx->cycles = STk_cons(tmp, ctx->cycles);

                 /* Read item */
                 val         = read_rec(port, ctx, FALSE);

                 if (CAR(fake) != STk_void) {
                   /* we have an inner reference on k */
                   ctx->inner_refs = add_inner_references(&val, ctx->inner_refs);
                 }

                 /* Patch the list of cycles with the correct value */
                 CDR(tmp) = val;
                 return val;
               }

  default:     STk_ungetc(c, port); error_bad_sharp_syntax(port, buffer);
  }

  return STk_void; /* for the compiler */
}


static SCM read_string(SCM port, int constant, int srfi_207)
{
  char hex_buffer[MAX_HEX_SEQ_LEN];    // used to read hex sequence
  int position = 0;
  int bad_hex_sequence = 0;
  int bad_srfi_207_char = -1;
  int k ,c, n;
  size_t j, len;
  char *p, *buffer;
  SCM z;

  j    = 0;
  len  = 100;
  p    = buffer = STk_must_malloc(len);

  while(((c = STk_getc(port)) != '"') && (c != EOF)) {
    if (c == '\\') {
      c = STk_getc(port);
      if (c == EOF) signal_error(port, "eof encountered after \\", STk_nil);
      switch(c) {
        case 'a' : c = '\a'; break;     /* Bell */
        case 'b' : c = '\b'; break;     /* Bs   */
        case 'e' : if (srfi_207) { while(((c = STk_getc(port)) != '"') && (c != EOF));
                                   error_bad_srfi_207_escape(port, "e"); }
                   else c = 0x1b;
                   break;           /* Esc  */
        case 'f' : if (srfi_207) error_bad_srfi_207_escape(port, "f");
                   else c = '\f';
                   break;               /* FF   */
        case 'n' : c = '\n'; break;     /* Lf   */
        case 'r' : c = '\r'; break;     /* Cr   */
        case 't' : c = '\t'; break;     /* Tab  */
        case 'v' : if (srfi_207) error_bad_srfi_207_escape(port, "v");
                   else c = '\v';
                   break;               /* VTab */
        case ' ' : do {
                        c = STk_getc(port);
                   } while (c == ' ' || c == '\t');

                  if (c != '\n') {
                    signal_error(port, "bad line continuation sequence in string",
                                 STk_nil);
                  }
                  /* FALLTHROUGH */
        case '\n': do {
                        c = STk_getc(port);
                   } while (c == ' ' || c == '\t');
                   break;
        case 'x' : {
                     /* the sequence must be read as UTF8 *only* if
                        - it is NOT part of a SRFI-207 string-notted bytevector, AND
                        - STk_use_utf8 is set  */
                     int seqlen = read_hex_sequence(port, hex_buffer, (!srfi_207) && STk_use_utf8);

                     if (seqlen < 0) {
                       bad_hex_sequence = 1;
                     } else {
                       if ((j + seqlen) >= len) {
                         len = len + len / 2;
                         buffer = STk_must_realloc(buffer, len);
                         p = buffer + j;
                       }
                       memcpy(p, hex_buffer, seqlen);
                       p += seqlen;
                       j += seqlen;
                     }
                     continue;
                    }
        case '0' :  if (srfi_207) error_bad_srfi_207_escape(port, "0");
                    else {
                      for( k=n=0 ; ; k++ ) {
                        c = STk_getc(port);
                        if (c == EOF)
                          signal_error(port,
                                       "eof encountered when reading char in string",
                                       STk_nil);

                        c &= 0377;
                        /* 3 digit max for bytes */
                        if (isdigit(c) && (c < '8') && k < 3)
                          n = n * 8 + c - '0';
                        else {
                          STk_ungetc(c, port);
                          break;
                        }
                      }
                      c = n & 0xff;
                    }
      }
    } else /* not an escape sequence, so if it's a SRFI-207 string,
              we must restrict chars to ASCII-printable */
        if (srfi_207 && ( (c < 32) || (c > 126) )) bad_srfi_207_char=j-1;

    if ((j + 1) >= len) {
      len = len + len / 2;
      buffer = STk_must_realloc(buffer, len);
      p = buffer + j;
    }
    j++;
    *p++ = c;
  }
  
  if (bad_hex_sequence) error_bad_inline_hexa_sequence(port, hex_buffer, 0);
  if (c == EOF) signal_error(port,"end of file while reading a string on ~S", port);
  *p = '\0';
  if (bad_srfi_207_char >=0 ) error_bad_srfi_207_char(port, buffer, bad_srfi_207_char);

  if (srfi_207) {
    /* if read_string was called for reading a SRFI-207 string-notated bytevector,
       we *must not* interpret it as UTF-8, so we just do what STk_makestring
       would do, but without the UTF8 stuff. */
    NEWCELL(z, string);
    STRING_CHARS(z)  = STk_must_malloc_atomic(j + 1);
    STRING_SPACE(z)  = STRING_SIZE(z) = j;
    STRING_LENGTH(z) = len;
    strcpy(STRING_CHARS(z), buffer);
  }
  else z = STk_makestring(j, buffer);
  if (constant)
    BOXED_INFO(z) |= STRING_CONST;

  STk_free(buffer);
  return z;
}


static SCM read_vector(SCM port, struct read_context *ctx)
{
  SCM v = STk_list2vector(read_list(port, ')', ctx));

  if (ctx->constant) BOXED_INFO(v) |= VECTOR_CONST;
  return v;
}

static SCM maybe_read_uniform_vector(SCM port, int c, struct read_context *ctx)
{
  char tok[MAX_TOKEN_SIZE];
  int tag, len;
  SCM v;

  len = read_word(port, c, tok, ctx->case_significant);

  if (strcasecmp(tok, "f") ==0 || strcasecmp(tok, "false") ==0) {
    /* This is the #f constant */
    return STk_false;
  } else {
    if ((!STk_uvectors_allowed &&  (strcmp(tok, "u8") == 0)) ||
        (STk_uvectors_allowed && (len == 2 || len == 3))) {
      c = STk_getc(port);
      if (c == '(') {
        tag = STk_uniform_vector_tag(tok);
        if (tag >= 0) {
          int konst = ctx->constant;

          /* Ok that's seems correct read the list of values (this IS a constant) */
          ctx->constant = TRUE;
          v =  STk_list2uvector(tag, read_list(port, ')', ctx));
          ctx->constant = konst;
          BOXED_INFO(v) |= VECTOR_CONST;
          return v;
        }
      } else if (c == '"') { /* SRFI-207: #u8"abc" => #u8(97 98 99) */
          int konst = ctx->constant;
          ctx->constant = TRUE;
          SCM str = read_string(port, TRUE , TRUE);

          len = STRING_SIZE(str);
          NEWCELL_WITH_LEN(v, uvector, sizeof(struct vector_obj) + len - 1);
          UVECTOR_TYPE(v) = UVECT_U8;
          UVECTOR_SIZE(v) = len;
          
          for (int i = 0; i < len; i++)
              ((unsigned char *) UVECTOR_DATA(v))[i] = (unsigned char) (((unsigned char *) STRING_CHARS(str))[i]);

          ctx->constant = konst;
          BOXED_INFO(v) |= VECTOR_CONST;
          return v;
      } else goto bad_spec;
    }
  }
 bad_spec:
  signal_error(port, "bad uniform vector specification ~A",STk_Cstring2string(tok));
  return STk_void;
}


static SCM read_rec(SCM port, struct read_context *ctx, int inlist)
{
  int c;
  SCM quote_type;

  for ( ; ; ) {
    c = flush_spaces(port, (char *) NULL, (SCM) NULL);

    switch (c) {
      case EOF:
        return STk_eof;
      case '(':
        return(read_list(port, ')', ctx));

      case '[': {
        SCM ref, read_bracket_func = SYMBOL_VALUE(sym_read_bracket, ref);

        if (read_bracket_func != STk_void) {
          STk_ungetc(c, port);
          return STk_C_apply(read_bracket_func, 1, port);
        }
        return(read_list(port, ']', ctx));
      }

    case '{': {
      SCM ref, read_brace_func = SYMBOL_VALUE(sym_read_brace, ref);

        if (read_brace_func != STk_void) {
          STk_ungetc(c, port);
          return STk_C_apply(read_brace_func, 1, port);
        }
        goto default_case;
      }

      case ')':
      case ']':
      case '}':
        if (inlist) {
          STk_ungetc(c, port);
          return STk_close_par;
        }
        warning_parenthesis(port);
        break;
      case '\'':
        quote_type = sym_quote;
        goto read_quoted;
      case '`':
        quote_type = sym_quasiquote;
    read_quoted:
        {
          SCM tmp = read_rec(port, ctx, inlist);
          if (tmp == STk_dot || tmp == STk_close_par)
            signal_error(port, "bad quote/quasiquote syntax", STk_nil);
          return LIST2(quote_type, tmp);
        }
      case '#':
        switch(c=STk_getc(port)) {
          // FIXME:
          case 'F' :
          case 'f' : if (STk_uvectors_allowed)
                       return maybe_read_uniform_vector(port, c, ctx);
                     goto default_sharp;
          case '\\': return read_char(port, STk_getc(port));
          case '(' : return read_vector(port, ctx);
          case '!' : { /* This can be a comment, a DSSSL keyword, or fold-case */
                       c = STk_getc(port);
                       if (c == 'o' || c == 'k' || c == 'r' || c == 'n' || c == 'f') {
                         SCM word = read_token(port, c, FALSE);

                         if (SYMBOLP(word)) {
                           char *s = SYMBOL_PNAME(word);

                           /* Try to see if it is a DSSL keyword */
                           if ((strcmp(s, "optional") == 0) ||
                               (strcmp(s, "key")      == 0) ||
                               (strcmp(s, "rest")     == 0))
                             return STk_makekey(s);

                           /* Treat fold-case and no-fold-case */
                           if ((strcmp(s, "fold-case") == 0) ||
                               (strcmp(s, "no-fold-case") == 0)) {
                             if (c == 'n') {
                               PORT_FLAGS(port) |= PORT_CASE_SENSITIVE;
                               ctx->case_significant = TRUE;
                             }
                             else {
                               PORT_FLAGS(port) &= ~PORT_CASE_SENSITIVE;
                               ctx->case_significant = FALSE;
                             }
                             continue;
                           }
                         }
                       }
                       /* if we are here, consider the rest of the line
                        * as a comment*/
                       do {
                         if (c == EOF) return STk_eof;
                       } while ((c=STk_getc(port)) != '\n');
                       STk_ungetc(c, port);
                       continue;
                     }
          case '|':  {
                       char prev = ' ';

                       ctx->comment_level += 1;
                       for ( ; ; ) {
                         switch (c = STk_getc(port)) {
                           case EOF:
                             goto end_comment;
                           case '\n':
                             break;
                           case '#':
                             if (prev == '|') {
                               ctx->comment_level -= 1;
                               if (!ctx->comment_level) goto end_comment;
                             }
                             break;
                           case '|':
                             if (prev == '#')
                               ctx->comment_level += 1;
                             break;
                           default: ;
                         }
                         prev = c;
                       }
                       end_comment:
                       c = flush_spaces(port, (char *) NULL, (SCM) NULL);
                       if (c == EOF) {
                         if (ctx->comment_level)
                           signal_error(port,
                                        "eof encountered when reading a comment",
                                        STk_nil);
                         else
                           return STk_eof;
                       } else {
                         STk_ungetc(c,port);
                         continue;
                       }
                       break;   /* for the compiler */
                    }
          case '<': {
                       char c2 = STk_getc(port);
                       if (c2 == '<' )
                         return read_here_string(port);
                       else  {
                         STk_ungetc(c2, port);
                         goto default_sharp;
                       }
                    }
          case '&': return STk_make_box(read_rec(port, ctx, inlist));
          case 'p':
          case 'P': return read_address(port);
          case 'S':
          case 's':
          case 'U':
          case 'u': if (STk_uvectors_allowed || c == 'u')
                      /* For R7RS #u8 is always valid (bytevectors) */
                      return maybe_read_uniform_vector(port, c, ctx);
                    else
                      goto default_sharp;
         case ';': /* R6RS comments */
                   read_rec(port, ctx, FALSE);
                   c = flush_spaces(port, NULL, NULL);
                   STk_ungetc(c, port);
                   if (inlist && (c == ')' || c == ']' || c == '}'))
                     return STk_close_par;
                   continue;
          case ',': /* SRFI-10 */
                    return read_srfi10(port,
                                       read_rec(port, ctx, inlist));
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9': return read_cycle(port, c, ctx);
          default:
            default_sharp:
                    STk_ungetc(c, port);
                    return read_token(port, '#', ctx->case_significant);
        }
        break; /* for the compiler */
      case ',': {
        SCM symb, tmp;

        c = STk_getc(port);
        if (c == '@')
          symb = sym_unquote_splicing;
        else {
          symb = sym_unquote;
          STk_ungetc(c, port);
        }
        tmp = read_rec(port, ctx, inlist);
        if (tmp == STk_dot || tmp == STk_close_par)
          signal_error(port, "bad unquote/unquote-splice syntax", STk_nil);
        return LIST2(symb, tmp);
      }
      case '"':
          return read_string(port, ctx->constant, FALSE); /* NOT a string-notated bytevector from */
      default:
    default_case: {
          SCM tmp = read_token(port, c, ctx->case_significant);
          if (tmp != sym_dot)
            return tmp;
          if (inlist)
            return STk_dot;
          signal_error(port, "dot outside of list", STk_nil);
        }
    }
  }
}

/*===========================================================================*\
 *
 * STk_read
 * STk_read_constant
 *
 *      The two entry points of the reader. STk_read_constant just set a
 *      flag to say that the object read (for strings, lists and vectors) is a
 *      constant
 *
\*===========================================================================*/
static SCM read_it(SCM port, int case_significant, int constant)
{
  int c;
  SCM res;
  struct read_context ctx;

  c = flush_spaces(port, (char *) NULL, (SCM) NULL);

  ctx.cycles           = STk_nil;
  ctx.inner_refs       = STk_nil;
  ctx.comment_level    = 0;
  ctx.case_significant = case_significant;
  ctx.constant         = constant;

  if (c == EOF)
    return STk_eof;

  STk_ungetc(c, port);

  res = read_rec(port, &ctx, FALSE);

  if (!NULLP(ctx.inner_refs)) {
    patch_references(port, ctx.inner_refs, ctx.cycles);
  }
  return res;
}


SCM STk_read(SCM port, int case_significant)
{
  return read_it(port, case_significant, FALSE);
}


SCM STk_read_constant(SCM port, int case_significant)
{
  return read_it(port, case_significant, TRUE);
}


char *STk_quote2str(SCM symb)
{
  if (symb == sym_quote)                return "\'";
  if (symb == sym_quasiquote)           return "`";
  if (symb == sym_unquote)              return ",";
  if (symb == sym_unquote_splicing)     return ",@";
  return NULL;
}

/*=======================================================================*\
 *
 * SRFI-10 support
 *
\* ======================================================================*/
static SCM ctor_table = STk_nil;

DEFINE_PRIMITIVE("define-reader-ctor",reader_ctor, subr2, (SCM symbol, SCM proc))
{
  SCM tmp;

  if (!SYMBOLP(symbol))
    STk_error("bad symbol ~S", symbol);
  if (STk_procedurep(proc) == STk_false)
    STk_error("bad procedure ~S", proc);

  tmp = STk_int_assq(symbol, ctor_table);
  if (tmp != STk_false)
    CDR(tmp) = proc;
  else {
    MUT_DECL(lck);
    MUT_LOCK(lck);
    ctor_table = STk_cons(STk_cons(symbol, proc), ctor_table);
    MUT_UNLOCK(lck);
  }
  return STk_void;
}


static SCM read_srfi10(SCM port, SCM l)
{
  int len = STk_int_length(l);
  SCM tmp;

  if (len < 0)
    signal_error(port, "bad list in a #,(...) form ~S", l);

  tmp = STk_int_assq(CAR(l), ctor_table);
  if (tmp == STk_false)
    signal_error(port, "bad tag in a #,(...) form ~S", CAR(l));
  else {
    /* result is (apply (cdr tmp) (cdr l)) */
    return STk_C_apply_list(CDR(tmp), CDR (l));
  }

  return STk_void;      /* For the C compiler */
}


/*
<doc EXT read-case-sensitive
 * (read-case-sensitive)
 * (read-case-sensitive value)
 *
 * This parameter object permits to change the default behaviour of
 * the |read| primitive when reading a symbol. If this parameter has a
 * a true value a symbol is not converted to a default case when interned.
 * Since ,(rseven) requires that symbol are case insignificant, the default
 * value  of this parameter is |#t|.
 * @lisp
 * (read-case-sensitive)        => |#t|
 * (read-from-string "ABC")     => ABC
 * (read-case-sensitive #f)
 * (read-from-string "ABC")     => abc
 * @end lisp
 * ,(bold "Note:")  Default behaviour can be changed for a whole execution
 * with the |--case-sensitive| or |case-insensitive| options.
 * @l
 * ,(bold "Note:") See also syntax for ,(ref :mark "bar-in-symbol" :text
 * [special characters]) in symbols.
 *
doc>
*/
static SCM read_case_sensitive_conv(SCM value)
{
  STk_read_case_sensitive = (value != STk_false);
  return MAKE_BOOLEAN(STk_read_case_sensitive);
}



/*===========================================================================*\
 *
 *                      I n i t i a l i z a t i o n
 *
\*===========================================================================*/
int STk_init_reader(void)
{
  sym_quote            = STk_intern("quote");
  sym_quasiquote       = STk_intern("quasiquote");
  sym_unquote          = STk_intern("unquote");
  sym_unquote_splicing = STk_intern("unquote-splicing");
  sym_dot              = STk_intern(".");
  sym_read_bracket     = STk_intern("%read-bracket");
  sym_read_brace       = STk_intern("%read-brace");

  /* read-error condition */
  read_error = STk_defcond_type("&read-error", STk_err_mess_condition,
                                LIST4(STk_intern("line"),
                                      STk_intern("column"),
                                      STk_intern("position"),
                                      STk_intern("span")),
                                STk_STklos_module);


  /* Declare SRFI-10 support function */
  ADD_PRIMITIVE(reader_ctor);

  /* Declare parameter read-case-sensitve */
  STk_make_C_parameter("read-case-sensitive",
                       MAKE_BOOLEAN(STk_read_case_sensitive),
                       read_case_sensitive_conv,
                       STk_STklos_module);
  return TRUE;
}
