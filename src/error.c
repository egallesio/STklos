/*
 *
 * e r r o r . c                        -- The error procedure
 *
 * Copyright © 1993-2022 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 14-Nov-1993 14:58
 */

#include "stklos.h"
#include "struct.h"

/*===========================================================================*\
 *
 * A simplified (and very ad hoc) version of printf for error messages.
 *       %c for character
 *       %s for string
 *       %S for string (surrounded by a pair of quotes)
 *       %d for decimal print
 *       %x for hexadecimal print
 *       %% for printing a '%'
 *       ~A for printing a Scheme object in display mode
 *       ~S for printing a Scheme object in write mode
 *       ~W for printing a Scheme object in write mode (circular)
 *       ~% for printing a newline
 *       ~~ for printing a tilde character
 *
\*===========================================================================*/


static void print_int(SCM port, unsigned int x, unsigned int base)
{
  if (x >= base) print_int(port, x / base, base);
  x %= base;
  STk_putc(x + (x >= 10 ? 'a'-10: '0'), port);
}

static void print_format(SCM port,char *format, va_list ap)
{
  register char *s;
  char *str;

  /* Parse given format */
  for (s = format; *s ; s++) {
    if (*s == '%') {
      /* % format (C-like) */
      switch (*++s) {
        case '%': STk_putc('%', port); break;

        case 'S': STk_putc('`', port); /* FALLTHROUGH */
        case 's': for (str = va_arg(ap, char *); *str; str++)
                    STk_putc(*str, port);
                  if (*s == 'S') STk_putc('\'', port);
                  break;
        case 'c': STk_putc(va_arg(ap, int), port); break;
        case 'x': print_int(port, va_arg(ap, unsigned int), 16); break;
        case 'd': {
                    int val =  va_arg(ap, unsigned int);

                    if (val < 0) {
                      STk_putc('-', port);
                      print_int(port, -val, 10);
                    }
                    else
                      print_int(port, val, 10);
                    break;
                  }
        default:  STk_putc('%', port);
                  if (*s) STk_putc(*s, port);
                  break;
      }
    } else if (*s == '~') {
      /* ~ format (CL like) */
      switch (*++s) {
        case 'A': STk_putc('`', port); /* FALLTHROUGH */
        case 'a': STk_print(va_arg(ap, SCM), port, DSP_MODE);
                  if (*s == 'A') STk_putc('\'', port);
                  break;
        case 'W': STk_putc('`', port);  /* FALLTHROUGH */
        case 'w': STk_print_star(va_arg(ap, SCM), port,WRT_MODE);
                  if (*s == 'W') STk_putc('\'', port);
                  break;
        case 'S': STk_putc('`', port);  /* FALLTHROUGH */
        case 's': STk_print(va_arg(ap, SCM), port, WRT_MODE);
                  if (*s == 'S') STk_putc('\'', port);
                  break;
        case '~': STk_putc('~', port);  break;
        case '%': STk_putc('\n', port); break;
        default:  STk_putc('~', port);
                  if (*s) STk_putc(*s, port);
                  break;
      }
    } else {
      /* Normal character */
      STk_putc(*s, port);
    }
  }
}

void STk_signal_error(SCM type, SCM where, SCM str, SCM msg, SCM irritants)
{
  SCM bt = STk_vm_bt();

  STk_raise_exception(STk_make_C_cond(type, 5, where, bt, str,
                                      msg, irritants));
}

SCM STk_format_error(char *format, ...)
{
  va_list ap;
  SCM out;

  /* Open a string port */
  out = STk_open_output_string();

  /* Build the message string in the string port */
  va_start(ap, format);
  print_format(out, format, ap);
  va_end(ap);

  /* Return errror message as a Scheme string */
  return STk_get_output_string(out);
}


static SCM make_error_condition(char *format, va_list ap)
{
  SCM out, bt;

  /* Grab a backtrace */
  bt = STk_vm_bt();

  /* Open a string port */
  out = STk_open_output_string();

  /* Build the message string in the string port */
  print_format(out, format, ap);

  /* and return error */
  out =  STk_get_output_string(out);
  return STk_make_C_cond(STk_err_mess_condition,
                         5,
                         STk_false, /* no location */
                         bt,
                         out,       /* formatted output */
                         out,       /* here too (because a C error */
                         STk_nil);  /* may contain C objects */
}


SCM STk_make_error(char *format, ...)
{
  va_list ap;
  SCM cond;

  va_start(ap, format);
  cond = make_error_condition(format, ap);
  va_end(ap);

  /* Return the error condition */
  return cond;
}


void STk_error(char *format, ...)
{
  va_list ap;
  SCM cond;

  va_start(ap, format);
  cond = make_error_condition(format, ap);
  va_end(ap);

  /* Signal error */
  STk_raise_exception(cond);
}


void STk_error_with_location(SCM loc, char *format, ...)
{
  va_list ap;
  SCM cond;

  va_start(ap, format);
  cond = make_error_condition(format, ap);
  STk_int_struct_set(cond, STk_intern("location"), loc);
  va_end(ap);

  /* Signal error */
  STk_raise_exception(cond);
}


void STk_warning(char *format, ...)
{
  va_list ap;
  SCM eport = STk_current_error_port();

  /* Print the prologue */
  STk_fprintf(eport, "\n**** Warning:\n");

  /* Print the message */
  va_start(ap, format);
  print_format(eport, format, ap);
  va_end(ap);

  STk_putc('\n', eport);
  STk_flush(eport);
}


void STk_panic(char *format, ...)
{
  va_list ap;
  SCM eport = STk_current_error_port();

  /* Print the prologue */
  STk_fprintf(eport, "\n**** PANIC:\n");

  /* Print the message */
  va_start(ap, format);
  print_format(eport, format, ap);
  va_end(ap);

  STk_putc('\n', eport);
  STk_flush(eport);

  /*  And terminate execution  */
  STk_puts("ABORT.\n", eport);
  _exit(1);
}


void STk_signal(char *str)
{
  STk_raise_exception(STk_make_C_cond(STk_message_condition,
                                      1,
                                      STk_Cstring2string(str)));
}



#ifdef STK_DEBUG
void STk_debug(char *format, ...)
{
  va_list ap;
  SCM eport = STk_current_error_port();

  /* Print the prologue */
  STk_fprintf(eport, "\033[31m**** Debug: ");

  va_start(ap, format);
  print_format(eport, format, ap);
  va_end(ap);

  STk_fprintf(eport, "\033[0m\n");
  STk_flush(eport);
}

void STk_gdb(SCM obj)           /* associated to the  gdb write function */
{
  STk_debug("Object 0x%lx value = ~s", (unsigned long) obj, obj);
}
#endif
