/*
 *
 * b a s e 6 4 . c			-- Base64 support for STk
 *
 * Copyright © 1998-2006 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *           Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 20-Jul-1998 12:19
 * Last file update: 21-Sep-2006 16:45 (eg)
 */

#include <stklos.h>

static char table[] =  
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static char rev_table[128];

#define OutChar(c, f) { 			\
    STk_putc((c), (f)); 			\
    if (++count>=72) {				\
      STk_putc('\n', (f));			\
      count=0;					\
    }						\
}


static void initialize_rev_table(void)
{
  char *p;
  int count = 0;

  for (p = table; *p; p++) rev_table[(int) *p] = count++;
}

static void error_bad_input_port(SCM obj)
{
  STk_error("bad input port ~S", obj);
}

static void error_bad_output_port(SCM obj)
{
  STk_error("bad output port ~S", obj);
}


static void encode(SCM f, SCM g)
{
  int c, state, count, old;

  state = old = count = 0;
  while ((c = STk_getc(f)) != EOF) {
    switch (++state) {
      case 1: OutChar(table[(c>>2) & 0x3f], g); 
	      break;
      case 2: OutChar(table[((old<<4) & 0x30) | ((c>>4) & 0x0f)], g); 
	      break;
      case 3: OutChar(table[((old<<2) & 0x3c) | ((c>>6) & 0x03)], g);
	      OutChar(table[c & 0x3f], g);
	      state = 0;
	      break;
    }
    old = c;
  }
  switch (state) {
    case 0: /* nothing */;
    case 1: OutChar(table[(old<<4) & 0x30], g);
            OutChar('=', g);
            OutChar('=', g);
	    break;
    case 2: OutChar(table[(old<<2) & 0x3c], g);
            OutChar('=', g);
	    break;
  }
}

static void decode(SCM f, SCM g)
{
  static int initialized = 0;
  int c, bits, group, j;

  if (!initialized) {
    initialize_rev_table();    
    initialized = 1;
  }

  group = 0;  j = 18;
  while ((c = STk_getc(f)) != EOF) {
    if (c != '\n') {
      if (c != '=') {
	bits = rev_table[c];
	group |= bits << j;
      }
      
      j -= 6;
      
      if (j < 0) {
	c = (group&0xff0000) >> 16; STk_putc(c, g);
	c = (group&0x00ff00) >> 8;  STk_putc(c, g);
	c = (group&0x0000ff);       STk_putc(c, g);
	group = 0;
	j = 18;
      }
    }
  }
}

/*
<doc EXT base64-encode
 * (base64-encode in)
 * (base64-encode in out)
 * 
 * Encode in Base64 the characters from input port |in| to the output port 
 * |out|. If |out| is not specified, it defaults to the current output port.
doc>
*/
DEFINE_PRIMITIVE("base64-encode", base64_encode, subr12, (SCM f, SCM g))
{
  if (!IPORTP(f))  error_bad_input_port(f);
  if (!g)
    g = STk_current_output_port();
  else
    if (!OPORTP(g)) error_bad_output_port(g);
  
  encode(f, g);
  return STk_void;
}

/*
<doc EXT base64-decode
 * (base64-decode in)
 * (base64-decode in out)
 * 
 * Decode the Base64 characters from input port |in| to the output port 
 * |out|. If |out| is not specified, it defaults to the current output port.
doc>
*/
DEFINE_PRIMITIVE("base64-decode", base64_decode, subr12, (SCM f, SCM g))
{
  if (!IPORTP(f))  error_bad_input_port(f);
  if (!g)
    g = STk_current_output_port();
  else
    if (!OPORTP(g)) error_bad_output_port(g);
  
  decode(f, g);
  return STk_void;
}

/*===========================================================================*\
 * 
 * 	Initialization code
 *
\*===========================================================================*/
int STk_init_base64(void)
{
  ADD_PRIMITIVE(base64_decode);
  ADD_PRIMITIVE(base64_encode);

  return TRUE;
}
