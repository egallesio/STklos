/*
 * curl.c                       -- Interface wrapper for cURL
 *
 * Copyright © 2022 Erick Gallesio - I3S-CNRS/ESSI <eg@essi.fr>
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
 *           Author: Erick Gallesio [eg@essi.fr]
 *    Creation date:  7-Feb-2022 15:55 (eg)
 * Last file update:  7-Feb-2022 17:13 (eg)
 */

#include <stklos.h>
#include <curl/curl.h>
#include <stklos/stklos.h>
#include "curl-incl.c"

struct curl_obj {
  stk_header header;
  CURL* handler;
};

#define CURLP(o)                (BOXED_TYPE_EQ((o), tc_curl))
#define CURL_HANDLER(c)         (((struct curl_obj *) (c))->handler)

static struct extended_type_descr xtype_curl = {
  .name  = "curl-handler",
};

static int tc_curl= 0;


//
// curl-init
//
DEFINE_PRIMITIVE("curl-init", curl_init, subr0, (void))
{
  SCM z = STk_nil;
  CURL *curl = curl_easy_init();

  if (!curl) STk_error("cannot initialize curl");

  NEWCELL(z, curl);
  CURL_HANDLER(z) = curl;
  return z;
}

//
// curl-cleanup
//
DEFINE_PRIMITIVE("curl-cleanup", curl_cleanup, subr1, (SCM h))
{
  if (!CURLP(h)) STk_error("bad curl handler ~S", h);
  curl_easy_cleanup(CURL_HANDLER(h));
  return STk_void;
}

//
// curl-setopt
//
DEFINE_PRIMITIVE("curl-set-option", curl_set_opt, subr3, (SCM h, SCM opt, SCM val))
{
  const struct curl_easyoption *copt;

  if (!CURLP(h))      STk_error("bad curl handler ~S", h);
  if (!KEYWORDP(opt)) STk_error("bad keyword for curl option ~S", opt);

  copt = curl_easy_option_by_name(KEYWORD_PNAME(opt));
  if (!copt)
    STk_error("no curl option with name ~S", opt);

  printf("id = %d, type = %d, flags = %d", copt->id, copt->type, copt->flags);

  
  return STk_void;
}


/* ----------------------------------------------------------------------
 *
 *      Module curl starts here
 *
 * ----------------------------------------------------------------------
 */
MODULE_ENTRY_START("stklos/curl")
{
  SCM module = STk_create_module(STk_intern("stklos/curl"));

  // Define a new extended type for curl objects
  tc_curl = STk_new_user_type(&xtype_curl);

  // Define curl primitives
  ADD_PRIMITIVE_IN_MODULE(curl_init, module);
  ADD_PRIMITIVE_IN_MODULE(curl_cleanup, module);
  ADD_PRIMITIVE_IN_MODULE(curl_set_opt, module);

  //Export all the symbols defined here
  STk_export_all_symbols(module);

  // Execute Scheme code
  STk_execute_C_bytecode(__module_consts, __module_code);
} MODULE_ENTRY_END


DEFINE_MODULE_INFO
