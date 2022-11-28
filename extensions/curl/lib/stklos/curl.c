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
 * Last file update: 15-Feb-2022 21:18 (eg)
 */

#include <stklos.h>
#include <curl/curl.h>
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
// curl-version
//
DEFINE_PRIMITIVE("curl-version", curl_version, subr0, (void))
{
  return STk_Cstring2string(curl_version());
}

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


  // curl_easy_setopt(curl, CURLOPT_VERBOSE, 1);
  // curl_easy_setopt(curl, CURLOPT_HEADER, 1);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *) stdout);
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

static size_t write_callback(void *data, size_t size, size_t nmemb, void *port)
{
  size_t sz = size * nmemb;

  STk_write_buffer((SCM) port, data, sz);
  return sz;
}

static size_t read_callback(void *data, size_t size, size_t nmemb, void *port)
{
  return  STk_read_buffer((SCM) port, data, size * nmemb);
}

static int debug_callback(CURL *handle, curl_infotype type,
                          char *data, size_t size,
                          void *userp)
{
  SCM port = (SCM) userp;

  (void) handle;
  (void) size;

  switch (type) {
    case CURLINFO_TEXT:
      STk_fprintf(port, "* %s", data);
      break;
    case CURLINFO_HEADER_IN:
      STk_fprintf(port, "< %s", data);
      break;
    case CURLINFO_HEADER_OUT:
      STk_fprintf(port, "> %s", data);
      break;

    // Do nothing for other cases
    case CURLINFO_DATA_IN:
    case CURLINFO_DATA_OUT:
    case CURLINFO_SSL_DATA_IN:
    case CURLINFO_SSL_DATA_OUT:
    default: ;
  }
  return 0;
}


static SCM set_transfer_port(CURL *curl, SCM port, int portnum)
{
  CURLcode code = CURLE_OK;
  CURLoption opt_data, opt_func;
  void *func;

  switch (portnum) {
  case 0:
    if (!IPORTP(port)) STk_error("bad input port ~S", port);
    opt_data = CURLOPT_READDATA;
    opt_func = CURLOPT_READFUNCTION;
    func = read_callback;
    break;
  case 1:
    if (!OPORTP(port)) STk_error("bad output port ~S", port);
    opt_data = CURLOPT_WRITEDATA;
    opt_func = CURLOPT_WRITEFUNCTION;
    func = write_callback;
    break;
  case 2:
  default:
    if (!OPORTP(port)) STk_error("bad output port ~S", port);
    opt_data = CURLOPT_DEBUGDATA;
    opt_func = CURLOPT_DEBUGFUNCTION;
    func = debug_callback;
    break;
  }
  // Set the opt_data to the given port
  code = curl_easy_setopt(curl, opt_data, port);
  if (code != CURLE_OK) STk_error("%s", curl_easy_strerror(code));

  // Set the opt_func option to the given port
  code = curl_easy_setopt(curl, opt_func, func);
  if (code != CURLE_OK) STk_error("%s", curl_easy_strerror(code));
  return STk_void;
}



DEFINE_PRIMITIVE("curl-set-option", curl_set_opt, subr3, (SCM h, SCM opt, SCM val))
{
  if (!CURLP(h))      STk_error("bad curl handler ~S", h);
  if (!KEYWORDP(opt)) STk_error("bad keyword for curl option ~S", opt);

  {
    char *name = KEYWORD_PNAME(opt);
    CURL *curl = CURL_HANDLER(h);
    const struct curl_easyoption *copt = curl_easy_option_by_name(name);
    CURLcode code = CURLE_OK;  // for the compiler;

    if (strncasecmp(name, "iport", 5) == 0)
      return set_transfer_port(curl, val, 0);
    else if (strncasecmp(name, "oport", 5) == 0)
      return set_transfer_port(curl, val, 1);
    else if (strncasecmp(name, "eport", 5) == 0)
      return set_transfer_port(curl, val, 2);
    else {
      // #:iport and #:oport option are treated specially. Other options are
      // treated by the standatd curl_easy_setopt (for the  handled types only).
      if (!copt) STk_error("no curl option with name ~S", opt);

      switch (copt->type) {
        case CURLOT_LONG: {
          long v = BOOLEANP(val) ? (long) (val == STk_true) : STk_integer_value(val);
          if (v == LONG_MIN)
            STk_error("bad integer value ~S for option %s", val, opt);
          code = curl_easy_setopt(curl, copt->id, v);
          break;
        }
        case CURLOT_STRING:
          if (!STRINGP(val))
            STk_error("bad string value ~S for option %s", val, opt);
          code = curl_easy_setopt(curl, copt->id, STRING_CHARS(val));
          break;

        case CURLOT_OBJECT:  /* pointer (void *) */
          if (STRINGP(val)) {
            // This is dangerous ....
            // but necessary to pass value to POST HTTP request with :postfields
            code = curl_easy_setopt(curl, copt->id, STRING_CHARS(val));
          } else {
            STk_error("don't know how to pass ~S to ~S option", val, opt);
          }
          break;
                          case CURLOT_VALUES:  /*      (a defined set or bitmask) */
        case CURLOT_OFF_T:   /* curl_off_t (a range of values) */
        case CURLOT_SLIST:   /*         (struct curl_slist *) */
        case CURLOT_CBPTR:   /*         (void * passed as-is to a callback) */
        case CURLOT_BLOB:    /* blob (struct curl_blob *) */
        case CURLOT_FUNCTION: /* function pointer */
          STk_error("option ~S (of type %d) is not handled by this library", opt, copt->type);
      }
    }
    if (code != CURLE_OK) STk_error("%s", curl_easy_strerror(code));
  }
  return STk_void;
}

DEFINE_PRIMITIVE("curl-perform", curl_perform, subr1, (SCM h))
{
  CURLcode code;

  if (!CURLP(h)) STk_error("bad curl handler ~S", h);

  code = curl_easy_perform(CURL_HANDLER(h));
  if (code != CURLE_OK) STk_error("%s", curl_easy_strerror(code));

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
  ADD_PRIMITIVE_IN_MODULE(curl_version, module);
  ADD_PRIMITIVE_IN_MODULE(curl_init, module);
  ADD_PRIMITIVE_IN_MODULE(curl_cleanup, module);
  ADD_PRIMITIVE_IN_MODULE(curl_set_opt, module);
  ADD_PRIMITIVE_IN_MODULE(curl_perform, module);

  // Call the CURL global initialization function
  // (necessary if we use several threads)
  curl_global_init(CURL_GLOBAL_DEFAULT);

  //Export all the symbols defined here
  STk_export_all_symbols(module);

  // Execute Scheme code
  STk_execute_C_bytecode(__module_consts, __module_code);
} MODULE_ENTRY_END


DEFINE_MODULE_INFO
