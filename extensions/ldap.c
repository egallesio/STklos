/*
 * ldap.c	-- LDAP access from Scheme
 * 
 * Copyright © 2002-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 22-Feb-2002 18:15 (eg)
 * Last file update:  6-May-2004 11:45 (eg)
 */

#include <stklos.h>
#include <ctype.h>
#include "extconf.h"

#ifdef HAVE_LDAP
#include <ldap.h>


/*===========================================================================*\
 * 
 *			Definition of the LDAP type 
 *
\*===========================================================================*/
static int tc_ldap;		/* the key for LDAP objects */

struct ldap_obj {
  stk_header header;
  LDAP *ld;
  SCM  server;
  int  port;
};

#define LDAPP(l)	  (BOXED_TYPE_EQ((l), tc_ldap))
#define LDAP_LD(l)	  (((struct ldap_obj *) (l))->ld)
#define LDAP_SERVER(l)	  (((struct ldap_obj *) (l))->server)
#define LDAP_PORT_FD(l)	  (((struct ldap_obj *) (l))->port)

static void print_ldap(SCM n, SCM port, int mode)
{
  char buffer[200];
  
  sprintf(buffer, "#[ldap %s:%d]", STRING_CHARS(LDAP_SERVER(n)),LDAP_PORT_FD(n));
  STk_puts(buffer, port);
}

static struct extended_type_descr xtype_ldap = {
  "ldap",
  print_ldap
};

/*===========================================================================*\
 * 
 *				UTILITIES
 *
\*===========================================================================*/


static void error_bad_ldap(SCM obj)
{
  STk_error("bad ldap connection object ~S", obj);
}

static void error_bad_list(SCM obj)
{
  STk_error("bad list ~S", obj);
}

static void error_bad_string(SCM obj)
{
  STk_error("bad string ~S", obj);
}

static char *strlower(char *s)
{
  char *t;

  for (t = s; *t; t++) *t = tolower(*t);
  return s;
}


static char **flat_it(SCM l) 
{
  char **res= NULL; /* for GCC */

  if (STRINGP(l)) {
    /* l is not a list but a single element */
    res = STk_must_malloc(2 * sizeof(char *));
    res[0] = STRING_CHARS(l);
    res[1] = NULL;
  } else if CONSP(l) {
    /* l is a list of values */
    int i = 0, cpt = STk_int_length(l);
    
    res = STk_must_malloc((cpt + 1) * sizeof(char *));
    while (!NULLP(l)) {
      if (!STRINGP(CAR(l))) error_bad_string(CAR(l));
      res[i++] = STRING_CHARS(CAR(l));
      l        = CDR(l);
    }
    res[i] = NULL;
  } else {
    error_bad_string(l);
  }
  return res;
}

/*===========================================================================*\
 * 
 *				LDAP-CONNECT
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%ldap-connect", ldap_connect, subr4, (SCM server, SCM port, 
						       SCM dn, SCM pass))
{
  int rc, p;
  char *s, *d, *pwd;
  SCM z;
  LDAP *ld;

  ENTER_PRIMITIVE(ldap_connect);

  /* 1. Control server parameter */
  if (!STRINGP(server)) STk_error("bad server name ~S", server);
  s = STRING_CHARS(server);  
  
  /* 2. Control port parameter */
  p = STk_integer_value(port);
  if (p == LONG_MIN) STk_error("bad port number ~S", port);

  /* 3. Control the dn field */
  if (dn == STk_false)
    d = NULL;
  else {
    if (!STRINGP(dn)) STk_error("bad dn field ~S", dn);
    d = STRING_CHARS(dn);
  }
  
  /* 4. Control the passwd field */
  if (pass == STk_false)
    pwd = NULL;
  else {
    if (!STRINGP(pass)) STk_error("bad password ~S", pass);
    pwd = STRING_CHARS(pass);
  }
  
  /* Here s, p, d and pwd contains the information for opening the ldap 
   * base and do the ldap_bind;
   */
   if (!(ld = ldap_init(s, p))) STk_error("cannot open LDAP on ~S\n", server);

   if ((rc = ldap_simple_bind_s(ld, d, pwd)) != LDAP_SUCCESS) {
     STk_error("%s", ldap_err2string(rc));
     exit(1);
   }

   /* Create the Scheme object */
   NEWCELL(z, ldap);
   LDAP_LD(z)      = ld;
   LDAP_SERVER(z)  = server;
   LDAP_PORT_FD(z) = p;
   
   return z;
}

/*===========================================================================*\
 * 
 *				LDAP-CLOSE
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("ldap-close", ldap_close, subr1, (SCM ld))
{
  ENTER_PRIMITIVE(ldap_close);

  if (!LDAPP(ld)) error_bad_ldap(ld);
  ldap_unbind_s(LDAP_LD(ld));

  return STk_void;
}


/*===========================================================================*\
 * 
 *				LDAP-SEARCH
 *
\*===========================================================================*/
SCM ldif_entry_out(LDAP *ld, LDAPMessage *msg)
{
  BerElement *ptr;
  int i;
  char *dn, *attr, **vals;
  SCM res;


  dn = ldap_get_dn(ld, msg);
  res = LIST2(STk_makekey("dn"), STk_Cstring2string(dn));
  ldap_memfree(dn);

  for (attr = ldap_first_attribute(ld, msg, &ptr); 
       attr != NULL;
       attr = ldap_next_attribute(ld, msg, ptr)) {
    vals = ldap_get_values(ld, msg, attr);
    if (vals) { 
      /* This slot has a value */
      SCM car = STk_makekey(strlower(attr));
      
      if (!vals[1]) {
	/* This is a single element value */
	res =  STk_dappend2(res, LIST2(car, STk_Cstring2string(vals[0])));
      } else {
	/* This is an element with multiple values: make a list */
	SCM lst = STk_nil;
	
	for(i = 0; vals[i]; i++) {
	  lst =  STk_cons(STk_Cstring2string(vals[i]), lst);
	}
	res = STk_dappend2(res, LIST2(car, STk_dreverse(lst)));
      }
      ldap_value_free(vals);
      free(attr);
    }
  }
  if (ptr) ber_free(ptr, 0);
  
  return res;
}

DEFINE_PRIMITIVE("%ldap-search", ldap_search, subr4, 
		 (SCM ld, SCM base, SCM scope, SCM filter))
{
  int rc, sco=0; 	/* = 0 for gcc */
  LDAPMessage *res, *e;
  LDAP *cld;
  SCM l;

  ENTER_PRIMITIVE(ldap_search);
  
  if (!LDAPP(ld))       error_bad_ldap(ld);
  if (!STRINGP(base))   STk_error("bad search base ~S", base);
  if (!STRINGP(filter)) STk_error("bad filter for seraching ~S", filter);
  
  switch (AS_LONG(scope)) {
  case AS_LONG(MAKE_INT(0)): sco = LDAP_SCOPE_BASE;	break;
  case AS_LONG(MAKE_INT(1)): sco = LDAP_SCOPE_ONELEVEL;	break;
  case AS_LONG(MAKE_INT(2)): sco = LDAP_SCOPE_SUBTREE;	break;
  default:		     STk_error("bad scope value ~S", scope);
  }

  /* Parameters here are correct. Start LDAP search */
  cld = LDAP_LD(ld);
  rc = ldap_search_s(cld, STRING_CHARS(base), sco, STRING_CHARS(filter),
		     NULL, 0, &res);
  
  if (rc != LDAP_SUCCESS) STk_error("%s", ldap_err2string(rc));
  
  /* Build the result as a Scheme list */
  l = STk_nil;
  for(e = ldap_first_entry(cld, res); e; e = ldap_next_entry(cld, e)) {
    l = STk_cons(ldif_entry_out(cld, e), l);
  }
  return STk_dreverse(l);
}

/*===========================================================================*\
 * 
 *				LDAP-ADD
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("%ldap-add", ldap_add, subr3, (SCM ld, SCM dn, SCM l))
{
  LDAPMod **mods;
  int i, n, len = STk_int_length(l);
  
  ENTER_PRIMITIVE(ldap_add);
  
  if (!LDAPP(ld)) 	    error_bad_ldap(ld);
  if (!STRINGP(dn)) 	    error_bad_string(dn);
  if (len < 0 || (len & 1)) error_bad_list(l);

     
  /* Allocate the mods block */
  mods = STk_must_malloc(((len/2) + 1) * sizeof(LDAPMod *));

  /* And fill it */
  for (i=0; !NULLP(l); i++, l=CDR(CDR(l))) {
    SCM key = CAR(l);
    SCM val = CAR(CDR(l));
    LDAPMod *m;

    if (!KEYWORDP(key)) STk_error("bad keyword ~S in ~S", key, l);
    m = STk_must_malloc(sizeof(LDAPMod));

    m->mod_op     = LDAP_MOD_ADD;
    m->mod_type   = KEYWORD_PNAME(key);
    m->mod_values = flat_it(val);
    mods[i]	  = m;
  }
  mods[i]= NULL;

  /* Do the LDAP addition */
  n = ldap_add_s(LDAP_LD(ld), STRING_CHARS(dn), mods);
  if (n != LDAP_SUCCESS) STk_error("Adding ~S: %s", dn, ldap_err2string(n));

  return STk_void;
}

/*===========================================================================*\
 * 
 *				LDAP-DELETE
 *
\*===========================================================================*/
DEFINE_PRIMITIVE("ldap-delete", ldap_del, subr2, (SCM ld, SCM dn))
{
  int n;
  
  ENTER_PRIMITIVE(ldap_del);
  if (!LDAPP(ld)) 	    error_bad_ldap(ld);
  if (!STRINGP(dn)) 	    error_bad_string(dn);
  
  n = ldap_delete_s(LDAP_LD(ld), STRING_CHARS(dn));
  if (n != LDAP_SUCCESS) STk_error("Deleting ~S: %s", dn, ldap_err2string(n));
  return STk_void;
}
   
/*===========================================================================*\
 * 
 *				LDAP-MODIFY
 *
\*===========================================================================*/


DEFINE_PRIMITIVE("%ldap-modify", ldap_modify, subr5,
		 (SCM ld, SCM dn, SCM add, SCM del, SCM mod))
{
  int ladd = STk_int_length(add);
  int ldel = STk_int_length(del);
  int lmod = STk_int_length(mod);
  int n, cpt = 0;
  LDAPMod **mods;

  ENTER_PRIMITIVE(ldap_modify);
  
  /* Control parameters */
  if (!LDAPP(ld))    	       error_bad_ldap(ld);
  if (!STRINGP(dn))  	       STk_error("bad dn ~S", dn);
  if (ladd < 0) error_bad_list(add);
  if (ldel < 0) error_bad_list(del);
  if (lmod < 0) error_bad_list(mod);
  
  /* Allocate the mods array */
  n = ladd/2 + ldel +lmod/2 + 1; 
  mods = STk_must_malloc(n * sizeof(LDAPMod *));
  


  /* Treat the deleted fields */
  for ( ; !NULLP(del); del = CDR(del)) {
    LDAPMod *m;
    
    m 		  = STk_must_malloc(sizeof(LDAPMod));
    m->mod_op     = LDAP_MOD_DELETE;
    m->mod_type   = KEYWORD_PNAME(CAR(del));
    mods[cpt++]	  = m;
  }

  /* Treat the added fields */
  for ( ; !NULLP(add); add = CDR(CDR(add))) {
    LDAPMod *m;

    m = STk_must_malloc(sizeof(LDAPMod));
    m->mod_op     = LDAP_MOD_ADD;
    m->mod_type   = KEYWORD_PNAME(CAR(add));
    m->mod_values = flat_it(CAR(CDR(add)));
    mods[cpt++]	  = m;
  }

  /* Treat the modified fields */
  for ( ; !NULLP(mod); mod = CDR(CDR(mod))) {
     LDAPMod *m;

    m = STk_must_malloc(sizeof(LDAPMod));
    m->mod_op     = LDAP_MOD_REPLACE;
    m->mod_type   = KEYWORD_PNAME(CAR(mod));
    m->mod_values = flat_it(CAR(CDR(mod)));
    mods[cpt++]	  = m;
  }

  /* Terminate the mods array */
  mods[cpt] = NULL;

  /* Do the LDAP modify */
  n = ldap_modify_s(LDAP_LD(ld), STRING_CHARS(dn), mods);
  if (n != LDAP_SUCCESS) STk_error("Modifying ~S: %s", dn, ldap_err2string(n));

  return STk_void;
}

DEFINE_PRIMITIVE("ldap-get-password", ldap_getpass, subr0, (void))
{
  char *s;
  SCM z;

  s = getpass("");
  if (!s) STk_error("terminal not available");

   z = STk_Cstring2string(s);
   memset(s, '\0', strlen(s)); /* more secure? */
   return z;
}


#endif /* HAVE_LDAP */

/*===========================================================================*\
 * 
 *				Initialization
 * 
\*===========================================================================*/
MODULE_ENTRY_START("ldap") 
{
#ifdef HAVE_LDAP
  DEFINE_USER_TYPE(tc_ldap, &xtype_ldap);
  ADD_PRIMITIVE(ldap_connect);
  ADD_PRIMITIVE(ldap_close);
  ADD_PRIMITIVE(ldap_search);
  ADD_PRIMITIVE(ldap_add);
  ADD_PRIMITIVE(ldap_del);
  ADD_PRIMITIVE(ldap_modify);
  ADD_PRIMITIVE(ldap_getpass);
#else
  STk_error("LDAP support has not been build on this platform");
#endif
}
MODULE_ENTRY_END
