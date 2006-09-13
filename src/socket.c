/*
 * socket.c				-- Socket acess for STklos
 * 
 * Copyright © 2003-2006 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date:  3-Jan-2003 18:45 (eg)
 * Last file update: 26-May-2006 10:52 (eg)
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <memory.h>
#include <errno.h>

#include <stklos.h>
#include "socket.h"

#ifdef THREADS_LURC
# include <lurc.h>
/* use the lurc IO wrapper */
# define socket(d,t,p)   lurc_io_socket(d,t,p)
# define connect(fd,a,l) lurc_io_connect(fd,a,l)
# define accept(fd,a,l)  lurc_io_accept(fd,a,l)
# define close(fd)       lurc_io_close(fd)
# define shutdown(fd,f)  lurc_io_shutdown(fd,f)
#endif

/*===========================================================================*\
 * 
 *			Definition of the socket type 
 *
\*===========================================================================*/
static void print_socket(SCM sock, SCM port, int mode)
{
  char buffer[1000];
  SCM name = SOCKET_HOSTNAME(sock);
  
  sprintf(buffer, "#[%s-socket %s:%ld (%d)]", 
	  (SOCKET_TYPE(sock) == SOCKET_SERVER) ? "server" : "client",
	  ((name == STk_false) ?  "*none*" : (char *) STRING_CHARS(name)),
	  SOCKET_PORTNUM(sock), SOCKET_FD(sock));
  STk_puts(buffer, port);
}

static struct extended_type_descr xtype_socket = {
  "socket",
  print_socket
};


static void socket_finalizer(SCM socket);

/*===========================================================================*\
 * 
 *				UTILITIES
 *
\*===========================================================================*/

static void error_bad_hostname(SCM obj)
{
  STk_error("bad hostname ~S", obj);
}

static void error_bad_portnum(SCM obj)
{
  STk_error("bad port number ~S", obj);
}

static void error_unknown_hostname(SCM obj)
{
  STk_error("unknown or misspelled host name ~S", obj);
}

static void error_cannot_crate_socket(void)
{
  STk_error("Cannot create socket");
}

static void error_bad_socket(SCM obj)
{
  STk_error("bad socket ~S", obj);
}


static void set_socket_io_ports(SCM sock, int line_buffered)
{
  int s, t;
  char *hostname, *fname;
  SCM in, out;

  hostname = STRING_CHARS(SOCKET_HOSTNAME(sock));
  fname    = STk_must_malloc_atomic(strlen(hostname) + 30);  
  sprintf(fname, "%s:%ld", hostname, SOCKET_PORTNUM(sock));
  
  s   = SOCKET_FD(sock);
  t   = dup(s);
  in  = STk_fd2scheme_port(s, "r", fname);
  out = STk_fd2scheme_port(t, "w", fname);

  if (NULLP(in) || NULLP(out)) 
    STk_error("cannot create socket IO ports");

  SOCKET_IN(sock)  = in;
  SOCKET_OUT(sock) = out;
  
  /* Set line buffered mode (if needed) */
  if (line_buffered) {
    STk_set_line_buffered_mode(in); /* not really useful */
    STk_set_line_buffered_mode(out);
  }
}


/*===========================================================================*\
 * 
 *				MAKE-SERVER-SOCKET
 *
\*===========================================================================*/

/*
<doc EXT make-server-socket
 * (make-server-socket)
 * (make-server-socket port-number)
 * 
 * |make-server-socket| returns a new socket object. If |port-number|
 * is specified, the socket is listening on the specified port; 
 * otherwise, the communication port is chosen by the system. 
doc>
*/
DEFINE_PRIMITIVE("make-server-socket", make_server_socket, subr01, (SCM port))
{
  struct sockaddr_in sin;
  socklen_t len;
  int long portnum;
  SCM z;
  int s, yes = 1;
  
  /* Determine port to use */
  portnum = (port) ? STk_integer_value(port) : 0;
  if (portnum < 0)  error_bad_portnum(port);

  /* Create a socket */
  s = socket(AF_INET, SOCK_STREAM, 0);
  if (s < 0) STk_error("cannot create socket");
  
  /* Allow reusing the socket port */
  if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes)) < 0) {
    close(s);
    STk_error(strerror(errno));
  }

  /* Bind the socket to a name */
  sin.sin_family      = AF_INET;
  sin.sin_port 	      = htons(portnum);
  sin.sin_addr.s_addr = INADDR_ANY;

  if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
    close(s);
    STk_error(strerror(errno));
  }

  /* Query the socket name (permits to get the true socket number if 0 was given */
  len = sizeof(sin);
  if (getsockname(s, (struct sockaddr *) &sin,  &len) < 0) {
    close(s);
    STk_error(strerror(errno));
  }

  /* Indicate that we are ready to listen */
  if (listen(s, 5) < 0) {
    close(s);
    STk_error(strerror(errno));
  }

  /* Now we can create the socket object */
  NEWCELL(z, socket);
  SOCKET_PORTNUM(z)	= ntohs(sin.sin_port);
  SOCKET_HOSTNAME(z)    = STk_false;
  SOCKET_HOSTIP(z)      = STk_false;
  SOCKET_FD(z)	 	= s;
  SOCKET_TYPE(z)	= SOCKET_SERVER;
  SOCKET_IN(z)		= STk_false;
  SOCKET_OUT(z)		= STk_false;
  SOCKET_READY(z)	= STk_false;
  SOCKET_CHILDREN(z)	= STk_nil;
  SOCKET_USER_DATA(z)	= NULL;
  
  STk_register_finalizer(z, socket_finalizer);
  
  return z;
}


/*===========================================================================*\
 * 
 *				MAKE-CLIENT-SOCKET
 *
\*===========================================================================*/

/*
<doc EXT make-client-socket
 *  (make-client-socket hostname port-number)
 *  (make-client-socket hostname port_number line-buffered) 
 * 
 * |make-client-socket| returns a new socket object. This socket
 * establishes a link between the running program and the application
 * listening on port |port-number| of |hostname|.  If  the optional argument
 * |line-buffered| has a true value, a line buffered policy is used when writing 
 * to the client socket (i.e. characters on the socket are tranmitted as soon 
 * as a ,(code "#\\newline") character is encountered). The default value of 
 * |line-buffered| is |#t|.
 * 
doc>
*/
static SCM internal_init_client_socket(SCM hostname, SCM port)
{
  struct hostent *hp;
  struct sockaddr_in server;
  int s, yes=1;
  SCM z;
  long val = STk_integer_value(port);
  
  /* Verify arguments */
  if(!STRINGP(hostname)) error_bad_hostname(hostname);
  if(val == LONG_MIN)    error_bad_portnum(port);

  /* Locate the host IP address */
  if ((hp=gethostbyname(STRING_CHARS(hostname))) == NULL)
    error_unknown_hostname(hostname);

  /* Get a socket */
  if ((s=socket(AF_INET,SOCK_STREAM,0)) < 0) error_cannot_crate_socket();
  
  /* Setup a connect address */
  memset(&server, 0, sizeof(server));
  memcpy((char*)&server.sin_addr, hp->h_addr, hp->h_length);
  server.sin_family = AF_INET;
  server.sin_port   = htons(val);

  /* Try to connect */
  if ((connect(s, (struct sockaddr *) &server, sizeof(server))    < 0) ||
      (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes)) < 0)) {
    close(s);
    STk_error("system reported \"%s\"", strerror(errno));
  }
  
  /* Create a new Scheme socket object */
  NEWCELL(z, socket);
  
  SOCKET_PORTNUM(z)  = ntohs(server.sin_port); /* Query true value */
  SOCKET_HOSTNAME(z) = STk_Cstring2string((char *) hp->h_name);
  SOCKET_HOSTIP(z)   = STk_Cstring2string((char *) inet_ntoa(server.sin_addr));
  SOCKET_FD(z)	     = s;
  SOCKET_TYPE(z)     = SOCKET_CLIENT;
  SOCKET_IN(z)	     = STk_false;
  SOCKET_OUT(z)	     = STk_false;
  SOCKET_READY(z)    = STk_false;
  SOCKET_PARENT(z)   = STk_nil;
  SOCKET_USER_DATA(z)= NULL;

  STk_register_finalizer(z, socket_finalizer);

  return z;
}


DEFINE_PRIMITIVE("%initialize-client-socket", init_client_socket, subr2,
				 (SCM hostname, SCM port))
{
  return internal_init_client_socket(hostname, port);
}


DEFINE_PRIMITIVE("make-client-socket", make_client_socket, subr23,
		 (SCM hostname, SCM port, SCM line_buffered))
{
 SCM sock;

 if (!line_buffered) line_buffered = STk_true;

 sock = internal_init_client_socket(hostname, port);
 set_socket_io_ports(sock,(line_buffered != STk_false));

 return sock;
}


/*===========================================================================*\
 * 
 *				SOCKET-SHUTDOWN
 *
\*===========================================================================*/

/*
<doc EXT socket-shutdown
 * (socket-shutdown sock)
 * (socket-shutdown sock close)
 *
 * |Socket-shutdown| shutdowns the connection associated to
 * |socket|. If the socket is a server socket, |socket-shutdown| is called
 * on all the client sockets connected to this server.
 * |Close| indicates if the the socket must be closed or not, when
 * the connection is destroyed. Closing the socket forbids further 
 * connections on the same port with the |socket-accept| procedure.
 * Omitting a value for |close| implies the closing of socket.
 * £
 * The following example shows a simple server: when there is 
 * a new connection on the port number 12345, the server displays 
 * the first line sent to it by the client, discards the others and
 * go back waiting for further client connections.
 * @lisp
 * (let ((s (make-server-socket 12345)))
 *    (let loop ()
 *       (let ((ns (socket-accept s)))
 *         (format #t "I've read: ~A\\n" 
 *                 (read-line (socket-input ns)))
 *         (socket-shutdown ns #f)
 *         (loop))))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("socket-shutdown", socket_shutdown, subr12, (SCM sock, SCM closeit))
{
  if (!SOCKETP(sock)) error_bad_socket(sock);
  if (closeit != STk_false) closeit = STk_true;

  /* Eventually close the associated sockets */
  if (SOCKET_TYPE(sock) == SOCKET_SERVER) { /* Shutdown associated client sockets */
    SCM tmp;

    for (tmp = STk_copy_tree(SOCKET_CHILDREN(sock)); !NULLP(tmp); tmp = CDR(tmp)){
      STk_socket_shutdown(CAR(tmp), closeit);
    }
  } else {    /* Delete this socket from the server socket (if needed) */
    SCM parent = SOCKET_PARENT(sock);

    if (!NULLP(parent)) {
      SOCKET_CHILDREN(parent) = STk_dremq(sock, SOCKET_CHILDREN(parent));
      SOCKET_PARENT(sock) = STk_nil;
    }
  }
    
  /* 
   * Warning: input and output can have already be garbaged: if the
   * socket is no more used, the input and output are not marked as
   * used and can (eventually) be released before the call to shutdown
   * by the socket finalizer.
   */
  if (SOCKET_FD(sock) >= 0) {
    if (closeit == STk_true) shutdown(SOCKET_FD(sock), 2);
    close(SOCKET_FD(sock));
    SOCKET_FD(sock) = -1;
  }
  if (SOCKET_IN(sock) != STk_false) {
    STk_close_port(SOCKET_IN(sock));
    SOCKET_IN(sock) = STk_false;
  }
  if (SOCKET_OUT(sock) != STk_false) {
    STk_close_port(SOCKET_OUT(sock));
    SOCKET_OUT(sock) = STk_false;
  }

  /* Call user shutdown hook */
  if (SOCKET_USER_DATA(sock))
    (*SOCKET_USER_SHUTDOWN(sock))(sock);

  return STk_void;
}


static void socket_finalizer(SCM socket)
{
  STk_socket_shutdown(socket, STk_true);
}

/*===========================================================================*\
 * 
 *				SOCKET-ACCEPT 
 *
\*===========================================================================*/

/*
<doc EXT socket-accept
 * (socket-accept socket)
 * (socket-accept socket line-buffered)
 * 
 * |socket-accept| waits for a client connection on the given
 * |socket|. If no client is already waiting for a connection, this
 * procedure blocks its caller; otherwise, the first connection request 
 * on the queue of pending connections is connected and |socket-accept| 
 * returns a new client socket to serve this request. 
 * This procedure must be called on a server socket created
 * with |make-server-socket|. The result of |socket-accept| is undefined. 
 * |Line-buffered| indicates if the port should be considered as a 
 * line buffered. If |line-buffered| is omitted, it defaults to |#t|.
 * £
 * The following example is a simple server which waits for a connection
 * on the port 12345 ,(footnote [Under Unix, you can simply connect to
 * a listening socket with the |telnet| command. With the given
 * example, this can be achieved by typing the following command in a 
 * window shell: 
 * ,(raw-code "$ telnet localhost 12345")]).
 *  Once the connection with the
 * distant program is established, we read a line on the input port
 * associated to the socket and we write the length of this line on its
 * output port. 
 * @lisp
 * (let* ((server (make-server-socket 13345))
 *        (client (socket-accept server))
 *        (l      (read-line (socket-input client))))
 *   (format (socket-output client) 
 *           "Length is: ~a\n" (string-length l))
 *   (socket-shutdown server))
 * @end lisp
 *
 *  Note that shutting down the |server| socket suffices here to close
 * also the connection to |client|. 
doc>
*/
DEFINE_PRIMITIVE("socket-accept", socket_accept, subr12, 
		 (SCM serv, SCM line_buffered))
{
  char *s;
  struct sockaddr_in sin;
  struct hostent *host;
  socklen_t len = sizeof(sin);
  int new_s;
  SCM z;

  if (!SOCKETP(serv)) STk_error("bad socket ~S", serv);
  if (!line_buffered) line_buffered = STk_true;
 
  /* Accept the connection */
  new_s = accept(SOCKET_FD(serv), (struct sockaddr *) &sin, &len);
  if (new_s < 0) STk_error(strerror(errno));

  /* Set the client info (if possible its name, otherwise its IP number) */
  host = gethostbyaddr((char *) &sin.sin_addr, sizeof(sin.sin_addr), AF_INET);
  s    = (char *) inet_ntoa(sin.sin_addr);

  /* Allocate and fill the new socket client for this connection */
  NEWCELL(z, socket);
  SOCKET_PORTNUM(z)  = SOCKET_PORTNUM(serv);
  SOCKET_HOSTNAME(z) = STk_Cstring2string(host ? (char*)host->h_name : s);
  SOCKET_HOSTIP(z)   = STk_Cstring2string(s);
  SOCKET_FD(z)	     = new_s;
  SOCKET_TYPE(z)     = SOCKET_CLIENT;
  SOCKET_IN(z)	     = STk_false;
  SOCKET_OUT(z)	     = STk_false;
  SOCKET_READY(z)    = STk_false;
  SOCKET_PARENT(z)   = serv;
  SOCKET_USER_DATA(z)= NULL;

  set_socket_io_ports(z, (line_buffered != STk_false));

  /* Place this socket in the socket list of its parent */
  SOCKET_CHILDREN(serv) = STk_cons(z, SOCKET_CHILDREN(serv));

  STk_register_finalizer(z, socket_finalizer);

  return z;
}


/*===========================================================================*\
 * 
 *				SOCKET PRIMITIVES 
 *
\*===========================================================================*/

/*
<doc EXT socket?
 * (socket? obj)
 *
 * Returns |#t| if |socket| is a socket, otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("socket?", socketp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(SOCKETP(obj));
}


/*
<doc EXT socket-client?
 * (socket-client? obj)
 *
 * Returns |#t| if |socket| is a client socket, otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("socket-client?", socket_clientp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(SOCKETP(obj) && SOCKET_TYPE(obj) == SOCKET_CLIENT);
}


/*
<doc EXT socket-server?
 * (socket-server? obj)
 *
 * Returns |#t| if |socket| is a server socket, otherwise returns |#f|.
doc>
*/
DEFINE_PRIMITIVE("socket-client?", socket_serverp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(SOCKETP(obj) && SOCKET_TYPE(obj) == SOCKET_CLIENT);
}


/*
<doc EXT socket-port-number
 * (socket-port-number socket)
 *
 * Returns the integer number of the port used for |socket|.
doc>
*/
DEFINE_PRIMITIVE("socket-port-number", socket_port_number, subr1, (SCM sock))
{
  if (!SOCKETP(sock)) error_bad_socket(sock);
  return MAKE_INT(SOCKET_PORTNUM(sock));
}


/*
<doc EXT socket-input socket-output
 * (socket-input socket)
 * (socket-output socket)
 *
 * Returns the file port associated for reading or writing with the
 * program connected with |socket|. If no connection has already been
 * established, these functions return |#f|.
 *
 * The following example shows how to make a client socket. Here we
 * create a socket on port 13 of the machine |kaolin.unice.fr|,(footnote
 * [Port 13 is generally used for testing:
 * making a connection to it permits to know the distant system's idea
 * of the time of day.]):
 * @lisp
 * (let ((s (make-client-socket "kaolin.unice.fr" 13)))
 *   (format #t "Time is: ~A~%" (read-line (socket-input s)))
 *   (socket-shutdown  s))
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("socket-input", socket_input, subr1, (SCM sock))
{
  if (!SOCKETP(sock)) error_bad_socket(sock);
  return SOCKET_IN(sock);
}


DEFINE_PRIMITIVE("socket-output", socket_output, subr1, (SCM sock))
{
  if (!SOCKETP(sock)) error_bad_socket(sock);
  return SOCKET_OUT(sock);
}


/*
<doc EXT socket-host-name
 * (socket-host-name socket)
 *
 * Returns a string which contains the name of the distant host
 * attached to |socket|. If |socket| has been created with
 * |make-client-socket| this procedure returns the official name of
 * the distant machine used for connection. If |socket| has been
 * created with |make-server-socket|, this function returns the
< * official name of the client connected to the socket. If no client
 * has used yet |socket|, this function returns |#f|.  
doc>
*/
DEFINE_PRIMITIVE("socket-host-name", socket_host_name, subr1, (SCM sock))
{
  if (!SOCKETP(sock)) error_bad_socket(sock);
  return SOCKET_HOSTNAME(sock);
}


/*
<doc EXT socket-host-address
 * (socket-host-address socket)
 *
 * Returns a string which contains the IP number of the distant host
 * attached to |socket|. If |socket| has been created with
 * |make-client-socket| this procedure returns the IP number of the
 * distant machine used for connection. If |socket| has been created
 * with |make-server-socket|, this function returns the address of the
 * client connected to the socket.  If no client has used yet
 * |socket|, this function returns |#f|.  
doc>
*/
DEFINE_PRIMITIVE("socket-host-address", socket_host_address, subr1, (SCM sock))
{
  if (!SOCKETP(sock)) error_bad_socket(sock);
  return SOCKET_HOSTIP(sock);
}


DEFINE_PRIMITIVE("socket-down?", socket_downp, subr1, (SCM sock))
{
  if (!SOCKETP(sock)) error_bad_socket(sock);
  return MAKE_BOOLEAN(SOCKET_FD(sock) == -1);
}


/*
<doc EXT socket-local-address
 * (socket-local-address socket)
 *
 * Returns a string which contains the IP number of the local host 
 * attached to |socket|.
doc>
*/ 
DEFINE_PRIMITIVE("socket-local-address", socket_local_addr, subr1, (SCM sock))
{
  struct sockaddr_in sin;
  socklen_t len = sizeof(sin);
  
  if (!SOCKETP(sock)) error_bad_socket(sock);

  if (getsockname(SOCKET_FD(sock), (struct sockaddr *) &sin, &len))
    STk_error("cannot get socket name of ~S", sock);
  
  return STk_Cstring2string((char *) inet_ntoa(sin.sin_addr));
}


/*===========================================================================*\
 * 
 *				Initialization
 * 
\*===========================================================================*/
int STk_init_socket(void)
{
  DEFINE_XTYPE(socket, & xtype_socket);

  ADD_PRIMITIVE(init_client_socket);
  ADD_PRIMITIVE(make_server_socket);
  ADD_PRIMITIVE(make_client_socket);
  ADD_PRIMITIVE(socket_shutdown);
  ADD_PRIMITIVE(socket_accept);

  ADD_PRIMITIVE(socketp);
  ADD_PRIMITIVE(socket_clientp);
  ADD_PRIMITIVE(socket_serverp);
  ADD_PRIMITIVE(socket_port_number);
  ADD_PRIMITIVE(socket_input);
  ADD_PRIMITIVE(socket_output);
  ADD_PRIMITIVE(socket_host_name);
  ADD_PRIMITIVE(socket_host_address);
  ADD_PRIMITIVE(socket_downp);
  ADD_PRIMITIVE(socket_local_addr);

  return TRUE;
}
