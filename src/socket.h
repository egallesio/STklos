/*
 * socket.c				-- Socket acess for STklos
 *
 * Copyright © 2003-2005 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 */

#ifndef _STKLOS_SOCKETS
#define _STKLOS_SOCKETS

/*===========================================================================*\
 *
 *			Definition of the socket type
 *
\*===========================================================================*/
struct socket_obj {
  stk_header header;
  long portnum;
  SCM hostname, hostip;
  int type, fd;
  SCM input, output;
  SCM ready_event;
  SCM data; 	/* parent for a client socket , children for a server socket */
  void * userdata;
  void (*usershutdown)(struct socket_obj *sock);
};

#define SOCKETP(x)		(BOXED_TYPE_EQ((x),tc_socket))
#define SOCKET_PORTNUM(x) 	(((struct socket_obj *) (x))->portnum)
#define SOCKET_HOSTNAME(x)	(((struct socket_obj *) (x))->hostname)
#define SOCKET_HOSTIP(x)	(((struct socket_obj *) (x))->hostip)
#define SOCKET_FD(x)		(((struct socket_obj *) (x))->fd)
#define SOCKET_TYPE(x)		(((struct socket_obj *) (x))->type)
#define SOCKET_IN(x)		(((struct socket_obj *) (x))->input)
#define SOCKET_OUT(x)		(((struct socket_obj *) (x))->output)
#define SOCKET_READY(x)		(((struct socket_obj *) (x))->ready_event)
#define SOCKET_PARENT(x)	(((struct socket_obj *) (x))->data)
#define SOCKET_CHILDREN(x)	(((struct socket_obj *) (x))->data) /* also */
#define SOCKET_USER_DATA(x)	(((struct socket_obj *) (x))->userdata)
#define SOCKET_USER_SHUTDOWN(x) (((struct socket_obj *) (x))->usershutdown)

#define SOCKET_SERVER		0
#define SOCKET_CLIENT		1

#endif
