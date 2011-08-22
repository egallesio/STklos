/*
 * f p o r t . h				-- File ports
 *
 * Copyright © 2000-2007 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date:  8-Jan-2000 14:48 (eg)
 * Last file update: 29-Jun-2007 22:18 (eg)
 *
 * This implementation is built by reverse engineering on an old SUNOS 4.1.1
 * stdio.h. It has been simplified to fit the needs for STklos. In particular
 * non buffered file are not implemented. Anyway this is faster than an
 * implementation using the C buffered IO (at least on glibc)
 *
 */

#define TTY_BUFSIZE   	256
#define OTHER_BUFSIZE 	4096

#define STK_IOFBF	(1 << 0) /* Full buffered*/
#define STK_IOLBF	(1 << 1) /* Line buffered */
#define STK_IONBF	(1 << 2) /* No buffered (unused for now) */
#define STK_IOEOF	(1 << 3) /* EOF encountered on this file */
#define STK_IOREAD	(1 << 4) /* File is opened in read */

struct fstream {
  unsigned char *base;  /* buffer start */
  unsigned char *ptr;   /* ptr on the current character in buffer  */
  int	cnt;	        /* # of chars in the buffer */
  int	bufsize;        /* buffer size */
  int   stream_flags;   /* flags */
  FILE  *f;             /* the file itself */
  short fd;	        /* file descriptor */
  SCM  read_event;
  SCM  write_event;
  SCM  idle_procs;
  void *user_data;
  int (*low_read)(struct fstream *f, void *buf, int count);
  int (*low_write)(struct fstream *f, void *buf, int count);
};


#define PORT_BASE(x)    	(((struct fstream *) (x))->base)
#define PORT_PTR(x)     	(((struct fstream *) (x))->ptr)
#define PORT_CNT(x)     	(((struct fstream *) (x))->cnt)
#define PORT_BUFSIZE(x) 	(((struct fstream *) (x))->bufsize)
#define PORT_STREAM_FLAGS(x) 	(((struct fstream *) (x))->stream_flags)
#define PORT_FILE(x)  		(((struct fstream *) (x))->f)
#define PORT_FD(x)  		(((struct fstream *) (x))->fd)
#define PORT_REVENT(x)		(((struct fstream *) (x))->read_event)
#define PORT_WEVENT(x)		(((struct fstream *) (x))->write_event)
#define PORT_IDLE(x)		(((struct fstream *) (x))->idle_procs)
#define PORT_USERDATA(x)	(((struct fstream *) (x))->user_data)
#define PORT_LOWREAD(x)		(((struct fstream *) (x))->low_read)
#define PORT_LOWWRITE(x)	(((struct fstream *) (x))->low_write)
