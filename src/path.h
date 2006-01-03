/*
 * p a t h . h			-- Path names management
 * 
 * Copyright © 2000-2004 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date:  9-Jan-2000 14:25 (eg)
 * Last file update: 14-Jul-2004 16:15 (eg)
 */

#ifdef OLD_WIN32
#  include <windows.h>
#  include <io.h>
#  define F_OK   00
#  define X_OK   01
#  define W_OK   02
#  define R_OK   04
#  define ISDIRSEP(ch) 	 ((ch)=='\\' || (ch)=='/')
#  define ISABSOLUTE(cp) (ISDIRSEP(*cp) || \
			 ((strlen(cp)>=3) && isalpha(*cp) &&(cp[1]==':') && \
			 ISDIRSEP(cp[2])))
#  define DIRSEP 	 '\\'
#  define SDIRSEP  	 "\\"
#  define PATHSEP	 ';'
#else /* OLD_WIN32 */
#  include <unistd.h>
#  include <pwd.h>
#  define ISDIRSEP(ch) 	 ((ch)=='/')
#  define ISABSOLUTE(s) (ISDIRSEP(*s))
#  define DIRSEP 	 '/'
#  define SDIRSEP  	 "/"
#  define PATHSEP	 ':'
#endif /* OLD_WIN32 */


#include <sys/types.h>
#include <sys/stat.h>

#ifdef OLD_WIN32
   /* One of above files includes <stdarg.h> with BC++  (and stdarg and 
    * vararg are not compatible 
    */
#  undef __STDARG_H 

#  ifdef _MSC_VER
#  include <direct.h>
#  include <process.h>
#  include <sys/stat.h>
#  define S_ISDIR(mode) ((mode & _S_IFMT) == _S_IFDIR)
#  define S_ISREG(mode) ((mode & _S_IFMT) == _S_IFREG)
#  else
#     ifdef BCC
        /* Borland defines the opendir/readdir/closedir functions. Use them. */
#       include <dirent.h>
#     endif
#  endif
#else
#  include <dirent.h>
#endif

