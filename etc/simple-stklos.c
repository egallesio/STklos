/*
 * stklos.c	-- STklos interpreter main function
 *
 * Copyright © 1999-2000 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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
 *    Creation date: 28-Dec-1999 21:19 (eg)
 */

#include "stklos.h"
#define BOOT_FILE "./boot.img"


int main(int argc, char *argv[])
{
  int ret;

  if (!STk_init_library()) {
    fprintf(stderr, "cannot initialize the STklos library\nABORT\n");
    exit(1);
  }

  /* boot the VM */
  ret = STk_load_code(BOOT_FILE);
  if (ret < 0) {
    fprintf(stderr, "cannot boot with \"%s\" file (code=%d)\n", BOOT_FILE, ret);
    exit(1);
  }
  return ret;
}
