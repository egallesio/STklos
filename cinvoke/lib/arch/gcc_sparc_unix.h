/*
C/Invoke Source Code File

Copyright (c) 2006 Will Weisser

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
   3. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/
#ifndef _ARCH_GCC_SPARC_UNIX_H
#define _ARCH_GCC_SPARC_UNIX_H

#include <errno.h>

typedef struct _ArchLibrary {
	void *dl;
} ArchLibrary;

typedef struct _ArchRetValue {
	int ivallo;
	int ivalhi;
	double dval;
} ArchRetValue;

typedef struct _ArchRegParms {
	int io0;
	int io1;
	int io2;
	int io3;
	int io4;
	int io5;
} ArchRegParms;

typedef char cinv_int8_t;
typedef short cinv_int16_t;
typedef int cinv_int32_t;
typedef long long cinv_int64_t;

#define CINV_E_NOMEM ((cinv_int32_t)ENOMEM)
#define CINV_S_NOMEM (strerror(ENOMEM))
#define CINV_NOMEM_NEEDSFREE 0
#define CINV_E_INVAL ((cinv_int32_t)EINVAL)

#define CINV_CC_DEFAULT CINV_CC_CDECL
#define CINV_T_2BYTE CINV_T_SHORT
#define CINV_T_4BYTE CINV_T_INT
#define CINV_T_8BYTE CINV_T_EXTRALONG

/////////////////////////////////////
// macros
/////////////////////////////////////
#define ARCH_SAVE_REGPARMS(regparms) \
	__asm__("st %%g5, %0\n \
			st %%i1, %1\n \
			st %%i2, %2\n \
			st %%i3, %3\n \
			st %%i4, %4\n \
			st %%i5, %5\n" : \
				"=m" ((regparms).io0), \
				"=m" ((regparms).io1), \
				"=m" ((regparms).io2), \
				"=m" ((regparms).io3), \
				"=m" ((regparms).io4), \
				"=m" ((regparms).io5));

#define ARCH_CALL(regparms, ep) \
	__asm__("ld %0, %%o0\n \
			ld %1, %%o1\n \
			ld %2, %%o2\n \
			ld %3, %%o3\n \
			ld %4, %%o4\n \
			ld %5, %%o5\n \
			ld %6, %%g1\n \
			call %%g1, 0\n \
			nop\n" :: \
			"m" ((regparms).io0), \
			"m" ((regparms).io1), \
			"m" ((regparms).io2), \
			"m" ((regparms).io3), \
			"m" ((regparms).io4), \
			"m" ((regparms).io5), \
			"m" (ep) : \
			"%o0", "%o1", "%o2", "%o3", \
			"%o4", "%o5", \
			"%g1");

#define ARCH_SAVE_RETURN(archvalue, type) \
	__asm__("st %%o0, %0\n \
			st %%o1, %1\n \
			std %%f0, %2\n" : \
			"=m" ((archvalue).ivalhi), \
			"=m" ((archvalue).ivallo), \
			"=m" ((archvalue).dval));

#define ARCH_SET_RETURN(archvalue, type) \
	__asm__("ld %0, %%i0\n \
			ld %1, %%i1\n \
			ld %2, %%f0\n \
			ld %3, %%f1\n" :: \
			"m" ((archvalue).ivalhi), \
			"m" ((archvalue).ivallo), \
			"m" ((archvalue).dval), \
			"m" (*(&(archvalue).dval + 4)) : \
			"%i0", "%i1", "%f0", "%f1");

// Allocating stack frames dynamically on sparc is a pain
// since executing a "save" instruction destroys the
// frame pointer, and directly manipulating the value of %sp
// will yield an inconsistent stack if an interrupt occurs.
// To get around this we take advantage of the fact that this
// macro is called after any local variable declarations and
// statically add 512 bytes to the calling stack frame by 
// declaring a local array.  This works great (as long as
// the argument list isnt longer than 512 + whatever other
// room is reserved on the stack!) because we dont care if
// the stpadding array's contents get mangled.  On the other
// hand always creating such a large stack frame in the case
// where the actual number of arguments being pushed may be
// small is a waste.
#define ARCH_PUT_STACK_BYTES(bcount) char __stpadding[512]; \
									__stpadding[0] = '\0';

#define ARCH_REMOVE_STACK_BYTES(bcount) /* empty */

#define ARCH_GET_STACK(sp) \
	__asm__("st %%sp, %0" : "=m" (sp));

#define ARCH_GET_FRAME_PTR(fp) \
	__asm__("st %%fp, %0" : "=m" (fp));

#define ARCH_CALLBACK_ARG_OFFSET (180);

#define ARCH_BIG_ENDIAN 1

#define ARCH_STACK_GROWS_DOWN 1

#define ARCH_STACK_SKIPTOP 68

#define ARCH_REGPARMS_IN_STACKSIZE 1

#define ARCH_CLAMP_NONFIRST_STRUCTALIGN 0

#endif
