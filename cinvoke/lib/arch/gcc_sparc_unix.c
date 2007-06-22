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
#ifdef CINVOKE_BUILD
#include "../cinvoke.h"
#include "../cinvoke-private.h"
#else
#include "cinvoke.h"
#include "cinvoke-private.h"
#endif

#include <dlfcn.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

void arch_free_errstr(char *str) {
	free(str);
}

cinv_status_t arch_library_create(CInvContext *context, const char *path,
	ArchLibrary *library_out) {
	void *dl = dlopen(path, RTLD_LAZY);
	if (!dl) {
		context_set_error(context, -1, strdup(dlerror()), 1);
		return CINV_ERROR;
	}
		
	library_out->dl = dl;

	return CINV_SUCCESS;
}
cinv_status_t arch_library_get_entrypoint(CInvContext *context,
	ArchLibrary *library, const char *name, void **entrypoint_out) {
	void *sym = dlsym(library->dl, name);
	if (!sym) {
		context_set_error(context, -1, strdup(dlerror()), 1);
		return CINV_ERROR;
	}

	*entrypoint_out = sym;

	return CINV_SUCCESS;
}
cinv_status_t arch_library_delete(CInvContext *context, ArchLibrary *library) {
	if (dlclose(library->dl)) {
		context_set_error(context, -1, strdup(dlerror()), 1);
		return CINV_ERROR;
	}

	return CINV_SUCCESS;
}


const static int LEN = 8192;

void copy_hi(void *dest, int val) {
	int tmp;
	memcpy(&tmp, dest, 4);
	tmp &= 0xFFC00000;
	tmp |= val;
	memcpy(dest, &tmp, 4);
}
void copy_lo(void *dest, short val) {
	short tmp;
	memcpy(&tmp, dest, 2);
	tmp &= 0xFC00;
	tmp |= val;
	memcpy(dest, &tmp, 2);
}

char *arch_callback_stub(void *functionp, void *param,
	short stacksize, cinv_callconv_t cc, cinv_type_t types[], int numparams) {
	int functionphi = ((int)functionp >> 10);
	short functionplo = ((int)functionp & 0x3FF);
	int paramhi = ((int)param >> 10);
	short paramlo = ((int)param & 0x3FF);
	
	char *ret = mmap(0, LEN, PROT_EXEC|PROT_READ|PROT_WRITE,
		MAP_ANON|MAP_PRIVATE, -1, 0);
	if (ret == MAP_FAILED)
		return NULL;
	//void f() {
	//    __asm__("mov %i0, %g5\n"
	//            "mov %i1, %o1\n"
	//            "mov %i2, %o2\n"
	//            "mov %i3, %o3\n"
	//            "mov %i4, %o4\n"
	//            "mov %i5, %o5\n"
	//            "sethi  %hi(0xaaaab800), %g1\n"
	//            "or  %g1, 0x3bb, %l0\n"
	//            "sethi  %hi(0xccccdc00), %g1\n"
	//            "or  %g1, 0x1dd, %o0\n"
	//            "call  %l0\n"
	//            "nop\n"
	//            "nop\n");
	//}
	memcpy(ret,
		"\x9d\xe3\xbf\x90\x8a\x10\x00\x18"
		"\x92\x10\x00\x19\x94\x10\x00\x1a"
		"\x96\x10\x00\x1b\x98\x10\x00\x1c"
		"\x9a\x10\x00\x1d\x03\x2a\xaa\xae"
		"\xa0\x10\x63\xbb\x03\x33\x33\x37"
		"\x90\x10\x61\xdd\x9f\xc4\x00\x00"
		"\x01\x00\x00\x00\x01\x00\x00\x00"
		"\x81\xc7\xe0\x08\x81\xe8\x00\x00",
		64);
	copy_hi(ret + 28, functionphi);
	copy_lo(ret + 34, functionplo);
	copy_hi(ret + 36, paramhi);
	copy_lo(ret + 42, paramlo);
	
	return ret;
}

void arch_free_stub(char *stub) {
	munmap(stub, LEN);
}

int arch_is_register_parm(cinv_callconv_t callingconvention, int index,
	int num_params, cinv_type_t types[]) {
	int num = 0;
	int i;
	for (i = 0; i <= index; i++) {
		if (types[i] == CINV_T_EXTRALONG ||
			types[i] == CINV_T_DOUBLE)
			num += 2;
		else
			num++;
	}
	return (num <= 6);
}
void set_parm(ArchRegParms *regparms, int index, void *p, cinv_type_t type) {
	int *toset[] = {
		&(regparms->io0), &(regparms->io1), &(regparms->io2),
		&(regparms->io3), &(regparms->io4), &(regparms->io5) 
	};
	switch (type) {
	case CINV_T_CHAR:
		*(toset[index]) = *(char *)p;
		break;
	case CINV_T_SHORT:
		*(toset[index]) = *(short *)p;
		break;
	case CINV_T_PTR:
	case CINV_T_INT:
	case CINV_T_LONG:
		*(toset[index]) = *(int *)p;
		break;
	case CINV_T_DOUBLE:
	case CINV_T_EXTRALONG:
		*(toset[index]) = (int)((*(long long *)p) >> 32);
		if (index < 5)
			*(toset[index + 1]) = (int)((*(long long *)p) & 0xFFFFFFFF);
		break;
	case CINV_T_FLOAT:
		*((float *)toset[index]) = *(float *)p;
		break;
	default: break;
	}
}

void arch_set_register_parms(ArchRegParms *regparms, 
	cinv_callconv_t callingconvention, int num_params, void *parameters[], 
	cinv_type_t types[]) {
	int num = 0;
	int i;
	for (i = 0; i < num_params; i++) {
		if (num < 6) {
			set_parm(regparms, num, parameters[i], types[i]);
			if (types[i] == CINV_T_EXTRALONG ||
				types[i] == CINV_T_DOUBLE)
				num += 2;
			else
				num++;
		}
	}
}
void get_parm(void *po, int index, ArchRegParms *regparms, cinv_type_t type) {
	int *toget[] = {
		&(regparms->io0), &(regparms->io1), &(regparms->io2),
		&(regparms->io3), &(regparms->io4), &(regparms->io5) 
	};
	switch (type) {
	case CINV_T_CHAR:
		*(char *)po = *toget[index];
		break;
	case CINV_T_SHORT:
		*(short *)po = *toget[index];
		break;
	case CINV_T_PTR:
	case CINV_T_INT:
	case CINV_T_LONG:
		*(int *)po = *toget[index];
		break;
	case CINV_T_DOUBLE:
	case CINV_T_EXTRALONG:
		if (index < 5) {
			*(long long *)po = ((long long)*toget[index]) << 32;
			*(long long *)po |= *toget[index + 1];
		} else {
			*(long long *)po &= 0xFFFFFFFF;
			*(long long *)po |= ((long long)*toget[index]) << 32;
		}
		break;
	case CINV_T_FLOAT:
		*(float *)po = *((float *)toget[index]);
		break;
	default: break;
	}
}
void arch_get_register_parms(ArchRegParms *regparms,
	cinv_callconv_t callingconvention, int num_params, void *parameters_out[],
	cinv_type_t types[]) {
	int num = 0;
	int i;
	for (i = 0; i < num_params; i++) {
		if (num < 6) {
			get_parm(parameters_out[i], num, regparms, types[i]);
			if (types[i] == CINV_T_EXTRALONG ||
				types[i] == CINV_T_DOUBLE)
				num += 2;
			else
				num++;
		}
	}
}

void arch_getval_char(ArchRetValue *archval, char *outval) {
	*outval = archval->ivalhi;
}
void arch_getval_short(ArchRetValue *archval, short *outval) {
	*outval = archval->ivalhi;
}
void arch_getval_int(ArchRetValue *archval, int *outval) {
	*outval = archval->ivalhi;
}
void arch_getval_long(ArchRetValue *archval, long int *outval) {
	*outval = archval->ivalhi;
}
void arch_getval_extralong(ArchRetValue *archval, long long int *outval) {
	*outval = archval->ivallo;
	*outval |= (((long long)archval->ivalhi) << 32);
}
void arch_getval_float(ArchRetValue *archval, float *outval) {
	*outval = *(float *)&archval->dval;
}
void arch_getval_double(ArchRetValue *archval, double *outval) {
	*outval = archval->dval;
}
void arch_getval_ptr(ArchRetValue *archval, void **outval) {
	*outval = (void *)archval->ivalhi;
}

void arch_setval_char(ArchRetValue *archval, char val) {
	archval->ivalhi = val;
}
void arch_setval_short(ArchRetValue *archval, short val) {
	archval->ivalhi = val;
}
void arch_setval_int(ArchRetValue *archval, int val) {
	archval->ivalhi = val;
}
void arch_setval_long(ArchRetValue *archval, long int val) {
	archval->ivalhi = val;
}
void arch_setval_extralong(ArchRetValue *archval, long long int val) {
	archval->ivallo = (val & 0xFFFFFFFF);
	archval->ivalhi = val >> 32;
}
void arch_setval_float(ArchRetValue *archval, float val) {
	archval->dval = *(double *)&val;
}
void arch_setval_double(ArchRetValue *archval, double val) {
	archval->dval = val;
}
void arch_setval_ptr(ArchRetValue *archval, void *val) {
	archval->ivalhi = (int)val;
}

void arch_size_char(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 4;
	*structsize_out = 1;
}
void arch_size_short(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 4;
	*structsize_out = 2;
	*structalign_out = 2;
}
void arch_size_int(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 4;
	*structsize_out = 4;
	*structalign_out = 4;
}
void arch_size_long(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 4;
	*structsize_out = 4;
	*structalign_out = 4;
}
void arch_size_extralong(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 8;
	*structsize_out = 8;
	*structalign_out = 4;
}
void arch_size_float(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 4;
	*structsize_out = 4;
	*structalign_out = 4;
}
void arch_size_double(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 8;
	*structsize_out = 8;
	*structalign_out = 8;
}
void arch_size_ptr(int *stacksize_out, int *structsize_out,
	int *structalign_out) {
	*stacksize_out = 4;
	*structsize_out = 4;
	*structalign_out = 4;
}
