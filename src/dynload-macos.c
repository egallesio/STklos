a/*
Copyright (c) 2002 Jorge Acereda  <jacereda@users.sourceforge.net> &
                   Peter O'Gorman <ogorman@users.sourceforge.net>
                   
Portions may be copyright others, see the AUTHORS file included with this
distribution.

Maintained by Peter O'Gorman <ogorman@users.sourceforge.net>

Bug Reports and other queries should go to <ogorman@users.sourceforge.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#ifndef _DLFCN_H_
#define _DLFCN_H_

#ifndef DLOPEN_NO_WARN
#warning "You are using dlopen(), a legacy API. Please use the Mach-O dylib loading APIs if at all possible"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined (__GNUC__) && __GNUC__ > 3
#define dl_restrict __restrict
#else
#define dl_restrict
#endif
/*
 * Structure filled in by dladdr().
 */

typedef struct dl_info {
        const char      *dli_fname;     /* Pathname of shared object */
        void            *dli_fbase;     /* Base address of shared object */
        const char      *dli_sname;     /* Name of nearest symbol */
        void            *dli_saddr;     /* Address of nearest symbol */
} Dl_info;

extern void * dlopen(const char *path, int mode);
extern void * dlsym(void * dl_restrict handle, const char * dl_restrict symbol);
extern const char * dlerror(void);
extern int dlclose(void * handle);
extern int dladdr(const void * dl_restrict, Dl_info * dl_restrict);

#define RTLD_LAZY	0x1
#define RTLD_NOW	0x2
#define RTLD_LOCAL	0x4
#define RTLD_GLOBAL	0x8
#define RTLD_NOLOAD	0x10
#define RTLD_NODELETE	0x80

/*
 * Special handle arguments for dlsym().
 */
#define	RTLD_NEXT		((void *) -1)	/* Search subsequent objects. */
#define	RTLD_DEFAULT	((void *) -2)	/* Use default search algorithm. */

#ifdef __cplusplus
}
#endif

#endif /* _DLFCN_H_ */



/*
Copyright (c) 2002 Peter O'Gorman <ogorman@users.sourceforge.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/* Just to prove that it isn't that hard to add Mac calls to your code :)
   This works with pretty much everything, including kde3 xemacs and the gimp,
   I'd guess that it'd work in at least 95% of cases, use this as your starting
   point, rather than the mess that is dlfcn.c, assuming that your code does not
   require ref counting or symbol lookups in dependent libraries
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <limits.h>
#include <mach-o/dyld.h>


#define ERR_STR_LEN 256
static void *dlsymIntern(void *handle, const char *symbol);
static const char *error(int setget, const char *str, ...);



/* Set and get the error string for use by dlerror */
static const char *error(int setget, const char *str, ...)
{
	static char errstr[ERR_STR_LEN];
	static int err_filled = 0;
	const char *retval;
	NSLinkEditErrors ler;
	int lerno;
	const char *dylderrstr;
	const char *file;
	va_list arg;
	if (setget <= 0)
	{
		va_start(arg, str);
		strncpy(errstr, "dlsimple: ", ERR_STR_LEN);
		vsnprintf(errstr + 10, ERR_STR_LEN - 10, str, arg);
		va_end(arg);
	/* We prefer to use the dyld error string if getset is 1*/
		if (setget == 0) {
			NSLinkEditError(&ler, &lerno, &file, &dylderrstr);
			fprintf(stderr,"dyld: %s\n",dylderrstr);
			if (dylderrstr && strlen(dylderrstr))
				strncpy(errstr,dylderrstr,ERR_STR_LEN);
		}		
		err_filled = 1;
		retval = NULL;
	}
	else
	{
		if (!err_filled)
			retval = NULL;
		else
			retval = errstr;
		err_filled = 0;
	}
	return retval;
}

/* dlopen */
void *dlopen(const char *path, int mode)
{
	void *module = 0;
	NSObjectFileImage ofi = 0;
	NSObjectFileImageReturnCode ofirc;
	static int (*make_private_module_public) (NSModule module) = 0;
	unsigned int flags =  NSLINKMODULE_OPTION_RETURN_ON_ERROR | NSLINKMODULE_OPTION_PRIVATE;

	/* If we got no path, the app wants the global namespace, use -1 as the marker
	   in this case */
	if (!path)
		return (void *)-1;

	/* Create the object file image, works for things linked with the -bundle arg to ld */
	ofirc = NSCreateObjectFileImageFromFile(path, &ofi);
	switch (ofirc)
	{
		case NSObjectFileImageSuccess:
			/* It was okay, so use NSLinkModule to link in the image */
			if (!(mode & RTLD_LAZY)) flags += NSLINKMODULE_OPTION_BINDNOW;
			module = NSLinkModule(ofi, path,flags);
			/* Don't forget to destroy the object file image, unless you like leaks */
			NSDestroyObjectFileImage(ofi);
			/* If the mode was global, then change the module, this avoids
			   multiply defined symbol errors to first load private then make
			   global. Silly, isn't it. */
			if ((mode & RTLD_GLOBAL))
			{
			  if (!make_private_module_public)
			  {
			    _dyld_func_lookup("__dyld_NSMakePrivateModulePublic", 
				(unsigned long *)&make_private_module_public);
			  }
			  make_private_module_public(module);
			}
			break;
		case NSObjectFileImageInappropriateFile:
			/* It may have been a dynamic library rather than a bundle, try to load it */
			module = (void *)NSAddImage(path, NSADDIMAGE_OPTION_RETURN_ON_ERROR);
			break;
		case NSObjectFileImageFailure:
			error(0,"Object file setup failure :  \"%s\"", path);
			return 0;
		case NSObjectFileImageArch:
			error(0,"No object for this architecture :  \"%s\"", path);
			return 0;
		case NSObjectFileImageFormat:
			error(0,"Bad object file format :  \"%s\"", path);
			return 0;
		case NSObjectFileImageAccess:
			error(0,"Can't read object file :  \"%s\"", path);
			return 0;		
	}
	if (!module)
		error(0, "Can not open \"%s\"", path);
	return module;
}

/* dlsymIntern is used by dlsym to find the symbol */
void *dlsymIntern(void *handle, const char *symbol)
{
	NSSymbol *nssym = 0;
	/* If the handle is -1, if is the app global context */
	if (handle == (void *)-1)
	{
		/* Global context, use NSLookupAndBindSymbol */
		if (NSIsSymbolNameDefined(symbol))
		{
			nssym = NSLookupAndBindSymbol(symbol);
		}

	}
	/* Now see if the handle is a struch mach_header* or not, use NSLookupSymbol in image
	   for libraries, and NSLookupSymbolInModule for bundles */
	else
	{
		/* Check for both possible magic numbers depending on x86/ppc byte order */
		if ((((struct mach_header *)handle)->magic == MH_MAGIC) ||
			(((struct mach_header *)handle)->magic == MH_CIGAM))
		{
			if (NSIsSymbolNameDefinedInImage((struct mach_header *)handle, symbol))
			{
				nssym = NSLookupSymbolInImage((struct mach_header *)handle,
											  symbol,
											  NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
											  | NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
			}

		}
		else
		{
			nssym = NSLookupSymbolInModule(handle, symbol);
		}
	}
	if (!nssym)
	{
		error(0, "Symbol \"%s\" Not found", symbol);
		return NULL;
	}
	return NSAddressOfSymbol(nssym);
}

const char *dlerror(void)
{
	return error(1, (char *)NULL);
}

int dlclose(void *handle)
{
	if ((((struct mach_header *)handle)->magic == MH_MAGIC) ||
		(((struct mach_header *)handle)->magic == MH_CIGAM))
	{
		error(-1, "Can't remove dynamic libraries on darwin");
		return 0;
	}
	if (!NSUnLinkModule(handle, 0))
	{
		error(0, "unable to unlink module %s", NSNameOfModule(handle));
		return 1;
	}
	return 0;
}


/* dlsym, prepend the underscore and call dlsymIntern */
void *dlsym(void *handle, const char *symbol)
{
	static char undersym[257];	/* Saves calls to malloc(3) */
	int sym_len = strlen(symbol);
	void *value = NULL;
	char *malloc_sym = NULL;

	if (sym_len < 256)
	{
		snprintf(undersym, 256, "_%s", symbol);
		value = dlsymIntern(handle, undersym);
	}
	else
	{
		malloc_sym = malloc(sym_len + 2);
		if (malloc_sym)
		{
			sprintf(malloc_sym, "_%s", symbol);
			value = dlsymIntern(handle, malloc_sym);
			free(malloc_sym);
		}
		else
		{
			error(-1, "Unable to allocate memory");
		}
	}
	return value;
}
