# Hacking STklos

This is a quick guide to STklos hacking. It's not detailed, so the
document doesn't become huge, and also because after understanding the basics,
hacking STklos should not be difficult.

# Directories

The subdirectories in the STklos source tree are:

* `doc` -- documentation, written mostly in Skribe
* `etc` -- various sample files for specific needs
* `examples` -- examples (oh, who could tell?)
* `ffi` -- `libffi` (a local copy)
* `gc` -- the Boehm-Demers-Weiser garbage collector, libgc (a local copy)
* `gmp` -- a slow compatible GNU MP )
* `lib` -- Scheme files, including from basic things like the boot program up to high-level things like modules implementing SRFIs
* `pcre` -- `libpcre` (a local copy)
* `pkgman` -- the package manager
* `src` -- the STklos core, written in C
* `tests` -- the tests, of course!
* `utils` -- utilities and wrappers

# STklos initialization

`main` is in `src/stklos.c`, where command line options are parsed and the
scheme interpreter is started:

* `STk_init_library` -- performs library initialization. This is done in `src/lib.c`, which is a 
  very simple file that just calls several initialization functions. Those functions are defined
  in different files under `src/`;
* `build_scheme_args` -- collects the command line options in the variable `*%program-args*`;
* `STk_load_boot` -- loads the boot file (if one is to be loaded);
* `STk_boot_from_C` -- actually boots the Scheme interpreter. This function is defined in 
  `src/vm.c`, where the STklos virtual machine code is.

In order to include Scheme code for execution during STklos startup, edit `lib/boot.stk`.

# Adding simple modules or SRFIs

* add your `fantastic-module.stk` to `lib/`
* include `fantastic-module.stk` and `fantastic-module.ostk`  in the variables
  `scheme_SRCS` and `scheme_OBJS`, in `lib/Makefile.am`
* if there should be a SRFI-0 synonym for the module or SRFI, add
  it to `lib/srfi-0.stk`
* add the tests to `test/`. If you are testing a new SRFI, add it to the
  `test-srfi.stk` file; otherwise, create a new file and include it
  in the list of loaded files in `do-test.stk`
* If it is a SRFI, add it to `SUPPORTED-SRFIS`
* If you are adding a new SRFI,
  - add its number and  name to the `*srfis*` variable in `doc/skb/srfi.stk`
  - add its documentation to `doc/skb/srfi.skb` (see below)

## Documentation


### Documenting SRFIs in `srfi.skb`

If the SRFI is loaded (there's a `srfi-nnn` file, and it must be `require`d,
use the `gen-loaded-srfi` procedure:

```
;; SRFI 111 -- Boxes
(gen-loaded-srfi 111)
```

If the SRFI is embedded (will be available without need for `require`),
use the `gen-embedded-srfi` procedure:

```
;; SRFI 112 -- Environment Inquiry
(gen-embedded-srfi 112)
```

### Documenting primitives written in C

Before `DEFINE_PRIMITIVE`, add a comment similar to the others you see
in the C files. An example:

```
/*
<doc EXT bignum?
 * (bignum? x)
 *
 * This predicates returns |#t| if |x| is an integer number too large to be
 * represented with a native integer.
 * @lisp
 * (bignum? (expt 2 300))     => |#t|   (very likely)
 * (bignum? 12)               => |#f|
 * (bignum? "no")             => |#f|
 * @end lisp
doc>
*/
DEFINE_PRIMITIVE("bignum?", bignump, subr1, (SCM x))
{
  return MAKE_BOOLEAN(BIGNUMP(x));
}
```

Pay attention to the parts of this comment: it begins with the primitive name,
then there's an explanation, then examples in Scheme.
Wrap symbols/identifiers in `|.|`; use `@lisp` and `@end lisp@` to
show an example of usage.

# Writing primitives in C

Use the macro `DEFINE_PRIMITIVE`:

```
DEFINE_PRIMITIVE("fixnum?", fixnump, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(INTP(obj));
}
```

The arguments for this example are

* Scheme name
* C function name (its full name will have the string "`STk_`" prepended to it)
* the type of primitive (in this case, it is a subroutine with one
  parameter -- "`subr1`"
* the arguents, surrounded by parentheses. In this case there is only
  one argument, "`obj`", and its type is "`SCM`" (which is the type of
  all Scheme objects in STklos).

Then add it:

```
ADD_PRIMITIVE(fixnump);
```

The name passed to `ADD_PRIMITIVE` is the C function name.

## Calling Scheme primitives

Recall that a primitive is defined like this:

```
DEFINE_PRIMITIVE("fixnum?", fixnump, subr1, (SCM obj))
{ ... }

ADD_PRIMITIVE(fixnump);
```

TO use this primitive later in C code, add the `STk_` prefix to its
C function name:

```
if (STk_fixnump(obj) == STk_false) ...
```

## Returning values

`STk_n_values(n, v1, v2, ..., vn)` returns `n` values from a procedure.

For example, `read-line` (defined in `port.c`) has these two lines:

```
return STk_n_values(2, res, STk_eof)
```

for when it found the end of the file, and

```
return STk_n_values(2, res, delim);
```

for when it did not yet reach EOF, so it returns the line delimiter as second value.

## Errors

The C function that raises errors is

* `STk_error(fmt, arg1, arg2, ...)` -- the STklos error procedure. `fmt` is a format string,
and after it there are arguments.

But as you can see in the top of several C files, it is useful to define
wrappers:

```
static void error_bad_number(SCM n)
{
  STk_error("~S is a bad number", n);
}

static void error_at_least_1(void)
{
  STk_error("expects at least one argument");
}

static void error_cannot_operate(char *operation, SCM o1, SCM o2)
{
  STk_error("cannot perform %s on ~S and ~S", operation, o1, o2);
}
```

## Unboxed types

The trditional way to representa data in Lisp languages is by *tagged objects*.
A long enough machine word is used to represent all types, and some bits are reserved
to distinguish the type of the object. In STklos, the *two least significant bits*
are used for this.

* `00` - pointer on an object descriptor (a box)
* `01` - fixnum
* `10` - small object (characters and others)
* `11` - small constant (`#t`, `#f`, `'()`, `#eof`, `#void`, dot, close-parenthesis)

The idea is that checking the type of these should be very fast, because it is done at runtime,
so to check wether an object is `#eof`, one needs only check if `obj & 0x4 == 0x3` (but
usually, we have macros for that).

STklos uses C `long` words so, for example, in a machine where `long int` is 32 bits long
the bit sequence

```
0000 0000 0000 0000 0000 0000 0010 0101
```

is a *fixnum* (because its two least significant digits are `01`, and the value
of the fixnum is 9 (because after discarding the `01` that is on the right of the sequence,
the number left is `1001`).


### Booleans

* `STk_true` is the SCM object for `#t`
* `STk_false` is the SCM object for `#f`

* `BOOLEANP(o)` checks wether the object `o` is boolean (the macro actually
  does `(((o) == STk_true) || ((o) == STk_false))`
* `MAKE_BOOLEAN(_cond)` expands to a conditional statement: if `_cond` is
  true, then the value is `STk_true`, otherwise it is `STk_false`.

### Fixnums

Fixnums are not allocated but have their two least significant bits set to `01`
(in Lisp-parlance, it has `01` as its *tag*).

* `INTP(o)` - returns STklos_true if `o` is a Scheme integer or `STklos_false`
  otherwise
* `MAKE_INT(n)` - takes a `long` C number and turns it into an `SCM` integer object.
  Actually, this will shift the number to the left by two positions and insert the tag
  If we could represent numbers as binary in C, it would be like this:

```
MAKE_INT( 000011000 ) --> 001100001
```

* `INT_VAL(o)` - returns the value of the fixnum `o`, as a C `long` value (the opposite
  of the previous operation)

## Boxed types

Boxed types are anything except for fixnums, small objects and small constants.
They are tagged with `00`.

* `BOXED_OBJP(o)` -- true if `o` is a boxed object
* `BOXED_TYPE_EQ(o,t)` -- checks wether `o` is a boxed object of type `t`
* `BOXED_TYPE(o)` -- returns the type of boxed object `o`
* `BOXED_INFO` -- returns the information of boxed object `o`

The tyoe definition for all possible types, in `stklos.h`, is self-explanatory:

```
typedef enum {
  tc_not_boxed=-1,
  tc_cons, tc_integer, tc_real, tc_bignum,  tc_rational,                /* 0 */
  tc_complex, tc_symbol, tc_keyword, tc_string, tc_module,              /* 5 */
  tc_instance, tc_closure, tc_subr0, tc_subr1, tc_subr2,                /* 10 */
  tc_subr3, tc_subr4, tc_subr5, tc_subr01, tc_subr12,                   /* 15 */
  tc_subr23, tc_vsubr, tc_apply, tc_vector, tc_uvector,                 /* 20 */
  tc_hash_table, tc_port, tc_frame, tc_next_method, tc_promise,         /* 25 */
  tc_regexp, tc_process, tc_continuation, tc_values, tc_parameter,      /* 30 */
  tc_socket, tc_struct_type, tc_struct, tc_thread, tc_mutex,            /* 35 */
  tc_condv, tc_box, tc_ext_func, tc_pointer, tc_callback,               /* 40 */
  tc_last_standard /* must be last as indicated by its name */
} type_cell;
```

### Lists

Here are some primitives for lists, for example:

* `CAR(p)` -- equivalent to Scheme `car`: returns the car of `p` (an SCM object)
* `CDR(p)` -- equivalent to Scheme `cdr`: returns the car of `p` (an SCM object, which certainly is a list)
* `CONSP(p)` - equivalent to Scheme `cons?`
* `NULLP(p)` - equivalent to Scheme `null?`
* `STk_cons` - equivalent to Scheme `cons`

### Strings

Another example are strings. They are defined as the following structure:

```
struct string_obj {
  stk_header header;
  int space;            /* allocated size  */
  int size;             /* # of bytes used */
  int length;           /* "external" length of the string */
  char *chars;
};
```

Then, some primitives:

```
#define STRING_SPACE(p)  (((struct string_obj *) (p))->space)
#define STRING_SIZE(p)   (((struct string_obj *) (p))->size)
#define STRING_LENGTH(p) (((struct string_obj *) (p))->length)
#define STRING_CHARS(p)  (((struct string_obj *) (p))->chars)
#define STRINGP(p)       (BOXED_TYPE_EQ((p), tc_string))
```

The following primitives are defined in a `str.c`, but `stklos.h` is
used by several files use them, so they're included with `EXTERN_PRIMITIVE`:

```
EXTERN_PRIMITIVE("string=?", streq, subr2, (SCM s1, SCM s2));
EXTERN_PRIMITIVE("string-ref", string_ref, subr2, (SCM str, SCM index));
EXTERN_PRIMITIVE("string-set!", string_set, subr3, (SCM str, SCM index, SCM value));
EXTERN_PRIMITIVE("string-downcase!", string_ddowncase, vsubr, (int argc, SCM *argv));
```

## Dynamically loadable modules

See some examples in `etc/`
