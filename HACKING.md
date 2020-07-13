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
* C function name (its full name will have the string "STk_" prepended to it)
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
* `10` - small object
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



### Lists

* `CAR(p)` -- equivalent to Scheme `car`: returns the car of `p` (an SCM object)
* `CDR(p)` -- equivalent to Scheme `cdr`: returns the car of `p` (an SCM object, which certainly is a list)
* `CONSP(p)` - equivalent to Scheme `cons?`
* `NULLP(p)` - equivalent to Scheme `null?`
* `STk_cons` - equivalent to Scheme `cons`

## Dynamically loadable modules

See some examples in `etc/`
