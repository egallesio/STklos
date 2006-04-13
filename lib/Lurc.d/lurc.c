#include <lurc.h>
#include <stklos.h>

//
//  Signal type definition

static int tc_signal;

struct signal_obj {
  stk_header header;
  lurc_signal_t sig;
};

#define SIGNALP(p)		(BOXED_TYPE_EQ((p), tc_signal))
#define SIGNAL_SIG(p)		(((struct signal_obj *) (p))->sig)

static void print_signal(SCM signal, SCM port, int mode)
{
  char *name = (char*)lurc_signal_name(&SIGNAL_SIG(signal));
  
  STk_puts("#[signal ", port);
  if (*name) 
    STk_puts(name, port);
  else
    STk_fprintf(port, "%lx", (unsigned long) signal);
  STk_putc(']', port);
}

/* The stucture which describes the signal type */
static struct extended_type_descr xtype_signal = {
  "lurc:signal",			/* name */
  print_signal			/* print function */
};

//
// Errors

static void error_bad_signal(SCM obj)
{
  STk_error("bad signal ~S", obj);
}

static void lurc_error(int ret)
{
  STk_error("lurc error ~a", lurc_strerror(ret));
}

//
// Primitives

DEFINE_PRIMITIVE("lurc:signal?", lurc_signalp, subr1, (SCM obj))
{
  return MAKE_BOOLEAN(SIGNALP(obj));
}

DEFINE_PRIMITIVE("lurc:signal-name", lurc_signal_name, subr1, (SCM sig))
{
  char *name;
  if (! SIGNALP(sig)) error_bad_signal(sig);
  name = (char*)lurc_signal_name(&SIGNAL_SIG(sig));
  if(name == NULL)
    name = "";
  return STk_Cstring2string(name);
}

static void signal_finalizer(SCM sig)
{
  lurc_signal_destroy(&SIGNAL_SIG(sig));
}

static SCM do_make_signal(char *name)
{
  SCM z;
  lurc_signal_attr_t attr;
  int ret;

  if((ret = lurc_signal_attr_init(&attr)) != 0)
    lurc_error(ret);

  if((ret = lurc_signal_attr_setname(&attr, name)) != 0){
    lurc_signal_attr_destroy(&attr);
    lurc_error(ret);
  }
  
  NEWCELL(z, signal);
  
  if((ret = lurc_signal_init(&SIGNAL_SIG(z), &attr)) != 0){
    lurc_signal_attr_destroy(&attr);
    lurc_error(ret);
  }

  if((ret = lurc_signal_attr_destroy(&attr)) != 0){
    lurc_signal_destroy(&SIGNAL_SIG(z));
    lurc_error(ret);
  }

  STk_register_finalizer(z, signal_finalizer);

  return z;
}

DEFINE_PRIMITIVE("lurc:signal", lurc_make_signal, subr01, (SCM name))
{
  SCM z;
  char *s;

  if (name) {
    if (!STRINGP(name))
      STk_error("bad signal name ~S", name);
    s = STRING_CHARS(name);
  }
  else s = NULL;

  z = do_make_signal(s);
  return z;
}

static void
enter_context(void *arg){
  SCM thunk = (SCM)arg;

  STk_C_apply(thunk, 0);
}

DEFINE_PRIMITIVE("%lurc:when", lurc_when, subr2, (SCM sig, SCM thunk))
{
  int ret;
  if (STk_procedurep(thunk) == STk_false) 
    STk_error("bad thunk ~S", thunk);
  if (! SIGNALP(sig)) error_bad_signal(sig);

  if((ret = lurc_when(&SIGNAL_SIG(sig), enter_context, thunk)) != 0)
    lurc_error(ret);

  return STk_void;
}

DEFINE_PRIMITIVE("%lurc:watch", lurc_watch, subr2, (SCM sig, SCM thunk))
{
  int ret;
  SCM *vm;
  if (STk_procedurep(thunk) == STk_false) 
    STk_error("bad thunk ~S", thunk);
  if (! SIGNALP(sig)) error_bad_signal(sig);

  vm = STk_save_vm();
  if((ret = lurc_watch(&SIGNAL_SIG(sig), enter_context, thunk)) != 0){
    STk_restore_vm(vm);
    lurc_error(ret);
  }
  STk_restore_vm(vm);

  return STk_void;
}


DEFINE_PRIMITIVE("lurc:emit", lurc_emit, subr1, (SCM sig))
{
  int ret;
  if (! SIGNALP(sig)) error_bad_signal(sig);

  if((ret = lurc_signal_emit(&SIGNAL_SIG(sig))) != 0)
    lurc_error(ret);

  return STk_void;
}

DEFINE_PRIMITIVE("lurc:await", lurc_await, subr1, (SCM sig))
{
  int ret;
  if (! SIGNALP(sig)) error_bad_signal(sig);

  if((ret = lurc_signal_await(&SIGNAL_SIG(sig))) != 0)
    lurc_error(ret);

  return STk_void;
}

DEFINE_PRIMITIVE("lurc:pause", lurc_pause, subr0, ())
{
  int ret;
  if((ret = lurc_pause()) != 0)
    lurc_error(ret);

  return STk_void;
}

//
// Module init

void test(void){
  lurc_signal_t sig = lurc_signal("test");
  int fail = 0;
  printf("entering watch\n");
  LURC_WATCH(&sig){
    printf("in watch\n");
    lurc_signal_emit(&sig);
    printf("pausing\n");
    lurc_pause();
    fail = 1;
  }
  if(fail)
    printf("TEST FAILED\n");
  else
    printf("test passed\n");
}

MODULE_ENTRY_START("lurc"){
  
  tc_signal = STk_new_user_type();

  /* Signal Type declaration */
  DEFINE_XTYPE(signal, &xtype_signal);

  ADD_PRIMITIVE(lurc_signalp);
  ADD_PRIMITIVE(lurc_signal_name);
  ADD_PRIMITIVE(lurc_make_signal);
  ADD_PRIMITIVE(lurc_when);
  ADD_PRIMITIVE(lurc_watch);
  ADD_PRIMITIVE(lurc_await);
  ADD_PRIMITIVE(lurc_emit);
  ADD_PRIMITIVE(lurc_pause);

  //  test();

}MODULE_ENTRY_END;
