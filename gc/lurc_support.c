#if defined(GC_LURC_THREADS) && !defined(GC_PTHREADS)

#include "private/gc_priv.h"
#include <lurc.h>

#ifdef LURC_ENABLE_PTHREAD
#error "LURC is compiled with pthread support, but the GC is not. Please use ./configure --enable-threads=lurc-pthreads if you intend to use LURC with PThreads. Or compile LURC without PThread support."
#endif

static int GC_lurc_initialized = 0;

void
GC_push_all_stacks(void){
  lurc_thread_t lt = NULL;
  void *llo, *lhi;
  /* give lurc the main thread's lo and hi */
  lurc_gc_setup_stack(GC_approx_sp(), GC_stackbottom);
  /* now iterate through all lurcs' threads */
  while ((lt = lurc_get_next_thread(lt)) != NULL) {
    lurc_gc_get_root(lt, &llo, &lhi);
    if(llo != NULL)
      GC_push_all_stack(llo, lhi);
  }
  /* does it have another part ? */
  lurc_gc_get_additional_root(&llo, &lhi);
  if (llo != NULL)
    GC_push_all_stack(llo, lhi);
}

void GC_push_thread_structures(void){}

void GC_stop_world(void){}
void GC_start_world(void){}

void GC_thr_init(void){
  if(!GC_lurc_initialized){
    GC_lurc_initialized = 1;
    lurc_gc_init(GC_enable, GC_disable);
  }
}

#endif /* GC_LURC_THREADS && ! GC_PTHREADS */
