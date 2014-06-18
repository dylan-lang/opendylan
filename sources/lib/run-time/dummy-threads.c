#include "run-time.h"
#include <stdio.h>
#include <stdlib.h>

extern dylan_object KPfalseVKi;
extern dylan_object KPtrueVKi;

#define ignore(x) x

/* THREADS SUPPORT */

dylan_value one_true_thread = DFALSE;

#define THREAD_SUCCESS I(0)

void initialize_threads_primitives() {
  make_teb();
}

TEB dylan_teb;

__attribute__((pure))
TEB* get_teb()
{
  return &dylan_teb;
}

TEB* make_teb()
{
  TEB* teb = &dylan_teb;

  teb->uwp_frame = Ptop_unwind_protect_frame;

  return teb;
}

void threads_get_stuffed () {
  fprintf(stderr, "This implementation does not support real threads\n");
  primitive_break();
}

dylan_value primitive_detach_thread(dylan_value t) {
  ignore(t);
  return(THREAD_SUCCESS);
}

dylan_value primitive_release_simple_lock(dylan_value l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_release_semaphore(dylan_value s) {
  ignore(s);
  return(THREAD_SUCCESS);
}
dylan_value primitive_owned_recursive_lock(dylan_value l) {
  ignore(l);
  return(DTRUE);
}
dylan_value primitive_destroy_simple_lock(dylan_value l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_semaphore_timed(dylan_value s, dylan_value ms) {
  ignore(s); ignore(ms);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_semaphore(dylan_value s) {
  ignore(s);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_simple_lock_timed(dylan_value l, dylan_value ms) {
  ignore(l); ignore(ms);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_simple_lock(dylan_value l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_make_recursive_lock(dylan_value l, dylan_value n) {
  ignore(l); ignore(n);
  return(THREAD_SUCCESS);
}
dylan_value primitive_release_recursive_lock(dylan_value l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_make_semaphore(dylan_value l, dylan_value n, dylan_value i, dylan_value m) {
  ignore(l); ignore(n); ignore(i); ignore(m);
  return(THREAD_SUCCESS);
}
dylan_value primitive_destroy_recursive_lock(dylan_value l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_owned_simple_lock(dylan_value l) {
  ignore(l);
  return(DTRUE);
}
dylan_value primitive_destroy_semaphore(dylan_value l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_recursive_lock_timed(dylan_value l, dylan_value ms) {
  ignore(l); ignore(ms);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_recursive_lock(dylan_value l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_thread_join_multiple(dylan_value v) {
  ignore(v);
  threads_get_stuffed();
  return(THREAD_SUCCESS);
}
dylan_value primitive_thread_join_single(dylan_value t) {
  ignore(t);
  threads_get_stuffed();
  return(THREAD_SUCCESS);
}
dylan_value primitive_initialize_current_thread(dylan_value t, DBOOL s) {
  ignore(s);
  one_true_thread = t;
  return(THREAD_SUCCESS);
}
dylan_value primitive_initialize_special_thread(dylan_value t) {
  one_true_thread = t;
  return(THREAD_SUCCESS);
}
dylan_value primitive_current_thread() {
  return(THREAD_SUCCESS);
}
dylan_value primitive_make_thread(dylan_value t, dylan_value n, dylan_value p, dylan_value f, DBOOL s) {
  ignore(t); ignore(n); ignore(p); ignore(f); ignore(s);
  /* threads_get_stuffed(); */
  return(THREAD_SUCCESS);  /* Keeps some compilers happy -- Won't actually get here */
}
dylan_value primitive_destroy_thread(dylan_value t) {
  ignore(t);
  return(THREAD_SUCCESS);
}
dylan_value primitive_destroy_notification(dylan_value n) {
  ignore(n);
  return(THREAD_SUCCESS);
}
dylan_value primitive_release_all_notification(dylan_value n, dylan_value l) {
  ignore(n); ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_make_notification(dylan_value l, dylan_value n) {
  ignore(l); ignore(n);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_notification_timed(dylan_value n, dylan_value l, dylan_value ms) {
  ignore(n); ignore(l); ignore(ms);
  return(THREAD_SUCCESS);
}
dylan_value primitive_wait_for_notification(dylan_value n, dylan_value l) {
  ignore(n); ignore(l);
  return(THREAD_SUCCESS);
}
dylan_value primitive_release_notification(dylan_value n, dylan_value l) {
  ignore(n); ignore(l);
  return(THREAD_SUCCESS);
}
void primitive_thread_yield() {
}
void primitive_sleep(dylan_value ms) {
  ignore(ms);
}
dylan_value primitive_make_simple_lock(dylan_value l, dylan_value n) {
  ignore(l); ignore(n);
  return(THREAD_SUCCESS);
}
dylan_value primitive_allocate_thread_variable(dylan_value i) {
  ignore(i);
  return(DFALSE);
}
dylan_value primitive_read_thread_variable(dylan_value h) {
  ignore(h);
  return(DFALSE);
}
dylan_value primitive_write_thread_variable(dylan_value h, dylan_value nv) {
  ignore(h); ignore(nv);
  return(nv);
}
