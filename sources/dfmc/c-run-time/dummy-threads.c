#include "run-time.h"
#include <stdio.h>
#include <stdlib.h>

extern OBJECT KPfalseVKi;
extern OBJECT KPtrueVKi;

#define ignore(x) x

/* THREADS SUPPORT */

D one_true_thread = DFALSE;

#define THREAD_SUCCESS I(0)

void initialize_threads_primitives() {
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

D primitive_detach_thread(D t) {
  ignore(t);
  return(THREAD_SUCCESS);
}

D primitive_release_simple_lock(D l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_release_semaphore(D s) {
  ignore(s);
  return(THREAD_SUCCESS);
}
D primitive_owned_recursive_lock(D l) {
  ignore(l);
  return(DTRUE);
}
D primitive_destroy_simple_lock(D l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_semaphore_timed(D s, D ms) {
  ignore(s); ignore(ms);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_semaphore(D s) {
  ignore(s);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_simple_lock_timed(D l, D ms) {
  ignore(l); ignore(ms);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_simple_lock(D l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_make_recursive_lock(D l, D n) {
  ignore(l); ignore(n);
  return(THREAD_SUCCESS);
}
D primitive_release_recursive_lock(D l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_make_semaphore(D l, D n, D i, D m) {
  ignore(l); ignore(n); ignore(i); ignore(m);
  return(THREAD_SUCCESS);
}
D primitive_destroy_recursive_lock(D l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_owned_simple_lock(D l) {
  ignore(l);
  return(DTRUE);
}
D primitive_destroy_semaphore(D l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_recursive_lock_timed(D l, D ms) {
  ignore(l); ignore(ms);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_recursive_lock(D l) {
  ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_thread_join_multiple(D v) {
  ignore(v);
  threads_get_stuffed();
  return(THREAD_SUCCESS);
}
D primitive_thread_join_single(D t) {
  ignore(t);
  threads_get_stuffed();
  return(THREAD_SUCCESS);
}
D primitive_initialize_current_thread(D t, DBOOL s) {
  ignore(s);
  one_true_thread = t;
  return(THREAD_SUCCESS);
}
D primitive_initialize_special_thread(D t) {
  one_true_thread = t;
  return(THREAD_SUCCESS);
}
D primitive_current_thread() {
  return(THREAD_SUCCESS);
}
D primitive_make_thread(D t, D n, D p, D f, DBOOL s) {
  ignore(t); ignore(n); ignore(p); ignore(f); ignore(s);
  /* threads_get_stuffed(); */
  return(THREAD_SUCCESS);	/* Keeps some compilers happy -- Won't actually get here */
}
D primitive_destroy_thread(D t) {
  ignore(t);
  return(THREAD_SUCCESS);
}
D primitive_destroy_notification(D n) {
  ignore(n);
  return(THREAD_SUCCESS);
}
D primitive_release_all_notification(D n, D l) {
  ignore(n); ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_make_notification(D l, D n) {
  ignore(l); ignore(n);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_notification_timed(D n, D l, D ms) {
  ignore(n); ignore(l); ignore(ms);
  return(THREAD_SUCCESS);
}
D primitive_wait_for_notification(D n, D l) {
  ignore(n); ignore(l);
  return(THREAD_SUCCESS);
}
D primitive_release_notification(D n, D l) {
  ignore(n); ignore(l);
  return(THREAD_SUCCESS);
}
void primitive_thread_yield() {
}
void primitive_sleep(D ms) {
  ignore(ms);
}
D primitive_make_simple_lock(D l, D n) {
  ignore(l); ignore(n);
  return(THREAD_SUCCESS);
}
D primitive_allocate_thread_variable(D i) {
  ignore(i);
  return(DFALSE);
}
D primitive_read_thread_variable(D h) {
  ignore(h);
  return(DFALSE);
}
D primitive_write_thread_variable(D h, D nv) {
  ignore(h); ignore(nv);
  return(nv);
}
