#include <synch.h>
#include <sys/synch.h> /* where is USYNCH_THREAD defined? */
#include <stdlib.h>
#include <stddef.h>
#include <thread.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <semaphore.h>
#include <sys/errno.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>

#include "c-run-time.h"

#define CLOCK CLOCK_REALTIME
#define THREAD_MARKED 1
#define THREAD_JOINED 2
#define VEC_SIZE 1024
#define TRUE 1

typedef struct
{
  void * lock;
  int thread;
  int count;
} reclock;

 /* definition apparently not found in .h files! */
/* I don't believe you */
/* typedef int timer_t; */ 


/* Temporary definition */
/* Z dylanXinternalX_P_false[1]; */

/*****************************************************************************/
/* LOCAL FUNCTION DECLARATIONS                                               */
/*****************************************************************************/

void * trampoline(void *  dylanfunc);

int checkthread(SOV * thread_vector, DTHREAD * thread, int length);

int try_reclock(reclock * handle);
void sleep_ten(void);

void suspend_threads(thread_t self, LINKP vlist);
void unsus_threads(thread_t self, LINKP vlist);
void update_threadvecs(int newindex, Z value, LINKP list);

LINKP update_veclist(thread_t newthread, void * thread_v);
int trim_veclist(thread_t newthread);
int step_list(thread_t thread, LINKP list, LINKP last);

LINKP listmaker(void);
void * keymaker(void);

itimerspec_t abs_timeout(int milsecs);
int timeout_check(itimerspec_t ivalue);

void * primlock_timed(int milsecs);
void new_thread(void);
int timed_rec_wait(void);

/* END FUNCTION DECLARATIONS  */


/* TEMP VARIABLES */

  char name[] = "firstlock", name2[] = "ftang";
  int result, resultb;
  void *lock,  *cond;
  sema_t * sema;
  reclock * rlock;

/* PERMANENT VARIABLES */

  int * default_vec[VEC_SIZE] = {0};
  int vec_index = 0;
  thread_t tid;
  mutex_t joinlock, atomiclock, thread_vector_lock, keylock, listlock,
       thr_initlock;
  thread_key_t threadkey;
  LINKP vectorlist;

  size_t reclocksize = sizeof(reclock);
  size_t mutsize = sizeof(mutex_t);
  size_t semasize = sizeof(sema_t);
  size_t condsize = sizeof(cond_t);
  size_t dvecsize = sizeof(default_vec);
  size_t linksize = sizeof(LINKP);

#define JOINLOCK &joinlock
#define ATOMICLOCK &atomiclock
#define THREADVLOCK &thread_vector_lock
#define KEYLOCK &keylock
#define VECLISTLOCK &listlock
#define THR_INITLOCK &thr_initlock

/* NB:  ALL OF THESE LOCKS MUST BE INITIALISED
        ALSO: listmaker and keymaker must be called. */


#ifdef pants
void *
main(void)
{
  thread_t * recwaiter;
  int recwait_er;
  
   /* lock = dylanXinternalXprimitive_make_simple_lock(name); */
   
   /*  dylanXinternalXprimitive_destroy_simple_lock(lock); */

  /* sema = dylanXinternalXprimitive_make_semaphore(name, 2, 6);*/
  /* dylanXinternalXprimitive_wait_for_semaphore(sema);*/
  /* dylanXinternalXprimitive_wait_for_semaphore_timed(sema, 100);*/

  
  
   /* dylanXinternalXprimitive_destroy_semaphore(sema); */

   /*  cond = dylanXinternalXprimitive_make_notification(name); */
   /* dylanXinternalXprimitive_destroy_notification(cond); */

  /* 
  DTHREAD * nthread;
  D_NAME pox;
  ZINT prio = 4;
  ZFN funny;

   dylanXinternalXprimitive_make_thread(nthread, pox, prio, NULL);

   */

   /*  dylanXinternalXprimitive_current_thread(); */

  /*  lock =  dylanXinternalXprimitive_make_simple_lock(name2); */

 /*  dylanXinternalXprimitive_wait_for_simple_lock(lock); */
  /*  mutex_lock(lock); */
  /*  dylanXinternalXprimitive_wait_for_notification(cond, lock); */
  /*  new_thread(); */
   
  /* dylanXinternalXprimitive_wait_for_simple_lock_timed(lock, 100); */
  /* dylanXinternalXprimitive_release_simple_lock(lock); */
   
  /*  result = mutex_unlock(lock); */
   /* return(resultb); */
  /* result =  dylanXinternalXprimitive_wait_for_notification_timed(cond, lock, 8440);*/
  /*printf("waitcondvar result is %d\n", result); */

 /*  rlock = dylanXinternalXprimitive_make_recursive_lock(name); */
 /*  dylanXinternalXprimitive_wait_for_recursive_lock_timed(rlock, 20);*/


/*printf("creating new thread\n");
  recwaiter = (thread_t *)malloc(sizeof(thread_t));
  recwait_er = thr_create(NULL, 0, (void *)timed_rec_wait, NULL, THR_NEW_LWP,
  recwaiter); */
  
  /*  dylanXinternalXprimitive_wait_for_recursive_lock(rlock);*/
 /*  dylanXinternalXprimitive_release_recursive_lock(rlock);*/
 /*  dylanXinternalXprimitive_release_recursive_lock(rlock);*/
 /*  dylanXinternalXprimitive_release_recursive_lock(rlock);*/
  /* dylanXinternalXprimitive_destroy_recursive_lock(rlock);*/

    /*  dylanXinternalXprimitive_sleep(8500); */
    /* dylanXinternalXprimitive_sleep(5000); */
     /*  dylanXinternalXprimitive_sleep(500);*/
   /*  abs_timeout(10);  */

   {
    int i = 0;

    fflush(stdin);
    printf("Hit return to quit ...");
    while ( i != 10 )
      i = getchar();	
    printf("\nQuat\n");
  }	
 }

#endif 

/*
int
timed_rec_wait(void)
{
  dylanXinternalXprimitive_wait_for_recursive_lock_timed(rlock, 100);
}
 NB: to run, this prim. function now needs to be passed a CONTAINER and ZINT
*/

/* 1 */
void 
dylanXinternalXprimitive_make_thread(DTHREAD * newthread, D_NAME name, ZINT priority, ZFN
		      func)
{
  int error, prio_error, prio = (int)(priority >> 2);
  thread_t * thread;

  thread = malloc(sizeof(thread));
  error = thr_create(NULL, 0, trampoline, func, 0, thread);
   /* trampoline is starting function for the new thread.  func should be 
   passed in by the call from Dylan.  Solaris should call trampoline with
    the single argument 'func' */

  prio_error = thr_setprio(*thread, prio);
  /* In Solaris, higher int = higher priority */
  newthread->handle1 = thread;
  newthread->handle2 = 0;

 /* testing */
  printf("thrresult is %d  and  %d \n", error, thread);
  printf("setting priority error is %d\n", prio_error);
}

void *
trampoline(void * func)
{
 /*  initialise_new_thread(thread); */
  /* HOW DO WE GET THE DTHREAD* for this?  */
  
  CALL0(func);
 /*  trim_veclist(thread); */
  /* HOW DO WE GET THE DTHREAD* for this?  */

}



/* 2 */

void
dylanXinternalXprimitive_destroy_thread(DTHREAD * thread)
{ 
  free(thread->handle1);
}

/* 3 */         
ZINT
dylanXinternalXprimitive_thread_join_single(DTHREAD * thread)  
   /* thread states: 0 = normal, 1 = marked, 2 = joined
      error ints for return:  0 = joined ok, 1 = error  */
{
  int error, i, state;
  thread_t departed;
  DTHREAD * result;

  /* NB: multiple threads cannot wait for the same thread to terminate */
  
    mutex_lock(JOINLOCK);
   (int)state = (thread->handle2);

    if (state == THREAD_MARKED)
    {
      (int)(thread->handle2) = THREAD_JOINED;
      mutex_unlock(JOINLOCK);
      return((ZINT)I(0));
    }
    else if (state == THREAD_JOINED)
    {
      mutex_unlock(JOINLOCK);
      return((ZINT)I(1));
    }
    mutex_unlock(JOINLOCK); /* 'if's have not fired  */
    
    error = thr_join((thread_t)(thread->handle1), &departed, NULL);
            /* wait for join */
  /* DOES thr_join want the thread integer, rather than a pointer??? */
  
    if (error != 0)
    {  return((ZINT)I(1)); }
    
    else
    {
      mutex_lock(JOINLOCK);
    (int)state = (thread->handle2); 
    }
    if (state == THREAD_MARKED)
    {
      (int)(thread->handle2) = THREAD_JOINED;
      mutex_unlock(JOINLOCK);
      return((ZINT)I(0));
    }
    else if (state == THREAD_JOINED)
    {
      mutex_unlock(JOINLOCK);
      return((ZINT)I(1));
    }
    mutex_unlock(JOINLOCK); /* in case 'if's have not fired  */
  
}


/* 4 */

Z
dylanXinternalXprimitive_thread_join_multiple(SOV * thread_vector)
{
   /* thread states: 0 = normal, 1 = marked, 2 = joined
      error ints for return: 0 = joined ok, 1 = error  */
  
  thread_t * departed;
  int i, state, length = ((int)(thread_vector->size)) >> 2;

  while (TRUE)
  {
    mutex_lock(JOINLOCK);
    
    for (i = 2; i < length + 2; i++)
    {
      DTHREAD * thread = (DTHREAD *) ((DTHREAD **)thread_vector)[i];
        /* get a thread's DTHREAD struc from the SOV */
      
      (int)state = (thread->handle2);
        /* get the integer representing the joining state */

      if (state == THREAD_MARKED)
      {
	(int)(thread->handle2) = THREAD_JOINED;
	mutex_unlock(JOINLOCK);
	return((DTHREAD *)thread);
      }
      else if (state == THREAD_JOINED)
      {
	mutex_unlock(JOINLOCK);
	return((Z)I(1));
      }
    }

    mutex_unlock(JOINLOCK); /* 'if's have not fired, thus still locked */
    dylanXinternalXprimitive_sleep(10 << 2);
        
  }
}


int
checkthread(SOV * thread_vector, DTHREAD * thread, int length)
{
  DTHREAD * candidate;
  int i;
  
  for (i = 2; i < length + 2; i++)
  {
   candidate = (DTHREAD *) ((DTHREAD **)thread_vector)[i];

   if ((candidate->handle1) == (thread->handle1))
   {
     return(0);
   }
 }
  return(1);
}


/* 5 */

void
dylanXinternalXprimitive_thread_yield(void)
{
  thr_yield();
} 

/* 6. */
void * 
dylanXinternalXprimitive_current_thread(void)
{
  thread_t tid;
  tid = thr_self();

  /* testing */
  printf("thr-id result is %d \n", tid);
  return((void *)tid); /* integer cast to 'void *' */
}
 

/* 7 */
ZINT 
dylanXinternalXprimitive_wait_for_simple_lock(CONTAINER * slock)
{
  int error;
  void * lhandle = (slock->handle);
  
  error = mutex_lock(lhandle);
  /*testing */
  printf("waitsiml error is %d\n", error);

  if (error == 0)
  {  return((ZINT)I(0));}     /*  0 is ok, other is problem */

  else { return((ZINT)I(1)); }
}

/* 8 */
ZINT
dylanXinternalXprimitive_wait_for_recursive_lock(CONTAINER * rlock)
{
  int error;
  reclock * handle = (rlock->handle);

  if (handle->thread == thr_self())
    handle->count += 1;
  else
  {
    error = mutex_lock(handle->lock);
    handle->thread = thr_self();
    handle->count = 1;
  }

    /*testing */ 
  /*
   printf("wfor-rec-lock error is %d\n", error);
   printf("reclock lock slot is %d\n", (*handle).lock);
   printf("reclock thread slot is %d\n", (*handle).thread);
   printf("reclock count slot is %d\n", (*handle).count);
   */

  if (error == 0) /* 0 = ok */
    { return((ZINT)I(0)); }
  else
  { return((ZINT)I(1)); }
}

     
/* 9 */
ZINT
dylanXinternalXprimitive_wait_for_semaphore(CONTAINER * sem)
{
  int error;
  void * handle = (sem->handle);

  /*  printf("going into wait_for_sema\n"); */
  
  error = sema_wait(handle);  /*  0 is ok, other is problem */
  /* testing */
  /*  printf("semwait result is %d\n", error); */

  if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
  else
    { return((ZINT)I(1)); }
}

/* 10 */

ZINT
dylanXinternalXprimitive_wait_for_notification(CONTAINER * cnotif, CONTAINER * cnotiflock)
{ 
  int error;
  cond_t * notif = (cnotif->handle);
  mutex_t * notiflock = (cnotiflock->handle);
  
  /*  printf("entering wait-for-notif.  \n"); */
  error = cond_wait(notif, notiflock);
  /* testing */
  /*  printf("cond_wait error is %d\n", error); */

  if (error == 0)   /* all ok */
    return((ZINT)I(0));
  /*
  
  else if (???????????) NEED TO TEST THAT LOCK LOCKED
    return((ZINT)I(2));
    */
  
  else return((ZINT)I(3));
       /* some other problem or error */
}

/* 11 */

ZINT
dylanXinternalXprimitive_wait_for_simple_lock_timed(CONTAINER * lock, ZINT zmilsecs)
{
  int error, milsecs = (zmilsecs >> 2);
  itimerspec_t ivalue;
  void * lhandle = (lock->handle);

  if (milsecs == 0)
  {
    error = mutex_trylock(lhandle);
  }
  else 
  {	
  ivalue = abs_timeout(milsecs);

  /*
  printf("returned from abs_timeout, going into while loop \n");
  printf("returned ivalue times are %d secs and %d nsecs \n",
  ivalue.it_value.tv_sec, ivalue.it_value.tv_nsec);
  */
  
  while (timeout_check(ivalue) == 0)
  {
    /*  printf("entering timeout while loop\n"); */
    
    if (mutex_trylock(lhandle) == 0)
    {          
      error = 0;
        /* printf("lock_timed_wait has got lock\n"); */
      return((ZINT)I(0));
    }
    else
    {
      /*  printf("going into dylanXinternalXprimitive_sleep, 10 ms. \n"); */
      dylanXinternalXprimitive_sleep(10 << 2);
       /* printf("back from dylanXinternalXprimitive_sleep, continuing loop\n"); */
    }
  }
  printf("lock_timed_wait has timed out\n");
  return((ZINT)I(1)); /*wait has timed out */
}

  /* testing */
 /*  printf("wforsiml error is %d\n", error); */

  if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
  else
    { return((ZINT)I(1)); }
}

void
sleep_ten(void)
{
  dylanXinternalXprimitive_sleep(10 << 2);
}

int
timeout_check(itimerspec_t ivalue)
{
  timespec_t cvalue;
  int clock_er;

  printf("entering timout_check \n");
  
  clock_er = clock_gettime(CLOCK, &cvalue); /*see man page for struct */
  printf("clock_er is %d \n", clock_er);

   printf("timeout_check ivalue times are %d secs and %d nsecs \n",
	  ivalue.it_value.tv_sec, ivalue.it_value.tv_nsec);
   printf("timeout_check clock times are %d secs and %d nsecs \n", cvalue.tv_sec,
	 cvalue.tv_nsec);
  

  /* check timeout, if true, return 1 */
  if ((long)ivalue.it_value.tv_sec < (long)cvalue.tv_sec)
  {
    printf("timout_check returning 1 from first condition\n");
    return(1);
  }
  else if (((long)ivalue.it_value.tv_sec == (long)cvalue.tv_sec) &&
      ((long)ivalue.it_value.tv_nsec <= ((long)cvalue.tv_nsec)))
  {
    printf("timout_check returning 1 from second condition\n");
    return(1);
  }
  else
    {
       printf("timout_check returning 0\n");
      return(0);
    }
}


int
try_reclock(reclock * handle)
{
  int error;

  printf("going into try_reclock\n");
  
  if ((*handle).thread == thr_self())
  {
    (*handle).count += 1;
  }
  else
  {
    error = mutex_trylock((*handle).lock);
  }
  if( error == 0)
  {
    (*handle).thread = thr_self();
    (*handle).count = 1;
    return(error);
  }
  else
  {
    return(error);
  }
}


/* 12 */
ZINT
dylanXinternalXprimitive_wait_for_recursive_lock_timed(CONTAINER * lock, ZINT zmilsecs)
{
  int error, milsecs = (zmilsecs >> 2);
  itimerspec_t ivalue;
  void * handle = (lock->handle);

  if (milsecs == 0)
  {
    error = try_reclock(handle);
    if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
    else
    { return((ZINT)I(1)); }
  }
    else
  {
    ivalue = abs_timeout(milsecs);
    /*
    printf("returned from reclock abs_timeout, going into while loop \n");
    printf("returned ivalue times are %d secs and %d nsecs \n",
	   ivalue.it_value.tv_sec, ivalue.it_value.tv_nsec);
    */

    while (timeout_check(ivalue) == 0)
    {
       /* printf("entering reclock timeout while loop\n"); */
      if (try_reclock(handle) == 0)
      {          
      error = 0;
        /* printf("reclock_timed_wait has got lock\n"); */
      return((ZINT)I(0));
      }
      else
      {
        /* printf("going into dylanXinternalXprimitive_sleep, 10 ms. \n"); */
      dylanXinternalXprimitive_sleep(10 << 2);
        /* printf("back from dylanXinternalXprimitive_sleep, continuing loop\n"); */
      }
    }
      /* printf("lock_timed_wait has timed out\n"); */
    
    return((ZINT)I(1)); /*wait has timed out */
  }
}

/* 13 */
ZINT
dylanXinternalXprimitive_wait_for_semaphore_timed(CONTAINER * lock, ZINT zmilsecs)
{
  int error, milsecs = (zmilsecs >> 2);
  itimerspec_t ivalue;
  void * handle = (lock->handle);

  if (milsecs == 0)
  {
    error = sema_trywait(handle);
    if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
    else
    { return((ZINT)I(1)); }
  }
  else 
  {	
  ivalue = abs_timeout(milsecs);
  /*
  printf("returned from abs_timeout, going into while loop \n");
  printf("returned ivalue times are %d secs and %d nsecs \n",
	 ivalue.it_value.tv_sec, ivalue.it_value.tv_nsec);
  */

  while (timeout_check(ivalue) == 0)
   {
      /* printf("entering sema-timeout while loop\n");  */
    
    if (sema_trywait(handle) == 0)
    {          
      error = 0;
       /* printf("semaphore_timed_wait has got semaphore\n"); */
      return((ZINT)I(0));
    }
    else
    {
        /* printf("going into dylanXinternalXprimitive_sleep, 10 ms. \n"); */
      dylanXinternalXprimitive_sleep(10 << 2);
        /* printf("back from dylanXinternalXprimitive_sleep, continuing loop\n"); */
    } 
   }
   /* printf("semaphore_timed_wait has timed out\n"); */
  return((ZINT)I(1)); /*wait has timed out */
  }

  /* testing */
 /*  printf("wforsema error is %d\n", error); */
  
}

/* 14 */
ZINT
dylanXinternalXprimitive_wait_for_notification_timed(CONTAINER * cvar, CONTAINER * lock,
				      ZINT zmilsecs)
{
  int error, timeout, milsecs = (zmilsecs >> 2);
  timestruc_t tout;
  cond_t * condvar = (cvar->handle);
  mutex_t * handle = (lock->handle);

  if (milsecs == 0)
  {
      /* printf("in zero milsecs condition\n"); */
    return((ZINT)I(1));
  }
  else if (milsecs % 1000 == 0)
  {
      /* printf("in no-mod milsecs condition\n"); */
    timeout = (milsecs / 1000);
    tout.tv_sec = (time(NULL) + timeout);
    tout.tv_nsec = 0;
    error = cond_timedwait(condvar, handle, &tout);

    if (error == 0)           /* 0 = ok */
      { return((ZINT)I(0)); }
    else if (error == ETIME)
      { return((ZINT)I(1)); }
    /*
       NEED A  'NOT LOCKED' CONDITION = 2
    */
    else {return((ZINT)I(3)); }
    
  }
  else
  {
    timeout = (milsecs / 1000) + 1;
         /* round seconds up; need more precision?*/
      /* printf("in some-mod milsecs condition\n");  */
    tout.tv_sec = (time(NULL) + timeout);
    tout.tv_nsec = 0;
    error = cond_timedwait(condvar, handle, &tout);

    if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
    else if (error == ETIME)
    { return((ZINT)I(1)); }
    /*
       NEED A  'NOT LOCKED' CONDITION = 2
    */
    else {return((ZINT)I(3)); }
  }
  /* testing */
    /* printf("wforcond-timed error is %d\n", error); */
}


/* 15 */
ZINT
dylanXinternalXprimitive_release_simple_lock(CONTAINER * lock)
{
  int error;

  error = mutex_unlock(lock->handle);
    /* printf("release-simlock error is %d\n", error); */

  if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
  else
    { return((ZINT)I(1)); }
  /* NEED TO CHECK IF LOCK NOT LOCKED, = return(2)  */
}

/* 16 */
ZINT
dylanXinternalXprimitive_release_recursive_lock(CONTAINER * rlock)
{
   int error, caller = thr_self();
   mutex_t * lock;
   reclock * handle = (rlock->handle);

/*
   printf("caller is %d\n", caller);
   printf("thread id is %d\n", ((*handle).thread));
   printf("count is %d\n", (*handle).count);
*/  
   if (handle->thread == caller && handle->count == 1)
  {
    lock = handle->lock;
    handle->lock = 0;
    handle->thread = 0;
    handle->count = 0;
    error = mutex_unlock(lock);
  }
  else if (handle->thread == caller && handle->count > 1)
  {
    handle->count -= 1;
    error = 0;
  }
  else  if (handle->count == 0)
  { error = 2; }
  else
    {error = 2; }
    /*testing */
   /*
   printf("release-rec-lock error is %d\n", error);
   printf("reclock lock slot is %d\n", (*handle).lock);
   printf("reclock thread slot is %d\n", (*handle).thread);
   printf("reclock count slot is %d\n", (*handle).count);
   printf("thread id is %d\n", caller);
   */

   if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
   else
    { return((ZINT)I(2)); }
}

/* 17 */
ZINT
dylanXinternalXprimitive_release_semaphore(CONTAINER * lock)
{
 int error ;

 error = sema_post(lock->handle);
   /* printf("release-sema  error is %d\n", error); */

 if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
 else
    { return((ZINT)I(1)); }
 
   /* NEED 'MAXCOUNT EXCEEDED' CONDITION, = 3  */
}

/* 18 */
ZINT
dylanXinternalXprimitive_release_notification(CONTAINER * notif, CONTAINER * lock)
{
  int error;
  cond_t * chandle = (notif->handle);
  mutex_t * lhandle = (lock->handle);

  mutex_trylock(lhandle);
    /* NB: this is a temporary solution: the synchronisation should
       be done by the Dylan programmer.  The function should CHECK to
       see that the lock is owned, but Solaris gives no direct way
       to do this.  Have to find one.  */
  error = cond_signal(chandle);
  mutex_unlock(lhandle);
    /* printf("release-notif error is %d\n", error); */

  if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
  else
    { return((ZINT)I(1)); }
  
  /* NEED 'LOCK NOT LOCKED' CONDITION = 2 */
}

/* 19 */
ZINT
dylanXinternalXprimitive_release_all_notification(CONTAINER * notif, CONTAINER * lock)
{
  int error;
  cond_t * chandle = (notif->handle);
  mutex_t * lhandle = (lock->handle);

  mutex_lock(lhandle);
  error = cond_broadcast(chandle);
  mutex_unlock(lhandle);
    /* printf("release-notif-all error is %d\n", error); */
  if (error == 0)           /* 0 = ok */
    { return((ZINT)I(0)); }
  else if (error == EINVAL) 
    { return((ZINT)I(2));}
}   /* NB: THIS FUNC SHOULD CHECK THAT LOCK IS LOCKED, for error */


/* 20 */   
void
dylanXinternalXprimitive_make_recursive_lock(CONTAINER * lock, D_NAME name)
{   int goal;
    void * newlock = malloc(mutsize);
    reclock * recinfo = malloc(reclocksize);
   
    goal =  mutex_init(newlock, 0, 0);

    recinfo->lock = newlock;
    recinfo->thread = 0;
    recinfo->count = 0;
    lock->handle = recinfo; 
    
 /* testing */
  /*
    printf("reclock-result is %d  and  %d\n", goal,newlock);
    printf("recinfo-result is %d  and  %d\n", recinfo, reclocksize);
    printf("lock slot value is %d\n", (*recinfo).lock);
    printf("thread slot value is %d\n", (*recinfo).thread);
    printf("count slot value is %d\n", (*recinfo).count);
  */
  
}

/* 21 */
void
dylanXinternalXprimitive_destroy_recursive_lock(CONTAINER * rlock)
{
  int error;
  reclock * lhandle = (rlock->handle);
  
  error = mutex_destroy(lhandle->lock);
  free(lhandle->lock);
  free(lhandle);
  
  /* testing */
    /* printf("reclockdest result is %d\n", error); */
}


/* 22 */
void 
dylanXinternalXprimitive_make_simple_lock(CONTAINER * lock, D_NAME name)
{
    int error;
    mutex_t * newlock = malloc(mutsize);
    error = mutex_init(newlock, 0, 0);
    if (error == 0)
    {
      (lock->handle) = newlock;
    }
    /* testing */
    /*
   printf("mutresult is %d  and  %d  and %d\n", error ,newlock,mutsize);
   */
}

     
/* 23 */
void
dylanXinternalXprimitive_destroy_simple_lock(CONTAINER * lock)
  {
    int error;
    mutex_t * lhandle = (lock->handle);
    
    error = mutex_destroy(lhandle);
    free(lhandle);
  /* testing */
   /*  printf("mdest result is %d\n", error); */
  }
     
/* 24 */
ZINT
dylanXinternalXprimitive_owned_simple_lock(CONTAINER * lock)
{
  /* NB: This is only a temp solution: FIND A GOOD ONE */
  int error;
  mutex_t * lhandle = (lock->handle);

  error = (mutex_trylock(lhandle));
  if (error == 0)
  {
    mutex_unlock(lhandle);
    return((ZINT)I(0));
  }
  else
  {  return((ZINT)I(1)); }
}


/* 25 */
ZINT
dylanXinternalXprimitive_owned_recursive_lock(CONTAINER * rlock)
{
  int error;
  reclock * rhandle = (rlock->handle);

  if ((rhandle->thread) == 0)
  {
    return((ZINT)I(0));
  }
  else
  {
    return((ZINT)I(1));
  }
}     


     
/* 26 */

void
dylanXinternalXprimitive_make_semaphore(CONTAINER * sema, D_NAME name,
			 ZINT zinitial, ZINT zmax)
{
    int error, initial = (zinitial >> 2), max = (zmax >> 2);
    sema_t * newsema;
    
    newsema = (sema_t *)malloc(semasize);
    error = sema_init(newsema, initial, 0, 0);
    (sema->handle) = newsema;

    /* printf("semresult is %d and  %d  and %d\n", error, newsema, semasize);*/
}

/* 27 */

void
dylanXinternalXprimitive_destroy_semaphore(CONTAINER * sema)
{
    int error;
    void * semhandle = (sema->handle);
    
    error = sema_destroy(semhandle);
    free(semhandle);

  /* testing */
    /* printf("sdest result is %d\n", error); */
}


/* 28 */
void
dylanXinternalXprimitive_make_notification(CONTAINER * notif, D_NAME name)
  {
    int error;
    void * newcond;
    
    newcond = malloc(condsize);
    error= cond_init(newcond, 0, 0);
    notif->handle = newcond;
    
 /* testing */
  /*printf("condresult is %d  and  %d  and %d\n", error, newcond, condsize);*/
  }
  
/* 29 */
void
dylanXinternalXprimitive_destroy_notification(CONTAINER * notif)
  {
    int error;
    void * nhandle = (notif->handle);
    error = cond_destroy(nhandle);
    free(nhandle);

  /* testing */
    /*  printf("condest result is %d\n", error); */
  }


/* 30 */

void
dylanXinternalXprimitive_sleep(ZINT zmilsecs)
{
 
  int minit_er, coninit_er, errno = 0, milsecs = (zmilsecs >> 2);
  itimerspec_t ivalue;
  mutex_t sleep_lock;
  cond_t sleep_cond;
  timestruc_t abs_tout;
  
    /* printf("entering dylanXinternalXprimitive_sleep\n");  */

  ivalue = abs_timeout(milsecs);
  printf("ivalue times are %d secs and %d nsecs \n",  ivalue.it_value.tv_sec,
	 ivalue.it_value.tv_nsec);
  abs_tout.tv_sec = (time_t)ivalue.it_value.tv_sec;
  abs_tout.tv_nsec = (long)ivalue.it_value.tv_nsec;
  

  minit_er = mutex_init(&sleep_lock, USYNC_THREAD, NULL);
  coninit_er = cond_init(&sleep_cond, USYNC_THREAD, NULL);
  mutex_lock(&sleep_lock);
  errno = cond_timedwait(&sleep_cond, &sleep_lock, &abs_tout);
  mutex_unlock(&sleep_lock);

  /* testing */
  /*
  printf("minit_er is %d \n", minit_er);
  printf("coninit_er is %d \n", coninit_er);
  printf("cond_wait error is %d \n", errno);
  */
  /* for time-out should return error of 62, = 'ETIME' in errno.h */
}


itimerspec_t
abs_timeout(int milsecs)
{
  int clock_er;
  itimerspec_t ivalue;
  timespec_t cvalue;
  div_t sectimes, wait_times; 
  long pre_milsecs = 0;

  clock_er = clock_gettime(CLOCK, &cvalue); /*see man page for struct */
  wait_times.quot = 0;
  wait_times.rem = 0;
  sectimes.quot = 0;
  sectimes.rem = 0;

  printf("entering abs_timeout, milsecs is %d\n", milsecs);

  if (milsecs >= 1000)
  {
    wait_times = div(milsecs, 1000); /* get whole secs to wait for*/
    pre_milsecs = (long)wait_times.rem + (long)((cvalue.tv_nsec / 1000000) + 1);
  }
        /* milsecs to wait plus clock milsecs to get abs timeout */
  else
  {
      pre_milsecs = (long)(milsecs + (long)(cvalue.tv_nsec / 1000000));
  }
  if (pre_milsecs < 1000) /* no whole secs to add from pre_milsecs */
   {
     ivalue.it_value.tv_nsec =(long)(pre_milsecs * 1000000);
     ivalue.it_value.tv_sec = (time_t)(wait_times.quot + cvalue.tv_sec);
   }
  else  /* get whole secs from pre_milsecs */
  {
    sectimes =  div(pre_milsecs, 1000);
    ivalue.it_value.tv_sec = (time_t)(wait_times.quot + sectimes.quot + cvalue.tv_sec);
    ivalue.it_value.tv_nsec = (long)(sectimes.rem * 1000000);
  }
    
  ivalue.it_interval.tv_sec = 0;
  ivalue.it_interval.tv_nsec = 0;


  /* testing */
 /*
    printf("wait_times quot is %d and rem is %d\n",
    wait_times.quot,wait_times.rem);
  printf("sectimes quot is %d and rem is %d\n", sectimes.quot, sectimes.rem);
  printf("clock times are %d secs and %d nsecs \n", cvalue.tv_sec,
	 cvalue.tv_nsec);

  printf("pre_milsecs are %d\n", pre_milsecs);
  printf("ivalue times are %d secs and %d nsecs \n",  ivalue.it_value.tv_sec,
	 ivalue.it_value.tv_nsec);
 */

    return(ivalue);
}

/* 31 */

Z
dylanXinternalXprimitive_assign_atomic_memory(void * * location, Z newval)
{
  mutex_lock(ATOMICLOCK);
  *location = newval;
  mutex_unlock(ATOMICLOCK);
  return(newval);
}   /* Use of ' * *' in arguments is a hack to avoid problems with 'void *'.
     */

/* 32 */
ZINT
dylanXinternalXprimitive_conditional_update_memory(void * * location, Z newval, Z oldval)
{
  ZINT result = 0;

  mutex_lock(ATOMICLOCK);
  if (*location == oldval)
  {
    *location = newval;
    result = 1;
    /* here success shown by 1, rather than by 0 as elsewhere */
  }
  mutex_unlock(ATOMICLOCK);
  return((ZINT)(I(result)));
}

/* 33 */
void *
dylanXinternalXprimitive_allocate_thread_variable(Z value)
{
  /* Assume that thread is already made.   If threads are running, we will
     have to suspend them anywa.  If none/few are running, then suspending
     them will not matter.
     Get lock, update default vector; suspend all threads *except self!*,
     walk along linked list updating all active thread vectors.
     Unsuspend threads, release lock.
   */
  
  void * newindex;
  thread_t self;
  
  mutex_lock(THREADVLOCK);
  mutex_lock(VECLISTLOCK);
  
  default_vec[vec_index] = value;
      /* give array element value of pointer to var value */
  newindex = (void *) vec_index;
     /* get index of vector element */
  ++vec_index;  /* increment index */

  self = thr_self();
  suspend_threads(self, vectorlist);
  update_threadvecs((int)newindex, value, vectorlist);
  unsus_threads(self, vectorlist);

  mutex_unlock(VECLISTLOCK);
  mutex_unlock(THREADVLOCK);
  return((void *)newindex);
}  /* what is returned is the vector index, i.e. an integer, not a pointer.*/

void
suspend_threads(thread_t self, LINKP list)
{
  if(list->next == 0)
  { ; }  /* list = tail, so stop recursion */
  
  else if(list->thread == self)
    /* first element is self - don't suspend, but do others*/
  {
    suspend_threads(self, (list->next));
  }
  else
  {
    thr_suspend((list->thread));
    suspend_threads(self, (list->next));
       /* do the rest of the list recursively */
  }
}

void
unsus_threads(thread_t self, LINKP list)
{
  if(list->next == 0)
  { ; }  /* list = tail, so stop recursion */
  
  else if(list->thread == self)
    /* first element is self - don't un-suspend, but do others*/
  {
    unsus_threads(self, (list->next));
  }
  else
  {
    thr_continue((list->thread));
    unsus_threads(self, (list->next));
       /* do the rest of the list recursively */
  }
  
}

void
update_threadvecs(int newindex, Z value, LINKP list)
{
  if(list->next == 0)
  { ; }  /* list = tail, so stop recursion */
  else
  {
    (list->thread_vec)[newindex] = value;
    update_threadvecs(newindex, value, (list->next));
  }
}
  

/* 34 */
Z 
dylanXinternalXprimitive_read_thread_variable(void * varhandle)
{
     /* as 'allocate' returns an integer value for the variable id, so
	'read' expects an integer rather than a pointer to be passed in */
  int error;
  void * thread_vector;
  Z result;

  error = thr_getspecific(threadkey, (void *)&thread_vector);
  /* get address of this thread's vector */
  
  result = ((void**)thread_vector)[(int)varhandle];
               /* get contents of vector at given index */
  return(result);
  /* return contents */

}

/* 35 */
Z 
dylanXinternalXprimitive_write_thread_variable(void * varhandle, Z newval)
{
      /* as 'allocate' returns an integer value for the variable id, so
	'write' expects an integer rather than a pointer to be passed in */
  int error;
  void * thread_vector;

  error = thr_getspecific(threadkey, (void *)&thread_vector);
  /* get address of this thread's vector */

  ((void**)thread_vector)[(int)varhandle] = newval;
      /* update contents of vector at given index */

  return(newval);
}


/* 36 */
void
dylanXinternalXprimitive_initialise_current_thread(DTHREAD * thread)
{
  int * thread_vector;
  int i;
  thread_t thread_id = (thr_self());

  mutex_lock(THR_INITLOCK);

  thread_vector = malloc(dvecsize);
  for (i = 0; i < VEC_SIZE; ++i)
  {
  (void *) thread_vector[i] = (void *)default_vec[i];
  }
   thr_setspecific(threadkey, thread_vector);

  /* Now: enter details in linked list */

  update_veclist(thread_id, thread_vector);

  /*Now: enter details in DTHREAD * for Dylan  */

  thread->handle1 = (void *)thread_id;
  thread->handle2 = 0;

  mutex_unlock(THR_INITLOCK);
  
       /* conflict here with dylanXinternalXprimitive_make_thread: here we have to put an
	  integer in the 'handle1' slot, while the 'make_thread' function
	  puts in a pointer returned by malloc.  A pointer to this primary
	  thread is not available.

	  Should we thus use integers all through? But this would inhibit
	  the use of 'free' later.

	  Don't want to 'free' primary thread, and most use of the 'handle1'
	  data is for identity comparison, thus it could be ok to have an
	  integer in the primary thread handle??? 
      */
}


void
initialise_new_thread(DTHREAD * thread)
{
  int * thread_vector;
  int i;
  thread_t thread_id = (thr_self());

  mutex_lock(THR_INITLOCK);

  thread_vector = malloc(dvecsize);
  for (i = 0; i < VEC_SIZE; ++i)
  {
  (void *) thread_vector[i] = (void *)default_vec[i];
  }
   thr_setspecific(threadkey, thread_vector); /* Is this right */

  /* Now: enter details in linked list */

  update_veclist(thread_id, thread_vector); /*lock used in this function */

  mutex_unlock(THR_INITLOCK);
}


void *
keymaker(void)
        /* This should be called once only to initialise each session*/
{       /* It provides a single key for all threads to use */
  static int once = 0;

  if (!once)
  {
    mutex_lock(KEYLOCK);
    if (!once)
    {
      thr_keycreate(&threadkey, free);
      once++;
    }
    mutex_unlock(KEYLOCK);
  }
}

LINKP
listmaker(void)
{
  vectorlist  = (malloc(linksize));

  vectorlist->thread = 0;
  vectorlist->thread_vec = 0;
  vectorlist->next = 0;
}       /* This forms the tail of the list. Subsequent new list
	   elements will be added on as new  heads.  */

LINKP
update_veclist(thread_t newthread, void * thread_v)
{
  LINKP new_element = malloc(linksize);

  mutex_lock(VECLISTLOCK);

  new_element->thread = newthread;
  new_element->thread_vec = thread_v; 
  new_element->next = vectorlist;
  vectorlist = new_element;

  mutex_unlock(VECLISTLOCK);
}

int
trim_veclist(thread_t thread)
{
  LINKP templist;
  
  if(vectorlist->next == 0)
  {return(4); }  /* vectorlist = tail */
  
  else if(vectorlist->thread == thread)
    /* first element is a hit */
  {
    mutex_lock(VECLISTLOCK);
    templist = vectorlist;
    free(vectorlist->thread_vec);
    vectorlist = (vectorlist->next);
    free(templist);
    mutex_unlock(VECLISTLOCK);
    return(0);
  }
  else
  {
    mutex_lock(VECLISTLOCK);
    step_list(thread, (vectorlist->next), vectorlist);
    mutex_unlock(VECLISTLOCK);
  }
    /* search the rest of the list recursively */
}

int
step_list(thread_t thread, LINKP list, LINKP last)
{
  LINKP templist;

  if (list->next == 0) /* list is the tail */
  {return(4);}
  else if(list->thread == thread) /* a hit */
  {
    templist = list;
    free(list->thread_vec);
    last->next = (list->next); /* cut out the 'hit' element */
    free(templist);
    return(0);
  }
  else
  {
    step_list(thread, (list->next), (list));
  }  /* search the rest of the list */
}
