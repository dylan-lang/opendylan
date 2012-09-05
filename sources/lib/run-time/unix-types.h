/* Unix specific type declarations, etc ... */

#include <signal.h>
#include <pthread.h>

typedef void                *HANDLE;
typedef unsigned char        BYTE;
typedef int                  BOOL;
typedef unsigned long        DWORD;
typedef long long            _int64;

#define WINAPI

#define FALSE               0
#define TRUE                1

#define CTRL_C_EVENT        0
#define CTRL_BREAK_EVENT    1
#define CTRL_CLOSE_EVENT    2
// 3 is reserved!
// 4 is reserved!
#define CTRL_LOGOFF_EVENT   5
#define CTRL_SHUTDOWN_EVENT 6

/* Critical section macros */

#ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#define define_CRITICAL_SECTION(lock) \
  pthread_mutex_t lock = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#else
#define define_CRITICAL_SECTION(lock) \
  pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER
#endif

#define extern_CRITICAL_SECTION(lock) \
  extern pthread_mutex_t lock
#define initialize_CRITICAL_SECTION(mutex) \
  do { \
    pthread_mutexattr_t _attr; \
    pthread_mutexattr_init(&_attr); \
    pthread_mutexattr_settype(&_attr, PTHREAD_MUTEX_RECURSIVE); \
    pthread_mutex_init((mutex), &_attr); \
    pthread_mutexattr_destroy(&_attr); \
  } while(0)

#define enter_CRITICAL_SECTION(lock)      pthread_mutex_lock(lock)
#define leave_CRITICAL_SECTION(lock)      pthread_mutex_unlock(lock)

/* Events */

#define EVENT_WAIT_SUCCESS TRUE
#ifndef INFINITE
#define INFINITE -1
#endif
#define create_EVENT(attributes, manualReset, initialState, Name) (HANDLE)100
#define wait_for_EVENT(handle, milliseconds) EVENT_WAIT_SUCCESS
#define set_EVENT(handle) TRUE

/* Console Interrupts */

#define set_CONSOLE_CTRL_HANDLER(handler, activate) {}
