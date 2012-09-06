/* Windows specific type declarations, etc ... */

#include <windows.h>

/* Critical section macros */

#define define_CRITICAL_SECTION(lock)     CRITICAL_SECTION lock
#define extern_CRITICAL_SECTION(lock)     extern CRITICAL_SECTION lock
#define initialize_CRITICAL_SECTION(lock) InitializeCriticalSection(lock)
#define enter_CRITICAL_SECTION(lock)      EnterCriticalSection(lock)
#define leave_CRITICAL_SECTION(lock)      LeaveCriticalSection(lock)

/* Events */

#define EVENT_WAIT_SUCCESS WAIT_OBJECT_0
#define create_EVENT(attributes, manualReset, initialState, Name) \
  CreateEvent(attributes, manualReset, initialState, Name)
#define wait_for_EVENT(handle, milliseconds) \
  WaitForSingleObject(handle, milliseconds)
#define set_EVENT(handle) \
  SetEvent(handle)

/* Console Interrupts */

#define set_CONSOLE_CTRL_HANDLER(handler, activate) SetConsoleCtrlHandler(handler, activate)
