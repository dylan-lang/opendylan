#if defined(OPEN_DYLAN_PLATFORM_WINDOWS)
typedef unsigned __int64 uint64_t;
#else
#include <stdint.h>
#endif

uint64_t dylan_current_thread_id(void);
void dylan_set_current_thread_name(const char *name);
