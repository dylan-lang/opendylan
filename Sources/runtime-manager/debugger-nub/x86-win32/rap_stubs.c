/* Stubs for remote debugger nub callbacks to the Dylan access-path library */

/* Since the remote debugger nub is running on a different machine than the
   Dylan access-path library to which it's connected, it can't invoke these
   callbacks.  We must include these stubs to permit proper linking of rnub.exe */

/* Stubs marked with "NEED PROTOCOL" should be replaced by a mechanism that
   allows the remote debugger nub to send a message to the access-path to
   perform the needed action. */

#include "nub-core.h"

void debugger_message(char* message, TARGET_ADDRESS addr1, TARGET_ADDRESS addr2) {}

void debugger_error(char* message, TARGET_ADDRESS addr1, TARGET_ADDRESS addr2) {}

void nub_debug_message(char* message, TARGET_ADDRESS addr1, TARGET_ADDRESS addr2) {}

/*---*** NEED PROTOCOL ***---*/
void create_thread_stop_reason_handler() {}
