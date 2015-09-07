/* ************************************************************************ */
/* ** basic-test.c                                                       ** */
/* ** Runs a program under the debugger nub and reports stop-reasons     ** */
/* ** ------------------------------------------------------------------ ** */
/* ** Author: Paul Howard  Copyright: (c) 1996 Functional Objects, Inc.   ** */
/* **                                 All Rights Reserved                ** */
/* ************************************************************************ */

#include <stdio.h>

#include "nub-core.h"

void create_thread_stop_reason_handler
  (LPDBGPROCESS process, LPDBGTHREAD thread, NUBINT priority)
{
  printf("create_thread_stop_reason_handler\n");
}

void nub_debug_message(char* message, TARGET_ADDRESS addr1, TARGET_ADDRESS addr2)
{
  printf("nub_debug_message %s: %#lx, %#lx\n", message,
         (unsigned long) addr1,
         (unsigned long) addr2);
}

void debugger_error(char* message, TARGET_ADDRESS addr1, TARGET_ADDRESS addr2)
{
  printf("debugger_error %s: %#lx, %#lx\n", message,
         (unsigned long) addr1,
         (unsigned long) addr2);
}

// Prints a description of a stop-reason from its code.
void print_stop_reason_description (NUB nub, NUBINT stopcode)
{
  NUBLIBRARY nublibrary;
  char name[1024];

  switch (stopcode) {
  case TIMED_OUT:
    printf("No stop reason was received within the timeout.\n");
    break;

  case ACCESS_VIOLATION_EXCEPTION_DBG_EVENT:
    printf("Exception: access violation.\n");
    break;

  case ARRAY_BOUNDS_EXCEPTION_DBG_EVENT:
    printf("Exception: array boundary check failed.\n");
    break;

  case ILLEGAL_INSTRUCTION_EXCEPTION_DBG_EVENT:
    printf("Exception: Illegal instruction.\n");
    break;

  case PRIVILEGED_INSTRUCTION_EXCEPTION_DBG_EVENT:
    printf("Exception: Privileged instruction.\n");
    break;

  case DENORMAL_EXCEPTION_DBG_EVENT:
    printf("Exception: Floating point denormal error.\n");
    break;

  case FLOAT_DIVIDE_BY_ZERO_EXCEPTION_DBG_EVENT:
    printf("Exception: Floating point division by zero.\n");
    break;

  case INEXACT_RESULT_EXCEPTION_DBG_EVENT:
    printf("Exception: Floating point inexact result.\n");
    break;

  case INVALID_OPERATION_EXCEPTION_DBG_EVENT:
    printf("Exception: Floating point invalid operation error.\n");
    break;

  case FLOAT_OVERFLOW_EXCEPTION_DBG_EVENT:
    printf("Exception: Floating point overflow.\n");
    break;

  case FLOAT_UNDERFLOW_EXCEPTION_DBG_EVENT:
    printf("Exception: Floating point underflow.\n");
    break;

  case FLOAT_STACK_CHECK_EXCEPTION_DBG_EVENT:
    printf("Exception: Floating point stack-check failed.\n");
    break;

  case INTEGER_DIVIDE_BY_ZERO_EXCEPTION_DBG_EVENT:
    printf("Exception: Integer division by zero.\n");
    break;

  case NONCONTINUABLE_EXCEPTION_DBG_EVENT:
    printf("Exception: Tried to continue from noncontinuable error.\n");
    break;

  case BREAKPOINT_EXCEPTION_DBG_EVENT:
    printf("Exception: Breakpoint hit.\n");
    break;

  case HARD_CODED_BREAKPOINT_DBG_EVENT:
    printf("Exception: Hard-coded breakpoint hit (first=%d)\n",
           nub_first_hard_coded_breakpoint(nub));
    break;

  case SINGLE_STEP_DBG_EVENT:
    printf("Exception: Single instruction step.\n");
    break;

  case CREATE_PROCESS_DBG_EVENT:
    printf("Event: Process creation.\n");
    break;

  case EXIT_PROCESS_DBG_EVENT:
    printf("Event: Process termination.\n");
    break;

  case CREATE_THREAD_DBG_EVENT:
    printf("Event: The process created a new thread.\n");
    break;

  case EXIT_THREAD_DBG_EVENT:
    printf("Event: The process terminated a thread.\n");
    break;

  case LOAD_DLL_DBG_EVENT:
    nublibrary = nub_stop_reason_library(nub);
    nub_get_library_undecorated_name(nub, nublibrary, sizeof name, name);
    printf("Event: The process loaded a DLL: %s", name);
    nub_get_library_filename(nub, nublibrary, sizeof name, name);
    printf(" (%s)\n", name);
    break;

  case UNLOAD_DLL_DBG_EVENT:
    printf("Event: The process unloaded a DLL.\n");
    break;

  case RIP_DBG_EVENT:
    printf("Event: The process died (RIP event).\n");
    break;

  case OUTPUT_DEBUG_STRING_DBG_EVENT:
    printf("Event: The process reported a debug message.\n");
    break;

  case PROFILER_DBG_EVENT:
    printf("Event: The profiler engine signalled a timer event.\n");
    break;

  default:
    printf("**** ERROR: A stop-reason with invalid code %d was reported.\n",
           stopcode);
    break;

  }
}


void main (int argc, char **argv)
{
  NUB                test_process;
  NUBINT             stop_reason_code;
  NUBINT             tether_success;
  BOOL               application_running;
  
  printf("Starting debugger nub test procedure.\n");

  // Attempt to open a local tether to the executable specified on
  // the command line. There's no command line error checking or
  // parsing here. I can't be bothered.

  printf("Attempting to tether locally to %s\n", argv[1]);
  test_process = open_local_tether(argv[1], "",
                                   0, NULL,
                                   0, NULL,
                                   "",
                                   0,
                                   &tether_success);

  // Now we error-check.

  if (tether_success == 0) {
    printf("Test failed. Could not open the executable %s.\n",
           argv[1]);
    ExitProcess(0);
  }

  // Get the process moving
  nub_application_restart(test_process);
  application_running = TRUE;

  // Enter a loop waiting for stop-reasons and printing them as they
  // arrive. When the exit-process stop reason occurs, shut down and
  // get the hell out of Dodge.

  while (application_running) {

    // There's no real reason to use a timeout here. This can happily
    // be changed.

    nub_wait_for_stop_reason_with_timeout(test_process, 2000,
                                          &stop_reason_code);
    print_stop_reason_description(test_process, stop_reason_code);

    // Now behave properly.

    if (stop_reason_code == EXIT_PROCESS_DBG_EVENT) {
      printf("Detected process termination. Should be quitting next...\n");
      application_running = FALSE;
    }
    else if (stop_reason_code == TIMED_OUT) {
      printf("The application is still running. Waiting again...\n");
    }
    else {
      printf("Continuing...\n");
      nub_application_continue(test_process);
    }
  }

  printf("Debugger nub test done.\n");
}
