#include "utilities.h"
#include "nub-core.h"
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>



void continue_application (NUB nub, int handling)
{
  int waitingpid, count = 0;
  LPDBGPROCESS mynub = (LPDBGPROCESS) nub;
  NUBINT pid = mynub->ProcessID;
  NUBINT status = mynub->State;
  while (!WIFEXITED(status) && count < 100) {
    call_check_error(ptrace( PTRACE_SYSCALL, pid,1,0), "ptrace_syscall lossage");
    waitingpid=waitpid(-1, &status, 0);
    check_error (waitingpid == -1, "waitpid error");
    printf("%d,",WSTOPSIG(status));count++;
    if (WSTOPSIG(status) != 5)
      printf("\n");
    if(waitingpid != pid)
      printf("signal from different pid: %d\n",waitingpid);
  }
  mynub->State = status;  
}


void nub_application_continue (NUB nub)
{
  continue_application(nub, 0); //EXCEPTIONS_HANDLED
}
