#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <stddef.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/ptrace.h>

#include "nub-core.h"


void doatt(pid,already)
{
  if (already) {
    printf("Detaching from %d\n",pid);
    if (ptrace (PTRACE_DETACH, pid, 0, 0) != 0)
      printf("Something went wrong detaching from %d\n",pid);
  } else {
    printf("Attaching to %d\n",pid);
    if (ptrace (PTRACE_ATTACH, pid, 0, 0) !=0)
      printf("Something went wrong attaching to %d\n",pid);
  }
}

    
main(int argc,char **argv)
{
  char buf[10], *val;
  int success, rval;
  void * mynub;
  mynub = open_local_tether("/amd/holmes/u/ldisk/yduj/linuxtest","",0,NULL,0,NULL,"",0,&success);
  if(success)
    printf("Created tether.");
  else
    printf("Some lossage creating tether...");
  
  buf[0] = 0;
  while(buf[0] != 'q') {
    printf("c, m, q: ");
    gets(buf);
    switch(buf[0]) {
    case 'm':
      printf("Getting memory\n");
      rval = read_32b_from_process_memory (mynub, 0x80000000,&success);
      if (success)
	perror("Read memory lost");
      else
	printf("Read memory: value %d\n", rval);
      break;
    case 'c': 
      printf("Continuing\n");
      nub_application_continue(mynub);
      break;
    case 'q':
      printf("Quitting\n");
      break;
    default:
      printf("Eh?");
      break;
    }
  }
  /*
  if(mainatt)
    doatt(mainpid, mainatt);
  if (friendatt)
    doatt(friendpid, friendatt);
  */
}
