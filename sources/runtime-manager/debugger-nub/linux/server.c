
#include "nub-core.h"
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>

bfd* create_object_file(char* module_name)
{
  asymbol ** symbol_table;
  int nsymbytes, number_of_symbols;
  bfd* mybfd = bfd_openr(module_name, NULL);
  bfd_init();
  bfd_set_default_target ();
  if (mybfd == NULL)
    bfd_perror("opening bfd lossage");
  else
    printf("Got a bfd!\n");
  nsymbytes = bfd_get_symtab_upper_bound(mybfd);
  symbol_table = (asymbol **) xmalloc (nsymbytes);
  number_of_symbols =
    bfd_canonicalize_symtab (mybfd, symbol_table);
  printf("Canonicalized %d symbols in %d bytes.  How about that start address? %x\n", number_of_symbols, nsymbytes, mybfd->start_address);
  return mybfd;
}

NUB create_and_debug_process 
  (SERVER dummy, 
   char *module_name, 
   char *arguments,
   NUBINT path_count, char **paths,
   NUBINT lib_path_count, char **lib_paths,
   char *workdir,
   NUBINT create_shell)
{
  int pid, waitingpid, status;
  LPDBGPROCESS    process_handle = (LPDBGPROCESS) malloc (sizeof (DBGPROCESS));
  process_handle->Command = module_name;
  process_handle->Arguments = arguments;
  process_handle->SymbolPaths = paths;
  process_handle->LibrarySearchPaths = lib_paths;
  process_handle->object_file = create_object_file(module_name);
  call_check_error(pid = fork(), "Fork failed!");
    
  if (pid == 0) { //child process
    //gdb does this.  Why?
    call_check_error(setpgrp (getpid (), getpid ()),"setpgrp failed");
    //The next thing that gdb does is to *exec* the process thru the shell. Why?
    //Let's try just execing the program.
    call_check_error(ptrace(PTRACE_TRACEME),"ptrace_traceme failed");
    execlp(module_name, module_name, arguments,NULL);
    check_error(errno, "If we got here, the child didn't exec");
  }
  //parent process
  waitingpid=waitpid(pid, &status, 0);
  check_error (waitingpid == -1, "waitpid error");
  process_handle->ProcessAttached = 1;    
  process_handle->ProcessID = pid;
  process_handle->State = status;
  return ((NUB) process_handle);

}

NUB debug_active_process
  (SERVER dummy,
   NUBPROCESS nubprocess,
   NUBINT symbol_path_count,
   char **symbol_paths,
   char *system_JIT_information)
{
  
}


