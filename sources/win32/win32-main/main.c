#include "run-time.h"
#include <stdlib.h>
#include <string.h>

extern int main (int argc, char *argv[]);
extern char* __stdcall GetCommandLineA (void);

int __stdcall WinMain(unsigned long hInstance,
                      unsigned long hPrevInstance,
                      unsigned long lpCmdLine,
                      int           nCmdShow)
{
  char*  command_line;
  char*  token;
  int    argc;
  char** argv;

  command_line = GetCommandLineA();
  argv = malloc((strlen(command_line)/2 + 1) * sizeof(char*));
  argc = 0;

  command_line = strdup(command_line);
  token = strtok(command_line, " ");
  while (token != NULL) {
    argv[argc] = token;
    argc++;
    token = strtok(NULL, " ");
  }

  main(argc, argv);
  return(0);
}
