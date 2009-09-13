#include <unistd.h>
#include <signal.h>
#include <errno.h>

#ifdef __APPLE__
#include <crt_externs.h>
#define environ (*_NSGetEnviron())
#else
extern char **environ;
#endif

char **system_environ(void)
{
  return environ;
}

/* Adapted from the SBCL run-time system, which in turn is derived
 * from the CMU CL system, which was written at Carnegie Mellon
 * University and released into the public domain.
 */
int system_spawn(char *program, char **argv, char **envp, char *dir,
		 int inherit_console,
		 int stdin_fd, int stdout_fd, int stderr_fd)
{
  int pid = vfork();
  int fd;
  sigset_t sset;

  if (pid != 0)
    return pid;

  if (dir) {
    /* Set a new working directory */
    if (chdir(dir) != 0)
      _exit(127);
  }

  if (!inherit_console) {
    /* Put us in our own process group. */
    setsid();
  }

  /* unblock signals */
  sigemptyset(&sset);
  sigprocmask(SIG_SETMASK, &sset, NULL);
  
  /* Set up stdin, stdout, and stderr */
  if (stdin_fd >= 0)
    dup2(stdin_fd, 0);
  if (stdout_fd >= 0)
    dup2(stdout_fd, 1);
  if (stderr_fd >= 0)
    dup2(stderr_fd, 2);
  
  /* Close all other fds. */
  for (fd = sysconf(_SC_OPEN_MAX) - 1; fd > 2; fd--)
    close(fd);
  
  /* Exec the program. */
  execve(program, argv, envp);

  /* The exec didn't work, flame out. */
  _exit(127);
}
