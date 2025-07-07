#ifdef __APPLE__
#define _DARWIN_C_SOURCE
#endif

#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <dlfcn.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <pwd.h>
#include <string.h>


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

char *system_getenv(char *name)
{
  return getenv(name);
}

int system_setenv(char *name, char *value, int overwrite)
{
  return setenv(name, value, overwrite);
}

int system_unsetenv(const char *name)
{
  return unsetenv(name);
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

void *system_dlopen(const char *name)
{
  return dlopen(name, RTLD_NOW | RTLD_GLOBAL);
}

DIR *system_opendir(const char *dirname)
{
  return opendir(dirname);
}

int system_closedir(DIR *dirp)
{
  return closedir(dirp);
}

struct dirent *system_readdir(DIR *dirp)
{
  return readdir(dirp);
}

const char *system_dirent_name(struct dirent *dirent)
{
  return dirent->d_name;
}

int system_stat(const char* path, struct stat* buf)
{
  return stat(path, buf);
}

int system_lstat(const char* path, struct stat* buf)
{
  return lstat(path, buf);
}

const char* unix_tmpdir(void)
{
  const char* tmpdir = getenv("TMPDIR");
  if (tmpdir == NULL || *tmpdir == '\0') {
#ifdef P_tmpdir
    tmpdir = P_tmpdir;
#else
    tmpdir = "/tmp";
#endif
  }
  return tmpdir;
}

int system_concurrent_thread_count(void)
{
  long count = sysconf(_SC_NPROCESSORS_ONLN);
  if (count < 0) {
    return 0;
  }
  return (int) count;
}

// Store the homedir associated with `username` into `homedir`. `homedir_size` is the
// size of the `homedir` buffer. Returns 0 on success, -1 on failure.
int system_user_homedir (const char* username, char* homedir, int homedir_size) {
  int passwd_bufsize = 0;
  if ((passwd_bufsize = sysconf(_SC_GETPW_R_SIZE_MAX)) == -1) {
    return -1;
  }
  char buffer[passwd_bufsize];
  struct passwd pwd;
  struct passwd *result = NULL;
  if (getpwnam_r(username, &pwd, buffer, passwd_bufsize, &result) != 0 || !result) {
    return -1;
  }
  int len = strlen(pwd.pw_dir);
  if (len >= homedir_size) {
    return -1;
  }
  strncpy(homedir, pwd.pw_dir, len);
  return 0;
}

// Store the username associated with `uid` into `username`. `username_size` is the size
// of the `username` buffer.  Returns 0 on success, -1 on failure.
int system_passwd_username_from_uid (uid_t uid, char* username, int username_size) {
  int passwd_bufsize = 0;
  if ((passwd_bufsize = sysconf(_SC_GETPW_R_SIZE_MAX)) == -1) {
    return -1;
  }
  char buffer[passwd_bufsize];
  struct passwd pwd;
  struct passwd *result = NULL;
  if (getpwuid_r(uid, &pwd, buffer, passwd_bufsize, &result) != 0 || !result) {
    return -1;
  }
  int len = strlen(pwd.pw_name);
  if (len >= username_size) {
    return -1;
  }
  strncpy(username, pwd.pw_name, len);
  return 0;
}
