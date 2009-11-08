/* Core_unix support functions written in C. */

#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

#include "ocaml_utils.h"

#define MAX_ERROR_LEN 4096

/*
   Report errors on the forked side and then exits with 254 (255 clashes with
   ssh)
*/
static void report_error(int fd, const char* str)
{
  char buf[MAX_ERROR_LEN];
  char buf2[MAX_ERROR_LEN];
  if (strerror_r(errno, buf, MAX_ERROR_LEN) == -1)
    snprintf(buf, 80, "Unknown error %d", errno);
  snprintf(buf2, MAX_ERROR_LEN, "%s (%s)\n", str, buf);
  buf2[MAX_ERROR_LEN - 1] = '\0';
  write(fd, buf2, strlen(buf2));
  fsync(2);
  /* we do not use exit because the child shouldn't
     do any more cleaning (e.g. flushing). */
  _exit(254);
}

/* Maximum number of arguments plus one (for terminating NULL) that may
   be passed to execv/execvp.  4096 is the minimum that POSIX allows,
   but note that it is not specified how much argument space is used
   up by variables in the user's environment.  This makes it hard to
   predict whether the system call can fail due to too many (implicit)
   arguments. */
#define ARG_MAX (4096 + 1)

/* Close function that handles signals correctly by retrying the close
   after EINTR.

   NOTE: we should never see EIO when closing pipes.  If so, it is
   reasonable to see this as a kernel bug, and it's pretty useless trying
   to catch/work around potential kernel bugs.  We assume that it works.
   An EBADF would be bad when closing successfully opened pipes, too, but
   in that case the pipe should be guaranteed to be closed anyway (unlike
   EIO).  This covers all errors that close could potentially return.
*/
static inline void safe_close(int fd, int err_fd)
{
  int ret;
  while ((ret = close(fd)) == -1 && errno == EINTR) /* empty loop */ ;
  if (ret == -1) report_error(err_fd ,"Failed to close fds");
  return;
}

static inline void safe_dup2(int src, int tgt, int err_fd)
{
  if (dup2 (src, tgt) == -1) report_error(err_fd,"Failed to dup2");
}

/*
  Tries to dup and report to err_fd in case it fails.
*/
static inline int safe_dup(int fd, int err_fd){
  int ret = dup (fd);
  if (ret == -1)
    report_error(err_fd, "could not dup fds in child process");
  return ret;
}

/* Given v_prog, an O'Caml string value specifying a program name,
   v_args, an O'Caml array specifying program arguments (not
   including the program name), and v_search_path, an O'Caml boolean
   value specifying whether to search the PATH, fork a child process
   that executes the specified program.  Return the child's pid together
   with fds connected via pipes to the stdin, stdout and stderr of the
   program such that if the fds are closed the pipes are broken.

   Beware!
   A great deal of work has gone into making this subtle function thoroughly
   robust and, hopefully, correct.  Changes should not be undertaken lightly.
*/

CAMLprim value extended_ml_create_process(value v_prog, value v_args,
                                 value v_stdin, value v_stdout, value v_stderr)
{
  CAMLparam5(v_prog, v_args, v_stdin, v_stdout, v_stderr);
  int stdin_fd = Int_val (v_stdin);
  int stdout_fd = Int_val (v_stdout);
  int stderr_fd = Int_val (v_stderr);

  /* It's ok to hold pointers into the O'Caml heap, since the memory
     space gets duplicated upon the fork, during which we keep the
     O'Caml lock. */
  char *prog = String_val(v_prog);

  pid_t child_pid;

  /* It is reasonable to assume that the array can never become too big for
     stack allocations. */
  char *args[ARG_MAX];
  int n_args =  Wosize_val(v_args);

  /* Note that the executable name also counts as an argument, and we
     also have to subtract one more for the terminating NULL! */
  if (n_args >= ARG_MAX - 1)
    caml_failwith("too many arguments for Unix.create_process");

  args[0] = prog;
  args[n_args + 1] = NULL;

  while (n_args) {
    args[n_args] = String_val(Field(v_args, n_args - 1));
    --n_args;
  }

  /* This function deliberately doesn't release the O'Caml lock (i.e. it
     doesn't call caml_enter_blocking_section) during the fork.  This is
     because we hold pointers into the ML heap across a fork, and
     releasing the lock immediately before the fork could theoretically
     cause the GC to run and move blocks before the fork duplicates the
     memory space.

     If the parent process has threads that turn out to suffer from too
     much latency during this fork, we may want to rewrite this function
     to copy the O'Caml values into the C heap before the fork and release
     the O'Caml lock.  It seems unlikely that forks will ever take so
     long that people care.  In Linux 2.6 forks are practically constant
     time even in the presence of ridiculous amounts of processes, and
     are reported to always have less than 500us latency.  Maybe the
     kernel does not even schedule threads during forks anyway.  */
  switch (child_pid = fork()) {
  case -1:
    uerror("unsafe_process", Nothing);
  case 0:
    /* Child process. */

    /* Just in case any of the pipes' file descriptors are 0, 1 or 2
       (not inconceivable, especially when running as a daemon),
       duplicate all three descriptors we need in the child to fresh
       descriptors before duplicating them onto stdin, stdout and stderr.

       This will ensure that there is one and only one copy of the file
       descriptors passed as arguments with id's higher than 2.
    */
    while (stdin_fd <= 2) stdin_fd = safe_dup(stdin_fd, stderr_fd);
    while (stdout_fd <= 2) stdout_fd = safe_dup(stdout_fd, stderr_fd);
    while (stderr_fd <= 2) stderr_fd = safe_dup(stderr_fd, stderr_fd);

    /* We must dup2 the descriptors back in place... */
    safe_dup2(stdin_fd, 0, stderr_fd);
    safe_dup2(stdout_fd, 1 , stderr_fd);
    safe_dup2(stderr_fd, 2, stderr_fd);

    /* And close the source */
    safe_close(stdin_fd, 2);
    safe_close(stdout_fd, 2);
    safe_close(stderr_fd, 2);

    execvp(prog, args);
    report_error(2 /* stderr */, "execvp/execv failed in child process");

  default : /* Parent process */
    CAMLreturn(Val_int(child_pid));
  }
}
