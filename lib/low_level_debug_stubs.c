#define _GNU_SOURCE
#include <string.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

static void signal_handler(int sig)
{
  char *signame = strsignal(sig);
  fprintf(stderr, "stopping process %d after signal %d (%s)\n",
          getpid(), sig, signame);
  fflush(stderr);
  kill(getpid(), SIGSTOP);
}

CAMLprim value low_level_debug_stop_upon_sigbus(value v_unit)
{
  signal(SIGBUS, signal_handler);
  return v_unit;
}

CAMLprim value low_level_debug_stop_upon_sigsegv(value v_unit)
{
  signal(SIGSEGV, signal_handler);
  return v_unit;
}

CAMLprim value low_level_debug_stop_upon_sigpipe(value v_unit)
{
  signal(SIGPIPE, signal_handler);
  return v_unit;
}

static void at_exit_handler(void)
{
  fprintf(stderr, "stopping process %d at exit\n", getpid());
  fflush(stderr);
  kill(getpid(), SIGSTOP);
}

CAMLprim value low_level_debug_stop_upon_exit(value v_unit)
{
  atexit(at_exit_handler);
  return v_unit;
}

CAMLprim value low_level_debug_stop_me_now(value v_unit)
{
  fprintf(stderr, "stopping process %d now\n", getpid());
  fflush(stderr);
  kill(getpid(), SIGSTOP);
  return v_unit;
}
