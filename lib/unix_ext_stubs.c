#define _GNU_SOURCE

#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <grp.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fnmatch.h>
#include <wordexp.h>
#include <stdio.h>
#include <assert.h>
#include <sys/uio.h>
#include <time.h>
#include <unistd.h>
#ifdef HAVE_SYSCALL_H
#include <syscall.h>
#else
#warning "no syscall.h"
#endif
#include "ocaml_utils.h"

/* Filesystem functions */

CAMLprim value unix_mknod_stub(
  value v_pathname, value v_mode, value v_perm, value v_major, value v_minor)
{
  CAMLparam1(v_pathname);

  int ret, len;
  char *pathname;
  mode_t mode = Int_val(v_perm);
  dev_t dev = 0;

  switch (Int_val(v_mode)) {
    case 0 : mode |= S_IFREG; break;
    case 2 :
      mode |= S_IFCHR;
      dev = makedev(Int_val(v_major), Int_val(v_minor));
      break;
    case 3 :
      mode |= S_IFBLK;
      dev = makedev(Int_val(v_major), Int_val(v_minor));
      break;
    case 5 : mode |= S_IFIFO; break;
    case 6 : mode |= S_IFSOCK; break;
    default : caml_invalid_argument("mknod");
  }

  len = caml_string_length(v_pathname) + 1;
  pathname = caml_stat_alloc(len);
  memcpy(pathname, String_val(v_pathname), len);

  caml_enter_blocking_section();
    ret = mknod(pathname, mode, dev);
    caml_stat_free(pathname);
  caml_leave_blocking_section();

  if (ret == -1) uerror("mknod", v_pathname);

  CAMLreturn(Val_unit);
}


/* I/O functions */

#define DIR_Val(v) *((DIR **) &Field(v, 0))
typedef struct dirent directory_entry;

CAMLprim value unix_sync(value v_unit)
{
  caml_enter_blocking_section();
    sync();
  caml_leave_blocking_section();
  return v_unit;
}

CAMLprim value unix_fsync(value v_fd)
{
  int ret;
  caml_enter_blocking_section();
    ret = fsync(Int_val(v_fd));
  caml_leave_blocking_section();
  if (ret == -1) uerror("fsync", Nothing);
  return Val_unit;
}

#if defined(_POSIX_SYNCHRONIZED_IO) && (_POSIX_SYNCHRONIZED_IO > 0)
CAMLprim value unix_fdatasync(value v_fd)
{
  int ret;
  caml_enter_blocking_section();
    ret = fdatasync(Int_val(v_fd));
  caml_leave_blocking_section();
  if (ret == -1) uerror("fdatasync", Nothing);
  return Val_unit;
}
#else
#warning "POSIX SIO not present; unix_fdatasync undefined"
#endif

CAMLprim value unix_dirfd(value v_dh)
{
  int ret = 0;
  if (DIR_Val(v_dh) == NULL)
    caml_invalid_argument("dirfd: NULL directory handle (probably closed)");
  ret = dirfd(DIR_Val(v_dh));
  if (ret == -1) uerror("dirfd", Nothing);
  return Val_int(ret);
}

CAMLprim value unix_readdir_ino_stub(value v_dh)
{
  DIR *d;
  directory_entry * e;
  d = DIR_Val(v_dh);
  if (d == (DIR *) NULL) unix_error(EBADF, "readdir_ino", Nothing);
  caml_enter_blocking_section();
    e = readdir((DIR *) d);
  caml_leave_blocking_section();
  if (e == (directory_entry *) NULL) caml_raise_end_of_file();
  else {
    CAMLparam0();
    CAMLlocal2(v_name, v_ino);
    value v_res;
    v_name = caml_copy_string(e->d_name);
    v_ino = caml_copy_nativeint(e->d_ino);
    v_res = caml_alloc_small(2, 0);
    Field(v_res, 0) = v_name;
    Field(v_res, 1) = v_ino;
    CAMLreturn(v_res);
  }
}

CAMLprim value unix_error_stub(value v_errcode, value v_cmdname, value cmd_arg)
{
  unix_error(Int_val(v_errcode), String_val(v_cmdname), cmd_arg);
  return Val_unit;
}

CAMLprim value unix_read_assume_fd_is_nonblocking_stub(
  value v_fd, value v_buf, value v_pos, value v_len)
{
  char *buf = String_val(v_buf) + Long_val(v_pos);
  ssize_t ret = read(Int_val(v_fd), buf, Long_val(v_len));
  if (ret == -1) uerror("unix_read_assume_fd_is_nonblocking", Nothing);
  return Val_long(ret);
}

CAMLprim value unix_write_assume_fd_is_nonblocking_stub(
  value v_fd, value v_buf, value v_pos, value v_len)
{
  char *buf = String_val(v_buf) + Long_val(v_pos);
  ssize_t ret = write(Int_val(v_fd), buf, Long_val(v_len));
  if (ret == -1) uerror("unix_write_assume_fd_is_nonblocking", Nothing);
  return Val_long(ret);
}

CAMLprim value unix_writev_assume_fd_is_nonblocking_stub(
  value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  ssize_t ret;
  struct iovec *iovecs = caml_stat_alloc(sizeof(struct iovec) * count);
  int i = count - 1;
  for (; i >= 0; --i) {
    struct iovec *iovec = &iovecs[i];
    value v_iovec = Field(v_iovecs, i);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    value v_iov_len = Field(v_iovec, 2);
    iovec->iov_base = String_val(v_iov_base) + Long_val(v_iov_pos);
    iovec->iov_len = Long_val(v_iov_len);
  }
  ret = writev(Int_val(v_fd), iovecs, count);
  caml_stat_free(iovecs);
  if (ret == -1) uerror("unix_writev_assume_fd_is_nonblocking", Nothing);
  return Val_long(ret);
}

CAMLprim value unix_writev_stub(value v_fd, value v_iovecs, value v_count)
{
  int i, count = Int_val(v_count), len = 0;
  ssize_t ret;
  char *buf, *dst;
  for (i = count - 1; i >= 0; --i) {
    value v_iovec = Field(v_iovecs, i);
    value v_iov_len = Field(v_iovec, 2);
    len += Long_val(v_iov_len);
  }
  buf = caml_stat_alloc(len);
  dst = buf + len;
  for (i = count - 1; i >= 0; --i) {
    value v_iovec = Field(v_iovecs, i);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    value v_iov_len = Field(v_iovec, 2);
    size_t iov_len = Long_val(v_iov_len);
    dst -= iov_len;
    memcpy(dst, String_val(v_iov_base) + Long_val(v_iov_pos), iov_len);
  }
  caml_enter_blocking_section();
    ret = write(Int_val(v_fd), buf, len);
    caml_stat_free(buf);
  caml_leave_blocking_section();
  if (ret == -1) uerror("unix_writev", Nothing);
  return Val_long(ret);
}


/* pselect */

typedef fd_set file_descr_set;

static inline void fdlist_to_fdset(value fdlist, fd_set *fdset, int *maxfd)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    int fd = Int_val(Field(l, 0));
    FD_SET(fd, fdset);
    if (fd > *maxfd) *maxfd = fd;
  }
}

static inline value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value l;
  value res = Val_int(0);

  Begin_roots2(l, res);
    for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
      int fd = Int_val(Field(l, 0));
      if (FD_ISSET(fd, fdset)) {
        value newres = caml_alloc_small(2, 0);
        Field(newres, 0) = Val_int(fd);
        Field(newres, 1) = res;
        res = newres;
      }
    }
  End_roots();
  return res;
}

static inline void decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

CAMLprim value unix_pselect_stub(
  value v_rfds, value v_wfds, value v_efds, value v_timeout, value v_sigmask)
{
  fd_set rfds, wfds, efds;
  double tm = Double_val(v_timeout);
  struct timespec ts;
  struct timespec *tsp;
  int maxfd = -1, ret;
  value v_res;
  sigset_t sigmask;

  decode_sigset(v_sigmask, &sigmask);

  Begin_roots3(v_rfds, v_wfds, v_efds);
    fdlist_to_fdset(v_rfds, &rfds, &maxfd);
    fdlist_to_fdset(v_wfds, &wfds, &maxfd);
    fdlist_to_fdset(v_efds, &efds, &maxfd);

    if (tm < 0.0) tsp = (struct timespec *) NULL;
    else {
      
      ts.tv_sec = (int) tm;
      ts.tv_nsec = (int) (1e9 * (tm - ts.tv_sec));
      tsp = &ts;
    }

    caml_enter_blocking_section();
      ret = pselect(maxfd + 1, &rfds, &wfds, &efds, tsp, &sigmask);
    caml_leave_blocking_section();

    if (ret == -1) uerror("pselect", Nothing);

    v_rfds = fdset_to_fdlist(v_rfds, &rfds);
    v_wfds = fdset_to_fdlist(v_wfds, &wfds);
    v_efds = fdset_to_fdlist(v_efds, &efds);
    v_res = caml_alloc_small(3, 0);
    Field(v_res, 0) = v_rfds;
    Field(v_res, 1) = v_wfds;
    Field(v_res, 2) = v_efds;
  End_roots();

  return v_res;
}


/* Clock functions */

#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > -1)
#define clockid_t_val(v_cl) ((clockid_t) Nativeint_val(v_cl))

CAMLprim value unix_clock_gettime(value v_cl)
{
  struct timespec ts;
  if (clock_gettime(clockid_t_val(v_cl), &ts))
    uerror("clock_gettime", Nothing);
  return caml_copy_double((double) ts.tv_sec + (double) ts.tv_nsec / 1e9);
}

CAMLprim value unix_clock_settime(value v_cl, value v_t)
{
  double t = Double_val(v_t);
  struct timespec ts;
  ts.tv_sec = t;
  ts.tv_nsec = (t - ts.tv_sec) * 1e9;
  if (clock_settime(clockid_t_val(v_cl), &ts))
    uerror("clock_settime", Nothing);
  return Val_unit;
}

CAMLprim value unix_clock_getres(value v_cl)
{
  struct timespec ts;
  if (clock_getres(clockid_t_val(v_cl), &ts))
    uerror("clock_getres", Nothing);
  return caml_copy_double((double) ts.tv_sec + (double) ts.tv_nsec / 1e9);
}

/* Unfortunately, it is currently not possible to
   extract the POSIX thread id given the OCaml-thread id due to lack of
   support for this feature in the OCaml-runtime.  The below function
   clearly does not do what is intended in the general case, but will
   probably usually do the right thing. */
static inline pthread_t pthread_t_val(value __unused v_tid)
{
  return pthread_self();
}

CAMLprim value unix_pthread_getcpuclockid(value v_tid)
{
  clockid_t c;
  if (pthread_getcpuclockid(pthread_t_val(v_tid), &c))
    uerror("pthread_getcpuclockid", Nothing);
  return caml_copy_nativeint(c);
}
#else
#warning "POSIX MON not present; clock functions undefined"
#endif


/* Resource limits */

static inline int resource_val(value v_resource)
{
  int resource;
  switch (Int_val(v_resource)) {
    case 0 : resource = RLIMIT_CORE; break;
    case 1 : resource = RLIMIT_CPU; break;
    case 2 : resource = RLIMIT_DATA; break;
    case 3 : resource = RLIMIT_FSIZE; break;
    case 4 : resource = RLIMIT_NOFILE; break;
    case 5 : resource = RLIMIT_STACK; break;
    case 6 : resource = RLIMIT_AS; break;
    default :
      /* impossible */
      caml_failwith("resource_val: unknown sum tag");
      break;
  }
  return resource;
}

static inline rlim_t rlim_t_val(value v_lim)
{
  return
    Is_block(v_lim)
    ? (rlim_t) Int64_val(Field(v_lim, 0))
    : RLIM_INFINITY;
}

static value Val_rlim_t(rlim_t lim)
{
  value v_rl;
  if (lim == RLIM_INFINITY) v_rl = Val_int(0);
  else {
    value v_arg = caml_copy_int64(lim);
    Begin_roots1(v_arg);
      v_rl = caml_alloc_small(1, 0);
    End_roots();
    Field(v_rl, 0) = v_arg;
  }
  return v_rl;
}

CAMLprim value unix_getrlimit(value v_resource)
{
  CAMLparam0();
  CAMLlocal2(v_cur, v_max);
    int resource = resource_val(v_resource);
    value v_limits;
    struct rlimit rl;
    if (getrlimit(resource, &rl)) uerror("getrlimit", Nothing);
    v_cur = Val_rlim_t(rl.rlim_cur);
    v_max = Val_rlim_t(rl.rlim_max);
    v_limits = caml_alloc_small(2, 0);
    Field(v_limits, 0) = v_cur;
    Field(v_limits, 1) = v_max;
  CAMLreturn(v_limits);
}

CAMLprim value unix_setrlimit(value v_resource, value v_limits)
{
  struct rlimit rl;
  int resource = resource_val(v_resource);
  value v_cur = Field(v_limits, 0), v_max = Field(v_limits, 1);
  rl.rlim_cur = rlim_t_val(v_cur);
  rl.rlim_max = rlim_t_val(v_max);
  if (setrlimit(resource, &rl)) uerror("setrlimit", Nothing);
  return Val_unit;
}


/* Resource usage */

CAMLprim value unix_getrusage(value v_who)
{
  CAMLparam0();
  CAMLlocal1(v_usage);
    int who = (Int_val(v_who) == 0) ? RUSAGE_SELF : RUSAGE_CHILDREN;
    struct rusage ru;
    if (getrusage(who, &ru)) uerror("getrusage", Nothing);
    v_usage = caml_alloc(16, 0);
    Store_field(v_usage, 0,
                caml_copy_double((double) ru.ru_utime.tv_sec +
                                 (double) ru.ru_utime.tv_usec / 1e6));
    Store_field(v_usage, 1,
                caml_copy_double((double) ru.ru_stime.tv_sec +
                                 (double) ru.ru_stime.tv_usec / 1e6));
    Store_field(v_usage, 2, caml_copy_int64(ru.ru_maxrss));
    Store_field(v_usage, 3, caml_copy_int64(ru.ru_ixrss));
    Store_field(v_usage, 4, caml_copy_int64(ru.ru_idrss));
    Store_field(v_usage, 5, caml_copy_int64(ru.ru_isrss));
    Store_field(v_usage, 6, caml_copy_int64(ru.ru_minflt));
    Store_field(v_usage, 7, caml_copy_int64(ru.ru_majflt));
    Store_field(v_usage, 8, caml_copy_int64(ru.ru_nswap));
    Store_field(v_usage, 9, caml_copy_int64(ru.ru_inblock));
    Store_field(v_usage, 10, caml_copy_int64(ru.ru_oublock));
    Store_field(v_usage, 11, caml_copy_int64(ru.ru_msgsnd));
    Store_field(v_usage, 12, caml_copy_int64(ru.ru_msgrcv));
    Store_field(v_usage, 13, caml_copy_int64(ru.ru_nsignals));
    Store_field(v_usage, 14, caml_copy_int64(ru.ru_nvcsw));
    Store_field(v_usage, 15, caml_copy_int64(ru.ru_nivcsw));
  CAMLreturn(v_usage);
}


/* System configuration */

CAMLprim value unix_sysconf(value v_name)
{
  int name;
  long ret;
  switch (Int_val(v_name)) {
    case 0 : name = _SC_ARG_MAX; break;
    case 1 : name = _SC_CHILD_MAX; break;
    case 2 : name = _SC_HOST_NAME_MAX; break;
    case 3 : name = _SC_LOGIN_NAME_MAX; break;
    case 4 : name = _SC_OPEN_MAX; break;
    case 5 : name = _SC_PAGESIZE; break;
    case 6 : name = _SC_RE_DUP_MAX; break;
    case 7 : name = _SC_STREAM_MAX; break;
    case 8 : name = _SC_SYMLOOP_MAX; break;
    case 9 : name = _SC_TTY_NAME_MAX; break;
    case 10 : name = _SC_TZNAME_MAX; break;
    case 11 : name = _SC_VERSION; break;
    
#if defined(__linux__)
    case 12 : name = _SC_PHYS_PAGES; break;
    case 13 : name = _SC_AVPHYS_PAGES; break;
#endif
    case 14 : name = _SC_IOV_MAX; break;
    default :
      /* impossible */
      caml_failwith("unix_sysconf: unknown sum tag");
      break;
  }
  ret = sysconf(name);
  if (ret == -1) uerror("sysconf", Nothing);
  return (caml_copy_int64(ret));
}


/* POSIX thread functions */

#define Mutex_val(v) (* ((pthread_mutex_t **) Data_custom_val(v)))
#define Condition_val(v) (* ((pthread_cond_t **) Data_custom_val(v)))

static void caml_pthread_check(int retcode, char *msg)
{
  char *err;
  size_t errlen, msglen;
  value str;

  if (retcode == 0) return;
  
  err = strerror(retcode);
  msglen = strlen(msg);
  errlen = strlen(err);
  str = caml_alloc_string(msglen + 2 + errlen);
  memmove(&Byte(str, 0), msg, msglen);
  memmove(&Byte(str, msglen), ": ", 2);
  memmove(&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
}

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
CAMLprim value unix_mutex_timedlock(value v_mtx, value v_timeo)
{
  int ret;
  pthread_mutex_t *mtx = Mutex_val(v_mtx);
  ret = pthread_mutex_trylock(mtx);
  if (ret == EBUSY) {
    double timeo = Double_val(v_timeo);
    struct timespec ts;
    ts.tv_sec = timeo;
    ts.tv_nsec = (timeo - ts.tv_sec) * 1e9;
    Begin_roots1(v_mtx);
    caml_enter_blocking_section();
      ret = pthread_mutex_timedlock(mtx, &ts);
    caml_leave_blocking_section();
    End_roots();
    if (ret == ETIMEDOUT) return Val_false;
  }
  caml_pthread_check(ret, "Mutex.timedlock");
  return Val_true;
}
#else
#warning "POSIX TMO not present; unix_mutex_timedlock undefined"
#endif

CAMLprim value unix_condition_timedwait(value v_cnd, value v_mtx, value v_timeo)
{
  CAMLparam2(v_cnd, v_mtx);
    int ret;
    pthread_cond_t *cnd = Condition_val(v_cnd);
    pthread_mutex_t *mtx = Mutex_val(v_mtx);
    double timeo = Double_val(v_timeo);
    struct timespec ts;
    ts.tv_sec = timeo;
    ts.tv_nsec = (timeo - ts.tv_sec) * 1e9;
    caml_enter_blocking_section();
      ret = pthread_cond_timedwait(cnd, mtx, &ts);
    caml_leave_blocking_section();
    if (ret == ETIMEDOUT) CAMLreturn(Val_false);
    caml_pthread_check(ret, "Condition.timedwait");
  CAMLreturn(Val_true);
}

static void caml_mutex_finalize(value v_mtx)
{
  pthread_mutex_t *mtx = Mutex_val(v_mtx);
  pthread_mutex_destroy(mtx);
  caml_stat_free(mtx);
}

static int caml_mutex_condition_compare(value v_mtx1, value v_mtx2)
{
  pthread_mutex_t *mtx1 = Mutex_val(v_mtx1);
  pthread_mutex_t *mtx2 = Mutex_val(v_mtx2);
  return mtx1 == mtx2 ? 0 : mtx1 < mtx2 ? -1 : 1;
}

static struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#if defined(_XOPEN_UNIX) && (_XOPEN_UNIX > 0)
CAMLprim value unix_create_error_checking_mutex(value __unused v_unit)
{
  pthread_mutex_t *mtx;
  pthread_mutexattr_t attrs;
  value v_res;
  pthread_mutexattr_init(&attrs);
  pthread_mutexattr_settype(&attrs, PTHREAD_MUTEX_ERRORCHECK);
  mtx = caml_stat_alloc(sizeof(pthread_mutex_t));
  caml_pthread_check(
    pthread_mutex_init(mtx, &attrs), "Mutex.create_error_checking");
  pthread_mutexattr_destroy(&attrs);
  v_res =
    caml_alloc_custom(&caml_mutex_ops, sizeof(pthread_mutex_t *), 1, 1000);
  Mutex_val(v_res) = mtx;
  return v_res;
}
#else
#warn "XOPEN_UNIX not defined or = 0; unix_create_error_checking_mutex not available"
#endif

/* Pathname resolution */

/* Seems like a sane approach to getting a reasonable bound for the
   maximum path length */
#ifdef PATH_MAX
#define JANE_PATH_MAX ((PATH_MAX <= 0 || PATH_MAX > 65536) ? 65536 : PATH_MAX)
#else
#define JANE_PATH_MAX (65536)
#endif

#ifdef __GLIBC__
CAMLprim value unix_realpath(value v_path)
{
  char *path = String_val(v_path);
  char *res = realpath(path, NULL);
  if (res == NULL) uerror("realpath", v_path);
  else {
    value v_res = caml_copy_string(res);
    free(res);
    return v_res;
  }
}
#else
CAMLprim value unix_realpath(value v_path)
{
  char *path = String_val(v_path);
  /* [realpath] is inherently broken without GNU-extension, and this
     seems like a reasonable thing to do if we do not build against
     GLIBC. */
  char resolved_path[JANE_PATH_MAX];
  if (realpath(path, resolved_path) == NULL) uerror("realpath", v_path);
  return caml_copy_string(resolved_path);
}
#endif


/* Temporary file and directory creation */

static inline void init_mktemp(char *loc, char *buf, value v_path)
{
  int i, len = caml_string_length(v_path);
  if (len > JANE_PATH_MAX - 7) caml_invalid_argument(loc);
  memcpy(buf, String_val(v_path), len);
  for (i = len; i < len + 6; ++i) buf[i] = 'X';
  buf[len + 6] = '\0';
}

CAMLprim value unix_mkstemp(value v_path)
{
  CAMLparam1(v_path);
  CAMLlocal1(v_res_path);
  char *loc = "mkstemp";
  char buf[JANE_PATH_MAX];
  int fd;
  value v_res;
  init_mktemp(loc, buf, v_path);
  caml_enter_blocking_section();
    fd = mkstemp(buf);
  caml_leave_blocking_section();
  if (fd == -1) uerror(loc, v_path);
  v_res_path = caml_copy_string(buf);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = v_res_path;
  Field(v_res, 1) = Val_int(fd);
  CAMLreturn(v_res);
}

CAMLprim value unix_mkdtemp(value v_path)
{
  CAMLparam1(v_path);
  char *loc = "mkdtemp";
  char *path;
  char buf[JANE_PATH_MAX];
  init_mktemp(loc, buf, v_path);
  caml_enter_blocking_section();
    path = mkdtemp(buf);
  caml_leave_blocking_section();
  if (path == NULL) uerror(loc, v_path);
  CAMLreturn(caml_copy_string(buf));
}


/* Signal handling */

CAMLprim value unix_abort(value v_unit)
{
  abort();
  return v_unit;
}


/* User id, group id management */

CAMLprim value unix_initgroups(value v_user, value v_group)
{
  int ret, user_len = caml_string_length(v_user) + 1;
  char *c_user = caml_stat_alloc(user_len);
  gid_t group = Long_val(v_group);
  memcpy(c_user, String_val(v_user), user_len);
  caml_enter_blocking_section();
    ret = initgroups(c_user, group);
    caml_stat_free(c_user);
  caml_leave_blocking_section();
  if (ret == -1) uerror("initgroups", Nothing);
  return Val_unit;
}


/* Globbing and shell string expansion */

CAMLprim value unix_fnmatch_make_flags(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= FNM_NOESCAPE; break;
      case 1 : flags |= FNM_PATHNAME; break;
      case 2 : flags |= FNM_PERIOD; break;
      case 3 : flags |= FNM_FILE_NAME; break;
      case 4 : flags |= FNM_LEADING_DIR; break;
      default : flags |= FNM_CASEFOLD; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value unix_fnmatch(value v_flags, value v_glob, value v_str)
{
  int flags = Int32_val(v_flags);
  char *glob = String_val(v_glob);
  char *str = String_val(v_str);
  int ret = fnmatch(glob, str, flags);
  switch (ret) {
    case 0 : return Val_true;
    case FNM_NOMATCH : return Val_false;
    default : caml_failwith("fnmatch");
  }
}

CAMLprim value unix_wordexp_make_flags(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= WRDE_NOCMD; break;
      case 1 : flags |= WRDE_SHOWERR; break;
      default : flags |= WRDE_UNDEF; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value unix_wordexp(value v_flags, value v_str)
{
  CAMLparam0();
  CAMLlocal1(v_res);
  int flags = Int32_val(v_flags);
  unsigned int i, len = caml_string_length(v_str) + 1;
  int ret;
  char *buf = caml_stat_alloc(len);
  char **w;
  wordexp_t p;
  memcpy(buf, String_val(v_str), len);
  caml_enter_blocking_section();
    ret = wordexp(buf, &p, flags);
    caml_stat_free(buf);
  caml_leave_blocking_section();
  switch (ret) {
    case 0 :
      v_res = caml_alloc(p.we_wordc, 0);
      w = p.we_wordv;
      for (i = 0; i < p.we_wordc; ++i)
        Store_field(v_res, i, caml_copy_string(w[i]));
      wordfree(&p);
      CAMLreturn(v_res);
    case WRDE_BADCHAR : caml_failwith("wordexp: bad char");
    case WRDE_BADVAL : caml_failwith("wordexp: undefined shell variable");
    case WRDE_CMDSUB : caml_failwith("wordexp: unwanted command substitution");
    case WRDE_NOSPACE : caml_failwith("wordexp: out of memory");
    case WRDE_SYNTAX : caml_failwith("wordexp: syntax error");
    default : caml_failwith("wordexp: impossible");
  }
}


/* Additional IP functionality */
/* rdouglass: no luck in my very brief attempt with getting this to work on mac 10.6.
 * dropping it for everything but linux for this release to get it out the door */
#if defined(__linux__)
#include <net/if.h>

CAMLprim value unix_if_indextoname(value v_index)
{
  char name[IF_NAMESIZE];
  if (if_indextoname((unsigned int) Int_val(v_index), name) == NULL)
    uerror("if_indextoname", Nothing);
  else return caml_copy_string(name);
}

#include "socketaddr.h"

#define MK_MCAST(NAME, OP) \
  CAMLprim value unix_mcast_##NAME(value v_ifname_opt, value v_fd, value v_sa) \
  { \
    int ret, fd = Int_val(v_fd); \
    union sock_addr_union sau; \
    struct sockaddr *sa = &sau.s_gen; \
    socklen_param_type sa_len; \
    get_sockaddr(v_sa, &sau, &sa_len); \
    switch (sa->sa_family) { \
      case AF_INET: { \
        struct ip_mreq mreq; \
        struct ifreq ifreq; \
        memcpy(&mreq.imr_multiaddr, \
               &((struct sockaddr_in *) sa)->sin_addr, \
               sizeof(struct in_addr)); \
        if (v_ifname_opt != Val_int(0)) { \
          value v_ifname = Field(v_ifname_opt, 0); \
          char *ifname = String_val(v_ifname); \
          int ifname_len = caml_string_length(v_ifname) + 1; \
          if (ifname_len > IFNAMSIZ) \
            caml_failwith("mcast_" STR(NAME) ": ifname string too long"); \
          strncpy(ifreq.ifr_name, ifname, IFNAMSIZ); \
          if (ioctl(fd, SIOCGIFADDR, &ifreq) < 0) \
            uerror("mcast_" STR(NAME), Nothing); \
          memcpy(&mreq.imr_interface, \
                 &((struct sockaddr_in *) &ifreq.ifr_addr)->sin_addr, \
                 sizeof(struct in_addr)); \
        } else mreq.imr_interface.s_addr = htonl(INADDR_ANY); \
        ret = \
          setsockopt(fd, IPPROTO_IP, IP_##OP##_MEMBERSHIP, \
                     &mreq, sizeof(mreq)); \
        if (ret == -1) uerror("mcast_" STR(NAME), Nothing); \
        return Val_unit; \
      } \
      default : \
        errno = EPROTONOSUPPORT; \
        uerror("mcast_" STR(NAME), Nothing); \
    } \
  }

MK_MCAST(join, ADD)
MK_MCAST(leave, DROP)
#else
#warning "not on linux, multicast stuff not included"
#endif
