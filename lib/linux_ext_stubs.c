#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE

#include <string.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <time.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/epoll.h>

#include "ocaml_utils.h"
#include "unix_utils.h"

#define DIR_Val(v) *((DIR **) &Field(v, 0))
#define UNIX_BUFFER_SIZE 16384

#include <sys/sendfile.h>

CAMLprim value
linux_sendfile_stub(value v_sock, value v_fd, value v_pos, value v_len)
{
  loff_t pos = Int_val(v_pos);
  int ret;

  caml_enter_blocking_section();
    ret = sendfile(Int_val(v_sock), Int_val(v_fd), &pos, Int_val(v_len));
  caml_leave_blocking_section();

  if (ret == -1) uerror("sendfile", Nothing);

  return Val_int(ret);
}

/**/

enum option_type {
  TYPE_BOOL = 0,
  TYPE_INT = 1,
  TYPE_LINGER = 2,
  TYPE_TIMEVAL = 3,
  TYPE_UNIX_ERROR = 4
};

extern value unix_getsockopt_aux(
  char *name,
  enum option_type ty, int level, int option,
  value v_socket);
extern value unix_setsockopt_aux(
  char *name,
  enum option_type ty, int level, int option,
  value v_socket, value v_status);


static int linux_tcpopt_bool[] = { TCP_CORK, TCP_NODELAY };

CAMLprim value linux_gettcpopt_bool_stub(value v_socket, value v_option)
{
  int option = linux_tcpopt_bool[Int_val(v_option)];
  return
    unix_getsockopt_aux("getsockopt", TYPE_BOOL, SOL_TCP, option, v_socket);
}

CAMLprim value linux_settcpopt_bool_stub(
  value v_socket, value v_option, value v_status)
{
  int option = linux_tcpopt_bool[Int_val(v_option)];
  return
    unix_setsockopt_aux(
      "setsockopt", TYPE_BOOL, SOL_TCP, option, v_socket, v_status);
}

/**/

static int nonblocking_no_sigpipe_flag = MSG_DONTWAIT | MSG_NOSIGNAL;

CAMLprim value linux_send_nonblocking_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_buf)
{
  char *buf = String_val(v_buf) + Int_val(v_pos);
  value v_res;
  int ret =
    send(Int_val(v_fd), buf, Int_val(v_len), nonblocking_no_sigpipe_flag);
  if (ret == -1) {
    if (errno != EAGAIN && errno != EWOULDBLOCK)
      uerror("send_nonblocking_no_sigpipe", Nothing);
    return Val_int(0);
  }
  v_res = caml_alloc_small(1, 0);
  Field(v_res, 0) = Val_int(ret);
  return v_res;
}

CAMLprim value linux_send_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_buf)
{
  char *buf = String_val(v_buf) + Int_val(v_pos);
  int ret = send(Int_val(v_fd), buf, Int_val(v_len), MSG_NOSIGNAL);
  if (ret == -1) uerror("send_no_sigpipe", Nothing);
  return Val_int(ret);
}

CAMLprim value linux_sendmsg_nonblocking_no_sigpipe_stub(
  value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  int ret;
  struct iovec *iovecs = caml_stat_alloc(sizeof(struct iovec) * count);
  struct msghdr msghdr = { NULL, 0, NULL, 0, NULL, 0, 0 };
  msghdr.msg_iov = iovecs;
  msghdr.msg_iovlen = count;
  for (--count; count >= 0; --count) {
    struct iovec *iovec = &iovecs[count];
    value v_iovec = Field(v_iovecs, count);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    value v_iov_len = Field(v_iovec, 2);
    iovec->iov_base = String_val(v_iov_base) + Int_val(v_iov_pos);
    iovec->iov_len = Int_val(v_iov_len);
  }
  ret = sendmsg(Int_val(v_fd), &msghdr, nonblocking_no_sigpipe_flag);
  caml_stat_free(iovecs);
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("sendmsg_nonblocking_no_sigpipe", Nothing);
  return Val_int(ret);
}

/**/

/* Send a file descriptor to another process via a socket.  In theory
   this could be in Unix_ext, but it has only been tested on Linux. */
CAMLprim value linux_send_fd_stub(value v_socket, value v_fd_to_send)
{
  int ret;
  int socket = Int_val(v_socket);
  int fd_to_send = Int_val(v_fd_to_send);
  struct msghdr msg = { NULL, 0, NULL, 0, NULL, 0, 0 };
  struct iovec iov[1];
  char *buf = "z";
  char control[CMSG_SPACE(sizeof(int))];
  struct cmsghdr *cmptr;

  msg.msg_control = control;
  msg.msg_controllen = sizeof(control);

  cmptr = CMSG_FIRSTHDR(&msg);
  cmptr->cmsg_level = SOL_SOCKET;
  cmptr->cmsg_type = SCM_RIGHTS;
  cmptr->cmsg_len = CMSG_LEN(sizeof(int));
  *((int *) CMSG_DATA(cmptr)) = fd_to_send;

  iov[0].iov_base = buf;
  iov[0].iov_len = 1;
  msg.msg_iov = iov;
  msg.msg_iovlen = 1;

  caml_enter_blocking_section();
    ret = sendmsg(socket, &msg, 0);
  caml_leave_blocking_section();

  if (ret < 0) uerror("send_fd", Nothing);

  return Val_unit;
}

CAMLprim value linux_recv_fd_stub(value v_socket)
{
  int ret;
  int socket = Int_val(v_socket);
  struct msghdr msg = { NULL, 0, NULL, 0, NULL, 0, 0 };
  struct iovec iov[1];
  char buf[1];
  char control[CMSG_SPACE(sizeof(int))];
  struct cmsghdr *cmptr;

  msg.msg_control = control;
  msg.msg_controllen = sizeof(control);

  iov[0].iov_base = buf;
  iov[0].iov_len = 1;
  msg.msg_iov = iov;
  msg.msg_iovlen = 1;

  caml_enter_blocking_section();
    ret = recvmsg(socket, &msg, 0);
  caml_leave_blocking_section();

  if (ret < 0) uerror("recv_fd", Nothing);

  cmptr = CMSG_FIRSTHDR(&msg);

  if (cmptr == NULL)
    caml_failwith("recv_fd: cmptr is null, no control message received");

  if (cmptr->cmsg_len != CMSG_LEN(sizeof(int)))
    caml_failwith("recv_fd: the message length is not the same size as an int");

  if (cmptr->cmsg_level != SOL_SOCKET)
    caml_failwith(
      "recv_fd: cmsg_level is not SOL_SOCKET, see man page for recvmsg");

  if (cmptr->cmsg_type != SCM_RIGHTS)
    caml_failwith(
      "recv_fd: cmsg_type is not SCM_RIGHTS, see man page for recvmsg");

  return Val_int(*((int *) CMSG_DATA(cmptr)));
}

/**/
#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > -1)
CAMLprim value linux_clock_process_cputime_id_stub(value __unused v_unit)
{
  return caml_copy_nativeint(CLOCK_PROCESS_CPUTIME_ID);
}

CAMLprim value linux_clock_thread_cputime_id_stub(value __unused v_unit)
{
  return caml_copy_nativeint(CLOCK_THREAD_CPUTIME_ID);
}
#else
#warning "POSIX MON not present; clock functions undefined"
#endif

/**/

CAMLprim value linux_get_terminal_size_stub(value __unused v_unit)
{
  int fd;
  struct winsize ws;
  int ret;
  value v_res;

  caml_enter_blocking_section();

  fd = open("/dev/tty", O_RDWR);

  if (fd == -1) {
    caml_leave_blocking_section();
    uerror("get_terminal_size__open", Nothing);
  }

  ret = ioctl(fd, TIOCGWINSZ, &ws);

  if (ret == -1) {
    ret = close(fd);
    while (ret == -1) {
      if (errno != EINTR) {
        caml_leave_blocking_section();
        uerror("get_terminal_size__ioctl_close", Nothing);
      }
    }
    caml_leave_blocking_section();
    uerror("get_terminal_size__ioctl", Nothing);
  }

  ret = close(fd);
  while (ret == -1) {
    if (errno != EINTR) {
      caml_leave_blocking_section();
      uerror("get_terminal_size__close", Nothing);
    }
  }

  caml_leave_blocking_section();

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_int(ws.ws_row);
  Field(v_res, 1) = Val_int(ws.ws_col);

  return v_res;
}

/**/

CAMLprim value linux_pr_set_pdeathsig_stub(value v_sig)
{
  int sig = caml_convert_signal_number(Int_val(v_sig));
  if (prctl(PR_SET_PDEATHSIG, sig) == -1) uerror("pr_set_pdeathsig", Nothing);
  return Val_unit;
}

CAMLprim value linux_pr_get_pdeathsig_stub(value __unused v_unit)
{
  int sig;
  if (prctl(PR_GET_PDEATHSIG, &sig) == -1) uerror("pr_get_pdeathsig", Nothing);
  return Val_int(sig);
}

/* Epoll */

/* NOTE: we store file descriptors as OCaml-value in event.data.fd! */

CAMLprim value linux_epoll_make_flags_stub(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= EPOLLIN; break;
      case 1 : flags |= EPOLLOUT; break;
      case 2 : flags |= EPOLLPRI; break;
      case 3 : flags |= EPOLLERR; break;
      case 4 : flags |= EPOLLHUP; break;
      case 5 : flags |= EPOLLET; break;
      default : flags |= EPOLLONESHOT; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value linux_epoll_get_flags_stub(value v_flags)
{
  int flags = Int32_val(v_flags);
  int n =
    ((flags & EPOLLIN) != 0) +
    ((flags & EPOLLOUT) != 0) +
    ((flags & EPOLLPRI) != 0) +
    ((flags & EPOLLERR) != 0) +
    ((flags & EPOLLHUP) != 0) +
    ((flags & EPOLLET) != 0) +
    ((flags & EPOLLONESHOT) != 0);
  value v_res = caml_alloc_small(n, 0);
  if (flags & EPOLLONESHOT) Field(v_res, --n) = Val_int(6);
  if (flags & EPOLLET) Field(v_res, --n) = Val_int(5);
  if (flags & EPOLLHUP) Field(v_res, --n) = Val_int(4);
  if (flags & EPOLLERR) Field(v_res, --n) = Val_int(3);
  if (flags & EPOLLPRI) Field(v_res, --n) = Val_int(2);
  if (flags & EPOLLOUT) Field(v_res, --n) = Val_int(1);
  if (flags & EPOLLIN) Field(v_res, --n) = Val_int(0);
  return v_res;
}

#define EPOLL_HAS(FLAG) \
  CAMLprim value linux_epoll_has_##FLAG##_stub(value v_flags) \
  { return Val_bool(Int32_val(v_flags) & FLAG); }

EPOLL_HAS(EPOLLIN)
EPOLL_HAS(EPOLLOUT)
EPOLL_HAS(EPOLLPRI)
EPOLL_HAS(EPOLLERR)
EPOLL_HAS(EPOLLHUP)
EPOLL_HAS(EPOLLET)
EPOLL_HAS(EPOLLONESHOT)

CAMLprim value linux_epoll_create_stub(value v_size)
{
  int fd = epoll_create(Int_val(v_size));
  if (fd == -1) uerror("epoll_create", Nothing);
  return Val_int(fd);
}

static inline value epoll_op(value v_epfd, value v_fd, value v_flags, int op)
{
  int ret;
  struct epoll_event ev;
  ev.events = Int32_val(v_flags);
  ev.data.fd = v_fd;
  ret = epoll_ctl(Int_val(v_epfd), op, Int_val(v_fd), &ev);
  if (ret == -1) uerror("epoll_ctl", Nothing);
  return Val_unit;
}

CAMLprim value linux_epoll_add_stub(value v_epfd, value v_fd, value v_flags)
{
  return epoll_op(v_epfd, v_fd, v_flags, EPOLL_CTL_ADD);
}

CAMLprim value linux_epoll_modify_stub(value v_epfd, value v_fd, value v_flags)
{
  return epoll_op(v_epfd, v_fd, v_flags, EPOLL_CTL_MOD);
}

CAMLprim value linux_epoll_del_stub(value v_epfd, value v_fd)
{
  if (epoll_ctl(Int_val(v_epfd), EPOLL_CTL_DEL, Int_val(v_fd), NULL) == -1)
    uerror("epoll_ctl", Nothing);
  return Val_unit;
}

CAMLprim value
linux_epoll_wait_stub(value v_epfd, value v_maxevents, value v_timeout)
{
  CAMLparam0();
  CAMLlocal2(v_res, v_flags);
  int maxevents = Int_val(v_maxevents);
  struct epoll_event *evs;
  int i;

  if (maxevents <= 0) caml_invalid_argument("epoll_wait: maxevents <= 0");

  evs = caml_stat_alloc(maxevents);

  caml_enter_blocking_section();
    i = epoll_wait(Int_val(v_epfd), evs, maxevents, Int_val(v_timeout));
  caml_leave_blocking_section();

  if (i == -1) {
    caml_stat_free(evs);
    uerror("epoll_wait", Nothing);
  }

  v_res = caml_alloc(i, 0);

  while (--i >= 0) {
    value v_ev;
    struct epoll_event *ev = &evs[i];
    v_flags = caml_copy_int32(ev->events);
    v_ev = caml_alloc_small(2, 0);
    Field(v_ev, 0) = ev->data.fd;
    Field(v_ev, 1) = v_flags;
    Store_field(v_res, i, v_ev);
  }

  caml_stat_free(evs);

  CAMLreturn(v_res);
}


/* Splicing - zero-copies between kernel buffers */

/* FIXME: not yet available with Fedora 5; the kernel supports this
   but not GLIBC.  Centos 5 seems to work fine with these functions.
   Uncomment this region once the switch to Centos 5 is done.  STILL NEEDS
   TESTING!!!

CAMLprim value linux_splice_make_flags_stub(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= SPLICE_F_MOVE; break;
      case 1 : flags |= SPLICE_F_NONBLOCK; break;
      case 2 : flags |= SPLICE_F_MORE; break;
      default : flags |= SPLICE_F_GIFT; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value linux_splice_stub(
  value v_assume_nonblocking,
  value v_fd_in, value v_off_in,
  value v_fd_out, value v_off_out,
  value v_len, value v_flags)
{
  int assume_nonblocking = Bool_val(v_assume_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  off_t off_in = Long_val(v_off_in);
  off_t off_out = (Int_val(v_off_out));
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;
  value v_res;

  if (assume_nonblocking)
    ret = splice(fd_in, &off_in, fd_out, &off_out, len, flags);
  else {
    caml_enter_blocking_section();
      ret = splice(fd_in, &off_in, fd_out, &off_out, len, flags);
    caml_leave_blocking_section();
  }

  if (ret == -1) uerror("splice", Nothing);

  v_res = caml_alloc_small(3, 0);
  Field(v_res, 0) = Val_long(ret);
  Field(v_res, 1) = Val_long(off_in);
  Field(v_res, 2) = Val_long(off_out);

  return v_res;
}

CAMLprim value linux_splice_stub_bc(value *argv, int __unused argn)
{
  return linux_splice_stub(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value linux_tee_stub(
  value v_assume_nonblocking,
  value v_fd_in, value v_fd_out,
  value v_len, value v_flags)
{
  int assume_nonblocking = Bool_val(v_assume_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_nonblocking)
    ret = tee(fd_in, fd_out, len, flags);
  else {
    caml_enter_blocking_section();
      ret = tee(fd_in, fd_out, len, flags);
    caml_leave_blocking_section();
  }

  if (ret == -1) uerror("tee", Nothing);

  return Val_long(ret);
}

CAMLprim value linux_vmsplice_stub(
  value v_assume_nonblocking,
  value v_fd, value v_iovecs, value v_count,
  value v_flags)
{
  int assume_nonblocking = Bool_val(v_assume_nonblocking);
  int fd = Int_val(v_fd);
  int count = Int_val(v_count);
  long total_len = 0;
  struct iovec *iovecs = copy_iovecs(&total_len, v_iovecs, count);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_nonblocking && total_len < THREAD_IO_CUTOFF)
    ret = vmsplice(fd, iovecs, count, flags);
  else {
    Begin_roots1(v_iovecs);
    caml_enter_blocking_section();
      ret = vmsplice(fd, iovecs, count, flags);
    caml_leave_blocking_section();
    End_roots();
  }

  if (ret == -1) uerror("vmsplice", Nothing);

  return Val_long(ret);
}

*/
