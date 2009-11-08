#include <sys/epoll.h>

#include "ocaml_utils.h"

/* Epoll */

/* NOTE: we store file descriptors as OCaml-values in event.data.fd! */

#define CAML_EPOLLIN 0
#define CAML_EPOLLOUT 1
#define CAML_EPOLLPRI 2
#define CAML_EPOLLERR 3
#define CAML_EPOLLHUP 4
#define CAML_EPOLLET 5
#define CAML_EPOLLONESHOT 6

CAMLprim value linux_epoll_make_flags_stub(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case CAML_EPOLLIN : flags |= EPOLLIN; break;
      case CAML_EPOLLOUT : flags |= EPOLLOUT; break;
      case CAML_EPOLLPRI : flags |= EPOLLPRI; break;
      case CAML_EPOLLERR : flags |= EPOLLERR; break;
      case CAML_EPOLLHUP : flags |= EPOLLHUP; break;
      case CAML_EPOLLET : flags |= EPOLLET; break;
      case CAML_EPOLLONESHOT : flags |= EPOLLONESHOT; break;
      default :
        caml_failwith("linux_epoll_make_flags_stub: unknown sum tag");
        break;
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
  value v_res;
  if (n == 0) return Atom(0);
  v_res = caml_alloc_small(n, 0);
  if (flags & EPOLLONESHOT) Field(v_res, --n) = Val_int(CAML_EPOLLONESHOT);
  if (flags & EPOLLET) Field(v_res, --n) = Val_int(CAML_EPOLLET);
  if (flags & EPOLLHUP) Field(v_res, --n) = Val_int(CAML_EPOLLHUP);
  if (flags & EPOLLERR) Field(v_res, --n) = Val_int(CAML_EPOLLERR);
  if (flags & EPOLLPRI) Field(v_res, --n) = Val_int(CAML_EPOLLPRI);
  if (flags & EPOLLOUT) Field(v_res, --n) = Val_int(CAML_EPOLLOUT);
  if (flags & EPOLLIN) Field(v_res, --n) = Val_int(CAML_EPOLLIN);
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
  /* ev.data.fd is only used by us internally in the C-bindings.
     Hence no need to convert it to a C-int first.
     See comment at beginning of epoll C-bindings. */
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
    /* ev->data.fd was already stored by us as an OCaml-value.
       See comment at beginning of epoll C-bindings. */
    Field(v_ev, 0) = ev->data.fd;
    Field(v_ev, 1) = v_flags;
    Store_field(v_res, i, v_ev);
  }

  caml_stat_free(evs);

  CAMLreturn(v_res);
}


/* Splicing - zero-copies between kernel buffers */


/*

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
  value v_assume_fd_is_nonblocking,
  value v_fd_in, value v_off_in,
  value v_fd_out, value v_off_out,
  value v_len, value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  off_t off_in = Long_val(v_off_in);
  off_t *off_in_p = (off_in < 0) ? NULL : &off_in;
  off_t off_out = Long_val(v_off_out);
  off_t *off_out_p = (off_out < 0) ? NULL : &off_out;
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;
  value v_res;

  if (assume_fd_is_nonblocking)
    ret = splice(fd_in, off_in_p, fd_out, off_out_p, len, flags);
  else {
    caml_enter_blocking_section();
      ret = splice(fd_in, off_in_p, fd_out, off_out_p, len, flags);
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
  value v_assume_fd_is_nonblocking,
  value v_fd_in, value v_fd_out,
  value v_len, value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_fd_is_nonblocking)
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
  value v_assume_fd_is_nonblocking,
  value v_fd, value v_iovecs, value v_count,
  value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd = Int_val(v_fd);
  int count = Int_val(v_count);
  long total_len = 0;
  struct iovec *iovecs = copy_iovecs(&total_len, v_iovecs, count);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_fd_is_nonblocking && total_len < THREAD_IO_CUTOFF)
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
