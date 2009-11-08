

#include <stdlib.h>
#include <time.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include "ocaml_utils.h"

/* The code for crc_octets and the constants involded is taken from 
   RFC2440 http://sunsite.icm.edu.pl/gnupg/rfc2440-6.html
 */
#define CRC24_INIT 0xb704ceL
#define CRC24_POLY 0x1864cfbL

typedef long crc24;
crc24 crc_octets(unsigned char *octets, size_t len) {
  crc24 crc = CRC24_INIT;
  int i;

  while (len--) {
    crc ^= (*octets++) << 16;
    for (i = 0; i < 8; i++) {
      crc <<= 1;
      if (crc & 0x1000000)
	crc ^= CRC24_POLY;
    }
  }
  return crc & 0xffffffL;
}

value caml_crc_octets(value v_str)
{
  unsigned char *octets = (unsigned char *) String_val(v_str);
  size_t len = caml_string_length(v_str);
  long crc = crc_octets(octets, len);
  return Val_int(crc);
}

/* Copyright abandoned; this code is in the public domain. */
/* Provided to GNUnet by peter@horizon.com */

/**
 * @file util/crc32.c
 * @brief implementation of CRC32
 **/

/* #include "gnunet_util.h" */

#define POLYNOMIAL (uLong)0xedb88320
typedef unsigned int uLong;
static uLong crc_table[256];

/*
 * This routine writes each crc_table entry exactly once,
 * with the ccorrect final value.  Thus, it is safe to call
 * even on a table that someone else is using concurrently.
 */
static void make_crc_table() {
  unsigned int i, j;
  uLong h = 1;
  crc_table[0] = 0;
  for (i = 128; i; i >>= 1) {
    h = (h >> 1) ^ ((h & 1) ? POLYNOMIAL : 0);
    /* h is now crc_table[i] */
    for (j = 0; j < 256; j += 2*i)
      crc_table[i+j] = crc_table[j] ^ h;
  }
}

/*
 * This computes the standard preset and inverted CRC, as used
 * by most networking standards.  Start by passing in an initial
 * chaining value of 0, and then pass in the return value from the
 * previous crc32() call.  The final return value is the CRC.
 * Note that this is a little-endian CRC, which is best used with
 * data transmitted lsbit-first, and it should, itself, be appended
 * to data in little-endian byte and bit order to preserve the
 * property of detecting all burst errors of length 32 bits or less.
 */
static uLong crc32(uLong crc, char const *buf, size_t len) {
  if (crc_table[255] == 0)
    make_crc_table();
  crc ^= 0xffffffff;
  while (len--)
    crc = (crc >> 8) ^ crc_table[(crc ^ *buf++) & 0xff];
  return crc ^ 0xffffffff;
}

value caml_crc32(value v_str) {
  char *octets = String_val(v_str);
  size_t len = caml_string_length(v_str);
  uLong crc = crc32(0, octets, len);
  return caml_copy_int64(crc);
}

/* Improved localtime implementation */

#include <time.h>
#include <errno.h>
#include <stdio.h>

static value alloc_tm(struct tm *tm)
{
  value res;
  res = caml_alloc_small(9, 0);
  Field(res,0) = Val_int(tm->tm_sec);
  Field(res,1) = Val_int(tm->tm_min);
  Field(res,2) = Val_int(tm->tm_hour);
  Field(res,3) = Val_int(tm->tm_mday);
  Field(res,4) = Val_int(tm->tm_mon);
  Field(res,5) = Val_int(tm->tm_year);
  Field(res,6) = Val_int(tm->tm_wday);
  Field(res,7) = Val_int(tm->tm_yday);
  Field(res,8) = tm->tm_isdst ? Val_true : Val_false;
  return res;
}

/*
 * converts a tm structure to a float with the assumption that that the structure
 * defines a gmtime
*/
CAMLprim value core_timegm (value tm_val) {
  struct tm tm;
  time_t res;

  tm.tm_sec  = Int_val(Field(tm_val,0));
  tm.tm_min  = Int_val(Field(tm_val,1));
  tm.tm_hour = Int_val(Field(tm_val,2));
  tm.tm_mday = Int_val(Field(tm_val,3));
  tm.tm_mon  = Int_val(Field(tm_val,4));
  tm.tm_year = Int_val(Field(tm_val,5));
  tm.tm_wday = Int_val(Field(tm_val,6));
  tm.tm_yday = Int_val(Field(tm_val,7));
  tm.tm_isdst = 0;  /*  tm_isdst is not used by timegm (which sets it to 0) */
  tm.tm_gmtoff = 0; /* tm_gmtoff is not used by timegm (which sets it to 0) */
  tm.tm_zone = NULL;

  res = timegm(&tm);

  if (res == (time_t) -1) caml_failwith("timegm");

  return caml_copy_double((double) res);
}

#define WRAP_TIME_FUN(NAME) \
  CAMLprim value core_##NAME (value t)         \
  { \
    time_t clock; \
    struct tm *tm; \
    struct tm tm_store; \
    clock = (time_t) Double_val(t); \
    tm = NAME##_r(&clock, &tm_store); \
    if (tm == NULL) caml_failwith("##NAME##"); \
    return alloc_tm(tm); \
  }

WRAP_TIME_FUN(localtime)
WRAP_TIME_FUN(gmtime)
