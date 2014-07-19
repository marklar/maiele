#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>

#include <ctype.h>
#include <limits.h>
#include <locale.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

#define UNIX_BUFFER_SIZE 16384
extern void uerror(char *, value) Noreturn;

static struct tm*
tm_val(value t, struct tm *tm) {
  if (!tm)
    return NULL;
  tm->tm_sec = Int_val(Field(t, 0));
  tm->tm_min = Int_val(Field(t, 1));
  tm->tm_hour = Int_val(Field(t, 2));
  tm->tm_mday = Int_val(Field(t, 3));
  tm->tm_mon = Int_val(Field(t, 4));
  tm->tm_year = Int_val(Field(t, 5));
  tm->tm_wday = Int_val(Field(t, 6));
  tm->tm_yday = Int_val(Field(t, 7));
  tm->tm_isdst = -1; /* tm.tm_isdst = Bool_val(Field(t, 8)); */
  return tm;
}

value
stew_strftime_tm(value fmt, value for_time) {
  CAMLparam2(fmt, for_time);
  CAMLlocal1(ret);
  struct tm tm = {0};
  char buf[8192];

  tm_val(for_time, &tm);

  strftime(buf, sizeof buf, String_val(fmt), &tm);

  ret = copy_string(buf);

  CAMLreturn(ret);
}
