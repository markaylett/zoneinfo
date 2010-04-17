#ifndef OLSON_H
#define OLSON_H

#include <time.h>

#define OLSON_API

typedef struct olson_* olson_t;
typedef const struct olson_* const_olson_t;
typedef struct zoneinfo_* zoneinfo_t;
typedef const struct zoneinfo_* const_zoneinfo_t;

OLSON_API olson_t
olson_create(const char* tzdir);

OLSON_API void
olson_destroy(olson_t ols);

/* This function has side-effects.  Calls must be serialised across multiple
   threads. */

OLSON_API const_zoneinfo_t
olson_get(olson_t ols, const char* name);

OLSON_API const_zoneinfo_t
olson_gmt(const_olson_t ols);

OLSON_API struct tm*
olson_localtime(const_zoneinfo_t sp, const time_t* timep, struct tm* tmp);

OLSON_API struct tm*
olson_gmtime(const_olson_t ols, const time_t* timep, struct tm* tmp);

OLSON_API struct tm*
olson_offtime(const_olson_t ols, const time_t* timep, const long offset,
              struct tm* tmp);

OLSON_API char*
olson_ctime(const_zoneinfo_t sp, const time_t* timep, char* buf);

OLSON_API time_t
olson_mktime(const_zoneinfo_t sp, struct tm* tmp);

OLSON_API time_t
olson_timelocal(const_zoneinfo_t sp, struct tm* tmp);

OLSON_API time_t
olson_timegm(const_olson_t ols, struct tm* tmp);

OLSON_API time_t
olson_timeoff(const_olson_t ols, struct tm* tmp, const long offset);

OLSON_API time_t
olson_time2posix(const_zoneinfo_t sp, time_t t);

OLSON_API time_t
olson_posix2time(const_zoneinfo_t sp, time_t t);

OLSON_API long
olson_gmtoff(const_zoneinfo_t sp, int isdst);

OLSON_API const char*
olson_zone(const_zoneinfo_t sp, int isdst);

OLSON_API char*
olson_asctime(const struct tm* timeptr, char* buf);

OLSON_API double
olson_difftime(time_t time1, time_t time0);

#if 0
# define asctime 0
# define asctime_r 0
# define ctime 0
# define ctime_r 0
# define difftime 0
# define gmtime 0
# define gmtime_r 0
# define localtime 0
# define localtime_r 0
# define mktime 0
# define offtime 0
# define posix2time 0
# define time2posix 0
# define timegm 0
# define timelocal 0
# define timeoff 0
# define tzset 0
# define tzsetwall 0
# define timezone 0
# define daylight 0
# define altzone 0
# define tzname 0
#endif

#endif /* OLSON_H */
