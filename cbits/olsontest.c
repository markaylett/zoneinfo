#include "olson.h"

#include <stdio.h>

int
main(int argc, char* argv[])
{
    olson_t ols;
    const_zoneinfo_t zi;
    time_t t = time(NULL);
    struct tm tm;
    char buf[255];

    if (!(ols = olson_create("c:/usr/etc/zoneinfo")))
        return 1;

    zi = olson_get(ols, "America/New_York");
    /*zi = olson_create("EST5EDT4,M3.2.0/02,M11.1.0/02");*/

    olson_localtime(zi, &t, &tm);

    printf("%s", olson_asctime(&tm, buf));
    printf("%d\n", (int)olson_gmtoff(zi, 0));
    printf("%s\n", olson_zone(zi, 0));
    printf("%d\n", (int)olson_gmtoff(zi, 1));
    printf("%s\n", olson_zone(zi, 1));
    olson_destroy(ols);
    return 0;
}
