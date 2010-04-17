AC_PREREQ(2.63)
AC_INIT([Haskell zoneinfo],[0.5],[mark.aylett@gmail.com])
AC_CONFIG_SRCDIR([zoneinfo.cabal])

AC_PROG_CC

AC_ARG_WITH(compiler,
    [AS_HELP_STRING([--with-compiler=CMD],[use Haskell compiler CMD])])

AC_MSG_CHECKING(for tm_gmtoff)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
[[struct tm x; tm_gmtoff = 0;]])], [result=yes], [result=no]);
if test x$result = xyes; then
    AC_MSG_RESULT(yes)
    TM_GMTOFF='-DTM_GMTOFF=tm_gmtoff'
else
    AC_MSG_RESULT(no)
    TM_GMTOFF=''
fi
AC_SUBST([TM_GMTOFF])

AC_MSG_CHECKING(for tm_zone)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
[[struct tm x; x.tm_zone = "";]])], [result=yes], [result=no]);
if test x$result = xyes; then
    AC_MSG_RESULT(yes)
    TM_ZONE='-DTM_ZONE=tm_zone'
else
    AC_MSG_RESULT(no)
    TM_ZONE=''
fi
AC_SUBST([TM_ZONE])

# Placeholder.
result=yes
if test x$result = xyes; then
    BUILD_PACKAGE_BOOL=True
else
    BUILD_PACKAGE_BOOL=False
fi
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_CONFIG_FILES([zoneinfo.buildinfo])
AC_OUTPUT
