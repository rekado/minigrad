dnl -*- Autoconf -*-

AC_INIT(minigrad, 0.1)
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

if $GUILE_TOOLS | grep -q compile; then
   # everything cool
   true
else
   AC_MSG_ERROR([Guile 2.0 required.])
fi

#PKG_CHECK_MODULES([LIBGV], [libgv])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_OUTPUT
