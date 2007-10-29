#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(ikarus-scheme, prerelease-0, aghuloum@cs.indiana.edu)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(ikarus-scheme, 0.0.1)
AC_CONFIG_SRCDIR([src/])


AM_PROG_AS
# Checks for programs.
AC_PROG_CC

CFLAGS="$CFLAGS -DNDEBUG -O3"

case "$target_os" in
  *linux*)
    LDFLAGS="-rdynamic $LDFLAGS"
    ;;
  esac

# Checks for libraries.
AC_HAVE_LIBRARY([gmp],,[AC_MSG_ERROR([
ERROR: Cannot find libgmp. 
ERROR: You might want to supply CFLAGS=-I</path/to/include> (containing
ERROR: the gmp.h header file), and LDFLAGS=-L</path/to/lib> (containing
ERROR: libgmp.so) if libgmp is installed in a non-standard location.
ERROR: libgmp can be obtained from <http://gmplib.org>. ])])
AC_SEARCH_LIBS(dlsym, dl,, [AC_MSG_ERROR([Cannot find libdl])])
AC_SEARCH_LIBS(cos, m,, [AC_MSG_ERROR([Cannot find libm])])

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([gmp.h],,[AC_MSG_ERROR([Cannot find gmp.h])])


AC_CHECK_HEADERS([assert.h dlfcn.h errno.h fcntl.h signal.h \
                  stdint.h stdlib.h string.h strings.h \
                  sys/mman.h sys/resource.h sys/stat.h \
                  sys/time.h sys/types.h sys/wait.h time.h \
                  unistd.h])


# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_CHECK_FUNCS([bzero gettimeofday memmove memset munmap setenv sqrt strerror])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES(Makefile src/Makefile scheme/Makefile doc/Makefile)
AC_OUTPUT #(Makefile src/Makefile scheme/Makefile doc/Makefile)

