## autoconf configure.ac && autoreconf -if
AC_PREREQ([2.69])
AC_INIT([pbdBASE],[0.3-0])
AC_CONFIG_SRCDIR([src/crossprod.c])

# Determine R_HOME.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

R_SCMD="${R_HOME}/bin/Rscript -e"


# pbdMPI
SPMD_CPPFLAGS=`${R_SCMD} "source('./R/conf/get_conf.r');get.conf('PKG_CPPFLAGS','"${R_ARCH}"')"`
SPMD_LDFLAGS=`${R_SCMD} "source('./R/conf/get_conf.r');get.conf('PKG_LIBS','"${R_ARCH}"')"`


# Report
echo " "
echo "****************** Results of kazaam package configure *****************"
echo " "
echo ">> SPMD_CPPFLAGS = ${SPMD_CPPFLAGS}"
echo ">> SPMD_LDFLAGS = ${SPMD_LDFLAGS}"
echo " "
echo "************************************************************************"
echo " "


AC_SUBST(SPMD_CPPFLAGS)
AC_SUBST(SPMD_LDFLAGS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
