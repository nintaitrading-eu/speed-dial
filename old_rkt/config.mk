# speed-dial version
VERSION = 0.1

# Customize below to fit your system

# paths
PREFIX = /usr/local
# Note: the below will be /share/man on linux.
MANPREFIX = ${PREFIX}/man
INCLUDE = /usr/src/include
SHARE = ${PREFIX}/share

# includes and libs
#INCS = -I${X11INC} -I${FREETYPEINC}
#INCS = -I${INCLUDE}
#LIBS = -L${X11LIB} -lX11 ${XINERAMALIBS} ${FREETYPELIBS}
#LIBS =

# compiler and linker
RACO = raco exe
