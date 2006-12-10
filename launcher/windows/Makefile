# Makefile template. Author: Simon Urbanek <simon@urbanek.info>
#
# by default %.c->%.o->% for each *.c file
# modify DST and add explicit rule(s) to use for other mappings
#
# $Id$

SRC = $(wildcard *.c)
OBJ = $(SRC:%.c=%.o)
DST = jgr.exe jselect.exe

ifeq ($(XCPREFIX),)
ifeq ($(shell i386-mingw32msvc-gcc --version >/dev/null 2>&1 && echo OK),OK)
XCPREFIX=i386-mingw32msvc-
else
ifeq ($(shell i686-pc-mingw32-gcc --version >/dev/null 2>&1 && echo OK),OK)
XCPREFIX=i686-pc-mingw32-
endif
endif
endif

ifeq ($(XCPREFIX),)
ifeq ($(XCROOT),)
ifeq ($(shell test -e /opt/xmingw/bin && echo OK),OK)
XCROOT=/opt/xmingw
endif
ifeq ($(shell test -e /cross/bin && echo OK),OK)
XCROOT=/cross
endif
ifneq ($(XCROOT),)
ifeq ($(shell $(XCROOT)/bin/i386-mingw32msvc-gcc --version >/dev/null 2>&1 && echo OK),OK)
XCPREFIX=i386-mingw32msvc-
else
ifeq ($(shell $(XCROOT)/bin/i686-pc-mingw32-gcc --version >/dev/null 2>&1 && echo OK),OK)
XCPREFIX=i686-pc-mingw32-
endif
endif
endif
endif

endif

# set full-path CC if we found the proper cross-compiler
ifeq ($(shell test -e $(XCROOT)/bin/$(XCPREFIX)gcc && echo OK),OK)
CC = $(XCROOT)/bin/$(XCPREFIX)gcc
WINDRES = $(XCROOT)/bin/$(XCPREFIX)windres
else
# if the compiler is not in XCROOT, try to find it in PATH
WHICHCC = $(shell which $(XCPREFIX)gcc)
ifeq ($(shell test -e "$(WHICHCC)" && echo OK),OK)
CC = $(WHICHCC)
endif
WHICHWR = $(shell which $(XCPREFIX)windres)
ifeq ($(shell test -e "$(WHICHWR)" && echo OK),OK)
WINDRES = $(WHICHWR)
endif
endif

CFLAGS+=-I$(XCROOT)/include -mno-cygwin -mwin32 -mwindows
LDFLAGS+=-mwindows -mno-cygwin
LIBS+=-L$(XCROOT)/lib

ifeq ($(DEBUG),)
LIBS+=-s
else
LIBS+=-g
CFLAGS+=-g
endif

all: $(DST) $(OBJ)

%.res.a: %.res
	$(WINDRES) -i $^ -o $@

jgr.exe: jgr.o prefsp.o jgr.res.a
	$(CC) $^ -o $@ $(LIBS) $(LDFLAGS)

jselect.exe: jselect.o jselect.res.a
	$(CC) $^ -o $@ $(LIBS) $(LDFLAGS)

%: %.o
	$(CC) $< -o $@ $(LIBS)

debug:
	$(MAKE) DEBUG=T all

clean:
	rm -rf *~ *.o *.a \#* .\#* $(DST)
