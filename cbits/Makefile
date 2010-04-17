# http://e2fsprogs.sourceforge.net/

PLATFORM = MINGW

CFLAGS = -I../include -Wall

LDFLAGS = -L.

CMODULES = libolson

CPROGRAMS = olsontest

libolson_OBJS = \
	asctime.o \
	difftime.o \
	localtime.o

olsontest_OBJS = \
	olsontest.o
olsontest_LIBS = \
	olson

all: all-aug

clean: clean-aug

diff:
	diff -wut --tabsize=4 asctime.c.orig asctime.c || true
	diff -wut --tabsize=4 difftime.c.orig difftime.c || true
	diff -wut --tabsize=4 localtime.c.orig localtime.c || true

include $(AUG_HOME)/etc/aug.mk
