PKGS=glib-2.0 gtk+-x11-2.0 libglade-2.0 alsa
CFLAGS=`pkg-config --cflags $(PKGS)` -g -O2 -Wall -W -Wno-unused-parameter
LDFLAGS=-g -O2
LDLIBS=`pkg-config --libs $(PKGS)` -lcrypto -lpopt -lm

all: dump_vos pmg

clean:
	-rm -f dump_vos pmg *.o

dump_vos: dump_vos.c

pmg: pmg.c

%.out: %.vos dump_vos
	./dump_vos $*.vos > $*.out
