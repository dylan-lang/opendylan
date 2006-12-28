module = GLib-2
pkgconfig-flags =$(shell  pkg-config --libs glib-2.0)
headers-dir = ../headers/glib-2.0/glib
headers = \
	../gmodule.h \
	../glibconfig.h \
	garray.h \
	gconvert.h \
	gdataset.h \
	gerror.h \
	ghash.h \
	giochannel.h \
	glist.h \
	gmain.h \
	gmem.h \
	gnode.h \
	goption.h \
	gquark.h \
	gscanner.h \
	gslist.h \
	gstring.h \
	gthread.h \
	gtree.h \
	gtypes.h \
	gunicode.h

include ../early.mk
include module.mk
include ../late.mk
