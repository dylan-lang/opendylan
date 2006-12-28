module = GDK-Pixbuf-2
pkgconfig-flags = $(shell pkg-config --libs gdk-pixbuf-2.0)
module-deps = GLib-2 GObject-2
headers-dir = ../headers/gtk-2.0/gdk-pixbuf
headers = \
	gdk-pixbuf-animation.h \
	gdk-pixbuf-core.h \
	gdk-pixbuf-enum-types.h \
	gdk-pixbuf-io.h \
	gdk-pixbuf-loader.h \
	gdk-pixbuf-transform.h

include ../early.mk
include module.mk
include ../late.mk
