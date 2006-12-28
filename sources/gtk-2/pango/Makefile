module = Pango
pkgconfig-flags =$(shell  pkg-config --libs pango)
module-deps = GLib-2 GObject-2
headers-dir = ../headers/pango-1.0/pango
headers = \
	pango-attributes.h \
	pango-break.h \
	pango-context.h \
	pango-coverage.h \
	pango-engine.h \
	pango-enum-types.h \
	pango-font.h \
	pango-fontmap.h \
	pango-fontset.h \
	pango-glyph.h \
	pango-glyph-item.h \
	pango.h \
	pango-item.h \
	pango-layout.h \
	pango-renderer.h \
	pango-script.h \
	pango-tabs.h \
	pango-types.h \
	pango-utils.h

include ../early.mk
include module.mk
include ../late.mk
