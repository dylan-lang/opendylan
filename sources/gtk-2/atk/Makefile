module = ATK
pkgconfig-flags = $(shell pkg-config --libs atk)
module-deps = GLib-2 GObject-2
headers-dir = ../headers/atk-1.0/atk
headers = \
	atkaction.h \
	atkcomponent.h \
	atkdocument.h \
	atkeditabletext.h \
	atkgobjectaccessible.h \
	atkhyperlink.h \
	atkhypertext.h \
	atkimage.h \
	atknoopobject.h \
	atknoopobjectfactory.h \
	atkobject.h \
	atkobjectfactory.h \
	atkregistry.h \
	atkrelation.h \
	atkrelationset.h \
	atkrelationtype.h \
	atkselection.h \
	atkstate.h \
	atkstateset.h \
	atkstreamablecontent.h \
	atktable.h \
	atktext.h \
	atkutil.h \
	atkvalue.h

include ../early.mk
include module.mk
include ../late.mk
