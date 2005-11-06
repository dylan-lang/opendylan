subdirs = \
	glib-2 \
	gobject-2 \
	pango \
	atk \
	gdk-pixbuf-2 \
	gdk-2 \
	gtk-2

all:
	for subdir in $(subdirs) ; \
	do $(MAKE) -C $$subdir ; done

copy-headers:
	for subdir in $(subdirs) ; \
	do $(MAKE) -C $$subdir copy-headers ; done

tidy:
	for subdir in $(subdirs) ; \
	do $(MAKE) -C $$subdir tidy ; done

clean:
	for subdir in $(subdirs) ; \
	do $(MAKE) -C $$subdir clean ; done

build: all
	for subdir in $(subdirs) ; \
	do $(MAKE) -C $$subdir build ; done
