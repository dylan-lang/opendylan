TARGET = $(ROOT)/usr/local/lib/functional-developer

install:
	install -d -o 0 -g 0 $(TARGET)
	cp -a README $(TARGET)/
	cp -a LICENSE $(TARGET)/
	cp -a bin $(TARGET)/
	cp -a databases $(TARGET)/
	cp -a Examples $(TARGET)/
	ln -sf ./Examples $(TARGET)/examples
	cp -a lib $(TARGET)/
	cp -a Sources $(TARGET)/
	ln -sf ./Sources $(TARGET)/sources
	chmod a-w -R $(TARGET)
	chmod u+w $(TARGET)/Examples/*
	install -d -o 0 -g 0 $(ROOT)/usr/local/bin
	ln -sf ../lib/functional-developer/bin/fdcompile $(ROOT)/usr/local/bin/fdcompile
	ln -sf ../lib/functional-developer/bin/xfdcompile $(ROOT)/usr/local/bin/xfdcompile
