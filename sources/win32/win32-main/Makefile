# Build Win32Main library for Functional Developer's C back-end

APPVER	= 4.0
TARGETOS = BOTH

!include <win32.mak>

CC	   = $(cc)
LIB        = $(WEBSTER_PERSONAL_LIB);$(WEBSTER_SYSTEM_LIB);$(LIB)
LINKLIB	   = $(implib) /nologo /out:
CFLAGS	   = $(cflags) $(cvarsmt) $(cdebug) /I$(WEBSTER_SYSTEM_INCLUDE)
OBJS	   = main.obj
LIBFILE	   = Win32Main.lib

$(LIBFILE): $(OBJS)
	$(LINKLIB)$(LIBFILE) $(OBJS)

lib:   $(LIBFILE)

install: $(LIBFILE)
	pushd $(WEBSTER_PERSONAL_LIB) & del /q /f $(LIBFILE) & popd
	copy $(LIBFILE) $(WEBSTER_PERSONAL_LIB)

clean:
	del /q /f $(OBJS) $(LIBFILE)
