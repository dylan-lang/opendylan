CFLAGS = /nologo /MT /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE"

win32-threads-primitives.obj: win32-threads-primitives.c
	cl $(CFLAGS) /c win32-threads-primitives.c
