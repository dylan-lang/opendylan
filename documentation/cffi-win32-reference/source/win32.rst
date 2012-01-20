**********************************
The Open Dylan Win32 API Libraries
**********************************

.. current-library:: win32-kernel
.. current-module:: win32-kernel

Introduction
============

This chapter is about Open Dylan’s set of Win32 API interface
libraries. These libraries provide a low-level Dylan interface to the
Win32 API in Microsoft Windows and Microsoft Windows NT.

Each Dylan library is a simple translation of Win32 API header files
into a set of interface declarations from Open Dylan’s C-FFI
library. So you can write Windows applications in Dylan by using the
same functions and types as you would in C, albeit with slightly
modified names so that they conform to Dylan naming conventions and
requirements.

The Open Dylan Win32 API has been constructed from several Dylan
libraries. Win32 functionality is divided among these libraries,
matching the contents of Microsoft’s DLLs, allowing Dylan applications
to avoid references to DLLs they do not need to use.

With the exception of changes necessitated by Dylan naming conventions
and requirements, the names of C items have been preserved in the
Open Dylan Win32 API libraries. Hence this chapter does not provide
an exhaustive list of the items available in the libraries. Instead, it
explains the name mapping scheme we used in the conversion, and provides
a collection of tips for writing Dylan applications with the libraries.
Finally, there is a list of items not supported in our versions of these
libraries.

Supported Win32 libraries
=========================

Each Dylan library representing a portion of the Win32 API has a single
module of the same name as the library itself. For example, the library
Win32-Common has a module also called Win32-Common. An exception to this
rule is Win32-User, which also exports the module Win32-Default-Handler.

The libraries are:

Win32-Common

-  Data types, constants (including error codes), and structure
   accessors that are shared by the other modules.
-  Most of these come from the Win32 header files *WINDEF.H*, *WINNT.H*,
   and *WINERROR.H*. (There is no DLL file supplied as standard with
   Windows that corresponds with this library, because there are no C
   functions in the header files to which it forms an interface.)
-  Win32-Kernel Non-GUI system services, as implemented in
   *KERNEL32.DLL* and declared in *WINBASE.H* (files, pipes, semaphores,
   atoms, time, and so on) and *WINNLS.H* (National Language Support).
-  *Note:* This library does not provide thread support. Thread support
   is being handled at a higher level by Dylan’s own Threads library.
   See the *Core Features* manual for details.
-  Win32-GDI Graphics Device Interface, drawing graphics and text, and
   printing. Corresponds to *WINGDI.H* and *GDI32.DLL*.
-  Win32-User Other windowing functions. Corresponds to *WINUSER.H* and
   *USER32.DLL*. Also contains :func:`win32-last-handler` which can handle
   conditions and display them to the application user a simply Win32
   dialog. That function is exported from the module
   Win32-Default-Handler.
-  Win32-Version Version management. Corresponds to *WINVER.H* and
   *VERSION.DLL*.
-  Win32-Dialog Common dialog boxes, as implemented in *COMDLG32.DLL*
   and declared in *COMMDLG.H*, *DLGS.H*, and *CDERR.H*.

Win32-Controls

-  “Common controls”, including list view, tree view, property sheets,
   and so on (*COMMCTRL.H* and *COMCTL32.DLL*).

Win32-Registry

-  Registry (*WINREG.H* and *ADVAPI32.DLL*).

Win32-Rich-Edit

-  “Rich edit” controls (*RICHEDIT.H* and *RICHED32.DLL*).
-  Win32-DDE Dynamic Data Exchange (*DDE.H* and *DDEML.H*).
-  Win32-Shell API for querying and extending the Windows Shell.
   Corresponds to *SHELLAPI.H* and *SHELL32.DLL*.
-  Winsock2 Corresponds to *WINSOCK2.H*, *QOS.H*, and *MSWSOCK.H*.

Content and organization of the Win32 API libraries
===================================================

The Open Dylan Win32 API libraries are modeled closely upon
Microsoft’s Win32 C libraries. Most names available in the Dylan
libraries are the same as those available in the C libraries, though of
course to conform to Dylan naming conventions and restrictions, many of
the C names have been translated.

.. note:: Look at the *library.dylan* file in each library to see what
   each library provides. (Look in *comlib.dylan* for Win32-Common.) The
   libraries generally include only features that apply to both Windows NT
   and Windows 95/98.

   If there is an additional area of Win32 you would like to see these
   libraries support, please inform the Open Dylan support team.

Notes on the translations
-------------------------

The Win32-Common module re-exports some names from the C-FFI module that
its user may need to use directly, without needing to use (or know
about) the C-FFI module itself. These names are: *null-pointer*,
*null-pointer?*, *pointer-address*, ``pointer-value``,
``pointer-value``, *pointer-cast*, ``<C-string>``,
``<C-unicode-string>``, *destroy*, and *with-stack-structure.*

Names that are documented as being obsolete and/or included in Win32
only for compatibility with Win16, are generally not defined in the
Dylan libraries. The names excluded are listed in `Index of Win32
names excluded from the Dylan libraries`_.

The extended API macros, defined in the optional C header file
*WINDOWSX.H*, are not supported.

Naming and mapping conventions
==============================

A Dylan application using the Win32 API will generally use the same API
names as a C program would, with the following modifications for
consistency with Dylan conventions.

Simple naming conventions
-------------------------

Type names are enclosed in angle brackets. For example, *HANDLE* becomes
``<HANDLE>``.

Names of constants are prefixed by *$*. For example, *OPAQUE* becomes
*$OPAQUE*.

Underscores are replaced by hyphens. Thus, a constant called *NO\_ERROR*
becomes *$NO-ERROR* and a class called *LIST\_ENTRY* becomes
``<LIST-ENTRY>``.

Hyphens will *not* be inserted between capitalized words (for example,
*CreateWindow* does not become *Create-Window*) since that is a less
obvious mapping that is more likely to cause confusion when switching
between Dylan code and Windows documentation.

Mapping the null value
----------------------

In place of ``NULL``, there are several constants providing null values
for frequently used types, such as *$NULL-HANDLE*, *$NULL-RECT*, and
*$NULL-STRING*. Null values for other pointer types may be designated
by the expression *null-pointer(<FOO>)*. Use the function
*null-pointer?* to test whether a value is null. Do not use the
expression *if(ptr)...* as is often done in C, since a null pointer is
not the same as ``#f``. There are also functions *null-handle* and
*null-handle?* for creating and testing handles, since conceptually they
are not necessarily pointers.

Mapping C types onto Dylan classes
----------------------------------

The multitude of integer data types in C code (``int``, ``long``,
``unsigned``, ``ULONG``, ``DWORD``, ``LRESULT``, and so on) are all
designated as ``<integer>`` (or some appropriate subrange thereof) in
Dylan method argument types. However, a ``<machine-word>`` needs to be
used to represent values that do not fit in the signed 30-bit
representation of an integer.

Names such as ``<DWORD>`` should not be used in application code because
they refer to the FFI designation of the C value representation, not to
a Dylan data type.

The C types ``BOOL`` and ``BOOLEAN`` are both mapped to ``<boolean>`` in
Dylan. Use ``#t`` and ``#f`` instead of ``TRUE`` and ``FALSE``.

.. note:: Beware that some functions, such as *TranslateAccelerator*,
   though documented to return ``TRUE`` or ``FALSE``, actually return ``int``
   instead of ``BOOL`` ; in such a case, you will have to compare the result
   to 0.

   Similarly, watch out for cases where C code passes ``TRUE`` or ``FALSE`` as
   an integer argument. To handle one common case, the Dylan implementation
   of *MAKELPARAM* accepts either an ``<integer>`` or ``<boolean>`` as the
   first argument.

The C types ``CHAR``, ``WCHAR``, and ``TCHAR`` are all mapped to
``<character>`` in Dylan. However, ``UCHAR`` is mapped to ``<integer>``
since that is how it is actually used.

Most of the pointer types in the Windows API have several names; for
example: *PRECT*, *NPRECT*, and *LPRECT*. In 16-bit code, these
distinguished between “near” and “far” pointers, but in 32-bit code
there is no difference. Rather than carry the duplicate names over into
Dylan, it would be simpler to use only the basic *P...* prefix names.
However, the *LP...* names seem to be used much more often, and hence
may be more familiar, and the Microsoft documentation still tends to use
the *LP...* names in most places. So the Dylan interface defines both
the *<P...>* and *<LP...>* names even though they have identical values.
The *NP...* names are not defined in Dylan since they are not as
commonly used.

Values of type ``char*`` in C are represented as instances of class
``<C-string>`` in Dylan. This is a subclass of ``<string>``, so all of the
normal string operations can be used directly. C function parameters of
type *char\** will also accept an instance of ``<byte-string>`` ; a C
pointer is created to point to the characters of the Dylan data, so the
string does not need to be copied. (Dylan byte strings maintain a NUL
character at the end in order to allow them to be used directly by C.)

in the current implementation, that involves automatically copying the
string at run time, but the need for copying is intended to be removed
later.

The *TEXT* function can also be used to coerce a string literal to a
``<C-string>``. This usage is consistent with the Win32 *TEXT* macro,
although the current purpose is different.

The Dylan declarations for C types will generally follow the *strict*
alternative versions of the C declarations. This means, for example,
that the various handle types such as ``<hmenu>`` and ``<hwnd>`` are
disjoint subclasses of ``<handle>``, instead of all being equivalent.

Creating methods from Windows alias functions
---------------------------------------------

Consider a Windows function called *Foo* which is an alias for either
*FooA* (an 8-bit character version) or *FooW* (a 16-bit character
version). In Dylan, only the name *Foo* will be defined, but it will be
a generic function with separate methods for arguments of types
``<C-string>``, ``<C-unicode-string>``, ``<byte-string>`` or
``<unicode-string>``. (Only the 8-bit versions will be supported in the
initial implementation, both because the compiler is not ready to handle
Unicode and because it will not work on Windows 95.)

Mapping C structure fields onto Dylan slot names
------------------------------------------------

Because slot names are not in a separate name space in Dylan, the names
of C structure fields will have the suffix *-value* added to form the
name of the Dylan accessor function. For example, the C statement:

.. code-block:: c

    pt->x = x;

becomes in Dylan:

.. code-block:: dylan

    pt.x-value := x;

There is not any attempt to append *?* to the names of predicate
functions since it is not obvious exactly which functions that should
apply to. The Dylan convention of *\*...\** for global variables is not
relevant since there are no global variables involved.

Handling return of multiple values
----------------------------------

In cases where the C library function takes a pointer argument as a
place to store a pointer, integer, or boolean value, the corresponding
Dylan function uses multiple return values to return such output
parameters following the original function return value. For example,
where C code does:

.. code-block:: c

    BOOL ok;
    DWORD NumberRead;

    ok = ReadConsoleInput(handle, buffer, length, & NumberRead);

in Dylan it would be:

.. code-block:: dylan

    let ( ok :: <boolean>, NumberRead :: <integer> ) =
      ReadConsoleInput(handle, buffer, length);

Similarly, this function returns multiple values instead of a structure:

.. code-block:: dylan

    let ( x, y ) = GetLargestConsoleWindowSize(handle);

Defining callback functions
===========================

The Win32-common library provides a ``define callback`` macro to make it
easy to define callback functions without the application programmer
needing to use the FFI ``define c-callable-wrapper`` macro directly. It is
used like this:

.. code-block:: dylan

    define callback WndProc :: <WNDPROC> = my-window-function;

This says that ``WndProc`` is being defined as a C function pointer of
type ``<WNDPROC>``, which when called from C causes the Dylan function
``my-window-function`` to be run. The Dylan function will be defined
normally using ``define method`` or ``define function``, and it is the
responsibility of the programmer to ensure that its argument signature
is consistent with what ``<WNDPROC>`` requires. For example:

.. code-block:: dylan

    define method my-window-function(
      hWnd :: <HWND>, // window handle
      message :: <integer>, // type of message
      uParam, // additional information
      lParam) // additional information
    => return :: <integer>;
      ...

Note that the *uParam* and *lParam* arguments might receive values of
either type ``<integer>`` or ``<machine-word>``, so it may be best not to
specialize them. Often these values are not used directly anyway, but
are passed to other functions (such as *LOWORD* and *HIWORD*) which
have methods for handling either representation.

The other types of function supported by ``define callback`` are dialog
functions (``<DLGPROC>``) and dialog hooks (*<LP...HOOKPROC>*), both of
which have the same argument types as a window function, but return a
``<boolean>``. (The dialog hook functions are actually declared in
*COMMDLG.H* as returning a *UINT*, but the value is always supposed to
be ``TRUE`` or ``FALSE``, so the Dylan callback interface has been
implemented using ``BOOL`` instead.)

Error messages
==============

The Win32-Kernel library provides the following utility functions.

.. function:: win32-error-message

   :signature: win32-error-message *error-code* => *message*

   :description:

     The *error-code* is an instance of ``<integer>`` or ``<machine-word>`` (type
     unioned).

     The *error-code* argument is either a Windows a Windows error code
     (such as returned by ``GetLastError``) or an ``SCODE`` (also known
     as an ``HRESULT``) value (such as returned by most OLE/COM
     functions).

     The function returns a text message (in a string) corresponding to
     the error code, ``#f`` if the code is not recognized. The returned
     string might have more than one line but does not have a newline at
     the end.

   :example:

     .. code-block:: dylan

         win32-error-message(5) => "Access is denied."

.. function:: report-win32-error

   :signature: report-win32-error *name* #key *error*

   :description:

     Signals a Dylan error if the Win 32 error code specified is not
     ``NO_ERROR``. If no code is specified, the value returned by the
     Win32 API ``GetLastError`` is used. The error that is signaled
     includes both the error code and the error message, as computed by
     the function :func:`win32-error-message`.

.. function:: check-win32-result

   :signature: check-win32-result *name* *result*

   :description:

     Many Windows functions return ``#f`` or ``NULL`` to mean failure.
     The function ``check-win32-result`` checks the result to see if it
     indicates failure, and if so it calls :func:`report-win32-error`.

   :example:

     .. code-block:: dylan

         check-win32-result("SetWindowText", SetWindowText(handle, label))

.. function:: ensure-no-win32-error

   :signature: ensure-no-win32-error *name*

   :description:

     Ensures that the Win32 API ``GetLastError`` does not indicate that
     an error occurred. If an occurs, it is signaled using
     :func:`report-win32-error`.

Handling Dylan conditions in a Win32 application
================================================

The Win32-User library exports from its Win32-Default-Handler module a
handler utility function called :func:`win32-last-handler`, defined on
objects of class ``<serious-condition>``.

.. function:: win32-last-handler
   :library: win32-user
   :module: win32-user

   :description:

     Displays a rudimentary Win32 dialog to allow the user to decide
     what to do with the Dylan condition that has been signalled.

     It is a handler utility function that can be by bound dynamically
     around a computation via ``let handler`` or installed globally via
     ``last-handler-definer``. It is automatically installed as the last
     handler simply by using the Win32-User library.

     The function has the following call syntax:

     .. code-block:: dylan

         win32-last-handler (serious-condition, next-handler)

     The *serious-condition* argument is an object of class serious
     condition. The *next-handler* argument is a function. The
     *win32-last-handler* function returns no values.

     The following form defines a dynamic handler around some body:

     .. code-block:: dylan

         let handler <serious-condition> = win32-last-handler;

     while the following form installs a globally visible last-handler:

     .. code-block:: dylan

         define last-handler <serious-condition> = win32-last-handler;

     See also *last-handler-definer* and *default-last-handler*, exported
     from the Functional Dylan-Extensions library and module, in the
     *Core Features* reference manual.

Dealing with the C function WinMain
===================================

In C, the programmer has to supply a *WinMain* function as the main
program for a GUI application, but in Dylan there is no main program as
such. The beginning of execution is indicated simply by a top-level
function call expression; this needs to be at the bottom of the last
file listed in the project file. The Win32-Kernel and Win32-User
libraries export functions to obtain the values which would have been
the arguments to *WinMain*:

.. code-block:: dylan

    application-instance-handle() => <HINSTANCE>
    application-command-line() => <string>
    // arguments without program name
    application-show-window() => <integer> // one of $SW-...

There is no accessor provided for the *WinMain* previous instance
parameter because on Win32, that parameter is always null, even for
Win32s as well as NT and Windows 95.

The program can be terminated, with an exit code, by calling either the
Win32 ``ExitProcess`` function or the ``exit-application`` function in
Open Dylan’s System library. The latter method is preferred if the
application might actually be run as part of another process.

The start of an application program might look something like this:

.. code-block:: dylan

    define method my-main ()
      let hInstance :: <HINSTANCE> = application-instance-handle();
      let wc :: <PWNDCLASS> = make(<PWNDCLASS>);
      wc.style-value := 0;
      wc.lpfnWndProc-value := MainWndProc;
      ...
      RegisterClass(wc);
      let hWnd = CreateWindow( ... );
      ShowWindow(hWnd, application-show-window());
      UpdateWindow(hWnd);
      let msg :: <PMSG> = make(<PMSG>);
      while ( GetMessage(msg, $NULL-HWND, 0, 0) )
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
      ExitProcess(msg.wParam-value);
    end method my-main;

    my-main(); // this is what initiates execution.

For a complete example program, see

    Examples\\Win32\\windows-ffi-example\\example.dylan

in the Open Dylan installation directory.

Combining bit mask constants
============================

Where C code would use the *\|* operator to combine bit mask constants,
Dylan code usually uses the *logior* function. However, a few such
constants have values of type ``<machine-word>`` when they will not fit in
a small integer, and *logior* only works on instances of ``<integer>``.
Because of this, the *win32-common* library exports a *%logior* function
which is used like *logior* except that it accepts values of either type
``<integer>`` or ``<machine-word>`` and returns a ``<machine-word>`` result.
It can be used in most places that accept a bit mask (C types *DWORD*,
*ULONG*, *LPARAM*, and so on), but must be used if any of the
arguments are a ``<machine-word>``. The contexts where this is likely to
occur are:

-  Window style parameter of *CreateWindow ($WS-...)*
-  Flags value for *CreateFile* or *CreateNamedPipe* *($FILE-FLAG-...)*
-  *$LOCALE-NOUSEROVERRIDE* for *LCTYPE* parameters for *GetLocaleInfoA*
   , *GetLocaleInfo*, and possibly others, or *dwFlags* parameter of
   *GetTimeFormat*, *GetNumberFormat*, *GetCurrencyFormat*, or
   *GetDateFormat*.
-  Mask and effects values in *CHARFORMAT* structure for “rich edit”
   controls *($CFM-...* and *$CFE-...)*
-  Mask value in *PARAFORMAT* structure for “rich edit” controls
    *($PFM-...)*

Other minor details
===================

The types ``<FARPROC>`` and ``<PROC>`` are defined as equivalent to
``<C-function-pointer>``, so any C function wrapper object can be passed
to a routine taking a ``<FARPROC>`` without needing to do any type
conversion like that needed in C.

Type casts between handles and integers (``<integer>`` or
``<machine-word>``) can be done by using *as*. For example:

.. code-block:: dylan

    window-class.hbrBackground-value :=
      as(<HBRUSH>, $COLOR-WINDOW + 1);

Note that pointers and handles must be compared using *=*, not *==*,
in order to compare the C address instead of the Dylan wrapper objects.

For type casts from one pointer type to another, use the function
:func:`pointer-cast` instead of ``as``. Think of ``as`` as converting
the data structure pointed to, while ``pointer-cast`` operates on just
the pointer itself.

The Dylan function :gf:`pointer-value` can be used to convert between a
Dylan integer and a *LARGE-INTEGER* or *ULARGE-INTEGER*. For example:

.. code-block:: dylan

    let li :: make( <PLARGE-INTEGER> ); pointer-value(li) := 0;

allocates a *LARGE-INTEGER* and sets its value to 0, without needing to
be concerned with the individual fields of the internal representation.
Alternatively, you can use an initialization keyword:

.. code-block:: dylan

    let li :: make( <PLARGE-INTEGER>, value: 0 );

The C macros *MAKEPOINT*, *MAKEPOINTS*, and *LONG2POINT* do not easily
translate to Dylan. Instead, use the Dylan function *lparam-to-xy* to
split a parameter into two signed numbers. For example:

.. code-block:: dylan

    let ( x, y ) = LPARAM-TO-XY(lParam);

In Dylan, ``<RECTL>`` is an alias of ``<RECT>`` instead of being a distinct
type. (In Win32, they are structurally equivalent but were separate
types for the sake of source code compatibility with Win16; there is no
need to maintain that artificial distinction in Dylan.)

Windows resource files (*.rc* files) can be included by using the LID
file field *RC-Files:*.

Index of Win32 names excluded from the Dylan libraries
======================================================

The names listed in the index below are excluded from the Open Dylan
Win32 API libraries because they are obsolete.

Functions for old-style metafiles (*CreateMetaFile*, *CloseMetaFile*,
and so on) are described in the Win32 API as being obsolete, but they
are being supported because they are needed for OLE applications to
exchange data with 16-bit applications.

Functions *wsprintf* and *wvsprintf* are not supported because the Dylan
function *format-to-string* serves the same purpose. Also, the FFI
doesn't currently support C functions with a variable number of
arguments.

The extended API macros defined in optional C header file *windowsx.h*
will not be supported by the Dylan interface.

The 64-bit utility macros *Int32x32To64*, *Int64ShllMod32*,
*Int64ShraMod32*, *Int64ShrlMod32*, and *UInt32x32To64* are not
planned to be supported since there is no clear need for them and the
functionality can be obtained by using Dylan extended integers. However,
an interface to function *MulDiv* is provided, since it is an ordinary C
function that is commonly used.

Characters
----------

    \_hread, \_hwrite, \_lclose, \_lcreat, \_llseek, \_lopen, \_lread,
    \_lwrite

A
-

    AccessResource, AllocDSToCSAlias, AllocResource, AllocSelector,
    AnsiLowerBuff, AnsiNext, AnsiPrev, AnsiToOem, AnsiToOemBuff,
    AnsiUpper, AnsiUpperBuff

B
-

    BN\_DBLCLK, BN\_DISABLE, BN\_DOUBLECLICKED, BN\_HILITE, BN\_PAINT,
    BN\_PUSHED, BN\_UNPUSHED, BS\_USERBUTTON

C
-

    CPL\_INQUIRE, ChangeSelector, CloseComm, CloseSound, CopyLZFile,
    CountVoiceNotes

D
-

    DOS3Call, DefHookProc, DefineHandleTable, DeviceMode,
    DlgDirSelect, DlgDirSelectComboBox

E
-

    EnumFonts, ERR\_..., ExtDeviceMode

F
-

    FixBrushOrgEx, FlushComm, FreeModule, FreeProcInstance,
    FreeSelector

G
-

    GCW\_HBRBACKGROUND, GCW\_HCURSOR, GCW\_HICON, GWW\_HINSTANCE,
    GWW\_HWNDPARENT, GWW\_ID, GWW\_USERDATA, GetAspectRatioFilter,
    GetAtomHandle, GetBitmapBits, GetBitmapDimension, GetBrushOrg,
    GetCharWidth, GetCodeHandle, GetCodeInfo, GetCommError, GetCurrentPDB,
    GetCurrentPosition, GetEnvironment, GetFreeSpace,
    GetFreeSystemResources, GetInstanceData, GetKBCodePage,
    GetMetaFile, GetMetaFileBits, GetPrivateProfileInt
    GetPrivateProfileSection, GetPrivateProfileSectionNames,
    GetPrivateProfileString, GetPrivateProfileStruct, GetProfileInt,
    GetProfileSection, GetProfileString, GetStringTypeA,
    GetStringTypeW, GetTempDrive, GetTextExtent, GetTextExtentEx,
    GetTextExtentPoint, GetThresholdEvent, GetThresholdStatus,
    GetViewportExt, GetViewportOrg, GetWindowExt, GetWindowOrg,
    GlobalCompact, GlobalDosAlloc, GlobalDosFree, GlobalFix,
    GlobalLRUNewest, GlobalLRUOldest, GlobalNotify, GlobalPageLock,
    GlobalPageUnlock, GlobalUnWire, GlobalUnfix, GlobalUnwire,
    GlobalWire

H
-

    HFILE, HFILE\_ERROR

L
-

    LZDone, LZStart, LimitEmsPages, LocalCompact, LocalInit,
    LocalNotify, LocalShrink, LockSegment

M
-

    MAKEPOINT, MakeProcInstance, MoveTo

N
-

    NetBIOSCall

O
-

    OemToAnsi, OemToAnsiBuff, OffsetViewportOrg, OffsetWindowOrg,
    OpenComm, OpenFile, OpenSound

P
-

    PM\_NOYIELD, ProfClear, ProfFinish, ProfFlush, ProfInsChk,
    ProfSampRate, ProfSetup, ProfStart, ProfStop

R
-

    READ, READ\_WRITE, ReadComm, RegCreateKey, RegEnumKey, RegOpenKey,
    RegQueryValue, RegSetValue

S
-

    SYSTEM\_FIXED\_FONT, ScaleViewportExt, ScaleWindowExt,
    SetBitmapDimension, SetCommEventMask, SetEnvironment,
    SetMetaFileBits, SetResourceHandler, SetScrollPos,
    SetScrollRange, SetSoundNoise, SetSwapAreaSize,
    SetViewportExt, SetViewportOrg, SetVoiceAccent, SetVoiceEnvelope,
    SetVoiceNote, SetVoiceQueueSize, SetVoiceSound,
    SetVoiceThreshold, SetWindowExt, SetWindowOrg, SetWindowsHook,
    StartSound, StopSound, SwitchStackBack, SwitchStackTo,
    SyncAllVoices

U
-

    UngetCommChar, UnhookWindowsHook, UnlockSegment

V
-

    ValidateCodeSegments, ValidateFreeSpaces

W
-

    WM\_CTLCOLOR, WNetAddConnection, WNetCancelConnection, WRITE,
    WaitSoundState, WriteComm, WritePrivateProfileSection,
    WritePrivateProfileString, WritePrivateProfileStruct,
    WriteProfileSection, WriteProfileString

Y
-

    Yield
