Startup
=======

When a Dylan library (either a shared library/DLL or executable) is
loaded, a number of initialization tasks need to take place:

- The Dylan run-time facilities, including the garbage collector and
  the main thread's thread environment block (TEB), need to be
  initialized if they haven't already been.
- The final addresses of any :drm:`<symbol>` objects referenced by the
  library need to be resolved, and data locations that reference
  symbols need to be patched to point to the resolved addresses. This
  is part of the "for-system" initialization.
- Top-level forms in each of the library's source files need to be
  evaluated. This is the "for-user" initialization.

The following describes how Dylan libraries are initialized under the
different compiler back-ends.

LLVM
----

Initialization for programs compiled by the LLVM back-end relies on
constructor functions, provided on most platforms to support (among
other things) C++ constructors. The LLVM linker makes use of two
different constructor priorities, system (priority 0) and user
(priority 65535).

The first constructor generated at system priority within the
``_glue.bc`` file generated for the ``dylan`` library is a call to
``_Init_Run_Time``, a C function defined in
``sources/lib/run-time/llvm-runtime-init.c`` to initialize the garbage
collector and other system facilities needed by the Dylan run-time.

For each compile unit that requires it, a constructor at system
priority is generated to do for-system initialization:

- When :drm:`<symbol>` resolution is required, a call to
  ``primitive-symbol-fixup`` is generated, passing a table of symbols
  to resolve and reference addresses to patch.

- If there are any system initializers in the compile unit, calls to
  these are also generated. These normally only occur when a class
  definition depends on non-static values.

The remaining for-user initialization is done in the per-library
startup glue function, named ``_Init_``\ *mangled-library-name*, where
*mangled-library-name* is the Dylan library name transformed by name
mangling. This function, generated in ``_glue.bc`` by ``emit-gluefile``
in ``sources/dfmc/llvm-linker/llvm-gluefile.dylan``, does the following:

1. Checks a flag to see if the library initialization has already been
   done, and exits if it has
#. Sets the initialization flag
#. Calls the library startup glue functions of all libraries
   referenced by this one.
#. Calls the for-user init functions for each compile-unit in the
   library. These contain the code generated for all of the top-level
   forms.
#. Calls the ``%install-boot-symbols`` function defined in
   ``sources/dylan/symbol-table.dylan``. (Dylan library only.)

For executables, execution begins at the usual ``main`` entry
point. The ``main`` function, generated in ``_main.bc`` by
``emit-gluefile`` in ``sources/dfmc/llvm-linker/llvm-gluefile.dylan``,
simply stores its ``argc`` and ``argv`` arguments in ``*argc*`` and
``*argv*`` runtime variables and calls the library startup glue
function.

HARP (Win32)
------------

When a Win32 DLL compiled by the Open Dylan HARP back-end is loaded,
execution starts at *mangled-library-name*\ ``Dll@12``, where
*mangled-library-name* is the Dylan library name transformed by name
mangling. This entry point, generated for each library by
``emit-executable-entry-points`` in
``sources/dfmc/harp-native-cg/linker.dylan``, stores the address of
the library's glue function, named ``_Init_``\ *mangled-library-name*, into the
``_init_dylan_library`` variable and branches to ``_DylanDllEntry@12``.

A `DllMain entry point
<https://msdn.microsoft.com/en-us/library/windows/desktop/ms682583%28v=vs.85%29.aspx>`_
receives three arguments: the module handle of the DLL, a reason code,
and a flag that indicates whether the DLL was loaded statically or
dynamically. The ``_DylanDllEntry@12`` routine, generated in ``sources/harp/x86-windows-rtg/ffi-barrier.dylan``, looks at the reason
code to determine what to do:

- For process attach, the routine:

  1. Stores the module handle in a DLL-local ``_module_hInstance`` variable.
  #. Allocates and initializes the TEB and GC-TEB, and initializes the
     garbage collector by calling ``dylan_init_memory_manager``
     and ``dylan_mm_register_thread`` (``DxDYLAN.dll`` only).
  #. Calls ``_dylan_initialize`` (described below)

- For thread attach, the routine clears the TLV vector slot of the TEB
  (``DxDYLAN.dll`` only).

- For thread detach, the routine:

  1. Deregisters the thread by calling
     ``dylan_mm_deregister_thread_from_teb`` (``DxDYLAN.dll`` only).
  #. Dealocates the TEB (``DxDYLAN.dll`` only)

- For process detach,

  1. Deregisters the master thread by calling
     ``dylan_mm_deregister_thread_from_teb`` (``DxDYLAN.dll`` only).
  #. Deinitializes the garbage collector by calling
     ``dylan_shut_down_memory_manager`` (``DxDYLAN.dll`` only).

The ``_dylan_initialize`` function (generated in
``sources/harp/native-rtg/ffi-barrier.dylan``):

1. Calls the runtime's ``init_dylan_data`` function, which:

   1. Calls ``primitive_fixup_unimported_dylan_data``
   #. Calls ``primitive_fixup_imported_dylan_data``
   #. Calls ``primitive_register_traced_roots``

#. Sets the FFI barrier state to ``$inside-dylan``.
#. Calls ``dylan_init_thread_local`` (which tail-calls the
   ``dylan_init_thread`` C function defined in
   ``sources/lib/run-time/exceptions.c``) passing a pointer to the
   ``call_init_dylan`` function. This function in turn is called as an
   MPS trampoline.
#. Resets the FFI barrier state to ``$outside-dylan``.

The ``call_init_dylan`` function retrieves the value of the
``_init_dylan_library`` variable (pointing to a library startup glue
function) and performs an indirect call.

Similarly, an Win32 executable starts execution at
*mangled-library-name*\ ``Exe``. This routine, which is also generated by
``emit-executable-entry-points``, stores the address of
``_Init_``\ *mangled-library-name* in the
``_init_dylan_library`` variable and branches to ``dylan_main``.
