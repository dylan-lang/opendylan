Some notes on Motley (the Functional Developer type library walker) as of 
Tue May 19 17:29:28  1998

  The translator is invoked at the command line like so:
    motley c:\winnt\system32\inetsrv\asp.dll
  It parses the type library contained in the specified file.  It then
  creates the following files:
    TYPELIB\TYPELIB.hdp, TYPELIB\TYPELIB.lid - project files
    TYPELIB\TYPELIB-library.dylan - library definition
    TYPELIB\TYPELIB-module.dylan - module definition, with all interface
				   types and methods exported
    TYPELIB\TYPELIB.dylan - definitions of types and interfaces
  where TYPELIB is the name extracted from the type library.

  Eventually, we should probably support looking up type libraries in the
  registry (start oleview.exe and expand the Type Libraries node for an
  example) and maybe be able to look up the type library for a particular
  interface name.  These are not hard to do.

  The walker should also probably be integrated with the project wizard.
  Scenario for use: 
    - Invoke the project wizard.
    - Click on a radio button or somesuch to specify that you want to
      translate a type library to generate your new project.
    - Up comes a list of type libraries, and a popup menu or somesuch you
      can use to select an interface (the wizard will then give you its
      type library).
    - The type library is translated.
  A similar model would apply to Collage.

  Currently the walker is based on a generate-and-edit model - the user is
  expected to hack up the generated source as necessary.  Collage will
  hopefully avoid this model.

  Some comments on reference counting with coclasses:
  - For a coclass which implements several interfaces, a function
    make-{name-of-coclass} is defined.  This function takes no arguments
    and creates an instance of the coclass using the coclass's primary
    interface, and returns that instance, plus that instance coerced to
    each of the other interfaces the coclass implements.  The coercions are
    performed using the as method, which calls QueryInterface.  The net
    result is that each of the interfaces returned from
    make-{name-of-coclass} has a single reference and must eventually be
    Released.

  Known bugs/limitations:
  - vtable interfaces are not translated - only dispatch interfaces.
  - Any pointers to C types are not imported from the c-ffi library, but
    are explicitly defined and not exported.  (HD doesn't complain if you
    exclude symbols which don't exist - I'm not sure that it's really
    technically legal, though.)
  - Pointers to pointers to dispatch interfaces are not supported.  I
    suppose they could be <LPDISPATCH*>s...
  - Pointers to typedefs may not be handled correctly.
  - Struct and union types are not translated.
    Dispatch interface methods seem to occasionally use pointers to
    structs/unions.  These methods will get translation errors.
  - Error handling could be improved.
  - Some variant type translations aren't really set up properly/properly
    tested: CY/I8/UI8 (<double-integer>?), ERROR/HRESULT (<HRESULT>), BOOL,
    SAFEARRAY, LPWSTR.
  - Dependencies on interfaces/types defined in other type libraries may
    have to be handled manually.
  - Interface inheritance is not supported particularly well - inherited
    properties and methods are duplicated in subclasses.

Specification file format:

  Origin: COM-type-library
  Type-library: <typelib-path>
  [Project: <destination-project-path>]
  Module: <module-name-to-generate>]
  Module-file: <module-file-to-generate>
  Generate: (dispatch-clients | vtable-clients | 
	     dispatch-servers | vtable-servers | dual-servers)
  Stub-file: <stub-file-to-generate>
  Interfaces: <interfaces-to-translate>
