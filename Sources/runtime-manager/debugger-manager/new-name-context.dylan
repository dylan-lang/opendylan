module:        dm-internals
synopsis:      Debugger name mangling and de-mangling.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// $BASIC-MANGLER
define constant $basic-mangler = make(<mangler>);

///// $CONSTANT-MANGLER
define constant $constant-mangler
  = make(<mangler-with-options>, constant-object-extension: #t);

///// $WRAPPER-MANGLER
define constant $wrapper-mangler
  = make(<mangler-with-options>, wrapper-object-extension: #t);

///// $IEP-MANGLER
define constant $iep-mangler
  = make(<mangler-with-options>, iep-extension: #t);

///// $DEMANGLER
define constant $demangler = make(<demangler>);


///// <DYLAN-NAME-CONTEXT> (Place holder, maybe, for <&module>)
//    As currently described in the DM document.

define class <dylan-name-context> (<object>)

  slot context-library :: <byte-string> = "dylan",
    init-keyword: library:;

  slot context-module :: <byte-string> = "dylan",
    init-keyword: module:;

end class;


///// $DYLAN-INTERNAL
//    A frequently-used name context - the "dylan" library and "internal"
//    module.

define constant $dylan-internal
  = make(<dylan-name-context>, module: "internal");

define constant $dylan-extensions
  = make(<dylan-name-context>, library: "dylan", module: "dylan-extensions");


define method demangle-qualified-name 
  (s :: <byte-string>) 
     => (lib :: <byte-string>,
         mod :: <byte-string>,
         name :: <byte-string>)
  let (name, mod, lib) = demangle-binding-spread($demangler, s);
  values (lib | "",
          mod | "", 
          name);  
end method;

define method demangle-dylan-name
   (full-mangled-name :: <byte-string>)
      => (name-part :: <byte-string>,
          module-part :: <byte-string>,
          library-part :: <byte-string>,
          method-name? :: <boolean>, method-iep? :: <boolean>,
          method-library-part :: false-or(<byte-string>),
          method-number-part :: false-or(<byte-string>))
  let (library-part, module-part, name-part)
    = demangle-qualified-name(full-mangled-name);
  let (constant?, wrapper?, method-iep?, method-name?)
    = demangler-extract-characteristics($demangler, full-mangled-name);
  let (method-library-part, method-number-part)
    = if (method-name?)
        demangler-extract-method-details($demangler, full-mangled-name)
      else
        values(#f, #f)
      end if;
  values
    (name-part,
     module-part,
     library-part,
     method-name?,
     method-iep?,
     method-library-part,
     method-number-part)
end method;

define method demangle-local-dylan-name
   (full-mangled-name :: <byte-string>) => (demang :: <byte-string>)
  demangle-name-locally($demangler, full-mangled-name)
end method;

define method mangle-local-dylan-name
    (s :: <byte-string>) => (mangled :: <byte-string>)
  mangle-name-locally($basic-mangler, s)
end method;

define method mangle-in-context 
  (s :: <byte-string>, cxt :: <dylan-name-context>,
   #key as-wrapper? = #f,
        as-static-object? = #f,
        as-entry-point? = #f) 
     => (mangled :: <byte-string>)
  let lib = cxt.context-library;
  let mod = cxt.context-module;
  if (as-wrapper?)
    mangle-binding-spread($wrapper-mangler, s, mod, lib);
  elseif (as-static-object?)
    mangle-binding-spread($constant-mangler, s, mod, lib);
  elseif (as-entry-point?)
    mangle-binding-spread($iep-mangler, s, mod, lib);
  else
    mangle-binding-spread($basic-mangler, s, mod, lib);
  end if;
end method;


///// ******** INTEPRETING MANGLES ********
//    Looks at a mangled name to decipher what kind of object is being
//    referenced by it.

/*
define method mangled-name-is-wrapper?
    (name :: <byte-string>) => (answer :: <boolean>)
  let (constant?, wrapper?, iep?, method?)
    = demangler-extract-characteristics($demangler, name);
  wrapper?
end method;
*/

/*
define method mangled-name-is-static?
    (name :: <byte-string>) => (answer :: <boolean>)
  let (constant?, wrapper?, iep?, method?)
    = demangler-extract-characteristics($demangler, name);
  constant?
end method;
*/

define method mangled-name-is-method?
    (name :: <byte-string>) => (answer :: <boolean>)
  let (constant?, wrapper?, iep?, method?)
    = demangler-extract-characteristics($demangler, name);
  method?
end method;

define method mangled-name-is-iep?
    (name :: <byte-string>) => (answer :: <boolean>)
  let (constant?, wrapper?, iep?, method?)
    = demangler-extract-characteristics($demangler, name);
  iep?
end method;

/*
define method mangled-name-is-C-name?
    (name :: <byte-string>) => (answer :: <boolean>)
  name[0] == '_';
end method;
*/

///// MAPPINGS
//    These functions map mangled names into other mangled names. For
//    example, given an entry point, get to the name for the method object,
//    and from that get to the name for the generic function object.


///// MANGLE-MAP-IEP-TO-METHOD
//    Just remove the capital I from the end!

define method mangle-map-iep-to-method
    (iep-name :: <byte-string>) => (method-name :: <byte-string>)
  demangler-extract-callable-object-name($demangler, iep-name)
end method;


///// MANGLE-MAP-METHOD-TO-GENERIC
//    Knock off everything including and following the first capital 'M'.

define method mangle-map-method-to-generic
    (method-name :: <byte-string>) => (generic-name :: <byte-string>)
  demangler-extract-generic-function-name
     ($demangler, method-name);
end method;


///// OBTAIN-COMPONENT-NAME
//    A name context contains the name of a dylan library. Often, this
//    needs to be mapped to the name of a shared object (or DLL),
//    which should be performed via this function.

define open generic obtain-component-name
    (application :: <debug-target>, libname :: <string>)
       => (name :: <string>);

define method obtain-component-name 
    (application :: <debug-target>, libname :: <string>) 
       => (name :: <string>)
  let cc = application.debug-target-compilation-context;
  if (cc)
    let subcontext =
      library-name-context(cc,
                           application,
                           libname);
    if (subcontext)
      subcontext.compilation-context-component-name |
      libname
    else
      libname
    end if
  else
    libname
  end if;
end method;


///// MANGLED-NAME-TO-REMOTE-LIBRARY
//    Given a dylan mangled name, find a <remote-library> object that
//    should be defining it. 

define method mangled-name-to-remote-library
    (application :: <debug-target>, name :: <byte-string>)
         => (search-library :: <remote-library>)
  let libname
    = demangler-extract-library-name($demangler, name);
  element(application.library-component-names,
          name,
          default: application.application-executable)
end method;
