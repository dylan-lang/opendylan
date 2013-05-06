module:      dm-internals
synopsis:    Searching for, and tabulating, named static objects used by
             the DM.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <STATIC-OBJECT-DIRECTORY>
//    A class used to encapsulate information about statically-heaped
//    objects created by the dylan library, and used by the debugger
//    manager for various purposes.

define class <static-object-directory> (<object>)

  constant slot directory-static-objects-by-module :: <string-table>
    = make(<string-table>);

  constant slot directory-wrappers-by-module :: <string-table>
    = make(<string-table>);

  constant slot directory-wrappers-by-page :: <table>
    = make(<table>);

  slot directory-wrappers-initialized? :: <boolean>,
    init-value: #f;

  constant slot directory-keywords-by-id :: <table>
    = make(<table>);

  slot directory-keywords-initialized? :: <boolean>,
    init-value: #f;

  slot directory-runtime-symbols :: false-or(<string-table>) = #f;

  slot canonical-true-object :: <remote-value> = as-remote-value(0);
  slot canonical-false-object :: <remote-value> = as-remote-value(0);
  slot canonical-empty-list-object :: <remote-value> = as-remote-value(0);
  slot canonical-unbound-object :: <remote-value> = as-remote-value(0);
end class;


///// RUNTIME-BOOLEAN-MARKERS
//    Returns the two booleans as <remote-value>.

define method dylan-runtime-boolean-markers
    (application :: <debug-target>)
  => (true :: <remote-value>, false :: <remote-value>)
  values
     (application.static-object-directory.canonical-true-object,
      application.static-object-directory.canonical-false-object)
end method;

///// INITIALIZE (<STATIC-OBJECT-DIRECTORY>)

define sealed method initialize
    (sod :: <static-object-directory>, #key, #all-keys) => ()

  // Make subtables for all modules within the dylan library that we
  // happen to care about.

  sod.directory-static-objects-by-module["dylan"] := make(<string-table>);
  sod.directory-static-objects-by-module["internal"] := make(<string-table>);
  sod.directory-wrappers-by-module["dylan"] := make(<string-table>);
  sod.directory-wrappers-by-module["internal"] := make(<string-table>);
  sod.directory-wrappers-by-module["dylan-extensions"] := make(<string-table>);
  sod.directory-wrappers-by-module["threads"] := make(<string-table>);

  // Introduce some convenient local pointers to prevent performing
  // the same hash over and over again.

  let objects-in-internal-module :: <string-table>  =
    sod.directory-static-objects-by-module["internal"];
  let objects-in-dylan-module :: <string-table> =
    sod.directory-static-objects-by-module["dylan"];
  let wrappers-in-internal-module :: <string-table> =
    sod.directory-wrappers-by-module["internal"];
  let wrappers-in-dylan-module :: <string-table> =
    sod.directory-wrappers-by-module["dylan"];
  let wrappers-in-dylan-extensions-module :: <string-table> =
    sod.directory-wrappers-by-module["dylan-extensions"];
  let wrappers-in-threads-module :: <string-table> =
    sod.directory-wrappers-by-module["threads"];

  // And now add the elements.

  objects-in-dylan-module["<integer>"]         := #"not-located";
  objects-in-dylan-module["<character>"]       := #"not-located";
  objects-in-dylan-module["<restart>"]         := #"not-located";
  objects-in-dylan-module["<abort>"]           := #"not-located";
  objects-in-dylan-module["<object>"]          := #"not-located";
  objects-in-internal-module["%false"]         := #"not-located";
  objects-in-internal-module["%true"]          := #"not-located";
  objects-in-internal-module["%unbound"]       := #"not-located";
  objects-in-internal-module["%empty-list"]    := #"not-located";
  objects-in-internal-module["%empty-vector"]  := #"not-located";


  wrappers-in-dylan-module["<pair>"]
    := pair(#"not-located", $pair-type);

  wrappers-in-dylan-module["<single-float>"]
    := pair(#"not-located", $single-float-type);

  wrappers-in-dylan-module["<double-float>"]
    := pair(#"not-located", $double-float-type);

  wrappers-in-dylan-module["<byte-string>"]
    := pair(#"not-located", $string-type);

  wrappers-in-dylan-module["<simple-object-vector>"]
    := pair(#"not-located", $vector-type);

  wrappers-in-dylan-extensions-module["<object-deque>"]
    := pair(#"not-located", $deque-type);

  wrappers-in-dylan-module["<empty-list>"]
    := pair(#"not-located", $empty-list-type);

  wrappers-in-dylan-module["<symbol>"]
    := pair(#"not-located", $symbol-type);

  wrappers-in-dylan-module["<class>"]
    := pair(#"not-located", $class-type);

  wrappers-in-dylan-module["<generic-function>"]
    := pair(#"not-located", $generic-function-type);

  wrappers-in-dylan-module["<simple-error>"]
    := pair(#"not-located", $simple-error-type);

  wrappers-in-dylan-module["<simple-warning>"]
    := pair(#"not-located", $simple-warning-type);

  wrappers-in-dylan-module["<simple-restart>"]
    := pair(#"not-located", $simple-restart-type);

  wrappers-in-dylan-module["<singleton>"]
    := pair(#"not-located", $singleton-type);

  wrappers-in-dylan-module["<table>"]
    := pair(#"not-located", $table-type);

  wrappers-in-dylan-module["<object-table>"]
    := pair(#"not-located", $table-type);

  wrappers-in-internal-module["<function-class>"]
    := pair(#"not-located", $class-type);

  wrappers-in-internal-module["<value-class>"]
    := pair(#"not-located", $class-type);

  wrappers-in-internal-module["<virtual-slot-descriptor>"]
    := pair(#"not-located", $virtual-slot-descriptor-type);

  wrappers-in-internal-module["<class-slot-descriptor>"]
    := pair(#"not-located", $class-slot-descriptor-type);

  wrappers-in-internal-module
    ["<each-subclass-slot-descriptor>"] 
      := pair(#"not-located", $each-subclass-slot-descriptor-type);

  wrappers-in-internal-module["<simple-closure-method>"]
    := pair(#"not-located", $simple-method-type);

  wrappers-in-internal-module["<standard-object-table>"]
    := pair(#"not-located", $table-type);

  wrappers-in-internal-module["<symbol-table>"]
    := pair(#"not-located", $table-type);

  wrappers-in-internal-module["<simple-object-array>"]
    := pair(#"not-located", $array-type);

  wrappers-in-dylan-extensions-module["<simple-element-type-array>"]
    := pair(#"not-located", $limited-array-type);

  wrappers-in-dylan-extensions-module["<stretchy-element-type-vector>"]
    := pair(#"not-located", $limited-stretchy-vector-type);

  wrappers-in-dylan-extensions-module["<simple-element-type-vector>"]
    := pair(#"not-located", $limited-simple-vector-type);

  wrappers-in-internal-module["<signature+values>"]
    := pair(#"not-located", $signature+values-type);

  wrappers-in-internal-module
    ["<signature+values+rest-value>"]
      := pair(#"not-located", $signature+values+rest-value-type);

  wrappers-in-internal-module["<signature+rest-value>"]
    := pair(#"not-located", $signature+rest-value-type);

  wrappers-in-internal-module["<keyword-signature+values>"]
    := pair(#"not-located", $keyword-signature+values-type);

  wrappers-in-internal-module
    ["<keyword-signature+values+rest-value>"]
      := pair(#"not-located", $keyword-signature+values+rest-value-type);

  wrappers-in-internal-module
    ["<keyword-signature+rest-value>"]
    := pair(#"not-located", $keyword-signature+rest-value-type);

  wrappers-in-internal-module["<finite-range>"]
    := pair(#"not-located", $finite-range-type);

  wrappers-in-internal-module["<infinite-range>"]
    := pair(#"not-located", $infinite-range-type);

  wrappers-in-internal-module["<constant-range>"]
    := pair(#"not-located", $constant-range-type);

  wrappers-in-internal-module["<empty-range>"]
    := pair(#"not-located", $empty-range-type);

  wrappers-in-dylan-extensions-module["<sealed-generic-function>"]
    := pair(#"not-located", $generic-function-type);

  wrappers-in-dylan-extensions-module["<incremental-generic-function>"]
    := pair(#"not-located", $generic-function-type);

  wrappers-in-dylan-extensions-module["<double-integer>"]
    := pair(#"not-located", $double-integer-type);

  wrappers-in-dylan-extensions-module["<machine-word>"]
    := pair(#"not-located", $machine-integer-type);

  wrappers-in-dylan-extensions-module["<union>"]
    := pair(#"not-located", $union-type);

  wrappers-in-dylan-extensions-module["<subclass>"]
    := pair(#"not-located", $subclass-type);

  wrappers-in-dylan-extensions-module["<bottom-type>"]
    := pair(#"not-located", $bottom-type);

  wrappers-in-dylan-extensions-module
    ["<instance-slot-descriptor>"]
      := pair(#"not-located", $instance-slot-descriptor-type);

  wrappers-in-dylan-extensions-module
    ["<repeated-slot-descriptor>"]
      := pair(#"not-located", $repeated-slot-descriptor-type);

  wrappers-in-dylan-extensions-module
    ["<simple-method>"]
      := pair(#"not-located", $simple-method-type);

  wrappers-in-dylan-extensions-module
    ["<keyword-method>"]
       := pair(#"not-located", $keyword-method-type);

  wrappers-in-dylan-extensions-module
    ["<keyword-closure-method>"]
      := pair(#"not-located", $keyword-method-type);

  wrappers-in-dylan-extensions-module
    ["<accessor-method>"]
      := pair(#"not-located", $accessor-method-type);

  wrappers-in-dylan-extensions-module
    ["<getter-accessor-method>"]
      := pair(#"not-located", $accessor-method-type);

  wrappers-in-dylan-extensions-module
    ["<setter-accessor-method>"]
      := pair(#"not-located", $accessor-method-type);

  wrappers-in-dylan-extensions-module
    ["<single-accessor-method>"]
      := pair(#"not-located", $accessor-method-type);

  wrappers-in-dylan-extensions-module
    ["<repeated-accessor-method>"]
      := pair(#"not-located", $accessor-method-type);

  wrappers-in-dylan-extensions-module
    ["<stretchy-object-vector>"]
      := pair(#"not-located", $stretchy-vector-type);

  wrappers-in-dylan-extensions-module
    ["<signature>"]
      := pair(#"not-located", $signature-required-only-type);

  wrappers-in-dylan-extensions-module
    ["<object-signature>"]
      := pair(#"not-located", $signature-required-only-type);

  wrappers-in-dylan-extensions-module
    ["<keyword-signature>"]
      := pair(#"not-located", $keyword-signature-type);

  wrappers-in-dylan-extensions-module["<string-table>"]
    := pair(#"not-located", $string-table-type);

  wrappers-in-threads-module["<thread>"]
    := pair(#"not-located", $thread-type);

  wrappers-in-threads-module["<synchronous-thread>"]
    := pair(#"not-located", $thread-type);

  wrappers-in-internal-module["<temp-pointer-type-class>"]
      := pair(#"not-located", $class-type);
  wrappers-in-internal-module["<c-pointer-type-class>"]
      := pair(#"not-located", $class-type);
  wrappers-in-internal-module["<c-automatic-pointer-designator-class>"]
      := pair(#"not-located", $class-type);
  wrappers-in-internal-module["<c-mapped-designator-class>"]
      := pair(#"not-located", $class-type);
  wrappers-in-internal-module["<c-struct/union-designator-class>"]
      := pair(#"not-located", $class-type);
  wrappers-in-internal-module["<c-struct-designator-class>"]
      := pair(#"not-located", $class-type);
  wrappers-in-internal-module["<c-union-designator-class>"]
      := pair(#"not-located", $class-type);

  sod.directory-keywords-by-id[#"format-string"] := #"not-located";
  sod.directory-keywords-by-id[#"format-arguments"] := #"not-located";

end method;


///// LOOKUP-STATIC-OBJECT
//    Finds the nonrelocatable address of a named object from the dylan
//    library, obtaining it from the cache if possible, otherwise filling
//    in the cache.
//    NB: In this function, and the other function in this file, there is
//        minimal error checking. These functions are internal to the DM's
//        implementation, and we expect all lookup operations to succeed.

define method lookup-static-object
    (application :: <debug-target>, name :: <string>, module :: <string>)
  => (address :: <remote-value>)
  let sod = application.static-object-directory;
  sod.directory-static-objects-by-module[module][name];
end method;


///// LOOKUP-STATIC-WRAPPER
//    Similar to LOOKUP-STATIC-OBJECT, but specialized on wrappers.

define method lookup-static-wrapper
    (application :: <debug-target>, name :: <string>, module :: <string>)
  => (address :: <remote-value>, type-descr :: <object-type-description>)
  let sod = application.static-object-directory;
  let (val-type-pair) =
    sod.directory-wrappers-by-module[module][name];
  values(head(val-type-pair), tail(val-type-pair))
end method;


///// GET-TYPE-FROM-WRAPPER
//    Given a <remote-value> known to point to a wrapper, compare it with
//    the wrappers in the static object directory, and return an
//    <object-type-description> for the wrapper.

define method get-type-from-wrapper
    (application :: <debug-target>, wrapper :: <remote-value>)
  => (type :: <object-type-description>)
  let path = application.debug-target-access-path;
  let sod = application.static-object-directory;
  if (sod.directory-wrappers-initialized?)
    let (page, offset) = page-relative-address(path, wrapper);
    let subtable = element(sod.directory-wrappers-by-page, page, default: #f);
    if (subtable)
      let type-d = element(subtable, offset, default: #f);
      type-d | $user-defined-type
    else
      $user-defined-type
    end if
  else
    $unknown-type
  end if;
end method;


///// LOOKUP-STATIC-KEYWORD
//    This time we are looking for the address of a <symbol>.

define method lookup-static-keyword
    (application :: <debug-target>, keyword :: <symbol>)
  => (address :: <remote-value>)
  element(application.static-object-directory.directory-keywords-by-id,
          keyword,
          default: as-remote-value(0))
end method;


///// INITIALIZE-STATIC-OBJECTS
//    Searches for all of the pre-defined static objects in the static
//    object directory.

define method initialize-static-objects
    (application :: <debug-target>) => ()
  let sod = application.static-object-directory;
  let context = make(<dylan-name-context>, module: "dylan", library: "dylan");
  let component = application.application-dylan-library;
  let illegal-value = as-remote-value(0);
  for (subtable keyed-by mod in sod.directory-static-objects-by-module)
    context.context-module := mod;
    for (dummy keyed-by object-name in subtable)
      let predetermined = subtable[object-name];
      let (value, address) =
        resolve-dylan-name(application, object-name, context,
                           indirect?: #f, library: component) |
        resolve-dylan-name(application, object-name, context,
                           indirect?: #t, library: component);
      subtable[object-name] := value | 
                               if (predetermined == #"not-located")
                                 illegal-value
                               else
                                 predetermined
                               end if;
    end for
  end for;
  sod.canonical-true-object := 
    lookup-static-object(application, "%true", "internal");
  sod.canonical-false-object := 
    lookup-static-object(application, "%false", "internal");
  sod.canonical-empty-list-object := 
    lookup-static-object(application, "%empty-list", "internal");
  sod.canonical-unbound-object := 
    lookup-static-object(application, "%unbound", "internal");
end method;


///// INITIALIZE-STATIC-WRAPPERS
//    Searches for all of the pre-defined static wrapper objects in the
//    static object directory.

define method initialize-static-wrappers
    (application :: <debug-target>) => ()

  let path = application.debug-target-access-path;
  let sod = application.static-object-directory;
  let page-table = sod.directory-wrappers-by-page;
  let context = make(<dylan-name-context>, library: "dylan", module: "dylan");
  let dylan-library = application.application-dylan-library;
  let illegal-address = as-remote-value(0);

  local method add-page-relative-wrapper-entry 
           (page :: <integer>, offset :: <integer>, 
            type :: <object-type-description>) => ()
          let offset-table = element(page-table, page, default: #f);
          unless (offset-table)
            offset-table := make(<table>);
            page-table[page] := offset-table;
          end unless;
          offset-table[offset] := type;
        end method;

  for (subtable keyed-by mod-name in sod.directory-wrappers-by-module)
    context.context-module := mod-name;
    for (address-type-pair keyed-by wrapper-name in subtable)
      let sym =
        find-symbol(path,
                    mangle-in-context(wrapper-name, context,
                                      as-wrapper?: #t, as-static-object?: #t),
                    library: dylan-library);
      let wrapper-address =
        if (sym) sym.remote-symbol-address else illegal-address end if;
      head(address-type-pair) := wrapper-address;
      let (wrapper-page, wrapper-offset) = 
        page-relative-address(path, wrapper-address);
      add-page-relative-wrapper-entry(wrapper-page,
                                      wrapper-offset,
                                      tail(address-type-pair));
    end for;
  end for;
  sod.directory-wrappers-initialized? := #t;
end method;


///// INITIALIZE-STATIC-KEYWORDS
//    Searches for all of the pre-defined static keyword objects in the
//    static object directory. This initializer additionally requires
//    a <remote-thread> that it can use to call spy functions on.

define method initialize-static-keywords
    (application :: <debug-target>, spy-thread :: <remote-thread>) => ()
  let sod = application.static-object-directory;
  let illegal-value = as-remote-value(0);
  sod.directory-keywords-by-id[#"format-string"] :=
    run-spy-on-thread(application, spy-thread, 
                      application.dylan-spy.spy-format-string-keyword) |
    illegal-value;
  sod.directory-keywords-by-id[#"format-arguments"] :=
    run-spy-on-thread(application, spy-thread, 
                      application.dylan-spy.spy-format-arguments-keyword) |
    illegal-value;
  sod.directory-keywords-initialized? := #t;
end method;


define method lookup-runtime-symbol
    (application :: <debug-target>, name :: <string>)
  => (address :: <remote-symbol>)
  let path = application.debug-target-access-path;
  let sod = application.static-object-directory;
  let runtime-symbols :: <string-table> =
    sod.directory-runtime-symbols
    | (sod.directory-runtime-symbols := make(<string-table>));

  element(runtime-symbols, name, default: #f)
    | (element(runtime-symbols, name) := 
	 find-symbol(path, name,
		     library: application.application-dylan-runtime-library));
end method;
