module:      devel-dbg-ui
synopsis:    Printing stack frames in the console debugger.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DEBUGGER-PRINT-STACK-FRAME
//    Given an <application-stack-frame>, print a readable description
//    of it to the console.
//    If 'full?' is #t, any lexical variables are also printed.
//    All frames are numbered for the user's convenience. The number
//    to display along with the frame is given by 'index'


// Catch all method.

define method debugger-print-stack-frame
  (frame :: <application-stack-frame>, index :: <integer>, #key full? = #f) 
    => ()
  format-out ("#%d [Couldn't interpret this stack frame.]\n", index);
end method;


// Unwind protect frame. Just record the fact that it's there!

define method debugger-print-stack-frame
  (frame :: <unwind-protect-frame>, index :: <integer>, #key full? = #f) 
    => ()
  format-out ("#%d [Dylan Cleanup Frame]\n", index);
end method;


define method debugger-print-stack-frame
   (frame :: <call-frame>, index :: <integer>, #key full? = #f)
      => ()
  let (frame-function, function-model) =
    call-frame-function(*open-application*, frame);
  let source-locator =
    remote-address-source-location
       (*open-application*,
        call-frame-instruction-pointer(*open-application*, frame)) |
    call-frame-nearest-source-locator(*open-application*, frame);

  let printable-source-locator =
    string-representation-of-source-locator(source-locator);

  let path =
    *open-application*.debug-target-access-path;

  local method format-one-variable
                 (name :: <string>, value :: <remote-value>, #key dylan? = #t)
                   => (printed :: <string>)
          let hex-rep = 
            format-to-string("0x%s",
              remote-value-as-string(path, value, 16));
          let printed-rep =
            if (dylan?)
              debugger-print-object
                (*open-application*, value, length: 20, level: 3)
            else
              hex-rep
            end if;
          if (*running-in-batch-mode?*)
            format-to-string("    %s = %s", name, printed-rep);
          else
            format-to-string("    $%d : %s  %s = %s",
                             add-history-value(value),
                             hex-rep,
                             name,
                             printed-rep);
          end if
        end method;

  if (~dylan-call-frame?(*open-application*, frame))
    format-out("#%d %s\n  [Foreign Call Frame 0x%s:0x%s]\n  %s\n",
               index,
               if (frame-function)
                 if (frame-function.remote-symbol-library)
                   concatenate (as-uppercase
                     (frame-function.remote-symbol-library.library-core-name),
                     "!",
                     frame-function.remote-symbol-name)
		 else
                   concatenate("INTERACTIVE!", 
                               frame-function.remote-symbol-name);
		 end if
               else
                 "????"
               end if,
               remote-value-as-string
                 (path,
                  call-frame-frame-pointer(*open-application*, frame),
                  16),
               remote-value-as-string
                 (path,
                  call-frame-instruction-pointer(*open-application*, frame),
                  16),
               printable-source-locator);
    if (full?)
      let (live-variables, live-values) =
        live-frame-lexical-variables(*open-application*, frame);
      for (i from 0 below size(live-variables))
        format-out("%s\n",
                   format-one-variable
                     (live-variables[i].lexical-variable-name,
                      live-values[i],
                      dylan?: #f))
      end for;
    end if;

  else

    format-out("#%d %s\n  [Dylan Call Frame 0x%s:0x%s]\n  %s\n",
               index,
               if (frame-function)
                 let printable-lib =
                   if (frame-function.remote-symbol-library)
                     as-uppercase
                      (frame-function.remote-symbol-library.library-core-name);
                   else
                     "INTERACTIVE"
                   end if;
                 let (function-name-part, function-module-part,
                      function-library-part, function-is-method?,
                      function-is-iep?, method-module-part, 
                      method-number-part) =
                   demangle-dylan-name(frame-function.remote-symbol-name);
                 let printable-name = function-name-part;
                 unless (function-module-part = "")
                   printable-name := 
                     concatenate(printable-name, ":", function-module-part);
                 end unless;
                 unless (function-library-part = "")
                   printable-name :=
                     concatenate(printable-name, ":", function-library-part);
                 end unless;
                 if (function-is-method?)
                   printable-name :=
                     concatenate(printable-name, "#", method-number-part);
                 end if;
                 concatenate(printable-lib, "!", printable-name);
               else
                 "????"
               end if,
               remote-value-as-string
                 (path,
                  call-frame-frame-pointer(*open-application*, frame),
                  16),
               remote-value-as-string
                 (path,
                  call-frame-instruction-pointer(*open-application*, frame),
                  16),
               printable-source-locator);
    if (full?)
      let (live-names, live-types, live-models, live-values) =
        active-dylan-lexical-variables(*open-application*, frame);
      for (i from 0 below size(live-names))
        format-out("%s\n",
                   format-one-variable
                     (live-names[i], live-values[i], dylan?: #t))
      end for;
    end if;
  end if;
end method;

define method string-representation-of-source-locator
    (loc :: <object>) => (rep :: <string>)
  "{It's a source location, but it's a little bit....strange}"
end method;

define method string-representation-of-source-locator
    (loc == #f) => (rep :: <string>)
  "{Unknown source location}"
end method;

define method string-representation-of-source-locator
    (source-location :: <line-source-location>) => (rep :: <string>)
  let source-record = source-location.source-location-source-record;
  let (filename, line-number) = 
    source-line-location(source-record, 
                         source-location.source-location-start-line);
  unless (filename)
    filename := "???.dylan"
  end unless;
  format-to-string("{Line %d of %s}", line-number, filename);
end method;

define method string-representation-of-source-locator
    (loc :: <source-locator>) => (rep :: <string>)
  format-to-string("{Line %d of %s}", loc.source-locator-linenumber,
                                      loc.source-locator-file);
end method;

/*
///// GLOBAL VARIABLES FOR STACK FRAME FILTERS.
//    This is a bit of a hack, but stack frame filtering is a bit of an
//    emergency request.

define variable *show-unwind-protects?* = #f;
define variable *show-foreign-calls?* = #t;
define variable *show-runtime-primitives?* = #f;

// *filtered-context*
// A sequence of pairs of strings. The head of each pair is a module name,
// the tail of each pair is a library name. Together, these form a namespace
// that we are not interested in.

define variable *filtered-contexts* = make(<stretchy-vector>, size: 0);
define variable *filtered-dlls* = make(<stretchy-vector>, size: 0);
define variable *show-unnamed-frames?* = #f;


///// $RUNTIME-PRIMITIVE-NAMES
//    A list of names of functions that are considered "runtime primitives".
//    These can be filtered from stack traces.

define constant $runtime-primitive-names =
  vector ("_dylan_init_thread",
          "_dylan_init_process",
          "_hacks__main",
          "primitive_call_first_dylan_iep",
          "KPinvoke_engine_nodeYdispatch_engineVdylanI",
          "general_engine_node_n",
          "general_engine_node_n_optionals",
          "primitive_break",
          "primitive_debug_message",
          "primitive_nlx",
          "call_dylan_function",
          "call_dylan_function_returning_all_values");


///// LOCATE-ALL-RUNTIME-PRIMITIVES
//    Searches for <remote-symbol> objects for all runtime functions
//    named in $runtime-primitive-names, and installs them in a slot
//    in the <application> object. This means that functions can later
//    be compared just by address, which is faster than continual name
//    comparisons.

define method locate-all-runtime-primitives
    (application :: <application>) => ()
  let seq = make(<stretchy-vector>, size: 0);
  let lib = core-name-to-library(application, "DYLAN");
  let path = application.debug-target-access-path;
  for (name in $runtime-primitive-names)
    let sym = find-symbol(path, name, library: lib);
    if (sym)
      add!(seq, sym);
    end if;
  end for;
  application.runtime-primitive-symbol-set := seq;
end method;


///// FUNCTION-NAME-IS-RUNTIME-PRIMITIVE?
//    Given a mangled function name, decide whether it is a runtime
//    primitive. This is probably very inefficient, but the plan first
//    is to get it working.

define method function-name-is-runtime-primitive?
    (name :: <remote-symbol>) => (answer :: <boolean>)

  // If we need to, locate all functions that correspond to runtime
  // primitives.

  unless (*open-application*.runtime-primitive-symbol-set)
    locate-all-runtime-primitives(*open-application*);
  end unless;

  // And now, compare the address of this symbol with the runtime
  // primitives.

  let found = #f;
  let i = 0;
  let limit = size(*open-application*.runtime-primitive-symbol-set);
  while ((~found) & (i < limit))
    let sym = *open-application*.runtime-primitive-symbol-set[i];
    found := (sym.remote-symbol-address = name.remote-symbol-address);
    i := i + 1;
  end while;
  found;
end method;


///// FUNCTION-NAME-IS-IN-FILTERED-DLL?
//    Given a <remote-symbol>, decide whether it is running in a DLL that
//    we aren't interested in.

define method function-name-is-in-filtered-DLL?
    (name :: <remote-symbol>) => (answer :: <boolean>)
  member?(name.remote-symbol-library, *filtered-dlls*);
end method;


///// FUNCTION-NAME-IS-IN-FILTERED-NAMESPACE?
//    Given a <remote-symbol>, calculates whether its namespace has been
//    filtered out.
//    This is a complete hack, and deserves death.

define method function-name-is-in-filtered-namespace?
    (name :: <remote-symbol>) => (answer :: <boolean>)
  let i = 0;
  let limit = size(*filtered-contexts*);
  let found = #f;
  while ((~found) & (i < limit))
    let mod = head(*filtered-contexts*[i]);
    let lib = tail(*filtered-contexts*[i]);
    let substring = concatenate("Y", mod, "V", lib);
    found := subsequence-position(name.remote-symbol-name, substring);
    i := i + 1;
  end while;
  found;
end method;


///// IS-ALREADY-FILTERED-NAMESPACE?
//    Decides whether a module/library pair is already in the filter
//    list.

define method is-already-filtered-namespace?
    (modname :: <string>, libname :: <string>) => (answer :: <boolean>)
  let i = 0;
  let limit = size(*filtered-contexts*);
  let found = #f;
  while ((~found) & (i < limit))
    let thismod = head(*filtered-contexts*[i]);
    let thislib = tail(*filtered-contexts*[i]);
    found := ((thismod = modname) & (thislib = libname));
    i := i + 1;
  end while;
  found;
end method;


///// ADD-FILTERED-NAMESPACE
//    If necessary, adds a module/library pair to the filter list.

define method add-filtered-namespace
   (modname :: <string>, libname :: <string>) => ()
  unless (is-already-filtered-namespace?(modname, libname))
    add!(*filtered-contexts*, pair(modname, libname))
  end unless
end method;


///// REMOVE-FILTERED-NAMESPACE
//    If necessary, removes a module/library pair from the filter list.

define method remove-filtered-namespace
    (modname :: <string>, libname :: <string>) => ()
  let new-set = make(<stretchy-vector>, size: 0);
  for (mod-lib in *filtered-contexts*)
    unless ((head(mod-lib) = modname) & (tail(mod-lib) = libname))
      add!(new-set, mod-lib);
    end unless;
  end for;
  *filtered-contexts* := new-set;
end method;


///// IS-ALREADY-FILTERED-DLL?
//    Decides whether a <remote-library> needs to be added to the
//    filter list. This is actually trivial - but abstraction rules!

define method is-already-filtered-DLL?
    (dll-object :: <remote-library>) => (answer :: <boolean>)
  member?(dll-object, *filtered-dlls*);
end method;


///// ADD-FILTERED-DLL
//    If necessary, adds a <remote-library> object to the filter list.

define method add-filtered-DLL
     (dll-object :: <remote-library>) => ()
  unless (is-already-filtered-DLL?(dll-object))
    add!(*filtered-dlls*, dll-object)
  end unless;
end method;


///// REMOVE-FILTERED-DLL
//    If necessary, removes a <remote-library> object from the filter list.

define method remove-filtered-DLL
    (dll-object :: <remote-library>) => ()
  if (is-already-filtered-DLL?(dll-object))
    remove!(*filtered-dlls*, dll-object)
  end if;
end method;


///// FILTERED-FUNCTION-NAME?

define method filtered-function-name?
    (name :: false-or(<remote-symbol>)) => (answer :: <boolean>)
  if (name)
    #t
  else
    function-name-is-in-filtered-DLL?(name) |
    function-name-is-in-filtered-namespace(name) |
    (function-name-is-runtime-primitive(name) & (~*show-runtime-primitives?*))
  end if;
end method;


///// USER-VISIBLE-STACK-FRAME?
//    Decides whether a stack frame should be displayed in the user's
//    stack trace. The decision is based on the namespace of the function
//    being executed in the stack frame. It is possible to filter out
//    low-level runtime primitives, and functions running within
//    particular namespaces.

define method user-visible-stack-frame?
    (application :: <application>, frame :: <application-stack-frame>)
        => (visible? :: <boolean>)
  #t
end method;

define method user-visible-stack-frame?
    (application :: <application>, frame :: <call-frame>)
        => (visible? :: <boolean>)
  if (*show-foreign-calls?*)
    let name = call-frame-function(application, frame);
    ~filtered-function-name?(name);
  else
    #f
  end if
end method;

define method user-visible-stack-frame?
    (application :: <application>, frame :: <unwind-protect-frame>)
        => (visible? :: <boolean>)
  *show-unwind-protects?*
end method;

define method user-visible-stack-frame?
    (application :: <application>, frame :: <dylan-call-frame>)
        => (visible? :: <boolean>)
  let name = call-frame-function(application, frame);
  ~filtered-function-name?(name);
end method;


///// BUILD-COMPLETE-STACK-TRACE
//    Generates a complete stack trace for the given thread. This installs
//    it into the <application> descriptor, and also creates a map of
//    the "interesting" frames.

define method build-complete-stack-trace
    (application :: <application>, thread :: <remote-thread>) => ()
  let this-frame = first-stack-frame(application, thread);
  let select-index = #f;
  let actual-index = 1;
  application.current-stack := make(<stretchy-vector>, size: 0);
  application.current-user-frame-map := make(<stretchy-vector>, size: 0);
  while (this-frame)
    add!(application.current-stack, this-frame);
    if (user-visible-stack-frame?(application, this-frame))
      add!(application.current-user-frame-map, actual-index);
      unless (select-index)
        select-index := actual-index;
      end unless;
    end if;
    actual-index := actual-index + 1;
    this-frame := previous-stack-frame(application, this-frame);
  end while;
  application.current-frame-index := select-index;
  application.current-user-frame-index := 1;
end method;


///// SELECT-NEXT-INTERESTING-STACK-FRAME
//    Basically implements the "up" command, according to the current
//    set of "interesting" stack frames. Assumes that entire stack has
//    been built.

define method select-next-interesting-stack-frame
    (application :: <application>) => ()
  if (application.current-user-frame-index > 1)
    application.current-user-frame-index :=
       application.current-user-frame-index - 1;
    application.current-frame-index :=
      application.current-user-frame-map
         [application.current-user-frame-index - 1];
  end if;
end method;


///// SELECT-PREVIOUS-INTERESTING-STACK-FRAME
//    Basicially implements the "down" command, according to the current
//    set of "interesting" stack frames. Assumes that entire stack has
//    been built.

define method select-previous-interesting-stack-frame
    (application :: <application>) => ()
  if (application.current-user-frame-index <  
         (size(application.current-user-frame-map)))
    application.current-user-frame-index :=
       application.current-user-frame-index + 1;
    application.current-frame-index :=
      application.current-user-frame-map
         [application.current-user-frame-index - 1];
  end if;
end method;


///// SELECT-TOP-INTERESTING-STACK-FRAME
//    The "top" command. Assmes that entire stack has been built.

define method select-top-interesting-stack-frame
    (application :: <application>) => ()
  application.current-user-frame-index := 1;
  application.current-frame-index :=
    application.current-user-frame-map
       [application.current-user-frame-index - 1];
end method;


///// SELECT-BOTTOM-INTERESTING-STACK-FRAME
//    The "bottom" command. Assumes that entire stack has been built.

define method select-bottom-interesting-stack-frame
    (application :: <application>) => ()
  application.current-user-frame-index :=
     size(application.current-user-frame-map);
  application.current-frame-index :=
    application.current-user-frame-map
       [application.current-user-frame-index - 1];
end method;


///// SELECT-NUMERED-FRAME
//    The "frame <n>" command. Assumes that entire stack has been built.

define method select-numbered-frame
    (application :: <application>, index :: <integer>) => ()
  if ((index > 0) & (index <= size(application.current-user-frame-index)))
    application.current-user-frame-index := index;
    application.current-frame-index :=
      application.current-user-frame-map
         [application.current-user-frame-index - 1];
  else
    debugger-message("WARNING: Tried to find a non-existant stack frame");
  end if;
end method;
*/
