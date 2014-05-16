module: variable-search
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant constant-prefix-string = "K";

define method locate-variable (o)
 => (variable-encoding, module-encoding, library-encoding);
  error("Failed to locate a variable binding for %=", o);
end method;

define method locate-variable (o :: <class>)
 => (variable-encoding, module-encoding, library-encoding);
  let variable-name = copy-sequence(debug-name(o));
  let module        = class-module(o);
  let module-name   = namespace-name(module);
  let library       = home-library(module);
  let library-name  = namespace-name(library);
  values(variable-name, module-name, library-name)
end method;

define thread variable *mangler* = make(<mangler>);

define variable *dl-handle* = #f;

define method variable-value
    (variable-name, module-name, library-name, #key default)
 => (object)
  local method failed-lookup ()
          error("Failed to locate variable %= in module %= of library %=",
                variable-name, module-name, library-name);
        end method;
  let mangled-name
    = format-to-string
        ("%s%s",
         constant-prefix-string,
         mangle-binding-spread
           (*mangler*, variable-name, module-name, library-name));
  unless (*dl-handle*)
    *dl-handle*
       := primitive-wrap-machine-word
            (%call-c-function ("dlopen")
                  (name :: <raw-byte-string>)
               => (object :: <raw-machine-word>)
                (primitive-cast-raw-as-pointer(integer-as-raw(0)))
              end);
  end unless;
  let val =
    primitive-cast-raw-as-pointer
    (%call-c-function ("dlsym")
         (handle :: <raw-machine-word>, name :: <raw-byte-string>)
      => (object :: <raw-machine-word>)
         (primitive-unwrap-machine-word(*dl-handle*),
          primitive-string-as-raw(mangled-name))
     end);
  if (primitive-machine-word-equal?(val, integer-as-raw(0)))
    failed-lookup();
  else val
  end if;
end method;
