module: variable-search
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant constant-prefix-string = "K";

define thread variable *mangler* = make(<mangler>);

define constant $RTLD-DEFAULT = -2;

define method variable-value
    (variable-name, module-name, library-name, #key default)
 => (object)
  let mangled-name
    = concatenate
        (constant-prefix-string,
         mangle-binding-spread
           (*mangler*, variable-name, module-name, library-name));
  let val =
    primitive-cast-raw-as-pointer
    (%call-c-function ("dlsym")
         (handle :: <raw-machine-word>, name :: <raw-byte-string>)
      => (object :: <raw-machine-word>)
         (integer-as-raw($RTLD-DEFAULT),
          primitive-string-as-raw(mangled-name))
     end);
  if (primitive-machine-word-equal?(val, integer-as-raw(0)))
    error("Failed to locate variable %= in module %= of library %=",
          variable-name, module-name, library-name);
  else val
  end if;
end method;
