module: variable-search
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant constant-prefix-string = "K";

define thread variable *mangler* = make(<mangler>);

define method variable-value
    (variable-name, module-name, library-name, #key default = unsupplied())
 => (object)
  local method failed-lookup ()
          if (default.supplied?)
            default
          else
            error("Failed to locate variable %= in module %= of library %=",
                  variable-name, module-name, library-name);
          end if;
        end method;
  let mangled-name :: <byte-string>
    = mangle-binding-spread(*mangler*, variable-name, module-name, library-name);
  let module :: <machine-word> = lookup-runtime-module(library-name);
  let val
    =  primitive-cast-raw-as-pointer
         (%call-c-function ("GetProcAddress", c-modifiers: "__stdcall")
               (hModule :: <raw-machine-word>, lpProcName :: <raw-byte-string>)
            => (object :: <raw-machine-word>)
             (primitive-unwrap-machine-word(module),
               primitive-string-as-raw(mangled-name))
           end);
  if (primitive-machine-word-equal?(val, integer-as-raw(0)))
    // For bootsrapping away from variable searching based on constants...
    // Remove this hack after the next release is built
    // Nosa  Feb 25, 1999
    let mangled-name = concatenate(constant-prefix-string, mangled-name);
    let val
      =  primitive-cast-raw-as-pointer
      (%call-c-function ("GetProcAddress", c-modifiers: "__stdcall")
         (hModule :: <raw-machine-word>, lpProcName :: <raw-byte-string>)
         => (object :: <raw-machine-word>)
         (primitive-unwrap-machine-word(module),
          primitive-string-as-raw(mangled-name))
       end);
    if (primitive-machine-word-equal?(val, integer-as-raw(0)))
      failed-lookup();
    else val
    end if;
  else
    // variable-searching on variables requires indirection
    primitive-element(val, integer-as-raw(0), integer-as-raw(0))
  end if;
end method;
