Module: dfmc-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define compiler-open generic local-mangle (back-end :: <back-end>, name)
 => (res :: <byte-string>);
define compiler-open generic global-mangle (back-end :: <back-end>, name)
 => (res :: <byte-string>);


// "R" is for recursive disambiguation by following string, with
// "_" inserted as appropriate for parsability:
define constant $singular-terminal-engine-node-prefix = "RSINGULAR_";


/// MANGLING

define method mangle-namespace-into
    (mangler :: <mangler>, namespace :: <module>)
  if (namespace.emitted-name)
    mangle-raw-into(mangler, namespace.emitted-name);
  else
    let start = mangler-position(mangler);
    mangle-namespace-spread-into
      (mangler, namespace.debug-name, namespace.home-library.debug-name);
    namespace.emitted-name
      := mangler-as-string(mangler, start: start);
  end if;
end method;

define function module-mangled-name (back-end :: <back-end>,
				     module :: <module>)
 => (name :: <byte-string>)
  module.emitted-name
   | begin
       let mangler = mangler-reset(back-end.mangler);
       mangle-namespace-into(mangler, module);
       mangler-as-string(mangler)
     end
end;


define method mangleable (name) => (name)
  name
end method;

define method mangleable (name :: <variable-name-fragment>) => (name)
  fragment-identifier(name)
end method;

define method local-mangle 
    (back-end :: <back-end>, name) => (res :: <byte-string>)
  mangle-name-locally(mangler(back-end), mangleable(name))
end method;

define method hygienic-mangle
    (back-end :: <back-end>, name, mark :: <integer>)
  mangle-name-hygienically(mangler(back-end), mangleable(name), mark);
end method;

define method global-mangle-with-module
    (back-end :: <back-end>, name, module :: <module>) 
 => (res :: <byte-string>)
  let mangler = mangler-reset(back-end.mangler);
  mangle-name-into(mangler, name);
  mangle-namespace-into(mangler, module);
  mangler-as-string(mangler)
end method;

define method global-mangle 
    (back-end :: <back-end>, name :: <variable-name-fragment>)
 => (res :: <byte-string>)
  global-mangle(back-end, lookup-binding(name, reference?: #f))
end method;

define method global-mangle 
    (back-end :: <back-end>, o :: <module-binding>) => (res :: <byte-string>)
  global-mangle-with-module(back-end, o.binding-identifier, o.binding-home)
end method;

define function binding-mangled-name (back-end :: <back-end>,
				      binding :: <module-binding>)
 => (name :: <byte-string>)
  let name = binding.emitted-name;
  if (instance?(name, <byte-string>))
    name
  else
    binding.emitted-name := global-mangle(back-end, binding);
  end;
end;

define method global-mangle
    (back-end :: <back-end>, o :: <variable-defining-form>)
 => (res :: <byte-string>)
  global-mangle(back-end, lookup-binding(o.form-variable-name, reference?: #f))
end method;

define method global-mangle 
    (back-end :: <back-end>, o :: <&object>) => (res :: <byte-string>)
  global-mangle(back-end, model-definition(o))
end method;

// TODO: Would use global-mangle-with-module with dylan-module(), but
// dylan-module() isn't available when some of the initial mangling
// is done.

define method global-mangle 
    (back-end :: <back-end>, name :: <byte-string>) => (res :: <byte-string>)
  let mangler = mangler-reset(back-end.mangler);
  mangle-name-into(mangler, name);
  mangle-namespace-spread-into(mangler, "dylan", "dylan");
  mangler-as-string(mangler)
end method;

// eof
