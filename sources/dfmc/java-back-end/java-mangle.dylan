module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $classfile-package-separator$ :: <byte-string> = "/";

define method global-mangle-with-module
    (back-end :: <java-back-end>, name :: <byte-string>, module :: <module>)
  let mang = mangler-reset (back-end.mangler);
  let lib = module.home-library;

  mangle-namespace-into (mang, lib);
  mangle-raw-into       (mang, $classfile-package-separator$);
  mangle-namespace-into (mang, module);
  mangle-raw-into       (mang, $classfile-package-separator$);
  mangle-name-into      (mang, name);

  mangler-as-string (mang)
end;

define method local-mangle (back-end :: <java-back-end>, name) => (str :: <byte-string>)
  let mang = mangler-reset (back-end.mangler);

//format-out ("local-mangle, %s\n", name);
//was  mangle-into(mang, name);
  mangle-name-into (mang, name);

  mangler-as-string (mang)
end;


define function jraw-mangle (back-end :: <java-back-end>, name) => (str :: <byte-string>)
  let mang = mangler-reset (back-end.mangler);

  mangle-name-into (mang, name);

  mangler-as-string (mang)
end;



// The actual mangling of certain things

define method global-mangle (back-end :: <java-back-end>, name :: <variable-name-fragment>) => (str :: <byte-string>)
  global-mangle-with-module (back-end, as (<byte-string>, name.fragment-identifier), name.fragment-module)
end;

define method global-mangle (back-end :: <java-back-end>, o :: <module-binding>) => (str :: <byte-string>)
//  let frag-id   = o.binding-variable-name.fragment-identifier;
//  let bind-home = o.binding-home;
//  global-mangle-with-module (back-end, frag-id, bind-home)
  global-mangle-with-module(back-end, o.binding-identifier, o.binding-home)
end;

define method global-mangle (back-end :: <java-back-end>, o :: <variable-defining-form>) => (str :: <byte-string>)
//  global-mangle (back-end, lookup-binding (o.form-variable-name))
  global-mangle(back-end, lookup-binding(o.form-variable-name, reference?: #f))
end;

// TODO: Would use global-mangle-with-module with dylan-module(), but
// dylan-module() isn't available when some of the initial mangling
// is done.

// overrides back-end
define method mangle-namespace-into (mangler :: <mangler>, namespace :: <namespace>)
  let  emitted = namespace.emitted-name;
  if (emitted)
    mangle-raw-into (mangler, emitted)
  else
    let start = mangler.mangler-position;
    mangle-name-into (mangler, namespace.debug-name);
    namespace.emitted-name := mangler-as-string (mangler, start: start)
  end;
end;


define method global-mangle (back-end :: <java-back-end>, name :: <string>) => (str :: <byte-string>)
  java-mangler (name, #f)
end;



define function java-mangler (name :: <string>, module :: <module>.false-or)
  let  back-end  =  *java-back-end*;
  if (~ module)
    module := back-end.current-module
  end;
  let  mangler = mangler-reset (back-end.mangler);
  if (module)
    mangle-namespace-into (mangler, module);
    mangle-raw-into       (mangler, $classfile-package-separator$)
  end;
  mangle-name-into  (mangler, name);
  mangler-as-string (mangler)
end;

define function java-global-mangle (name)
  global-mangle (*java-back-end*, name)
end;

define function java-local-mangle (name)
  local-mangle (*java-back-end*, name)
end;

define function java-raw-mangle (name)
  jraw-mangle (*java-back-end*, name)
end;

/*
// dunno what this for. ancient bagage
define constant $dylan-type-string :: <byte-string> = "<dylan-type-string>";

define method java-type-name (o :: <&class>)
  $dylan-type-string
end;

define method java-type-name (o :: subclass(<&raw-type>))
  o.raw-type-c-name
end;

define method java-repeated-type-name (o)
  java-type-name(o)
end;

define method java-repeated-type-name (o :: <&class>)
  if (o == dylan-value(#"<byte-character>"))
    "character"
  else
    java-type-name(o)
  end
end;
*/
/// !@#$ hack for when slot-type is not yet filled in

/*
define method java-type-name (o)
  $dylan-type-string
end;
*/
