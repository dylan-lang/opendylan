module:    dylan-rtg
Synopsis:  Support for name mangling for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




///// Mangling functions



/// Support for normal C calls

define open generic c-mangle 
    (be :: <harp-back-end>, name :: <byte-string>)
     => (mangled :: <byte-string>);

define method c-mangle 
    (be :: <harp-back-end>, name :: <byte-string>)
     => (mangled :: <byte-string>)
  concatenate("_", name);
end method;

define method c-mangled-ref 
    (be :: <harp-back-end>, name :: <byte-string>)
     => (ref :: <constant-reference>)
  ins--constant-ref(be, c-mangle(be, name));
end method;


define method c-mangled-indirect-ref 
    (be :: <harp-back-end>, name :: <byte-string>)
     => (ref :: <constant-reference>)
  ins--indirect-constant-ref(be, c-mangle(be, name));
end method;


define method c-full-mangled-ref
    (be :: <harp-back-end>, name :: <byte-string>)
     => (ref :: <constant-reference>)
  let name = raw-mangle(be, name);
  ins--constant-ref(be, c-mangle(be, name));
end method;

define method c-full-mangled-indirect-ref
    (be :: <harp-back-end>, name :: <byte-string>)
     => (ref :: <constant-reference>)
  let name = raw-mangle(be, name);
  ins--indirect-constant-ref(be, c-mangle(be, name));
end method;



/// Support for the Windows-specific STDCALL convention
/// STDCALLs in Windows are name mangled specially.

define method stdcall-mangle 
    (be :: <harp-back-end>, name :: <byte-string>, number :: <byte-string>) 
     => (mangled :: <byte-string>)
  concatenate("_", name, "@", number);
end method;

define method stdcall-mangled-ref 
    (be :: <harp-back-end>, name :: <byte-string>, number :: <byte-string>)
     => (ref :: <constant-reference>)
  ins--constant-ref(be, stdcall-mangle(be, name, number));
end method;




/// Support for the various types of Dylan mangling
///
/// We import raw-mangle from DFMC - and do global mangling by steam, 'cos 
/// the DFMC interface is just too much hassle.


define constant $harp-mangler = make(<mangler>);

define method full-mangle 
    (be :: <harp-back-end>, name :: <byte-string>, 
     #key module = "dylan", library = "dylan")
    => (mangled-name :: <byte-string>)
  mangle-binding-spread($harp-mangler, name,
			module, library);
end method;


define macro mangler-function-definer
  { define ?ref-opts mangler-function ?:name }
    => { define method ?name 
               (be :: <harp-back-end>, name :: <byte-string>)
               => (ref :: <constant-reference>)
           ?ref-opts
         end method }

ref-opts:
  { indirect ?mangle-opts } 
    => { ins--indirect-constant-ref(be, ?mangle-opts) }
  { direct ?mangle-opts } 
    => { ins--constant-ref(be, ?mangle-opts) }
  { ?mangle-opts } 
    => { ins--constant-ref(be, ?mangle-opts) }

mangle-opts:
  { }
    => { raw-mangle(be, name) }
  { dylan } 
    => { full-mangle(be, name) }
  { dispatch-engine } 
    => { full-mangle(be, name, module: "dispatch-engine") }
  { extensions } 
    => { full-mangle(be, name, module: "dylan-extensions") }
  { internal } 
    => { full-mangle(be, name, module: "internal") }
  { threads-prims } 
    => { full-mangle(be, name, module: "threads-primitives") }
  { mw } 
    => { full-mangle(be, name, module: "machine-word-lowlevel") }
  { literal ... } 
    => { concatenate($constant-prefix, ...) }
  { iep ... } 
    => { concatenate(..., $iep-suffix) }
  { wrapper ... } 
    => { concatenate(..., $wrapper-suffix) }
end macro;


define mangler-function mangled-ref;

define indirect mangler-function mangled-indirect-ref;

define literal mangler-function constant-mangled-ref;

define literal dylan mangler-function dylan-library-constant-mangled-ref;

define literal threads-prims mangler-function dylan-threads-prims-constant-mangled-ref;

define literal internal mangler-function dylan-internal-constant-mangled-ref;

define indirect internal mangler-function dylan-internal-variable-mangled-ref;

define literal extensions mangler-function dylan-extensions-constant-mangled-ref;

define indirect extensions mangler-function dylan-extensions-indirect-mangled-ref;

define literal wrapper dylan mangler-function dylan-library-wrapper-mangled-ref;

define literal wrapper internal mangler-function dylan-internal-wrapper-mangled-ref;

define literal wrapper extensions mangler-function dylan-extensions-wrapper-mangled-ref;

define literal iep dylan mangler-function dylan-library-iep-mangled-ref;

define literal iep internal mangler-function dylan-internal-iep-mangled-ref;

define literal iep threads-prims mangler-function dylan-threads-prims-iep-mangled-ref;

define literal iep dispatch-engine mangler-function dylan-dispatch-engine-iep-mangled-ref;

define literal iep mw mangler-function dylan-mw-iep-mangled-ref;

define literal iep extensions mangler-function dylan-extensions-iep-mangled-ref;

define indirect dispatch-engine mangler-function dylan-dispatch-engine-indirect-mangled-ref;




//// Name mangling for primitives


define method primitive-name 
    (be :: <harp-back-end>, name :: <byte-string>) 
    => (prim-name :: <byte-string>)
  raw-mangle(be, concatenate("primitive-", name));
end method;



define method c-primitive-name 
    (be :: <harp-back-end>, name :: <byte-string>) 
    => (prim-name :: <byte-string>)
  c-mangle(be, raw-mangle(be, name));
end method;




/// Entry point name mangling



define method entry-point-name 
    (be :: <harp-back-end>, name :: <byte-string>, num :: <integer>)
    => (name :: <byte-string>)
  raw-mangle(be, format-to-string("%s-%=", as-lowercase(name), num));
end method;



define method entry-point-name 
    (be :: <harp-back-end>, name :: <byte-string>, num == #"dynamic")
    => (name :: <byte-string>)
  raw-mangle(be, as-lowercase(name));
end method;


