Module: dfmc-native-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method estimate-harp-virtual-registers-size
    (backend :: <harp-native-back-end>, o :: <&iep>) => (size :: <integer>)
  round(1.3 * number-temporaries(o.function));
end method;

// On Linux, C Compiler doesn't prepend C names with underscores

define sideways method c-name
    (back-end :: <native-unix-back-end>, name :: <string>) => (name :: <string>)
 name
end method c-name;


define sideways method shared-library-entry-point-name
    (back-end :: <harp-native-back-end>, name :: <string>) => (name :: <string>)
  c-name(back-end, concatenate(name, "Dll@12"));
end method shared-library-entry-point-name;

define sideways method shared-library-entry-point-name
    (back-end :: <native-unix-back-end>, name :: <string>) => (name :: <string>)
  c-name(back-end, concatenate(name, "SO"));
end method shared-library-entry-point-name;


define sideways method shared-library-runtime-entry-point-name
    (back-end :: <harp-native-back-end>) => (name :: <string>)
  c-name(back-end, "DylanDllEntry@12");
end method shared-library-runtime-entry-point-name;

define sideways method shared-library-runtime-entry-point-name
    (back-end :: <native-unix-back-end>) => (name :: <string>)
  c-name(back-end, "DylanSOEntry");
end method shared-library-runtime-entry-point-name;


define constant $import-prefix = "__imp_";

define sideways method emit-imported-name
    (back-end :: <harp-native-back-end>, stream, o) => (name :: <string>)
  concatenate($import-prefix, emit-name(back-end, stream, o));
end method;

// Imports aren't renamed on Linux

define sideways method emit-imported-name
    (back-end :: <native-unix-back-end>, stream, o) => (name :: <string>)
  emit-name(back-end, stream, o);
end method;

// Imports don't require an extra indirection on Linux

define sideways method make-imported-constant-reference
    (back-end :: <native-unix-back-end>, o,
     #key indirect?) => (name :: <constant-reference>)
  if (indirect?)
    ins--indirect-constant-ref(back-end, o, import?: #t);
  else
    ins--constant-ref(back-end, o, import?: #t);
  end if;
end method;

// No compiler issues with import generation on Linux

define sideways method emit-import-adjustment?
    (back-end :: <native-unix-back-end>) => (adjust? :: <boolean>)
  #f
end method emit-import-adjustment?;


// ELF Outputters use this emitter to emit type and size of data, so the 
// Linker can create appropriate dynamic relocation records for them

define sideways method emit-data-footer
    (back-end :: <native-unix-back-end>, stream, name,
     #key model-object = unsupplied()) => ()
  output-data-footer
  (back-end, stream, name,
   model-object: model-object);
end method;


define sideways method emit-imports
    (back-end :: <native-unix-back-end>, cr, ld :: <library-description>) => ()
end method;

define sideways method emit-library-imported-data
    (back-end :: <native-unix-back-end>, stream, description :: <library-description>,
     #key compilation-layer)
 => ()
end method;



