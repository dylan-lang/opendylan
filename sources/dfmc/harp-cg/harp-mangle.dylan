Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method harp-local-mangle (name)
  local-mangle(*harp-back-end*, name);
end method;

define method harp-raw-mangle (name)
  raw-mangle(*harp-back-end*, name);
end method;

/// STRUCT MANGLING

define method struct-field-name 
    (class :: <&class>, slotd :: <&slot-descriptor>, position)
  harp-local-mangle
    (slotd.^debug-name | format-to-string("anon-slot-%d", position))
end method;

define method glue-name-raw (name :: <byte-string>)
  concatenate("_Init_", name)
end method;

define method glue-name (name)
  glue-name-raw(harp-local-mangle(as-lowercase(as(<string>, name))))
end method;

define method library-description-glue-name (ld)
  glue-name(library-description-emit-name(ld))
end method;

define method cr-init-name (ld, cr-name)
  concatenate(ld.library-description-glue-name, "_X_",
	      harp-local-mangle(cr-name))
end method;

// eof
