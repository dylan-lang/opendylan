Module: dfmc-llvm-linker
Author: Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method emit-mainfile
    (back-end :: <llvm-back-end>, ld :: <library-description>,
     #rest keys, #key, #all-keys)
  // FIXME
end;

define sideways method emit-gluefile
    (back-end :: <llvm-back-end>, ld :: <library-description>, cr-names,
     #key assembler-output? = unsupplied(), 
          downloadable-data? = #f,
          debug-info? = #t,
          compilation-layer,
     #all-keys)
  // FIXME
end;


/// Compilation record init function naming

define method glue-name-raw (name :: <byte-string>)
  concatenate("_Init_", name)
end method;

define method glue-name (back-end :: <llvm-back-end>, name)
  glue-name-raw(local-mangle(back-end, as-lowercase(as(<string>, name))))
end method;

define method library-description-glue-name (back-end :: <llvm-back-end>, ld)
  glue-name(back-end, library-description-emit-name(ld))
end method;

define method cr-init-name (back-end :: <llvm-back-end>, ld, cr-name)
  concatenate(library-description-glue-name(back-end, ld), "_X_",
	      local-mangle(back-end, cr-name))
end method;
