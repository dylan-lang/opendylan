Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open generic rts-dropping-args(back-end :: <harp-back-end>) => ();

define method emit-epilog
    (back-end :: <harp-back-end>, lambda :: <&iep>) => ()
  ins--tag(back-end, back-end.cg-variables.exit-tag);
  ins--rem(back-end, "function epilog");
  
  if (instance?(lambda.function, <&c-callable-function>)
	& lambda.function.c-modifiers ~= "__stdcall")
    ins--rts-and-drop(back-end, 0);
  else
    rts-dropping-args(back-end);
  end if;
  *trace-harp?* & trace-harp?(back-end, "EXIT");
end method emit-epilog;

// define method emit-cg-variables-sizes
//     (back-end :: <harp-back-end>) => ()
//   format-out("\n### HARP-CG DATA \n");
//   format-out("\n### LAMBDA %=\n", back-end.cg-variables.current-lambda);
//   format-out("\n### TEMPORARIES %=\n",
// 	     number-temporaries(back-end.cg-variables.current-lambda.function));
//   format-out("\n### IMPORTS %=\n", back-end.cg-variables.imports.size);
//   format-out("\n### RUNTIME-REFERENCES %=\n",
// 	     back-end.cg-variables.runtime-references.size);
//   format-out("\n### CG-REFERENCES %=\n", back-end.cg-variables.cg-references.size);
//   format-out("\n### MODEL-REFERENCES %=\n", back-end.cg-variables.model-references.size);
// 
//   format-out("\n### CG-TEMPORARIES %=\n", back-end.cg-variables.cg-temporaries.size);
//   format-out("\n### TAGS %=\n", back-end.cg-variables.tags.size);
// end method;
