Module: dfmc-java-linker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/*
define method emit-mainfile (stream, library)
break(); //MT
  format(stream, "main (int argc, char *argv[]) {\n");
  format(stream, "  extern void %s ();\n", glue-name(library.debug-name));
  format(stream, "  %s();\n", glue-name(library.debug-name));
  format(stream, "  return(0);\n");
  format(stream, "}\n");

  finalize-java-linking () ;
end method;

define method emit-gluefile (stream, library, cr-names)
break(); //MT
  let used-glue-names = used-glue-names(library);
  let cr-init-names = cr-init-names(library, cr-names);
  let rt-init-names = list(glue-name-raw("Run_Time"));
  let init-names = concatenate(rt-init-names, used-glue-names, cr-init-names);
  format(stream, "void %s () {\n", glue-name(library.debug-name));
  for (name in init-names)
    format(stream, "  extern void %s();\n", name);
  end for;
  format(stream, "  static initp = 0;\n");
  format(stream, "  if (!initp) {\n");
  format(stream, "    initp = 1;\n");
  for (name in init-names)
    format(stream, "    %s();\n", name);
  end for;
  format(stream, "  }\n");
  format(stream, "}\n");
end method;


define method cr-init-name (library, cr-name)
break(); //MT
  concatenate(library.debug-name.glue-name, "_X_", java-local-mangle(cr-name))
end method;

define method cr-init-names (library, cr-names)
break(); //MT
  map(curry(cr-init-name, library), cr-names)
end method;

define method glue-name-raw (name :: <byte-string>)
break(); //MT
  concatenate("_Init_", name)
end method;

define method glue-name (name)
break(); //MT
  glue-name-raw(java-local-mangle(as-lowercase(as(<string>, name))))
end method;

define constant use-clauses = access(dfmc-namespace, use-clauses);

define method used-library-names (library)
break(); //MT
  map(used-name, library.use-clauses)
end method;

define method used-glue-names (library)
break(); //MT
  map(glue-name, used-library-names(library))
end method;
*/
