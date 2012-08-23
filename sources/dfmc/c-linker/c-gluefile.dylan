Module: dfmc-c-linker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method emit-mainfile
    (back-end :: <c-back-end>, ld, #rest flags, #key, #all-keys)
  let lib-name = library-description-emit-name(ld);
  let c-file = #f;
  with-build-area-output (stream = ld, base: "_main", type: "c")
    write(stream, "#include <stdlib.h>\n");
    write(stream, "#include <gc/gc.h>\n\n");
    write(stream, "#include \"run-time.h\"\n\n");

    format(stream, "int main (int argc, char *argv[]) {\n");
    format(stream, "  extern void %s ();\n", glue-name(lib-name));
    format(stream, "  extern D %s;\n", command-arguments-name());
    format(stream, "  extern D %s;\n", command-name-name());

    format(stream, "  GC_INIT();\n");

    write (stream, "  D args = primitive_make_vector((argc > 0) ? argc - 1 : 0);\n");
    write (stream, "  int i;\n");
    format(stream, "  if (argc > 0)\n");
    format(stream, "    %s = primitive_raw_as_string(argv[0]);\n", command-name-name());
    format(stream, "  else\n");
    format(stream, "    %s = primitive_raw_as_string(\"unknown\");\n", command-name-name());
    write (stream, "  for (i = 1; i < argc; i++) \n");
    write (stream, "    primitive_vector_element_setter\n");
    write (stream, "      (primitive_raw_as_string(argv[i]), args,\n");
    write (stream, "       primitive_raw_as_integer(i - 1));\n");
    format(stream, "  %s = (D)args;\n", command-arguments-name());
    format(stream, "  %s();\n", glue-name(lib-name));
    format(stream, "  return(0);\n");
    format(stream, "}\n");
    c-file := stream-locator(stream);
  end with-build-area-output;
end method;

define sideways method emit-gluefile
    (back-end :: <c-back-end>, ld, cr-names, #rest flags, #key, #all-keys)
  let lib-name = library-description-emit-name(ld);
  let c-file = #f;
  with-build-area-output (stream = ld, base: "_glue", type: "c")
    let used-glue-names = used-glue-names(ld);
    let cr-init-names = cr-init-names(ld, cr-names);
    let rt-init-names = list(glue-name-raw("Run_Time"));
    let init-names = concatenate(rt-init-names, used-glue-names, cr-init-names);
    write (stream, "#include \"run-time.h\"\n\n");
    format(stream, "void %s () __attribute__((constructor));\n", glue-name(lib-name));
    format(stream, "void %s () {\n", glue-name(lib-name));
    for (name in init-names)
      format(stream, "  extern void %s();\n", name);
    end for;
    format(stream, "  static int initp = 0;\n");
    format(stream, "  if (!initp) {\n");
    format(stream, "    initp = 1;\n");
    for (name in init-names)
      format(stream, "    %s();\n", name);
    end for;
    if (dylan-library-library-description?(ld))
      without-dependency-tracking
        let install-boot-symbols = ^iep(dylan-value(#"%install-boot-symbols"));
        format-emit(back-end, stream, 1, "    { extern ~ ^();\n",
                    $dylan-type-string, install-boot-symbols);
        format-emit(back-end, stream, 1, "      ^(); }\n",
                    install-boot-symbols);
      end;
    end if;
    format(stream, "  }\n");
    format(stream, "}\n");
    c-file := stream-locator(stream);
  end with-build-area-output;
end method;

define method command-arguments-name ()
  c-raw-mangle("*command-arguments*");
end method;

define method command-name-name ()
  c-raw-mangle("*command-name*");
end method;

define method cr-init-name (ld, cr-name)
  concatenate(ld.library-description-glue-name, "_X_",
              c-local-mangle(cr-name))
end method;

define method cr-init-names (ld, cr-names)
  concatenate
    (map(method (cr)
           concatenate(cr-init-name(ld, cr), $system-init-code-tag);
         end,
         cr-names),
     map(method (cr)
           concatenate(cr-init-name(ld, cr), $user-init-code-tag);
         end,
         cr-names))
end method;

define method glue-name-raw (name :: <byte-string>)
  concatenate("_Init_", name)
end method;

define method glue-name (name)
  glue-name-raw(c-local-mangle(as-lowercase(as(<string>, name))))
end method;

define method library-description-glue-name (ld)
  glue-name(library-description-emit-name(ld))
end method;

define method used-glue-names (ld)
  map(library-description-glue-name, library-description-used-descriptions(ld))
end method;
