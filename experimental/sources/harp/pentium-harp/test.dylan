module: pentium-harp-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $fake-imports =
#[
"Kbyte_vector_moduleYdylan_userVbyte_vector",
"Kbyte_vector_libraryYdylan_userVbyte_vector",
"byte_vector_libraryYdylan_userVbyte_vector",
"_Init_byte_vector_"
];

define method fake-imports (#rest all-keys, #key type, print-harp? = #t) => ()
  do-file-test(make(<pentium-back-end>),
               "fake-imports",
               apply(vector, print-harp?: print-harp?, all-keys),
               fake-import);
end method;

define method fake-import (back-end :: <pentium-back-end>, outputter)
  for (export in $fake-imports)
      let import = concatenate("__imp_", export);

      output-external(back-end, outputter, export);
      output-public(back-end, outputter, import);

      output-definition(back-end, outputter, import,
			section: #"variables");
      output-data-item(back-end, outputter, export);
  end for;
end method;
