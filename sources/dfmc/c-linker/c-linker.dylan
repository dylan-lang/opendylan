Module: dfmc-c-linker
Author: Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// NOTE: Nothing actually uses this class ...
/// define class <c-linker> (<linker>) end;

define function c-output-basename (basename :: <string>) => (c-basename)
  concatenate(basename, "_dude")
end function;

// DRIVER PROTOCOL FUNCTIONS
define sideways method emit-library-records
    (back-end :: <c-back-end>, ld :: <library-description>, #rest flags, #key, #all-keys)
  for (cr in library-description-compilation-records(ld))
    apply(emit-library-record, back-end, cr, flags);
  end;
end method;

define sideways method emit-library-record
    (back-end :: <c-back-end>, cr :: <compilation-record>, 
     ld :: <library-description>, #rest flags, #key, #all-keys)
  if (compilation-record-needs-linking?(cr))
    let c-file = #f;
    with-dependent($compilation of cr)
      with-build-area-output
        (stream = ld,
	 base: output-basename(back-end, makefile-target(ld), compilation-record-name(cr)),
	 type: "c")
	let name = cr.compilation-record-source-record.source-record-name;
        progress-line("Linking %s.dylan", name);
        link-all(back-end, stream, cr);
	c-file := stream-locator(stream);
      end;
    end;
    compilation-record-needs-linking?(cr) := #f;
    maybe-compile-c-file(back-end, ld, c-file);
  end if;
end method;

define method c-compiler-command-line
    (platform, ld, c-file) => (command :: <byte-string>)
  //---*** GENERATE UNIX COMMAND LINE HERE!
  ""
end method c-compiler-command-line;

define method c-compiler-command-line
    (platform == #"x86-win32", ld, c-file) => (command :: <byte-string>)
  let include 
    = environment-variable("FUNCTIONAL_DEVELOPER_RELEASE_INCLUDE")
      | begin
	  let install = environment-variable("FUNCTIONAL_DEVELOPER_RELEASE_INSTALL");
	  install
	    & as(<string>,
		 subdirectory-locator(as(<directory-locator>, install), "include"))
	end;
  let c-flags = environment-variable("FUNCTIONAL_DEVELOPER_C_FLAGS");
  let object-file = backend-object-file-name(makefile-target-using-os-name(#"win32"), c-file);
  let command = "cl /c /DCRTAPI1=_cdecl /DCRTAPI2=_cdecl /nologo /D_X86_=1 "
                "/D_WIN32_IE=0x0300 /DWINVER=0x0400 /DWIN32 /D_WIN32 /D_MT /MT /Z7 /Od /W2 ";
  command := concatenate(command, "/Fo", object-file, " ");
  if (include)
    command := concatenate(command, "/I", include, " ");
  end;
  if (c-flags)
    command := concatenate(command, c-flags, " ");
  end;
  concatenate(command, as(<string>, c-file))
end method c-compiler-command-line;

define method maybe-compile-c-file
    (back-end :: <c-back-end>, ld :: <library-description>, c-file :: <locator>) => ()
  let saved-wd = working-directory();
  let c-filename = locator-name(c-file);
  block ()
    local method display-compiler-results (buffer :: <byte-string>, 
					   #key end: last = #f)
	    unless (last)
	      last := size(buffer);
	    end;
	    if (buffer[last - 1] == '\n')
	      last := last - 1;
	    end;
	    let start = 0;
	    let skip = 0;
	    while (start < last)
	      let eol = position(buffer, '\n', skip: skip);
	      unless (eol)
		eol := last;
	      end;
	      let text = copy-sequence(buffer, start: start, end: eol);
	      if (text[size(text) - 1] == '\r')
		text := copy-sequence(text, end: size(text) - 1);
	      end;
	      unless (text = c-filename)
		progress-line("%s", text);
	      end;
	      start := eol + 1;
	      skip := skip + 1;
	    end;
	  end method;
    if (library-description-os-name(ld) == $os-name 
	  & library-description-processor-name(ld) == $machine-name)
      progress-line("Compiling %s.", c-file);
      working-directory() := locator-directory(c-file);
      let command = c-compiler-command-line($platform-name, ld, c-filename);
      let status = run-application(command, outputter: display-compiler-results);
      unless (status = 0)
	progress-line("Compilation of %s failed.", c-filename);
	//---*** SHOULD WE ABORT THE COMPILATION HERE SOMEHOW?
      end
    end
  cleanup
    working-directory() := saved-wd;
  end
end method maybe-compile-c-file;

define method maybe-compile-c-file
    (back-end :: <c-back-end>, ld :: <library-description>, c-file :: <string>) => ()
  maybe-compile-c-file(back-end, ld, as(<file-locator>, c-file))
end method maybe-compile-c-file;

//// TOP-LEVEL

define constant $symbol-fixup-name = #"%resolve-symbol";

define constant $system-init-code-tag = "for_system";
define constant $user-init-code-tag = "for_user";

define method link-all 
    (back-end :: <c-back-end>, stream :: <stream>, cr :: <compilation-record>)
  with-simple-abort-retry-restart 
      ("Abort the emission phase", "Restart the emission phase")
    let heap = cr.compilation-record-model-heap;
    write(stream, "#include \"run-time.h\"\n\n");
    emit-typedefs(back-end, stream, cr);
    emit-externs(back-end, stream, cr);
    emit-forwards(back-end, stream, cr);
    emit-indirection-definitions(back-end, stream, cr);
    write(stream, "/* Variables */\n\n");
    for (binding in heap.heap-defined-bindings)
      emit-definition(back-end, stream, binding);
    end for;
    write-element(stream, '\n');
    write(stream, "/* Objects */\n\n");
    for (literal in heap.heap-defined-object-sequence)
      emit-data-definition(back-end, stream, literal);
    end for;
    write(stream, "/* Code */\n\n");
    for (literal in heap.heap-defined-object-sequence)
      emit-code-definition(back-end, stream, literal);
    end for;
    write(stream, "\n/* SYSTEM INIT CODE */\n\n");
    emit-init-code-head(back-end, stream, cr, $system-init-code-tag);
    format-emit(back-end, stream, 1, "extern ~ ^(~);\n", 
                $dylan-type-string,
                ^iep(dylan-value($symbol-fixup-name)), 
                $dylan-type-string);
    for (refs in heap.heap-load-bound-references)
      emit-fixups
        (back-end, stream, load-bound-referenced-object(refs.first), refs);
    end for;
    emit-init-code-body(back-end, stream, heap.heap-root-system-init-code);
    emit-init-code-tail(back-end, stream);
    write(stream, "/* USER INIT CODE */\n\n");
    emit-init-code-head(back-end, stream, cr, $user-init-code-tag);
    emit-init-code-body(back-end, stream, heap.heap-root-init-code);
    emit-init-code-tail(back-end, stream);
    write(stream, "\n/* eof */\n");
  end;
end method;

define method emit-init-code-head
    (back-end :: <c-back-end>, stream, 
       cr :: <compilation-record>, tag :: <string>)
 => ()
 format(stream, "void %s%s () {\n", 
        cr-init-name(compilation-record-library(cr),
                     compilation-record-name(cr)),
        tag);
end method;

define method emit-init-code-body 
    (back-end :: <c-back-end>, stream, lambdas :: <sequence>) => ()
  for (code in lambdas)
    emit-definition(back-end, stream, code.^iep);
  end for;
end method;

define method emit-init-code-tail
    (back-end :: <c-back-end>, stream) => ()
 write(stream, "  return;\n}\n\n");
end method;

define method emit-code-definition
    (back-end :: <c-back-end>, stream, o :: <&iep>)
  emit-definition(back-end, stream, o);
end method;

define method emit-code-definition
    (back-end :: <c-back-end>, stream, o)
end method;

define method emit-data-definition
    (back-end :: <c-back-end>, stream, o :: <&iep>)
end method;

define method emit-data-definition
    (back-end :: <c-back-end>, stream, o)
  emit-definition(back-end, stream, o);
end method;

define method emit-typedefs 
    (back-end :: <c-back-end>, stream :: <stream>, cr :: <compilation-record>)
  write(stream, "/* Typedefs for referenced classes */\n\n");
  let heap = cr.compilation-record-model-heap;
  for (object in heap.heap-referenced-objects)
    emit-heap-typedefs(back-end, stream, heap, object);
  end;
  write(stream, "\n/* Typedefs for defined classes */\n\n");
  for (object in heap.heap-defined-objects)
    emit-heap-typedefs(back-end, stream, heap, object);
  end;
  write-element(stream, '\n');
end method;

define method emit-heap-typedefs
    (back-end :: <c-back-end>, stream :: <stream>, heap, object)
  // Do nothing.
end method;

define method emit-heap-typedefs
    (back-end :: <c-back-end>, stream :: <stream>, heap, object :: <&class>)
  emit-typedef(back-end, stream, object);
  let referenced-sizes 
    = element(heap.heap-referenced-repeated-object-sizes, 
              object, default: #());
  let defined-sizes 
    = element(heap.heap-defined-repeated-object-sizes, 
              object, default: #());
  for (size in referenced-sizes)
    emit-repeated-struct-definer-name(back-end, stream, object, size);
    write(stream, ";\n");
  end for;
  for (size in defined-sizes)
    if (~element(referenced-sizes, size, default: #f))
      emit-repeated-struct-definer-name(back-end, stream, object, size);
      write(stream, ";\n");
    end;
  end for;
  write-element(stream, '\n');
end method;

define method emit-externs 
    (back-end :: <c-back-end>, stream :: <stream>, cr :: <compilation-record>)
  write(stream, "/* Referenced object declarations */\n\n");
  let heap = cr.compilation-record-model-heap;
  for (object in heap.heap-referenced-objects)
    emit-forward(back-end, stream, object);
  end for;
  for (object in heap.heap-referenced-bindings)
    emit-forward(back-end, stream, object);
  end for;
  write-element(stream, '\n');
end method;

define method emit-forwards
    (back-end :: <c-back-end>, stream :: <stream>, cr :: <compilation-record>)
  write(stream, "/* Defined object declarations */\n\n");
  let heap = cr.compilation-record-model-heap;
  for (object in heap.heap-defined-objects)
    emit-forward(back-end, stream, object);
  end for;
  write-element(stream, '\n');
end method;

define method emit-indirection-definitions
    (back-end :: <c-back-end>, stream :: <stream>, cr :: <compilation-record>)
  write(stream, "/* Indirection variables */\n\n");
  let heap = cr.compilation-record-model-heap;
  for (refs in heap.heap-load-bound-references)
    let object = load-bound-referenced-object(first(refs));
    emit-indirection-definition(back-end, stream, object);
  end for;
  write-element(stream, '\n');
end method;

define method emit-fixups
    (back-end :: <c-back-end>, stream :: <stream>, object, refs)
  write(stream, "{\n");
  format-emit*(back-end, stream, "\t~ T0;\n\n", $dylan-type-string);
  format-emit*(back-end, stream, "\tT0 = ");
  emit-resolve-for-fixup(back-end, stream, object);
  write(stream, ";\n");
  format-emit*(back-end, stream, "\tif (T0 != @) {\n", object);
  let fixed-indirection-variable = #f;
  for (ref in refs)
    if (instance?(ref, <load-bound-code-reference>))
      if (~fixed-indirection-variable)
        fixed-indirection-variable := #t;
        emit-fixup(back-end, stream, object, ref);
      end;
    else
      emit-fixup(back-end, stream, object, ref);
    end;
  end;
  format-emit*(back-end, stream, "\t}\n");
  write(stream, "}\n");
end method;

define method emit-fixup
    (back-end :: <c-back-end>, stream :: <stream>, object, ref)
end method;

define method emit-fixup
    (back-end :: <c-back-end>, stream :: <stream>, 
     object, ref :: <load-bound-code-reference>)
  format-emit*(back-end, stream, "\t\t? = T0;\n", object);
end method;

define method emit-fixup
    (back-end :: <c-back-end>, stream :: <stream>, 
     object, ref :: <load-bound-binding-reference>)
  format-emit*(back-end, stream, "\t\t^ = T0;\n", 
               load-bound-referencing-binding(ref));
end method;

define method emit-fixup
    (back-end :: <c-back-end>, stream :: <stream>, 
     object, ref :: <load-bound-instance-slot-reference>)
  let referencing-object = load-bound-referencing-object(ref);
  let slotd = load-bound-referencing-slot(ref);
  let (primitive, offset) 
    = fixed-slot-primitive-fixup-info
        (^object-class(referencing-object), slotd);
  format-emit*(back-end, stream, "\t\t^(T0, @", 
               primitive, referencing-object);
  format(stream, ", %d);\n", offset);
end method;

define method emit-fixup
    (back-end :: <c-back-end>, stream :: <stream>, 
     object, ref :: <load-bound-repeated-slot-reference>)
  let referencing-object = load-bound-referencing-object(ref);
  let slotd = load-bound-referencing-slot(ref);
  let index = load-bound-referencing-slot-index(ref);
  let (primitive, base-offset) 
    = repeated-slot-primitive-fixup-info
        (^object-class(referencing-object), slotd);
  format-emit*(back-end, stream, "\t\t^(T0, @",
               primitive, referencing-object);
  format(stream, ", %d, %d);\n", base-offset, index);
end method;

// Symbol fixups.

// define constant $primitive-resolve-symbol-string
//   = c-raw-mangle("primitive-resolve-symbol");

define method emit-resolve-for-fixup
    (back-end :: <c-back-end>, stream :: <stream>, object :: <symbol>)
  format-emit(back-end, stream, 1, "^(@);\n", 
              ^iep(dylan-value($symbol-fixup-name)), object);
  // format-emit*
  //  (back-end, stream, "~(@)", $primitive-resolve-symbol-string, object);
end method;

// eof
