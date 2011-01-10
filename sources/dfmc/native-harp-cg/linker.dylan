module: dfmc-native-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method register-imports-in-heap
    (back-end :: <harp-native-back-end>,
     cr :: <compilation-record>,
     heap :: <model-heap>) => ()
  let ld = cr.compilation-record-library;
  let dylan-library? = dylan-library-library-description?(ld);

  if (dylan-library?)
    $tlv-writer-counter-ref := $tlv-writer-counter;
  else
    $tlv-writer-counter-ref :=
      make-imported-constant-reference
      (back-end, $tlv-writer-counter.cr-refers-to-object);
  end if;

  next-method();
end method;


define sideways method output-compilation-record-data
  (back-end :: <harp-native-back-end>, name :: <byte-string>, compiled-lambda) => ()
  case 
    *current-compilation* =>
      let compiled-lambda =
	if (*loose-mode?*)
	  compiled-lambda
	else
	  // Strip everything but debug-info from the compiled lambda
	  flush-compiled-lambda-post-output(compiled-lambda);
	end if;
      let data :: <string-table> =
	compilation-record-back-end-data(*current-compilation*);
      data[name] := compiled-lambda;
    otherwise => #f;
  end case;
end method;


define sideways method emit-library-initializer
    (back-end :: <native-windows-back-end>, stream, ld,
     emit-call-used :: <method>,
     emit-call-crs :: <method>,
     emit-branch-on-init :: <method>,
     init-name :: <byte-string>,
     harp-output?, debug-info?)
 => ();
  let initializer
    = with-harp-emitter (back-end, stream, init-name,
                         harp-debug: harp-output?,
                         export: #t)
        let return-tag = make-tag(back-end);
        emit-branch-on-init(back-end, return-tag);
        emit-call-used(back-end);
        emit-call-crs(back-end);
        ins--tag(back-end, return-tag);
        ins--rts-and-drop(back-end, 0);
      end with-harp-emitter;
  
  output-compiled-lambda(back-end, stream,
                         initializer,
                         section: #"init-code",
                         debug-info?: debug-info?);
end method;

define sideways method emit-library-initializer
    (back-end :: <native-unix-back-end>, stream, ld,
     emit-call-used :: <method>,
     emit-call-crs :: <method>,
     emit-branch-on-init :: <method>,
     init-name :: <byte-string>,
     harp-output?, debug-info?) => ()

  let local-init-name = concatenate(init-name, "_local_");
  let local-init-ref = ins--constant-ref(back-end, local-init-name);
  let init-dylan-library = ins--indirect-constant-ref(back-end, "_init_dylan_library");
  let so-entry = ins--constant-ref(back-end, "DylanSOEntry");

  output-external(back-end, stream, init-dylan-library);

  output-compiled-lambda(back-end, stream,
			 with-harp-emitter(back-end,
                                           stream,
                                           init-name,
                                           harp-debug: harp-output?,
                                           export: #t)
			   let return-tag = make-tag(back-end);
			   emit-branch-on-init(back-end, return-tag);
			   emit-call-used(back-end);
			   ins--move(back-end, init-dylan-library, local-init-ref);
			   ins--jmp(back-end, so-entry, 1);
			   
			   ins--tag(back-end, return-tag);
			   ins--rts-and-drop(back-end, 0);
			 end with-harp-emitter,
                         section: #"init-code",
                         debug-info?: debug-info?);

  output-compiled-lambda(back-end, stream,
			 with-harp-emitter(back-end,
			                   stream,
							   local-init-name,
							   harp-debug: harp-output?,
							   public: #f,
							   export: #f)
			       
			   emit-call-crs(back-end);
			   ins--rts-and-drop(back-end, 0);

			 end with-harp-emitter,
						 section: #"init-code",
						 debug-info?: debug-info?);
end method;

define sideways method emit-executable-entry-points
    (back-end :: <native-windows-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()
  let constant-ref = curry(ins--constant-ref, back-end);
  let lib-name = as-lowercase(as(<string>, library-description-emit-name(ld)));
  let name = glue-name(lib-name);
  let name-ref = constant-ref(name);
  let dylan-library? = *compiling-dylan?*;
  let mangled-lib-name = harp-raw-mangle(lib-name);
  let init-dylan-library
    = ins--indirect-constant-ref(back-end, "_init_dylan_library");
  let dylandllentry
    = constant-ref(shared-library-runtime-entry-point-name(back-end));

  output-external(back-end, stream, init-dylan-library);
  output-external(back-end, stream, dylandllentry);

  let dllentry = 
    invoke-harp(back-end,
		method (back-end :: <harp-back-end>)
                  ins--move(back-end, init-dylan-library, name-ref);
                  ins--jmp(back-end, dylandllentry, 1);
                end method,
		shared-library-entry-point-name(back-end, mangled-lib-name),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, dllentry,
                         section: #"init-code",
			 debug-info?: debug-info?);

  let dylanexeentry = constant-ref(c-name(back-end, "dylan_main"));
  output-external(back-end, stream, dylanexeentry);
  let exeentry = 
    invoke-harp(back-end,
                method(back-end :: <harp-back-end>)
		    ins--move(back-end, init-dylan-library, name-ref);
		    ins--jmp(back-end, dylanexeentry, 1);
		end method,
		c-name(back-end, concatenate(mangled-lib-name, "Exe")),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, exeentry,
			 section: #"init-code",
			 debug-info?: debug-info?);

  let dylan-main = c-name(back-end, "dylan_main_0");
  let dylanexeentry = constant-ref(dylan-main);
  output-external(back-end, stream, dylanexeentry);

  let exeentry = 
    invoke-harp(back-end,
		method(back-end :: <harp-back-end>)
		    ins--move(back-end, init-dylan-library, name-ref);
		    ins--jmp(back-end, dylanexeentry, 1);
		end method,
		c-name(back-end, concatenate(mangled-lib-name, "Exe0")),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, exeentry,
			 section: #"init-code",
			 debug-info?: debug-info?);

  unless (dylan-library?)
    cache-import-in-library(back-end, dylan-main, dylan-library-description());
  end unless;
end method;

define sideways method emit-executable-entry-points
    (back-end :: <native-unix-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()

  let constant-ref = curry(ins--constant-ref, back-end);
  let lib-name = as-lowercase(as(<string>, library-description-emit-name(ld)));
  let name = glue-name(lib-name);
  let name-ref = constant-ref(name);
  let exit-application = ins--constant-ref(back-end, "primitive_exit_application");

  if (main-unit?(back-end))
    output-external(back-end, stream, name-ref);
  end;

  output-external(back-end, stream, exit-application);

  let exeentry = 
    invoke-harp(back-end,
		method (back-end :: <harp-back-end>)
                  ins--call(back-end, name-ref, 0);
                  ins--move(back-end, back-end.registers.reg-arg0, 0);
                  ins--jmp(back-end, exit-application, 1);
		end method,
		c-name(back-end, "main"),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, exeentry,
			 section: #"init-code",
			 debug-info?: debug-info?);

end method;

define sideways method emit-shared-library-entry-points
    (back-end :: <native-windows-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()
end method;

define sideways method emit-shared-library-entry-points
    (back-end :: <native-unix-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()
  let constant-ref = curry(ins--constant-ref, back-end);
  let lib-name = as-lowercase(as(<string>, library-description-emit-name(ld)));
  let mangled-lib-name = harp-raw-mangle(lib-name);

  let dylandllexit = constant-ref(c-name(back-end, "DylanSOExit"));
  output-external(back-end, stream, dylandllexit);

  let dllexit = 
    invoke-harp-asm(back-end,
                    method (back-end :: <harp-back-end>)
                      ins--call(back-end, dylandllexit, 0);
                    end method,
                    concatenate(mangled-lib-name, "SOexit"),
                    section: #"elf-fini-code",
                    harp-debug: harp-output?,
                    export: #f);
  
  output-compiled-lambda(back-end, stream, dllexit,
                         section: #"elf-fini-code",
			 debug-info?: debug-info?);
end method;

define sideways method main-unit?
    (back-end :: <native-windows-back-end>) => (main? :: <boolean>)
  #f
end method;

define sideways method main-unit?
    (back-end :: <native-unix-back-end>) => (main? :: <boolean>)
  #t
end method;
