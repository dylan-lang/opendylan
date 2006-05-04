module: dfmc-native-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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


define sideways method emit-glue-data
    (back-end :: <native-unix-back-end>, stream, ld) => ()
  let dylan-library? = *compiling-dylan?*;
  output-data(back-end, stream, client?: (~ dylan-library?));
  output-glue(back-end, stream);
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

  let argc = ins--indirect-constant-ref(back-end, c-name(back-end, "TargcT"));
  let argv = ins--indirect-constant-ref(back-end, c-name(back-end, "TargvT"));

  output-external(back-end, stream, argc);
  output-external(back-end, stream, argv);

  let exeentry = 
    invoke-harp(back-end,
		method(back-end :: <harp-back-end>)
		    let argc-param = make-n-register(back-end);
		    let argv-param = make-n-register(back-end);
		    op--c-load-arguments(back-end, argc-param, argv-param);
		    ins--move(back-end, argc, argc-param);
		    ins--move(back-end, argv, argv-param);
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
    (back-end :: <native-unix-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()

  let output-one-fn =
    method (name :: <byte-string>, fn :: <function>, #rest keys)
      apply(invoke-harp, back-end, fn, name, 
            outputter: stream, harp-debug: harp-output?, keys);
    end method;


  let dylan-library? = *compiling-dylan?*;
  output-functions(back-end, stream, output-one-fn, client?: (~ dylan-library?));

end method;

define sideways method main-unit?(back-end :: <native-unix-back-end>) => (main? :: <boolean>)
  #t
end method;
