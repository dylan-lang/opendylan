module: dfmc-pentium-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method register-imports-in-heap
    (back-end :: <pentium-back-end>, cr :: <compilation-record>, heap :: <model-heap>) => ()

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
  (back-end :: <pentium-back-end>, name :: <byte-string>, compiled-lambda) => ()
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


// For Linux, we output only a main entry point for applications;
// There are no shared library entry-points as such.
// Also, cache command line and arguments for future runtime processing.

define sideways method emit-external-entry-points
    (back-end :: <linux-pentium-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()

  let constant-ref = curry(ins--constant-ref, back-end);
  let lib-name = as-lowercase(as(<string>, library-description-emit-name(ld)));
  let name = glue-name(lib-name);
  let name-ref = constant-ref(name);
  let init-dylan-library = ins--indirect-constant-ref(back-end, "_init_dylan_library");

  output-external(back-end, stream, init-dylan-library);

  let dylan-main = c-name(back-end, "dylan_main_0");
  let argc = ins--indirect-constant-ref(back-end, c-name(back-end, "TargcT"));
  let argv = ins--indirect-constant-ref(back-end, c-name(back-end, "TargvT"));

  let dylanexeentry = constant-ref(dylan-main);

  output-external(back-end, stream, dylanexeentry);
  output-external(back-end, stream, argc);
  output-external(back-end, stream, argv);

  let exeentry = 
    invoke-harp(back-end,
		method(back-end :: <harp-back-end>)
		    let argc-param = make-n-register(back-end);
		    let argv-param = make-n-register(back-end);

		    ins--load-stack-arg-n(back-end, argc-param, 0);
		    ins--load-stack-arg-n(back-end, argv-param, 1);
		    ins--move(back-end, argc, argc-param);
		    ins--move(back-end, argv, argv-param);
		    ins--move(back-end, init-dylan-library, name-ref);
		    ins--jmp(back-end, dylanexeentry, 1);
		end method,
		c-name(back-end, "main"),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, exeentry,
			 section: #"init-code",
			 debug-info?: debug-info?);
end method;
