Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro with-harp-emitter
  { with-harp-emitter (?back-end:expression, ?stream:expression, ?name:expression, ?keys:*) ?:body end }
    => { begin

           let main =
             method(back-end :: <harp-back-end>)
		 let old-variables = back-end.cg-variables;

                 dynamic-bind (
			       *harp-outputter*     = ?stream,
			       *emitting-data?*     = #f,
			       *tail-calls*         = #())

		   with-harp-variables(back-end,
				       prototype: back-end.cg-variables)
		     ?body
		   end with-harp-variables;

               end dynamic-bind;
             end method;

           invoke-harp(?back-end, main, ?name, ?keys);


	 end }
end macro with-harp-emitter;

define macro with-harp-init-emitter
  { with-harp-init-emitter (?back-end:expression, ?name:expression, ?keys:*) ?:body end }
    => { begin
	invoke-harp(?back-end,
		    method(back-end :: <harp-back-end>)
			
			dynamic-bind (*emitting-init-code?* = #t,
				      *emitting-data?*      = #f)

			ins--adjust-stack(back-end, 0);

                        ?body;

			ins--rts-and-drop(back-end, 0);
		    end;
		   end method,
	     ?name,
	     ?keys);

	 end }
end macro with-harp-init-emitter;

define macro with-harp-variables
  { with-harp-variables (?back-end:expression, ?keys:*) ?:body end }
    => { 
	  let old-variables = ?back-end.cg-variables;

	  block()
	    ?back-end.cg-variables := make(<harp-cg-variables>, ?keys);
	    ?body
	  cleanup
	    ?back-end.cg-variables := old-variables;
	  end block;

	  }
end macro with-harp-variables;

define macro with-harp-outputter
  { with-harp-outputter (?back-end:expression, ?stream:expression, ?ld:expression, ?keys:*)
      ?body:body
    end }
    => { 
	  let locator = build-area-output-locator(?ld, ?keys);

	  ?stream := open-emit-output(?back-end, locator, ?keys);

          dynamic-bind (*harp-outputter* = ?stream,
			*stream-outputters?* = ?stream.stream-outputters?)
	    block()
	      ?body
	    cleanup
	      if (?stream)
	        close-emit-output(?back-end, ?stream, locator);
	      end if;
            end block;
          end dynamic-bind;
	  
       }
end macro with-harp-outputter;

define function stream-outputters?(outputter :: <harp-outputter>) => (stream-outputters? :: <boolean>)

  let stream? = #f;
  local method stream-outputter?(outputter :: <harp-outputter>)
	    stream? := stream? | instance?(outputter, <stream-wrapper-outputter>);
	end method;

  do-outputters(stream-outputter?, outputter);

  stream?;

end function;

// Make runtime-models for entry-points and primitives

define macro entry-points-definer
  { define entry-points ?type:name ?:name }
    =>
  {
   define constant "$" ## ?name ## "s" =
     make-entry-points(?"name", "$number-" ## ?type ## "s", make-runtime-object);
  }
end macro entry-points-definer;

define macro code-entry-points-definer
  { define code-entry-points ?type:name ?:name }
    =>
  {
   define constant "$" ## ?name ## "s" =
     make-entry-points(?"name", "$number-" ## ?type ## "s", make-runtime-reference);
  }
end macro code-entry-points-definer;

define function make-entry-points
    (entry-point :: <string>, number-entry-points :: <integer>, make-reference :: <function>)
 => (entry-points :: <vector>)
  let entry-points = make(<vector>, size: number-entry-points + 1);
  let raw-name = harp-raw-mangle(as-lowercase(entry-point));

  for (i from 0 below number-entry-points)
    entry-points[i] := format-to-string("%s_%d", raw-name, i)
  end for;
  entry-points[number-entry-points] := raw-name;
  map(make-reference, entry-points);
end function;


define macro named-entry-points-definer
  { define named-entry-points ?type:name ?:name }
    =>
  {
   make-named-entry-points(?"name", "$number-" ## ?type ## "s");
  }
end macro named-entry-points-definer;

define function make-named-entry-points(entry-point :: <string>, number-entry-points :: <integer>)
 => ()

  let entry-point = as-lowercase(entry-point);
  for (i from 0 below number-entry-points)
    make-entry-point(format-to-string("%s-%d", entry-point, i))
  end for;
  make-entry-point(entry-point)
end function;


define macro named-entry-point-definer
  { define named-entry-point ?:name }
    =>
  {
   make-entry-point(?"name");
  }
end macro named-entry-point-definer;

define method make-entry-point
  (entry-point :: <string>,
   #key module, library) => (entry-point)
  let raw-name = 
    if (module & library)
      format-to-string("%s%s",
		       harp-dylan-mangle(entry-point, module, library),
		       $iep-suffix);
    else
      harp-raw-mangle(as-lowercase(entry-point));
    end if;
  $named-entry-points[raw-name] := make-runtime-object(raw-name)
end method;

define constant $harp-mangler = make(<mangler>);

define method harp-dylan-mangle
  (name :: <string>, module :: <string>, library :: <string>) => (mangled-name :: <string>)
  concatenate($constant-prefix,
	      mangle-binding-spread($harp-mangler,
				    as-lowercase(name),
				    as-lowercase(module),
				    as-lowercase(library)));
end method;

define macro named-entry-point-in-definer
  { define named-entry-point-in ?module:name ?library:name ?:name }
    =>
  {
   make-entry-point(?"name",
                    module: ?"module",
                    library: ?"library");
  }
end macro named-entry-point-in-definer;


define macro runtime-reference-definer
  { define runtime-reference ?:name }
    =>
  {
   define constant "$" ## ?name =
     make-runtime-reference(harp-raw-mangle(as-lowercase(?"name")));
  }
end macro runtime-reference-definer;

define macro local-runtime-reference-definer
  { define local-runtime-reference ?:name }
    =>
  {
   define constant "$" ## ?name =
     make-local-runtime-reference(harp-raw-mangle(as-lowercase(?"name")));
  }
end macro local-runtime-reference-definer;

define macro c-runtime-reference-definer
  { define c-runtime-reference ?:name }
    =>
  {
   define constant "$" ## ?name =
     make-c-runtime-reference(harp-raw-mangle(as-lowercase(?"name")));
  }
end macro c-runtime-reference-definer;

define macro dylan-reference-definer
  { define dylan-reference ?:name ?module:name ?library:name}
    =>
  {
   define constant "$" ## ?name =
     harp-dylan-mangle(?"name", ?"module", ?"library");
  }
end macro dylan-reference-definer;

define macro harp-operation-definer
  { define harp-operation ?:name(?argument-class:name, ?result-class:name) ?operation:name
  }
    =>
  { 
    define method "op--" ## ?name ## "%" (register :: ?argument-class) => (?name :: ?result-class)
      ?operation
    end;
  }
end macro;


define macro call-protocol
  { call-protocol
     (?protocol:name, ?back-end:name)
     (?emitters:*) (?arguments:*)
    end }
    =>
    { call-protocol-aux
       (?protocol, ?back-end)
       (?emitters) (?arguments) ()
      end }
end macro;

define macro call-protocol-aux
  { call-protocol-aux
     (?protocol:name, ?back-end:name)
     () ()
     (?processed-arguments:*)
    end }
    =>
    { ?protocol(?back-end, ?processed-arguments) }

  { call-protocol-aux
     (?protocol:name, ?back-end:name)
     () (rest ?arguments:expression)
     (?processed-arguments:*)
    end }
    =>
    { apply(?protocol, ?back-end, ?processed-arguments,
	    emit-references(?back-end, ?arguments)) }

  { call-protocol-aux
     (?protocol:name, ?back-end:name)
     (?emitter:name, ?emitters:*) (?argument:expression, ?arguments:*)
     ()
    end }
    =>
    { call-protocol-aux
       (?protocol, ?back-end) (?emitters) (?arguments)
       ("emit-" ## ?emitter(?back-end, #f, ?argument))
      end }

  { call-protocol-aux
     (?protocol:name, ?back-end:name)
     (?emitter:name, ?emitters:*) (?argument:expression, ?arguments:*)
     (?processed-arguments:*)
    end }
    =>
    {  call-protocol-aux
       (?protocol, ?back-end) (?emitters) (?arguments)
       (?processed-arguments, "emit-" ## ?emitter(?back-end, #f, ?argument))
       end }
end macro;


define macro emit-identity
  { emit-identity
     (?back-end:name, #f, ?argument:expression) }
    =>
    { ?argument }
end macro;

define macro preserving-cleanup-state
  { preserving-cleanup-state (?back-end:name) ?:body end }
    =>
  { 
   // Preserve stack-pointer in unwind-protect cleanup to allow
   // for stack allocation in cleanup body
   let stack-pointer = make-n-register(?back-end);
   ins--move(?back-end, stack-pointer, ?back-end.registers.reg-stack);

   op--cleanup-preserve-state-entry(?back-end);

   ?body;

   op--cleanup-preserve-state-exit(?back-end);

   ins--move(?back-end, ?back-end.registers.reg-stack, stack-pointer);

  }
end macro;

define open generic op--cleanup-preserve-state-entry
    (back-end :: <harp-back-end>) => ();

define method op--cleanup-preserve-state-entry
    (back-end :: <harp-back-end>) => ()
end method;

define open generic op--cleanup-preserve-state-exit
    (back-end :: <harp-back-end>) => ();

define method op--cleanup-preserve-state-exit
    (back-end :: <harp-back-end>) => ()
end method;
