Module:    dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function c-ffi-default-inline-policy () => (policy)
  // #{ inline-only }
  #{ }
end function;

define function parse-boolean-fragment (fragment, #key default = #f)
 => (result)
  macro-case(fragment)
    { #f } => #f;
    { #t } => #t;
    { ?anything-else:* } => default;
  end macro-case;
end function;

define class <C-ffi-result-descriptor> (<object>)
  slot name, init-keyword: name:;
  slot designator-name, init-keyword: designator-name:;
  slot model-type;
  constant slot void? :: <boolean>, init-keyword: void?:, init-value: #f;
end;


define class <c-ffi-argument-descriptor> (<object>)
  slot name, init-keyword: name:;
  slot designator-name, init-keyword: designator-name:;
  slot model-type;
  constant slot call-discipline :: <symbol>,  // input output or in-out
    init-keyword: call-discipline:;
//  slot other-options :: <list> = #();
end;

/*
define class <c-callable-definition> (<variable-defining-form>)
  slot c-name, init-keyword: c-name:;
  slot argument-specs :: <sequence>, init-keyword: argument-specs:;
  slot result-spec :: <C-ffi-result-descriptor>, init-keyword: result-spec:;
  slot dylan-function, init-keyword: dylan-function:;
  slot options :: <sequence>, init-keyword: options:;
end class <c-callable-definition>;

define class <c-function-definition> (<variable-defining-form>)
  slot c-name, init-keyword: c-name:;
  slot argument-specs :: <sequence>, init-keyword: argument-specs:;
  slot result-spec :: <C-ffi-result-descriptor>, init-keyword: result-spec:;
  slot generic-function-method :: <boolean>,
    init-keyword: generic-function-method:, init-value: #f;
  slot options :: <sequence>, init-keyword: options:;
end class <c-function-definition>;
*/


//
// DEFINE C-FUNCTION
//

/*
define option <c-function-c-name-option>
  => c-name: :: expression
end option;

define option <c-function-c-modifiers-option>
  => c-modifiers: :: expression
end option;

define option <c-function-indirect-option>
  => indirect: :: expression excludes <c-function-c-name-option>
end option;

define option <c-function-gf-method-option>
  => generic-function-method: :: expression
end option;

define constant $c-function-options =
  list(<c-function-c-name-option>,
       <c-function-c-modifiers-option>,
       <c-function-indirect-option>,
       <c-function-gf-method-option>);
*/

define &macro C-function-definer
  { define ?mods:* C-function ?dylan-name:name ?spec:* end }
    => 
  begin
    let (arg-specs, result-spec, c-name, options)
      = parse-c-function-spec(dylan-name, spec);
    let (arg-fragments, result-fragment, parameter-list-fragment,
	 return-values-fragment, define-gf?, parameter-names-fragment)
      = parse-early-options(arg-specs, result-spec, options, dylan-name);
    let inline-policy = mods;
    let body = #{ begin
                    if (?=$trace-ffi-calls)
                      // we depend on folding one of the branches for
                      // performance reasons, so $trace-ffi-calls better be
                      // a constant.  apply(values, ...)
                      // isn't optimized, that's why the whole c-function-body
                      // code segment is duplicated here.
                      ?=log-entry(?c-name, ?parameter-names-fragment);
                      let (#rest results) = c-function-body
                                               ?dylan-name
                                               (c-name ?c-name),
                                               (options ??options, ...),
                                               ?result-fragment,
                                               ??arg-fragments, ...
                                             end;
                      apply(?=log-exit, ?c-name, results);
                      apply(values, results)
                    else
                      c-function-body
		        ?dylan-name
                        (c-name ?c-name),
                        (options ??options, ...),
                        ?result-fragment,
                        ??arg-fragments, ...
                      end
                    end
                  end
                };
    if (define-gf?)
      #{ define ?inline-policy method ?dylan-name ?parameter-list-fragment
	  => ?return-values-fragment;
           ?body
	 end }
    else
      #{ define ?inline-policy function ?dylan-name ?parameter-list-fragment
          => ?return-values-fragment;
           ?body
         end }
    end if;
  end

mods:
    { } => c-ffi-default-inline-policy();
    { ?other:* } => #{ ?other };

spec:
    { } => #();
    { ?stuff:*; ... } => pair(stuff, ...);
end &macro;

define method parse-early-options
    (arg-specs :: <sequence>,
     result-spec :: <C-ffi-result-descriptor>,
     function-options :: <sequence>,
     definition-name)
 => (arg-fragments :: <sequence>,
     result-fragment :: <template>,
     parameter-list-fragment :: <template>,
     return-values-fragment :: <template>,
     function-generic? :: <boolean>,
     parameter-names-fragment :: <template>);

  collecting (return-values, arg-fragments, parameters, parameter-names)
    let result-fragment = #f;
    let vname = result-spec.name;
    let type = ~void?(result-spec) & result-spec.designator-name;
    if(~void?(result-spec))
      collect-into(return-values, #{ ?vname :: import-type-for(?type) });
      result-fragment := #{ (result ?vname :: ?type) };
    else
      result-fragment := #{ (result void) }
    end if;

    let indirect-expr = getf(function-options, #"indirect");
    let indirect? = indirect-expr
      & parse-boolean-fragment(indirect-expr, default: #"error");
    unless (instance?(indirect?, <boolean>))
      note(<invalid-c-function-indirect-value>,
	   source-location: fragment-source-location(indirect-expr),
	   definition-name: definition-name,
	   indirect-expression: indirect-expr);
      indirect? := #f;
    end unless;

    if (indirect?)
      let indirect-parameter-name = gensym("functional-parameter");
      collect-into
	(arg-fragments,
	 #{ (parameter ?indirect-parameter-name :: <C-function-pointer>,
	     call-discipline: input:,
	     // Comma is needed to get through the c-function-body macro ...
	     indirect:, #t) });
      collect-into(parameters,
		   #{ ?indirect-parameter-name :: <C-function-pointer> });
    end if;

    for (arg in arg-specs)
      let nn = arg.name;
      let type = arg.designator-name;
      let discipline = arg.call-discipline;
      // let options = arg.other-options;
      // !@#$ may need to check for other parameter options here
      collect-into(arg-fragments, #{ (parameter ?nn :: ?type,
				      call-discipline: ?discipline) }); 
      if (call-discipline(arg) ~= #"input")
	// add a return value
	collect-into(return-values,
		     #{ ?nn :: import-type-for-reference(?type) });
        if (call-discipline(arg) = #"in-out")
	  // parameter to the dylan function
          collect-into(parameter-names, #{ ?nn });
	  collect-into(parameters,
                       #{ ?nn :: export-type-for-reference(?type) })
        end;
      else
        collect-into(parameter-names, #{ ?nn });
        collect-into(parameters, #{ ?nn :: export-type-for(?type) })
      end;

    end for;

    let gf-method-expr = getf(function-options, #"generic-function-method");
    let gf-method? = gf-method-expr
      & parse-boolean-fragment(gf-method-expr, default: #"error");
    unless (instance?(gf-method?, <boolean>))
      note(<invalid-c-function-gf-method-value>,
	   source-location: fragment-source-location(gf-method-expr),
	   definition-name: definition-name,
	   gf-method-expression: gf-method-expr);
      gf-method? := #f;
    end unless;

    let params = collected(parameters);
    let param-names = collected(parameter-names);
    let returns = collected(return-values);
    values(collected(arg-fragments),
	   result-fragment,
	   #{ (??params, ...) },
	   #{ (??returns, ...) },
	   gf-method?,
           #{ ??param-names, ... });
  end collecting;
end method;
	   
// !@#$ this belongs elsewhere...
define method getf (keys :: <sequence>, key, #key default = #f)
  block (return)
    let limit = size(keys);
    iterate loop (i = 0)
      if (i >= limit)
	return(default);
      elseif (keys[i] == key)
	return(keys[i + 1])
      else
	loop(i + 2)
      end if
    end iterate;
    return(default)
  end block;
end method;



define method c-function-parse-parameter-spec
    (kind == #"input",
     spec :: <c-ffi-argument-descriptor>,
     model :: <&designator-class>)
 => (dylan-function-parameter,
     dylan-function-extra-return,
     stack-allocation-head,
     in-out-arg-set-form,
     c-function-parameter,
     c-function-argument,
     extra-return-value)
  let export-type = ^mapped-export-type(model);
  let raw-export-type = ^raw-type-name(model);
  let nom = spec.name;  
  let unboxer = ^unboxer-function-name(model);
  let export-fn = ^export-function(model);
  let export-form 
    = if (export-fn)
        #{ ?export-fn(?nom) }
      else
        nom
      end;
  values(#{ ?nom :: ?export-type },	 // dylan-function-parameter
         #f,				 // dylan-function-extra-return
         #f,				 // stack-allocation-head
         #f,				 // in-out-arg-set-form
         #{ ?nom :: ?raw-export-type },  // c-function-parameter
         #{ ?unboxer(?export-form) },	 // c-function-argument
         #f)				 // extra-return-value
end;

define method c-function-parse-parameter-spec
    (kind == #"output",
     spec :: <c-ffi-argument-descriptor>,
     model :: <&designator-class>)
 => (dylan-function-parameter,
     dylan-function-extra-return,
     stack-allocation-head,
     in-out-arg-set-form,
     c-function-parameter,
     c-function-argument,
     extra-return-value)
  let ref-type-model = ^referenced-type(model);
  unless(designator-class?(ref-type-model))
    note(<output-parameter-not-a-pointer>,
	 source-location: fragment-source-location(spec.designator-name),
	 designator-name: spec.designator-name,
	 parameter-name: spec.name);
    ref-type-model := ^eval-designator(#{ <C-void*> });
  end unless;
  let import-type = ^mapped-import-type(ref-type-model);
  let raw-export-type = ^raw-type-name(model);
  let nom = spec.name;  
  let unboxer = ^unboxer-function-name(model);
  let stack-size = #{ size-of(?ref-type-model) };
  let type-name = spec.designator-name;
  values(#f,				 // dylan-function-parameter
	 #{ ?nom :: ?import-type },	 // dylan-function-extra-return
	 #{ (?nom :: ?type-name, size: ?stack-size) },
	                                 // stack-allocation-head 
	 #f,				 // in-out-arg-set-form
	 #{ ?nom :: ?raw-export-type },  // c-function-parameter
	 #{ ?unboxer(?nom) },	         // c-function-argument
	 #{ pointer-value(?nom) })       // extra-return-value
end method;

define method c-function-parse-parameter-spec
    (kind == #"in-out",
     spec :: <c-ffi-argument-descriptor>,
     model :: <&designator-class>)
 => (dylan-function-parameter,
     dylan-function-extra-return,
     stack-allocation-head,
     in-out-arg-set-form,
     c-function-parameter,
     c-function-argument,
     extra-return-value)
  let ref-type-model = ^referenced-type(model);
  unless (designator-class?(ref-type-model))
    note(<input-output-parameter-not-a-pointer>,
	 source-location: fragment-source-location(spec.designator-name),
	 designator-name: spec.designator-name,
	 parameter-name: spec.name);
    ref-type-model := ^eval-designator(#{ <C-void*> });
  end unless;

  let import-type = ^mapped-import-type(ref-type-model);
  let ref-export-type = ^mapped-export-type(ref-type-model);
  let nom = spec.name;  
  let raw-export-type = ^raw-type-name(model);
  let unboxer = ^unboxer-function-name(model);
  let temp-name = gensym("exported-", nom);
  let type-name = spec.designator-name;
  let stack-size = #{ size-of(?ref-type-model) };
  values(#{ ?nom :: ?ref-export-type},	 // dylan-function-parameter
         #{ ?nom :: ?import-type },	 // dylan-function-extra-return
         #{ (?temp-name :: ?type-name, size: ?stack-size) },
					 // stack-allocation-head 
         #{ pointer-value(?temp-name) := ?nom; }, // in-out-arg-set-form
         #{ ?nom :: ?raw-export-type },  // c-function-parameter
         #{ ?unboxer(?temp-name) },	 // c-function-argument
         #{ pointer-value(?temp-name) }) // extra-return-value
end;



define macro maybe-add-form
  { maybe-add-form(?var:name, ?value:expression) }
  => { if (?value) add!(?var, ?value) end }
end;


define method c-function-parse-input-output-parameters
    (name, specs :: <sequence>)
 => (dylan-function-parameter-list :: <stretchy-vector>,
     dylan-function-extra-returns-list :: <stretchy-vector>,
     stack-allocation-heads :: <stretchy-vector>,
     in-out-arg-set-forms :: <stretchy-vector>,
     c-function-parameter-list :: <stretchy-vector>,
     c-function-arguments :: <stretchy-vector>,
     extra-return-values :: <stretchy-vector>);
  let dylan-function-parameter-list = make(<stretchy-vector>);
  let dylan-function-extra-returns-list = make(<stretchy-vector>);
  let stack-allocation-heads = make(<stretchy-vector>);
  let in-out-arg-set-forms = make(<stretchy-vector>);
  let c-function-parameter-list = make(<stretchy-vector>);
  let c-function-arguments = make(<stretchy-vector>);
  let extra-return-values = make(<stretchy-vector>);
  for (spec in specs)
    let model = spec.model-type;
    unless (model)
      model := ^eval-designator(spec.designator-name);
      unless(designator-class?(model))
	generate-unresolved-designator-error(spec.designator-name,
	  name, #{ c-function-parameter }, #());
        model := ^eval-designator(#{ <C-void*> });
	spec.model-type := model;
	spec.designator-name := #{ <C-void*> };
      end unless;
    end unless;
    let (dylan-function-parameter,
         dylan-function-extra-return,
         stack-allocation-head,
         in-out-arg-set-form,
         c-function-parameter,
         c-function-argument,
         extra-return-value)
     = c-function-parse-parameter-spec(spec.call-discipline, spec, model);
    maybe-add-form(dylan-function-parameter-list, dylan-function-parameter);
    maybe-add-form(dylan-function-extra-returns-list,
                   dylan-function-extra-return);
    maybe-add-form(stack-allocation-heads, stack-allocation-head);
    maybe-add-form(in-out-arg-set-forms, in-out-arg-set-form);
    maybe-add-form(c-function-parameter-list, c-function-parameter);
    maybe-add-form(c-function-arguments, c-function-argument);
    maybe-add-form(extra-return-values, extra-return-value);
  end for;
  values (dylan-function-parameter-list,
	  dylan-function-extra-returns-list,
	  stack-allocation-heads,
	  in-out-arg-set-forms,
	  c-function-parameter-list,
	  c-function-arguments,
	  extra-return-values);
end method c-function-parse-input-output-parameters;


///
/// define c-function my-name
///   output parameter out1 :: <C-int*>;
///   input output parameter inout1 :: <mapped-subtype-of-C-char**>;
///   input output parameter inout2 :: <C-int*>;
///   parameter in1 :: <some-c-struct*>;
///   parameter in2 :: <C-short>;
///   parameter in3 :: <C-double>;
///   parameter strct1 :: <my-struct-by-value*>;
///   result r :: <C-int>;
///   c-name: "my_name";
/// end;
/// =>
/// define method my-name 
///                        // dylan-function-parameter-list
///			   (inout1 ::
///			       //   export-type
///			       <export-type-for-mapped-subtype-of-c-char**>, 
///                         inout2 :: <C-int*>,
///		            in1    :: <some-c-struct*>,
///		            in2    :: <integer>,
///		            in3    :: <double-float>,
///			    strct1 :: <my-struct-by-value*>)
///		        => (r      :: <integer>, 
///			    // dylan-function-extra-returns-list
///		            out1   :: <integer>,    // import-type
///	                    inout1 :: <C-char*>,    // import-type
///		            inout2 :: <integer>)
///   // stack-allocation-heads
///   %with-stack-block(out1 :: <C-int*>, size-of(<C-int>))  // export-type
///     // in-out-arg-set-forms
///     let inout1-temp = export-fn(inout1);

///
///	let r :: <integer> = box-integer(
///      %call-C-function("my_name"),
///          // c-function-parameter-list
///	     ((out1   :: <raw-c-pointer>),        //    raw-export-type
///	      (inout1 :: <raw-c-pointer>),        //    raw-export-type
///	      (inout2 :: <raw-c-pointer>),
///	      (in1    :: <raw-c-pointer>),
///	      (in2    :: <raw-c-short>),
///           (in3    :: <raw-c-double>),
///	      (strct1 :: <my-struct-by-value>))
///	   =>(r :: <raw-c-int>)
///	    // c-function-arguments
///	    (unbox-pointer(out1),  // unbox to raw-export-type
///	     unbox-pointer(inout1-temp),
///          unbox-pointer(inout2),
///	     unbox-pointer(in1),
///	     unbox-integer(in2),
///	     unbox-double(in3),
///	     unbox-pointer(strct1))
///       end;
///     values(r, 
///	       // extra-return-values
///	       pointer-value(out1),
///            pointer-value(inout1-temp),
///	       pointer-value(inout2))
///   end  // with-stack-block
/// end method;
/*

  %call-c-function(string)
      ((name1 :: <parameter-raw-type-or-struct-type>),
       (name2 :: <a-raw-type>),
       ...)
    =>(return-name :: <return-raw-type>)
   (raw-parameter1, raw-parameter2, ...)
  end;
     
  %call-c-function-indirect ()
      ((name1 :: <parameter-raw-type-or-struct-type>),
       (name2 :: <a-raw-type>),
       ...)
    =>(return-name :: <return-raw-type>)
   (raw-c-function-pointer,raw-parameter1, raw-parameter2, ...)
  end;

  functions that return a struct by value:
      
The macro expansion allocates enough stack space and passes a pointer as 
the first argument to %call-c-named-function.
When %call-c-named-function returns either the import function for the
struct is run, or the default import function that allocates space for 
it using something like malloc. and the data is copied to the newly
allocated space, and the pointer is returned.

*/

/*

 C callable:


								       

%define-C-callable-function(optional-string,
			       optional-return-value-pointer,
			       return-value-raw-type,
			       [, raw-parameter :: raw-type]*)
    begin
       body
    end;						       

Each of the raw-parameters is bound inside body.  Body must return
the appropriate value for return-value.

The form returns the raw c pointer to the entry point.
If optional-string is given a symbol is created that will allow C code 
thayt is linked with this code to access the entry point of that name.

The macroexpansion for define c-callable-wrapper must arrange for the
boxers and import functions (if applicable) to get called on the raw
arguments and passed to the function given to c-callable-wrapper.

define c-callable-wrapper $fn-pointer
          of method (x y z) do-something(x, y) end
  output parameter out1 :: <C-int*>;
  input output parameter inout1 :: <C-char*>;
  parameter in1 :: <C-int>;
  result r :: <C-int>;
  c-name: "do_something";
end;

  =>

%define-c-callable-function("do_something",
			    #f,
			    <raw-c-int>,
			    out1 :: <raw-c-pointer>,
			    inout1 :: <raw-c-pointer>,
			    in1 :: <raw-c-int>)
  begin
    let boxed-out1 = as(<C-int*>, box-pointer(out1));
    let boxed-inout1 = as(<C-char*>, box-pointer(inout1));
    let (boxed-result, out1-value, inout1-value) =
      method (x y z) do-something(x, y, z) end
      (as(<C-int*>, box-pointer(inout1)),
       box-integer(in1));
      pointer-value(boxed-out1) := out1-value;
    pointer-value(boxed-inout1) := inout1-value;
    unbox-integer(boxed-result)
  end;

define C-variable-address $fn-pointer :: <C-function-pointer>
       :c-name "do_something"

*/


define &macro make-c-callable
  { make-c-callable ?dylan-name:*
        (c-name ?c-name:expression),
        (options ?key-options:*),
        (dylan-function ?dylan-function:expression),
        ?result-spec:*,
        ?args:*
      end }
  => expand-make-c-callable(form, dylan-name, c-name, args, result-spec,
			    dylan-function, key-options);
args:
  { } => #();
  { ?arg:*, ...}
  => pair(arg, ...);
arg:
  { (parameter ?arg-name:name :: ?type:expression,
     call-discipline: ?discipline:expression,  ?key-options:*) }
  => apply(make,
	   <c-ffi-argument-descriptor>,
	   name: arg-name,
	   designator-name: type,
	   // !@#$ fragment-value bogosity
	   //      should probably be able to to as(<symbol>, discipline)
	   call-discipline: as(<symbol>, fragment-value(discipline)),
	   key-options);
result-spec:
  { (result void) }
  => make(<c-ffi-result-descriptor>,
	  name: gensym(),
	  void?: #t);
  { (result ?result-name:name :: ?type:expression, ?key-options:*) }
  => apply(make,
	   <c-ffi-result-descriptor>,
	   name: result-name,
	   designator-name: type,
	   key-options);
key-options:
   { } => #()
   { ?key:expression, ?value:expression, ... } 
    // !@#$ fragment-value bogosity
    //      should probably be able to to as(<symbol>, discipline)
    => pair(as(<symbol>, fragment-value(key)), pair(value, ...));
c-name:
  { #f } => #f
  { ?x:expression } => x;
end;
  


define method generate-c-callable-body
    (dylan-name, c-name, arg-specs, result-spec, dylan-func, options)
 => (fragment);
  
  let (arg-fragments, result-fragment)
    = parse-early-options(arg-specs, result-spec, options, dylan-name);
  #{ make-c-callable ?dylan-name
        (c-name ?c-name),
        (options ??options, ...),
        (dylan-function ?dylan-func),
        ?result-fragment,
        ??arg-fragments, ...
   end }
end;

//
// DEFINE C-CALLABLE-WRAPPER
//

/*
define option <c-callable-export-option>
  => export: :: expression
end option;

define constant $c-callable-options =
  list(<c-function-c-name-option>,
       <c-function-c-modifiers-option>,
       <c-callable-export-option>);
*/

define &macro C-callable-wrapper-definer
  { define c-callable-wrapper ?dylan-name:name of ?dylan-func:expression
      ?spec end }
  =>
  begin
    let (arg-specs, result-spec, c-name, options)
      = parse-c-function-spec(dylan-name, spec);
    let make-c-callable-fragment =
      generate-c-callable-body(dylan-name, c-name, arg-specs, result-spec,
			       dylan-func, options);
    #{ define constant ?dylan-name = ?make-c-callable-fragment }
  end;
  { define c-callable-wrapper of ?dylan-func:expression
      ?spec end }
  =>
  begin
    let macro-variable = fragment-macro(form);
    let dylan-name
      = gensym(macro-variable, "-pointer-", as (<string>, gensym()));
    let (arg-specs, result-spec, c-name, options)
      = parse-c-function-spec(dylan-name, spec);
    let make-c-callable-fragment =
      generate-c-callable-body("C-callable-wrapper", c-name, arg-specs, result-spec,
			       dylan-func, options);
    #{ define constant ?dylan-name = ?make-c-callable-fragment;
       ?dylan-name } // reference it to avoid warnings.
  end;

spec:
{ } => #();
{ ?stuff:*; ... } => pair(stuff, ...);
{ ?other:* }
  => begin
       note(<unrecognized-clause>,
	    source-location: fragment-source-location(other),
	    definition-name: "C-callable-wrapper");
       #()
     end;
end &macro;


//
// PARSING C FUNCTION/CALLABLE PARAMETERS
//

define option <c-parameter-c-name-option>
  => c-name: :: expression
end option;

define constant $c-parameter-options =
  list(<c-parameter-c-name-option>);

define property <input-adjective-property> => input-adjective?: = #f
  value input = #t;
end property;

define property <output-adjective-property> => output-adjective?: = #f
  value output = #t;
end property;

define constant $c-parameter-properties =
  list(<input-adjective-property>, <output-adjective-property>);

define method parse-c-function-spec (form-name, specs :: <sequence>)
 => (arg-specs :: <sequence>,
     result-spec :: <C-ffi-result-descriptor>,
     c-name :: false-or(<fragment>),
     options :: <sequence>)

  let arg-specs = make(<stretchy-vector>);
  let result-spec :: false-or(<C-ffi-result-descriptor>) = #f;
  let options = make(<stretchy-vector>);

  for (spec in specs)
    macro-case (spec)
      { ?mods parameter ?name:name :: ?designator:expression, ?poptions:* }
      => begin
	   let keys = parse-options($c-parameter-options, poptions, form-name);
	   add!(arg-specs,
		apply(make,
		      <c-ffi-argument-descriptor>,
		      designator-name: designator,
		      call-discipline: mods,
		      name: name,
		      keys))
	 end
      { result ?name:name :: ?designator:expression }
      => if (result-spec)
	   note(<multiple-return-clauses>,
		source-location: fragment-source-location(spec),
		definition-name: form-name);
	 else
	   result-spec := make(<C-ffi-result-descriptor>,
			       designator-name: designator, name: name)
	 end if;
      { ?function-options:* }
      => parse-options!(options, function-options)
      { ?key:symbol ?key-value:expression }
      => begin
	   add!(options, as(<symbol>, key));
	   add!(options, key-value);
	 end;

    mods:
    { ?adjectives:* }
    => begin
         local method process-parameter-adjectives
             (#key input-adjective? = #f, output-adjective? = #f)
          => (keyword)
           if (output-adjective?)
             if (input-adjective?)
               #"in-out";
	     else
               #"output";
	     end if;
	   else
             #"input";
	   end if;
         end method;
         apply(process-parameter-adjectives,
	       parse-property-adjectives($c-parameter-properties,
					 adjectives, form-name));
       end;

    function-options:
      { } => #()
      { ?key:symbol ?value:expression, ... } 
      => pair(as(<symbol>, key), pair(value, ...));
      { ?other:* }
      => begin
	   note(<unrecognized-clause>,
		source-location: fragment-source-location(other),
		definition-name: form-name);
	   #();
	 end;
    end macro-case;
  end for;
  unless (result-spec)
    result-spec := make(<C-ffi-result-descriptor>,
			void?: #t,
			name: gensym());
  end unless;
  values(arg-specs, result-spec, getf(options, #"c-name"), options)
end method parse-c-function-spec;



define method parse-options! (options :: <stretchy-vector>, spec :: <sequence>)
 => ();
  for (index from 0 below size(spec) by 2)
    let key :: <symbol> = as(<symbol>, spec[index]);
    let value :: <fragment> = spec[index + 1];
    add!(options, key);
    add!(options, value);
  end for;
  values();
end method parse-options!;
    

define method  expand-make-c-callable
    (form, dylan-name, c-name-expr, argument-specs, result-desc,
     dylan-function, options)
  let result-designator
    = result-desc
      & ~void?(result-desc)
      & ^eval-designator(result-desc.designator-name);
  if (result-desc
	& ~void?(result-desc)
	& ~designator-class?(result-designator))
    generate-unresolved-designator-error(result-desc.designator-name,
      dylan-name, #{ c-callable-result }, #());
    result-designator := #f;
  end if;
  // fill in model types for parameter descriptors
  do(method (desc)
       desc.model-type := ^eval-designator(desc.designator-name);
       unless (designator-class?(desc.model-type))
	 generate-unresolved-designator-error(desc.designator-name,
	   dylan-name, #{ c-callable-parameter }, #());
	 desc.designator-name := #{ <C-raw-int*> };
         desc.model-type := ^eval-designator(#{ <C-raw-int*> });
       end unless;
     end,
     argument-specs);
  let (all-parameters, passed-on-parameters, output-parameters) =
    callable-parse-input-output-parameters(argument-specs);
  let result-unboxer
    = result-designator & result-designator.^unboxer-function-name;
  let result-name = result-designator & result-desc.name;
  let export-form = result-designator & ^export-function(result-designator);
  let raw-result-form
    = if (result-designator)
	if (export-form)
	  #{ ?result-unboxer(?export-form(?result-name)) };
	else
	  #{ ?result-unboxer(?result-name) };
	end if // (result-desc)
      else // no result-desc so return something harmless.
	#{ integer-as-raw(0) }
      end if;
  let result-raw-type
    = if (result-designator)
	result-designator.^raw-type-name;
      else
	#{ <raw-C-unsigned-long> }
      end if;
  let (output-binding-forms, output-value-locals,
       output-pointer-setting-forms)
    = callable-output-parameter-handling(output-parameters, result-desc);
  let parameter-boxing-forms
    = callable-box-input-parameters(passed-on-parameters);

  let export-expr = getf(options, #"export");
  let export
    = export-expr & ^top-level-eval(export-expr, on-failure: #"failed");
  unless (instance?(export, <boolean>))
    note(<invalid-c-callable-wrapper-export-value>,
       source-location: fragment-source-location(export-expr),
       definition-name: dylan-name,
       export-expression: export-expr);
    export := #f;
  end unless;

  let modifiers-expr = getf(options, #"c-modifiers");
  let modifiers = modifiers-expr & ^top-level-eval(modifiers-expr);
  if (modifiers & ~instance?(modifiers, <string>))
    note(<invalid-c-modifiers-value>,
	 source-location: fragment-source-location(modifiers-expr),
	 definition-name: dylan-name,
	 modifiers-expression: modifiers-expr);
    modifiers := #f;
  end if;

  let c-name = c-name-expr & ^top-level-eval(c-name-expr);
  if (c-name & ~instance?(c-name, <string>))
    note(<invalid-c-name-value>,
	 source-location: fragment-source-location(c-name-expr),
	 definition-name: dylan-name,
	 c-name-expression: c-name-expr);
    c-name := #f;
  end if;

  let options-form
    = if(modifiers)
        if (c-name)
	  #{ ?c-name, c-modifiers: ?modifiers, export: ?export }
	else
	  #{ #f, c-modifiers: ?modifiers, other-name: ?dylan-name,
             export: ?export }
	end;
      elseif (c-name)
	#{ ?c-name, export: ?export }
      else
	#{ #f, other-name: ?dylan-name, export: ?export }
      end if;
  let binding-form = if (empty?(output-value-locals))
		       #{ ?dylan-function(??parameter-boxing-forms, ...); }
		     else
		       #{ let (??output-value-locals, ...)
			   = ?dylan-function(??parameter-boxing-forms, ...); }
		     end;

  #{ make-c-pointer(<C-function-pointer-instantiation>,
		    primitive-cast-pointer-as-raw
                     (%c-callable-function (?options-form)
		        (??all-parameters, ...)
		        => (raw :: ?result-raw-type)
		        ?output-binding-forms;
		      ?binding-form
		        ?output-pointer-setting-forms;
		      ?raw-result-form;
		     end),
                   #[])};
end;
	 
	 
define method callable-box-input-parameters
    (parameters :: <sequence>)
 => (box-forms :: <sequence>);
  let box-forms = make(<stretchy-vector>);
  for (spec in parameters)
    let nom = spec.name;
    let designator = spec.model-type;
    if (spec.call-discipline = #"in-out"
	  & ~designator-class?(^referenced-type(designator)))
      designator := ^eval-designator(#{ <C-raw-int*> });
    end if;
    let low-type = designator.^low-level-type;
    let boxer = designator.^boxer-function-name;
    let import = designator.^import-function;
    let form = if (import)
		 #{ ?import(boxer-for-designator
			      (?low-type,
			       ?nom,
			       ?boxer)) }
	       else
		 #{ boxer-for-designator(?low-type, ?nom, ?boxer) }
	       end;
    if (spec.call-discipline = #"in-out")
      add!(box-forms,
        #{ begin
             let ptr = ?form;
             let null? =
               primitive-machine-word-equal?
                (primitive-cast-pointer-as-raw
                   (primitive-unwrap-c-pointer(ptr)),
                 integer-as-raw(0));
             if (null?) #f else pointer-value(ptr) end
           end });
    else
      add!(box-forms, form);
    end if;
  end for;
  box-forms
end method;


define method callable-output-parameter-handling
    (output-parameters, result-desc)
 => (output-binding-forms :: <template>,
     output-value-binds :: <sequence>,
     output-pointer-setting-forms :: <template>);

  let binding-forms = make(<stretchy-vector>);
  let pointer-setting-forms = make(<stretchy-vector>);
  let return-value-binds = make(<stretchy-vector>);

  if(result-desc & ~void?(result-desc))
    let nom = result-desc.name;
    let type = result-desc.designator-name;
    let form = #{ ?nom :: export-type-for(?type) };
    add!(return-value-binds, form);
  end if;
  for (spec in output-parameters)
    let designator = spec.model-type;
    let nom = spec.name;
    let designator-name = spec.designator-name;
    let type = designator-name;
    unless (designator-class?(^referenced-type(designator)))
      designator-name := #{ <C-raw-int*> }; 
      designator := ^eval-designator(designator-name);
      type := designator-name;
    end;
    let pointer-local-name = gensym(nom, "-output-pointer");
    let return-value-local-name = gensym(nom, "-output-local");
    let return-value-local-bind =
         #{ ?return-value-local-name :: export-type-for-reference(?type) };
    add!(binding-forms,
	 #{ let ?pointer-local-name
	     = make-c-pointer
                 (?designator-name, ?nom, #[]) });
//                 (concrete-class(?designator-name), ?nom, #[]) });
    add!(pointer-setting-forms,
      #{ begin
           let null? = primitive-machine-word-equal?
	                 (primitive-cast-pointer-as-raw
			    (primitive-unwrap-c-pointer(?pointer-local-name)),
			  integer-as-raw(0));
           if (~null?)
             pointer-value(?pointer-local-name) := ?return-value-local-name;
	   end if;
         end });
    add!(return-value-binds, return-value-local-bind);
  end for;
  values(template-sequence-to-template(binding-forms),
	 return-value-binds,
	 template-sequence-to-template(pointer-setting-forms))
end method;

define method template-sequence-to-template (seq :: <sequence>)
 => (statement-sequence-template :: <template>);
  if (empty?(seq))
    #{ begin end }
  else
    #{ ??seq; ... }
  end
end;



define method callable-parse-input-output-parameters
    (specs :: <sequence>)
 => (all :: <sequence>, passed-on :: <sequence>, output :: <sequence>);
  let passed-on = make(<stretchy-vector>);
  let output = make(<stretchy-vector>);
  let all = make(<stretchy-vector>);
  for (spec in specs)
    let nom = spec.name;
    let designator = spec.model-type;
    unless (designator-class?(designator))
      designator := ^eval-designator(spec.designator-name);
      unless (designator-class?(designator))
	generate-unresolved-designator-error(spec.designator-name,
	  "C-callable-wrapper", #{ c-callable-parameter }, #());
	designator := ^eval-designator(#{ <C-void*> });
      end unless;
    end unless;
    let raw-type = designator.^raw-type-name;
    if (spec.call-discipline ~= #"input")
      unless (^referenced-type(designator))
	if (spec.call-discipline = #"output")
	  note(<output-parameter-not-a-pointer>,
	       source-location: fragment-source-location(spec.designator-name),
	       designator-name: spec.designator-name,
	       parameter-name: spec.name);
	else
	  note(<input-output-parameter-not-a-pointer>,
	       source-location: fragment-source-location(spec.designator-name),
	       designator-name: spec.designator-name,
	       parameter-name: spec.name);
	end if;
      end unless;
      add!(output, spec)
    end;
    if (spec.call-discipline ~= #"output")
      add!(passed-on, spec);
    end if;
    add!(all, #{ ?nom :: ?raw-type })
  end for;
  values(all, passed-on, output)
end method callable-parse-input-output-parameters;



define &macro c-function-body
  { c-function-body ?function-name:name
     (c-name ?c-name:expression),
     (options ?key-options:*),
     ?result-spec:*,
     ?args:*
  end }
  => expand-c-function-body(form, function-name, result-spec, args,
			    key-options);
args:
  { } => #();
  { ?arg:*, ...} => pair(arg, ...);
arg:
  { (parameter ?arg-name:name :: ?type:expression,
     call-discipline: ?discipline:expression,  ?key-options:*) }
  => apply(make,
	   <c-ffi-argument-descriptor>,
	   name: arg-name,
	   designator-name: type,
	   // !@#$ fragment-value bogosity
	   //      should probably be able to to as(<symbol>, discipline)
	   call-discipline: as(<symbol>, fragment-value(discipline)),
	   key-options);
result-spec:
  { (result void) }
  => make(<c-ffi-result-descriptor>,
	  name: gensym(),
	  void?: #t);
  { (result ?result-name:name :: ?type:expression, ?key-options:*) }
  => apply(make,
	   <c-ffi-result-descriptor>,
	   name: result-name,
	   designator-name: type,
	   key-options);
key-options:
   { } => #()
   { ?key:expression, ?value:expression, ... } 
	   // !@#$ fragment-value bogosity
	   //      should probably be able to to as(<symbol>, discipline)
    => pair(as(<symbol>, fragment-value(key)), pair(value, ...));
end;

define method expand-c-function-body
    (form :: <fragment>,
     dylan-name :: <variable-name-fragment>,
     result-desc :: <c-ffi-result-descriptor>,
     arg-specs :: <sequence>,
     options :: <sequence>)
 => (expansion);

  let result-designator
    = ~void?(result-desc) & ^eval-designator(result-desc.designator-name);
  unless (void?(result-desc)
	  | designator-class?(result-designator))
    generate-unresolved-designator-error(result-desc.designator-name,
      dylan-name, #{ c-function-result }, #());
    result-designator := #f;
  end unless;

  do(method (desc)
       desc.model-type := ^eval-designator(desc.designator-name);
       unless(designator-class?(desc.model-type))
	 generate-unresolved-designator-error(desc.designator-name,
	   dylan-name, #{ c-function-parameter }, #());
	 desc.model-type := ^eval-designator(#{ <C-void*> });
       end unless;
     end,
     arg-specs);

  let (dylan-function-parameter-list,
       dylan-function-extra-returns-list,
       stack-allocation-heads,
       in-out-arg-set-forms,
       c-function-parameter-list,
       c-function-arguments,
       extra-return-values)
    = c-function-parse-input-output-parameters(dylan-name, arg-specs);

  let c-name-expr = getf(options, #"c-name");
  let c-name = #f;
  if (c-name-expr)
    c-name := ^top-level-eval(c-name-expr);
    unless (instance?(c-name, <string>))
      note(<invalid-c-name-value>,
	   source-location: fragment-source-location(c-name-expr),
	   definition-name: dylan-name,
	   c-name-expression: c-name-expr);
      c-name := #f;
    end unless;
  end if;

  let indirect-expr = getf(options, #"indirect");
  let indirect = #f;
  if (indirect-expr)
    indirect := parse-boolean-fragment(indirect-expr, default: #"error");
    unless (instance?(indirect, <boolean>))
      // Don't need to generate an error here as it was already done
      // during a call to (parse-early-options?)
      indirect := #f;
    end unless;
  end if;

  if (c-name & indirect)
    note(<conflicting-c-name-and-indirect-options>,
	 source-location: fragment-source-location(form),
	 definition-name: dylan-name);
    c-name := #f;
  end if;

  unless (c-name | indirect)
    note(<no-c-name-or-indirect-option>,
	 source-location: fragment-source-location(form),
	 definition-name: dylan-name);
    c-name := "dummy_c_name";
  end unless;

  let result-boxer
    = result-designator & result-designator.^boxer-function-name;
  let result-name = if (result-designator)
		      result-desc.name;
		    else
		      #{ tmp }
		    end if;
  let result-dylan-type
    = result-designator & result-designator.^mapped-import-type;
  let result-raw-type = if (result-designator)
			  result-designator.^raw-type-name;
			else
			  #{ <raw-c-void> }
			end if;
  let result-low-type = if (result-designator)
			  result-designator.^low-level-type
			else
			  #f
			end if;
  let import-function
    = result-designator
      & (result-designator.^import-function
	   | #{ identity });
  let return-values =
    if (result-designator)
      pair( #{ ?result-name }, as(<list>, extra-return-values));
    else
      extra-return-values
    end;

  let modifiers-expr = getf(options, #"c-modifiers", default: #f);
  // this allows it to be a named constant or macro
  let modifiers 
    = if (modifiers-expr)
        let val = ^top-level-eval(modifiers-expr);
        if (~instance?(val, <string>))
	  note(<invalid-c-modifiers-value>,
	       source-location: fragment-source-location(modifiers-expr),
	       definition-name: dylan-name,
	       modifiers-expression: modifiers-expr);
          "";
        else
         val;
       end if;
      else
        ""
      end;

  let foreign-call
    = if (indirect)
	#{ %call-C-function-indirect (c-modifiers: ?modifiers)
	       (??c-function-parameter-list, ...)
            => (?result-name :: ?result-raw-type)
             (??c-function-arguments, ...)
           end }
      else
        #{ %call-C-function (?c-name , c-modifiers: ?modifiers)
	       (??c-function-parameter-list, ...)
	    => (?result-name :: ?result-raw-type)
	     (??c-function-arguments, ...)
	   end }
      end if;
  let result-binding
    = if (result-designator)
	#{ let ?result-name :: ?result-dylan-type
	    = ?import-function (boxer-for-designator
				  (?result-low-type,
				   ?foreign-call,
				   ?result-boxer)) };
      else
	#{ ?foreign-call }
      end if;
  let stack-allocation-size = size(stack-allocation-heads);
  local method build-with-stack-body (i ::<integer>, body)
	  if (i >= stack-allocation-size)
	    body
	  else
	    let stack-head = stack-allocation-heads[i];
	    build-with-stack-body
	      (i + 1,
	       #{ with-stack-block ?stack-head
		   ?body
		  end })
	  end if;
	end method;
  let inner-body
    = build-with-stack-body(0, 
			    #{ ??in-out-arg-set-forms  ...
				?result-binding;
			      values(??return-values, ...) });
  inner-body;
end;



define &macro with-stack-block
  { with-stack-block (?var:name :: ?type:expression, #key ?size:expression)
     ?:body
    end }
    => begin
         // TODO: CORRECTNESS: This pending procedural expansions 
         // retargetable to modules other than dylan internal.
         let destroy 
           = make-variable-name-like
               (var, name: #"destroy", record: #f, source-position: #f);
         #{ begin
              let temp = #f;
	    block ()
	      temp := make(?type, size: ?size);
	      let ?var :: ?type = temp;
	      ?body
	    cleanup
	      if (temp)
	        ?destroy(temp);
	      end if;
	    end block;
	   end  // body
	  }
       end;
end &macro;
