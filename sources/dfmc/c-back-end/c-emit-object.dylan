Module: dfmc-c-back-end
Author: Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// REFERENCES

// Direct objects are always emitted in-line. References to indirect objects
// are genuine references to previously dumped structures.

// because of multiple inheritance, must override default emit-reference
// method for direct-object

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o) => ()
  if (direct-object?(o))   // !@#$ need <direct-object>
    emit-object(back-end, stream, o);
  else
    format-emit*(back-end, stream, "&^", o);
  end if
end method;

// INDIRECT REFERENCES

define method emit-indirect-reference
    (back-end :: <c-back-end>, stream :: <stream>, o) => ()
  format-emit*(back-end, stream, "~^", $indirection-prefix, o);
end method;

//// CLASS-SPECIFIC EMISSION

// RAW TYPES

// Raw types have no run-time presence. The heap walker doesn't include
// raw types in the heap, but references may remain in some situations
// (signatures for example), although this problem may go away.

// Raw type references are replaced by references to a "static type"
// type marker. This is currently just <object>.

// TODO: Define a distinguished raw type marker is we still need to
// emit raw type references.

define method raw-type-marker () dylan-value(#"<object>") end;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&raw-type>)
 => ()
  emit-reference(back-end, stream, raw-type-marker())
end method;

// WORK AROUND TO PREVENT WARNINGS

// Previously, this code was specialized on <back-end>, but
// then we'd get ambiguous method warnings.

define method emit-object
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&engine-node>) => (object);
  ^engine-node-callback(o);
  next-method();
end method;

// RAW VALUES

define method emit-object
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&raw-object>)
 => ()
  print-raw-object(o.^raw-object-value, stream);
end method;

define method print-raw-object (o :: <object>, stream :: <stream>) => ()
  print-object(o, stream)
end method;

define method emit-object
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&raw-boolean>)
 => ()
  print-object(if (o.^raw-object-value) 1 else 0 end, stream);
end method;

// BOOLEANS

/* TODO: TAGGED BOOLEANS
define method emit-object
    (back-end :: <c-back-end>, stream :: <stream>, o :: <boolean>)
 => ()
  write(stream, if (o) $true-string else $false-string end); 
end method;
*/

// FLOATS

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <float>) => ()
  emit-object(back-end, stream, o);
end method;

define method emit-object
    (back-end :: <c-back-end>, stream :: <stream>, o :: <float>) => ()
  print-raw-object(o, stream)
end method;

define method print-raw-object (o :: <float>, stream :: <stream>) => ()
  let s = float-to-string(o);
  //---*** Is there a better way to do this???
  block (done)
    let i = size(s) - 1;
    while (i > -1)
      select (s[i])
	's' => s[i] := 'e'; done();	//---*** Should be 'f' but GCC complains!
	'd' => s[i] := 'e'; done();
	'x' => s[i] := 'e'; done();	//---*** Should be 'l' but GCC complains!
	otherwise => ;
      end;
      i := i - 1
    end
  end;
  write(stream, s)
end method;

// INTEGERS

define method emit-object
    (back-end :: <c-back-end>, stream :: <stream>, c :: <integer>) => ()
  write(stream, "(D) ");
  print-raw-object(generic-logior(generic-ash(c, 2), 1), stream);
end method;

define method print-raw-object (o :: <double-integer>, stream :: <stream>) => ()
  if (zero?(%double-integer-high(o)))
    format(stream, "0x%sL", machine-word-to-string(%double-integer-low(o), prefix: #f))
  else
    format(stream, "0x%s%sL", machine-word-to-string(%double-integer-high(o), prefix: #f),
	                      machine-word-to-string(%double-integer-low(o), prefix: #f))
  end if
end method;

define method print-raw-object (o :: <machine-word>, stream :: <stream>) => ()
  format(stream, "0x%sL", machine-word-to-string(o, prefix: #f))
end method;

// CHARACTERS

define constant $delete-character = as(<character>, 127);

define method graphic? (character :: <character>)
  let code :: <integer> = as(<integer>, character);
  code >= as(<integer>, ' ') & code < as(<integer>, $delete-character)
end method graphic?;

define method emit-raw-character-data // !@#$ should be <byte-character>
    (back-end :: <c-back-end>, stream :: <stream>, c :: <character>)
 => ()
  select (c) 
    '\\' => write(stream, "\\\\");
    '\"' => write(stream, "\\\"");
    '\'' => write(stream, "\\'");
    '\n' => write(stream, "\\n");
    '\f' => write(stream, "\\f");
    '\t' => write(stream, "\\t");
    '\r' => write(stream, "\\r");
    otherwise =>
      if (c.graphic?)
	write-element(stream, c);
      else
	format(stream, "\\x%x", as(<integer>, c));
      end if;
  end select
end method;

define method emit-object // !@#$ should be <byte-character>
    (back-end :: <c-back-end>, stream :: <stream>, c :: <character>)
 => ()
  write(stream, "C('");
  emit-raw-character-data(back-end, stream, c);
  write(stream, "')");
end method;

/*
define inline function do-emit-name-using-emitter
    (back-end :: <c-back-end>, stream, o,
     emitted-name :: <function>, emitted-name-setter :: <function>,
     emitter :: <function>)
 => (name)
  unless (instance?(o.emitted-name, <byte-string>))
    o.emitted-name := emitter(back-end);
  end unless;
  write(stream, o.emitted-name);
end function;

define macro emit-name-using-emitter
  { emit-name-using-emitter 
       (?back-end:expression, ?stream:expression, ?object:expression, 
        ?emitted-name:expression, ?emitted-name-setter:expression, 
        ?emit-value:expression) }
 => { do-emit-name-using-emitter
        (?back-end, ?stream, ?object, ?emitted-name, ?emitted-name-setter,
         method (?=back-end) ?emit-value end) }
end method;
*/

/// USED FOR EMULATOR SPEED IMPROVEMENT

define macro emit-name-using-emitter
  { emit-name-using-emitter 
       (?back-end:expression, ?stream:expression, ?object:expression, 
        ?emitted-name:expression, ?emitted-name-setter:expression, 
        ?emit-value:expression) }
 => { unless (instance?(?emitted-name(?object), <byte-string>))
        ?emitted-name-setter(?emit-value, ?object);
      end unless;
      write(?stream, ?emitted-name(?object)) }
end macro;

define method emit-reference 
    (back-end :: <c-back-end>, stream :: <stream>, o :: <module-binding>)
 => () 
  // We check to see if the binding is defined here so that we don't
  // end up with an error when using a symbol from another library
  // that is exported but undefined. The compiler has already
  // emitted warnings so we can just silently continue at this
  // point to avoid a compiler crash.
  if (o.emitted-name | o.binding-defined?)
    emit-name-using-emitter
      (back-end, stream, o, emitted-name, emitted-name-setter,
       format-to-string("%s", global-mangle(back-end, o)))
  end if;
end method;

define method same-name? (x, y) x == y end;

define method same-name? 
    (x :: <variable-name-fragment>, y :: <variable-name-fragment>) => (same?)
  x.fragment-identifier = y.fragment-identifier
end method;
  
define method ambiguous-lexical-variable? 
    (env :: <lambda-lexical-environment>, var :: <temporary>) 
 => (ambiguous? :: <boolean>)
  block (return)
    for-temporary (tmp in env)
      if (tmp ~== var 
            & same-name?(var.name, tmp.name))
        return(#t);
      end if;
    end for-temporary;
    #f
  end block;
end method;

// Hacky workaround for the fact that
// the frame-offset of two anonymous
// lexical variables can be the same.
define thread variable *name-salt* = 1;

define method ambiguous-parameter? 
    (parameters :: <sequence>, var :: <temporary>) 
 => (ambiguous? :: <boolean>)
  block (return)
    for (tmp in parameters)
      if (tmp ~== var 
            & same-name?(var.name, tmp.name))
          if (tmp.frame-offset == var.frame-offset)
            var.frame-offset := tmp.frame-offset + *name-salt*;
            *name-salt* := *name-salt* + 1;
          end;
        return(#t);
      end if;
    end for;
    #f
  end block;
end method;

define function anonymous-temporary-at? 
    (env :: <lambda-lexical-environment>, var :: <temporary>) 
 => (anonymous? :: <boolean>)
  if (named?(var))
    // let offset = var.frame-offset;
    // block (return)
    //   for-temporary (tmp in env)
    //     if (tmp ~== var & tmp.frame-offset = var.frame-offset)
    //	     return(#t);
    //     end if;
    //   end for-temporary;
    //   #f
    // end block;
    #f
  else
    #t
  end if;
end function;

define constant $anonymous-temporary-cache = make(<object-table>);

define function emit-named-temporary
    (back-end :: <c-back-end>, stream :: <stream>, o :: <temporary>)
 => ()
  emit-name-using-emitter
    (back-end, stream, o, emitted-name, emitted-name-setter,
     if (anonymous-temporary-at?(o.environment, o))
       let number = o.frame-offset;
       element($anonymous-temporary-cache, number, default: #f)
         | (element($anonymous-temporary-cache, number)
              := format-to-string("T%d", number));
     elseif (ambiguous-lexical-variable?(o.environment, o)) 
       format-to-string
	 ("%s", hygienic-mangle(back-end, o.name, o.frame-offset));
     else
       format-to-string
	 ("%s", local-mangle(back-end, o.name));
     end if);
end function;

define method emit-object 
    (back-end :: <c-back-end>, stream :: <stream>, o :: <temporary>)
 => ()
  emit-named-temporary(back-end, stream, o);
end method;

define method emit-object 
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <lexical-local-variable>)
 => ()
  emit-named-temporary(back-end, stream, o);
end method;

define method emit-object 
    (back-end :: <c-back-end>, stream :: <stream>, o :: <lexical-variable>)
 => ()
  let lambda = o.environment.lambda-environment.lambda;
  emit-name-using-emitter
    (back-end, stream, o, emitted-name, emitted-name-setter,
     if (ambiguous-parameter?(parameters(lambda), o)) 
       format-to-string
	 ("%s", hygienic-mangle(back-end, o.name, o.frame-offset));
     else
       format-to-string
	 ("%s", local-mangle(back-end, o.name));
     end if);
  // emit-named-temporary(back-end, stream, o);
  // emit-name-using-emitter
  //   (back-end, stream, o, emitted-name, emitted-name-setter,
  //    format-to-string("%s", local-mangle(back-end, o.name)));
end method;

/// STRUCT MANGLING

define method emit-struct-name
    (back-end :: <c-back-end>, stream :: <stream>, class :: <&class>) => ()
  format-emit*(back-end, stream, "_^", class);
end method;

define method emit-struct-definer-name
    (back-end :: <c-back-end>, stream :: <stream>, class :: <&class>) => ()
  write(stream, "define_");
  emit-struct-name(back-end, stream, class);
end method;

define method emit-repeated-struct-definer-name
    (back-end :: <c-back-end>, stream :: <stream>, 
     class :: <&class>, size :: <integer>) 
 => ()
    emit-struct-definer-name(back-end, stream, class);
    format(stream, "(%d)", size)
end method;

define method emit-struct-field-name 
    (back-end :: <c-back-end>, stream :: <stream>, 
     class :: <&class>, slotd :: <&slot-descriptor>, position) => ()
  emit-name-using-emitter
    (back-end, stream, slotd, emitted-type-name, emitted-type-name-setter,
     if (slotd.^debug-name)
       c-local-mangle(slotd.^debug-name)
     else
       c-local-mangle(format-to-string("anon-slot-%d", position))
     end);
end method;

// NOTE: The mangling for objects with a repeated slot must match the
//       mangling in the corresponding C preprocessor macro.

define method emit-repeated-struct-name 
    (back-end :: <c-back-end>, stream :: <stream>, 
     c :: <&class>, size :: <integer>) => ()
  emit-struct-name(back-end, stream, c);
  format(stream, "_%d", size);
end method;

define method emit-type-name // !@#$ NEED UNIFYING MODEL TYPE
    (back-end :: <c-back-end>, stream :: <stream>, o :: <object>) => ()
  let rslotd = o.&object-class.^repeated-slot-descriptor;
  if (rslotd)
    emit-repeated-struct-name
      (back-end, stream, o.&object-class, 
       ^slot-value(o, ^size-slot-descriptor(rslotd)))
  else
    emit-struct-name(back-end, stream, o.&object-class)
  end;
end method;

// CODE

define constant $number-xeps = 10;

define method emit-xep-reference 
    (back-end :: <c-back-end>, stream :: <stream>, ep :: <&lambda-xep>) => ()
  let req-size = ^entry-point-number-required(ep);
  let size 
    = if (^entry-point-key?(ep))
        req-size + ^entry-point-number-keys(ep) + 1
      else
        req-size
      end if;
  format(stream, if (size < $number-xeps) "&%s_%d" else "&%s" end,
         if (^entry-point-key?(ep))
	   $rest-key-xep-string
         elseif (^entry-point-rest?(ep))
           $rest-xep-string 
         else 
           $xep-string 
         end if,
         size);
end method;


define method emit-xep-reference
    (back-end :: <c-back-end>, stream :: <stream>, ep :: <&slot-accessor-xep>) => ()
  format(stream, "&%s", ^entry-point-name(ep))
end method;

// This reflects the number of implementation args (required args plus maybe optionsl
// vector) the g.f. takes.  Zero through this many implementation args are passed
// spread as separate C arguments by the dispatch engine routines;  more will be
// passed as a single (hopefully) stack- allocated vector.

define constant $special-gf-engine-max-args = 7;

define method emit-xep-reference 
    (back-end :: <c-back-end>, stream :: <stream>, ep :: <&generic-function-xep>)
 => ()
  let req-size :: <integer> = ^entry-point-number-required(ep);
  let optionals? = ^entry-point-optionals?(ep);
  let impargs :: <integer> = if (optionals?) req-size + 1 else req-size end;
  format(stream, 
         if (impargs <= $special-gf-engine-max-args)
           "&%s_%d"
         else
           "&%s"
         end, 
         if (optionals?) 
           $gf-optional-xep-string
         else 
           $gf-xep-string 
         end if,
         req-size);
end method;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&xep>) => ()
  emit-xep-reference(back-end, stream, o);
end method;

define constant $number-meps = $number-xeps;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&keyword-method-mep>) => ()
  let size 
    = ^entry-point-number-required(o) + ^entry-point-number-keys(o) + 1;
  format(stream, if (size < $number-meps) "&%s_%d" else "&%s" end, 
	 $key-mep-string, size);
end method;


define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&engine-node-ep>) => ();
  emit-engine-node-ep-reference(back-end, stream, ^engine-node(o), o)
end method;

define method emit-engine-node-ep-reference
    (back-end :: <c-back-end>, stream :: <stream>, e :: <&engine-node>, o :: <&engine-node-ep>)
 => ()
  format(stream, "&%s_engine", raw-mangle(back-end, ^entry-point-name(o)))
end method;

define method emit-engine-node-ep-reference
    (back-end :: <c-back-end>, stream :: <stream>, 
     e :: <&engine-node>, o :: <&function-linked-engine-node-ep>)
 => ()
  let epstr :: <byte-string> = raw-mangle(back-end, ^entry-point-name(o));
  let req-size :: <integer> = ^engine-node-ep-number-required(o);
  let mepargs :: <integer> = if (^engine-node-ep-optionals?(o)) req-size + 1 else req-size end;
  if (mepargs > $special-gf-engine-max-args)
    format(stream, "&%s_engine_n", epstr)
  else
    format(stream, "&%s_engine_%d", epstr, mepargs)
  end if;
end method;

define method emit-engine-node-ep-reference
    (back-end :: <c-back-end>, stream :: <stream>, 
     e :: <&discriminator>, ep :: <&discriminator-ep>) 
 => ()
  let epname :: <symbol> = ^entry-point-name(ep);
  let epstr :: false-or(<byte-string>)
    = select (epname)
	#"discriminate-on-argument" => "discriminate";
	#"if-type" => "if_type_discriminator";
	#"typecheck" => "typecheck_discriminator";
      end select;
  let req-size :: <integer> = ^discriminator-nrequired(e);
  if (req-size > $special-gf-engine-max-args)
    format(stream, "&%s_engine_n_n", epstr)
  else
    let nmepargs :: <integer> 
      = if (^discriminator-optionals?(e)) req-size + 1 else req-size end;
    format(stream, "&%s_engine_%d_%d", epstr, ^discriminator-argnum(e) + 1, nmepargs)
  end if
end method;


define method emit-code (back-end :: <c-back-end>, o) => ()
end method;

define method emit-parameter-type
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <&raw-type>, #key index :: false-or(<integer>))
  format-emit*(back-end, stream, "^", o);
end method;


define method emit-parameter-type
    (back-end :: <c-back-end>, stream :: <stream>, 
     o, #key index :: false-or(<integer>))
  write(stream, $dylan-type-string);
end method;

define method emit-return-types
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>)
  let signature = ^function-signature(function(o));
  if (~signature | spec-value-rest?(signature-spec(function(o)))
       // !@#$ avoid problems with accessor returning raw values
       | ^instance?(o.function, dylan-value(#"<accessor-method>")))
    emit-parameter-type(back-end, stream, dylan-value(#"<object>"));
  else
    emit-parameter-type
      (back-end, stream, 
       first(^signature-values(signature), default: dylan-value(#"<object>")));
  end if;
end method;

// CALLED WHEN DFM IS PRESENT

define function emit-parameters 
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>,
     parameters :: <sequence>, sig :: false-or(<&signature>)) => ()
  // !@#$ avoid problems with accessor declaring raw values
  let accessor? = ^instance?(o.function, dylan-value(#"<accessor-method>"));
  write-element(stream, '(');
  let parm-ix = 0;
  let first? = #t;
  local method emit-parameter (type)
	  unless (first?)
	    write(stream, ", ");
	  end;
	  emit-parameter-type(back-end, stream, type);
	  write(stream, " ");
	  if (instance?(type, <&raw-aggregate-type>))
	    write(stream, "tmp_");
	  end if;
	  format-emit*(back-end, stream, "%", parameters[parm-ix]);    
	  first? := #f;
	  parm-ix := parm-ix + 1;
	end;
  // need to get required parameter types out of signature
  if (sig)
    for (required in sig.^signature-required,
         i from 0 below sig.^signature-number-required)
      emit-parameter(if (accessor?) dylan-value(#"<object>") else required end);
    end;
  else
    for (i from 0 below parm-ix)
      emit-parameter(parameters[i].specializer);
    end;
  end if;
    
  // For other parameters we must not use signature
  for (i from parm-ix below size(parameters))
    emit-parameter(parameters[i].specializer);
  end;
  write-element(stream, ')');
end function;

// CALLED WHEN THERE AREN'T PARAMETERS I.E., DFM'S BEEN PURGED

define function emit-signature-types
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <&iep>, sig-spec :: <signature-spec>, sig :: <&signature>)
 => ()
  let accessor? = ^instance?(o.function, dylan-value(#"<accessor-method>"));
  write-element(stream, '(');
  let first? = #t;
  local method emit-parameter (param, type)
    if (first?) first? := #f; else write(stream, ", "); end if;
    let type = if (accessor?) dylan-value(#"<object>") else type end;
    emit-parameter-type(back-end, stream, type);
    // write-element(stream, ' ');
    // emit-object(back-end, param);
  end;
  let spec = #f;
  for (// spec in spec-argument-required-variable-specs(sig-spec),
       type in ^signature-required(sig),
       i from 0 below ^signature-number-required(sig))
    emit-parameter(spec, type)
  end for;
  when (^signature-optionals?(sig))
    emit-parameter(spec, dylan-value(#"<object>"));
  end when;
  for (spec in spec-argument-key-variable-specs(sig-spec))
    emit-parameter(spec, dylan-value(#"<object>"))
  end for;
  write-element(stream, ')');
end function;

// CALLED WHEN THERE ISN'T A SIGNATURE MODEL

define function emit-dynamic-signature-types
    (back-end :: <c-back-end>, stream :: <stream>, sig-spec :: <signature-spec>)
 => ()
  write-element(stream, '(');
  let first? = #t;
  local method emit-parameter (param)
    if (first?) first? := #f; else write(stream, ", "); end if;
    emit-parameter-type(back-end, stream, dylan-value(#"<object>"));
    // write-element(stream, ' ');
    // emit-object(back-end, param);
  end;
  for (spec in spec-argument-required-variable-specs(sig-spec))
    emit-parameter(spec)
  end for;
  when (spec-argument-optionals?(sig-spec))
    // HACK: FIX ME
    emit-parameter(spec-argument-rest-variable-spec(sig-spec) | "rest"); 
  end when;
  for (spec in spec-argument-key-variable-specs(sig-spec))
    emit-parameter(spec)
  end for;
  write-element(stream, ')');
end function;

define method external-lambda? (o :: <&method>) => (well?)
  o.model-definition
end method;

define method emit-lambda-interface 
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>) => ();
  // TODO: Turn on source locations.
  // emit-source-location(back-end, stream, o.body);
  emit-lambda-interface-using-function(back-end, stream, o, o.function);
end;

define method emit-lambda-interface-using-function
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>,
     fun :: <&callable-object>)
 => ()
  let fn = o.function;
  unless (external-lambda?(fun))
    write(stream, "static ");
  end;
  emit-return-types(back-end, stream, o);
  format-emit*(back-end, stream, " ^ ", o);
  let sig = ^function-signature(fn);
  if (parameters(fn))
    emit-parameters(back-end, stream, o, parameters(o), sig);
  elseif (sig)
    emit-signature-types(back-end, stream, o, signature-spec(fn), sig);
  else
    emit-dynamic-signature-types(back-end, stream, signature-spec(fn));
  end if;
end method;

define method emit-lambda-body
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>)
  emit-lambda-body-using-function(back-end, stream, o, o.function);
end;

define method capture-environment-string 
    (m :: <&method>) => (res :: <byte-string>)
  $capture-environment-string
end method;

define method capture-environment-string 
    (m :: <&keyword-closure-method>) => (res :: <byte-string>)
  $capture-keyword-environment-string
end method;

/// these 2 methods should share more....
define method emit-lambda-body-using-function
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>,
     fun)
  dynamic-bind (*current-environment* = o.environment)
    write(stream, "{\n");
    // with-current-css (get-default-call-site-summary(o.function))
      let volatile?
        = block (result)
            for-computations (c in o)
              if (instance?(c, <block>) & ~c.entry-state.local-entry-state?)
                result(#t);
              end if;
            end for-computations;
            #f
          end block; 
      for-temporary (tmp in o.environment)
	if (used?(tmp))
	  emit-local-definition(back-end, stream, tmp, volatile?);
	end if;
      end for-temporary;
      unless (empty?(o.environment.closure))
	if (closure-size(o.environment) > 0)
	  format-emit*(back-end, stream, "\t~\n", 
		       capture-environment-string(fun));
	end if;
	/*
	if (closure-self-referencing?(o.environment))
	  format-emit*(back-end, stream, "\t~ ~ = ~;\n", 
		       $dylan-type-string, $closure-string, 
		       $function-register-string);
	end if;
	*/
      end unless;
      write-element(stream, '\n');
      emit-computations(back-end, stream, 1, o.body, #f);
      write(stream, "}\n");
    // end with-current-css;
  end dynamic-bind;
end method emit-lambda-body-using-function;

define method emit-lambda
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&lambda>) => ()
end method;

define method emit-lambda
    (back-end :: <c-back-end>, stream :: <stream>, o :: <&iep>) => ()
/* This is longer needed; removed this for new retraction model

  for-lambda (sub-f in o)
    emit-lambda(back-end, stream, sub-f);
    write-element(stream, '\n');
  end;
*/
  emit-lambda-interface(back-end, stream, o);
  write-element(stream, ' ');
  with-labeling-from-dynamic
    emit-lambda-body(back-end, stream, o);
  end;
end method emit-lambda;

// !@#$ temporary

/* TODO: OBSOLETE?
define method debug-string (object)
  let debug-name = object.^debug-name;
  if (instance?(debug-name, <variable-name-fragment>))
    debug-name.fragment-identifier
  else
    as(<symbol>, debug-name)
  end
end method debug-string;
*/

define method emit-code (back-end :: <c-back-end>, o :: <&iep>)
  // format-out("--- %s ---\n", o.function);
  unless (code(o)) // DFM EXISTS?
    let stream = back-end.lambda-stream;
    clear-contents(stream);
    allocate-registers(o.function); 
    emit-lambda(back-end, stream, o);
    o.code := stream-contents-as(<byte-string>, stream);
    // format-out("%s\n", o.code);
    if (*retract-dfm?*)
      if (lambda-top-level?(o))
	retract-method-dfm(o);
	retract-method-dfm(o.function);
      end if;
    end if;
  end unless;
end method;

define method emit-init-code-label (stream, o :: <&lambda>)
  debug-assert(o.emitted-name, "Missing emitted-name for %s", o);
  format(stream, "I%d", o.emitted-name);
end method emit-init-code-label;

define thread variable *emitting-init-code?* = #f;

define method emit-init-code (back-end :: <c-back-end>, o :: <&iep>)
  // format-out("--- %s ---\n", o.function);
  unless (code(o)) // DFM EXISTS?
    let stream = back-end.lambda-stream;
    clear-contents(stream);
    allocate-registers(o.function); 
    dynamic-bind (*emitting-init-code?* = #t)
      emit-lambda-body(back-end, stream, o);
    end dynamic-bind;
    emit-init-code-label(stream, o.function);
    write(stream, ":\n");
    o.code := stream-contents-as(<byte-string>, stream);
  end unless;
end method emit-init-code;
