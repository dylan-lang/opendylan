Module: dfmc-c-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $safe-vector-ref = "VECTOR_REF_OR_F";
define constant $mv-get-element-string = "MV_GET_ELT";
define constant $mv-set-element-string = "MV_SET_ELT";
define constant $mv-get-rest-at-string = "MV_GET_REST_AT";
define constant $mv-set-rest-at-string = "MV_SET_REST_AT";
define constant $mv-set-count-string   = "MV_SET_COUNT";

define constant $spill-multiple-values-string    = "MV_SPILL";
define constant $unspill-multiple-values-string  = "MV_UNSPILL";

define constant $unused-arg-string = "P_unused_arg";

define constant $global-all-rest 
  = make(<multiple-value-temporary>,
         environment: $top-level-environment);
$global-all-rest.required-values := 0;
$global-all-rest.rest-values? := #t;

define method emit-parameter-type
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <type-estimate-raw>, #key index :: false-or(<integer>))
  emit-parameter-type(back-end, stream, as(<&type>, o))
end method;

define method emit-parameter-type
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <type-estimate-limited-instance>, #key index :: false-or(<integer>))
  emit-parameter-type
    (back-end, stream, ^object-class(type-estimate-singleton(o)));
end method;

define method emit-parameter-type
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <type-estimate-union>, #key index :: false-or(<integer>))
  emit-parameter-type
    (back-end, stream, first(type-estimate-unionees(o)), index: index);
end method;

define method emit-parameter-type
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <type-estimate-values>, #key index :: false-or(<integer>))
  let fixed-values = type-estimate-fixed-values(o);
  let itype
    = if (size(fixed-values) > 0)
        if (index & index < size(fixed-values))
          fixed-values[index]
        else
          fixed-values[0]
        end if
      else 
        dylan-value(#"<object>")
      end if;
  emit-parameter-type(back-end, stream, itype)
end method;

define method closure? (o)
  #f
end method;

define method closure? (o :: <&lambda-or-code>)
  ~lambda-top-level?(o) & (closure-size(o.environment) ~= 0)
end method;

/// TEMPORARY DEFINITIONS

define constant $loop-shadow-tmp-suffix = "T";

define method emit-local-tmp-definition 
    (back-end :: <c-back-end>, stream :: <stream>,
     tmp :: <temporary>, volatile? :: <boolean>) => ()
  format-emit*(back-end, stream, "\t");
  if (volatile?) format-emit*(back-end, stream, "volatile ") end;
  let type = type-estimate(tmp); // lookup-type(tmp, current-css(), tmp.generator);
  // A bit nasty, but the right thing to do (see the emit-computation
  // on <make-cell>, <get-cell-value>, <set-cell-value>)
  if (tmp.cell? & closed-over?(tmp))
    emit-parameter-type(back-end, stream, dylan-value(#"<object>"));
  else
    emit-parameter-type(back-end, stream, type);
  end;
  // if (tmp.cell?)
  //   format-emit*
  //     (back-end, stream, "* % = %(@);\n", tmp, tmp, #f);
  // else
    let gen = generator(tmp);
    if (instance?(gen, <loop-merge>) & loop-merge-initial?(gen))
      format-emit*(back-end, stream, " %~,", tmp, $loop-shadow-tmp-suffix); 
    end if;
    format-emit*(back-end, stream, " %;\n", tmp);
  // end if
end method;

define method emit-local-tmp-definition 
    (back-end :: <c-back-end>, stream :: <stream>, 
     tmp :: <multiple-value-temporary>, volatile? :: <boolean>) 
    => ()
  // define a var for each required element of local mv-temp.
  // note that there is no need for a variable for the mv-temp 
  // itself -- only for its elements.
  // let type = lookup-type(tmp, current-css(), tmp.generator); // ***** WRONG?
  let type = type-estimate(tmp);
  for (i from 0 below required-values(tmp))
    format-emit*(back-end, stream, "\t");
    if (volatile?) format-emit*(back-end, stream, "volatile ") end;
    emit-parameter-type(back-end, stream, type, index: i);
    format-emit*(back-end, stream, " %_~;\n", tmp, i);
  end for;
  if (required-values(tmp) = 0
      | tmp.rest-values?)
    format-emit*(back-end, stream, "\t");
    if (volatile?) format-emit*(back-end, stream, "volatile ") end;
    emit-parameter-type(back-end, stream, type);
    format-emit*(back-end, stream, " %;\n", tmp);
  end if;    
end method;

define method emit-local-definition 
    (back-end :: <c-back-end>, stream :: <stream>,
     tmp :: <temporary>, volatile? :: <boolean>) => ()
  emit-local-tmp-definition(back-end, stream, tmp, volatile?);
end method;

define method emit-local-definition 
    (back-end :: <c-back-end>, stream :: <stream>, 
     tmp :: <stack-vector-temporary>, volatile? :: <boolean>) => ()
  if (tmp.number-values = 0)
    format-emit* (back-end, stream, "\t");
    if (volatile?) format-emit*(back-end, stream, "volatile ") end;
    emit-parameter-type(back-end, stream, dylan-value(#"<object>"));
    format-emit*(back-end, stream, " % = @;\n", tmp, #[]);
  else
    format-emit*(back-end, stream, "\t");
    if (volatile?) format-emit*(back-end, stream, "volatile ") end;
    // TODO: integrate this with the real object dumper
    let class = &object-class(#[]);
    let wrapper = ^class-mm-wrapper(class);
    emit-repeated-struct-name(back-end, stream, class, tmp.number-values);
    format-emit*(back-end, stream, " % = {@, %};\n", 
		 tmp, wrapper, tmp.number-values);
  end if;
end method;

define method emit-local-definition 
    (back-end :: <c-back-end>, stream :: <stream>, 
     tmp :: <lexical-local-variable>, volatile? :: <boolean>) => ()
  emit-local-tmp-definition(back-end, stream, tmp, volatile?);
end method;

define method emit-local-definition
    (back-end :: <c-back-end>, stream :: <stream>,
     tmp :: <lexical-variable>, volatile? :: <boolean>) => ()
//   if (tmp.cell?)
//     format-emit*
//       (back-end, stream, "\t~* % = ~(tmp_%);\n", 
//        $dylan-type-string, tmp, $cell-string, tmp);
//   end if
end method;

define inline function closure-reference? (o) => (res :: <boolean>)
  o.environment ~== *current-environment*
end function;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, 
     o :: <stack-vector-temporary>) => ()
  if (closure-reference?(o))
    emit-closure-reference(back-end, stream, o);
  else 
    if (o.number-values > 0)
      format(stream, "&");
    end if;
    emit-object(back-end, stream, o);
  end if;
end method;

define method dump-closure (env :: <lexical-environment>, o) => ()
  format-out("FAILED TO FIND %= in ENV-%=[", o, env);
  let closure = env.closure;
  for (i from 0 below closure.size)
     format-out("%s ", closure[i]);
  end for;
  format-out("] of %=\n", o.environment.lambda);
end method;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <object-reference>) => ()
  let o = o.reference-value;
  if (load-bound-object?(o))
    emit-indirect-reference(back-end, stream, o);
  else
    emit-reference(back-end, stream, o);
  end;
end method;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <method-reference>) => ()
  let o = function(o.reference-value);
  emit-reference(back-end, stream, o);
end method;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <defined-constant-reference>) => ()
  emit-reference(back-end, stream, referenced-binding(o));
end method;

define function top-level-closure-reference? 
    (o, env :: <lambda-lexical-environment>) => (well? :: <boolean>)
  method-top-level?(env.lambda)
end function;

define function emit-closure-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <temporary>) => ()
  if (top-level-closure-reference?(o, *current-environment*))
    format-emit*(back-end, stream, "~", $method-reference-string);
  else
    let offset = closure-offset(*current-environment*, o);
    unless (offset)
      dump-closure(*current-environment*, o);      
      offset := -1;
    end unless;
    format-emit*
      (back-end, stream, "~(~)", $closure-reference-string, offset);
  end if
end function;

define method emit-reference
    (back-end :: <c-back-end>, stream :: <stream>, o :: <temporary>) => ()
  if (closure-reference?(o))
    emit-closure-reference(back-end, stream, o);
  else
    emit-object(back-end, stream, o);
  end if;
end method;

/// COMPUTATIONS

define method emit-label
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <computation>)
  maybe-label!(c);
  format-emit(b, s, d, "\tL~: ;\n", c.label);
end method;

define method emit-goto
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <computation>)
  maybe-label!(c);
  format-emit(b, s, d, "\tgoto L~;\n", c.label);
end method;

define method emit-computations
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <computation>, last)
  iterate loop (c = c)
    if (c & c ~== last)
      emit-source-location(b, s, d, c);
      emit-computation(b, s, d, c);
      loop(next-computation(c))
    end if;
  end iterate;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <computation>)
end method;

/// LOOP MERGE

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <loop-merge>)
  if (loop-merge-initial?(c))
    if (temporary(c))
      format-emit(b, s, d, "\t#@~;\n", 
                  temporary(c), temporary(c), $loop-shadow-tmp-suffix);
    end if;
  else
    format-emit(b, s, d, "\t#@;\n", temporary(c), loop-merge-parameter(c));
  end if;
end method;

/// EMIT

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <make-closure>)
  let o      = function(c.computation-closure-method);
  let sigtmp = c.computation-signature-value;
  let key?   = instance?(o, <&keyword-method>);
  if (closure?(o))
    let init?      = computation-init-closure?(c);
    let top-level? = computation-top-level-closure?(c);
    let env        = o.environment;
    if (sigtmp)
      if (top-level?)
        format-emit
          (b, s, d, "\t#~(@, @", 
           c.temporary, 
	   if (key?)
	     $set-keyword-method-signature-string
	   else
	     $set-method-signature-string
	   end if,
           o, sigtmp);
      else
	format-emit
	  (b, s, d, "\t#~(@, @, ~", 
	   c.temporary, 
	   if (init?) 
	     if (key?)
	       $make-keyword-closure-initd-with-signature-string
	     else
	       $make-closure-initd-with-signature-string
	     end if
	   else
	     if (key?)
	       $make-keyword-closure-with-signature-string
	     else
	       $make-closure-with-signature-string
	     end if
	   end if,
	   o, sigtmp, closure-size(env));
      end if
    else
      format-emit
        (b, s, d, "\t#~(@, ~", 
         c.temporary, 
         if (init?) 
	   if (key?)
	     $make-keyword-closure-initd-string 
	   else
	     $make-closure-initd-string 
	   end if
	 else 
	   if (key?)
	     $make-keyword-closure-string
	   else
	     $make-closure-string
	   end if
	 end,
         o, closure-size(env));
    end if;
    if (init? & ~top-level?)
      for (tmp in env.closure)
        // unless (tmp == c.temporary) // RECURSIVE TMP
  	  format-emit(b, s, d, ", @", tmp);
        // end unless
      end for;
    end if;
    write(s, ");\n");
  else
    if (sigtmp)
      format-emit
        (b, s, d, "\t#~(@, @);\n", 
         c.temporary, 
	 if (key?)
	   $make-keyword-method-with-signature-string
	 else
	   $make-method-with-signature-string
	 end if, 
	 o, sigtmp);
    else
      format-emit(b, s, d, "\t#@;\n", c.temporary, o);
    end if;
  end;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <initialize-closure>)
  let o = function(c.computation-closure-method);
  if (closure?(o))
    let env = o.environment;
    format-emit
      (b, s, d, "\t~(@, ~", 
       if (instance?(o, <&keyword-method>))
	 $initialize-keyword-closure-string
       else 
	 $initialize-closure-string
       end if,
       computation-closure(c), closure-size(env));
    for (tmp in env.closure)
      // unless (tmp == computation-closure(c)) // RECURSIVE TMP
	format-emit(b, s, d, ", @", tmp);
      // end unless
    end for;
    write(s, ");\n");
  end if;
end method;

define constant $primitive-read-thread-variable-string
  = c-raw-mangle("primitive-read-thread-variable");

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <variable-reference>) 
  if (c.referenced-binding.binding-thread?)
    format-emit(b, s, d, "\t#~(@);\n",
                c.temporary,
                $primitive-read-thread-variable-string,
                c.referenced-binding);
  else
    format-emit(b, s, d, "\t#@;\n", c.temporary, c.referenced-binding);
  end if;
end method;

// define method emit-computation
//     (b :: <c-back-end>, s :: <stream>, d :: <integer>,
//      c :: <temporary-transfer-computation>)
//   let target = c.temporary;
//   let source = c.computation-value;
//   unless (instance?(source, <temporary>) &
//           frame-offset(target) == frame-offset(source))
//     emit-transfer(b, s, d, target, source);
//   //was: format-emit(b, s, d, "\t#@;\n", target, source);
//   end unless;
// end method;

define method emit-rest-arguments
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, arguments, 
     #key first? = #t, max)
  for (argument in arguments)
    unless (first?) format-emit(b, s, d, ", "); end unless;
    format-emit(b, s, d, "@", argument);
    first? := #f;
  end;
  write-element(s, ')');
  if (max & arguments.size > max) 
    write-element(s, ')');
  end if;
  write-element(s, ';');
end method;

define method emit-call-prolog 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <function-call>, f)
end method;

define function emit-closure-call-prolog 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <function-call>)
  format-emit(b, s, d, "\t~ = @;\n", 
              $function-register-string, c.function);
end function;

define method emit-call-prolog 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <function-call>, f :: <&iep>)
  let env = f.environment;
  unless (~env | empty?(f.environment.closure))
    emit-closure-call-prolog(b, s, d, c);
  end unless;
end method;

define constant $mep-call-prolog-string = "MEP_CALL_PROLOG";

define method emit-call-prolog 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <method-call>, f)
  format-emit
    (b, s, d, "\t~(@, @, ~);\n", 
     $mep-call-prolog-string, c.function, c.next-methods, c.arguments.size);
end method;

define constant $miep-call-prolog-string = "MIEP_CALL_PROLOG";

define method emit-call-prolog 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <method-call>, 
     f :: <&iep>)
  if (^next?(function(f)))
    format-emit(b, s, d, "\t~(@);\n", $miep-call-prolog-string, c.next-methods);
  end if;
end method;


define constant $engine-node-call-prolog-string = "ENGINE_NODE_CALL_PROLOG";


define method emit-call-prolog
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <engine-node-call>, 
     f :: <&generic-function>)
  format-emit
    (b, s, d, "\t~(@, @, ~);\n", 
     $engine-node-call-prolog-string, c.function, c.engine-node, c.arguments.size);
end method;


define constant $congruent-call-prolog-string = "CONGRUENT_CALL_PROLOG";


define method emit-call-prolog
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <simple-call>, 
     f :: <&generic-function>)
  if (call-congruent?(c))
    format-emit
      (b, s, d, "\t~(@, ~);\n", 
       $congruent-call-prolog-string, c.function, c.arguments.size);
  else
    next-method()
  end if
end method;


define constant $call-string = "CALL";
define constant $max-call-templates = 10;
define constant $mep-call-string = "MEP_CALL";

define function emit-k-call-prefix
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     call-string :: <string>, number :: <integer>, function)
 => ()
  format-emit(b, s, d, "~~(@", call-string, number, function);
end function;

define function emit-n-call-prefix
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     call-string :: <string>, function)
 => ()
  format-emit(b, s, d, "~N(@", call-string, function);
end function;

define function emit-call-string
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     call-string :: <string>, max-templates :: <integer>, 
     number :: <integer>, function)
 => ()
  if (number <= max-templates)
    emit-k-call-prefix(b, s, d, call-string, number, function);
  else
    emit-n-call-prefix(b, s, d, call-string, function);
    format(s, ", %d)", number);
  end if;
end function;

define function emit-iep-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <simple-call>, f :: <&iep>) 
 => ()
  format-emit(b, s, d, "^(", f);
  emit-rest-arguments(b, s, d, c.arguments);
end function;

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <simple-call>, f :: <&iep>)
  emit-iep-call(b, s, d, c, f);
end method;

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <simple-call>, f)
  emit-call-string
    (b, s, d, $call-string, 
     $max-call-templates, c.arguments.size, c.function);
  emit-rest-arguments
    (b, s, d, c.arguments, first?: #f, max: $max-call-templates);
end method;

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <method-call>, f :: <&iep>)
    emit-iep-call(b, s, d, c, f);
end method;

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <method-call>, f)
  let number = c.arguments.size;
  let numbered? = number <= $max-call-templates;
  if (numbered?) 
    emit-k-call-prefix(b, s, d, $mep-call-string, number, c.function);
  else
    emit-n-call-prefix(b, s, d, $mep-call-string, c.function);
    write-element(s, ')');
  end if;
  emit-rest-arguments
    (b, s, d, c.arguments, first?: ~numbered?, max: $max-call-templates);
end method;



define constant $max-engine-node-call-templates = 7;
define constant $engine-node-call-string = "ENGINE_NODE_CALL";

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <engine-node-call>, f :: <&generic-function>)
  let number :: <integer> = c.arguments.size;
  let eng = c.engine-node;
  let numbered? = number <= $max-engine-node-call-templates;
  format-emit(b, s, d, if (numbered?) "~~(@" else "~N(~,@)" end,
	      $engine-node-call-string, number, eng);
  emit-rest-arguments(b, s, d, c.arguments, first?: #f, max: $max-engine-node-call-templates);
end method;

define constant $max-congruent-call-templates = 7;
define constant $congruent-call-string = "CONGRUENT_CALL";

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <simple-call>, f :: <&generic-function>)
  if (call-congruent?(c))
    let number :: <integer> = c.arguments.size;
    let numbered? = number <= $max-congruent-call-templates;
    format-emit(b, s, d, if (numbered?) "~~(" else "~N(~)" end,
		$congruent-call-string, number);
    emit-rest-arguments(b, s, d, c.arguments, first?: numbered?, max: $max-congruent-call-templates);
  else
    next-method()
  end if
end method;


define constant $apply-string = "APPLY";
define constant $max-apply-templates = 10;
define constant $mep-apply-string = "MEP_APPLY";
define constant $max-mep-apply-templates = $max-apply-templates;
define constant $engine-node-apply-string = "ENGINE_NODE_APPLY";
define constant $max-engine-node-apply-templates = 0;

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <apply>, f)
  emit-call-string
    (b, s, d, $apply-string, 
     $max-apply-templates, c.arguments.size, c.function);
  emit-rest-arguments
    (b, s, d, c.arguments, first?: #f, max: $max-apply-templates);
end method;

define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <engine-node-apply>, f :: <&generic-function>)
  let number :: <integer> = c.arguments.size;
  let eng :: <&engine-node> = c.engine-node;
  let numbered? = number <= $max-engine-node-apply-templates;
  format-emit(b, s, d, if (numbered?) "~~(@" else "~N(@,~" end,
	      $engine-node-apply-string, number, eng);
  emit-rest-arguments(b, s, d, c.arguments, first?: numbered?);
end method;


define method emit-call
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <method-apply>, f)
  let number = c.arguments.size;
  if (number <= $max-mep-apply-templates) 
    emit-k-call-prefix(b, s, d, $mep-apply-string, number, c.function);
    format-emit(b, s, d, ", @", c.next-methods);
  else
    emit-n-call-prefix(b, s, d, $mep-apply-string, c.function);
    format-emit(b, s, d, ", @, ~)", c.next-methods, number);
  end if;
  emit-rest-arguments
    (b, s, d, c.arguments, first?: #f, max: $max-mep-apply-templates);
end method;

define method emit-computation 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <function-call>)
  let effective-function = call-effective-function(c);
  emit-call-prolog(b, s, d, c, effective-function);
  format-emit(b, s, d, "\t#", c.temporary);
  emit-call(b, s, d, c, effective-function);
  format-emit(b, s, d, "\n");
  // transfer any required mv-temp values out of global MV area
  if (instance?(c.temporary, <multiple-value-temporary>))
    emit-transfer(b, s, d, c.temporary, $global-all-rest);
  end if;  
end method;

define method emit-primitive-call 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>,
     c :: <primitive-call>, f :: <&primitive>)
  let number-sig-vals = ^signature-number-values(primitive-signature(f));
  let sig-vals = ^signature-values(primitive-signature(f));

  if (number-sig-vals > 1)
    format-emit(b, s, d, "^_byref(", f);
  else
    format-emit(b, s, d, "^(", f);
  end if;

  for (argument in c.arguments, i from 1)
    unless (i = 1) write-element(s, ','); end unless;
    format-emit(b, s, d, "@", argument);
  end;

  // add to arguments ptr.s to non-first return values
  for (i from 1 below number-sig-vals)
    unless (size(c.arguments) = 0 & i = 1) 
      write-element(s, ','); 
    end unless;
    let this-ret-val = sig-vals[i];
    write-element(s, '(');
    emit-parameter-type(b, s, this-ret-val);
    write-element(s, '*');
    write-element(s, ')');
    let number-requested :: <integer>
      = if (c.temporary) required-values(c.temporary) else 0 end if;
    if (i < number-requested)  // is there a spot for it?
      format-emit(b, s, d, "@", 
                  make-address-of(mv-temp-ref(c.temporary, i)));
    else                       // if not, use canonical dummy address
      format-emit(b, s, d, "~", $unused-arg-string);
    end if;
  end for;
  format-emit(b, s, d, ");");
end method;

define method emit-call 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <primitive-call>, f :: <&primitive>)
  // f.emitter(b, s, d, c, f);
  emit-primitive-call(b, s, d, c, f);
end method;

define constant $guaranteed-initialized-slot-value-string
  = "SLOT_VALUE_INITD";
define constant $slot-value-string
  = "SLOT_VALUE";
define constant $slot-value-setter-string
  = "SLOT_VALUE_SETTER";

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <slot-value>)
  format-emit(b, s, d, "\t#~(@, ~);\n", 
              c.temporary, 
              if (computation-guaranteed-initialized?(c))
                $guaranteed-initialized-slot-value-string
              else
                $slot-value-string
              end,
              computation-instance(c), computation-slot-offset(c))
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <slot-value-setter>)
  format-emit(b, s, d, "\t#~(@, @, ~);\n", 
              c.temporary, $slot-value-setter-string,
              computation-new-value(c),
              computation-instance(c), computation-slot-offset(c))
end method;

define method emit-repeated-slot-value-string 
    (b :: <c-back-end>, s :: <stream>, c :: <any-repeated-slot-value>, setter?)
  write(s, "REPEATED_");
  let type = ^slot-type(computation-slot-descriptor(c));
  emit-parameter-type(b, s, repeated-representation(type));
  write(s, "_SLOT_VALUE");
  when (computation-index-tagged?(c))
    write(s, "_TAGGED");
  end when;
  when (setter?)
    write(s, "_SETTER");
  end when;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <repeated-slot-value>)
  format-emit(b, s, d, "\t#", c.temporary); 
  emit-repeated-slot-value-string(b, s, c, #f);
  format-emit(b, s, d, "(@, ~, @);\n", 
              computation-instance(c), computation-slot-offset(c),
              computation-index(c))
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <repeated-slot-value-setter>)
  format-emit(b, s, d, "\t#", c.temporary); 
  emit-repeated-slot-value-string(b, s, c, #t);
  format-emit(b, s, d, "(@, @, ~, @);\n", 
              computation-new-value(c),
              computation-instance(c), computation-slot-offset(c),
              computation-index(c))
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <stack-vector>)
  if (c.temporary & c.temporary.used?)
    let class = &object-class(#[]);
    let rpt = ^repeated-slot-descriptor(class);
    for (argument in c.arguments, i from 0)
      format-emit(b, s, d, "\t%.", c.temporary);
      emit-struct-field-name (b, s, class, rpt, i);
      format-emit(b, s, d, "[~] = @;\n", i, argument);
    end for;
  end if;
end method;

define method emit-computation 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <primitive-call>)
  format-emit(b, s, d, "\t#", mv-temp-lhs(c.temporary, 0));
  emit-call(b, s, d, c, c.primitive);
  format-emit(b, s, d, "\n");
  // transfer any required mv-temp values out of global MV area
  // NOTE: this goes away if/when we pass non-first returns by ref
  // if (instance?(c.temporary, <multiple-value-temporary>))
  //   emit-transfer(b, s, d, c.temporary, $global-all-rest);
  // end if;  
end method;

define function maybe-emit-merge-transfer
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     merge :: <computation>, refn :: <function>)
  if (instance?(merge, <merge>))
    let ref         = refn(merge);
    let merge-tmp   = temporary(merge);
    let merge-used? = merge-tmp & used?(merge-tmp);
    if (merge-used? & ref)
      emit-transfer(b, s, d, merge-tmp, ref);
    end if;
  end if;
end function;
  
define method emit-transfer
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     lhs, rhs)
  format-emit(b, s, d, "\t#@;\n", lhs, rhs);
end method;

define method emit-transfer
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     lhs :: <multiple-value-temporary>, rhs :: <multiple-value-temporary>)
//  if (generator(lhs))
//    format-emit(b,s,d, "\t/* ~ */\n", format-to-string("%=", 
//      computation-source-location(generator(lhs))));
//  end if;
  let min-required = min(lhs.required-values, rhs.required-values);
  let max-required = max(lhs.required-values, rhs.required-values);
  for (i from 0 below min-required)
    format-emit(b, s, d, "\t#@;\n", 
    mv-temp-lhs(lhs, i), mv-temp-ref(rhs, i));
  end for;
  case
    // precise (rhs) to imprecise (lhs)
    (~ rhs.rest-values?) & lhs.rest-values?
      => if (rhs.required-values < lhs.required-values)
           for (i from rhs.required-values below lhs.required-values)
             format-emit(b, s, d, "\t#@;\n", 
               mv-temp-lhs(lhs, i), #f);
           end for;
         elseif (lhs.required-values < rhs.required-values)
           for (i from lhs.required-values below rhs.required-values)
             if (i == 0)
               if (lhs ~== $global-all-rest)
                 format-emit(b, s, d, "\t#@;\n", mv-temp-lhs(lhs,i),
                   mv-temp-ref(rhs, i));
               end if;
             else
               format-emit(b, s, d, "\t~(~, @);\n", 
                 $mv-set-element-string, i, mv-temp-ref(rhs, i));
             end if;
           end for;
         end if;
	format-emit(b, s, d, "\t~(~);\n",
                    $mv-set-count-string, max-required);

    // imprecise (rhs) to imprecise (lhs)
    rhs.rest-values? & lhs.rest-values?
      => if (rhs.required-values < lhs.required-values)
           for (i from rhs.required-values below lhs.required-values)
             if (i == 0)
               if (rhs ~== $global-all-rest)
                 format-emit(b, s, d, "\t#@;\n", mv-temp-lhs(lhs, i),
                   mv-temp-ref(rhs, i));
               end if;
             else
               format-emit(b, s, d, "\t#~(~);\n", mv-temp-lhs(lhs, i),
                 $mv-get-element-string, i);
             end if;
           end for;
         elseif (lhs.required-values < rhs.required-values)
           for (i from lhs.required-values below rhs.required-values)
             if (i == 0)
               if (lhs ~== $global-all-rest)
                 format-emit(b, s, d, "\t#@;\n", mv-temp-lhs(lhs,i),
                   mv-temp-ref(rhs, i));
               end if;
             else
               format-emit(b, s, d, "\t~(~, @);\n", 
                 $mv-set-element-string, i, mv-temp-ref(rhs, i));
             end if;
           end for;
         end if;

    // imprecise (rhs) to precise (lhs)
    rhs.rest-values? & (~ lhs.rest-values?)
      => if (rhs.required-values < lhs.required-values)
           for (i from rhs.required-values below lhs.required-values)
             if (i == 0)
               if (rhs ~== $global-all-rest)
                 format-emit(b, s, d, "\t#@;\n", mv-temp-lhs(lhs, i),
                   mv-temp-ref(rhs, i));
               end if;
             else
               format-emit(b, s, d, "\t#~(~);\n", mv-temp-lhs(lhs, i),
                 $mv-get-element-string, i);
             end if;
           end for;
         end if;

    // precise (rhs) to precise (lhs)
    (~ rhs.rest-values?) & (~ lhs.rest-values?)
      => if (rhs.required-values < lhs.required-values)
           for (i from rhs.required-values below lhs.required-values)
             format-emit(b, s, d, "\t#@;\n", 
               mv-temp-lhs(lhs, i), #f);
           end for;
         end if;
     end case;

  if (required-values(lhs) == 0 & required-values(rhs) == 0
      & lhs ~== $global-all-rest & rhs ~== $global-all-rest)
    format-emit(b, s, d, "\t#@;\n", mv-temp-lhs(lhs, 0), mv-temp-ref(rhs, 0));
  end if;

end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <if>)
  let merge = next-computation(c);
  local method dead-branch? 
            (branch :: <computation>, ref :: false-or(<value-reference>))
	  branch == merge & ~ref
	end method;
  if (dead-branch?(c.consequent, merge-left-value(merge)))
    format-emit(b, s, d, "\tif (@ == @) {\n", c.test, #f);
    emit-computations(b, s, d + 1, c.alternative, merge);
    maybe-emit-merge-transfer(b, s, d + 1, merge, merge-left-value);
  elseif (dead-branch?(c.alternative, merge-right-value(merge)))
    format-emit(b, s, d, "\tif (@ != @) {\n", c.test, #f);
    emit-computations(b, s, d + 1, c.consequent, merge);
    maybe-emit-merge-transfer(b, s, d + 1, merge, merge-right-value);
  else
    format-emit(b, s, d, "\tif (@ != @) {\n", c.test, #f);
    emit-computations(b, s, d + 1, c.consequent, merge);
    maybe-emit-merge-transfer(b, s, d + 1, merge, merge-left-value);
    format-emit(b, s, d, "\t} else {\n");
    emit-computations(b, s, d + 1, c.alternative, merge);
    maybe-emit-merge-transfer(b, s, d + 1, merge, merge-right-value);
  end if;
  format-emit(b, s, d, "\t}\n");
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <return>)
  if (*emitting-init-code?*)
    format-emit(b, s, d, "\tgoto ");
    emit-init-code-label(s, *current-environment*.lambda);
    write(s, ";\n");
  else
    if (instance?(c.computation-value, <multiple-value-temporary>))
      // spill local required values into global MV area
      emit-transfer(b, s, d, $global-all-rest, c.computation-value);
      format-emit(b, s, d, "\treturn(@);\n", 
		  mv-temp-ref(c.computation-value, 0));
    else
      format-emit(b, s, d, "\treturn(@);\n", c.computation-value);
    end if;
  end if;
end method;

define constant $primitive-write-thread-variable-string
  = c-raw-mangle("primitive-write-thread-variable");

define method emit-computation 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <assignment>)
  // TODO: split out <definition>, tie that to the actual definiton
  if (c.assigned-binding.binding-thread?)
    format-emit(b, s, d, "\t#~(@, @);\n",
                c.temporary,
                $primitive-write-thread-variable-string,
                c.assigned-binding, c.computation-value);
  else
    format-emit(b, s, d, "\t#@ = @;\n", 
                c.temporary, c.assigned-binding, c.computation-value);
  end if;
end method;

define constant $primitive-allocate-thread-variable-string
  = c-raw-mangle("primitive-allocate-thread-variable");

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <definition>)
  if (c.assigned-binding.binding-thread?)
    format-emit(b, s, d, "\t#@ = ~(@);\n",
                c.temporary,
                c.assigned-binding,
                $primitive-allocate-thread-variable-string,
                c.computation-value);
  else
    next-method();
  end if;
end method;

define method emit-computation 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
       c :: <conditional-update!>)
  format-emit(b, s, d, "\t#CONDITIONAL_UPDATE(@, @, @);\n", 
              c.temporary, c.assigned-binding, 
              c.computation-value, c.computation-test-value);
end method;

// multiple values

// There's a slightly distasteful hack in here, in that a multiple
// value temporary names both all the multiple values, but can be
// referred to as a normal temporary as part of code generation to
// just refer to the first value.

define method emit-reference
    (b :: <c-back-end>, s :: <stream>, 
     o :: <multiple-value-temporary-reference>) => ()
  if ((required-values(o.ref-temp) = 0
       & o.ref-index > 0)
      | (required-values(o.ref-temp) > 0
         & o.ref-index >= required-values(o.ref-temp)))
//    format-out("XXXXX uh oh, being asked for index %d of temp %=!\n", 
//	       o.ref-index, o.ref-temp);
    if (o.ref-index > 0) 
      // let type = lookup-type(o.ref-temp, current-css(), o.ref-temp.generator);
      let type = type-estimate(o.ref-temp);
      if (instance?(type, <type-estimate-raw>))
        write-element(s, '(');
        emit-parameter-type(b, s, type);
        write-element(s, ')');
      end if;
    end if;
    if (lhs?(o))
      format-out("Trying to set a too-large index!(%=, %d)\n",
        o.ref-temp, o.ref-index);
      emit-object(b, s, o.ref-temp);
    else
      format-emit(b, s, 0, "~(~);\n", $mv-get-element-string, o.ref-index);
    end if;
  elseif (required-values(o.ref-temp) = 0)
    emit-object(b, s, o.ref-temp);
  else
    emit-object(b, s, o.ref-temp);
    format-emit(b, s, 0, "_~", o.ref-index);
  end if;
end method;

define method emit-reference
    (b :: <c-back-end>, s :: <stream>, 
     o :: <multiple-value-temporary>) => ()
 emit-reference(b, s, mv-temp-ref(o, 0));
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <extract-single-value>)
  format-emit(b, s, d, "\t#", c.temporary);
  // emit-reference will do mv-get if index too large
  format-emit(b, s, d, "@;\n", mv-temp-ref(c.computation-value, c.index));
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <extract-rest-value>)
  format-emit(b, s, d, "\t#~(@, ~);\n",
              c.temporary, $mv-get-rest-at-string, 
              mv-temp-ref(c.computation-value, 0), c.index);
end method;

// lhs is c.temporary + rest-values?(c.temporary)
// rhs is c.fixed + c.rest-value
//   - 0th element is handled specially - it never goes into MV area
define method emit-computation 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <values>)
  let lhs-temp = c.temporary;
  when (used?(lhs-temp)) // HACK: THIS SHOULDN'T BE NECESSARY
  let lhs-num-required = required-values(lhs-temp);
  let lhs-wants-rest? = rest-values?(c.temporary);

  let rhs-supplied-values = c.fixed-values;
  let rhs-num-supplied = size(rhs-supplied-values);
  let rhs-has-rest? = c.rest-value ~= #f;
  let rhs-rest-temp = c.rest-value;
  
  iterate loop (i = 0)  // i indexes into both lhs and rhs
    if (i < lhs-num-required)       // need another
      if (i < rhs-num-supplied)     // it's supplied
        format-emit(b, s, d, "\t@ = @;\n", 
                    mv-temp-lhs(lhs-temp, i), rhs-supplied-values[i]);
      elseif (rhs-has-rest?)  // not supplied -- try #rest part of <values>
        format-emit(b, s, d, "\t@ = ~(@,~);\n", 
                    mv-temp-lhs(c.temporary, i),
		    $safe-vector-ref, c.rest-value, 
                    i - rhs-num-supplied);
      else   // no #rest from <values> - must be #f
        format-emit(b, s, d, "\t@ = @;\n", 
                    mv-temp-lhs(c.temporary, i), #f);
      end if;
      loop (i + 1);
    elseif (i < rhs-num-supplied)      // <values> has more to give
      if (i = 0) // special case for 0th element -- assign to temp anyway
        format-emit(b,s,d,"\t@ = @;\n",mv-temp-lhs(lhs-temp,i),
		    rhs-supplied-values[i]) ;
      end if;
      if (lhs-wants-rest?)   // is there somewhere to put them?
        // put i^th fixed value into MV area
        format-emit(b, s, d, "\t~(~, @);\n", 
                   $mv-set-element-string, 
                   i, rhs-supplied-values[i]);
        loop(i + 1);  
      end if;           // if lhs doesn't want any more, we're done.
      // don't loop if no #rest on lhs
    // lhs has all it wants, rhs has given all it can, 
    elseif (lhs-wants-rest? & rhs-has-rest?)  // handle #rest to #rest case.
      if (i = 0)  // special case for 0 again
	format-emit(b, s, d, "\t#", lhs-temp);   // temp = 
      else
	format-emit(b,s,d, "\t");
      end if;
// want to index into rhs by how much has been extracted already, 
// but can't (1st param must be a vector, with a size slot).
// so populate MV area from actual start of rhs #rest vector temp. 
      format-emit(b, s, d, "~(@, ~);\n",
		  $mv-set-rest-at-string, 
		  rhs-rest-temp, rhs-num-supplied);
    elseif (i = 0)  // no #rest to #rest -- check for 0th case one more time
      format-emit(b, s, d, "\t#@;\n", c.temporary, #f);
    end if;
  end;  // iterate loop

  // if we are in an imprecise context, 
  // must set the count
  if (lhs-wants-rest?)
    if (~ rhs-has-rest?) // & if didn't set it with rest-to-rest
      format-emit(b, s, d, "\t~(~);\n",
		  $mv-set-count-string, rhs-num-supplied);
    end if;
  end if;
  end when;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <temporary-transfer-computation>)
  emit-transfer(b, s, d, c.temporary, c.computation-value);
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <multiple-value-spill>)
  let comp = c.computation-value;
  if (instance?(comp, <multiple-value-temporary>) & comp.rest-values?)
    format-emit(b, s, d, "\t#~(@);\n", c.temporary, 
		$spill-multiple-values-string, c.computation-value);
  end if;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <multiple-value-unspill>) 
  let temp = c.temporary;
  let comp = c.computation-value;
  let previous-comp =
    if (instance?(comp.generator, <multiple-value-spill>))
      comp.generator.computation-value;
    else
      error("emit-computation <multiple-value-spill>");
    end if;
  if (instance?(previous-comp, <multiple-value-temporary>) & previous-comp.rest-values?)
    format-emit(b, s, d, "\t#~(@);\n",
		temp, $unspill-multiple-values-string, comp);
  end if;
  emit-transfer(b, s, d, temp, previous-comp);
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <adjust-multiple-values>) 
//  format-emit(b, s, d, "\t~(~);\n",
//              $adjust-multiple-values-string,
//              c.number-of-required-values);
  next-method();  // do transfer
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <adjust-multiple-values-rest>) 
//  format-emit(b, s, d, "\t~(~);\n",
//              $adjust-multiple-values-rest-string,
//              c.number-of-required-values);
  next-method();  // do transfer
end method;

// loop

// define constant $loop-string = "while(1)";

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <loop>)
  for (merge in loop-merges(c))
    let tmp = temporary(merge);
    if (tmp & used?(tmp))
      format-emit(b, s, d, "\t@~ = @;\n", 
                  tmp, $loop-shadow-tmp-suffix, 
                  loop-merge-parameter(merge));
    end if;
  end for;
  emit-label(b, s, d - 1, c);  
  // format-emit(b, s, d, "\t~ {\n", $loop-string);
  emit-computations(b, s, d + 1, loop-body(c), c.next-computation);
  // format-emit(b, s, d, "\t}\n");
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <loop-call>)
  let loop = loop-call-loop(c);
  for (initial-merge in loop-merges(loop),
       call-merge    in loop-call-merges(c))
    let tmp = temporary(initial-merge);
    if (tmp & used?(tmp))
      format-emit(b, s, d, "\t@~ = @;\n", 
                  tmp, $loop-shadow-tmp-suffix, 
                  loop-merge-argument(call-merge));
    end if;
  end for;
  emit-goto(b, s, d, c.loop-call-loop);
  // format-emit(b, s, d, "\tcontinue;\n");
end method;
 
define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <end-loop>)
  // format-emit(b, s, d, "\tbreak;\n");
end method;
 
// non-local control flow

define constant $make-bind-exit-frame-string = "MAKE_EXIT_FRAME";
define constant $frame-destination-string    = "FRAME_DEST";
define constant $frame-return-value-string   = "FRAME_RETVAL";
define constant $exit-string                 = "NLX";

// define constant merge-exit-value = merge-left-value;
define constant merge-body-value = merge-right-value;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <bind-exit>)
  let merge = c.next-computation;
  if (c.entry-state.local-entry-state?)
    emit-computations(b, s, d, c.body, c.next-computation);
    maybe-emit-merge-transfer(b, s, d, merge, merge-body-value);
    emit-label(b, s, d, c);
  else
    let merge-tmp = temporary(merge);
    format-emit
      (b, s, d, "\t% = ~();\n", c.entry-state, $make-bind-exit-frame-string);
    format-emit
      (b, s, d, "\tif (nlx_setjmp(~(%))) {\n",
       $frame-destination-string, c.entry-state);
    if (used?(c.temporary))
      format-emit
	(b, s, d, "\t\t@ = ~(@);\n", 
	 merge-tmp, $frame-return-value-string, c.entry-state);
      // note that blocks are converted in all-rest context
      if (required-values(merge-tmp) > 0)
	emit-transfer(b, s, d + 1, merge-tmp, $global-all-rest);
      end if;
    end if;
    format-emit(b, s, d, "\t} else {\n");
    emit-computations(b, s, d + 1, c.body, c.next-computation);
    maybe-emit-merge-transfer(b, s, d, merge, merge-body-value);
    format-emit(b, s, d + 1, "\t/* invalidate @ */\n", c.entry-state);
    format-emit(b, s, d, "\t}\n");
  end if;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <exit>)
  if (c.entry-state.local-entry-state?)
    let me-block :: <bind-exit> = c.entry-state.me-block;
    let nc = me-block.next-computation;
    let temp = if (instance?(nc, <bind-exit-merge>)) temporary(nc) end;
    // was: format-emit(b, s, d, "\t#@;\n", temp, c.computation-value);
    emit-transfer(b, s, d, temp, c.computation-value);  // new
    emit-goto(b, s, d, me-block);
  else
    // spill to MV area before doing non-local exit (which saves
    // the MV area values)
    if (instance?(c.computation-value, <multiple-value-temporary>)
        & required-values(c.computation-value) > 1)
      emit-transfer(b, s, d, $global-all-rest, c.computation-value);
    end if;
    format-emit
      (b, s, d, "\t#~(@, @);\n", temporary(c),
       $exit-string, c.entry-state, c.computation-value);
  end if;
end method;

define constant $make-unwind-protect-frame-string = "MAKE_UNWIND_FRAME";
define constant $continue-unwind-string           = "CONTINUE_UNWIND";
define constant $fall-through-unwind-string       = "FALL_THROUGH_UNWIND";

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <unwind-protect>)
  format-emit
    (b, s, d, "\t% = ~();\n", 
     c.entry-state, $make-unwind-protect-frame-string);
  format-emit
    (b, s, d, "\tif (!nlx_setjmp(~(@))) {\n",
     $frame-destination-string, c.entry-state);
  let end-protected 
    = emit-computations(b, s, d + 1, c.body, c.next-computation);
  format-emit(b, s, d + 1, "\t~(@);\n", 
	      $fall-through-unwind-string, c.protected-temporary);
  format-emit(b, s, d, "\t}\n");
  emit-computations(b, s, d, c.cleanups, c.next-computation);
  format-emit(b, s, d, "\t~();\n", $continue-unwind-string);
end method;

define method emit-computation
     (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <nop-computation>)
end method;

// types

define constant $primitive-type-check-string
  = c-raw-mangle("primitive-type-check");

define method emit-check-type?
    (back-end :: <c-back-end>, c :: <check-type>)
 => (well? :: <boolean>)
  // Don't emit type checks for the Dylan library
  ~compiling-dylan-library?()
end method;

define method emit-check-type?
    (back-end :: <c-back-end>, c :: <keyword-check-type>)
 => (well? :: <boolean>)
  // We must emit these cause they check for things like size :: <integer>
  // for vector()
  #t
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <check-type>)
  if (emit-check-type?(b, c))
    format-emit(b, s, d, "\t~(@, @);\n", 
                $primitive-type-check-string,
                c.computation-value, c.type);
  end if;
  next-method();
end method;

define constant $multiple-value-check-type-prologue-string
  = "MV_CHECK_TYPE_PROLOGUE";
define constant $multiple-value-check-type-epilogue-string
  = "MV_CHECK_TYPE_EPILOGUE";

define constant $multiple-value-check-type-rest-string 
  = "MV_CHECK_TYPE_REST";

define function canonicalize-check-type (type)
  type | dylan-value(#"<object>")
end function;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <multiple-value-check-type>)
  if (compiling-dylan-library?())
    next-method();
  else
    if (used?(c.temporary))
      for (i from 0 below required-values(temporary(c)))
	emit-transfer(b, s, d, 
		      mv-temp-lhs(c.temporary, i),
		      mv-temp-ref(c.computation-value, i));
      end;
      format-emit(b, s, d, "\t{\n");
      format-emit(b, s, d + 1, "\t~(@);\n",
		  $multiple-value-check-type-prologue-string,
		  mv-temp-ref(computation-value(c), 0));
      for (i from 0 below required-values(temporary(c)))
	format-emit(b, s, d + 1, "\t~(@, @);\n", 
		    $primitive-type-check-string,
		    mv-temp-lhs(c.temporary, i),
		    canonicalize-check-type(types(c)[i]));
      end;
      format-emit(b, s, d + 1, "\t~();\n", 
		  $multiple-value-check-type-epilogue-string);
      format-emit(b, s, d, "\t}\n");
    end if;
  end if;
end method;

define method emit-computation 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
     c :: <multiple-value-check-type-rest>)
  if (compiling-dylan-library?())
    next-method();
  else
    format-emit(b, s, d, "\t#~(@, @, ~", 
		temporary(c), 
		$multiple-value-check-type-rest-string, 
                mv-temp-ref(computation-value(c), 0),
		canonicalize-check-type(rest-type(c)),
		size(types(c)));
    for (type in types(c))
      format-emit(b, s, d, ", @", canonicalize-check-type(type));
    end for;
    write(s, ");\n");
  end if;
end method;

// cell for assignment

define method emit-make-cell-string 
    (b :: <c-back-end>, s :: <stream>, type)
  write(s, "MAKE_");
  emit-parameter-type(b, s, type);
  write(s, "_CELL");
end method;

define method emit-set-cell-value-string 
    (b :: <c-back-end>, s :: <stream>, type)
  write(s, "SET_");
  emit-parameter-type(b, s, type);
  write(s, "_CELL_VAL");
end method;

define method emit-get-cell-value-string 
    (b :: <c-back-end>, s :: <stream>, type)
  write(s, "GET_");
  emit-parameter-type(b, s, type);
  write(s, "_CELL_VAL");
end method;

// TODO: MISPLACED

define method cell-representation-type (t :: <cell>) => (type)
  cell-representation(cell-type(t))
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <make-cell>)
  if (closed-over?(c.temporary))  
    format-emit(b, s, d, "\t#", c.temporary);
    emit-make-cell-string(b, s, cell-representation-type(c.temporary));
    format-emit(b, s, d, "(@);\n", c.computation-value);
  else
    format-emit(b, s, d, "\t#@;\n", c.temporary, c.computation-value);
  end if;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <get-cell-value>)
  if (closed-over?(c.computation-cell))  
    format-emit(b, s, d, "\t#", c.temporary);
    emit-get-cell-value-string(b, s, cell-representation-type(c.computation-cell));
    format-emit(b, s, d, "(@);\n", c.computation-cell);
  else
    format-emit(b, s, d, "\t#@;\n", c.temporary, c.computation-cell);
  end if;
end method;

define method emit-computation
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <set-cell-value!>)
  if (closed-over?(c.computation-cell))  
    format-emit(b, s, d, "\t#", c.temporary); 
    emit-set-cell-value-string(b, s, cell-representation-type(c.computation-cell));
    format-emit(b, s, d, "(@, @);\n", c.computation-cell, c.computation-value);
  else
    format-emit(b, s, d, "\t#@ = @;\n", 
                c.temporary, c.computation-cell, c.computation-value);
  end if;
end method;

// SOURCE LOCATIONS

define method emit-source-location
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c :: <computation>)
 => ()
  let loc = dfm-source-location(c);
  if (instance?(loc, <source-location>))
    let record = source-location-source-record(loc);
    let start-offset = source-location-start-offset(loc);
    let start-line = source-offset-line(start-offset);
    let (file-name, adjusted-line) = source-line-location(record, start-line);
    format-emit(b, s, d, "\t// ");
    format(s, "%s:%d\n",
           source-record-location(record),
           start-line + source-record-start-line(record));
  end if;
/* TODO: Plug back in one day when we can make the C compilers understand.
  if (loc & instance?(loc, <source-location>))
    let record = source-location-source-record(loc);
    let start-offset = source-location-start-offset(loc);
    let start-line = source-offset-line(start-offset);
    let (file-name, adjusted-line) = source-line-location(record, start-line);
    // format-out("#line %d \"%s\"\n", adjusted-line, file-name);
    format(s, "#line %d \"%s\"\n", adjusted-line, file-name);
  end;
*/
end method;

// Computation can be #f sometimes for empty bodies.

define method emit-source-location 
    (b :: <c-back-end>, s :: <stream>, d :: <integer>, c) => ()
end method;
