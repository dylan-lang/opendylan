Module:   dfmc-definitions
Synopsis: Signature specs.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Variable, argument, and value specs.

define abstract dood-class <variable-spec> (<dood-dfmc-object>)
  lazy constant slot spec-variable-name,
    required-init-keyword: variable-name:;
end dood-class;

define sealed domain make       (subclass(<variable-spec>));
define sealed domain initialize (<variable-spec>);

define function dylan-object-expression ()
  %library-description-object-expression(dylan-library-description())
    | (%library-description-object-expression(dylan-library-description())
	 := dylan-variable-name(#"<object>"));
end function;

define method spec-type-expression 
    (spec :: <variable-spec>) => (res)
  dylan-object-expression()
end method;

define abstract dood-class <type-expression-spec> (<dood-dfmc-object>)
  lazy constant slot spec-type-expression = #f,
    init-keyword: type-expression:;
end dood-class;

define method spec-variable-typed? (var :: <type-expression-spec>) => (well? :: <boolean>)
  spec-type-expression(var) ~== #f 
    & spec-type-expression ~== dylan-object-expression()
end method;

define method spec-variable-typed? (var :: <variable-spec>) => (well? :: <boolean>)
  #f
end method;

define class <required-variable-spec> (<variable-spec>) end;
define class <typed-required-variable-spec>
    (<type-expression-spec>, <required-variable-spec>) 
end class;

define method make
    (class == <required-variable-spec>, #rest all-keys, 
     #key variable-name, type-expression)
 => (res :: <required-variable-spec>)
  if (type-expression)
    apply(make, <typed-required-variable-spec>, all-keys)
  else
    next-method()
  end if
end method;


define class <next-variable-spec>     (<variable-spec>) end;
define class <typed-next-variable-spec>
    (<type-expression-spec>, <next-variable-spec>) 
end class;

define class <rest-variable-spec>     (<variable-spec>) end;
define class <typed-rest-variable-spec>     
    (<type-expression-spec>, <rest-variable-spec>)
end class;

define class <key-variable-spec> (<variable-spec>) 
end class;

define method spec-keyword-expression 
    (spec :: <key-variable-spec>) => (res)
  // TODO: less consing
  let name = spec-variable-name(spec);
  make-literal-fragment(fragment-name(name))
end method;

define method spec-default-expression 
    (spec :: <key-variable-spec>) => (res)
  %library-description-false-expression(dylan-library-description())
    | (%library-description-false-expression(dylan-library-description())
	 := make(<false-fragment>, record: #f, source-position: #f));
end method;

define dood-class <defaulted-key-variable-spec> (<key-variable-spec>)
  lazy constant slot spec-default-expression,
    required-init-keyword: default-expression:;
end dood-class;

define dood-class <complex-key-variable-spec> 
    (<type-expression-spec>, <defaulted-key-variable-spec>)
  lazy constant slot spec-keyword-expression,
    required-init-keyword: keyword-expression:;
end dood-class;

define method make
    (class == <key-variable-spec>, #rest all-keys, 
     #key variable-name, 
          keyword-expression, type-expression, default-expression)
 => (res :: <key-variable-spec>)
  local method non-object-type-expression? (type-expression)
	  type-expression 
	    & macro-case (type-expression)
  	        { <object> } => #f;
  	        { ?other:* } => #t;
	      end macro-case;
        end method,
        method non-false-default-expression? (default-expression)
	  default-expression
	    & macro-case (default-expression)
  	        { #f }       => #f;
  	        { ?other:* } => #t;
	      end macro-case;
	end method;
  if (non-object-type-expression?(type-expression) | keyword-expression)
    if (keyword-expression)
      apply(make, <complex-key-variable-spec>, all-keys)
    else
      apply(make, <complex-key-variable-spec>, 
            keyword-expression:
              make-literal-fragment(fragment-name(variable-name)),
            all-keys)
    end if
  elseif (non-false-default-expression?(default-expression))
    make(<defaulted-key-variable-spec>,
         variable-name:      variable-name,
         default-expression: default-expression)
  else
    next-method(<key-variable-spec>, variable-name: variable-name);
  end if
end method;

//// SIGNATURE SPECS FOR VARIOUS KINDS OF FUNCTION.

define constant <variable-specs> = <simple-object-vector>;
// define constant variable-specs = vector;
// define constant $empty-variable-specs = #[];

define abstract dood-class <signature-spec> (<dood-dfmc-object>)
end dood-class;

define method make
    (class == <signature-spec>, #rest all-keys, 
     #key argument-rest-variable-spec, argument-key?, 
          value-required-variable-specs, value-rest-variable-spec)
 => (res :: <signature-spec>)
  // format-out("ALL-KEYS %=\n", all-keys);
  if (argument-key?) // complex args?
    if (value-rest-variable-spec) // complex vals?
      apply(make, <complex-signature+complex-values-spec>, all-keys)
    elseif (value-required-variable-specs)
      apply(make, <complex-signature+values-spec>, all-keys)
    else 
      apply(make, <complex-signature-spec>, all-keys)
    end if
  elseif (argument-rest-variable-spec) // rested args?
    if (value-rest-variable-spec) // complex vals?
      apply(make, <rested-signature+complex-values-spec>, all-keys)
    elseif (value-required-variable-specs)
      apply(make, <rested-signature+values-spec>, all-keys)
    else 
      apply(make, <rested-signature-spec>, all-keys)
    end if
  else // simple args
    if (value-rest-variable-spec) // complex vals?
      apply(make, <required-signature+complex-values-spec>, all-keys)
    elseif (value-required-variable-specs)
      apply(make, <required-signature+values-spec>, all-keys)
    else 
      apply(make, <required-signature-spec>, all-keys)
    end if
  end if;
end method;

define sealed domain make       (subclass(<signature-spec>));
define sealed domain initialize (<signature-spec>);

define method spec-argument-required-variable-specs (spec :: <signature-spec>)
  #[]
end method;

define method spec-argument-rest-variable-spec (spec :: <signature-spec>)
  #f
end method;

define method spec-argument-next-variable-spec (spec :: <signature-spec>)
  #f
end method;

define method spec-argument-key-variable-specs (spec :: <signature-spec>)
  #[]
end method;

define method spec-argument-key? (spec :: <signature-spec>)
  #f
end method;

define inline function spec-argument-number-required (spec :: <signature-spec>)
 => (number :: <integer>)
  size(spec-argument-required-variable-specs(spec))
end;

define method spec-value-required-variable-specs (spec :: <signature-spec>)
  #[]
end method;

define inline function spec-value-number-required (spec :: <signature-spec>)
 => (number :: <integer>)
  size(spec-value-required-variable-specs(spec))
end;

define function spec-default-value-rest-variable-spec () => (spec :: <rest-variable-spec>)
  %library-description-default-value-rest-spec(dylan-library-description())
    | (%library-description-default-value-rest-spec(dylan-library-description())
	 := make(<rest-variable-spec>,
		 variable-name: dylan-variable-name(#"results")));
end function;

define method spec-value-rest-variable-spec (spec :: <signature-spec>)
  spec-default-value-rest-variable-spec()
end method;

define inline function spec-value-rest? (spec :: <signature-spec>) => (rest? :: <boolean>)
  as-boolean(spec-value-rest-variable-spec(spec))
end;


/// VALUES-SPEC

define class <values-spec> (<signature-spec>)
end class;

define method make
    (class == <values-spec>, #rest all-keys, 
     #key value-required-variable-specs, value-rest-variable-spec)
 => (res :: <values-spec>)
  // format-out("ALL-KEYS %=\n", all-keys);
  if (value-rest-variable-spec) 
    apply(make, <complex-values-spec>, all-keys)
  elseif (value-required-variable-specs)
    apply(make, <required-values-spec>, all-keys)
  else 
    next-method()
  end if
end method;


define dood-class <required-values-spec> (<values-spec>)
  lazy constant slot spec-value-required-variable-specs :: <variable-specs>,
    required-init-keyword: value-required-variable-specs:;
end dood-class;

define method spec-value-rest-variable-spec (spec :: <required-values-spec>)
  #f
end method;

define dood-class <complex-values-spec> (<required-values-spec>)
  lazy constant slot spec-value-rest-variable-spec :: false-or(<rest-variable-spec>),
    required-init-keyword: value-rest-variable-spec:;
end dood-class;


define primary dood-class <required-signature-spec> (<signature-spec>)
  lazy constant slot spec-argument-required-variable-specs :: <variable-specs>,
    required-init-keyword: argument-required-variable-specs:;
end dood-class;

define class <required-signature+values-spec>
    (<required-values-spec>, <required-signature-spec>)
end class;

define class <required-signature+complex-values-spec> 
    (<complex-values-spec>, <required-signature+values-spec>)
end class;


/// COMPLEX-SIGNATURE-SPEC

define primary dood-class <rested-signature-spec> (<required-signature-spec>)
  lazy constant slot spec-argument-rest-variable-spec :: false-or(<rest-variable-spec>),
    required-init-keyword: argument-rest-variable-spec:;
end dood-class;

define class <rested-signature+values-spec> 
    (<required-values-spec>, <rested-signature-spec>)
end class;

define class <rested-signature+complex-values-spec>
    (<complex-values-spec>, <rested-signature+values-spec>)
end class;


define primary dood-class <complex-signature-spec> (<rested-signature-spec>)
  lazy constant slot spec-argument-key-variable-specs :: <variable-specs>,
    required-init-keyword: argument-key-variable-specs:;
  lazy constant slot spec-argument-key? :: type-union(<boolean>, singleton(#"all")),
    required-init-keyword: argument-key?:;
end dood-class;

define inline function spec-argument-all-keys? (spec :: <signature-spec>)
  spec-argument-key?(spec) == #"all"
end function;

define inline function spec-argument-number-keys (spec :: <signature-spec>)
 => (number :: <integer>)
  size(spec-argument-key-variable-specs(spec))
end;

define inline function spec-argument-rest? (spec :: <signature-spec>) => (rest? :: <boolean>)
  as-boolean(spec-argument-rest-variable-spec(spec))
end;

define inline function spec-argument-optionals? (spec :: <signature-spec>)
 => (optionals? :: <boolean>)
  as-boolean(spec-argument-key?(spec) | spec-argument-rest?(spec))
end;

define class <complex-signature+values-spec> 
    (<required-values-spec>, <complex-signature-spec>)
end class;

define class <complex-signature+complex-values-spec>
    (<complex-values-spec>, <complex-signature+values-spec>)
end class;


/// METHOD SIGNATURES

define abstract dood-class <method-signature-spec> (<signature-spec>) 
  lazy slot spec-argument-next-variable-spec :: false-or(<next-variable-spec>),
    required-init-keyword: argument-next-variable-spec:;
end dood-class;

define method make
    (class == <method-signature-spec>, #rest all-keys, 
     #key argument-rest-variable-spec, argument-key?, 
          value-required-variable-specs, value-rest-variable-spec)
 => (res :: <method-signature-spec>)
  // format-out("ALL-KEYS %=\n", all-keys);
  if (argument-key?) // complex args?
    if (value-rest-variable-spec) // complex vals?
      apply(make, <method-complex-signature+complex-values-spec>, all-keys)
    elseif (value-required-variable-specs)
      apply(make, <method-complex-signature+values-spec>, all-keys)
    else 
      apply(make, <method-complex-signature-spec>, all-keys)
    end if
  elseif (argument-rest-variable-spec) // rested args
    if (value-rest-variable-spec) // complex vals?
      apply(make, <method-rested-signature+complex-values-spec>, all-keys)
    elseif (value-required-variable-specs)
      apply(make, <method-rested-signature+values-spec>, all-keys)
    else 
      apply(make, <method-rested-signature-spec>, all-keys)
    end if
  else // simple args
    if (value-rest-variable-spec) // complex vals?
      apply(make, <method-required-signature+complex-values-spec>, all-keys)
    elseif (value-required-variable-specs)
      apply(make, <method-required-signature+values-spec>, all-keys)
    else 
      apply(make, <method-required-signature-spec>, all-keys)
    end if
  end if;
end method;

define class <method-required-signature-spec>
    (<method-signature-spec>, <required-signature-spec>)
end class;

define class <method-required-signature+values-spec>
    (<required-values-spec>, <method-required-signature-spec>)
end class;

define class <method-required-signature+complex-values-spec> 
    (<complex-values-spec>, <method-required-signature+values-spec>)
end class;


define class <method-rested-signature-spec> 
    (<rested-signature-spec>, <method-signature-spec>)
end class;

define class <method-rested-signature+values-spec> 
    (<required-values-spec>, <method-rested-signature-spec>)
end class;

define class <method-rested-signature+complex-values-spec>
    (<complex-values-spec>, <method-rested-signature+values-spec>)
end class;


define class <method-complex-signature-spec> 
    (<complex-signature-spec>, <method-rested-signature-spec>)
end class;

define class <method-complex-signature+values-spec> 
    (<required-values-spec>, <method-complex-signature-spec>)
end class;

define class <method-complex-signature+complex-values-spec>
    (<complex-values-spec>, <method-complex-signature+values-spec>)
end class;

define class <method-polymorphic-signature-spec> (<method-signature-spec>)
  constant slot real-signature-spec :: <method-signature-spec>,
    required-init-keyword: signature:;
  constant slot type-variables :: <collection>,
    required-init-keyword: variables:;
end;

define method spec-argument-required-variable-specs (spec :: <method-polymorphic-signature-spec>)
  concatenate-as(<simple-object-vector>, spec.type-variables,
                 spec-argument-required-variable-specs(spec.real-signature-spec))
end method;

define method spec-argument-rest-variable-spec (spec :: <method-polymorphic-signature-spec>)
  spec-argument-rest-variable-spec(spec.real-signature-spec)
end method;

//define method spec-argument-next-variable-spec (spec :: <method-polymorphic-signature-spec>)
//  spec-argument-next-variable-spec(spec.real-signature-spec)
//end method;

define method spec-argument-key-variable-specs (spec :: <method-polymorphic-signature-spec>)
  spec-argument-key-variable-specs(spec.real-signature-spec)
end method;

define method spec-argument-key? (spec :: <method-polymorphic-signature-spec>)
  spec-argument-key?(spec.real-signature-spec)
end method;

/// GENERIC-SIGNATURE-SPEC

define constant <generic-signature-spec> = <signature-spec>;


/// PARSING ROUTINES

define serious-program-warning <unexpected-next-in-values-declaration>
  format-string "Unexpected #next in result values declaration - ignoring.";
end serious-program-warning;

define serious-program-warning <unexpected-key-in-values-declaration>
  format-string "Unexpected #key in result values declaration - ignoring.";
end serious-program-warning;

define serious-program-warning <unexpected-all-keys-in-values-declaration>
  format-string 
    "Unexpected #all-keys in result values declaration - ignoring.";
end serious-program-warning;

//// General purpose parameter/values list parser.

define method parse-variables-list (fragment) 
 => (requireds :: <variable-specs>,
     next :: false-or(<next-variable-spec>),
     rest :: false-or(<rest-variable-spec>),
     key? :: <boolean>,
     all-keys? :: <boolean>,
     keys :: <variable-specs>)

  collecting (required :: <variable-specs>, key :: <variable-specs>)
    let next      = #f;
    let rest      = #f;
    let key?      = #f;
    let all-keys? = #f;

    macro-case (fragment)

      { ?parameters:* }
        => values(collected(required), next, rest, key?, all-keys?, collected(key));

    parameters:
      { }
        => #f;
      { ?:name, ?parameters }
        => collect-first-into
             (required, make(<required-variable-spec>,
                             variable-name:   name));
      { ?:name :: ?type1:expression => ?type2:expression, ?parameters }
        => collect-first-into
             (required, make(<typed-required-variable-spec>,
                             variable-name:   name,
                             //for now, in real: limited(<function>) ?
                             type-expression: as-expression( #{ <function> } )));
      { ?:name :: ?type:expression, ?parameters }
        => collect-first-into
             (required, make(<typed-required-variable-spec>,
                             variable-name:   name,
                             type-expression: type));
      { ?:name == ?object:expression, ?parameters }
        => collect-first-into
             (required, make(<typed-required-variable-spec>,
                             variable-name:   name,
                             type-expression: 
                               as-expression(#{ singleton(?object) })));
      { ?next-etc }
        => #f;

    next-etc:
      { \#next ?:name, ?rest-etc }
        => next := make(<next-variable-spec>,
                        variable-name: name);
      { \#next ?:name :: ?type:expression, ?rest-etc }
        => next := make(<typed-next-variable-spec>,
                        variable-name:   name,
                        type-expression: type);
      { ?rest-etc }
        => #f;

    rest-etc:
      { }
        => #f;
      { \#rest ?:name, ?key-etc }
        => rest := make(<rest-variable-spec>,
                        variable-name: name);
      { \#rest ?:name :: ?type:expression, ?key-etc }
        => rest := make(<typed-rest-variable-spec>,
                        variable-name:   name,
                        type-expression: type);
      { ?key-etc }
        => #f;

    key-etc:
      { }
        => #f;
      { \#key, \#all-keys }
        => key? := (all-keys? := #t);
      { \#key ?key-spec-etc }
        => key? := #t;

    key-spec-etc:
      { }
        => #f;
      { ?opt-keyword ?:name :: ?type:expression ?opt-default, ?key-spec-etc }
        => collect-first-into
             (key, make(<key-variable-spec>,
                        variable-name:      name,
                        type-expression:    type,
                        default-expression: opt-default,
                        keyword-expression: opt-keyword));
      { ?all-keys-etc }
        => #f;

    all-keys-etc:
      { }
        => #f;
      { \#all-keys }
        => all-keys? := #t;

    opt-keyword:
      { }
        => #f;
      { ?:symbol }
        => symbol;

    opt-default:
      { }
        => #f;
      { = ?:expression }
        => expression;

    end macro-case;
  end collecting;
end method;

//// General purpose signature parser.

define method parse-values-list (vals) 
 => (requireds :: false-or(<variable-specs>), rest :: false-or(<rest-variable-spec>))
  let (value-requireds, value-next, value-rest, 
       value-key?, value-all-keys?, value-keys)
    = if (vals)
	parse-variables-list(vals);
      else
	values(#f, #f, #f, #f, #f, #f)
      end if;
  when (value-next)
    note(<unexpected-next-in-values-declaration>,
	 source-location: fragment-source-location(vals));
  end when;
  when (value-key?)
    note(<unexpected-key-in-values-declaration>,
	 source-location: fragment-source-location(vals));
  end when;
  when (value-all-keys?)
    note(<unexpected-all-keys-in-values-declaration>,
	 source-location: fragment-source-location(vals));
  end when;
  values(value-requireds, value-rest)
end method;

define method parse-signature-as 
    (sig-class :: <class>, fragment) => (signature :: <signature-spec>, remains)
  local method parse-using-fragments (sig-class, args, vals)
	  let (requireds, next, rest, key?, all-keys?, keys)
	    = parse-variables-list(args);
	  let (value-requireds, value-rest)
	    = parse-values-list(vals);
	  let sig-spec =
	  make(sig-class, 
	       argument-required-variable-specs: requireds,
	       argument-next-variable-spec:      next,
	       argument-rest-variable-spec:      rest,
	       argument-key?:                    if (all-keys?) #"all" else key? end,
	       argument-key-variable-specs:      keys,
	       value-required-variable-specs:    value-requireds,
	       value-rest-variable-spec:         value-rest);
          sig-spec
	end method;
  macro-case (fragment)
    { (All (?type-vars:*) ?rest:*) ?more:* }
      => begin
           let (sig, mor) = parse-signature-as(sig-class, rest);
           values(make(<method-polymorphic-signature-spec>,
                       signature: sig,
                       variables: parse-type-variable-list(type-vars),
                       argument-next-variable-spec: sig.spec-argument-next-variable-spec),
                  more);
         end;
    { (?args:*) => (?vals:*); ?more:* }
      => values(parse-using-fragments(sig-class, args, vals), more);
    { (?args:*) => (?vals:*) ?more:* }
      => values(parse-using-fragments(sig-class, args, vals), more);
    { (?args:*) => ?val:variable; ?more:* }
      => values(parse-using-fragments(sig-class, args, val), more);
    { (?args:*); ?more:* }
      => values(parse-using-fragments(sig-class, args, #f), more);
    { (?args:*) ?more:* }
      => values(parse-using-fragments(sig-class, args, #f), more);

  end macro-case;
end method;

define function parse-type-variable-list (fragment :: <fragment>) => (result :: <variable-specs>)
  collecting (required :: <variable-specs>)
    macro-case (fragment)

      { ?parameters:* }
        => collected(required);

    parameters:
      { }
        => #f;
      { ?:name, ?parameters }
        => collect-first-into
             (required, make(<typed-required-variable-spec>,
                             variable-name:   name,
                             type-expression: as-expression( #{ <type> })));
      { ?:name :: ?type:expression, ?parameters }
        => collect-first-into
             (required, make(<typed-required-variable-spec>,
                             variable-name:   name,
                             type-expression: as-expression( #{ subclass(?type) })));
//not yet sure whether singleton types will be needed
//      { ?:name == ?object:expression, ?parameters }
//        => collect-first-into
//             (required, make(<typed-required-variable-spec>,
//                             variable-name:   name,
//                             type-expression: 
//                               as-expression(#{ singleton(?object) })));
      { }
        => #f;
    end macro-case;
  end collecting
end;
//// Utilities.

define inline function as-boolean (object) => (boolean :: <boolean>)
  if (object) #t else #f end
end function;
