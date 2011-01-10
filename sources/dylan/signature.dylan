module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline-only function sigcheck (val, str)
  if (~val)
    error(make(<argument-error>,
	       format-string: "Attempt to create a function signature for which %=",
	       format-arguments: list(str)))
  end if
end function;


define method make
    (class == <signature>, #rest all-keys, 
     #key required, key?, all-keys?, values: vals = #[], rest-value = <object>,
          number-required, number-values,
     #all-keys)
 => (res)
  
  let (number-required :: <integer>, required :: <simple-object-vector>)
    = select (required by instance?)
	<integer> => 
	  sigcheck(~number-required | number-required == required,
		   "number-required: was different from required:.");
	  values(required, $signature-<object>-types);
	<sequence> =>
	  let v :: <simple-object-vector> = as(<simple-object-vector>, required);
	  let n :: <integer> = size(v);
	  values(if (number-required)
		   sigcheck(number-required <= n, 
			    "number-required: is greater than the size of the required: sequence.");
		   number-required
		 else
		   n
		 end if,
		 v);
      end select;

  let (number-values :: <integer>, vals :: <simple-object-vector>)
    = select (vals by instance?)
	<integer> => 
	  sigcheck(~number-values | number-values == vals,
		   "number-values: was specified when values: was an integer.");
	  values(vals, $signature-<object>-types);
	<sequence> =>
	  let v :: <simple-object-vector> = as(<simple-object-vector>, vals);
	  let n :: <integer> = size(v);
	  values(if (number-values)
		   sigcheck(number-values <= n,
			    "number-values: is greater than the size of the values: sequence.");
		   number-values
		 else
		   n
		 end if, 
		 v)
      end select;

  let default-values? = begin
			  local method loop (i :: <integer>)
				  if (i >= number-values)
				    #t
				  elseif (vector-element(vals, i) == <object>)
				    loop(i + 1)
				  end if
				end method;
			  loop(0)
			end;
  let default-rest-value?-value 
    = rest-value == <object>;
  let default-rest-value? 
    = ~rest-value | default-rest-value?-value;
  if (all-keys? & ~ key?)
    error(make(<argument-error>,
	       format-string:
		 "Attempt to create a non-#key function signature for which all-keys? is true.",
	       format-arguments: #[]))
  end if;
  if (~key? & default-values? & default-rest-value?)
    apply(next-method, <signature>,
          default-values?:     default-values?,
	  rest-value?:         ~(~rest-value),
          required:            required,
	  number-required:     number-required,
          number-values:       number-values,
	  all-keys)
  else
    apply(make,
          if (key?)
            if (default-values?)
              if (default-rest-value?)
                <keyword-signature>
              else 
                <keyword-signature+rest-value>
              end if
	    else            
	      if (default-rest-value?)
		<keyword-signature+values>
	      else 
		<keyword-signature+values+rest-value>
	      end if
            end if
          else
            if (default-values?)
              <signature+rest-value>
            else 
              if (default-rest-value?)
                <signature+values> 
              else 
                <signature+values+rest-value> 
              end if 
            end if
          end if,
          default-values?:     default-values?,
	  values:              vals,
	  number-values:       number-values,
	  rest-value:          rest-value,
	  rest-value?:         ~(~rest-value),
          required:            required,
	  number-required:     number-required,
          all-keys)
   end if
end method;

define method signature-keys (sig :: <signature>)
  #[]
end method;

define method signature-values 
    (sig :: <signature>) => (res :: <simple-object-vector>)
  if (signature-default-values?(sig))
    $signature-<object>-types
  else
    #[]
  end if
end method;

define method signature-rest-value
    (sig :: <signature>) => (res :: false-or(<type>))
  if (signature-rest-value?(sig))
    <object>
  else
    #f
  end if
end method;

define method initialize
    (sig :: <signature>, #rest all-keys, 
     #key properties, next?, sealed-domain?, default-values?,
     #all-keys)
  next-method();
  if (properties)
    signature-next?(sig)               := next?;
    signature-sealed-domain?(sig)      := sealed-domain?;
    signature-default-values?(sig)     := default-values?;
  else
    apply(initialize-packed-slots, sig, all-keys)
  end if;
  initialize-signature-completeness(sig);
end method;

define leaf packed-slots signature-properties (<signature>, <object>)
  field   slot signature-number-required = 0, field-size: 8,
    init-keyword: number-required:;
  field   slot signature-number-values   = 0, field-size: 8,
    init-keyword: number-values:;
  boolean slot signature-key? = #f,
    init-keyword: key?:;
  boolean slot signature-all-keys? = #f,
    init-keyword: all-keys?:;
  boolean slot signature-rest? = #f,
    init-keyword: rest?:;
  boolean slot signature-rest-value? = #f,
    init-keyword: rest-value?:;
  boolean slot signature-next? = #f,
    init-keyword: next?:;
  boolean slot signature-default-values? = #f,
    init-keyword: default-values?:;
  boolean slot signature-sealed-domain? = #f,
    init-keyword: sealed-domain?:;
  boolean slot signature-complete? = #t,
    init-keyword: complete?:;
end packed-slots;

define inline method signature-optionals? 
    (sig :: <signature>) => (result :: <boolean>)
  signature-key?(sig) | signature-rest?(sig)
end method signature-optionals?;


// This function asks the question:  Does this method, in a generic function's
// methods, imply a sealed domain over the method's specializers?
// define sealed generic sealed-domain? (m :: <method>) => (well? :: <boolean>);

define constant $required-argument-count 
  = "they don't have the same number of required arguments";

define constant $required-argument-type 
    = "some of the method's required parameter specializers aren't subtypes "
    "of their counterparts in the generic";

define constant $not-both-keyword 
  = "one parameter list includes #key, but the other does not";

define constant $not-both-variable 
  = "one parameter list includes #rest, but the other does not";

define constant $mandatory-keyword
  = "the method does not recognize some mandatory keywords of the "
    "generic";

define constant $required-values-count
  = "they don't have the same number of required results";

define constant $required-values-type
  = "some of the method's required values types don't match the type "
    "constraints of the generic";

define constant $generic-values-not-variable
  = "the method's values list includes #rest, but the generic's values list"
    "does not";

define constant $required-values-count-too-small
  = "the generic has more required return values than the method";

define constant $rest-values-type
  = "the method's rest value type is not a subtype of the rest value "
    "type of the generic";

define method congruent? (gsig :: <signature>, msig :: <signature>)
 => (congruent? :: <boolean>, reason);

  block (return)
    local method fail (reason)
	    return(#f, reason)
	  end method fail;

    // --- required arguments ---

    // They have the same number of required arguments.
    //
    // Each of the method's parameter specializers is a subtype of the
    // corresponding parameter specializer of the generic function.

    let greq :: <simple-object-vector> = gsig.signature-required;
    let mreq :: <simple-object-vector> = msig.signature-required;
    let gsiz :: <integer> = gsig.signature-number-required;
    let msiz :: <integer> = msig.signature-number-required;
    if (gsiz ~== msiz)
      fail($required-argument-count);
    end if;
    for (i :: <integer> from 0 below gsiz)
      unless (lazy-subtype?(vector-element(mreq, i), vector-element(greq, i)))
	fail($required-argument-type);
      end unless
    end for;

    // --- optional arguments ---

    // One of the following is true:
    //
    //   both accept keyword arguments
    //
    //   both accept a variable number of arguments
    //
    //   both require a fixed number of arguments
    //
    // If the generic function accepts keyword arguments, each method
    // must recognize the mandatory keywords of the generic function.

    case

      gsig.signature-key? =>
	unless (msig.signature-key?)
	  fail($not-both-keyword);
	end unless;
	let gsig :: <keyword-signature> = gsig;
	let msig :: <keyword-signature> = msig;
	let mkeys :: <simple-object-vector> = msig.signature-keys;
	for (key in gsig.signature-keys)
	  unless (member?(key, mkeys))
	    fail($mandatory-keyword);
	  end unless;
	end for;

      msig.signature-key? =>
        fail($not-both-keyword);

      // If neither has keys, we move on to the variable number cases.

      gsig.signature-rest? =>
        unless (msig.signature-rest?)
          fail($not-both-variable);
        end;

      msig.signature-rest? =>
        fail($not-both-variable);

      // This is exhaustive. At this point neither has key, and neither
      // has rest, so they must both have a fixed number.

    end case;

    // --- value declarations ---
    let grestv? = gsig.signature-rest-value?;
    let mrestv? = msig.signature-rest-value?;
    let gvals :: <simple-object-vector> = gsig.signature-values;
    let mvals :: <simple-object-vector> = msig.signature-values;
    let gvsiz :: <integer> = gsig.signature-number-values;
    let mvsiz :: <integer> = msig.signature-number-values;
    if (~grestv? & mrestv?) fail($generic-values-not-variable) end;
    if (mvsiz < gvsiz) fail($required-values-count-too-small) end;
    if (grestv?) 
      let grestv :: <type> = gsig.signature-rest-value;
      for (i :: <integer> from gvsiz below mvsiz)
	unless (lazy-subtype?(vector-element(mvals, i), grestv))
	  fail($required-values-type)
	end unless
      end for;
      if (mrestv? & ~lazy-subtype?(msig.signature-rest-value, grestv))
	fail($rest-values-type)
      end if;
    elseif (mvsiz ~== gvsiz)
      fail($required-values-count)
    end if;
    for (i :: <integer> from 0 below gvsiz)
      unless (lazy-subtype?(vector-element(mvals, i), vector-element(gvals, i)))
	fail($required-values-type)
      end unless
    end for;

    values(#t, #"congruent")
  end block;

end method congruent?;




define inline method type-complete? (sig :: <signature>) => (well? :: <boolean>)
  signature-complete?(sig)
end method;


define function initialize-signature-completeness (sig :: <signature>) => (well? :: <boolean>)
  signature-complete?(sig) 
    := ( type-complete?-sov(signature-required(sig), signature-number-required(sig))
	  & (~instance?(sig, <signature-rest-value-mixin>)
	       | type-complete?(signature-rest-value(sig)))
	  & (~instance?(sig, <signature-values-mixin>)
	       | type-complete?-sov(signature-values(sig), signature-number-values(sig))));
end function;


define inline method recompute-type-complete! (sig :: <signature>) => (well? :: <boolean>)
  signature-complete?(sig) | initialize-signature-completeness(sig)
end method;


define method map-congruency-classes (f :: <function>, sig :: <signature>) => ()
  map-congruency-classes-sov(f, signature-required(sig), signature-number-required(sig))
end method;


define method reduce-incomplete-classes (f :: <function>, sig :: <signature>, ans) 
 => (ans)
  reduce-incomplete-classes-sov(f, signature-required(sig), signature-number-required(sig), ans)
end method;


// While strictly true, this isn't what makes a method unusable.
//define method reduce-incomplete-classes (f :: <function>, x :: <keyword-signature>, ans)
// => (ans)
//  reduce-incomplete-classes-sov(f, signature-key-types(x), signature-number-keys(x), next-method())
//end method;


define method map-congruency-classes (f :: <function>, x :: <signature-rest-value-mixin>) => ()
  map-congruency-classes(f, signature-rest-value(x));
  next-method();
end method;


define method reduce-incomplete-classes (f :: <function>, x :: <signature-rest-value-mixin>, ans)
 => (ans )
  reduce-incomplete-classes(f, signature-rest-value(x), next-method())
end method;


define method map-congruency-classes (f :: <function>, x :: <signature-values-mixin>) => ()
  map-congruency-classes-sov(f, signature-values(x), signature-number-values(x));
  next-method();
end method;


define method reduce-incomplete-classes (f :: <function>, x :: <signature-values-mixin>, ans)
 => (ans)
  reduce-incomplete-classes-sov(f, signature-values(x), signature-number-values(x), next-method())
end method;
