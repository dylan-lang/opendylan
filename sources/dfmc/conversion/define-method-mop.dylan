Module:   dfmc-conversion
Synopsis: The compile-time method protocol.
Author:   Keith Playford, from Jonathan's run-time code.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Method checking.

define serious-program-warning <duplicate-method-definition>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string
    "This method on %= is one of multiple definitions with the same "
    "specializers in this library.";
  format-arguments variable-name;
end serious-program-warning;

define serious-program-warning <imported-method-redefinition>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  slot condition-library,
    required-init-keyword: library:;
  format-string
    "A method on %= with the same specializers as this one is imported "
    "from %=.";
  format-arguments variable-name, library;
end serious-program-warning;

define serious-program-warning <method-on-a-sealed-imported-generic-function>
  slot condition-library-name,
    init-keyword: library-name:;
  slot condition-variable-name,
    init-keyword: variable-name:;
  format-string "This method on %= cannot be added because the generic was "
                "defined sealed in %=.";
  format-arguments variable-name, library-name;
end serious-program-warning;

define serious-program-warning <method-in-external-sealed-domain>
  slot condition-library-name,
    init-keyword: library-name:;
  slot condition-variable-name,
    init-keyword: variable-name:;
  format-string "This method on %= cannot be added because it is in a domain "
                "declared sealed in %=.";
  format-arguments variable-name, library-name;
end serious-program-warning;

define program-warning <method-visible-to-sibling-libraries>
  slot condition-variable-name,
    init-keyword: variable-name:;
  format-string 
    "This method on %= is visible to sibling libraries because its "
    "argument types are all based on imported classes.";
  format-arguments
    variable-name;
end program-warning;

// TODO: If a method falls outside any sealed domain (regardless of 
// whether it is within others) according to this test, it becomes
// a potentially blocking method. Hopefully, we can use this kind of
// information to refine the checks required when a class is defined. 

/*
define method number-locally-defined-domains 
    (gf :: <&generic-function>) => (res :: <integer>)
  let var = model-variable-binding(gf);
  let local-defs = untracked-lookup-local-modifying-definitions(var);
  let number :: <integer> = 0;
  for (def in local-defs)
    when (instance?(def, <domain-definition>))
      number := number + 1;
    end when;
  end for;
  number
end method;
*/

define method check-model-at-definition 
    (m :: <&method>) => (ok? :: <boolean>)
  // Needs to be called in proper context so that various modifying
  // definition lookups look at the right set of libraries.
  debug-assert(current-library-description?(model-library(m)));
  let gf = ^method-generic-function(m);
  // Don't need to check methods upgraded to functions or 
  // compiler-generated methods.
  let ok? = #t;
  if (single-method-generic-function?(gf) & gf ~== m)
    note(<method-on-a-sealed-imported-generic-function>,
         source-location: model-source-location(m),
         variable-name:   model-variable-name(m),
         library-name:    model-library(gf).library-description-emit-name);
    ok? := #f;
  elseif (gf ~== m & ~instance?(m, <&initializer-method>))
    // If this method implicitly defined the generic, not all checks are
    // required.
    if (form-parent-form(model-definition(gf)) ~== (model-definition(m)))
      // Congruence check.
      if (~check-congruence(gf, m))
        ok? := #f;
      end;
      // If the method and the generic are defined in the same library, 
      // we can again elide a bunch of checks.
      if (~current-library-description?(model-library(gf)))
        // Simple sealing violation check.
        if (^generic-function-sealed?(gf))
          note(<method-on-a-sealed-imported-generic-function>,
               source-location: model-source-location(m),
               variable-name:   model-variable-name(m),
               library-name:    model-library(gf).library-description-emit-name);
          ok? := #f;
        end;
        // Domain sealing check.
	// TODO: THE FOLLOWING SHOULD BE HIDDEN INSIDE A
	//       for-generic-function-explicitly-defined-external-domains 
	// let number-local-domains = number-locally-defined-domains(gf);
	for (domain in ^generic-function-imported-defined-domains(gf),
	     i :: <integer> from 0)
	  if (/* i >= number-local-domains & */
	      ~argument-types-known-disjoint?
		(^signature-required(^function-signature(m)),
		 ^domain-types(domain)))
            note(<method-in-external-sealed-domain>,
                 source-location: model-source-location(m),
                 variable-name:   model-variable-name(m),
                 library-name:    model-library(domain).library-description-emit-name);
            ok? := #f;
          
          end;
        end;
        // Leakage check.
        if (~(^generic-function-sideways?(gf) | ^method-sideways?(m))
              & all-types-known-imported?
                  (model-library(m), ^function-signature(m)))
          note(<method-visible-to-sibling-libraries>,
               source-location: model-source-location(m),
               variable-name:   model-variable-name(m));
        end;
      end;
    end;
  end;
  ok?
end method;

define method check-model (m :: <&method>) => ()
  // Needs to be called in proper context so that various modifying
  // definition lookups look at the right set of libraries.
  debug-assert(current-library-description?(model-library(m)));
  let gf = ^method-generic-function(m);
  // Don't need to check methods upgraded to functions or 
  // compiler-generated methods.
  if (single-method-generic-function?(gf) & gf ~== m)
  elseif (gf ~== m & ~instance?(m, <&initializer-method>))
    // Duplication check.
    let known-methods 
      = if (^method-local-only?(m))
          // We only need compare against other methods defined in this
          // library if the method has library-specific specializers.
          ^generic-function-local-methods-known(gf);
        else
          // Otherwise, we go the long way.
          ^generic-function-methods-known(gf);
        end;
    local method duplicate-method? (other-m :: <&method>)
      m ~== other-m & methods-have-same-specializers?(m, other-m)
    end method;
    let duplicates = choose(duplicate-method?, known-methods);
    if (~empty?(duplicates))
      let m-library = model-library(m);
      local method imported-method? (other-m :: <&method>)
        m-library ~== model-library(other-m);
      end method;
      let imported-duplicates = choose(imported-method?, duplicates);
      if (~empty?(imported-duplicates))
        // TODO: There should only be one since used libraries should be
        // checked for clashes when first used together.
        for (other-m in imported-duplicates)
          note(<imported-method-redefinition>,
               source-location: model-source-location(m),
               variable-name:   model-variable-name(m),
               library:         model-library(other-m).library-description-emit-name);
        end;
      end;
      if (size(imported-duplicates) ~= size(duplicates))
        note(<duplicate-method-definition>,
             source-location: model-source-location(m),
             variable-name:   model-variable-name(m));
      end;
    end;
  end;
end method;

define method ^generic-function-sideways? 
    (gf :: <&generic-function>) => (well? :: <boolean>)
  form-sideways?(model-definition(gf))
end method;

define method ^method-sideways? 
    (m :: <&method>) => (well? :: <boolean>)
  form-sideways?(model-definition(m))
end method;

define method ^method-generic-function (object :: <&method>)
  let binding = model-variable-binding(object);
  when (binding)
    let model = binding-model-object(binding, default: not-found());
    debug-assert(found?(model), "method GF not found");
    model
  end when;
end method;

define method argument-types-known-disjoint? (types1, types2)
  any?(^known-disjoint?, types1, types2);
end method;

// TODO: This hinges on what "exact same specializers" means in the
// description of add-method. It isn't exactly type equivalence -
// perhaps type equivalence plus the being the same kind of type?

define method methods-have-same-specializers? 
    (m1 :: <&method>, m2 :: <&method>) => (well? :: <boolean>)
  let types1 = ^signature-required(^function-signature(m1));
  let types2 = ^signature-required(^function-signature(m2));
  types1.size = types2.size 
    & every?(^type-equivalent?, types1, types2)
end method;

define method ^method-local-only? (m :: <&method>) => (well? :: <boolean>)
  some-types-known-local?(model-library(m), ^function-signature(m));
end method;

define method some-types-known-local? 
    (library, sig :: <&signature>) => (well? :: <boolean>)
  let n :: <integer> = ^signature-number-required(sig);
  let req :: <simple-object-vector> = ^signature-required(sig);
  (iterate loop (i :: <integer> = 0)
    if (i == n) 
      #f 
    elseif (type-known-local?(library, req[i]))
      #t
    else
      loop(i + 1)
    end
  end iterate)
end method;

define method type-known-local? (library, type) => (well? :: <boolean>)
  #f
end method;

define method type-known-local? 
    (library, type :: <&class>) => (well? :: <boolean>)
  library == model-library(type)
end method;

define method type-known-local? 
    (library, type :: <&singleton>) => (well? :: <boolean>)
  let object = ^singleton-object(type);
  instance?(object, <&class>) & type-known-local?(library, object)
end method;

define method type-known-local? 
    (library, type :: <&union>) => (well? :: <boolean>)
  type-known-local?(library, ^union-type1(type))
    | type-known-local?(library, ^union-type2(type))
end method;

define method type-known-local? 
    (library, type :: <&subclass>) => (well? :: <boolean>)
  type-known-local?(library, ^subclass-class(type));
end method;

define method all-types-known-imported? (library, types :: <collection>)
 => (imported? :: <boolean>)
  every?(curry(type-known-imported?, library), types);
end method;


define method all-types-known-imported? (library, sig :: <&signature>)
  let n :: <integer> = ^signature-number-required(sig);
  let req :: <simple-object-vector> = ^signature-required(sig);
  (iterate loop (i :: <integer> = 0)
    if (i == n) #t elseif (type-known-imported?(library, req[i])) loop(i + 1) end
  end iterate)
end method;


define method all-types-known-imported? (library, dom :: <&domain>)
  let n :: <integer> = ^domain-number-required(dom);
  (iterate loop (i :: <integer> = 0)
    if (i == n) #t elseif (type-known-imported?(library, ^domain-type(dom, i))) loop(i + 1) end
  end iterate)
end method;



define method type-known-imported? 
    (library, type) => (imported? :: <boolean>)
  #f
end method;

define method type-known-imported? 
    (library, type :: <&class>) => (imported? :: <boolean>)
  library ~== model-library(type);
end method;

// It's often quite legitimate to define a method on a singleton of 
// an imported class if you create the instance, so we don't really want 
// to warn in that case, unless perhaps it's some kind of known
// "interned" thing like a number or symbol.

define method type-known-imported? 
    (library, type :: <&singleton>) => (imported? :: <boolean>)
  let object = type.^singleton-object;
  let class = ^object-class(object);
  object == #()
    | class == dylan-value(#"<boolean>")
    | class == dylan-value(#"<symbol>")
    | if (instance?(object, <&class>) | instance?(object, <&function>))
	library ~== model-library(object)
      else
	^subtype?(class, dylan-value(#"<complex>"))
      end if
end method;

define method type-known-imported? 
    (library, type :: <&union>) => (imported? :: <boolean>)
  type-known-imported?(library, type.^union-type1)
    | type-known-imported?(library, type.^union-type2)
end method;

define method type-known-imported? 
    (library, type :: <&limited-integer>) => (imported? :: <boolean>)
  type-known-imported?(library, dylan-value(#"<integer>"));
end method;

define method type-known-imported? 
    (library, type :: <&subclass>) => (imported? :: <boolean>)
  type-known-imported?(library, type.^subclass-class);
end method;

// eof
