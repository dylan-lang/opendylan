module:   sealing-workbench
author:   Paul Haahr
synopsis: Function classes and objects in the sealing world.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////
//// Function meta-objects
////

define class <&function> (<&object>)
  slot &function-parameters :: <sequence>, required-init-keyword: parameters:;
end class <&function>;

define class <&generic> (<&function>)
  slot &generic-methods :: <sequence>, init-value: #();
  slot open?, init-value: #f, init-keyword: open?:;
  slot sealed-domains :: <sequence>, init-value: #();
end class <&generic>;

define class <sealed-domain> (<object>)
  slot &generic :: <&generic>, required-init-keyword: &generic:;
  slot types :: <sequence>, required-init-keyword: types:;
  slot library :: <library>, required-init-keyword: library:;
end class <sealed-domain>;

define class <&method> (<&function>)
  slot calls-next-method?, init-value: #f, init-keyword: calls-next-method?:;
end class <&method>;

define &class &<function> = <&function> (&<object>);
define &class &<generic> = <&generic> (&<object>);
define &class &<method> = <&method> (&<object>);

define constant no-applicable-&method
  = &make(&<method>, name: #"no-applicable-method",
          parameters: #(), library: $dylan-library);


////
//// congruence
////

// too much type-punning here

define method &congruent? (types-1 :: <sequence>, types-2 :: <sequence>)
  types-1.size == types-2.size & every?(&subtype?, types-1, types-2)
end method &congruent?;

define method &congruent? (function :: <&function>, types :: <sequence>)
  &congruent?(function.&function-parameters, types)
end method &congruent;

define method &congruent? (types :: <sequence>, function :: <&function>)
  &congruent?(types, function.&function-parameters)
end method &congruent;

define method &congruent?
    (function-1 :: <&function>, function-2 :: <&function>)
  &congruent?(function-1.&function-parameters,
              function-2.&function-parameters)
end method &congruent;


////
//// defining methods
////

define method &add-method (&generic :: <&generic>, &method :: <&method>) => ()
  unless (&generic.open? | &generic.library == &method.library)
    error("sealing violation in adding %= to %=", &method, &generic)
  end unless;
  unless (&congruent?(&method, &generic))
    error("%= not congruent with %=", &method, &generic)
  end unless;

  for (domain in &generic.sealed-domains)
    if (&method.library ~== domain.library
        & ~any?(&disjoint?, &method.&function-parameters, domain.types))

      // If we've reached here, there is nothing in this method that is
      // provably disjoint.  If we can add disjointness between some
      // classes to make it disjoint, we do that, otherwise we have
      // to signal the error.

      let disjointified = #();

      for (old-type in domain.types,
	   new-type in &method.&function-parameters)
	unless (&joint?(old-type, new-type))
	  require-disjointness(old-type, new-type, domain);
	  disjointified := add!(disjointified,
				vector(old-type, new-type))
	end if;
      end for;

      select (disjointified.size)
	0 =>
	  error("sealing violation in adding %= to %= (sealed in %= over %=)",
		&method, &generic, domain.library, domain.types);
	1 =>
	  ;
	otherwise =>
	  signal("too conservative sealing enforcement in adding %= to %= "
                 "(sealed in %= over %=): disjointness enforced on %=",
		 &method, &generic, domain.library, domain.types,
		 disjointified)
      end select

    end if
  end for;

  &generic.&generic-methods := add!(&generic.&generic-methods, &method)
end method &add-method;


////
//// method sealing
////

define method &seal-generic
    (&generic :: <&generic>, types :: <sequence>, library :: <library>)
  unless (&congruent?(types, &generic))
    error("%= not congruent with %=", types, &generic);
  end unless;
  let domain = 
    make(<sealed-domain>, &generic: &generic, types: types, library: library);
  local method add-domain(to)
	  to.sealed-domains := add!(to.sealed-domains, domain)
	end method add-domain;
  add-domain(&generic);
  add-domain(library);
  values()
end method &seal-generic;

define method sealed-over? (&generic :: <&generic>, &types :: <sequence>)
  ~&generic.open?
  | any?(curry(&congruent?, &types), map(types, &generic.sealed-domains))
end method sealed-over?;

define method require-disjointness
    (type-1 :: <&type>, type-2 :: <&type>, reason)
  format-out("!! require disjointness of %= and %= because %=\n",
	     type-1, type-2, reason)
end method require-disjointness;

define method finalize-definition (domain :: <sealed-domain>)
  let gf = domain.&generic;
  local method relevent?(meth)
	  ~any?(&disjoint?, meth.&function-parameters, domain.types)
	end method relevent?;
  let methods = choose(relevent?, gf.&generic-methods);

  for (sealing-type in domain.types, position from 0)
    let specializers = map(compose(rcurry(element, position),
				   &function-parameters),
			   gf.&generic-methods);

    for (specializer in specializers)
      // potentially blocking from rule 3
      // Si = specializer
      // Ti = sealing-type

      // for (all Dj and Dk pairs)
      //   require-disjointness(Dj, Dk)
      // end for;

      // TODO:  get rid of redundancy

      for (Dj in specializer.&all-subclasses,
	   Dk in sealing-type.&all-subclasses)
        // TODO: open subclasses of sealed classes
	if (Dk.open? & Dj.open? & ~&subtype?(Dk, specializer))
	  require-disjointness(Dj, Dk, domain)
	end unless;
      end for;

    end for;
  end for;

end method finalize-definition;
