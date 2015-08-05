Module:   dfmc-conversion
Synopsis: The generic function compile-time MOP code
Author:   Paul Haahr, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract serious-program-warning <method-not-congruent>
  slot condition-incongruent-method,
    required-init-keyword: meth:;
  format-arguments meth;
end serious-program-warning;

define method check-congruence
    (gf :: <&generic-function>, m :: <&method>) => (ok? :: <boolean>)
  let (congruent?, warning-class)
    = ^signatures-congruent?
         (^function-signature(gf), ^function-signature(m));
  if (~congruent?)
    note(warning-class,
         source-location: model-source-location(m),
         meth:            model-definition(m));
  end;
  congruent?
end method;

// signatures-congruent?
//
// This predicate is used to determine whether a method signature is
// congruent with a generic function's signature.  Note that this is
// any asymmetric interface:  the question being asked is whether a
// method with the signature msig could be added to a generic function
// with the signature gsig.
//
// A second parameter, a reason for the lack of the congruence, is
// returned -- it's a shorthand, but might be useful for a program
// trying to display why two signatures differ yet doesn't want to do
// detailed analysis.  The possible reasons are:
//
//   #"required-argument-count"
//   #"required-argument-type"
//   #"optional-arguments"
//   #"required-keyword"
//   #"required-values-count"
//   #"required-values-type"
//   #"rest-values-type"
//
// The rules for congruence are spelled out in chapter 6, pp 91-92, of
// the 29 Sep 95 DRM.  The congruent rules in the comments are lifted
// from that text and Copyright (C) 1995 by Apple Computer.

define serious-program-warning <method-not-congruent-required-argument-count>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "they don't have the same number of required arguments";
end serious-program-warning;

define serious-program-warning <method-not-congruent-required-argument-type>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "some of the method's required parameter specializers aren't subtypes "
    "of their counterparts in the generic";
end serious-program-warning;

define serious-program-warning <method-not-congruent-not-both-keyword>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "one parameter list includes #key, but the other does not";
end serious-program-warning;

define serious-program-warning <method-not-congruent-not-both-variable>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "one parameter list includes #rest, but the other does not";
end serious-program-warning;

define serious-program-warning <method-not-congruent-mandatory-keyword>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "the method does not recognize some mandatory keywords of the "
    "generic";
end serious-program-warning;

define serious-program-warning <method-not-congruent-required-values-count>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "they don't have the same number of required results";
end serious-program-warning;

define serious-program-warning <method-not-congruent-required-values-type>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "some of the method's required values types don't match the type "
    "constraints of the generic";
end serious-program-warning;

define serious-program-warning <method-not-congruent-generic-values-not-variable>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "the method's values list includes #rest, but the generic's values list "
    "does not";
end serious-program-warning;

define serious-program-warning <method-not-congruent-required-values-count-too-small>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "the generic has more required return values than the method";
end serious-program-warning;

define serious-program-warning <method-not-congruent-no-required-values>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "the generic has required return values, but the method does not";
end serious-program-warning;

define serious-program-warning <method-not-congruent-rest-values-type>
    (<method-not-congruent>)
  format-string "Method %= not congruent with generic function - "
    "the method's rest value type is not a subtype of the rest value "
    "type of the generic";
end serious-program-warning;

define method ^signatures-congruent?
    (gsig :: <&signature>, msig :: <&signature>)
 => (congruent? :: <boolean>, warning-class :: false-or(<class>))

  block (return)
    local method fail (warning-class)
            return(#f, warning-class)
          end method fail;

    // --- required arguments ---

    // They have the same number of required arguments.
    //
    // Each of the method's parameter specializers is a subtype of the
    // corresponding parameter specializer of the generic function.

    let greq = gsig.^signature-required;
    let mreq = msig.^signature-required;
    if (gsig.^signature-number-required ~= msig.^signature-number-required)
      fail(<method-not-congruent-required-argument-count>);
    end if;
    unless (every?(^subtype?, mreq, greq))
      fail(<method-not-congruent-required-argument-type>);
    end unless;

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

      gsig.^signature-key? =>
        unless (msig.^signature-key?)
          fail(<method-not-congruent-not-both-keyword>);
        end unless;
        for (key in gsig.^signature-keys)
          unless (member?(key, msig.^signature-keys, test: ^id?))
            fail(<method-not-congruent-mandatory-keyword>);
          end unless;
        end for;

      msig.^signature-key? =>
        fail(<method-not-congruent-not-both-keyword>);

      // If neither has keys, we move on to the variable number cases.

      gsig.^signature-rest? =>
        unless (msig.^signature-rest?)
          fail(<method-not-congruent-not-both-variable>);
        end;

      msig.^signature-rest? =>
        fail(<method-not-congruent-not-both-variable>);

      // This is exhaustive. At this point neither has key, and neither
      // has rest, so they must both have a fixed number.

    end case;

    // --- value declarations ---

    // In addition, the value declarations must be congruent, defined
    // as follows:

    let grest = gsig.^signature-rest-value;
    if (~grest)

      // If the generic function's parameter list does not contain a rest
      // value declaration, then
      //
      //   The method's parameter list must not contain a rest value
      //   declaration.
      //
      //   The two parameter lists must contain the same number of
      //   required value declarations.
      //
      //   Each value type in the method's parameter list must be a
      //   subtype of the corresponding value type in the generic
      //   function's parameter list.

      if (msig.^signature-rest-value)
        fail(<method-not-congruent-generic-values-not-variable>);
      end if;
      let gvals = gsig.^signature-values;
      let mvals = msig.^signature-values;
      if (gsig.^signature-number-values ~= msig.^signature-number-values)
        fail(<method-not-congruent-required-values-count>);
      end if;
      unless (every?(^subtype?, mvals, gvals))
        fail(<method-not-congruent-required-values-type>);
      end unless;

    else

      // If the generic function's parameter list contains a rest value
      // declaration, then:
      //
      //   The method's parameter list is permitted, but not
      //   required, to contain a rest value declaration.
      //
      //   The method's parameter list must contain at least as many
      //   required value declarations as the generic function's
      //   parameter list.
      //
      //   Each value type in the method's parameter list must be a
      //   subtype of the corresponding value type in the generic
      //   function's parameter list. If the method has a rest value
      //   type, it corresponds to the generic function's rest value
      //   type. If the method has more required value types than the
      //   generic function, the extra ones must be subtypes of the
      //   generic function's rest value type.

      let gvals = gsig.^signature-values;
      let mvals = msig.^signature-values;
      if (msig.^signature-number-values < gsig.^signature-number-values)
        if (msig.^signature-number-values = 0)
          fail(<method-not-congruent-no-required-values>);
        else
          fail(<method-not-congruent-required-values-count-too-small>);
        end;
      end if;
      for (mval in mvals, index from 0 below msig.^signature-number-values)
        let gval = element(gvals, index, default: grest);
        unless (^subtype?(mval, gval))
          fail(<method-not-congruent-required-values-type>);
        end unless;
      end for;
      let mrest = msig.^signature-rest-value;
      if (mrest & ~^subtype?(mrest, grest))
        fail(<method-not-congruent-rest-values-type>);
      end if;

    end if;

    values(#t, #f)
  end block;

end method ^signatures-congruent?;
