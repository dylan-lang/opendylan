Module:   dfmc-definitions
Synopsis: The parser for binding specifications.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Used by potential multiple-value binding forms: define constant, 
// define variable, and let.

define method bound-variable-names (spec :: <values-spec>) => (res :: <variable-specs>)
  concatenate(map(spec-variable-name, spec-value-required-variable-specs(spec)),
              if (spec-value-rest-variable-spec(spec))
                vector(spec-variable-name(spec-value-rest-variable-spec(spec)))
              else
                #[]
              end)
end method;

define method bound-type-expressions (spec :: <values-spec>) => (res :: <variable-specs>)
  concatenate(map(spec-type-expression, spec-value-required-variable-specs(spec)),
              if (spec-value-rest-variable-spec(spec))
                vector(spec-type-expression(spec-value-rest-variable-spec(spec)))
              else
                #[]
              end)
end method;

define method parse-value-bindings (bindings)
  let (value-requireds, value-rest)
    = parse-values-list(bindings);
  let spec
    = make(<values-spec>,
	   value-required-variable-specs: value-requireds,
	   value-rest-variable-spec:      value-rest);
  spec
end method;
