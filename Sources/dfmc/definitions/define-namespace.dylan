Module:   dfmc-definitions
Synopsis: Code common to library/module definition processing
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define dood-class <namespace-defining-form> /* abstract */
    (<defining-form>, <installable-form-properties>)
  constant slot form-namespace-name,
    required-init-keyword: name:;
  lazy constant slot form-use-clauses,
    required-init-keyword: use-clauses:;
  lazy constant slot form-create-clauses,
    required-init-keyword: create-clauses:;
  lazy constant slot form-export-clauses,
    required-init-keyword: export-clauses:;
end;

// Virtual slots
define sideways method namespace-name
    (space :: <namespace>) => (name :: <symbol>)
  form-namespace-name(namespace-definition(space));
end method;

// eof
