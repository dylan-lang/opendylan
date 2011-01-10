Module:   employee-explorer
Synopsis: The employee class for the Employee Explorer
Author:   Keith Playford after Ed Cessna
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Simple employee tree structure.

define class <employee> (<object>)
  slot id :: <integer>,
    required-init-keyword: id:;

  slot full-name :: <string>,
    required-init-keyword: full-name:;

  slot first-name :: <string>,
    required-init-keyword: first-name:;

  slot last-name :: <string>,
    required-init-keyword: last-name:;

  slot extension :: <string>,
    required-init-keyword: extension:;

  slot boss :: false-or(<employee>),
    required-init-keyword: boss:;

  slot %subordinates :: false-or(<sequence>) = #f;
end class;

define variable *bosses* = #f;

define method bosses () => (bosses :: <sequence>)
  *bosses* | (*bosses* := compute-bosses())
end method;

define method subordinates (e :: <employee>) => (sub :: <sequence>)
  %subordinates(e) | (%subordinates(e) := compute-subordinates(e))
end method;

define method subordinates? (e :: <employee>) => (well? :: <boolean>)
  ~empty?(subordinates(e))
end method;
