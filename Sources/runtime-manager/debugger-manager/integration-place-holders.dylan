module:       dm-internals
synopsis:     Integration Place Holders:
              Functions I hope to get from someplace else at some point...
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <SOURCE-LOCATOR>
//    This should eventually be imported from the source-records library.
//    Should be fairly painless to swap in the real definition.

define class <source-locator> (<object>)

  constant slot source-locator-file :: <string>,
    required-init-keyword: filename:;

  constant slot source-locator-linenumber :: <integer>,
    required-init-keyword: line:;

end class;


///// LOC
//    A convenience function for constructing a source-locator.

define method loc (f :: <string>, l :: <integer>) => (scl)
  make(<source-locator>,
       filename: f,
       line: l);
end method;


