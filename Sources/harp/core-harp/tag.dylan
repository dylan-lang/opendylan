module:    base-harp
Synopsis:  The <tag> class definition.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Tags



/// Here is the code from bb.lisp


define open primary class <tag> (<object>)
  slot tag-no :: <integer>, init-value: -1, init-keyword: tag-no:;
  slot tag-bb, init-value: #F;
  slot tag-variables :: <harp-variables>, init-keyword: variables:;
end;


define open generic make-tag  (backend :: <harp-back-end>) => (new :: <tag>);

define method make-tag (backend :: <harp-back-end>) => (new :: <tag>)
  let vars = backend.variables;
  let num = inc!(vars.taag-no);
  make(<tag>, tag-no: num, variables: vars);
end;
