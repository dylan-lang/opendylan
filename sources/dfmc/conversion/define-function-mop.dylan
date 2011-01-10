Module:   dfmc-conversion
Synopsis: The compile-time function protocol.
Author:   Keith Playford, from Jonathan's run-time code.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Function initialization.

define compiler-sideways method ^next? (function :: <&method>) => (value :: <boolean>)
  // This information is only available once the DFM for the method has
  // been generated.
  if (^function-next?(function) == #"unknown")
    ensure-method-model(function);
  end if;
  ^function-next?(function)
end method ^next?;

define compiler-sideways method ^next?-setter 
    (new-value :: <boolean>, function :: <&method>) => (value :: <boolean>)
  ^function-next?(function) := new-value;
end method ^next?-setter;
