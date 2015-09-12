Module:    coloring-stream-internals
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright 2015 Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define method print-object
    (attributes :: <text-attributes>,
     stream :: <windows-coloring-stream>)
 => ()
  ignore(attributes, stream);
end method print-object;
