Module: dfmc-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define compiler-open class <emitter> (<code-walker>)
  constant slot emitter-back-end :: <back-end>, required-init-keyword: back-end:;
  constant slot emitter-stream :: <stream>, required-init-keyword: stream:;
end class;

define compiler-open generic make-emitter
    (back-end :: <back-end>, stream :: <stream>) => (emitter :: <emitter>);
