module:    harp-registers
Synopsis:  The class definition for <real-register>
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract open primary class <real-register> (<register>)
  slot real-register-mask :: <integer>, 
      init-value: 0, init-keyword: mask:;
  slot real-register-number :: <integer>, 
      init-value: 0, init-keyword: number:;
  slot real-register-pname, 
      init-value: #F, init-keyword: pname:;
  slot real-register-preserved-mask :: <integer>, 
      init-value: 0, init-keyword: preserved-mask:;
  slot real-register-c-preserved-mask :: <integer>, 
      init-value: 0, init-keyword: c-preserved-mask:;
end;

