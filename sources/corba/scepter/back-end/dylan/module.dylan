Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-module> (<dim>)
end class;

define method make-dim (node :: <ast-module>)
 => (model :: <dim-module>)
  make(<dim-module>, node: node);
end method;
