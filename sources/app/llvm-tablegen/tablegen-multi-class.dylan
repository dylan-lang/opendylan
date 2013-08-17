Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2011 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <tablegen-multi-class> (<object>)
  constant slot multi-class-name :: <string>,
    required-init-keyword: name:;
  constant slot multi-class-template-parameters :: <sequence>,
    init-value: #[], init-keyword: template-parameters:;
  constant slot multi-class-definitions :: <sequence>,
    init-value: #[], init-keyword: definitions:;
end class;
