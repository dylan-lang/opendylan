Module: java-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// XXX: The below is just a guess at what used to be here. This file was lost
// at some point in time.

define class <java-package> (<object>)
  constant slot name-component :: <string>,
    required-init-keyword: name:;
  constant slot super-package :: false-or(<java-package>) = #f,
    init-keyword: super:;
end;

ignore(super-package);

define method java-package (name :: <string>, #key super)
  make(<java-package>, name: name, super: super);
end;

define constant $java-default-package$ = make(<java-package>, name: "");

define method package-concatenate-with-name (package :: <java-package>, name :: <string>)
  // XXX: This should be taking super-package into account to get a full package name.
  concatenate(package.name-component, ".", name)
end;
