Module:    release-info-implementation
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Release constants
define constant $release-product-name = "Functional Developer";
define constant $release-edition      = "Emulator";
define constant $release-edition-type = #"emulator";
define constant $release-version      = "Internal Release";
define constant $release-version-type = #"1.1";
define constant $release-beta?        = #f;

define constant $release-copyright
  = "Copyright (c) 1997-2004, Functional Objects, Inc. "
    "All rights reserved.";


/// Implementations

define sideways method release-product-name () => (name :: <string>)
  $release-product-name
end method release-product-name;

define sideways method release-edition () => (name :: <string>)
  $release-edition
end method release-edition;

define sideways method release-edition-type () => (name :: <symbol>)
  $release-edition-type
end method release-edition-type;

define sideways method release-version () => (name :: <string>)
  $release-version
end method release-version;

define sideways method release-version-type () => (version :: <symbol>)
  $release-version-type
end method release-version;

define sideways method release-beta? () => (beta? :: <boolean>)
  $release-beta?
end method release-beta?;

define sideways method release-copyright () => (name :: <string>)
  $release-copyright
end method release-copyright;
