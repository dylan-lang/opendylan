Module:    release-info-implementation
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Release constants
define constant $release-product-name     = "Functional Developer";
define constant $release-trademarked-name = "Functional Developer(tm)";
define constant $release-edition          = "Internal Edition";
define constant $release-edition-type     = #"internal";
define constant $release-version          = "Version 2.1 [Alpha 4]";
define constant $release-version-type     = #"2.1";
define constant $release-beta?            = #t;

define constant $release-copyright
  = "Copyright (c) 1997-2004, Functional Objects, Inc. "
    "All rights reserved.";


/// Implementations

define sideways method release-product-name () => (name :: <string>)
  $release-product-name
end method release-product-name;

define sideways method release-trademarked-name () => (name :: <string>)
  $release-trademarked-name
end method release-trademarked-name;

define sideways method release-edition () => (edition :: <string>)
  $release-edition
end method release-edition;

define sideways method release-edition-type () => (edition :: <symbol>)
  $release-edition-type
end method release-edition-type;

define sideways method release-version () => (version :: <string>)
  release-full-version($release-version)
end method release-version;

define sideways method release-version-type () => (version :: <symbol>)
  $release-version-type
end method release-version-type;

define sideways method release-beta? () => (beta? :: <boolean>)
  $release-beta?
end method release-beta?;

define sideways method release-copyright () => (copyright :: <string>)
  $release-copyright
end method release-copyright;
