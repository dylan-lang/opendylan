Module:    release-info
Synopsis:  Functional Developer release information -- settings and user profiles
Author:    Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Functional Objects settings

// General Open Dylan settings are those settings pertaining to all
// software maintained to the Dylan Hackers, not specifically to
// the Open Dylan compiler.  Maybe they should be called something
// like <dylan-hackers-settings>? --tc

define settings <open-dylan-local-settings>
    (<local-software-settings>)
  key-name "1.0";
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
end settings <open-dylan-local-settings>;

define settings <open-dylan-user-settings>
    (<current-user-software-settings>)
  key-name "1.0";
end settings <open-dylan-user-settings>;


/// Library and service pack settings

define constant $local-settings = make(<open-dylan-local-settings>);

define variable *library-packs-override* = #"unknown";

define function raw-encoded-library-packs () => (encoded-packs :: <machine-word>)
  if (*library-packs-override* = #"unknown")
    let raw-override = environment-variable("OPEN_DYLAN_LIBRARY_PACKS");
    if (raw-override)
      *library-packs-override* := string-to-machine-word(raw-override)
    else
      *library-packs-override* := #f
    end
  end;
  *library-packs-override*
    | $local-settings.library-packs
end function raw-encoded-library-packs;

define function release-encoded-library-packs
    () => (encoded-packs :: <machine-word>)
  raw-encoded-library-packs()
end function release-encoded-library-packs;
