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

define settings <general-open-dylan-local-settings>
    (<local-software-settings>)
  key-name "Open Dylan";
end settings <general-open-dylan-local-settings>;

define settings <unversioned-open-dylan-local-settings>
    (<general-open-dylan-local-settings>)
  key-name "Open Dylan";
end settings <unversioned-open-dylan-local-settings>;

define settings <open-dylan-local-settings-1-0>
    (<unversioned-open-dylan-local-settings>)
  key-name "1.0";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
end settings <open-dylan-local-settings-1-0>;

define settings <open-dylan-local-settings-1-1>
    (<unversioned-open-dylan-local-settings>)
  key-name "1.1";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
  slot console-tools :: <boolean> = #f;
end settings <open-dylan-local-settings-1-1>;

define settings <open-dylan-local-settings-1-2>
    (<unversioned-open-dylan-local-settings>)
  key-name "1.2";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
  slot console-tools :: <boolean> = #f;
end settings <open-dylan-local-settings-1-2>;

define settings <open-dylan-local-settings-2-0>
    (<unversioned-open-dylan-local-settings>)
  key-name "2.0";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
  slot console-tools :: <boolean> = #f;
end settings <open-dylan-local-settings-2-0>;

define settings <open-dylan-local-settings-2-1>
    (<unversioned-open-dylan-local-settings>)
  key-name "2.1";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
  slot console-tools :: <boolean> = #f;
end settings <open-dylan-local-settings-2-1>;

define constant <open-dylan-local-settings>
  = <open-dylan-local-settings-1-0>;

define settings <general-open-dylan-user-settings>
    (<current-user-software-settings>)
  key-name "Open Dylan";
end settings <general-open-dylan-user-settings>;

define settings <unversioned-open-dylan-user-settings>
    (<general-open-dylan-user-settings>)
  key-name "Open Dylan";
end settings <unversioned-open-dylan-user-settings>;

define settings <open-dylan-user-settings-1-0>
    (<unversioned-open-dylan-user-settings>)
  key-name "1.0";
end settings <open-dylan-user-settings-1-0>;

define settings <open-dylan-user-settings-1-1>
    (<unversioned-open-dylan-user-settings>)
  key-name "1.1";
end settings <open-dylan-user-settings-1-1>;

define settings <open-dylan-user-settings-1-2>
    (<unversioned-open-dylan-user-settings>)
  key-name "1.2";
end settings <open-dylan-user-settings-1-2>;

define settings <open-dylan-user-settings-2-0>
    (<unversioned-open-dylan-user-settings>)
  key-name "2.0";
end settings <open-dylan-user-settings-2-0>;

define settings <open-dylan-user-settings-2-1>
    (<unversioned-open-dylan-user-settings>)
  key-name "2.1";
end settings <open-dylan-user-settings-2-1>;

define constant <open-dylan-user-settings>
  = <open-dylan-user-settings-1-0>;


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

define function release-encoded-required-library-packs
    () => (encoded-packs :: <machine-word>)
  let installed-packs :: <machine-word> = raw-encoded-library-packs();
  if (*library-packs-override*)
    // If the developer has overriden the library packs setting, claim
    // only the installed packs are required to assist bootstrapping...
    installed-packs
  else
    let packs :: <machine-word> = as(<machine-word>, 0);
    for (pack :: <integer> from 1 to $maximum-library-packs,
	 bit :: <machine-word> = as(<machine-word>, 1) then u%shift-left(bit, 1))
      if (library-pack-required?(pack))
	packs := %logior(packs, bit)
      end
    end;
    packs
  end
end function release-encoded-required-library-packs;

define function release-encoded-optional-library-packs
    () => (encoded-packs :: <machine-word>)
  let packs :: <machine-word> = raw-encoded-library-packs();
  if (*library-packs-override*)
    // If the developer has overriden the library packs setting, claim
    // all packs are required to avoid license checks during bootstrapping...
    as(<machine-word>, 0)
  else
    for (pack :: <integer> from 1 to $maximum-library-packs,
	 bit :: <machine-word> = as(<machine-word>, 1) then u%shift-left(bit, 1))
      if (library-pack-required?(pack))
	packs := %logand(packs, %lognot(bit))
      end
    end;
    packs
  end
end function release-encoded-optional-library-packs;

define function release-service-pack
    () => (service-pack :: <integer>)
  $local-settings.service-pack
end function release-service-pack;
