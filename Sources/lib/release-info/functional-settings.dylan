Module:    release-info-internals
Synopsis:  Functional Developer release information -- settings and user profiles
Author:    Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Functional Objects settings

define settings <functional-objects-local-settings>
    (<local-software-settings>)
  key-name "Functional Objects";
end settings <functional-objects-local-settings>;

define settings <unversioned-functional-developer-local-settings>
    (<functional-objects-local-settings>)
  key-name "Functional Developer";
end settings <unversioned-functional-developer-local-settings>;

define settings <functional-developer-local-settings-1-0>
    (<unversioned-functional-developer-local-settings>)
  key-name "1.0";
end settings <functional-developer-local-settings-1-0>;

define settings <functional-developer-local-settings-1-1>
    (<unversioned-functional-developer-local-settings>)
  key-name "1.1";
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
end settings <functional-developer-local-settings-1-1>;

define settings <functional-developer-local-settings-1-2>
    (<unversioned-functional-developer-local-settings>)
  key-name "1.2";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
end settings <functional-developer-local-settings-1-2>;

define settings <functional-developer-local-settings-2-0>
    (<unversioned-functional-developer-local-settings>)
  key-name "2.0";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
  slot console-tools :: <boolean> = #f;
end settings <functional-developer-local-settings-2-0>;

define settings <functional-developer-local-settings-2-1>
    (<unversioned-functional-developer-local-settings>)
  key-name "2.1";
  slot service-pack :: <integer> = 0;
  slot library-packs :: <machine-word> = as(<machine-word>, 0);
  slot console-tools :: <boolean> = #f;
end settings <functional-developer-local-settings-2-1>;

define constant <functional-developer-local-settings>
  = <functional-developer-local-settings-2-1>;


define settings <functional-objects-user-settings>
    (<current-user-software-settings>)
  key-name "Functional Objects";
end settings <functional-objects-user-settings>;

define settings <unversioned-functional-developer-user-settings>
    (<functional-objects-user-settings>)
  key-name "Functional Developer";
end settings <unversioned-functional-developer-user-settings>;

define settings <functional-developer-user-settings-1-0>
    (<unversioned-functional-developer-user-settings>)
  key-name "1.0";
end settings <functional-developer-user-settings-1-0>;

define settings <functional-developer-user-settings-1-1>
    (<unversioned-functional-developer-user-settings>)
  key-name "1.1";
end settings <functional-developer-user-settings-1-1>;

define settings <functional-developer-user-settings-1-2>
    (<unversioned-functional-developer-user-settings>)
  key-name "1.2";
end settings <functional-developer-user-settings-1-2>;

define settings <functional-developer-user-settings-2-0>
    (<unversioned-functional-developer-user-settings>)
  key-name "2.0";
end settings <functional-developer-user-settings-2-0>;

define settings <functional-developer-user-settings-2-1>
    (<unversioned-functional-developer-user-settings>)
  key-name "2.1";
end settings <functional-developer-user-settings-2-1>;

define constant <functional-developer-user-settings>
  = <functional-developer-user-settings-2-1>;


/// Library and service pack settings

define constant $local-settings = make(<functional-developer-local-settings>);

define variable *library-packs-override* = #"unknown";

define function raw-encoded-library-packs () => (encoded-packs :: <machine-word>)
  if (*library-packs-override* = #"unknown")
    let raw-override = environment-variable("FUNCTIONAL_DEVELOPER_LIBRARY_PACKS");
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

define function release-contains-console-tools?
    () => (tools? :: <boolean>)
  $local-settings.console-tools
end function release-contains-console-tools?;
