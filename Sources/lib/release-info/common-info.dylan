Module:    release-info-internals
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Release constants

define constant $release-full-copyright =
"This software contains confidential and trade secret information belonging\n"
"to Functional Objects, Inc.  It may not be copied for any reason other\n"
"than for archival and back-up purposes, or as set forth in the applicable\n"
"license agreement.\n"
"\n"
"This is commercial computer software.  Use, duplication or disclosure by\n"
"the United States Government is subject to the restrictions set forth in\n"
"FAR 52.227-14 Alt III or FAR 52.227-19, as applicable.  Use by agencies of\n"
"the Department of Defense is subject to Functional Objects' commercial\n"
"license agreement acccompanying the software, in accordance with DFAR\n"
"227.7202-1(a).  For purposes of the FAR, the Software shall be deemed\n"
"\"unpublished\" and licensed with disclosure prohibitions, rights reserved\n"
"under the copyright laws of the United States.  Functional Objects, Inc.,\n"
"86 Chandler St, Somerville, MA 02144.\n"
"\n"
// "Dylan is a trademark of Apple Computer Inc.\n" // bet you it's not!
"Functional Developer is a trademark of Functional Objects, Inc.\n"
"All marks are trademarks or registered trademarks of their respective\n"
"owners.\n";

define constant $release-support-address 
  = "dylan-support@functionalobjects.com";

define constant $release-web-address
  = "http://www.functionalobjects.com/products";

define constant $bug-report-template-filename = "bug-report.txt";
define constant $license-agreement-filename   = "License.txt";


/// Release info querying functions

define open generic release-product-name () => (name :: <string>);
define open generic release-trademarked-name () => (name :: <string>);
define open generic release-edition () => (edition :: <string>);
define open generic release-edition-type () => (edition :: <symbol>);
define open generic release-beta? () => (beta? :: <boolean>);
define open generic release-version () => (version :: <string>);
define open generic release-version-type () => (version :: <symbol>);
define open generic release-copyright () => (copyright :: <string>);

define sideways method release-name () => (name :: <string>)
  format-to-string("%s %s", release-trademarked-name(), release-edition())
end method release-name;

define function release-full-name
    () => (full-name :: <string>)
  format-to-string("%s %s",
		   release-name(),
		   release-version())
end function release-full-name;

define function release-full-copyright
    () => (full-copyright :: <string>)
  format-to-string("%s\n%s\n%s\n\n%s",
		   release-name(),
		   release-copyright(),
		   release-version(),
		   $release-full-copyright)
end function release-full-copyright;

define function release-support-address
    () => (address :: <string>)
  $release-support-address
end function release-support-address;

define function release-web-address
    () => (address :: <string>)
  $release-web-address
end function release-web-address;

define constant $internal-release-editions = #[#"internal", #"emulator"];

define function release-internal?
    () => (internal? :: <boolean>)
  member?(release-edition-type(), $internal-release-editions)
end function release-internal?;

define function release-contains-edition?
    (edition :: <symbol>) => (contains-edition? :: <boolean>)
  let edition-type = release-edition-type();
  let basic? = edition-type == #"basic";
  select (edition)
    #"basic"    => #t;
    #"enhanced" => ~basic?;
    #"internal" => release-internal?();
  end
end function release-contains-edition?;

/// At this writing, the Basic Edition is once again a free product ...
define function release-free-edition?
    () => (free? :: <boolean>)
  release-edition-type() = #"basic"
end function release-free-edition?;


/// Library pack information

define function release-library-packs
    (#key encoded-packs = release-encoded-library-packs())
 => (library-packs :: <sequence>)
  let packs = make(<stretchy-object-vector>);
  let pack = 1;
  while (~zero?(encoded-packs))
    if (%logbit?(0, encoded-packs))
      add!(packs, pack)
    end;
    encoded-packs := u%shift-right(encoded-packs, 1);
    pack := pack + 1;
  end;
  packs
end function release-library-packs;

define function release-required-library-packs
    () => (library-packs :: <sequence>)
  let encoded-packs = release-encoded-required-library-packs();
  release-library-packs(encoded-packs: encoded-packs)
end function release-required-library-packs;

define function release-optional-library-packs
    () => (library-packs :: <sequence>)
  let encoded-packs = release-encoded-optional-library-packs();
  release-library-packs(encoded-packs: encoded-packs)
end function release-optional-library-packs;

define method library-pack-name (pack :: <integer>) => (name :: <string>)
  let info = find-library-pack-info(pack);
  if (info)
    as(<string>, info.info-name)
  else
    format-to-string("Pack %d", pack)
  end
end method library-pack-name;

define method library-pack-name (pack :: <symbol>) => (name :: <string>)
  let info = find-library-pack-info(pack);
  if (info)
    as(<string>, info.info-name)
  else
    "None"
  end
end method library-pack-name;

define method library-pack-full-name (pack :: <integer>) => (name :: <string>)
  let info = find-library-pack-info(pack);
  if (info)
    info.info-title
  else
    format-to-string("Library Pack %d", pack)
  end
end method library-pack-full-name;

define method  library-pack-full-name (pack :: <symbol>) => (name :: <string>)
  let info = find-library-pack-info(pack);
  if (info)
    info.info-title
  else
    format-to-string("%s (Vaporware) Library Pack", pack)
  end
end method library-pack-full-name;

define method library-pack-number (pack :: <integer>) => (pack :: false-or(<integer>))
  pack > 0 & pack <= $maximum-library-packs
    & pack
end method library-pack-number;

define method library-pack-number (pack :: <symbol>) => (pack :: false-or(<integer>))
  let info = find-library-pack-info(pack);
  if (info)
    info.info-pack-number
  else
    #f
  end
end method library-pack-number;

define method library-pack-required? (pack :: <integer>) => (required? :: <boolean>)
  let info = find-library-pack-info(pack);
  if (info)
    info.info-required?
  else
    #f
  end
end method library-pack-required?;

define method library-pack-required? (pack :: <symbol>) => (required? :: <boolean>)
  let info = find-library-pack-info(pack);
  if (info)
    info.info-required?
  else
    #f
  end
end method library-pack-required?;

define method release-contains-library-pack? (pack :: <integer>) => (installed? :: <boolean>)
  let encoded-packs = release-encoded-library-packs();
  %logbit?(pack - 1, encoded-packs)
end method release-contains-library-pack?;

define method release-contains-library-pack? (pack :: <symbol>) => (installed? :: <boolean>)
  let pack :: false-or(<integer>) = library-pack-number(pack);
  pack
    & release-contains-library-pack?(pack)
end method release-contains-library-pack?;

define function release-service-pack-message
    () => (message :: false-or(<string>))
  let service-pack = release-service-pack();
  if (service-pack > 0)
    format-to-string("Service Pack %d", service-pack)
  end
end function release-service-pack-message;

define function release-full-version
    (version :: <string>) => (full-version :: <string>)
  let service-pack-message  = release-service-pack-message();
  let console-tools-message = if (release-contains-console-tools?()) "Console Tools" end;
  let strings
    = vector(version,
	     service-pack-message  & format-to-string(" [%s]", service-pack-message),
	     console-tools-message & format-to-string(" [%s]", console-tools-message));
  apply(concatenate-as, <string>, remove(strings, #f))
end function release-full-version;


/// Release disk layout information

define function release-directory
    () => (directory :: <directory-locator>)
  let filename = application-filename();
  unless (filename) error("application-filename returned #f") end;
  let locator = as(<file-locator>, filename);
  let bin-directory = locator-directory(locator);
  locator-directory(bin-directory)
end function release-directory;

define function release-subdirectory
    (subdirectory :: <pathname>,
     #key directory :: <directory-locator> = release-directory())
 => (subdirectory :: <directory-locator>)
  merge-locators(as(<directory-locator>, subdirectory), directory)
end function release-subdirectory;

define function release-file
    (subdirectory :: <pathname>,
     #key directory :: <directory-locator> = release-directory())
 => (subdirectory :: <file-locator>)
  merge-locators(as(<file-locator>, subdirectory), directory)
end function release-file;

define function release-examples-directory
    () => (directory :: <directory-locator>)
  release-subdirectory("Examples/")
end function release-examples-directory;

define function release-sources-directory
    () => (directory :: <directory-locator>)
  let user-directory
    = environment-variable("FUNCTIONAL_DEVELOPER_USER_SOURCES");
  if (user-directory)
    as(<directory-locator>, user-directory)
  else
    release-subdirectory("Sources/")
  end
end function release-sources-directory;

define function release-library-packs-directory
    () => (directory :: <directory-locator>)
  merge-locators(as(<directory-locator>, "Library-Packs/"),
		 release-sources-directory())
end function release-library-packs-directory;

define function release-templates-directory
    () => (directory :: <directory-locator>)
  release-subdirectory("Templates/")
end function release-templates-directory;

define function release-runtime-directory
    () => (directory :: <directory-locator>)
  release-subdirectory("Redistributable/")
end function release-runtime-directory;

define function release-source-templates-directory
    () => (directory :: <directory-locator>)
  release-subdirectory("Source/", 
		   directory: release-templates-directory())
end function release-source-templates-directory;

define function release-bug-report-template-location
    () => (location :: <file-locator>)
  release-file($bug-report-template-filename,
	       directory: release-templates-directory())
end function release-bug-report-template-location;

define function release-license-agreement-location
    () => (location :: <file-locator>)
  release-file($license-agreement-filename)
end function release-license-agreement-location;
