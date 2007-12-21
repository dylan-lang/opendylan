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
"Original Code is Copyright 1995-2004 Functional Objects, Inc.\n"
"All rights reserved.\n"
"Portions Copyright 2004-2007 Dylan Hackers.\n"
"\n"
"The software programs and libraries that make up Open Dylan\n"
"are subject to the Functional Objects Library Public License Version\n"
"1.0 (the \"License\"); you may not use this software except in\n"
"compliance with the License. \n"
"\n"
"Software distributed under the License is distributed on an \"AS IS\"\n"
"basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See\n"
"the License for the specific language governing rights and limitations\n"
"under the License.  You may obtain a copy of the License at\n"
"http://www.functionalobjects.com/licenses/library-public-license-1.0.txt\n"
"\n"
"Alternatively, the software may be used under the terms of the GNU\n"
"Lesser General Public License, in which case the provisions of GNU\n"
"Lesser General Public License are applicable instead of those\n"
"above. If you wish to allow use of your version of this library only\n"
"under the terms of the GNU Lesser General Public License and not to\n"
"allow others to use your version of this library under the License,\n"
"indicate your decision by deleting the provisions above and replacing\n"
"them with the notice and other provisions required by the GNU Lesser\n"
"General Public License. If you do not delete the provisions\n"
"above, a recipient may use your version of this library under either\n"
"the License or the GNU Lesser General Public License.\n";

define constant $release-support-address 
  = "bugs@opendylan.org";

define constant $release-web-address
  = "http://www.opendylan.org/";

define constant $bug-report-template-filename = "bug-report.txt";
define constant $license-agreement-filename   = "License.txt";


/// Release constants
define constant $release-product-name     = "Open Dylan";
define constant $release-edition          = "Hacker Edition";
define constant $release-edition-type     = #"internal";
define constant $release-version          = "Version 1.0 [Beta 5]";
define constant $release-version-type     = #"1.0";
define constant $release-beta?            = #t;

define constant $release-copyright
  = "Copyright (c) 1997-2004, Functional Objects, Inc.\n"
    "Portions Copyright (c) 2004-2007, Dylan Hackers\n"
    "All rights reserved.";


/// Release info querying functions
define method release-product-name () => (name :: <string>)
  $release-product-name
end method release-product-name;

define method release-edition () => (edition :: <string>)
  $release-edition
end method release-edition;

define method release-edition-type () => (edition :: <symbol>)
  $release-edition-type
end method release-edition-type;

define method release-version () => (version :: <string>)
  release-full-version($release-version)
end method release-version;

define method release-version-type () => (version :: <symbol>)
  $release-version-type
end method release-version-type;

define method release-beta? () => (beta? :: <boolean>)
  $release-beta?
end method release-beta?;

define method release-copyright () => (copyright :: <string>)
  $release-copyright
end method release-copyright;


/// Implementations
define sideways method release-name () => (name :: <string>)
  format-to-string("%s", release-edition())
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

define function release-free-edition?
    () => (free? :: <boolean>)
  #t
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
    = environment-variable("OPEN_DYLAN_USER_SOURCES");
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
