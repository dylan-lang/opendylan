Module:    release-info
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Release constants
define constant $release-full-copyright =
  "The software programs and libraries that make up Open Dylan\n"
  "are subject to the following license terms.\n"
  "\n";

define constant $release-support-address
  = "bugs@opendylan.org";

define constant $release-web-address
  = "http://www.opendylan.org/";

define constant $bug-report-template-filename = "bug-report.txt";
define constant $license-agreement-filename   = "License.txt";
define constant $help-filename                = "opendylan.chm";


/// Release constants
define constant $release-product-name     = "Open Dylan";
define constant $release-version          = "2012.1pre1";

define constant $release-copyright
  = "Copyright (c) 1997-2004, Functional Objects, Inc.\n"
    "Portions Copyright (c) 2004-2012, Dylan Hackers\n"
    "Portions Copyright (c) 2001-2002, Ravenbrook Ltd.";


/// Release info querying functions
define method release-product-name () => (name :: <string>)
  $release-product-name
end method release-product-name;

define method release-short-version () => (version :: <string>)
  $release-version
end method release-short-version;

define method release-version () => (version :: <string>)
  concatenate("Version ", $release-version)
end method release-version;

define method release-copyright () => (copyright :: <string>)
  $release-copyright
end method release-copyright;


/// Implementations
define function release-full-name
    () => (full-name :: <string>)
  format-to-string("%s %s",
		   release-product-name(),
		   release-version())
end function release-full-name;

define function release-full-copyright
    () => (full-copyright :: <string>)
  format-to-string("%s\n%s\n%s\n\n%s",
		   release-product-name(),
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

/// Library pack information

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

define method release-contains-library-pack? (pack :: <integer>) => (installed? :: <boolean>)
  let encoded-packs = release-encoded-library-packs();
  %logbit?(pack - 1, encoded-packs)
end method release-contains-library-pack?;

define method release-contains-library-pack? (pack :: <symbol>) => (installed? :: <boolean>)
  let pack :: false-or(<integer>) = library-pack-number(pack);
  pack
    & release-contains-library-pack?(pack)
end method release-contains-library-pack?;


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
    release-subdirectory("sources/")
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

define function release-help-location
    () => (location :: <file-locator>)
  release-file($help-filename,
               directory: release-subdirectory("Documentation/"))
end function release-help-location;

