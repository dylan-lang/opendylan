Module:    path-utilities
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Utilities for manipulating pathnames

define method filename-with-extension
    (name :: <string>, extension :: false-or(<string>))
 => (extended-locator :: <file-locator>)
  filename-with-extension(as(<file-locator>, name), extension)
end method filename-with-extension;

define method filename-with-extension
    (locator :: <file-locator>, extension :: false-or(<string>))
 => (extended-locator :: <file-locator>)
  if (extension & extension.empty?)
    locator
  else
    make(<file-locator>,
	 directory: locator.locator-directory,
	 base:      locator.locator-base,
	 extension: extension)
  end
end method filename-with-extension;

define method filename-with-extensions
    (name :: <string>, suffix :: <string>, extension :: false-or(<string>))
 => (extended-locator :: <file-locator>)
  filename-with-extension(concatenate(name, suffix), extension)
end method filename-with-extensions;

define method filename-with-extensions
    (file :: <file-locator>, suffix :: <string>, ext :: <string>) 
 => (extended-name :: <file-locator>)
  make(<file-locator>,
       directory: file.locator-directory,
       base:      concatenate(file.locator-base, suffix),
       extension: ext)
end method;

define method new-filename-extension
    (name :: <string>, extension :: <string>)
 => (new-locator :: <file-locator>)
  new-filename-extension(as(<file-locator>, name), extension)
end method new-filename-extension;

define method new-filename-extension
    (locator :: <file-locator>, extension :: <string>)
 => (new-locator :: <file-locator>)
  let new-base = locator.locator-base;
  assert(new-base, 
	 "Adding extension to filename with no base: %s",
	 locator);
  make(<file-locator>,
       base:      new-base,
       extension: extension)
end method new-filename-extension;

define method file-in-directory
    (directory :: <directory-locator>, file :: <string>,
     #key extension)
 => (file :: <file-locator>)
  if (extension)
    make(<file-locator>,
	 directory: directory,
	 base:      file,
	 extension: extension)
  else
    make(<file-locator>,
	 directory: directory,
	 name:      file)
  end
end method file-in-directory;

define method file-in-directory
    (directory :: <directory-locator>, locator :: <file-locator>,
     #key extension)
 => (file :: <file-locator>)
  file-in-directory(directory, locator.locator-name, extension: extension)
end method file-in-directory;

define method file-in-directory
    (directory == #f, file :: <string>, #key extension)
 => (file :: <file-locator>)
  file-in-directory(working-directory(), file, extension: extension)
end method file-in-directory;
