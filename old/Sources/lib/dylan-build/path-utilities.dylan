Module:    path-utilities
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Utilities for manipulating pathnames
// We never convert a path to a directory automatically
// call ensure-directory-name if necessary
// all calls (except filename-without-extension which may be removed) return 
// <locator>s. client has to convert to string if necessary.

define method ensure-directory-name (directory :: <byte-string>)
 => (path :: <byte-string>);
  if (empty?(directory) | last(directory) = $pathname-separator)
    directory
  else
    concatenate(directory, $pathname-separator-as-string)
  end if;
end method;

define method ensure-directory-name (directory :: <locator>)
 => (path :: <locator>);
  let directory = as(<string>, directory);
  as(<locator>, ensure-directory-name(directory))
end method;

define function subdirectory-locator (directory :: <pathname>, 
				      sub-directory-names :: <sequence>)
 => (path :: <locator>);
  let locator = as(<locator>, directory);
  let dir = locator-directory(locator);
  let path
    = make(object-class(dir),
           elements: concatenate(dir.path-elements, sub-directory-names));
  as(<locator>, override-locator(locator, directory: path))
end function;

define function ensure-directory-exists 
    (locator :: <pathname>, directory-name :: <string>)
 => (directory :: <locator>);
  let file-locator = subdirectory-locator(locator, directory-name);
  ensure-directories-exist(file-locator);
  file-locator
end function;

define method filename-with-extension(file :: <pathname>, ext :: <string>) 
 => (extended-name :: <string>)
  as(<string>, override-locator(as(<locator>, file), extension: ext))
end method;

define method filename-without-extension(name :: <pathname>)
 => (base-name :: <string>)
  locator-base(as(<locator>, name))
end method;

define function parent-directory(path :: <pathname>)
 => (path :: <locator>);
  let locator = as(<locator>, path);
  let dir = locator-directory(locator);
  let path = path-elements(dir);
  let new-path = make(object-class(dir),
		      elements: copy-sequence(path, end: size(path) - 1));
  override-locator(locator, directory: new-path)
end;
