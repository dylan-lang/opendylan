Module:       system-internals
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $posix-separator           = '/';
define constant $posix-extension-separator = '.';


define sealed abstract class <posix-file-system-locator> (<file-system-locator>)
end class <posix-file-system-locator>;

define sealed method string-as-locator
    (class == <posix-file-system-locator>, string :: <string>)
 => (locator :: <posix-file-system-locator>)
  let pos = find-delimiter-from-end(string, $posix-separator);
  if (pos == string.size - 1)
    string-as-locator(<posix-directory-locator>, string)
  else
    string-as-locator(<posix-file-locator>, string)
  end
end method string-as-locator;


define sealed class <posix-directory-locator> 
    (<file-system-directory-locator>, <posix-file-system-locator>)
  sealed constant slot locator-relative? :: <boolean> = #f,
    init-keyword: relative?:;
  sealed constant slot locator-path :: <simple-object-vector>,
    required-init-keyword: path:;
end class <posix-directory-locator>;

define sealed method make
    (class == <posix-directory-locator>,
     #key server :: false-or(<server-locator>) = #f,
          path :: false-or(<sequence>) = #f,
          relative? :: <boolean> = #f,
          directory :: false-or(<posix-directory-locator>) = #f,
          name :: false-or(<string>))
 => (locator :: <posix-directory-locator>)
  if (server)
    locator-error("Cannot specify server for posix directory locator: %=",
		  server)
  end;
  let path
    = if (name | directory)
	concatenate(if (directory) directory.locator-path else #[] end,
		    if (name) vector(name) else #[] end)
      else
	path
      end;
  next-method(class,
	      path:      canonicalize-path(path),
	      relative?: relative?)
end method make;

define sealed method initialize
    (locator :: <posix-directory-locator>, #key server) => ()
  next-method();
end method initialize;

define method locator-server
    (locator :: <posix-directory-locator>) => (server == #f)
  #f
end method locator-server;

define sealed method locator-name
    (locator :: <posix-directory-locator>)
 => (name :: false-or(<string>))
  let path = locator.locator-path;
  unless (empty?(path))
    path[size(path) - 1]
  end
end method locator-name;

define sealed method \=
    (locator1 :: <posix-directory-locator>,
     locator2 :: <posix-directory-locator>)
 => (equal? :: <boolean>)
  locator1.locator-relative? = locator2.locator-relative?
    & locator1.locator-path.size = locator2.locator-path.size
    & every?(\=, locator1.locator-path, locator2.locator-path)
end method \=;

define sealed method string-as-locator
    (class == <posix-directory-locator>, string :: <string>)
 => (locator :: <posix-directory-locator>)
  let (path, relative?)
  = parse-path(string, test: curry(\==, $posix-separator));
  make(<posix-directory-locator>,
       path: path,
       relative?: relative?)
end method string-as-locator;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <posix-directory-locator>)
 => (string :: <string>)
  let separator = $posix-separator;
  path-to-string(locator.locator-path,
		 class: class,
		 separator: separator,
		 relative?: locator.locator-relative?)
end method locator-as-string;

define sealed method locator-test
    (locator :: <posix-directory-locator>) => (test :: <function>)
  \=
end method locator-test;

define method locator-might-have-links?
    (locator :: <posix-directory-locator>) => (links? == #t)
  #t
end method locator-might-have-links?;


define sealed class <posix-file-locator> 
    (<file-system-file-locator>, <posix-file-system-locator>)
  sealed constant slot locator-directory :: false-or(<posix-directory-locator>) = #f,
    init-keyword: directory:;
  sealed constant slot locator-base :: false-or(<string>) = #f,
    init-keyword: base:;
  sealed constant slot locator-extension :: false-or(<string>) = #f,
    init-keyword: extension:;
end class <posix-file-locator>;

define sealed method make
    (class == <posix-file-locator>,
     #key directory :: false-or(<posix-directory-locator>),
          base :: false-or(<string>),
          extension :: false-or(<string>),
          name :: false-or(<string>))
 => (locator :: <posix-file-locator>)
  let directory
    = unless (directory & current-directory-locator?(directory))
	directory
      end;
  let pos = name & find-delimiter-from-end(name, $posix-extension-separator);
  let base = base | if (pos) copy-sequence(name, end: pos) else name end;
  let extension = extension | if (pos) copy-sequence(name, start: pos + 1) end;
  if (~base)
    locator-error("Attemped to create a file locator without a base")
  end;
  next-method(class,
	      directory: directory,
	      base: base,
	      extension: extension)
end method make;

define sealed method locator-name
    (locator :: <posix-file-locator>)
 => (name :: false-or(<string>))
  let base = locator.locator-base;
  let extension = locator.locator-extension;
  if (extension)
    concatenate(base | "",
		delimiter-to-string($posix-extension-separator),
		extension)
  else
    base
  end
end method locator-name;

define sealed method \=
    (locator1 :: <posix-file-locator>,
     locator2 :: <posix-file-locator>)
 => (equal? :: <boolean>)
  locator1.locator-directory = locator2.locator-directory
    & locator1.locator-base = locator2.locator-base
    & locator1.locator-extension = locator2.locator-extension
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <posix-file-locator>)
 => (string :: <string>)
  let directory = locator.locator-directory;
  let name = locator.locator-name;
  if (directory)
    concatenate-as(class, as(<string>, directory), name)
  else
    name
  end
end method locator-as-string;

define sealed method string-as-locator
    (class == <posix-file-locator>, string :: <string>)
 => (locator :: <posix-file-locator>)
  let pos = find-delimiter-from-end(string, $posix-separator);
  let (directory, name)
    = if (pos)
	values(as(<posix-directory-locator>, 
		  copy-sequence(string, end: pos)),
	       copy-sequence(string, start: pos + 1))
      else
	values(#f, string)
      end;
  make(<posix-file-locator>,
       directory: directory,
       name: name)
end method string-as-locator;


/// Posix locator overrides

define method simplify-locator
    (locator :: <posix-directory-locator>)
 => (simplified-locator :: <posix-directory-locator>)
  // Posix locators can't safely be simplified because '..' has a complicated
  // meaning when dealing with links, so just return the original.
  locator
end method simplify-locator;
