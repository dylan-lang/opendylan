Module:       system-internals
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $microsoft-separators   = #['\\', '/'];
define constant $extension-separator    = '.';
define constant $volume-separator       = ':';
define constant $unc-prefix             = "\\\\";
define constant $alternative-unc-prefix = "//";


define sealed abstract class <microsoft-server-locator> (<server-locator>)
end class <microsoft-server-locator>;


define sealed class <microsoft-unc-locator> (<microsoft-server-locator>)
  sealed constant slot locator-host :: <string>,
    required-init-keyword: host:;
end class <microsoft-unc-locator>;

define sealed method make
    (class == <microsoft-unc-locator>,
     #key name :: false-or(<string>) = #f,
          host :: false-or(<string>) = #f)
 => (locator :: <microsoft-unc-locator>)
  next-method(class, host: host | name)
end method make;

define sealed method locator-name
    (locator :: <microsoft-unc-locator>) => (name :: <string>)
  locator.locator-host
end method locator-name;

define sealed method \=
    (locator1 :: <microsoft-unc-locator>,
     locator2 :: <microsoft-unc-locator>)
 => (equal? :: <boolean>)
  case-insensitive=(locator1.locator-host, locator2.locator-host)
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <microsoft-unc-locator>)
 => (string :: <string>)
  concatenate-as(class, $unc-prefix, locator.locator-host)
end method locator-as-string;


define sealed class <microsoft-volume-locator> (<microsoft-server-locator>)
  sealed constant slot locator-drive :: <character>,
    required-init-keyword: drive:;
end class <microsoft-volume-locator>;

define sealed method make
    (class == <microsoft-volume-locator>,
     #key name :: false-or(<string>) = #f,
          volume :: false-or(<string>) = name,
          drive :: false-or(<character>) = #f)
 => (locator :: <microsoft-volume-locator>)
  if (volume)
    unless (volume.size == 1)
      locator-error("Invalid drive specification %=", volume)
    end;
    next-method(class, drive: volume[0])
  else
    next-method()
  end
end method make;

define sealed method locator-volume
    (locator :: <microsoft-volume-locator>) => (volume :: <string>)
  make(<byte-string>, size: 1, fill: locator.locator-drive)
end method locator-volume;

define sealed method locator-name
    (locator :: <microsoft-volume-locator>) => (name :: <string>)
  locator.locator-volume
end method locator-name;

define sealed method \=
    (locator1 :: <microsoft-volume-locator>,
     locator2 :: <microsoft-volume-locator>)
 => (equal? :: <boolean>)
  case-insensitive=(locator1.locator-drive, locator2.locator-drive)
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <microsoft-volume-locator>)
 => (string :: <string>)
  concatenate-as(class,
		 make(<byte-string>, size: 1, fill: locator.locator-drive),
		 delimiter-to-string($volume-separator))
end method locator-as-string;


define sealed abstract class <microsoft-file-system-locator> (<file-system-locator>)
end class <microsoft-file-system-locator>;

define sealed method string-as-locator
    (class == <microsoft-file-system-locator>, string :: <string>)
 => (locator :: <microsoft-file-system-locator>)
  let pos = find-delimiters-from-end(string, $microsoft-separators);
  if (pos == string.size - 1)
    string-as-locator(<microsoft-directory-locator>, string)
  else
    string-as-locator(<microsoft-file-locator>, string)
  end
end method string-as-locator;


define sealed class <microsoft-directory-locator> 
    (<file-system-directory-locator>, <microsoft-file-system-locator>)
  sealed constant slot locator-server :: false-or(<microsoft-server-locator>) = #f,
    init-keyword: server:;
  sealed constant slot locator-relative? :: <boolean> = #f,
    init-keyword: relative?:;
  sealed constant slot locator-path :: <simple-object-vector>,
    required-init-keyword: path:;
end class <microsoft-directory-locator>;

define sealed method make
    (class == <microsoft-directory-locator>,
     #key server :: false-or(<microsoft-server-locator>) = #f,
          path :: false-or(<sequence>) = #f,
          relative? :: <boolean> = #f,
          directory :: false-or(<microsoft-directory-locator>) = #f,
          name :: false-or(<string>))
 => (locator :: <microsoft-directory-locator>)
  let path
    = if (name | directory)
	concatenate(if (directory) directory.locator-path else #[] end,
		    if (name) vector(name) else #[] end)
      else
	path
      end;
  next-method(class,
	      server:    server,
	      path:      canonicalize-path(path),
	      relative?: relative?)
end method make;

define sealed method locator-name
    (locator :: <microsoft-directory-locator>)
 => (name :: false-or(<string>))
  let path = locator.locator-path;
  unless (empty?(path))
    path[size(path) - 1]
  end
end method locator-name;

define sealed method \=
    (locator1 :: <microsoft-directory-locator>,
     locator2 :: <microsoft-directory-locator>)
 => (equal? :: <boolean>)
  locator1.locator-relative? = locator2.locator-relative?
    & locator1.locator-server = locator2.locator-server
    & locator1.locator-path.size = locator2.locator-path.size
    & every?(case-insensitive=, locator1.locator-path, locator2.locator-path)
end method \=;

define sealed method string-as-locator
    (class == <microsoft-directory-locator>, string :: <string>)
 => (locator :: <microsoft-directory-locator>)
  let unc?
    = prefix-equal?(string, $unc-prefix)
        | prefix-equal?(string, $alternative-unc-prefix);
  let volume?
    = ~unc? & string.size > 1 & string[1] == $volume-separator;
  let (server, next-pos)
    = case
	unc? =>
	  let start = $unc-prefix.size;
	  let pos
	    = find-delimiters(string, $microsoft-separators, start: start);
	  if (pos)
	    let host = copy-sequence(string, start: start, end: pos);
	    values(make(<microsoft-unc-locator>, host: host), pos)
	  else
	    locator-error("Invalid directory %=", string)
	  end;
	volume? =>
	  values(make(<microsoft-volume-locator>, drive: string[0]), 2);
	otherwise =>
	  values(#f, 0);
      end;
  let (path, relative?)
    = parse-path(string, 
		 start: next-pos,
		 test: rcurry(member?, $microsoft-separators));
  make(<microsoft-directory-locator>,
       server:    server,
       path:      path,
       relative?: relative?)
end method string-as-locator;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <microsoft-directory-locator>)
 => (string :: <string>)
  let server = locator.locator-server;
  let directory-string
    = path-to-string(locator.locator-path,
		     class:     class,
		     separator: $microsoft-separators[0],
		     relative?: locator.locator-relative?);
  if (server)
    concatenate-as(class,
		   as(class, server),
		   directory-string)
  else
    directory-string
  end
end method locator-as-string;

define sealed method locator-test
    (locator :: <microsoft-directory-locator>) => (test :: <function>)
  case-insensitive=
end method locator-test;


define sealed class <microsoft-file-locator> 
    (<file-system-file-locator>, <microsoft-file-system-locator>)
  sealed constant slot locator-directory :: false-or(<microsoft-directory-locator>) = #f,
    init-keyword: directory:;
  sealed constant slot locator-base :: false-or(<string>) = #f,
    init-keyword: base:;
  sealed constant slot locator-extension :: false-or(<string>) = #f,
    init-keyword: extension:;
end class <microsoft-file-locator>;

define sealed method make
    (class == <microsoft-file-locator>,
     #key directory :: false-or(<microsoft-directory-locator>) = #f,
          base :: false-or(<string>),
          extension :: false-or(<string>),
          name :: false-or(<string>))
 => (locator :: <microsoft-file-locator>)
  let directory
    = unless (directory & current-directory-locator?(directory))
	directory
      end;
  let pos = name & find-delimiter-from-end(name, $extension-separator);
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
    (locator :: <microsoft-file-locator>)
 => (name :: false-or(<string>))
  let base = locator.locator-base;
  let extension = locator.locator-extension;
  if (extension)
    concatenate(base | "",
		delimiter-to-string($extension-separator),
		extension)
  else
    base
  end
end method locator-name;

define sealed method \=
    (locator1 :: <microsoft-file-locator>,
     locator2 :: <microsoft-file-locator>)
 => (equal? :: <boolean>)
  locator1.locator-directory = locator2.locator-directory
    & case-insensitive=(locator1.locator-base, locator2.locator-base)
    & case-insensitive=(locator1.locator-extension, locator2.locator-extension)
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <microsoft-file-locator>)
 => (string :: <string>)
  let directory = locator.locator-directory;
  let name = locator.locator-name;
  if (directory)
    concatenate-as(class, as(<string>, directory), name)
  else
    as(class, name)
  end
end method locator-as-string;

define sealed method string-as-locator
    (class == <microsoft-file-locator>, string :: <string>)
 => (locator :: <microsoft-file-locator>)
  let pos = find-delimiters-from-end(string, $microsoft-separators);
  let (directory, name)
    = if (pos)
	values(as(<microsoft-directory-locator>, 
		  copy-sequence(string, end: pos + 1)),
	       copy-sequence(string, start: pos + 1))
      else
	values(#f, string)
      end;
  make(<microsoft-file-locator>,
       directory: directory,
       name: name)
end method string-as-locator;
