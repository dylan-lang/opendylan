Module:       system-internals
Synopsis:     Abstract modeling of locations
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $macintosh-separator = ':';
define constant $macintosh-extension-separator = '.';


define sealed abstract class <macintosh-server-locator> (<server-locator>)
end class <macintosh-server-locator>;


define sealed class <macintosh-volume-locator> (<macintosh-server-locator>)
  sealed constant slot locator-volume :: <byte-string>,
    required-init-keyword: volume:;
end class <macintosh-volume-locator>;

define sealed method make
    (class == <macintosh-volume-locator>,
     #key name :: false-or(<string>) = #f,
          volume :: false-or(<string>) = #f)
 => (locator :: <macintosh-volume-locator>)
  next-method(class, volume: volume | name)
end method make;

define sealed method locator-name
    (locator :: <macintosh-volume-locator>) => (name :: <string>)
  locator.locator-volume
end method locator-name;

define sealed method \=
    (locator1 :: <macintosh-volume-locator>,
     locator2 :: <macintosh-volume-locator>)
 => (equal? :: <boolean>)
  case-insensitive=(locator1.locator-volume, locator2.locator-volume)
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <macintosh-volume-locator>)
 => (string :: <string>)
  concatenate-as(class,
		 locator.locator-name,	
		 delimiter-to-string($macintosh-separator))
end method locator-as-string;


define sealed abstract class <macintosh-file-system-locator> (<file-system-locator>)
end class <macintosh-file-system-locator>;

define sealed method string-as-locator
    (class == <macintosh-file-system-locator>, string :: <string>)
 => (locator :: <macintosh-file-system-locator>)
  let pos = find-delimiter-from-end(string, $macintosh-separator);
  if (pos == string.size - 1)
    string-as-locator(<macintosh-directory-locator>, string)
  else
    string-as-locator(<macintosh-file-locator>, string)
  end
end method string-as-locator;


define sealed class <macintosh-directory-locator> 
    (<file-system-directory-locator>, <macintosh-file-system-locator>)
  sealed constant slot locator-server :: false-or(<macintosh-server-locator>) = #f,
    init-keyword: server:;
  sealed constant slot locator-relative? :: <boolean> = #f,
    init-keyword: relative?:;
  sealed constant slot locator-path :: <simple-object-vector>,
    required-init-keyword: path:;
end class <macintosh-directory-locator>;

define sealed method make
    (class == <macintosh-directory-locator>,
     #key server :: false-or(<macintosh-server-locator>) = #f,
          path :: false-or(<sequence>) = #f,
          relative? :: <boolean> = #f,
          directory :: false-or(<macintosh-directory-locator>) = #f,
          name :: false-or(<string>))
 => (locator :: <macintosh-directory-locator>)
  let path
    = if (name | directory)
	concatenate(if (directory) directory.locator-path else #[] end,
		    if (name) vector(name) else #[] end)
      elseif (path)
	as(<simple-object-vector>, path)
      else
	#[]
      end;
  next-method(class,
	      server:    server,
	      path:      path,
	      relative?: relative?)
end method make;

define sealed method locator-name
    (locator :: <macintosh-directory-locator>)
 => (name :: false-or(<string>))
  let path = locator.locator-path;
  unless (empty?(path))
    path[size(path) - 1]
  end
end method locator-name;

define sealed method \=
    (locator1 :: <macintosh-directory-locator>,
     locator2 :: <macintosh-directory-locator>)
 => (equal? :: <boolean>)
  locator1.locator-relative? = locator2.locator-relative?
    & locator1.locator-server = locator2.locator-server
    & locator1.locator-path.size = locator2.locator-path.size
    & every?(case-insensitive=, locator1.locator-path, locator2.locator-path)
end method \=;

define sealed method string-as-locator
    (class == <macintosh-directory-locator>, string :: <string>)
 => (locator :: <macintosh-directory-locator>)
  let relative? = string[0] = $macintosh-separator;
  let (server, start)
    = if (relative?)
	values(#f, 0)
      else
	let pos = find-delimiter(string, $macintosh-separator)
	            // If there's just a name, presume it's a volume name ...
	            | size(string);
	let volume = copy-sequence(string, end: pos);
	values(make(<macintosh-volume-locator>, volume: volume), pos)
      end;
  let path = macintosh-parse-path(string, relative?: relative?, start: start);
  make(<macintosh-directory-locator>,
       server:    server,
       path:      path,
       relative?: relative?)
end method string-as-locator;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <macintosh-directory-locator>)
 => (string :: <string>)
  let server = locator.locator-server;
  let path = locator.locator-path;
  let relative? = locator.locator-relative?;
  let directory-string
    = macintosh-path-to-string(locator.locator-path, locator.locator-relative?, class);
  if (server)
    concatenate-as(class,
		   as(class, server),
		   directory-string)
  else
    directory-string
  end
end method locator-as-string;

define sealed method locator-test
    (locator :: <macintosh-directory-locator>) => (test :: <function>)
  case-insensitive=
end method locator-test;

define method macintosh-parse-path
    (string :: <string>,
     #key relative? :: <boolean> = #f,
          start :: <integer> = 0,
          end: stop :: <integer> = string.size)
 => (path :: <simple-object-vector>)
  let path :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let old-position :: <integer> = start;
  let position :: <integer> = old-position;
  while (position < stop)
    let character = string[position];
    if (character = $macintosh-separator)
      case
	position = start =>
	  // Ensures that ":" satisfies the current-directory-locator? predicate...
	  if (relative?)
	    add!(path, #"self")
	  end;
	string[position - 1] = $macintosh-separator =>
	  add!(path, #"parent");
      end;
      if (old-position < position)
	add!(path, copy-sequence(string, start: old-position, end: position))
      end;
      old-position := position + 1;
    end;
    position := position + 1
  end;
  if (old-position < stop)
    add!(path, copy-sequence(string, start: old-position, end: stop))
  end;
  as(<simple-object-vector>, path)
end method macintosh-parse-path;

//---*** It is a pity that we need this for efficiency...
define sealed copy-down-method macintosh-parse-path
    (string :: <byte-string>,
     #key relative? :: <boolean> = #f,
          start :: <integer> = 0,
          end: stop :: <integer> = string.size)
 => (path :: <simple-object-vector>);

define function macintosh-path-to-string
    (path :: <sequence>, relative? :: <boolean>, class :: subclass(<string>))
 => (string :: <string>)
  let string-size :: <integer> = size(path) + if (relative?) 1 else 0 end;
  for (item in path)
    let item-size = select (item)
		      #"self"   => -1;          // Not even a separator will appear ...
		      #"parent" => 0;
		      otherwise => item.size;
		    end;
    string-size := string-size + item-size;
  end;
  let string = make(class, size: string-size);
  let pos :: <integer> = 0;
  if (relative?)
    string[pos] := $macintosh-separator;
    pos := pos + 1;
  end;
  for (item in path)
    select (item)
      #"self" => 
	#f;
      #"parent" => 
	string[pos] := $macintosh-separator;
	pos := pos + 1;
      otherwise => 
	for (character :: <character> in item)
	  string[pos] := character;
	  pos := pos + 1;
	end;
	string[pos] := $macintosh-separator;
	pos := pos + 1;
    end
  end;
  string
end function macintosh-path-to-string;


define sealed class <macintosh-file-locator> 
    (<file-system-file-locator>, <macintosh-file-system-locator>)
  sealed constant slot locator-directory :: false-or(<macintosh-directory-locator>) = #f,
    init-keyword: directory:;
  sealed constant slot locator-base :: false-or(<string>) = #f,
    init-keyword: base:;
  sealed constant slot locator-extension :: false-or(<string>) = #f,
    init-keyword: extension:;
end class <macintosh-file-locator>;

define sealed method make
    (class == <macintosh-file-locator>,
     #key directory :: false-or(<macintosh-directory-locator>) = #f,
          base :: false-or(<string>),
          extension :: false-or(<string>),
          name :: false-or(<string>))
 => (locator :: <macintosh-file-locator>)
  let directory
    = unless (directory & current-directory-locator?(directory))
	directory
      end;
  let pos = name & find-delimiter-from-end(name, $macintosh-extension-separator);
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
    (locator :: <macintosh-file-locator>)
 => (name :: false-or(<string>))
  let base = locator.locator-base;
  let extension = locator.locator-extension;
  if (extension)
    concatenate(base | "",
		delimiter-to-string($macintosh-extension-separator),
		extension)
  else
    base
  end
end method locator-name;

define sealed method \=
    (locator1 :: <macintosh-file-locator>,
     locator2 :: <macintosh-file-locator>)
 => (equal? :: <boolean>)
  locator1.locator-directory = locator2.locator-directory
    & case-insensitive=(locator1.locator-base, locator2.locator-base)
    & case-insensitive=(locator1.locator-extension, locator2.locator-extension)
end method \=;

define sealed method locator-as-string
    (class :: subclass(<string>), locator :: <macintosh-file-locator>)
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
    (class == <macintosh-file-locator>, string :: <string>)
 => (locator :: <macintosh-file-locator>)
  let pos = find-delimiter-from-end(string, $macintosh-separator);
  let (directory, name)
    = if (pos)
	values(as(<macintosh-directory-locator>, 
		  // Include trailing separator to properly handle #"parent"
		  // references that appear just before the filename ...
		  copy-sequence(string, end: pos + 1)),
	       copy-sequence(string, start: pos + 1))
      else
	values(#f, string)
      end;
  make(<macintosh-file-locator>,
       directory: directory,
       name: name)
end method string-as-locator;
