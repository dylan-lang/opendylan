module: locator-internals
author: Tim McNerney
revised: 02-Feb-95 mf
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Posix Locators

define constant $posix-separator = '/';

define abstract class <posix-locator>
  (<locator-directory-slot>,
   <locator-derived-base-slot>,
   <locator-derived-type-slot>,
   <locator-derived-name-slot>,
   <physical-locator>) // must be last, I think
  end class;

define class <posix-file-locator> (<file-locator>,
				   <posix-locator>)
end class;

define class <posix-directory-locator> (<directory-locator>,
					<posix-locator>)
end class;

define method locator-case-sensitive?
    (locator :: <posix-locator>) => (sensitive? :: <boolean>)
  #t
end method locator-case-sensitive?;

define method locator-extension 
    (locator :: <posix-locator>) => (extension :: <locator-extension>)
  as(<string>, locator.locator-type | "")
end method;

/*
// START POSSIBLE BOGOSITY
define method match-name
    (pattern :: <locator>, candidate :: <locator>)
 => (result :: type-union(<string>, singleton(#"fail")))
  match (locator-name (pattern), locator-name (candidate))
end method;

define method instantiate-name 
    (pattern :: <locator>, match :: <locator-match>)
 => (result :: false-or(<string>)) // and more
  instantiate (locator-name (pattern), locator-name (match))
end method;
// END POSSIBLE BOGOSITY
*/

define method make
    (class == <posix-locator>,
     #key directory-path, directory = directory-path, base, type, prefix, name, extension)
 => (locator :: <posix-locator>)
  verify-locator-arguments
    (#f, #f, #f, directory, base, type, prefix, name, extension);
  ensure-no-redundancy (directory, base, type, prefix, name, extension);
  if (prefix)
    let prefix-locator = as (<posix-locator>, prefix); // inefficient!
    directory := prefix-locator.locator-directory;
  else
    directory := f-or-as (<posix-directory-path>, directory);
  end if;
  if (name)
    base := #"derived";
    type := #"derived";
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>, parse-posix-extension);
    name := #"derived";
  else
    type := f-or-as (<posix-type-path>, type);
    name := #"derived";
  end if;
  let subclass = if ((base | type) & name ~= "")
		   <posix-file-locator>
		 else
		   <posix-directory-locator>
		 end;
  make (subclass, directory: directory, name: name, base: base, type: type)
end method;

define method \= 
    (loc1 :: <posix-locator>, loc2 :: <posix-locator>)
 => (equal? :: <boolean>)
  next-method () 
    & locator-directory (loc1) = locator-directory (loc2)
    & if (derived-name? (loc1) & derived-name? (loc2))
	locator-base (loc1) = locator-base (loc2)
	  & locator-type (loc1) = locator-type (loc2)
      else
	locator-name (loc1) = locator-name (loc2)
      end if
end method;

define method as
    (class == <directory-locator>, locator :: <posix-file-locator>)
 => (locator :: <directory-locator>)
  // This doesn't deal with base == #f case
  make (<posix-directory-locator>,
	directory: down (parse-directory-element
			   (base-as-string (locator-base (locator))),
			 locator-directory (locator)))
end method;

define method as
    (class == <file-locator>, locator :: <posix-directory-locator>)
 => (locator :: <file-locator>)
  // This doesn't deal with root case
  make (<posix-file-locator>,
	directory: up (locator-directory (locator)),
	base: parse-base (directory-element-as-string
			    (last (locator-directory (locator)))))
end method;

// Merging

define method merge-locators
    (loc1 :: <posix-locator>, loc2 :: <locator>)
 => (result :: <posix-locator>)
  make(<posix-locator>,
       directory: merge-components(locator-directory (loc1),
				   locator-directory (loc2)),
       base: merge-components(locator-base(loc1), locator-base(loc2)),
       type: merge-components(locator-type(loc1), locator-type(loc2)))
end method;

define method abbreviate-locator 
    (locator :: <posix-locator>, default :: <locator>)
 => (result :: <posix-locator>)
  let base = locator-base(locator);
  let type = locator-type(locator);
  let directory = locator-directory(locator);
  let default-directory = locator-directory(default);
  let abbreviated-directory
    = abbreviate-component(directory, default-directory);
  //--- andrewa: added this kludge so that things work out okay when
  //--- the locators are identical directories.
  if (~abbreviated-directory & ~base)
    as(<posix-locator>, "./")
  else
    make(<posix-locator>,
	 directory: abbreviated-directory,
	 base: base, type: type)
  end
end method;

define method override-locator 
    (locator :: <posix-locator>,
     #key directory-path = unsupplied (),
	  directory = directory-path,
	  base = unsupplied (),
	  type = unsupplied (),
	  prefix = unsupplied (),
	  name = unsupplied (),
	  extension = unsupplied (),
     #all-keys)
 => (result :: <posix-locator>)
  ensure-no-redundancy (supplied? (directory), supplied? (base), supplied? (type),
			supplied? (prefix), supplied? (name), supplied? (extension));
  if (unsupplied? (name))
    if (supplied? (extension))
      type := extension & parse-extension (extension, <posix-type-path>,
					   parse-posix-extension);
    end;
    base := override-component (locator-base (locator), base, #f);
    type := override-component (locator-type (locator), type, <posix-type-path>);
  end;
  if (supplied? (prefix))
    if (supplied? (name))
      make (<posix-locator>, prefix: prefix, name: name)
    else
      make (<posix-locator>, prefix: prefix, base: base, type: type)
    end
  else
    directory := override-component (locator-directory (locator), directory,
				     <posix-directory-path>);
    if (supplied? (name))
      make (<posix-locator>, directory: directory, name: name)
    else
      make (<posix-locator>, directory: directory, base: base, type: type)
    end
  end
end method;

define method default-locator
    (locator :: <posix-locator>,
     #key directory-path, directory = directory-path, base, type,
          prefix, name, extension, 
     #all-keys)
 => (result :: <posix-locator>)
  ensure-no-redundancy (directory, base, type, prefix, name, extension);
  let defaults = (prefix | name)
    & as (<posix-locator>, concatenate (prefix | "", name | "")); // inefficient!
  if (prefix)
    directory := defaults.locator-directory;
  else
    directory := f-or-as (<posix-directory-path>, directory);
  end if;
  if (name)
    base := defaults.locator-base;
    type := defaults.locator-type
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>, parse-posix-extension);
  else
    type := f-or-as (<posix-type-path>, type);
  end if;
  make (<posix-locator>,
	directory: default-component (locator-directory (locator), directory),
	base: default-component (locator-base (locator), base),
	type: default-component (locator-type (locator), type))
end method;							        

// Matching

define method instantiate
    (pattern :: <posix-locator>, match :: <locator-match>)
 => (result :: <posix-locator>)
  make(<posix-locator>, directory: instantiate-directory(pattern, match),
                        base: instantiate-base(pattern, match),
                        type: instantiate-type(pattern, match))
end method;

define method match 
    (pattern :: <posix-locator>, candidate :: <posix-locator>)
 => (result :: type-union(<posix-match>, singleton(#"fail")))
  let directory-match = match-directory(pattern, candidate);
  let base-match = match-base(pattern, candidate);
  let type-match = match-type(pattern, candidate);
  if (matched?(directory-match) 
      & matched?(base-match) & matched?(type-match))
    make(<posix-match>, directory: directory-match,
	                base: base-match,
	                type: type-match)
  else
    #"fail"
  end if;
end method;

define class <posix-match> (<locator-directory-slot>,
			    <locator-base-slot>, // nothing
			    <locator-type-slot>, // fancy here
			    <locator-match>)
end class;

define method \=
    (loc1 :: <posix-match>, loc2 :: <posix-match>)
 => (equal? :: <boolean>)
  next-method() 
  & locator-directory (loc1) = locator-directory (loc2)
  & locator-base (loc1) = locator-base (loc2)
  & locator-type (loc1) = locator-type (loc2)
end method;

/*
define method parse-name 
    (locator-string, #key start: base-start = 0)
 => (base :: <string>, type :: <type-path>)
  let ext-start = position('.', locator-string, start: base-start + 1);
  if (ext-start == #f)
    values(parse-base(copy-sequence(locator-string, 
				    start: base-start)),
	   #f)
  else
    values(parse-base
	     (copy-sequence(locator-string, 
			    start: base-start,
			    end: ext-start | size(locator-string))),
	   parse-extension(locator-string, 
			   <posix-type-path>,
			   parse-posix-extension,
			   start: ext-start + 1))
  end if;
end method;
*/

define method as
    (class == <posix-locator>, locator-string :: <string>)
 => (locator :: <posix-locator>)
  // parse as posix locator-string: /dir.../name.ext (how about nested types?)
  let dir-end = position-from-end($posix-separator, locator-string);
  let (directory, base-start)
    = if (dir-end == #f)
	values(#f, 0)
      else
	values(parse-directory(locator-string, 
			       <posix-directory-path>, $posix-separator,
			       start: 0,
			       end: dir-end),
	       dir-end + 1)
      end if;
  if (base-start = size(locator-string))
    make(<posix-directory-locator>, directory: directory) // <posix-locator>
  else							  //  would suffice
    // +++ delay deriving base&type???  A: YES
    //let (base, type) = parse-name(locator-string, start: base-start);
    make(<posix-file-locator>, directory: directory, // <posix-locator>
	                       //base: base, //  would suffice
	                       //type: type
	                       base: #"derived",
	                       type: #"derived",
	                       name: copy-sequence(locator-string,
						   start: base-start))
  end if;
end method;

define method derived-name?
    (locator :: <posix-locator>) => (derived? :: <boolean>)
  derived?(raw-name(locator))
end method;

define method locator-prefix 
    (locator :: <posix-locator>) => (prefix :: <locator-prefix>)
  as(<string>, locator-directory(locator) | "")
end method;

// Unnecessary, because this is default behavior
/*
define method locator-suffix (locator :: <posix-locator>)
  ""                           // Put "~n~" sorts of extension thingies here?
end method;
*/
