module: locator-internals
author: Tim McNerney
revised: 13-Feb-96 mf
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $abstract-separator = '/';

define abstract class <locator-abstract-host-slot> (<locator-host-slot>)
  // required keyword host:; // Not yet implemented
end class;

define abstract class <abstract-locator>
  (<locator-abstract-host-slot>,
   <locator-directory-slot>,
   <locator-derived-base-slot>,
   <locator-derived-type-slot>,
   <locator-derived-name-slot>,
   <locator-version-slot>,
   <locator>) // must be last, I think
end class;

define method locator-case-sensitive?
    (locator :: <abstract-locator>) => (sensitive? :: <boolean>)
  #t
end method locator-case-sensitive?;

define method locator-scheme 
    (locator :: <abstract-locator>) => (scheme :: <locator-scheme>)
  #"abstract"
end method;

define method \= 
    (loc1 :: <abstract-locator>, loc2 :: <abstract-locator>)
 => (equal? :: <boolean>)
  next-method() 
  & locator-host (loc1) = locator-host (loc2)
  & locator-directory (loc1) = locator-directory (loc2)
  & locator-base (loc1) = locator-base (loc2)
  & locator-type (loc1) = locator-type (loc2)
  & locator-version (loc1) = locator-version (loc2)
end method;

define method make 
    (class == <abstract-locator>,
     #key host, directory-path, directory = directory-path, base, type, version,
          prefix, name, extension)
 => (locator :: <abstract-locator>)
  verify-locator-arguments
    (host, #f, #f, directory, base, type, prefix, name, extension);
  if (prefix)
    let prefix-locator = as (<abstract-locator>, prefix); // inefficient!
    host := prefix-locator.locator-host;
    directory := prefix-locator.locator-directory;
  else
    host := f-or-as (<host-path>, host);
    directory := f-or-as (<abstract-directory-path>, directory);
  end if;
  if (name)
    base := #"derived";
    type := #"derived";
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>, parse-abstract-extension);
    name := #"derived";
  else
    type := f-or-as (<posix-type-path>, type);
    name := #"derived";
  end if;
  let subclass = if ((base | type | version) & name ~= "")
		   <abstract-file-locator>
		 else
		   <abstract-directory-locator>
		 end;
  make (subclass, host: host, directory: directory, name: name,
	base: base, type: type, version: version)
end method;

define class <abstract-file-locator> (<file-locator>,
				      <abstract-locator>)
end class;

define class <abstract-directory-locator> (<directory-locator>,
					   <abstract-locator>)
end class;

define method as
    (class == <directory-locator>, locator :: <abstract-file-locator>)
 => (locator :: <directory-locator>)
  // This doesn't deal with base == #f case
  make (<abstract-directory-locator>,
	host: locator-host (locator),
	directory: down (parse-directory-element
			   (base-as-string (locator-base (locator))),
			 locator-directory (locator)))
end method;

define method as
    (class == <file-locator>, locator :: <abstract-directory-locator>)
 => (locator :: <file-locator>)
  // This doesn't deal with root case
  make (<abstract-file-locator>,
	host: locator-host (locator),
	directory: up (locator-directory (locator)),
	base: parse-base (directory-element-as-string
			    (last (locator-directory (locator)))))
end method;

// Merging

define method merge-locators
    (loc1 :: <abstract-locator>, loc2 :: <locator>)
 => (result :: <abstract-locator>)
  make (<abstract-locator>,
	host: merge-components (locator-host (loc1), locator-host (loc2)),
	directory: merge-components (locator-directory (loc1),
				     locator-directory (loc2)),
	base: merge-components (locator-base (loc1), locator-base (loc2)),
	type: merge-components (locator-type (loc1), locator-type (loc2)),
	version: merge-components (locator-version (loc1),
				   locator-version (loc2))) //Precludes
end method;						   // version paths?

define method abbreviate-locator 
    (locator :: <abstract-locator>, default :: <locator>)
 => (result :: <abstract-locator>)
  make (<abstract-locator>,
	host: abbreviate-component (locator-host (locator), locator-host (default)),
	directory: abbreviate-component (locator-directory (locator),
					 locator-directory (default)),
	base: locator-base (locator), // always keep?
	type: abbreviate-component (locator-type (locator), locator-type (default)),
	version: locator-version (locator)) // always keep?
					   // (probably not)
end method;

define method override-locator
    (locator :: <abstract-locator>,
     #key host = unsupplied(),
	  directory-path = unsupplied(),
	  directory = directory-path,
	  base = unsupplied(),
	  type = unsupplied(),
	  version = unsupplied(),
	  prefix = unsupplied(),
	  name = unsupplied(),
	  extension = unsupplied(),
     #all-keys)
 => (result :: <abstract-locator>)
  ensure-no-redundancy((supplied? (host) | supplied? (directory)),
		       supplied? (base), supplied? (type), supplied? (prefix),
		       supplied? (name), supplied? (extension));
  version := override-component(locator-version(locator), //Precludes
				version, #f);	          // version paths?
  if (unsupplied? (name))
    if (supplied? (extension))
      type := extension & parse-extension (extension, <posix-type-path>,
					   parse-abstract-extension);
    end;
    base := override-component (locator-base (locator), base, #f);
    type := override-component (locator-type (locator), type, <posix-type-path>);
  end;
  if (supplied? (prefix))
    if (supplied? (name))
      make (<abstract-locator>, prefix: prefix, name: name, version: version)
    else
      make (<abstract-locator>, prefix: prefix, base: base, type: type,
	    version: version)
    end
  else
    host := override-component (locator-host (locator), host, <host-path>);
    directory := override-component (locator-directory (locator), directory,
				     <abstract-directory-path>);
    if (supplied? (name))
      make (<abstract-locator>, host: host, directory: directory,
	    name: name, version: version)
    else
      make (<abstract-locator>, host: host, directory: directory,
	    base: base, type: type, version: version)
    end
  end
end method;

define method default-locator
    (locator :: <abstract-locator>,
     #key host, directory-path, directory = directory-path, base, type, version,
          prefix, name, extension, #all-keys)
 => (result :: <abstract-locator>)
  ensure-no-redundancy ((host | directory), base, type, prefix, name, extension);
  let defaults = (prefix | name)
    & as (<abstract-locator>, concatenate (prefix | "", name | "")); // inefficient!
  if (prefix)
    host := defaults.locator-host;
    directory := defaults.locator-directory;
  else
    host := f-or-as (<host-path>, host);
    directory := f-or-as (<abstract-directory-path>, directory);
  end if;
  if (name)
    base := defaults.locator-base;
    type := defaults.locator-type
  elseif (extension)
    type := parse-extension (extension, <posix-type-path>, parse-abstract-extension);
  else
    type := f-or-as (<posix-type-path>, type);
  end if;
  make (<abstract-locator>,
	host: default-component (locator-host (locator), host),
	directory: default-component (locator-directory (locator), directory),
	base: default-component (locator-base (locator), base),
	type: default-component (locator-type (locator), type),
	version: default-component (locator-version (locator), //Precludes
				    version))		      // version paths
end method;							        

//

define class <abstract-match>
  (<locator-abstract-host-slot>,
   <locator-directory-slot>,
   <locator-base-slot>,
   <locator-type-slot>,
   <locator-version-slot>,
   <locator-match>)
end class;

define method \= 
    (loc1 :: <abstract-match>, loc2 :: <abstract-match>)
 => (equal? :: <boolean>)
  next-method() 
  & locator-host (loc1) =
 locator-host (loc2)
  & locator-directory (loc1) = locator-directory (loc2)
  & locator-base (loc1) = locator-base (loc2)
  & locator-type (loc1) = locator-type (loc2)
  & locator-version (loc1) = locator-version (loc2)
end method;

define method instantiate
    (pattern :: <abstract-locator>, match :: <locator-match>)
 => (result :: <abstract-locator>)
  make(<abstract-locator>,   
       host: instantiate-host(pattern, match),
       directory: instantiate-directory(pattern, match),
       base: instantiate-base(pattern, match),
       type: instantiate-type(pattern, match),
       version: instantiate-version(pattern, match))
end method;

define method match
    (pattern :: <abstract-locator>, candidate :: <abstract-locator>)
 => (result :: fail-or(<abstract-match>))
  let host-match = match-host(pattern, candidate);
  let directory-match = match-directory(pattern, candidate);
  let base-match = match-base(pattern, candidate);
  let type-match = match-type(pattern, candidate);
  let version-match = match-version(pattern, candidate);
  if (matched?(host-match) & matched?(directory-match) 
      & matched?(base-match) & matched?(type-match)
      & matched?(version-match))
    make(<abstract-match>, host: host-match, 
                           directory: directory-match,
	                   base: base-match, 
                           type: type-match,
	                   version: version-match)
  else
    #"fail"
  end if;
end method;

	       
define method grok-abstract-host 
    (locator-string :: <string>)
 => (host, dir-start, host-only? :: <boolean>)
  let prefix? = looking-at?("abstract:", locator-string);
  let first-slash = position($abstract-separator, locator-string);
  let second-slash = first-slash & position($abstract-separator, locator-string, start: first-slash + 1);
  let third-slash = second-slash & position($abstract-separator, locator-string, start: second-slash + 1);
  if (first-slash & second-slash & second-slash == first-slash + 1)
    if (third-slash)
      values(parse-host(locator-string, 
			start: second-slash + 1,
			end: third-slash),
	     third-slash,
	     #f)
    else
      values(parse-host(locator-string, 
			start: second-slash + 1),
	     #f,
	     #t)
    end if
  else // no host case
    values(#f, 
	   if (prefix?)
	     size("abstract:") else 0
	     end if,
	   #f)
  end if;
end method;

// Old fascist implementation
  /*
  if (~looking-at?("abstract://", locator-string))
    locator-error("AS <ABSTRACT-LOCATOR>: Not a valid abstract locator string");
  end if;
  let dir-start = position($abstract-separator, locator-string, start: size("abstract://"));
  if (dir-start == #f)
    locator-error("AS <ABSTRACT-LOCATOR>: No directory start");
  end if;
  */

define method as
    (class == <abstract-locator>, locator-string :: <string>)
 => (locator :: <abstract-locator>)
  // Parse as abstract locator:  abstract://host/dir.../name.ext;version
  let locator-string = as-lowercase!(trim-whitespace(locator-string));
  let (host, dir-start, host-only?) = grok-abstract-host(locator-string);
  if (host-only?)
    make(<abstract-directory-locator>, host: host)
  else
    let dir-end = position-from-end($abstract-separator, locator-string);
    let (directory, base-start)
      = if (~dir-end | dir-end < dir-start) // ever happen?
	  values(#f, dir-start)
	elseif (dir-end = dir-start) // same slash
	  //#f
	  values(make(<abstract-directory-path>, elements: #(), relative-path?: #f),
		 dir-end + 1)
	else
	  values(parse-directory(locator-string, 
				 <abstract-directory-path>, $abstract-separator,
				 start: dir-start,
				 end: dir-end),
		 dir-end + 1)
	end if;
    if (dir-end & dir-end + 1 = size(locator-string))
      make (<abstract-directory-locator>, host: host, directory: directory)
    else
      //let base-start = dir-end + 1;
      let ext-start = position ('.', locator-string, start: base-start + 1);
      let ver-start = position (';', locator-string, start: base-start);
      let base = parse-base (copy-sequence (locator-string,
					    start: base-start,
					    end: ext-start
					      | ver-start
					      | size (locator-string)));
      let type = if (ext-start)
		   parse-extension (locator-string,
				    <posix-type-path>,
				    parse-abstract-extension,
				    start: ext-start + 1,
				    end: ver-start | size(locator-string))
		 else
		   unspecified-or-empty-path (<posix-type-path>)
		 end if;
      let version = ver-start & parse-version (copy-sequence (locator-string, 
							      start: ver-start + 1));
      make (<abstract-file-locator>, host: host, directory: directory,
	    base: base, type: type, version: version);
    end if;
  end if;
end method;

define method locator-prefix 
    (locator :: <abstract-locator>) => (prefix :: <locator-prefix>)
  let host = locator-host (locator);
  if (host == #f)
    locator-error (<locator-print-error>,
		   "AS <STRING>: Cannot represent abstract locator as a string: no host")
  else
    concatenate("abstract://",
                as(<string>, host),
	        as(<string>, locator-directory(locator) | "/"))
  end if;
end method;

define method locator-name 
    (locator :: <abstract-locator>) => (name :: <locator-name>)
  let raw-name = raw-name(locator);
  if (derived?(raw-name))
    let derived-name = concatenate (base-as-string (locator-base (locator)),
				    as(<string>, locator-type (locator) | ""));
    raw-name(locator) := derived-name;
    derived-name
  else
    raw-name
  end if;
end method;

define method locator-suffix 
    (locator :: <abstract-locator>) => (suffix :: <string>)
  version-as-string(locator-version (locator))
end method;

// Abstract hosts

define class <abstract-host> (<object>)
  constant slot locator-host :: <host-path>, init-keyword: host:;
  constant slot translations :: <collection>,
    init-keyword: translations:;	// a collection of <mapping>s
end class;

// mappings

define class <mapping> (<object>)
  slot from :: <abstract-locator>, init-keyword: from:;
  constant slot to :: <physical-locator>, init-keyword: to:;
end class;

define method as
    (class == <mapping>, sequence :: <sequence>)
 => (mapping :: <mapping>)
  make(<mapping>,
       from: as(<abstract-locator>, sequence.first),
       to: as(<physical-locator>, sequence.second))
end;

define method as 
    (class == <mapping>, string :: <string>)
 => (mapping :: <mapping>)
  as(class, list("/**/*.*;*", string))
end;

define method make-abstract-host-table ()
  make (<equal-table>)
end method;

define variable *abstract-host-table* :: <mutable-explicit-key-collection>
  = make-abstract-host-table();

define method clear-abstract-host-table
    (#key host-table :: <mutable-explicit-key-collection>
            = *abstract-host-table*)
 => ()
  remove-all-keys!(host-table)
end method;

define method add-abstract-host
    (host, translations :: <collection>,
     #key host-table = *abstract-host-table*)
 => (host :: <abstract-host>)
  let host-path = as (<host-path>, host);
  local method as-mapping (template)
	  let map = as (<mapping>, template);
	  map.from := default-locator (map.from, host: host-path);
	  if (map.from.locator-directory.relative-path?)
	    map.from.locator-directory.relative-path? := #f;
	  end;
	  map
	end;
  let mappings = map (as-mapping, translations);

  host-table [host-path] := make (<abstract-host>,
				  host: host-path,
				  translations: mappings);
end method;

define method add-abstract-host
    (host, translations :: <string>,
     #key host-table = *abstract-host-table*)
 => ()
  add-abstract-host (host, list (translations), host-table: host-table);
end;

define method remove-abstract-host
    (host, #key host-table = *abstract-host-table*) => ()
  remove-key!(host-table, as(<host-path>, host));
end method;

define method as
    (class == <abstract-host>, host-path :: <host-path>)
 => (host :: <abstract-host>)
  element(*abstract-host-table*, host-path, default: #f)
    | locator-error(<locator-translation-error>,
		    "AS <abstract-host>: no abstract host translation for %=",
		    host-path)
end method;

// translations

// general

define method as 
    (class == <physical-locator>, locator :: <abstract-locator>)
 => (locator :: <physical-locator>)
  block (return)
    for (mapping in translations (as (<abstract-host>, locator.locator-host)))
      let locator-match = match (mapping.from, locator);
      if (locator-match.matched?)
	return (instantiate (mapping.to, locator-match));
      end;
    end;
    locator-error (<locator-translation-error>,
		   "AS <physical-locator>:  No translation found");
  end block;
end method;


define method as
    (class == <abstract-locator>, locator :: <physical-locator>)
 => (locator :: <abstract-locator>)
  block (return)
    for (abstract-host in *abstract-host-table*)
      for (mapping in translations (abstract-host))
	let locator-match = match (mapping.to, locator);
	if (locator-match.matched?)
	  return (instantiate (mapping.from, locator-match));
	end;
      end;
    end;
    locator-error (<locator-translation-error>,
		   "AS <abstract-locator>:  No back-translation found")
  end block;
end method;

// degenerate cases

define sideways method as
    (class == <physical-locator>, locator-string :: <string>)
 => (locator :: <physical-locator>)
  as(class, as(<locator>, locator-string))
end method;

// Don't even _think_ of putting this back in!
// (unless you want to have fun debugging another infinite loop)
/*
  define method as (class == <abstract-locator>, locator-string :: <string>) => (value)
  as(class, as(<locator>, locator-string))
end method;
*/

define sideways method as
    (class == <physical-locator>, locator :: <physical-locator>)
 => (value :: <physical-locator>)
  locator
end method;

define method as
    (class == <abstract-locator>, locator :: <abstract-locator>)
 => (locator :: <abstract-locator>)
  locator
end method;
