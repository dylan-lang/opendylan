module: locator-internals
author: Tim McNerney
revised: 13-Feb-95 mf
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Microsoft locators

define constant $microsoft-separators   = #['\\', '/'];
define constant $extension-separator    = '.';
define constant $volume-separator       = ':';
define constant $unc-prefix             = "\\\\";
define constant $alternative-unc-prefix = "//";

define abstract class <microsoft-locator> (<locator-directory-slot>,
					   <locator-derived-base-slot>,
					   <locator-derived-type-slot>,
					   <locator-derived-name-slot>,
					   <physical-locator>)
end class;

define method locator-case-sensitive?
    (locator :: <microsoft-locator>) => (sensitive? :: <boolean>)
  #f
end method locator-case-sensitive?;

define method \=
    (loc1 :: <microsoft-locator>, loc2 :: <microsoft-locator>)
 => (equal? :: <boolean>)
  next-method() 
    & locator-directory(loc1) = locator-directory(loc2)
    & locator-type(loc1) = locator-type(loc2)
    & microsoft-component=(locator-base(loc1), locator-base(loc2))
end method;

define method make 
    (class == <microsoft-locator>,
     #key host, volume, directory-path, directory = directory-path, 
          base, type, prefix, name, extension)
 => (locator :: <microsoft-locator>)
  verify-locator-arguments
    (host, #f, volume, directory, base, type, prefix, name, extension);
  if (prefix)
    let prefix-locator = as(<microsoft-locator>, prefix); // inefficient!
    host := prefix-locator.locator-host;
    volume := prefix-locator.locator-volume;
    directory := prefix-locator.locator-directory;
  else
    host := f-or-as(<microsoft-host-path>, host);
    directory := f-or-as(<microsoft-directory-path>, directory);
  end if;
  case
    name =>
      base := #"derived";
      type := #"derived";
    extension =>
      type := parse-extension(extension, <microsoft-type-path>,
			      parse-microsoft-extension);
      name := #"derived";
    otherwise =>
      type := f-or-as (<microsoft-type-path>, type);
      name := #"derived";
  end;
  let subclass
    = if ((base | type) & name ~= "")
	if (host) <unc-file-locator> else <ms-dos-file-locator> end;
      else
	if (host) <unc-directory-locator> else <ms-dos-directory-locator> end;
      end;
  make(subclass, host: host, directory: directory, name: name,
       base: base, type: type, volume: volume)
end method;

define method as
    (class == <microsoft-locator>, locator-string :: <string>)
 => (locator :: <microsoft-locator>)
  if (looking-at?($unc-prefix, locator-string) // i.e. two initial backslashes
	| looking-at?($alternative-unc-prefix, locator-string))
    as(<unc-locator>, locator-string);
  else
    as(<ms-dos-locator>, locator-string);
  end if;
end method;

define method locator-prefix 
    (locator :: <microsoft-locator>) => (prefix :: <string>)
  // This only deals with fields common to both ms-dos- and unc- locators
  as(<string>, locator-directory(locator) | "")
end method;

define method derive-type 
    (locator :: <microsoft-locator>,
     #key path-class,
          element-parser :: false-or(<function>))
 => (type)
  ignore(path-class, element-parser);
  next-method(locator,
	      path-class: <microsoft-type-path>,
	      element-parser: parse-microsoft-extension)
end method;

define method locator-suffix 
    (locator :: <microsoft-locator>)
 => (suffix :: <string>)
  ""
end method;

// merging

define method override-locator 
    (locator :: <microsoft-locator>,
     #key host = unsupplied (),
	  volume = unsupplied (),
	  directory-path = unsupplied (),
	  directory = directory-path,
	  base = unsupplied (),
	  type = unsupplied (),
	  prefix = unsupplied (),
	  name = unsupplied (),
	  extension = unsupplied (),
     #all-keys)
 => (result :: <microsoft-locator>)
  ensure-no-redundancy ((supplied? (host) | supplied? (volume) | supplied? (directory)),
			supplied? (base), supplied? (type), supplied? (prefix),
			supplied? (name), supplied? (extension));
  if (unsupplied? (name))
    if (supplied? (extension))
      type := extension & parse-extension (extension, <microsoft-type-path>,
					   parse-microsoft-extension);
    end;
    base := override-component(locator-base (locator), base, #f);
    type := override-component(locator-type (locator), type,
				<microsoft-type-path>);
  end;
  if (supplied? (prefix))
    if (supplied? (name))
      make (<microsoft-locator>, prefix: prefix, name: name)
    else
      make (<microsoft-locator>, prefix: prefix, base: base, type: type)
    end
  else
    host := override-component(locator-host(locator), host,
			       <microsoft-host-path>);
    volume := override-component(locator-volume(locator), volume, #f);
    directory := override-component(locator-directory(locator), directory,
				    <microsoft-directory-path>);
    if (supplied?(name))
      make(<microsoft-locator>, host: host, volume: volume,
	   directory: directory,  name: name)
    else
      make(<microsoft-locator>, host: host, volume: volume,
	   directory: directory, base: base, type: type)
    end
  end
end method;

define method default-locator
    (locator :: <microsoft-locator>,
     #key host, volume, directory-path, directory = directory-path,
          base, type, prefix, name, extension, 
     #all-keys)
 => (result :: <microsoft-locator>)
  ensure-no-redundancy ((host | volume | directory), base, type, prefix, name, extension);
  let defaults = (prefix | name)
    & as (<microsoft-locator>, concatenate (prefix | "", name | "")); // inefficient!
  if (prefix)
    host := defaults.locator-host;
    volume := defaults.locator-volume;
    directory := defaults.locator-directory;
  else
    host := f-or-as (<host-path>, host);
    directory := f-or-as (<microsoft-directory-path>, directory);
  end if;
  if (name)
    base := defaults.locator-base;
    type := defaults.locator-type
  elseif (extension)
    type := parse-extension (extension, <microsoft-type-path>,
			     parse-microsoft-extension);
  else
    type := f-or-as (<microsoft-type-path>, type);
  end if;
  make (<microsoft-locator>,
	host: default-component (locator-host (locator), host),
	volume: default-component (locator-volume (locator), volume),
	directory: default-component (locator-directory (locator), directory),
	base: default-component (locator-base (locator), base),
	type: default-component (locator-type (locator), type))
end method;							        

// MS-DOS locator

define abstract class <ms-dos-locator>
  (<locator-volume-slot>,
   <microsoft-locator>) // must be last, I think
end class;

define method \= 
    (loc1 :: <ms-dos-locator>, loc2 :: <ms-dos-locator>)
 => (equal? :: <boolean>)
  next-method() 
    & microsoft-component=(locator-volume(loc1), locator-volume(loc2))
end method;

define class <ms-dos-file-locator> (<file-locator>,
				    <ms-dos-locator>)
end class;

define class <ms-dos-directory-locator> (<directory-locator>,
					 <ms-dos-locator>)
end class;

define method as
    (class == <directory-locator>, locator :: <ms-dos-file-locator>)
 => (locator :: <directory-locator>)
  // This doesn't deal with base == #f case
  // check for .DIR?
  make (<ms-dos-directory-locator>,
	volume: locator-volume (locator),
	directory: down (parse-directory-element
			   (base-as-string (locator-base (locator))),
			 locator-directory (locator)))
end method;

define method as
    (class == <file-locator>, locator :: <ms-dos-directory-locator>)
 => (locator :: <file-locator>)
  // This doesn't deal with root case
  make (<ms-dos-file-locator>,
	volume: locator-volume (locator),
	directory: up (locator-directory (locator)),
	base: parse-base (directory-element-as-string
			    (last (locator-directory (locator)))))
end method;

// merge

define method merge-locators
    (loc1 :: <ms-dos-locator>, loc2 :: <locator>)
 => (result :: <ms-dos-locator>)
  make(<microsoft-locator>,
       volume: merge-components(locator-volume(loc1), locator-volume(loc2)),
       directory: merge-components(locator-directory(loc1),
				   locator-directory(loc2)),
       base: merge-components(locator-base(loc1), locator-base(loc2)),
       type: merge-components(locator-type(loc1), locator-type(loc2)))
end method;

define method abbreviate-locator
    (locator :: <microsoft-locator>, default :: <locator>)
 => (result :: <microsoft-locator>)
  if (microsoft-component=(locator-volume(locator), locator-volume(default))
	& locator-host(locator) = locator-host(default))
    let base = locator-base(locator);
    let type = locator-type(locator);
    let directory = locator-directory(locator);
    let default-directory = locator-directory(default);
    let abbreviated-directory
      = abbreviate-component(directory, default-directory);
    //--- andrewa: added this kludge so that things work out okay when
    //--- the locators are identical directories.
    if (~abbreviated-directory & ~base)
      as(<microsoft-locator>, "./")
    else
      make(<microsoft-locator>,
	   directory: abbreviated-directory,
	   base: base, type: type)
    end
  else
    locator
  end
end method;

//

define method as
    (class == <ms-dos-locator>, locator-string :: <string>)
 => (locator :: <ms-dos-locator>)
  // Parse as MS-DOS locator: D:\dir...\name.ext
  let volume-end = position($volume-separator, locator-string);
  let (volume, dir-start)
    = if (volume-end)
	values(copy-sequence(locator-string, end: volume-end),
	       volume-end + 1)
      else
	values(#f, 0)
      end if;
  let dir-end
    = find-delimiters-from-end(locator-string, $microsoft-separators);
  let (directory, base-start)
    = if (dir-end == #f)
	values(#f, dir-start)
      else
	values(parse-directory(locator-string, 
			       <microsoft-directory-path>, $microsoft-separators,
			       start: dir-start,
			       end: dir-end),
	       dir-end + 1)
      end if;
  if (base-start = size(locator-string))
    make(<ms-dos-directory-locator>,			// <ms-dos-locator>
	 directory: directory, volume: volume)		//  would suffice
  else
    let ext-start = position ($extension-separator, locator-string, start: base-start + 1);
    let base = parse-base (copy-sequence (locator-string,
					  start: base-start,
					  end: ext-start
					    | size (locator-string)));
    let type = if (ext-start)
		 parse-extension (locator-string,
				  <microsoft-type-path>,
				  parse-microsoft-extension,
				  start: ext-start + 1)
	       else
		 unspecified-or-empty-path (<microsoft-type-path>)
	       end if;
    make(<ms-dos-file-locator>, volume: volume,       // <ms-dos-locator>
				directory: directory, //  would suffice
	                        base: base, 
	                        type: type)
  end if;
end method;

define method locator-prefix 
    (locator :: <ms-dos-locator>)
 => (prefix :: <locator-prefix>)
  let volume = locator-volume (locator);
  if (volume)
    concatenate(as(<string>, volume), ":", next-method())
  else
    next-method();
  end if;
end method;

// matching

define class <ms-dos-match>
  (<locator-volume-slot>,
   <locator-directory-slot>,
   <locator-base-slot>,
   <locator-type-slot>,
   <locator-match>)
end class;

define method instantiate 
    (pattern :: <ms-dos-locator>, match :: <locator-match>)
 => (result :: <ms-dos-locator>)
  make(<microsoft-locator>,   
       volume: instantiate-volume(pattern, match),
       directory: instantiate-directory(pattern, match),
       base: instantiate-base(pattern, match),
       type: instantiate-type(pattern, match))
end method;

define method match
    (pattern :: <ms-dos-locator>, candidate :: <ms-dos-locator>)
 => (result :: type-union(<ms-dos-match>, singleton(#"fail")))
  let volume-match = match-volume(pattern, candidate);
  let directory-match = match-directory(pattern, candidate);
  let base-match = match-base(pattern, candidate);
  let type-match = match-type(pattern, candidate);
  if (matched?(volume-match) & matched?(directory-match) 
      & matched?(base-match) & matched?(type-match))
    make(<ms-dos-match>,
	 volume: volume-match, 
	 directory: directory-match,
	 base: base-match, 
	 type: type-match)
  else
    #"fail"
  end if;
end method;

// Microsoft UNC locator

define abstract class <unc-locator>
  (<locator-host-slot>,
   <microsoft-locator>) // must be last, I think
end class;

define method \= 
    (loc1 :: <unc-locator>, loc2 :: <unc-locator>)
 => (equal? :: <boolean>)
  next-method() 
    & locator-host(loc1) = locator-host(loc2);
end method;

define class <unc-file-locator> (<file-locator>,
				 <unc-locator>)
end class;

define class <unc-directory-locator> (<directory-locator>,
				      <unc-locator>)
end class;

define method as
    (class == <directory-locator>, locator :: <unc-file-locator>)
 => (locator :: <directory-locator>)
  // This doesn't deal with base == #f case
  // check for .DIR?
  make (<unc-directory-locator>,
	host: locator-host (locator),
	directory: down (parse-directory-element
			   (base-as-string (locator-base (locator))),
			 locator-directory (locator)))
end method;

define method as
    (class == <file-locator>, locator :: <unc-directory-locator>)
 => (locator :: <file-locator>)
  // This doesn't deal with root case
  make (<ms-dos-file-locator>,
	host: locator-host (locator),
	directory: up (locator-directory (locator)),
	base: parse-base (directory-element-as-string
			    (last (locator-directory (locator)))))
end method;

// merging

define method merge-locators
    (loc1 :: <unc-locator>, loc2 :: <locator>)
 => (result :: <unc-locator>)
  make(<microsoft-locator>,
       host: merge-components(locator-host(loc1), locator-host(loc2)),
       directory: merge-components(locator-directory(loc1),
				   locator-directory(loc2)),
       base: merge-components(locator-base(loc1),
			      locator-base(loc2)),
       type: merge-components(locator-type(loc1),
			      locator-type(loc2)))
end method;

//

define method as
    (class == <unc-locator>, locator-string :: <string>)
 => (locator :: <unc-locator>)
  // Parse as UNC locator:  \\host\dir...\name.ext
  // (we know we're looking at $unc-prefix)
  let start = size($unc-prefix);
  let dir-start
    = find-delimiters(locator-string, $microsoft-separators, start: start);
  unless (dir-start)
    locator-error(<locator-parse-error>, "AS <UNC-LOCATOR>:  No end host delimiter");
  end unless;
  let host-name = copy-sequence(locator-string, start: start, end: dir-start);
  let host = make(<microsoft-host-path>, elements: vector(host-name));
  // Rest same as AS <MS-DOS-LOCATOR> should abstract
  let dir-end
    = find-delimiters-from-end(locator-string, $microsoft-separators);
  let (directory, base-start)
    = if (dir-end == #f)
	values(#f, 0)
      else
	values(parse-directory(locator-string, 
			       <microsoft-directory-path>, 
			       $microsoft-separators,
			       start: dir-start,
			       end: dir-end),
	       dir-end + 1)
      end if;
  if (base-start = size(locator-string))
    make(<unc-directory-locator>, host: host, 		// <unc-locator>
				  directory: directory) //  would suffice
  else
    let ext-start = position ($extension-separator, locator-string, start: base-start + 1);
    let base = parse-base (copy-sequence (locator-string,
					  start: base-start,
					  end: ext-start
					    | size (locator-string)));
    let type = if (ext-start)
		 parse-extension (locator-string,
				  <microsoft-type-path>,
				  parse-microsoft-extension,
				  start: ext-start + 1)
	       else
		 unspecified-or-empty-path (<microsoft-type-path>)
	       end if;
    make(<unc-file-locator>,
	 host: host,	   // <unc-locator>
	 directory: directory, //  would suffice
	 base: base, 
	 type: type)
  end if;
end method;

define method locator-prefix 
    (locator :: <unc-locator>) => (prefix :: <string>)
  concatenate-as(<string>,
		 $unc-prefix,
		 as(<string>, locator-host(locator) | ""), // host must exist?
		 next-method())
end method;

// matching

define class <unc-match>
  (<locator-host-slot>,
   <locator-directory-slot>,
   <locator-base-slot>,
   <locator-type-slot>,
   <locator-match>)
end class;

define method instantiate 
    (pattern :: <unc-locator>, match :: <locator-match>)
 => (result :: <unc-locator>)
  make(<unc-locator>,   
       host: instantiate-host(pattern, match),
       directory: instantiate-directory(pattern, match),
       base: instantiate-base(pattern, match),
       type: instantiate-type(pattern, match))
end method;

define method match
    (pattern :: <unc-locator>, candidate :: <unc-locator>)
 => (result :: type-union(<unc-match>, singleton(#"fail")))
  let host-match = match-host(pattern, candidate);
  let directory-match = match-directory(pattern, candidate);
  let base-match = match-base(pattern, candidate);
  let type-match = match-type(pattern, candidate);
  if (matched?(host-match) & matched?(directory-match) 
      & matched?(base-match) & matched?(type-match))
    make(<unc-match>, host: host-match, 
                      directory: directory-match,
	              base: base-match, 
                      type: type-match)
  else
    #"fail"
  end if;
end method;


// Microsoft paths

define abstract class <microsoft-path> (<path>)
end class <microsoft-path>;

define method microsoft-component=
    (component1, component2) => (equal? :: <boolean>)
  component1 = component2
end method microsoft-component=;

define method microsoft-component=
    (component1 :: <string>, component2 :: <string>) => (equal? :: <boolean>)
  case-insensitive=(component1, component2)
end method microsoft-component=;

define method microsoft-component=
    (component1 :: <byte-string>, component2 :: <byte-string>)
 => (equal? :: <boolean>)
  case-insensitive=(component1, component2)
end method microsoft-component=;

//--- Have to use case insensitive comparison of strings
define method \= 
    (path1 :: <microsoft-path>, path2 :: <microsoft-path>)
 => (equal? :: <boolean>)
  object-class(path1) == object-class(path2)
    & relative-path?(path1) == relative-path?(path2)
    & iterate search (elts1 = path1.elements, elts2 = path2.elements)
        case
	  elts1 == #() | elts2 == #() =>
	    elts1 == elts2;
	  microsoft-component=(elts1.head, elts2.head) =>
	    search(elts1.tail, elts2.tail);
	  otherwise =>
	    #f
	end
      end iterate;
end method;


define class <microsoft-directory-path> (<microsoft-path>, <directory-path>)
end class;

define method might-have-links?
    (path :: <microsoft-directory-path>) => (might-have-links? :: <boolean>)
  #f
end;

define method as
    (class == <string>, path :: <microsoft-directory-path>)
 => (string :: <string>)
  let absolute-slash = if (path.relative-path?) "" else "\\" end;
  as-string-internal(path, absolute-slash, "\\", "\\",
		     element-as-string: directory-element-as-string);
end method;


define class <microsoft-host-path> (<microsoft-path>, <host-path>)
end class <microsoft-host-path>;


define class <microsoft-type-path> (<microsoft-path>, <type-path>)
end class;

define method as
    (class == <string>, path :: <microsoft-type-path>)
 => (string :: <string>)
  if (path.elements.empty?)
    ""
  else
    as-string-internal(path, ".", ".", "",
		       element-as-string: microsoft-type-as-extension)
  end if;
end method;

