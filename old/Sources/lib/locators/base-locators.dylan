module: locator-internals
author: Tim McNerney
revised: 13-Feb-96 mf
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Locators

/// <locator-default> and <locator> have been moved to the basic-locator-classes
/// module in functional-extensions

//---*** Should this be exported from here (or functional-extensions)
//---*** or should we leave it in file-system?
// define constant <pathname> = type-union(<string>, <locator>);

// GENERICS
define generic wild-locator? (x) => (wild? :: <boolean>);
define generic absolute-locator? (x) => (absolute? :: <boolean>);
define generic relative-locator? (x) => (relative? :: <boolean>);
define generic merge-locators (locator1, locator2) => (locator);
define generic abbreviate-locator (locator, default) => (locator);
define generic simplify-locator (locator) => (locator);
define generic override-locator
    (locator, 
     #key directory, directory-path, base, type, prefix, name, extension
          /* , host, port, user-id, password, transfer-type */)
 => (locator);
define generic default-locator
    (locator, 
     #key directory, directory-path, base, type, prefix, name, extension
          /* , host, port, user-id, password, transfer-type */)
 => (locator);

define generic add-abstract-host (host, translations :: <collection>, #key host-table);
define generic remove-abstract-host (host, #key host-table);
define generic clear-abstract-host-table (#key host-table);

define generic instantiate (pattern, match);

define generic match (pattern, candidate);

define generic locators-match? (candidate, pattern);

define generic translate-locator (locator, from, to);

// locator slot types
//
// This is a first attempt to start to get all of these slots typed
// properly. Gradually we should be able to make each one of these
// types more specific once it becomes clear what the intention is.

define constant <locator-scheme> = <object>;
define constant <locator-host> = <object>;
define constant <locator-port> = <object>;
define constant <locator-user-id> = <object>;
define constant <locator-password> = <object>;
define constant <locator-volume> = <object>;
define constant <locator-directory> = <object>;
define constant <locator-base> = <object>;
define constant <locator-type> = <object>;
define constant <locator-version> = <object>;
define constant <locator-search-keys> = <object>;
define constant <locator-transfer-type> = <object>;

define constant <locator-prefix> = <object>;
define constant <locator-name> = <object>;
define constant <locator-extension> = <object>;
define constant <locator-suffix> = <object>;

// (virtual) slots
define generic locator-case-sensitive? (locator) => (sensitive? :: <boolean>);
define generic locator-scheme (locator) => (scheme :: <locator-scheme>);
define generic locator-host (locator) => (host :: <locator-host>);
define generic locator-port (locator) => (port :: <locator-port>);
define generic locator-user-id (locator) => (user-id :: <locator-user-id>);
define generic locator-password (locator) => (password :: <locator-password>);
define generic locator-volume (locator) => (volume :: <locator-volume>);
define generic locator-directory (locator) => (directory :: <locator-directory>);
define generic locator-base (locator) => (base :: <locator-base>);
define generic locator-type (locator) => (type :: <locator-type>);
define generic locator-version (locator) => (version :: <locator-version>);
define generic locator-search-keys (locator) => (keys :: <locator-search-keys>);
define generic locator-transfer-type (locator) => (type :: <locator-transfer-type>);

define generic locator-prefix (locator) => (prefix :: <locator-prefix>);
define generic locator-name (locator) => (name :: <locator-name>);
define generic locator-extension (locator) => (extensions :: <locator-extension>);
define generic locator-suffix (locator) => (suffix :: <locator-suffix>);

//---*** andrewa: this is a temporary definition, I hope to rename
//---*** locator-directory as locator-directory-path once it is safe.
define constant locator-directory-path = locator-directory;

// Merge coercers

define method merge-locators
    (locator-string1 :: <string>, locator-string2 :: <string>)
 => (locator)
  merge-locators(as(<locator>, locator-string1), as(<locator>, locator-string2))
end method;

define method merge-locators
    (locator :: <locator>, locator-string :: <string>)
 => (locator)
  merge-locators(locator, as(<locator>, locator-string))
end method;

define method merge-locators
    (locator-string :: <string>, locator :: <locator>)
 => (locator)
  merge-locators(as(<locator>, locator-string), locator)
end method;

// degenerate case

define method merge-locators
    (x == #f, y == #f) => (locator :: singleton(#f))
  #f
end method;

// abbreviate coercers

define method abbreviate-locator
    (locator-string1 :: <string>, locator-string2 :: <string>)
 => (locator)
  abbreviate-locator(as(<locator>, locator-string1),
		     as(<locator>, locator-string2))
end method;

define method abbreviate-locator 
    (locator :: <locator>, locator-string :: <string>)
 => (locator)
  abbreviate-locator(locator, as(<locator>, locator-string))
end method;

define method abbreviate-locator 
    (locator-string :: <string>, locator :: <locator>)
 => (locator)
  abbreviate-locator(as(<locator>, locator-string), locator)
end method;

// Internal API for merging components (called by "merge") 

define method merge-components 
    (component, default) => (merged-component)
  // Deal with #f #"unspecific"
  component | default
end method;

// Internal API for abbreviating components (called by "abbreviate") 

define sealed generic abbreviate-component
    (component, default, #key test :: <function>)
 => (abbreviated-component);

define method abbreviate-component 
    (component, default, #key test :: <function> = \=)
 => (abbreviated-component)
  // Deal with #f #"unspecific"
  if (test(component, default))
    #f
  else
    component
  end if;
end method;

// Internal API for defaulting components (called by "default")

define method default-component 
    (component, default) => (default-component)
  // simple-minded implementation
  // need to deal with #"unspecific"
  if (instance?(component, <path>) | instance?(default, <path>))
    merge-components (component, default)
  else
    component | default
  end if;
end method;

// Internal API for overriding components (called by "override")

define method override-component 
    (component, override, coercion-class :: false-or(<class>))
 => (overridden-component)
  // simple-minded implementation
  // need to deal with #"unspecific"
  if (unsupplied?(override))
    component
  elseif (coercion-class & override)
    as(coercion-class, override)
  else
    override
  end if
end method;

// simplify coercer

define method simplify-locator 
    (locator-string :: <string>) => (locator)
  simplify-locator(as(<locator>, locator-string))
end method;

// defaults for abbreviate and simplify

define method abbreviate-locator 
    (locator :: <locator>, defaults :: <locator>) => (locator :: <locator>)
  locator
end method;

define method simplify-locator 
    (locator :: <locator>) => (locator :: <locator>)
  locator
end method;

//

/// <physical-locator> has been moved to the basic-locator-classes
/// module in functional-extensions

define sideways method \= 
    (loc1 :: <locator>, loc2 :: <locator>)
 => (equal? :: <boolean>)
  object-class(loc1) == object-class(loc2);
end method;

define sideways method as
    (class == <locator>, locator-string :: <string>)
 => (locator :: <locator>)
  as(as-locator-class(locator-scheme(locator-string),
		      default: <native-locator>),
     locator-string);
end method;

// degenerate case(s)

define sideways method as
    (class :: subclass(<locator>), locator :: <locator>)
 => (locator :: <locator>)
  if (subtype?(object-class(locator), class))
    locator
  else
    next-method()
  end if;
end method as;

/*
define method as (class == <locator>, locator :: <locator>)
  locator
end method;
*/
// locator match

define abstract class <locator-match> (<locator-defaults>)
end class;

define method locators-match? 
    (candidate :: <locator>, pattern :: <locator>)
 => (match? :: <boolean>)
  matched?(match(pattern, candidate));
end method;

define method translate-locator 
    (locator :: <locator>, from, to) => (locator :: <locator>)
  let locator-match = match(from, locator);
  if (failed?(locator-match))
    locator
  else
    instantiate(to, locator-match)
  end if
end method;

define method \= 
    (loc1 :: <locator-match>, loc2 :: <locator-match>)
 => (equal? :: <boolean>)
  object-class(loc1) == object-class(loc2);
end method;

define method matched? (x) 
  x ~= #"fail"
end method;

define method failed? (x) 
  x == #"fail"
end method;

define macro fail-or
  { fail-or(?type:expression) }
    => { type-union(?type, singleton(#"fail")) }
end macro fail-or;

// default slot "accessors" for uniform API

define method locator-scheme 
    (locator :: <locator-defaults>) => (scheme :: <locator-scheme>)
  #f  // never "not-applicable"
end method;

define method locator-host 
    (locator :: <locator-defaults>) => (host :: <locator-host>)
  #f //#"not-applicable"
end method;

define method locator-port 
    (locator :: <locator-defaults>) => (port :: <locator-port>)
  #f //#"not-applicable"
end method;

define method locator-user-id 
    (locator :: <locator-defaults>) => (used-id :: <locator-user-id>)
  #f //#"not-applicable"
end method;

define method locator-password 
    (locator :: <locator-defaults>) => (password :: <locator-password>)
  #f //#"not-applicable"
end method;

define method locator-volume 
    (locator :: <locator-defaults>) => (volume :: <locator-volume>)
  #f //#"not-applicable"
end method;

define method locator-directory 
    (locator :: <locator-defaults>) => (directory :: <locator-directory>)
  #f //#"not-applicable"
end method;

define method locator-base 
    (locator :: <locator-defaults>) => (base :: <locator-base>)
  #f //#"not-applicable"
end method;

define method locator-type 
    (locator :: <locator-defaults>) => (type :: <locator-type>)
  #f //#"not-applicable"
end method;

define method locator-version 
    (locator :: <locator-defaults>) => (version :: <locator-version>)
  #f //#"not-applicable"
end method;

define method locator-search-keys 
    (locator :: <locator-defaults>) => (search-keys :: <locator-search-keys>)
  #f //#"not-applicable"
end method;

define method locator-transfer-type 
    (locator :: <locator-defaults>) => (type :: <locator-transfer-type>)
  #f //#"not-applicable"
end method;


// These always return a string

define method locator-prefix 
    (locator :: <locator-defaults>) => (prefix :: <string>)
  ""
end method;

define method locator-name 
    (locator :: <locator-defaults>) => (name :: <string>)
  ""
end method;

define method locator-extension 
    (locator :: <locator-defaults>) => (extension :: <string>)
  ""
end method;

define method locator-suffix 
    (locator :: <locator-defaults>) => (suffix :: <string>)
  ""
end method;

// Now everyone can use this!

define sideways method as
    (class == <string>, locator :: <locator-defaults>)
 => (string :: <string>)
  concatenate-as(<string>,
		 locator-prefix(locator) | "",
		 locator-name(locator)   | "",
		 locator-suffix(locator) | "")
end method;


//

define abstract class <directory-locator> (<locator>)
end class;

define abstract class <file-locator> (<locator>)
end class;

define method as
    (class == <directory-locator>, locator :: <directory-locator>)
 => (locator :: <directory-locator>)
  locator
end method;

define method as
    (class == <file-locator>, locator :: <file-locator>) 
 => (locator :: <file-locator>)
  locator
end method;

define method as
    (class == <directory-locator>, locator-string :: <string>)
 => (locator :: <directory-locator>)
  as (<directory-locator>, as (<native-locator>, locator-string))
end method;

define method as 
    (class == <file-locator>, locator-string :: <string>)
 => (locator :: <file-locator>)
  as (<file-locator>, as (<native-locator>, locator-string))
end method;

// slot mixins

define abstract class <locator-host-slot> (<object>)
  constant slot locator-host :: false-or(<path>) = #f,
    init-keyword: host:;
end class;

define method match-host
    (pattern :: <locator-host-slot>,
     candidate :: <locator-host-slot>)
 => (result :: fail-or(<match-path>))
  match (locator-host (pattern), locator-host (candidate))
end method;

define method instantiate-host 
    (pattern :: <locator-host-slot>, match :: <locator-match>)
 => (result :: <host-path>)
  instantiate(locator-host (pattern), locator-host (match))
end method;

define abstract class <locator-port-slot> (<object>)
  constant slot locator-port :: <locator-port> = #f, 
    init-keyword: port:;
end class;

define abstract class <locator-user-id-slot> (<object>)
  constant slot locator-user-id :: <locator-user-id> = #f,
    init-keyword: user-id:;
end class;

define abstract class <locator-password-slot> (<object>)
  constant slot locator-password :: <locator-password> = #f,
    init-keyword: password:;
end class;

define abstract class <locator-volume-slot> (<object>)
  constant slot locator-volume :: <locator-volume> = #f,
    init-keyword: volume:;
end class;

define method match-volume
    (pattern :: <locator-volume-slot>, candidate :: <locator-volume-slot>)
 => (result :: fail-or(<match-path>))
  match (locator-volume (pattern), locator-volume (candidate))
end method;

define method instantiate-volume
    (pattern :: <locator-volume-slot>, match :: <locator-match>)
 => (result :: false-or(<string>))
  instantiate (locator-volume (pattern), locator-volume (match))
end method;

define abstract class <locator-directory-slot> (<object>)
  constant slot locator-directory :: false-or(<path>) = #f,
    init-keyword: directory:;
end class;

define method absolute-locator? 
    (locator :: <locator-directory-slot>) => (absolute? :: <boolean>)
  absolute-locator? (locator-directory (locator))
end method;

define method relative-locator? 
    (locator :: <locator-directory-slot>) => (relative? :: <boolean>)
  relative-locator? (locator-directory (locator))
end method;

define method match-directory
    (pattern :: <locator-directory-slot>,
     candidate :: <locator-directory-slot>)
 => (result :: fail-or(<match-path>))
  match (locator-directory (pattern), locator-directory (candidate))
end method;

define method instantiate-directory 
    (pattern :: <locator-directory-slot>, match :: <locator-match>)
 => (result :: <directory-path>)
  instantiate(locator-directory(pattern), locator-directory(match))
end method;

define method simplify-locator 
    (locator :: <locator-directory-slot>) => (locator)
  let directory = locator.locator-directory;
  if (directory)
    let simplified-directory = simplify-locator(directory);
    if (locator-base(locator) | simplified-directory)
      override-locator(locator, directory: simplified-directory)
    else
      override-locator(locator, 
		       directory: make(object-class(directory),
				       elements: #[#"self"],
				       relative-path?: #t))
    end
  else
    locator
  end
end method;

/*---*** andrewa: this isn't used, and doesn't fit very well anyway
define function parent-directory
    (path :: <pathname>) => (path :: <string>)
  //---*** Maybe this can use 'up'?
  let locator = as(<locator>, path);
  let dir = locator-directory(locator);
  let path = path-elements(dir);
  let new-path
    = make(object-class(dir),
	   elements: copy-sequence(path, end: size(path) - 1));
  as(<string>, override-locator(locator, directory: new-path))
end function parent-directory;
*/

// derived stuff

define method derived? 
    (object) => (derived? :: <boolean>)
  object == #"derived"
end method;

// base

define abstract class <locator-base-attribute> (<object>) 
end class;

define abstract class <locator-base-slot> (<locator-base-attribute>)
  constant slot locator-base :: <locator-base> = #f,
    init-keyword: base:;
end class;

define abstract class <locator-derived-base-slot> (<locator-base-attribute>)
  slot raw-base = #f,
    init-keyword: base:;
end class;

/*
define method base-setter (locator :: <locator-derived-base-slot>, new-value)
  raw-base(locator) := new-value
end method;
*/

define method derive-base 
    (locator :: <locator-derived-base-slot>)
 => (base :: <string>)
  let name = locator.locator-name;
  let first-dot = position ('.', name, start: 1);
  if (first-dot)
    name := copy-sequence (name, end: first-dot);
  end;
  parse-base(name)
end;

define method locator-base 
    (locator :: <locator-derived-base-slot>)
 => (base :: <locator-base>)
  let raw-base = raw-base(locator);
  if (derived?(raw-base))
    let derived-base = derive-base (locator);
    raw-base(locator) := derived-base;
    derived-base
  else
    raw-base
  end if
end method;

define method match-base 
    (pattern :: <locator-base-attribute>,
     candidate :: <locator-base-attribute>)
 => (result :: fail-or(<string>))
  match (locator-base (pattern), locator-base (candidate))
end method;

define method instantiate-base (pattern :: <locator-base-attribute>,
				     match :: <locator-match>)
                              => result :: false-or (<string>); // and more
  instantiate (locator-base (pattern), locator-base (match))
end method;

// type

define abstract class <locator-type-attribute> (<object>)
end class;

define abstract class <locator-type-slot> (<locator-type-attribute>)
  constant slot locator-type :: <locator-type> = #f,
    init-keyword: type:;
end class;

define abstract class <locator-derived-type-slot> (<locator-type-attribute>)
  slot raw-type = #f,
    init-keyword: type:;
end class;

define method locator-extension
    (locator :: <locator-type-attribute>) => (value :: <string>)
  as (<string>, locator-type (locator) | "")
end method;

/*
define method type-setter (locator :: <locator-derived-type-slot>, new-value)
  raw-type(locator) := new-value
end method;
*/

define method derive-type
    (locator :: <locator-derived-type-slot>,
     #key path-class = <posix-type-path>,	// default is posix
          element-parser :: <function> = parse-posix-extension)
 => (type :: false-or(<type-path>))
  let name = locator.locator-name;
  let first-dot = position ('.', name, start: 1);
  if (first-dot)
    parse-extension (name, path-class, element-parser, start: first-dot + 1)
  elseif (name = "")
    #f
  else
    unspecified-or-empty-path (path-class)
  end
end;

define method locator-type 
    (locator :: <locator-derived-type-slot>) => (type :: <locator-type>)
  let raw-type = raw-type(locator);
  if (derived?(raw-type))
    let derived-type = derive-type (locator); // not especially efficient
    raw-type(locator) := derived-type;
    derived-type
  else
    raw-type
  end if
end method;

define method match-type
    (pattern :: <locator-type-attribute>,
     candidate :: <locator-type-attribute>)
 => (result :: fail-or(<match-path>))
  match (locator-type (pattern), locator-type (candidate))
end method;

define method instantiate-type 
    (pattern :: <locator-type-attribute>,
     match :: <locator-match>)
 => (result :: false-or(type-union(<type-path>, <string>)))
  instantiate (locator-type (pattern), locator-type (match))
end method;

// name

define abstract class <locator-name-attribute> (<object>)
end class;

/*
define abstract class <locator-name-slot> (<locator-name-attribute>)
  constant slot locator-name :: <locator-name> = #f,
    init-keyword: name:;
end class;
*/

define abstract class <locator-derived-name-slot> (<locator-name-attribute>)
  slot raw-name = #"derived", 
    init-keyword: name:;
end class;

/*
define method name-setter (locator :: <locator-derived-name-slot>, new-value)
  raw-name(locator) := new-value
end method;
*/

define method locator-name 
    (locator :: <locator-derived-name-slot>) => (name :: <locator-name>)
  let raw-name = raw-name(locator);
  if (derived?(raw-name))
    let type = locator-type (locator);
    let base = locator-base (locator);
    if ((base == #f) & (type ~= #f) & (size(type) > 0))
      // type but no base cannot be represented as a string: e.g. ".text"
      locator-error (<locator-print-error>,
		     "AS <STRING>: Cannot represent locator as a string: ext w/o base")
    end if;
    let derived-name = concatenate (base-as-string (base), as(<string>, type | ""));
    raw-name(locator) := derived-name;
    derived-name
  else
    raw-name
  end if;
end method;

// version

define abstract class <locator-version-slot> (<object>)
  constant slot locator-version :: <locator-version> = #f,
    init-keyword: version:;
end class;

define method match-version (pattern :: <locator-version-slot>,
			     candidate :: <locator-version-slot>)
  let p-version = locator-version (pattern);
  let c-version = locator-version (candidate);
  if (p-version == #"wild" | p-version = c-version)
    c-version
  else
    #"fail"
  end if;
end method;

define method instantiate-version (pattern :: <locator-version-slot>,
				   match :: <locator-match>)
  instantiate (locator-version (pattern), locator-version (match))
end method;

//

define abstract class <locator-search-keys-slot> (<object>)
  constant slot locator-search-keys :: <locator-search-keys> = #f,
    init-keyword: search-keys:;
end class;

define abstract class <locator-transfer-type-slot> (<object>)
  constant slot locator-transfer-type :: <locator-transfer-type> = #f,
    init-keyword: transfer-type:;
end class;

// (Relatively) generic parsing support

define method parse-base 
    (base :: <string>) => (result)
  select (base by \=)
    ""        => #f;
    "*"       => #"wild";
    otherwise => base;
  end
end method;

define method base-as-string 
    (base :: <string>) => (string :: <string>)
  base
end method;

define method base-as-string 
    (base == #f) => (string :: <string>)
  ""
end method;

define method base-as-string 
    (base :: <symbol>) => (string :: <string>)
  if (base == #"wild")
    "*"
  else
    locator-error(<locator-print-error>, "BASE-AS-STRING: invalid keyword")
  end if;
end method;

// This is shared by posix, abstract, and url locators
// ms-dos extensions will be different
// (this name is a bit ambiguous, here by "extension" we mean
//  an element of a composite type.  By "type"
//  we mean the whole (composite) type.)
// +++ fix comments for consistent terminology

define method parse-abstract-extension (extension :: <string>)
  let type = parse-posix-extension (extension);
  if (instance? (type, <string>))
    let type = parse-microsoft-extension(extension);
    if (instance?(type, <string>))
      select (extension by \=)
	"gzip"		=> #"gzip";
	"postscript"	=> #"postscript";
	otherwise	=> extension;
      end
    else
      type
    end;
  else
    type
  end
end method;

define method parse-posix-extension (extension :: <string>)
  // do this with a real table eventually
  select (extension by \=)
    ""		=> #f;
    "*"		=> #"wild-inferiors";
    "Z"		=> #"compress";
    "dylan"	=> #"dylan";
    "gz"	=> #"gzip";
    "html"	=> #"html";
    "ps"	=> #"postscript";
    "text"	=> #"text";
    "uu"	=> #"uuencode";
    otherwise	=> extension;
  end
end method;

define method posix-type-as-extension (type :: <string>)
  type
end;

define method posix-type-as-extension (type :: <symbol>)
  select (type)
    #"wild-inferiors"	=> "*";
    #"compress"		=> "Z";
    #"dylan"		=> "dylan";
    #"gzip"		=> "gz";
    #"html"		=> "html";
    #"object"		=> "o";
    #"postscript"	=> "ps";
    #"text"		=> "text";
    #"uuencode"		=> "uu";
    otherwise => locator-error (<locator-print-error>,
				"POSIX-TYPE-AS-EXTENSION: not a valid type");
  end
end method;

// microsoft types

define method parse-microsoft-extension (extension :: <string>)
  // do this with a real table eventually
  select (extension by \=)
    ""		=> #f;
    "*"		=> #"wild-inferiors";
    "Z"		=> #"compress";
    "dyl"	=> #"dylan";
    "gz"	=> #"gzip";
    "htm"	=> #"html";
    "o"		=> #"object";
    "ps"	=> #"postscript";
    "txt"	=> #"text";
    "uu"	=> #"uuencode";
    otherwise	=> extension;
  end
end method;

define method microsoft-type-as-extension (type :: <string>)
  type
end;

define method microsoft-type-as-extension (type :: <symbol>)
  select (type)
    #"wild-inferiors"	=> "*";
    #"compress"		=> "Z";
    #"dylan"		=> "dyl";
    #"gzip"		=> "gz";
    #"html"		=> "htm"; // Is this right?
    #"object"		=> "o";
    #"postscript"	=> "ps";
    #"text"		=> "txt";
    #"uuencode"		=> "uu";
    otherwise => locator-error (<locator-print-error>,
				"MICROSOFT-TYPE-AS-EXTENSION: not a valid type");
  end
end method;

define method parse-version (version :: <string>)
  // implement branches eventually
  select (version by \=)
    ""        => #f;
    "newest"  => #"newest";
    "oldest"  => #"oldest";
    "*"       => #"wild";
    otherwise => string-to-integer(version);
  end select;
end method;

define method version-as-string 
    (version == #f) => (string :: <string>)
  ""
end method;

define method version-as-string 
    (version :: <integer>) => (string :: <string>)
  concatenate-as(<string>, ";", integer-to-string(version))
end method version-as-string;

define method version-as-string 
    (version :: <symbol>) => (string :: <string>)
  // implement branches eventually
  select (version)
    #"newest" => ";newest";
    #"oldest" => ";oldest";
    #"wild"   => ";*";
    otherwise =>
      locator-error(<locator-print-error>,
		    "VERSION-AS-STRING: illegal keyword");
  end select;
end method;

/* I don't think this needs to be defined here anymore... -mf

define method as (class :: subclass(<string>), integer :: <integer>) => (value)
  let index = 0;
  let string = ""; // dummy
  local method print-buddy (integer :: <integer>, output :: <function>)
	  let base = 10;  // need more generality?
          let (quotient :: <integer>, remainder :: <integer>)
            = truncate/(integer, base);
          unless (zero?(quotient))
            // Recurse until you have all the digits pushed on stack
            print-buddy(quotient, output)
          end unless;
          // Then as each recursive call unwinds, turn the digit (in
          // remainder) into a character and output the character.
          output("0123456789ABCDEF..."[remainder])
	end method print-buddy,

        method count! (char)
	  index := index + 1;
	end method count!,
	  
        method fill! (char) 
	  string[index] := char;
	  index := index + 1
	end method fill!;

  if (negative?(integer))
    index := 0; // side-effected by count!
    string := make(class, size: print-buddy(- integer, count!) + 1);
    string[0] := '-';
    index := 1; // side-effected by fill!
    print-buddy(- integer, fill!)
  else
    index := 0; // side-effected by count!
    string := make(class, size: print-buddy(integer, count!));
    index := 0; // side-effected by fill!
    print-buddy(integer, fill!)
  end if;
  string // return value
end method as;
*/

/*---*** andrewa: we can use string-to-integer now
define method as (class == <integer>, string :: <string>) => (value)
  // ASCII & UNICODE only!
  local method atoi-internal (string, size, index, result)
	  if (index = size)
	    result
	  else
	    atoi-internal(string, size, index + 1,
			  (result * 10) 
			    + as(<integer>, string[index])
			    - as(<integer>, '0'))
	  end if;
	end method;
  atoi-internal(string, size(string), 0, 0);
end method;
*/

define method parse-path-elements 
    (locator-string :: <string>,
     delimiter :: <character>,
     start :: <integer>, finish :: <integer>,
     #key element-parser :: <function> = identity)
 => (path-elements)
  let elements = make(<stretchy-vector>);
  let position :: <integer> = start;
  local method maybe-add-element () => ()
	  unless (start = position)
	    let substring
	      = copy-sequence(locator-string, start: start, end: position);
	    add!(elements, element-parser(substring))
	  end;
	  start := position + 1
	end method maybe-add-element;
  while (position < finish)
    if (locator-string[position] = delimiter)
      maybe-add-element()
    end;
    position := position + 1;
  end;
  unless (position <= start)
    maybe-add-element()
  end;
  elements
end method;

define method parse-path-elements 
    (locator-string :: <string>,
     delimiters :: <sequence>,
     start :: <integer>, finish :: <integer>,
     #key element-parser :: <function> = identity)
 => (path-elements :: <sequence>)
  let elements = make(<stretchy-vector>);
  let position :: <integer> = start;
  local method maybe-add-element () => ()
	  unless (start = position)
	    let substring
	      = copy-sequence(locator-string, start: start, end: position);
	    add!(elements, element-parser(substring))
	  end;
	  start := position + 1
	end method maybe-add-element;
  while (position < finish)
    member?(locator-string[position], delimiters) & maybe-add-element();
    position := position + 1;
  end;
  unless (position <= start)
    maybe-add-element()
  end;
  elements
end method;

define method find-delimiters
    (string :: <string>, delimiters :: <sequence>,
     #key start :: <integer> = 0)
 => (position :: false-or(<integer>))
  let size = size(string);
  block (return)
    for (index :: <integer> from start below size - 1)
      when (member?(string[index], delimiters))
	return(index)
      end
    end
  end
end method find-delimiters;

define method find-delimiters-from-end
    (string :: <string>, delimiters :: <sequence>,
     #key start :: <integer> = 0)
 => (position :: false-or(<integer>))
  let size = size(string);
  block (return)
    for (index :: <integer> from size - 1 to start by -1)
      when (member?(string[index], delimiters))
	return(index)
      end
    end
  end
end method find-delimiters-from-end;

define constant <directory-element> = false-or(type-union(<string>, <symbol>));

define method parse-directory-element 
    (string :: <string>)
 => (directory-element :: <directory-element>)
  select (string by \=)
    "."       => #"self";
    ".."      => #"parent";
    "*"       => #"wild";
    "**"      => #"wild-inferiors";
    otherwise => string
  end
end method;

define sealed generic directory-element-as-string 
    (element :: <directory-element>) => (string :: <string>);

define method directory-element-as-string 
    (element :: <string>) => (string :: <string>)
  element
end method;

define method directory-element-as-string 
    (element == #f) => (string :: <string>)
  ""
end method;

define method directory-element-as-string 
    (element :: <symbol>) => (string :: <string>)
  select (element)
    #"self"           => ".";
    #"parent"         => "..";
    #"wild"           => "*";
    #"wild-inferiors" => "**";
    otherwise =>
      locator-error(<locator-print-error>,
		    "DIRECTORY-ELEMENT-AS-STRING: not a valid element")
  end select;
end method;

// This should go into a utility or CL-compatibility package
//define method digit-char? (character :: <character>)
//  member?(character, "0123456789")
//end method;

define method parse-host-element 
    (element :: <string>)
 => (result :: type-union(<integer>, <string>))
  if (every?(rcurry(member?, "0123456789"), element))
    string-to-integer(element)
  else
    element
  end if;
end method;

define method parse-host
    (locator-string :: <string>, 
     #key start :: <integer> = 0,
          end: finish :: <integer> = size(locator-string))
 => (path :: <host-path>)
  make(<host-path>, 
       elements: parse-path-elements(locator-string, '.',
				     start, finish,
				     element-parser: parse-host-element));
end method;

define method parse-directory 
    (locator-string :: <string>, 
     class :: subclass(<directory-path>), 
     delimiter :: <character>,
     #key start :: <integer> = 0,
          end: finish :: <integer> = size(locator-string))
 => (path :: <directory-path>)
  let absolute? = locator-string[start] == delimiter;
  if (absolute?)
    start := start + 1;
  end;
  let elements = if  (start > finish)
		   #()
		 else
		   parse-path-elements(locator-string, delimiter, 
				       start, finish, 
				       element-parser: parse-directory-element)
		 end;
  make (class, elements: elements, relative-path?: ~absolute?)
end method;
	  
define method parse-directory 
    (locator-string :: <string>, 
     class :: subclass(<directory-path>), 
     delimiters :: <sequence>,
     #key start :: <integer> = 0,
          end: finish :: <integer> = size(locator-string))
 => (path :: <directory-path>)
  let absolute? = member?(locator-string[start], delimiters);
  if (absolute?)
    start := start + 1;
  end;
  let elements
    = if (start > finish)
	#()
      else
	parse-path-elements(locator-string, delimiters, 
			    start, finish, 
			    element-parser: parse-directory-element)
      end;
  make(class, elements: elements, relative-path?: ~absolute?)
end method;
	  
define method parse-extension 
    (locator-string :: <string>, 
     class :: subclass(<path>), element-parser :: <function>,
     #key start :: <integer> = 0,
          end: finish :: <integer> = size(locator-string))
 => (extension :: <path>)
  make(class,
       elements: parse-path-elements(locator-string, '.', start, finish,
				     element-parser: element-parser));
end method;

// Basic parsing support

define method looking-at? 
    (prefix :: <string>, string :: <string>, #key start :: <integer> = 0)
 => (looking-at? :: <boolean>)
  let prefix-size = size(prefix);
  if (size(string) < (prefix-size + start))
    #f;
  else
    block (return)
      if (start = 0)
	for (index from 0 below prefix-size)
	  if (prefix[index] ~= string[index])
	    return(#f);
	  end if;
	finally #t;
	end for;
      else
	for (p-index from 0 below prefix-size,
	     s-index from start)
	  if (prefix[p-index] ~= string[s-index])
	    return(#f);
	  end if;
	finally #t;
	end for;
      end if;
    end block;
  end if;
end method;

// General instantiation

define sideways method make
    (class == <locator>, #rest init-args, #key scheme, prefix)
 => (locator :: <locator>)
  if (prefix)
    if (scheme)
      error("MAKE <LOCATOR>: Can't supply both prefix and scheme");
    else
      scheme := locator-scheme (prefix);
    end;
  end;
  apply(make, as-locator-class(scheme, default: <native-locator>),
	init-args);
end method;

// Redundancy check

define method verify-locator-arguments
    (host :: <locator-host>, port :: <locator-port>, 
     volume :: <locator-volume>, directory :: <locator-directory>,
     base :: <locator-base>, type :: <locator-type>,
     prefix :: <locator-prefix>, name :: <locator-name>,
     extension :: <locator-extension>)
 => ()
  local method explicit? (x) => (explicit? :: <boolean>)
	  ~(x == #f | derived?(x))
	end method;
  case
    //---*** andrewa: this test would be a good idea, but currently
    //---*** locators don't have to name files or directories.
    // ~directory & ~name & ~base =>
    //   error("MAKE <LOCATOR>: Locator specified with both no directory and no name or base");
    otherwise =>
      ensure-no-redundancy
	(host | port | volume | directory, base, type, prefix,
	 name, extension);
  end case;
end method;

define method ensure-no-redundancy
    (prefix-component, base :: <locator-base>, type :: <locator-type>,
     prefix :: <locator-prefix>, name :: <locator-name>, 
     extension :: <locator-extension>)
 => ()
  local method explicit? (x)
	  ~(x == #f | derived?(x))
	end method;
  case
    prefix & prefix-component =>
      error("MAKE <LOCATOR>: Can't supply both prefix and any of host, port, volume, or directory");
    name & (explicit?(base) | extension | explicit?(type)) =>
      error("MAKE <LOCATOR>: Can't supply both name and any of base, extension, or type");
    extension & type =>
      error("MAKE <LOCATOR>: Can't supply both extension and type");
    otherwise =>
      values();
  end case;
end method;


/// Locator pretty names

define generic locator-pretty-name
    (pathname :: type-union(<locator>, <string>),
     #key hide-prefix?, hide-type?, hide-suffix?, name-first?)
 => (string :: <string>);

define sealed method locator-pretty-name
    (locator :: <locator>,
     #key hide-prefix? :: <boolean> = #f,
          hide-type?   :: <boolean> = #f,
          hide-suffix? :: <boolean> = #t,
          name-first?  :: <boolean> = #f)
 => (string :: <string>)
  let prefix    = if (hide-prefix?) "" else locator-prefix(locator) end;
  let extension = if (hide-type?)   "" else locator-extension(locator) end;
  let suffix    = if (hide-suffix?) "" else locator-suffix(locator) end;
  let base      = base-as-string(locator-base(locator));
  let name+suffix = concatenate(base, extension, suffix);
  if (name-first?)
    if (hide-prefix?) name+suffix
    else concatenate(name+suffix, " (", prefix, ")") end
  else
    if (hide-prefix?) name+suffix
    else concatenate(prefix, name+suffix) end
  end
end method locator-pretty-name;

define sealed method locator-pretty-name
    (string :: <string>,
     #rest keys,
     #key hide-prefix? :: <boolean> = #f,
          hide-type?   :: <boolean> = #f,
          hide-suffix? :: <boolean> = #t,
          name-first?  :: <boolean> = #f)
 => (string :: <string>)
  ignore(hide-prefix?, hide-type?, hide-suffix?, name-first?);
  apply(locator-pretty-name, as(<locator>, string), keys)
end method locator-pretty-name;
