module: locator-internals
author: Tim McNerney
revised: 13-Feb-95 mf
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Paths

define method f-or-as 
    (class :: <class>, x) => (object)
  x & as (class, x)
end method;

define abstract class <path> (<mutable-sequence>)
  slot relative-path? :: <boolean> = #f,
    init-keyword: relative-path?:;
  slot elements :: <list>;
end class;

define method initialize 
    (path :: <path>, #rest keys, #key elements = #()) => ()
  apply(next-method, path, keys);
  if (path.relative-path? & empty?(elements))
    error("MAKE(<PATH>): relative path created with no elements")
  end;
  path.elements := as (<list>, elements);
end;

define method element
    (sequence :: <path>, key :: <object>, #rest all-keys, #key default)
 => (object :: <object>)
  apply(element, elements(sequence), key, all-keys)
end method element;

define method element-setter
    (new-value, sequence :: <path>, key :: <object>)
 => (object :: <object>)
  element(elements(sequence), key) := new-value;
end method element-setter;

define function path-elements 
    (path :: <path>) => (elements :: <list>)
  path.elements
end function;

define inline function path-next-state
    (collection :: <path>, state :: <list>) => (l :: <list>)
  state.tail
end function;

define inline function path-finished-state?
    (collection :: <path>, state :: <list>, limit) => (result :: <boolean>)
  state == #()
end function;

define inline function path-current-key
    (collection :: <path>, state :: <list>) => (result :: <integer>)
  iterate search (l :: <list> = elements(collection), k :: <integer> = 0)
    if (l == state)
      k
    else
      search(l.tail, k + 1)
    end if
  end iterate
end function;

define inline function path-current-element
    (collection :: <path>, state :: <list>) => (current-element)
  state.head
end function;

define inline function path-current-element-setter
    (current-element, collection :: <path>, state :: <list>)
 => (current-element)
  state.head := current-element
end function;

define method forward-iteration-protocol 
    (path :: <path>)
 => (initial-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(elements(path),
	 #(),
	 path-next-state,
	 path-finished-state?,
	 path-current-key,
	 path-current-element,
	 path-current-element-setter,
	 method (collection, state) => (value) state end)
end method;

define method as 
    (class == <string>, path :: <path>)
 => (string :: <string>)
  locator-error(<locator-print-error>,
		"AS <STRING> <PATH>:  Looks like you blew your class inheritance somewhere")
end method;

define method \= 
    (path1 :: <path>, path2 :: <path>) => (equal? :: <boolean>)
  object-class(path1) == object-class(path2)
  & relative-path?(path1) = relative-path?(path2)
  & elements(path1) = elements(path2);
end method;

define method unspecified-or-empty-path 
    (class :: subclass (<path>)) => (result)
  #f // make (class, elements: #(), relative-path?: #t)
end method;

/*
define method \= (path1 :: <path>, path2 == #f)
  path1.relative-path? & (path1.elements.empty?)
end method;

define method \= (path1 == #f, path2 :: <path>)
  \= (path2, #f)
end method;
*/

define method as 
    (class :: subclass(<path>), path :: <path>)
 => (path :: <path>)
  if (instance? (path, class))
    path
  else
    make (class, elements: path.elements, relative-path?: path.relative-path?)
  end
end method as;

define method as 
    (class :: subclass(<path>), x :: <string>)
 => (path :: <path>)
  make(class, elements: list(x));
end method as;

define method as
    (class :: subclass(<path>), x :: <sequence>)
 => (path :: <path>)
  make (class, elements: as (<list>, x));
end method as;

// wild?

define method wild-locator? 
    (path :: <path>) => (wild? :: <boolean>)
  let path-elements = elements(path);
  member?(#"wild-inferiors", path-elements)
    | member?(#"wild", path-elements)
end method;

/*
define method size>1? (list :: <empty-list>) => result :: singleton(#f);
  #f
end method;

define method size>1? (list :: <pair>) => result :: <boolean>;
  instance?(tail(list), <pair>)
end method;
*/

define method up
    (path :: <path>) => (new-path :: <path>)
  let size = size(elements(path));
  if (size = 0)
     locator-error(<locator-translation-error>, "UP applied to empty path", path);
  else
    make(object-class(path), 
	 elements: copy-sequence(elements(path), end: size - 1),
	 relative-path?: relative-path? (path));
  end if;
end method;  

define method down 
    (name, path :: false-or (<path>)) => (new-path)
  if (path)
    let elts = path.elements;
    let new-size = if (name) 1 + elts.size else elts.size end;
    let new-elements = make (type-for-copy (elts), size: new-size);
    replace-subsequence! (new-elements, elts);
    if (name)
      last (new-elements) := name;
    end;
    make (object-class (path), elements: new-elements,
	  relative-path?: relative-path? (path))
  else
    name
  end;
end method;

define method simplify-locator
    (path :: <path>) => (path :: <path>)
  // +++ if the FS doesn't support links then we can remove #"up"s too
  // can't make any assumptions here => identity function
  path;  
end method;

define method absolute-locator? 
    (path :: <path>) => (absolute? :: <boolean>)
  ~path.relative-path?
end method;

define method relative-locator? 
    (path :: <path>) => (relative? :: <boolean>)
  path.relative-path?
end method;

// Merge

define method merge-components 
    (path1 :: <path>, path2 :: <path>) 
 => (new-path :: <path>)
  if (path1.relative-path?)
    simplify-locator(make(object-class(path1),
			  elements: concatenate(path2.elements, path1.elements),
			  relative-path?: path2.relative-path?))
  else
    path1
  end
end method;

// abbreviate

/*
p: /a/b/c/d/e/
d: /a/b/c/d/
=> e/

p: /a/b/c/
d: /a/b/c/d/
=> ../
*/

define method abbreviate-internal 
    (p-list :: <list>, d-list :: <list>, #key test :: <function> = \=)
 => (result :: <list>)
  case
    p-list.empty? =>
      make (<list>, size: d-list.size, fill: #"parent");
    d-list.empty? =>
      p-list;
    p-list.head = d-list.head =>
      abbreviate-internal(p-list.tail, d-list.tail, test: test);
    otherwise =>
      concatenate(abbreviate-internal(#(), d-list, test: test), p-list);
  end
end method;

define method abbreviate-component
    (path :: <path>, default :: <path>, #key test :: <function> = \=)
 => (result :: false-or (<path>))
  case
    path.relative-path?    => path;
    default.relative-path? => path;
    path = default         => #f;
    otherwise =>
      let result-elements
	= abbreviate-internal(path.elements, default.elements, test: test);
      make(object-class(path), elements: result-elements, relative-path?: #t)
  end
end method;

// as <string> support

define method as-string-internal
    (path :: <path>, prefix :: <string>,
     delimiter :: <string>, suffix :: <string>,
     #key element-as-string = curry (as, <string>))
 => (string :: <string>)
  // This will probably cons too much for prime time
  let string :: <string> = prefix;
  for (rest = path.elements then rest.tail, until: rest.tail.empty?)
    string := concatenate(string, element-as-string (rest.head), delimiter);    
  finally
    if (rest.empty?)
      concatenate(string, suffix);
    else
      concatenate(string, element-as-string (rest.head), suffix);
    end;
  end;
end method;  

// specific paths

// directory paths

define class <directory-path> (<path>)
  // virtual constant slot might-have-links? :: <boolean>;
end class;

define method abbreviate-component
    (path1 :: <directory-path>, path2 :: <directory-path>,
     #key test :: <function> = \=)
 => (abbreviated-path :: false-or(<directory-path>))
  // Abbreviation only works on simplified locators
  // i.e. remove all . and .. entries before abbreviating
  let path1 = simplify-locator(path1);
  let path2 = simplify-locator(path2);
  if (path1 & path2 & path1 ~= path2)
    next-method(path1, path2, test: test)
  end
end method abbreviate-component;

define method might-have-links?
    (path :: <directory-path>) => (might-have-links? :: <boolean>)
  #t
end;

// slash-directory-path (used by abstract, and posix dir paths)

define class <slash-directory-path> (<directory-path>)
end class;

define method as
    (class == <string>, path :: <slash-directory-path>)
 => (string :: <string>)
  let absolute-slash = if (path.relative-path?) "" else "/" end;
  if (path.elements.empty?)
    absolute-slash
  else
    as-string-internal(path, absolute-slash, "/", "/",
		       element-as-string: directory-element-as-string)
  end if;
end method;

// simplify:  assumptions about links

define method simplify-locator
    (path :: <directory-path>) => (path :: false-or(<directory-path>))
  let elements = path.elements;
  let relative? = path.relative-path?;
  let new-elements
    = if (might-have-links?(path) | ~member?(#"parent", elements.tail))
	if (member?(#"self", elements))
	  remove(elements, #"self")
	else
	  elements
	end if
      else
	let result :: <list> = #();
	for (e in remove(elements, #"self"))
	  if ((e == #"parent") & ~result.empty? & (result.head ~= #"parent"))
	    result := result.tail
	  else
	    result := pair(e, result);
	  end if;
	end for;
	reverse!(result)
      end if;
  case
    new-elements = elements =>
      path;
    empty?(new-elements) & relative? =>
      #f;
    otherwise =>
      make(object-class(path),
	   elements: new-elements,
	   relative-path?: relative?);
  end
end method;

// posix directory

define class <posix-directory-path> (<slash-directory-path>)
end class;

// abstract directory (just like posix, but never has links)

define class <abstract-directory-path> (<slash-directory-path>)
end class;

define method might-have-links?
    (path :: <abstract-directory-path>) => (might-have-links? :: <boolean>)
  #f
end;

// backwards paths

define class <backwards-path> (<object>)
end class;

// host-paths

define class <host-path> (<backwards-path>, <path>)
end class;

define method as
    (class == <string>, path :: <host-path>)
 => (string :: <string>)
  as-string-internal (path, "", ".", "")
end method;

define method as
    (class == <host-path>, locator-string :: <string>)
 => (path :: <host-path>)
  // !!! This doesn't look right
  // locator-host (as (<locator>, locator-string))
  // It wasn't, maybe this'll work
  grok-abstract-host (locator-string)
end method;

define method as
    (class == <host-path>, locator :: <locator>)
 => (value :: <host-path>)
  //---*** Does locator-host always return a <host-path>?
  locator-host(locator)
end method;

// type-paths

define class <type-path> (<backwards-path>, <path>)
end class;

// This is pretty useless unless you know what extension naming
// conventions there are, so don't define it here
/*
define method as (class == <string>, path :: <type-path>) => (value)
  as-string-internal(path, ".", ".");
end method;
*/

// posix-type-paths

define class <posix-type-path> (<type-path>)
end class;

define method as
    (class == <string>, path :: <posix-type-path>)
 => (string :: <string>)
  if (path.elements.empty?)
    ""
  else
    as-string-internal(path, ".", ".", "",
		       element-as-string: posix-type-as-extension)
  end if;
end method;

// matching

define class <match-path> (<path>)
end class;

define method pair-if 
    (first, rest)
 => (result :: false-or (<list>))
  rest & pair (first, rest)
end;

define method match-internal 
    (p-list :: <list>, c-list :: <list>, seen-**?)
 => (result :: false-or (<list>))
  if (p-list.empty?)
    c-list.empty? & #()
  else
    let p-first = p-list.head;
    if (seen-**? & (p-first == #"wild" | p-first == #"wild-inferiors"))
      locator-error (<locator-merge-error>,
		     "MATCH: Can't have ** followed by * in same pattern")
    elseif (c-list.empty?)
      (p-first == #"wild-inferiors") & #()
    else
      select (p-first)
	#"wild" =>
	  pair-if (c-list.head, match-internal (p-list.tail, c-list.tail, #f));
	#"wild-inferiors" =>
	  pair-if (c-list.head, match-internal (p-list.tail, c-list, #t))
	    | pair-if (c-list.head, match-internal (p-list, c-list.tail, #f));
	otherwise =>
	  (p-first = c-list.head)
	    & match-internal (p-list.tail, c-list.tail, seen-**?);
      end select
    end if
  end if
end method;

define method match 
    (pattern :: <path>, candidate :: <path>)
 => (result :: type-union(<match-path>, singleton(#"fail")))
// => (result :: fail-or(<match-path>))   -- this would be more logical
  if (pattern.relative-path? ~= candidate.relative-path?)
    #"fail"
  else
    let result-elements = match-internal (pattern.elements, candidate.elements, #f);
    if (result-elements)
      make (<match-path>, elements: result-elements)
    else
      #"fail"
    end
  end
end method;

define method match 
    (pattern == #f, candidate :: <path>)
 => (result :: singleton (#"fail"))
  #"fail"
end method;

define method match 
    (pattern :: <path>, candidate == #f)
 => (result :: false-or(singleton (#"fail")))
  select ((pattern.elements.size = 1)  &  pattern.elements.head)
    #"wild" , #"wild-inferiors" => #f;
    otherwise => #"fail";
  end
end method;

// instantiation

define method instantiate-internal 
    (p-list :: <list>, m-list :: <list>)
 => (result :: <list>)
  if (p-list.empty?)
    #()
  else
    select (p-list.head)
      #"wild" =>
	if (m-list.empty?)
	  locator-error (<locator-merge-error>,
			 "INSTANTIATE:  Ran out of match elements for *")
	else
	  pair (m-list.head, instantiate-internal (p-list.tail, m-list.tail));
	end;
      #"wild-inferiors" =>
	concatenate (m-list, instantiate-internal (p-list.tail, #()));
      otherwise =>
	pair (p-list.head, instantiate-internal (p-list.tail, m-list));
    end select
  end if
end method;

define method instantiate
    (pattern :: <path>, match :: <match-path>)
 => (result :: <path>)
  make (object-class (pattern),
	elements: instantiate-internal (pattern.elements, match.elements),
	relative-path?: pattern.relative-path?)
end method;

define method instantiate 
    (pattern :: <path>, match == #f)
 => (result :: <path>)
  if (wild-locator? (pattern))
    instantiate (pattern, make (<match-path>, elements: #()))
  else
    pattern
  end if
end method;


