module:      dfmc-mangling
author:      Tony "Obi-wan" Mann, Jonathan Bachrach, Paul Howard
synopsis:    Demangling support
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Note:
// So far, demangling is exclusively a debugging task. It need not be
// quite as concerned with efficiency as mangling. Nevertheless, I'm
// sure that there is room for improvement here.

define class <demangler> (<abstract-mangler>)
end class;

define constant $hygiene-marker-id? = curry(\==, $hygiene-char);
define constant $library-separator-id? = curry(\==, $library-marker);
define constant $module-separator-id? = curry(\==, $module-marker);
define constant $method-mangled-marker-id? = curry(\==, $method-mangled-marker);

define method initialize (mangler :: <demangler>, #key, #all-keys) => ()
  next-method();
  initialize-demangler-table(mangler);
end method;

define method initialize-demangler-table (mangler :: <demangler>) => ()
  let table = mangler-table(mangler);
  // many characters are unexpected
  for (i from $min-character-code to $max-character-code) 
    table[i] := #f; // means call a user-supplied handler function
  end for;
  // Fill in the escape
  table[as(<integer>, $escape-marker)] := demangle-escape;
  // fill in special cases
  for (mangle in $mangles-data)
    table[as(<integer>, mangle[1])] := mangle[0];
  end for;
  // fill C allowable versions
  for (i from as(<integer>, 'a') to as(<integer>, 'z'))
    table[i] := as(<character>, i);
  end for;
  for (i from as(<integer>, '0') to as(<integer>, '9'))
    table[i] := as(<character>, i);
  end for;
end method;

/*
// Demangler handler functions have the following signature:-
   (name :: <byte-string>,   // string being demangled
    pos :: <integer>,        // position of character being considered
    char :: <character>)     // the character
   =>
   (demangled-data,          // a string or character
    new-pos :: <integer>)    // position at which to resume the demangle

*/


define function demangle-escape
    (name :: <byte-string>, pos :: <integer>, char :: <character>)
    => (res :: <character>, new-pos :: <integer>)
  let code = 0;
  let limit = name.size;
  local method read-next (at :: <integer>) => (next :: <integer>)
          if (at < limit)
            let val = as(<integer>, name[at]) - as(<integer>, '0');
            if ((val >= 0) & (val <= 9))
              code := (code * 10) + val;
              read-next(at + 1);
            else at
            end if;
          else at
          end if;
        end method;
  let next = read-next(pos + 1);
  values(as(<character>, code), 
         if ((next < limit) & name[next] = $escape-marker) 
           next + 1 
         else next 
         end)
end function;

define function default-demangler-handler-function 
    (name :: <byte-string>, pos :: <integer>, ch :: <character>)
    => (data, pos)
  if (member?(ch, $all-decoration-markers))
    values("", pos + 1)
  else
    values(ch, pos + 1)   // This used to signal an error, but that's
                          // a bit extreme. Just copy the character.
  end if;
end function;

define method demangle-name-into 
    (mangler :: <demangler>, name :: <byte-string>, handler-function :: <function>)
  let pos = 0;
  let limit = name.size;
  while (pos < limit)
    let this = mangler-table(mangler)[as(<integer>, name[pos])];
    select (this by instance?)
      <character> =>
        pos := pos + 1;
        mangle-raw-into(mangler, this);
      <function> =>
        let (data, new-pos) = this(name, pos, name[pos]);
        pos := new-pos;
        mangle-raw-into(mangler, data);
      otherwise =>
        let (data, new-pos) = handler-function(name, pos, name[pos]);
        pos := new-pos;
        mangle-raw-into(mangler, data);
    end select;
  end while;
end method;


define method demangle-name-raw
   (mangler :: <demangler>, name :: <byte-string>, 
     #key handler-function = default-demangler-handler-function)
    => (res :: <byte-string>)
  mangler-reset(mangler);
  demangle-name-into(mangler, name, handler-function);
  mangler-as-string(mangler)
end method;


define method demangle-name-locally
   (mangler :: <demangler>, name :: <byte-string>, 
     #key handler-function = default-demangler-handler-function)
    => (res :: <byte-string>, hygienic-marker :: false-or(<integer>))
  let boundary = name.size - $local-suffix.size;
  let marker = #f;
  let stripped-name =
    if (copy-sequence(name, start: boundary) = $local-suffix)
      copy-sequence(name, end: boundary)
    else // look for a hygiene marker
      let hm = find-key(name, $hygiene-marker-id?);
      if (hm)
        marker := string-to-integer(copy-sequence(name, start: hm + 1));
        copy-sequence(name, end: hm)
      else name
      end if;
    end if;
  values(demangle-name-raw(mangler, stripped-name, 
                           handler-function: handler-function),
         marker);
end method;


define method demangle-binding-spread
   (mangler :: <demangler>, name :: <byte-string>, 
     #key handler-function = default-demangler-handler-function)
    => (var-name :: <byte-string>, 
        module-name :: false-or(<byte-string>), 
        library-name :: false-or(<byte-string>))
  let lsep        = find-key(name, $library-separator-id?);
  let binding-end = find-key(name, $method-mangled-marker-id?) | name.size;
  if (lsep & (binding-end > (lsep + 1)))
    let msep = find-key(name, $module-separator-id?);
    if (msep & (lsep > msep))
      let vname = copy-sequence(name, end: msep);
      let mname = copy-sequence(name, start: msep + 1, end: lsep);
      let lname = copy-sequence(name, start: lsep + 1, end: binding-end);
      values(demangle-name-raw(mangler, vname, handler-function: handler-function),
	     demangle-name-raw(mangler, mname, handler-function: handler-function),
	     demangle-name-raw(mangler, lname, handler-function: handler-function))
    else
      let vname  = copy-sequence(name, end: lsep);
      let dylan? = (name[lsep + 1] == $dylan-module-marker);
      let lname  =
	if (dylan?) "dylan"
	else copy-sequence(name, start: lsep + 1, end: binding-end)
	end if;
      let dlname = demangle-name-raw(mangler, lname, handler-function: handler-function);
      let dmname = 
	if (dylan?) as-lowercase(as(<string>, $demangle-dylan-module[name[lsep + 2]]))
	else dlname 
	end if;
      values(demangle-name-raw(mangler, vname, handler-function: handler-function),
	     dmname,
	     dlname)
    end if
  else
    values(demangle-name-raw(mangler, name, handler-function: handler-function),
	   #f, 
	   #f);
  end if;
end method;

/*
  *** STATUS NOTE
  *** So far, all of the functions below are used exclusively by
  *** the debugger-manager, and were added by phoward (24-JUN-98).
*/

///// DEMANGLER-EXTRACT-CHARACTERISTICS
//    Returns various interesting details about a mangled name.

define method demangler-extract-characteristics
    (demangler :: <demangler>, name :: <byte-string>)
  => (constant? :: <boolean>,
      wrapper? :: <boolean>,
      iep? :: <boolean>,
      method? :: <boolean>)
  if (name.size == 0)
    values(#f, #f, #f, #f)
  else
    values(name[0] == $constant-marker,
           name[name.size - 1] == $wrapper-marker,
           name[name.size - 1] == $iep-marker,
           member?($method-mangled-marker, name))
  end if;
end method;


///// DEMANGLER-EXTRACT-LIBRARY-NAME
//    Given any mangled name, extract the library that was
//    responsible for emitting it. 

define method demangler-extract-library-name
    (demangler :: <demangler>, name :: <byte-string>)
  => (extracted-name :: false-or(<byte-string>))
  let (constant?, wrapper?, iep?, method?)
    = demangler-extract-characteristics(demangler, name);
  if (method?)
    let (method-library, method-counter)
      = demangler-extract-method-details(demangler, name);
    method-library
  else
   let (var-name, module-name, library-name) 
     = demangle-binding-spread(demangler, name);
   library-name
  end if
end method;


///// DEMANGLER-EXTRACT-METHOD-DETAILS
//    Given the mangled name of a method object (or equivalent
//    entry point), extract out the details about the method.

define method demangler-extract-method-details
    (demangler :: <demangler>, name :: <byte-string>)
  => (method-library :: false-or(<byte-string>),
      method-index :: false-or(<byte-string>))
  let cname = demangler-extract-callable-object-name(demangler, name);
  let mm = find-key(cname, $method-mangled-marker-id?);
  if (mm)
    let detail-part = copy-sequence(cname, start: mm + 1);
    let mm2 = find-key(detail-part, $method-mangled-marker-id?);
    if (mm2)
      let method-lib-part =
        begin
          let lib-part = copy-sequence(detail-part, end: mm2);
          if (lib-part = "")
            let (n, m, l) = demangle-binding-spread(demangler, cname);
            l
          else
            lib-part
          end if;
        end;
      let method-num-part = copy-sequence(detail-part, start: mm2 + 1);
      values(method-lib-part & demangle-name-raw(demangler, method-lib-part),
             method-num-part)
    else
      values(#f, "0")
    end if
  else
    values(#f, "0")
  end if;
end method;


///// DEMANGLER-EXTRACT-GENERIC-FUNCTION-NAME
//    Given the mangled name of a method object (or equivalent
//    entry point), produce the mangled name of the associated
//    generic function.

define method demangler-extract-generic-function-name
    (demangler :: <demangler>, name :: <byte-string>)
  => (extracted-name :: <byte-string>)
  // Look for a mangled method marker.
  let mm = find-key(name, $method-mangled-marker-id?);
  if (mm)
    copy-sequence(name, end: mm)
  else
    name
  end if
end method;


///// DEMANGLER-EXTRACT-CALLABLE-OBJECT-NAME
//    Given the mangled name of an entry point, return the
//    mangled name of the associated callable object.
//    Status Note: Currently, this just means knocking off a
//                 trailing $IEP-MARKER character.

define method demangler-extract-callable-object-name
    (demangler :: <demangler>, name :: <byte-string>)
  => (extracted-name :: <byte-string>)
  let (constant?, wrapper?, iep?, method?)
    = demangler-extract-characteristics(demangler, name);
  if (iep?)
    copy-sequence(name, end: name.size - 1)
  else
    name
  end if
end method;
