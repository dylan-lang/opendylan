Module:    IDVM-loader
Language:  infix-dylan
Author:    Eliot Miranda
Synopsis:  IDVM code loader via DOSS for the emulator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Simple top-level utilities

define method load-code-from (s :: <string>)
              => top-level-forms :: <sequence> /* of <function> */;
  let rs = read-stream-over(as(<pathname>,s), element-type: #"byte");

  block ()
    load-code-from-stream(rs)
  cleanup
    close(rs)
  end block
end method;


define method post-load-fixup-proxies(vectors)
  for (code-vector in vectors)
    for (i from 0 below code-vector.size)
      let obj = code-vector[i];

      if (instance?(obj, <loaded-object-proxy>))
        code-vector[i] := obj.loaded-object;
      end if;
    end for;
  end for;
end method;

define method load-code-from-stream (s :: <stream>)
              => top-level-forms :: <sequence> /* of <function> */;

  let loader = make(<doss-loader>, stream: s);
  let forms  = make(<deque>);

  add!(forms,loader.fetch-object);
  while (~ s.stream-at-end?)
    add!(forms,loader.fetch-next-object);
  end while;

  close(s);

  as(<vector>,forms)
end method;


// Top-level method builder
define method build-vm-method  (params            :: <sequence>,
                                code-vector       :: <simple-object-vector>,
                                stack-size        :: <integer>,
                                generic-name      :: union(<symbol>,<string>),
                                uses-next-method? :: <boolean>,
                                uses-rest?        :: <boolean>,
                                takes-all-keys?   :: <boolean>,
                                key-value-pairs   :: union(<simple-object-vector>,singleton(#f)))
              => <function>;

  format-out("building method for %=\n", generic-name);
  let m = build-vm(as(<simple-object-vector>,params),
                   code-vector,
                   stack-size,
                   uses-next-method: #f,  // !@#$ TEMPORARY - FOR EMULATOR
                   uses-rest: uses-rest?,
                   takes-all-keys: takes-all-keys?,
                   key-value-pairs: key-value-pairs,
                   method-name: as(<symbol>, generic-name));
  m
end method;

define method build-vm-closure (params             :: <sequence>,
                                code-vector        :: <simple-object-vector>,
                                stack-size         :: <integer>,
                                generic-name       :: union(<symbol>,singleton(#f)),
                                uses-next-method? :: <boolean>,
                                uses-rest?        :: <boolean>,
                                takes-all-keys?   :: <boolean>,
                                key-value-pairs   :: union(<simple-object-vector>,singleton(#f)),
                                closed-var-indices :: <simple-object-vector>)
              => <function>;
  format-out("building closure for %=\n", generic-name);
  make(<vm-method-info>, 
       parameters: as(<simple-object-vector>,params),
       vm-code-vec: code-vector,
       stack-size: stack-size,
       closed-var-indices: closed-var-indices,
       uses-next-method: uses-next-method?,
       uses-rest: uses-rest?,
       takes-all-keys: takes-all-keys?,
       key-value-pairs: key-value-pairs,
       method-name: generic-name);
end method;


/* Overview of name-space scheme.

  Constants are held in a constant table, indexed by mangled name.
  Variables are closed-over by a getter function (0 args) and a setter
  function (1 arg) stored in the getter and setter tables.
*/

define constant constant-table = make(<equal-table>);
define constant getters        = make(<equal-table>);
define constant setters        = make(<equal-table>);

// Proxy HACK!!
define constant proxies        = make(<equal-table>);

define method make-variable (name :: <string>,
                             #key initial-value = #f)
              => (reader :: <function>, writer :: <function>);

  values(getters[name] := method ( ) initial-value      end,
         setters[name] := method (v) initial-value := v end)
end method;

/// Here we define the functions that resolve symbolically-dumped objects
/// 

define constant shes-not-there = list("Let me tell you 'bout the way she walks.");

define class <fixup-proxy> (<object>)
  slot value, init-value: shes-not-there;
end class;

define method lookup-mangled(name :: <string>) => <object>;

  let value = element(constant-table, name, default: shes-not-there);

  if (value ~== shes-not-there)
    format-out("LOOKUP %= => %=\n", name, value);
    value
  else
    format-out("%s not found, making proxy\n", name);
    proxies[name] = make(<fixup-proxy>)
  end if
end method;


define method whinge-if-absent(name :: <string>, thing) => thing :: <object>;
  if (thing ~== shes-not-there)
    thing
  else
    error("IDVM loader couldn't resolve %=\n", name)
  end if
end method;

define method lookup-variable-reader(name :: <string>, dummy-def) => <object>;
  format-out("Looking up variable reader %=\n",name);
  whinge-if-absent(name,element(getters, name, default: shes-not-there))  
end method;

define method lookup-variable-writer(name :: <string>, dummy-def) => <object>;
  format-out("Looking up variable writer %=\n",name);
  whinge-if-absent(name,element(setters, name, default: shes-not-there))  
end method;

define method lookup-variable-definer(name :: <string>, value :: <object>) => <object>;
  format-out("Looking up variable definer %=\n",name);
  make-variable(name, initial-value: value);
  #f;
end method;

// Why do we need constant-definer & local-constant-definer?

define method lookup-constant-definer(name :: <string>, value :: <object>) => <object>;
  let thing = element(constant-table, name, default: shes-not-there);

  if (thing ==  shes-not-there)
    format-out("Installing constant definer %= => a %=\n",name, value.object-class);
    constant-table[name] := value
  else
    format-out("Overwriting constant definer %= => a %=\n",name, thing.object-class);
    constant-table[name] := value
  end if
end method;



define method install-constant 
    (name :: <string>, value :: <object>) => ();
  constant-table[name] := value;
end method;

define method install-variable
    (name :: <string>, 
     variable-getter :: <function>, variable-setter :: <function>) 
    => ();
  getters[name] := variable-getter;
  setters[name] := variable-setter;
end method;

define method install-variable-reader
    (name :: <string>, variable-getter :: <function>) => ();
  getters[name] := variable-getter;
end method;




define method const-keys(s :: <string>) => (s :: <sequence>);
  choose(method (key) subsequence-position(key,s) end,
         constant-table.key-sequence)
end method;

define method constant-entry (s :: <string>) => (res :: <object>)
  let keys = const-keys(s);
  if (size(keys) == 1)
    constant-table[keys[0]]
  else
    error("Ambiguous keys for %=. Found %=\n", s, keys)
  end if;
end method;


define method variable-reader-keys(s :: <string>) => (s :: <sequence>);
  choose(method (key) subsequence-position(key,s) end,
         getters.key-sequence)
end method;

define method variable-writer-keys(s :: <string>) => (s :: <sequence>);
  choose(method (key) subsequence-position(key,s) end,
         setters.key-sequence)
end method;

define method variable-entry (s :: <string>) => (res :: <object>)
  let keys = variable-reader-keys(s);
  if (size(keys) == 1)
    getters[keys[0]]()
  else
    error("Ambiguous keys for %=. Found %=\n", s, keys)
  end if;
end method;

define method variable-entry-setter (newval, s :: <string>) => (newval)
  let keys = variable-writer-keys(s);
  if (size(keys) == 1)
    setters[keys[0]](newval);
    newval
  else
    error("Ambiguous keys for %=. Found %=\n", s, keys)
  end if;
end method;

