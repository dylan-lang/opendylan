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
  let rs = make(<file-stream>, locator: as (<locator>, s), direction: #"input", element-type: <byte>);

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

define method load-code-from-stream (s :: <file-stream>)
              => top-level-forms :: <sequence> /* of <function> */;

  let loader = make(<doss-loader>, stream: s);
  let forms  = make(<deque>);

  push(forms,loader.fetch-object);
  while (~ s.stream-at-end?)
    push(forms,loader.fetch-next-object);
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

  idvm-report?(*standard-output*, "building method for %=\n", generic-name);
  let m = build-vm(as(<simple-object-vector>,params),
                   code-vector,
                   stack-size,
                   uses-next-method: uses-next-method?,
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
  idvm-report?(*standard-output*, "building closure for %=\n", generic-name);
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

define constant build-vm-generic-function = method(
                                        generic-name :: <string>,
                                        generic-function-methods :: <list>,
                                        number-required :: <integer>,
                                        uses-key?        :: <boolean>,
                                        uses-rest?        :: <boolean>,
                                        takes-all-keys?   :: <boolean>,
                                        function-properties :: <integer>)

  idvm-report?(*standard-output*, "building generic function for %=\n", generic-name);

   let gf = make(<generic-function>, number-required: number-required, key?: uses-key?, rest?: uses-rest?,
                 all-keys?: takes-all-keys?, debug-name: generic-name);

   for (amethod in generic-function-methods)
    add-method(gf, amethod);
   end for;

   gf;
end method;

define constant build-vm-class =
 method(class-name :: <string>,
	direct-superclasses :: <list>,
        slots :: <simple-object-vector>)

  idvm-report?(*standard-output*, "building class for %=\n", class-name);

  let the-class =
   make(<class>,
        superclasses: as(<vector>, direct-superclasses),
        slots: slots,
        debug-name: class-name);

   the-class;
  
end method;

define constant build-vm-singleton =
 method(object :: <object>)

  idvm-report?(*standard-output*, "building singleton for %=\n", object);

  make(<singleton>, object: object);

end method;


// Proxy HACK!!
define constant proxies        = make(<equal-table>);

define method make-variable (name :: <string>,
                             #key initial-value,
                                  object)
              => (reader :: <function>, writer :: <function>);

  let getter = method ( ) initial-value      end;
  let setter = method (v) initial-value := v end;

  if (object)
    object.&getter := getter;
    object.&setter := setter;
  else
    *current-namespace*[name] := make(<&object>, getter: getter, setter: setter);
  end if;
  values(getter, setter)
end method;

define method make-dummy-variable (name :: <string>)
              => (reader :: <function>, writer :: <function>);

  let initial-value = shes-not-there;
  let getter =
           method()
             if (initial-value == shes-not-there)
               query-error("IDVM ENGINE couldn't resolve %=\n", name);
             else
               initial-value;
             end if;
           end method;
  let setter =
           method(v)
               initial-value := v;
           end method;

  *current-namespace*[name] := make(<&object>, getter: getter, setter: setter);
  values(getter, setter)
end method;

/// Here we define the functions that resolve symbolically-dumped objects
/// 

define class <fixup-proxy> (<object>)
  slot value, init-function: method() shes-not-there; end method;
end class;

define method lookup-mangled(name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;

  let object = lookup-namespace(name, module-name, library-name);

  if (object == shes-not-there)
    idvm-report?(*standard-output*,"%s not found, making proxy\n", name);
    proxies[name] = make(<fixup-proxy>)
  else
    if (slot-initialized?(object, &value))  // predefined as a constant?
      object.&value;
    else
      if ((name = "_P_unbound")
          & (module-name = #"internal")
          & (library-name = #"dylan"))
        object.&value;
      else
        object.&value := object.&getter();
      end if;
    end if;
    idvm-report?(*standard-output*, "LOOKUP %= => %=\n", name, object.&value);
    object.&value;
  end if;
end method;


define method whinge-if-absent(name :: <string>, thing) => thing :: <object>;
  if (thing ~== shes-not-there)
    thing
  else
    query-error("IDVM loader couldn't resolve %=\n", name)
  end if
end method;

define method lookup-variable-reader(dummy-def, name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  idvm-report?(*standard-output*, "Looking up variable reader %=\n", name);
  let object = lookup-namespace(name, module-name, library-name);
  if (object == shes-not-there)
    make-dummy-variable(name);
  else
    if (slot-initialized?(object, &getter)) // predefined as a variable?
      object.&getter;
    else
      make-variable(name, initial-value: object.&value, object: object);
    end if;
  end if; 
end method;

define method lookup-variable-writer(dummy-def, name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  idvm-report?(*standard-output*, "Looking up variable writer %=\n", name);
  let object = lookup-namespace(name, module-name, library-name);
  if (object == shes-not-there)
    let (getter, setter) = make-dummy-variable(name);
    setter;
  else
    if (slot-initialized?(object, &setter)) // predefined as a variable?
      object.&setter;
    else
      let (getter, setter) = make-variable(name, initial-value: object.&value, object: object);
      setter;
    end if;
  end if; 
end method;

define method lookup-variable-definer(value :: <object>, name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  idvm-report?(*standard-output*, "Looking up variable definer %=\n", name);
  let object = lookup-namespace(name, module-name, library-name);
  if (object == shes-not-there)
    make-variable(name, initial-value: value);
  else
    if (slot-initialized?(object, &setter)) // predefined as a variable?
      object.&setter(value);
    else
      make-variable(name, initial-value: value, object: object);
    end if;
  end if;
  #f;
end method;

// Why do we need constant-definer & local-constant-definer?

define method lookup-constant-definer(value :: <object>, name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  let object = lookup-namespace(name, module-name, library-name);

  if (object ==  shes-not-there)
    idvm-report?(*standard-output*, "Installing constant definer %= => a %=\n", name, value.object-class);
    *current-namespace*[name] := make(<&object>, value: value);
    value;
  else
    if (slot-initialized?(object, &setter)) // predefined as a variable?
      object.&setter(value);
    end if;
    object.&value := value;
    idvm-report?(*standard-output*, "Overwriting constant definer %= => a %=\n", name, object.&value.object-class);
    value;
  end if
end method;

define constant congruent? =
method(gf1 :: <generic-function>, gf2 :: <generic-function>)
 let (gf1-req, gf1-rest, gf1-key) = gf1.function-arguments;
 let gf1-opt = gf1-rest | gf1-key;
 let (gf2-req, gf2-rest, gf2-key) = gf2.function-arguments;
 let gf2-opt = gf2-rest | gf2-key;
 gf1-req = gf2-req
 & ((gf1-opt & gf2-opt) | ~(gf1-opt | gf2-opt));
end method;

define method lookup-constant-definer(value :: <generic-function>, name :: <string>, module-name :: <symbol>, library-name :: <symbol>) => <object>;
  let object = lookup-namespace(name, module-name, library-name);


  if (object ==  shes-not-there)
    idvm-report?(*standard-output*,"Installing constant definer %= => a %=\n", name, value.object-class);
    *current-namespace*[name] := make(<&object>, value: value);
    value;
  else
   if (~ slot-initialized?(object, &value))
    object.&value := value;
   end if;
   if (slot-initialized?(object, &setter)) // predefined as a variable?
     object.&setter(value);
   end if;
   if (congruent?(object.&value, value))
     object.&value;
   else
    let (object-req, object-rest, object-key) = object.&value.function-arguments;
    let (value-req, value-rest, value-key) = value.function-arguments;
    cerror("Discard the current value and rebind to a new generic",
	   "Old Lambda list [required: %s #rest: %s #key: %s] is not congruent with the New lambda list [required: %s #rest: %s #key: %s] of %s",
           object-req, object-rest, object-key,
           value-req, value-rest, value-key,
           name);
     idvm-report?(*standard-output*,"Overwriting constant definer %= => a %=\n", name, object.&value.object-class);
     object.&value := value;
   end if;
  end if
end method;

/*******************************
define method const-keys(s :: <string>) => (s :: <sequence>);
  choose(method (key) subsequence-position(key,s) end,
         idvm-namespace.key-sequence)
end method;

define method constant-entry (s :: <string>) => (res :: <object>)
  let keys = const-keys(s);
  if (size(keys) == 1)
    idvm-namespace[keys[0]].&value
  else
    error("Ambiguous keys for %=. Found %=\n", s, keys)
  end if;
end method;


define method variable-reader-keys(s :: <string>) => (s :: <sequence>);
  choose(method (key) subsequence-position(key,s) end,
         idvm-namespace.key-sequence)
end method;

define method variable-writer-keys(s :: <string>) => (s :: <sequence>);
  choose(method (key) subsequence-position(key,s) end,
         idvm-namespace.key-sequence)
end method;

define method variable-entry (s :: <string>) => (res :: <object>)
  let keys = variable-reader-keys(s);
  if (size(keys) == 1)
    idvm-namespace[keys[0]].&getter()
  else
    error("Ambiguous keys for %=. Found %=\n", s, keys)
  end if;
end method;

define method variable-entry-setter (newval, s :: <string>) => (newval)
  let keys = variable-writer-keys(s);
  if (size(keys) == 1)
    idvm-namespace[keys[0]].&setter(newval);
    newval
  else
    error("Ambiguous keys for %=. Found %=\n", s, keys)
  end if;
end method;
***********************************/