module:        dm-internals
synopsis:      Functions for low-level access to the layout of dylan objects.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// READ-DYLAN-VALUE
//    Wraps up read-value from access path. Handles the access-violation
//    condition.
//    TODO: Check page protection on the address. If the page is guarded,
//          the value must be read within the context of the application
//          via a function in the spy.

define variable success = #f;
define variable val = as-remote-value(0);

define method read-dylan-value 
    (ap :: <debug-target>, address :: <remote-location>)
      => (v :: <remote-value>, ok :: <boolean>)
  success := #f;
  val := as-remote-value(0);
  block ()
    val := read-value (ap.debug-target-access-path, address);
    success := #t;
  exception (x :: <remote-access-violation-error>)
    val := as-remote-value(0);
    success := #f;
  end block;
  values (val, success);
end method;


///// READ-INSTANCE-HEADER
//    Given an instance of any dylan object, read the header field and
//    return it as a remote value. "ok" determines whether the read was
//    successful.

define method read-instance-header 
    (ap :: <debug-target>, object :: <remote-value>)
       => (v :: <remote-value>, ok :: <boolean>)
  read-dylan-value (ap, indexed-remote-value (object, 0));
end method;


///// READ-INSTANCE-SLOT-ELEMENT
//    Given an instance of any dylan object, return the i'th slot value
//    as a <remote-value>

define method read-instance-slot-element 
    (ap :: <debug-target>, object :: <remote-value>, i :: <integer>)
       => (v :: <remote-value>, ok :: <boolean>)
  read-dylan-value (ap, indexed-remote-value (object, i + 1));
end method;


///// WRAPPER-TO-CLASS
//    Given an instance of a wrapper, find the class and return it as
//    a <remote-value>.

define method wrapper-to-class 
    (ap :: <debug-target>, wrapper :: <remote-value>)
       => (c :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element (ap, wrapper, 0)
end method;


///// GET-METHOD-SPECIALIZERS
//    Given a (remote)  instance of <method>, return the vector of specializers
//    as a <remote-value>.

define method get-method-specializers 
    (ap :: <debug-target>, method-object :: <remote-value>)
      => (s :: <remote-value>, ok :: <boolean>)

  let (siggy, ok) =
     read-instance-slot-element (ap, method-object, 1);

  if (ok)
     let (required-args, ok2) =
         read-instance-slot-element (ap, siggy, 0);
     if (ok2)
        values (required-args, #t)
     else
        values (as-remote-value(0), #f)
     end if
  else
     values (as-remote-value(0), #f)
  end if
end method;


///// METHOD-IEP
//    Given an instance of <method>, return the method's IEP
//    as a <remote-value>.

define method method-iep 
    (ap :: <debug-target>, method-object :: <remote-value>)
      => (iep :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element (ap, method-object, 3);
end method;


///// GF-METHODS
//    Given a remote instance of <generic-function>, returns the vector
//    of methods as a <remote-value>

define method gf-methods 
    (ap :: <debug-target>, gf :: <remote-value>)
       => (m :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element (ap, gf, 6);
end method;


///// DYLAN-INTEGER-DATA
//    Untag a dylan integer and return it as an actual <integer>

define method dylan-integer-data 
    (ap :: <debug-target>, integer-instance :: <remote-value>)
       => (i :: <integer>)
  tagged-remote-value-as-integer (integer-instance)
end method;


///// DYLAN-CHARACTER-DATA
//    Untag a dylan character and return it as an actual <character>

define method dylan-character-data 
    (ap :: <debug-target>, character-instance :: <remote-value>)
       => (c :: <character>)
  tagged-remote-value-as-character (character-instance)
end method;


///// DYLAN-VECTOR-SIZE
//    Given a simple-object-vector instance, return its size as an integer.

define method dylan-vector-size 
    (ap :: <debug-target>, sov-instance :: <remote-value>)
       => (i :: <integer>)
       let (size, ok) =
            read-instance-slot-element (ap, sov-instance, 0);

       if (ok)
            tagged-remote-value-as-integer(size);
       else
            0;
       end if;
end method;


///// DYLAN-VECTOR-ELEMENT
//    Given a simple-object-vector instance and an index, return the
//    appropriate vector element as a <remote-value>

define method dylan-vector-element (ap :: <debug-target>,
                                    sov-instance :: <remote-value>,
                                    i :: <integer>) => (v :: <remote-value>)

       let (slot, ok) = read-instance-slot-element (ap, sov-instance, i + 1);
       if (ok)
          slot;
       else
          as-remote-value(0);
       end if
end method;


///// DYLAN-HEAD
//    Given an instance of a dylan pair, return the first element as a
//    <remote-value>

define method dylan-head (ap :: <debug-target>, pair :: <remote-value>)
                            => (val :: <remote-value>)

       let (hd, ok) = read-instance-slot-element (ap, pair, 0);
       if (ok)
          hd;
       else
          as-remote-value(0);
       end if
end method;


///// DYLAN-TAIL
//    Given an instance of a dylan pair, return the second element as a
//    <remote-value>

define method dylan-tail (ap :: <debug-target>, pair :: <remote-value>)
                            => (val :: <remote-value>)

       let (tl, ok) = read-instance-slot-element (ap, pair, 1);
       if (ok)
          tl;
       else
          as-remote-value(0);
       end if
end method;


///// DYLAN-CLASS-NON-CLASS-DESCRIPTORS
//    What a bloody strange name...
//    Given a class instance, this returns an instance which shoul be a simple-
//    object-vector of slot descriptors.

define method dylan-class-non-class-descriptors
                (ap :: <debug-target>, class-object :: <remote-value>)
                 => (val :: <remote-value>)

       let (descrs, ok) = read-instance-slot-element (ap, class-object, 15);
       if (ok)
          descrs;
       else
          as-remote-value(0);
       end if
end method;


///// DYLAN-CLASS-DIRECT-SUPERCLASSES
//    What a bloody strange name...
//    Given a class instance, this returns an instance which should be a
//    vector of direct superclasses.

define method dylan-class-direct-superclasses
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  let (sups, ok) = read-instance-slot-element (ap, class-object, 1);
  if (ok)
    sups;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-CLASS-ALL-SUPERCLASSES
//    Given a class instance, this returns an instance which should be a
//    sequence of superclasses

define method dylan-class-all-superclasses
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  let (sups, ok) = read-instance-slot-element (ap, class-object, 2);
  if (ok)
    sups;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-CLASS-MM-WRAPPER
//    Given a class instance, this returns the wrapper that direct
//    instances of the class will posess.

define method dylan-class-mm-wrapper
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  let (wrapper, ok) = read-instance-slot-element (ap, class-object, 12);
  if (ok)
    wrapper;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-CLASS-REPEATED-SLOT-DESCRIPTOR
//    Given a class instance, returns the repeated slot descriptor.

define method dylan-class-repeated-slot-descriptor
                (ap :: <debug-target>, class-object :: <remote-value>)
                 => (val :: <remote-value>)

       let (descr, ok) = read-instance-slot-element (ap, class-object, 14);
       if (ok)
          descr;
       else
          as-remote-value(0);
       end if
end method;


///// DYLAN-SLOT-GETTER
//    Given a slot descriptor instance, returns the "getter" object, presumably
//    of type <function>.

define method dylan-slot-getter
               (ap :: <debug-target>, slot-descriptor :: <remote-value>)
                => (getter-function :: <remote-value>)

       let (getter, ok) = read-instance-slot-element (ap, slot-descriptor, 7);
       if (ok)
          getter;
       else
          as-remote-value(0);
       end if
end method;


///// DYLAN-STRING-DATA
//    Given a <byte-string> instance, returns a local copy.

define method dylan-string-data (ap :: <debug-target>,
                                 string-instance :: <remote-value>)
                                 => (val :: <byte-string>)

       let (size, ok) = read-instance-slot-element (ap, string-instance, 0);
       size := tagged-remote-value-as-integer(size);
       if (ok)
          read-byte-string (ap.debug-target-access-path, 
                            indexed-remote-value (string-instance, 2),
                            size);
       else
          "";
       end if;
end method;


///// DYLAN-HANDLER-TYPE
//    Given a remote <handler> instance, returns a remote <type> instance.

define method dylan-handler-type
  (application :: <debug-target>, handler-instance :: <remote-value>)
    => (type-instance :: <remote-value>)
  let (type-val, ok) =
    read-instance-slot-element(application, handler-instance, 0);
  if (ok)
    type-val;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-HANDLER-FUNCTION
//    Given a remote <handler> instance, returns a remote <function> instance.

define method dylan-handler-function
  (application :: <debug-target>, handler-instance :: <remote-value>)
    => (function-instance :: <remote-value>)
  let (function-val, ok) =
    read-instance-slot-element(application, handler-instance, 1);
  if (ok)
    function-val;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-HANDLER-TEST
//    Given a remote <handler> instance, returns a remote <function> instance.

define method dylan-handler-test
  (application :: <debug-target>, handler-instance :: <remote-value>)
    => (test-instance :: <remote-value>)
  let (test-val, ok) =
    read-instance-slot-element(application, handler-instance, 2);
  if (ok)
    test-val;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-HANDLER-INIT-ARGUMENTS
//    Given a remote <handler> instance, returns a remote <vector> instance.

define method dylan-handler-init-arguments
  (application :: <debug-target>, handler-instance :: <remote-value>)
    => (vector-instance :: <remote-value>)
  let (vector-val, ok) =
    read-instance-slot-element(application, handler-instance, 3);
  if (ok)
    vector-val;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-THREAD-NAME
//    Given a remote <thread> instance, returns the name of the thread
//    as a <remote-value>

define method dylan-thread-name
  (application :: <debug-target>, thread-instance :: <remote-value>)
    => (string-instance :: <remote-value>)
  let (string-instance, ok) =
    read-instance-slot-element(application, thread-instance, 3);
  if (ok)
    string-instance;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-SIMPLE-CONDITION-FORMAT-STRING
//    Given a remote <simple-condition> instance, returns a remote <string>

define method dylan-simple-condition-format-string
   (application :: <debug-target>, condition-instance :: <remote-value>)
     => (string-instance :: <remote-value>)
  let (string-val, ok) =
    read-instance-slot-element(application, condition-instance, 0);
  if (ok)
    string-val;
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-SIMPLE-CONDITION-FORMAT-ARGUMENTS
//    Given a remote <simple-condition> instance, returns a remote <vector>

define method dylan-simple-condition-format-arguments
   (application :: <debug-target>, condition-instance :: <remote-value>)
     => (vector-instance :: <remote-value>)
  let (vector-val, ok) =
    read-instance-slot-element(application, condition-instance, 1);
  if (ok)
    vector-val;
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-SINGLE-FLOAT-DATA
//    Given a single-float instance, returns a local copy.

define method dylan-single-float-data (ap :: <debug-target>,
                                       float-instance :: <remote-value>)
                                       => (val :: <single-float>)

       let x :: <single-float> = 0.0;
//       block ()
          x := read-single-float (ap.debug-target-access-path,
                                  indexed-remote-value (float-instance, 1));
//       exception (pants :: <remote-access-violation-error>) 
//          x := 0.0;
//       end block;
       x;
end method;

 
///// DYLAN-OBJECT?
//    Returns #t if the supplied instance looks like a dylan object.
//    Strategy: If the object is tagged as an integer or character, then
//    it is a dylan object. Otherwise, it is treated as a pointer to
//    a dylan object, which should have a header pointing to a valid 
//    wrapper...

define method dylan-object? (ap :: <debug-target>, instance :: <remote-value>)
                             => (val :: <boolean>)

   let integer-representation = as-integer(instance);

   if (logand (integer-representation, 3) > 0)

      // This instance has a tag.
      if (logand (integer-representation, 1) > 0)
         // Integer instance
         #t
      elseif (logand (integer-representation, 2) > 0)
         // Character instance
         #t
      else
         // I think this can't happen!!!
         #f
      end if

   else

      // This is an untagged instance.
      // If it's a dylan object, it's header should point to a wrapper, and the
      // header of the wrapper should point back to itself. This algorithm
      // tests for this, returning false if any reads fail.

      let (wrapper, ok1) = read-instance-header (ap, instance);
      if (ok1)
         let (wrapper-wrapper, ok2) = read-instance-header (ap, wrapper);
         if (ok2)
            let (wrapper-wrapper-header, ok3) = 
                  read-instance-header (ap, wrapper-wrapper);
            if ((ok3) & (wrapper-wrapper = wrapper-wrapper-header))
               #t
            else
               #f
            end if
         else
            #f
         end if
      else
         #f
      end if

   end if

end method;


///// DYLAN-INSTANCE-SYMBOLIC-NAME
//    Given any direct (but untagged) instance, this attempts to find a
//    name in the symbol table whose definition points to this object.

define method dylan-instance-symbolic-name 
    (ap :: <debug-target>, untagged-instance :: <remote-value>)
       => (lib :: <string>, mod :: <string>, name :: <string>)
  let (closest, offset) = 
    symbol-relative-address(ap.debug-target-access-path, untagged-instance);
  if (closest)
    demangle-qualified-name(closest.remote-symbol-name)
  else
    values ("pants", "poo", "nothing");
  end if
end method;


///// DYLAN-SLOT-NAME
//    Given a <remote-value> corresponding to a dylan slot descriptor, returns
//    the name of the slot if it can be found.

define method dylan-slot-name (ap :: <debug-target>,
                               slot-descriptor :: <remote-value>)
                               => (name :: <string>)

       let (lib, mod, nm) =
         dylan-instance-symbolic-name (ap,
             dylan-slot-getter (ap, slot-descriptor));
       nm;
end method;


///// DYLAN-SYMBOL-NAME
//    Given a remote instance of <symbol>, returns a remote instance of
//    <byte-string> naming the symbol.

define method dylan-symbol-name (ap :: <debug-target>,
                                 sym :: <remote-value>)
                                 => (name :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element (ap, sym, 0);
end method;


///// DYLAN-SINGLETON-OBJECT
//    Given a remote instance of <singleton>, returns a remote instance of
//    <object> that the singleton type represents.

define method dylan-singleton-object
    (application :: <debug-target>, single :: <remote-value>)
       => (remote-instance :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element(application, single, 0);
end method;


///// DYLAN-MACHINE-WORD-DATA
//    Given a remote instance of <machine-word>, returns the <remote-value>
//    that holds it.

define method dylan-machine-word-data
    (application :: <debug-target>, mw :: <remote-value>)
       => (remote-word :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element(application, mw, 0);
end method;


///// DYLAN-STRETCHY-VECTOR-REPRESENTATION
//    Given a stretchy vector representation instance, returns the filled
//    size of the stretchy vector

define method dylan-stretchy-vector-representation
    (ap :: <debug-target>, svec-instance :: <remote-value>)
       => (rep :: <remote-value>)
  svec-instance;
end method;


///// DYLAN-STRETCHY-VECTOR-SIZE
//    Given a stretchy vector representation instance, returns the filled
//    size of the stretchy vector

define method dylan-stretchy-vector-size 
    (ap :: <debug-target>, svr-instance :: <remote-value>)
       => (i :: <integer>)
  let (size, ok) =
    read-instance-slot-element (ap, svr-instance, 0);

  if (ok)
    tagged-remote-value-as-integer(size);
  else
    0;
  end if;
end method;


///// DYLAN-STRETCHY-VECTOR-ELEMENT
//    Given a stretchy-vector-representation instance and an index, return the
//    appropriate vector element as a <remote-value>

define method dylan-stretchy-vector-element 
    (ap :: <debug-target>, svr-instance :: <remote-value>,
     i :: <integer>) => (v :: <remote-value>)
  let (vec, ok) = read-instance-slot-element (ap, svr-instance, 1);
  if (ok)
    dylan-vector-element(ap, vec, i);
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-TABLE-VECTOR
//    Given a remote instance of <table>, returns the appropriate
//    <table-vector> instance.

define method dylan-table-vector
    (application :: <debug-target>, table-instance :: <remote-value>)
        => (table-vector :: <remote-value>)
  let (slot, ok) = read-instance-slot-element(application, table-instance, 0);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-TABLE-KEYS-VECTOR
//    Given a remote instance of <table>, returns the vector instance that
//    holds the keys.

define method dylan-table-keys-vector
    (application :: <debug-target>, table-instance :: <remote-value>)
       => (keys-vector :: <remote-value>)
  let table-vector = dylan-table-vector(application, table-instance);
  if (table-vector = as-remote-value(0))
    table-vector
  else
    let (slot, ok) = 
      read-instance-slot-element(application, table-vector, 9);
    if (ok)
      slot
    else
      as-remote-value(0)
    end if
  end if
end method;


///// DYLAN-TABLE-VALUES-VECTOR
//    Given a remote instance of <table>, returns the vector instance that
//    holds the values.

define method dylan-table-values-vector
    (application :: <debug-target>, table-instance :: <remote-value>)
       => (keys-vector :: <remote-value>)
  let table-vector = dylan-table-vector(application, table-instance);
  if (table-vector = as-remote-value(0))
    table-vector
  else
    let (slot, ok) = 
      read-instance-slot-element(application, table-vector, 10);
    if (ok)
      slot
    else
      as-remote-value(0)
    end if
  end if
end method;
