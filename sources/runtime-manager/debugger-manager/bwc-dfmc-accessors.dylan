module:        dm-internals
synopsis:      Functions for low-level access to the layout of dylan objects.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// WRITE-DYLAN-VALUE
//
define method write-dylan-value
    (ap :: <debug-target>, address :: <remote-location>, value :: <remote-value>)
      => (v :: <remote-value>, ok :: <boolean>)

  if (instance? (address, <active-remote-register>)
      | page-write-permission? (ap.debug-target-access-path, address))
    block ()
      values (write-value (ap.debug-target-access-path, address, value), #t);
    exception (x :: <remote-access-violation-error>)
      values (as-remote-value(0), #f);
    end block;
  else
    let spy-thread = select-thread-for-spy(ap);
    let result =
      if (spy-thread)
        run-spy-on-thread
          (ap, 
           spy-thread,
           ap.C-spy.write-location-through-barrier,
           address, 
           value);
      else
        #f
      end if;
    if (result)
      values (result, #t);
    else
      values (as-remote-value(0), #f);
    end if;
  end if;
end method;


//// BEGIN DEBUG CODE

define variable *fast-read-count* = 0;  // TODO: Remove this line
define variable *slow-read-count* = 0;  // TODO: Remove this line

//// END DEBUG CODE

///// READ-DYLAN-VALUE
//
define method read-dylan-value 
    (ap :: <debug-target>, address :: <remote-location>)
      => (v :: <remote-value>, ok :: <boolean>)

  // This is the quick method for reading a dylan value. This method can only be used
  // if it is known that there is no MM barrier on the address from which to read
  local method read-dylan-value-quick (ap, address)
    let val = as-remote-value(0);
    let success = #f;
    *fast-read-count* := *fast-read-count* + 1; // TODO: Remove this line.
    block ()
      val := read-value (ap.debug-target-access-path, address);
      success := #t;
    exception (x :: <remote-access-violation-error>)
      val := as-remote-value(0);
      success := #f;
    end block;
    values (val, success);
  end method;

  // If this is the first read in a new debugger transaction then flush the cache
  if (ap.new-debugger-transaction?)
    ap.pages-safe-to-read-cache := make (<set>);
    ap.new-debugger-transaction? := #f;
  end if;

  // A value can be read using the quick method if we know there is no MM barrier on
  // the address from which to read. MM barriers cover a whole page. The is no
  // barrier on an address when:
  //   1. The address is an active register
  //   2. The address's page is in the cache i.e. we've read from this page before in
  //      the current transaction
  //   3. The address's page is not read-protected
  // If it is determined there is no barrier on a page, it is guaranteed that no
  // barrier will be placed on the page during the remainder of the transaction.
  //
  if (instance? (address, <active-remote-register>))
    read-dylan-value-quick (ap, address);
  else
    let page-number = remote-address-page-number (ap.debug-target-access-path, address);
    if (member? (page-number, ap.pages-safe-to-read-cache))
      read-dylan-value-quick (ap, address);
    else
      if (page-read-permission? (ap.debug-target-access-path, address))
        add! (ap.pages-safe-to-read-cache, page-number);
        read-dylan-value-quick (ap, address);
      else
        // There is a barrier on the address to read from so the value must be read
        // in the context of the application. This is done by invoking a primitive in
        // the runtime using the spy. Reading the value in this way will result in
        // the barrier being removed. No other barriers will be erected.
        //
        let spy-thread = select-thread-for-spy(ap);
        let result = 
          if (spy-thread)
            run-spy-on-thread 
              (ap, spy-thread,
               ap.C-spy.read-location-through-barrier, address);
          else
            #f
          end if;
        *slow-read-count* := *slow-read-count* + 1;
        if (result)
          add! (ap.pages-safe-to-read-cache, page-number);
          values (result, #t);
        else
          values (as-remote-value(0), #f);
        end if;
      end if;
    end if;
  end if;
end method;

/*
///// TEMPORARY DEFINITIONS

define method read-dylan-value
    (application :: <debug-target>, address :: <remote-location>)
         => (value :: <remote-value>, worked? :: <boolean>)

  let value = as-remote-value(0);
  let worked? = #f;

  block ()
    value := read-value(application.debug-target-access-path, address);
    worked? := #t;
  exception (pants :: <remote-access-violation-error>)
    worked? := #f;
    value := as-remote-value(0);
  end block;

  values(value, worked?);
end method;
*/

///// TAGS
//    The DM is given knowledge of the dylan object tagging scheme employed
//    by the runtime.

define constant $dylan-tag-pointer = 0;
define constant $dylan-tag-integer = 1;
define constant $dylan-tag-character = 2;
define constant $dylan-tag-illegal = 3;

define method inspect-instance-tag
    (application :: <debug-target>, instance :: <remote-value>)
       => (tag-info :: <integer>)
  remote-value-low-order-bits(instance, 2)
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
  let (iclass, ok) = wrapper-to-iclass(ap, wrapper);
  if (ok)
    values(dylan-iclass-class(ap, iclass), #t)
  else
    values(as-remote-value(0), #f)
  end if  
end method;


///// WRAPPER-TO-ICLASS
//    Given an instance of a wrapper, find the implementation class, and
//    return a <remote-value>.

define method wrapper-to-iclass
    (ap :: <debug-target>, wrapper :: <remote-value>)
       => (c :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element(ap, wrapper, 0)
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


///// METHOD-MEP
//    Given an instance of <method>, return the method's MEP
//    as a <remote-value>.

define method method-mep 
    (ap :: <debug-target>, method-object :: <remote-value>)
       => (iep :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element (ap, method-object, 5);
end method;


///// DYLAN-LAMBDA-XEP
//    Given an instance of a callable object, return the XEP
//    as a <remote-value>.

define method dylan-lambda-xep
    (ap :: <debug-target>, lambda-object :: <remote-value>)
       => (iep :: <remote-value>, ok :: <boolean>)
  read-instance-slot-element (ap, lambda-object, 0);
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

define method dylan-vector-element 
    (ap :: <debug-target>, sov-instance :: <remote-value>,
     i :: <integer>) => (v :: <remote-value>)
  let (slot, ok) = read-instance-slot-element (ap, sov-instance, i + 1);
  if (ok)
    slot;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ENTRY-VECTOR-SIZE
//    Given an entry-vector instance, return its size as an integer.

define method dylan-entry-vector-size 
    (ap :: <debug-target>, sov-instance :: <remote-value>)
       => (i :: <integer>)
  let (size, ok) =
    read-instance-slot-element (ap, sov-instance, 1);

  if (ok)
    tagged-remote-value-as-integer(size);
  else
    0;
  end if;
end method;


///// DYLAN-ENTRY-VECTOR-ELEMENT
//    Given an entry-vector instance and an index, return the
//    appropriate vector element as a <remote-value>

define method dylan-entry-vector-element 
    (ap :: <debug-target>, sov-instance :: <remote-value>,
     i :: <integer>) => (v :: <remote-value>)
  let (slot, ok) = read-instance-slot-element (ap, sov-instance, i + 2);
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

define method dylan-tail 
    (ap :: <debug-target>, pair :: <remote-value>)
       => (val :: <remote-value>)
  let (tl, ok) = read-instance-slot-element (ap, pair, 1);
  if (ok)
    tl;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-CLASS
//    Given an implementation class instance, this returns the actual class
//    instance.

define method dylan-iclass-class
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (cls, ok) = read-instance-slot-element (ap, iclass-object, 0);
  if (ok)
    cls
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-CLASS-ICLASS
//    Given an actual class instance, returns its current implementation
//    class.

define method dylan-class-iclass
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  let (icls, ok) = read-instance-slot-element(ap, class-object, 2);
  if (ok)
    icls
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-ICLASS-INSTANCE-SLOT-DESCRIPTORS
//    Given a class instance, this returns an instance which shoul be a simple-
//    object-vector of slot descriptors.

define method dylan-iclass-instance-slot-descriptors
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (descrs, ok) = read-instance-slot-element (ap, iclass-object, 5);
  if (ok)
    descrs;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-ALL-SLOT-DESCRIPTORS
//    Returns all slot descriptors associated with a class.

define method dylan-iclass-all-slot-descriptors
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (descrs, ok) = read-instance-slot-element (ap, iclass-object, 11);
  if (ok)
    descrs;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-DIRECT-SLOT-DESCRIPTORS
//    Returns the descriptors for slots defined directly by this class
//    (ie, excluding inherited slots).

define method dylan-iclass-direct-slot-descriptors
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (descrs, ok) = read-instance-slot-element (ap, iclass-object, 10);
  if (ok)
    descrs;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-DIRECT-METHODS
//    Returns the direct methods on this class.

define method dylan-iclass-direct-methods
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (descrs, ok) = read-instance-slot-element (ap, iclass-object, 14);
  if (ok)
    descrs;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-DIRECT-SUPERCLASSES
//    What a bloody strange name...
//    Given a class instance, this returns an instance which should be a
//    vector of direct superclasses.

define method dylan-iclass-direct-superclasses
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (sups, ok) = read-instance-slot-element (ap, iclass-object, 7);
  if (ok)
    sups;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-ALL-SUPERCLASSES
//    Given a class instance, this returns an instance which should be a
//    sequence of superclasses

define method dylan-iclass-all-superclasses
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (sups, ok) = read-instance-slot-element (ap, iclass-object, 8);
  if (ok)
    sups;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-DIRECT-SUBCLASSES
//    Given a class instance, this returns an instance which should be a
//    sequence of subclasses

define method dylan-iclass-direct-subclasses
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (subs, ok) = read-instance-slot-element (ap, iclass-object, 9);
  if (ok)
    subs;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-MM-WRAPPER
//    Given a class instance, this returns the wrapper that direct
//    instances of the class will posess.

define method dylan-iclass-mm-wrapper
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (wrapper, ok) = read-instance-slot-element (ap, iclass-object, 1);
  if (ok)
    wrapper;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-REPEATED-SLOT-DESCRIPTOR
//    Given a class instance, returns the repeated slot descriptor.

define method dylan-iclass-repeated-slot-descriptor
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (descr, ok) = read-instance-slot-element (ap, iclass-object, 4);
  if (ok)
    descr;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-CLASS-INSTANCE-SLOT-DESCRIPTORS
//    Given a class instance, this returns an instance which shoul be a simple-
//    object-vector of slot descriptors.

define method dylan-class-instance-slot-descriptors
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-instance-slot-descriptors
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-ALL-SLOT-DESCRIPTORS
//    Returns all slot descriptors associated with a class.

define method dylan-class-all-slot-descriptors
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-all-slot-descriptors
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-DIRECT-SLOT-DESCRIPTORS
//    Returns the descriptors for slots defined directly by this class
//    (ie, excluding inherited slots).

define method dylan-class-direct-slot-descriptors
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-direct-slot-descriptors
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-DIRECT-METHODS
//    Returns the direct methods on this class.

define method dylan-class-direct-methods
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-direct-methods
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-DIRECT-SUPERCLASSES
//    What a bloody strange name...
//    Given a class instance, this returns an instance which should be a
//    vector of direct superclasses.

define method dylan-class-direct-superclasses
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-direct-superclasses
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-ALL-SUPERCLASSES
//    Given a class instance, this returns an instance which should be a
//    sequence of superclasses

define method dylan-class-all-superclasses
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-all-superclasses
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-DIRECT-SUBCLASSES
//    Given a class instance, this returns an instance which should be a
//    sequence of subclasses

define method dylan-class-direct-subclasses
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-direct-subclasses
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-MM-WRAPPER
//    Given a class instance, this returns the wrapper that direct
//    instances of the class will posess.

define method dylan-class-mm-wrapper
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-mm-wrapper
      (ap, dylan-class-iclass(ap, class-object))
end method;


///// DYLAN-CLASS-REPEATED-SLOT-DESCRIPTOR
//    Given a class instance, returns the repeated slot descriptor.

define method dylan-class-repeated-slot-descriptor
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-repeated-slot-descriptor
      (ap, dylan-class-iclass(ap, class-object))
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

define method dylan-string-data 
    (ap :: <debug-target>, string-instance :: <remote-value>)
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


///// DYLAN-DOUBLE-INTEGER-DATA
//    Given a remote <double-integer> object, return the high and low
//    words as <remote-value>.

define method dylan-double-integer-data
    (application :: <debug-target>, instance :: <remote-value>)
       => (hi :: <remote-value>, lo :: <remote-value>)
  let (lo, oklo)
    = read-instance-slot-element(application, instance, 0);
  let (hi, okhi)
    = read-instance-slot-element(application, instance, 1);
  if (oklo & okhi)
    values(hi, lo)
  else
    values(as-remote-value(0), as-remote-value(0))
  end if;
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
//        x := read-single-float (ap.debug-target-access-path,
//                                indexed-remote-value (float-instance, 1));
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
  let tag = inspect-instance-tag(ap, instance);
  let answer = #f;
  select (tag)
    $dylan-tag-illegal =>
      answer := #f;
    $dylan-tag-integer =>
      answer := #t;
    $dylan-tag-character =>
      answer := #t;
    $dylan-tag-pointer =>
      let (w, w-ok) = read-instance-header(ap, instance);
      let (ww, ww-ok) = read-instance-header(ap, w);
      let (www, www-ok) = read-instance-header(ap, ww);
      answer := (ww = www);
  end select;
  answer;
end method;


///// INSTANCE-SYMBOLIC-NAME
//    Given any direct (but untagged) instance, this attempts to find a
//    name in the symbol table whose definition points to this object.

define method dylan-instance-symbolic-name 
    (ap :: <debug-target>, untagged-instance :: <remote-value>)
       => (lib :: <string>, mod :: <string>, name :: <string>)
  let (closest, offset) = 
    symbol-table-symbol-relative-address
        (ap.debug-target-symbol-table, untagged-instance);

  if (closest)
    demangle-qualified-name(closest.remote-symbol-name)
  else
    values ("pants", "poo", "nothing");
  end if
end method;


///// DYLAN-SLOT-NAME
//    Given a <remote-value> corresponding to a dylan slot descriptor, returns
//    the name of the slot if it can be found.

define method dylan-slot-name 
    (ap :: <debug-target>, slot-descriptor :: <remote-value>)
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
  read-instance-slot-element(application, single, 1);
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
  let (rep, ok) =
    read-instance-slot-element (ap, svec-instance, 0);

  if (ok)
    rep;
  else
    0;
  end if;
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
  let (slot, ok) = read-instance-slot-element (ap, svr-instance, i + 2);
  if (ok)
    slot;
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
      read-instance-slot-element(application, table-vector, 8);
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
      read-instance-slot-element(application, table-vector, 9);
    if (ok)
      slot
    else
      as-remote-value(0)
    end if
  end if
end method;


///// DYLAN-SIGNATURE-PROPERTIES
//    Given any instance of <signature>, abstractify and return the
//    properties.

define method dylan-signature-properties
    (application :: <debug-target>, sig-instance :: <remote-value>)
 => (number-required :: <integer>, number-values :: <integer>)
  let (slot, ok) =
    read-instance-slot-element(application, sig-instance, 0);
  if (ok)
    let properties-integer =
      truncate/(remote-value-low-order-bits(slot, 16), 4);
    let number-required = logand(properties-integer, 255);
    let number-values = logand(truncate/(properties-integer, 16), 255);
    values(number-required, number-values);
  else
    values(0, 0)
  end if;
end method;


///// DYLAN-SIGNATURE-REQUIRED
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of required parameters.

define method dylan-signature-required
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (req :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 0);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-SIGNATURE-VALUES
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of value types.

define method dylan-signature-values
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (vals :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 1);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-KEYWORD-SIGNATURE-VALUES
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of value types.

define method dylan-keyword-signature-values
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (vals :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 1);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-SIGNATURE-REST-VALUE
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to #f, or the type of the #rest return value.

define method dylan-signature-rest-value
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (rst :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 2);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;

define method dylan-value-signature-rest-value
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (rst :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 2);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;

define method dylan-keyword-signature-rest-value
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (rst :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 2);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;

define method dylan-value-keyword-signature-rest-value
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (rst :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 2);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-SIGNATURE-KEYS
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of keywords
//    (Note the jump from slot 2 to slot 4 here. It isn't a mistake.
//    There's a "properties" field at position 3 that we are not interested
//    in).

define method dylan-signature-keys
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (kwds :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 4);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-SIGNATURE-KEY-TYPES
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of keyword argument types.

define method dylan-signature-key-types
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (ktypes :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 5);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-RANGE-START
//    Give a remote range instance, returns a <remote-value> for the number
//    at the start of the range.

define method dylan-range-start
    (application :: <debug-target>, range-instance :: <remote-value>)
        => (st :: <remote-value>)

  let (slot, ok)
    = read-instance-slot-element(application, range-instance, 0);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-RANGE-BY
//    Give a remote range instance, returns a <remote-value> for the number
//    at the step of the range.

define method dylan-range-by
    (application :: <debug-target>, range-instance :: <remote-value>)
        => (st :: <remote-value>)

  let (slot, ok)
    = read-instance-slot-element(application, range-instance, 1);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-RANGE-SIZE
//    Give a remote range instance, returns a <remote-value> for the number
//    at the size of the range.

define method dylan-range-size
    (application :: <debug-target>, range-instance :: <remote-value>)
        => (st :: <remote-value>)

  let (slot, ok)
    = read-instance-slot-element(application, range-instance, 2);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-SLOT-GETTER-FUNCTION
//    Given a remote slot descriptor, return the value for the getter 
//    function.

define method dylan-slot-getter-function
    (application :: <debug-target>, slot-instance :: <remote-value>)
         => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, slot-instance, 7);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-SLOT-SETTER-FUNCTION
//    Given a remote slot descriptor, return the value for the setter
//    function.

define method dylan-slot-setter-function
    (application :: <debug-target>, slot-instance :: <remote-value>)
         => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, slot-instance, 8);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-SLOT-OWNER-CLASS
//    Given a remote slot descriptor, return the value for the class that
//    defines this slot.

define method dylan-slot-owner-class
    (application :: <debug-target>, slot-instance :: <remote-value>)
         => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, slot-instance, 4);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-SLOT-INIT-KEYWORD
//    Given a remote slot descriptor, return the value for the init keyword.

define method dylan-slot-init-keyword
    (application :: <debug-target>, slot-instance :: <remote-value>)
         => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, slot-instance, 5);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-SLOT-INIT-KEYWORD-REQUIRED
//    Given a remote slot descriptor, return the remote equivalent of either
//    #f or #t.

define method dylan-slot-init-keyword-required
    (application :: <debug-target>, slot-instance :: <remote-value>)
         => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, slot-instance, 6);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-SLOT-INIT-VALUE
//    A <remote-value> for the init-data in a slot, if it is known.

define method dylan-slot-init-value
    (application :: <debug-target>, slot-instance :: <remote-value>)
         => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, slot-instance, 3);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-SLOT-SPECIALIZER
//    Given a remote slot descriptor, return the value for the type that
//    this slot is specialized on.

define method dylan-slot-specializer
    (application :: <debug-target>, slot-instance :: <remote-value>)
         => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, slot-instance, 9);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-LAMBDA-SIGNATURE
//    Given a remote instance of any callable object, return a pointer to
//    the signature.

define method dylan-lambda-signature
    (application :: <debug-target>, lambda-instance :: <remote-value>)
        => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, lambda-instance, 1);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-METHOD-KEYWORD-SPECIFIERS
//    Returns a pointer to the sequence of keyword specifiers for a method.

define method dylan-method-keyword-specifiers
    (application :: <debug-target>, method-instance :: <remote-value>)
        => (val :: <remote-value>)

  let (slot, ok) =
    read-instance-slot-element(application, method-instance, 6);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;
