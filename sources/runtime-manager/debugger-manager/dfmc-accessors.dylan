module:        dm-internals
synopsis:      Functions for low-level access to the layout of dylan objects.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// SLOT POSITIONS

define constant $iclass-mm-wrapper-offset = 2;
define constant $iclass-repeated-slot-descriptor = 3;
define constant $iclass-instance-slot-descriptors-offset = 4;
define constant $iclass-direct-superclasses-offset = 7;
define constant $iclass-all-superclasses-offset = 8;
define constant $iclass-direct-subclasses-offset = 13;
define constant $iclass-direct-methods-offset = 14;
define constant $iclass-direct-slot-descriptors-offset = 15;
define constant $iclass-all-slot-descriptors-offset = 16;
define constant $iclass-class-storage-offset = 21;

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
      let (quick-val, quick-success) = read-dylan-value-quick(ap, address);
      if (page-read-permission? (ap.debug-target-access-path, address))
        add! (ap.pages-safe-to-read-cache, page-number);
        values(quick-val, quick-success);
      else
        // We have to read this location via the spy, but there's no
        // point in even attempting this if the "quick" call resulted in
        // a remote-access-violation. The spy function will just
        // signal access-violations itself, and we don't want that.
        let spy-thread = select-thread-for-spy(ap);
        let result = 
          if (spy-thread & quick-success)
            run-spy-on-thread 
              (ap, spy-thread,
               ap.C-spy.read-location-through-barrier, address);
          else
            #f
          end if;
        if (result)
          add! (ap.pages-safe-to-read-cache, page-number);
          values (result, #t);
        else
          values(quick-val, quick-success);
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
define constant $dylan-tag-boolean = 3;

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
         read-instance-slot-element (ap, siggy, 1);
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
  read-instance-slot-element (ap, method-object, 2);
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
  read-instance-slot-element (ap, gf, 4);
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


///// DYLAN-MULTIDIMENSIONAL-ARRAY-SIZE
//    Given a <multidimensional-array> instance, return its total
//    number of elements as an integer.

define method dylan-multidimensional-array-size
    (ap :: <debug-target>, mda-instance :: <remote-value>)
       => (i :: <integer>)
  let (size, ok) =
    read-instance-slot-element(ap, mda-instance, 1);
  if (ok)
    tagged-remote-value-as-integer(size)
  else
    0
  end if
end method;


///// DYLAN-MULTIDIMENSIONAL-ARRAY-DIMENSIONS
//    Given a <multidimensional-array> instance, return its
//    dimensions. (Just returns a remote-value that points to
//    the sequence of dimensions. The caller needs to pick this apart).

define method dylan-multidimensional-array-dimensions
    (ap :: <debug-target>, mda-instance :: <remote-value>)
       => (vec :: <remote-value>)
  let (vec, ok) =
    read-instance-slot-element(ap, mda-instance, 0);
  if (ok)
    vec
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-MULTIDIMENSIONAL-ARRAY-ROW-MAJOR-ELEMENT
//    Given a <multidimensional-array> instance, return the
//    nth row-major element of the array.

define method dylan-multidimensional-array-row-major-element
    (ap :: <debug-target>, mda-instance :: <remote-value>, n :: <integer>)
       => (data :: <remote-value>)
  let (data, ok) =
    read-instance-slot-element(ap, mda-instance, 2 + n);
  if (ok)
    data
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-LIMITED-ARRAY-SIZE
//    Given a <limited-array> instance, return its total
//    number of elements as an integer.

define method dylan-limited-array-size
    (ap :: <debug-target>, mda-instance :: <remote-value>)
       => (i :: <integer>)
  let (size, ok) =
    read-instance-slot-element(ap, mda-instance, 2);
  if (ok)
    tagged-remote-value-as-integer(size)
  else
    0
  end if
end method;


///// DYLAN-LIMITED-ARRAY-ELEMENT-TYPE
//    Given a simple-element-type-array instance, return its element-type.

define method dylan-limited-array-element-type 
    (ap :: <debug-target>, instance :: <remote-value>)
       => (i :: <remote-value>)
  let (slot, ok) = read-instance-slot-element(ap, instance, 0);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if;
end method;


///// DYLAN-LIMITED-ARRAY-DIMENSIONS
//    Given a <limited-array> instance, return its
//    dimensions. (Just returns a remote-value that points to
//    the sequence of dimensions. The caller needs to pick this apart).

define method dylan-limited-array-dimensions
    (ap :: <debug-target>, mda-instance :: <remote-value>)
       => (vec :: <remote-value>)
  let (vec, ok) =
    read-instance-slot-element(ap, mda-instance, 1);
  if (ok)
    vec
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-LIMITED-ARRAY-ROW-MAJOR-ELEMENT
//    Given a <limited-array> instance, return the
//    nth row-major element of the array.

define method dylan-limited-array-row-major-element
    (ap :: <debug-target>, mda-instance :: <remote-value>, n :: <integer>)
       => (data :: <remote-value>)
  let (data, ok) =
    read-instance-slot-element(ap, mda-instance, 3 + n);
  if (ok)
    data
  else
    as-remote-value(0)
  end if
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


///// DYLAN-LIMITED-VECTOR-SIZE
//    Given a simple-element-type-vector instance, return its size as an integer.

define method dylan-limited-vector-size 
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


///// DYLAN-LIMITED-VECTOR-ELEMENT-TYPE
//    Given a simple-element-type-vector instance, return its element-type.

define method dylan-limited-vector-element-type 
    (ap :: <debug-target>, instance :: <remote-value>)
       => (i :: <remote-value>)
  let (slot, ok) = read-instance-slot-element(ap, instance, 0);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if;
end method;


///// DYLAN-TABLE-SIZE
//    Returns the number of entries in a table.
//    NB: This method is awfully aware of how the table data structure
//        is implemented, and knows the location of several slots.

define method dylan-table-size
    (ap :: <debug-target>, table-instance :: <remote-value>)
  => (i :: <integer>)
  block ()
    let tvec = read-instance-slot-element(ap, table-instance, 1);
    let additions = read-instance-slot-element(ap, tvec, 4);
    let deletions = read-instance-slot-element(ap, tvec, 6);
    let sz =
      tagged-remote-value-as-integer(additions) -
        tagged-remote-value-as-integer(deletions);
    sz
  exception(<remote-access-violation-error>)
    0
  end block
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


///// DYLAN-LIMITED-VECTOR-ELEMENT
//    Given a simple-element-type-vector instance and an index, return the
//    appropriate vector element as a <remote-value>

define method dylan-limited-vector-element 
    (ap :: <debug-target>, sov-instance :: <remote-value>,
     i :: <integer>) => (v :: <remote-value>)
  let (slot, ok) = read-instance-slot-element (ap, sov-instance, i + 2);
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
  let (cls, ok) = read-instance-slot-element (ap, iclass-object, 1); // XX 1
  if (ok)
    cls
  else
    as-remote-value(0)
  end if
end method;

/*
///// DYLAN-CLASS-DEBUG-NAME
//    Given an actual class instance, returns the debug name. This will
//    actually replicate the slot data, rather than returning the remote
//    value.

define method dylan-class-debug-name
    (application :: <debug-target>, class-instance :: <remote-value>)
  => (debug-name :: false-or(<string>))
  let debug-name-data = 
     read-instance-slot-element(application, class-instance, 1);
  let classification =
     classify-dylan-object(application, debug-name-data);
  if (classification == $boolean-false)
    #f
  else
    dylan-string-data(application, debug-name-data)
  end if
end method;
*/

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


///// DYLAN-ICLASS-CLASS-STORAGE
//    Given a class instance, this returns the vector of class storage
//    spaces.

define method dylan-iclass-class-storage
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (spaces, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-class-storage-offset);
  if (ok)
    spaces;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-ICLASS-INSTANCE-SLOT-DESCRIPTORS
//    Given a class instance, this returns an instance which shoul be a simple-
//    object-vector of slot descriptors.

define method dylan-iclass-instance-slot-descriptors
    (ap :: <debug-target>, iclass-object :: <remote-value>)
       => (val :: <remote-value>)
  let (descrs, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-instance-slot-descriptors-offset);
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
  let (descrs, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-all-slot-descriptors-offset); 
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
  let (descrs, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-direct-slot-descriptors-offset);
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
  let (descrs, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-direct-methods-offset); 
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
  let (sups, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-direct-superclasses-offset);
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
  let (sups, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-all-superclasses-offset);
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
  let (subs, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-direct-subclasses-offset);
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
  let (wrapper, ok) 
    = read-instance-slot-element
        (ap, iclass-object, $iclass-mm-wrapper-offset); 
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
  let (descr, ok)
    = read-instance-slot-element
        (ap, iclass-object, $iclass-repeated-slot-descriptor); 
  if (ok)
    descr;
  else
    as-remote-value(0);
  end if
end method;

/*
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
*/

///// DYLAN-CLASS-ALL-SUPERCLASSES
//    Given a class instance, this returns an instance which should be a
//    sequence of superclasses

define method dylan-class-all-superclasses
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-all-superclasses
      (ap, dylan-class-iclass(ap, class-object))
end method;

/*
///// DYLAN-CLASS-DIRECT-SUBCLASSES
//    Given a class instance, this returns an instance which should be a
//    sequence of subclasses

define method dylan-class-direct-subclasses
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-direct-subclasses
      (ap, dylan-class-iclass(ap, class-object))
end method;
*/

///// DYLAN-CLASS-MM-WRAPPER
//    Given a class instance, this returns the wrapper that direct
//    instances of the class will posess.

define method dylan-class-mm-wrapper
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-mm-wrapper
      (ap, dylan-class-iclass(ap, class-object))
end method;

/*
///// DYLAN-CLASS-REPEATED-SLOT-DESCRIPTOR
//    Given a class instance, returns the repeated slot descriptor.

define method dylan-class-repeated-slot-descriptor
    (ap :: <debug-target>, class-object :: <remote-value>)
       => (val :: <remote-value>)
  dylan-iclass-repeated-slot-descriptor
      (ap, dylan-class-iclass(ap, class-object))
end method;
*/


///// DYLAN-SLOT-GETTER
//    Given a slot descriptor instance, returns the "getter" object, presumably
//    of type <function>.

define method dylan-slot-getter
   (ap :: <debug-target>, slot-descriptor :: <remote-value>)
      => (getter-function :: <remote-value>)
  let (getter, ok) = read-instance-slot-element (ap, slot-descriptor, 4); // XX 4
  if (ok)
    getter;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-STRING-DATA
//    Given a <byte-string> instance, returns a local copy.

define method dylan-string-data 
    (ap :: <debug-target>, string-instance :: <remote-value>,
     #key clip-at = #f)
       => (val :: <byte-string>, clipped? :: <boolean>)
  let (size, ok) = read-instance-slot-element (ap, string-instance, 0);
  let size = tagged-remote-value-as-integer(size);
  let clipped? = #f;
  if (clip-at & (size > clip-at))
    clipped? := #t;
    size := clip-at;
  end if;
  if (ok)
    values(
      read-byte-string (ap.debug-target-access-path, 
                        indexed-remote-value (string-instance, 2),
                        size),
      clipped?)
  else
    values("", #f);
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
       => (data :: <double-integer>)
  let (lo, oklo)
    = read-instance-slot-element(application, instance, 0);
  let (hi, okhi)
    = read-instance-slot-element(application, instance, 1);
  make(<double-integer>,
       high: hi, low: lo);
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

define method dylan-single-float-data 
    (application :: <debug-target>, float-instance :: <remote-value>)
       => (val :: <single-float>)
  let x :: <single-float> = 0.0;
  let float-address = indexed-remote-value(float-instance, 1);
  block ()
    x := read-single-float
           (application.debug-target-access-path, float-address);
  exception (<remote-access-violation-error>)
    x := 0.0;
  end block;
  x
end method;


///// DYLAN-DOUBLE-FLOAT-DATA
//    Given a double-float instance, returns a local copy.

define method dylan-double-float-data 
    (application :: <debug-target>, float-instance :: <remote-value>)
       => (val :: <double-float>)
  let x :: <double-float> = as(<double-float>, 0.0);
  let float-address = indexed-remote-value(float-instance, 1);
  block ()
    x := read-double-float
           (application.debug-target-access-path, float-address);
  exception (<remote-access-violation-error>)
    x := as(<double-float>, 0.0);
  end block;
  x
end method;

 
///// DYLAN-OBJECT?
//    Returns #t if the supplied instance looks like a dylan object.
//    Strategy: If the object is tagged as an integer or character, then
//    it is a dylan object. Otherwise, it is treated as a pointer to
//    a dylan object, which should have a header pointing to a valid 
//    wrapper...

define method dylan-object? 
    (ap :: <debug-target>, instance :: <remote-value>, #key address? :: <boolean>)
 => (val :: <boolean>)
  let tag = inspect-instance-tag(ap, instance);
  select (tag)
    $dylan-tag-boolean =>
      if (ap.static-object-directory.booleans-tagged?)
        ~address?
      end;
    $dylan-tag-integer =>
      ~address?;
    $dylan-tag-character =>
      ~address?;
    $dylan-tag-pointer =>
      let (w, w-ok) = read-instance-header(ap, instance);
      if (w-ok)
        let (ww, ww-ok) = read-instance-header(ap, w);
        if (ww-ok)
          let (www, www-ok) = read-instance-header(ap, ww);
          if (www-ok)
	    ww = www
	      & address-corresponds-to-primitive?
	          (ap, ww, ap.wrapper-wrapper-primitive)
	  end if
	end if
      end if;
  end select;
end method;


///// INSTANCE-SYMBOLIC-NAME
//    Given any direct (but untagged) instance, this attempts to find a
//    name in the symbol table whose definition points to this object.

define method dylan-instance-symbolic-name 
    (ap :: <debug-target>, untagged-instance :: <remote-value>)
       => (lib :: <string>, 
           mod :: <string>, 
           name :: <string>,
           exact? :: <boolean>)
  let (closest, offset) = 
    symbol-table-symbol-relative-address
        (ap.debug-target-symbol-table, untagged-instance);
  if (closest)
    let (lib, mod, name) = demangle-qualified-name(closest.remote-symbol-name);
    values(lib, mod, name, offset == 0);
  else
    values ("???", "???", "???");
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


///// DYLAN-UNION-TYPE-FIRST-MEMBER
//    Given a remote instance of <union>, returns a remote instance of the
//    first member type.

define method dylan-union-type-first-member
    (application :: <debug-target>, union :: <remote-value>)
 => (member-1 :: <remote-value>)
  let (slot, ok)
    = read-instance-slot-element(application, union, 1);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-UNION-TYPE-SECOND-MEMBER
//    Given a remote instance of <union>, returns a remote instance of the
//    second member type.

define method dylan-union-type-second-member
    (application :: <debug-target>, union :: <remote-value>)
 => (member-2 :: <remote-value>)
  let (slot, ok)
    = read-instance-slot-element(application, union, 2);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-SUBCLASS-TYPE-CLASS
//    Given a remote instance of <subclass>, returns a remote instance of
//    <class> that the subclass type is referring to.

define method dylan-subclass-type-class
    (application :: <debug-target>, subcltype :: <remote-value>)
 => (clss :: <remote-value>)
  let (slot, ok)
    = read-instance-slot-element(application, subcltype, 1);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
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
    as-remote-value(0);
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


///// DYLAN-LIMITED-STRETCHY-VECTOR-REPRESENTATION
//    Given a stretchy element-type vector representation instance, returns the filled
//    size of the stretchy vector

define method dylan-limited-stretchy-vector-representation
    (ap :: <debug-target>, svec-instance :: <remote-value>)
       => (rep :: <remote-value>)
  let (rep, ok) =
    read-instance-slot-element (ap, svec-instance, 1);

  if (ok)
    rep;
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-LIMITED-STRETCHY-VECTOR-SIZE
//    Given a stretchy vector representation instance, returns the filled
//    size of the stretchy vector

define method dylan-limited-stretchy-vector-size 
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


///// DYLAN-LIMITED-STRETCHY-VECTOR-ELEMENT-TYPE
//    Given a stretchy-element-type-vector instance, return its element-type.

define method dylan-limited-stretchy-vector-element-type 
    (ap :: <debug-target>, instance :: <remote-value>)
       => (i :: <remote-value>)
  let (slot, ok) = read-instance-slot-element(ap, instance, 0);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if;
end method;


///// DYLAN-LIMITED-STRETCHY-VECTOR-ELEMENT
//    Given a stretchy-element-type-vector-representation instance and an index, 
//    return the appropriate vector element as a <remote-value>

define method dylan-limited-stretchy-vector-element 
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
  let (slot, ok) = read-instance-slot-element(application, table-instance, 1);
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


///// DYLAN-WRAPPER-PROPERTIES
//    Given any instance of <mm-wrapper>, abstractify and return a
//    summary of information associated with the objects that it
//    wraps.

define method dylan-wrapper-properties
    (application :: <debug-target>, wrapper :: <remote-value>)
 => (fixed-slot-count :: <integer>,
     vector-scaling :: <integer>)
  let version-word-address = indexed-remote-value(wrapper, 4);
  let field-word-address = indexed-remote-value(wrapper, 3);
  let field-word =
    read-value(application.debug-target-access-path, field-word-address);
  let version-word = 
    read-value(application.debug-target-access-path, version-word-address);
  let fixed-slot-count =
    tagged-remote-value-as-integer(field-word);
  let vector-indicator =
    remote-value-low-order-bits(version-word, 3);
  let vector-scaling =
    if (vector-indicator == 7)
      0
    elseif ((vector-indicator == 4) | (vector-indicator == 5))
      1
    else
      4
    end if;
  values(fixed-slot-count, vector-scaling);
end method;


///// DYLAN-SIGNATURE-PROPERTIES
//    Given any instance of <signature>, abstractify and return the
//    properties.

define method dylan-signature-properties
    (application :: <debug-target>, sig-instance :: <remote-value>)
 => (number-required :: <integer>, 
     number-values :: <integer>,
     rest? :: <boolean>, 
     keys? :: <boolean>, 
     all-keys? :: <boolean>,
     default-rest? :: <boolean>,
     default-values? :: <boolean>)
  let (slot, ok) =
    read-instance-slot-element(application, sig-instance, 0);
  if (ok)
    let properties-integer =
      truncate/(remote-value-low-order-bits(slot, 27), 4);
    let number-required = logand(properties-integer,                 #xff);
    let number-values =   logand(truncate/(properties-integer, 256), #xff);
    let rest-bit =        logand(properties-integer,                 #x40000);
    let keys-bit =        logand(properties-integer,                 #x10000);
    let all-keys-bit =    logand(properties-integer,                 #x20000);
    let default-rest-bit = logand(properties-integer,               #x200000);
    let default-vals-bit = logand(properties-integer,               #x400000); 
    values(number-required, 
           number-values, 
           rest-bit > 0, 
           keys-bit > 0, 
           all-keys-bit > 0,
           default-rest-bit > 0,
           default-vals-bit > 0);
  else
    values(0, 0, #f, #f, #f, #f, #f)
  end if;
end method;


///// DYLAN-SIGNATURE-REQUIRED
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of required parameters.

define method dylan-signature-required
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (req :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 1);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-DEQUE-REPRESENTATION
//    Returns the implementation object from a deque instance.

define method dylan-deque-representation
    (application :: <debug-target>, instance :: <remote-value>)
  => (rep :: <remote-value>)
  let (slot, ok) = read-instance-slot-element(application, instance, 1);
  if (ok) slot else as-remote-value(0) end if
end method;


///// DYLAN-DEQUE-REPRESENTATION-PROPERTIES
//    Returns the first-index, last-index and current capacity of
//    an island-deque representation.

define method dylan-deque-representation-properties
    (application :: <debug-target>, rep :: <remote-value>)
  => (capacity :: <integer>, first :: <integer>, last :: <integer>)
  let first-int = read-instance-slot-element(application, rep, 0);
  let last-int = read-instance-slot-element(application, rep, 1);
  let size-int = read-instance-slot-element(application, rep, 2);
  values
    (tagged-remote-value-as-integer(size-int),
     tagged-remote-value-as-integer(first-int),
     tagged-remote-value-as-integer(last-int));
end method;


///// DYLAN-DEQUE-SIZE
//    Returns the number of elements in a deque instance.

define method dylan-deque-size
    (application :: <debug-target>, instance :: <remote-value>)
   => (size :: <integer>)
  let rep = dylan-deque-representation(application, instance);
  let (cap, first, last) = 
    dylan-deque-representation-properties(application, rep);
  last - first + 1
end method;


///// DYLAN-DEQUE-ELEMENT
//    Returns an indexed element from a deque instance.

define method dylan-deque-element
    (application :: <debug-target>, instance :: <remote-value>,
     i :: <integer>)
   => (elt :: <remote-value>)
  let rep = dylan-deque-representation(application, instance);
  let (cap, first, last) = 
    dylan-deque-representation-properties(application, rep);
  let (el, ok) =
    read-instance-slot-element(application, rep, 3 + first + i);
  if (ok) el else as-remote-value(0) end if;
end method;


///// DYLAN-SIGNATURE-VALUES
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of value types.

define method dylan-signature-values
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (vals :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 2);
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
     read-instance-slot-element(application, sig-instance, 4);
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
     read-instance-slot-element(application, sig-instance, 3);
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
     read-instance-slot-element(application, sig-instance, 4);
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
     read-instance-slot-element(application, sig-instance, 5);
  if (ok)
    slot
  else
    as-remote-value(0);
  end if;
end method;


///// DYLAN-SIGNATURE-KEYS
//    Given an instance of <signature>, return a <remote-value> corresponding
//    to the sequence of keywords

define method dylan-signature-keys
    (application :: <debug-target>, sig-instance :: <remote-value>)
       => (kwds :: <remote-value>)

  let (slot, ok) =
     read-instance-slot-element(application, sig-instance, 2);
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
     read-instance-slot-element(application, sig-instance, 3);
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
    read-instance-slot-element(application, slot-instance, 4); // XX 4
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
    read-instance-slot-element(application, slot-instance, 5); // XX 5
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
    read-instance-slot-element(application, slot-instance, 2); // XX 2
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
    read-instance-slot-element(application, slot-instance, 3); // XX 3
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
    read-instance-slot-element(application, slot-instance, 1); // XX 1
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
    read-instance-slot-element(application, slot-instance, 6); // XX 6
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
    read-instance-slot-element(application, method-instance, 4);
  if (ok)
    slot
  else
    as-remote-value(0)
  end if
end method;
