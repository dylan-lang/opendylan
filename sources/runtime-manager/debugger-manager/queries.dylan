module:         dm-internals
synopsis:       Various queries about remote dylan objects
author:         Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// The following constant is used to terminate iterations along
//    CONS-cell chains. 
//    This is completely arbitrary, but there is no practical way to
//    determine that a list is circular at the moment.

define constant $possible-circularity-punt-value = 5000;


///// REMOTE-INSTANCE?
//    Attempts to detect whether one object is a general instance of
//    another. This is a "static analysis" implementation, which is
//    convenient in that it doesn't upset the state of the application.
//    However, a better implementation of this might be to run
//    the instance?-iep within the application, or to create a more
//    primitive spy function (perhaps in HARP) to do an instance test.

define method remote-instance?
    (application :: <debug-target>, instance :: <remote-value>,
     class-instance :: <remote-value>)
       => (answer :: <boolean>)
  let answer = #f;
  block ()

    // Find out what the broad "type" of the dylan object is (making
    // sure that it is indeed a dylan object!). At the moment, return
    // #f if the object is tagged. Also return #f if the supposed class
    // is in fact not a class at all.

    let instance-classification = 
      classify-dylan-object(application, instance);
    let class-classification = 
      classify-dylan-object(application, class-instance);
    if (instance-classification == $character-type)
      let character-class =      
        lookup-static-object
           (application, "<character>", "dylan");
      answer := (character-class & (character-class = class-instance));
    elseif (instance-classification == $integer-type)
      let integer-class =      
        lookup-static-object
           (application, "<integer>", "dylan");
      answer := (integer-class & (integer-class = class-instance));
    elseif (instance-classification == $unknown-type)
      answer := #f
    elseif (class-classification ~== $class-type)
      answer := #f
    else

      // If the object is a direct instance of the class, there will be
      // a pointer ID between the object's wrapper, and the wrapper used
      // by the class.
      // Otherwise, find out what class the object is really an instance
      // of, and then decide whether this class is a subclass of the class
      // given to the query.
      // If, at any time, a remote access violation occurs, just return
      // #f.

      let (instance-wrapper, ok) =
        read-instance-header(application, instance);
      let class-wrapper =
        dylan-class-mm-wrapper(application, class-instance);
      if (ok & (instance-wrapper = class-wrapper))
        answer := #t
      else
        let (actual-class, ok) = 
          wrapper-to-class(application, instance-wrapper);
        answer := remote-subclass?(application, actual-class, class-instance);
      end if;
    end if;
  exception (pants :: <remote-access-violation-error>)
    answer := #f
  end block;
  answer
end method;


///// DYLAN-VALUE-UNBOUND?
//    Attempts to detect whether an object corresponds to the runtime's
//    canonical unbound value.

define method dylan-value-unbound?
    (application :: <debug-target>, instance :: <remote-value>)
       => (answer :: <boolean>)
  instance = lookup-static-object(application, "%unbound", "internal")
end method;


///// DYLAN-RUNTIME-UNBOUND-MARKER
//    Returns the value that should be used as the canonical "unbound"
//    marker.
//    Usage Note: The interactive downloader (a DM client) uses this function
//    to "fill up" regions of memory with %unbound.

define method dylan-runtime-unbound-marker
    (application :: <debug-target>) => (marker :: <remote-value>)
  lookup-static-object(application, "%unbound", "internal")
end method;


///// REMOTE-SUBCLASS?
//    Determines whether one remote value is a subclass of another.

define method remote-subclass?
    (application :: <debug-target>, sub :: <remote-value>, 
     super :: <remote-value>)
       => (answer :: <boolean>)

  // Both remote values must be classifiable as classes for this
  // to work. Simply test whether 'super' is contained within
  // the list of all 'sub's superclasses. (With a quick test to
  // see whether the two classes are ID to each other, in which
  // case we return #t straight away).

  let type-sub = classify-dylan-object(application, sub);
  let type-super = classify-dylan-object(application, super);
  if ((type-sub == $class-type) & (type-super == $class-type))
    if (sub = super)
      #t
    else
      let all-supers = dylan-class-all-superclasses(application, sub);
      let super-seq = canonicalize-sequence(application, all-supers);
      let super-count = size(super-seq);
      let i = 0;
      let found = #f;
      while ((i < super-count) & (~found))
        if (super-seq[i] = super)
          found := #t
        else
          i := i + 1
        end if;
      end while;
      found;
    end if;
  else
    #f
  end if
end method;


///// REMOTE-MEMBER?
//    Again, performs static analysis. This time, an object is compared
//    against some kind of collection. The DM interprets the collection as
//    it sees fit to determine whether the object is a member of that
//    collection.

define method remote-member?
    (application :: <debug-target>, instance :: <remote-value>,
     collection-instance :: <remote-value>)
        => (answer :: <boolean>)
  let (keys, vals) = 
     remote-collection-inspect(application, collection-instance);
  member?(instance, vals, test: \=);
end method;


///// REMOTE-COLLECTION-INSPECT
//    Given any kind of collection, builds up two parallel sequences of
//    keys and elements. Both sequences contain <remote-value> objects.
//    For anything other than an explicitly-keyed collection, the first
//    sequence will contain (remote) integer values.

define method remote-collection-inspect
    (application :: <debug-target>, collection-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), 
            value-sequence :: <sequence>)

  // Find out the type of the instance. If it is not a collection of
  // any sort we know about, there's not much we can do!

  let instance-type = 
    classify-dylan-object(application, collection-instance);

  // And now dispatch to a more specific function.

  select (instance-type)

    $empty-list-type =>
      values(#f, #[]);

    $pair-type =>
      remote-list-inspect(application, collection-instance,
                          first-index: first-index, last-index: last-index);

    $vector-type =>
      remote-vector-inspect(application, collection-instance,
                          first-index: first-index, last-index: last-index);

    $array-type =>
      remote-array-linear-inspect
         (application, collection-instance, 
          first-index: first-index, last-index: last-index);

    $stretchy-vector-type =>
      remote-stretchy-vector-inspect(application, collection-instance,
                          first-index: first-index, last-index: last-index);

    $string-type =>
      remote-byte-string-inspect(application, collection-instance,
                          first-index: first-index, last-index: last-index);

    $deque-type =>
      remote-deque-inspect
        (application, collection-instance,
         first-index: first-index, last-index: last-index);

    $table-type =>
      remote-table-inspect(application, collection-instance,
                          first-index: first-index, last-index: last-index);

    $string-table-type =>
      remote-table-inspect(application, collection-instance,
                          first-index: first-index, last-index: last-index);

    $limited-vector-type =>
      remote-limited-vector-inspect
	(application, collection-instance,
	 first-index: first-index, last-index: last-index);

    $limited-array-type =>
      remote-limited-array-linear-inspect
         (application, collection-instance, 
          first-index: first-index, last-index: last-index);

    $limited-stretchy-vector-type =>
      remote-limited-stretchy-vector-inspect
	(application, collection-instance,
	 first-index: first-index, last-index: last-index);

    otherwise =>
      values(#f, #[]);  // Two empty sequences is better than an error!

  end select;

end method;

define method remote-vector-inspect
    (application :: <debug-target>, vector-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)
  let limit = dylan-vector-size(application, vector-instance);
  unless (last-index & (last-index < (limit - 1)))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := 
      dylan-vector-element(application, vector-instance, i);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;

define method remote-limited-vector-inspect
    (application :: <debug-target>, vector-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)
  let limit = dylan-limited-vector-size(application, vector-instance);
  unless (last-index & (last-index < (limit - 1)))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := 
      dylan-limited-vector-element(application, vector-instance, i);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;

define method remote-array-linear-inspect
    (application :: <debug-target>, array-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)
  let limit = dylan-multidimensional-array-size(application, array-instance);
  unless (last-index & (last-index < (limit - 1)))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := 
      dylan-multidimensional-array-row-major-element
        (application, array-instance, i);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;

define method remote-limited-array-linear-inspect
    (application :: <debug-target>, array-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)
  let limit = dylan-limited-array-size(application, array-instance);
  unless (last-index & (last-index < (limit - 1)))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := 
      dylan-limited-array-row-major-element
        (application, array-instance, i);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;

define method remote-list-inspect
    (application :: <debug-target>, list-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)

  // BEFORE-LAST?
  // Checks that an index is still within the valid subrange, given that
  // the last-index might not even have a numerical value!

  local method before-last? (idx :: <integer>) => (answer :: <boolean>)
          if (last-index)
            idx <= last-index
          else
            #t
          end if;
        end method;

  let key-sequence = #f;
  let value-sequence = make(<stretchy-vector>, size: 0);
  let i = 0;
  let this-cons :: <remote-value> = list-instance;
  block (exit)
    while (classify-dylan-object(application, this-cons) == $pair-type)
      if (i < first-index)
        // Don't add this element - it's before the required subrange.
        i := i + 1;
        this-cons := dylan-tail(application, this-cons);
      elseif (~before-last?(i))
        // Don't add this element - it's after the required subrange.
        // Also, abort chaining through the list.
        exit()
      else
        // Add this element, and keep iterating.
        add!(value-sequence, dylan-head(application, this-cons));
        i := i + 1;
        this-cons := dylan-tail(application, this-cons);
      end if;
    end while;
  end block;
  values(key-sequence, value-sequence);
end method;

define method remote-table-inspect
    (application :: <debug-target>, table-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)

  // Get the remote vectors of keys and elements, and their sizes - which
  // should be equal.

  let keys-vector = dylan-table-keys-vector(application, table-instance);
  let vals-vector = dylan-table-values-vector(application, table-instance);
  let keys-limit = dylan-entry-vector-size(application, keys-vector);
  let vals-limit = dylan-entry-vector-size(application, vals-vector);

  // We know that there is a specific constant internally defined in the
  // dylan library that indicates an unused key in the table. We need to
  // know this value.

  let dylan-lib = application.application-dylan-library;
  let empty-entry = resolve-dylan-name(application,
                                       "$table-entry-empty",
                                       $dylan-internal,
                                       library: dylan-lib);

  // Make sure that last-index, if it was not supplied, has a meaningful
  // numeric value of some kind.

  unless (last-index & (last-index < keys-limit))
    last-index := keys-limit - 1;
  end unless;
  let j = 0;
  let key-sequence = make(<stretchy-vector>, size: 0);
  let value-sequence = make(<stretchy-vector>, size: 0);
  if (empty-entry)
    for (i from 0 below keys-limit)
      let this-key :: <remote-value> 
         = dylan-entry-vector-element(application, keys-vector, i);
      unless (this-key = empty-entry)
        if ((j >= first-index) & (j <= last-index))
          add!(key-sequence, this-key);
          add!(value-sequence, 
               dylan-entry-vector-element(application, vals-vector, i));
          j := j + 1;
        end if;
      end unless
    end for;
  end if;
  values(key-sequence, value-sequence);
end method;

define method remote-stretchy-vector-inspect
    (application :: <debug-target>, sv-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)
  let rep = dylan-stretchy-vector-representation(application, sv-instance);
  let limit = dylan-stretchy-vector-size(application, rep);
  unless (last-index & (last-index < limit))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := dylan-stretchy-vector-element(application, rep, i);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;

define method remote-limited-stretchy-vector-inspect
    (application :: <debug-target>, sv-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)
  let rep = dylan-limited-stretchy-vector-representation(application, sv-instance);
  let limit = dylan-limited-stretchy-vector-size(application, rep);
  unless (last-index & (last-index < limit))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := dylan-limited-stretchy-vector-element(application, rep, i);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;

define method remote-deque-inspect
    (application :: <debug-target>, deq-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), 
            value-sequence :: <sequence>)
  let limit = dylan-deque-size(application, deq-instance);
  unless (last-index & (last-index < limit))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := dylan-deque-element(application, deq-instance, i);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;

define method remote-byte-string-inspect
    (application :: <debug-target>, bs-instance :: <remote-value>,
     #key first-index = 0, last-index = #f)
        => (key-sequence :: false-or(<sequence>), value-sequence :: <sequence>)
  let actual-string = dylan-string-data(application, bs-instance);
  let limit = size(actual-string);
  unless (last-index & (last-index < limit))
    last-index := limit - 1
  end unless;
  let key-sequence = #f;
  let value-sequence = make(<vector>, size: last-index - first-index + 1);
  let j = 0;
  for (i from first-index to last-index)
    value-sequence[j] := character-as-tagged-remote-value(actual-string[i]);
    j := j + 1;
  end for;
  values(key-sequence, value-sequence);
end method;


///// REMOTE-GENERIC-FUNCTION-INSPECT
//    Returns information about a generic function.

define method remote-generic-function-inspect 
    (application :: <debug-target>, gf-instance :: <remote-value>)
       => (signature :: <remote-value>,
           methods :: <sequence>)
  values
    (dylan-lambda-signature(application, gf-instance),
     canonicalize-sequence(application, gf-methods(application, gf-instance)))
end method;


///// REMOTE-METHOD-INSPECT
//    Returns information about a method.

define method remote-method-inspect
    (application :: <debug-target>, method-instance :: <remote-value>)
       => (signature :: <remote-value>,
           internal-entry-point :: <remote-value>,
           key-specifiers :: <sequence>)
  let signature = as-remote-value(0);
  let internal-entry-point = as-remote-value(0);
  let key-specifiers = #[];
  let classification = classify-dylan-object(application, method-instance);
  select (classification)
    $simple-method-type =>
      signature := dylan-lambda-signature(application, method-instance);
      internal-entry-point := method-mep(application, method-instance);

    $keyword-method-type =>
      signature := dylan-lambda-signature(application, method-instance);
      internal-entry-point := method-iep(application, method-instance);
      key-specifiers :=
        canonicalize-sequence
          (application,
           dylan-method-keyword-specifiers(application, method-instance));

    $accessor-method-type =>
      internal-entry-point := dylan-lambda-xep(application, method-instance);

    otherwise => #f;
  end select;
  values(signature, internal-entry-point, key-specifiers);
end method;


///// REMOTE-SIGNATURE-INSPECT
//    Returns information about the signature of a callable object.

define method remote-signature-inspect
    (application :: <debug-target>, sig :: <remote-value>)
        => (sig-required-types :: <sequence>,
            sig-value-types :: <sequence>,
            sig-rest-type :: false-or(<remote-value>),
            sig-keys :: false-or(<sequence>),
            sig-key-types :: false-or(<sequence>))

  // Find out what kind of signature we are looking at, or even if we
  // are looking at a signature at all.

  let sig-type = classify-dylan-object(application, sig);
  let sig-required-types = #[];
  let sig-value-types = #[];
  let got-sig-value-types = #f;
  let sig-rest-type = #f;
  let got-sig-rest-type = #f;
  let sig-keys = #f;
  let sig-key-types = #f;

  let (limit-required, 
       limit-values,
       rest?,
       keys?,
       all-keys?,
       default-rest?,
       default-vals?) =
     dylan-signature-properties(application, sig);

  if (default-rest?)
    got-sig-rest-type := 
      lookup-static-object(application, "<object>", "dylan");
  end if;
  if (default-vals?)
    got-sig-value-types := 
      vector(lookup-static-object(application, "<object>", "dylan"));
  end if;

  select (sig-type)
    $signature-required-only-type =>
      sig-required-types :=
	canonicalize-sequence
	  (application, 
           dylan-signature-required(application, sig),
           max-size-override: limit-required);

    $signature+values-type =>
      sig-required-types :=
	canonicalize-sequence
          (application, dylan-signature-required(application, sig),
           max-size-override: limit-required);
      sig-value-types :=
	canonicalize-sequence
	  (application, dylan-signature-values(application, sig),
           max-size-override: limit-values);

    $signature+rest-value-type =>
      sig-required-types :=
	canonicalize-sequence
	  (application, dylan-signature-required(application, sig),
           max-size-override: limit-required);
      sig-rest-type :=
	dylan-signature-rest-value(application, sig);

    $signature+values+rest-value-type =>
      sig-required-types :=
	canonicalize-sequence
	  (application, dylan-signature-required(application, sig),
           max-size-override: limit-required);
      sig-value-types :=
	canonicalize-sequence
	  (application, dylan-signature-values(application, sig),
           max-size-override: limit-values);
      sig-rest-type :=
	dylan-value-signature-rest-value(application, sig);

    $keyword-signature-type =>
      sig-required-types :=
	canonicalize-sequence
	  (application, dylan-signature-required(application, sig),
           max-size-override: limit-required);
      sig-keys :=
	canonicalize-sequence
	  (application, dylan-signature-keys(application, sig));
      sig-key-types :=
	canonicalize-sequence
	  (application, dylan-signature-key-types(application, sig));

    $keyword-signature+values-type =>
      sig-required-types :=
	canonicalize-sequence
	  (application, dylan-signature-required(application, sig),
           max-size-override: limit-required);
      sig-value-types :=
	canonicalize-sequence
	  (application, dylan-keyword-signature-values(application, sig),
           max-size-override: limit-values);
      sig-keys :=
	canonicalize-sequence
	  (application, dylan-signature-keys(application, sig));
      sig-key-types :=
	canonicalize-sequence
	  (application, dylan-signature-key-types(application, sig));

    $keyword-signature+rest-value-type =>
      sig-required-types :=
	canonicalize-sequence
	  (application, dylan-signature-required(application, sig),
           max-size-override: limit-required);
      sig-rest-type :=
	dylan-keyword-signature-rest-value(application, sig);
      sig-keys :=
	canonicalize-sequence
	  (application, dylan-signature-keys(application, sig));
      sig-key-types :=
	canonicalize-sequence
	  (application, dylan-signature-key-types(application, sig));

    $keyword-signature+values+rest-value-type =>
      sig-required-types :=
	canonicalize-sequence
	  (application, dylan-signature-required(application, sig),
           max-size-override: limit-required);
      sig-value-types :=
	canonicalize-sequence
	  (application, dylan-keyword-signature-values(application, sig),
           max-size-override: limit-values);
      sig-rest-type :=
          dylan-value-keyword-signature-rest-value(application, sig);
      sig-keys :=
	canonicalize-sequence
	  (application, dylan-signature-keys(application, sig));
      sig-key-types :=
	canonicalize-sequence
	  (application, dylan-signature-key-types(application, sig));

    otherwise => #f;
  end select;

  if (got-sig-value-types)
    sig-value-types := got-sig-value-types
  end if;

  if (got-sig-rest-type)
    sig-rest-type := got-sig-rest-type
  end if;

  values(sig-required-types, sig-value-types, sig-rest-type,
         sig-keys, sig-key-types);

end method;


///// REMOTE-SINGLETON-INSPECT
//    Returns information about a singleton type.

define method remote-singleton-inspect
    (application :: <debug-target>, si :: <remote-value>)
 => (singleton-val :: <remote-value>)
  let (obj, ok) = dylan-singleton-object(application, si);
  obj
end method;


///// REMOTE-CLASS-INSPECT
//    Returns information about a class.

define method remote-class-inspect 
    (application :: <debug-target>, ci :: <remote-value>,
     #key use-incarnation: iclass = #f)
       => (direct-subs :: <sequence>,
           direct-supers :: <sequence>,
           all-supers :: <sequence>,
           direct-slots :: <sequence>,
           all-slots :: <sequence>,
           direct-methods :: <sequence>)
  let iclass-object = iclass | dylan-class-iclass(application, ci);
  values
    (canonicalize-sequence
       (application, 
        dylan-iclass-direct-subclasses(application, iclass-object)),
     canonicalize-sequence
       (application,
        dylan-iclass-direct-superclasses(application, iclass-object)),
     canonicalize-sequence
       (application,
        dylan-iclass-all-superclasses(application, iclass-object)),
     canonicalize-sequence
       (application,
        dylan-iclass-direct-slot-descriptors(application, iclass-object)),
     canonicalize-sequence
       (application,
        dylan-iclass-all-slot-descriptors(application, iclass-object)),
     canonicalize-sequence
       (application,
        dylan-iclass-direct-methods(application, iclass-object)))
end method;


///// REMOTE-SLOT-INSPECT
//    Returns information about a slot in a class.

define method remote-slot-inspect
    (application :: <debug-target>, slot-instance :: <remote-value>)
        => (slot-basic-name :: <string>,
            slot-basic-type :: <symbol>,
            slot-owner-class :: <remote-value>,
            slot-getter-fun :: false-or(<remote-value>),
            slot-setter-fun :: false-or(<remote-value>),
            slot-init-keyword :: false-or(<remote-value>),
            slot-init-req? :: <boolean>,
            slot-init-val :: false-or(<remote-value>),
            slot-spec :: <remote-value>)

  // NULLIFY-IF-FALSE
  // A local method that turns a <remote-value> into #f if it is
  // equal to #f _within_ the runtime.

  local method nullify-if-false (x :: <remote-value>)
                     => (maybe-x :: false-or(<remote-value>))
          let cl = classify-dylan-object(application, x);
          if (cl == $boolean-false)
            #f
          else
            x
          end if;
        end method;

  // REMOTE-BOOLEAN-TO-ACTUAL-BOOLEAN
  // A local convenience function that turns <remote-value> into a
  // boolean.

  local method remote-boolean-to-actual-boolean (x :: <remote-value>)
                      => (local-x :: <boolean>)
          let maybe-x = nullify-if-false(x);
          if (maybe-x)
            #t
          else
            #f
          end if
        end method;

  // Find out what kind of runtime slot descriptor this is.

  let slot-classification =
     classify-dylan-object(application, slot-instance);

  // Use more primitive accessors for most of the return values, turning
  // remote booleans into real booleans as necessary.

  let slot-basic-name = dylan-slot-name(application, slot-instance);
  let slot-basic-type = #"instance-slot";
  let slot-owner-class = dylan-slot-owner-class(application, slot-instance);
  let slot-getter-fun =
    nullify-if-false(dylan-slot-getter-function(application, slot-instance));
  let slot-setter-fun =
    nullify-if-false(dylan-slot-setter-function(application, slot-instance));
  let slot-init-keyword =
    nullify-if-false(dylan-slot-init-keyword(application, slot-instance));
  let slot-init-req? =
    remote-boolean-to-actual-boolean
       (dylan-slot-init-keyword-required(application, slot-instance));
  let slot-init-val = 
    nullify-if-false(dylan-slot-init-value(application, slot-instance));
  let slot-spec = dylan-slot-specializer(application, slot-instance);

  // We really hope one of these 'select' branches will be applicable....

  select(slot-classification)
     $instance-slot-descriptor-type =>
       slot-basic-type := #"instance-slot";

     $repeated-slot-descriptor-type =>
       slot-basic-type := #"repeated-slot";

     $virtual-slot-descriptor-type =>
       slot-basic-type := #"virtual-slot";

     $class-slot-descriptor-type =>
       slot-basic-type := #"class-slot";

     $each-subclass-slot-descriptor-type =>
       slot-basic-type := #"each-subclass-slot";

  end select;

  // And return all the values.
  values(slot-basic-name,
         slot-basic-type,
         slot-owner-class,
         slot-getter-fun,
         slot-setter-fun,
         slot-init-keyword,
         slot-init-req?,
         slot-init-val,
         slot-spec)
end method;


///// REMOTE-PAIR-INSPECT
//    Returns the head and tail of a pair as <remote-value>

define method remote-pair-inspect
    (application :: <debug-target>, pair-instance :: <remote-value>)
       => (head-instance :: <remote-value>, tail-instance :: <remote-value>)
  values(dylan-head(application, pair-instance),
         dylan-tail(application, pair-instance));
end method;


///// REMOTE-RANGE-INSPECT
//    Returns the from, end, and step values of a range, where relevant.
//    TODO: There's some pants in here.
//          We need to know the ending value of finite ranges, but
//          range instances only have a "size" field, from which we have
//          to calculate the last value in the range. This calculation
//          is very error-prone. We might want to call a spy function
//          here instead.

define method remote-range-inspect
    (application :: <debug-target>, range-instance :: <remote-value>)
        => (start-val :: <remote-value>,
            end-val :: false-or(<remote-value>),
            step-val :: <remote-value>)

  // This cannot be assumed to work, but it probably will in most cases.

  local method compute-end 
                  (starting :: <remote-value>, 
                   stepping :: <remote-value>,
                   sizeval :: <remote-value>)
                     => (ending :: <remote-value>)
    let a = tagged-remote-value-as-integer(starting);
    let b = tagged-remote-value-as-integer(stepping);
    let c = tagged-remote-value-as-integer(sizeval);
    let d = a + (b * c);
    integer-as-tagged-remote-value(d)
  end method;

  let range-classification = 
    classify-dylan-object(application, range-instance);

  let start-val = integer-as-tagged-remote-value(0);
  let end-val = #f;
  let sz = integer-as-tagged-remote-value(0);
  let step-val = integer-as-tagged-remote-value(0);

  // Depending on exactly what kind of range this is, set the return
  // values.

  select(range-classification)
    $infinite-range-type =>
      start-val := dylan-range-start(application, range-instance);
      end-val := #f;
      step-val := dylan-range-by(application, range-instance);

    $finite-range-type =>
      start-val := dylan-range-start(application, range-instance);
      step-val := dylan-range-by(application, range-instance);
      sz := dylan-range-size(application, range-instance);
      end-val := compute-end(start-val, step-val, sz);

    $constant-range-type =>
      start-val := dylan-range-start(application, range-instance);
      step-val := dylan-range-by(application, range-instance);
      sz := dylan-range-size(application, range-instance);
      end-val := compute-end(start-val, step-val, sz);

    $empty-range-type =>
      start-val := dylan-range-start(application, range-instance);
      end-val := dylan-range-start(application, range-instance);
      step-val := dylan-range-by(application, range-instance);

  end select;

  values(start-val, end-val, step-val);
end method;


///// CLASSIFY-RUNTIME-VALUE
//    Returns a symbol that abstractly describes a runtime object, whether
//    it is a dylan object or a foreign object.

define method classify-runtime-value
    (application :: <debug-target>, value :: <remote-value>,
     #key address? :: <boolean>)
 => (description :: <symbol>)
  let classification
    = classify-dylan-object(application, value, address?: address?);
  select (classification)
    $double-integer-type                => #"dylan-large-integer";
    $machine-integer-type               => #"dylan-machine-word";
    $unsigned-machine-integer-type      => #"dylan-unsigned-machine-word";
    $single-float-type                  => #"dylan-single-float";
    $double-float-type                  => #"dylan-double-float";
    $string-type                        => #"dylan-string";
    $vector-type                        => #"dylan-vector";
    $limited-vector-type                => #"dylan-limited-vector";
    $array-type                         => #"dylan-dimensioned-array";
    $limited-array-type                 => #"dylan-limited-array";
    $pair-type                          =>
      if (dylan-pair-proper-list?(application, value))
	#"dylan-list"
      else
	#"dylan-dotted-pair"
      end if;
    $empty-list-type                    => #"dylan-list";
    $symbol-type                        => #"dylan-symbol";
    $class-type                         => #"dylan-class";
    $simple-method-type                 => #"dylan-method";
    $keyword-method-type                => #"dylan-method";
    $generic-function-type              => #"dylan-generic-function";
    $boolean-false                      => #"dylan-canonical-false";
    $boolean-true                       => #"dylan-canonical-true";
    $unbound-type                       => #"dylan-canonical-unbound";
    $user-defined-type                  => #"dylan-general-object";
    $integer-type                       => #"dylan-integer";
    $character-type                     => #"dylan-character";
    $unknown-type                       => 
      if (foreign-address-is-function?(application, value))
	#"foreign-function"
      else
	#"foreign-object"
      end;
    $singleton-type                     => #"dylan-singleton-type";
    $union-type                         => #"dylan-union-type";
    $subclass-type                      => #"dylan-subclass-type";
    $bottom-type                        => #"dylan-bottom-type";
    $stretchy-vector-type               => #"dylan-stretchy-vector";
    $limited-stretchy-vector-type       => #"dylan-limited-stretchy-vector";
    $deque-type                         => #"dylan-deque";
    $table-type                         => #"dylan-object-table";
    $string-table-type                  => #"dylan-string-table";
    $infinite-range-type                => #"dylan-range";
    $finite-range-type                  => #"dylan-range";
    $constant-range-type                => #"dylan-range";
    $empty-range-type                   => #"dylan-range";
    otherwise                           => #"dylan-general-object";
  end select
end method;

define method foreign-address-is-function?
    (application :: <debug-target>, value :: <remote-value>)
 => (is-function? :: <boolean>)
  let st = debug-target-symbol-table(application);
  let symbol = symbol-table-symbol-relative-address(st, value);
  symbol & symbol.remote-symbol-address == value
end method foreign-address-is-function?;

///// DYLAN-PAIR-PROPER-LIST? (Internal function)
//    Given an instance of a dylan object that has been proved already
//    to be a <pair>, scans the data structure to see whether the
//    pair is the head of a genuine list.

define method dylan-pair-proper-list?
    (application :: <debug-target>, pair-object :: <remote-value>,
     #key count = 0)
 => (well? :: <boolean>)
  // We can't allow this to break if the list is circular. At the moment,
  // limit the recursion arbitrarily.
  if (count > $possible-circularity-punt-value)
    #f
  else
    let tail-instance = dylan-tail(application, pair-object);
    let tail-class = classify-dylan-object(application, tail-instance);
    if (tail-class == $empty-list-type)
      #t
    elseif (tail-class == $pair-type)
      dylan-pair-proper-list?(application, tail-instance, count: count + 1)
    else
      #f
    end if
  end if
end method;


///// REMOTE-INSTANCE-WRAPPER
//    A temporary function used exclusively by the console debugger's
//    remote object walker.

define variable *wrapper-wrapper* = #f;

define method remote-instance-wrapper
    (application :: <debug-target>, instance :: <remote-value>)
 => (wrapper-if-applicable :: false-or(<remote-value>))

  let tag = inspect-instance-tag(application, instance);
  let wrapper-if-applicable = #f;

  if (tag == $dylan-tag-pointer)
    let (wrapper, ok) = read-instance-header(application, instance);
    if (ok)
      let (wrapper-wrapper, ok) = read-instance-header(application, wrapper);
      if (ok)
        unless(*wrapper-wrapper*)
          let lib = find-library-called(application, "DYLAN");
          let wwsym =
            find-symbol(application.debug-target-access-path,
                        "KLmm_wrapperGYinternalVdylanW",
                        library: lib);
          if (wwsym)
            *wrapper-wrapper* := remote-symbol-address(wwsym);
	  end if;
	end unless;
        if (wrapper-wrapper = *wrapper-wrapper*)
          wrapper-if-applicable := wrapper;
	end if
      end if
    end if
  end if;
  wrapper-if-applicable;
end method;

///// WRAPPER-TRACE-INFORMATION
//    A temporary function used exclusively by the console debugger's
//    remote object walker.

define method wrapper-trace-information
    (application :: <debug-target>, proved-wrapper :: <remote-value>)
  => (symbolic-name :: <byte-string>,
      presented-name :: <byte-string>,
      module-name :: <byte-string>,
      slot-count :: <integer>,
      repeat-information :: <symbol>,
      repeat-offset :: <integer>)

  let symbolic-name = "Unknown Wrapper";
  let presented-name = "Unknown Class";
  let module-name = "INTERNAL";
  let slot-count = 0;
  let repeat-information = #"no-repeat";
  let repeat-offset = 0;
  let path = application.debug-target-access-path;
  let wsym = symbol-relative-address(path, proved-wrapper);

  if (wsym)
    symbolic-name := wsym.remote-symbol-name;
    let (lib, mod, nm) = demangle-qualified-name(symbolic-name);
    presented-name := as-uppercase(nm);
    module-name := as-uppercase(mod);
  end if;

  block ()
    let class-object = wrapper-to-iclass(application, proved-wrapper);
    let slot-descriptors =
      dylan-iclass-instance-slot-descriptors(application, class-object);
    slot-count :=
      dylan-vector-size(application, slot-descriptors);
    let rdescriptor =
      dylan-iclass-repeated-slot-descriptor(application, class-object);
    if (classify-dylan-object(application, rdescriptor) ~= $boolean-false)
      repeat-information := #"slot-repeat";
      repeat-offset := slot-count - 1;
    end if;
  exception (<remote-access-violation-error>)
    slot-count := 0
  end block;

  values(symbolic-name, presented-name, module-name, slot-count, 
         repeat-information, repeat-offset);
end method;


///// DYLAN-OBJECT-SIZE
//    Returns size information about an instance.

define method dylan-object-size
    (application :: <debug-target>, instance :: <remote-value>)
  => (byte-size-of-whole-object :: <integer>,
      number-of-fixed-fields :: <integer>,
      number-of-repeated-elements :: <integer>)
  let tag = inspect-instance-tag(application, instance);
  let path = application.debug-target-access-path;
  select (tag)
    $dylan-tag-integer => values(4, 0, 0);
    $dylan-tag-character => values(4, 0, 0);
    $dylan-tag-boolean => values(4, 0, 0);
    $dylan-tag-pointer =>
      let (wrapper, valid?) = read-instance-header(application, instance);
      if (valid?)
        let (fixed-slot-count, vector-scaling) =
          dylan-wrapper-properties(application, wrapper);
        if (vector-scaling == 0)
          values((fixed-slot-count + 1) * 4, fixed-slot-count, 0)
        else
          let element-count-address =
            indexed-remote-value(instance, fixed-slot-count + 1);
          let element-count-tagged = read-value(path, element-count-address);
          let element-count = 
            tagged-remote-value-as-integer(element-count-tagged);
          values((fixed-slot-count + 1) * 4, fixed-slot-count, element-count);
        end if;
      else
        values(4, 0, 0);
      end if;
  end select;
end method;


///// DYLAN-CLASS-SLOT-STORAGE
//    An accessor for slots that have class-level allocation (ie "class"
//    and "each-subclass" slots).
//    Inputs:
//      application     - The <debug-target> tether object.
//      class-instance  - A <remote-value> pointing to the <class>.
//    Outputs:
//      (Three parallel sequences)
//      basic-names     - A sequence of <string> objects: the slot names.
//      descriptors     - The slot descriptors themselves (as <remote-value>s).
//      values          - The current values being stored.

define method dylan-class-slot-storage
    (application :: <debug-target>, class-instance :: <remote-value>,
     #key use-incarnation: iclass = #f)
  => (basic-names :: <sequence>,
      descriptors :: <sequence>,
      vals :: <sequence>)
  let iclass-object = iclass | dylan-class-iclass(application, class-instance);
  let class-storage-object = 
    dylan-iclass-class-storage(application, iclass-object);
  let storage-spaces =
    canonicalize-sequence(application, class-storage-object);
  let basic-names = make(<vector>, size: storage-spaces.size);
  let descriptors = make(<vector>, size: storage-spaces.size);
  let vals = make(<vector>, size: storage-spaces.size);
  for (i from 0 below storage-spaces.size)
    let this-space = storage-spaces[i];
    select (classify-dylan-object(application, this-space))

      $pair-type =>
        let storage-val = dylan-head(application, this-space);
        let remote-descriptor = dylan-tail(application, this-space);
        let remote-getter = dylan-slot-getter(application, remote-descriptor);
        let (lib, mod, name)
          = dylan-instance-symbolic-name(application, remote-getter);
        basic-names[i] := name;
        descriptors[i] := remote-descriptor;
        vals[i] := storage-val;

      otherwise =>
        let marker = dylan-runtime-unbound-marker(application);
        basic-names[i] := "unknown-slot-name";
        descriptors[i] := marker;
        vals[i] := marker;

    end select
  end for;
  values(basic-names, descriptors, vals);
end method;


///// DYLAN-CLASS-BROWSER-INFORMATION
//    A new inspection API that is based on a class, rather than an instance
//    of a class. Returns summary information about how instances of the
//    class should be browsed.
//    Inputs:
//      application     - The <debug-target> tether object.
//      class-instance  - A remote value pointing to a <class> object.
//    Outputs:
//      slots           - A sequence of <pair> object. The head of each
//                        pair is a <string> representing the name of the
//                        slot. The tail is an <integer> giving the
//                        offset of the slot in <remote-value> sized units.
//      navigation     -  A <string-table> keyed on slot names, and mapping
//                        to the offset.
//      repeat         -  If the class has a repeated slot, this gives the
//                        name, otherwise is returned as #f.
//      count-offset   -  If the class has a repeated slot, this returns the
//                        offset at which the element count may be found.
//      element-size   -  Returns an integer giving the byte-size of each
//                        element for those cases where the elements are not
//                        <remote-value>-sized. If this is #f, then the 
//                        elements are all "standard" dylan objects.
//      element-offset -  If the class has a repeated slot, this returns the
//                        offset at which the 0th element can be found.
//      class-slot-count  If the class has any class-allocation slots
//                        associated with it, this returns the count.

define method dylan-class-browser-information
     (application :: <debug-target>, class-instance :: <remote-value>,
      #key use-incarnation: iclass = #f)
 => (slots :: <sequence>,
     navigation :: <string-table>,
     repeat :: false-or(<string>),
     count-offset :: false-or(<integer>),
     element-size :: false-or(<integer>),
     element-offset :: false-or(<integer>),
     class-slot-count :: <integer>)

  let path = application.debug-target-access-path;
  let slots = #f;
  let navigation = #f;
  let repeat = #f;
  let count-offset = 0;
  let element-size = #f;
  let element-offset = 0;

  // Pull basic details out of the class object, and its corresponding
  // implementation class.

  let iclass-object = iclass | dylan-class-iclass(application, class-instance);
  let wrapper = dylan-iclass-mm-wrapper(application, iclass-object);
  let (fixed-slot-count, vector-scaling) =
    dylan-wrapper-properties(application, wrapper);
  let has-byte-repeats? = (vector-scaling == 1);
  let slot-storage-vector = 
    dylan-iclass-class-storage(application, iclass-object);
  let slotd-vec = 
    dylan-iclass-instance-slot-descriptors(application, iclass-object);
  let slotd-count = dylan-vector-size(application, slotd-vec);
  let rdescriptor =
    dylan-iclass-repeated-slot-descriptor(application, iclass-object);
  let rdescriptor-genuine? =
    if (classify-dylan-object(application, rdescriptor) == $boolean-false)
      #f
    else
      #t
    end if;

  // Now initialize the return values.

  slots := make(<vector>, size: slotd-count);
  navigation := make(<string-table>, size: slotd-count);
  count-offset := slotd-count + 1;
  element-offset := count-offset + 1;

  // Make sure that the element-size is updated if the repeated slot only
  // holds bytes.

  if (has-byte-repeats?)
    element-size := 1
  end if;

  // Add the details of each slot descriptor to the "slots" and
  // "navigation" results.

  for (i from 0 below slotd-count)
    let remote-descriptor = dylan-vector-element(application, slotd-vec, i);
    let remote-getter = dylan-slot-getter(application, remote-descriptor);
    let (lib, mod, slot-name) = 
      dylan-instance-symbolic-name(application, remote-getter);
    slots[i] := pair(slot-name, i + 1);
    navigation[slot-name] := (i + 1);
  end for;

  // If there is a repeated slot, fill in slot name.

  if (rdescriptor-genuine?)
    let rslot-getter = dylan-slot-getter(application, rdescriptor);
    let (lib, mod, rslot-name) =
      dylan-instance-symbolic-name(application, rslot-getter);
    repeat := rslot-name;
  end if;

  // And return it all...

  values
    (slots, navigation, repeat, count-offset, element-size, element-offset,
     dylan-vector-size(application, slot-storage-vector));
end method;


///// REMOTE-COLLECTION-SIZE
//    Given any kind of collection, returns the number of elements that
//    it is known to contain.
//    This function is permitted to return -1 to denote "infinity",
//    for a collection that is of indefinite size.

define method remote-collection-size
    (application :: <debug-target>, collection-instance :: <remote-value>)
      => (entry-count :: <integer>)

  // Classify the instance.
  let instance-type = classify-dylan-object(application, collection-instance);

  // And dispatch to the appropriate accessor.
  select (instance-type)

    $empty-list-type => 0;

    $pair-type =>
      let cons-count = 0;
      let current = collection-instance;
      let current-type = $pair-type;
      block (return)
        while (current-type == $pair-type)
          cons-count := cons-count + 1;
          if (cons-count > $possible-circularity-punt-value)
            return(-1)
          else
            current := dylan-tail(application, current);
            current-type := classify-dylan-object(application, current);
          end if;
        end while;
        return(cons-count)
      end block;

    $vector-type => 
      dylan-vector-size(application, collection-instance);

    $array-type => 
      dylan-multidimensional-array-size(application, collection-instance);

    $stretchy-vector-type =>
      let svr = dylan-stretchy-vector-representation
                  (application, collection-instance);
      dylan-stretchy-vector-size(application, svr);

    $limited-vector-type => 
      dylan-limited-vector-size(application, collection-instance);

    $limited-array-type => 
      dylan-limited-array-size(application, collection-instance);

    $limited-stretchy-vector-type =>
      let svr = dylan-limited-stretchy-vector-representation
                  (application, collection-instance);
      dylan-limited-stretchy-vector-size(application, svr);

    $string-type =>
      dylan-string-data(application, collection-instance).size;

    $deque-type =>
      dylan-deque-size(application, collection-instance);

    $table-type =>
      dylan-table-size(application, collection-instance);

    $string-table-type =>
      dylan-table-size(application, collection-instance);

    $empty-range-type => 0;

    $finite-range-type =>
      tagged-remote-value-as-integer
        (dylan-range-size(application, collection-instance));

    $constant-range-type =>
      tagged-remote-value-as-integer
        (dylan-range-size(application, collection-instance));

    $infinite-range-type => -1;

    otherwise => 0;

  end select;
end method;
