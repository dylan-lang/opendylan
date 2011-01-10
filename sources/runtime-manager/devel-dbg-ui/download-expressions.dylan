module:      devel-dbg-ui
synopsis:    The classes of expressions that are evaluated by downloading
             objects into the runtime, and returning references to them.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *V* = 4;  /// Yeah, hack!

define constant $dylan-namespace
  = make(<dylan-name-context>, library: "dylan", module: "dylan");
define constant $internal-namespace 
  = make(<dylan-name-context>, library: "dylan", module: "internal");


///// <DOWNLOADED-OBJECT-EXPRESSION>
//    The superclass of all expressions that can be evaluated without
//    executing dylan code in the application, but that require data to
//    be downloaded.

define abstract class <downloaded-object-expression> (<simple-expression>)

  slot expression-calculated-byte-size :: <integer>,
    init-value: 0;

  slot expression-size-known? :: <boolean>,
    init-value: #f;

end class;


///// <DOWNLOADED-STRING-EXPRESSION>
//    An expression described on the command line as a string.
//    Syntax: "characters"

define class <downloaded-string-expression> (<downloaded-object-expression>)

  constant slot expression-string :: <string>,
    required-init-keyword: string:;

end class;

define class <downloaded-raw-string-expression> 
                (<downloaded-string-expression>)
end class;


///// <DOWNLOADED-VECTOR-EXPRESSION>
//    An expression described on the command line as a literal vector
//    of further simple expressions.
//    Syntax: #[<simple-expression>, ...]

define class <downloaded-vector-expression> (<downloaded-object-expression>)

  constant slot expression-vector-elements :: <sequence>,
    required-init-keyword: elements:;

end class;


///// <DOWNLOADED-LIST-EXPRESSION>
//    An expression described on the command line as a literal list
//    of further simple expressions. Downloaded as CONS pairs into the
//    runtime.
//    Syntax: #(<simple-expression>, ...)

define class <downloaded-list-expression> (<downloaded-object-expression>)

  constant slot expression-list-elements :: <sequence>,
    required-init-keyword: elements:;

end class;


///// <DOWNLOADED-INSTANCE-EXPRESSION>
//    An expression described on the command line as class name and a
//    sequence of slot values. All of these are simple expressions,
//    but an error will occur if the first expression does not evaluate
//    to a class.
//    Syntax: {<class-simple-expression>, <simple-expression>, ...}

define class <downloaded-instance-expression> (<downloaded-object-expression>)

  constant slot expression-class-expression :: <simple-expression>,
    required-init-keyword: class-expression:;

  constant slot expression-instance-expressions :: <sequence>,
    required-init-keyword: slots:;

end class;


///// CALCULATE-SIZE-OF-DOWNLOADED-OBJECT
//    Calculates how big (in bytes) that an object will be when downloaded
//    into the runtime.

define method calculate-size-of-downloaded-object
    (expression :: <downloaded-instance-expression>) => (s :: <integer>)
  ignore(expression.expression-class-expression);
  (1 + size(expression.expression-instance-expressions)) * *V*;
end method;

define method calculate-size-of-downloaded-object
    (expression :: <downloaded-list-expression>) => (s :: <integer>)
  3 * *V* * size(expression.expression-list-elements);
end method;

define method calculate-size-of-downloaded-object
    (expression :: <downloaded-vector-expression>) => (s :: <integer>)
  (2 + size(expression.expression-vector-elements)) * *V*;
end method;

define method calculate-size-of-downloaded-object
    (expression :: <downloaded-string-expression>) => (s :: <integer>)
  (2 * *V*) + size(expression.expression-string);
end method;

define method calculate-size-of-downloaded-object
    (expression :: <downloaded-raw-string-expression>) => (s :: <integer>)
  size(expression.expression-string);
end method;

///// DOWNLOADED-EXPRESSION-SIZE
//    An accessor that returns the size of an object if it is known. If
//    it is not known, calculates, caches and returns it.

define method downloaded-expression-size
    (expression :: <downloaded-object-expression>) => (s :: <integer>)
  if (expression.expression-size-known?)
    expression.expression-calculated-byte-size
  else
    expression.expression-calculated-byte-size :=
       calculate-size-of-downloaded-object(expression);
    expression.expression-size-known? := #t;
    expression.expression-calculated-byte-size;
  end if
end method;


///// GET-BLOCK-FOR-OBJECT
//    Determines whether there is room for a specified object in the current
//    interactive block. If there is, it returns that block as the destination
//    for this object. If there isn't, it allocates a new block and returns
//    that. If allocation of a new block fails, this function returns #f,
//    and no further downloading is possible.

define method get-block-for-object
    (application :: <application>, object :: <downloaded-object-expression>)
       => (maybe-block :: false-or(<static-block>))

  // Initialize the downloader if necessary.

  unless (application.downloader-initialized?)
    initialize-application-downloader(application)
  end unless;

  // If we know we can't do any downloading, just return false. Otherwise,
  // if there is room in the current block for the object, then return it.

  // TODO:
  // This function could scan "used" blocks. Just because a block ran out
  // of room for some object, it does not mean that there is not room left
  // in one of them for this particular object...

  if (application.further-downloading-possible?)
    let object-size = object.downloaded-expression-size;
    let current-block = application.current-static-block;
    if (current-block & 
         (current-block.static-block-remaining-size >= object-size))
      current-block
    else
      begin-new-static-block(application)
    end if;
  else
    debugger-message("WARNING: Failed to allocate any interactive memory!");
    #f
  end if;
end method;


///// BEGIN-NEW-STATIC-BLOCK
//    Links the current static block onto the end of the sequence of
//    blocks we've used up, and allocates a new one.

define method begin-new-static-block
    (application :: <application>) => (maybe-block :: false-or(<static-block>))
  
  if (application.further-downloading-possible?)
    debugger-message("Attempting to allocate some new interactive memory...");
    let new-block = 
      allocate-single-static-block
        (application.debug-target-access-path,
         application.runtime-allocator-primitive.remote-symbol-address);
    if (new-block)
      if (application.current-static-block)
        add!(application.allocated-static-blocks, 
             application.current-static-block);
      end if;
      application.current-static-block := new-block;
      debugger-message("Allocated a block of size %d at runtime address 0x%s",
                       new-block.static-block-size,
                       remote-value-as-string
                         (application.debug-target-access-path,
                          new-block.static-block-base-address,
                          16));
      new-block;
    else
 //     application.further-downloading-possible? := #f;
      debugger-message("WARNING: Failed to allocate NEW interactive memory!");
      #f;
    end if;
  else
    #f;
  end if;
end method;


///// INITIALIZE-APPLICATION-DOWNLOADER
//    Searches for the allocator primitive spy function in the runtime, and
//    tries to allocate the first block.

define method initialize-application-downloader
    (application :: <application>) => ()
  debugger-message("Initializing the interactive downloader...");
  application.downloader-initialized? := #t;
  application.further-downloading-possible? := #t;
  application.runtime-allocator-primitive :=
     find-symbol(application.debug-target-access-path,
                 application.runtime-allocator-name);
  if (application.runtime-allocator-primitive)
    debugger-message("Done.");
    begin-new-static-block(application);
  else
    debugger-message("WARNING: Failed to initialize the tether downloader.");
    debugger-message("         You ain't got no 'malloc' in your runtime.");
    application.further-downloading-possible? := #f;
  end if;
end method;


///// DOWNLOAD-OBJECT-INTO-RUNTIME
//    Picks apart the representation of the object, and spatters it
//    flatulently into the runtime.

define method download-object-into-runtime
    (application :: <application>, object :: <downloaded-object-expression>)
       => (maybe-address :: false-or(<remote-value>))
  // A default method in case I don't implement all if this straight away...
  #f
end method;

define method download-object-into-runtime
    (application :: <application>, object :: <downloaded-string-expression>)
       => (maybe-address :: false-or(<remote-value>))

  // If we haven't done any downloading yet, perform the required
  // initialization.

  unless (application.downloader-initialized?)
    initialize-application-downloader(application);
  end unless;

  // If we can download the object, lets do it!

  if (application.further-downloading-possible?)
    let my-block = get-block-for-object(application, object);
    if (my-block)
      block-align-remote-value(my-block);
      unless (application.runtime-byte-string-wrapper)
        let lib = core-name-to-library(application, "DYLAN");
        application.runtime-byte-string-wrapper :=
           find-symbol(application.debug-target-access-path,
                       mangle-in-context("<byte-string>", $dylan-namespace,
                                         as-wrapper?: #t),
                       library: lib);
      end unless;
      if (application.runtime-byte-string-wrapper)
        let str = object.expression-string;
        let sz = integer-as-tagged-remote-value(size(str));
        let wrapper = 
          application.runtime-byte-string-wrapper.remote-symbol-address;
        let reference =
          download-remote-value-into
            (application.debug-target-access-path, my-block, wrapper);
        download-remote-value-into
          (application.debug-target-access-path, my-block, sz);
        download-byte-string-into
          (application.debug-target-access-path, my-block, str);
        reference;
      else
        #f
      end if
    else
      #f
    end if
  else
    #f
  end if
end method;

define method download-object-into-runtime
    (application :: <application>, object :: <downloaded-raw-string-expression>)
       => (maybe-address :: false-or(<remote-value>))

  // If we haven't done any downloading yet, perform the required
  // initialization.

  unless (application.downloader-initialized?)
    initialize-application-downloader(application);
  end unless;

  // If we can download the object, lets do it!

  if (application.further-downloading-possible?)
    let my-block = get-block-for-object(application, object);
    if (my-block)
      let str = object.expression-string;
        let reference =
          download-byte-string-into
            (application.debug-target-access-path, my-block, str);
        reference;
    else
      #f
    end if
  else
    #f
  end if
end method;

define method download-object-into-runtime
    (application :: <application>, object :: <downloaded-vector-expression>)
       => (maybe-address :: false-or(<remote-value>))

  // If we haven't done any downloading yet, perform the required
  // initialization.

  unless (application.downloader-initialized?)
    initialize-application-downloader(application);
  end unless;

  // Evaluate all the component expressions for the vector. (This may
  // involve recursive downloads, but not the invocation of dylan code,
  // because components are restricted to be <simple-expression>s).

  let all-evaluated? = #t;

  for (expression in object.expression-vector-elements)
    evaluate-simple-expression(application, expression);
    all-evaluated? := expression.expression-evaluated? & all-evaluated?;
  end for;

  // If we can download the object, lets do it!

  if (application.further-downloading-possible? & all-evaluated?)
    let my-block = get-block-for-object(application, object);
    if (my-block)
      block-align-remote-value(my-block);
      unless (application.runtime-sov-wrapper)
        let lib = core-name-to-library(application, "DYLAN");
        application.runtime-sov-wrapper :=
           find-symbol(application.debug-target-access-path,
                       mangle-in-context
                         ("<simple-object-vector>",
                          $dylan-namespace,
                          as-wrapper?: #t), 
                       library: lib);
      end unless;

      // We can only proceed if we were able to find that wrapper!

      if (application.runtime-sov-wrapper)
        let vector-size = size(object.expression-vector-elements);
        let vector-object = make(<vector>, size: vector-size + 2);
        let i = 2;
        vector-object[0] := 
          application.runtime-sov-wrapper.remote-symbol-address;
        vector-object[1] :=
          integer-as-tagged-remote-value(vector-size);
        for (item in object.expression-vector-elements)
          vector-object[i] := item.expression-value;
          i := i + 1;
        end for;
        download-remote-value-vector-into
           (application.debug-target-access-path, my-block, vector-object);
      else
        #f
      end if;  
    else
      #f
    end if
  else
    #f
  end if
end method;

define method download-object-into-runtime
    (application :: <application>, object :: <downloaded-list-expression>)
       => (maybe-address :: false-or(<remote-value>))

  // If we haven't done any downloading yet, perform the required
  // initialization.

  unless (application.downloader-initialized?)
    initialize-application-downloader(application);
  end unless;

  // Evaluate all the component expressions for the list. (This may
  // involve recursive downloads, but not the invocation of dylan code,
  // because components are restricted to be <simple-expression>s).

  let all-evaluated? = #t;

  for (expression in object.expression-list-elements)
    evaluate-simple-expression(application, expression);
    all-evaluated? := expression.expression-evaluated? & all-evaluated?;
  end for;

  // If we can download the object, lets do it!

  if (application.further-downloading-possible? & all-evaluated?)
    let my-block = get-block-for-object(application, object);
    if (my-block)
      block-align-remote-value(my-block);
      // We need the pair wrapper object and the canonical empty list
      // object.
      unless (application.runtime-pair-wrapper)
        let lib = core-name-to-library(application, "DYLAN");
        application.runtime-pair-wrapper :=
           find-symbol(application.debug-target-access-path,
                       mangle-in-context("<pair>", $dylan-namespace,
                                         as-wrapper?: #t), 
                       library: lib);
      end unless;
      unless (application.runtime-empty-list-object)
        let lib = core-name-to-library(application, "DYLAN");
        application.runtime-empty-list-object :=
           find-symbol(application.debug-target-access-path,
                       mangle-in-context("%empty-list", $internal-namespace,
                                         as-static-object?: #t),
                       library: lib);
      end unless;

      // Only if we know about those two objects can we proceed.

      if (application.runtime-pair-wrapper &
          application.runtime-empty-list-object)

        let tail-reference = 
          application.runtime-empty-list-object.remote-symbol-address;
        let object-reference = #f;
        let wrapper =
          application.runtime-pair-wrapper.remote-symbol-address;
        let i = size(object.expression-list-elements) - 1;
        
        while ((i >= 0) & (tail-reference))
          let value = object.expression-list-elements[i].expression-value;
          let node = vector(wrapper, value, tail-reference);
          object-reference :=
            download-remote-value-vector-into
               (application.debug-target-access-path, my-block, node);
          tail-reference := object-reference;
          i := i - 1;
        end while;

        // Return what should now be a pointer to the first node of the list.
        object-reference;
      else
        #f
      end if
    else
      #f
    end if
  else
    #f
  end if
end method;


///// EVALUATE-SIMPLE-EXPRESSION
//    For a downloaded object, the expression is evaluated by downloading
//    the object, and storing its reference as the value. Downloaded
//    objects have no lvalue - they cannot be assigned to.

define method evaluate-simple-expression
  (application :: <application>, expression :: <downloaded-object-expression>)
     => ()
  unless (expression.expression-evaluated?)
    let reference = download-object-into-runtime(application, expression);
    if (reference)
      expression.expression-evaluated? := #t;
      expression.expression-value := reference;
    end if;
  end unless;
end method;

