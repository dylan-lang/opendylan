Module:    dfmc-application
Synopsis:  Serving collection objects from the application
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// COLLECTION-SIZE (Environment Protocol Method)
//    Returns the size of a collection.

define method collection-size
    (application :: <dfmc-application>, collection :: <collection-object>)
 => (size :: <integer>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = collection.application-object-proxy;
    let cl = runtime-proxy-to-remote-value(application, proxy);
    remote-collection-size(target, cl)
  end
end method collection-size;


///// COLLECTION-KEYS (Environment Protocol Method)
//    Returns (possibly a subrange of) the sequence of keys in a runtime
//    collection object. (Emergency method for the implicit case).

define method collection-keys
    (application :: <dfmc-application>, collection :: <collection-object>,
     #key range: collection-range = #f)
 => (key-seq :: false-or(<sequence>))
  // The collection is implicit, which means that we are only required to
  // return a range, rather than an actual collection of keys. If the
  // range has been delimited by the caller, then we can just return it
  // back.
  collection-range
    | range(from: 0, below: collection-size(application, collection), by: 1)
end method collection-keys;

define method collection-keys
    (application :: <dfmc-application>,
     collection :: <explicit-key-collection-object>,
     #key range = #f)
 => (key-seq :: <simple-object-vector>)
  do-collection-contents(application, collection, #"keys", range: range)
end method collection-keys;

define method do-collection-keys
    (function :: <function>, application :: <dfmc-application>,
     collection :: <sequence-object>)
 => ()
  let collection-range = collection-size(application, collection);
  do(function, range(from: 0, below: collection-range, by: 1))
end method do-collection-keys;

define method do-collection-keys
    (function :: <function>, application :: <dfmc-application>,
     collection :: <explicit-key-collection-object>)
 => ()
  do-collection-contents(application, collection, #"keys",
                         function: function)
end method do-collection-keys;


///// COLLECTION-ELEMENTS (Environment Protocol Method)
//    Returns (possibly a subrange of) the sequence of elements in the
//    runtime collection objects.

define method collection-elements
    (application :: <dfmc-application>, collection :: <collection-object>,
     #key range = #f)
 => (key-seq :: false-or(<sequence>))
  do-collection-contents(application, collection, #"elements", range: range)
end method collection-elements;

define method do-collection-elements
    (function :: <function>, application :: <dfmc-application>,
     collection :: <collection-object>)
 => ()
  do-collection-contents(application, collection, #"elements",
                         function: function)
end method do-collection-elements;


/// Shared utility

define method do-collection-contents
    (application :: <dfmc-application>,collection :: <collection-object>,
     type :: one-of(#"keys", #"elements"),
     #key range :: false-or(<range>) = #f,
          function :: false-or(<function>) = #f)
 => (contents :: false-or(<simple-object-vector>))
  local method get-index-subset
            () => (start :: <integer>, fin :: false-or(<integer>))
          case
            ~range =>
              values(0, #f);
            range.empty? =>
              values(0, 0);
            otherwise =>
              let first = range.first;
              let last  = range.last;
              assert(first <= last,
                     "Attempting to query collection-keys with bogus range %=",
                     range);
              values(first, last);
          end
        end method;

  let target = application.application-target-app;
  let proxy = collection.application-object-proxy;
  let (start, fin) = get-index-subset();

  // Within a debugger transaction, use the DM to inspect the required
  // subset of the collection. That done, construct application environment
  // objects for each key.

  with-debugger-transaction (target)
    let proxy-value = runtime-proxy-to-remote-value(application, proxy);
    let (key-vals, el-vals)
      = remote-collection-inspect(target, proxy-value,
                                  first-index: start,
                                  last-index: fin);
    let values = if (type == #"keys") key-vals | #[] else el-vals end;
    if (function)
      for (value in values)
        let object
          = make-environment-object-for-runtime-value(application, value);
        function(object)
      end
    else
      map-as(<simple-object-vector>,
             curry(make-environment-object-for-runtime-value, application),
             values)
    end
  end
end method do-collection-contents;
