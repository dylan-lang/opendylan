module:    dfmc-application
synopsis:  Dylan objects environment protocols from the application.
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// PAIR-HEAD (Environment Protocol Method)
//    Returns the head of a pair.

define method pair-head
    (application :: <dfmc-application>, pair-object :: <pair-object>)
 => (head-object :: false-or(<application-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let pair-value
      = runtime-proxy-to-remote-value
          (application, pair-object.application-object-proxy);
    let (head-value, tail-value) = remote-pair-inspect(target, pair-value);
    ignore(tail-value);
    make-environment-object-for-runtime-value(application, head-value)
  end
end method pair-head;


///// PAIR-TAIL (Environment Protocol Method)
//    Returns the tail of a pair.

define method pair-tail
    (application :: <dfmc-application>, pair-object :: <pair-object>)
 => (tail-object :: false-or(<application-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let pair-value
      = runtime-proxy-to-remote-value
          (application, pair-object.application-object-proxy);
    let (head-value, tail-value) = remote-pair-inspect(target, pair-value);
    ignore(head-value);
    make-environment-object-for-runtime-value(application, tail-value)
  end
end method pair-tail;


///// COLLECTION-SIZE (Environment Protocol Method)
//    Returns the size of a collection.
//---*** andrewa: I implemented this naive method, I'm sure there is
//---*** a more efficient way to do this.

define method collection-size
    (application :: <dfmc-application>, range-object :: <range-object>)
 => (sz :: false-or(<integer>))
  // Within a debugger transaction, query the DM for the contents of the
  // range. If the range is infinite, the DM will tell us that there is no
  // end value, so we'll return #f.
  let target = application.application-target-app;
  let has-end?
    = with-debugger-transaction (target)
        let range-value
          = runtime-proxy-to-remote-value
              (application, range-object.application-object-proxy);
        let (starter, ender, stepper)
          = remote-range-inspect(target, range-value);
        ender ~== #f
      end;

  // Use the next method to actually find out the size
  if (has-end?)
    next-method()
  end
end method collection-size;


///// RANGE-START (Environment Protocol Method)
//    Returns the beginning of a range.

define method range-start
    (application :: <dfmc-application>, range-object :: <range-object>)
 => (s :: <number-object>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let range-value
      = runtime-proxy-to-remote-value
          (application, range-object.application-object-proxy);
    let (starter, ender, stepper)
      = remote-range-inspect(target, range-value);
    make-environment-object-for-runtime-value(application, starter)
  end
end method range-start;


///// RANGE-END (Environment Protocol Method)
//    Returns the end of a range.

define method range-end
    (application :: <dfmc-application>, range-object :: <range-object>)
 => (e :: false-or(<number-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let range-value
      = runtime-proxy-to-remote-value
          (application, range-object.application-object-proxy);
    let (starter, ender, stepper)
      = remote-range-inspect(target, range-value);
    if (ender)
      make-environment-object-for-runtime-value(application, ender)
    end
  end
end method range-end;


///// RANGE-BY (Environment Protocol Method)
//    Returns the step of a range.

define method range-by
    (application :: <dfmc-application>, range-object :: <range-object>)
 => (b :: false-or(<number-object>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let range-value
      = runtime-proxy-to-remote-value
          (application, range-object.application-object-proxy);
    let (starter, ender, stepper)
      = remote-range-inspect(target, range-value);
    make-environment-object-for-runtime-value(application, stepper)
  end
end method range-by;
