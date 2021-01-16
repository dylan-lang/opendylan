Module:    apple-dylan-test-suite
Filename:  test-condition.dylan
Summary:   Apple Dylan test suite, test-condition
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc.
           All rights reserved.
----------------------------------------------*/

// Chapter 13. Conditions


// see if they exist
define test condition-classes ()
  check-true("", every?
    (rcurry(instance?, <class>),
     list(<condition>,
          <serious-condition>,
          <error>,
          <simple-error>,
          <type-error>,
          <warning>,
          <simple-warning>,
          <restart>,
          <simple-restart>)));
//          <abort>)));
end test condition-classes;

// check superclasses for each condition class
define test condition-classes-hierarchy
  ()
  check-true("", every?
    (method (x)
       every?(curry(subtype?, x.first), x.tail)
     end method,
     list(list(<simple-error>, <error>, <serious-condition>, <condition>),
          list(<type-error>, <error>, <serious-condition>, <condition>),
          list(<error>, <serious-condition>, <condition>),
          list(<serious-condition>, <condition>),
          list(<simple-warning>, <warning>, <condition>),
          list(<warning>, <condition>),
          list(<simple-restart>, <restart>, <condition>),
// amit: as <abort> was unbound in the emulator I took it out of this
// list
          list(<restart>, <condition>),
          list(<restart>, <condition>))));
end test condition-classes-hierarchy;

define test simple-error-slots ()
  let e
    = make(<simple-error>,
           format-string: "This is a test.",
           format-arguments: #(1, 2, 3));
  check-true("", "This is a test." = e.condition-format-string);
  check-true("", #(1, 2, 3) = e.condition-format-arguments);
end test simple-error-slots;

define test type-error-slots ()
  let e = make(<type-error>, value: 7, type: <string>);
  check-true("", 7 = e.type-error-value & <string> = e.type-error-expected-type);
end test type-error-slots;

define test simple-warning-slots ()
  let e
    = make(<simple-warning>,
           format-string: "This is a test.",
           format-arguments: #(1, 2, 3));
  check-true("", "This is a test." = e.condition-format-string);
  check-true("", #(1, 2, 3) = e.condition-format-arguments);
end test simple-warning-slots;

// condition: is accepted by <restart>, but ignored for now
define test restart-slots ()
  let e = make(<simple-restart>, condition: #f);
  check-true("", instance?(e, <restart>));
end test restart-slots;

define class <test-condition> (<condition>)
end class <test-condition>;

define class <test-error> (<simple-error>)
end class <test-error>;

// signal and default-handler
// While making instances of <simple-error> and <simple-warning>
// the slot format-string seems to be unbound.

// signal
define test signal-1 ()
  check-condition("", <error>, signal(make(<test-condition>)));
end test signal-1;

// simple-error
define test signal-2 ()
  check-condition("", <simple-error>,
		  signal(make(<test-error>, format-string: "Just testing.")));
end test signal-2;

// simple-warning
define test signal-3 ()
  check-condition("", <error>,
		  signal(make(<simple-warning>,
			      format-string: "Just testing.")))
end test signal-3;

define class <catcher-test-condition> (<condition>)
end class <catcher-test-condition>;

define method default-handler (c :: <catcher-test-condition>)
  #"catcher"
end method default-handler;

// No-handler: in test SIGNAL-4 means the condition should not be
// caught by the test suite's handler. Run the form "raw" so it can
// be caught by the default-handler for <catcher-test-condition>
//

// default handler called by default
define test signal-4 ()
  check-true("", signal(make(<catcher-test-condition>)) = #"catcher");
end test signal-4;

// Are signal and error really supposed to return true here???

// string, no args
define test signal-5 ()
  check-condition("", <simple-warning>, signal("Just testing."));
end test signal-5;

// string, some args
define test signal-6 ()
  check-condition("", <simple-error>, signal("Just testing.", 1, 2, 3));
end test signal-6;

define test error-1 ()
  check-condition("", <test-error>, error(make(<test-error>)));
end test error-1;

// string, no args
define test error-2 ()
  check-condition("", <error>, error("Just testing."));
end test error-2;

// string, some args
define test error-3 ()
  check-condition("", <simple-error>, error("Just testing.", 1, 2, 3));
end test error-3;

define test cerror-1 ()
  check-condition("", <test-error>,
		  cerror("Restart description", make(<test-error>)));
end test cerror-1;

// string, no args
define test cerror-2 ()
  check-condition("", <simple-error>,
		  cerror("Restart description", "Just testing."));
end test cerror-2;

// string, some args
define test cerror-3 ()
  check-condition("", <simple-error>,
		  cerror("Restart description", "Just testing.", 1, 2, 3));
end test cerror-3;

define test check-type-0 ()
  check-condition("", <type-error>, check-type(3, <string>));
end test check-type-0;

// This is because abort is undefined!!
//define test abort-0 ()
//  check-condition("", <abort>, abort());
//end test abort-0;

// Handler-bind
/*
// No condition to handle
define test handler-bind ()
  let (result1, result2, result3)
    = begin
        let handler <simple-error> = method (c, h)
                                       list(c, h)
                                     end method;
        values(1, 2, 3)
      end;
  check-true("", list(result1, result2, result3) = #(1, 2, 3));
end test handler-bind;


 I dont understand what is being attempted here

// #f if no forms
define test handler-bind-1 ()
  check-false("", let handler <simple-error> = method (c, h)
                                    list(c, h)
                                  end method);
end test handler-bind-1;


// With condition to handle
define test handler-bind-2 ()
  let (result1, result2)
    = begin
        let handler <simple-error> = method (c, h)
                                       values(c, h)
                                     end method;
        signal(make(<simple-error>));
      end;
  check-true("", instance?(result1, <condition>));
  // signalled condition
  check-true("", instance?(result2, <function>));
end test handler-bind-2;

// next-handler

// call the handler in the function
define test handler-bind-3 ()

    let handler <simple-error> = method (c, h)
                                   #"outer"
                                 end method;
    let handler <simple-error> = method (c, d)
                                   d()
                                 end method;
  check-equal("", signal(make(<simple-error>)),
  #"outer");
end test handler-bind-3;

// check test arg
define test handler-bind-4 ()
  check-true("", begin
    let handler <simple-error> = method (c, h)
                                   #"outer"
                                 end method;
    let handler
      (<condition>,
       test: method (c)
               if (instance?(c, <type-error>))
                 #t
               else
                 #f
               end if
             end method) = method (c, d)
                             #"inner"
                           end method;
      signal(make(<type-error>))
  end
  = #"inner");
  check-true("", begin
      let handler <simple-error> = method (c, h)
                                     #"outer"
                                   end method;
      let handler
        (<condition>,
         test: method (c)
                 if (instance?(c, <type-error>))
                   #t
                 else
                   #f
                 end if
               end method) = method (c, d)
                               #"inner"
                             end method;
        signal(make(<simple-error>))
    end
    = #"outer");
end test handler-bind-4;

// handler-case

// signal with string signals simple-warning
define test handler-case ()
  check-true("", block ()
    signal("hey!")
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<simple-warning>");
end test handler-case;

// but signal can signal anything
define test handler-case-1 ()
  check-true("", block ()
    signal(make(<simple-restart>))
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<simple-restart>");
end test handler-case-1;

// error with string signals simple-error
define test handler-case-2 ()
  check-true("", block ()
    error("hey!")
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<simple-error>");
end test handler-case-2;

// but error can signal anything
define test handler-case-3 ()
  check-true("", block ()
    error(make(<type-error>))
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<type-error>");
end test handler-case-3;

// cerror with string signals simple-error
define test handler-case-4 ()
  check-true("", block ()
    cerror("restart", "hey!")
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<simple-error>");
end test handler-case-4;

// but cerror can signal anything
define test handler-case-5 ()
  check-true("", block ()
    error(make(<abort>))
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<abort>");
end test handler-case-5;

// check-type with string signals type-error
define test handler-case-6 ()
  check-true("", block ()
    check-type("restart", <integer>)
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<type-error>");
end test handler-case-6;

// abort
define test handler-case-7 ()
  check-true("", block ()
    abort()
  exception (<simple-error>)
    #"<simple-error>"
  exception (<type-error>)
    #"<type-error>"
  exception (<simple-warning>)
    #"<simple-warning>"
  exception (<simple-restart>)
    #"<simple-restart>"
  exception (<abort>)
    #"<abort>"
  end block
  = #"<abort>");
end test handler-case-7;

// default handler

// this should return #f and also print a warning
define test default-handler-1 ()
  check-true("", #f
  = make(<simple-warning>,
         format-string: "This warning is part of the test suite, please ignore.").default-handler);
end test default-handler-1;

// <simple-restart> should be able to have default-handler
// called upon it as <condition> is one of its superclasses
//
define test default-handler-2 (signal: <error>, )
  check-true("", make(<simple-restart>).default-handler);
end test default-handler-2;

// restart-query

// there is a default handler for <restart>
define test restart-query ()
  check-true("", make(<simple-restart>).restart-query); //  do nothing | #t
end test restart-query;

// do-handler

// check the args to do-handlers
define test do-handler ()
  let all-handlers-info = #();
  let handler-fcn
    = method (condition, next-handler)
        list(condition, next-handler)
      end method;
  let handler <test-condition> = handler-fcn;
  //
  // This collects info about all active handlers into a list.
  //
  do-handlers
    (method (type, test, function, description)
       all-handlers-info
         := pair(list(type, test, function, description), all-handlers-info)
     end method);
  check-true("", every?
    (method (type-info)
       let type = type-info[0];
       let test = type-info[1];
       let function = type-info[2];
       let description = type-info[3];
       if (type = <test-condition>)
         type.test = #t
         // test returns #t
         & description = #f
         // description defaults to #f
         & function = handler-fcn
       else
         instance?(function, <function>) & instance?(test, <function>)
       end if
     end method,
     all-handlers-info));
end test do-handler;

// most recent first, both done
define test do-handler-2 ()
  let all-handlers-info = #();
  let inner-handler-fcn
    = method (condition, next-handler)
        list(condition, next-handler)
      end method;
  let outer-handler-fcn
    = method (condition, next-handler)
        list(condition, next-handler)
      end method;
  let handler <test-error> = outer-handler-fcn;
  let handler <test-condition> = inner-handler-fcn;
  //
  // This collects info about all active handlers into a list.
  //
  do-handlers
    (method (type, test, function, description)
       all-handlers-info
         := pair(list(type, test, function, description), all-handlers-info)
     end method);
  begin
    local method ordering (type)
            find-key
              (all-handlers-info,
               method (s)
                 s.head = type
               end method)
          end method ordering;
    <test-error>.ordering < <test-condition>.ordering
  end;
  check-true("", every?
      (method (type-info)
         let type = type-info[0];
         let test = type-info[1];
         let function = type-info[2];
         let description = type-info[3];
         if (type = <test-condition>)
           type.test = #t
           // test returns #t
           & description = #f
           // description defaults to #f
           & function = inner-handler-fcn
         elseif (type = <test-error>)
           type.test = #t
           // test returns #t
           & description = #f
           // description defaults to #f
           & function = outer-handler-fcn
         else
           instance?(function, <function>) & instance?(test, <function>)
         end if
       end method,
       all-handlers-info));
end test do-handler-2;

// Should probably extend this to include implementation-specific
// instantiable condition-types.
//

define test return-allowed? ()
  check-true("", every?
    (method (instantiable-error-type)
       let result = make(instantiable-error-type).return-allowed?;
       #f = result | #t = result
     end method,
     list(<simple-error>,
          <type-error>,
          <simple-warning>,
          <simple-restart>,
          <abort>)));
end test return-allowed?;

// Should probably extend this to include implementation-specific
// instantiable condition-types.
//

define test return-description ()
  check-true("", every?
    (method (instantiable-error-type)
       let condition = make(instantiable-error-type);
       let allowed? = condition.return-allowed?;
       if (#f = allowed?)
         #t
       elseif (#t = allowed?)
         let description = condition.return-description;
         description = #f
         | instance?(description, <string>)
         | instance?(description, <function>)
       else
         #f
       end if
     end method,
     list(<simple-error>,
          <type-error>,
          <simple-warning>,
          <simple-restart>,
          <abort>)));
end test return-description;
*/
define suite test-condition-suite ()
  test condition-classes;
  test condition-classes-hierarchy;
  test simple-error-slots;
  test type-error-slots;
  test simple-warning-slots;
  test restart-slots;
  test signal-1;
  test signal-2;
  test signal-3;
  test signal-4;
  test signal-5;
  test signal-6;
  test error-1;
  test error-2;
  test error-3;
  test cerror-1;
  test cerror-2;
  test cerror-3;
  test check-type-0;
//  test abort-0; - abort is undefined
//  test handler-bind;
//test handler-bind-1;
//  test handler-bind-2;
//  test handler-bind-3;
//  test handler-bind-4;
//  test handler-case;
//  test handler-case-1;
//  test handler-case-2;
//  test handler-case-3;
//  test handler-case-4;
//  test handler-case-5;
//  test handler-case-6;
//  test handler-case-7;
//  test default-handler-1;
//  test default-handler-2;
 // test restart-query;
//  test do-handler;
//  test do-handler-2;
//  test return-allowed?;
//  test return-description;
end suite;
