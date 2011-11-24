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


define test condition-classes (description: "see if they exist")
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

define test condition-classes-hierarchy
  (description: "check superclasses for each condition class")
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

define test simple-error-slots (description: "")
  let e
    = make(<simple-error>,
           format-string: "This is a test.",
           format-arguments: #(1, 2, 3));
  check-true("", "This is a test." = e.condition-format-string);
  check-true("", #(1, 2, 3) = e.condition-format-arguments);
end test simple-error-slots;

define test type-error-slots (description: "")
  let e = make(<type-error>, value: 7, type: <string>);
  check-true("", 7 = e.type-error-value & <string> = e.type-error-expected-type);
end test type-error-slots;

define test simple-warning-slots (description: "")
  let e
    = make(<simple-warning>,
           format-string: "This is a test.",
           format-arguments: #(1, 2, 3));
  check-true("", "This is a test." = e.condition-format-string);
  check-true("", #(1, 2, 3) = e.condition-format-arguments);
end test simple-warning-slots;

define test restart-slots
  (description: "condition: is accepted by <restart>, but ignored for now")
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

define test signal-1 (description: "signal")
  check-condition("", <error>, signal(make(<test-condition>)));
end test signal-1;

define test signal-2 (description: "simple-error")
  check-condition("", <simple-error>, 
		  signal(make(<test-error>, format-string: "Just testing.")));
end test signal-2;

define test signal-3 (description: "simple-warning")
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

define test signal-4
  (description: "default handler called by default")
  check-true("", signal(make(<catcher-test-condition>)) = #"catcher");
end test signal-4;

// Are signal and error really supposed to return true here???

define test signal-5 (description: "string, no args")
  check-condition("", <simple-warning>, signal("Just testing."));
end test signal-5;

define test signal-6
  (description: "string, some args")
  check-condition("", <simple-error>, signal("Just testing.", 1, 2, 3));
end test signal-6;

define test error-1 (description: "")
  check-condition("", <test-error>, error(make(<test-error>)));
end test error-1;

define test error-2 (description: "string, no args")
  check-condition("", <error>, error("Just testing."));
end test error-2;

define test error-3 (description: "string, some args")
  check-condition("", <simple-error>, error("Just testing.", 1, 2, 3));
end test error-3;

define test cerror-1 (description: "")
  check-condition("", <test-error>, 
		  cerror("Restart description", make(<test-error>)));
end test cerror-1;

define test cerror-2 (description: "string, no args")
  check-condition("", <simple-error>,
		  cerror("Restart description", "Just testing."));
end test cerror-2;

define test cerror-3 (description: "string, some args")
  check-condition("", <simple-error>,
		  cerror("Restart description", "Just testing.", 1, 2, 3));
end test cerror-3;

define test check-type-0 (description: "")
  check-condition("", <type-error>, check-type(3, <string>));
end test check-type-0;

// This is because abort is undefined!!
//define test abort-0 (description: "")
//  check-condition("", <abort>, abort());
//end test abort-0;

// Handler-bind
/*
define test handler-bind (description: "No condition to handle")
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

define test handler-bind-1 (description: "#f if no forms")
  check-false("", let handler <simple-error> = method (c, h)
                                    list(c, h)
                                  end method);
end test handler-bind-1;


define test handler-bind-2 (description: "With condition to handle")
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

define test handler-bind-3 (description: "call the handler in the function")

    let handler <simple-error> = method (c, h)
                                   #"outer"
                                 end method;
    let handler <simple-error> = method (c, d)
                                   d()
                                 end method;
  check-equal("", signal(make(<simple-error>)),  
  #"outer");
end test handler-bind-3;

define test handler-bind-4 (description: "check test arg")
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

define test handler-case
  (description: "signal with string signals simple-warning")
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

define test handler-case-1 (description: "but signal can signal anything")
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

define test handler-case-2
  (description: "error with string signals simple-error")
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

define test handler-case-3 (description: "but error can signal anything")
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

define test handler-case-4
  (description: "cerror with string signals simple-error")
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

define test handler-case-5 (description: "but cerror can signal anything")
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

define test handler-case-6
  (description: "check-type with string signals type-error")
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

define test handler-case-7 (description: "abort")
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

define test default-handler-1
  (description: "this should return #f and also print a warning")
  check-true("", #f
  = make(<simple-warning>,
         format-string: "This warning is part of the test suite, please ignore.").default-handler);
end test default-handler-1;

// <simple-restart> should be able to have default-handler
// called upon it as <condition> is one of its superclasses
//
define test default-handler-2 (signal: <error>, description: "")
  check-true("", make(<simple-restart>).default-handler);
end test default-handler-2;

// restart-query

define test restart-query
  (description: "there is a default handler for <restart>")
  check-true("", make(<simple-restart>).restart-query); //  do nothing | #t
end test restart-query;

// do-handler

define test do-handler (description: "check the args to do-handlers")
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

define test do-handler-2 (description: "most recent first, both done")
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

define test return-allowed? (description: "")
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

define test return-description (description: "")
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
