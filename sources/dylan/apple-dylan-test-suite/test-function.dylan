Module:    apple-dylan-test-suite
Filename:  test-function.dylan
Summary:   Apple Dylan test suite, test-function
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// Chapter 10. Functions

define test method-0 (description: "does it return a <method>?")
  check("", instance?,
     method (a, b)
       1 + 2
     end method,
     <method>);
end test \method-0;

define test method-1 (description: "Does it do something reasonable")
  check-equal("", method (a, b)
    a + b
  end (4, 5),
  apply(method (a, b)
            a + b
          end method,
          #(4, 5)));
  check-equal("", method (a, b)
      a + b
    end method
      (4, 5)
   , 9);
end test method-1;

define test bind-methods-0 (description: "") 
check("", \=, #(1, 2, 3, 4, 5),
  do (method (s :: <sequence>)
    local method plus-rev (seq1) 
       map(\+, seq1, seq1.reverse)
    end method plus-rev;
    local method same-element? (seq2 :: <sequence>)
       apply(\=, seq2)
    end method same-element?;
    s.plus-rev.same-element?
  end method, #(1, 2, 3, 4, 5)));
end test;

define test bind-methods-1 ()
 check-true("", method (s :: <sequence>)
      local method plus-rev (seq1)
              map(\+, seq1, seq1.reverse)
            end method plus-rev,
            method double-plus (seq2)
              seq2.plus-rev
            end method double-plus;
      s.plus-rev.double-plus
    end method
      (#(1, 2, 3, 4, 5))
    = #(12, 12, 12, 12, 12));
end test;

define test bind-methods-2 ()
  check-true("", begin
      local method func1 ()
              "func1"
            end method func1,
            method func2 ()
              "func2"
            end method func2;
      "body form 1";
      "body form 2";
      concatenate(func1(), func2())
    end
    = "func1func2");
end;

// Design note 8 clarifies Dylan's behavior for ambiguous method specialization
// Something seems to be wrong with add-method
// wonder why this is not working

define test ambiguous-method-1 (description: "unambiguous calls")
  let type1 = limited(<integer>, min: 1, max: 10);
  let type2 = limited(<integer>, min: 0, max: 7);
  let f = make(<generic-function>);
  add-method
    (f,
     method (a :: type1)
       1
     end method);
  add-method
    (f,
     method (a :: type2)
       2
     end method);
  add-method
    (f,
     method (a == 5)
       #"five"
     end method);
  check-equal("", f(8), 1);
  check-equal("", f(0), 2);
  check-equal("", f(5), #"five");
end test ambiguous-method-1;

define test ambiguous-method-2
  (signal: <error>, description: "error on ambiguous method")
  let type1 = limited(<integer>, min: 1, max: 10);
  let type2 = limited(<integer>, min: 0, max: 7);
  let f = make(<generic-function>);
  add-method
    (f,
     method (a :: type1)
       1
     end method);
  add-method
    (f,
     method (a :: type2)
       2
     end method);
  add-method
    (f,
     method (a == 5)
       #"five"
     end method);
  check-condition("", <error>, f(3));
end test ambiguous-method-2;

// add-method (86)

define test add-method-type (description: "")
  check("", instance?, add-method, <generic-function>);
end test add-method-type;

define test sorted-applicable-methods-type (description: "")
  check("", instance?, sorted-applicable-methods, <generic-function>);
end test sorted-applicable-methods-type;

// make-read-only

define test make-read-only-type (description: "")
  check("", instance?, make-read-only, <generic-function>);
end test make-read-only-type;

// freeze-methods
// generic-function-methods

define test generic-function-methods-0 (description: "")
  check-true("", ~deque-instance.generic-function-methods.empty?);
end test generic-function-methods-0;

define test generic-function-methods-type (description: "")
  check("", instance?, generic-function-methods, <generic-function>);
end test generic-function-methods-type;

// function-arguments

define test function-arguments-0 (description: "")
  check-equal("", function-arguments
    (method (a, b)
       list(a, b)
     end method),
  2);
end test function-arguments-0;

define test function-arguments-1 (disabled: #"description", "")
  begin
    let (req-no, rest-bool, kwd-seq) = withdraw.function-arguments;
    check-equal("", req-no, 2); 
    check-equal("", rest-bool, #t); 
    check-equal("", kwd-seq, #(#"passwd"));
  end
end test function-arguments-1;

// KJP: #f -> '(passwd:)

// applicable-method?

define test applicable-method?-type (description: "")
  check("", instance?, applicable-method?, <function>);
end test applicable-method?-type;

// find-method

define test find-method-type (description: "")
  check("", instance?, find-method, <function>);
end test find-method-type;

// method-specializers

define test method-specializers-type (description: "")
  check("", instance?, method-specializers, <generic-function>);
end test method-specializers-type;

// remove-method

define test remove-method-type (description: "")
  check("", instance?, remove-method, <function>);
end test remove-method-type;

define suite test-function-suite ()
  test method-0;
  test method-1;
  test bind-methods-0;
  test bind-methods-1;
  test bind-methods-2;
  test ambiguous-method-1;
  test ambiguous-method-2;
  test add-method-type;
  test sorted-applicable-methods-type;
  test make-read-only-type;
  test generic-function-methods-0;
  test generic-function-methods-type;
  test function-arguments-0;
  test function-arguments-1;
  test applicable-method?-type; 
  test find-method-type;
  test method-specializers-type;
  test remove-method-type;
end suite;
