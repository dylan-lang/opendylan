Module:       testworks
Synopsis:     Testworks testing harness
Author:       Andrew Armstrong, James Kirsch
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Check macros

// Note that do-check and do-check-condition wrap up the real macro
// arguments inside methods to delay their evaluation until they are
// within the scope of whatever condition handling is required.

define macro check
  { check (?check-name:expression, 
           ?check-function:expression, ?check-args:*) }
    => { do-check(method () ?check-name end,
                  method () vector(?check-function, vector(?check-args)) end) }
end macro check;

define macro check-equal
  { check-equal (?check-name:expression, 
                 ?expected-value:expression, ?actual-value:expression) }
    => { do-check(method () ?check-name end,
                  method () 
                    vector(\=, vector(?expected-value, ?actual-value))
                  end) }
end macro check-equal;

define macro check-instance?
  { check-instance? (?check-name:expression,
                     ?value-type:expression, ?value:expression) }
    => { do-check(method () ?check-name end,
                  method () 
                    vector(instance?, vector(?value, ?value-type))
                  end) }
end macro check-instance?;

define macro check-true
   { check-true (?check-name:expression, ?check-expression:expression) }
    => { do-check(method () ?check-name end,
                  method ()
                    vector(\~=, vector(#f, ?check-expression))
                  end) }
end macro check-true;

define macro check-false
  { check-false (?check-name:expression, ?check-expression:expression) }
    => { do-check(method () ?check-name end,
                  method ()
                    vector(\=, vector(#f, ?check-expression))
                  end) }
end macro check-false;

define macro check-condition
  { check-condition 
     (?check-name:expression, 
      ?check-condition:expression, ?check-body:expression) }
    => { do-check-condition(method () ?check-name end,
                            method () 
                              vector(?check-condition,
                                     method () ?check-body end)
                            end) }
end macro check-condition;

define macro check-no-errors
  { check-no-errors(?check-name:expression, ?check-body:expression) }
    => { check-true(?check-name, begin ?check-body; #t end) }
end macro check-no-errors;




/// Check implementation functions

define method evaluate-name-function
    (name-function :: <function>) => (name-or-error)
  maybe-trap-errors
    (begin
       let name = name-function();
       if (instance?(name, <string>))
         if (*announce-checks?* & *announce-check-function*)
           *announce-check-function*(name)
         end
       else
         error("Check name is not a string: %=", name)
       end;
       name
     end)
end method evaluate-name-function;

define method do-check
    (name-function :: <function>, argument-function :: <function>) 
 => (status :: <result-status>)
  block ()
    let name = evaluate-name-function(name-function);
    let check-arguments = maybe-trap-errors(argument-function());
    case
      instance?(name, <error>) =>
	record-check("[*** Invalid name ***]", name, name, #f);
      instance?(check-arguments, <error>) =>
	record-check(name, check-arguments, check-arguments, #f);
      otherwise =>
	let function  = check-arguments[0];
	let arguments = check-arguments[1];
	let result = maybe-trap-errors(apply(function, arguments));
	let status :: <result-status>
	  = if (~result)
	      #"failed"
	    elseif (instance?(result, <error>))
	      result
	    else
	      #"passed"
	    end if;
	if (status == #"failed" & debug-failures?())
	  break("Check failed: %s", name)
	end if;
	record-check(name, status, function, arguments)
    end case;
  exception (r :: <simple-restart>,
	     init-arguments: vector(format-string:, "Skip this check",
				    format-arguments:, #[]))
    #"failed"
  end block;
end method do-check;

define method do-check-condition
    (name-function :: <function>, argument-function :: <function>) 
 => (status :: <result-status>)
  let name = evaluate-name-function(name-function);
  let check-arguments = maybe-trap-errors(argument-function());
  let condition-class
    = if (instance?(check-arguments, <sequence>))
        maybe-trap-errors
          (begin
             let class = check-arguments[0];
             unless (instance?(class, <class>))
               error("Check collection class is not a class: %=", class)
             end;
             class
           end)
      end;
  case
    instance?(name, <error>) =>
      record-check("[*** Invalid name ***]", name, name, #f);
    instance?(check-arguments, <error>) =>
      record-check(name, check-arguments, check-arguments, #f);
    instance?(condition-class, <error>) =>
      record-check(name, condition-class, condition-class, #f);
    otherwise =>
      let function = check-arguments[1];
      let result
        = maybe-trap-errors
            (block (return)
               let handler condition-class
                 = method (condition :: <condition>, next-handler :: <function>)
                     ignore(condition, next-handler);
                     return(#"passed")
                   end;
               function();
               #"failed"
             end);
      if (result == #"failed" & debug-failures?())
        break("Check condition failed: %s", name)
      end;
      record-check(name, result, condition-class, #f)
  end
end method do-check-condition;


/// Check failure reporting

define method failure-reason
    (status :: <result-status>,
     operation /* :: <check-operation-type> */,
     value :: <check-value-type>)
 => (reason :: false-or(<string>))
  #f
end method failure-reason;

define method failure-reason
    (status :: <result-status>,
     operation :: <string>,
     value :: <check-value-type>)
 => (reason :: false-or(<string>))
  operation
end method failure-reason;

define method failure-reason
    (status == #"failed",
     operation :: <function>,
     value :: <check-value-type>)
 => (reason :: false-or(<string>))
  select (operation)
    \= =>
      format-to-string("%= ~= %=",  first(value), second(value));
    \== =>
      format-to-string("%= ~== %=", first(value), second(value));
    instance? =>
      format-to-string("%= not an instance of %=", first(value), second(value));
    otherwise =>
      #f;
  end
end method failure-reason;

define method failure-reason
    (status == #"failed",
     operation :: subclass(<condition>),
     value :: <check-value-type>)
 => (reason :: false-or(<string>))
  "expected condition not signaled"
end method failure-reason;

// Make two tries to get a nice error message and then give up!
define function safe-error-to-string
    (error :: <error>) => (string :: <string>)
  block ()
    format-to-string("%s", error)
  exception (format-error :: <error>)
    block ()
      format-to-string("*** Crashed printing error: %s", format-error)
    exception (<error>)
      "*** Crashed printing error ***"
    end
  end
end function safe-error-to-string;

define method failure-reason
    (status :: <error>,
     operation :: <check-operation-type>,
     value :: <check-value-type>)
 => (string :: false-or(<string>))
  safe-error-to-string(status)
end method failure-reason;

define method print-failure-reason
    (status :: <result-status>,
     operation :: <check-operation-type>,
     value :: <check-value-type>)
 => ()
  let reason = failure-reason(status, operation, value);
  reason & test-output(" [%s]", reason)
end method print-failure-reason;

define method print-error
    (error :: <error>) => ()
  test-output(" [%s]", safe-error-to-string(error))
end method print-error;


/// Check progress functions

define method print-check-progress
    (result :: <unit-result>) => ()
  let status = result.result-status;
  let name = result.result-name;
  select (status)
    #"not-executed" =>
      test-output("Ignored check: %s", name);
    otherwise =>
      test-output("Ran check: %s %s", name, status-name(status));
  end;
  print-failure-reason(status, result.result-operation, result.result-value);
  test-output("\n");
end method print-check-progress;


/// Check recording

define thread variable *check-recording-function* = print-check-progress;

define method record-check
    (name :: <string>, status :: <result-status>,
     operation :: <check-operation-type>, value :: <check-value-type>)
 => (status :: <result-status>)
  *check-recording-function*(make(<check-result>,
                                  name: name, status: status, 
                                  operation: operation, value: value));
  status
end method record-check;


