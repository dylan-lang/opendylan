Module:       testworks
Synopsis:     Contains <component> definitions for Testworks test harness
Author:       Shri Amit, Andrew Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Component
///
/// This is the class of objects that can be performed in a test suite.
/// It is the superclass of both <test> and <suite>.  Note that there are
/// no <check> or <benchmark> classes so they aren't considered "components".

define class <component> (<object>)
  constant slot component-name :: <string>, 
    required-init-keyword: name:;
  constant slot component-description :: <string> = "", 
    init-keyword: description:;
  constant slot component-tags :: <sequence> = #[], 
    init-keyword: tags:;
end class <component>;


/// Result handling

define class <component-result> (<result>)
  constant slot result-subresults :: <sequence> = make(<stretchy-vector>), 
    init-keyword: subresults:;
end class <component-result>;

define class <test-result> (<component-result>)
end class <test-result>;

define method result-type-name
    (result :: <test-result>) => (name :: <string>)
  "Test"
end;

define class <suite-result> (<component-result>)
end class <suite-result>;

define method result-type-name
    (result :: <suite-result>) => (name :: <string>)
  "Suite"
end;


/// Perform component

define method perform-component
    (component :: <component>, options :: <perform-options>,
     #key report-function        = *default-report-function*,
          report-format-function = *format-function*)
 => (component-result :: <component-result>)
  let progress-format-function
    = options.perform-progress-format-function;
  let announce-checks? = options.perform-announce-checks?;
  let result
    = dynamic-bind (*format-function* = progress-format-function,
                    *announce-checks?* = announce-checks?)
        maybe-execute-component(component, options)
      end;
  display-results(result,
		  report-function: report-function,
		  report-format-function: report-format-function);
  result;
end method perform-component;


/// Execute component

// This function can be used to implement any desired
// criteria to execute or not execute independent
// tests & suites.

define open generic execute-component?
    (component :: <component>, options :: <perform-options>);

define method execute-component? 
    (component :: <component>, options :: <perform-options>)
 => (answer :: <boolean>)
  tags-match?(options.perform-tags, component.component-tags);
end method execute-component?;

define method maybe-execute-component
    (component :: <component>, options :: <perform-options>) 
 => (result :: <component-result>)
  let announce-function
    = options.perform-announce-function;
  if (announce-function)
    announce-function(component)
  end;
  let (subresults, perform-status)
    = if (execute-component?(component, options))
	execute-component(component, options)
      else
	values(#(), #"not-executed")
      end;
  let result-class = select (component by instance?)
                       <test>  => <test-result>;
                       <suite> => <suite-result>;
                     end;
  make(result-class,
       name:         component.component-name,
       status:       perform-status,
       subresults:   subresults)
end method maybe-execute-component;

