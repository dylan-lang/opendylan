Module:       testworks
Summary:      TestWorks a test harness library
Author:       Andrew Armstrong, Shri Amit, James Kirsch
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///*** Constants ***///

define constant $all = #[#"all"];

define constant <check-operation-type> 
  = false-or(type-union(<function>, <error>, <string>, subclass(<condition>)));

define constant <check-value-type>
  = false-or(type-union(<sequence>, <condition>));


/// Result handling

define constant <result-status>
  = type-union(one-of(#"passed", #"failed", #"not-executed"), <error>);

define method status-name
    (status :: <result-status>) => (name :: <string>)
  select (status)
    #"passed"       => "passed";
    #"failed"       => "failed";
    #"not-executed" => "not executed";
    otherwise       => "crashed";
  end
end method status-name;

define class <result> (<object>)
  constant slot result-name :: <string>,
    required-init-keyword: name:;
  constant slot result-status :: <result-status>, 
    required-init-keyword: status:;
end class <result>;

define open generic result-type-name
    (result :: <result>) => (name :: <string>);

define method \=
    (result1 :: <result>, result2 :: <result>)
 => (equal? :: <boolean>)
  // We want to know if two error messages are the same, so that "crashed"
  // tests aren't always presented as differences.  However, \= isn't
  // specialized on <error>, so we create our own test:
  local method same-error-message?
            (s1 :: <object>, s2 :: <object>)
         => (same? :: <boolean>)
          instance?(s1, <simple-error>)
          & instance?(s2, <simple-error>)
          & format-to-string(condition-format-string(s1),
                             condition-format-arguments(s1))
            = format-to-string(condition-format-string(s2),
                               condition-format-arguments(s2));
        end method same-error-message?;
  result1.result-name = result2.result-name
  & (result1.result-status = result2.result-status
     | same-error-message?(result1.result-status, result2.result-status))
end method \=;


///*** State Variables ***///

define thread variable *debug?* = #f;

define thread variable *format-function* = format-out;

define thread variable *announce-checks?* :: <boolean> = #f;

define thread variable *announce-check-function* :: false-or(<function>) = #f;

define method debug-failures?
    () => (debug-failures? :: <boolean>)
  *debug?* == #t
end method debug-failures?;

define method debug? 
    () => (debug? :: <boolean>)
  *debug?* ~= #f
end method debug?;

define method test-output
    (format-string :: <string>, #rest format-args) => ()
  apply(*format-function*, format-string, format-args)
end method test-output;

///*** Generic Classes, Helper Functions, and Helper Macros ***///

define method plural (n :: <integer>) => (ending :: <string>)
  if (n == 1) "" else "s" end if
end;

define macro maybe-trap-errors 
  { maybe-trap-errors (?body:body) }
    => { local method maybe-trap-errors-body () ?body end;
         if (*debug?*)
           maybe-trap-errors-body();
         else
           block ()
             maybe-trap-errors-body();
           exception (the-error :: <error>)
             the-error
           end;
         end; }
end macro maybe-trap-errors; 
 
define method tags-match? (run-tags :: <sequence>, object-tags :: <sequence>)
 => (bool :: <boolean>)
  run-tags = $all | ~empty?(intersection(run-tags, object-tags))
end method tags-match?;


/// Perform options

// this class defines all the options that might be used
// to control test suite performing.

define open class <perform-options> (<object>)
  slot perform-tags :: <sequence> = $all, 
    init-keyword: tags:;
  slot perform-announce-function :: false-or(<function>) = #f,
    init-keyword: announce-function:;
  slot perform-announce-checks? :: <boolean> = *announce-checks?*,
    init-keyword: announce-checks?:;
  slot perform-progress-format-function = *format-function*,
    init-keyword: progress-format-function:;
  slot perform-progress-function = *default-progress-function*,
    init-keyword: progress-function:;
  slot perform-debug? = *debug?*,
    init-keyword: debug?:;
end class <perform-options>;
