Module: dfmc-management
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Progress reporting

define thread variable *progress-stream* = #f;
define thread variable
  *progress-library* :: false-or(<project-library-description>) = #f;

define thread variable *library-increment* :: <float> = 1.0;
define thread variable *source-record-increment* :: <float> = 1.0;

define thread variable *stage-text-prefix* = "";

define thread variable *current-progress* :: <float> = 0.0;

// for synchronization at the stage and library level
define thread variable *previous-progress* :: <float> = 0.0;

define variable *internal-reporting?* :: <boolean> = #t;
// define variable *progress-debugging?* :: <boolean> = #f;

define function internal-reporting-setter
    (report? :: <boolean>)  => (report? :: <boolean>)
  *internal-reporting?* := report?;
end function;

// internal (debugging) output
define method progress-line (#rest args) => ()
  if(*internal-reporting?*)
    apply(curry(internal-progress-text, *progress-library*), args);
  end;
end method;

define function %progress-debugging(the-string, #rest args)
  debug-out(#"Progress-reports", the-string, args);
end;

// internal progress debugging output
define method progress-debug-message (#rest args) => ()
  apply(%progress-debugging, args)
end method;

// for manual progress reporting
define method progress-report(add-done-time :: <float>, #key abort-safe? = #t)
  if(*progress-library*)
    *current-progress* := *current-progress* + add-done-time;
    library-progress-report(*progress-library*, *current-progress*,
                            abort-safe?: abort-safe?)
  end;
end;

// for manual synchronization
define method current-progress(done-time :: <float>, #key abort-safe? = #t)
  if(*progress-library*)
    progress-debug-message
      ("Synchronizing progress for %s to %=",
       *progress-library*.library-description-project,
       done-time);
    *current-progress* := done-time;
    library-progress-report(*progress-library*, *current-progress*,
                            abort-safe?: abort-safe?)
  end;
end;

// for extra text
define method progress-report-text(format-text :: <string>, #rest args)
  apply(curry(library-progress-text, *progress-library*), format-text, args)
end;

define method source-record-progress-report(#key abort-safe? = #t)
  if(*progress-library*)
    *current-progress* := *current-progress* + *source-record-increment*;
    library-progress-report(*progress-library*, *current-progress*,
                            abort-safe?: abort-safe?);
  end;
end;

define method source-record-progress-text(format-text :: <string>,
                                          #rest args);
  apply(progress-report-text, format-text, args)
end;

define sideways method report-condition (condition :: <condition>) => ()
  library-condition-report(*progress-library*, condition)
end method;

define open generic library-progress-text(context,
                                          #rest args);

define open generic library-stage-text(context,
                                       #rest args);

define open generic internal-progress-text(context,
                                           #rest args);

define open generic library-progress-report(context,
                                            sofar :: <float>,
                                            #key abort-safe? = #t);

define open generic library-condition-report(context,
                                             condition :: <condition>);

// this macro is called by clients to reset the progress
// we cannot synchronize in case of recursive invocations
define macro with-progress-reports
  { with-progress-reports ?:body end }
  => {
       dynamic-bind (*current-progress* = 0.0)
         ?body;
       end dynamic-bind;
      }
end macro with-progress-reports;

define function do-with-library-progress
    (function :: <function>, library :: <project-library-description>,
     increment :: <float>)
  dynamic-bind (*library-increment* = increment,
                *progress-library* = library,
                *previous-progress* = *current-progress*)
    progress-debug-message("library increment: %s %=",
                           library, *library-increment*);
    progress-report(0.0); // manually report current progress
    function();
    progress-debug-message("Synchronizing progress for library %s",
                           library.library-description-project);
    current-progress(*previous-progress* + *library-increment*);
  end dynamic-bind;
end function do-with-library-progress;

// this macro is called by clients before an operation on a library is called
define macro with-library-progress
  { with-library-progress (?context:expression, ?increment:expression) ?:body end }
    => { do-with-library-progress(method () ?body end, ?context, ?increment) }
end macro with-library-progress;

define function do-with-stage-progress
    (function :: <function>, stage :: <string>, increment :: <float>)
  let library = *progress-library*;
  let library-stage-increment = *library-increment* * increment;
  let number-of-records =
    if(library)
      size(library.compilation-context-records)
    else
      0
    end;
  dynamic-bind (*stage-text-prefix* = stage,
                *previous-progress* = *current-progress*,
                *source-record-increment* = if(number-of-records > 0)
                                              library-stage-increment /
                                                number-of-records
                                            else
                                              0.0
                                            end)
    block ()
      if(library)
        library-stage-text(library, stage);
        let library-name =
          as-lowercase(as(<string>,
                       library-description-emit-name(library)));
        progress-line("%s %s", stage, library-name);
        progress-debug-message("library stage increment: %=",
                               library-stage-increment);
        progress-debug-message("source record increment: %=",
                               *source-record-increment*);
      end;
      function();
    cleanup
      if(library)
        progress-debug-message("###Stage %s completed, previous progress = %=",
                               stage, *previous-progress*);
        progress-debug-message("Synchronizing stage progress for %s",
                               library.library-description-project);
        current-progress(*previous-progress* + library-stage-increment);
      end;
    end block;
  end dynamic-bind;
end function do-with-stage-progress;

// this macro is called by the compiler before each stage
define macro with-stage-progress
  { with-stage-progress (?text-prefix:expression,
                         ?stage-increment:expression)
      ?:body
    end }
    => { do-with-stage-progress
          (method () ?body end, ?text-prefix, ?stage-increment) }
end macro with-stage-progress;

// the constants below should add up to 1.00

define constant $installing-stage :: <float> = 0.01;
// there are in fact two passes while generating definitions
// we don't report second pass
// TO DO: need some solution
define constant $parsing-stage :: <float> = 0.15;
define constant $models-stage-time :: <float> = 0.08;
define constant $dfm-stage-time :: <float> = 0.09;
define constant $bindings-check-stage-time :: <float> = 0.01;
define constant $typist-stage-time :: <float> = 0.01;
define constant $optimize-stage-time :: <float> = 0.21;
define constant $heaping-stage-time :: <float> = 0.20;
define constant $linking-stage-time :: <float> = 0.10;
define constant $save-db-stage-time :: <float> = 0.15;
