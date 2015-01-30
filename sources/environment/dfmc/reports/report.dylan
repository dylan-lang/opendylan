Module:    dfmc-environment-reports
Synopsis:  DFMC report generator
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Report generator

define constant $depth-library = 1;
define constant $depth-modules = 2;
define constant $depth-variables = 3;
define constant $depth-definitions = 4;

define class <report> (<object>)
  slot report-stream :: <stream>,
    required-init-keyword: stream:;
  slot report-name,
    required-init-keyword: name:;
  slot report-context,
    required-init-keyword: context:;
  slot report-inherited? :: <boolean> = #f,
    init-keyword: inherited?:;
  slot report-internal? :: <boolean> = #f,
    init-keyword: internal?:;
  slot report-source? :: <boolean> = #f,
    init-keyword: source?:;
  slot report-depth :: <integer> = $depth-definitions;
  slot report-indent-depth :: <integer> = 0,
    setter: %report-indent-depth-setter;
  slot report-indent-pending :: <boolean> = #f;
end class <report>;

// Initialize: convert depth keyword to depth value
define method initialize
    (report :: <report>,
     #key depth :: one-of(#"library",
                          #"modules",
                          #"variables",
                          #"definitions") = #"definitions")
 => ()
  next-method();
  report.report-depth := select (depth)
                           #"library"     => $depth-library;
                           #"modules"     => $depth-modules;
                           #"variables"   => $depth-variables;
                           #"definitions" => $depth-definitions;
                         end;
end method initialize;

// Set indent depth
define method report-indent-depth-setter
    (depth :: <integer>, report :: <report>)
 => (depth :: <integer>)
  report.%report-indent-depth := depth;
//report.report-indent-pending := #t;
  depth
end method report-indent-depth-setter;

// Write text to the report
define generic report-out (report :: <report>, object :: <object>) => ();

// Write an object in Dylan format to the report
define generic report-object (report :: <report>, object :: <object>) => ();


/// Report generation

// Add a level of indenting for a segment of report generation
define macro report-indent-one
  { report-indent-one (?report:name)
      ?:body
    end }
    => { block ()
           let save-indent-depth = ?report.report-indent-depth;
           ?report.report-indent-depth := save-indent-depth + 1;
           block ()
             ?body
//           exception (condition :: <condition>)
             // Prevent errors from stopping the report
//             debug-out(#"dfmc-environment-reports", "Condition: %=", condition);
           end;
           ?report.report-indent-depth := save-indent-depth;
         end }
end macro report-indent-one;

// Write indentation at start of report line
define method report-write-indent (report :: <report>) => ()
  if (report.report-indent-pending)
    report.report-indent-pending := #f;
    let stream = report.report-stream;
    for (i from report.report-indent-depth above 0)
      format(stream, "  ");
    end for;
  end if;
end method report-write-indent;

// Write formatted output to a report
define method format-to-report
    (report :: <report>, format-string :: <string>, #rest items) => ()
  report-write-indent(report);
  apply(format, report.report-stream, format-string, items);
  if (last(format-string, default: #f) = '\n')
    report.report-indent-pending := #t;
  end if;
end method format-to-report;


/// Basic report output

// Report a string
define method report-out (report :: <report>, string :: <string>) => ()
  format-to-report(report, "%s", string);
end method report-out;

// Report a number
define method report-out (report :: <report>, number :: <number>) => ()
  format-to-report(report, "%d", number);
end method report-out;


/// Basic Dylan object reporting

// Report an arbitrary object
define method report-object (report :: <report>, object :: <object>) => ()
  format-to-report(report, "%=", object);
end method report-object;

// Report a symbol
define method report-object (report :: <report>, symbol :: <symbol>) => ()
  format-to-report(report, "#\"%s\"", as(<string>, symbol));
end method report-object;

// Report a number
define method report-object (report :: <report>, number :: <number>) => ()
  report-out(report, number);
end method report-object;

// Report a boolean
define method report-object (report :: <report>, boolean :: <boolean>) => ()
  format-to-report(report, if (boolean) "#t" else "#f" end);
end method report-object;

// Report a string
define method report-object (report :: <report>, string :: <string>) => ()
  format-to-report(report, "\"%s\"", string);
end method report-object;

// Report the elements of a collection
define function report-elements
    (report :: <report>, collection :: <collection>) => ()
  for (object in collection,
       i from size(collection) to 0)
    report-object(report, object);
    if (i > 1)
      report-out(", ");
    end if;
  end for;
end function report-elements;

// Report a collection
define method report-object
    (report :: <report>, collection :: <collection>) => ()
  report-out(report, "{");
  report-elements(report, collection);
  report-out(report, "}");
end method report-object;

// Report a vector
define method report-object (report :: <report>, vector :: <vector>) => ()
  report-out(report, "[");
  report-elements(report, collection);
  report-out(report, "]");
end method report-object;

// Report a list
define method report-object (report :: <report>, list :: <list>) => ()
  report-out(report, "(");
  report-elements(report, collection);
  report-out(report, ")");
end method report-object;

// Report a pair
define method report-object (report :: <report>, pair :: <pair>) => ()
  report-out(report, "(");
  report-object(report, pair.head);
  report-out(report, " . ");
  report-object(report, pair.tail);
  report-out(report, ")");
end method report-object;
