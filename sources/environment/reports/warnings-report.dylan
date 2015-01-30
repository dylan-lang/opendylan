Module:    environment-reports
Author:    Andy Armstrong
Synopsis:  Compiler warnings report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Warning reports

define class <warnings-report> (<project-report>)
  //--- Not used yet...
  // constant slot %subprojects? :: <boolean> = #t,
  //   init-keyword: subprojects?:;
end class <warnings-report>;

install-report(#"warnings", "Compiler warnings report", <warnings-report>);


/// Report protocols

define method write-report-as
    (stream :: <stream>, report :: <warnings-report>, _format == #"text")
 => ()
  let project = report.report-project;
  let last-library = #f;
  for (warning :: <warning-object> in project-warnings(project))
    let library = environment-object-library(project, warning);
    unless (library == last-library)
      if (last-library)
        format(stream, "-----------------------\n\n")
      end;
      format(stream, "Warnings in %s library:\n",
             environment-object-primitive-name(project, library));
      format(stream, "-----------------------\n\n");
      last-library := library
    end;
    print-environment-object-name
      (stream, project, warning,
       qualify-names?:   #f,
       full-message?:    #t);
    format(stream, "\n")
  end
end method write-report-as;
