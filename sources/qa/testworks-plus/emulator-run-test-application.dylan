Module:    testworks-plus
Filename:  run-test-application.dylan
Author:    Shri Amit(amit), Andrew Armstrong (andrewa)
Synopsis:  The main "startup" function for test-suite apps
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method perform-suite-to-file
    (suite :: <suite>, filename :: <string>,
     #key report-function   = log-report-function,
          progress-function = null-progress-function)
 => (result :: <result>)
  let stream = make(<file-stream>, locator: filename, direction: #"output");
  block ()
    perform-suite
      (suite,
       progress-function: progress-function,
       report-format-function: method (format-string, #rest args)
				 apply(format, stream, format-string, args)
			       end,
       report-function: report-function)
  cleanup
    close(stream)
  end
end method perform-suite-to-file;

define method run-test-application
    (suite :: <suite>, 
     #key filename,
          report-function = log-report-function,
          progress-function = null-progress-function)
 => (result :: <result>)
  if (filename)
    perform-suite-to-file(suite, filename,
                          report-function: report-function,
                          progress-function: progress-function)
  else
    perform-suite(suite,
                  report-function: report-function,
                  progress-function: progress-function)
  end
end method run-test-application;