Module:    dfmc-environment-reports-test
Synopsis:  dfmc-environment-reports test library
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Test report generation

define method test-report
    (library-name :: <string>,
     #key inherited? :: <boolean> = #f,
          internal? :: <boolean> = #t,
          source? :: <boolean> = #f,
          depth :: one-of(#"library",
                          #"modules",
                          #"variables",
                          #"definitions") = #"definitions")
 => ()
  let project = lookup-named-project(library-name);
  let context = project.project-current-compilation-context;
  let report = make(<report>,
                    stream: *standard-output*,
                    name: library-name,
                    context: context,
                    inherited?: inherited?,
                    internal?: internal?,
                    source?: source?,
                    depth: depth);
  do-report(report);
end method test-report;
