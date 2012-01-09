Module:    environment-reports
Author:    Bruce Mitchener
Synopsis:  Dependency report generator
Copyright:    Original Code is Copyright (c) 2011 Dylan Hackers.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dependency-report> (<project-report>)
  constant slot visited = make(<string-table>);
end class <dependency-report>;

install-report(#"dependency-graph", "Dependency graph", <dependency-report>,
               formats: #[#"text", #"dot"]);

define method write-report-as
    (stream :: <stream>, report :: <dependency-report>, _format == #"text") => ()
  let project = report.report-project;
  format(stream, "Dependency data for %s:\n\n", project.project-name);
  write-project(report, stream, project, _format);
end method write-report-as;

define method write-project
    (report :: <dependency-report>, stream :: <stream>, project :: <project-object>,
     _format == #"text")
  unless (element(report.visited, project.project-name, default: #f))
    report.visited[project.project-name] := #t;
    unless (open-project-compiler-database(project))
      parse-project-source(project)
    end;
    let used = project.project-used-projects;
    format(stream, "%s:\n", project.project-name);
    for (p in used)
      format(stream, "    %s\n", p.project-name);
    end;
    for (p in used)
      write-project(report, stream, p, _format);
    end;
  end;
end method write-project;

define method write-report-as
    (stream :: <stream>, report :: <dependency-report>, _format == #"dot") => ()
  let project = report.report-project;
  format(stream, "digraph dependencies {\n");
  write-project(report, stream, project, _format);
  format(stream, "    label=\"Dependency graph for %s\"\n", project.project-name);
  format(stream, "    fontsize=20\n");
  format(stream, "}\n");
end method write-report-as;

define method write-project
    (report :: <dependency-report>, stream :: <stream>, project :: <project-object>,
     _format == #"dot")
  unless (element(report.visited, project.project-name, default: #f))
    report.visited[project.project-name] := #t;
    unless (open-project-compiler-database(project))
      parse-project-source(project)
    end;
    let used = project.project-used-projects;
    for (p in used)
      format(stream, "    \"%s\" -> \"%s\"\n", project.project-name, p.project-name);
    end;
    for (p in used)
      write-project(report, stream, p, _format);
    end;
  end;
end method write-project;

