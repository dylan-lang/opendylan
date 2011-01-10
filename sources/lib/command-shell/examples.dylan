Module: dw
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *top-prompt* = "dw=> ";

define constant *dw-banner* = "\tDylanWorks compiler v.00\n";

define variable *top-level-loop* = make(<command-loop>,
					command-table: make(<table>),
					prompt: *top-prompt*,
					source: *standard-input*,
					sink: *standard-output*,
					banner: *dw-banner*,
					level: 0);

define variable *current-library-context* = #f;
define variable *current-file-context* = #f;

// this one for testing
define command-argument
  flags: #("f"),
  keyword: #"file",
  value?: #t
end;

define command-argument
  flags: #("force-compile"),
  keyword: #"force-compile"
end;

define command-argument
  flags: #("force"),
  keyword: #"force"
end;

define command-argument
  flags: #f,
  keyword: #"project"
end;

define command-argument
  flags: #f,
  keyword: #"boolean"
end;

define command-argument
  flags: #("force-parse"),
  keyword: #"force-parse"
end;

define command-argument
  flags: #("export-only"),
  keyword: #"export-only"
end;

define command-argument
  flags: #("flush"),
  keyword: #"flush"
end;

define command-argument
  flags: #("harp"),
  keyword: #"harp"
end;

define command-argument
  flags: #f,
  keyword: #"library"
end;

define command-argument
  flags: #f,
  keyword: #"filename"
end;

define command-argument
  flags: #f,
  keyword: #"formname"
end;

define command-argument
  flags: #("m"),
  keyword: #"mode",
  value?: #t
end;

define command-argument
  flags: #f,
  keyword: #"restart-option"
end;

define command-argument
  flags: #("report"),
  keyword: #"report"
end;

// for debugging

define shell-command in *top-level-loop*
  print(file, mode, library, force-compile, force-parse, harp)
  format-out("file %s ", file);
  format-out("mode %s ", mode);
  format-out("library %s ", library);
  format-out("force-compile %s ", force-compile);
  format-out("force-parse %s ", force-parse);
  format-out("harp %s ", harp);
  // format-out("report %s ", report);
  format-out("\n");
end;

define shell-command in *top-level-loop* 
  exit()
  signal(make(<end-of-loop>))
end;

define shell-command in *top-level-loop* 
  compile(library, force-compile, force-parse, harp, report)
  compile-library
    (library, force-compile?: force-compile, force-parse?: force-parse, harp-output?: harp);
  if(report)
    user-message(context, "Reporting...\n")
  end;
end;

define shell-command in *top-level-loop* 
  recompile(library)
  recompile-library (library);
end;

define shell-command in *top-level-loop* 
  statistics(library, force)
  report-library-statistics(library, force?: force);
end;

define shell-command in *top-level-loop* 
  save(library, export-only, flush)
  save-library (library, export-only?: export-only, flush?: flush);
end;

define shell-command in *top-level-loop* 
  flush(library)
  let saved-write? = write-databases?();
  block ()
    write-databases?() := #f;
    save-library (library, flush?: #t);
  cleanup
    write-databases?() := saved-write?;
  end block;
end;

define shell-command in *top-level-loop* 
  save(library, export-only)
  save-library (library, export-only?: export-only);
end;

// alternate entry points for debugging  Nosa

define shell-command in *top-level-loop* 
  compile-to-heap(library)
  compile-library(library, force-compile?: #t, skip-emit?: #t, skip-link?: #t);
end;

define shell-command in *top-level-loop* 
  compile-to-models(library)
  compile-library-to-models(library);
end;

define shell-command in *top-level-loop* 
  heap(library, harp)
  heap-library
    (library, harp-output?: harp);
end;

define shell-command in *top-level-loop* 
  emit(library, harp)
  emit-library
    (library, harp-output?: harp);
end;

define shell-command in *top-level-loop* 
  link(library, harp)
  link-library
    (library, harp-output?: harp);
end;

define shell-command in *top-level-loop* 
  link-glue(library, harp)
  link-glue
    (library, harp-output?: harp);
end;

define shell-command in *top-level-loop* 
  type-estimate(library)
  type-estimate-library(library);
end;

define shell-command in *top-level-loop* 
  optimize(library)
  optimize-library(library);
end;

define shell-command in *top-level-loop* 
  update(library)
  update-library-definitions(library);
end;

define shell-command in *top-level-loop* 
  in(library)
  *current-library-context* := library;
end;

define shell-command in *top-level-loop* 
  in-file(filename)
  *current-file-context* := filename;
end;

define shell-command in *top-level-loop* 
  compile-file(filename, force-compile, force-parse, harp)
  if (*current-library-context*)
    compile-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, force-compile?: force-compile, force-parse?: force-parse, harp-output?: harp);
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command in *top-level-loop* 
  recompile-file(filename, harp)
  if (*current-library-context*)
    recompile-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, harp-output?: harp);
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command in *top-level-loop* 
  emit-file(filename, harp)
  if (*current-library-context*)
    emit-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, harp-output?: harp);
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command in *top-level-loop* 
  link-file(filename, harp)
  if (*current-library-context*)
    link-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, harp-output?: harp);
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command in *top-level-loop* 
  emit-form(formname, harp)
  if (*current-library-context* & *current-file-context*)
    emit-source-record
      (as-lowercase(as(<string>, *current-file-context*)), *current-library-context*, harp-output?: harp, form?: as-lowercase(as(<string>, formname)));
  else
    format-out("\nSet current-library using:  in <library>         before attempting this command");
    format-out("\nSet current-file    using:  in-file <filename>   before attempting this command\n");
  end if;
end;

define shell-command in *top-level-loop*
  report(library)
  load-library(library);
  user-message(context, "Reporting...\n")
end;

define shell-command in *top-level-loop*
  load(library)
  load-library(library);
end;

define shell-command in *top-level-loop* 
  build-locations ()
  user-message(context, "System root: %s\n", lookup-system-root());
  let personal = lookup-personal-root();
  if(personal)
    user-message(context, "Personal root: %s\n", personal)
  end;
end;

define shell-command in *top-level-loop* 
  find-library (library)
  let project = lookup-named-project(library);
  user-message(context, "%s\n", project);
end;

define shell-command in *top-level-loop* 
  close (project)
  close-project(project);
end;

define shell-command in *top-level-loop* 
  close-all ()
  close-all-projects();
end;

define shell-command in *top-level-loop* 
  registries ()
  let (processor, os) = default-platform-info(*default-project-class*);
  let registries = find-registries(processor, os);
  for(r in registries)
    user-message(context, "%s\n", r);
  end;
end;

define shell-command in *top-level-loop*
  update-libraries (library, force)
  update-libraries(library, force?: force);
end;

define shell-command in *top-level-loop*
  help ()
  user-message(context, "Available commands: \n");
  for(c in sort(map(curry(as, <byte-string>), command-list(context))))
    user-message(context, "%s\n", c);
  end;
end;

define shell-command in *top-level-loop*
  set-write-databases?(boolean)
  write-databases?() :=  boolean = "#t" | boolean = "true";
end;

define shell-command in *top-level-loop*
  set-read-databases?(boolean)
  read-databases?() :=  boolean = "#t" | boolean = "true";
end;

define shell-command in *top-level-loop*
  write-databases?()
  user-message(context, "%s\n", write-databases?());
end;

define shell-command in *top-level-loop*
  read-databases?()
  user-message(context, "%s\n", read-databases?());
end;

define shell-command in *top-level-loop*
  trace-optimizations ()
  *trace-optimizations?* := #t;
end;

define shell-command in *top-level-loop*
  untrace-optimizations ()
  *trace-optimizations?* := #f;
end;

define shell-command in *top-level-loop*
  trace-harp ()
  *trace-harp?* := #t;
end;

define shell-command in *top-level-loop*
  untrace-harp ()
  *trace-harp?* := #f;
end;

define shell-command in *top-level-loop*
  enter-debugger ()
  break("Entering debugger");
end;

// Recovery Protocol

define shell-command in *top-level-loop* 
  c (restart-option)
  invoke-restart(restart-option);
end;

define shell-command in *top-level-loop* 
  a ()
  signal(make(<abort>));
end;

define shell-command in *top-level-loop* 
  d ()
  invoke-restart(0);
end;

define shell-command in *top-level-loop* 
  p ()
  print-restart-options();
end;


define function dw()
  block()
    apply(run-dw, os/application-arguments());
  exception(<end-of-stream-error>)
    format-out("Done\n");
  exception(<end-of-loop>)
    format-out("Done\n");
  end block;
end;

// main function of the compiler driver
define function run-dw(#rest command-args)
  let handler (<serious-condition>) = condition-handler;
// if no command arguments start the command loop
    if(command-args.size > 0)
      let cl = *top-level-loop*;
      user-message(cl, cl.banner);
      apply(execute-command, cl, command-args);
    // exception(e :: <error>)
    //  format(*standard-error*, "%s\n", e);
    //  exit(#t);
    // exception(se :: <simple-error>)
    //  apply(format, *standard-error*, 
    //  condition-format-string(se), condition-format-arguments(se));
    //  exit(#t);
    else
      run-command-loop(*top-level-loop*);
    end;
end;

dw();
