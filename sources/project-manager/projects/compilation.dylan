Module:   projects-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Listener interface
//
// Note that lookup-named-project can create a project, and we want it
// to be in the same compiler transaction as the outer command.  Hence
// wrap whole thing in with-used-project-cache.

define method update-libraries (key, #rest keys, #key, #all-keys)
  with-used-project-cache
    apply(update-libraries, lookup-named-project(key), keys)
  end;
end method;

define method compile-library (key, #rest keys, #key, #all-keys)
  with-used-project-cache
    apply(compile-library, lookup-named-project(key), keys)
  end;
end method;

define method link-library (key, #rest keys, #key, #all-keys)
 => (linked? :: <boolean>)
  with-used-project-cache
    apply(link-library, lookup-named-project(key), keys)
  end;
end method;

define open generic save-project(project :: <project>,
                                 #rest keys, #key save-db? = #f, #all-keys);

define method save-project(project :: <project>,
                           #rest keys, #key save-db? = #f, flush?, #all-keys)
  let context = project-current-compilation-context(project);
  if (save-db? & context)
    save-project-database(project, flush?: flush?)
  end
end;

define method save-project-database (project :: <project>, #key flush?) => ()
  //---*** andrewa: temporary kludge to avoid using format-out,
  //---*** this should really take part in the progress properly.
  let context = project-current-compilation-context(project);
  assert(context,
         "Attempting to save database for project with no context!");
  with-walk-progress (internal-message("Walked %d", count))
    save-compilation-context(context, flush?: flush?)
  end;
  project.%database-saved := #t;
  note-database-saved(project)
end method save-project-database;

define method project-build-info(project :: <string>)
 => (found? :: <boolean>, personal? :: <boolean>, build-location)
  let project = lookup-named-project(project, create?: #f);
  if (project)
    values(#t,
           project.project-personal-library?,
           project.project-build-location);
  end if;
end method;

define method link-library(project :: <project>, #rest keys,
                           #key target-type,
                                arch,
                                extent = #"changes",
                                build-script,
                                progress-callback = ignore,
                                mode, release?,
                           #all-keys)
 => (linked? :: <boolean>)
  let type = target-type | project-target-type(project);
  let build-options
    = concatenate(select (type)
                    #"exports"    => #["exports"];
                    #"dll"        =>
                      select (mode)
                        #"combine" => #["unify-dll"];
                        otherwise => #["dll"];
                      end;
                    #"executable" =>
                      select (mode)
                        #"combine" => #["unify-exe"];
                        otherwise => #["exe"];
                      end;
                  end,
                  if (release?) #["release"] else #[] end);
  let build-location = project-build-location(project);
  build-system(build-options,
               directory: build-location,
               build-script: build-script,
               arch: arch,
               progress-callback: progress-callback,
               project-build-info: project-build-info,
               force?: extent == #"all")
end method;

define method load-library (key)
  with-used-project-cache
    let project = lookup-named-project(key);
    with-progress-reports
      parse-project(project);
    end;
  end;
end method;

define method load-namespace (key)
  with-used-project-cache
    let project = lookup-named-project(key);
    project-load-namespace(project, update-sources?: #t, update-used?: #t);
  end;
end method;

// this is external interface
// make sure it is not called internally
define function lookup-named-project (key, #key create? = #t)
  if (instance?(key, <project>))
    key
  else
    let key = as(<symbol>, key);
    let project =
      choose-project(rcurry(project-key?, key));

    if(project)
      verify-project-database(project);
      if(project.%database-in-memory &
           ~project-dynamic-environment(#"compiler-transaction"))
        project.project-top-level? := #t
      end;
      project
    elseif(create?)
      make-project(*default-project-class*, key: key);
    else
      #f
    end;
  end;
end function;

define function ensure-project-database(project :: <project>, #key parse? = #f)
 => (context, in-memory? :: <boolean>, current? :: <boolean>, saved? :: <boolean>);
  if(~project.%database-in-memory & parse?)
   with-used-project-cache
    // Have to update sources, since compiler can't read old ones.
    canonicalize-project-sources(project, update-sources?: #t);
    parse-project-sources(project.project-current-compilation-context);
   end
  end;
  let context = project.%database-in-memory &
          project.project-current-compilation-context;
  values(context,
         project.%database-in-memory,
         project.%database-in-memory-current,
         project.%database-saved)
end;

define function resignal-project-warning(c :: <condition>, #key abort?)
  apply(user-warning, c.condition-format-string, c.condition-format-arguments);
  apply(internal-message, c.condition-format-string, c.condition-format-arguments);
  if (abort?)
    user-warning("Aborting compilation due to errors");
    signal(make(<abort-compilation>,
                warnings: 0,
                serious-warnings: 0,
                errors: 1))
  end;
  // in case the signal is not caught
  #()
end;

// Entry points

define function project-load-namespace (project :: <project>, #rest keys)
 => (compilation-contexts-to-recompile :: <sequence>);
  with-lock($pm-lock)
  with-used-project-cache
    block()
      debug-out(#"driver", "project-load-namespace %s, %s\n", project, keys);
      debug-message("project-load-namespace of %s, %s", project.project-name, keys);
      project-stage-text(project, "Loading namespace for library %s",
                         as(<string>, project.project-library-name));

      if(project-dynamic-environment(#"compiler-transaction"))
        remove-all-personal-owners(project);
      end;
      apply(canonicalize-project-sources, project, keys);
      project.project-top-level? := #t;
      // Calling this can cause premature closing of projects which are in
      // the process of being opened (or have been opened) via a using projects'
      // hdp file, but haven't been needed by the compiler yet, and hence
      // are not recorded as having owners and being used..
      // close-unused-projects();
      let context = project-current-compilation-context(project);
      let all-used-contexts = all-known-compilation-contexts(context);
      let personal-contexts = choose(method (c)
                                       let p = c.compilation-context-project;
                                       project-personal-library?(p)
                                     end,
                                     all-used-contexts);
      project.project-namespace-loaded := #t;
      personal-contexts
    exception(c :: <source-record-error>)
      resignal-project-warning(c, abort?: #t)
    end;
  end with-used-project-cache
  end with-lock
end;

define function parse-project(project :: <project>,
                              #rest keys,
                              #key force-parse?,
                              update-used? = #t,
                              force-parse-used?)
 => (aborted? :: <boolean>);
  debug-assert(~%project-closed?(project), "Attempt to compile closed project");
  block()
    apply(%parse-project, project, keys);
    #f
  exception(c :: <abort-compilation>)
    internal-message("Aborting compilation of %s due to warnings",
                     project.project-name);
    project-progress-text(project, "Aborting compilation of %s due to warnings",
                          project.project-name);
    #t
  end;
end;

define function %database-invalidated(project :: <project>)
  let already-seen :: <object-table> = make(<object-table>);
  local method note-invalid
            (project :: <project>)
          unless (element(already-seen, project, default: #f))
            note-database-invalidated(project);
            already-seen[project] := #t;
            do(note-invalid, project.project-owners)
          end
        end method note-invalid;
  note-invalid(project)
end;

define function %parse-project(project :: <project>,
                               #key force-parse?,
                               update-used? = #t,
                               force-parse-used?)
 => (compilation-contexts-to-recompile :: <sequence>);
  block()
    let personal-contexts =
      project-load-namespace(project,
                             update-sources?: #t,
                             force-parse?: force-parse?,
                             update-used?: update-used?,
                             force-parse-used?: force-parse-used?);
    close-unused-projects();
    for (subcontext in personal-contexts using backward-iteration-protocol)
      let parsed? = parse-project-sources(subcontext);
      parsed? & %database-invalidated(subcontext.compilation-context-project);
    end;

    personal-contexts
  exception(c :: <source-record-error>)
    resignal-project-warning(c, abort?: #t)
  end;
end;


// Should be #t (always strip by default), #f (never strip by default),
// or #"tight" (strip if tight by default)
define variable *strip-default-policy* = #"tight";

define function parse-and-compile
    (context, strip-policy, parse?, #rest keys)
  block()
    let project = compilation-context-project(context);
    let settings = project-build-settings(project);
    let strip? = if (strip-policy == #"tight")
                   project-compilation-mode(project) == #"tight"
                 else
                   strip-policy
                 end;
    note-compiling-definitions(project);
    with-project-progress(project)
      let parsed? = parse? & parse-project-sources(context);
      debug-message("Parse-project-sources returned %s", parsed?);
      parsed? & %database-invalidated(project);
      let status =
        apply(compile-project-definitions,
              context,
              build-settings: settings,
              strip?: strip?,
              keys);
      if(status) note-compiled-definitions(project)
      else
        debug-message("Compile-project-definitions for project %s returned #f",
                      project.project-name)
      end;
    end;
  exception(c :: <source-record-error>)
    resignal-project-warning(c, abort?: #t)
  end;
end function;

define method compile-library (project :: <project>,
                               #rest flags,
                               #key force-compile? = #f,
                                    force-parse?   = #f,
                                    abort-on-all-warnings?     = #f,
                                    abort-on-serious-warnings? = #f,
                                    default-binding = #f,
                                    strip? = *strip-default-policy*,
                                    save?  = #f,
                                    // More fine-grained forcing controls..
                                    force-batch? = force-compile?,
                                    force-objects? = force-compile?,
                               #all-keys)
 => (aborted? :: <boolean>);
  debug-assert(~%project-closed?(project), "Attempt to compile closed project");
  debug-assert(project.project-personal-library?, "Attempt to compile read-only project");
  project-dynamic-environment(#"default-binding") := default-binding;

  let context = project-current-compilation-context(project);
  with-used-project-cache

      %parse-project(project,
                     force-parse?: force-parse?,
                     update-used?: #t,
                     force-parse-used?: #f);
      block(finish)
        apply(parse-and-compile,
              context, strip?, #f,
              compile-all?: force-batch?,
              compile-if-built?: force-objects?,
              flags);
          #f
      exception(c :: <abort-compilation>)
        internal-message("Aborting compilation of %s due to warnings",
                         project.project-name);
        project-progress-text(project, "Aborting compilation of %s due to warnings",
                              project.project-name);
        #t
      end;

  end;
end method;

define method build-project(project :: <project>,
                            #key force? = #f,
                                 save?  = #f,
                                 abort-on-all-warnings?     = #f,
                                 abort-on-serious-warnings? = #f,
                                 default-binding = #f,
                                 assembler-output? = unsupplied(), harp-output? = #f,
                                 debug-info? = #t, gc? = #f, gc-stats? = #f,
                                 recursive? = #t)

end;

define thread variable *contexts-to-recompile* = #f;

define method update-libraries (project :: <project>,
                                #key force? = #f,
                                     save?  = #f,
                                     abort-on-all-warnings?     = #f,
                                     abort-on-serious-warnings? = #f,
                                     continue-after-abort? = #f,
                                     default-binding = #f,
                                     strip? = *strip-default-policy*,
                                     assembler-output? = unsupplied(),
                                     harp-output? = #f, dfm-output? = #f,
                                     debug-info? = #t, gc? = #f, gc-stats? = #f,
                                     flush? = #f,
                                     // More fine-grained forcing controls..
                                     force-parse?   = force?,
                                     force-compile? = force?,
                                     force-batch?   = force-compile?,
                                     // This isn't very useful yet...
                                     force-objects? = force-compile?,
                                     recursive? = #t)
 => (aborted? :: <boolean>);
  if (gc-stats?) enable-gc-messages() end;
  debug-assert(~%project-closed?(project), "Attempt to compile closed project");
  debug-assert(project.project-personal-library?,
               "Attempt to compile read-only project");
  project-dynamic-environment(#"default-binding") := default-binding;
  let aborted? = #f;
  with-used-project-cache
    with-progress-reports
      block(finish)
        let handler <abort-compilation> =
          method(c, next)
              internal-message("Aborting compilation of %s due to errors",
                               project.project-name);
              project-progress-text(project,
                                    "Aborting compilation of %s due to errors",
                                    project.project-name);
              aborted? := #t;
              finish()
          end;
        let context = project-current-compilation-context(project);
        let all-contexts-to-recompile =
          project-load-namespace(project, force-parse?: force-parse?,
                                 update-sources?: #t, update-used?: #t);
        close-unused-projects();
        let contexts-to-recompile =
          if (recursive?)
            all-contexts-to-recompile;
          else
            list(context);
          end if;
        if (*contexts-to-recompile*)
          *contexts-to-recompile* :=
            reduce(method (contexts, context)
                     pair(as-lowercase(as(<string>, context.compilation-context-project.project-library-name)),
                          contexts)
                   end method,
                   #(),
                   contexts-to-recompile);
        end if;

        let count = contexts-to-recompile.size;
        dynamic-bind (*number-of-libraries-for-operation* = count)
          internal-progress-text(context,
                               "Number of libraries to compile: %d", count);

          let skip-heaping = #f;
          if(empty?(contexts-to-recompile))
            aborted? := #t;
            finish()
          end;
          for (cc in contexts-to-recompile using backward-iteration-protocol)
            block (continue)
              let proj = compilation-context-project(cc);
              let handler <abort-compilation> =
                method(c, next)
                    internal-message("Aborting compilation of %s due to warnings",
                                     proj.project-name);
                    project-progress-text(proj,
                                          "Aborting compilation of %s due to warnings",
                                          proj.project-name);
                    aborted? := #t;
                    if(continue-after-abort?)
                      skip-heaping := #t;
                      continue()
                    else
                      finish()
                    end
                end;
              parse-and-compile(cc, strip?, #t,
                                abort-on-all-warnings?: abort-on-all-warnings?,
                                abort-on-serious-warnings?: abort-on-serious-warnings?,
                                skip-heaping?: skip-heaping,
                                compile-all?: force-batch?,
                                compile-if-built?: force-objects?,
                                assembler-output?: assembler-output?,
                                harp-output?: harp-output?,
                                dfm-output?: dfm-output?,
                                debug-info?: debug-info?,
                                gc?: gc?, gc-stats?: gc-stats?,
                                save?: save?,
                                flush?: flush?);
              if(save?)
                proj.%database-saved := #t;
                note-database-saved(proj)
              end;

            end block;
          end for;
        end dynamic-bind;
    cleanup
      do(method(p)
             p.project-personal-library? &
             compilation-definitions-inconsistent?(p.project-current-compilation-context) &
             %database-invalidated(p)
         end,
         // TO DO: cannot ask for used contexts I think
         // should unset namespace loaded ?
         *all-open-projects*)
      end block;
    end with-progress-reports;
  end with-used-project-cache;
  aborted?
end method;

// Compile a project in its entirety using forced full GCs after each subproject
// compilation
// Do bottom-up compilation after project canonicalization and first GC
//   compile-project compiles a toplevel project recursively
//   compile-subproject compiles a subproject non-recursively
// Only small stuff (e.g. <string>s not <project>s) permitted here
// GC must happen at safe point when nothing is being hanged on to in
// machine stack or registers


define function compile-project-with-gc(project,
                                        compile-project :: <function>,
                                        compile-subproject :: <function>,
                                        #rest keys)
  local method projects-to-recompile(projects :: <list>, last-project :: <string>)
          iterate process-projects(projects :: <list> = projects)
            if (projects.empty?) projects
            else
              let project :: <string> = projects.first;
              if (project = last-project)
                projects.tail
              else
                process-projects(projects.tail)
              end if;
            end if;
          end iterate;
        end method;
  local method compile-subprojects-with-gc(subprojects :: <list>,
                                           projects-canonicalized? :: <boolean>)
          // format-out("\n### projects-to-compile %=\n", subprojects);
          let no-of-projects :: <integer> = subprojects.size;
          unless (no-of-projects = 0)
          let done? =
          block()
            let subproject :: <string> = subprojects.first;
            if (no-of-projects = 1)
              let toplevel? = ~ projects-canonicalized?;
              apply(compile-project, subproject,
                    recursive?: toplevel?, keys);
            else
              apply(compile-subproject, subproject, keys);
            end if;
            #t
          exception(c :: <garbage-collection>)
            let last-project :: <string> = c.garbage-collection-info;

            // do any outstanding projects stuff here ...
            note-compiled-definitions(last-project);

            unless (projects-canonicalized?)
              *contexts-to-recompile* :=
                projects-to-recompile(*contexts-to-recompile*, last-project);
            end unless;

            close-all-projects();
            // format-out("\n### SIZE OF HEAP BEFORE COLLECTION: %=\n", room());

            // Now uses the full GC ramp
            // collect-garbage!();

            // format-out("\n### SIZE OF HEAP AFTER COLLECTION: %=\n", room());
            #f
          end block;
          if (done?)
            compile-subprojects-with-gc
              (subprojects.tail, projects-canonicalized?)
          else
            compile-subprojects-with-gc
              (if (projects-canonicalized?)
                 subprojects.tail
               else
                 let contexts = *contexts-to-recompile*;
                 *contexts-to-recompile* := #f;
                 contexts
               end if,
               #t);
          end if;
          end unless;
  end method;

  dynamic-bind (*contexts-to-recompile* = #())
    compile-subprojects-with-gc(list(project), #f);
  end dynamic-bind;
end function;


// If using combined object files, dump the canonical source
// as a single file.
define function maybe-dump-combined-sources (project :: <project>)
  let context = project.project-current-compilation-context;
  let objects = compilation-context-object-names(context);
  let sources = compilation-context-sources(context);
  let dir = project.project-build-location;
  when (objects.size == 1 & sources.size > 1 & dir)
    let file = make(<file-locator>,
                    directory: dir,
                    base:      objects.first,
                    extension: "dylan");
    with-open-file (stream = file, direction: #"output")
      for (sr in sources)
        for (i from 0 below sr.source-record-start-line) new-line(stream) end;
        write(stream, source-record-contents(sr));
      end;
    end with-open-file;
  end;
end function maybe-dump-combined-sources;
