Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Support for "Edit Compiler Warnings"

//--- For the time being, just keep one "Edit Compiler Warnings" state object
define variable *compiler-warnings-state* :: false-or(<compiler-warnings-state>) = #f;

define sealed class <compiler-warnings-state> (<object>)
  sealed constant slot %project :: <project-object>,
    required-init-keyword: project:;
  sealed slot %warnings :: <sequence> = #[];
  sealed slot %index    :: <integer>  = 0;
end class <compiler-warnings-state>;

define method initialize
    (state :: <compiler-warnings-state>, #key) => ()
  let project  = state.%project;
  let warnings = project-warnings(project);
  local method warning-location<
	    (warning1 :: <warning-object>, warning2 :: <warning-object>)
	 => (less-than? :: <boolean>)
	  let loc1 = environment-object-source-location(project, warning1);
	  let loc2 = environment-object-source-location(project, warning2);
	  if (loc1 & loc2)
	    let rec1  = source-location-source-record(loc1);
	    let rec2  = source-location-source-record(loc2);
	    let name1 = source-record-location(rec1);
	    let name2 = source-record-location(rec2);
	    if (name1 = name2)
	      let off1  = source-location-start-offset(loc1);
	      let off2  = source-location-start-offset(loc2);
	      let line1 = source-offset-line(off1);
	      let line2 = source-offset-line(off2);
	      if (line1 = line2)
		let col1 = source-offset-column(off1);
		let col2 = source-offset-column(off2);
		col1 < col2
	      else
		line1 < line2
	      end
	    else
	      as(<string>, name1) < as(<string>, name2)
	    end;
	  else
	    loc1 ~= #f
	  end
	end method;
  let warnings = sort(warnings, test: warning-location<);
  state.%warnings := warnings;
  state.%index    := 0;
end method initialize;

define command edit-compiler-warnings (frame :: <environment-editor>)
    "Start editing the compiler warnings for this buffer's project."
  // Reset to initial state, then fall through to 'edit-next-compiler-warning'
  *compiler-warnings-state* := #f;
  edit-next-compiler-warning(frame)
end command edit-compiler-warnings;

define command edit-next-compiler-warning (frame :: <environment-editor>)
    "Edit the source for the next compiler warning.\n"
    "With a negative numeric argument, edit the source for the previous warning."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let project   = buffer-project(buffer);
  let direction = if (frame-numeric-arg(frame) < 0) -1 else 1 end;
  local method initialize-warnings ()
	  unless (project)
	    command-error("There is no project for this buffer")
	  end;
	  *compiler-warnings-state* := make(<compiler-warnings-state>, project: project);
	  deuce/display-message(window, "Editing warnings for project %s",
				environment-object-primitive-name(project, project))
	end method;
  block (return)
    let state = *compiler-warnings-state*;
    case 
      ~state | frame-numeric-arg-state(frame) == #"universal" =>
	initialize-warnings();
      state.%project ~== project =>
	let result
	  = yes-no-or-cancel-dialog(window,
				    "Project has changed; get warnings for the new project?");
	select (result)
	  #t        => initialize-warnings();
	  #f        => #f;
	  otherwise => return();
	end;
      direction > 0 & state.%index >= size(state.%warnings) =>
	let result
	  = yes-no-or-cancel-dialog(window,
				    "No more warnings; get warnings for the project again?");
	select (result)
	  #t        => initialize-warnings();
	  #f        => return();
	  otherwise => return();
	end;
    end;
    let state    = *compiler-warnings-state*;
    let index    = state.%index;
    let warnings = state.%warnings;
    when (if (direction > 0) index < size(warnings) else index > 0 end)
      let next     = index + direction;
      let index    = if (direction < 0) next else index end;
      let warning  = warnings[index];
      let location = environment-object-source-location(project, warning);
      if (location)
	let record   = source-location-source-record(location);
	let locator  = source-record-location(record);
	let offset   = source-location-start-offset(location);
	let line     = source-record-start-line(record) + source-offset-line(offset) - 1;
	let column   = source-offset-column(offset);
	state.%index := next;
	if (find-buffer-from-pathname(frame-editor(frame), locator) == buffer)
	  let line = line-index->line(buffer, line);
	  let bp   = if (line) make-bp(line, min(column, deuce/line-length(line)))
		     else interval-start-bp(buffer) end;
	  queue-redisplay(window, $display-point, centering: 0);
	  move-point!(bp)
	else
	  editor-open-file(locator, start-line: line, start-column: column)
	end
      else
	deuce/display-message(window, "No source for warning: %s",
			      environment-object-display-name(project, warning, #f))
      end
    end;
    frame-last-command-type(frame) := #"motion"
  end
end command edit-next-compiler-warning;
