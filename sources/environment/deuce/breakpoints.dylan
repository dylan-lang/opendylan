Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Breakpoint lookup
//
// Because the breakpoints are different for a particular buffer depending
// on the project we are showing, we have a per-buffer table mapping from
// project to breakpoint table.

define sealed class <breakpoint-info> (<object>)
  constant slot info-breakpoint :: <breakpoint-object>,
    required-init-keyword: breakpoint:;
  constant slot info-state :: deuce/<breakpoint-state>,
    required-init-keyword: state:;
end class <breakpoint-info>;

define constant <breakpoints-table> = <object-table>;

define method do-buffer-breakpoints-tables
    (function :: <function>, buffer :: <basic-buffer>) => ()
  let buffer-table = get-property(buffer-properties(buffer), #"breakpoints");
  when (buffer-table)
    for (table :: <breakpoints-table> keyed-by project in buffer-table)
      function(project, table)
    end
  end
end method do-buffer-breakpoints-tables;

define method buffer-breakpoints-table
    (buffer :: <basic-buffer>, project :: <project-object>)
 => (table :: false-or(<breakpoints-table>))
  let buffer-table = get-property(buffer-properties(buffer), #"breakpoints");
  buffer-table & element(buffer-table, project, default: #f)
end method buffer-breakpoints-table;

define method deuce-note-project-sources-updated
    (project :: <project-object>) => ()
  for (buffer in editor-buffers($environment-editor))
    let buffer-table = get-property(buffer-properties(buffer), #"breakpoints");
    buffer-table & remove-key!(buffer-table, project)
  end;
  //--- This is a bit overly aggressive, but it will ensure that all files
  //--- will recompute their current project etc.
  deuce-note-active-project-changed()
end method deuce-note-project-sources-updated;

define method deuce-note-project-closed
    (project :: <project-object>) => ()
  for (buffer in editor-buffers($environment-editor))
    let buffer-table = get-property(buffer-properties(buffer), #"breakpoints");
    buffer-table & remove-key!(buffer-table, project)
  end
end method deuce-note-project-closed;

define method ensure-buffer-breakpoints-table
    (buffer :: <basic-buffer>, project :: <project-object>)
 => (table :: <breakpoints-table>)
  let buffer-table
    = get-property(buffer-properties(buffer), #"breakpoints")
        | begin
	    let buffer-table = make(<object-table>);
	    put-property!(buffer-properties(buffer), #"breakpoints", buffer-table);
	    buffer-table
	  end;
  element(buffer-table, project, default: #f)
    | begin
	let table = make(<breakpoints-table>);
	element(buffer-table, project) := table;
	install-buffer-project-breakpoints(buffer, project);
	table
      end
end method ensure-buffer-breakpoints-table;

define method add-line-breakpoint
    (table :: <breakpoints-table>, line :: <basic-line>, breakpoint :: <breakpoint-object>,
     #key state = breakpoint-state(breakpoint)) => ()
  element(table, line)
    := make(<breakpoint-info>, breakpoint: breakpoint, state: state)
end method add-line-breakpoint;

define method note-line-breakpoint-state-changed
    (table :: <breakpoints-table>, line :: <basic-line>,
     breakpoint :: <source-location-breakpoint-object>, state :: <breakpoint-state>) => ()
  let info = element(table, line, default: #f);
  if (state = #"destroyed")
    info & remove-key!(table, line)
  else
    let state = breakpoint-state(breakpoint);
    when (~info | state ~= info.info-state | breakpoint ~= info.info-breakpoint)
      add-line-breakpoint(table, line, breakpoint, state: state)
    end
  end
end method note-line-breakpoint-state-changed;

define method ensure-line-breakpoint
    (table :: <breakpoints-table>, mode :: <dylanworks-mode>, line :: <basic-line>,
     project :: <project-object>)
 => (breakpoint :: false-or(<source-location-breakpoint-object>))
  let info = table & element(table, line, default: #f);
  if (info)
    //---*** Should reset line's source location at this point if it has changed
    info.info-breakpoint
  else
    let source-location = line-source-location(mode, line, shadow?: #t);
    when (source-location)
      let breakpoint 
	= make(<source-location-breakpoint-object>,
	       project: project,
	       object: source-location);
      add-line-breakpoint(table, line, breakpoint);
      breakpoint
    end
  end
end method ensure-line-breakpoint;


/// Breakpoints

define method frame-note-all-breakpoints-changed
    (frame :: <environment-editor>, state :: <breakpoint-state>) => ()
  // Do nothing -- redisplay happens elsewhere.
end method frame-note-all-breakpoints-changed;

// NB Have to iterate over editor data structures to find breakpoints
// since they will have been deleted from the breakpoint tables at
// this point if the state was #"destroyed".
// NB "project == #f" means "all projects", which only happens with the
// #"destroyed" state, when there's no active project.
define method deuce-note-all-breakpoints-changed
    (project :: false-or(<project-object>), state :: <breakpoint-state>) => ()
  for (buffer in editor-buffers($environment-editor))
    when (file-buffer?(buffer))
      dynamic-bind (*buffer* = buffer)
	do-buffer-breakpoints-tables
	  (method (table-project :: <project-object>, table :: <breakpoints-table>)
	     when (~project | project == table-project)
	       do-lines
		 (method (line, start-index, end-index, last-line?)
		    ignore(start-index, end-index, last-line?);
		    let info = element(table, line, default: #f);
		    when (info)
		      let bkp = info.info-breakpoint;
		      note-line-breakpoint-state-changed(table, line, bkp, state)
		    end
		  end method,
		  buffer)
	     end
	   end method,
	   buffer)
      end
    end
  end;
  // Redisplay all editor windows associated with the project
  do-environment-editor-windows
    (method (window :: <window>)
       queue-redisplay(window, $display-all);
       sheet-mapped?(window) & redisplay-window(window)
     end method,
     project: project)
end method deuce-note-all-breakpoints-changed;

define method deuce-note-single-breakpoint-changed
    (project :: <project-object>,
     breakpoint :: <breakpoint-object>,
     state :: <breakpoint-state>) => ()
  #f
end method deuce-note-single-breakpoint-changed;

define method deuce-note-single-breakpoint-changed
    (project :: <project-object>,
     breakpoint :: <source-location-breakpoint-object>,
     state :: <breakpoint-state>) => ()
  let (line, section) = find-breakpoint-line(project, breakpoint);
  when (line & section)
    let buffer = section-home-buffer(section, editor: $environment-editor);
    let table  = buffer & buffer-breakpoints-table(buffer, project);
    when (table)
      note-line-breakpoint-state-changed(table, line, breakpoint, state)
    end;
    // Redisplay the relevant line (if visible) in all editor windows
    do-environment-editor-windows
      (method (window :: <window>)
	 with-editor-state-bound (window)
	   let dline = find-display-line(window, line);
	   when (dline)
	     queue-redisplay(window, $display-line, line: line, index: 0, centering: #f);
	     sheet-mapped?(window) & redisplay-window(window)
	   end
	 end
       end method,
       project: project)
  end
end method deuce-note-single-breakpoint-changed;

// environment-tools protocol: deletes breakpoint from buffer if deleted elsewhere
define method frame-note-breakpoint-state-changed
    (frame :: <environment-editor>,
     breakpoint :: <source-location-breakpoint-object>, state :: <breakpoint-state>) => ()
  #f
end method frame-note-breakpoint-state-changed;

define method do-source-record-breakpoints
    (function :: <function>,
     project :: <project-object>, source-record :: <source-record>) => ()
  for (breakpoint in source-location-breakpoints(project))
    let location :: <source-location> = breakpoint-object(breakpoint);
    when (source-record = source-location-source-record(location))
      function(breakpoint)
    end
  end
end method do-source-record-breakpoints;

// Environment-Tools protocol: reads breakpoint source-locations before eg running app
// NB assumes source-location and buffer breakpoint numbers kept in synch
define sideways method record-breakpoint-source-locations
    (project :: <project-object>)
  let frame = default-editor-frame();
  dynamic-bind (*editor-frame* = frame)
    for (breakpoint in source-location-breakpoints(project))
      let location :: <source-location> = breakpoint-object(breakpoint);
      let record :: <source-record> = source-location-source-record(location);
      let section = find-section-for-source-location(project, record, location);
      let buffer = section & section-home-buffer(section, editor: $environment-editor);
      when (buffer)
	dynamic-bind (*buffer* = buffer)
	  let table = buffer-breakpoints-table(buffer, project);
	  table
	    & record-buffer-breakpoint-source-locations(project, buffer, table)
        end
      end
    end
  end
end method record-breakpoint-source-locations;

define method record-buffer-breakpoint-source-locations
    (project :: <project-object>, buffer :: <basic-buffer>, table :: <breakpoints-table>)
  local method record-location
	    (line, start-index, end-index, last-line?) => ()
	  ignore(start-index, end-index, last-line?);
	  let info = table & element(table, line, default: #f);
	  when (info)
	    let breakpoint = info.info-breakpoint;
	    let old-loc    = breakpoint-object(breakpoint);
	    let new-loc    = line-source-location(buffer-major-mode(buffer), line, shadow?: #t);
	    when (new-loc & ~source-location-equal?(new-loc, old-loc))
	      remove-key!(source-location-breakpoints(project), old-loc);
	      element(source-location-breakpoints(project), new-loc) := breakpoint;
	      breakpoint-object(breakpoint) := new-loc;
	      let application = project-application(project);
	      when (application)
		server-note-breakpoint-state-changed(application, breakpoint, #"destroyed");
		server-note-breakpoint-state-changed(application, breakpoint, #"created");
	      end
	    end
	  end
	end method;
  do-lines(record-location, buffer)
end method record-buffer-breakpoint-source-locations;

// Deuce protocol: record breakpoint source locations before closing file
define method exit-mode
    (buffer :: <basic-buffer>, mode :: <dylanworks-mode>) => ()
  do-buffer-breakpoints-tables
    (method (project :: <project-object>, table :: <breakpoints-table>)
       record-buffer-breakpoint-source-locations(project, buffer, table)
     end,
     buffer);
  next-method()
end method exit-mode;

define method install-buffer-project-breakpoints
    (buffer :: <basic-buffer>, project :: <project-object>) => ()
  let source-record = buffer-source-record(buffer);
  when (source-record)
    let table = ensure-buffer-breakpoints-table(buffer, project);
    remove-all-keys!(table);
    local method install-buffer-breakpoint (breakpoint)
	    let line = find-breakpoint-line(project, breakpoint);
	    add-line-breakpoint(table, line, breakpoint)
	  end method;
    do-source-record-breakpoints
      (install-buffer-breakpoint, project, source-record)
  end
end method install-buffer-project-breakpoints;

define method find-breakpoint-line
    (project :: <project-object>, breakpoint :: <source-location-breakpoint-object>)
 => (line :: false-or(<basic-line>), section :: false-or(<section>))
  let location = breakpoint-object(breakpoint);
  let record   = location & source-location-source-record(location);
  let (section, line)
    = record & find-section-for-source-location(project, record, location);
  values(line, section)
end method find-breakpoint-line;

// Remove any breakpoint if the line is killed
define method note-multi-line-deletion
    (mode :: <dylanworks-mode>, interval :: <basic-interval>) => ()
  next-method();
  let buffer = interval-buffer(interval);
  do-buffer-breakpoints-tables
    (method (project :: <project-object>, table :: <breakpoints-table>)
       local method destroy-line-breakpoint (line, start-index, end-index, last-line?)
	       let info = element(table, line, default: #f);
	       when (info)
		 let breakpoint = info.info-breakpoint;
		 destroy-breakpoint(breakpoint);
		 // Nuke the properties just to make sure
		 remove-key!(table, line)
	       end
	     end method;
       do-lines(destroy-line-breakpoint, interval)
     end,
     buffer)
end method note-multi-line-deletion;

define method deuce/line-breakpoint?
    (mode :: <dylanworks-mode>, line :: <basic-line>)
 => (state :: deuce/<breakpoint-state>)
  let window = frame-window(*editor-frame*);
  unless (line-empty?(line)
	    | bp-looking-at?(line-start(line), "//")
            | instance?(line-node(line), <dylan-header-node>))
    let frame   = sheet-frame(window);
    let project = frame-current-project(frame);
    when (project)
      let buffer = section-home-buffer(line-section(line), editor: $environment-editor);
      // Show nothing if there is no home buffer.  Interactive source records
      // have their own home buffer, so this shouldn't really happen...
      when (buffer)
	let table = ensure-buffer-breakpoints-table(buffer, project);
	let info = element(table, line, default: #f);
	if (info)
	  info.info-state
	else
	  #"none"
	end
      end
    end
  end
end method deuce/line-breakpoint?;

define method deuce/line-breakpoint?-setter
    (state :: deuce/<breakpoint-state>, mode :: <dylanworks-mode>,
     line :: <basic-line>)
 => (state :: deuce/<breakpoint-state>)
  let window  = frame-window(*editor-frame*);
  let frame   = sheet-frame(window);
  let project = frame-current-project(frame);
  when (project)
    let buffer = section-home-buffer(line-section(line), editor: $environment-editor);
    // Show nothing if there is no home buffer.  Interactive source records
    // have their own home buffer, so this shouldn't really happen...
    let table = buffer & buffer-breakpoints-table(buffer, project);
    let breakpoint
      = table & ensure-line-breakpoint(table, mode, line, project);
    debug-message("Ensured breakpoint %=, state %=, table %=",
		  breakpoint, state, table);
    when (breakpoint)
      breakpoint-state(breakpoint) := state;
      next-method()
    end
  end
end method deuce/line-breakpoint?-setter;

define method breakpoint-state
    (breakpoint :: false-or(<source-location-breakpoint-object>))
 => (state :: deuce/<breakpoint-state>)
  case
    ~breakpoint =>
      #"none";
    breakpoint-profile?(breakpoint) =>
      #"profile";
    breakpoint-transient?(breakpoint) =>
      #"step";
    breakpoint-stop?(breakpoint) =>
      if (breakpoint-enabled?(breakpoint))
	#"enabled-break"
      else
	#"disabled-break"
      end;
    breakpoint-message?(breakpoint) =>
      if (breakpoint-enabled?(breakpoint))
	#"enabled-trace"
      else
	#"disabled-trace"
      end;
    breakpoint-test(breakpoint) =>
      #"test-break";
    otherwise =>
      #"none";
  end
end method breakpoint-state;

define method breakpoint-state-setter
    (state :: deuce/<breakpoint-state>, breakpoint :: <source-location-breakpoint-object>)
 => (state :: deuce/<breakpoint-state>)
  select (state)
    #"none"           =>
      destroy-breakpoint(breakpoint);
    #"enabled-break"  =>
      breakpoint-enabled?(breakpoint) := #t;
      breakpoint-stop?(breakpoint)    := #t;
    #"disabled-break" =>
      breakpoint-enabled?(breakpoint) := #f;
      breakpoint-stop?(breakpoint)    := #t;
    #"test-break"     =>
      not-yet-implemented
	(message: "Test breakpoints are not implemented yet.");
    #"step"           =>
      breakpoint-transient?(breakpoint) := #t;
    #"enabled-trace"  => 
      breakpoint-enabled?(breakpoint) := #t;
      breakpoint-stop?(breakpoint)    := #f;
    #"disabled-trace" => 
      breakpoint-enabled?(breakpoint) := #f;
      breakpoint-stop?(breakpoint)    := #f;
    #"profile"        => 
      breakpoint-profile?(breakpoint) := #t;
  end;
  state
end method breakpoint-state-setter;


/// Breakpoint popup menu

define method do-handle-presentation-event
    (mode :: <dylanworks-mode>, window :: <environment-deuce-pane>,
     line :: <basic-line>, type == deuce/<dylan-breakpoint>,
     #rest keys,
     #key bp, x, y, button, modifiers, event-type,
	  menu-function = dylanworks-breakpoint-menu) => ()
  ignore(bp, x, y, button, modifiers, event-type);
  apply(next-method, mode, window, line, type,
	menu-function: menu-function, keys)
end method do-handle-presentation-event;

//---*** andrewa: we should really get rid of this somehow, and move
//---*** to making the correct target instead.
define method dylanworks-breakpoint-menu
    (window :: <environment-deuce-pane>, mode :: <dylanworks-mode>,
     line :: <basic-line>, #key bp, x, y) => ()
  ignore(bp);
  block ()
    let frame = sheet-frame(window);
    primary-object-interval(window)
      := make-interval(line-start(line), line-end(line), in-order?: #t);
    let project = frame-current-project(frame);
    let section = line-section(line);
    let buffer = section-home-buffer(section, editor: $environment-editor);
    let table = project & buffer-breakpoints-table(buffer, project);
    let info = table & element(table, line, default: #f);
    let target
      = if (info)
	  //---*** Should reset line's source location at this point if it has changed
	  info.info-breakpoint
	else
          line-source-location(mode, line, shadow?: #t)
	end;
    display-environment-popup-menu(frame, target, x: x, y: y)
  cleanup
    primary-object-interval(window) := #f;
  end
end method dylanworks-breakpoint-menu;
