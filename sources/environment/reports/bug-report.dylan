Module:    environment-reports
Author:    Andy Armstrong
Synopsis:  Bug report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $bug-report-maximum-contents = 50;
define constant $bug-report-describe-depth   = 2;
define constant $bug-report-qualify-names?   = #f;

define constant $bug-report-first-line
  = "+++++++++++++++++++ DYLAN APPLICATION FAILURE REPORT +++++++++++++++++++";
define constant $bug-report-last-line
  = "++++++++++++++++++++++++++++++++++ END +++++++++++++++++++++++++++++++++";


define class <bug-report> (<project-report>)
  constant slot report-objects :: <object-table> = make(<object-table>);
  constant slot report-threads :: false-or(<sequence>) = #f,
    init-keyword: threads:;
  constant slot report-maximum-contents :: <integer>
      = $bug-report-maximum-contents,
    init-keyword: maximum-contents:;
  constant slot report-describe-depth :: false-or(<integer>)
      = $bug-report-describe-depth,
    init-keyword: describe-depth:;
  constant slot report-show-internal-functions? :: <boolean> = #f,
    init-keyword: show-internal-functions?:;
  constant slot report-qualify-names? :: <boolean>
      = $bug-report-qualify-names?,
    init-keyword: qualify-names?:;
//---*** Not currently used...
//  constant slot report-include-source? :: <boolean> = #t,
//    init-keyword: include-source?:;
end class <bug-report>;

install-report(#"bug-report", "Bug report", <bug-report>,
	       formats: #[#"text", #"html"]);

define constant $bug-report-sections
  = #[#["Failure Summary",           #"summary"],
      #["Installation Information",  #"installation-info"]];

define constant $bug-report-application-sections
  = #[#["Application State",         #"application-state"],
      #["Backtrace",                 #"backtrace"]];

define constant $bug-report-extra-html-sections
  = #[#["Objects",                   #"object-contents"]];

define method write-report-as
    (stream :: <stream>, report :: <bug-report>, _format == #"text") => ()
  local method write-section
	    (section :: <vector>) => ()
	  let section-title   = section[0];
	  let section-keyword = section[1];
	  format(stream, "%s\n", $report-separator);
	  format(stream, "%s:\n\n", as-uppercase(section-title));
	  write-bug-report-section(stream, report, section-keyword)
	end method write-section;
  let project = report.report-project;
  format(stream, "%s\n", $bug-report-first-line);
  do(write-section, $bug-report-sections);
  if (application-tethered?(project))
    do(write-section, $bug-report-application-sections)
  end;
  format(stream, "%s\n", $report-separator);
  format(stream, "%s\n", $bug-report-last-line)
end method write-report-as;

define method write-report-as
    (stream :: <stream>, report :: <bug-report>, _format == #"html") => ()
  let title = format-to-string("%s Bug Report", release-product-name());
  with-html-output (stream, title)
    local method write-section
	      (section :: <vector>) => ()
	    let section-title   = section[0];
	    let section-keyword = section[1];
	    write-html(stream, #"h2", section-title, #"/h2", '\n', '\n');
	    write-html-bug-report-section(stream, report, section-keyword)
	  end method write-section;
    let project = report.report-project;
    compute-bug-report-objects(report);
    do(write-section, $bug-report-sections);
    if (application-tethered?(project))
      do(write-section, $bug-report-application-sections)
    end;
    do(write-section, $bug-report-extra-html-sections)
  end
end method write-report-as;


/// write-bug-report-section

define generic write-bug-report-section
    (stream :: <stream>, report :: <bug-report>, section :: <symbol>)
 => ();

define method write-bug-report-section
    (stream :: <stream>, report :: <bug-report>, section == #"summary")
 => ()
  let project = report.report-project;
  let application = project.project-application;
  write-bug-report-names-and-values
    (stream, report, 
     vector("Application", "Arguments", "Condition"),
     if (application)
       vector(as(<string>, application.application-filename),
	      application.application-arguments,
	      application.application-stop-reason-message | "[None]")
     else
       vector("[not running]", "", "")
     end,
     name-suffix: ": ")
end method write-bug-report-section;

define method write-bug-report-section
    (stream :: <stream>, report :: <bug-report>, section == #"installation-info")
 => ()
  let properties = make(<stretchy-vector>);
  local method record-property
	    (title :: <string>, value :: <string>)
	  add!(properties, pair(title, value))
	end method record-property;
  let (serial-number, evaluation?, expiration, user, organization) = license-info();
  let os-variant
    = select ($os-variant)
	#"winxp"   => "Windows XP";
	#"win2000" => "Windows 2000";
	#"winnt"   => "Windows NT";
	#"win95"   => "Windows 95";
	#"win98"   => "Windows 98";
	#"winme"   => "Windows ME";
	#"win3.1"  => "Windows 3.1"
      end;
  let edition-name = format-to-string("%s %s", release-product-name(), release-edition());
  user & record-property("User", user);
  organization & record-property("Organization", organization);
  record-property("Software edition", edition-name);
  record-property("Software version", release-version());
  record-property("Serial number", serial-number);
  record-property("Operating system", 
		  format-to-string("%s %s", 
				   os-variant, $os-version));
  write-bug-report-names-and-values
    (stream, report, map(head, properties), map(tail, properties),
     name-suffix: ": ")
end method write-bug-report-section;

define method write-bug-report-section
    (stream :: <stream>, report :: <bug-report>, section == #"application-state")
 => ()
  let project = report.report-project;
  let application = project.project-application;
  format(stream, "Application components:\n\n");
  for (component in application.application-components)
    format(stream, "  Version %s: %s\n",
	   component-version-string(project, component),
	   component-image-filename(project, component))
  end;
  format(stream, "\n");
  format(stream, "Active application threads:\n\n");
  for (thread in application.application-threads,
       index from 1)
    format(stream, "  %d. %s\n",
	   index,
	   environment-object-display-name(project, thread, #f))
  end
end method write-bug-report-section;

define method write-bug-report-section
    (stream :: <stream>, report :: <bug-report>, section == #"backtrace")
 => ()
  let project = report.report-project;
  let application = project.project-application;
  let threads = report.report-threads | application.application-threads;
  for (thread :: <thread-object> in threads)
    write-bug-report-thread-backtrace(stream, report, thread)
  end
end method write-bug-report-section;

define function write-bug-report-thread-backtrace
    (stream :: <stream>, report :: <bug-report>, thread :: <thread-object>,
     #key start :: <integer> = 0, 
          end: stop :: false-or(<integer>) = #f)
 => ()
  let project = report.report-project;
  format(stream, "Backtrace for %s:\n\n",
	 environment-object-display-name(project, thread, #f));
  let stack = thread-complete-stack-trace(project, thread);
  let stop :: <integer> = stop | stack.size;
  let show-internal-functions? = report.report-show-internal-functions?;
  let index :: <integer> = 1;
  block (return)
    for (frame :: <stack-frame-object> in stack)
      if (index > stop) return() end;
      if (show-internal-functions?
	    | ~instance?(stack-frame-function(project, frame),
			 <internal-method-object>))
	if (index >= start)
	  format(stream, "#%d ", index);
	  write-bug-report-stack-frame(stream, report, frame);
	  format(stream, "\n")
	end;
	index := index + 1
      end
    end
  end
end function write-bug-report-thread-backtrace;

define function write-bug-report-stack-frame
    (stream :: <stream>, report :: <bug-report>, frame :: <stack-frame-object>,
     #key show-variables? = #t)
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let override-name = stack-frame-override-name(project, frame);
  let function = stack-frame-function(project, frame);
  format(stream, "%s\n",
	 override-name
	   | environment-object-display-name
	       (project, function | frame, #f, 
		qualify-names?: qualify-names?));
  write-bug-report-object-location(stream, report, frame, indentation: "  ");
  if (show-variables?)
    write-bug-report-frame-variables(stream, report, frame, indentation: "  ")
  end
end function write-bug-report-stack-frame;

define function write-bug-report-object-location
    (stream :: <stream>, report :: <bug-report>, frame :: <stack-frame-object>,
     #key indentation = "")
 => ()
  let project = report.report-project;
  let function = stack-frame-function(project, frame);
  //---*** How do we display the location of foreign code?
  let location = function & environment-object-source-location(project, function);
  format(stream, "%s", indentation);
  if (location)
    let record = location.source-location-source-record;
    let locator = record.source-record-location;
    let offset = location.source-location-start-offset;
    let line   = offset.source-offset-line + record.source-record-start-line;
    format(stream, "Line %d of %s\n",
	   line,
	   locator | "interactive definition")
  else
    format(stream, "[Unknown source location]\n")
  end;
end function write-bug-report-object-location;

define function write-bug-report-frame-variables
    (stream :: <stream>, report :: <bug-report>, frame :: <stack-frame-object>,
     #key indentation = "")
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let variables = stack-frame-local-variables(project, frame);
  let new-values = make(<stretchy-vector>);
  for (variable :: <local-variable-object> in variables)
    let value = variable-value(project, variable);
    let class = application-object-class(project, value);
    if (class)
      let table = report.report-objects;
      let index = element(table, value, default: #f);
      unless (index)
	if (instance?(value, <composite-object>))
	  let new-index = size(table) + 1;
	  table[value] := new-index
	end;
	add!(new-values, variable)
      end
    end
  end;
  write-bug-report-names-and-values
    (stream, report, variables, variables,
     name-label-key:
       method (variable :: <local-variable-object>)
	 print-environment-object-to-string
	   (project, variable, qualify-names?: qualify-names?)
       end,
     value-write-function:
       method (stream :: <stream>, variable :: <local-variable-object>)
	 let value = variable-value(project, variable);
	 write-bug-report-object(stream, report, value)
       end,
     indentation: concatenate("  ", indentation),
     separator:   " = ");
  write-bug-report-variable-contents
    (stream, report, new-values, 
     indentation: indentation)
end function write-bug-report-frame-variables;

define function write-bug-report-variable-contents
    (stream :: <stream>, report :: <bug-report>, variables :: <sequence>,
     #key indentation = "")
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let object-contents-indentation = concatenate("  ", indentation);
  for (variable :: <local-variable-object> in variables)
    let value = variable-value(project, variable);
    let class = application-object-class(project, value);
    format(stream, "\n%s", indentation);
    print-environment-object
      (stream, project, variable, qualify-names?: qualify-names?);
    format(stream, " = Instance of ");
    print-environment-object-name
      (stream, project, class, qualify-names?: #f,
       qualify-names?: qualify-names?);
    write-bug-report-object-index(stream, report, value);
    format(stream, "\n");
    write-bug-report-object-contents(stream, report, value,
				     indentation: object-contents-indentation)
  end
end function write-bug-report-variable-contents;

define function write-bug-report-object-contents
    (stream :: <stream>, report :: <bug-report>, object :: <application-object>,
     #key indentation = "")
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let (names, instances, total-size)
    = bug-report-object-contents(report, object);
  let actual-size = size(names);
  let missing-size = total-size - actual-size;
  write-bug-report-names-and-values
    (stream, report, names, instances,
     indentation: indentation,
     separator: " = ",
     name-label-key:
       method (name)
	 select (name by instance?)
	   <environment-object> =>
	     environment-object-display-name
	       (project, name, #f, qualify-names?: qualify-names?);
	   <string> =>
	     name;
	   otherwise =>
	     format-to-string("%=", name)
	 end
       end,
     value-write-function:
       method (stream :: <stream>, instance :: <environment-object>)
	 write-bug-report-object(stream, report, instance)
       end);
  if (missing-size > 0)
    format(stream, "%s... [%d more]\n", 
	   indentation, missing-size)
  end
end function write-bug-report-object-contents;

define function write-bug-report-names-and-values
    (stream :: <stream>, report :: <bug-report>,
     names :: <sequence>, values :: <sequence>,
     #key indentation = "",
          name-suffix = "",
          separator = "",
          name-label-key = identity,
          value-write-function = write)
 => ()
  let name-labels = map(name-label-key, names);
  let max-label-size = reduce(max, 0, map(size, name-labels));
  let spaces :: <byte-string>
    = make(<byte-string>, size: max-label-size + 1, fill: ' ');
  for (name-label in name-labels,
       value in values)
    write(stream, indentation);
    write(stream, name-label);
    write(stream, name-suffix);
    write(stream, spaces, end: max-label-size - size(name-label) + 1);
    write(stream, separator);
    value-write-function(stream, value);
    format(stream, "\n")
  end
end function write-bug-report-names-and-values;

define function write-bug-report-object
    (stream :: <stream>, report :: <bug-report>, object :: <environment-object>)
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  print-environment-object
    (stream, project, object, qualify-names?: qualify-names?);
  write-bug-report-object-index(stream, report, object)
end function write-bug-report-object;

define function write-bug-report-object-index
    (stream :: <stream>, report :: <bug-report>, object :: <environment-object>)
 => ()
  let table = report.report-objects;
  let index = element(table, object, default: #f);
  index & format(stream, " [%d]", index)
end function write-bug-report-object-index;


/// write-html-bug-report-section

define generic write-html-bug-report-section
    (stream :: <html-wrapper-stream>, report :: <bug-report>, section :: <symbol>)
 => ();

define method write-html-bug-report-section
    (stream :: <html-wrapper-stream>, report :: <bug-report>, section == #"summary")
 => ()
  let project = report.report-project;
  let application = project.project-application;
  write-html-bug-report-names-and-values
    (stream, report, 
     vector("Application", "Arguments", "Condition"),
     vector(as(<string>, application.application-filename),
	    application.application-arguments,
	    application.application-stop-reason-message | "[None]"),
     name-suffix: ": ")
end method write-html-bug-report-section;

define method write-html-bug-report-section
    (stream :: <html-wrapper-stream>, report :: <bug-report>, section == #"installation-info")
 => ()
  let properties = make(<stretchy-vector>);
  local method record-property
	    (title :: <string>, value :: <string>)
	  add!(properties, pair(title, value))
	end method record-property;
  let (serial-number, evaluation?, expiration, user, organization) = license-info();
  let os-variant
    = select ($os-variant)
	#"winxp"   => "Windows XP";
	#"win2000" => "Windows 2000";
	#"winnt"   => "Windows NT";
	#"win95"   => "Windows 95";
	#"win98"   => "Windows 98";
	#"winme"   => "Windows ME";
	#"win3.1"  => "Windows 3.1"
      end;
  let edition-name = format-to-string("%s %s", release-product-name(), release-edition());
  user & record-property("User", user);
  organization & record-property("Organization", organization);
  record-property("Software edition", edition-name);
  record-property("Software version", release-version());
  record-property("Serial number", serial-number);
  record-property("Operating system", 
		  format-to-string("%s %s", 
				   os-variant, $os-version));
  write-html-bug-report-names-and-values
    (stream, report, map(head, properties), map(tail, properties),
     name-suffix: ": ")
end method write-html-bug-report-section;

define method write-html-bug-report-section
    (stream :: <html-wrapper-stream>, report :: <bug-report>, section == #"application-state")
 => ()
  let project = report.report-project;
  let application = project.project-application;
  let components = application.application-components;
  let threads = application.application-threads;
  write-html(stream,
	     #"p", "Application components:", '\n', '\n',
	     #"ol", '\n');
  for (component in components)
    let name = environment-object-display-name(project, component, #f);
    write-html(stream,
	       #"li", name,
	       #"/a", #"/li", '\n')
  end;
  write-html(stream,
	     #"p", "Active application threads:", '\n', '\n',
	     #"ol", '\n');
  for (thread in threads,
       index :: <integer> from 1)
    let reference = format-to-string("#thread%d", index);
    let name = environment-object-display-name(project, thread, #f);
    write-html(stream,
	       #"li", make(<html-reference>, name: reference), name,
	       #"/a", #"/li", '\n')
  end;
  write-html(stream,
	     #"/ol", '\n')
end method write-html-bug-report-section;

define method write-html-bug-report-section
    (stream :: <html-wrapper-stream>, report :: <bug-report>, section == #"backtrace")
 => ()
  let project = report.report-project;
  let application = project.project-application;
  let threads = report.report-threads | application.application-threads;
  for (thread :: <thread-object> in threads,
       index :: <integer> from 1)
    let anchor = format-to-string("thread%d", index);
    let name = environment-object-display-name(project, thread, #f);
    write-html(stream,
	       #"h2", make(<html-anchor>, name: anchor),
	       "Backtrace for ", name, #"/h2", '\n');
    write-html-bug-report-thread-backtrace(stream, report, thread)
  end
end method write-html-bug-report-section;

define method write-html-bug-report-section
    (stream :: <html-wrapper-stream>, report :: <bug-report>, 
     section == #"object-contents")
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let application = project.project-application;
  let table = report.report-objects;
  let objects = make(<simple-object-vector>, size: table.size);
  for (id :: <integer> keyed-by value :: <environment-object> in table)
    objects[id - 1] := value
  end;
  for (value :: <environment-object> in objects,
       index :: <integer> from 1)
    let class = application-object-class(project, value);
    let name = format-to-string("object%d", index);
    write-html(stream, #"h3", make(<html-anchor>, name: name));
    write-html(stream, "#", index, " ");
    print-environment-object
      (stream, project, value, qualify-names?: qualify-names?);
    write-html(stream, #"/a", #"/h3", '\n');
    write-html(stream, #"p", "[");
    case
      class =>
	write-html(stream, "Instance of ");
	print-environment-object-name
	  (stream, project, class, qualify-names?: #f);
      otherwise =>
	write-html(stream, "Foreign object");
    end;
    write-html(stream, "]", #"/p", '\n');
    if (instance?(value, <composite-object>))
      write-html-bug-report-object-contents
	(stream, report, value, indentation: "  ")
    end
  end
end method write-html-bug-report-section;

define function write-html-bug-report-thread-backtrace
    (stream :: <html-wrapper-stream>, report :: <bug-report>, thread :: <thread-object>)
 => ()
  let project = report.report-project;
  let stack = thread-complete-stack-trace(project, thread);
  let show-internal-functions? = report.report-show-internal-functions?;
  let index :: <integer> = 1;
  let start :: <integer> = 1;
  let stop :: <integer>  = stack.size;
  block (return)
    for (frame :: <stack-frame-object> in stack)
      if (index > stop) return() end;
      if (show-internal-functions?
	    | ~instance?(stack-frame-function(project, frame),
			 <internal-method-object>))
	if (index >= start)
	  write-html-bug-report-stack-frame(stream, report, frame, index);
	  new-line(stream)
	end;
	index := index + 1
      end
    end
  end
end function write-html-bug-report-thread-backtrace;

define function write-html-bug-report-stack-frame
    (stream :: <html-wrapper-stream>, report :: <bug-report>, 
     frame :: <stack-frame-object>, index :: <integer>)
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let override-name = stack-frame-override-name(project, frame);
  let function = stack-frame-function(project, frame);
  let anchor = format-to-string("frame%d", index);
  let name
    = override-name
        | environment-object-display-name
            (project, function | frame, #f, qualify-names?: qualify-names?);
  write-html(stream,
	     #"h3", make(<html-anchor>, name: anchor),
	     "#", index, " ", name, #"/h3", '\n');
  write-html-bug-report-object-location(stream, report, frame, indentation: "  ");
  write-html-bug-report-frame-variables(stream, report, frame, indentation: "  ");
end function write-html-bug-report-stack-frame;

define function write-html-bug-report-object-location
    (stream :: <html-wrapper-stream>, report :: <bug-report>, frame :: <stack-frame-object>,
     #key indentation = "")
 => ()
  let project = report.report-project;
  let function = stack-frame-function(project, frame);
  //---*** How do we display the location of foreign code?
  let location = function & environment-object-source-location(project, function);
  format(stream, "%s", indentation);
  if (location)
    let record = location.source-location-source-record;
    let locator = record.source-record-location;
    let offset = location.source-location-start-offset;
    let line   = offset.source-offset-line + record.source-record-start-line;
    let file = if (locator) as(<string>, locator) else "interactive definition" end;
    write-html(stream, #"br", "Line ", line, " of ", file, '\n')
  else
    write-html(stream, #"br", "[Unknown source location]", '\n')
  end;
end function write-html-bug-report-object-location;

define function write-html-bug-report-frame-variables
    (stream :: <html-wrapper-stream>, report :: <bug-report>, frame :: <stack-frame-object>,
     #key indentation = "")
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let variables = stack-frame-local-variables(project, frame);
  write-html-bug-report-names-and-values
    (stream, report, variables, variables,
     name-label-key:
       method (variable :: <local-variable-object>)
	 print-environment-object-to-string
	   (project, variable, qualify-names?: qualify-names?)
       end,
     value-write-function:
       method 
	   (stream :: <html-wrapper-stream>,
	    variable :: <local-variable-object>)
	 let value = variable-value(project, variable);
	 write-html-environment-object-reference(stream, report, value)
       end,
     indentation: concatenate("  ", indentation),
     separator:   " = ")
end function write-html-bug-report-frame-variables;

define function write-html-bug-report-object-contents
    (stream :: <html-wrapper-stream>, report :: <bug-report>,
     object :: <application-object>,
     #key indentation = "")
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let (names, instances, total-size)
    = bug-report-object-contents(report, object);
  let actual-size = size(names);
  let missing-size = total-size - actual-size;
  write-html-bug-report-names-and-values
    (stream, report, names, instances,
     indentation: indentation,
     separator: " = ",
     name-label-key:
       method (name)
	 select (name by instance?)
	   <environment-object> =>
	     environment-object-display-name
	       (project, name, #f, qualify-names?: qualify-names?);
	   <string> =>
	     name;
	   otherwise =>
	     format-to-string("%=", name)
	 end
       end,
     value-write-function:
       method (stream :: <html-wrapper-stream>, instance :: <environment-object>)
	 write-html-environment-object-reference(stream, report, instance)
       end);
  if (missing-size > 0)
    format(stream, "%s... [%d more]\n", 
	   indentation, missing-size)
  end
end function write-html-bug-report-object-contents;

define function write-html-bug-report-names-and-values
    (stream :: <html-wrapper-stream>, report :: <bug-report>,
     names :: <sequence>, values :: <sequence>,
     #key indentation = "",
          name-suffix = "",
          separator = "",
          name-label-key = identity,
          value-write-function = write)
 => ()
  let name-labels = map(name-label-key, names);
  let max-label-size = reduce(max, 0, map(size, name-labels));
  let spaces :: <byte-string>
    = make(<byte-string>, size: max-label-size + 1, fill: ' ');
  write-html(stream, indentation, #"ul", '\n');
  for (name-label in name-labels,
       value in values)
    write-html(stream,
	       indentation, #"li", name-label, name-suffix, separator);
    value-write-function(stream, value);
    write-html(stream,
	       #"/li", '\n');
  end;
  write-html(stream, indentation, #"/ul", '\n')
end function write-html-bug-report-names-and-values;

define function write-html-environment-object-reference
    (stream :: <html-wrapper-stream>, report :: <bug-report>, object :: <environment-object>)
 => ()
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  let table = report.report-objects;
  let index = element(table, object, default: #f);
  if (index)
    let name = format-to-string("#object%d", index);
    write-html(stream,
	       make(<html-reference>, name: name))
  end;
  print-environment-object
    (stream, project, object, qualify-names?: qualify-names?);
  index & write-html(stream, " [", index, "]", #"/a")
end function write-html-environment-object-reference;


/// Utilities

//---*** How do we get this from environment-debugger?
define function stack-frame-override-name
    (project :: <project-object>, frame :: <stack-frame-object>)
 => (name :: false-or(<string>))
  #f
end function stack-frame-override-name;

define method bug-report-object-contents
    (report :: <bug-report>, object :: <application-object>)
 => (names :: <sequence>, values :: <sequence>, total-size :: <integer>)
  values(#[], #[], 0)
end method bug-report-object-contents;

define method bug-report-object-contents
    (report :: <bug-report>, collection :: <collection-object>)
 => (names :: <sequence>, values :: <sequence>, total-size :: <integer>)
  let project = report.report-project;
  let maximum-contents = report.report-maximum-contents;
  let total = collection-size(project, collection);
  let size = min(maximum-contents, total);
  let elements-range = range(from: 0, below: size);
  let keys = collection-keys(project, collection, range: elements-range);
  let elements = collection-elements(project, collection, range: elements-range);
  values(keys, elements, total)
end method bug-report-object-contents;

define method bug-report-object-contents
    (report :: <bug-report>, object :: <composite-object>)
 => (names :: <sequence>, values :: <sequence>, total-size :: <integer>)
  let project = report.report-project;
  let maximum-contents = report.report-maximum-contents;
  let (names, instances) = composite-object-contents(project, object);
  let total = size(names);
  if (total > maximum-contents)
    values(copy-sequence(names,     end: maximum-contents),
	   copy-sequence(instances, end: maximum-contents),
	   total)
  else
    values(names, instances, total)
  end
end method bug-report-object-contents;

define method compute-bug-report-objects
    (report :: <bug-report>) => ()
  let project = report.report-project;
  let table = report.report-objects;
  let describe-depth = report.report-describe-depth;
  let threads
    = report.report-threads
        | project.project-application.application-threads;
  local method maybe-record-object
	    (object :: <environment-object>, depth :: <integer>)
	  if (instance?(object, <composite-object>)
		| instance?(object, <foreign-object>))
	    let index = element(table, object, default: #f);
	    unless (index)
	      let new-index = size(table) + 1;
	      table[object] := new-index;
	      if (~describe-depth | depth < describe-depth)
		let (names, values, total-size)
		  = bug-report-object-contents(report, object);
		ignore(names, total-size);
		do(rcurry(maybe-record-object, depth + 1), values)
	      end
	    end
	  end
	end method maybe-record-object;
  for (thread :: <thread-object> in threads)
    let stack = thread-complete-stack-trace(project, thread);
    for (frame :: <stack-frame-object> in stack,
	 index :: <integer> from 1)
      let variables = stack-frame-local-variables(project, frame);
      let new-values = make(<stretchy-vector>);
      for (variable :: <local-variable-object> in variables)
	let value = variable-value(project, variable);
	maybe-record-object(value, 1)
      end
    end
  end
end method compute-bug-report-objects;
