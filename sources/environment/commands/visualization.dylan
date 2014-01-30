Module:    environment-commands
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define method parse-next-argument
    (context :: <environment-context>, type == <graph-visualization>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <graph-visualization>, next-index :: <integer>)
  let (host, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (host)
    let port = if (stop == next-index)
                 if (any?(curry(\=, ':'), host))
                   let names = split(host, ':');
                   unless (names.size == 2)
                     parse-error("couldn't parse a visualizer for input")
                   end;
                   host := names[0];
                   string-to-integer(names[1]);
                 end
               else
                 let (port, rnext-index)
                   = parse-next-word(text, start: next-index, end: stop);
                 if (port)
                   next-index := rnext-index;
                   string-to-integer(port)
                 end;
               end;
    values(make(<graph-visualization>, host: host, port: port | 1234),
           next-index)
  else
    parse-error("no connection information found")
  end;
end;


define class <visualization-property> (<environment-property>)
end class <visualization-property>;

define command-property visualization => <visualization-property>
  (summary:       "connected visualization",
   documentation: "The currently connected visualization, if any.",
   type:          <graph-visualization>)
end command-property visualization;

define method show-property
    (context :: <environment-context>, property :: <visualization-property>)
 => ()
  let visualization = context.context-visualization;
  if (visualization)
    message(context, "Connection to visualizer at %s on port %d",
            visualization.visualization-host,
            visualization.visualization-port)
  else
    command-error("No connected visualization")
  end
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <visualization-property>,
     host :: <graph-visualization>, #key save?)
 => ()
  context.context-visualization := host
end method set-property;

define class <visualization-type-property> (<environment-property>)
end class <visualization-type-property>;

define command-property visualization-type => <visualization-type-property>
  (summary:       "transport type: either sexp or json",
   documentation: "The current transport type, defaults to json.",
   type:          <string>)
end command-property visualization-type;

define method show-property
    (context :: <environment-context>, property :: <visualization-type-property>)
 => ()
  let type = context.context-visualization.visualization-type;
  if (type)
    message(context, "Visualization type is %s", type);
  else
    command-error("No visualization type")
  end
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <visualization-type-property>,
     type :: <string>, #key save?)
 => ()
  if (type = "sexp")
    context.context-visualization.visualization-type := #"sexp"
  else
    context.context-visualization.visualization-type := #"json"
  end
end method set-property;

define class <visualization-filter-property> (<environment-property>)
end class <visualization-filter-property>;

define command-property visualization-filter => <visualization-filter-property>
 (summary:       "filter top level forms",
  documentation: "filters top level forms by prefix (or filename if ending in .dylan)",
  type:          <string>)
end command-property visualization-filter;

define method show-property
    (context :: <environment-context>, property :: <visualization-filter-property>)
 => ()
  let filter = context.context-visualization.visualization-filter;
  if (filter)
    message(context, "Visualization filter is %s", filter)
  else
    command-error("No filter defined")
  end
end;

define method set-property
    (context :: <environment-context>, property :: <visualization-filter-property>,
     filter :: <string>, #key save?)
 => ()
  let filter = if (empty?(filter)) #f else filter end;
  context.context-visualization.visualization-filter := filter
end;

define command-group visualizer
    (summary: "project commands",
     documentation: "Commands applying to projects.")
  property visualization;
  property visualization-type;
  property visualization-filter;
end command-group visualizer;
