Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Library pack argument parsing

define method parameter-type-name
    (type == <library-pack-info>) => (name :: <string>)
  "library pack"
end method parameter-type-name;

define method parse-next-argument
    (context :: <environment-context>, type == <library-pack-info>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <library-pack-info>, next-index :: <integer>)
  let (name, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (name)
    block (return)
      let info = find-library-pack-info(as(<symbol>, name));
      if (info)
	values(info, next-index)
      else
	parse-error("%s is not an installed library pack", name)
      end
    end
  else
    parse-error("Missing library pack argument")
  end
end method parse-next-argument;


/// Library pack properties

// Library packs

define class <library-packs-property> (<environment-property>)
end class <library-packs-property>;

define command-property library-packs => <library-packs-property>
  (summary:       "Installed library packs",
   documentation: "The currently installed library packs.")
end command-property library-packs;

define method show-property
    (context :: <environment-context>, property :: <library-packs-property>)
 => ()
  let stream = context.context-server.server-output-stream;
  print-table(stream, installed-library-packs(),
	      label-key: method (info :: <library-pack-info>)
			   as-uppercase(as(<string>, info.info-name))
			 end,
	      value-key: info-title,
	      separator: " - ")
end method show-property;

// Examples

define class <examples-property> (<environment-property>)
end class <examples-property>;

define command-property examples => <examples-property>
  (summary:       "All installed examples",
   documentation: "The currently installed examples.")
end command-property examples;

define method show-property
    (context :: <environment-context>, property :: <examples-property>)
 => ()
  let stream = context.context-server.server-output-stream;
  for (info :: <library-pack-info> in installed-library-packs())
    if (instance?(info, <basic-library-pack-info>))
      describe-libraries(context, format-to-string("%s Examples", info.info-title),
			 info.info-examples)
    end
  end
end method show-property;

// Test Suites

define class <test-suites-property> (<environment-property>)
end class <test-suites-property>;

define command-property test-suites => <test-suites-property>
  (summary:       "All installed test-suites",
   documentation: "The currently installed test-suites.")
end command-property test-suites;

define method show-property
    (context :: <environment-context>, property :: <test-suites-property>)
 => ()
  let stream = context.context-server.server-output-stream;
  for (info :: <library-pack-info> in installed-library-packs())
    if (instance?(info, <basic-library-pack-info>))
      describe-libraries(context, format-to-string("%s test suites", info.info-title),
			 info.info-test-suites)
    end
  end
end method show-property;


/// Library pack states

register-state-type(#"library-pack");

define method find-state-value
    (context :: <server-context>, state == #"library-pack", library-pack-name :: <string>)
 => (value :: <library-pack-info>)
  let info = find-library-pack-info(as(<symbol>, library-pack-name));
  info | command-error("Library pack %s isn't registered", library-pack-name)
end method find-state-value;

define method describe-state
    (context :: <server-context>, info :: <library-pack-info>,
     #key prefix :: <string> = "", 
          full? :: <boolean> = #f)
 => ()
  message(context, "%s:", info.info-title);
  if (info.info-description)
    message(context, "%s", info.info-description)
  end;
  let subprefix = concatenate(prefix, "  ");
  describe-libraries(context, "Libraries", info.info-libraries, prefix: subprefix);
  describe-libraries(context, "Examples", info.info-examples, prefix: subprefix);
  describe-libraries(context, "Test Suites", info.info-test-suites, prefix: subprefix);
end method describe-state;

define method describe-libraries
    (context :: <server-context>, title :: <string>, libraries :: <sequence>, 
     #key prefix :: <string> = "",
          full? :: <boolean> = #f)
 => ()
  let stream = context.context-server.server-output-stream;
  message(context, "%s%s:", prefix, title);
  print-table(stream, libraries,
	      label-key: method (info :: <library-info>)
			   as-uppercase(as(<string>, info.info-name))
			 end,
	      value-key: info-description,
	      separator: " - ",
	      prefix: concatenate(prefix, "  "))
end method describe-libraries;

register-state-type(#"library");

define method find-state-value
    (context :: <server-context>, state == #"library", library-name :: <string>)
 => (info :: <library-info>)
  let info = find-library-info(as(<symbol>, library-name));
  info | command-error("Library %s isn't registered by a library pack", library-name)
end method find-state-value;

define method describe-state
    (context :: <server-context>, info :: <library-info>,
     #key prefix :: <string> = "", 
          full? :: <boolean> = #t)
 => ()
  let parent-info = info.info-merge-parent | info;
  let location = info.info-location;
  message(context, "%s%s: %s:", 
	  prefix, 
	  select (info by instance?)
	    <example-info>    => "Example";
	    <test-suite-info> => "Test Suite";
	    otherwise         => "Library";
	  end,
	  info.info-title);
  if (info.info-description)
    message(context, "%s%s", prefix, info.info-description)
  end;
  if (full?)
    message(context, "%s  Location: %s%s", prefix, location,
	    if (~file-exists?(location)) " (not installed)" else "" end);
    message(context, "%s  Merge parent: %s", prefix, parent-info.info-name);
    let binary-info = info.info-binary;
    if (binary-info)
      message(context, "%s  Binary name: %s", prefix, info.info-binary-name | "#f");
      if (~empty?(binary-info.info-merged-libraries))
	message(context, "%s  Merged libraries: %=",
		prefix, map(info-name, binary-info.info-merged-libraries))
      end
    end
  end
end method describe-state;


/// Project commands

define command-group library-packs
    (summary: "library pack commands",
     documentation: "Commands applying to library packs.")
  property library-packs;
  property examples;
  property test-suites;
end command-group library-packs;
