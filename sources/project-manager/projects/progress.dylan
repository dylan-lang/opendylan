Module:   projects-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// progress reports

/*---*** andrewa: isn't used, any reason why we can't just remove it?
define constant <compiler-operation-type> = one-of(#"all", #"parse", #"compile-only",
						   #"compile+link", #"link");
define thread variable 
  *current-compiler-operation* :: false-or(<compiler-operation-type>) = #f;
*/

// For now we'll report only per operation

define thread variable *operation-increment* :: <float> = 1.0;

define thread variable *number-of-libraries-for-operation* :: <integer> = 1;

// wrap any project level operation in this macro
define macro with-project-progress
  { with-project-progress (?project:expression) ?:body end }
  => { with-library-progress(?project.project-current-compilation-context,
			     *operation-increment* / *number-of-libraries-for-operation*)
	 ?body
       end with-library-progress }
end macro with-project-progress;

// for use by clients (like the environment)
define open generic project-progress-report(project :: <base-project>, 
					    sofar :: <integer>,
					    abort-safe? :: <boolean>);

define method project-progress-report(project :: <base-project>, 
				      sofar :: <integer>,
				      abort-safe? :: <boolean>)
  // do nothing
  #f
end;

define open generic project-condition-report(project :: <base-project>, 
					     condition :: <condition>);

define method project-condition-report(project :: <base-project>, 
				       condition :: <condition>)
  condition-message(condition)
end;

define open generic project-progress-text(project :: <base-project>, #rest args);

define method project-progress-text(project :: <base-project>, #rest args)
  apply(project-message, project, args)
end;

define open generic project-stage-text(project :: <base-project>, #rest args);

define method project-stage-text(project :: <base-project>, #rest args)
  //--- andrewa: nobody wants to see these in stand-alone, they are
  //--- only of use in the environment.
  #f
end;

define open generic project-internal-progress-text(project :: <base-project>, #rest args);

define method project-internal-progress-text(project :: <base-project>, #rest args)
  apply(internal-message, args);
end;

// implementations of generics exported by the compiler
define sideways method library-progress-text(context, #rest args)
  if (context)
    apply(project-progress-text, context.compilation-context-project, args)
  end
end;

define sideways method library-stage-text(context, #rest args)
  if (context)
    let project = context.compilation-context-project;
    let message = apply(format-to-string, args);
    project-stage-text(project, "%s library %s", message, 
		       as(<string>, project.project-library-name)); 
  end
end;

define sideways method internal-progress-text(context, #rest args)
  if (context)
    apply(project-internal-progress-text, context.compilation-context-project, args)
  else
    apply(internal-message, args);
  end
end;

define sideways method library-progress-report(context, sofar :: <float>, 
					       #key abort-safe? :: <boolean>)
//  debug-message("Progress %d", round(100 * sofar));
  project-progress-report(context.compilation-context-project,
			  round(100 * sofar),
			  abort-safe?)
end;

define sideways method library-condition-report(context, 
						condition :: <condition>)
  if (context)
    project-condition-report(context.compilation-context-project, condition)
  else
    condition-message(condition)
  end
end;

define function project-dump-conditions(project :: <project>, stream :: <stream>)
 => (warning-count, serious-warning-count, error-count);
  let context = project.project-current-compilation-context;
  conditions-for(context, stream)
end;

/// Command-line message printing
//
// If running an internal image, we display only the internal messages.
// If not, we display only the progress messages.
// It might be useful to be able to choose to display internal messages
// in external images, but we can't for now.

define constant $message-prefix = "//+";

define variable *show-compiler-messages?* :: <boolean> = #t;

define function show-compiler-messages? () => (messages? :: <boolean>)
  *show-compiler-messages?*
end function show-compiler-messages?;

define function show-compiler-messages?-setter 
    (messages? :: <boolean>) => (messages? :: <boolean>)
  *show-compiler-messages?* := messages?
end function show-compiler-messages?-setter;

//---*** andrewa: use symbols to avoid evaluating release-internal?
//---*** before the release-info backend has been initialized.
define variable *show-internal-messages* :: <symbol> = #"internal-release";

define function show-internal-compiler-messages? () => (messages? :: <boolean>)
  show-compiler-messages?()
    & select (*show-internal-messages*)
	#"internal-release" => release-internal?();
	#"always"           => #t;
	#"never"            => #f;
      end
end function show-internal-compiler-messages?;

define function show-internal-compiler-messages?-setter 
    (messages? :: <boolean>) => (messages? :: <boolean>)
  *show-internal-messages* := if (messages?) #"always" else #"never" end;
  messages?
end function show-internal-compiler-messages?-setter;

define variable *last-project* :: false-or(<project>) = #f;

define function project-message
    (project :: <project>, #rest args) => ()
  if (show-compiler-messages?() & ~show-internal-compiler-messages?())
    unless (project = *last-project*)
      let name = project.project-name;
      let location = project.project-location;
      if (name | location)
	format-out("%s Project now", $message-prefix);
	name & format-out(" %s", name);
	location & format-out(" from %s", as(<string>, location));
	format-out("\n")
      end;
      *last-project* := project
    end;
    format-out("%s ", $message-prefix);
    apply(format-out, args);
    format-out("\n");
  end
end function project-message;

define function internal-message
    (#rest args) => ()
  if (show-internal-compiler-messages?())
    format-out("%s ", $message-prefix);
    apply(format-out, args);
    format-out("\n");
  end
end function internal-message;

define function condition-message
    (condition :: <condition>) => ()
  if (show-compiler-messages?())
    format-out("%=\n", condition)
  end
end function condition-message;
