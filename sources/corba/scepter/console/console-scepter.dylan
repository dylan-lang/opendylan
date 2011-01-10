Module:    console-scepter
Author:    Keith Dennison, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <console-scepter> (<scepter>)
  constant slot scepter-arguments :: <vector>, init-function: application-arguments;
  slot scepter-usage? :: <boolean> = #f;
  slot scepter-help-all? :: <boolean> = #f;
  slot scepter-version? :: <boolean> = #f;
end class;

define sealed domain make(subclass(<console-scepter>));
define sealed domain initialize(<console-scepter>);

define method initialize (scepter :: <console-scepter>, #key)
  next-method();
  scepter.scepter-program-name := application-name();
  debug-out(#"console", "Application name: %s", scepter.scepter-program-name);
  debug-out(#"console", "Application arguments: %s", scepter.scepter-arguments);
end method;

define method scepter-default-back-end-class (scepter :: <console-scepter>)
 => (back-end-class :: subclass(<scepter-back-end>));
  scepter-back-end-class("dylan");
end method;

define method scepter-default-front-end-class (scepter :: <console-scepter>)
 => (front-end-class :: subclass(<scepter-front-end>));
  scepter-front-end-class("file");
end method;

define sealed method scepter-parse-options (scepter :: <console-scepter>)
 => ()
  let arguments = canonicalize-arguments(scepter.scepter-arguments);
  debug-out(#"console", "Canonicalized arguments: %s", arguments);
  parse-command-line-arguments(scepter, arguments);
end method;

define method console-scepter-error (scepter :: <console-scepter>, condition :: <error>)
  scepter.scepter-usage? := #t;
  scepter.scepter-compile? := #f;
  error(condition);
end method;

define method scepter-report-condition (scepter :: <console-scepter>, condition :: <condition>)
 => ()
  let stream = *standard-error*;
  idl-condition-header(stream, condition);
  format(stream, "unrecoverable internal error");
end method;

define method scepter-report-condition (scepter :: <console-scepter>, condition :: <simple-condition>)
 => ()
  let stream = *standard-error*;
  idl-condition-header(stream, condition);
  format(stream, "unrecoverable internal error: ");
  apply(format, stream, condition-format-string(condition), condition-format-arguments(condition));
end method;

define method scepter-report-condition (scepter :: <console-scepter>, condition :: <idl-condition>)
 => ()
  let stream = *standard-error*;
  idl-condition-header(stream, condition);
  idl-condition-title(stream, condition);
  idl-condition-body(stream, condition);
end method;

define method idl-condition-header (stream :: <stream>, condition :: <idl-condition>)
  format(stream, "\n%s: ", scepter-program-name(get-scepter()));
  if (condition.idl-condition-source)
    format(stream, "%s: ", as(<string>, condition.idl-condition-source));
  end if;
end method;

define method idl-condition-header (stream :: <stream>, condition :: <condition>)
  let scepter = get-scepter();
  format(stream, "\n%s: ", scepter.scepter-program-name);
  if (scepter.scepter-source)
    format(stream, "%s: ", as(<string>, scepter.scepter-source));
  end if;
end method;

define method scepter-format-progress-start (scepter :: <console-scepter>, control-string, #rest args)
  format(*standard-error*, "\n");
  apply(format, *standard-error*, control-string, args);
  format(*standard-error*, "...");
end method;

define method scepter-format-progress-end (scepter :: <console-scepter>, result, control-string, #rest args)
  apply(scepter-format-progress-start, scepter, control-string, args);
  if (result)
    format(*standard-error*, "Done.");
  else
    format(*standard-error*, "Aborted.");
  end if;
end method;  

define method scepter-print-usage (scepter :: <console-scepter>)
  format(*standard-error*,
	   "\nUsage: %s option* file\n\n"
	   "  Compile the specified IDL file\n\n"
	   "  Options:\n",
         scepter.scepter-program-name);
  let sorted-options = sort!(as(<vector>, scepter.scepter-options),
                             test: method (x, y)
                                     x.scepter-option-name < y.scepter-option-name
                                   end method);
  for (option in sorted-options)
    if (scepter.scepter-help-all? | ~option.scepter-option-hidden?)
      format(*standard-error*, "    %s\n", option.scepter-option-usage);
    end if;
  end for;
  format(*standard-error*, "\n");
end method;

