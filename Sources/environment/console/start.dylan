Module:    console-environment
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $success-exit-code = 0;
define constant $error-exit-code   = -1;

define function main 
    (arguments :: <string>)
  block()
    validate-license(#t)
  exception (c :: <license-validation-failure>)
    format(*standard-error*, "%s\nPlease visit %s to obtain a valid license.\n",
	   c, release-web-address());
    exit-application($error-exit-code)
  end block;
  let input-stream = *standard-input*;
  let output-stream = *standard-output*;
  let server
    = make-environment-command-line-server
        (input-stream:   input-stream,
	 output-stream:  output-stream);
  let class
    = case
	release-internal?() => <internal-main-command>;
	otherwise           => <main-command>;
      end;
  let command
    = block ()
	parse-command-line(server, arguments, class: class)
      exception (error :: <parse-error>)
	format(output-stream, "%s\n", error);
	exit-application($error-exit-code)
      end;
  let status-code :: <integer> = execute-command(command);
  exit-application(status-code)
end function main;

define function dylan-banner
    () => (banner :: <string>)
  format-to-string("%s\n%s\n%s",
		   release-name(),
		   release-version(),
		   release-copyright())
end function dylan-banner;

define function application-arguments-as-string
    () => (arguments :: <string>)
  with-output-to-string (stream)
    for (argument in application-arguments(),
	 separator = "" then " ")
      write(stream, separator);
      write(stream, argument)
    end
  end
end function application-arguments-as-string;

main(application-arguments-as-string());
