Module:    console-environment
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function main
    (arguments :: <string>)
  let input-stream = *standard-input*;
  let output-stream = *standard-output*;
  let server
    = make-environment-command-line-server
        (input-stream:   input-stream,
         output-stream:  output-stream);
  let command
    = block ()
        parse-command-line(server, arguments, class: <main-command>)
      exception (error :: <parse-error>)
        format(output-stream, "%s\n", error);
        exit-application($usage-error-exit-code)
      end;
  let status-code :: <integer> = execute-command(command);
  exit-application(status-code)
end function main;

define function dylan-banner
    () => (banner :: <string>)
  format-to-string
    ("Welcome to %s, %s.\n"
       "\n"
       "For documentation on %s, see %s.\n"
       "See %s/getting-started-cli/ for an introduction to the command line tools.\n"
       "\n"
       "Type \"help\" for more information.",
     release-product-name(),
     release-version(),
     release-product-name(), release-web-address(),
     release-web-address());
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
