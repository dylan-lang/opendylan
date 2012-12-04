module: cpp-tester
Author: Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define sideways method default-handler (condition :: <error>)
  let condition-string :: <string> =
    block ()
      format-to-string("%s", condition);
    exception ( print-error :: <error> )
      format-to-string("%=\nsignalled while trying to print an instance of %=",
                       print-error, object-class(condition));
    end block;
  format-out("Unhandled Dylan error:\n"
             "%s\n"
             "Quitting.", condition-string);

  exit-application(0);
end method;
*/

/* HACK to work around Win32 console bug: */
do-next-output-buffer(*standard-output*);

block ()
  let ok = #t;
  for (arg in application-arguments())
    case
      arg = "-s" =>
        *return-white-space?* := #t;
      arg[0] = '-' & arg[1] = 'I' =>
        push-last(*cpp-include-path*, copy-sequence(arg, start: 2));
      otherwise =>
        begin format-out("Unrecognized option: %s\n", arg); ok := #f; end;
    end case;
  end for;
  unless (ok)
    format-out("Error.  Usage:\n"
               "\t%s [-s] [-I\\include\\dir ...]\n", application-name());
    exit-application(1);
  end unless;
  let in = *standard-input*; //make(<file-stream>, direction: #"input", locator: "in.cpp");
  let out = *standard-output*; //make(<file-stream>, direction: #"output", locator: "out.cpp");
  test-cpp-unlex(in, out);
exception (cond :: <warning>)
  format-out("Lexer warning: %s\n", cond);
exception (cond :: <error>)
  format-out("Lexer error: %s\n", cond);
end;

define function unlex-token-stream (in :: <stream>, out :: <stream>) => ()
  for (current-token = read-element(in) then read-element(in),
       until: instance?(current-token, <eoi>))
    if (instance?(current-token, <white-space>))
      new-line(out);
    else
      write(out, current-token.lexer-string);
    end if;
  end for;
  values()
end;

define function test-cpp-unlex (in-stream :: <stream>,
                                out-stream :: <stream>) => ()
  let cpp = make(<cpp-stream>,
                 source-name: "input",
                 inner-stream:
                   make(<ansi-C-lexer>,
                        source-name: "input",
                        inner-stream:
                          make(<pre-lexer>,
                               source-name: "input",
                               inner-stream: in-stream)));
  unlex-token-stream(cpp, out-stream);
  values();
end function test-cpp-unlex;
