Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Operating system tests

define operating-system constant-test $architecture-little-endian? ()
  check-true("$architecture-little-endian? is true if x86",
             $machine-name ~== #"x86" | $architecture-little-endian?);
end constant-test $architecture-little-endian?;

define operating-system constant-test $os-name ()
  //---*** Fill this in...
end constant-test $os-name;

define operating-system constant-test $os-variant ()
  //---*** Fill this in...
end constant-test $os-variant;

define operating-system constant-test $os-version ()
  //---*** Fill this in...
end constant-test $os-version;

define operating-system constant-test $platform-name ()
  //---*** Fill this in...
end constant-test $platform-name;

define operating-system constant-test $machine-name ()
  //---*** Fill this in...
end constant-test $machine-name;


/// Operating System functions

define operating-system function-test login-name ()
  check-instance?("login-name returns #f or a string",
                  false-or(<string>), login-name());
end function-test login-name;

define operating-system function-test login-group ()
  check-instance?("login-group returns #f or a string",
                  false-or(<string>), login-group());
end function-test login-group;

define operating-system function-test owner-name ()
  check-instance?("owner-name returns #f or a string",
                  false-or(<string>), owner-name());
end function-test owner-name;

define operating-system function-test owner-organization ()
  check-instance?("owner-organization returns #f or a string",
                  false-or(<string>), owner-organization());
end function-test owner-organization;

define operating-system class-test <application-process> ()
  //---*** Fill this in...
end class-test <application-process>;

define operating-system function-test run-application ()
  // Synchronous true exit
  with-test-unit ("run-application synchronous true exit")
    let (exit-code, signal, child)
      = run-application("exit 0", under-shell?: #t);
    check-equal("true exit 0", 0, exit-code);
    check-equal("true no signal", #f, signal);
    check-false("no child for synchronous run-application", child);
  end;

  // Synchronous false exit
  with-test-unit ("run-application synchronous false exit")
    let (exit-code, signal, child)
      = run-application("exit 1", under-shell?: #t);
    check-equal("false exit 1", 1, exit-code);
    check-equal("false no signal", #f, signal);
    check-false("no child for synchronous run-application", child);
  end;

  // Asynchronous true exit
  with-test-unit ("run-application asynchronous true exit")
    let (exit-code, signal, child)
      = run-application("exit 0", asynchronous?: #t, under-shell?: #t);
    check-equal("asynchronous exit 0", 0, exit-code);
    check-equal("asynchronous no signal", #f, signal);
    check-instance?("child returned for asynchronous run-application",
                    <application-process>, child);

    let (exit-code, signal) = wait-for-application-process(child);
    check-equal("wait true exit 0", 0, exit-code);
    check-equal("wait true no signal", #f, signal);
  end;

  // Asynchronous false exit
  with-test-unit ("run-application asynchronous false exit")
    let (exit-code, signal, child)
      = run-application("exit 8", asynchronous?: #t, under-shell?: #t);
    check-equal("asynchronous exit 0", 0, exit-code);
    check-equal("asynchronous no signal", #f, signal);
    check-instance?("child returned for asynchronous run-application",
                    <application-process>, child);

    let (exit-code, signal) = wait-for-application-process(child);
    check-equal("wait false exit 8", 8, exit-code);
    check-equal("wait false no signal", #f, signal);
  end;

  // Output stream
  with-test-unit ("run-application output stream")
    let (exit-code, signal, child, stream)
      = run-application("echo hello, world",
                        asynchronous?: #t, output: #"stream",
                        under-shell?: #t);
    check-equal("asynchronous exit 0", 0, exit-code);
    check-equal("asynchronous no signal", #f, signal);
    check-instance?("child returned for asynchronous run-application",
                    <application-process>, child);
    check-instance?("stream returned for run-application w/output stream",
                    <stream>, stream);

    let contents = read-to-end(stream);
    check-equal("echo results read from stream",
                concatenate("hello, world", $line-end), contents);

    let (exit-code, signal) = wait-for-application-process(child);
    check-equal("wait echo exit 0", 0, exit-code);
    check-equal("wait echo no signal", #f, signal);

    close(stream);
  end;

  // Error stream
  with-test-unit ("run-application error stream")
    let (exit-code, signal, child, stream)
      = run-application("echo hello, world>&2",
                        asynchronous?: #t, error: #"stream",
                        under-shell?: #t);
    check-equal("asynchronous exit 0", 0, exit-code);
    check-equal("asynchronous no signal", #f, signal);
    check-instance?("child returned for asynchronous run-application",
                    <application-process>, child);
    check-instance?("stream returned for run-application w/error stream",
                    <stream>, stream);

    let contents = read-to-end(stream);
    check-equal("echo results read from stream",
                concatenate("hello, world", $line-end), contents);

    let (exit-code, signal) = wait-for-application-process(child);
    check-equal("wait echo exit 0", 0, exit-code);
    check-equal("wait echo no signal", #f, signal);

    close(stream);
  end;

  // Output/error common stream
  with-test-unit ("run-application output/error common stream")
    let command
      = select ($os-name)
          #"win32" =>
            "echo hello, world&echo DANGER WILL ROBINSON!>&2&echo ok";
          otherwise =>
            "echo hello, world;echo DANGER WILL ROBINSON!>&2;echo ok";
        end;
    let (exit-code, signal, child, stream)
      = run-application(command,
                        asynchronous?: #t, output: #"stream", error: #"output",
                        under-shell?: #t);
    check-equal("asynchronous exit 0", 0, exit-code);
    check-equal("asynchronous no signal", #f, signal);
    check-instance?("child returned for asynchronous run-application",
                    <application-process>, child);
    check-instance?("stream returned for run-application w/error stream",
                    <stream>, stream);

    let contents = read-to-end(stream);
    check-equal("echo results read from stream",
                concatenate("hello, world", $line-end,
                            "DANGER WILL ROBINSON!", $line-end,
                            "ok", $line-end),
                contents);

    let (exit-code, signal) = wait-for-application-process(child);
    check-equal("wait echo exit 0", 0, exit-code);
    check-equal("wait echo no signal", #f, signal);

    close(stream);
  end;

  // Outputter
  with-test-unit ("run-application outputter")
    let command
      = select ($os-name)
          #"win32" =>
            "echo hello, world&echo DANGER WILL ROBINSON!>&2&echo ok";
          otherwise =>
            "echo hello, world;echo DANGER WILL ROBINSON!>&2;echo ok";
        end;

    let contents = "";
    local
      method outputter (msg :: <byte-string>, #key end: _end)
        contents := concatenate(contents, copy-sequence(msg, end: _end));
      end;

    let (exit-code, signal, child, #rest streams)
      = run-application(command, under-shell?: #t, outputter: outputter);
    check-equal("echo exit 0", 0, exit-code);
    check-equal("echo no signal", #f, signal);
    check-false("no child for synchronous run-application", child);
    check("no streams returned", empty?, streams);

    check-equal("echo results read from stream",
                concatenate("hello, world", $line-end,
                            "DANGER WILL ROBINSON!", $line-end,
                            "ok", $line-end),
                contents);
  end;

  // Input stream
  with-test-unit ("run-application input stream")
    let shell
      = select ($os-name)
          #"win32" =>
            "cmd.exe /q/k";
          otherwise =>
            environment-variable("SHELL");
        end;
    let (exit-code, signal, child, stream)
      = run-application(shell, asynchronous?: #t, input: #"stream");

    check-equal("asynchronous exit 0", 0, exit-code);
    check-equal("asynchronous no signal", #f, signal);
    check-instance?("child returned for asynchronous run-application",
                    <application-process>, child);
    check-instance?("stream returned for run-application w/input stream",
                    <stream>, stream);

    write(stream, "exit 8");
    new-line(stream);
    close(stream);

    let (exit-code, signal) = wait-for-application-process(child);
    check-equal("wait cmd exit 8", 8, exit-code);
    check-equal("wait cmd no signal", #f, signal);
  end;

   // Input and output streams
   with-test-unit ("run-application input/output streams")
     let (exit-code, signal, child, input-stream, output-stream)
       = run-application("sort", asynchronous?: #t,
                         input: #"stream", output: #"stream");

     check-equal("asynchronous exit 0", 0, exit-code);
     check-equal("asynchronous no signal", #f, signal);
     check-instance?("child returned for asynchronous run-application",
                     <application-process>, child);
     check-instance?("input stream returned for run-application",
                     <stream>, input-stream);
     check-instance?("output stream returned for run-application",
                     <stream>, output-stream);

     write(input-stream, "Dylan");
     new-line(input-stream);
     write(input-stream, "programming");
     new-line(input-stream);
     write(input-stream, "language");
     new-line(input-stream);
     close(input-stream);

     let contents = read-to-end(output-stream);
     check-equal("sort results read from stream",
                 concatenate("Dylan", $line-end,
                             "language", $line-end,
                             "programming", $line-end),
                 contents);

     let (exit-code, signal) = wait-for-application-process(child);
     check-equal("wait sort exit 0", 0, exit-code);
     check-equal("wait sort no signal", #f, signal);

     close(output-stream);
   end;

   // Environment variable setting
   with-test-unit ("run-application environment variable setting")
     check-false("test preconditions: OS_TEST_RUN_APPLICATION not set",
                 environment-variable("OS_TEST_RUN_APPLICATION"));

     let env = make(<string-table>);
     env["OS_TEST_RUN_APPLICATION"] := "Dylan programming language";

     let command
      = select ($os-name)
          #"win32" =>
            "echo %OS_TEST_RUN_APPLICATION%&echo %PATH%";
          otherwise =>
            "echo $OS_TEST_RUN_APPLICATION; echo $PATH";
        end;
     let (exit-code, signal, child, stream)
     = run-application(command, under-shell?: #t,
                       asynchronous?: #t, output: #"stream",
                       environment: env);
     check-equal("asynchronous exit 0", 0, exit-code);
     check-equal("asynchronous no signal", #f, signal);
     check-instance?("child returned for asynchronous run-application",
                     <application-process>, child);
     check-instance?("stream returned for run-application w/output stream",
                     <stream>, stream);

     let contents = read-to-end(stream);
     check-equal("echo w/environment variable results read from stream",
                 concatenate("Dylan programming language", $line-end,
                             environment-variable("PATH"), $line-end),
                 contents);

     let (exit-code, signal) = wait-for-application-process(child);
     check-equal("wait echo exit 0", 0, exit-code);
     check-equal("wait echo no signal", #f, signal);

     close(stream);
   end;
end function-test run-application;

define operating-system function-test wait-for-application-process ()
  //---*** Fill this in...
end function-test wait-for-application-process;

define operating-system function-test load-library ()
  //---*** Fill this in...
end function-test load-library;

define operating-system function-test current-process-id ()
  let pid = current-process-id();
  check-true("current-process-id is an integer", instance?(pid, <integer>));
  check-true("current-process-id > 0", pid > 0);
end;

define operating-system function-test parent-process-id ()
  let pid = parent-process-id();
  check-true("parent-process-id is an integer", instance?(pid, <integer>));
end;


// Application startup handling

define operating-system function-test application-name ()
  check-instance?("application-name returns #f or a string",
                  false-or(<string>), application-name());
end function-test application-name;

define operating-system function-test application-filename ()
  let filename = application-filename();
  check-true("application-filename returns #f or a valid, existing file name",
             ~filename
               | begin
                   let locator = as(<file-locator>, filename);
                   file-exists?(locator)
                 end)
end function-test application-filename;

define operating-system function-test application-arguments ()
  check-instance?("application-arguments returns a sequence",
                  <sequence>, application-arguments());
end function-test application-arguments;

define operating-system function-test tokenize-command-string ()
  //---*** Fill this in...
end function-test tokenize-command-string;

define operating-system function-test command-line-option-prefix ()
  check-instance?("command-line-option-prefix returns a character",
                  <character>, command-line-option-prefix());
end function-test command-line-option-prefix;

define operating-system function-test exit-application ()
  //---*** Fill this in...
end function-test exit-application;

define operating-system function-test register-application-exit-function ()
  //---*** Fill this in...
end function-test register-application-exit-function;


// Environment variables

define operating-system function-test environment-variable ()
  check-false("unset environment variable returns false",
              environment-variable("HIGHLY_UNLIKELY_TO_BE_SET"));
  check-instance?("PATH is set and is a string",
                  <string>, environment-variable("PATH"));
end function-test environment-variable;

define operating-system function-test environment-variable-setter ()
  check-equal("environment-variable-setter returns new value",
              "new-value",
              environment-variable("OS_TEST_E_V_S") := "new-value");
  check-equal("newly set value reflected in environment",
              "new-value", environment-variable("OS_TEST_E_V_S"));
  check-false("environment-variable-setter to #f returns #f",
              environment-variable("OS_TEST_E_V_S") := #f);
  check-false("newly unset value reflected in environment",
              environment-variable("OS_TEST_E_V_S"));
end function-test environment-variable-setter;

define operating-system function-test tokenize-environment-variable ()
  //---*** Fill this in...
end function-test tokenize-environment-variable;


// Macro tests

define operating-system macro-test with-application-output-test ()
  with-application-output (stream = "echo hello, world", under-shell?: #t)
    let contents = read-to-end(stream);
    check-equal("echo results read from stream",
                concatenate("hello, world", $line-end), contents);
  end;
end macro-test with-application-output-test;
