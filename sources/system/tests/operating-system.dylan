Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Operating system tests

define test test-$architecture-little-endian? ()
  check-true("$architecture-little-endian? is true if x86",
             $machine-architecture ~== #"x86" | $architecture-little-endian?);
end test;

define test test-$os-name ()
  //---*** Fill this in...
end test;

define test test-$os-variant ()
  //---*** Fill this in...
end test;

define test test-$os-version ()
  //---*** Fill this in...
end test;

define test test-$platform-name ()
  //---*** Fill this in...
end test;

define test test-$machine-architecture ()
  //---*** Fill this in...
end test;


/// Operating System functions

define test test-login-name ()
  check-instance?("login-name returns #f or a string",
                  false-or(<string>), login-name());
end test;

define test test-login-group ()
  check-instance?("login-group returns #f or a string",
                  false-or(<string>), login-group());
end test;

define test test-owner-name ()
  check-instance?("owner-name returns #f or a string",
                  false-or(<string>), owner-name());
end test;

define test test-owner-organization ()
  check-instance?("owner-organization returns #f or a string",
                  false-or(<string>), owner-organization());
end test;

define test test-<application-process> ()
  //---*** Fill this in...
end test;

define test test-run-application-synchronous-true-exit ()
  let (exit-code, signal, child)
    = run-application("exit 0", under-shell?: #t);
  check-equal("true exit 0", 0, exit-code);
  check-equal("true no signal", #f, signal);
  check-false("no child for synchronous run-application", child);
end test;

define test test-run-application-synchronous-false-exit ()
  let (exit-code, signal, child)
    = run-application("exit 1", under-shell?: #t);
  check-equal("false exit 1", 1, exit-code);
  check-equal("false no signal", #f, signal);
  check-false("no child for synchronous run-application", child);
end test;

define test test-run-application-asynchronous-true-exit ()
  let (exit-code, signal, child)
    = run-application("exit 0", asynchronous?: #t, under-shell?: #t);
  check-equal("asynchronous exit 0", 0, exit-code);
  check-equal("asynchronous no signal", #f, signal);
  check-instance?("child returned for asynchronous run-application",
                  <application-process>, child);

  let (exit-code, signal) = wait-for-application-process(child);
  check-equal("wait true exit 0", 0, exit-code);
  check-equal("wait true no signal", #f, signal);
end test;

define test test-run-application-asynchronous-false-exit ()
  let (exit-code, signal, child)
    = run-application("exit 8", asynchronous?: #t, under-shell?: #t);
  check-equal("asynchronous exit 0", 0, exit-code);
  check-equal("asynchronous no signal", #f, signal);
  check-instance?("child returned for asynchronous run-application",
                  <application-process>, child);

  let (exit-code, signal) = wait-for-application-process(child);
  check-equal("wait false exit 8", 8, exit-code);
  check-equal("wait false no signal", #f, signal);
end test;

define test test-run-application-output-stream ()
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
end test;

define test test-run-application-error-stream ()
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
end test;

define test test-run-application-output/error-common-stream ()
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
end test;

define test test-run-application-outputter ()
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
end test;

define test test-run-application-input-stream ()
  let shell
    = select ($os-name)
        #"win32" =>
          "cmd.exe /q/k";
        otherwise =>
          environment-variable("SHELL");
      end;
  if (~shell & environment-variable("GITHUB_ACTIONS"))
    shell := "/usr/bin/bash"
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
end test;

define test test-run-application-input-and-output-streams ()
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
end test;

define test test-run-application-environment-variable-setting ()
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
end test;

define test test-wait-for-application-process ()
  //---*** Fill this in...
end test;

define test test-load-library ()
  assert-signals(<error>,
                 load-library("does.not.exist"));
end test;

define test test-current-process-id ()
  let pid = current-process-id();
  check-true("current-process-id is an integer", instance?(pid, <integer>));
  check-true("current-process-id > 0", pid > 0);
end;

define test test-parent-process-id ()
  let pid = parent-process-id();
  check-true("parent-process-id is an integer", instance?(pid, <integer>));
end;

define test test-machine-concurrent-thread-count ()
  let thread-count = machine-concurrent-thread-count();
  check-true("There is at least one core", thread-count >= 1);
end;


define test test-command-line-option-prefix ()
  check-instance?("command-line-option-prefix returns a character",
                  <character>, command-line-option-prefix());
end test;


// Environment variables

define test test-environment-variable ()
  check-false("unset environment variable returns false",
              environment-variable("HIGHLY_UNLIKELY_TO_BE_SET"));
  check-instance?("PATH is set and is a string",
                  <string>, environment-variable("PATH"));
end test;

define test test-environment-variable-setter ()
  check-equal("environment-variable-setter returns new value",
              "new-value",
              environment-variable("OS_TEST_E_V_S") := "new-value");
  check-equal("newly set value reflected in environment",
              "new-value", environment-variable("OS_TEST_E_V_S"));
  check-false("environment-variable-setter to #f returns #f",
              environment-variable("OS_TEST_E_V_S") := #f);
  check-false("newly unset value reflected in environment",
              environment-variable("OS_TEST_E_V_S"));
end test;

define test test-tokenize-environment-variable ()
  //---*** Fill this in...
end test;


// Macro tests

define test test-with-application-output ()
  with-application-output (stream = "echo hello, world", under-shell?: #t)
    let contents = read-to-end(stream);
    check-equal("echo results read from stream",
                concatenate("hello, world", $line-end), contents);
  end;
end test;

define suite operating-system-test-suite ()
  test test-$architecture-little-endian?;
  test test-$os-name;
  test test-$os-variant;
  test test-$os-version;
  test test-$platform-name;
  test test-$machine-architecture;
  test test-login-name;
  test test-login-group;
  test test-owner-name;
  test test-owner-organization;
  test test-<application-process>;
  test test-run-application-synchronous-true-exit;
  test test-run-application-synchronous-false-exit;
  test test-run-application-asynchronous-true-exit;
  test test-run-application-asynchronous-false-exit;
  test test-run-application-output-stream;
  test test-run-application-error-stream;
  test test-run-application-output/error-common-stream;
  test test-run-application-outputter;
  test test-run-application-input-stream;
  test test-run-application-input-and-output-streams;
  test test-run-application-environment-variable-setting;
  test test-wait-for-application-process;
  test test-load-library;
  test test-current-process-id;
  test test-parent-process-id;
  test test-machine-concurrent-thread-count;
  test test-command-line-option-prefix;
  test test-environment-variable;
  test test-environment-variable-setter;
  test test-tokenize-environment-variable;
  test test-with-application-output;
end suite;
