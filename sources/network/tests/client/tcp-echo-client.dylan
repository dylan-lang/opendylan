Module:       sockets-tests-client
Author:       Toby Weinberg, Jason Trenouth
Synopsis:     TCP Echo Client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $test-input :: <string> =
    "the quick brown fox\n"
    "jumped over the lazy dogs\n"
    ".\n";

define method tcp-echo-client () => ()
  start-sockets();
  with-socket (client-socket, host: "127.0.0.1", port: 7)
    test-echo(client-socket)
  end with-socket;
end method;

register-client("TCP Echo", tcp-echo-client);

define method test-echo (client-socket)
  let test-output = 
    with-input-from-string (input-stream = $test-input)
      with-output-to-string (output-stream)
        request-echo(client-socket, input-stream, output-stream)
      end with-output-to-string;
    end with-input-from-string;
  check-equal("Test Echo", $test-input, test-output);
end method;


define method request-echo (client-socket, input-stream, output-stream)
  block()
    let input = read-line(input-stream);
    until (input = ".")
      write-line(client-socket, input);
      let echo = read-line(client-socket, on-end-of-stream: #"eoi");
      if (echo == #"eoi")
	error("server died unexpectedly");
      end if;
      write-line(output-stream, echo);
      input := read-line(input-stream, on-end-of-stream: #"eoi");
    end until;
    write-line(output-stream, input); // add '.' to end for comparison purposes
    close(client-socket);
  exception (condition :: <recoverable-socket-condition>)
    close(client-socket, abort?: #t);
  end block;
end method;



