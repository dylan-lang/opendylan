Module:       test-report
Synopsis:     A tool to generate reports from test run logs
Author:       Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// read-log-file

define constant $testworks-plus-message
  = "Make sure the test report was generated using the \"-report log\"\n"
    "option to testworks-plus.";

define method read-log-file
    (test-stream :: <file-stream>, #key ignored-tests = #[], ignored-suites = #[])
 => (result :: false-or(<result>))
  block (return)
    let last-line = #f;
    // Read next non-blank line.  Error if EOF reached, since that means
    // the log file wasn't written correctly anyway.
    local method read-next-line (#key error?) => (line :: <string>)
	    let next-line = last-line;
	    if (next-line)
	      last-line := #f;
	      next-line
	    else
	      let line = read-line(test-stream);
	      while (line = "")
		line := read-line(test-stream);
	      end while;
	      line
	    end if
	  end method read-next-line;
    local method unread-line (line :: <string>) => ()
	    last-line := line
	  end method unread-line;
    local method line-starts-with (line :: <string>, s :: <string>) => (b :: <boolean>)
            block (return)
              let len = size(line);
              for (i from 0 below size(s))
                if (i >= len | line[i] ~= s[i])
                  return(#f);
                end if;
              end for;
              #t
            end block
          end method line-starts-with;
    local method maybe-read-keyword-line
	      (keyword :: <string>) => (value :: false-or(<string>))
	    let line = read-next-line();
            if (line-starts-with(line, keyword))
              as(<string>, copy-sequence(line, start: keyword.size))
            else
              unread-line(line);
              #f
            end
          end method maybe-read-keyword-line;
    local method read-keyword-line (keyword :: <string>) => (value :: <string>)
	    maybe-read-keyword-line(keyword)
	      | application-error(#"token-not-found",
				  "Error parsing report: The keyword \"%s\" was not found.\n%s\n",
                                  $testworks-plus-message, keyword)
	  end method read-keyword-line;
    local method read-end-token () => ()
            unless (line-starts-with(read-next-line(), "end"))
              application-error(#"end-token-not-found",
                                "Error parsing report: 'end' token not found.\n%s\n",
                                $testworks-plus-message);
            end;
          end method read-end-token;
    local method read-log-file-section () => (result :: false-or(<result>))
	    let type          = read-keyword-line("Object: ");
	    let name          = read-keyword-line("Name: ");
	    when (type = "Suite")
	      debug-message("Reading %s...", name)
	    end;
	    let status-string = read-keyword-line("Status: ");
	    let reason        = maybe-read-keyword-line("Reason: ");
            let seconds       = #f;
            let microseconds  = #f;
            let allocation    = #f;
	    let subresults
	      = if (type = "Check")
                  read-end-token();
                elseif (type = "Benchmark")
                  // If there is no "Reason:" line for a benchmark then there
                  // are "Seconds:" and "Allocation:".
                  if (~reason)
                    let time = read-keyword-line("Seconds: ");
                    let alloc = read-keyword-line("Allocation: ");
                    let (secs, index) = string-to-integer(time);
                    seconds := secs;
                    microseconds := string-to-integer(time, start: index + 1);
                    allocation := string-to-integer(alloc);
                  end if;
                  read-end-token();
		else  // type is "Test" or "Suite"
		  let subresults = make(<stretchy-vector>);
		  let line = read-next-line();
		  until (line-starts-with(line, "end"))
		    unread-line(line);
		    let subresult = read-log-file-section();
		    subresult & add!(subresults, subresult);
		    line := read-next-line();
		  end;
		  subresults
		end;
	    let status
	      = select (status-string by \=)
		  "passed"       => #"passed";
		  "failed"       => #"failed";
		  "not executed" => #"not-executed";
		  "crashed"      => recreate-error(reason);
		  otherwise =>
		    error("Unexpected status '%s' in report", status-string);
		end;
	    select (type by \=)
	      "Check" =>
		make(<check-result>, 
		     name: name, status: status, 
		     operation: reason, value: #f);
              "Benchmark" =>
                make(<benchmark-result>,
                     name: name, status: status, operation: reason, value: #f,
                     seconds: seconds, microseconds: microseconds,
                     bytes: allocation);
	      "Test" =>
		unless (member?(as-lowercase(name), ignored-tests, test: \=))
		  make(<test-result>, 
		       name: name, status: status, subresults: subresults)
		end;
	      "Suite" =>
		if (~member?(as-lowercase(name), ignored-suites, test: \=))
		  debug-message("Read %s", name);
		  make(<suite-result>,
		       name: name, status: status, subresults: subresults)
		else
		  debug-message("Ignored %s", name)
		end;
	      otherwise =>
		error("Unexpected component type '%s'", type);
	    end
	  end;
    block ()
      read-log-file-section();
    exception (e :: <end-of-stream-error>)
      application-error(#"end-of-file",
                        "Error parsing report: End of file reached.\n%s\n",
                        $testworks-plus-message);
    end block
  end block
end method read-log-file;

define method read-log-file
    (path :: <string>, #key ignored-tests = #[], ignored-suites = #[])
 => (result :: <result>)
  let start-token = $test-log-header;
  let stream
    = make(<file-stream>,
	   direction: #"input",
	   locator:   path,
	   if-does-not-exist: #"signal");
  block (return)
    while (#t)
      let line = read-line(stream, on-end-of-stream: #f);
      select (line by \=)
	#f =>
	  application-error(#"start-token-not-found",
			    "The log file '%s' doesn't contain any log information.\n%s\n",
                            path, $testworks-plus-message);
	start-token =>
	  return();
	otherwise =>
	  #f;
      end
    end
  end;
  read-log-file(stream, ignored-tests: ignored-tests, ignored-suites: ignored-suites)
    | application-error(#"no-matching-results",
			"There are no matching results in log file %s\n%s\n",
			path, $testworks-plus-message)
end method read-log-file;


define class <recreated-error> (<simple-error>)
end class <recreated-error>;

define method recreate-error
    (string :: <string>) => (error :: <recreated-error>)
  make(<recreated-error>,
       format-string: "%s",
       format-arguments: vector(string))
end method recreate-error;
