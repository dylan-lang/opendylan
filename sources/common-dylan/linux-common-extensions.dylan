Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     Common extensions to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline-only function get-application-commandline () => (res :: <string>)
  let pid
    = raw-as-integer(%call-c-function("getpid")
		       () => (pid :: <raw-c-signed-int>)
		       ()
		    end);

  let cmdline-path
    = concatenate("/proc/", integer-to-string(pid), "/cmdline");
  let cmdline-fd = -1;
  let cmdline :: <byte-string> = "";
  block ()
    cmdline-fd 
      := raw-as-integer(%call-c-function ("open")
			  (path :: <raw-byte-string>,
			   flags :: <raw-c-signed-int>,
			   mode :: <raw-c-signed-int>)
			  => (fd :: <raw-c-signed-int>)
			  (primitive-string-as-raw(cmdline-path),
			   integer-as-raw(0),
			   integer-as-raw(0))
		       end);
    if (cmdline-fd > 0)
      let count :: <integer> = 1;
      while (count > 0)
	let buffer = make(<byte-string>, size: 8192, fill: '\0');
	count
	  := raw-as-integer(%call-c-function ("read")
			      (fd :: <raw-c-signed-int>,
			       buffer :: <raw-byte-string>,
			       size :: <raw-c-unsigned-long>)
			      => (count :: <raw-c-signed-int>)
			      (integer-as-raw(cmdline-fd),
			       primitive-string-as-raw(buffer),
			       integer-as-raw(8192))
			   end);
	if (count > 0)
	  cmdline := concatenate(cmdline, copy-sequence(buffer, end: count));
	end;
      end;
    end;
  cleanup
    if (cmdline-fd > 0)
      %call-c-function ("close")
	(fd :: <raw-c-signed-int>) => (ok? :: <raw-c-signed-int>)
	(integer-as-raw(cmdline-fd))
      end
    end
  end;
  cmdline
end;

define inline-only function get-application-filename
    () => (filename :: false-or(<byte-string>))
  let pid
    = raw-as-integer(%call-c-function("getpid")
		       () => (pid :: <raw-c-signed-int>)
		       ()
		    end);
  let exe-path
    = concatenate("/proc/", integer-to-string(pid), "/exe");
  let buffer = make(<byte-string>, size: 8192, fill: '\0');
  let count
    = raw-as-integer(%call-c-function ("readlink")
		       (path :: <raw-byte-string>,
			buffer :: <raw-byte-string>,
			bufsize :: <raw-c-unsigned-long>)
		       => (count :: <raw-c-signed-int>)
		       (primitive-string-as-raw(exe-path),
			primitive-string-as-raw(buffer),
			integer-as-raw(8192))
		    end);
  unless (count = -1)
    copy-sequence(buffer, end: count)
  end;
end;