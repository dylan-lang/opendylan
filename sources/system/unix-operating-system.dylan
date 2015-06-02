Module:       system-internals
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Temporary storage

define macro with-storage
  { with-storage (?:name, ?size:expression) ?:body end }
  => { begin
         let ?name = primitive-wrap-machine-word(integer-as-raw(0));
         let storage-size :: <integer> = ?size;
         block ()
           ?name := primitive-wrap-machine-word
                      (primitive-cast-pointer-as-raw
                         (%call-c-function ("MMAllocMisc")
                            (nbytes :: <raw-c-size-t>) => (p :: <raw-c-pointer>)
                            (integer-as-raw(storage-size))
                          end));
           if (primitive-machine-word-equal?
                 (primitive-unwrap-machine-word(?name), integer-as-raw(0)))
             error("unable to allocate %d bytes of storage", ?size);
           end;
           ?body
         cleanup
           if (primitive-machine-word-not-equal?
                 (primitive-unwrap-machine-word(?name), integer-as-raw(0)))
             %call-c-function ("MMFreeMisc")
               (p :: <raw-c-pointer>, nbytes :: <raw-c-size-t>) => ()
                 (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(?name)),
                  integer-as-raw(storage-size))
             end;
             #f
           end
         end
       end }
end macro with-storage;

define constant $os-variant = $os-name;
define constant $os-version = "Unknown";

define constant $command-line-option-prefix = '-';

define function command-line-option-prefix
    () => (prefix :: <character>)
  $command-line-option-prefix
end function command-line-option-prefix;

define function login-name () => (name :: false-or(<string>))
  let value = primitive-wrap-machine-word
                (primitive-cast-pointer-as-raw
                   (%call-c-function ("getlogin") () => (name :: <raw-byte-string>) () end));
  if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(value),
                                        integer-as-raw(0)))
    primitive-raw-as-string
      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(value)))
  else
    #f
  end
end function login-name;

define function login-group () => (group :: false-or(<string>))
  let group = primitive-wrap-machine-word
                (%call-c-function ("getgid") () => (gid :: <raw-c-unsigned-int>) () end);
  let value = primitive-wrap-machine-word
                (primitive-cast-pointer-as-raw
                   (%call-c-function ("getgrgid")
                        (gid :: <raw-c-unsigned-int>) => (group :: <raw-c-pointer>)
                      (primitive-unwrap-machine-word(group))
                    end));
  if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(value),
                                        integer-as-raw(0)))
    group-name(value)
  else
    #f
  end
end function login-group;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-name () => (name :: false-or(<string>))
  #f
end function owner-name;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-organization () => (organization :: false-or(<string>))
  #f
end function owner-organization;

define constant $environment-variable-delimiter = ':';

define function environment-variable
    (name :: <byte-string>) => (value :: false-or(<byte-string>))
  let value = primitive-wrap-machine-word
                (primitive-cast-pointer-as-raw
                   (%call-c-function ("getenv")
                        (name :: <raw-byte-string>) => (value :: <raw-byte-string>)
                      (primitive-string-as-raw(name))
                    end));
  if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(value),
                                        integer-as-raw(0)))
    let value :: <byte-string>
      = primitive-raw-as-string
          (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(value)));
    value.size > 0
      & value
  else
    #f
  end
end function environment-variable;

define function environment-variable-setter
    (new-value :: false-or(<byte-string>), name :: <byte-string>)
 => (new-value :: false-or(<byte-string>))
  if (new-value)
    //---*** Should we signal something if this call fails?
    %call-c-function ("setenv")
        (name :: <raw-byte-string>, new-value :: <raw-byte-string>,
         overwrite :: <raw-c-signed-int>)
     => (result :: <raw-c-signed-int>)
      (primitive-string-as-raw(name), primitive-string-as-raw(new-value),
       integer-as-raw(1))
    end;
  else
    %call-c-function ("unsetenv")
        (name :: <raw-byte-string>)
     => (result :: <raw-c-signed-int>)
      (primitive-string-as-raw(name))
    end;
  end if;
  new-value
end function environment-variable-setter;

define class <application-process> (<object>)
  constant slot application-process-id :: <integer>,
    required-init-keyword: process-id:;
  slot %application-process-state :: one-of(#"running", #"exited"),
    init-value: #"running";
  slot %application-process-status-code :: <integer>,
    init-value: 0;
end class;

define function make-pipe() => (read-fd :: <integer>, write-fd :: <integer>);
  with-storage (fildes, 2 * raw-as-integer(primitive-word-size()))
    let result
      = raw-as-integer
          (%call-c-function("pipe")
             (new-value :: <raw-c-pointer>) => (result :: <raw-c-signed-int>)
             (primitive-cast-raw-as-pointer
                (primitive-unwrap-machine-word(fildes)))
           end);
    if (result < 0)
      error("pipe creation failed");
    end if;
    let read-fd
      = raw-as-integer(primitive-c-signed-int-at
                         (primitive-unwrap-machine-word(fildes),
                          integer-as-raw(0), integer-as-raw(0)));
    let write-fd
      = raw-as-integer(primitive-c-signed-int-at
                         (primitive-unwrap-machine-word(fildes),
                          integer-as-raw(1), integer-as-raw(0)));
    values(read-fd, write-fd)
  end with-storage
end function;

define constant $null-device = "/dev/null";
define constant $posix-shell = "/bin/sh";

// Note: streams are always returned in the order stdin, stdout, stderr
//       even though not all of them are always returned.
define function run-application
    (command :: type-union(<string>, limited(<sequence>, of: <string>)),
     #key under-shell? = #t,
          inherit-console? = #t,
          activate? = #t,       // ignored on POSIX systems
          minimize? = #f,       // ignored on POSIX systems
          hide? = #f,           // ignored on POSIX systems
          outputter :: false-or(<function>) = #f,
          asynchronous? = #f,

          environment :: false-or(<explicit-key-collection>),
          working-directory :: false-or(<pathname>) = #f,

          input :: type-union(one-of(#"inherit", #"null", #"stream"),
                              <pathname>) = #"inherit",
          if-input-does-not-exist :: one-of(#"signal", #"create") = #"signal",
          output :: type-union(one-of(#"inherit", #"null", #"stream"),
                               <pathname>) = #"inherit",
          if-output-exists :: one-of(#"signal", #"new-version", #"replace",
                                     #"overwrite", #"append",
                                     #"truncate") = #"replace",
          error: _error :: type-union(one-of(#"inherit", #"null", #"stream", #"output"),
                              <pathname>) = #"inherit",
          if-error-exists :: one-of(#"signal", #"new-version", #"replace",
                                    #"overwrite", #"append",
                                    #"truncate") = #"replace")
 => (exit-code :: <integer>, signal :: false-or(<integer>),
     child :: false-or(<application-process>), #rest streams);

  ignore(activate?, minimize?, hide?);

  let close-fds :: <list> = #();
  let streams :: <list> = #();

  let input-fd
    = select (input)
        #"inherit" =>
          -1;
        #"null" =>
          let read-fd
            = unix-open($null-device, $O_RDONLY, $file_create_permissions);
          close-fds := add(close-fds, read-fd);
          read-fd;
        #"stream" =>
          let (read-fd, write-fd) = make-pipe();
          streams := add(streams, make(<file-stream>,
                                       locator: write-fd,
                                       file-descriptor: write-fd,
                                       direction: #"output"));
          close-fds := add(close-fds, read-fd);
          read-fd;
        otherwise =>
          let pathstring = as(<byte-string>, expand-pathname(input));
          let mode-code
            = if (if-input-does-not-exist == #"create")
                logior($O_RDONLY, $O_CREAT);
              else
                $O_RDONLY;
              end if;
          let read-fd
            = unix-open(pathstring, mode-code, $file_create_permissions);
          close-fds := add(close-fds, read-fd);
          read-fd;
      end select;

  local
    method open-output (key, if-exists, output-fd) => (fd :: <integer>);
      select (key)
        #"inherit" =>
          -1;
        #"null" =>
          let write-fd
            = unix-open($null-device, $O_WRONLY, $file_create_permissions);
          close-fds := add(close-fds, write-fd);
          write-fd;
        #"stream" =>
          let (read-fd, write-fd) = make-pipe();
          streams := add(streams, make(<file-stream>,
                                       locator: read-fd,
                                       file-descriptor: read-fd,
                                       direction: #"input"));
          close-fds := add(close-fds, write-fd);
          write-fd;
        #"output" =>
          output-fd;
        otherwise =>
          let pathstring = as(<byte-string>, expand-pathname(key));
          let mode-code
            = select (if-exists)
                #"signal" =>
                  error("not yet");
                #"new-version", #"replace" =>
                  logior($O_WRONLY, $O_CREAT); // FIXME is this correct?
                #"overwrite", #"append" =>
                  $O_WRONLY;
                #"truncate" =>
                  logior($O_WRONLY, $O_TRUNC);
              end select;
          let fd = unix-open(pathstring, mode-code, $file_create_permissions);
          if (if-output-exists == #"append")
            unix-lseek(fd, 0, $seek_end);
          end if;
          close-fds := add(close-fds, fd);
          fd;
      end select;
    end method;

  let output-fd = open-output(output, if-output-exists, #f);
  let error-fd = open-output(_error, if-error-exists, output-fd);

  let outputter-read-fd = -1;
  if (outputter)
    let (read-fd, write-fd) = make-pipe();
    outputter-read-fd := read-fd;
    error-fd := output-fd := write-fd;
    close-fds := add(close-fds, write-fd);
  end if;

  let dir = working-directory & as(<byte-string>, working-directory);

  let (program :: <byte-string>, argv-size :: <integer>)
    = if (under-shell?)
        if (instance?(command, <string>))
          values($posix-shell, 4)
        else
          values($posix-shell, 3 + command.size)
        end if
      else
        if (instance?(command, <string>))
          values(command, 2)
        else
          values(command[0], 1 + command.size)
        end if
      end if;

  let pid
    = with-storage (argv, argv-size * raw-as-integer(primitive-word-size()))
        if (under-shell?)
          primitive-c-pointer-at(primitive-unwrap-c-pointer(argv),
                                 integer-as-raw(0), integer-as-raw(0))
            := primitive-string-as-raw("sh");
          primitive-c-pointer-at(primitive-unwrap-c-pointer(argv),
                                 integer-as-raw(1), integer-as-raw(0))
            := primitive-string-as-raw("-c");
          if (instance?(command, <string>))
            primitive-c-pointer-at(primitive-unwrap-c-pointer(argv),
                                   integer-as-raw(2), integer-as-raw(0))
              := primitive-string-as-raw(as(<byte-string>, command));
          else
            for (i from 0 below command.size)
              primitive-c-pointer-at(primitive-unwrap-c-pointer(argv),
                                     integer-as-raw(2 + i),
                                     integer-as-raw(0))
                := primitive-string-as-raw(as(<byte-string>, command[i]));
            end for;
          end if
        else
          if (instance?(command, <string>))
            primitive-c-pointer-at(primitive-unwrap-c-pointer(argv),
                                   integer-as-raw(0), integer-as-raw(0))
              := primitive-string-as-raw(as(<byte-string>, command));
          else
            for (i from 0 below command.size)
              primitive-c-pointer-at(primitive-unwrap-c-pointer(argv),
                                     integer-as-raw(i), integer-as-raw(0))
                := primitive-string-as-raw(as(<byte-string>, command[i]));
            end for;
          end if
        end if;

        let (envp, envp-size)
            = if (environment)
                make-envp(environment)
              else
                values(primitive-wrap-machine-word
			 (primitive-cast-pointer-as-raw
			    (%call-c-function ("system_environ")
			       () => (environ :: <raw-c-pointer>)
			       ()
			     end)),
		       0)
              end if;

        block ()
          primitive-c-pointer-at(primitive-unwrap-c-pointer(argv),
                                 integer-as-raw(argv-size - 1), integer-as-raw(0))
            := primitive-cast-raw-as-pointer(integer-as-raw(0));

          raw-as-integer
            (%call-c-function("system_spawn")
               (program :: <raw-byte-string>,
                argv :: <raw-c-pointer>,
                envp :: <raw-c-pointer>,
                dir :: <raw-byte-string>,
                inherit-console? :: <raw-c-signed-int>,
                input-fd :: <raw-c-signed-int>,
                output-fd :: <raw-c-signed-int>,
                error-fd :: <raw-c-signed-int>) => (pid :: <raw-c-signed-int>)
               (primitive-string-as-raw(program),
                primitive-cast-raw-as-pointer
                  (primitive-unwrap-machine-word(argv)),
                primitive-cast-raw-as-pointer
                  (primitive-unwrap-machine-word(envp)),
                if (dir)
                  primitive-string-as-raw(dir)
                else
                  primitive-cast-raw-as-pointer(integer-as-raw(0))
                end,
                integer-as-raw(if (inherit-console?) 1 else 0 end),
                integer-as-raw(input-fd),
                integer-as-raw(output-fd),
                integer-as-raw(error-fd))
             end)
        cleanup
          if (environment)
            %call-c-function ("MMFreeMisc")
              (p :: <raw-c-pointer>, nbytes :: <raw-c-size-t>) => ()
                (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(envp)),
                 integer-as-raw(envp-size))
            end;
          end if;
        end block;
      end with-storage;

  // Close fds that belong to the child
  do(unix-close, close-fds);

  if (asynchronous?)
    apply(values, 0, #f, make(<application-process>, process-id: pid),
          reverse!(streams))
  else
    if (outputter)
      run-outputter(outputter, outputter-read-fd);
    end if;

    let (_return-pid, status-code) = %waitpid(pid, 0);
    let signal-code = logand(status-code, #o177);
    let exit-code = ash(status-code, -8);
    apply(values, exit-code, (signal-code ~= 0) & signal-code, #f,
          reverse!(streams))
  end if
end function run-application;

define constant $BUFFER-MAX = 4096;

define function run-outputter
    (outputter :: <function>, outputter-read-fd :: <integer>) => ();
  let dylan-output-buffer = make(<byte-string>, size: $BUFFER-MAX, fill: '\0');
  let output-buffer
    = primitive-wrap-machine-word
        (primitive-cast-pointer-as-raw
           (primitive-string-as-raw(dylan-output-buffer)));
  iterate loop ()
    let count = unix-raw-read(outputter-read-fd, output-buffer, $BUFFER-MAX);
    if (count > 0)
      outputter(dylan-output-buffer, end: count);
      loop();
    end if;
  end iterate;
  unix-close(outputter-read-fd)
end function;

define function wait-for-application-process
    (process :: <application-process>)
 => (exit-code :: <integer>, signal :: false-or(<integer>));
  if (process.%application-process-state == #"running")
    let (_return-pid, return-status)
      = %waitpid(process.application-process-id, 0);
    process.%application-process-status-code := return-status;
    process.%application-process-state := #"exited";
  end if;
  let status-code = process.%application-process-status-code;
  let signal-code = logand(status-code, #o177);
  let exit-code = ash(status-code, -8);
  values(exit-code, (signal-code ~= 0) & signal-code);
end function;

define function %waitpid
    (wpid :: <integer>, options :: <integer>)
 => (pid :: <integer>, status :: <integer>);
  with-storage (statusp, raw-as-integer(primitive-word-size()))
    let pid
      = raw-as-integer
          (%call-c-function ("waitpid")
             (wpid :: <raw-c-signed-int>, timeloc :: <raw-c-pointer>, options :: <raw-c-unsigned-int>) => (pid :: <raw-c-signed-int>)
             (integer-as-raw(wpid),
              primitive-cast-raw-as-pointer
                (primitive-unwrap-machine-word(statusp)),
              integer-as-raw(options))
           end);
    let status
      = raw-as-integer
          (primitive-c-signed-int-at(primitive-unwrap-machine-word(statusp),
                                     integer-as-raw(0),
                                     integer-as-raw(0)));
    values(pid, status)
  end with-storage
end function;

// The result returned from this must be freed with MMFreeMisc.
define function make-envp
    (environment :: <explicit-key-collection>)
 => (result :: <machine-word>, size :: <integer>)
  let temp-table :: <string-table> = make(<string-table>);

  // Obtain the current environment as a <string-table> keyed by the
  // environment variable name
  let old-envp :: <machine-word>
    = primitive-wrap-machine-word(primitive-cast-pointer-as-raw
        (%call-c-function ("system_environ") () => (environ :: <raw-c-pointer>)
           ()
         end));
  block (envp-done)
    for (i :: <integer> from 0)
      let raw-item
        = primitive-c-pointer-at(primitive-unwrap-machine-word(old-envp),
                                 integer-as-raw(i), integer-as-raw(0));
      if (primitive-machine-word-equal?
            (raw-item, primitive-cast-raw-as-pointer(integer-as-raw(0))))
        envp-done();
      else
        let item = primitive-raw-as-string(raw-item);
        block (item-done)
          for (char in item, j from 0)
            if (char == '=')
              let key = copy-sequence(item, end: j);
              temp-table[key] := item;
              item-done();
            end if;
          end for;
        end block;
      end if;
    end for;
  end block;

  // Override anything set in the user-supplied environment
  for (value keyed-by key in environment)
    temp-table[key] := concatenate(key, "=", value);
  end for;

  // Create a new environment vector and initialize it
  let envp-size = (temp-table.size + 1) * raw-as-integer(primitive-word-size());
  let new-envp
    = primitive-wrap-machine-word
        (primitive-cast-pointer-as-raw
           (%call-c-function ("MMAllocMisc")
              (nbytes :: <raw-c-size-t>) => (p :: <raw-c-pointer>)
              (integer-as-raw(envp-size))
            end));
  for (i :: <integer> from 0, item keyed-by key in temp-table)
    primitive-c-pointer-at(primitive-unwrap-c-pointer(new-envp),
                           integer-as-raw(i),
                           integer-as-raw(0))
      := primitive-string-as-raw(item);
  end for;

  values(new-envp, envp-size)
end function;

///---*** NOTE: The following functions need real implementations!

define function create-application-event
    (event :: <string>) => (event-object :: <machine-word>)
  as(<machine-word>, 0)
end function create-application-event;

define constant $INFINITE_TIMEOUT = -1;

define function wait-for-application-event
    (event-object :: <machine-word>, #key timeout :: <integer> = $INFINITE_TIMEOUT)
 => (success? :: <boolean>)
  #t
end function wait-for-application-event;

define function signal-application-event
    (event :: <string>) => (success? :: <boolean>)
  #t
end function signal-application-event;

define function load-library
    (name :: <string>) => (module)
  let module
    = primitive-wrap-machine-word
        (primitive-cast-pointer-as-raw
	   (%call-c-function ("system_dlopen")
	        (name :: <raw-byte-string>)
	     => (handle :: <raw-c-pointer>)
	      (primitive-string-as-raw(name))
	    end));
  module
end function load-library;

define function current-process-id
    () => (pid :: <integer>)
  raw-as-integer(%call-c-function("getpid")
                     () => (pid :: <raw-c-signed-int>)
                     ()
                 end);
end;

define function parent-process-id
    () => (pid :: <integer>)
  raw-as-integer(%call-c-function("getppid")
                     () => (pid :: <raw-c-signed-int>)
                     ()
                 end);
end;
