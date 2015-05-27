module:     access-path-implementation
synopsis:   Implementation of debugger connections
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <ACCESS-CONNECTION>

define class <local-access-connection> (<access-connection>)
  constant slot spy-function-argument-vector :: <REMOTE-ARG-ARRAY>
    = make(<REMOTE-ARG-ARRAY>, element-count: $max-spy-function-arguments);
  constant slot stepping-locations-vector :: <REMOTE-ARG-ARRAY>
    = make(<REMOTE-ARG-ARRAY>, element-count: $max-stepping-locations);
end class;


///// START-APPLICATION-ON-CONNECTION
//    This function is called to initialize an instance of 
//    <application-access-path> and calls the server function to create 
//    the running process. If the access connection is local, then the 
//    server returns a packaged process descriptor (a <NUB>) which is 
//    saved in the access connection.

define method start-application-on-connection
  (conn :: <local-access-connection>,
   command :: <string>, 
   arguments :: <string>,
   symbol-file-directories :: <sequence>,
   working-directory :: false-or(<string>),
   library-search-paths :: <sequence>,
   #key own-shell? = #t) => ()

  let create-shell =
    if (own-shell?)
      1
    else
      0
    end if;

  let symfile-c-strings = map(curry(as, <C-string>), symbol-file-directories);
  let symfile-dir-array = make(<C-string*>, 
                               element-count: symfile-c-strings.size);
  for (i :: <integer> from 0 below symfile-c-strings.size)
    symfile-dir-array[i] := symfile-c-strings[i];
  end for;

  let lsp-c-strings = map(curry(as, <C-string>), library-search-paths);
  let lsp-dir-array = make(<C-string*>, 
                           element-count: lsp-c-strings.size);
  for (i :: <integer> from 0 below lsp-c-strings.size)
    lsp-dir-array[i] := lsp-c-strings[i];
  end for;

  let (process, success)
    = open-local-tether(command, 
                        arguments, 
                        symbol-file-directories.size,
                        symfile-dir-array,
                        library-search-paths.size,
                        lsp-dir-array,
                        working-directory | "",
                        create-shell);

  destroy(symfile-dir-array);
  do(destroy, symfile-c-strings);
  destroy(lsp-dir-array);
  do(destroy, lsp-c-strings);

  if (success == 0)
    signal(make(<access-path-creation-error>));
  else
    conn.connection-process := process;
    add!(conn.access-debugger-connection.connection-open-tethers, conn);
  end if;
end method;


///// ATTACH-APPLICATION-ON-CONNECTION
//    This function is called to initialize an instance of 
//    <process-access-path> and calls the server function to attach to
//    the running process. If the access connection is local, then the 
//    server returns a packaged process descriptor (a <NUB>) which is 
//    saved in the access connection.

define method attach-application-on-connection
  (conn :: <local-access-connection>,
   process :: <remote-process>,
   symbol-file-directories :: <sequence>,
   system-info :: <string>) => ()

  let symfile-c-strings = map(curry(as, <C-string>), symbol-file-directories);
  let symfile-dir-array = make(<C-string*>, 
                               element-count: symfile-c-strings.size);
  for (i :: <integer> from 0 below symfile-c-strings.size)
    symfile-dir-array[i] := symfile-c-strings[i];
  end for;

  let (process, success)
    = attach-local-tether(process.nub-descriptor, 
                          symbol-file-directories.size,
                          symfile-dir-array,
                          system-info);

  destroy(symfile-dir-array);
  do(destroy, symfile-c-strings);

  if (success == 0)
    signal(make(<access-path-creation-error>));
  else
    conn.connection-process := process;
    add!(conn.access-debugger-connection.connection-open-tethers, conn);
  end if;
end method;
