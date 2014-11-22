Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define command-group environment
    (summary: "Open Dylan commands",
     documentation: "The set of commands provided by Open Dylan.")
  group basic;
  group property;
  group system;
  group library-packs;
  group project;
  group browsing;
  group reports;
  group build;
end command-group environment;

define method context-command-group
    (context :: <environment-context>) => (group :: <command-group>)
  $environment-command-group
end method context-command-group;

define method context-command-prefix
    (context :: <environment-context>) => (prefix :: <character>)
  $command-prefix-character
end method context-command-prefix;

define method make-environment-command-line-server
    (#key banner :: false-or(<string>) = #f,
          input-stream :: <stream>,
          output-stream :: <stream>,
          echo-input? :: <boolean> = #f,
          profile-commands? :: <boolean> = #f)
 => (server :: <command-line-server>)
  let context = make(<environment-context>, banner: banner);
  make(<command-line-server>,
       context:           context,
       input-stream:      input-stream,
       output-stream:     output-stream,
       echo-input?:       echo-input?,
       profile-commands?: profile-commands?)
end method make-environment-command-line-server;
