Module:    console-scepter
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define scepter-option
  name: "help",
  usage: "/help                   - prints usage message and exits",
  value?: #f,
  callback:      method (option, scepter :: <console-scepter>)
                   scepter.scepter-usage? := #t;
                   scepter.scepter-compile? := #f;
                 end method,
  init-callback: method (option, scepter :: <console-scepter>)
                   scepter.scepter-usage? := #f
                 end method
end scepter-option;

define scepter-option
  name: "help-all",
  usage: "/help-all               - prints usage message including internal options and exits",
  value?: #f,
  hidden?: #t,
  callback:      method (option, scepter :: <console-scepter>)
                   scepter.scepter-usage? := #t;
                   scepter.scepter-compile? := #f;
                   scepter.scepter-help-all? := #t;
                 end method,
  init-callback: method (option, scepter :: <console-scepter>)
                   scepter.scepter-usage? := #f;
                   scepter.scepter-help-all? := #f;
                 end method
end scepter-option;

define scepter-option
  name: "version",
  usage: "/version                - prints version info",
  value?: #f,
  callback:      method (option, scepter)
                   scepter.scepter-version? := #t;
                 end method,
  init-callback: method (option, scepter)
                   scepter.scepter-version? := #f;
                 end method
end scepter-option;

define function main () => ()
  debug-out(#"console", "Console Scepter starting");
  let scepter = make(<console-scepter>, directory: working-directory());
  scepter-invoke(scepter);
  if (scepter.scepter-version?)
    do(rcurry(scepter-front-end-banner, *standard-output*), all-scepter-front-end-classes());
    do(rcurry(scepter-back-end-banner, *standard-output*), all-scepter-back-end-classes());
  end if;
  if (scepter.scepter-usage?)
    scepter-print-usage(scepter)
  end if;
  debug-out(#"console", "Console Scepter exiting");
end function;

main();

