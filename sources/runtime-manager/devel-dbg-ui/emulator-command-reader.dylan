module:      devel-dbg-ui
synopsis:    Console IO and the stop button
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// READ-COMMAND
//    Picks up a command entered at the console

define method read-command () => (s :: <string>)
  let maybe-s = read-line(*standard-input*);
  if (instance?(maybe-s, <string>))
    maybe-s
  else
    as(<string>, maybe-s);
  end if;
end method;

define method show-source (filename :: <string>,
                           from-line :: <integer>,
                           to-line :: <integer>,
                           indicate-line :: <integer>)
end method;


///// INITIALIZE-STOP-BUTTON
//    Generates and displays the stop button. Should
//    be called _after_ initialize-command-reader, but
//    _before_ the first call to poll-stop-button

define method initialize-stop-button () => ()
end method;


///// POLL-STOP-BUTTON
//    Returns #t if the stop button has been pressed
//    since the last call to poll-stop-button (or since
//    the stop button was displayed, if this is the
//    first call). Otherwise returns #f

define method poll-stop-button () => (p? :: <boolean>)
  #f
end method;


///// TERMINATE-STOP-BUTTON
//    Closes the stop button. Should be called before
//    quitting the debugger.

define method terminate-stop-button () => ()
end method;

