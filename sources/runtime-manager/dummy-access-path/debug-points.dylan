module:      access-path-implementation
synopsis:    Implementation of debug points.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $legal-success-values = #[#"already-exists",
                                          #"does-not-exist",
                                          #"cannot-operate-on-register",
                                          #"no-support",
                                          #"ok"];


///// CONSTANTS EQUIVALENT TO INTEGER CODES RETURNED BY DEBUGGER NUB.


define constant $not-supported                 = 0;
define constant $breakpoint-already-exists     = 1;
define constant $breakpoint-does-not-exist     = 2;
define constant $watchpoint-already-exists     = 3;
define constant $watchpoint-does-not-exist     = 4;
define constant $set-breakpoint-failed         = 5;
define constant $clear-breakpoint-failed       = 6;
define constant $ok                            = 7;

define constant $exists                        = 1;
define constant $not-found                     = 0;


///// EXPORTED GENERIC FUNCTIONS


define generic enable-breakpoint (ap :: <access-path>, 
                                  address :: <remote-value>)
                                  => (_ :: <boolean>);

define generic disable-breakpoint (ap :: <access-path>,
                                   address :: <remote-value>) 
                                   => (_ :: <boolean>);

define generic query-breakpoint? (ap :: <access-path>,
                                  address :: <remote-value>)
                                  => (_ :: <boolean>);

define generic enable-read-watchpoint (ap :: <access-path>,
                                       address :: <remote-value>,
                                       size :: <integer>)
                                       => (_ :: <boolean>);

define generic disable-read-watchpoint (ap :: <access-path>,
                                        address :: <remote-value>)
                                        => (_ :: <boolean>);

define generic query-read-watchpoint? (ap :: <access-path>,
                                       address :: <remote-value>)
                                       => (_ :: <boolean>);

define generic enable-write-watchpoint (ap :: <access-path>,
                                        address :: <remote-value>,
                                        size :: <integer>)
                                        => (_ :: <boolean>);

define generic disable-write-watchpoint (ap :: <access-path>,
                                         address :: <remote-value>)
                                         => (_ :: <boolean>);

define generic query-write-watchpoint? (ap :: <access-path>,
                                        address :: <remote-value>)
                                        => (_ :: <boolean>);

define generic enable-execute-watchpoint (ap :: <access-path>,
                                          address :: <remote-value>,
                                          size :: <integer>)
                                          => (_ :: <boolean>);

define generic disable-execute-watchpoint (ap :: <access-path>,
                                           address :: <remote-value>)
                                           => (_ :: <boolean>);

define generic query-execute-watchpoint? (ap :: <access-path>,
                                          address :: <remote-value>)
                                          => (_ :: <boolean>);


///// ENABLE-BREAKPOINT


define method enable-breakpoint (ap :: <access-path>,
                                 address :: <remote-value>)
                                 => (_ :: <boolean>)

       let success-code :: <integer> =
              set-breakpoint-in-application (ap.connection, address);

       // Attempt to map the success code to a legal success boolean.

       (success-code == $ok);

end method;

define method set-breakpoint-in-application
                     (conn :: <local-access-connection>,
                      ra :: <remote-value>) => (_ :: <remote-value>)

  if (ra >= $memory-size)
    $set-breakpoint-failed
  elseif (conn.process.memory.breakpointed?[ra] == #"break")
    $breakpoint-already-exists
  elseif (conn.process.memory.breakpointed?[ra] == #"replace")
    $breakpoint-already-exists
  else
    conn.process.memory.breakpointed?[ra] := #"break";
    $ok
  end if
end method;


///// DISABLE-BREAKPOINT


define method disable-breakpoint (ap :: <access-path>,
                                  address :: <remote-value>)
                                  => (_ :: <boolean>)

       let success-code :: <integer> =
              clear-breakpoint-in-application (ap.connection, address);

       // Map the returned success code onto a boolean.

       (success-code == $ok);

end method;

define method clear-breakpoint-in-application
                     (conn :: <local-access-connection>,
                      ra :: <remote-value>) => (_ :: <integer>)

  if (ra >= $memory-size)
    $clear-breakpoint-failed
  elseif (conn.process.memory.breakpointed?[ra] == #"clear")
    $breakpoint-does-not-exist
  else
    conn.process.memory.breakpointed?[ra] := #"clear";
    $ok
  end if
end method;


///// QUERY-BREAKPOINT?


define method query-breakpoint? (ap :: <access-path>,
                                address :: <remote-value>)
                                => (_ :: <boolean>)

       let code :: <integer> =
             query-breakpoint-in-application (ap.connection, address);

       // Map the code onto a boolean.

       (code == $exists);

end method;

define method query-breakpoint-in-application
                   (conn :: <local-access-connection>,
                    ra :: <remote-value>) => (_ :: <integer>)

   if (conn.process.memory.breakpointed?[ra] == #"break")
     $exists
   elseif (conn.process.memory.breakpointed?[ra] == #"replace")
     $exists
   else
     $not-found
   end if
end method;


///// ENABLE-READ-WATCHPOINT


define method enable-read-watchpoint (ap :: <access-path>, 
                                      address :: <remote-value>,
                                      size :: <integer>) => (_ :: <boolean>)

       #f
end method;


///// DISABLE-READ-WATCHPOINT


define method disable-read-watchpoint (ap :: <access-path>,
                                       address :: <remote-value>)
                                       => (_ :: <boolean>)

       #f
end method;


///// QUERY-READ-WATCHPOINT?


define method query-read-watchpoint? (ap :: <access-path>,
                                      address :: <remote-value>)
                                      => (_ :: <boolean>)

       #f
end method;


///// ENABLE-WRITE-WATCHPOINT


define method enable-write-watchpoint (ap :: <access-path>, 
                                      address :: <remote-value>,
                                      size :: <integer>) => (_ :: <boolean>)

       #f
end method;


///// DISABLE-WRITE-WATCHPOINT


define method disable-write-watchpoint (ap :: <access-path>,
                                        address :: <remote-value>)
                                         => (_ :: <boolean>)

       #f
end method;


///// QUERY-WRITE-WATCHPOINT?


define method query-write-watchpoint? (ap :: <access-path>,
                                       address :: <remote-value>)
                                        => (_ :: <boolean>)

       #f
end method;


///// ENABLE-EXECUTE-WATCHPOINT


define method enable-execute-watchpoint (ap :: <access-path>, 
                                         address :: <remote-value>,
                                         size :: <integer>) => (_ :: <boolean>)

       #f
end method;


///// DISABLE-EXECUTE-WATCHPOINT


define method disable-execute-watchpoint (ap :: <access-path>,
                                          address :: <remote-value>)
                                          => (_ :: <boolean>)

       #f
end method;


///// QUERY-EXECUTE-WATCHPOINT?


define method query-execute-watchpoint? (ap :: <access-path>,
                                         address :: <remote-value>)
                                         => (_ :: <boolean>)

       #f
end method;


