module:       sockets-internals
Author:       Toby Weinberg
Synopsis:     General socket condition classes.
              More specific stuff is in the platform specific accessors
              where most errors are actually detected.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(socket-condition-details, host-address-setter, host-name-setter,
          host-port-setter, host-port, the-closed-socket,
          the-closed-socket-setter, accessor-started?-value,
          accessor-started?-value-setter, thread-that-closed-accessor,
          thread-that-closed-accessor-setter, calling-thread,
          calling-thread-setter, calling-function-setter,
          <socket-accessor-condition>, explanation-setter,
          protocol-name, protocol-name-setter, service-name,
          service-name-setter);

define open /* abstract */ class <socket-condition> (<simple-condition>)
  slot socket-condition-details :: <object>, init-keyword: details:,
    init-value: #f;
end class <socket-condition>;

define open class <socket-warning> (<socket-condition>, <warning>)
end class <socket-warning>;

define open class <socket-error> (<socket-condition>, <error>)
end class <socket-error>;

define open class <recoverable-socket-condition>
    (<socket-condition>, <serious-condition>)
end class;

define open class <internal-socket-error> (<socket-error>)
end class;

define open class <sockets-not-initialized> (<socket-error>)
end class;

define open class <blocking-call-interrupted> (<recoverable-socket-condition>)
end class;

define open class <out-of-resources> (<recoverable-socket-condition>)
end class;

define open class <network-not-responding> (<recoverable-socket-condition>)
end class;

define open class <invalid-address> (<recoverable-socket-condition>)
  slot host-address, init-keyword: bad-address:;
end class;

define class <host-not-found> (<recoverable-socket-condition>)
  slot host-name, init-keyword: host-name:;
  slot host-address, init-keyword: host-address:;
end class;

define class <service-not-found> (<recoverable-socket-condition>, <sealed-object>)
  slot service-name, init-keyword: service:;
  slot protocol-name, init-keyword: protocol:;
end class;

define class <server-not-responding> (<recoverable-socket-condition>, <sealed-object>)
  slot host-address, init-keyword: host-address:;
end class;

define class <host-unreachable> (<recoverable-socket-condition>, <sealed-object>)
  slot host-address, init-keyword: host-address:;
  slot host-port, init-keyword: host-port:;
end class;

define class <connection-failed> (<recoverable-socket-condition>, <sealed-object>)
  slot host-address, init-keyword: host-address:;
  slot host-port, init-keyword: host-port:;
end class;

define class <connection-closed> (<recoverable-socket-condition>, <sealed-object>)
  slot host-address, init-keyword: host-address:;
  slot host-port, init-keyword: host-port:;
end class;

define class <address-in-use> (<recoverable-socket-condition>, <sealed-object>)
  slot host-address, init-keyword: host-address:;
  slot host-port, init-keyword: host-port:;
end class;

define class <socket-closed> (<recoverable-socket-condition>, <sealed-object>)
  slot the-closed-socket, init-keyword: socket:
end class;


define class <socket-accessor-closed-error> (<internal-socket-error>, <sealed-object>)
  slot calling-function :: <string>, init-keyword: calling-function:;
  slot calling-thread :: <thread>, init-keyword: calling-thread:;
  slot accessor-started?-value, init-keyword: accessor-started?-value:;
  slot thread-that-closed-accessor,
    init-keyword: thread-that-closed-accessor:;
end class;

/*
define method socket-warning(format-string :: <string>,
                              #rest format-arguments) => ()
  signal(make(<socket-warning>,
         format-string: format-string,
         format-arguments: format-arguments))
end method socket-warning;

define method socket-error(format-string :: <string>,
                            #rest format-arguments) => ()
  error(make(<socket-error>,
        format-string: format-string,
        format-arguments: format-arguments))
end method socket-error;

define method print-object(condition :: <socket-condition>,
                           stream :: <stream>)
 => ()
  print(condition.object-class, stream);
  write(stream, "; ");
  write(stream, condition.socket-condition-message);
  write(stream, ".");
end method print-object;

define method default-handler(warning :: <socket-warning>)
 => false :: singleton(#f);
  write(*standard-error*, "Warning ");
  format(*standard-error*,"%=\n", warning);
  #f;
end method default-handler;


define method default-handler(error :: <socket-error>)
 => false :: singleton(#f);
  write(*standard-error*, "Error ");
  format(*standard-error*,"%=\n", error);
  #f;
end method default-handler;
*/
