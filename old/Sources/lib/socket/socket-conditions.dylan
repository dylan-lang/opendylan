module: socket-conditions
Author: James Casey
Synopsis: Socket conditions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <socket-condition> (<condition>)
  slot socket-condition-format-string :: <string>, 
    init-keyword: format-string:, init-value: "";
  slot socket-condition-format-arguments, 
    init-keyword: format-arguments:, init-value: #[];
  virtual slot socket-condition-message :: <string>;
end class <socket-condition>;

define inline method socket-condition-message(this :: <socket-condition>)
 =>(retval :: <string>)
  format-to-string(this.socket-condition-format-string, 
		   this.socket-condition-format-arguments);
end method socket-condition-message;

define open class <socket-warning> (<socket-condition>,<warning>)
end class <socket-warning>;

define open class <socket-error> (<socket-condition>, <error>)  
end class <socket-error>;

define class <unknown-host-error> (<socket-error>)
  slot host-name, init-keyword: host-name:;
end class <unknown-host-error>;

define class <socket-connection-refused-error> (<socket-error>)
  slot host-name, init-keyword: host-name:;
  slot host-port, init-keyword: host-port:;
end class <socket-connection-refused-error>;

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
