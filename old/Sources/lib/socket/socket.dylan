module: socket-internals
Author: James Casey
Synopsis: Public Interface to socket class
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <socket> (<object>)
  slot socket-accessor :: <socket-accessor>;
  slot socket-localport :: <integer> = 0;
  slot socket-port :: <integer> = 0;
  slot socket-inet-address :: <inet-address>;
  slot socket-data-stream :: <stream>;
end class <socket>;

// We return a concrete subclass of <socket>
define method make(class == <socket>,  #rest args, #key)
 =>(retval :: <socket>)
  if(~ factory-class)
    apply(make, <simple-socket>, args);
  else
    apply(make, factory-class, args);
  end
end;

define method initialize(this :: <socket>, 
			 #key host-name :: false-or(<byte-string>),
			 inet-address :: false-or(<inet-address>),
			 port :: false-or(<integer>) )
  
  socket-create(this);
  block ()
    socket-connect(this, 
		   inet-address: inet-address, 
		   host-name: host-name,
		   port: port);
    this
  exception(c :: <error>, 
	      test: method(c) ~instance?(c,<unknown-host-error>) end)
    signal(make(<socket-error>));
  end block;
end method initialize;


// Private
define variable factory-class :: false-or(<class>) = #f;

// Public
define method socket-factory-setter(what :: <class>,
				    this :: <socket>) 
 => ()
  if (factory-class ~= #f)
    signal(make(<socket-error>,
		format-string: "Socket factory already set; can only set once"));
  end;
  factory-class := what;
  what;
end method socket-factory-setter;

define generic socket-create(this :: <socket>)
 =>(); // throws <socket-error>

define generic socket-connect(this :: <socket>, 
		       #key host-name :: false-or(<byte-string>), 
		       inet-address :: false-or(<inet-address>), 
		       port :: <integer> = 0) 
 =>(); // throws <unknown-host-error>, <socket-error>

/*        
define generic bind(this :: <socket, 
		    #key where :: <inet-address>,
		    port :: <integer> = 0 ) 
 => ();

define generic listen(this :: <socket>, 
		      #key backlog :: <integer>) 
 => ();

define generic accept(this :: <socket>, 
		      what :: <socket>) 
 =>();
*/
define generic socket-close(this :: <socket>)
 =>();


