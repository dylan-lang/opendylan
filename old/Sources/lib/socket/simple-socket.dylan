Module: socket-internals
Author: James Casey
Synopsis: Default Socket Implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <simple-socket> (<socket>)
end class <simple-socket>;

define method descriptor(this :: <simple-socket>)
 => (retval :: false-or(<integer>))
  socket-descriptor(this.socket-accessor);
end method descriptor;

// Methods
define method socket-create(this :: <simple-socket>)
 =>()
  let ret = c-socket-create();
  if (ret == -1)
    socket-error(socket-error-string("create"));
  else
    this.socket-accessor := new-accessor(#"socket", locator: ret);
  end if;
end method socket-create;

define method socket-connect(this :: <simple-socket>, 
		      #key inet-address: in-addr :: false-or(<inet-address>),
		      host-name: in-hostname :: false-or(<byte-string>),
		      port: in-port :: <integer>) 
 => ()
  this.socket-port := in-port;
  if (in-addr)
    this.socket-inet-address := in-addr;
  else
    this.socket-inet-address := inet-address-by-host-name(in-hostname);
  end;
  let (ret :: <integer>, lport :: <integer> ) = 
    c-socket-connect(this.descriptor,
		     address(this.socket-inet-address),
		     this.socket-port);
  if (ret == -1)
    socket-error(socket-error-string("connect"));
  else
    this.socket-localport := lport;
  end if;
  this.socket-data-stream := make(<socket-stream>, 
				  accessor: this.socket-accessor);
end method socket-connect;

define method socket-read(desc :: <integer>, buffer :: <vector>,
			  offset :: <integer>, count :: <integer>)
 =>(nread :: <integer>)
  c-socket-recv(desc, buffer, offset, count, 0);
end method socket-read;

define method socket-write(desc :: <integer>, buffer :: <vector>,
			  offset :: <integer>, count :: <integer>)
 =>(nread :: <integer>)
  c-socket-send(desc, buffer, offset, count, 0);
end method socket-write;

/*
define method socket-bind(this :: <simple-socket>, 
				   where :: <inet-address>,
				   lport :: <integer
 => ()
  this.inet-address := where;
  socket-bind(this.%c-desc, where.address, lport);
end method socket-bind;
  
define method socket-listen(this :: <simple-socket>, 
				     backlog :: <integer>) 
 =>()
  socket-listen(this.%c-desc, backlog);
end method socket-listen;

define method socket-accept(this :: <simple-socket>, 
			    what :: <simple-socket>) 
 => ()
  socket-accept(this.%c-desc, what.%c-desc);
end method socket-accept;
*/

define method socket-close(this :: <simple-socket>) 
 => ()
    if (this.descriptor)
      synchronize-output(this.socket-data-stream);
      let (ret :: <integer>)=
	c-socket-close(this.descriptor); // Do connection breakdown
        if (ret == -1)
	  socket-error(socket-error-string("close"));
	end if;
    end if; 
end method socket-close;


