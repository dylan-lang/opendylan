Module:    bank-server
Synopsis:  The initialization code for the CORBA server of the bank example.
Author:    Claudio Russo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <location-service> = one-of(#"naming-service", #"shared-file");

define constant $bank-ior-file :: <string> = "c:\\temp\\bank-demo.ior";

define constant $name = make(CosNaming/<Name>, size: 1, fill: make(CosNaming/<NameComponent>, id: "Current Bank", kind: "Bank"));

define constant $dbms = make(<odbc-dbms>);
define constant $datasource-name :: <string> = "bankDB";
define constant $user-name :: <string> = "";
define constant $user-password :: <string>  = "";

define method initialize-server ()
  let location-service = get-location-service();  

  // get reference to ORB
  let orb = CORBA/ORB-init(make(CORBA/<arg-list>), "Functional Developer ORB");

  // get reference to root POA, initially in the holding state
  let RootPOA = CORBA/ORB/resolve-initial-references(orb, "RootPOA");

  with-dbms ($dbms)
     // connect to the database 
     let database = make(<database>, datasource-name: $datasource-name);
     let user =  make(<user>, user-name: $user-name, password: $user-password);
     let connection = connect(database, user); 

     // make the server frame, initialize and refresh it.
     let server-frame = make(<server-frame>, connection: connection);
     server-frame.refresh-check-button.gadget-value := #t;
     refresh(server-frame);

     //  make the bank servant
     let bank = make(<bank-implementation>, connection: connection, poa: RootPOA, name: "Dylan Bank", server-frame: server-frame);
    
     // get the servant's object reference from the poa
     let bank-reference = PortableServer/POA/servant-to-reference(bank.poa, bank);

     // activate the bank's POA using its POA manager.
     let POAManager = PortableServer/POA/the-POAManager(bank.poa);
     PortableServer/POAManager/activate(POAManager);

     // register the bank with the location service
     register-bank(orb, location-service, bank-reference);

     // create a separate thread to shut down the orb, unblocking the main thread.
     make(<thread>, 
          function: method () 
                      start-frame(server-frame);
                      CORBA/ORB/shutdown(orb, #t);
            	    end method);
     
     // block the main thread
     CORBA/ORB/run(orb);

     // remove from location service
     unregister-bank(orb, location-service, bank-reference);

     // close the bank's connection.
     disconnect(connection);
  end with-dbms;
end method;

define method register-bank
    (orb :: corba/<orb>, location-service == #"naming-service", bank-reference :: corba/<object>)
  let name-service = as(CosNaming/<NamingContext>, CORBA/ORB/resolve-initial-references(orb, "NameService"));
  block ()
    CosNaming/NamingContext/bind(name-service, $name, bank-reference);
  exception(e :: CosNaming/NamingContext/<AlreadyBound>) 
    CosNaming/NamingContext/rebind(name-service, $name, bank-reference);
  end block;
end method;

define method register-bank
    (orb :: corba/<orb>, location-service == #"shared-file", bank-reference :: corba/<object>)
  corba/orb/object-to-file(orb, $bank-ior-file, bank-reference);
end method;

define method unregister-bank
    (orb :: corba/<orb>, location-service == #"naming-service", bank-reference :: corba/<object>)
  let name-service = as(CosNaming/<NamingContext>, CORBA/ORB/resolve-initial-references(orb, "NameService"));
  block ()
    CosNaming/NamingContext/unbind(name-service, $name);
  exception(e :: CosNaming/NamingContext/<NotFound>) 
    // ignore
  end block;
end method;

define method unregister-bank
    (orb :: corba/<orb>, location-service == #"shared-file", bank-reference :: corba/<object>)
  // do nothing
end method;

define method get-location-service () => (ls :: <location-service>)
  block (return)
    let option = "location-service";
    let value-delimiter = ':';
    let arguments = application-arguments();
    for (i from 0 to size(arguments))
      let pos = subsequence-position(arguments[i], option);
      if (pos)
        let after = pos + size(option);
        let name =
          if (element(arguments[i], after) = value-delimiter)
            copy-sequence(arguments[i], start: after + 1)
          else
            arguments[i + 1]
          end if;
        let ls :: <location-service> = as(<symbol>, name);
        return (ls)
      end if;
    end for;
    #"shared-file"
  exception (condition :: <error>)
    notify-user("Unknown location service, using shared file");
    #"shared-file"
  end block;
end method;

begin
  initialize-server();
end;





