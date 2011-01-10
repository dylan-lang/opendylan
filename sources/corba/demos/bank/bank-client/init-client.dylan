Module:    bank-client
Synopsis:  The initialization code for the CORBA client of the bank example.
Author:    Claudio Russo, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <location-service> = one-of(#"naming-service", #"shared-file");

define constant $bank-ior-file :: <string> = "c:\\temp\\bank-demo.ior";

define constant $name = make(CosNaming/<Name>, size: 1, fill: make(CosNaming/<NameComponent>, id: "Current Bank", kind: "Bank"));

define method initialize-client ()
  let orb = CORBA/ORB-init(make(CORBA/<arg-list>), "Functional Developer ORB");
  let ls = get-location-service();
  block ()
    let bank = lookup-bank(orb, ls);
    let bank-frame = make(<bank-frame>, bank: bank);
    start-frame(bank-frame);
  exception (lookup-bank-failure(orb, ls))
    notify-user("Cannot locate the Bank. Click OK to Exit.");
  end block;
end method;

define method lookup-bank (orb :: corba/<orb>, location-service == #"shared-file")
    => (bank :: bankingdemo/<bank>)
  as(BankingDemo/<bank>, corba/orb/file-to-object(orb, $bank-ior-file));
end method;

define method lookup-bank (orb :: corba/<orb>, location-service == #"naming-service")
    => (bank :: bankingdemo/<bank>)
  let name-service = as(CosNaming/<NamingContext>, CORBA/ORB/resolve-initial-references(orb, "NameService"));
  as(BankingDemo/<bank>, CosNaming/NamingContext/resolve(name-service, $name));
end method;

define method lookup-bank-failure (orb :: corba/<orb>, location-service == #"naming-service")
  CosNaming/NamingContext/<NotFound>
end method;

define method lookup-bank-failure (orb :: corba/<orb>, location-service == #"shared-file")
  <file-does-not-exist-error>
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
  initialize-client()
end;
