Module:    bank-client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $Bank-class-ID = "{01B68F80-CCC9-11D2-A51C-00600808472F}";
define method main ()
  Debug-message("Startup - about to initialize OLE");
  OLE-initialize();
  Debug-message("Initialized OLE");
  with-COM-interface (bank :: <IBank> = $bank-class-id,
                      interface-id: $IID-<IBank-provider>)
    debug-message("Obtained interface, starting frame");
    let bank-frame = make(<bank-frame>, bank: bank);
    start-frame(bank-frame);
  end;
end method;

/*
define method main ()
  with-COM-interface (bank-ptr :: <Interface> = $bank-class-id)
    break("Got interface %s of class %s", bank-ptr, object-class(bank-ptr));
    let riid = as(<REFIID>, $IID-<IBank-server>);
    let (status :: <HRESULT>, bank :: <Interface>)
       = QueryInterface(bank-ptr, riid);
    break("QueryInterface -> (%s, %s[%s])", status, bank, object-class(bank));
    let bank-frame = make(<bank-frame>, bank: bank);
    start-frame(bank-frame);
  end;
end method;
*/
// Go for it!


main();
