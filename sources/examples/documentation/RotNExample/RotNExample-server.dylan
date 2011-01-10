Module:    RotNExample-server
Synopsis:  A simple COM server demo.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define COM-interface <IRotN-implementation> (<IRotNExample>)
  slot IRotNExample/key :: type-union(<integer>, <machine-word>) = 13;
end;

define method IRotNExample/encrypt 
        (this :: <IRotN-implementation>, pre :: <string>)
     => (result :: <HRESULT>, post :: <string>)
  if (instance?(this.IRotNExample/key, <integer>))
    let post = make(<string>, size: pre.size);
    for (char keyed-by index in pre)
      post[index] := rot-char-by-n(char, this.IRotNExample/key);
    end for;
    values($S-OK, post)
  else
    values($E-INVALIDARG, "")
  end if
end;

define method IRotNExample/decrypt 
        (this :: <IRotN-implementation>, pre :: <string>)
     => (result :: <HRESULT>, post :: <string>)
  if (instance?(this.IRotNExample/key, <integer>))
    let post = make(<string>, size: pre.size);
    for (char keyed-by index in pre)
      post[index] := rot-char-by-n(char, -this.IRotNExample/key);
    end for;
    values($S-OK, post)
  else
    values($E-INVALIDARG, "")
  end if
end;

define function rot-char-by-n
        (char :: <character>, n :: <integer>) => (r :: <character>)
  let char-as-int = as(<integer>, char);
  local method rot-if-in-range 
          (lower :: <integer>, upper :: <integer>) => ()
    if (lower <= char-as-int & char-as-int <= upper)
      char-as-int := lower + modulo(char-as-int - lower + n, upper - lower + 1);
    end if;
  end method;
  rot-if-in-range(as(<integer>, 'a'), as(<integer>, 'z'));
  rot-if-in-range(as(<integer>, 'A'), as(<integer>, 'Z'));
  as(<character>, char-as-int)
end;

define method terminate (this :: <IRotN-implementation>) => ()
  next-method();
  PostQuitMessage(0);  // Cause main event loop to terminate.
end;

define coclass $RotNExample-type-info
  name "RotNExample";
  uuid $RotNExample-class-id;
  default interface <IRotN-implementation>;
end coclass;

define method main () => ()
  if (OLE-util-register-only?())
    register-coclass($RotNExample-type-info, "FunDev.RotNExample");
  else
    let factory :: <class-factory>
            = make-object-factory($RotNExample-type-info);

    with-stack-structure (pmsg :: <PMSG>)
      while (GetMessage(pmsg, $NULL-HWND, 0, 0))
        TranslateMessage(pmsg);          
        DispatchMessage(pmsg);
      end while;     
    end with-stack-structure;

    revoke-registration(factory);
  end if;
end method main;

begin
  main();
end;
