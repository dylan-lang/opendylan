Module:    RotNExample-client
Synopsis:  A brief description of the project.
Author:    1998/7/31 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main () => ()
  with-ole
    format-out("Client connecting to server.\n");
    let server = make-RotNExample();
    local method encrypt-and-decrypt () => ()
      let plaintext = "And he was going ooo-la, oooooo-la...";
      format-out("Plaintext is %=, encrypting.\n", plaintext);
      let ciphertext = IRotNExample/encrypt(server, plaintext);
      format-out("Ciphertext is %=, decrypting.\n", ciphertext);
      let decrypted = IRotNExample/decrypt(server, ciphertext);
      format-out("Decrypted text is %=.\n", decrypted);
    end method;
    encrypt-and-decrypt();
    server.IRotNExample/key := 3;
    format-out("Set key to %d.\n", server.IRotNExample/key);
    encrypt-and-decrypt();
    format-out("Client releasing server.\n");
    release(server);
  end with-ole;
end method main;

begin
  main();
end;
