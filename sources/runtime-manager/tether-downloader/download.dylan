module:      tether-downloader-internals
synopsis:    Functionality for downloading byte vectors into static blocks.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DOWNLOAD-BYTE-VECTOR-INTO
//    Injects a byte vector into the given static block.

define method download-byte-vector-into
  (access-path :: <access-path>, static-block :: <static-block>,
   bytes :: <byte-vector>,
   #key from-index = 0, to-index = #f)
      => (object-address :: false-or(<remote-value>))

  download-byte-string-into
    (access-path, static-block, as(<byte-string>, bytes),
     from-index: from-index, to-index: to-index);

end method;


///// DOWNLOAD-BYTE-STRING-INTO
//    Injects a byte string into the given static block.

define method download-byte-string-into
  (access-path :: <access-path>, static-block :: <static-block>,
   bytes :: <byte-string>,
   #key from-index = 0, to-index = #f)
      => (object-address :: false-or(<remote-value>))

  unless (to-index)
    to-index := size(bytes) - 1;
  end unless;

  if (access-path == static-block.block-access-path)
    if (static-block.static-block-remaining-size <= to-index)
      // No room at the inn.
      // (Client should have checked anyway, but just in case).
      #f
    else
      let destination-addr = 
        byte-indexed-remote-value(static-block.static-block-base-address,
                                  static-block.block-byte-cursor);
      write-byte-string(access-path, destination-addr, bytes,
                        ending-index: to-index);
      static-block.static-block-remaining-size :=
         static-block.static-block-remaining-size - (to-index + 1);
      static-block.block-byte-cursor :=
         static-block.block-byte-cursor + to-index + 1;
      destination-addr;
    end if
  else
    // This block was not created with the access-path instance that was
    // supplied to this function.
    #f;
  end if
end method;


///// DOWNLOAD-REMOTE-VALUE-INTO
//    Injects a single <remote-value> into a static block, and returns its
//    address.

define method download-remote-value-into
  (access-path :: <access-path>, static-block :: <static-block>,
   value :: <remote-value>)
     => (object-address :: false-or(<remote-value>))
  let object-size = remote-value-byte-size(access-path);
  if (access-path == static-block.block-access-path)
    if (static-block.static-block-remaining-size < object-size)
      // No more room in this block.
      #f
    else
      let destination-addr =
        byte-indexed-remote-value(static-block.static-block-base-address,
                                  static-block.block-byte-cursor);
      write-value(access-path, destination-addr, value);
      static-block.static-block-remaining-size :=
         static-block.static-block-remaining-size - object-size;
      static-block.block-byte-cursor :=
         static-block.block-byte-cursor + object-size;
      destination-addr;
    end if;
  else
    // This memory block was not created with the access-path instance
    // that was supplied to this function.
    #f;
  end if
end method;


///// DOWNLOAD-REMOTE-VALUE-VECTOR-INTO
//    Injects a vector of <remote-value> objects into a static block.

define method download-remote-value-vector-into
    (access-path :: <access-path>, static-block :: <static-block>,
     value-vector :: <vector>)
       => (object-address :: false-or(<remote-value>))
  let object-size = size(value-vector) * remote-value-byte-size(access-path);
  if (object-size == 0)
    #f
  elseif (object-size > static-block.static-block-remaining-size)
    #f
  else
    let i = 0;
    let destination-first = 
      download-remote-value-into(access-path, static-block, value-vector[i]);
    let destination-this = destination-first;
    let limit = size(value-vector);
    i := i + 1;
    while (destination-this & (i < limit))
      destination-this := 
        download-remote-value-into(access-path, static-block, value-vector[i]);
      i := i + 1;
    end while;
    destination-first;
  end if
end method;
