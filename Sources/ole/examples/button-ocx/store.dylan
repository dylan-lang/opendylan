Module:    button-ocx
Synopsis:  Persistent storage
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $storage-validation-key = 70707; // arbitrary value

define method save-frame-to-storage (frame :: <my-frame>,
				     istream :: <storage-istream>) => ();

  istream-write-integer(istream, $storage-validation-key);
  istream-write-string(istream, frame.my-text);
end method;


define method load-frame-from-storage (frame :: <my-frame>,
				       istream :: <storage-istream>) => ();
  block()
    let key = istream-read-integer(istream);
    if (key = $storage-validation-key)
      frame.my-text := istream-read-string(istream);
    else
      debug-message("ignoring invalid data");
    end if;
  exception (err :: <ole-error>)
    debug-message("ignoring error: %s", err);
  end block;
end method;
