Module: test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function create-test-stream () => (istream :: <LPSTREAM>)
  let (status :: <HRESULT>, istg :: <Interface>)
    = StgCreateDocfile($NULL-OLESTR,
                       $STGM-READWRITE + $STGM-SHARE-EXCLUSIVE 
                         + $STGM-CREATE + $STGM-DELETEONRELEASE,
                       0);
  check-ole-status(status, "StgCreateDocfile", $null-interface);
  let (status :: <HRESULT>, stream-interface :: <Interface>)
    = IStorage/CreateStream(istg,
                            OLESTR("COM Test Stream"),
                            $STGM-READWRITE + $STGM-SHARE-EXCLUSIVE
                              + $STGM-CREATE,
                            0, 0);
  check-ole-status(status, "IStorage/CreateStream", istg);
  pointer-cast(<LPSTREAM>, stream-interface);
end;

define test com-istream-test
  (name: "com-istream-test",
   description: "tests <storage-istream> Dylan stream support")

  let istream :: <LPSTREAM> = create-test-stream();
  check-true("<LPSTREAM> is <storage-istream>",
             instance?(istream, <storage-istream>));
  check-no-errors("write-element", write-element(istream, 'c'));
  check-no-errors("write", write(istream, "STRING"));
  check-equal("stream-position after write", 7, istream.stream-position);
  check-no-errors("stream-position-setter", istream.stream-position := 3);
  check-equal("stream-position after set", 3, istream.stream-position);
  check-no-errors("write sequence", write(istream, OLESTR("ring a ding")));
  check-equal("stream-size", 14, istream.stream-size);
  istream.stream-position := 0;
  check-equal("read", "cSTr", read(istream, 4));
  let bbuf = make(<byte-string>, size: 5, fill: 'z');
  check-equal("read-into! string call", 3, read-into!(istream, 3, bbuf, start: 1));
  check-equal("read-into! string value", "zingz", bbuf);
  let vbuf = make(<vector>, size: 5, fill: 'z');
  check-equal("read-into! vector call", 3, read-into!(istream, 3, vbuf, start: 1));
  check-equal("read-into! vector value", #['z', ' ', 'a', ' ', 'z'], vbuf);
  check-equal("read-element", 'd', read-element(istream));
  check-equal("stream-position after read", 11, istream.stream-position);
  check-equal("peek", 'i', peek(istream));
  check-equal("stream-position after peek", 11, istream.stream-position);
  check-equal("read-element after peek", 'i', read-element(istream));
  check-condition("incomplete read", <incomplete-read-error>, read(istream, 3));
end;

