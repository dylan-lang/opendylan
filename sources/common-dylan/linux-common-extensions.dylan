Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     Common extensions to Dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline-only function get-application-filename
    () => (filename :: false-or(<byte-string>))
  let exe-path = "/proc/self/exe";
  let buffer = make(<byte-string>, size: 8192, fill: '\0');
  let count
    = raw-as-integer(%call-c-function ("readlink")
                       (path :: <raw-byte-string>,
                        buffer :: <raw-byte-string>,
                        bufsize :: <raw-c-size-t>)
                       => (count :: <raw-c-ssize-t>)
                       (primitive-string-as-raw(exe-path),
                        primitive-string-as-raw(buffer),
                        integer-as-raw(8192))
                    end);
  unless (count = -1)
    copy-sequence(buffer, end: count)
  end;
end;
