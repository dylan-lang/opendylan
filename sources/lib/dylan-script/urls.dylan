Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Most urls look a bit like this

define method split-url
    (string :: <byte-string>) 
 => (host-part :: <byte-string>, 
     port-part :: false-or(<byte-string>), 
     path-part :: false-or(<byte-string>))
  assert(string[0] == '/' & string[1] == '/');
  let host-start = 2;
  let host-end = find-delimiters(string, #['/', ':'], start: host-start);
  let host = copy-sequence
               (string, start: host-start, end: host-end | size(string));
  let (port, port-end)
    = if (host-end & string[host-end] == ':')
        let port-end = find-delimiters(string, #['/'], start: host-end + 1);
        let port
          = copy-sequence
              (string, start: host-end + 1, end: port-end | size(string));
        values(port, port-end);
      else
        values(#f, host-end);
      end;
  let file 
    = if (port-end)
        copy-sequence(string, start: port-end);
      else
        #f
      end;
  values(host, port, file);
end method;

define method find-delimiters
    (string :: <string>, delimiters :: <sequence>,
     #key start :: <integer> = 0)
 => (position :: false-or(<integer>))
  block (return)
    for (index :: <integer> from start below string.size)
      when (member?(string[index], delimiters))
	return(index)
      end
    end
  end
end method find-delimiters;

// eof
