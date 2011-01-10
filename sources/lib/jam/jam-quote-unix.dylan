Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2006 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// quote-argument
//
// Add quoting characters as necessary
//
// See section 2.2 of the Shell and Utilities volume of SUSv3
// (http://www.unix.org/single_unix_specification).
//
define function quote-argument
    (argument :: <byte-string>,
     #key start: _start :: <integer> = 0,
          end: _end :: <integer> = argument.size)
 => (result :: <byte-string>);
  local
    // Non-quoted characters
    method nqc (index :: <integer>) => (result :: <byte-string>);
      if (index < _end)
        select (argument[index])
          '|', '&', ';', '<', '>', '(', ')', '$', '`',
          '\\', '\"', '\'', ' ', '\t' =>
            q(index);
          otherwise =>
            nqc(index + 1);
        end select;
      elseif (_start = _end)
        "\"\""
      else
        copy-sequence(argument, start: _start, end: _end)
      end if
    end method,

    // Quote string
    method q (index :: <integer>) => (result :: <byte-string>);
      with-output-to-string (s)
        write-element(s, '\"');
        write(s, argument, start: _start, end: index);
        qc(s, index);
        write-element(s, '\"');
      end
    end method,

    // Quoted character
    method qc (s :: <stream>, index :: <integer>) => ();
      if (index < _end)
        let c = argument[index];
        select (c)
          '$', '`', '\"', '\\' =>
            write-element(s, '\\');
            write-element(s, c);
            qc(s, index + 1);
            
          otherwise =>
            write-element(s, c);
            qc(s, index + 1);
        end
      end if
    end method;
  
  nqc(_start)
end function;
