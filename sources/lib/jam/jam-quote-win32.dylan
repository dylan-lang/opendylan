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
// See http://msdn.microsoft.com/en-us/library/17w5ykft.aspx
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
          '|', '<', '>', '&', '^', ' ', '\t', '\"' =>
            q(index);
          '\\' =>
            bs(index + 1, index);
          otherwise =>
            nqc(index + 1);
        end select;
      elseif (_start = _end)
        "\"\""
      else
        copy-sequence(argument, start: _start, end: _end)
      end if
    end method,

    // Strings of backslashes
    method bs
        (index :: <integer>, start-index :: <integer>)
     => (result :: <byte-string>);
      if (index < _end)
        select (argument[index])
          ' ', '\t' =>
            q(index);
          '\"' =>
            q(start-index);
          '\\' =>
            bs(index + 1, start-index);
          otherwise =>
            nqc(index + 1);
        end select;
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

    // Quoted characters
    method qc (s :: <stream>, index :: <integer>) => ();
      if (index < _end)
        let c = argument[index];
        select (c)
          '\\' =>
            qbs(s, index + 1, 1);
            
          '\"' =>
            write(s, "\\\"");
            qc(s, index + 1);
            
          otherwise =>
            write-element(s, c);
            qc(s, index + 1);
        end
      end if
    end method,

    // Quoted strings of backslashes
    method qbs (s :: <stream>, index :: <integer>, count :: <integer>) => ();
      if (index < _end)
        let c = argument[index];
        select (c)
          '\\' =>
            qbs(s, index + 1, count + 1);

          '\"' =>
            for (i from 0 below count)
              write(s, "\\\\");
            end for;
            write(s, "\\\"");
            qc(s, index + 1);

          otherwise =>
            for (i from 0 below count)
              write-element(s, '\\');
            end for;
            write-element(s, c);
            qc(s, index + 1);
        end select;
      else
        for (i from 0 below count)
          write(s, "\\\\");
        end for;
      end if;
    end method;
  
  nqc(_start)
end function;
