Module: internal
Author: Neal Feinberg 
        via Tim McNerney and Keith Playford and jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Does a heuristic check to determine whether this string looks like
// a Lisp format string by scanning for the most common Lisp format 
// directives.

define constant $likely-lisp-directives 
  = #('a', 'A', 's', 'S', '%', '&', '~');

define method lisp-format-string? (format-string :: <string>) 
  block (return)
    for (char in format-string, i from 0 below size(format-string) - 1)
      let next-char = format-string[i + 1];
      if (char = '~' & member?(next-char, $likely-lisp-directives))
        return(#t)
      end;
    finally
      #f
    end;
  end;
end method;

define variable *format-strings* = make(<table>);

// Take a Dylan format string and produce a Lisp equivalent.

define method dylan-to-lisp-format-string (format-string :: <string>)
  element(*format-strings*, format-string, default: #f)
  | begin
      let source-size = size (format-string);
      let quote-next = #f;
      let result = make (<stretchy-vector>);
      for (char in format-string, index from 0 below source-size)
	let next-char = (index + 1 < source-size) & format-string[index + 1];
	case 
	  quote-next =>
	    add! (result, quote-next);
	    quote-next := #f;
	  (char = '~') => 
	    add! (result, char);
	    if (member?(next-char, $likely-lisp-directives))
	      quote-next := next-char;
	    end if;
	  (char ~= '%') => 
	    add! (result, char);
	  ((char = '%') & (next-char = '%')) => 
	    quote-next := '%';
	  otherwise =>
	    select (next-char)
	      '='       => add!(result, '~'); quote-next := 'S';
	      '&'       => add!(result, '~'); quote-next := '&';
	      'S', 's'  => add!(result, '~'); quote-next := 'A';
	      'D', 'd'  => add!(result, '~'); quote-next := 'D';
	      'X', 'x'  => add!(result, '~'); quote-next := 'X';
	      'C', 'c'  => add!(result, '~'); quote-next := 'A';
	      otherwise => add!(result, char);
	    end select;
	end case;
      end for;
      *format-strings*[format-string] := as (<string>, result);
    end;
end method;

// eof
