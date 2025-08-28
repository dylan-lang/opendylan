Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004-2025 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Split command modeled after Jamplus's extension to Perforce Jam

define function jam-builtin-split
    (jam :: <jam-state>, strings :: <sequence>, split-chars-seq :: <sequence>)
 => (result :: <sequence>);
  // Build a set of delimiter characters
  let delimiters = make(<set>);
  for (split-chars in split-chars-seq)
    for (c in split-chars)
      add!(delimiters, c);
    end for;
  end for;

  local
    method delimiter-splitter
        (str :: <string>, start-pos :: <integer>, end-pos :: <integer>)
     => (sep-start :: false-or(<integer>), sep-end :: false-or(<integer>))
      iterate loop (i :: <integer> = start-pos,
		    sep-start :: false-or(<integer>) = #f)
        if (i >= end-pos)
          values(sep-start, i)
        elseif (member?(str[i], delimiters))
          loop(i + 1, sep-start | i)
        elseif (sep-start)
          values(sep-start, i)
        else
          loop(i + 1, #f)
        end if
      end iterate;
    end method;

  let result = make(<stretchy-vector>);
  for (str :: <string> in strings)
    concatenate!(result, split(str, delimiter-splitter, remove-if-empty?: #t));
  end for;
  result
end function;
