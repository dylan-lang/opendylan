Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004-2025 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// SHELL command modeled after Boost Jam's extension to Perforce Jam

define function jam-builtin-shell
    (jam :: <jam-state>, command :: <sequence>, #rest lol)
 => (result :: <sequence>);
  let exit-status? :: <boolean> = #f;
  let no-output? :: <boolean> = #f;
  let strip-eol? :: <boolean> = #f;
  for (option-list :: <sequence> in lol)
    select (first(option-list, default: "") by \=)
      "exit-status" =>
        exit-status? := #t;
      "no-output" =>
        no-output? := #t;
      "strip-eol" =>
        strip-eol? := #t;
      otherwise =>
        // Ignore
        #f;
    end select;
  end for;

  let (_exit-code, _signal, child, stream)
    = run-application(first(command, default: ""),
                      asynchronous?: #t, output: #"stream",
                      under-shell?: #t);
  let contents = read-to-end(stream);
  let contents
    = if (no-output?)
        ""
      elseif (strip-eol?)
        for (i :: <integer> from contents.size above 0 by -1,
             while: contents[i - 1] == '\n' | contents[i - 1] == '\r')
        finally
          copy-sequence(contents, end: i)
        end
      else
        contents
      end if;

  let (exit-code, signal) = wait-for-application-process(child);

  if (exit-status?)
    vector(contents, integer-to-string(exit-code))
  else
    vector(contents)
  end if
end function;
