Module: unicode-data-generator
Synopsis: Generator for name tables
Author: Ingo Albrecht <prom@berlin.ccc.de>
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

define method ucd-generate-character-name (ucd :: <ucd-database>, filename :: <object>)
 => ();
  message("Generating character name table...");
  let os = make(<file-stream>, locator: filename, direction: #"output");
  let db = ucd-characters(ucd);

  format(os, "module: unicode-data\n\n");

  for(plane from 0 below $u-plane-count)
    format(os, "define constant $unicode-character-names-plane%d = #[\n", plane);
    for(char from 0 below $u-plane-size)
      let cp = plane * $u-plane-size + char;
      let uc = ucd-characters(ucd)[cp];
      if (uc & uc-name-indices(uc))
        let indices = join(map(integer-to-string, uc-name-indices(uc)), ",");
        format(os, "#[%s],\n", indices);
      else
        format(os, "#f,\n");
      end;
    end for;
    format(os, "];\n");
  end;

  let tokens = ucd-name-tokens(ucd);
  format(os, "define constant $unicode-character-name-tokens = #[\n");
  for (token in tokens, i from 0)
    if (i == 0)
      format(os, "\"%s\"", token);
    else
      format(os, ",\n\"%s\"", token);
    end;
  end;
  format(os, "\n];\n");

  force-output(os);

  message("done\n");
end method;
