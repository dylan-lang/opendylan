module: doss-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// For booting name tables.




define macro constant-namer
  { constant-namer ( ?name:expression , ?value:expression ) } =>
  { block ()
      install-constant(?name,?value);
      #f;
    exception (c :: <error>)
      format(#t, "\nWARNING: ~s not defined as a constant", ?name);
      #f
    end }
end macro;


define macro variable-namer
  { variable-namer ( ?name:expression , ?value:expression ) } =>
  { block ()
      let variable-getter = method () ?value end;
      let variable-setter = method (newval) ?value := newval end;
      install-variable(?name, variable-getter, variable-setter);
      #f;
    exception (c :: <error>)
      format(#t, "\nWARNING: ~s not defined as a variable", ?name);
      #f
    end }
end macro;
