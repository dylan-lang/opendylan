module: idvm-namespace
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// For booting name tables.




define macro library-namer
  { library-namer ( ?name:expression ) } =>
  { install-library(?name); }
end macro;

define macro module-namer
  { module-namer ( ?name:expression ) } =>
  { install-module(?name); }
end macro;

define macro constant-namer
  { constant-namer ( ?name:expression , ?value:expression ) } =>
  { install-constant(?name,?value); }
end macro;


define macro variable-namer
  { variable-namer ( ?name:expression , ?value:expression ) } =>
  { let variable-getter = method () ?value end;
    let variable-setter = method (newval) ?value := newval end;
     install-constant(?name, ?value);
     install-variable(?name, variable-getter, variable-setter); }
end macro;
