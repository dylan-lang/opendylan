Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *orbix-include-files* = make(<table>);

define suite orbix-include-idl (setup-function: setup-orbix-include-idl,
                                cleanup-function: cleanup-orbix-include-idl)
  test orbix-include-idl-AttrDf;
  test orbix-include-idl-ConstDf;
  test orbix-include-idl-Contd;
  test orbix-include-idl-Contr;
  test orbix-include-idl-ExcDf;
  test orbix-include-idl-IRDfs;
  test orbix-include-idl-InterDf;
  test orbix-include-idl-ModDf;
  test orbix-include-idl-OperDf;
  test orbix-include-idl-ParmDf;
  test orbix-include-idl-Rep;
  test orbix-include-idl-TypDf;
  test orbix-include-idl-daemon;
end suite;

define method setup-orbix-include-idl ()
  push(*cpp-include-path*, ".");
  setup-idl-files(*orbix-include-files*);
  scepter-case-sensitive-reserved-words?(get-scepter()) := #t;
end method;

define method cleanup-orbix-include-idl ()
  cleanup-idl-files(*orbix-include-files*);
  scepter-reset-options(get-scepter());
end method;

