module:    idvm-harp
Synopsis:  The <idvm-harp-variables> class - variables for IDVM
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define primary class <idvm-harp-variables> (<harp-variables>)
  //  "A vector of IDVM-style stack argument registers"
  slot argument-registers  :: <stretchy-vector>;
end class;

define method initialize
   (vars :: <idvm-harp-variables>, #rest r) => <idvm-harp-variables>;
  next-method();
  vars.argument-registers := make(<stretchy-vector>);
  vars;
end;

define method make-harp-variables-internal
    (backend :: <idvm-back-end>, name, #rest keys, #key, #all-keys) 
     => (new :: <idvm-harp-variables>)
  apply(make, <idvm-harp-variables>, function-name: name, keys);
end method;


