module:    harp-cg-back-end
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract primary class <harp-cg-back-end> (<back-end>)
 slot cg-variables :: <harp-cg-variables>;
end class <harp-cg-back-end>;

define method initialize
   (back-end :: <harp-cg-back-end>, #rest r, #key prototype-back-end :: false-or(<harp-cg-back-end>)) => (new :: <harp-cg-back-end>)
  next-method();
  back-end.cg-variables := make(<harp-cg-variables>, prototype: prototype-back-end & prototype-back-end.cg-variables);
  back-end;
end;
