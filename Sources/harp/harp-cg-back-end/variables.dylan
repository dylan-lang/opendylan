module: harp-cg-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define primary class <harp-cg-variables> (<object>)

  slot current-lambda = #f;
  slot current-scl = #f;
  slot current-parameters :: <simple-object-vector> = #[];

  slot stack-shift :: <number>;
  slot args-to-be-dropped;

  slot count-vreg;// :: <virtual-register>;
  slot next-methods-vreg = #f;// :: <virtual-register>;
  slot result-vreg = #f;// :: <virtual-register>;
  slot function-vreg = #f;// :: <virtual-register>;

  slot exit-tag;

  slot imports :: <table>;
  slot runtime-references :: <table>;
  slot cg-temporaries :: <table>;
  slot tags :: <table>;
  slot cg-references :: <string-table>;
  slot model-references :: <table>;
  slot cg-multiple-values = #f;
  slot required-multiple-values = #f;

end class <harp-cg-variables>;

define method initialize(vars :: <harp-cg-variables>, #rest r, #key prototype :: false-or(<harp-cg-variables>))

  next-method();

  if (prototype)
    vars.imports := prototype.imports;
    vars.runtime-references := prototype.runtime-references;
    vars.cg-references := prototype.cg-references;
    vars.model-references := prototype.model-references;

    let cg-temporaries :: <table> = prototype.cg-temporaries;
    let tags :: <table> = prototype.tags;

    remove-all-keys!(cg-temporaries);
    remove-all-keys!(tags);

    vars.cg-temporaries := cg-temporaries;
    vars.tags := tags;
  else
    vars.imports := make(<table>);
    vars.runtime-references := make(<table>);
    vars.cg-references := make(<string-table>);
    vars.model-references := make(<table>);
    vars.cg-temporaries := make(<table>);
    vars.tags := make(<table>);
  end if;

  vars;
end method initialize;
  
