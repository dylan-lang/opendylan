Module: dfmc-harp-native-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method immediate?? (register)
  instance?(register, <integer>)
end method immediate??;

define sideways method op--store-multiple-values-count
(back-end :: <harp-native-back-end>, reg) => ()
  if (reg.immediate??)
    if (reg == 1)
      ins--reset-values(back-end);
    else
      ins--set-values(back-end);
      op--st-mv-count(back-end, reg);
    end if;
  else
    let done-tag = make(<tag>);
    let reset-tag = make(<tag>);
      ins--beq(back-end, reset-tag, reg, 1);
      ins--set-values(back-end);
      op--st-mv-count(back-end, reg);
      ins--bra(back-end, done-tag);
      ins--tag(back-end, reset-tag);
      ins--reset-values(back-end);
      ins--tag(back-end, done-tag);
  end if;
  values();
end method op--store-multiple-values-count;

define sideways method op--bmvset(back-end :: <harp-native-back-end>, tag :: <tag>) => ()
  ins--bmvset(back-end, tag);
end method;


define program-warning <native-harp-warning>
  slot condition-description, required-init-keyword: description:;
  format-string "%s";
  format-arguments description;
end;

define sideways method harp-warning (be :: <harp-native-back-end>, #rest args)
  note(<native-harp-warning>,
       description: apply(format-to-string, args));
end;
