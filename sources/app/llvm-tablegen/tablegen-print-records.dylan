Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Default backend function, dumping out all records in sorted order

define function tablegen-print-records () => ();
  local
    method print-superclass (superclass :: <tablegen-class>) => ();
      do(print-superclass, superclass.record-superclasses);
      format(*standard-output*, " %s", superclass.record-name);
    end method;

  format(*standard-output*, "------------- Classes -----------------\n");

  for (class-name in sort(key-sequence($tablegen-classes)))
    let class = $tablegen-classes[class-name];
    format(*standard-output*, "class %s", class-name);
    unless (empty?(class.record-template-parameters))
      format(*standard-output*, "<");
      for (parameter in class.record-template-parameters, first? = #t then #f)
        unless (first?) write(*standard-output*, ", ") end;
        format(*standard-output*, "%s %s:%s = %s",
               parameter.initializer-type,
               class-name,
               parameter.initializer-name,
               parameter.initializer-value);
      end for;
      format(*standard-output*, ">");
    end unless;
    format(*standard-output*, " {");
    if (empty?(class.record-superclasses))
      format(*standard-output*, "\n");
    else
      format(*standard-output*, "\t//");
      do(print-superclass, class.record-superclasses);
      format(*standard-output*, "\n");
    end if;

    for (initializer in class.record-initializers)
      format(*standard-output*, "  %s %s = %s;\n",
             initializer.initializer-type,
             initializer.initializer-name,
             initializer.initializer-value);
    end for;

    format(*standard-output*, "}\n");
  end for;

  format(*standard-output*, "------------- Defs -----------------\n");

  for (def-name in sort(key-sequence($tablegen-definitions)))
    let def = $tablegen-definitions[def-name];
    format(*standard-output*, "def %s {", def-name);
    if (empty?(def.record-superclasses))
      format(*standard-output*, "\n");
    else
      format(*standard-output*, "\t//");
      do(print-superclass, def.record-superclasses);
      format(*standard-output*, "\n");
    end if;

    for (initializer in def.record-initializers)
      format(*standard-output*, "  %s %s = %s;\n",
             initializer.initializer-type,
             initializer.initializer-name,
             initializer.initializer-value);
    end for;

    format(*standard-output*, "}\n");
  end for;
end function;

