Module:  sql-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <invalid-argument> (<error>)
end class <invalid-argument>;


define open class <dbms-not-specified> (<error>)
end class <dbms-not-specified>;

define open class <connection-not-specified> (<error>)
end class <connection-not-specified>;


define open class <data-not-available> (<error>)
end class <data-not-available>;


define open concrete class <result-set-mutation-error> (<error>)
end class <result-set-mutation-error>;


define open concrete class <invalid-datatype-hint> (<warning>)
  constant slot datatype-hint :: <object>,
    required-init-keyword: datatype-hint:;
end class <invalid-datatype-hint>;

//---*** andrewa: not used, for some reason
ignore(datatype-hint);

define open abstract class <database-error> (<error>)
end class <database-error>;

define open abstract class <sql-error> (<database-error>)
end class <sql-error>;

define open class <unhandled-diagnostic> (<sql-error>)
  constant slot diagnostic,
    required-init-keyword: diagnostic:;
end class <unhandled-diagnostic>;

define method condition-to-string
    (condition :: <unhandled-diagnostic>)
 => (string :: false-or(<string>))
  format-to-string("Database error: %s\n", condition.diagnostic.message-text)
end method condition-to-string;
