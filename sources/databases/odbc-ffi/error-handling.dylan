module: dylan-odbc-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method succeeded(x)
  x == $sql-success | x == $sql-success-with-info;
end;

define class <odbc-error> (<error>)
  slot function-name :: <string>;
  slot sqlstate :: <string>;
  slot error-code :: <integer>;
  slot error-message :: <string>;
  slot warning-message :: <string>;
end class;

define method report (condition :: <odbc-error>)
  format-out("ODBC Error in ~s:  SQLState ~a.  Error code ~d\nMessage:  ~a\n",
	     condition.function-name, condition.sqlstate,
	     condition.error-code, condition.error-message);
  if (slot-initialized?(condition, warning-message))
    format-out("Warning:  %s\n",condition.warning-message);
  end if;
end method;

