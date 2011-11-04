Module: odbc-ffi
Author: yduJ
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *trace-odbc-functions* = #f;

define method succeeded(x)
  x == $sql-success | x == $sql-success-with-info;
end;

define macro c-func-with-err-definer
  { define c-func-with-err ?:name ?options:* end } 
=>
  { define c-function ?name ## "-c-func" ?options end;
    define method ?name (#rest args)
	let (result, #rest more-results) = apply(?name ## "-c-func", args);
	if (*trace-odbc-functions* = #t)
	  format-out("%s with %s - return-code: %d\n",
		     if (succeeded(result)) "Success" else "No success" end if,
		     ?"name",
		     result);
	end if;
	apply (values, result, more-results);
    end method;  }
end macro;


