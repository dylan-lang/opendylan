Module: dfmc-macro-expander
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro handling-parse-errors
  { handling-parse-errors 
      ?:body 
    on-error (?:variable, #rest ?options:*)
      ?fixup:body
    end }
    => { do-handling-parse-errors
           (method () ?body end, method (?variable) ?fixup end, ?options) }
end macro;

define thread variable *parse-handler-depth* = 0;

define inline method do-handling-parse-errors (body, fixup, #key context = "form")
  let this-depth = *parse-handler-depth* + 1;
  local method closest-handler? (condition)
    this-depth = *parse-handler-depth*
  end method;
  dynamic-bind (*parse-handler-depth* = this-depth)
    block ()
      body();
    exception (e :: type-union(<macro-match-error>, <reader-error>),
                 test: closest-handler?)
      fixup(e)
    end;
  end;
end method;

// eof
