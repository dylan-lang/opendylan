Module: corba-tests-client
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *single-marshalling-stream* :: false-or(<marshalling-stream>) = #f;

define method check-marshalling (message, typecode, input, #key coerce = identity, test = \=)
  if (*single-marshalling-stream*)
    do-check-marshalling(*single-marshalling-stream*, message, typecode, input, coerce, test);
  else
    with-marshalling-stream (stream, inner-stream: #f)
      do-check-marshalling(stream, message, typecode, input, coerce, test); 
    end;
  end;
end method;

define method do-check-marshalling (stream, message, typecode, input, coerce, test)
  marshall(typecode, input, stream);
  let output = unmarshall(typecode, stream);
  check(message, test, input, coerce(output));
end method;

