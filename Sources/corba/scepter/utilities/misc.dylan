Module:    scepter-utilities
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// FIND: simple-minded CL-style 

define method find (x :: <object>, collection :: <collection>,
                    #key
                    key = identity,
                    test = \=,
                    default = #f,
                    reverse-test? ::<boolean> = #f,
                    return-test? :: <boolean> = #f)

  let tester = 
    if (reverse-test?)
      method (x, y) test(y, x) end method;
    else
      test;
    end if;
  block(return)
    for (y in collection)
      let result = tester(x, y.key);
      if (result)
        if (return-test?)
          return(result);
        else
          return(y);
        end if;
      end if;
    end for;
    return(default);
  end block;
end method;

// PRINT-SEPARATED-COLLECTION

define method print-separated-collection
  (collection, separator, stream,
  #key key = identity, start = 0, printer = method (object, stream) format(stream, "%=", object) end method);
  for (i from start below collection.size)
    printer(collection[i].key, stream);
    unless (i == (collection.size - 1))
      format(stream, separator);
    end unless;
  end for;
end method;

// SEEN-INCLUDE-FILENAME-BEFORE?  
//
// define method seen-include-filename-before? (name :: <string>)
//  member?(name, *include-file-names*, test: \=);
// end method;

