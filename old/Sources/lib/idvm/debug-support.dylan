Dylan headers: (:module :language) (("infix-dylan") ("idvm"))
Module: idvm
Language: infix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *print-debug* = #t;

define method idvm-format(format-string :: <string>, #rest args)
    if (*print-debug*)
        apply(format-out,format-string,args)
    end
end;

define generic my-debug-name(o :: <object>);

define method my-debug-name(o :: <object>) as(<string>,o.object-class.debug-name) end;

define method my-debug-name(o :: <class>) concatenate("<",o.debug-name,">") end;
// singletons for classes that don't inherit from <class> for translator implementation reasons.
define method my-debug-name(o == <function>) "<function>" end;

define method my-debug-name(o :: <boolean>)
 select(o by id?)
  #f => "#f";
  #t => "#t";
 end select;
end;
