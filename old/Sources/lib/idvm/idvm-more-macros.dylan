module:    idvm
Synopsis:  Macros to  support IDVM method building
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




/// SELECT-IDVM-CLOSURE
/// The top level closure building macro.
//  It returns a method with between 0 and 10 required arguments, 
//  depending on the arg-count. (More required arguments are not
//  supported). 
//
// Example usage:
//
// select-idvm-closure (arg-count, type-vector, #next nm, #rest rv)
//   idvm-invoke-code((vm-code, nm, env), rv, #f, process-keys: #t);
// end
//
// This usage selects and returns a method based on arg-count. It will
// find the type specifioers from type-vector. Methods uach have (#next nm,
// #rest rv) in their signature. The methods invoke code with (vm-code, nm,
// env) as the initial arguments for threading, followed by the required 
// arguments, followed by the remaining args. The keywords 
// process-keys: and stack-size: are passed through for processing by
// invoke-idvm-code
//

define macro select-idvm-closure

  { select-idvm-closure (?argc:expression, ?pt:expression, ?method-type:*)
      idvm-invoke-code((?before-args:*), ?remaining-args:*);
    end  }
    => { select-method-by-count (?argc) 
            ((a0 :: ?pt[0]) (a1 :: ?pt[1]) (a2 :: ?pt[2]) (a3 :: ?pt[3]) 
             (a4 :: ?pt[4]) (a5 :: ?pt[5]) (a6 :: ?pt[6]) (a7 :: ?pt[7]) 
             (a8 :: ?pt[8]) (a9 :: ?pt[9]) (a10 :: ?pt[10]), ?method-type)
           idvm-invoke-code((?before-args)
                            (a0) (a1) (a2) (a3) (a4) (a5) 
                            (a6) (a7) (a8) (a9) (a10), 
                            ?remaining-args)
         end select-method-by-count  }

end macro;



/// SELECT-METHOD-BY-COUNT
/// The main closure building macro.
//
// Example usage:
//
// select-method-by-count (required-args)
//         ((a0 :: type0) (a1 :: type1), #next nm, #rest rv)
//   idvm-invoke-code((vm-code, nm, env) (a0) (a1), rv, #f, process-keys: #t);
// end
//
//
// This usage tests the required args, expecting a value between 0 & 2.
// Depending on the value it will return a method with one of these 
// signatures: 
//
//     (#next nm, #rest rv)
//     (a0 :: type0, #next nm, #rest rv)
//     (a0 :: type0, a1 :: type1, #next nm, #rest rv)
//
// The body of the method will invoke the function/macro idvm-invoke-code
// with all of the arguments in the first set of parens, followed by an
// appropriate number of arguments from the next set of parens (according to
// the required arg count) followed by the remaining arguments.

define macro select-method-by-count

  { select-method-by-count (?count:expression) 
        ((?param:name :: ?type:expression), ?method-type:*)
      ?function:name((?first-args:*) (?mid-arg:expression), ?last-args:*);
    end  }
    => { if (?count == 1)
             method (?param :: ?type, ?method-type)
               ?function(?first-args, ?mid-arg, ?last-args)
             end method;
         elseif (?count == 0)
             method (?method-type)
               ?function(?first-args, ?last-args)
             end method;
         else
           idvm-invalid-method-error(?count)
         end if }

  { select-method-by-count (?count:expression) 
        (?params:* (?param:name :: ?type:expression), ?method-type:*)
      ?function:name((?first-args:*) ?mid-args:* (?mid-arg:expression), ?last-args:*);
    end }
    => { if (?count == static-size ?mid-args (?mid-arg) end)
             selection-method (?params (?param :: ?type), ?method-type)
               ?function((?first-args) ?mid-args (?mid-arg), ?last-args)
             end;
         else
           select-method-by-count (?count) (?params, ?method-type)
             ?function((?first-args) ?mid-args, ?last-args)
           end
         end if }

end macro;


/// SELECTION-METHOD
//
// This is plainly and simply a helper macro for SELECT-METHOD-BY-COUNT
// which rearranges parameters into comma separated lists from adjacent
// parenthetized values. 

define macro selection-method

  { selection-method (?parameters, ?method-type:*)
      ?function:name((?first-args:*) ?mid-args, ?last-args:*);
    end }
    => { method (?parameters, ?method-type)
           ?function(?first-args, ?mid-args, ?last-args)
         end }

parameters:
  { } 
    => { }
  { (?param:name :: ?type:expression) ...} 
    => { ?param :: ?type, ... }

mid-args:
  { }
    => { }
  { (?arg:expression) ...}
    => { ?arg, ... }

end macro;
 




/// IDVM-INVOKE-CODE
/// A mechanism for invoking IDVM code vectors in a regular way.
//
// The macro has a consistent (functional-style) syntax, and may be
// used to invoke an IDVM thread with either a fixed-size or dynamic-size 
// argument vector (depending on whether the stack-size: key is given).
// In addition, if the process-keys: keyword is given, then the macro will 
// arrange to call the process-keys instruction before calling the first
// instruction in the vector.


define macro idvm-invoke-code

  { idvm-invoke-code (?vm-code:expression, ?args-then-keys) }
    => { idvm-invoke-code-aux ?vm-code ?args-then-keys end  }
 
args-then-keys:
  { #rest ?the-keys}
    => { ; ?the-keys }
  {?arg1:expression, ... }
    => { , ?arg1 ... }
  
end macro;


// Use a helper macro to unpick the keyword options, after the arguments and
// keywords have been separated.

define macro idvm-invoke-code-aux

  { idvm-invoke-code-aux 
       ?vm-code:expression, ?result:expression, ?other-args:* ; 
       #rest ?invoker, #key ?stack-size, #all-keys
    end }
    => { ?invoker(?stack-size, ?vm-code, ?result, ?other-args) }
 
  { idvm-invoke-code-aux 
       ?vm-code:expression, ?result:expression, ?other-args:* ; 
       #rest ?invoker
    end }
    => { ?invoker (?vm-code, ?result, locals: ?other-args) }
 
invoker:
  { #key ?process-keys, ?stack-size}
    => { invoke-code-building-vectored-locals-processing-keys }
  { #key ?process-keys}
    => { invoke-code-processing-keys }
  { #key ?stack-size}
    => { invoke-code-building-vectored-locals }
  { }
    => { invoke-code }
  {arg1:expression, ... }
    => { ... }
  
end macro;



define macro invoke-code-building-vectored-locals

  { invoke-code-building-vectored-locals
      (?stack-size:expression, ?vm-code:expression, ?result:expression, ?more) }
    => { let vec = make(<simple-object-vector>, size: ?stack-size);
         initialize-locals-vector(vec, 0, ?more);
         invoke-code-with-vectored-locals(?vm-code, ?result, locals-vector: vec);  }

end macro;



define macro invoke-code-building-vectored-locals-processing-keys

  { invoke-code-building-vectored-locals-processing-keys
      (?stack-size:expression, ?vm-code:expression, ?result:expression, ?more) }
    => { let vec = make(<simple-object-vector>, size: ?stack-size);
         initialize-locals-vector(vec, 0, ?more);
         invoke-code-with-vectored-locals-processing-keys
             (?vm-code, ?result, locals-vector: vec);  }

end macro;

define macro initialize-locals-vector

  { initialize-locals-vector (?vec:expression, ?index:expression) }
    => { }

  { initialize-locals-vector (?vec:expression, ?index:expression,
                              ?this-val:expression, ?more) }
    => { ?vec[?index] := ?this-val;
         initialize-locals-vector(?vec, ?index + 1, ?more); }


end macro;





define macro static-size
  { static-size end }                                          => {0};
  { static-size (?e1:*) end }                                  => {1};
  { static-size (?e1:*) (?e2:*) end }                          => {2};
  { static-size (?e1:*) (?e2:*) (?e3:*) end }                  => {3};
  { static-size (?e1:*) (?e2:*) (?e3:*) (?e4:*) end }          => {4};
  { static-size (?e1:*) (?e2:*) (?e3:*) (?e4:*) (?e5:*) end }  => {5};

  { static-size (?e1:*) (?e2:*) (?e3:*) (?e4:*) (?e5:*) ?rest:* 
    end } 
    =>  {5 + static-size ?rest end};
end macro;

