module:    harp-instructions
Synopsis:  Macro support for basic blocks.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define macro output-instruction
  { output-instruction(?backend:expression, ?op:expression, ?args:*) }
    => { begin
           let vars = ?backend.variables;
           let vec = vars.sv-instructions;
           let fp = vars.fp-instructions;
           let new-fp = fp + instruction-size;
           if (vars.in-back-end)
             output-error(?backend, ?op, ?args);
           end if;
           let vec :: <instructions-vector> =
	     ensure-room-in-vector(vec, new-fp);
           vars.sv-instructions := vec;
           vec[fp] := ?op;
           output-instruction-args(vec, fp, ?args);
           vars.fp-instructions := new-fp;
         end }
end macro;


define macro output-instruction-args 
  { output-instruction-args(?vec:expression, ?index:expression) }
    => { output-instruction-args(?vec, ?index, #f, #f, #f, #f) }

  { output-instruction-args(?vec:expression, ?index:expression, ?arg1:expression) }
    => { output-instruction-args(?vec, ?index, ?arg1, #f, #f, #f) }
  { output-instruction-args(?vec:expression, ?index:expression, ?arg1:expression, 
                            ?arg2:expression) }
    => { output-instruction-args(?vec, ?index, ?arg1, ?arg2, #f, #f) }

  { output-instruction-args(?vec:expression, ?index:expression, ?arg1:expression, 
                            ?arg2:expression, ?arg3:expression) }
    => { output-instruction-args(?vec, ?index, ?arg1, ?arg2, ?arg3, #f) }

  { output-instruction-args(?vec:expression, ?index:expression, ?arg1:expression, 
                            ?arg2:expression, ?arg3:expression, ?arg4:expression) }
    => { ?vec[?index + 1] := ?arg1;
         ?vec[?index + 2] := ?arg2;
         ?vec[?index + 3] := ?arg3;
         ?vec[?index + 4] := ?arg4 }

  { output-instruction-args(?vec:expression, ?index:expression, ?arg1:expression, 
                            ?arg2:expression, ?arg3:expression, ?more-args:*) }
    => { output-instruction-args(?vec, ?index, ?arg1, ?arg2, ?arg3, vector(?more-args)) }

end macro;
