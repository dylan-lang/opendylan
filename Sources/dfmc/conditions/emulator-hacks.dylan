Module:   dfmc-conditions-implementation
Author:   Paul Haahr
Synopsis: A little more hackery to make the emulator behave like Dylan.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Should offer abort and retry cleanups.

// Note: see native-specific version.

define macro with-simple-abort-retry-restart
  { with-simple-abort-retry-restart (?abort:expression, ?retry:expression) 
      ?:body
    end }
    => { local method _loop_ ()
           block () 
             ?body
           exception (r :: <simple-restart>, description: ?abort)
             #f
           exception (r :: <simple-restart>, description: ?retry)
             _loop_()
           end
         end;
         _loop_() }
end macro;

// <simple-condition> format string slots

// import-cl-functions(
//   simple-condition-format-string (as: condition-format-string),
//   simple-condition-format-arguments (as: condition-format-arguments)
// );


// Format strings in simple conditions have, in the emulator, a format
// string using Lisp's ~ directives, instead of Dylan's % ones.  This
// version of format uses those directives, for use with the default
// mechanism for printing program conditions.

import-cl-functions(
  format (as: lisp/format)
);

define constant lisp-format-to-dylan-stream
  = method
	(stream :: <stream>, format-string :: <string>, #rest arguments) => ()
      format(stream, "%s", apply(lisp/format, #(), format-string, arguments))
    end method;

// A hook to start the debugger.

import-cl-functions(
  invoke-debugger
);


// A not-quite-real implementation of do-handlers for the emulator,
// which only picks up restarts, and only those restarts installed by
// the with-program-restarts macro.  Since the program conditions
// system uses do-handlers only to look for its own restarts, this is
// sufficient for our purposes.

define thread variable *program-restarts* = #();

define class <pseudo-restart> (<object>)
  slot restart-type :: subclass(<restart>), required-init-keyword: type:;
  slot restart-function :: <function>, required-init-keyword: function:;
  slot restart-test :: <function> = always(#t), init-keyword: test:;
  slot restart-init-arguments = #(), init-keyword: init-arguments:;
end class <pseudo-restart>;

define macro with-program-restarts

  { with-program-restarts ?:body ?catchers end }
    => { block (exit-restart-block) 
           let _body = method () ?body end;
           ?catchers
         end }

 // This only matches a limited form of the exception clause.
 // Since this macro is not for public use, and it only invoked from
 // condition-block, it hopefully won't be a problem.
 catchers:
  { }
    => { _body() }
  { ... restart (?type:expression, ?options:*) ?:body }
    => { dynamic-bind (*program-restarts* 
			 = pair
			     (make(<pseudo-restart>,
				   type: ?type,
				   function: 
				     method (_condition_ :: ?type, next-handler)
				       exit-restart-block(?body)
				     end,
				   ?options),
			      *program-restarts*))
	   ...
         end }
  { ... restart (?:name :: ?type:expression, ?options:*) ?:body }
    => { dynamic-bind (*program-restarts*
			 = pair
                             (make(<pseudo-restart>,
				   type: ?type,
				   function: 
				     method (_condition_ :: ?type, next-handler)
				       exit-restart-block(?body)
				     end,
				   ?options),
			      *program-restarts*))
           ...
         end }

end macro with-program-restarts;

define method do-handlers (function :: <function>) => (false :: singleton(#f))
  for (restart in *program-restarts*)
    function(restart.restart-type,
	     restart.restart-test,
	     restart.restart-function,
	     restart.restart-init-arguments)
  end for;
  #f
end method do-handlers;
