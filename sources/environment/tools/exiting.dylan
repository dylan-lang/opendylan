Module:    environment-tools
Synopsis:  Environment tools
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//--- cpage: 1997.09.23 We should probably add a "start-application" function
//           to environment-framework for symmetry with frame-exit-application.

//--- cpage: 1997.09.23 We should move these default methods into
//           environment-framework.

define generic frame-do-exitable-frames
    (frame :: <frame>, action :: <function>) => ();

// Default method
define method frame-do-exitable-frames
    (frame :: <frame>, action :: <function>)
 => ()
   do-frames(action);
end method frame-do-exitable-frames;

define variable *application-exiting?* :: <boolean> = #f;

// Return the owned modal dialog of a frame, if any
define method frame-owned-modal-dialog
    (frame  :: <frame>)
 => (dialog :: false-or(<frame>))
  block (return)
    do(method (frame :: <frame>) => ()
	 when (frame.frame-mode ~== #"modeless")
	   return (frame);
	 end;
       end method,
       frame-owned-frames(frame));
    #f
  end
end method frame-owned-modal-dialog;

// Default method
define sideways method frame-exit-application
    (frame :: <frame>) => (exit? :: <boolean>)
  unless (*application-exiting?*)
    *application-exiting?* := #t;
    // Exit frames until we've asked all of them, or one stops the exit.
    *application-exiting?*
      := block (return)
           frame-do-exitable-frames
	     (frame,
	      method (frame-to-exit :: <frame>) => ()
                // Before asking a frame whether it can exit, make sure it isn't
                // a modal dialog, a modal dialog owner, or disabled (which
                // usually means it owns a native modal dialog that doesn't have
                // a DUIM mirror).
		let owned-dialog          = frame-owned-modal-dialog(frame-to-exit);
		let modal?   :: <boolean> = frame-to-exit.frame-mode ~== #"modeless";
                let enabled? :: <boolean> = frame-to-exit.frame-enabled?;
                let dialog?  :: <boolean> = owned-dialog ~== #f | modal? | ~enabled?;
		if (~dialog? & frame-can-exit?(frame-to-exit))
		  if (instance?(frame-to-exit, <environment-fixed-project-frame>))
		    frame-to-exit.frame-exiting? := #t
		  end;
		  exit-frame(frame-to-exit);
                else
                  // Clear exiting? flags since exiting has been stopped
		  frame-do-exitable-frames
		    (frame,
                     method (frame-to-stop-exiting :: <frame>) => ()
                       when (instance?(frame-to-stop-exiting,
                                       <environment-fixed-project-frame>))
                         frame-to-stop-exiting.frame-exiting? := #f
                       end
		     end);
                  // Activate the dialog or owner frame
		  when (dialog?)
		    // Raise a frame so the user can see what's stopping the exit.
                    // If the exit-frame is disabled, beep in the original frame;
                    // restoring, raising and beeping won't occur until the
                    // exit-frame becomes enabled, which is too late to be useful.
                    let frame-to-activate :: <frame>
                      = case
                          owned-dialog ~== #f => owned-dialog;
                          ~enabled?           => frame;
                          otherwise           => frame-to-exit;
                        end;
                    call-in-frame(frame-to-activate,
                                  method (_frame :: <frame>) => ()
                                    deiconify-frame(_frame);
                                    raise-frame(_frame);
                                    beep(_frame);
                                  end method,
                                  frame-to-activate);
		  end when;
		  return(#f);
		end;
	      end method);
           #t
         end block;
  end unless;
  *application-exiting?*
end method frame-exit-application;


/// Exiting the environment

// Do environment frames in a certain order
define method frame-do-exitable-frames
    (frame :: <environment-frame>, action :: <function>)
 => ()
  let frames = collect-frames();
  // Do editor frames.
  do(method (frame :: <frame>) => ()
       //--- cpage: 1997.09.22 Apparently, <environment-editor> isn't visible in
       //           this library, so we'll have to define editor frames in terms
       //           of a negative space.
       ~instance?(frame, <environment-fixed-project-frame>)
         & ~instance?(frame, <environment-primary-frame>)
         & action(frame)
     end,
     frames);
  // Do project frames.
  do(method (frame :: <frame>) => ()
       instance?(frame, <environment-fixed-project-frame>)
         & action(frame);
     end,
     frames);
  // Do the primary frame last.
  do(method (frame :: <frame>) => ()
       instance?(frame, <environment-primary-frame>)
       & action(frame);
     end,
     frames);
end method frame-do-exitable-frames;

define method collect-frames ()
  let frames = make(<stretchy-vector>);
  do-frames(method (frame :: <frame>)
	      frames := add!(frames, frame);
	    end method);
  frames
end method collect-frames;
