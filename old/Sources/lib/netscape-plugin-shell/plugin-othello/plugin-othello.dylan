module: plugin-othello
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

/// a example implemetation of a dylan implemented plugin

define variable *othello-window* = #f;


define class <othello-plugin> (<plugin-instance>)
  slot current-frame :: <integer> = 0;
end;

// the saved data should be flat (no embedded pointers to other stuff)
// and it should be allocated using NPP-MemAlloc
define c-struct <othello-plugin-saved-data>
end;

define c-pointer-type <othello-plugin-saved-data*>
  => <othello-plugin-saved-data>;


define method NPP-New (mime-type :: <byte-string>,
		       c-instance :: <NPP>,
		       mode :: <integer>,
		       parameters :: <list>,
		       saved :: <NPSavedData*>)
 => (instance :: <othello-plugin>);
  // could really do some more stuff with the parameters
  // and saved data
  // Note that c-instance: is a required-init-keyword.
  ignore(mime-type);
  ignore(parameters);
  make(<othello-plugin>, c-instance: c-instance, mode: mode, saved-data: saved)
end;

define method NPP-destroy (p :: <othello-plugin>) => (saved :: <NPSavedData*>);
  ignore(p);
  // this gets called just as we start to destroy this instance of 
  // the plugin
  // You can create some saved data here for when the plugin gets invoked
  // on the same url again.
  // do not do any graphic operations because the window is now invalid.
  // illustrate how to create the saved data:
  if(*othello-window*)
    destroy-frame(*othello-window*);
    *othello-window* := #f;
  end;
  let saved-data-package = as(<NPSavedData*>,
			      NPN-MemAlloc(size-of(<NPSavedData>)));
  let saved-data = as(<othello-plugin-saved-data*>,
		      NPN-MemAlloc(size-of(<othello-plugin-saved-data>)));
  saved-data-package.SavedData-buf := saved-data;
  saved-data-package.SavedData-len := size-of(<othello-plugin-saved-data>);
  saved-data-package
end;
  
define method NPP-SetWindow (dylan-instance :: <othello-plugin>,
			     np-window :: <NPWindow*>)
 => (err :: <integer>);
    //
    // *Developers*: Before setting fWindow to point to the
    // new window, you may wish to compare the new window
    // info to the previous window (if any) to note window
    // size changes, etc.
    //
  // if the window has not changed, but the dimensions did change then
  // this is the place to do any updates necessary.
  // ** you need to call next-method so that we can connect your
  // window function (plugin-window-handler) to the window.
  if (null-pointer?(np-window.window-handle))
    if(*othello-window*)
      destroy-frame(*othello-window*);
      *othello-window* := #f;
    end;
  elseif (*othello-window*)
    // signal an error?
  else 
    let f = attach-othello(window-handle(np-window),
			    0, 0,
			    npwin-width(np-window),
			    npwin-height(np-window));
    *othello-window* := f;
  end;
  next-method();
  // The default method here sets up the window handler to call
  // plugin-window-handler if the window was not NULL
end;

define method plugin-window-handler (dylan-instance :: <othello-plugin>,
				     hwnd :: <HWND>,
				     message :: <integer>,
				     wparam :: <integer>,
				     lparam :: <integer>)
 => (r :: <integer>);
  
  /*
  select (message)
    $WM-LBUTTONDOWN => cerror(make(<serious-condition>));
  end;
  
  NPN-Status(dylan-instance,
	     concatenate(hexmsg("msg: ", message),
			 hexmsg(" wparam: ", wparam),
			 hexmsg(" lparam: ", lparam)));
  */
  NPN-Status(dylan-instance, "Othello");

  
  // NPN-Status(dylan-instance, hexmsg("Got message number ", message));
  next-method();
end;
  
define method hexmsg (str :: <string>, n :: <integer>)
 => (s :: <string>);
  local method hexit (i :: <integer>, l :: <list>) => (l :: <string>);
	  let (quotient :: <integer>, remainder :: <integer>)
	    = truncate/(i, #x10);
	  if(zero?(quotient))
	    as(<string>,
	       map(method (i :: <integer>) "0123456789ABCDEF"[i] end,
		   pair(remainder,l)))
	    else
	     hexit(quotient, pair(remainder, l))
	  end if;
	end method hexit;
  concatenate(str, "0x0", hexit(n, #()));
end;



define method NPP-NewStream (instance :: <othello-plugin>,
			     mime-type :: <string>,
			     stream :: <NPStream*>,
			     seekable? :: <boolean>,
			     stype :: <integer>)
 => (err :: <integer>, stype :: <integer>);
  ignore(instance);
  ignore(mime-type);
  ignore(stream);
  ignore(seekable?);
  // if your plugin must operate file based, you may wish to do this:
  //    values ($NPERR-NO-ERROR, $NP-ASFILE)
  // or in netscape 3.0:
  //    values ($NPERR-NO-ERROR, $NP-ASFILEONLY)
  // remember, though, that use of NP_ASFILE is strongly discouraged;
  // your plugin should attempt to work with data as it comes in on
  // the stream if at all possible
  values($NPERR-NO-ERROR, stype)
end method;
  

// (From npshell.cpp)
// *Developers*: 
// These next 2 functions are directly relevant in a plug-in which handles the
// data in a streaming manner.  If you want zero bytes because no buffer space
// is YET available, return 0.  As long as the stream has not been written
// to the plugin, Navigator will continue trying to send bytes.  If the plugin
// doesn't want them, just return some large number from NPP_WriteReady(), and
// ignore them in NPP_Write().  For a NP_ASFILE stream, they are still called
// but can safely be ignored using this strategy.
//


define constant $STREAMBUFSIZE = #x0FFFFFFF;

define method NPP-WriteReady (instance :: <othello-plugin>,
			      np-stream :: <NPStream*>)
 => (ready-for :: <integer>);
  ignore(instance);
  ignore(np-stream);

  $STREAMBUFSIZE
end;


define method NPP-Write (instance :: <othello-plugin>, np-stream :: <NPStream*>,
			 offset :: <integer>, len :: <integer>,
			 buf :: <c-void*>)
 => (bytes-written :: <integer>);
  ignore(instance);
  ignore(np-stream);
  ignore(offset);
  ignore(buf);
  // *** Do something interesting with the data in the stream here.
  len
    // The number of bytes accepted.  Return a
    // negative number here if, e.g., there was an error
    // during plugin operation and you want to abort the
    // stream
end;

/*
 * The reason the stream was destroyed is indicated by the by the
 * parameter reason. The most common reason code is NPRES_DONE,
 * indicating simply that the stream completed normally because all data
 * was sent to the instance. Other possible reason codes are
 * NPRES_USER_BREAK, indicating that the user canceled the stream by
 * clicking the "Stop" button, and NPRES_NETWORK_ERR, indicating that the
 * stream failed due to network problems. The complete list of reason
 * codes is found in npapi.h.
 */
define method NPP-DestroyStream (instance :: <othello-plugin>,
				 np-stream :: <NPStream*>,
				 reason :: <integer>)
 => (err :: <integer>);
  ignore(instance);
  ignore(np-stream);
  ignore(reason);
  // *** if the reason was anything other than $NPRES-DONE we may want to
  //  do something to indicate that.
  $NPERR-NO-ERROR
end;

define method NPP-StreamAsFile (instance :: <othello-plugin>,
				np-stream :: <NPStream*>,
				fname :: <string>)
  => ();
  ignore(instance);
  ignore(np-stream);
  ignore(fname);
  // If we asked for the stream as a file then here it is in fname in
  // the local file system
  // **** operate on the data in the file as if it came from np-stream.
  values()
end;

define method NPP-Print (instance :: <othello-plugin>,
			 print-info :: <NPPrint*>)
 => ();
  ignore(instance);

  if(print-info.npprint-mode = $NP-FULL)
    //
    // *Developers*: If your plugin would like to take over
    // printing completely when it is in full-screen mode,
    // set printInfo->pluginPrinted to TRUE and print your
    // plugin as you see fit.  If your plugin wants Netscape
    // to handle printing in this case, set printInfo->pluginPrinted
    // to FALSE (the default) and do nothing.  If you do want
    // to handle printing yourself, printOne is true if the
    // print button (as opposed to the print menu) was clicked.
    // On the Macintosh, platformPrint is a THPrint; on Windows,
    // platformPrint is a structure (defined in npapi.h) containing
    // the printer name, port, etc.
    //

            //
            // *Developers*: If your plugin is embedded, or is full-screen
            // but you returned false in pluginPrinted above, NPP_Print
            // will be called with mode == NP_EMBED.  The NPWindow
            // in the printInfo gives the location and dimensions of
            // the embedded plugin on the printed page.  On the Macintosh,
            // platformPrint is the printer port; on Windows, platformPrint
            // is the handle to the printing device context.
            //
    // **** do something here for full screen
  else
    // **** do something here for embedded thing
  end if;
  values()
end;


define method NPP-UrlNotify (dylan-instance :: <othello-plugin>,
			     dylan-url :: <string>,
			     reason :: <integer>,
			     dylan-notify-data :: <integer>)
 => ();
  ignore(dylan-instance);
  ignore(dylan-url);
  ignore(reason);
  ignore(dylan-notify-data);

  // *** Do what you want to indicate that this URL transaction is over
  // if the reason is not $NPRES-DONE then we may want to do
  // something special
  // We can use dylan-notify-data here to find out more about the
  // transaction that is finishing here.
  values();
end;

