module: netscape-plugin-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

// users of this module will have to subclass this
// and define a method for initialize thjat does what they like with
// saved-data, and mode
define class <plugin-instance> (<object>)
  slot plugin-window :: <NPWindow*>;
  slot win32-window :: <HWND>;
  slot plugin-mode :: <integer>, // NP_EMBED or NP_FULL
    required-init-keyword: mode:;
  slot old-wnd-proc :: <HACK-WNDPROC>;
  slot plugin-saved-data :: <NPSavedData*>,
    required-init-keyword: saved-data:;
  slot plugin-c-instance :: <NPP>,   // bogus
    required-init-keyword: c-instance:;
end;



/*
typedef struct _PluginInstance
{
    NPWindow*       fWindow;
    HWND            hWnd;
    uint16          fMode;
    FARPROC         lpfnOldWndProc;
    NPSavedData*    pSavedInstanceData;
    PluginInstance* pNext;
} PluginInstance;
*/


define method npp-initialize-internal () => (err :: <integer>);
  $NPERR-NO-ERROR
end;

define method npp-shutdown-internal () => ();
  // may need to shut down the dylan world here
end;


define macro with-idvm-restarts
  { with-idvm-restarts
     ?:body
    end }
    => { begin
	   let handler (<serious-condition>) = listener-condition-handler;
           ?body
	 end }
end;



// a plugin implementation will need to provide a method with matching
// parameters, and returning it's own subclass of <plugin-instance>.
// there should only be one method on this.
define open generic NPP-New (mime-type :: <byte-string>,
			     c-instance :: <NPP>,
			     mode :: <integer>,
			     parameters :: <list>,
			     saved :: <NPSavedData*>)
 => (instance :: <plugin-instance>);

define method NPP-New-internal (plugintype :: <NPMIMEType>,
				c-instance :: <NPP>,
				mode :: <integer>,
				argc :: <integer>,
				argn :: <C-char**>,
				argv :: <C-char**>,
				saved :: <NPSavedData*>)
 => (err :: <integer>);
  with-idvm-restarts
    if (null-pointer?(c-instance))
      $NPERR-INVALID-INSTANCE-ERROR
    else
      let dylan-instance
	= NPP-New(convert-c-string(plugintype),
		  c-instance,
		  mode,
		  convert-c-alist(argc, argn, argv),
		  saved);
      // install the new dylan plugin and place a "pointer" to it into
      // so that we can get to it given the instance from C later.
      // !@#$ This could be done in an initialize method...
      associate-plugin(c-instance, dylan-instance);
      // null entries for the window for this plugin instance
      dylan-instance.win32-window := null-pointer(<HWND>);
      dylan-instance.plugin-window := null-pointer(<NPWindow*>);
      $NPERR-NO-ERROR
    end;
  end;
end;


// a plugin will have to provide a method on this for it's own plugin class
define open generic NPP-destroy (p :: <plugin-instance>)
 => (saved :: <NPSavedData*>);


define method NPP-destroy-internal (c-instance :: <NPP>)
 => (err :: <integer>, saved :: <NPSavedData*>);
  with-idvm-restarts
    if(null-pointer?(c-instance))
      values($NPERR-INVALID-INSTANCE-ERROR,
	     null-pointer(<NPSavedData*>))
    else
      let dylan-instance = find-dylan-plugin(c-instance.pdata);
      if(dylan-instance)
	let hwnd = dylan-instance.win32-window;
	let old-proc = dylan-instance.old-wnd-proc;
	unless (null-wndproc?(old-proc))
	  SetWindowLongHACKHACK(hwnd,$GWL-WNDPROC, old-proc);
	end;
	let saved-data = NPP-destroy(dylan-instance);
	// no more need to find this dylan plugin instance
	remove-plugin(c-instance, dylan-instance);
	values($NPERR-NO-ERROR, saved-data);
      else
	values($NPERR-NO-ERROR, null-pointer(<NPSavedData*>));
      end if
    end if
  end;
end;


// a plugin must provide a method on this for it own plugin class
define open generic NPP-SetWindow (dylan-instance :: <plugin-instance>,
				   np-window :: <NPWindow*>)
 => (err :: <integer>);
  

define method NPP-SetWindow-internal (c-instance :: <NPP>,
				      plugin-window :: <NPWindow*>)
 => (err :: <integer>);
  with-idvm-restarts
  if(null-pointer?(c-instance))
    $NPERR-INVALID-INSTANCE-ERROR
  else
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      NPP-SetWindow(dylan-instance, plugin-window)
    else
      $NPERR-INVALID-INSTANCE-ERROR
    end if // (dylan-instance)
  end if // (null-pointer?)
  end
end;



// The plugin will need to define a method for this
define open generic NPP-NewStream
    (instance :: <plugin-instance>,
     mime-type :: <string>,
     stream :: <NPStream*>,
     seekable? :: <boolean>,
     stype :: <integer>)
 => (err :: <integer>, stype :: <integer>);



define method NPP-NewStream-internal
    (c-instance :: <NPP>,
     c-mime-type :: <NPMIMEType>,
     stream :: <NPStream*>,
     c-seekable :: <integer>,
     stype* :: <uint16*>)
 => (err :: <integer>);
  with-idvm-restarts
  // simulating input output parameters here
  let stype :: <integer> = pointer-value(stype*);
  if(null-pointer?(c-instance))
    values($NPERR-INVALID-INSTANCE-ERROR, stype)
  else
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      let mime-type = convert-c-string(c-mime-type);
      let (err, stype)
	= NPP-NewStream(dylan-instance,
			mime-type,
			stream,
			if(c-seekable = 0) #f else #t end,
			stype);
      pointer-value(stype*) := stype;
      err
    else
      $NPERR-INVALID-INSTANCE-ERROR
    end if // (dylan-instance)
  end if // (null-pointer?)
  end
end method;

// The plugin will need to define a method for this.
// The noop method need only return a large number to indicate it is ready to
// handle any amount of data that might come in.
define open generic NPP-WriteReady (instance :: <plugin-instance>,
				    np-stream :: <NPStream*>)
 => (ready-for :: <integer>);

  
define method NPP-WriteReady-internal (c-instance :: <NPP>,
				       plugin-stream :: <NPStream*>)
 => (ready-for :: <integer>);
  with-idvm-restarts
  if(null-pointer?(c-instance))
    0
  else
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      NPP-WriteReady(dylan-instance, plugin-stream)
    else
	0
    end if // (dylan-instance)
  end if // (null-pointer?)
  end
end method;

	
define open generic NPP-Write
    (instance :: <plugin-instance>,
     np-stream :: <NPStream*>,
     offset :: <integer>,
     len :: <integer>,
     buf :: <c-void*>)
 => (bytes-written :: <integer>);

define method NPP-Write-internal (c-instance :: <NPP>,
				  np-stream :: <NPStream*>,
				  offset :: <integer>,
				  len :: <integer>,
				  buf :: <c-void*>)
 => (bytes-written :: <integer>);
  with-idvm-restarts
  if(null-pointer?(c-instance))
    -1
  else
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      NPP-Write(dylan-instance, np-stream, offset, len, buf);
    else
      -2
    end if
  end if
  end
end method;
				  


define open generic NPP-DestroyStream
    (instance :: <plugin-instance>,
     np-stream :: <NPStream*>,
     reason :: <integer>)
 => (err :: <integer>);



define method NPP-DestroyStream-internal (c-instance :: <NPP>,
					  np-stream :: <NPStream*>,
					  reason :: <integer>)
 => (err :: <integer>);
  with-idvm-restarts  
  if(null-pointer?(c-instance))
    $NPERR-INVALID-INSTANCE-ERROR
  else
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      NPP-DestroyStream(dylan-instance, np-stream, reason);
    else
      $NPERR-INVALID-INSTANCE-ERROR
    end if
  end if
  end
end method;
  
define open generic NPP-StreamAsFile (instance :: <plugin-instance>,
				      np-stream :: <NPStream*>,
				      fname :: <string>)
  => ();

define method NPP-StreamAsFile-internal(c-instance :: <NPP>,
					np-stream :: <NPStream*>,
					fname :: <C-char*>)
 => ();
  with-idvm-restarts
  if(~null-pointer?(c-instance))
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      NPP-StreamAsFile(dylan-instance, np-stream, convert-c-string(fname));
    end if
  end if
  end
end method;
  

define open generic NPP-Print (instance :: <plugin-instance>,
			       print-info :: <NPPrint*>)
 => ();

define method NPP-Print-internal (c-instance :: <NPP>,
				  print-info :: <NPPrint*>)
 => ()
  with-idvm-restarts
  if(~null-pointer?(c-instance))
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      NPP-Print(dylan-instance, print-info);
    end if
  end if
  end
end method;
    

define open generic NPP-SetWindow (dylan-instance :: <plugin-instance>,
				   np-window :: <NPWindow*>)
 => (err :: <integer>);


define method NPP-SetWindow (dylan-instance :: <plugin-instance>,
			     np-window :: <NPWindow*>)
 => (err :: <integer>);
  // need to set up the call back for the window and update pointers
  // in dylan-instance to point to the handles to the window stuff
  if (~null-pointer?(np-window.window-handle)
	& null-pointer?(dylan-instance.win32-window))
    //
    // *Developers*: Before setting fWindow to point to the
    // new window, you may wish to compare the new window
    // info to the previous window (if any) to note window
    // size changes, etc.
    //
    dylan-instance.plugin-window := np-window;
    let hwin = np-window.window-handle;
    associate-window(dylan-instance, hwin);
    // install our window proc
    dylan-instance.old-wnd-proc
      := SetWindowLongPAB(hwin, $GWL-WNDPROC, c-func-window-handler);
    // associate the window with this instance so the wind-proc can
    // find it when it gets called
  elseif (dylan-instance.win32-window ~= np-window.window-handle)
    // remember the new window
    dylan-instance.plugin-window := np-window;
    // reset the window proc for the old window
    SetWindowLongHACKHACK(dylan-instance.win32-window,
		     $GWL-WNDPROC,
		     dylan-instance.old-wnd-proc);
    // remember the new window handle
    dylan-instance.win32-window := plugin-window.window-handle;
    if(~null-pointer?(dylan-instance.win32-window))
      dylan-instance.old-wnd-proc
	:= SetWindowLongPAB(dylan-instance.win32-window,
			    $GWL-WNDPROC,
			    c-func-window-handler);
    end if;
  else
    // window stayed the same, but something about it changed so
    // deal with that if we need to.
  end if;
  $NPERR-NO-ERROR
end method;


define method NPP-GetJavaClass-internal () => (p :: <C-void*>);
  // don't do Java for now
  null-pointer(<C-void*>)
end;


define c-callable-wrapper c-func-window-handler
    of plugin-window-handler-internal
  parameter hwnd :: <HWND>;
  parameter message :: <WORD>;
  parameter wparam :: <WORD>;
  parameter lparam :: <LONG>;
  result r :: <LONG>;
  c-name: "Dylan_plugin_windowproc", c-modifiers: "__stdcall";
end;

define method plugin-window-handler-internal (hwnd :: <HWND>,
					      message :: <integer>,
					      wparam :: <integer>,
					      lparam :: <integer>)
 => (r :: <integer>);
  with-idvm-restarts
  let dylan-instance = window-find-instance(hwnd);
  plugin-window-handler(dylan-instance, hwnd, message, wparam, lparam)
  end
end;

// if the plugin intends to do anything with the window then it will need to 
// define a method for this.
define open  generic plugin-window-handler
    (dylan-instance :: <plugin-instance>,
     hwnd :: <HWND>,
     message :: <integer>,
     wparam :: <integer>,
     lparam :: <integer>)
 => (r :: <integer>);


// default method
define method plugin-window-handler (dylan-instance :: <plugin-instance>,
				     hwnd :: <HWND>,
				     message :: <integer>,
				     wparam :: <integer>,
				     lparam :: <integer>)
 => (r :: <integer>);

  CallWindowProcPAB(dylan-instance.old-wnd-proc,
		    hwnd,
		    message,
		    wparam,
		    lparam)
end;




					  
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.

define method NPN-DestroyStream (dylan-instance :: <plugin-instance>,
				 np-stream :: <NPStream*>,
				 reason :: <integer>)
 => (err :: <integer>)
  let c-instance = plugin-c-instance(dylan-instance);
  NPN-DestroyStream-internal(c-instance, np-stream, reason)
end method;

define method NPN-GetUrl (dylan-instance :: <plugin-instance>,
			  url :: <string>,
			  target :: <string>)
 => (err :: <integer>);
  let c-instance = plugin-c-instance(dylan-instance);
  let c-url = convert-to-c-string(url);
  let c-target = convert-to-c-string(target);
  block ()
    NPN-GetUrl-internal(c-instance, c-url, c-target);
  cleanup
    cleanup-converted-c-string(c-url);
    cleanup-converted-c-string(c-target);
  end block;
end method;

define method NPN-GetUrlNotify (dylan-instance :: <plugin-instance>,
				url :: <string>,
				target :: <string>)
 => (err :: <integer>, dylan-notify-data :: <integer>);
  let c-instance = plugin-c-instance(dylan-instance);
  let c-url = convert-to-c-string(url);
  let c-target = convert-to-c-string(target);
  let (c-notify-data :: <NPNotify-Data>, dylan-notify-data :: <integer>)
    = new-notify-data();
  let err :: <integer> = 0;
  block ()
    err := NPN-GetUrlNotify-internal(c-instance, url, target, c-notify-data);
  cleanup
    cleanup-converted-c-string(c-url);
    cleanup-converted-c-string(c-target);
  end block;
  values(err, dylan-notify-data);
end;
    
  
define open generic NPP-UrlNotify (dylan-instance :: <example-plugin>,
				   dylan-url :: <string>,
				   reason :: <integer>,
				   dylan-notify-data :: <integer>)
 => ();

define method NPP-UrlNotify-internal (c-instance :: <NPP>,
				      url :: <c-char*>,
				      reason :: <integer>,
				      notify-data :: <NPNotify-data>)
 => ();
  with-idvm-restarts
  if(~null-pointer?(c-instance))
    let dylan-instance = find-dylan-plugin(c-instance.pdata);
    if (dylan-instance)
      let dylan-url = convert-c-string(url);
      let dylan-notify-data = get-dylan-notify-data(notify-data);
      NPP-UrlNotify(dylan-instance, dylan-url, reason, dylan-notify-data)
    end if;
  end if;
  end
end method;
	
	
define method NPN-NewStream (dylan-instance :: <plugin-instance>,
			     mime-type :: <string>,
			     target :: <string>)
 => (err :: <integer>,
     np-stream :: <NPStream*>);
  let c-instance = plugin-c-instance(dylan-instance);
  let c-mime-type = convert-to-c-string(mime-type);
  let c-target = convert-to-c-string(target);
  let err :: <integer> = 0;
  let np-stream = #f;
  block ()
    // blech -- no multiple-value-setq
    //let (xerr, xnp-stream)
    //  = 
    NPN-NewStream-internal(c-instance, c-mime-type, c-target);
    // err := xerr; 
    // np-stream := xnp-stream;
  cleanup
    cleanup-converted-c-string(c-mime-type);
    cleanup-converted-c-string(c-target);
  end block;
  // values(err, np-stream);
end method;
    
define method NPN-PostUrl (dylan-instance :: <plugin-instance>,
			   url :: <string>,
			   target :: <string>,
			   len :: <integer>,
			   buf :: <string>,
			   file :: <boolean>)
 => (err :: <integer>);
  let c-instance = plugin-c-instance(dylan-instance);
  let c-url = convert-to-c-string(url);
  let c-target = convert-to-c-string(target);
  let c-buf = convert-to-c-string(buf);
  block ()
    NPN-PostUrl-internal(c-instance, c-url, c-target, len,
			 c-buf, if (file) 1 else 0 end);
  cleanup
    cleanup-converted-c-string(c-url);
    cleanup-converted-c-string(c-target);    
    cleanup-converted-c-string(c-buf);
  end block;
end method;
  
			     
define method NPN-PostUrlNotify (dylan-instance :: <plugin-instance>,
				 url :: <string>,
				 target :: <string>,
				 len :: <integer>,
				 buf :: <string>,
				 file :: <boolean>)
 => (err :: <integer>, dylan-notify-data :: <integer>);
  let c-instance = plugin-c-instance(dylan-instance);
  let c-url = convert-to-c-string(url);
  let c-target = convert-to-c-string(target);
  let c-buf = convert-to-c-string(buf); 
  let (c-notify-data :: <NPNotify-Data>, dylan-notify-data :: <integer>)
    = new-notify-data();
  let err :: <integer> = 0;
  block ()
    err := NPN-PostUrlNotify-internal(c-instance, c-url, c-target, len,
				      c-buf,
				      if (file) 1 else 0 end,
				      c-notify-data);
  cleanup
    cleanup-converted-c-string(c-url);
    cleanup-converted-c-string(c-target);    
    cleanup-converted-c-string(c-buf);
  end block;
  values(err, dylan-notify-data);
end method;
			  
define method NPN-RequestRead (np-stream :: <NPStream*>, rangelist :: <list>)
 => (err :: <integer>);
  let c-rangelist = convert-to-c-rangelist(rangelist);
  block ()
    NPN-RequestRead-internal (np-stream, c-rangelist);
  cleanup
    cleanup-c-rangelist(c-rangelist);
  end block
end method;

define method NPN-Status (dylan-instance :: <plugin-instance>,
			  message :: <string>)
 => ();
  let c-instance = plugin-c-instance(dylan-instance);
  let c-message :: <C-char*> = convert-to-c-string(message);
  block ()
    NPN-Status-internal(c-instance, c-message);
  cleanup
    cleanup-converted-c-string(c-message);
  end block;
end;
    
define method NPN-UserAgent (dylan-instance :: <plugin-instance>)
 => (name :: <string>);
  let c-instance = plugin-c-instance(dylan-instance);
  let c-agent = NPN-UserAgent-internal(c-instance);
  convert-c-string(c-agent);
end method;
			     

define method NPN-Write (dylan-instance :: <plugin-instance>,
			 np-stream :: <NPStream*>,
			 offset :: <integer>,
			 len :: <integer>,
			 buf :: <string>)
 => (bytes-written :: <integer>);
  let c-instance = plugin-c-instance(dylan-instance);  
  let c-buf = convert-to-c-string(buf);
  block ()
    NPN-Write-internal(c-instance, np-stream, offset, len, c-buf);
  cleanup
    cleanup-converted-c-string(c-buf);
  end block;
end method;



//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.


define method convert-c-string (p :: <c-char*>)
 => (str :: <byte-string>);
  let len = strlen(p);
  let string = make(<byte-string>, size: len);
  for (i from 0 below len)
    string[i] := as(<byte-character>, pointer-value(p, index: i));
  end;
  string
end;


define c-function strlen
  parameter str :: <C-char*>;
  result l :: <C-unsigned-long>;
  c-name: "strlen";
end;


define method convert-c-alist (len :: <integer>,
			       names :: <C-char**>,
			       vals :: <c-char**>)
 => (alist :: <list>);
  let alist = #();
  for (i from 0 below len)
    let name = convert-c-string(pointer-value(names, index: i));
    let value = convert-c-string(pointer-value(vals, index: i));
    alist := pair(pair(name, value), alist);
  end for;
  alist
end;

define method initalize (instance :: <plugin-instance>,
			 #key c-instance)
  associate-plugin(c-instance, instance);
  instance.old-wnd-proc := null-wndproc(<C-HACK-WNDPROC>);
  // make sure we can get from c-instance to instance with find-dylan-plugin
end;
			 
define constant $plugin-table = make(<table>);
define variable *next-plugin-number* = 1;

define method associate-plugin (c-instance :: <NPP>,
				dylan-instance :: <plugin-instance>)
 => ();
  // !@#$ this number allocation  needs to be interlocked
  let plugin-number = *next-plugin-number*;
  *next-plugin-number* := *next-plugin-number* + 1;
  
  c-instance.pdata := plugin-number;
  $plugin-table[plugin-number] := dylan-instance;
  values();
end;


define method find-dylan-plugin (plugin-number) => (p :: <plugin-instance>);
  element($plugin-table, plugin-number, default: #f)
end;

define method remove-plugin (c-instance :: <NPP>,
			     dylan-instance :: <plugin-instance>)
 => ();
  let plugin-number = c-instance.pdata;
  remove-key!($plugin-table, plugin-number);
  plugin-c-instance(dylan-instance) := null-pointer(<NPP>);
  values()
end;


define method convert-to-c-string (target :: <string>) => (p :: <C-char*>);
  let sz = size(target);
  let p = make(<C-char*>, element-count: sz + 1);
  for (c in target,
       i from 0 below sz)
    pointer-value(p, index: i) := as(<integer>, c);
  end;
  // null terminate for C
  pointer-value(p, index: sz) := 0;
  p
end method;
    

define method cleanup-converted-c-string (c-url :: <c-char*>)
 => ();
  destroy(c-url);
  values()
end;


define variable *notify-data-table* = make(<table>);


define method new-notify-data ()
 => (c-notify-data :: <NPNotify-Data>, dylan-notify-data :: <integer>);
  // allocate a new integer for notify-data
  // linear search, but there shouldn't be more than a handful active
  // at any time.
  for (i from 0 below ash(1, 16),
	 until: element(*notify-data-table*, i, default: #f))
  finally
    element(*notify-data-table*, i) := #t;
    // allocate a <NPNotify-Data>
    let c-notify-data = make(<NPNotify-Data>);
    // set pointer-value of notify-data to new integer
    pointer-value(c-notify-data) := i;
    // return both notify-data and new integer
    values(c-notify-data, i)
  end for
end method;

define method get-dylan-notify-data (c-notify-data :: <NPNotify-data>)
 => (d :: <integer>);
  // get integer pointer-value of notify-data
  let dylan-notify-data = pointer-value(c-notify-data);
  // deallocate integer
  remove-key!(*notify-data-table*, dylan-notify-data);
  // deallocate notify-data
  destroy(c-notify-data);
  // return integer
  dylan-notify-data;
end;



define constant $hwnd-dylan-instance-table = make(<table>);

define method associate-window(dylan-instance :: <plugin-instance>,
			       hwin :: <HWND>) => ();
  
  unless(null-pointer?(dylan-instance.win32-window))
    remove-key!($hwnd-dylan-instance-table,
		pointer-address(dylan-instance.win32-window));
  end;    
  element($hwnd-dylan-instance-table, pointer-address(hwin)) := dylan-instance;
  dylan-instance.win32-window := hwin;
  values();
end;

define method window-find-instance (hwin :: <HWND>)
  => (dylan-instance :: <plugin-instance>);
  element($hwnd-dylan-instance-table, pointer-address(hwin), default: #f);
end;

// implement these range things in dylan as a sequence of <pair>
define constant range-offset = head;
define constant range-length = tail;


define method cleanup-c-rangelist (c-rangelist :: <NPByteRange*>) => ();
  //......
end method;

define method convert-to-c-rangelist (rangelist :: <sequence>)
 => (c-rangelist :: <NPByteRange*>);
  let previous-c-range = #f;
  let first-c-range = #f;
  for (range in rangelist)
    let current-c-range = make(<NPByteRange*>);
    current-c-range.offset := range.range-offset;
    current-c-range.length := range.range-length;
    if (previous-c-range)
      previous-c-range.next := current-c-range;
    else
      first-c-range := current-c-range;
    end;
    previous-c-range := current-c-range;
  finally
    if (previous-c-range)
      previous-c-range.next := null-pointer(<NPByteRange*>);
    else
      first-c-range := null-pointer(<NPByteRange*>);
    end;
  end;
  first-c-range
end method;

/*

// !@#$ temporary
define c-function CallWindowProc
  parameter proc :: <WNDPROC>;
  parameter hwnd :: <HWND>;
  parameter message :: <WORD>;
  parameter wparam :: <WORD>;
  parameter lparam :: <LONG>;
  result r :: <long>;
  c-name: "CallWindowProcA", c-modifiers: "__stdcall";
end;
*/



define C-function SetWindowLongPAB
  parameter hWnd       :: <HWND>;
  parameter nIndex     :: <C-int>;
  parameter dwNewLong  :: <LONG>;
  result value :: <C-HACK-WNDPROC>;
  c-name: "SetWindowLongHACK1";
end;

define C-function SetWindowLongHACKHACK
  parameter hWnd       :: <HWND>;
  parameter nIndex     :: <C-int>;
  parameter dwNewLong  :: <C-HACK-WNDPROC>;
  result value :: <C-HACK-WNDPROC>;
  c-name: "SetWindowLongHACK2";
end;



define C-function CallWindowProcPAB
  parameter lpPrevWndFunc :: <C-HACK-WNDPROC>;
  parameter hWnd       :: <HWND>;
  parameter Msg        :: <UINT>;
  parameter wParam     :: <WPARAM>;
  parameter lParam     :: <LPARAM>;
  result value :: <LRESULT>;
  c-name: "CallWindowProcA", c-modifiers: "__stdcall";
end;


define method null-wndproc? (x :: <HACK-WNDPROC>) => (b :: <boolean>);
  x = 0.0;
end;



define method null-wndproc (x == <C-HACK-WNDPROC>) => (b :: <HACK-WNDPROC>);
  0.0;
end;
