module: netscape-plugin-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

// for vmtether
define macro c-pointer-type-definer
  { define c-pointer-type ?new-name:name => ?ref-type:expression }
    => { define constant ?new-name = ?ref-type.pointer-type }
end;

define c-pointer-type <C-void*> => <C-void>;
define c-pointer-type <C-char*> => <C-char>;
define c-pointer-type <C-char**> => <C-char*>;
define c-pointer-type <c-signed-short*> => <c-signed-short>;
define c-pointer-type <c-int*> => <C-int>;

define constant <uint32> = <c-unsigned-int>; //!@#$ should be unsigned 32bit
define constant <int32> = <c-signed-int>; //!@#$ should be signed 32bit
define constant <uint16> = <c-unsigned-short>; //!@#$ should be unsigned 16bit
define constant <int16> = <c-signed-short>; //!@#$ should be signed 16bit
define constant <int16*> = <c-signed-short*>; //!@#$ should be signed 16bit


define constant <NPBool> = <C-unsigned-char>;  // !@#$ does this work?
define constant <NPError> = <int16>;
define constant <NPReason> = <int16>;
define constant <NPMIMEType> = <C-char*>;

define c-pointer-type <NPMIMEType*> => <NPMIMEType>;
define c-pointer-type <uint16*> => <uint16>;

define constant <jref> = <C-void*>;

define constant <NPNotify-Data> = <C-signed-short*>;


/*  From the netscape doc for NPP
 *
 * The type NPP points to a structure representing a single instance of
 * your plug-in. The structure is created by Netscape and the NPP pointer
 * to the structure is passed to your plug-in initially as a parameter to
 * NPP_New. Thereafter, the NPP pointer is used in almost all API calls
 * to identify the instance on which the API should operate. Netscape
 * informs the plug-in of the deletion of the instance via NPP_Destroy;
 * after this call returns the NPP pointer is no longer valid.  
 * 
 * The structure pointed to by a NPP contains two fields: pdata and
 * ndata. pdata is a plug-in private value that your plug-in can use to
 * store a pointer to an internal data structure associated with the
 * instance; Netscape does not modify this value. ndata is a Netscape
 * private value that should not be modified by your plug-in. 
 */

define c-struct <NPP_t>
  slot pdata :: <c-unsigned-long>; // plugin private data
  slot ndata :: <c-void*>;	// netscape private data
end;

define c-pointer-type <NPP> => <NPP_t>;


/*
 * 
 * NPStream
 * 
 * A NPStream object represents a stream of data either produced by
 * Netscape and consumed by the plug-in, or produced by the plug-in and
 * consumed by Netscape. For streams produced by Netscape, the NPStream
 * is created by Netscape and passed to the plug-in initially as a
 * parameter to NPP_NewStream. Thereafter, a pointer to the NPStream is
 * used in all API calls that operate on the stream (such as
 * NPP_WriteReady and NPP_Write). Netscape informs the plug-in of the
 * deletion of the stream via NPP_DestroyStream; after this call returns
 * the NPStream object is no longer valid.   
 * 
 * For streams produced by the plug-in, the NPStream is created by
 * Netscape and returned as an output parameter when the plug-in calls
 * NPN_NewStream. Thereafter, the plug-in must pass a pointer to the
 * NPStream to all API calls that operate on the stream, such as
 * NPN_Write and NPN_DestroyStream.  
 * 
 * The pdata field of the NPStream is a plug-in private value that your
 * plug-in can use to store a pointer to an internal data structure
 * associated with the stream; Netscape does not modify this value. ndata
 * is a Netscape private value that should not be modified by your
 * plug-in. url is a string containing the URL that the data in the
 * stream is read from or written to. end is the offset in bytes of the
 * end of the stream (equivalent to the length of the stream in
 * bytes). (Note that end may be zero for streams of unknown length, such
 * as streams returned from older FTP servers or generated "on the fly"
 * by CGI scripts.) lastmodified is the time the data in the URL was last
 * modified (if applicable), measured in seconds since 12:00 midnight
 * GMT, January 1, 1970.  
 * 
 * Syntax
 * 
 * typedef struct _NPStream
 * {
 *     void*       pdata;          /* plug-in private data */
 *     void*       ndata;          /* netscape private data */
 *     const char* url;
 *     uint32      end;
 *     uint32      lastmodified;
 * } NPStream;
 * 
 * See also
 * 
 * NPP_NewStream
 * NPP_DestroyStream
 * NPN_NewStream
 * NPN_DestroyStream 
 * 
 */


define c-struct <NPStream>
  slot NPStream-pdata :: <c-void*>;	// plug-in private data
  slot NPStream-ndata :: <c-void*>;	// netscape private data
  slot NPStream-url :: <c-char*>;
  slot NPStream-end :: <uint32>;	
  slot NPStream-lastmodified :: <uint32>;
end;


define c-pointer-type <NPStream*> => <NPStream>;
define c-pointer-type <NPStream**> => <NPStream*>;



/*
 * NPByteRange
 * 
 * A NPByteRange object represents a request of a particular range of
 * bytes from a stream. Multiple NPByteRange objects can be linked
 * together into a list to represent a set of discontiguous byte ranges.
 * The only API call that uses the NPByteRange type is NPN_RequestRead.
 * 
 * The offset field of the NPByteRange indicates the offset in bytes of
 * the requested range, from the beginning of the stream if the offset is
 * positive, or from the end of the stream if the offset is negative.
 * length indicates the number of bytes to fetch from the specified
 * offset. next points to the next NPByteRange request in the list of
 * requests, or NULL if this is the last request.
 * 
 * Syntax
 * 
 * typedef struct _NPByteRange
 * {
 *     int32       offset;         /* negative offset means from the end */
 *     uint32      length;
 *     struct _NPByteRange* next;
 * } NPByteRange;
 * 
 * See also
 * 
 * NPN_RequestRead 
 * 
 */

define c-struct <NPByteRange>
  slot offset :: <int32>;	// negative offset means from the end 
  slot length :: <uint32>;
  slot next :: <NPByteRange*>;
end;

define c-pointer-type <NPByteRange*> => <NPByteRange>;


/* 
 * 
 * NPSavedData
 * 
 * The NPSavedData object contains a block of per-instance information
 * saved by Netscape after the deletion of the instance, and possibly
 * returned to the plug-in if the instance is revisited by the user.
 * When the plug-in's NPP_Destroy function is called, it optionally may
 * allocate a NPSavedData object using the NPN_MemAlloc function, fill in
 * the fields, and return it to Netscape as an output parameter.  See
 * NPP_Destroy for a code example of the use of NPSavedData.
 * 
 * The buf field holds a pointer to a memory buffer allocated by the
 * plug-in with NPN_MemAlloc. The memory buffer can be any reasonable
 * size; its contents are private to the plug-in and will not be modified
 * by Netscape. len should be set by the plug-in to the length in bytes
 * of the buffer pointed to by buf.
 * 
 * Syntax
 * 
 * typedef struct _NPSavedData
 * {
 *     int32       len;
 *     void*       buf;
 * } NPSavedData;
 * 
 * See also
 * 
 * NPP_New
 * NPP_Destroy 
 * 
 */

define c-struct <NPSavedData>
  slot SavedData-len :: <int32>;
  slot SavedData-buf :: <c-void*>;
end;

define c-pointer-type <NPSavedData*> => <NPSavedData>;

define c-pointer-type <NPSavedData**> => <NPSavedData*>;


/*
 * NPSetWindowCallbackStruct
 * 
 * NPSetWindowCallbackStruct is used on UNIX only. 
 * 
 * On UNIX, the ws_info field of an NPWindow points to an object of type
 * NPSetWindowCallbackStruct, allocated by Netscape. The structure is
 * valid for the lifetime of the NPWindow (until NPP_SetWindow is called
 * again or the instance is destroyed). The fields display, visual,
 * colormap, and depth contain the standard X Toolkit attributes of the
 * top-level shell window in the Netscape window hierarchy.
 * 
 * Syntax
 * 
 * typedef struct
 * {
 *     int32               type;
 *     Display*            display;
 *     Visual*             visual;
 *     Colormap            colormap;
 *     unsigned int        depth;
 * } NPSetWindowCallbackStruct;
 * 
 * See also
 * 
 * NPP_SetWindow
 * NPWindow 
 * 
 */

/*
 * NPWindow
 * 
 * The NPWindow structure contains information about the native window,
 * or portion of the native window, into which the plug-in instance can
 * draw. A pointer to an NPWindow (allocated by Netscape) is passed to
 * the plug-in via NPP_SetWindow and is valid until NPP_SetWindow is
 * called again or the instance is destroyed.
 * 
 * The window field is a handle to a native window element in the
 * Netscape window hierarchy on Windows and UNIX. On the Macintosh,
 * window is a pointer to a NP_Port. The position and size of this
 * plug-in area (native window element on Window and UNIX, rectangle
 * within a native window on the Mac) are specified by x, y, height, and
 * width. Note that these values should not be modified by the plug-in.
 * 
 * On the Macintosh, clipRect is the rectangle in port coordinates to
 * which the plug-in should clip its drawing. Clipping to the clipRect
 * prevents the plug-in from overwriting the status bar, scroll bars,
 * etc. when partially scrolled off the screen.
 * 
 * On UNIX, ws_info points to a NPSetWindowCallbackStruct. 
 * 
 * Syntax
 * 
 * typedef struct _NPWindow 
 * {
 *     void*       window;         /* Platform specific window handle */
 *     uint32      x;              /* Position of top left corner relative */
 *     uint32      y;              /*   to a netscape page. */
 *     uint32      width;          /* Maximum window size */
 *     uint32      height;
 *     NPRect      clipRect;       /* Clipping rectangle in port coordinates */
 *                                 /* Used by Mac only. */
 * #ifdef XP_UNIX
 *     void *      ws_info;        /* Platform-dependent additional data */
 * #endif /* XP_UNIX */
 * } NPWindow;
 * 
 * See also
 * 
 * NPP_SetWindow
 * NP_Port
 * NPSetWindowCallbackStruct 
 * 
 *
 *
 * typedef struct _NPRect
 * {
 *     uint16	top;
 *     uint16	left;
 *     uint16	bottom;
 *     uint16	right;
 * } NPRect;
 * 
 */

define c-struct <NPrect>
  slot nprect-top :: <uint16>;
  slot nprect-left :: <uint16>;
  slot nprect-bottom :: <uint16>;
  slot nprect-right :: <uint16>;
end;

define c-pointer-type <NPrect*> => <NPrect>;


define c-struct <NPWindow>
  slot window-handle :: <HWND>;
  slot npwin-x :: <uint32>;
  slot npwin-y :: <uint32>;
  slot npwin-width :: <uint32>;
  slot npwin-height :: <uint32>;
  slot npwin-cliprect :: <NPrect>;  // not use don windows
  // slot ws_info :: <c-void*>;
end;

define c-pointer-type <NPWindow*> => <NPWindow>;


/*
 * 
 * NPFullPrint
 * 
 * When printing in full-page mode, NPFullPrint is a substructure of
 * NPPrint. If your plug-in wants to take complete control of the
 * printing process, print the page and set the field pluginPrinted to
 * TRUE before returning. If your plug-in wants to simply render its area
 * of the page, set pluginPrinted to FALSE and return immediately.
 * 
 * printOne is TRUE if the plug-in should avoid displaying print dialogs
 * and instead print a single copy of the page to the default
 * printer. platformPrint points to platform-specific printing
 * information; on the Macintosh, platformPrint is a THPrint.
 * 
 * Syntax
 * 
 * typedef struct _NPFullPrint
 * {
 *     NPBool      pluginPrinted;  /* Set TRUE if plugin handled fullscreen */
 *                                 /*      printing */
 *     NPBool      printOne;       /* TRUE if plugin should print one copy  */
 *                                 /*      to default printer */
 *     void*       platformPrint;  /* Platform-specific printing info */
 * } NPFullPrint;
 * 
 * See also
 * 
 * NPP_Print
 * NPPrint 
 * 
 */

define c-struct <NPFullPrint>
//  slot pluginprinted :: <NPBool>;
//  slot printOne :: <NPBool>;
//  slot platformPrint :: <c-void*>;
end;

define c-pointer-type <NPFullPrint*> => <NPFullPrint>;




/*
 * 
 * NPEmbedPrint
 * 
 * When printing in embedded mode, NPEmbedPrint is a substructure of
 * NPPrint. The field window is the NPWindow the plug-in should use for
 * printing. platformPrint points to additional platform-specific
 * printing info; on the Macintosh, platformPrint is a THPrint.
 * 
 * Syntax
 * 
 * typedef struct _NPEmbedPrint
 * {
 *     NPWindow    window;
 *     void*       platformPrint;  /* Platform-specific printing info */
 * } NPEmbedPrint;
 * 
 * See also
 * 
 * NPP_Print
 * NPPrint 
 * 
 */

define c-struct <NPEmbedPrint>
  slot window :: <NPWindow>;
  slot platformPrint :: <c-void*>;
  // pointer-type-name: <NPEmbedPrint*>
end;

// for vmtether
define c-pointer-type <NPEmbedPrint*> => <NPEmbedPrint>;

/*
 * 
 * NPPrint
 * 
 * The NPPrint structure contains information the plug-in may need to
 * print itself in full-page or embedded modes. A pointer to a NPPrint
 * object (previously allocated by Netscape) is passed to the plug-in as
 * a parameter to NPP_Print. The pointer and fields within the structure
 * are valid only for the duration of the NPP_Print call.
 * 
 * If the mode field is NP_FULL, the plug-in may optionally print in
 * full-page mode, and the fullPrint field is valid (see NPFullPrint and
 * NPP_Print). If the mode field is NP_EMBED, the plug-in should print in
 * embedded mode, and the embedPrint field is valid (see NPEmbedPrint).
 * 
 * Syntax
 * 
 * typedef struct _NPPrint
 * {
 *     uint16      mode;           /* NP_FULL or NP_EMBED */
 *     union
 *     {
 *         NPFullPrint fullPrint;  /* if mode is NP_FULL */
 *         NPEmbedPrint embedPrint;/* if mode is NP_EMBED */
 *     } print;
 * } NPPrint;
 * 
 * See also
 * 
 * NPP_Print
 * NPFullPrint
 * NPEmbedPrint 
 * 
 */

/// 
define c-union <print-slot-union>
  //slot fullPrint :: <NPFullPrint>;
  // slot embedPrint :: <NPEmbedPrint>;
end;



define c-struct <NPPrint>
  slot npprint-mode :: <uint16>;
  slot npprint-print :: <print-slot-union>;
end;

define c-pointer-type <NPPrint*> => <NPPrint>;



/*
 * 
 * NP_Port
 * 
 * NP_Port is used on the Macintosh only. 
 * 
 * On the Macintosh, the window field of an NPWindow points to an object
 * of type NP_Port, allocated by Netscape. The structure is valid for the
 * lifetime of the NPWindow (until NPP_SetWindow is called again or the
 * instance is destroyed).
 * 
 * The fields portx and porty determine the top-left corner of the
 * plug-in rectangle in port coordinates (taking into account the scroll
 * position). The field port is the standard Macintosh port into which
 * the plug-in should draw. Note that since the port is shared between
 * the plug-in and other plug-ins and Netscape, the plug-in should take
 * care to draw only within the area designated by the NPWindow, and to
 * always save the current port settings and set the desired port
 * settings before drawing and restore the previous port settings after
 * drawing.
 * 
 * Syntax
 * 
 * typedef struct NP_Port
 * {
 *     CGrafPtr    port;           /* Grafport */
 *     int32       portx;          /* position inside the topmost window */
 *     int32       porty;
 * } NP_Port;
 * 
 * See also
 * 
 * NPP_SetWindow
 * NPWindow 
 * 
 */


// !@#$# yikes!
// the window procs need to preserve all their bits so we pass them 
// around as <C-float>s.  Yuck
// See the corresponding and equally ugly hacks at the end of npwin.cpp and 
// plugin-glue.dylan

define constant <C-HACK-WNDPROC> = <C-float>;
define constant <HACK-WNDPROC> = <float>;
