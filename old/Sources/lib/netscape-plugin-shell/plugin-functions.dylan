module: netscape-plugin-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

/*
 * 
 * NPP_Destroy
 * 
 * Deletes a specific instance of a plug-in and returns an error value. 
 * 
 * NPP_Destroy is called when a plug-in instance is deleted, typically
 * because the user has left the page containing the instance, closed the
 * window, or quit the application. You should delete any private
 * instance-specific information stored in instance->pdata. If the
 * instance being deleted is the last instance created by your plug-in,
 * NPP_Shutdown will subsequently be called, where you can delete any
 * data allocated in NPP_Initialize to be shared by all your plug-in's
 * instances. Note that you should not perform any graphics operations in
 * NPP_Destroy as the instance's window is no longer guaranteed to be
 * valid.
 * 
 * You can use the save parameter if you want to save some state or other
 * information to be reused by a new instance with the same URL. This
 * data will be passed to the new instance via a parameter to NPP_New,
 * called when the instance is created. For example, a video player might
 * choose to save the frame number that was last displayed. If the user
 * returned to the page, when the new instance was created it would have
 * the previous frame number passed to it, so it could initially display
 * the same frame.
 * 
 * Note that ownership of the buf field of the NPSavedData structure
 * passes from the plug-in to Netscape when NPP_Destroy returns. Netscape
 * can and will discard this data based on arbitrary criteria such as its
 * size and the user's page history; thus you should not save critical
 * data using this mechanism. To ensure that Netscape does not crash or
 * leak memory when the saved data is discarded, the buf should be a flat
 * structure (i.e., a simple structure with no allocated substructures)
 * allocated with NPN_MemAlloc. For example:
 * 
 * char* myData = "Here is some saved data.\n";
 * int32 myLength = strlen(myData) + 1;
 * *save = (NPSavedData*) NPN_MemAlloc(sizeof(NPSavedData));
 * (*save)->len = myLength;
 * (*save)->buf = (void*) NPN_MemAlloc(myLength);
 * strcpy((*save)->buf, myData);
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPP_Destroy(NPP instance, NPSavedData **save);
 * 
 * See also
 * 
 * NPP_New
 * NPP_Shutdown
 * NPP
 * NPSavedData 
 * 
 */

define c-callable-wrapper c-fun-NPP-Destroy of NPP-Destroy-internal
  parameter instance :: <NPP>;
  output parameter save :: <NPSavedData**>;
  result r :: <NPError>;
  c-name: "NPP_Destroy";
end;

  
/*
 * 
 * NPP_DestroyStream
 * 
 * Indicates the closure and deletion of a stream, and returns an error
 * value.
 * 
 * The NPP_DestroyStream function is called when the stream identified by
 * stream for the plug-in instance denoted by instance will be
 * destroyed. You should delete any private data allocated in
 * stream->pdata at this time.
 * 
 * The reason the stream was destroyed is indicated by the by the
 * parameter reason. The most common reason code is NPRES_DONE,
 * indicating simply that the stream completed normally because all data
 * was sent to the instance. Other possible reason codes are
 * NPRES_USER_BREAK, indicating that the user canceled the stream by
 * clicking the "Stop" button, and NPRES_NETWORK_ERR, indicating that the
 * stream failed due to network problems. The complete list of reason
 * codes is found in npapi.h.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPP_DestroyStream(NPP instance, NPStream *stream, NPError reason);
 * 
 * See also
 * 
 * NPP_NewStream
 * NPStream 
 * 
 */

define c-callable-wrapper c-fun-NPP-DestroyStream of NPP-DestroyStream-internal
  parameter instance :: <NPP>;
  parameter stream :: <NPStream*>;
  parameter reason :: <NPReason>;
  result r :: <NPError>;
  c-name: "NPP_DestroyStream";
end;





/*	      
 * 
 * NPP_GetJavaClass
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Returns the Java class associated with the plug-in. For more
 * information, read about calling plug-in native methods from Java.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * jref NPP_GetJavaClass(void);
 * 
 */

define c-callable-wrapper c-fun-NPP-GetJavaClass
  of NPP-GetJavaClass-internal
  result r :: <c-void*>;
  c-name: "NPP_GetJavaClass";
end;


/*
 * 
 * 
 * NPP_HandleEvent
 * 
 * Currently this function is used only on the Macintosh.
 * 
 * Delivers a platform-specific event to the instance.
 * 
 * On the Macintosh, event is a pointer to a standard Macintosh
 * EventRecord. All standard event types are passed to the instance as
 * appropriate. In general, return TRUE if you handle the event and FALSE
 * if you ignore the event.
 * 
 *    Mouse events: Sent if the mouse is within the bounds of the instance. 
 *    Key events: Sent if the instance has text focus (see below). 
 *    Update events: Sent if the update region intersects the instance's bounds. 
 *    Activate events: Sent to all instances in the window being activate
 *                     or deactivated.  
 *    Suspend/Resume events: Sent to all instances in all windows. 
 *    Null events: Sent to all instances in all windows. 
 * 
 * In addition to these standard types, three additional event types may
 * be passed in the event->what field of the EventRecord:
 * 
 *    getFocusEvent: Sent when the instance may become the focus of subsequent 
 * key events, as a result of the user clicking on the instance or
 * pressing the tab key to focus the instance. If your instance accepts
 * key events, return TRUE, and key events will be sent to the instance
 * until it receives a loseFocusEvent. If your plug-in ignores key
 * events, return FALSE, and the key events will be processed by Netscape
 * itself.
 * 
 *    loseFocusEvent: Sent when the instance has lost the text focus, as a
 * result of the user clicking elsewhere on the page or pressing the tab
 * key to move the focus. No key events will be sent to the instance
 * until the next getFocusEvent.
 * 
 *    adjustCursorEvent: Send when the mouse enters or leaves the bounds
 * of the instance. If your plug-in wants to set the cursor when the
 * mouse is within the instance, set the cursor and return TRUE. If you
 * don't want a special cursor, return FALSE and Netscape will use the
 * standard arrow cursor.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * int16 NPP_HandleEvent(NPP instance, void *event);
 * 
 */

/*
 * 
 * NPP_Initialize
 * 
 * Provides global initialization for a plug-in, and returns an error
 * value.
 * 
 * This function is called once when a plug-in is loaded, before the
 * first instance is created. You should allocate any memory or resources
 * shared by all instances of your plug-in at this time. After the last
 * instance has been deleted, NPP_Shutdown will be called, where you can
 * release any memory or resources allocated by NPP_Initialize.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPP_Initialize(void)
 * 
 * See also
 * 
 * NPP_Shutdown 
 * 
 */

define c-callable-wrapper c-fun-NPP-Initialize of NPP-Initialize-internal
  result r :: <NPError>;
  c-name: "NPP_Initialize";
end;





/*
 * 
 * 
 * NPP_New
 * 
 * Creates a new instance of a plug-in and returns an error value.
 * 
 * NPP_New creates a new instance of your plug-in with MIME type
 * specified by pluginType. The parameter mode is NP_EMBED if the
 * instance was created by an EMBED tag, or NP_FULL if the instance was
 * created by a separate file. You can allocate any instance-specific
 * private data in instance->pdata at this time. The NPP pointer is valid
 * until the instance is destroyed.
 * 
 * The parameters argc, argn and argv pass a list of name-value pairs to
 * the plug-in for the HTML attributes associated with this
 * instance. Since Netscape ignores any non-standard attributes within an
 * EMBED tag, you can use private attributes to communicate
 * instance-specific options or other information to the plug-in. For
 * example, the following EMBED tag has the standard attributes SRC,
 * HEIGHT, and WIDTH, and the private attribute LOOP:
 * 
 * <EMBED SRC="movie.avi" HEIGHT=100 WIDTH=100 LOOP=TRUE>
 * 
 * With this EMBED tag, Netscape would pass the following values to the
 * plug-in instance:
 * 
 * argc = 4 argn = { "SRC", "HEIGHT", "WIDTH", "LOOP" }
 * argv = { "movie.avi", "100", "100", "TRUE" }
 * 
 * If you have previously saved data for this instance of the plug-in
 * using the function NPP_Destroy, you may receive this data via the
 * saved parameter. If saved is non-NULL, ownership of the NPSavedData
 * object passes from Netscape back to the plug-in; therefore, the
 * plug-in is responsible for freeing the memory for the NPSavedData and
 * the buffer within it.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPP_New(NPMIMEType *pluginType, NPP instance, uint16 mode, 
 *                 int16 argc, char *argn[], char *argv[], NPSavedData *saved);
 * 
 * See also
 * 
 * NPP_Destroy
 * NPP
 * NPSavedData 
 * 
 */

define c-callable-wrapper c-fun-NPP-New of NPP-New-internal
  parameter plugintype :: <NPMIMEType>;
  parameter instance :: <NPP>;
  parameter mode :: <uint16>;
  parameter argc :: <int16>;
  parameter argn :: <c-char**>;
  parameter argv :: <c-char**>;
  parameter saved :: <NPSavedData*>;
  result r :: <NPError>;
  c-name: "NPP_New";
end;



/*
 * 
 *  NPP_NewStream
 * 
 * Notifies an instance of a new data stream and returns an error value.
 * 
 * NPP_NewStream notifies the instance denoted by instance of the
 * creation of a new stream specifed by stream. The NPStream* pointer is
 * valid until the stream is destroyed. The MIME type of the stream is
 * provided by the parameter type.
 * 
 * The stream may have been started by Netscape from the file specified
 * in the SRC attribute of the EMBED tag (for an embedded instance) or
 * the file itself (for a full-page instance). A plug-in can also request
 * a stream with function NPN_GetURL. NPP_DestroyStream is called when
 * the stream completes (either successfully or abnormally). The plug-in
 * can terminate the stream itself by calling NPN_DestroyStream.
 * 
 * The parameter stype defines the mode of the stream. By default the
 * mode is NP_NORMAL, but the plug-in can request a different mode if
 * necessary:
 * 
 *    NP_NORMAL: Data in the stream is delivered to the instance in a
 * series of calls to NPP_WriteReady and NPP_Write. This mode allows the
 * plug-in to process the data progressively as it arrives from the
 * network or file system.
 * 
 *    NP_ASFILEONLY: New in Netscape Navigator 3.0. Data in the stream is
 * saved by Netscape to a file in the local cache. When the stream is
 * complete, Netscape calls NPP_StreamAsFile to deliver the path of the
 * file to the plug-in. If the stream comes from a file that is already
 * local, NPP_StreamAsFile is simply called immediately (no data is
 * copied). This mode allows the plug-in full random access to the data
 * using platform-specific file operations.
 * 
 *    NP_ASFILE: Most plug-ins that need the stream saved to a file
 * should use the more efficient mode NP_ASFILEONLY (above); this mode is
 * preserved for compatibility only. NP_ASFILE differs from NP_ASFILEONLY
 * in that data is delivered to the plug-in, via a series of calls to
 * NPP_Write, as it is saved to the file (as in mode NP_NORMAL). If the
 * data in the stream comes from a file that is already local, the data
 * is still read, sent to the plug-in via NPP_Write, and written to a
 * file in the local cache.
 * 
 *    NP_SEEK: Data in the stream is not automatically delivered to the
 * instance, but can be randomly accessed by the plug-in as needed, via
 * calls to NPN_RequestRead. The parameter seekable is TRUE if the stream
 * inherently supports random access (for example, local files or HTTP
 * servers that support byte-range requests). If the plug-in requests
 * mode NP_SEEK for a stream that is not inherently seekable (seekable =
 * FALSE), the data in the stream will be copied to the local cache. Any
 * requests for data made by the plug-in via NPN_RequestRead will only be
 * fulfilled when all data has been read and stored in the cache. Note
 * that as an optimization to extract the maximum benefit from existing
 * network connections, Netscape will continue to read data sequentially
 * out of the stream (as in mode NP_NORMAL) until the first
 * NPN_RequestRead call is made.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPP_NewStream(NPP instance, NPMIMEType type, NPStream *stream, 
 *                       NPBool seekable, uint16* stype);
 * 
 * See also
 * 
 * NPP_DestroyStream
 * NPP_StreamAsFile
 * NPP_Write
 * NPP_WriteReady
 * NPN_DestroyStream
 * NPN_RequestRead
 * NPStream 
 * 
 */

define c-callable-wrapper c-fun-NPP-NewStream of NPP-NewStream-internal
  parameter instance :: <NPP>;
  parameter type :: <NPMIMEType>;
  parameter stream :: <NPStream*>;
  parameter seekable :: <NPBool>;
  parameter stype :: <uint16*>;
  result r :: <NPError>;
  c-name: "NPP_NewStream";
end;


/*
 * 
 * NPP_Print
 * 
 * Requests a platform-specific print operation for the instance.
 * 
 * NPP_Print requests the plug-in instance identified by instance to
 * print itself. If the instance is full-page, NPP_Print will initially
 * be called with platformPrint->mode equal to NP_FULL before Netscape
 * displays any print dialogs. If the plug-in wishes to completely
 * control the print process for this instance, it can access the
 * platform-specific printer information in platformPrint and handle the
 * print dialogs and printing process as it sees fit. If the plug-in does
 * print the instance in full-page mode, it should set pluginPrinted to
 * TRUE. If the plug-in does not want to take control of the complete
 * printing process it can set pluginPrinted to FALSE (the default value)
 * and Netscape will display whatever print dialogs are necessary and
 * call NPP_Print again (this time, with platformPrint->mode equal to
 * NP_EMBED).
 * 
 * If the instance is embedded, or full-page but the plug-in declined
 * control of the print process as described above, NPP_Print will be
 * called with platformPrint->mode equal to NP_EMBED.
 * platformPrint->embedPrint.window contains the window that the plug-in
 * should render the instance into for printing.
 * 
 * On the Macintosh, when printing in mode NP_FULL the field
 * platformPrint contains a standard Macintosh THPrint (see Printing.h).
 * 
 * On Windows, note that the coordinates for the window rectangle are in
 * TWIPS. Therefore you need to convert the x-y point using the Windows
 * API call DPtoLP() when you output text.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * void NPP_Print(NPP instance, NPPrint *platformPrint);
 * 
 * See also
 * 
 * NPPrint
 * NPFullPrint
 * NPEmbedPrint 
 * 
 */

define c-callable-wrapper c-fun-NPP-Print of NPP-Print-internal
  parameter instance :: <NPP>;
  parameter platformPrint :: <NPPrint*>;
  c-name: "NPP_Print";
end;


/* 
 * 
 * NPP_SetWindow
 * 
 * Sets the window in which a plug-in draws, and returns an error value.
 * 
 * NPP_SetWindow informs the plug-in instance specified by instance of
 * the the window denoted by window in which the instance draws. This
 * NPWindow pointer is valid for the life of the instance, or until
 * NPP_SetWindow is called again with a different value. Subsequent calls
 * to NPP_SetWindow for a given instance typically indicate that the
 * window has been resized. If either window or window->window are NULL,
 * the plug-in must not perform any additional graphics operations on the
 * window and should free any resources associated with the window.
 * 
 * On Windows and UNIX, the platform-specific window information
 * specified in window->window is a handle to a subwindow of the Netscape
 * window hierarchy. On the Macintosh, this field points to a NP_Port
 * structure.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPP_SetWindow(NPP instance, NPWindow *window);
 * 
 * See also
 * 
 * NPWindow
 * NPPort 
 * 
 */

define c-callable-wrapper c-fun-NPP-SetWindow of NPP-SetWindow-internal
  parameter instance :: <NPP>;
  parameter window :: <NPWindow*>;
  result r :: <NPError>;
  c-name: "NPP_SetWindow";
end;


/* 
 * 
 * NPP_Shutdown
 * 
 * Provides global deinitialization for a plug-in.
 * 
 * This function is called once after the last instance of your plug-in
 * is destroyed. Use this function to release any memory or resources
 * shared across all instances of your plug-in.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * void NPP_Shutdown(void);
 * 
 * See also
 * 
 * NPP_Initialize 
 * 
 */

define c-callable-wrapper c-fun-NPP-Shutdown of NPP-Shutdown-internal
  c-name: "NPP_Shutdown";
end;


/*
 * 
 * NPP_StreamAsFile
 * 
 * Provides a local file name for the data from a stream.
 * 
 * NPP_StreamAsFile provides the instance with a full path to a local
 * file, identified by fname, for the stream specified by
 * stream. NPP_StreamAsFile is called as a result of the plug-in
 * requesting mode NP_ASFILEONLY or NP_ASFILE in a previous call to
 * NPP_NewStream. If an error occurs while retrieving the data or writing
 * the file, fname may be NULL.
 * 
 * Note that if the file is created from a stream from the network, the
 * file is locked in the Netscape disk cache until the stream or its
 * instance is destroyed.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * void NPP_StreamAsFile(NPP instance, NPStream *stream, const char* fname);
 * 
 * See also
 * 
 * NPP_NewStream
 * NPStream 
 * 
 */


define c-callable-wrapper c-fun-NPP-StreamAsFile of NPP-StreamAsFile-internal
  parameter instance :: <NPP>;
  parameter stream :: <NPStream*>;
  parameter fname :: <c-char*>;
  c-name: "NPP_StreamAsFile";
end;


/* 
 * 
 * NPP_URLNotify
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Notifies the instance of the completion of a URL request.
 * 
 * NPP_URLNotify is called when Netscape completes a NPN_GetURLNotify or
 * NPN_PostURLNotify request, to inform the plug-in that the request,
 * identified by url, has completed for the reason specified by
 * reason. The most common reason code is NPRES_DONE, indicating simply
 * that the request completed normally. Other possible reason codes are
 * NPRES_USER_BREAK, indicating that the request was halted due to a user
 * action (for example, clicking the "Stop" button), and
 * NPRES_NETWORK_ERR, indicating that the request could not be completed
 * (for example, because the URL could not be found). The complete list
 * of reason codes is found in npapi.h.
 * 
 * The parameter notifyData is the same plug-in-private value passed as
 * an argument to the corresponding NPN_GetURLNotify or NPN_PostURLNotify
 * call, and can be used by your plug-in to uniquely identify the
 * request.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * void NPP_URLNotify(NPP instance, const char* url, NPReason reason, 
 *                    void* notifyData);
 * 
 * See also
 * 
 * NPN_GetURLNotify
 * NPN_PostURLNotify 
 * 
 */

define c-callable-wrapper c-fun-NPP-UrlNotify of NPP-UrlNotify-internal
  parameter instance :: <NPP>;
  parameter url :: <c-char*>;
  parameter reason :: <NPReason>;
  parameter notify-data :: <NPNotify-data>;
  c-name: "NPP_URLNotify";
end;


/*
 * 
 * NPP_Write
 * 
 * Delivers data from a stream and returns the number of bytes written.
 * 
 * NPP_Write is called after a call to NPP_NewStream in which the plug-in
 * requested a normal-mode stream, in which the data in the stream is
 * delivered progressively over a series of calls to NPP_WriteReady and
 * NPP_Write. The function delivers a buffer buf of len bytes of data
 * from the stream identified by stream to the instance. The parameter
 * offset is the logical position of buf from the beginning of the data
 * in the stream.
 * 
 * The function returns the number of bytes written (consumed by the
 * instance). A negative return value causes an error on the stream,
 * which will subsequently be destroyed via a call to NPP_DestroyStream.
 * 
 * Note that a plug-in must consume at least as many bytes as it
 * indicated in the preceeding NPP_WriteReady call. All data consumed
 * must be either processed immediately or copied to memory allocated by
 * the plug-in: the buf parameter is not persistent.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * int32 NPP_Write(NPP instance, NPStream *stream, int32 offset, 
 *                 int32 len, void *buf);
 * 
 * See also
 * 
 * NPP_DestroyStream
 * NPP_NewStream
 * NPP_WriteReady
 * NPStream 
 * 
 */


define c-callable-wrapper c-fun-NPP-Write of NPP-Write-internal
  parameter instance :: <NPP>;
  parameter stream :: <NPStream*>;
  parameter offset :: <int32>;
  parameter len :: <int32>;
  parameter buf :: <c-void*>;
  result r :: <int32>;
  c-name: "NPP_Write";
end;

/*
 * 
 * NPP_WriteReady
 * 
 * Returns the maximum number of bytes that an instance is prepared to
 * accept from the stream.
 * 
 * NPP_WriteReady determines the maximum number of bytes that the
 * instance will consume from the stream in a subsequent call
 * NPP_Write. This function allows Netscape to only send as much data to
 * the instance as the instance is capable of handling at a time,
 * allowing more efficient use of resources within both Netscape and the
 * plug-in.
 * 
 * For example, suppose the plug-in allocates in NPP_NewStream an 8K
 * buffer to hold the data written from that stream. In the first call to
 * NPP_WriteReady it could return 8192, resulting in a call to NPP_Write
 * with a buffer of up to 8K bytes. After this data is copied from
 * Netscape's buffer to the plug-in's buffer, the plug-in begins to
 * asynchronously process the data. When the next NPP_WriteReady call is
 * made, only half of the data has been processed, so to avoid allocating
 * additional buffers, the plug-in could return 4096, resulting in a call
 * to NPP_Write of up to 4K bytes.
 * 
 * Note that the buffer passed to NPP_Write may be larger than the size
 * returned from NPP_WriteReady. The value returned from NPP_WriteReady
 * is only a promise to consume a certain amount of data from the buffer,
 * not an upper limit on the buffer size. In the example above, if the
 * plug-in allocates an 8K buffer and returns 8192 from NPP_Write, but
 * gets 10000 bytes from Netscape in a subsequent call to NPP_Write, the
 * plug-in should copy the first 8192 bytes from Netscape's buffer into
 * its own buffer, and return 8192 (the number of bytes actually
 * consumed) from NPP_Write.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * int32 NPP_WriteReady(NPP instance, NPStream *stream);
 * 
 * See also
 * 
 * NPP_Write
 * NPStream 
 * <NPPrint>
 * 
 */

define c-callable-wrapper c-fun-NPP-WriteReady of NPP-WriteReady-internal
  parameter instance :: <NPP>;
  parameter stream :: <NPStream*>;
  result r :: <int32>;
  c-name: "NPP_WriteReady";
end;