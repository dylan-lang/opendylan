module: netscape-plugin-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

/* 
 * 
 * NPN_DestroyStream
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Closes and deletes a stream.
 * 
 * The NPN_DestroyStream function closes and deletes the stream
 * identified by stream for the plug-in instance denoted by instance. The
 * stream can be either a stream that Netscape created and passed to the
 * plug-in in NPP_NewStream, or a stream created by the plug-in via a
 * call to NPN_NewStream.
 * 
 * The reason the stream was destroyed is indicated by the by the
 * parameter reason. The most common reason code is NPRES_DONE,
 * indicating simply that the stream completed normally because all data
 * was sent by the plug-in to Netscape. Other possible reason codes are
 * NPRES_USER_BREAK, indicating that the plug-in is terminating the
 * stream due to a user request, and NPRES_NETWORK_ERR, indicating that
 * the stream failed due to network problems. The complete list of reason
 * codes is found in npapi.h.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPN_DestroyStream(NPP instance, NPStream *stream, NPError reason);
 * 
 * See also
 * 
 * NPN_NewStream
 * NPStream 
 * 
 */

define c-function NPN-DestroyStream-internal
  parameter instance :: <NPP>;
  parameter stream :: <NPStream*>;
  parameter reason :: <NPReason>;
  result r :: <NPError>;
  c-name: "NPN_DestroyStream";
end;





/*
 * 
 * NPN_GetJavaEnv
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Returns the Java execution environment, needed to make any calls to
 * Java methods from your plug-in using the JRI. For more information,
 * read about accessing Java methods from plug-ins.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * JavaEnv* NPN_GetJavaEnv(void);
 * 
 */

define c-function NPN-GetJavaEnv
  result r :: <c-void*>;	// should really use the java def
				// here, but we probably won't use it anyway
  c-name: "NPN_GetJavaEnv";
end;


/*
 * 
 * NPN_GetJavaPeer
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Returns the Java object associated with the plug-in instance. For more
 * information, read about calling plug-in native methods from Java.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * jref NPN_GetJavaPeer(NPP instance);
 * 
 */


define c-function NPN-GetJavaPeer-internal
  parameter instance :: <NPP>;
  result r :: <c-void*>;	// should really use the java def
				// here, but we probably won't use this anyway
  c-name: "NPN_GetJavaPeer";
end;


/*
 * 
 * NPN_GetURL
 * 
 * Requests that a new stream be created with the contents of the
 * specified URL. To request a stream and receive notification of the
 * result, use NPN_GetURLNotify.
 * 
 * NPN_GetURL requests the creation of a new stream for the URL
 * identified by url for the plug-in instance specified by instance. The
 * parameter target determines the recipient of the stream. Note that
 * this operation is asynchronous: control will always immediately return
 * to your plug-in, but the stream will not be created until some point
 * in the future, or never created in the event of network problems or
 * user cancellation.
 * 
 * If target is non-NULL, the stream is sent to Netscape and Netscape
 * displays the contents of the stream in the named target window or
 * frame. (For more information on named targets, see Netscape's
 * documentation on targeting windows.) Note that if the target refers to
 * the window or frame containing the instance, the instance will be
 * destroyed and the plug-in may be unloaded. For example, a plug-in
 * instance could draw a button that acts like a link to another web
 * page. When the user clicks the button, the plug-in would call
 * NPN_GetURL to go to the page:
 * 
 * err = NPN_GetURL(instance, "http://home.netscape.com/", "_self");
 * 
 * If target is NULL, the stream containing the contents of the URL will
 * be delivered to the plug-in via a call to NPP_NewStream, one or more
 * calls to NPP_WriteReady and NPP_Write, and a call to NPP_DestroyStream
 * when the stream is complete. Using NPN_GetURL in this mode allows the
 * plug-in to retrieve arbitrary data from the network or file system and
 * process the data itself.
 * 
 * Note that NPN_GetURL is typically asynchronous: it returns immediately
 * and only later handles the request. For this reason you may find it
 * useful to instead call NPN_GetURLNotify so your plug-in is notified
 * upon successful or unsuccessful completion of the request.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPN_GetURL(NPP instance, const char *url, const char *target);
 * 
 * See also
 * 
 * NPN_GetURLNotify
 * NPN_PostURL
 * NPP_URLNotify 
 * 
 */


define c-function NPN-GetUrl-internal
  parameter instance :: <NPP>;
  parameter url :: <c-char*>;
  parameter target :: <c-char*>;
  result r :: <NPError>;
  c-name: "NPN_GetURL";
end;


/*
 * 
 * NPN_GetURLNotify
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Requests that a new stream be created with the contents of the
 * specified URL, and receives notification of the result.
 * 
 * NPN_GetURLNotify functions identically to NPN_GetURL except that it
 * calls NPN_URLNotify upon successful or unsuccessful completion of the
 * request. The parameter notifyData is a plug-in-private value that can
 * be used to uniquely identify the request; it will be returned to the
 * plug-in as a parameter to the corresponding NPP_URLNotify call. Note
 * that NPN_GetURLNotify is typically asynchronous: it returns
 * immediately and only later handles the request and calls
 * NPP_URLNotify.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPP_GetURLNotify(NPP instance, const char* url,
 *			   const char* target, void* notifyData);
 * 
 * See also
 * 
 * NPN_GetURL
 * NPN_URLNotify 
 * 
 */

define c-function NPN-GetUrlNotify-internal
  parameter instance :: <NPP>;
  parameter url :: <c-char*>;
  parameter target :: <c-char*>;
  parameter notify-data :: <c-void*>;
  result r :: <NPError>;
  c-name: "NPN_GetURLNotify";
end;



/*
 * 
 * NPN_MemAlloc
 * 
 * Allocates memory from the Navigator's memory space.
 * 
 * Call NPN_MemAlloc to allocate size bytes of memory in the Navigator's
 * memory space. Netscape returns NULL if insufficient memory is
 * available. NPN_MemAlloc is particularly important on the Macintosh,
 * since the Macintosh version of Netscape Navigator frequently fills its
 * memory partition with cached data which is only purged as
 * necessary. Since NPN_MemAlloc automatically frees cached information
 * if necessary to fulfill the request, calls to NPN_MemAlloc may succeed
 * where direct calls to NewPtr() would fail.
 * 
 * If you allocate saved instance data in NPP_Destroy, be sure to
 * allocate the memory with this function, since Netscape may delete the
 * saved data with the equivalent of NPN_MemFree at any time. See the
 * code example in the documentation for NPP_Destroy for an example of
 * allocating saved data using this function.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * void *NPN_MemAlloc (uint32 size);
 * 
 * See also
 * 
 * NPN_MemFlush
 * NPN_MemFree 
 * 
 */


define c-function NPN-MemAlloc
  parameter size :: <uint32>;
  result r :: <c-void*>;
  c-name: "NPN_MemAlloc";
end;

/*
 * 
 * NPN_MemFlush
 * 
 * Currently this function is implemented only on the Macintosh.
 * 
 * Requests that Navigator free size bytes of memory, and returns the
 * amount freed.
 * 
 * Generally, plug-ins should use NPN_MemAlloc to allocate memory in
 * Navigator's memory space, since this function automatically frees
 * cached data if necessary to fulfill the request. Use NPN_MemFlush in
 * cases when calling NPN_MemAlloc is not possible, for example when
 * calling system APIs that indirectly allocate memory. To request that
 * Netscape free as much memory as possible, call NPN_MemFlush repeatedly
 * until it returns 0.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * uint32 NPN_MemFlush(uint32 size);
 * 
 * See also
 * 
 * NPN_MemAlloc
 * NPN_MemFree 
 * 
 */


// only used on mac ...

/* 
 * 
 * NPN_MemFree
 * 
 * Deallocate the block of memory, specified by ptr, that was previously
 * allocated using NPN_MemAlloc.
 * 
 * Syntax
 * 
 * #include <npapi.h>      
 * void NPN_MemFree (void *ptr);
 * 
 * See also
 * 
 * NPN_MemAlloc
 * NPN_MemFlush 
 * 
 */

define c-function NPN-MemFree
  parameter p :: <c-void*>;
  c-name: "NPN_MemFree";
end;

/*
 * 
 * NPN_NewStream
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Requests the creation of a new stream of data produced by the plug-in
 * and consumed by Netscape.
 * 
 * NPN_NewStream asks Netscape to create a new stream for the instance
 * specified by instance. The MIME type of the stream is specified by
 * type, and the target window or frame is denoted by target.  For more
 * information on named targets, see Netscape's frames documentation.
 * 
 * The stream created by Netscape is returned in the variable
 * stream. This stream object can then be used by the plug-in in
 * subsequent calls to NPN_Write to write data into the stream for
 * consumption by Netscape. When the plug-in has written all data into
 * the stream, call NPN_DestroyStream to terminate the stream and
 * deallocate the NPStream object.
 * 
 * For example, the following code creates a new stream of HTML text
 * displayed by Netscape in a new window (error handling has been omitted
 * for simplicity):
 * 
 * NPStream stream; 
 * char* myData = "<HTML><B>This is a message from my plug-in!</B></HTML>";
 * int32 myLength = strlen(myData) + 1; 
 * err = NPN_NewStream(instance, "text/html", "_blank", &stream); 
 * err = NPN_Write(instance, stream, myLength, myData); 
 * err = NPN_DestroyStream(instance, stream, NPRES_DONE);
 * 
 * Note your plug-in can create another instance of itself by specifying
 * its own MIME type and a new target name in a call to
 * NPN_NewStream. Note also that if the target refers to the window or
 * frame containing the instance (for example, "_current"), as soon as
 * the plug-in writes data to the stream (via a call to NPN_Write) the
 * instance and stream will be destroyed as Netscape replaces the old
 * contents of the target with the new data in the stream.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPN_NewStream(NPP instance, NPMIMEType type, 
 *                       const char *target, NPStream **stream);
 * 
 * See also
 * 
 * NPN_Write
 * NPN_DestroyStream
 * NPStream 
 *
 */

define c-function NPN-NewStream-internal
  parameter instance :: <NPP>;
  parameter type :: <NPMIMEType>;
  parameter target :: <c-char*>;
  output parameter stream  :: <NPStream**>;
  result r :: <NPError>;
  c-name: "NPN_NewStream";
end;


/*
 * 
 * NPN_PostURL
 * 
 * NPN_PostURL is only partially implemented in Netscape Navigator 2.0.
 * 
 * Posts data from a file or buffer to a URL. To post data and receive
 * notification of the result, use NPN_PostURLNotify.
 * 
 * NPN_PostURL works similarly to NPN_GetURL, but in reverse. Like
 * NPN_GetURL, an instance, url, and target are specified by the
 * plug-in. But while NPN_GetURL reads data from the URL and either
 * displays it in the target window or delivers it to the plug-in,
 * NPN_PostURL writes data to the URL and displays the server's response
 * in the target window or delivers it to the plug-in.
 * 
 * The data to post can be either contained in a file or a memory
 * buffer. To post a file, set the flag file to TRUE, the buffer buf to
 * the path name string for a file, and len to the length of the path
 * string. The the file-type URL prefix "file://" is optional. Note that
 * on Windows and the Macintosh, if a file is posted with any protocol
 * other than FTP the file must be text with UNIX-style line breaks ('\n'
 * separators only).
 * 
 * To post data in a memory buffer, set the flag file to FALSE, the
 * buffer buf to the data to post, and len to the length of
 * buffer. NPN_PostURL works identically with buffers and files.
 * 
 * Possible URL types include FTP, HTTP, mail, and news. For protocols
 * where the headers must be distinguished from the body, such as HTTP,
 * the buffer or file should contain the headers, followed by a blank
 * line, then the body. If no custom headers are required, simply add a
 * blank line ('\n') to the beginning of the file or buffer. NOTE:
 * Passing headers (even a blank line) in a memory buffer is not
 * supported by NPN_PostURL. If you need to specify headers (or leave a
 * blank line for the defaults) in a memory buffer, you must use
 * NPN_PostURLNotify.
 * 
 * For example, the following code posts two name-value pairs to a CGI
 * script via HTTP. The response from the server will be sent to the
 * plug-in in a new stream created with NPP_NewStream.
 * 
 * char* myData = "Content Type:\tapplication/x-www-form-urlencoded\nContent Length:\t25\n\nname1=value1&name2=value2\n"; 
 * uint32 myLength = strlen(myData) + 1; 
 * err = NPN_PostURL(instance,
 *                   "http://hoohoo.ncsa.uiuc.edu/cgi-bin/post-query", 
 *                    NULL, myLength, myData, FALSE);
 * 
 * The following code will send a mail message with the default headers
 * from the client machine:
 * 
 * char* myData = "\nHi Fred, this is a message from my plug-in!"; 
 * uint32 myLength = strlen(myData) + 1; 
 * err = NPN_PostURL(instance, "mailto:fred@somewhere.com", 
 *                   NULL, myLength, myData, FALSE);
 * 
 * This code will upload a file from the root of the local file system to
 * a FTP server and display the response in a frame named "response":
 * 
 * char* myData = "file:///myFileName"; 
 * uint32 myLength = strlen(myData) + 1; 
 * err = NPN_PostURL(instance, 
 *                   "ftp://fred@ftp.somewhere.com/pub/",
 *                   "response", myLength, myData, TRUE);
 * 
 * Note that NPN_PostURL is typically asynchronous: it returns
 * immediately and only later handles the request. For this reason you
 * may find it useful to instead call NPN_PostURLNotify so your plug-in
 * is notified upon successful or unsuccessful completion of the request.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPN_PostURL(NPP instance, const char *url, const char *window,
 *                     uint32 len, const char *buf, NPBool file);
 * 
 * See also
 * 
 * NPN_GetURL
 * NPN_PostURLNotify 
 * 
 */

define c-function NPN-PostUrl-internal
  parameter instance :: <NPP>;
  parameter url :: <c-char*>;
  parameter target :: <c-char*>;
  parameter len :: <uint32>;
  parameter buf :: <c-char*>;
  parameter file :: <NPBool>;
  result r :: <NPError>;
  c-name: "NPN_PostURL";
end;

/*
 * 
 * NPN_PostURLNotify
 *  
 * New in Netscape Navigator 3.0.
 * 
 * Posts data from a file or buffer to a URL, and receives notification
 * of the result.
 * 
 * NPN_PostURLNotify functions identically NPN_PostURL in most ways
 * except that (1) it supports specifying headers when posting a memory
 * buffer, and (2) it calls NPN_URLNotify upon successful or unsuccessful
 * completion of the request. The parameter notifyData is a
 * plug-in-private value that can be used to uniquely identify the
 * request; it will be returned to the plug-in as a parameter to the
 * corresponding NPP_URLNotify call. Note that NPN_PostURLNotify is
 * typically asynchronous: it returns immediately and only later handles
 * the request and calls NPP_URLNotify.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPN_PostURLNotify(NPP instance, const char *url, 
 *                           const char *window, uint32 len, 
 *                           const char *buf, NPBool file, void* notifyData);
 * 
 * See also
 * 
 * NPN_PostURL
 * NPN_URLNotify 
 * 
 */

define c-function NPN-PostUrlNotify-internal
  parameter instance :: <NPP>;
  parameter url :: <c-char*>;
  parameter target :: <c-char*>;
  parameter len :: <uint32>;
  parameter buf :: <c-char*>;
  parameter file :: <NPBool>;
  parameter notifyData :: <c-void*>;
  result r :: <NPError>;
  c-name: "NPN_PostURLNotify";
end;


/*
 * 
 * NPN_RequestRead
 * 
 * Requests a range of bytes for a seekable stream.
 * 
 * NPN_RequestRead requests a range of bytes identified by rangeList to
 * be read from the stream denoted by stream. The rangeList is a linked
 * list of NPByteRange objects, each of which specifies an offset and
 * length into the stream. The data will be written to the plug-in via
 * subsequent calls to NPP_WriteReady and NPP_Write.
 * 
 * The plug-in can call NPN_RequestRead on streams that were not
 * initially placed NP_SEEK mode as long as the stream is inherently
 * seekable (see NPP_NewStream); NPN_RequestRead simply automatically
 * changes the mode to NP_SEEK. If the stream is not inherently seekable,
 * the stream must have been put in NP_SEEK mode initially (since
 * Netscape must cache all the stream data on disk in order to access it
 * randomly). If NPN_RequestRead is called on a stream that is not
 * inherently seekable and not initially in mode NP_SEEK, it returns the
 * error code NPERR_STREAM_NOT_SEEKABLE.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * NPError NPN_RequestRead((NPStream *stream, NPByteRange *rangeList);
 * 
 * See also
 * 
 * NPP_NewStream
 * NPStream 
 * 
 */

define c-function NPN-RequestRead-internal
  parameter stream :: <NPStream*>;
  parameter rangelist :: <NPByteRange*>;
  result r :: <NPError>;
  c-name: "NPN_RequestRead";
end;


/*
 * 
 * NPN_Status
 * 
 * Displays a status message.
 * 
 * NPN_Status displays the status message indicated by message for the
 * instance denoted by instance.  The message appears in the Navigator
 * client user interface; in Navigator 2.0 the message is drawn at the
 * bottom of the browser window on all platforms.
 * 
 * This function is useful for simulating the Navigator's behavior for
 * links. When the user moves the mouse over a link in a browser window,
 * the Navigator displays the URL of the link in the status message
 * area. If your plug-in has a button or other object that will act as a
 * link when clicked, you should make your user interface consistent with
 * the rest of the Navigator by calling NPN_Status to display the URL of
 * the link when user moves the mouse over the button.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * void NPN_Status((NPP instance, const char *message);
 * 
 */

define c-function NPN-Status-internal
  parameter instance :: <NPP>;
  parameter message :: <c-char*>;
  c-name: "NPN_Status";
end;

/*
 * 
 * NPN_UserAgent
 * 
 * Returns a string containing the Navigator's "user agent" field. The
 * user agent is the part of the HTTP header that identifies the browser
 * during transfers.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * const char* NPN_UserAgent(NPP instance);
 * 
 * See also
 * 
 * NPN_Version 
 * 
 */


define c-function NPN-UserAgent-internal
  parameter instance :: <NPP>;
  result r :: <c-char*>;
  c-name: "NPN_UserAgent";
end;

/*
 * 
 * NPN_Version
 * 
 * Returns the plug-in API version information.
 * 
 * NPN_Version returns the plug-in version number in plugin_major and
 * plugin_minor, and Netscape Navigator version in netscape_major and
 * netscape_minor. The plug-in version is determined by the constants
 * defined in npapi.h when the plug-in is compiled; the Netscape verion
 * is determine by the same constants when Netscape is compiled.
 * 
 * The plug-in glue code (in npmac.cpp for the Macintosh and npwin.cpp
 * for Windows) checks the major version numbers and refuses to load the
 * plug-in if the major version of Netscape is greater than the major
 * version of the plug-in. Thus the major version number in npapi.h will
 * only change if old plug-ins will be strictly and explicitly
 * incompatible with a new version of Netscape Navigator. The minor
 * version number will change whenever the plug-in API functionality
 * changes with a new version of Navigator. For example, if new
 * capabilities or API entry points are added in a new Navigator version,
 * plug-ins should check the minor version of Netscape to ensure that the
 * capabilities exist before using them.
 * 
 * Syntax
 * 
 * #include <npapi.h>
 * void NPN_Version(int *plugin_major, int *plugin_minor,
 *		    int *netscape_major, int *netscape_minor);
 * 
 * See also
 * 
 * NPN_UserAgent 
 * 
 */

define c-function NPN-Version
  output parameter plugin-major :: <c-int*>;
  output parameter plugin-minor :: <c-int*>;
  output parameter netscape-major :: <c-int*>;
  output parameter netscape-minor :: <c-int*>;
  c-name: "NPN_Version";
end;

/*
 * 
 * Npn_Write
 * 
 * New in Netscape Navigator 3.0.
 * 
 * Writes data to a stream for consumption by Netscape.
 * 
 * A plug-in can call NPN_Write after creating a stream with
 * NPN_NewStream. The function delivers a buffer buf of len bytes of data
 * from the stream identified by stream to the instance. The parameter
 * offset is the logical position of buf from the beginning of the data
 * in the stream.
 * 
 * The function returns the number of bytes written (consumed by
 * Netscape), which may depend on the size of Netscape's memory buffers,
 * the number of active streams, and other factors. A negative return
 * value indicates that Netscape encountered an error while processing
 * the data, so the plug-in should terminate the stream by calling
 * NPN_DestroyStream. See the documentation for NPN_NewStream for an
 * example showing the use of NPN_Write.
 * 
 * Syntax
 * 
 * #include npapi.h
 * int32 NPN_Write(NPP instance, NPStream *stream, int32 offset, 
 *                 int32 len, void *buf);
 * 
 * See also
 * 
 * NPN_NewStream
 * NPN_DestroyStream
 * NPP_Write
 * NPStream 
 * 
 */

define c-function NPN-Write-internal
  parameter instance :: <NPP>;
  parameter stream :: <NPStream*>;
  parameter offset :: <int32>;
  parameter len :: <int32>;
  parameter buf :: <c-void*>;
  result r :: <int32>;
  c-name: "NPN_Write";
end;
			     