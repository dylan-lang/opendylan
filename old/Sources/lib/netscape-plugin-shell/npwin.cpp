/* npwin.cpp */

//\\// INCLUDE
#ifndef _INC_WINDOWS    
#include <windows.h>
#endif

// netscape
#ifndef _NPAPI_H_
#include "npapi.h"
#endif
#ifndef _NPUPP_H_
#include "npupp.h"
#endif

//\\// DEFINE
#ifdef WIN32
    #define NP_EXPORT
#else
    #define NP_EXPORT _export
#endif

//\\// GLOBAL DATA
NPNetscapeFuncs* g_pNavigatorFuncs = NULL;

//!@#$ stuff to satisfy the dylan win32 libs

extern "C" {
HANDLE application_hInstance;
HANDLE application_hPrevInstance;
LPSTR  application_lpCmdLine;
int    application_nCmdShow;
void hacks__main(void);
LONG APIENTRY ConnectionProtocol(void);
}
int connection_initialized = 0;
int dylan_initialized = 0;
void *valid_stack_base = NULL;

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// DllEntryPoint
//
//
BOOL WINAPI 
DllEntryPoint( HINSTANCE  hinstDLL,			// handle of DLL module 
	       DWORD  fdwReason,	// reason for calling function 
	       LPVOID  lpvReserved)
{
  // set the global application instance so windows can be created
  application_hInstance = hinstDLL;
  application_hPrevInstance = NULL;
  application_lpCmdLine = NULL;
  application_nCmdShow = SW_SHOWNORMAL;

	switch (fdwReason) {
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_PROCESS_DETACH:
		case DLL_THREAD_DETACH:
		break;
	}
	return TRUE;
}

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// Private_GetJavaClass (global function)
//
//	Given a Java class reference (thru NPP_GetJavaClass) inform JRT
//	of this class existence
//
JRIGlobalRef
Private_GetJavaClass(void)
{
    jref clazz = NPP_GetJavaClass();
    if (clazz) {
		JRIEnv* env = NPN_GetJavaEnv();
		return JRI_NewGlobalRef(env, clazz);
    }
    return NULL;
}

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
//						PLUGIN DLL entry points   
//
// These are the Windows specific DLL entry points. They must be exoprted
//

// we need these to be global since we have to fill one of its field
// with a data (class) which requires knowlwdge of the navigator
// jump-table. This jump table is known at Initialize time (NP_Initialize)
// which is called after NP_GetEntryPoint
static NPPluginFuncs* g_pluginFuncs;

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// NP_GetEntryPoints
//
//	fills in the func table used by Navigator to call entry points in
//  plugin DLL.  Note that these entry points ensure that DS is loaded
//  by using the NP_LOADDS macro, when compiling for Win16
//
NPError WINAPI NP_EXPORT 
NP_GetEntryPoints(NPPluginFuncs* pFuncs)
{
    // trap a NULL ptr 
    if(pFuncs == NULL)
        return NPERR_INVALID_FUNCTABLE_ERROR;

    // if the plugin's function table is smaller than the plugin expects,
    // then they are incompatible, and should return an error 
    if(pFuncs->size < sizeof NPPluginFuncs)
        return NPERR_INVALID_FUNCTABLE_ERROR;

    pFuncs->version       = (NP_VERSION_MAJOR << 8) | NP_VERSION_MINOR;
    pFuncs->newp          = NPP_New;
    pFuncs->destroy       = NPP_Destroy;
    pFuncs->setwindow     = NPP_SetWindow;
    pFuncs->newstream     = NPP_NewStream;
    pFuncs->destroystream = NPP_DestroyStream;
    pFuncs->asfile        = NPP_StreamAsFile;
    pFuncs->writeready    = NPP_WriteReady;
    pFuncs->write         = NPP_Write;
    pFuncs->print         = NPP_Print;
    pFuncs->urlnotify     = NPP_URLNotify;
    pFuncs->event         = NULL;       /// reserved 
	
	g_pluginFuncs		  = pFuncs;

    return NPERR_NO_ERROR;
}

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// NP_Initialize
//
//	called immediately after the plugin DLL is loaded
//
NPError WINAPI NP_EXPORT 
NP_Initialize(NPNetscapeFuncs* pFuncs)
{
    // trap a NULL ptr 
    if(pFuncs == NULL)
        return NPERR_INVALID_FUNCTABLE_ERROR;

    
    g_pNavigatorFuncs = pFuncs; // save it for future reference 

    // if the plugin's major ver level is lower than the Navigator's,
    // then they are incompatible, and should return an error 
    if(HIBYTE(pFuncs->version) > NP_VERSION_MAJOR)
        return NPERR_INCOMPATIBLE_VERSION_ERROR;

    // if the Navigator's function table is smaller than the plugin expects,
    // then they are incompatible, and should return an error 
    if(pFuncs->size < sizeof NPNetscapeFuncs)
        return NPERR_INVALID_FUNCTABLE_ERROR;

    {
      void *base = &base;
      void* current_base = (void*)((long)base|0xfffc);
      
      if(valid_stack_base == NULL)
	valid_stack_base = current_base;
      // if we run again in a different thread then don't let it happen.
      if(current_base != valid_stack_base)
	return NPERR_INCOMPATIBLE_VERSION_ERROR;

      // initialize the dylan world
      if(!connection_initialized)
	{
	  ConnectionProtocol();  // for idvm listener
	  connection_initialized = 1;
	}
      if(!dylan_initialized)
	{
	  hacks__main();         // and for the dylan world
	  dylan_initialized = 1;
	}
    }
	// We have to defer this call until g_pNavigatorFunc is set
	g_pluginFuncs->javaClass	  = Private_GetJavaClass();

	// NPP_Initialize is a standard (cross-platform) initialize function.
	// I have no idea why it's only called thru this function and I don't
	// even understand the existence of this function
    return NPP_Initialize();
}

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// NP_Shutdown
//
//	called immediately before the plugin DLL is unloaded.
//	This functio shuold check for some ref count on the dll to see if it is
//	unloadable or it needs to stay in memory. 
//
NPError WINAPI NP_EXPORT 
NP_Shutdown()
{
	// why is it calling NP_Shutdown?
    NPP_Shutdown();

    g_pNavigatorFuncs = NULL;

    return NPERR_NO_ERROR;
}

//						END - PLUGIN DLL entry points   
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.

/*    NAVIGATOR Entry points    */

/* These entry points expect to be called from within the plugin.  The
   noteworthy assumption is that DS has already been set to point to the
   plugin's DLL data segment.  Don't call these functions from outside
   the plugin without ensuring DS is set to the DLLs data segment first,
   typically using the NP_LOADDS macro
*/

/* returns the major/minor version numbers of the Plugin API for the plugin
   and the Navigator
*/
void NPN_Version(int* plugin_major, int* plugin_minor, int* netscape_major, int* netscape_minor)
{
    *plugin_major   = NP_VERSION_MAJOR;
    *plugin_minor   = NP_VERSION_MINOR;
    *netscape_major = HIBYTE(g_pNavigatorFuncs->version);
    *netscape_minor = LOBYTE(g_pNavigatorFuncs->version);
}

/* causes the specified URL to be fetched and streamed in
*/
NPError NPN_GetURL(NPP instance, const char *url, const char *window)
{
    return g_pNavigatorFuncs->geturl(instance, url, window);
}

/* causes the specified URL to be fetched and streamed in
*/
NPError NPN_GetURLNotify(NPP instance, const char *url, const char *window,
			 void* notifyData)
{
    return g_pNavigatorFuncs->geturlnotify(instance, url, window, notifyData);
}


NPError NPN_PostURL(NPP instance, const char* url, const char* window, uint32 len, const char* buf, NPBool file)
{
    return g_pNavigatorFuncs->posturl(instance, url, window, len, buf, file);
}

NPError NPN_PostURLNotify(NPP instance, const char* url,
			  const char* window, uint32 len,
			  const char* buf, NPBool file,
			  void* notifyData)
{
    return g_pNavigatorFuncs->posturlnotify(instance, url, window, len, buf,
					    file, notifyData);
}


/* Requests that a number of bytes be provided on a stream.  Typically
   this would be used if a stream was in "pull" mode.  An optional
   position can be provided for streams which are seekable.
*/
NPError NPN_RequestRead(NPStream* stream, NPByteRange* rangeList)
{
    return g_pNavigatorFuncs->requestread(stream, rangeList);
}

/* Creates a new stream of data from the plug-in to be interpreted
   by Netscape in the current window.
*/
NPError NPN_NewStream(NPP instance, NPMIMEType type, 
								const char* target, NPStream** stream)
{
    return g_pNavigatorFuncs->newstream(instance, type, target, stream);
}

/* Provides len bytes of data.
*/
int32 NPN_Write(NPP instance, NPStream *stream,
                int32 len, void *buffer)
{
    return g_pNavigatorFuncs->write(instance, stream, len, buffer);
}

/* Closes a stream object.  
reason indicates why the stream was closed.
*/
NPError NPN_DestroyStream(NPP instance, NPStream* stream, NPError reason)
{
    return g_pNavigatorFuncs->destroystream(instance, stream, reason);
}

/* Provides a text status message in the Netscape client user interface
*/
void NPN_Status(NPP instance, const char *message)
{
    g_pNavigatorFuncs->status(instance, message);
}

/* returns the user agent string of Navigator, which contains version info
*/
const char* NPN_UserAgent(NPP instance)
{
    return g_pNavigatorFuncs->uagent(instance);
}

/* allocates memory from the Navigator's memory space.  Necessary so that
   saved instance data may be freed by Navigator when exiting.
*/
void* NPN_MemAlloc(uint32 size)
{
    return g_pNavigatorFuncs->memalloc(size);
}

/* reciprocal of MemAlloc() above
*/
void NPN_MemFree(void* ptr)
{
    g_pNavigatorFuncs->memfree(ptr);
}

/* private function to Netscape.  do not use!
*/
void NPN_ReloadPlugins(NPBool reloadPages)
{
    g_pNavigatorFuncs->reloadplugins(reloadPages);
}

JRIEnv* NPN_GetJavaEnv(void)
{
	return g_pNavigatorFuncs->getJavaEnv();
}

jref NPN_GetJavaPeer(NPP instance)
{
	return g_pNavigatorFuncs->getJavaPeer(instance);
}

////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
// Kludges for the vmtether ---


extern "C"
{

float
SetWindowLongHACK1(HWND hwnd, int nindex, LONG knew);

float
SetWindowLongHACK2(HWND hwnd, int nindex, float knew);

}

float
SetWindowLongHACK1(HWND hwnd, int nindex, long knew)
{
  float hack_result;
  long *hack_alias = (long *)&hack_result;


  *hack_alias = SetWindowLong(hwnd, nindex, knew);
  return(hack_result);
}


float
SetWindowLongHACK2(HWND hwnd, int nindex, float knew)
{
  float hack_result;
  long *hack_alias = (long *)&hack_result;
  long* hack_knew_alias = (long*)&knew;

  *hack_alias = SetWindowLong(hwnd, nindex, *hack_knew_alias);

   return(hack_result);
}

