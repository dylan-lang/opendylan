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

// resources
#include "test-resource.h"

//\\// DEFINE
#ifdef WIN32
    #define NP_EXPORT
#else
    #define NP_EXPORT _export
#endif



HINSTANCE application_hInstance = NULL;

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// 

HINSTANCE real_dll_handle = NULL;
DWORD last_error = 0;
LPTSTR last_error_message = NULL;
#define MAX_STR_LEN 256

BOOLEAN reload_real_dll()
{
  char lib_name_buf[MAX_STR_LEN];
  int len = LoadString(application_hInstance,
		       DLL_NAME_STRING_ID,
		       lib_name_buf,
		       MAX_STR_LEN - 1);
  if(len == 0)
    return(0);
  real_dll_handle = LoadLibrary(lib_name_buf);
  if(!real_dll_handle)
    {
      last_error = GetLastError();
      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER
		    | FORMAT_MESSAGE_IGNORE_INSERTS
		    | FORMAT_MESSAGE_FROM_SYSTEM,
		    NULL,
		    last_error,
		    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		    (LPTSTR) &last_error_message,
		    0,
		    NULL);
		    
    }
      
  
  if(real_dll_handle)
    return(1);
  else
    return(0);
}



FARPROC get_real_entry_point(LPCTSTR name)
{
  if(!real_dll_handle)
    if(!reload_real_dll())
      return(0);
  return(GetProcAddress(real_dll_handle, name));
}  

  






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
  // For now...
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
typedef NPError (WINAPI* GETENTRYFUNC)(NPPluginFuncs* pFuncs);

NPError WINAPI NP_EXPORT 
NP_GetEntryPoints(NPPluginFuncs* pFuncs)
{
  // trap a NULL ptr 
  if(pFuncs == NULL)
    return NPERR_INVALID_FUNCTABLE_ERROR;

  GETENTRYFUNC real_func
    = (GETENTRYFUNC)get_real_entry_point("NP_GetEntryPoints");
  if(!real_func)
    return(NPERR_MODULE_LOAD_FAILED_ERROR);
  else
    return((*real_func)(pFuncs));
}

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// NP_Initialize
//
//	called immediately after the plugin DLL is loaded
//
typedef NPError (WINAPI *INITFUNC)(NPNetscapeFuncs* pFuncs);


NPError WINAPI NP_EXPORT 
NP_Initialize(NPNetscapeFuncs* pFuncs)
{
  // trap a NULL ptr 
  if(pFuncs == NULL)
    return NPERR_INVALID_FUNCTABLE_ERROR;

  INITFUNC real_func = (INITFUNC)get_real_entry_point("NP_Initialize");
  if(!real_func)
    return(NPERR_MODULE_LOAD_FAILED_ERROR);
  else
    return((*real_func)(pFuncs));
}

//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\.
////\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//.
// NP_Shutdown
//
//	called immediately before the plugin DLL is unloaded.
//	This functio shuold check for some ref count on the dll to see if it is
//	unloadable or it needs to stay in memory. 
//
typedef NPError (WINAPI *SHUTDOWNFUNC)();

NPError WINAPI NP_EXPORT 
NP_Shutdown()
{
  
  SHUTDOWNFUNC real_func = (SHUTDOWNFUNC)get_real_entry_point("NP_Shutdown");
  if(!real_func)
    return(NPERR_MODULE_LOAD_FAILED_ERROR);
  else
    return((*real_func)());
}

