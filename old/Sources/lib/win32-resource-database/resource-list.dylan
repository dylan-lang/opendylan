Module:    win32-resource-database-internal 
Synopsis:  generate list of resources in an object file
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant list-debug-out = debug-out;

// this should be fluid but it doesn't work in scc
define variable *current-database* = #f;

define method ErrorHandler(text :: <string>) => ();
  debug-out( text);
  error(text);
end method ErrorHandler;

define method enumerate-resources(handle :: <HINSTANCE>, 
				  #key 
				    database :: <resource-database> = unsupplied()) 
 => ecode :: <integer>;

  let status = 0;

  if (null-handle?(handle))
    ErrorHandler("Invalid handle to resource module"); 
  end if;

  if(unsupplied?(database))
    ErrorHandler("No database supplied");
  end if;

  list-debug-out("The file contains the following resources:\n\n"); 

  *current-database* := database;

  status := 
    EnumResourceTypes(handle,	// module handle 
		      EnumTypesProc,// callback function 
		      0);	// extra parameter 
 
  list-debug-out("Status: %=\n", status);
  status;
end;

//    FUNCTION: EnumTypesFunc(HANDLE, LPSTR, LONG) 
//    PURPOSE:  Resource type callback 

define method EnumTypesFunc( 
    hModule :: <HANDLE>,	// module handle 
    lpType :: <LPTSTR>,		// address of resource type 
    lParam :: <integer>)	// extra parameter, could be 
				// used for error checking 
 => value :: <boolean>;      

  processing-type(*current-database*, lpType);
  // Find the names of all resources of type lpType. 
  EnumResourceNames(hModule, 
		    lpType, 
		    EnumNamesProc, 
		    0); 
  #t
end method EnumTypesFunc; 

//    FUNCTION: EnumNamesFunc(HANDLE, LPSTR, LPSTR, LONG) 
//    PURPOSE:  Resource name callback 

define method EnumNamesFunc( 
    hModule :: <HANDLE>,	// module handle 
    lpType :: <LPCTSTR>,	// address of resource type 
    lpName :: <LPTSTR>,		// address of resource name 
    lParam :: <integer>)	// extra parameter, could be 
				// used for error checking 
=> value :: <boolean>;      
                      
  store-resource-name(*current-database*, lpName);
  // Find the languages of all resources of type 
  // lpType and name lpName. 
 
  EnumResourceLanguages(hModule, 
			lpType, 
			lpName, 
			EnumLangsProc,
			0); 
  #t
end method EnumNamesFunc; 

//    FUNCTION: EnumLangsFunc(HANDLE, LPSTR, LPSTR, WORD, LONG) 
//    PURPOSE:  Resource language callback 

define method EnumLangsFunc( 
    hModule :: <HANDLE>,	// module handle 
    lpType :: <LPCTSTR>,	// address of resource type 
    lpName :: <LPCTSTR>,	// address of resource name 
    wLang :: <integer>,		// resource language 
    lParam :: <integer>)	// extra parameter, could be 
				//  used for error checking 
=> value :: <boolean>;     

  let hResInfo :: <HANDLE> = 
    FindResourceEx( hModule,  lpType,  lpName,  wLang); 
 
  list-debug-out("\t\tLanguage: %=\n", as(<integer>, wLang)); 
 
  let size-of-resource = SizeofResource( hModule,  hResInfo);
  list-debug-out("\t\thResInfo == %x,  Size == %d\n\n", 
		 as(<integer>, hResInfo.pointer-address), size-of-resource);

  store-resource-details(*current-database*, hResInfo, size-of-resource, wLang);
  #t
end method EnumLangsFunc; 
