Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed method enumerate-resources
    (handle :: <HINSTANCE>, 
     #key database :: false-or(<resource-database>) = #f)
 => (success? :: <boolean>)
  assert(~null-handle?(handle), "Invalid handle to resource module");
  assert(database, "No database supplied");
  debug-message("The file contains the following resources:\n");
  dynamic-bind (*current-database*   = database,
		*current-type*       = #f,
		*current-type-table* = #f,
		*current-resource*   = #f)
    let success? = EnumResourceTypes(handle, EnumResTypeProc, 0);
    debug-message("Status: %=", success?);
    success?
  end
end method enumerate-resources;


define sealed method enumerate-resource-types
    (hModule :: <HANDLE>,		// module handle
     lpType  :: <LPTSTR>,		// address of resource type
     lParam  :: <lparam-type>)		// extra parameter, could be used for error checking
 => (value :: <boolean>)      
  unless (null-pointer?(lpType))
    processing-type(*current-database*, lpType);
    // Find the names of all resources of type lpType
    EnumResourceNames(hModule, lpType, EnumResNameProc, 0)
  end;
  #t
end method enumerate-resource-types;

define callback EnumResTypeProc :: <ENUMRESTYPEPROC> = enumerate-resource-types;
  

define sealed method enumerate-resource-names
    (hModule :: <HANDLE>,		// module handle
     lpType  :: <LPCTSTR>,		// address of resource type
     lpName  :: <LPTSTR>,		// address of resource name
     lParam  :: <lparam-type>)		// extra parameter, could be used for error checking
 => (value :: <boolean>)      
  unless (null-pointer?(lpName))
    store-resource-name(*current-database*, lpName);
    // Find the languages of all resources of type lpType and name lpName
    EnumResourceLanguages(hModule, lpType, lpName, EnumResLangProc, 0)
  end;
  #t
end method enumerate-resource-names;

define callback EnumResNameProc :: <ENUMRESNAMEPROC> = enumerate-resource-names;


define sealed method enumerate-resource-languages
    (hModule :: <HANDLE>,		// module handle
     lpType  :: <LPCTSTR>,		// address of resource type
     lpName  :: <LPCTSTR>,		// address of resource name
     wLang   :: <integer>,		// resource language
     lParam  :: <lparam-type>)		// extra parameter, could be used for error checking
 => (value :: <boolean>)
  let hResInfo :: <HANDLE> = 
    FindResourceEx(hModule, lpType, lpName, wLang);
  debug-message("    Language: %=", wLang);
  let resource-size = SizeofResource(hModule, hResInfo);
  debug-message("    hResInfo = %=, size = %d\n",
		pointer-address(hResInfo), resource-size);
  store-resource-details(*current-database*, hResInfo, resource-size, wLang);
  #t
end method enumerate-resource-languages;

define callback EnumResLangProc :: <ENUMRESLANGPROC> = enumerate-resource-languages;
