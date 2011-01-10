Module:    OLE-Automation
Synopsis:  Create a "Type Library" from one or more ITypeInfo interfaces.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method create-type-library ( file-name :: <string>, object,
				   #key doc-string, name, locale, uuid,
				        major, minor = 0,
				        help-file, help-context, flags
				     ) => ()

  let ole-file-name = as(<LPOLESTR>, file-name);
  let ( status :: <HRESULT>, lib-creator :: <LPCREATETYPELIB>) =
    CreateTypeLib($SYS-WIN32, ole-file-name);
  check-ole-status(status, "CreateTypeLib", $null-interface, file-name);
  unless ( ole-file-name == file-name )
    destroy(ole-file-name);
  end unless;
  if ( uuid )
    ICreateTypeLib/SetGuid(lib-creator, uuid);
  end if;
  set-lib-string(lib-creator, ICreateTypeLib/SetName, name);
  set-lib-string(lib-creator, ICreateTypeLib/SetDocString, doc-string);
  if ( locale )
    ICreateTypeLib/SetLcid(lib-creator, locale);
  else
    locale := $default-locale;
  end if;
  if ( major )
    ICreateTypeLib/SetVersion(lib-creator, major, minor);
  end if;
  set-lib-string(lib-creator, ICreateTypeLib/SetHelpFileName, help-file);
  if ( help-context )
    ICreateTypeLib/SetHelpContext(lib-creator, help-context);
  end if;
  if ( flags )
    ICreateTypeLib/SetLibFlags(lib-creator, flags);
  end if;
  let ref-table :: <table> = make(<table>);
  add-object-to-library(lib-creator, object, locale, ref-table);
  do(Release, ref-table);

  check-ole-status(
    ICreateTypeLib/SaveAllChanges(lib-creator),
		   "SaveAllChanges", lib-creator);

  // documentation doesn't make clear whether Release should be called or
  // not, but it seems to work.
  unless (zero?(Release(lib-creator)))
    error("invalid Release of ICreateTypeLib");
  end;

end create-type-library;

define method set-lib-string (lib-creator :: <LPCREATETYPELIB>,
			      function :: <function>, string ) => ()
  if ( string & ~ empty?(string) )
    let ole-string = as(<LPOLESTR>, string);
    let status = function(lib-creator, ole-string);
    if ( status )
      check-ole-status(status, function, lib-creator, string);
    end if;
    unless ( ole-string == string )
      destroy(ole-string);
    end unless;
  end if;
end set-lib-string;

define method add-object-to-library ( lib-creator :: <LPCREATETYPELIB>,
				     objects :: <sequence>,
				     locale, ref-table) => ()
  for ( object :: <Interface> in objects )
    add-object-to-library(lib-creator, object, locale, ref-table);
  end for;
end method;

define method add-object-to-library ( lib-creator :: <LPCREATETYPELIB>,
				     object :: <Interface>,
				     locale,
				     ref-table ) => ()
  let ( status, typeinfo ) = QueryInterface(object, $IID-ITypeInfo);
  if ( SUCCEEDED?(status) )
    let type-creator = create-type-info(lib-creator, typeinfo, ref-table);
    // Doc doesn't say whether Release should be called or not, but if it
    // isn't, we are left with an extra ref count on the lib-creator.
    let rel-count = Release(type-creator);
    assert( zero?(rel-count) );
    Release(typeinfo);
  else
    let ( status, dispatch ) = QueryInterface(object, $IID-IDispatch);
    if ( null?(dispatch) )
      error("%= is neither ITypeInfo nor IDispatch", object);
    else
      let ( status :: <HRESULT>, count :: <integer> ) =
	IDispatch/GetTypeInfoCount(dispatch);
      for ( index from 0 below count )
	let ( status, typeinfo ) =
	  IDispatch/GetTypeInfo(dispatch, index, locale);
	if ( SUCCEEDED?(status) )
	  let type-creator =
	    create-type-info(lib-creator, typeinfo, ref-table);
	  let rel-count = Release(type-creator);
	  assert( zero?(rel-count) );
	  Release(typeinfo);
	end if;
      end for;
      Release(dispatch);
    end if;
  end if;
end method add-object-to-library;



define macro checked-call
 { checked-call(?function:name(?args:*)) }
    =>
 { let status = ?function(?args);
   if ( FAILED?(status) )
     // ole-cerror(status, ?"function", ?args);
     ole-warning(status, ?"function", ?args);
   end if;
  }
end macro;

define method ole-warning( status :: <HRESULT>, context, instance,
			  #rest args );
  if ( FAILED?(status) )
    with-c-string (str = format-to-string("%s\n",
					  make-ole-error(status, context, instance, args)))
      OutputDebugString(str);
    end;
  end if;
end;


define method create-type-info ( lib-creator :: <LPCREATETYPELIB>,
				 typeinfo :: <LPTYPEINFO>,
				 ref-table :: <table> )
 => (type-creator :: <LPCREATETYPEINFO>);

  // Information about the TypeInfo as a whole

  let ( status :: <HRESULT>, Name :: <BSTR>, DocString :: <BSTR>,
        HelpContext :: <integer>, HelpFile :: <BSTR> ) =
    ITypeInfo/GetDocumentation(typeinfo, $MEMBERID-NIL);
  let ( status, attributes :: <LPTYPEATTR> ) = ITypeInfo/GetTypeAttr(typeinfo);
  check-ole-status(status, "GetTypeAttr", typeinfo);
  if ( null-pointer?(Name) )
    Name := copy-as-BSTR("[unnamed]"); // arbitrary dummy value
  end if;
  // Until we have support for directly displaying Unicode strings:
  let debug-name :: <byte-string> = as(<byte-string>, Name);
  let ( status, type-creator ) =
    ICreateTypeLib/CreateTypeInfo(lib-creator, Name,
				  attributes.typekind-value);
  if ( FAILED?(status) )
    if ( status = $TYPE-E-NAMECONFLICT ) // user error
      error("Duplicate TypeInfo name: \"%s\"", debug-name);
    else
      ole-error(status, "CreateTypeInfo", lib-creator, debug-name);
    end if;
  end if;
  unless ( empty?(DocString) )
    ICreateTypeInfo/SetDocString(type-creator, DocString);
  end;
  unless ( zero?(HelpContext) )
    ICreateTypeInfo/SetHelpContext(type-creator, HelpContext);
  end;
  unless ( empty?(HelpFile) )
    set-lib-string(lib-creator, ICreateTypeLib/SetHelpFileName, HelpFile);
  end;
  ICreateTypeInfo/SetVersion(type-creator, attributes.wMajorVerNum-value,
			     attributes.wMinorVerNum-value);
  let iid = attributes.guid-value;
  unless ( null?(iid) )
    if ( iid = $IID-IDispatch )
      // If we allowed this, RegisterTypeLib would over-write the "Interface"
      // registry entry for the standard interface "IDispatch".
      // (Ref support call #458)
      error("Interface %= needs a unique UUID for type library registration.",
	    debug-name);
    end if;
    ICreateTypeInfo/SetGuid(type-creator, iid);
  end unless;
  let first-component :: <integer> = 0;
  let flags :: <integer> = attributes.wTypeFlags-value;
  unless ( zero?(flags) )
    ICreateTypeInfo/SetTypeFlags(type-creator, flags);
/* // don't do this yet because it is getting "Duplicate TypeInfo name"
    if ( ~ zero?(logand(flags, $TYPEFLAG-FDUAL)) )
      first-component := -1;
    end if;
*/
  end unless;
  /* // only for TYPEKIND = TKIND_RECORD
  ICreateTypeInfo/SetAlignment(type-creator, attributes.cbAlignment-value);
  */

  let restart-args =
    vector(format-string: "Continue validating other members of type \"%s\"",
	   format-arguments: vector(debug-name));

  // Information for each component interface of a coclass
  // or the base class of an interface

  for ( component-index from first-component
	 below attributes.cImplTypes-value )
    block()
      let ( status1, typeref ) =
	ITypeInfo/GetRefTypeOfImplType(typeinfo, component-index);
      check-ole-status(status1, "GetRefTypeOfImplType", typeinfo);
      let ( status2, component-typeinfo :: <LPTYPEINFO> ) =
	ITypeInfo/GetRefTypeInfo(typeinfo, typeref);
      check-ole-status(status2, "GetRefTypeInfo", typeinfo);
      component-typeinfo := dylan-interface(component-typeinfo);
      let external-typeinfo :: <LPTYPEINFO> =
	if ( instance?(component-typeinfo, <Dylan-Type-Info>) )
	  // Need to convert the internal representation to an external
	  // representation in the type library.
	  let ext-type =
	    element(ref-table, component-typeinfo, default: #f);
	  if ( ext-type == #f )
	    // Not already converted.
	    // For standard interfaces, try to use the official definitions. 
	    if ( component-typeinfo == $IUnknown-type-info )
	      ext-type := get-standard-typeinfo(ref-table, component-typeinfo,
						$IID-IUnknown);
	    elseif ( component-typeinfo == $IDispatch-type-info )
	      ext-type := get-standard-typeinfo(ref-table, component-typeinfo,
						$IID-IDispatch);
	    end if;
	    if ( ext-type == #f )
	      // Otherwise create a new typeinfo in the library.
	      let component-type-creator =
		create-type-info(lib-creator, component-typeinfo, ref-table);
	      let ( status, created-type-info ) =
		QueryInterface(component-type-creator, $IID-ITypeInfo);
	      check-ole-status(status, "ITypeInfo of ICreateTypeInfo",
			       component-type-creator);
	      element(ref-table, component-typeinfo) := created-type-info;
	      Release(component-type-creator);
	      ext-type := created-type-info;
	    end if;
	  end if;
	  ext-type
	else
	  component-typeinfo
	end if;
      let ( status, reftype ) =
	ICreateTypeInfo/AddRefTypeInfo(type-creator, external-typeinfo);
      check-ole-status(status, "AddRefTypeInfo", type-creator,
		       external-typeinfo);
      let status =
	ICreateTypeInfo/AddImplType(type-creator, component-index, reftype);
      check-ole-status(status, "AddImplType", type-creator, reftype);
      let ( status, impltypeflags ) =
	ITypeInfo/GetImplTypeFlags(typeinfo, component-index);
      if ( SUCCEEDED?(status) )
	let status = ICreateTypeInfo/SetImplTypeFlags(type-creator,
						      component-index,
						      impltypeflags);

	unless ( zero?(impltypeflags) )
	  check-ole-status(status, "SetImplTypeFlags", type-creator);
	end;
      end if;
      unless ( component-typeinfo == external-typeinfo )
	Release(component-typeinfo);
      end unless;
    exception (<simple-restart>, init-arguments: restart-args)
    end block;
  end for;

  // Information for each member function

  for (function-index from 0 below attributes.cFuncs-value )
    block()
      let ( status, fd :: <LPFUNCDESC> ) =
	ITypeInfo/GetFuncDesc(typeinfo, function-index);
      check-ole-status(status, "GetFuncDesc", typeinfo, function-index);
      checked-call(ICreateTypeInfo/AddFuncDesc(type-creator, function-index, fd));
      let function-member-id :: <disp-id> = fd.memid-value;
      begin
	let max-n :: <integer> = fd.cParams-value + 2;
	let names-array = make(<LPBSTR>, element-count: max-n);
	let ( status :: <HRESULT>, n-names :: <integer> ) =
	  ITypeInfo/GetNames(typeinfo, function-member-id,
			     names-array, max-n);
	unless ( FAILED?(status) )
	  checked-call(
	  ICreateTypeInfo/SetFuncAndParamNames
	    (type-creator, function-index,
	     pointer-cast(<LPLPOLESTR>, names-array),
	     n-names));
	  for ( i from 0 below n-names )
	    destroy(pointer-value(names-array, index: i));
	  end for;
	end unless;
	destroy(names-array);
      end;
      let ( doc-status :: <HRESULT>, fn-name :: <BSTR>, fn-doc :: <BSTR>,
	   fn-help-context :: <integer>, fn-help-file :: <BSTR> ) =
	ITypeInfo/GetDocumentation(typeinfo, function-member-id);
      unless ( FAILED?(doc-status) )


	unless ( empty?(fn-doc) )
	  checked-call(
	  ICreateTypeInfo/SetFuncDocString(type-creator,
					   function-index, fn-doc));
	end unless;
	unless ( zero?(fn-help-context) )
	  ICreateTypeInfo/SetFuncHelpContext(type-creator, function-index,
					     fn-help-context);
	end unless;
	destroy(fn-name);
	destroy(fn-doc);
	destroy(fn-help-file);
      end unless;

      ITypeInfo/ReleaseFuncDesc(typeinfo, fd);
    exception (<simple-restart>, init-arguments: restart-args)
    end block;
  end for;

  // Information for each property in a dispatch interface

  for ( var-index from 0 below attributes.cVars-value )
    block ()
      let ( status, vd :: <LPVARDESC> ) =
	ITypeInfo/GetVarDesc(typeinfo, var-index);
      if ( SUCCEEDED?(status) )
	status := ICreateTypeInfo/AddVarDesc(type-creator, var-index, vd);
	if ( FAILED?(status) )
	  ole-cerror(status, "ICreateTypeInfo/AddVarDesc",
		     type-creator, var-index, vd);
	end if;
	let ( doc-status :: <HRESULT>, var-name :: <BSTR>, var-doc :: <BSTR>,
	      var-help-context :: <integer>, var-help-file :: <BSTR> ) =
	  ITypeInfo/GetDocumentation(typeinfo, vd.memid-value);
	unless ( FAILED?(doc-status) )
	  ICreateTypeInfo/SetVarName(type-creator, var-index, var-name);
	  unless ( empty?(var-doc) )
	    ICreateTypeInfo/SetVarDocString(type-creator, var-index, var-doc);
	  end unless;
	  unless ( zero?(var-help-context) )
	    ICreateTypeInfo/SetVarHelpContext(type-creator, var-index,
					       var-help-context);
	  end unless;
	  destroy(var-name);
	  destroy(var-doc);
	  destroy(var-help-file);
	end unless;
	ITypeInfo/ReleaseVarDesc(typeinfo, vd);
      end if;
    exception (<simple-restart>, init-arguments: restart-args)
    end block;
  end for;

  // base type of a dispatch interface

/*  // This isn't quite right yet -- the inherited type needs to be
    // recorded in the type library before it can be be referenced here.
    // Specifically, AddRefTypeInfo is going to invoke the GetContainingTypeLib
    // method, which doesn't yet work for this case.  But we don't really
    // need to put this in the type library anyway, so leave it out for now.
    //		-- DNG 10/28/97				???
  let inherit = typeinfo.inherited-typeinfo;
  if (inherit)
    let ( status, reftype ) =
      ICreateTypeInfo/AddRefTypeInfo(type-creator, inherit);
    check-ole-status(status, "AddRefTypeInfo", type-creator, inherit);
    let status =
      ICreateTypeInfo/AddImplType(type-creator, 0, reftype);
    check-ole-status(status, "AddImplType", type-creator, reftype);
  end if;
*/

/*  others that might need to be used later:
  ICreateTypeInfo/SetTypeDescAlias (type-creator, ptdescAlias);
  ICreateTypeInfo/DefineFuncAsDllEntry (type-creator, index, szDllName,
					szProcName);
*/

  // Although from the way it is documented, it would seem that `LayOut'
  // doesn't have anything to do for a dispinterface, if it isn't called,
  // then when trying to read the resulting type library,
  // ITypeInfo::GetTypeAttr fails with TYPE_E_INVALIDSTATE = 
  // "Invalid forward reference, or reference to uncompiled type."
  let layout-status = ICreateTypeInfo/LayOut(type-creator);
  check-ole-status(layout-status, "LayOut", type-creator);

  ITypeInfo/ReleaseTypeAttr(typeinfo, attributes);
  destroy(Name);
  destroy(DocString);
  destroy(HelpFile);

  type-creator
end create-type-info;

define function get-standard-typeinfo(ref-table :: <table>,
				      dylan-typeinfo :: <Dylan-Type-Info>,
				      interface-id :: <REFIID>)
 => ( typeinfo :: false-or(<LPTYPEINFO>) )
  let $std-lib-name = "STDOLE32.TLB";
  // Check the cache first.
  let standard-library = element(ref-table, $std-lib-name, default: #f);
  if ( standard-library == #f )
    // Try to open the standard OLE type library by its file name.
    let file-name = as(<LPOLESTR>, $std-lib-name);
    let ( status, interface ) = LoadTypeLib(file-name);
    destroy(file-name);
    if ( SUCCEEDED?(status) )
      standard-library := interface;
    else
      // If it couldn't find the file, maybe it can find it from the registry.
      let lib-id :: <REFGUID> =
	make-GUID(#x00020430, 0, 0, #xC0, 0, 0, 0, 0, 0, 0, #x46);
      let ( status, interface ) = LoadRegTypeLib(lib-id, 2, 0, 0);
      destroy(lib-id);
      if ( SUCCEEDED?(status) )
	standard-library := interface
      else
	standard-library := $null-interface;
      end if;
    end if;
    // Cache it for next time and to call Release later.
    element(ref-table, $std-lib-name) := standard-library;
  end if;
  if ( null-pointer?(standard-library) )
    #f
  else
    // Find the desired interface description in the type library.
    let (status :: <HRESULT>, standard-typeinfo :: <Interface>) =
      ITypeLib/GetTypeInfoOfGuid(standard-library, interface-id);
    if ( SUCCEEDED?(status) )
      element(ref-table, dylan-typeinfo) := standard-typeinfo;
      standard-typeinfo
    else
      #f
    end if
  end if
end get-standard-typeinfo;


define method fetch-type-library (typeinfo :: <Dylan-Type-Info>,
				  guid :: <REFGUID>,
				  #key module-handle,
				  force? :: <boolean> = #f)
 => (typelib :: <LPTYPELIB>)

  if ( guid == $default-type-id )
    error("no class ID for %=", typeinfo);
  end if;
  let ( status :: <HRESULT>, typelib :: <LPTYPELIB> ) =
    if ( force? )
      values($E-ABORT, $null-interface)
    else
      LoadRegTypeLib(guid, typeinfo.major-version, typeinfo.minor-version,
		     typeinfo.type-info-locale);
    end if;
  if ( FAILED?(status) )
    // If the type library is not already set up, then create and
    // register it.
    AddRef(typeinfo); // force OLE initialization
    let ( path-buf, path-length ) = make-typelib-path(typeinfo, module-handle);
    let file-name = copy-as-BSTR(path-buf);
    let help-dir = if ( empty?(typeinfo.help-file) )
		     $NULL-OLESTR
		   else
		     let i :: <integer> = path-length - 5;
		     while ( i > 0 & path-buf[i] ~= '\\' )
		       i := i - 1;
		     end while;
		     path-buf[i] := '\0';
		     as(<LPOLESTR>, path-buf)
		   end if;
    create-type-library(file-name, typeinfo,
			doc-string: typeinfo.doc-string,
			name: typeinfo.interface-name,
			locale: typeinfo.type-info-locale,
			uuid: guid,
			// flags: libflags,
			major: typeinfo.major-version,
			minor: typeinfo.minor-version,
			help-file: typeinfo.help-file,
			help-context: typeinfo.help-context);
    let ( status :: <HRESULT>, lib :: <LPTYPELIB> ) = LoadTypeLib(file-name);
    if ( FAILED?(status) )
      ole-error(status, "LoadTypeLib", $null-interface, file-name);
    else
      typelib := lib;
      let reg-status = RegisterTypeLib(typelib, file-name, help-dir);
      check-ole-status(reg-status, "RegisterTypeLib", typelib, file-name);
    end if;
    unless ( help-dir == path-buf )
      destroy(help-dir);
    end unless;
    destroy(file-name);
    destroy(path-buf);
    Release(typeinfo);
  end if;
  typelib
end fetch-type-library;

define function make-typelib-path ( typeinfo :: <Dylan-Type-Info>,
				    module-handle :: false-or(<HMODULE>) )
 => (path-buf :: <LPTSTR>, // caller should call `destroy' on result
     path-length :: <integer>)

  let buf-size = $MAX-PATH;
  let path-buf :: <LPTSTR> = make(<LPTSTR>, size: buf-size + 1);
  let hModule :: <HMODULE> = module-handle | $NULL-HINSTANCE;
  let path-length :: <integer> =
    GetModuleFileName(hModule, path-buf, buf-size);
  if ( path-length <= 0 )
    error("GetModuleFileName error %d", GetLastError());
  end if;
  let suffix = format-to-string("-%d-%d-%x.TLB",
				typeinfo.major-version,
				typeinfo.minor-version,
				typeinfo.type-info-locale);
  let i :: <integer> = path-length - 1;
  while ( i > 0 & path-buf[i] ~= '.' & path-buf[i] ~= '\\' )
    i := i - 1;
  end while;
  if ( path-buf[i] ~= '.' ) i := path-length; end if;
  for ( c in suffix )
    path-buf[i] := c;
    i := i + 1;
  end for;
  path-buf[i] := '\0';
  values(path-buf, path-length)
end make-typelib-path;
