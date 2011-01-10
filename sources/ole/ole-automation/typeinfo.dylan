Module:    OLE-Automation
Synopsis:  A Dylan implementation of the ITypeInfo interface.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <U8> = limited(<integer>, min: 0, max: #xFF);
define constant <U16> = limited(<integer>, min: 0, max: #xFFFF);

//  ====  ITypeInfo implementation  ====

// Note:  $LOCALE-USER-DEFAULT indicates the default for doing a lookup,
// but the following is the default for implementing a type.
define constant $default-locale =
  MAKELCID(MAKELANGID($LANG-NEUTRAL,$SUBLANG-NEUTRAL),$SORT-DEFAULT);

define constant $default-type-id = $IID-IDispatch;

define COM-interface <Dylan-Type-Info> ( <ITypeInfo> )

  slot guid-value :: <REFGUID> = $default-type-id; // init-keyword: uuid:

  // The following slots are so far only used for ITypeInfo/GetTypeAttr:
  slot type-info-locale :: <integer> = $default-locale, init-keyword: locale:;
  constant slot major-version :: <U16> = 0, init-keyword: major-version:;
  constant slot minor-version :: <U16> = 0, init-keyword: minor-version:;

  // currently used only for GetDocumentation:
  constant slot interface-name :: <string> = $NULL-OLESTR,
    init-keyword: name:;
  constant slot doc-string :: <string> = $NULL-OLESTR,
    init-keyword: documentation:;
  constant slot help-context :: <integer> = 0,
    init-keyword: help-context:;
  slot help-file :: <string> = $NULL-OLESTR,
    init-keyword: help-file:;

  slot containing-coclass = #f;
end <Dylan-Type-Info>;

define method initialize (this :: <Dylan-Type-Info>, #rest args,
			  #key inherit = #f,
			  help-file: help-file-arg = unsupplied(),
			  locale = unsupplied(),
			  uuid = unsupplied(),
			  #all-keys) => ();
  next-method();
  if (supplied?(uuid))
    // allow specification as either a structure or string.
    this.guid-value := as(<REFGUID>, uuid);
  end if;
  if (inherit)
    let inherit :: <Dylan-Type-Info> = inherit;
    if (unsupplied?(help-file-arg))
      this.help-file := inherit.help-file;
    end if;
    if (unsupplied?(locale))
      this.type-info-locale := inherit.type-info-locale;
    end if;
    if (unsupplied?(uuid))
      this.guid-value := inherit.guid-value;
    end if;
  end if;
end method initialize;

define generic inherited-typeinfo (this :: <LPTYPEINFO>)
 => (inherit :: false-or(<LPTYPEINFO>));

define method inherited-typeinfo (this :: <Dylan-Type-Info>)
 => (inherit :: singleton(#f));
  #f
end method inherited-typeinfo;

define COM-interface <Disp-Type-Info> (<Dylan-Type-Info>)
  // Sequence of <function-description>
  slot local-functions :: <simple-vector> = #[];
  slot local-variables :: <simple-vector> = #[];
  constant slot inherited-typeinfo :: false-or(<Disp-Type-Info>) = #f,
    init-keyword: inherit:;
  // lookup table for stock properties:
  constant slot dispatch-table :: <object-table> = make(<object-table>);
  slot nesting-level :: <integer> = -1; // set in initialize method.
end <Disp-Type-Info>;

define constant $unknown-disp-id = $DISPID-UNKNOWN;

define sealed abstract primary class <member-description> ( <object> )
  constant slot member-name :: <string>, required-init-keyword: name:;
  slot member-disp-id :: <disp-id> = $unknown-disp-id,
    init-keyword: disp-id:;

  // used only for GetDocumentation:
  constant slot doc-string :: <string> = $NULL-OLESTR,
    init-keyword: documentation:;
  constant slot help-context :: <integer> = 0,
    init-keyword: help-context:;
  // Note: help-file is not an option at this level; everything in a
  //	type library shares the same help file.
end class;

define method initialize (this :: <Disp-Type-Info>,
			  #key methods = #[], properties = #[],
			  #all-keys) => ();
  next-method();
  this.local-functions := as(<simple-vector>, methods);
  this.local-variables := as(<simple-vector>, properties);
  if (~this.inherited-typeinfo)
    // IDispatch nesting level is 1 (IUnknown is 0, but we don't get that far)
    this.nesting-level := 1;
  else
    this.nesting-level := 1 + this.inherited-typeinfo.nesting-level;
  end if;
  // Assign disp-id values where not specified by user
  assign-disp-ids(this.local-functions,
		  make-dispid(offset: 0,
			      nesting-level: this.nesting-level,
			      function?: #t),
		  this.dispatch-table);
  assign-disp-ids(this.local-variables,
		  make-dispid(offset: 0,
			      nesting-level: this.nesting-level,
			      function?: #f),
		  this.dispatch-table);
end method initialize;

define function assign-disp-ids (members :: <simple-vector>,
				 first-member-id :: <disp-id>,
				 dispatch-table :: <object-table>)
  for (member :: <member-description> in members,
       member-id :: <disp-id> = first-member-id then \%+(member-id, 1))
    let user-id :: <disp-id> = member.member-disp-id;
    if (user-id == $unknown-disp-id)
      member.member-disp-id := member-id;
    elseif (user-id ~= member-id)
      assert(user-id <= 0,
	     "Non-negative explicit dispids not allowed, in: %s", member);
      dispatch-table[user-id] := member;
    end if;
  end for;
end assign-disp-ids;


// See documentation of the "DISPID" type in MSDN.
define inline function make-dispid (#key offset :: <U16>,
				         nesting-level :: <U8>,
				         function? :: <boolean> = #f)
 => (dispid :: <machine-word>)
  let hi :: <integer> = logior(nesting-level,
			       if (function?) #x6000 else #x4000 end);
  %logior(offset, %shift-left(as(<machine-word>, hi), 16));
end;

define inline function dispid-offset (dispid :: <machine-word>)
 => (offset :: <U16>)
  as(<integer>, %logand(#xFFFF, dispid))
end;

define inline function dispid-nesting-level (dispid :: <machine-word>)
 => (nesting-level :: <U8>)
  logand(#x3F, as(<integer>, u%shift-right(dispid, 16)))
end;

define inline function dispid-function? (dispid :: <machine-word>)
 => (function? :: <boolean>)
  %logbit?(29, dispid)
end;

define constant $href-inherited = #x7000; // arbitrary offset for validation

define method ITypeInfo/GetRefTypeOfImplType(this :: <Dylan-Type-Info>,
					     index :: <integer> )
 => (status :: <HRESULT>, reftype :: <integer>);
  if ( index = 0 & ~ null?(this.inherited-typeinfo) )
    values( $S-OK, $href-inherited )
  else
    values( $TYPE-E-ELEMENTNOTFOUND, $bogus-index )
  end if
end;

define method ITypeInfo/GetImplTypeFlags(this :: <Dylan-Type-Info>,
					 index :: <integer>)
 => (status :: <HRESULT>, impltypeflags :: <integer>);
  if ( index = 0 & ~ null?(this.inherited-typeinfo) )
    values( $S-OK, 0 ) // no clear what flags are relevant here
  else
    values( $TYPE-E-ELEMENTNOTFOUND, 0 )
  end if
end;

define method ITypeInfo/GetRefTypeInfo(this :: <Dylan-Type-Info>,
				       hreftype :: <object>)
  => (status :: <HRESULT>, ptinfo :: <Interface>);
  if ( hreftype = $href-inherited )
    let typeinfo = this.inherited-typeinfo;
    IUnknown/AddRef(typeinfo);
    values( $S-OK, typeinfo )
  else
    values( $E-INVALIDARG, $NULL-interface )
  end if
end;


//  ====  representation of methods of interface  ====

define sealed primary class <function-description> ( <member-description> )
  constant slot function-object :: <function>,
    required-init-keyword: function:;

  constant slot argument-names :: <sequence> = #(),
    init-keyword: argument-names:;
  slot argument-types :: <sequence> = #(),
    init-keyword: argument-types:;
  slot result-type :: false-or(<ole-type>) = #f,
    init-keyword: result-type:;
  constant slot scodes :: false-or(<sequence>) = #f,
    init-keyword: scodes:; // Legal SCODES for the function.
  constant slot flags :: <integer> = 0,	// logior of $FUNCFLAG-...
    init-keyword: flags:;
  // This is #f if function has a REST arg.
  slot function-keywords :: false-or(<sequence>) = #[];
end class;

define constant $wrong-number-message =
  "Wrong number of argument %s for describing function \"%s\"";

define method initialize (this :: <function-description>,
			  #key keywords = unsupplied(),
			       result-type: result = unsupplied(),
			  #all-keys)
  next-method();
  let function :: <function> = this.function-object;
  let (number-required :: <U16>, has-rest? :: <boolean>, keys) =
    function-arguments(function);
  this.function-keywords
    := if (supplied?(keywords))
	 assert(has-rest? | keys == #"all" |
		  (keys & every?(rcurry(member?, keys), keywords)),
		"Invalid keywords %s for %s", keywords, function);
	 keywords
       elseif (keys == #"all" | (has-rest? & keys == #f))
	 // #all-keys or #rest with no #key.
	 #f
       else
	 keys | #[]
       end;
  let numkeys :: <U16> = size(this.function-keywords | #[]);
/*
  if ( has-rest? & zero?(numkeys) )
    // A rest argument is represented in the DISPPARAMS structure as simply
    // the last positional argument, whose value must be a 1-dimensional array.
    // It isn't counted as either a positional or optional argument, so
    // nothing really needs to be done with it here.
  end if;
*/
  let total-args :: <U16> = number-required - 1 + numkeys;
  if (empty?(this.argument-types) & total-args > 0)
    // TODO: what about keyword args?
    // TODO: This is usually bogus, because the specializers of a generic
    // function are <object> unless otherwise declared, but the user is
    // probably expecting us to use the specializers of his *method*...
    this.argument-types := copy-sequence(function-specializers(function),
					 start: 1);
  end if;
  assert(size(this.argument-types) = total-args,
	 $wrong-number-message, "types", this.member-name);
  assert(size(this.argument-names) = total-args,
	 $wrong-number-message, "names", this.member-name);
  unless (supplied?(result))
    // TODO: This is usually bogus, because the return types of a generic
    // function are #rest <object> unless otherwise declared, but the user is
    // probably expecting us to use the return values of his *method*...
    let (return-types, rest-type) = function-return-values(function);
    this.result-type := if (size(return-types) < 1)
			  rest-type // #f if none, which is the right thing.
			else
			  return-types[0]
			end;
  end unless;
end method initialize;


define method ITypeInfo/GetFuncDesc (this :: <Disp-Type-Info>,
				     index :: <integer>)
 => (status :: <HRESULT>, fd :: <LPFUNCDESC> );
  if (index < 0 | index >= size(this.local-functions))
    values($E-INVALIDARG, null-pointer(<LPFUNCDESC>))
  else
    let func :: <function-description> = this.local-functions[index];
    values($S-OK,
	   make-funcdesc(func.member-disp-id,
			 func.argument-types,
			 func.function-keywords &
			   func.function-keywords.size,
			 func.result-type, func.scodes, func.flags));
  end if
end method ITypeInfo/GetFuncDesc;

define constant $phony-vtbl-offset = -77; // arbitrary value for validation

define method make-funcdesc (memid :: <disp-id>, // Function member ID
			     argument-types :: <sequence>,
			     number-optional :: false-or(<integer>),
			     result-type :: false-or(<ole-type>),
			     scodes :: false-or(<sequence>), // SCODES returned
			     flags :: <integer>) // logior of $FUNCFLAG-...
 => result :: <LPFUNCDESC>;
  let fd :: <LPFUNCDESC> = make(<LPFUNCDESC>);
  fd.memid-value := memid;
  let num-codes :: <integer> = if (scodes) size(scodes) else -1 end;
  fd.cScodes-value := num-codes;
  fd.lprgscode-value := 
    if (num-codes > 0)
      let scp = make(<C-HRESULT*>, element-count: num-codes);
      for (i from 0 below num-codes)
	pointer-value(scp, index: i) := scodes[i];
      end for;
      scp
    else
      null-pointer(<C-HRESULT*>);
    end if;

  let num-argument-types :: <integer> = argument-types.size;
  fd.cParams-value := num-argument-types;
  let num-opts :: <integer> = number-optional | -1; // special value indicating a "rest" arg
  fd.cParamsOpt-value := num-opts;
  if (empty?(argument-types))
    fd.lprgelemdescParam-value := null-pointer(<LPELEMDESC>);
  else
    let elems :: <LPELEMDESC> =
      make(<LPELEMDESC>, element-count: argument-types.size);
    for (i from 0 by 1, argtype in argument-types)
      let elem :: <LPELEMDESC> = pointer-value-address(elems, index: i);
      let tdesc = elem.tdesc-value;
      let direction-flags :: <U16> = set-typedesc(argtype, tdesc);
      elem.idldesc-value.wIDLFlags-value := direction-flags;
    end for;
    fd.lprgelemdescParam-value := elems;
  end if;

  fd.funckind-value := $FUNC-DISPATCH;
  fd.invkind-value  := $INVOKE-FUNC;
  fd.callconv-value := 0; // phony value; shouldn't be used with $FUNC-DISPATCH
  fd.oVft-value := $phony-vtbl-offset; // not used with $FUNC-DISPATCH

  let resptr = fd.elemdescFunc-value.tdesc-value;
  if (result-type == #f)
    resptr.vt-value := $VT-VOID; // no result value
  else
    set-typedesc(result-type, resptr);
  end if;
  fd.elemdescFunc-value.idldesc-value.wIDLFlags-value := $IDLFLAG-FRETVAL;
  fd.wFuncFlags-value := flags;
  fd
end method make-funcdesc;

define method ITypeInfo/ReleaseFuncDesc (this :: <Disp-Type-Info>,
					 pfuncdesc :: <LPFUNCDESC>) => ();
  if (pfuncdesc.oVft-value ~= $phony-vtbl-offset)
    error("Invalid FUNCDESC");	// ???
  else
    if (pfuncdesc.cScodes-value > 0)
      destroy(pfuncdesc.lprgscode-value);
    end if;
    let elems = pfuncdesc.lprgelemdescParam-value;
    unless (null-pointer?(elems)) // this test may not be necessary ???
      destroy(elems);
    end;
    destroy(pfuncdesc);
  end if;
  values()
end method ITypeInfo/ReleaseFuncDesc;


define constant $default-data-type = <object>;

define sealed abstract primary class <data-description> (<member-description>)
  slot data-type :: <ole-type> = $default-data-type, init-keyword: type:;
end class;

// Dummy method to simplify ITypeInfo/GetNames
define method argument-names(this :: <member-description>); #[] end;

define sealed primary class <variable-description> (<data-description>)
  constant slot variable-getter :: false-or(<function>) = #f,
    init-keyword: getter:;
  constant slot variable-setter :: false-or(<function>) = #f,
    init-keyword: setter:;
end class;

define method initialize (this :: <variable-description>,
			  #key type = unsupplied(),
			  #all-keys)
 => ();
  next-method();
  if (unsupplied?(type) & this.variable-getter)
    let return-types :: <sequence> =
      function-return-values(this.variable-getter);
    this.data-type := if (empty?(return-types))
			$default-data-type
		      else
			return-types[0]
		      end;
  end if;
  values()
end method initialize;

/* // someday may need to add something like this for OLE control support:
define class <bindable-variable-description> (<variable-description>)
  slot flags :: <integer> = 0,	// logior of $VARFLAG-...
    init-keyword: flags:;
end class;
*/

define sealed primary class <constant-description> (<data-description>)
  constant slot constant-value :: <object>, required-init-keyword: value:;
end class;

define method initialize (this :: <constant-description>, #key, #all-keys)
 => ();
  next-method();
  if (this.data-type == $default-data-type)
    this.data-type := object-class(this.constant-value);
  end if;
  values()
end method initialize;


define method ITypeInfo/GetVarDesc (typeinfo :: <Dylan-Type-Info>,
				    index :: <integer>)
 => (status :: <HRESULT>, vd :: <LPVARDESC>);
  values($E-INVALIDARG, null-pointer(<LPVARDESC>))
end method ITypeInfo/GetVarDesc;

define method ITypeInfo/GetVarDesc (this :: <Disp-Type-Info>,
				    index :: <integer>)
 => (status :: <HRESULT>, vd :: <LPVARDESC>);
  if (index < 0 | index >= size(this.local-variables))
    values($E-INVALIDARG, null-pointer(<LPVARDESC>))
  else
    let var :: <data-description> = this.local-variables[index];
    block ()
      let vd :: <LPVARDESC> = make-vardesc(var);
      vd.memid-value := var.member-disp-id;
      values($S-OK, vd)
    exception ( condition :: <error> )
      error("Error while processing property \"%s\":\n%s",
	    var.member-name, condition);
      values($E-FAIL, null-pointer(<LPVARDESC>))
    end block;
  end if
end method ITypeInfo/GetVarDesc;

// Note that we don't support $VAR-PERINSTANCE or $VAR-STATIC here since
// they presume that a C program could access the location directly.
// $VAR-CONST would make sense for a <constant-description>, but
// ICreateTypeInfo/AddVarDesc returns $TYPE-E-BADMODULEKIND if $VAR-CONST
// is used in a $TKIND-DISPATCH.

define method make-vardesc (var :: <data-description>) => vd :: <LPVARDESC>;
  let vd :: <LPVARDESC> = make(<LPVARDESC>);
  vd.lpstrSchema-value := $NULL-OLESTR;
  vd.elemdescVar-value.idldesc-value.dwReserved := 0;
  let tdesc = vd.elemdescVar-value.tdesc-value;
  set-typedesc(var.data-type, tdesc);
  vd.varkind-value := $VAR-DISPATCH;
  vd.lpvarValue-value := $NULL-VARIANT;
  vd.elemdescVar-value.idldesc-value.wIDLFlags-value := $IDLFLAG-NONE;
  vd
end method make-vardesc;

define method make-vardesc (var :: <variable-description>)
 => vd :: <LPVARDESC>;
  let vd :: <LPVARDESC> = next-method();
  vd.wVarFlags-value :=
    if (var.variable-setter == #f) $VARFLAG-FREADONLY else 0 end if;
  vd
end method make-vardesc;

define method make-vardesc (cd :: <constant-description>)
 => vd :: <LPVARDESC>;
  let vd :: <LPVARDESC> = next-method();
  vd.wVarFlags-value := $VARFLAG-FREADONLY;
  vd
end method make-vardesc;

define method ITypeInfo/ReleaseVarDesc (this :: <Disp-Type-Info>,
					pVarDesc :: <LPVARDESC>) => ();
  if (pVarDesc.varkind-value = $VAR-CONST)
    destroy(pVarDesc.lpvarValue-value);
  end if;
  destroy(pVarDesc);
  values()
end method ITypeInfo/ReleaseVarDesc;


define method store-result (pvarResult :: <LPVARIANT>,
			    value :: <object>,
			    type :: <object>) => status :: <HRESULT>;
  if (null-pointer?(pvarResult))
    if (value == #f) $S-OK else $E-INVALIDARG end if
  else
    VariantInit(pvarResult); // set to $VT-EMPTY
    pointer-value(pvarResult) := value;
    $S-OK
  end if
end method store-result;

define method store-result (pvarResult :: <LPVARIANT>,
			    value == #f,
			    type  == #f) => status :: <HRESULT>;
  // no result value
  unless (null-pointer?(pvarResult))
    VariantInit(pvarResult); // set to $VT-EMPTY
  end unless;
  $S-OK
end method store-result;

define method store-result (pvarResult :: <LPVARIANT>,
			    value :: <integer>,
			    type :: <type>) => status :: <HRESULT>;
  if (null-pointer?(pvarResult))
    $E-INVALIDARG
  elseif ( type == <C-long> | type == <integer> )
    // If the result is supposed to be `long', return 4 bytes
    // regardless of whether the actual value could have fit in 2.
    VariantInit(pvarResult); // set to $VT-EMPTY
    pvarResult.vt-value := $VT-I4;
    pvarResult.u-value.lVal-value := value;
    $S-OK
  elseif ( subtype?(type, <number>) )
    // convert to floating point if that is required.
    store-result(pvarResult, as(type, value), type)
  else // choose a representation to match the actual value
    next-method()
  end if
end method store-result;



//  ====  other type info operations  ====


// portion of ITypeInfo/GetTypeAttr common to different kinds of typeinfo:
define method get-type-attr (this :: <Dylan-Type-Info>,
			     kind :: <integer>,
			     #key nfunctions :: <integer> = 0,
			          nvariables :: <integer> = 0)
 => ( ptypeattr :: <LPTYPEATTR> );
  let ta :: <LPTYPEATTR> = make(<LPTYPEATTR>);
  copy-into!(ta.guid-value, this.guid-value, size-of(<GUID>));
  ta.lcid-value  := this.type-info-locale;
  ta.dwReserved  := 0;
  ta.memidConstructor-value := $MEMBERID-NIL;
  ta.memidDestructor-value  := $MEMBERID-NIL;
  ta.lpstrSchema-value := $NULL-OLESTR;
  ta.cbSizeInstance-value := nfunctions + nvariables; // ?
  ta.typekind-value := kind;  // $TKIND-...
  ta.cFuncs-value := nfunctions;
  ta.cVars-value := nvariables;
  ta.cImplTypes-value := if (null?(this.inherited-typeinfo))
			   0
			 else
			   1
			 end;
  ta.cbSizeVft-value := 0;  // not relevant for $TKIND-DISPATCH
  ta.cbAlignment-value := size-of(<C-int>); // not clear how this would be used
  ta.wTypeFlags-value := 0; // see $TYPEFLAG-...
  ta.wMajorVerNum-value := this.major-version;
  ta.wMinorVerNum-value := this.minor-version;
  //  ta.tdescAlias-value only applies for $TKIND-ALIAS
  ta.tdescAlias-value.vt-value := $VT-EMPTY;
  ta.idldescType-value.dwReserved := 0;
  ta.idldescType-value.wIDLFlags-value := $IDLFLAG-NONE;
  ta
end;

define method ITypeInfo/GetTypeAttr(this :: <Disp-Type-Info>)
 => ( status :: <HRESULT>, ptypeattr :: <LPTYPEATTR> );
  let ta :: <LPTYPEATTR> = get-type-attr(this,
					 $TKIND-DISPATCH,
					 nfunctions:
					   this.local-functions.size,
					 nvariables:
					   this.local-variables.size);
  values($S-OK, ta)
end;


define method ITypeInfo/ReleaseTypeAttr(this :: <Dylan-Type-Info>,
					ptypeattr :: <LPTYPEATTR>) => ();
  // first verify that this was created by the method above.
  if ( ptypeattr.lcid-value ~= this.type-info-locale )
    error("Invalid TYPEATTR");	// ???
  else
    destroy(ptypeattr);
  end if;
  values()
end;

define generic find-member(this :: <ITypeInfo>, memid :: <disp-id>)
	=> member :: false-or(<member-description>);

define method ITypeInfo/GetDocumentation(this :: <Dylan-Type-Info>,
					 memid :: <disp-id>)
 => (status :: <HRESULT>, Name :: <BSTR>, doc-string :: <BSTR>,
     HelpContext :: <integer>, HelpFile :: <BSTR> ); 

  // Problem: the caller is supposed to de-allocate the BSTR values
  // received, but won't know to do that if NULL is passed as the address. ???

  if (memid = $MEMBERID-NIL)
    // documentation for the type as a whole
    values($S-OK,
	   copy-as-BSTR(this.interface-name),
	   copy-as-BSTR(this.doc-string),
	   this.help-context, copy-as-BSTR(this.help-file));
  else
    let try-member = find-member(this, memid);
    if (try-member == #f)
      let base-type = this.inherited-typeinfo;
      if (base-type)
	ITypeInfo/GetDocumentation(base-type, memid)
      else
	values($TYPE-E-ELEMENTNOTFOUND, $NULL-BSTR, $NULL-BSTR, 0, $NULL-BSTR)
      end if
    else
      let member :: <member-description> = try-member;
      let context = member.help-context;
      values($S-OK,
	     copy-as-BSTR(member.member-name),
	     copy-as-BSTR(member.doc-string),
	     context, if (zero?(context)) $NULL-BSTR
		      else copy-as-BSTR(this.help-file)
		      end if);
    end if
  end if
end;

define method find-member (this :: <Dylan-Type-Info>, memid :: <disp-id>)
 => (member :: singleton(#f))
  #f
end;

define method ITypeInfo/GetIDsOfNames(this :: <Disp-Type-Info>,
				      rglpszNames :: <LPLPOLESTR>,
				      cNames :: <integer>,
				      rgmemid :: <LPMEMBERID>)
				 => status :: <HRESULT>;
  if (zero?(cNames))
    $S-OK
  else
    let status :: <HRESULT> = $S-OK;
    let fname = pointer-value(rglpszNames);
    block(end-search)
      for (info = this then info.inherited-typeinfo, while: info)
	for (func :: <function-description> in info.local-functions)
	  if (string-equal(fname, func.member-name))
	    // member ID for the function
	    pointer-value(rgmemid) := func.member-disp-id;
	    for (i from 1 below cNames) // for each argument name
	      let aname = pointer-value(rglpszNames, index: i);
	      block(next-arg)
		for (argname in func.argument-names, na from 0 by 1)
		  if (string-equal(fname, argname))
		    pointer-value(rgmemid, index: i) := na; // ID of argument
		    next-arg();
		  end if;
		end for;
		status := $DISP-E-UNKNOWNNAME;
		pointer-value(rgmemid, index: i) := $DISPID-UNKNOWN;
	      end block;
	    end for;
	    end-search(status);
	  end if;
	end for;
      end for;
      for (info = this then info.inherited-typeinfo, while: info)
	for (var :: <data-description> in info.local-variables)
	  if (string-equal(fname, var.member-name))
	    // member ID for the variable
	    pointer-value(rgmemid) := var.member-disp-id;
	    for (i from 1 below cNames) // shouldn't be any
	      status := $DISP-E-UNKNOWNNAME;
	      pointer-value(rgmemid, index: i) := $DISPID-UNKNOWN;
	    end;
	    end-search(status);
	  end if;
	end for;
      end for;
      status := $DISP-E-UNKNOWNNAME;
      for (i from 0 below cNames)
	status := $DISP-E-UNKNOWNNAME;
	pointer-value(rgmemid, index: i) := $DISPID-UNKNOWN;
      end;
      end-search(status);
    end block // end-search
  end if;
end method ITypeInfo/GetIDsOfNames;

define method string-equal(s1 :: <string>, s2 :: <string> )
			=> equal :: <boolean>; 
  // Case-insensitive comparison.
  size(s1) = size(s2) &
    block (return)
      for ( c1 :: <character> in s1, c2 :: <character> in s2 )
	if ( c1 ~= c2 & as-uppercase(c1) ~= as-uppercase(c2) )
	  return(#f);
	end if;
      end for;
      #t
    end block
end method;

define method ITypeInfo/GetNames(this :: <Dylan-Type-Info>,
				 memid :: <disp-id>,
				 rgbstrNames :: <LPBSTR>,
				 cMaxNames :: <integer> )
 => ( status :: <HRESULT>, pcNames :: <integer> );

  let member = find-member(this, memid);
  if (member == #f)
    let base-type = this.inherited-typeinfo;
    if (base-type)
      ITypeInfo/GetNames(base-type, memid, rgbstrNames, cMaxNames)
    else
      values($TYPE-E-ELEMENTNOTFOUND, 0)
    end if
  else
    pointer-value(rgbstrNames) := copy-as-BSTR(member.member-name);
    let last-index :: <integer> = 0;
    for (argname :: <string> in member.argument-names,
	 i from 1 below cMaxNames)
      pointer-value(rgbstrNames, index: i) := copy-as-BSTR(argname);
      last-index := i;
    end for;
    values($S-OK, last-index + 1);
  end if
end method ITypeInfo/GetNames;


// User code can signal this to return an error code from a dispatch member.
define open primary class <OLE-server-condition> (<condition>)
  constant sealed slot ole-server-status :: <HRESULT>,
    required-init-keyword: status:;
  constant sealed slot ole-server-arg-err :: <integer> = 0,
    init-keyword: arg-err:;
  sealed slot ole-server-exception :: <LPEXCEPINFO> = $NULL-EXCEPINFO,
    init-keyword: excepinfo:;
end;

define method exit-invoke (status :: <HRESULT>, #rest args)
  let condition :: <OLE-server-condition> =
    apply(make, <OLE-server-condition>, status: status, args);
  signal(condition);
  error("No handler for %s", condition);
end;

define method condition-to-string
    (condition :: <OLE-server-condition>) => (string :: <string>)
  let status = condition.ole-server-status;
  let message = win32-error-message(status);
  format-to-string("{<OLE-server-condition> %= [%=]}", status, message);
end method condition-to-string;

//  ====  Invoke  ====

define method ITypeInfo/Invoke(this :: <Disp-Type-Info>,
			       instance :: <object>, // usually <IDispatch>
			       memid :: <disp-id>,
			       wFlags :: <integer>,
			       pdispparams :: <LPDISPPARAMS>,
			       pvarResult :: <LPVARIANT>,
			       pexcepinfo :: <LPEXCEPINFO>)
	=> ( status :: <HRESULT>, ArgErr :: <integer> );

  block()
    let member = find-member(this, memid);
    invoke-member(member, instance, wFlags, pdispparams,
		  pvarResult, pexcepinfo)
  exception ( err :: <OLE-server-condition> )
    let pex = ole-server-exception(err);
    unless ( null-pointer?(pex) | null-pointer?(pexcepinfo) )
      copy-into!(pexcepinfo, pex, size-of(<EXCEPINFO>));
    end unless;
    values( ole-server-status(err), ole-server-arg-err(err) )
  exception (<abort>)
    values( $E-ABORT, 0 )
  end block
end method ITypeInfo/Invoke;

define method invoke-member(member == #f,
			    instance :: <object>,
			    wFlags /* :: <integer> */ ,
			    pdispparams /* :: <LPDISPPARAMS> */ ,
			    pvarResult /* :: <LPVARIANT> */ ,
			    pexcepinfo /* :: <LPEXCEPINFO> */ )
	=> ( status :: <HRESULT>, ArgErr :: <integer> );

  values( $TYPE-E-ELEMENTNOTFOUND, 0 )
end;

define method invoke-member(func :: <function-description>,
			    instance :: <object>,
			    wFlags :: <integer>,
			    pdispparams :: <LPDISPPARAMS>,
			    pvarResult :: <LPVARIANT>,
			    pexcepinfo /* :: <LPEXCEPINFO> */)
	=> ( status :: <HRESULT>, ArgErr :: <integer> );

  if (  zero?(logand(wFlags, $DISPATCH-METHOD)) )
    values( $E-INVALIDARG, 0 )
  else
    let ( status, result ) =
      apply-to-dispparams(func.function-object, instance, pdispparams);
    store-result(pvarResult, result, func.result-type);
    values( status | $S-OK, 0 )
  end if
end method invoke-member;


define method invoke-member(var :: <variable-description>,
			    instance :: <object>, // usually <IDispatch>
			    wFlags :: <integer>,
			    pdispparams :: <LPDISPPARAMS>,
			    pvarResult :: <LPVARIANT>,
			    pexcepinfo /* :: <LPEXCEPINFO> */)
	=> ( status :: <HRESULT>, ArgErr :: <integer> );

  let status :: <HRESULT> = $S-OK;
  let nargs :: <integer> = size(pdispparams);
  if ( ~ zero?(logand(wFlags, $DISPATCH-PROPERTYGET)) ) // get value
    let value = var.variable-getter(instance);
    let type = var.data-type;
    status :=
      if ( zero?(nargs) ) // short-cut case for non-indexed property
	store-result(pvarResult, value, type);
      else // general case for indexed property
	return-property(value, type, pdispparams, pvarResult);
      end if;
  elseif ( ~ zero?(logand(wFlags, logior($DISPATCH-PROPERTYPUT,
					   $DISPATCH-PROPERTYPUTREF))) )
    // set value
    if ( zero?(nargs) )
      status := $DISP-E-BADPARAMCOUNT; // value not provided
    else
      let argptr :: <LPVARIANT> = pdispparams.rgvarg-value;
      let new-value = pointer-value(argptr);
      unless ( ~ zero?(logand(wFlags, $DISPATCH-PROPERTYPUTREF)) )
	// Unless pass-by-reference (although not sure of exact intended
	// semantics of that???), we need to make a copy of any
	// non-scalar values because the received value will be deallocated
	// by the caller.
	new-value := copy-automation-value(new-value);
      end unless;
      // Note that if the value is an <interface>, the setter method will
      // need to be responsible for doing AddRef on the new value and
      // Release on the old value.
      let nx :: <integer> = nargs - 1; // number of index arguments
      if ( zero?(nx) )
	if ( var.variable-setter == #f )
	  status := $DISP-E-MEMBERNOTFOUND; // read-only
	elseif ( ole-instance?(new-value, var.data-type) )
	  var.variable-setter(new-value, instance); // store new value
	else
	  status := $DISP-E-TYPEMISMATCH;
	end if;
      else
	let array = var.variable-getter(instance);
	if ( ~ ole-instance?(new-value, element-type(var.data-type)) )
	  status := $DISP-E-TYPEMISMATCH;
	elseif ( (nx = 1) & instance?(array, <collection>) )
	  let index = pointer-value(argptr, index: 1);
	  element(array, index) := new-value;
	elseif ( instance?(array, <array>) & (nx = rank(array)) )
	  let args :: <simple-object-vector> =
	    make(<simple-object-vector>, size: nx);
	  for ( i :: <integer> from 0 below nx )
	    args[i] := pointer-value(argptr, index: nx - i);
	  end for;
	  apply(aref-setter, new-value, array, args);
	else
	  status := $DISP-E-BADPARAMCOUNT;
	end if;
      end if;
    end if;
  else status := $E-INVALIDARG;
  end if;
  values( status, 0 )
end method invoke-member;

define function return-property (value, type, 
				 pdispparams :: <LPDISPPARAMS>,
				 pvarResult :: <LPVARIANT> )
 => status :: <HRESULT>;
  let nargs :: <integer> = size(pdispparams);
  if ( zero?(nargs) ) // non-indexed property
    store-result(pvarResult, value, type)
  elseif ( (nargs = 1) & instance?(value, <collection>) )
    // property with one index argument
    store-result(pvarResult, 
		 apply-to-dispparams(element, value, pdispparams),
		 element-type(type))
  elseif ( instance?(value, <array>) & (nargs = rank(value)) )
    // indexed property with multiple arguments
    store-result(pvarResult, 
		 apply-to-dispparams(aref, value, pdispparams),
		 element-type(type))
  else // wrong number of arguments supplied
    $DISP-E-BADPARAMCOUNT
  end if
end return-property;

define method invoke-member(cd :: <constant-description>,
			    instance :: <object>,
			    wFlags :: <integer>,
			    pdispparams :: <LPDISPPARAMS>,
			    pvarResult :: <LPVARIANT>,
			    pexcepinfo /* :: <LPEXCEPINFO> */)
	=> ( status :: <HRESULT>, ArgErr :: <integer> );

  let status :: <HRESULT> = 
    if ( ~ zero?(logand(wFlags, $DISPATCH-PROPERTYGET)) ) // get value
      return-property(cd.constant-value, cd.data-type,
		      pdispparams, pvarResult);
    else $DISP-E-MEMBERNOTFOUND
    end if;
  values( status, 0 )
end method invoke-member;


define method element-type( type :: <type> ) => ( type :: <ole-type> );
  <object>
end method;

define method element-type ( t :: <LPTYPEDESC> ) => (type :: <ole-type>);
  if ( t.vt-value = $VT-SAFEARRAY )
    typedesc-Dylan-class(t.u-value.lptdesc-value)
  else
    <object>
  end if
end method;

define generic ole-instance? ( object :: <object>, type :: <ole-type> )
 => ( satisifies? :: <boolean> );

define method ole-instance? ( object :: <object>, type :: <type> )
 => ( satisifies? :: <boolean> );
  instance?(object, type)
end method;

define method ole-instance? ( object :: type-union(<number>, <machine-word>),
			      type :: subclass(<C-number>) )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method ole-instance? ( object :: <boolean>,
			      type :: type-union(subclass(<C-boolean>),
						 subclass(<VARIANT-BOOL>)) )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method ole-instance? ( object :: <object>, type == <VARIANT> )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method ole-instance? ( object :: <character>,
			      type :: subclass(<C-character>) )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method ole-instance? ( object :: <string>,
			      type :: subclass(<string>) )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method ole-instance? ( object :: type-union(<HRESULT>,<integer>),
			      type == <C-HRESULT> )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method ole-instance? ( object :: <collection>,
			      type ::  <array-type-description> )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method ole-instance? ( object :: <object>,
			      type ::  <array-type-description> )
 => ( satisifies? :: <boolean> );
  #f
end method;

define method ole-instance? ( object :: <object>, type :: <LPTYPEDESC> )
 => ( satisifies? :: <boolean> );
  ole-instance?(object, typedesc-Dylan-class(type))
end method;


define inline method ole-instance? ( object :: <C-long*>, type == <C-int*> )
 => ( satisifies? :: <boolean> );
  #t
end method;

define inline method ole-instance? ( object :: <C-int*>, type == <C-long*> )
 => ( satisifies? :: <boolean> );
  #t
end method;

define method copy-automation-value ( value :: <object> )
  value
end;

define method copy-automation-value ( value :: <ole-vector> )

  if ( null-pointer?(value) )
    // Shouldn't really happen, but just to be safe.
    value
  elseif ( zero?(logand(value.fFeatures-value,
			  logior($FADF-BSTR, $FADF-UNKNOWN,
				   $FADF-DISPATCH, $FADF-VARIANT))) )
    // scalar elements, so can do simple copy.
    as(<simple-vector>, value)
  else
    // may need to copy the elements also.
    map-as(<simple-vector>, copy-automation-value, value)
  end if
end;

// see also method for <ole-array> in "arrays.dylan".

define method copy-automation-value ( value :: <BSTR> );
  // should be <unicode-string> when that is supported ???
  as(<byte-string>, value)
end;

/* // No, this will have to be done by the setter method because it needs
   // to also Release the old value.
define method copy-automation-value ( value :: <LPUNKNOWN> );
  AddRef(value);
  value
end;

define method copy-automation-value
    ( value :: type-union(<C-pointer>, <collection> );
  // Internal error; shouldn't happen.
  cerror($continue-message,
	 "Unexpected type of OLE Automation property value %=", value);
  value
end;
*/

define method find-member (this :: <Disp-Type-Info>, memid :: <disp-id>)
	=> member :: false-or(<member-description>);
  let value =
  if (negative?(memid))
    iterate lookup (info :: false-or(<disp-type-info>) = this)
      info & (element(info.dispatch-table, memid, default: #f)
		| lookup(info.inherited-typeinfo))
    end iterate;
  elseif (instance?(memid, <machine-word>))
    let memid :: <machine-word> = memid;
    let level = dispid-nesting-level(memid);
    iterate lookup (info :: false-or(<disp-type-info>) = this)
      if (~info)
	#f
      elseif (info.nesting-level ~== level)
	lookup(info.inherited-typeinfo)
      elseif (dispid-function?(memid))
	element(info.local-functions, dispid-offset(memid), default: #f)
      else
	element(info.local-variables, dispid-offset(memid), default: #f)
      end;
    end iterate;
  else
    #f
  end;
  debug-message("find-member(%=, %=) = %=", this, memid, value);
  value
end;

define method Disp-Invoke (this      /* :: <IDispatch> */,
			  ptinfo     :: <Disp-Type-Info>,
			  dispidMember :: <disp-id>,
			  wFlags     :: <integer>,
			  pparams    :: <LPDISPPARAMS>,
			  pvarResult :: <LPVARIANT>,
			  pexcepinfo :: <LPEXCEPINFO> )
 => ( status :: <HRESULT>, ArgErr :: <integer> );

  // Note that the member ID and disp ID values are the same in
  // this implementation.  That isn't a requirement, but it makes
  // things a lot simpler.
  ITypeInfo/Invoke(ptinfo, this, dispidMember,
		   wFlags, pparams, pvarResult, pexcepinfo)
end method Disp-Invoke;

define method Disp-GetIDsOfNames(typeinfo :: <Disp-Type-Info>,
				 rgszNames, cNames,
				 rgdispid :: <LPDISPID> )
 	=> status :: <HRESULT>;
  ITypeInfo/GetIDsOfNames(typeinfo, rgszNames, cNames, rgdispid)
end method Disp-GetIDsOfNames;


// operations not supported:

// Note that the following operations are not defined because they are
// only applicable for a "component object class (coclass)":
//   ITypeInfo/GetRefTypeOfImplType, ITypeInfo/GetRefTypeInfo, 
//   ITypeInfo/CreateInstance

define sideways method ITypeInfo/GetTypeComp (This :: <ITypeInfo>)
 => (status :: <HRESULT>, ptcomp :: <Interface>);
  values( $E-NOTIMPL, $NULL-interface )
end;

define constant $bogus-index = -1; // meaningless, but have to return something

define sideways method ITypeInfo/GetDllEntry(This :: <ITypeInfo>, memid, invkind)
 => (status :: <HRESULT>, DLL-name :: <BSTR>,
     name :: <BSTR>, ordinal :: <integer>);
  values( $E-NOTIMPL, $NULL-BSTR, $NULL-BSTR, $bogus-index )
end;

define sideways method ITypeInfo/AddressOfMember(This :: <ITypeInfo>,
                                                 memid, invkind)
 => (status :: <HRESULT>, pv :: <C-void*>);
  values( $E-NOTIMPL, $NULL-VOID )
end;

define sideways method ITypeInfo/GetMops(This :: <ITypeInfo>, memid)
 => (status :: <HRESULT>, mops :: <BSTR>);
  values( $E-NOTIMPL, $NULL-BSTR )
end;

define sideways method ITypeInfo/GetContainingTypeLib(This :: <ITypeInfo>)
 => (status :: <HRESULT>, ptlib :: <Interface>, index :: <integer>);
  values( $E-NOTIMPL, $NULL-interface, $bogus-index )
end;

/* // Don't bother defining this, as it wouldn't get used since a
   // TYPEDESC usually appears embedded in some other structure.
define method destroy(td :: <LPTYPEDESC>, #key ) => ();
  let vt :: <integer> = td.vt-value;
  if ( vt >= $VT-PTR & vt <= $VT-SAFEARRAY )
    destroy(td.u-value.lptdesc-value);
  end if;
  next-method();
end method destroy;
*/



// misc. stuff related to types

// defined as a macro in "oleauto.h":
define inline function LHashValOfName(lcid, szName) => value :: <integer>;
  LHashValOfNameSys($SYS-WIN32, lcid, szName)
end;

// Don't bother with these because they aren't documented:
// oleauto.h: #define WHashValOfLHashVal(lhashval) \
// oleauto.h: #define IsHashValCompatible(lhashval1, lhashval2) \

