Module:    OLE-Automation
Synopsis:  Type info support for custom v-table interfaces.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define COM-interface <vtable-type-info> (<dylan-type-info>)
  // Sequence of <vtable-member-description>
  slot vtable-members :: <simple-vector> = #[];
  slot inherited-typeinfo :: false-or(<LPTYPEINFO>) = #f;
  slot type-flags :: <integer> = 0,  // logior of $TYPEFLAG-... values
    init-keyword: typeflags:;
  slot first-local-member :: <integer> = 0;
  slot nesting-level :: <integer> = 0;
end <vtable-type-info>;

define method initialize (this :: <vtable-type-info>, #rest args,
			 #key inherit :: false-or(<LPTYPEINFO>) = #f,
			      members :: <sequence> = #[],
			 #all-keys) => ();
  next-method();
  this.inherited-typeinfo := if (inherit)
			       AddRef(inherit);
			       inherit
			     end if;
  this.nesting-level := if (instance?(inherit, <vtable-type-info>))
			  1 + inherit.nesting-level
			else
			  0
			end;
  this.vtable-members := 
    if (this.nesting-level == 0)
      as(<simple-vector>, members)
    else
      let inherited-members = inherit.vtable-members;
      this.first-local-member := size(inherited-members);
      concatenate-as(<simple-vector>, inherited-members, members)
    end if;
  if (this.guid-value == $default-type-id
	& this.interface-name ~= "IDispatch") //unless is $IDispatch-type-info
    // no valid default for custom interfaces
    error("uuid not specified for %= %=",
	  this, as(<byte-string>, this.interface-name));
  end if;
end method initialize;

define method terminate (this :: <vtable-type-info>) => ();
  next-method();
  let inherit = this.inherited-typeinfo;
  if (inherit)
    Release(inherit);
  end if;
end method terminate;


//  ====  representation of methods of interface  ====

define sealed primary class <vtable-member-description> (<member-description>)
  constant slot argument-names :: <sequence> = #(),
    init-keyword: argument-names:;
  slot argument-types :: <sequence> = #(),
    init-keyword: argument-types:;
  slot result-type :: false-or(<ole-type>) = #f,
    init-keyword: result-type:;
  constant slot func-flags :: <integer> = 0, // logior of $FUNCFLAG-...
    init-keyword: flags:;
  constant slot invoke-flags :: <integer> = $INVOKE-FUNC,
    init-keyword: invoke:;
end class;


define method ITypeInfo/GetFuncDesc (typeinfo :: <vtable-type-info>,
				     index :: <integer>)
 => (status :: <HRESULT>, fd :: <LPFUNCDESC> );
  let vindex = index + typeinfo.first-local-member;
  let el = element(typeinfo.vtable-members, vindex, default: #f);
  if (el == #f)
    values($E-INVALIDARG, null-pointer(<LPFUNCDESC>))
  else
    values($S-OK, make-member-desc(el, typeinfo.nesting-level, vindex))
  end if
end method ITypeInfo/GetFuncDesc;

define method make-member-desc (func :: <vtable-member-description>,
				nesting-level :: <integer>,
				vtable-index :: <integer>)
 => (result :: <LPFUNCDESC>);
  let fd :: <LPFUNCDESC> = make(<LPFUNCDESC>);

  let memid :: <integer> = vtable-index + $first-vtable-member-id;
  fd.memid-value := make-dispid(offset: memid,
				nesting-level: nesting-level,
				function?: #t);
  fd.cScodes-value := -1;
  fd.lprgscode-value := null-pointer(<C-HRESULT*>);

  let argument-types :: <sequence> = func.argument-types;
  let total-number :: <integer> = size(argument-types);
  fd.cParams-value := total-number;
  fd.cParamsOpt-value := 0;
  if ( zero?(total-number) )
    fd.lprgelemdescParam-value := null-pointer(<LPELEMDESC>);
  else
    let elems :: <LPELEMDESC> =
      make(<LPELEMDESC>, element-count: total-number);
    for ( i from 0 below total-number,
	  argtype in argument-types )
      let elem :: <LPELEMDESC> = pointer-value-address(elems, index: i);
      let direction-flags :: <U16> = 
	set-typedesc(argtype, elem.tdesc-value);
      elem.idldesc-value.wIDLFlags-value := direction-flags;
    end for;
    fd.lprgelemdescParam-value := elems;
  end if;

  fd.funckind-value := $FUNC-PUREVIRTUAL;
  fd.invkind-value  := func.invoke-flags;
  fd.callconv-value := $CC-STDCALL;
  fd.oVft-value := vtable-index * size-of(<C-function-pointer>);

  let result-type :: false-or(<ole-type>)  = func.result-type;
  let resptr = fd.elemdescFunc-value.tdesc-value;
  if ( result-type == #f )
    resptr.vt-value := $VT-VOID; // no result value
  else
    set-typedesc(result-type, resptr);
  end if;
  fd.elemdescFunc-value.idldesc-value.wIDLFlags-value := $IDLFLAG-FRETVAL;

  fd.wFuncFlags-value := func.func-flags;

  fd
end method make-member-desc;

define method ITypeInfo/ReleaseFuncDesc(typeinfo :: <vtable-type-info>,
					pfuncdesc :: <LPFUNCDESC>) => ();
  if ( pfuncdesc.callconv-value ~= $CC-STDCALL )
    error("Invalid FUNCDESC");	// ???
  else
    destroy(pfuncdesc.lprgelemdescParam-value);
    destroy(pfuncdesc);
  end if;
  values()
end method ITypeInfo/ReleaseFuncDesc;


define method ITypeInfo/GetTypeAttr(typeinfo :: <vtable-type-info>)
 => ( status :: <HRESULT>, ptypeattr :: <LPTYPEATTR> );

  let ta :: <LPTYPEATTR> = get-type-attr(typeinfo, $TKIND-INTERFACE);
  ta.cbSizeInstance-value := size-of(referenced-type(<Dylan-interface>));
  let vtbl-size :: <integer> =
    typeinfo.vtable-members.size * size-of(<C-function-pointer>);
  let base-type = typeinfo.inherited-typeinfo;
  unless ( null?(base-type) )
    ta.cImplTypes-value := 1;
    unless ( instance?(base-type, <vtable-type-info>) )
      // If vtable-members does not already include the inherited members, 
      // then add in the vtable size of the inherited type.
      let ( status, attributes :: <LPTYPEATTR> ) =
	ITypeInfo/GetTypeAttr(base-type);
      if(SUCCEEDED?(status))
	vtbl-size := vtbl-size + attributes.cbSizeVft-value;
	ITypeInfo/ReleaseTypeAttr(base-type, attributes);
      end if;
    end unless;
  end unless;
  ta.cbSizeVft-value := vtbl-size;
  // should automatically add $TYPEFLAG-FOLEAUTOMATION when appropriate ???
  ta.wTypeFlags-value := typeinfo.type-flags; // see $TYPEFLAG-...
  let num :: <integer> =
    typeinfo.vtable-members.size - typeinfo.first-local-member;
  ta.cFuncs-value := num;
  values( $S-OK, ta )
end method ITypeInfo/GetTypeAttr;


// arbitrary offset for validation
define constant $first-vtable-member-id = #x4000;

define method find-member (this :: <vtable-type-info>,
			   memid :: <ffi-integer>)
	=> member :: false-or(<vtable-member-description>);

  let offset = dispid-offset(memid);
  let value =
    element(this.vtable-members,
	    offset - $first-vtable-member-id,
	    default: #f);
  // debug-message("find-member(%=, %=) = %=", this, memid, value);
  value
end;


