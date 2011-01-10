Module:    OLE-Automation
Synopsis:  Description of an OLE object before actually instantiating it.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// "coclass" = 
// Component object model class. A top-level object in the object hierarchy.


//  Default methods

define sideways method ITypeInfo/GetRefTypeOfImplType(This :: <ITypeInfo>, index)
 => (status :: <HRESULT>, reftype :: <integer>);
  values( $E-NOTIMPL, $bogus-index )
end;

define sideways method ITypeInfo/GetImplTypeFlags(This :: <ITypeInfo>, index)
 => (status :: <HRESULT>, impltypeflags :: <integer>);
  values( $E-NOTIMPL, 0 )
end;

define sideways method ITypeInfo/GetRefTypeInfo(This :: <ITypeInfo>, hreftype)
 => (status :: <HRESULT>, ptinfo :: <Interface>);
  values( $E-NOTIMPL, $NULL-interface )
end;

define sideways method ITypeInfo/CreateInstance(this :: <ITypeInfo>,
				       puncOuter /* :: <Interface> */,
				       riid /* :: <REFIID> */)
 => (status :: <HRESULT>, object :: <Interface>);
  // only meaningful for a coclass type.
  values( $TYPE-E-WRONGTYPEKIND, $null-interface )
end method ITypeInfo/CreateInstance;


//  Dylan implementation

// The following class serves as the type info interface for a coclass object,
// and it also contains the information for how to instantiate the object.
define COM-interface <coclass-Type-Info> ( <Dylan-Type-Info> )
  slot instance-class :: <class>, init-value: <Simple-Component-Object>,
		init-keyword: class:, setter: #f;
  slot instance-args :: <sequence>, init-value: #[], init-keyword: args:,
		setter: #f;
  slot coclass-interfaces :: <vector>, init-value: #[],
			init-keyword: interfaces:, setter: #f;
end <coclass-Type-Info>;

define class <component-interface-description> ( <object> )
  slot instance-typeinfo :: false-or(<Interface>) = #f,
		init-keyword: typeinfo:;
  slot instance-class :: <class>, init-value: <Simple-Dispatch>,
		init-keyword: class:, setter: #f;
  slot instance-args :: <sequence>, init-value: #[], init-keyword: args:,
    setter: #f;
  slot instance-type-flags :: <integer>, // logior of $IMPLTYPEFLAG-... values
		init-value: 0, init-keyword: flags:;
end <component-interface-description>;


define method initialize(this :: <component-interface-description>,
			 #rest args, #key) => ();
  next-method();
  unless (this.instance-typeinfo |
	    (this.instance-typeinfo := this.instance-class.type-information) )
    error("No typeinfo for %= in %=", this.instance-class, this);
  end unless;
end initialize;

define method instantiate-component
    ( component-descr :: <component-interface-description>,
      controlling-unknown )
 => ( instance :: <IUnknown> );
  apply(make, component-descr.instance-class,
	controlling-unknown: controlling-unknown,
	typeinfo: component-descr.instance-typeinfo,
	component-descr.instance-args)
end instantiate-component;

define method initialize (this :: <coclass-Type-Info>, #rest args, #key)
 => ();
  next-method();
  if ( this.guid-value == $default-type-id )
    // The GUID is optional for dispatch interfaces but required here.
    error("Missing `uuid:' for %=", this);
  end if;
  let default = #f;
  let first-dest = #f;
  let components = this.coclass-interfaces;
  for ( component :: <component-interface-description> in components )
    let flags = component.instance-type-flags;
    if ( zero?(logand(flags, $IMPLTYPEFLAG-FSOURCE)) )
      if ( ~ zero?(logand(flags, $IMPLTYPEFLAG-FDEFAULT)) )
	default := component;
      elseif ( first-dest == #f )
	first-dest := component;
      end if;
    end if;
    let typeinfo = component.instance-typeinfo;
    if ( instance?(typeinfo, <Dylan-Type-Info>) )
      typeinfo.containing-coclass := this;
    end if;
    IUnknown/AddRef(typeinfo);
  end for;
  unless ( default | ( first-dest == #f ) )
    // If no explicit default, designate the first destination as the default.
    default := first-dest;
    first-dest.instance-type-flags :=
      logior(first-dest.instance-type-flags, $IMPLTYPEFLAG-FDEFAULT);
  end unless;
  if ( default )
    let default-typeinfo = default.instance-typeinfo;
    if ( instance?(default-typeinfo, <Dylan-Type-Info>)
	  & (default-typeinfo.guid-value = $default-type-id) )
      // Use the class ID as the ID for the default interface if it doesn't
      // already have a unique ID.
      default-typeinfo.guid-value := this.guid-value;
    end if;
  end if;
end method initialize;

define method terminate (this :: <coclass-Type-Info>) => ();
  next-method();
  for ( component :: <component-interface-description> in
	 this.coclass-interfaces )
    IUnknown/Release(component.instance-typeinfo);
  end for;
  values()
end method terminate;


// ITypeInfo methods

define constant $href-offset = #x5000; // arbitrary offset for validation

define method ITypeInfo/GetRefTypeOfImplType(this :: <coclass-Type-Info>,
					     index :: <integer> )
 => (status :: <HRESULT>, reftype :: <integer>);
  if ( index >= 0 & index < size(this.coclass-interfaces) )
    values( $S-OK, index + $href-offset )
  else
    values( $TYPE-E-ELEMENTNOTFOUND, $bogus-index )
  end if
end;

define method ITypeInfo/GetImplTypeFlags(this :: <coclass-Type-Info>,
					 index :: <integer>)
 => (status :: <HRESULT>, impltypeflags :: <integer>);
  let component = element(this.coclass-interfaces, index, default: #f);
  if ( component )
    values( $S-OK, component.instance-type-flags )
  else
    values( $TYPE-E-ELEMENTNOTFOUND, 0 )
  end if
end;

define method ITypeInfo/GetRefTypeInfo(this :: <coclass-Type-Info>,
				       hreftype :: <integer>)
  => (status :: <HRESULT>, ptinfo :: <Interface>);
  let index = hreftype - $href-offset;
  let component = element(this.coclass-interfaces, index, default: #f);
  if ( component )
    let typeinfo = component.instance-typeinfo;
    IUnknown/AddRef(typeinfo);
    values( $S-OK, typeinfo )
  else
    values( $E-INVALIDARG, $NULL-interface )
  end if
end;


define method ITypeInfo/CreateInstance(this :: <coclass-Type-Info>,
				       outer-unknown :: <LPUNKNOWN>,
				       interface-id :: <REFIID>)
 => (status :: <HRESULT>, object :: <C-void*>);
  let class-id = this.guid-value;
  let ( status, active ) = GetActiveObject(class-id, $NULL-VOID);
  if ( status = $S-OK )
    IUnknown/QueryInterface(active, interface-id)
  else
    CoCreateInstance(class-id, outer-unknown, $CLSCTX-ALL, interface-id)
  end if
end method ITypeInfo/CreateInstance;


define method ITypeInfo/GetDocumentation(this :: <coclass-Type-Info>,
					 memid :: <integer> )
 => (status :: <HRESULT>, Name :: <BSTR>, doc-string :: <BSTR>,
     HelpContext :: <integer>, HelpFile :: <BSTR> ); 

  let member = element(this.coclass-interfaces, memid, default: #f);
  if ( member == #f )
    next-method()
  else
    ITypeInfo/GetDocumentation(member.instance-typeinfo, $MEMBERID-NIL)
  end if
end;

define method ITypeInfo/GetTypeAttr(this :: <coclass-Type-Info>)
 => ( status :: <HRESULT>, ptypeattr :: <LPTYPEATTR> );

  let ta :: <LPTYPEATTR> = get-type-attr(this, $TKIND-COCLASS);
  let num :: <integer> = size(this.coclass-interfaces);
  ta.cImplTypes-value := num;
  values( $S-OK, ta )
end;

define method ITypeInfo/GetContainingTypeLib (typeinfo :: <coclass-Type-Info>)
 => (status :: <HRESULT>, ptlib :: <Interface>, index :: <integer>);
  values($S-OK,
	 fetch-type-library(typeinfo, typeinfo.guid-value),
	 0)
end;

define method ITypeInfo/GetContainingTypeLib(typeinfo :: <Dylan-Type-Info>)
 => (status :: <HRESULT>, ptlib :: <Interface>, index :: <integer>);
  let coclass-typeinfo = typeinfo.containing-coclass;
  block(return)
    if ( coclass-typeinfo )
      for ( component :: <component-interface-description>
	     in coclass-typeinfo.coclass-interfaces,
	   index from 0 )
	if ( component.instance-typeinfo == typeinfo )
	  let ( status, typelib, coclass-index ) =
	    ITypeInfo/GetContainingTypeLib(coclass-typeinfo);
	  return(status, typelib, coclass-index + 1 + index);
	end if;
      end for;
    end if;
    values($S-OK,
	   fetch-type-library(typeinfo, typeinfo.guid-value),
	   0 )
  end block
end;



// Instantiation class for an OLE object

define open COM-interface <Simple-Component-Object> ( <lib-init-mixin>,
						     <IUnknown>,
						     <factory-args-mixin>,
						     <object-with-dll-lock> )
  constant slot object-typeinfo :: <Interface>,
		required-init-keyword: typeinfo:;
  slot object-default-source-interface :: <Interface> = $null-interface;
end <Simple-Component-Object>;

define method initialize(this :: <Simple-Component-Object>,
			 #rest rest-args, #key, #all-keys ) => ();
  next-method();
  let description = this.object-typeinfo;
  IUnknown/AddRef(description);
  let default-component = #f;
  let ctl-unk = controlling-unknown(this);
  let components = description.coclass-interfaces;
  let n-interfaces :: <integer> = size(components);
  // Do this in reverse order so that the interface listed first will be
  // created last and hence be first in the `interface-table' to be the
  // default interface found for $IID-IDispatch.
  for ( i :: <integer> from n-interfaces - 1 to 0 by -1 )
    let component-descr :: <component-interface-description> = components[i];
    let flags = component-descr.instance-type-flags;
    if ( zero?(logand(flags, $IMPLTYPEFLAG-FSOURCE)) )
      if ( zero?(logand(flags, $IMPLTYPEFLAG-FDEFAULT)) )
	let interface = instantiate-component(component-descr, ctl-unk);
      else
	if ( default-component )
	  // Shouldn't have more than one default, but...
	  let interface = instantiate-component(default-component, ctl-unk);
	end if;
	default-component := component-descr;
      end if;
    else // source interface
      if ( null?(this.object-default-source-interface) |
	    ~ zero?(logand(flags, $IMPLTYPEFLAG-FDEFAULT)) )
	this.object-default-source-interface :=
	  component-descr.instance-typeinfo;
      end if;
    end if;
  end for;
  if ( default-component )
    let interface = instantiate-component(default-component, ctl-unk);
  end if;
  values()
end method initialize;

define method terminate(this :: <Simple-Component-Object>) => ();
  next-method();
  IUnknown/Release(this.object-typeinfo);
  values()
end;



// class factory

define method make-object-factory ( coclass-typeinfo :: <coclass-Type-Info>,
				    #rest other-args )
 => ( factory :: <class-factory> );
  // Using <class-factory-with-lock> instead of <class-factory> for use
  // from "ole-control-framework/dll.dylan".
  apply(make, <class-factory-with-lock>,
	clsid: coclass-typeinfo.guid-value,
	class: coclass-typeinfo.instance-class,
	typeinfo: coclass-typeinfo,
	concatenate(other-args, coclass-typeinfo.instance-args))
end make-object-factory;

/*
So now the user can do:

  define class <my-co-object> ( <Simple-Component-Object> )
    ...
  end;

  define constant my-type-info =
    make(<coclass-Type-Info>,
	 class: <my-co-object>,
	 uuid: make-GUID(...),
	 interfaces: vector(make(<component-interface-description>,
				 class: <my-dispatch-object1>,
				 typeinfo: make(<Disp-Type-Info>, ...)),
			    make(<component-interface-description>,
				 class: <my-dispatch-object2>,
				 typeinfo: make(<Disp-Type-Info>, ...),
				 ...)
			    ...
			    ));
				 

  let factory = make-object-factory(my-type-info);
*/
