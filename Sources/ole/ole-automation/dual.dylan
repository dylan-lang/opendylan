Module:    OLE-Automation
Synopsis:  Macro to define a COM dual interface.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro dual-interface-definer
  { 
    define ?modifiers:* dual-interface ?class-name:name (?superclass:name)
      ?slots-and-stuff:* 
    end 
  } => { 
    define ?modifiers dispatch-interface ?class-name ## "+dispatch"
		 (?superclass)
      ?slots-and-stuff 
    end;

    define inline-only constant ?class-name ## "+dispatch-vstruct" :: <type> = 
      ?superclass ## "-vstruct";

    define ?modifiers custom-interface ?class-name
		 (?class-name ## "+dispatch")
      ?slots-and-stuff 
    end;

    define dual-type-info "$" ## ?class-name ## "-dual-type" (?superclass)
      dispatch-type type-information(?class-name ## "+dispatch");
      ?slots-and-stuff
    end;

    define inline method vtable-type-information (x :: subclass(?class-name))
	   => ( result :: <dual-type-info> )
      "$" ## ?class-name ## "-dual-type"
    end;

    define inline method type-information (x :: subclass(?class-name))
	   => ( result :: <dual-type-info> )
      "$" ## ?class-name ## "-dual-type"
    end;
  }
end macro dual-interface-definer;

define C-struct <IDispatch>-vstruct
  sealed inline-only slot vtbl-unknown :: <IUnknown>-vstruct,
		getter: #f, setter: #f;
  sealed inline-only slot vtbl-GetTypeInfoCount :: <C-function-pointer>;
  sealed inline-only slot vtbl-GetTypeInfo :: <C-function-pointer>;
  sealed inline-only slot vtbl-GetIDsOfNames :: <C-function-pointer>;
  sealed inline-only slot vtbl-Invoke :: <C-function-pointer>;
end C-struct;

define constant <simple-dispatch>-vstruct :: <type> = <IDispatch>-vstruct;


define inline sealed sideways method dummy-value-for-type ( type == <BSTR> )
  => ( value :: <BSTR> );
  $NULL-BSTR
end;




define macro dual-type-info-definer
  { 
    define ?modifiers:* dual-type-info ?variable-name:name
				 (?supertype:expression)
      dispatch-type ?disptype:expression;
      ?slots-and-stuff:* 
    end 
  } => { 
    define internal-vtable-type-info ?variable-name (?supertype) 
      typeinfo-class { <dual-type-info> };
      class-options { dispatch: ?disptype };
      typeinfo { ?slots-and-stuff };
      members { ?slots-and-stuff }; 
    end;
   }
end macro;

define generic dispatch-type-information
    (interface :: type-union(<Interface>, <class>))
 => (result :: false-or(<LPTYPEINFO>));

define inline method dispatch-type-information (cls :: <class>)
 => (info :: singleton(#f))
  #f
end;

define inline method dispatch-type-information (cls :: subclass(<IDispatch>))
 => ( info :: false-or(<LPTYPEINFO>) )
  let typeinfo = type-information(cls);
  typeinfo & dispatch-type-information(typeinfo)
end;


define inline method dispatch-type-information (obj :: <IDispatch>)
 => (info :: false-or(<LPTYPEINFO>) )
  dispatch-type-information(object-class(obj));
end;


define inline method dispatch-type-information (info :: <dylan-type-info>)
 => (info :: singleton(#f))
  #f
end;

define inline method dispatch-type-information (info :: <disp-type-info> )
 => (info :: <disp-type-info>)
  info
end;

define COM-interface <dual-type-info> ( <vtable-type-info> )
  constant slot dispatch-type-information :: <LPTYPEINFO>,
	required-init-keyword: dispatch:;
end <dual-type-info>;

// From MSDN documentation:
//
// By default, the TYPEKIND enumeration for a dual interface is
// TKIND_INTERFACE. Tools that bind to interfaces should check the type
// flags for TYPEFLAG_FDUAL. If this flag is set, the TKIND_DISPATCH type
// description is available through a call to
// ITypeInfo::GetRefTypeOfImplType with an index of -1, followed by a call
// to ITypeInfo::GetRefTypeInfo.
 
define method initialize(this :: <dual-type-info>, #rest args, #key, #all-keys)
 => ();
  next-method();
  IUnknown/AddRef(this.dispatch-type-information);
  this.type-flags :=
    logior(this.type-flags, $TYPEFLAG-FDUAL, $TYPEFLAG-FOLEAUTOMATION);
end method initialize;

define method terminate(this :: <dual-type-info>) => ();
  next-method();
  IUnknown/Release(this.dispatch-type-information);
end method terminate;

define constant $href-dispatch = $href-inherited + 1; // arbitrary key

define method ITypeInfo/GetRefTypeOfImplType(this :: <dual-type-info>,
					     index == -1 )
 => (status :: <HRESULT>, reftype :: <integer>);
  values( $S-OK, $href-dispatch )
end;

define method ITypeInfo/GetRefTypeInfo(this :: <dual-type-info>,
				       hreftype == $href-dispatch)
  => (status :: <HRESULT>, ptinfo :: <LPTYPEINFO>);
  let typeinfo = this.dispatch-type-information;
  IUnknown/AddRef(typeinfo);
  values( $S-OK, typeinfo )
end;
