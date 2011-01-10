Module:    motley
Author:    Seth LaForge
Synopsis:  Manipulation and writing dylan clients for OLE type libraries
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sealed class <motley-error> (<simple-error>)
end class <motley-error>;

define sealed class <ignore-translation-error> (<error>)
end class <ignore-translation-error>;

define function motley-error (format-string :: <string>, #rest format-arguments)
	     => ()
  error(make(<motley-error>, format-string: format-string,
	     format-arguments: format-arguments));
end function;


// Pairs of strings - first element is name of pointer type, second element
// is name of base type.
define constant pointer-types-used = make(<deque>);


//////// PROTOCOL

define constant $target-types = 
	#(#"dispatch-clients", #"dispatch-servers",
	  #"vtable-interfaces", #"dual-interfaces");
// define constant <target-types> = apply(one-of, $target-types);
define constant <target-types> = 
	one-of(#"dispatch-clients", #"dispatch-servers",
	       #"vtable-interfaces", #"dual-interfaces");

// Parameters required for translation:
define class <motley-parameters> (<object>)
  constant slot target-type :: <target-types>, 
  	required-init-keyword: target-type:;
  constant slot server-suffix :: <string> = "",
  	init-keyword: server-suffix:;
  constant slot client-suffix :: false-or(<string>) = #f,
  	init-keyword: client-suffix:;
  constant slot to-translate :: false-or(<string-table>) = #f,
  	init-keyword: to-translate:;
end class <motley-parameters>;

define constant <mp> = <motley-parameters>;

define generic write-interface
	(s :: <stream>, this, parms :: <mp>, #key #all-keys) => ();


//// slot-defining classes:

define abstract class <motley-named> (<object>)
  slot c-name :: <string>, init-keyword: c-name:;
  slot dylan-name :: <string>, init-keyword: dylan-name:;
end class <motley-named>;

define abstract class <motley-documented> (<object>)
  slot doc-string :: false-or(<string>) = #f;
  slot help-file :: false-or(<string>) = #f;
  slot help-context :: <integer>;
end class <motley-documented>;

define abstract class <motley-more> (<motley-documented>)
  slot guid :: <string>;
//  slot lcid :: <integer>;
  slot major-version-number;
  slot minor-version-number;
end class <motley-more>;


//// Actual motley classes:

// Information about a type library - wraps an (ITypeLib *).
define sealed class <type-library> (<motley-named>, <motley-more>)
  slot contents :: <sequence>;
end class <type-library>;


// Information about an enumeration - wraps an (ITypeInfo *).
define sealed class <enumeration-info> (<motley-named>, <motley-more>)
  slot members :: <sequence>;
  slot hidden? :: <boolean>;
end class <enumeration-info>;

define sealed class <enumeration-member-info> (<motley-named>)
  constant slot member-value, init-keyword: value:;
end class <enumeration-member-info>;


// Information about a typedef - wraps an (ITypeInfo *).
define sealed class <typedef-info> (<motley-named>, <motley-more>)
  slot aliased-type :: <type-info>;
  slot hidden? :: <boolean>;
end class <typedef-info>;


// Information about a coclass - wraps an (ITypeInfo *).
define sealed class <coclass-info> (<motley-named>, <motley-more>)
  slot translation-errors :: <sequence>;
  slot default-interface :: false-or(<type-info>) = #f;
  slot interfaces :: <sequence>;
  slot hidden? :: <boolean>;
end class <coclass-info>;


// Information about an interface - wraps an (ITypeInfo *).
define abstract class <interface-info> (<motley-named>, <motley-more>)
  slot translation-errors :: <sequence>;
  slot member-functions :: <sequence>;
  slot hidden? :: <boolean>;
  slot superclass :: false-or(<user-type-info>) = #f;
end class <interface-info>;

define sealed class <dispatch-interface-info> (<interface-info>)
  slot dual? :: <boolean> = #f;
end;

define sealed class <vtable-interface-info> (<interface-info>)
end;


// Information about a struct - wraps an (ITypeInfo *).
// Structs are represented almost identically to interfaces.
define sealed class <struct-info> (<interface-info>)
end class <struct-info>;


// Information about a union - wraps an (ITypeInfo *).
// Unions are represented almost identically to interfaces.
define sealed class <union-info> (<interface-info>)
end class <union-info>;


// Information about a member function - wraps a (FUNCDESC *).
define class <function-info> (<motley-named>, <motley-documented>)
  slot id;
  slot function-type :: one-of(#"member-function", #"member-variable");
  slot has-getter :: <boolean> = #f;
  slot has-setter :: <boolean> = #f;
  slot element-function :: <boolean> = #f;
  slot size-function :: <boolean> = #f;
  slot results :: <sequence>;
  slot arguments :: <sequence>;
end class <function-info>;


// Information about a member function argument or return value.
define sealed class <argument-info> (<object>)
  constant slot argument-name :: <string>, init-keyword: name:;
  slot argument-type :: <type-info>, init-keyword: type:;
  constant slot argument-mode :: one-of(#"in", #"out", #"in-out"), init-keyword: mode:;
  constant slot argument-optional :: <boolean> = #f, init-keyword: optional:;
end class <argument-info>;


// Information about a type.
define abstract class <type-info> (<object>)
end class <type-info>;

define sealed class <variant-type-info> (<type-info>)
  slot variant-type /* :: <VARTYPE> */ ;
end class <variant-type-info>;

define sealed class <user-type-info> (<type-info>, <motley-named>)
//  slot user-type /* :: <HREFTYPE> */ ;
  slot type-kind :: <integer>;
end class <user-type-info>;

define sealed class <pointer-type-info> (<type-info>)
  slot enclosed-type :: <type-info>;
end class <pointer-type-info>;



// IMPLEMENTATION

define macro check-ole
  {
    check-ole(?:name(?args:*))
  } => {
    let (status :: <HRESULT>, #rest rest) = ?name(?args);
    // Temporary HACK to work around bugs 2709 and 3477
    let rest = as(<list>, rest);
    if (FAILED?(status))
      ole-error(status, ?"name", ?args);
    end if;
    apply(values, rest)
  }
end macro check-ole;


define macro get-documentation
  {
    get-documentation(?documentee:expression, ?documenter:*)
  } => {
    let (bstrName :: <BSTR>, bstrDocString :: <BSTR>, dwHelpContext,
	 bstrHelpFile :: <BSTR>) =
		check-ole(?documenter);
    ?documentee.c-name := copy-automation-value(bstrName);
    ?documentee.dylan-name := c-name-to-dylan-name(?documentee.c-name);
    if (size(bstrDocString) > 0)
      ?documentee.doc-string := copy-automation-value(bstrDocString);
    end if;
    if (size(bstrHelpFile) > 0)
      ?documentee.help-file := copy-automation-value(bstrHelpFile);
    end if;
    ?documentee.help-context := dwHelpContext;
    destroy(bstrName); destroy(bstrDocString); destroy(bstrHelpFile);
  }
end macro get-documentation;


define macro with-string-stream
  { 
    with-string-stream (?:name) ?:body end 
  } => { 
    let ?name = make(<string-stream>, direction: #"output");
    block ()
      ?body;
      ?name.stream-contents
    cleanup
      close(?name)
    end block;
  }
end macro with-string-stream;


define macro char-is-definer
  { define char-is ?:name = ?:expression } =>
  { define function "char-is-" ## ?name ## "?" (?=char :: <character>)
					    => (is-it? :: <boolean>)
      ?expression
    end function; }
end macro char-is-definer;

define char-is hex = ('0' <= char & char <= '9') |
                     ('a' <= char & char <= 'f') |
                     ('A' <= char & char <= 'F');

/* TODO: Verify that the complexity here is indeed unnecessary.

define char-is digit = '0' <= char & char <= '9';
define char-is alpha = ('a' <= char & char <= 'z') |
                       ('A' <= char & char <= 'Z');
define char-is name =
  select (char)
    '!','@','*','<','=','>','|','^','$','%','@','-','+','~','?','/' => #t;
    otherwise => #f;
  end select;

define function c-name-to-dylan-name (c-name :: <string>)
				  => (dylan-name :: <string>)
  let dylan-name :: <string> = copy-sequence(c-name);
  for (char in c-name, index from 0)
    if (~(char-is-alpha?(char) | char-is-name?(char) |
	  (char-is-digit?(char) & index > 0)))
      dylan-name[index] := '-'
    end if;
  end for;
  dylan-name
end function c-name-to-dylan-name;

*/

define function c-name-to-dylan-name (c-name :: <string>)
				  => (dylan-name :: <string>)
  // Since C names are a subset of Dylan names, our translation 
  // policy is simply to replace _ with -, except for leading 
  // underscores which are left alone. This is because leading hyphens
  // are parsed as unary - in Dylan.
  let dylan-name :: <string> = copy-sequence(c-name);
  let leading? :: <boolean> = #t;
  for (char in c-name, index from 0)
    if (char == '_' & ~leading?)
      dylan-name[index] := '-'
    else
      leading? := #f;
    end if;
  end for;
  dylan-name
end function c-name-to-dylan-name;

define method initialize (this :: <type-library>, #key file = #f,
			  typelib :: false-or(<interface>) = #f)
		      => ()

  next-method();

  if (file)
    let file-bstr = copy-as-BSTR(as(<string>, file));
    block ()
      typelib := check-ole(LoadTypeLib(file-bstr));
    cleanup
      destroy(file-bstr);
    end block;
  end if;

  let typelib :: <interface> = typelib;

  block ()

    get-documentation(this, ITypeLib/GetDocumentation(typelib, $MEMBERID-NIL));

    begin
      let (ptlibattr :: <LPTLIBATTR>) = check-ole(ITypeLib/GetLibAttr(typelib));
      this.guid := as(<string>, ptlibattr.guid-value);
//      this.lcid := ptlibattr.lcid-value;
      this.major-version-number := ptlibattr.wMajorVerNum-value;
      this.minor-version-number := ptlibattr.wMinorVerNum-value;
      ITypeLib/ReleaseTLibAttr(typelib, ptlibattr);
    end;

    begin
      let num-infos :: <integer> = ITypeLib/GetTypeInfoCount(typelib);
      this.contents := make(<deque>);
      for (type-ctr from 0 below num-infos)
	block ()
	  let type-kind :: <integer>
		  = check-ole(ITypeLib/GetTypeInfoType(typelib, type-ctr));
	  let type-info-types =
	    select (type-kind)
	      $TKIND-ENUM => list(<enumeration-info>);
	      $TKIND-ALIAS => list(<typedef-info>);
	      $TKIND-COCLASS => list(<coclass-info>);
	      $TKIND-INTERFACE => list(<vtable-interface-info>);
	      $TKIND-DISPATCH => list(<dispatch-interface-info>, 
				      <vtable-interface-info>);
	      $TKIND-RECORD => list(<struct-info>);
	      $TKIND-UNION => list(<union-info>);
	      otherwise #f; // Nothing.
	    end select;
	  for (type-info-type in type-info-types)
	    let iface :: <interface> =
		    check-ole(ITypeLib/GetTypeInfo(typelib, type-ctr));
	    block ()
	      push-last(this.contents, make(type-info-type, typeinfo: iface));
	    exception (<ignore-translation-error>)
	      #f;
	    end block;
	    Release(iface);
	  end for;
	exception (err :: <error>)
	  push-last(this.contents, err);
	end block;
      end for;
    end;

  cleanup

    if (file)
      Release(typelib);
    end if;

  end block;
end method initialize;


define method initialize (this :: <enumeration-info>,
			  #key typeinfo :: <interface>) => ()
  next-method();

  // Process documentation:
  get-documentation(this,
		    ITypeInfo/GetDocumentation(typeinfo, $MEMBERID-NIL));
  this.dylan-name := concatenate("<", this.dylan-name, ">");

  block ()
    // Process other interface info:
    let (ptattr :: <LPTYPEATTR>) = check-ole(ITypeInfo/GetTypeAttr(typeinfo));
    this.hidden? := 0 ~= logand(ptattr.wTypeFlags-value,
			       $TYPEFLAG-FHIDDEN + $TYPEFLAG-FRESTRICTED);
    let num-members :: <integer> = ptattr.cVars-value;
    ITypeInfo/ReleaseTypeAttr(typeinfo, ptattr);

    // Get the members:
    this.members := make(<deque>);
    let name :: <LPBSTR> = make(<LPBSTR>);
    block ()
      for (member-index from 0 below num-members)
	let member-info :: <LPVARDESC> =
		check-ole(ITypeInfo/GetVarDesc(typeinfo, member-index));
	let memid = member-info.memid-value;
	let value = member-info.u-value.lpvarValue-value.pointer-value;
	ITypeInfo/ReleaseVarDesc(typeinfo, member-info);
	let (num-filled :: <integer>)
		= check-ole(ITypeInfo/GetNames(typeinfo, memid, name, 1));
	if (num-filled ~= 1)
	  motley-error("Could not get name for member %d of enumeration %s\n",
		       member-index, this.dylan-name);
	end if;
	let c-name :: <string> = copy-automation-value(pointer-value(name));
	push-last(this.members,
		  make(<enumeration-member-info>, c-name: c-name,
		       dylan-name: concatenate
                                     ("$", c-name-to-dylan-name(c-name)),
		       value: value));
      end for;
    cleanup
      destroy(name);
    end block;
  exception (err :: <error>)
    error("translating enum %s: %s", this.c-name, err);
  end block;
end method initialize;


define method initialize (this :: <typedef-info>,
			  #key typeinfo :: <interface>) => ()
  next-method();

  // Process documentation:
  get-documentation(this, ITypeInfo/GetDocumentation(typeinfo, $MEMBERID-NIL));
  this.dylan-name := concatenate("<", this.dylan-name, ">");

  block ()
    // Process other interface info:
    let (ptattr :: <LPTYPEATTR>) = check-ole(ITypeInfo/GetTypeAttr(typeinfo));
    block ()
      this.hidden? := 0 ~= logand(ptattr.wTypeFlags-value,
				 $TYPEFLAG-FHIDDEN + $TYPEFLAG-FRESTRICTED);
      let aliased-typedesc :: <LPTYPEDESC> = ptattr.tdescAlias-value;
      this.aliased-type := make(<type-info>, typeinfo: typeinfo,
			      typedesc: aliased-typedesc);
    cleanup
      ITypeInfo/ReleaseTypeAttr(typeinfo, ptattr);
    end block;
  exception (err :: <error>)
    error("translating typedef %s: %s", this.c-name, err);
  end block;
end method initialize;


define method initialize (this :: <coclass-info>,
			  #key typeinfo :: <interface>) => ()
  next-method();

  // Process documentation:
  get-documentation(this, ITypeInfo/GetDocumentation(typeinfo, $MEMBERID-NIL));
  // this.dylan-name := concatenate("<", this.dylan-name, ">");

  block ()
    // Process other interface info:
    let (num-ifaces :: <integer>) =
      begin
	let (ptattr :: <LPTYPEATTR>) =
		check-ole(ITypeInfo/GetTypeAttr(typeinfo));
	this.hidden? := 0 ~= logand(ptattr.wTypeFlags-value,
				   $TYPEFLAG-FHIDDEN + $TYPEFLAG-FRESTRICTED);
	this.guid := as(<string>, ptattr.guid-value);
//	this.lcid := ptattr.lcid-value;
	this.major-version-number := ptattr.wMajorVerNum-value;
	this.minor-version-number := ptattr.wMinorVerNum-value;
	let num-ifaces :: <integer> = ptattr.cImplTypes-value;
	ITypeInfo/ReleaseTypeAttr(typeinfo, ptattr);
	values(num-ifaces)
      end;
    this.translation-errors := make(<deque>);
    this.interfaces := make(<deque>);

    // Get interface info:
    for (index from 0 below num-ifaces)
      block ()
	let reftype =
		check-ole(ITypeInfo/GetRefTypeOfImplType(typeinfo, index));
	let flags = check-ole(ITypeInfo/GetImplTypeFlags(typeinfo, index));
	let interface-typeinfo = 
		make(<pointer-type-info>, enclosed-type:
		     make(<user-type-info>,
			  user-type: reftype, typeinfo: typeinfo));
	if (0 ~= logand(flags, $IMPLTYPEFLAG-FSOURCE))
	  motley-error("source interface %s not supported", this.c-name);
	elseif (0 ~= logand(flags, $IMPLTYPEFLAG-FDEFAULT))
	  if (this.default-interface)
	    motley-error("more than one default interface");
	  end if;
	  this.default-interface := interface-typeinfo;
	else
	  push-last(this.interfaces, interface-typeinfo);
	end if;
      exception (e :: <motley-error>)
	push-last(this.translation-errors, e);
      end block;
    end for;

  exception (err :: <error>)
    error("translating coclass %s: %s", this.c-name, err);
  end block;
end method initialize;



define method initialize (this :: <dispatch-interface-info>,
			  #key typeinfo :: <interface>) => ()
  next-method();
  let (err, vtable-HREF) = ITypeInfo/GetRefTypeOfImplType(typeinfo, -1);
  this.dual? := (err == $S-OK);
end method initialize;


define method initialize (this :: <vtable-interface-info>,
			  #key typeinfo :: <interface>) => ()
  let (ptattr :: <LPTYPEATTR>) = check-ole(ITypeInfo/GetTypeAttr(typeinfo));
  let typekind = ptattr.typekind-value;
  ITypeInfo/ReleaseTypeAttr(typeinfo, ptattr);
  if (typekind == $TKIND-DISPATCH)
    let (err, vtable-HREF) = ITypeInfo/GetRefTypeOfImplType(typeinfo, -1);
    if (err == $S-OK)
      let vtable-interface = 
	      check-ole(ITypeInfo/GetRefTypeInfo(typeinfo, vtable-HREF));
      initialize(this, typeinfo: vtable-interface);
    else
      error(make(<ignore-translation-error>));
    end if;
  else
    next-method();
  end if;
end method initialize;


define method initialize (this :: <interface-info>,
			  #key typeinfo :: <interface>) => ()
  next-method();

  // Process documentation:
  get-documentation(this, ITypeInfo/GetDocumentation(typeinfo, $MEMBERID-NIL));
  this.dylan-name := concatenate("<", this.dylan-name, ">");

  block ()
    // Process other interface info:
    let (num-funcs :: <integer>, num-props :: <integer>, 
	 num-superclasses :: <integer>) =
      begin
	let (ptattr :: <LPTYPEATTR>) = 
		check-ole(ITypeInfo/GetTypeAttr(typeinfo));
	this.hidden? := 0 ~= logand(ptattr.wTypeFlags-value,
				   $TYPEFLAG-FHIDDEN + $TYPEFLAG-FRESTRICTED);
	this.guid := as(<string>, ptattr.guid-value);
//	this.lcid := ptattr.lcid-value;
	this.major-version-number := ptattr.wMajorVerNum-value;
	this.minor-version-number := ptattr.wMinorVerNum-value;
	let num-funcs :: <integer> = ptattr.cFuncs-value;
	let num-props :: <integer> = ptattr.cVars-value;
	let num-superclasses :: <integer> = ptattr.cImplTypes-value;
	ITypeInfo/ReleaseTypeAttr(typeinfo, ptattr);
	values(num-funcs, num-props, num-superclasses)
      end;
    this.translation-errors := make(<deque>);
    this.member-functions := make(<deque>);

    // Get superclass info:
    if (num-superclasses > 1)
      motley-error("interface %s has more than one superclass", this.c-name);
    elseif (num-superclasses = 1)
      let super-href = check-ole(ITypeInfo/GetRefTypeOfImplType(typeinfo, 0));
      this.superclass := make(<user-type-info>,
			      user-type: super-href, typeinfo: typeinfo);
    end if;

    // Get property info:
    let name :: <LPBSTR> = make(<LPBSTR>);
    block ()
      for (member-index from 0 below num-props)
	block ()
	  push-last(this.member-functions,
		    make(<function-info>, interface-info: this,
			 typeinfo: typeinfo, index: member-index,
			 kind: #"member-variable",
			 vtable?: instance?(this, <vtable-interface-info>)));
	exception (e :: <motley-error>)
	  push-last(this.translation-errors, e);
	end block;
      end for;
    cleanup
      destroy(name);
    end block;

    // Get member function info:
    for (function-index from 0 below num-funcs)
      block ()
	let function-info :: <function-info> =
		make(<function-info>, interface-info: this, typeinfo: typeinfo,
		     index: function-index, kind: #"member-function",
		     vtable?: instance?(this, <vtable-interface-info>));
	let match :: false-or(<function-info>) =
	       any?(method (f2-info :: <function-info>) => (r)
		      function-info.id = f2-info.id & f2-info
		    end method, this.member-functions);
	if (match)
	  match.has-getter := match.has-getter | function-info.has-getter;
	  match.has-setter := match.has-setter | function-info.has-setter;
	else
	  push-last(this.member-functions, function-info);
	end if;
      exception (<ignore-translation-error>)
	#f; // Do nothing.
      exception (e :: <motley-error>)
	push-last(this.translation-errors, e);
      end block;
    end for;
  exception (err :: <error>)
    error("translating interface %s: %s", this.c-name, err);
  end block;
end method initialize;


define method initialize 
	(this :: <function-info>, #key interface-info :: <interface-info>,
	 typeinfo :: <interface>, index :: <integer>,
	 kind :: one-of(#"member-function", #"member-variable"),
	 vtable? :: <boolean> = #f) => ()

  next-method();

  select (this.c-name by \=)
    // I don't know why these come through:
    // IUnknown:
    "QueryInterface", "AddRef", "Release",
    // IDispatch:
    "Invoke", "GetIDsOfNames", "GetTypeInfo", "GetTypeInfoCount" => 
	error(make(<ignore-translation-error>));
    otherwise => #f;
  end select;

  this.dylan-name := concatenate(c-name-to-dylan-name(interface-info.c-name),
				 "/", this.dylan-name);

  this.arguments := make(<deque>);
  this.results := make(<deque>);
  this.function-type := #"member-variable";

  select (kind)
    #"member-function" =>
      initialize-member-function(this, typeinfo, index, vtable?);
    #"member-variable" =>
      initialize-member-variable(this, typeinfo, index);
  end;

  get-documentation(this, ITypeInfo/GetDocumentation(typeinfo, this.id));

  // Recognize element methods:
  this.element-function := this.function-type = #"member-variable" &
			   this.id = 0 & /* this.c-name = "Item" & */
			   this.arguments.size = 1;

  // Recognize size methods:
  this.size-function := this.function-type = #"member-variable" &
			this.has-getter & this.c-name = "Count" &
			this.arguments.size = 0;
end method initialize;

define method initialize-member-variable
    (this :: <function-info>, typeinfo :: <interface>, index :: <integer>)

  let fd :: <LPVARDESC> = check-ole(ITypeInfo/GetVarDesc(typeinfo, index));

  block ()
    this.id := fd.memid-value;
    this.has-getter := #t;
    this.has-setter := #t;
    push-last(this.results,
	      make(<argument-info>, name: "result",
		   type: make(<type-info>,
			      typedesc: fd.elemdescvar-value.tdesc-value,
			      typeinfo: typeinfo),
		   mode: #"out"));
  cleanup
    ITypeInfo/ReleaseVarDesc(typeinfo, fd);
  end block;
end method initialize-member-variable;


define method initialize-member-function
    (this :: <function-info>, typeinfo :: <interface>, index :: <integer>, vtable? :: <boolean>)

  let fd :: <LPFUNCDESC>
    = check-ole(ITypeInfo/GetFuncDesc(typeinfo, index));

  block ()
    this.id := fd.memid-value;

    select (fd.invkind-value)
      $INVOKE-FUNC => this.function-type := #"member-function";
      $INVOKE-PROPERTYGET => this.has-getter := #t;
      $INVOKE-PROPERTYPUT => this.has-setter := #t;
      $INVOKE-PROPERTYPUTREF =>
	  motley-error("Cannot translate PROPERTYPUTREF method %s",
		       this.c-name);
    end select;

    let num-args :: <integer> = fd.cParams-value;
    // Element 0 will be the function name:
    let argument-names :: <LPBSTR> = make(<LPBSTR>,
					  element-count: num-args + 1);
    block ()
      let (num-args-filled :: <integer>) =
	      check-ole(ITypeInfo/GetNames(typeinfo, this.id,
					   argument-names, num-args + 1));

      let return-typedesc :: <LPTYPEDESC> =
	      fd.elemdescFunc-value.tdesc-value;
      if (~vtable? & this.function-type = #"member-function")
	push-last(this.results,
		  make(<argument-info>,
		       name: "status",
		       type: make(<variant-type-info>, 
				  variant-type: $VT-HRESULT),
		       mode: #"out"));
      end if;
      if (return-typedesc.vt-value ~= $VT-VOID)
	push-last(this.results,
		  make(<argument-info>,
		       name: "result",
		       type: make(<type-info>, typedesc: return-typedesc,
				  typeinfo: typeinfo),
		       mode: #"out"));
      end if;

      for (argument-index from 0 below num-args)
	let elemdesc :: <LPELEMDESC> = pointer-value-address(
		fd.lprgelemdescParam-value, 
		index: argument-index);
	let paramflags = elemdesc.u-value.paramdesc-value.wParamFlags-value;
	let in :: <boolean> = 0 ~= logand(paramflags, $PARAMFLAG-FIN);
	let out :: <boolean> = 0 ~= logand(paramflags, $PARAMFLAG-FOUT);
	let opt :: <boolean> = 0 ~= logand(paramflags, $PARAMFLAG-FOPT);
	let mode = if (in & out) #"in-out" elseif (out) #"out"
		   else #"in" end if;
	let arg :: <argument-info> =
		make(<argument-info>,
		     name:
		      if (argument-index < num-args-filled - 1)
			copy-automation-value(pointer-value(argument-names,
					       index: argument-index + 1))
		      else
			format-to-string("unnamed-%s", argument-index + 1)
		      end if,
		     type: make(<type-info>, typedesc: elemdesc.tdesc-value,
				typeinfo: typeinfo),
		     mode: mode, optional: opt);
	if (arg.argument-mode = #"out" | arg.argument-mode = #"in-out")
	  if (~ instance?(arg.argument-type, <pointer-type-info>))
	    motley-error("%= parameter %s :: %= is not a pointer",
			 arg.argument-mode, arg.argument-name,
			 with-string-stream (ss)
			   write-interface(ss, arg.argument-type, 
				   make(<motley-parameters>,
					target-type: #"dispatch-clients"));
			 end with-string-stream);
	  end if;
	end if;
	push-last(this.arguments, arg);
      end for;
    cleanup
      destroy(argument-names);
    end block;

    select (this.function-type)
      #"member-variable" =>
	if (vtable?)
	  this.results.size := 0;	// Get rid of HRESULT.
	  if (empty?(this.arguments))
	    motley-error("Cannot translate getter/putter %s: no parameters",
			 this.c-name);
	  end if;
	  let result-arg = pop(this.arguments);
	  if (this.has-getter)
	    if (~instance?(result-arg.argument-type, <pointer-type-info>))
	      motley-error("Cannot translate getter %s: output argument is "
			   "not a pointer", this.c-name);
	    end if;
	    result-arg.argument-type := 
		    result-arg.argument-type.enclosed-type;
	  end if;
	  push(this.results, result-arg);
	else
	  // Fix up setters to have a getter format:
	  if (this.has-setter)
	    if (empty?(this.arguments))
	      motley-error("Cannot translate putter %s: no parameters",
			   this.c-name);
	    end if;
	    push(this.results, pop(this.arguments));
	  end if;
	end if;
      #"member-function" =>
	if (vtable? & this.results.size = 1)
	  let new-results = make(<deque>);
	  while (~this.arguments.empty? & 
		 this.arguments.last.argument-mode = #"out")
	    let last-arg = pop-last(this.arguments);
	    last-arg.argument-type := last-arg.argument-type.enclosed-type;
	    push(new-results, last-arg);
	  end while;
	  this.results := concatenate!(this.results, new-results);
	end if;
    end select;

  cleanup
    ITypeInfo/ReleaseFuncDesc(typeinfo, fd);
  end block;
end method initialize-member-function;


define method make (class == <type-info>, #rest keys,
		    #key typedesc :: <LPTYPEDESC>, #all-keys)
		=> (r :: <type-info>)
  apply(make,
  	select (typedesc.vt-value)
	  $VT-PTR, $VT-SAFEARRAY /* , $VT-CARRAY */ => <pointer-type-info>;
	  $VT-USERDEFINED => <user-type-info>;
	  otherwise => <variant-type-info>;
	end select, typedesc: typedesc, keys)
end method make;


define method initialize (this :: <variant-type-info>,
			  #key variant-type: vt = #f,
			  typedesc :: false-or(<LPTYPEDESC>) = #f, 
			  #all-keys) => ()
  next-method();
  this.variant-type := vt | (typedesc & typedesc.vt-value);
end method initialize;


define method initialize (this :: <user-type-info>,
			  #key typedesc :: false-or(<LPTYPEDESC>) = #f,
			  user-type: reftype = #f,
			  typeinfo :: <interface>,
			  #all-keys) => ()
  next-method();
  if (~reftype)
    when (typedesc)
      reftype := typedesc.u-value.hreftype-value;
    end;
  end if;
//  this.user-type := reftype;
  let (user-typeinfo :: <interface>) =
	  check-ole(ITypeInfo/GetRefTypeInfo(typeinfo, reftype));
  block ()
    let (bstrName :: <BSTR>, bstrDocString :: <BSTR>, dwHelpContext,
	 bstrHelpFile :: <BSTR>) =
	    check-ole(ITypeInfo/GetDocumentation(user-typeinfo, $MEMBERID-NIL));
    this.c-name := copy-automation-value(bstrName);
    this.dylan-name := c-name-to-dylan-name(this.c-name);
    destroy(bstrName); destroy(bstrDocString); destroy(bstrHelpFile);

    let (ptattr :: <LPTYPEATTR>) = check-ole(ITypeInfo/GetTypeAttr(user-typeinfo));
    this.type-kind := ptattr.typekind-value;
    ITypeInfo/ReleaseTypeAttr(user-typeinfo, ptattr);
  cleanup
    Release(user-typeinfo);
  end block;
end method initialize;


define method initialize (this :: <pointer-type-info>,
			  #key typedesc :: false-or(<LPTYPEDESC>) = #f,
			  typeinfo :: false-or(<interface>) = #f, 
			  result? :: <boolean> = #f,
			  enclosed-type: enc :: false-or(<type-info>) = #f) 
		      => ()
  next-method();
  this.enclosed-type := enc | (typedesc &
                                 make(<type-info>,
				      typedesc: typedesc.u-value.lptdesc-value,
				      typeinfo: typeinfo));
  unless (instance?(this.enclosed-type, <user-type-info>) &
	  (this.enclosed-type.type-kind = $TKIND-DISPATCH |
	   this.enclosed-type.type-kind = $TKIND-INTERFACE))
    let (dylan-types, c-type) = type-base-names(this.enclosed-type);
    unless (c-type)
      motley-error("invalid type: pointer to %s", 
	      with-string-stream (ss)
		write-interface(ss, this.enclosed-type, 
		  make(<motley-parameters>, target-type: #"dispatch-clients"));
	      end with-string-stream);
    end unless;
  end unless;
end method initialize;

define function write-comma-seperated-list
	(s :: <stream>, list :: <sequence>,
	 #key seperator :: <string> = ", ", writer :: <function> = write) => ()
  let sep :: <string> = "";
  for (element in list)
    format(s, "%s", sep);
    writer(s, element);
    sep := seperator;
  end for;
end function write-comma-seperated-list;

define method write-module 
	(s :: <stream>, this :: <type-library>, module :: <string>, p :: <mp>) 
     => ()
  local method write-exclude () => ()
    if (~ pointer-types-used.empty?)
      write(s, ", exclude: { ");
      write-comma-seperated-list(s, pointer-types-used,
	      writer: method(s, t) format(s, "<%s>", t.head) end method);
      write(s, " }");
    end if;
    format(s, ";\n");
  end method write-exclude;
  format(s, "define module %s\n", module);
  format(s, "  use functional-dylan;\n");
  format(s, "  use ole-automation"); write-exclude();
  format(s, "  use c-ffi"); write-exclude();
  for (interface in this.contents)
    if (translate?(interface, p))
      let bindings :: <deque> = make(<deque>);
      get-bindings(interface, bindings, p);
      if (~bindings.empty?)
	write(s, "  export ");
	write-comma-seperated-list(s, bindings);
	format(s, ";\n");
      end if;
    end if;
  end for;
  format(s, "end module %s;\n", module);
end method write-module;


define generic translate? 
	(this, p :: <mp>) 
     => (r :: <boolean>, unsupported? :: <boolean>, unsupported-message);

define method translate? 
	(this, p :: <mp>) 
     => (r :: <boolean>, unsupported? :: <boolean>, unsupported-message)
  values(#t, #f, #f)
end method translate?;

define method translate? 
	(this :: type-union(<interface-info>, <coclass-info>), p :: <mp>) 
     => (r :: <boolean>, unsupported? :: <boolean>, unsupported-message)
  values(~p.to-translate | element(p.to-translate, this.c-name, default: #f), 
	 #f, #f)
end method translate?;

define method translate?
	(this :: <dispatch-interface-info>, p :: <mp>) 
     => (r :: <boolean>, unsupported? :: <boolean>, unsupported-message)
  values((~p.to-translate | element(p.to-translate, this.c-name, default: #f)) &
	 (p.target-type == #"dispatch-clients" | 
	  p.target-type == #"dispatch-servers" |
	  (p.target-type == #"dual-interfaces" & this.dual?)), 
	 #f, #f)
end method translate?;

define method translate?
	(this :: <vtable-interface-info>, p :: <mp>) 
     => (r :: <boolean>, unsupported? :: <boolean>, unsupported-message)
  values((~p.to-translate | element(p.to-translate, this.c-name, default: #f)) &
	 (p.target-type == #"vtable-interfaces"), 
	 #f, #f)
end method translate?;

define method translate? 
	(this :: <struct-info>, p :: <mp>) 
     => (r :: <boolean>, unsupported? :: <boolean>, unsupported-message)
  values(#t, #f, #f)
end method translate?;

define method translate? 
	(this :: <function-info>, p :: <mp>) 
     => (r :: <boolean>, unsupported? :: <boolean>, unsupported-message)
  let tt = p.target-type;
  if (tt ~== #"dispatch-clients" & 
      this.function-type == #"member-variable")
    if (~this.arguments.empty?)
      values(#f, #t, format-to-string(
	     "Indexed properties are not supported by %s", as(<string>, tt)));
    elseif (tt ~== #"vtable-interfaces" & ~this.has-getter & this.has-setter)
      values(#f, #t, format-to-string(
	   "Write-only properties are not supported by %s", as(<string>, tt)));
    else
      values(#t, #f, #f)
    end if
  else
    values(#t, #f, #f)
  end if;
end method translate?;


define generic get-bindings (this, bindings :: <deque>, p :: <mp>) 
			 => ();

define method get-bindings (this :: <object>, bindings :: <deque>, p :: <mp>) 
			=> ()
  // NOP
end method get-bindings;

define method get-bindings (this :: <motley-named>, bindings :: <deque>,
			    p :: <mp>) => ()
  push-last(bindings, this.dylan-name);
end method get-bindings;

define method get-bindings (this :: <type-library>, bindings :: <deque>,
			    p :: <mp>) => ()
  for (interface in this.contents)
    if (translate?(interface, p))
      get-bindings(interface, bindings, p);
    end if;
  end for;
end method get-bindings;

define method get-bindings (this :: <enumeration-info>, bindings :: <deque>,
			    p :: <mp>) => ()
  if (#t | ~ this.hidden?)
    next-method();
    for (member in this.members)
      get-bindings(member, bindings, p);
    end for;
  end if;
end method get-bindings;

define method get-bindings (this :: <typedef-info>, bindings :: <deque>,
			    p :: <mp>) => ()
  if (#t | ~ this.hidden?) next-method(); end if;
end method get-bindings;

define method get-bindings (this :: <coclass-info>, bindings :: <deque>,
			    p :: <mp>) => ()
  push-last(bindings, concatenate("$", this.dylan-name, "-class-id"));
  if (p.target-type ~= #"dispatch-servers")
    push-last(bindings, concatenate("make-", this.dylan-name));
  end if;
end method get-bindings;

define method get-bindings (this :: <dispatch-interface-info>, 
			    bindings :: <deque>, p :: <mp>) => ()
  if (#t | ~ this.hidden?)
    if (p.target-type = #"dual-interfaces")
      get-vtable-bindings(this, bindings, p);
    else
      next-method();
    end if;
  end if;
end method get-bindings;

define method get-bindings (this :: <vtable-interface-info>, 
			    bindings :: <deque>, p :: <mp>) => ()
  if (#t | ~ this.hidden?)
    get-vtable-bindings(this, bindings, p);
  end if;
end method get-bindings;

define function get-vtable-bindings (this :: <interface-info>, 
				     bindings :: <deque>, p :: <mp>) => ()
  push-last(bindings, format-to-string("<%s%s>", 
				       c-name-to-dylan-name(this.c-name),
				       p.server-suffix));
  push-last(bindings, format-to-string("$iid-<%s%s>", 
				       c-name-to-dylan-name(this.c-name),
				       p.server-suffix));
  if (p.client-suffix)
    push-last(bindings, format-to-string("<%s%s>", 
					 c-name-to-dylan-name(this.c-name),
					 p.client-suffix));
  end if;
  for (member in this.member-functions)
    get-bindings(member, bindings, p);
  end for;
end function get-vtable-bindings;

define method get-bindings (this :: <interface-info>, bindings :: <deque>,
			    p :: <mp>) => ()
  if (#t | ~ this.hidden?)
    next-method();
    for (member in this.member-functions)
      get-bindings(member, bindings, p);
    end for;
  end if;
end method get-bindings;

define method get-bindings (this :: <function-info>, bindings :: <deque>,
			    p :: <mp>) => ()
  if (translate?(this, p))
    if (this.function-type = #"member-variable")
      if (this.has-getter)
	push-last(bindings, this.dylan-name);
      end if;
      if (this.has-setter)
	push-last(bindings, concatenate(this.dylan-name, "-setter"));
      end if;
    else
      next-method();
    end if;
  end if;
end method get-bindings;


define method write-interface
	(s :: <stream>, this :: <type-library>, p :: <mp>, #key) => ()
  format(s, "/* Type library: %s version %=.%=\n", this.c-name,
	 this.major-version-number, this.minor-version-number);
  if (this.doc-string)
    format(s, " * Description: %s\n", this.doc-string);
  end if;
  format(s, " * GUID: %s\n */\n", this.guid);
  for (interface in this.contents)
    if (translate?(interface, p))
      format(s, "\n\n");
      write-interface(s, interface, p);
    end if;
  end for;
  write-pointer-definitions(s);
end method write-interface;


define method write-interface (s :: <stream>, this :: <error>, p :: <mp>, #key) 
			   => ()
  format(s, "/* Translation error: %s */\n", condition-to-string(this));
end method write-interface;


define method write-interface
	(s :: <stream>, this :: <enumeration-info>, p :: <mp>, #key) => ()
  write(s, "/* ");
  if (this.hidden?) write(s, "hidden? "); end if;
  format(s, "Enumeration: %s\n", this.c-name);
  if (this.doc-string)
    format(s, " * Description: %s\n", this.doc-string);
  end if;
  format(s, " */\n");
  format(s, "define constant %s = type-union(<integer>, <machine-word>);\n", 
	 this.dylan-name);
  for (member in this.members)
    format(s, "define constant %s = ",
	   member.dylan-name);
    if (instance?(member.member-value, <machine-word>))
      format(s, "as(<machine-word>, %=)", member.member-value);
    else
      format(s, "%=", member.member-value);
    end if;
    format(s, ";\n");
  end for;
end method write-interface;


define method write-interface
	(s :: <stream>, this :: <typedef-info>, p :: <mp>, #key) => ()
  write(s, "/* ");
  if (this.hidden?) write(s, "hidden? "); end if;
  format(s, "Typedef: %s\n", this.c-name);
  if (this.doc-string)
    format(s, " * Description: %s\n", this.doc-string);
  end if;
  format(s, " */\n");
  format(s, "define constant %s = ", this.dylan-name);
  write-interface(s, this.aliased-type, p);
  format(s, ";\n");
end method write-interface;


define method write-interface
	(s :: <stream>, this :: <coclass-info>, p :: <mp>, #key) => ()
  write(s, "/* ");
  if (this.hidden?) write(s, "hidden? "); end if;
  format(s, "COM class: %s version %=.%=\n", this.c-name,
	 this.major-version-number, this.minor-version-number);
  format(s, " * GUID: %s\n", this.guid);
  if (this.doc-string)
    format(s, " * Description: %s\n", this.doc-string);
  end if;
  format(s, " */\n");

  format(s, "define constant $%s-class-id = as(<REFCLSID>, \"%s\");\n\n",
  	 this.dylan-name, this.guid);

  if (p.target-type = #"dispatch-clients" | 
      p.target-type = #"vtable-interfaces" | 
      p.target-type = #"dual-interfaces")
    format(s, "define function make-%s () => (default-interface :: ",
	   this.dylan-name);
    write-interface(s, this.default-interface, p);
    for (iface in this.interfaces, index from 2)
      format(s, ", interface-%d :: ", index);
      write-interface(s, iface, p);
    end for;
    format(s, ")\n");
    for (error in this.translation-errors)
      format(s, "  /* Translation error: %s. */\n", 
	     condition-to-string(error));
    end for;
    write(s, "  let default-interface = ");
    select (p.target-type)
      #"dispatch-clients" =>
	write(s, "make(");
	write-interface(s, this.default-interface, p);
	format(s, ", class-id: $%s-class-id);\n", this.dylan-name);
	format(s, "  values(default-interface");
	for (iface in this.interfaces)
	  format(s, ",\n         make(");
	  write-interface(s, iface, p);
	  format(s, ", disp-interface: default-interface)");
	end for;
	format(s, ")\n");
      #"vtable-interfaces", #"dual-interfaces" =>
	write(s, "pointer-cast(");
	write-interface(s, this.default-interface, p);
	format(s, ", create-COM-instance($%s-class-id, "
	       "interface-id: $iid-<%s%s>));\n", this.dylan-name,
	       this.default-interface.enclosed-type.dylan-name, 
	       p.server-suffix);
	for (iface in this.interfaces, index from 2)
	  format(s, "  let (status :: <HRESULT>, interface-%d) = "
		 "QueryInterface(default-interface, $iid-<%s%s>);\n",
		 index, iface.enclosed-type.dylan-name, 
		 p.server-suffix);
	  format(s, "  check-ole-status(status, \"QueryInterface\", "
		 "default-interface, \"$iid-<%s%s>\");\n",
		 iface.enclosed-type.dylan-name, p.server-suffix);
	end for;
	format(s, "  values(default-interface");
	for (iface in this.interfaces, index from 2)
	  format(s, ",\n         pointer-cast(");
	  write-interface(s, iface, p);
	  format(s, ", interface-%d)", index);
	end for;
	format(s, ")\n");
    end select;
    format(s, "end function make-%s;\n", this.dylan-name);
    if (custom-interface-type?(p.target-type))
      s.new-line;
    end if;
  end if;

  if (p.target-type = #"dispatch-servers" | 
      p.target-type = #"vtable-interfaces" | 
      p.target-type = #"dual-interfaces")
    format(s, "/* You could define your coclass something like this:\n");
    format(s, "define coclass $%s-type-info\n", this.dylan-name);
    format(s, "  name %=;\n", this.c-name);
    format(s, "  uuid $%s-class-id;\n", this.dylan-name);
    for (error in this.translation-errors)
      format(s, "  /* Translation error: %s. */\n", 
	     condition-to-string(error));
    end for;
    format(s, "  default interface ");
    if (custom-interface-type?(p.target-type))
      format(s, "<%s%s>", this.default-interface.enclosed-type.dylan-name, 
	     p.server-suffix);
    else
      write-interface(s, this.default-interface, p);
    end if;
    format(s, ";\n");
    for (iface in this.interfaces)
      format(s, "  interface ");
      if (custom-interface-type?(p.target-type))
	format(s, "<%s%s>", iface.enclosed-type.dylan-name, 
	       p.server-suffix);
      else
	write-interface(s, iface, p);
      end if;
      format(s, ";\n");
    end for;
    format(s, "end coclass;\n*/\n");
  end if;
end method write-interface;


define method write-interface
	(s :: <stream>, this :: <dispatch-interface-info>, p :: <mp>, #key) 
     => ()
  select (p.target-type)
    #"dispatch-clients", #"dispatch-servers", #"dual-interfaces" => next-method();
    otherwise => #f;
  end select;
end method write-interface;


define method write-interface
	(s :: <stream>, this :: <vtable-interface-info>, p :: <mp>, #key) => ()
  select (p.target-type)
    #"vtable-interfaces" => next-method();
    otherwise => #f;
  end select;
end method write-interface;


define function custom-interface-type? (tt)
  tt == #"vtable-interfaces" | tt == #"dual-interfaces"
end;

define method write-interface
	(s :: <stream>, this :: <interface-info>, p :: <mp>, #key) => ()
  if (~p.to-translate | element(p.to-translate, this.c-name, default: #f))
    let tt = p.target-type;
    format(s, "/* %s%s interface: %s version %=.%=\n"
	      " * GUID: %s\n",
	   if (this.hidden?) "hidden " else "" end,
	   select (this.object-class)
	     <vtable-interface-info> => "Custom";
	     <dispatch-interface-info> => "Dispatch";
	   end select,
	   this.c-name,
	   this.major-version-number, this.minor-version-number,
	   this.guid);
    if (this.doc-string)
      format(s, " * Description: %s\n", this.doc-string);
    end if;
    format(s, " */\n");
    let interface-type = select (tt)
			   #"dispatch-clients" => "dispatch-client";
			   #"dispatch-servers" => "dispatch-interface";
			   #"dual-interfaces" => "dual-interface";
			   #"vtable-interfaces" => "vtable-interface";
			 end select;
    let interface-name = 
      if (custom-interface-type?(tt))
	concatenate("<", c-name-to-dylan-name(this.c-name),
		    p.server-suffix, ">")
      else
	this.dylan-name
      end if;
    format(s, "define %s%s %s", 
	   if (tt ~= #"dispatch-clients") "open " else "" end if,
	   interface-type, interface-name);
    select (tt)
      #"dispatch-servers", #"dual-interfaces" => 
	  format(s, " (<simple-dispatch>)\n");
      #"vtable-interfaces" => 
	  if (this.superclass)
	    format(s, " (<%s%s>)\n", 
		   this.superclass.dylan-name, 
		   select (this.superclass.dylan-name by \=)
		     "IUnknown", "IDispatch" => "";
		     otherwise => p.server-suffix;
		   end select);
	  else
	    format(s, " (<IUnknown>)\n");
	  end if;
      #"dispatch-clients" =>
	  format(s, "\n");
    end select;
    for (error in this.translation-errors)
      format(s, "  /* Translation error: %s. */\n", condition-to-string(error));
    end for;
    if (custom-interface-type?(tt) & p.client-suffix)
      format(s, "  client-class <%s%s>", 
	     c-name-to-dylan-name(this.c-name), p.client-suffix);
      select (this.superclass.dylan-name by \=)
        "IUnknown", "IDispatch" => #f;
        otherwise => format(s, " (<%s%s>)", this.superclass.dylan-name, 
			    p.client-suffix);
      end select;
      format(s, ";\n");
    end if;
    format(s, "  uuid \"%s\";\n", this.guid);
    let c-types? = tt = #"dispatch-servers" |
                   tt = #"dual-interfaces" |
    		   tt = #"vtable-interfaces";
    for (function-info in this.member-functions)
      s.new-line;
      write-interface(s, function-info, p, owner: this, c-types?: c-types?);
    end for;
    format(s, "end %s %s;\n", interface-type, interface-name);
    if (tt = #"dispatch-servers")
      new-line(s);
      for (function-info in this.member-functions)
	write-interface(s, function-info, p, owner: this, generic?: #t)
      end for;
    end if;
  end if;
end method write-interface;


define method write-interface
	(s :: <stream>, this :: type-union(<struct-info>, <union-info>),
	 p :: <mp>, #key) => ()
  write(s, "/* ");
  if (this.hidden?) write(s, "hidden? "); end if;
  let type-name = select (this.object-class)
    <struct-info> => "struct";
    <union-info> => "union";
  end select;
  format(s, "%s: %s version %=.%=\n", type-name, this.c-name,
	 this.major-version-number, this.minor-version-number);
  if (this.doc-string)
    format(s, " * Description: %s\n", this.doc-string);
  end if;
  format(s, " */\n");
  format(s, "define C-%s %s\n", type-name, this.dylan-name);
  for (error in this.translation-errors)
    format(s, "  /* Translation error: %s. */\n", condition-to-string(error));
  end for;
  for (function-info in this.member-functions)
    if (function-info.doc-string)
      format(s, "  /* %s */\n", this.doc-string);
    end if;
    if (function-info.function-type ~= #"member-variable")
      format(s, "  /* Strange - this %s has a member function %s! */\n",
	     type-name, function-info.dylan-name);
    else
      format(s, "  slot %s :: ", function-info.dylan-name);
      write-interface(s, function-info.results.first.argument-type, p, 
		      c-type?: #t);
      format(s, ";\n");
    end if;
  end for;
  format(s, "end C-%s %s;\n", type-name, this.dylan-name);
end method write-interface;


define constant $general-function-name = "vtable-member";
define constant $standard-function-name = "function";
define constant $standard-variable-name = "property";


define method write-interface
	(s :: <stream>, this :: <function-info>, p :: <mp>, 
	 #key generic? :: <boolean> = #f, owner :: <interface-info>,
	 c-types? :: <boolean> = #f) => ()
  let tt = p.target-type;
  let type-writer = 
	  rcurry(write-interface, p, c-type?: c-types?, 
		 use-refs?: ~generic? & tt ~= #"vtable-interfaces");
  let result-type-writer = 
	  rcurry(write-interface, p, c-type?: c-types?, result?: #t, 
		 use-refs?: ~generic? & tt ~= #"vtable-interfaces");
  if (~generic? & this.doc-string)
    format(s, "  /* %s */\n", this.doc-string);
  end if;
  let (to-translate?, unsupported-property?, unsupported-message) = 
    translate?(this, p);
  if (unsupported-property? & ~generic?)
    format(s, "  /* Warning: %s. */\n", unsupported-message);
  end if;
  if (generic?)
    unless (unsupported-property?)
      unless (this.function-type == #"member-variable" & ~this.has-getter)
	format(s, "define open generic %s (this", this.dylan-name);
	if (tt ~== #"vtable-interfaces")
	  format(s, " :: %s", owner.dylan-name);
	end if;
	if (~this.arguments.empty?)
	  write(s, ", ");
	  write-comma-seperated-list(s, this.arguments, writer: type-writer);
	end if;
	write(s, ") => (");
	write-comma-seperated-list(s, this.results, writer: result-type-writer);
	format(s, ");\n");
      end unless;
      when (this.function-type == #"member-variable" & this.has-setter)
	format(s, "define open generic %s-setter (", this.dylan-name);
	write-interface(s, this.results.first, p, c-type?: c-types?);
	format(s, ", this");
	if (tt ~== #"vtable-interfaces")
	  format(s, " :: %s", owner.dylan-name);
	end if;
	if (~this.arguments.empty?)
	  write(s, ", ");
	  write-comma-seperated-list(s, this.arguments, writer: type-writer);
	end if;
	write(s, ") => (");
	write-interface(s, this.results.first, p, c-type?: c-types?);
	format(s, ");\n");
      end when;
    end unless;
  else
    write(s, "  ");
    if (unsupported-property?)
      write(s, "/* ");
    end if;
    if (tt == #"dispatch-clients")
      if (this.element-function)
	write(s, "element ");
      end if;
      if (this.size-function)
	write(s, "size ");
      end if;
    end if;
    let member-type = this.function-type;
    if (this.function-type == #"member-function")
      let first-arg-type = ~this.results.empty? & 
	                     this.results.first.argument-type;
      if (tt = #"vtable-interfaces" &
	    ~(first-arg-type &
		instance?(first-arg-type, <variant-type-info>) &
		first-arg-type.variant-type = $VT-HRESULT))
	write(s, $general-function-name);
	member-type := #"vtable-member";
      else
	write(s, $standard-function-name);
      end if;
    elseif (this.function-type == #"member-variable")
      if (this.has-getter & ~ this.has-setter)
	write(s, "constant ");
      elseif (~ this.has-getter & this.has-setter)
	write(s, "write-only ");
      end if;
      if (tt = #"dispatch-servers" | tt = #"dual-interfaces" | 
	  tt = #"vtable-interfaces")
	write(s, "virtual ");
      end if;
      write(s, $standard-variable-name);
    end if;
    format(s, " %s ", this.dylan-name);
    if (this.function-type = #"member-function" | ~empty?(this.arguments))
      write(s, "(");
      write-comma-seperated-list(s, this.arguments, writer: type-writer);
      write(s, ") ");
    end if;
    select (member-type)
      #"member-function" =>
	  write(s, "=> (");
	  write-comma-seperated-list(s, copy-sequence(this.results, start: 1), 
				     writer: result-type-writer);
	  write(s, ")");
      #"vtable-member" =>
	  write(s, "=> (");
	  write-comma-seperated-list(s, this.results, 
				     writer: result-type-writer);
	  write(s, ")");
      #"member-variable" =>
	  write(s, ":: ");
	  write-interface(s, this.results.first.argument-type, p, 
			  c-type?: c-types?);
    end select;
    if (this.c-name ~= this.dylan-name)
      format(s, ", name: \"%s\"", this.c-name);
    end if;
    if (tt ~= #"vtable-interfaces")
      write(s, ", disp-id: ");
      if (instance?(this.id, <machine-word>))
	format(s, "as(<machine-word>, %=)", this.id);
      else
	format(s, "%=", this.id);
      end if;
    end if;
    write(s, ";");
    if (unsupported-property?)
      write(s, " */");
    end if;
    s.new-line;
  end if;
end method write-interface;


define method write-interface
	(s :: <stream>, this :: <argument-info>, p :: <mp>, 
	 #key c-type? :: <boolean> = #f, use-refs? :: <boolean> = #f,
	 result? :: <boolean> = #f) => ()
  if (this.argument-optional)
    write(s, "/*optional*/ ");
  end if;
  format(s, "arg-%s :: ", this.argument-name);
  let mode = this.argument-mode;
  let mode-name = select (mode) #"in-out" => "inout"; 
  				#"out" => "out"; 
				otherwise => #f;
		  end select;
  if (result?)
    write-interface(s, this.argument-type, p, c-type?: c-type?);
  elseif (use-refs? & mode-name)
    format(s, "%s-ref(", mode-name);
    write-interface(s, this.argument-type.enclosed-type, p, c-type?: #t);
    write(s, ")");
  else
    mode-name & format(s, "/*%s*/ ", mode-name);
    write-interface(s, this.argument-type, p, c-type?: c-type?);
  end if;
end method write-interface;


define method write-interface
	(s :: <stream>, this :: <type-info>, p :: <mp>, 
	 #key c-type? :: <boolean> = #f) => ()
  let (dylan-types, c-type, comment) = type-base-names(this, p: p);
  if (c-type?) 
    format(s, "<%s>", c-type);
  elseif (size(dylan-types) = 1)
    format(s, "<%s>", first(dylan-types));
  else
    write(s, "type-union(");
    write-comma-seperated-list(s, dylan-types,
	    writer: method(s, t) format(s, "<%s>", t) end method);
    write(s, ")");
  end if;
  if (comment)
    format(s, " /* <%s> */", comment);
  end if;
end method write-interface;


define method write-pointer-definitions (s :: <stream>) => ()
  if (~ pointer-types-used.empty?)
    format(s, "\n\n/* Pointer definitions: */\n");
    for (pp in pointer-types-used)
      let pointer-name :: <string> = pp.head;
      let base-name :: <string> = pp.tail;
      format(s, "define C-pointer-type <%s> => <%s>; ", 
	     pointer-name, base-name);
      format(s, "ignorable(<%s>);\n", pointer-name);
    end for;
  end if;
end method write-pointer-definitions;


define constant integer-types :: <list> = #("integer", "machine-word");

define method type-base-names (this :: <variant-type-info>, 
			       #key p :: false-or(<mp>) = #f,
			       writing-interface :: <boolean> = #f)
			   => (dylan-types :: false-or(<list>),
			       c-type :: false-or(<string>),
			       comment :: false-or(<string>))
  ignorable(p, writing-interface);
  select (this.variant-type)
    $VT-I2 => 		values(#("integer"), 		"C-signed-short");
    $VT-I4 => 		values(integer-types, 		"C-signed-long");
    $VT-R4 => 		values(#("single-float"), 	"C-float");
    $VT-R8 => 		values(#("double-float"), 	"C-double");
    $VT-CY => 		values(#("double-integer"), 	"CY");
    $VT-DATE => 	values(#("double-float"), 	"C-double");
    $VT-BSTR => 	values(#("string"), 		"BSTR");
    $VT-DISPATCH =>	values(#("LPDISPATCH"), 	"LPDISPATCH");
    $VT-ERROR => 	values(#("HRESULT"), 		"C-HRESULT");
    $VT-BOOL => 	values(#("boolean"), 		"VARIANT-BOOL");
    $VT-VARIANT => 	if (p & p.target-type = #"vtable-interfaces")
    			  values(#("LPVARIANT"), 	"VARIANT")
			else
    			  values(#("object"), 		"VARIANT")
			end if;
    $VT-UNKNOWN => 	values(#("LPUNKNOWN"), 		"LPUNKNOWN");
    $VT-I1 => 		values(#("integer"), 		"C-signed-char");
    $VT-UI1 => 		values(#("integer"), 		"C-unsigned-char");
    $VT-UI2 => 		values(#("integer"), 		"C-unsigned-short");
    $VT-UI4 => 		values(integer-types, 		"C-unsigned-long");
    $VT-I8 => 		values(#("double-integer"), 	"LARGE-INTEGER");
    $VT-UI8 => 		values(#("PULARGE-INTEGER"), 	"ULARGE-INTEGER");
    $VT-INT => 		values(integer-types,		"C-signed-long");
    $VT-UINT => 	values(integer-types, 		"C-unsigned-long");
    $VT-VOID => 	values(#f,	 		"C-void");
    $VT-HRESULT => 	values(#("HRESULT"), 		"C-HRESULT");
    $VT-SAFEARRAY => 	values(#("ole-array"), 		#f);
    $VT-LPSTR => 	values(#("LPSTR"), 		"LPSTR");
    $VT-LPWSTR => 	values(#("LPWSTR"), 		"LPWSTR");

    otherwise => motley-error("Unknown variant type %=", this.variant-type);
  end select
end method type-base-names;


define method type-base-names (this :: <user-type-info>,
			       #key p :: false-or(<mp>) = #f,
			       writing-interface :: <boolean> = #f)
			   => (dylan-types :: false-or(<list>),
			       c-type :: false-or(<string>),
			       comment :: false-or(<string>))
  ignorable(p, writing-interface);
  select (this.type-kind)
    $TKIND-ENUM =>
      values(list(this.dylan-name), "C-int");
    $TKIND-ALIAS, $TKIND-COCLASS =>
      // This is wrong for TKIND-ALIAS - we should return the C-type of the
      // aliased type - but that would require more machinery.
      // For some reason, we seem to get $TKIND-COCLASS for interfaces in a
      // coclass interface list.
      values(list(this.dylan-name), #f);
    $TKIND-RECORD, $TKIND-UNION, $TKIND-DISPATCH, $TKIND-INTERFACE =>
      // We don't really support pointers to dispatch interfaces, but pointers
      // to dual interfaces look like dispatch interfaces.
      values(list(this.dylan-name), this.dylan-name);
    otherwise => motley-error("Unknown user type %=", this.type-kind);
  end select;
end method type-base-names;


define method type-base-names (this :: <pointer-type-info>,
			       #key p :: false-or(<mp>) = #f,
			       writing-interface :: <boolean> = #f)
			   => (dylan-types :: false-or(<list>),
			       c-type :: false-or(<string>),
			       comment :: false-or(<string>))
  if (instance?(this.enclosed-type, <user-type-info>) &
      (this.enclosed-type.type-kind = $TKIND-DISPATCH |
       this.enclosed-type.type-kind = $TKIND-INTERFACE))
    if (p & (p.target-type = #"vtable-interfaces" | 
	     p.target-type = #"dual-interfaces"))
      if (p.client-suffix)
	let n = concatenate(this.enclosed-type.dylan-name, 
			    p.client-suffix);
	values(list(n), n, #f)
      else
	values(#("LPUNKNOWN"), "LPUNKNOWN", this.enclosed-type.dylan-name)
      end if;
    else
      type-base-names(this.enclosed-type, p: p)
    end if
  else
    let (dylan-types, c-type, comment) = 
	    type-base-names(this.enclosed-type, p: p);
    let pointer-type = concatenate(c-type, "*");
    let pointer-pair = pair(pointer-type, c-type);
    if (~ member?(pointer-pair, pointer-types-used, test: \=))
      push-last(pointer-types-used, pointer-pair);
    end if;
    values(list(pointer-type), pointer-type, 
	   comment & concatenate(comment, "*"))
  end if
end method type-base-names;

