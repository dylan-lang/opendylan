Module:    OLE-Automation
Synopsis:  A Dylan implementation of the IDispatch interface.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open COM-interface <Simple-Dispatch> ( <lib-init-mixin>,
					      <object-with-dll-lock>,
					      <IDispatch>,
					      <factory-args-mixin> )
  slot typeinfo, init-keyword: typeinfo:, init-value: #f;
end <Simple-Dispatch>;

define method initialize(this :: <Simple-Dispatch>, #rest args, #key) => ();
  next-method();
  when (this.typeinfo)
    // make sure it is <disp-type-info>, not <dual-type-info>
    this.typeinfo := this.typeinfo.dispatch-type-information;
  end;
  unless (this.typeinfo)
    this.typeinfo := this.dispatch-type-information;
  end;
  unless (this.typeinfo) 
    error("No typeinfo available for instance of subclass of <Simple-Dispatch>");
  end unless;
  unless ( instance?( this.typeinfo, <collection> ) )
    this.typeinfo := make(<degenerate-collection>, value: this.typeinfo);
  end unless;
  do(IUnknown/AddRef, this.typeinfo);

  let guid = this.typeinfo[0].guid-value;
  unless ( guid == $default-type-id |
	   IsEqualIID?(guid, $IID-IDispatch) |
	   IsEqualIID?(guid, $IID-NULL) )
    // Cause this dispatch object to be returned in response to queries for
    // the typeinfo's ID.
    add-interface(this, guid);
  end unless;

  values()
end;

define method terminate(this :: <Simple-Dispatch>) => ();
  next-method();
  do(IUnknown/Release, this.typeinfo);
  values()
end;


// Contains exactly one element and ignores the index.
define class <degenerate-collection> ( <sequence> )
  slot the-value, required-init-keyword: value:, setter: #f;
end;

define method size( x :: <degenerate-collection> ) => size :: <integer>;
  1
end;

define method element( x :: <degenerate-collection>, index :: <integer>,
		      #key default ) => (value :: <object>);
  x.the-value
end;


define method IDispatch/Invoke(this :: <Simple-Dispatch>,
			       dispidMember, riid :: <REFIID>, lcid, wFlags,
			       pdispparams :: <LPDISPPARAMS>,
			       pvarResult :: <LPVARIANT>,
			       pexcepinfo :: <LPEXCEPINFO>)
 => ( status :: <HRESULT>, ArgErr :: <integer> );

    if( ~ IsEqualIID?(riid, $IID-NULL) )
      values( $DISP-E-UNKNOWNINTERFACE, 0 )
    else
      Disp-Invoke(this, element(this.typeinfo, lcid),
		  dispidMember, wFlags, pdispparams,
		  pvarResult, pexcepinfo);
    end if
end;


define method IDispatch/GetTypeInfoCount(this :: <Simple-Dispatch>)
 => ( status :: <HRESULT>, count :: <integer> );
  values( $S-OK, 1 )
end;

define method IDispatch/GetTypeInfo(this :: <Simple-Dispatch>,
				    itinfo :: <integer>, lcid)
 =>( status :: <HRESULT>, typeinfo :: <Interface> );

  if( itinfo ~= 0 )
    values( $DISP-E-BADINDEX, $null-interface )
  else 
    let info = element(this.typeinfo, lcid);
    AddRef(info);
    values( $S-OK, info )
  end if
end method IDispatch/GetTypeInfo;


define method IDispatch/GetIDsOfNames (this :: <Simple-Dispatch>,
				       riid :: <REFIID>,
				       rgszNames /* :: <LPOLESTR*> */,
				       cNames /* :: <integer> */,
				       lcid /* :: <integer> */,
				       rgdispid /* :: <LPDISPID> */)
 => status :: <HRESULT>;

  if ( ~ IsEqualIID?( riid, $IID-NULL) )
    $DISP-E-UNKNOWNINTERFACE
  else
    Disp-GetIDsOfNames(element(this.typeinfo, lcid),
		       rgszNames, cNames, rgdispid)
  end if
end;

// default methods
define sideways method IDispatch/GetTypeInfoCount (this :: <IDispatch>)
 => ( status :: <HRESULT>, count :: <integer> );
  values( $S-OK, 0 )
end;

define sideways method IDispatch/GetTypeInfo(this :: <IDispatch>, itinfo, lcid)
 =>( status :: <HRESULT>, typeinfo :: <Interface> );
  values( $E-NOTIMPL, $NULL-interface )
end method IDispatch/GetTypeInfo;



//  ---  parameter lists for Invoke  ---

define constant $NULL-EXCEPINFO :: <LPEXCEPINFO> = null-pointer(<LPEXCEPINFO>);

// This is a fix (workaround?) for bug #1000408: Sometimes (apparently when
// VBScript doesn't know exactly which dispatch interface it's talking to) it
// sends args over by reference, which to us looks like an lpvariant of an
// lpvariant...  We don't really care, so just always drill down to a real type
define function dispparam-value (lp :: <LPVARIANT>, index :: <integer>)
  let value = pointer-value(lp, index: index);
  if (instance?(value, <LPVARIANT>))
    dispparam-value(value, 0)
  else
    value
  end
end;

// Call a function on the arguments specified by a DISPPARAMS structure
define method apply-to-dispparams
    ( function :: <function>, // the function to be called.
     instance :: <object>, // instance to pass as first argument
			   //  (usually <IDispatch>, but doesn't matter here.)
     parms :: <LPDISPPARAMS> // the function arguments.
       ); // (returns whatever the function returns)
  let numparms = parms.cArgs-value; // total number of arguments
  if ( zero?(numparms) ) // short cut when no parameters
    function(instance);
  else
    let numnamed = parms.cNamedArgs-value; // number of named arguments
    let numpos = numparms - numnamed; // number of positional arguments
    let argptr :: <LPVARIANT> = parms.rgvarg-value;
    if ( numpos = 1 & numnamed = 0 )
      // short cut for just one required parameter
      function(instance, dispparam-value(argptr, 0))
    else
      // Construct a sequence consisting of the positional parameters followed
      // by a keyword and value pair for each named argument.
      let ( number-required :: <integer>, has-rest? :: <boolean>, keywords ) =
	function-arguments(function);
      let numargs :: <U16> = numparms + numnamed;
      let restarg :: <vector> = #[];
      if ( (numpos = number-required) & has-rest? )
	// Note that the number-required includes the instance argument,
	// which is not counted in numpos, so if the two are equal, that
	// really means we have one extra argument.
	let extra-arg = dispparam-value(argptr, 0); // last argument is first
	if ( instance?(extra-arg,<vector>) )
	  restarg := extra-arg;
	  // adjust size for spreading "rest" arg
	  numargs := numargs - 1 + size(restarg);
	  numpos := numpos - 1;
	end if;
      end if;
      // make the argument sequence
      let args :: <vector> = make(<vector>, size: numargs);
      // fill in the positional arguments
      for ( i :: <U16> from 0 below numpos )
	args[i] := dispparam-value(argptr, numparms - i - 1);
      end for;
      if ( zero?(numnamed) )
	// spread "rest" arg, if any
	for ( elem in restarg,
	      i :: <U16> from numpos by 1 )
	  args[i] := elem;
	end for;
      else // convert "named" arguments to keyword arguments
  /*
	if ( ~ empty?(this.function-keywords) )
	  keywords := this.function-keywords;
	end if;
  */
	let ndptr = parms.rgdispidNamedArgs-value;
	for ( i :: <integer> from numnamed - 1 to 0 by -1,
	     j :: <U16> from numpos by 2 )
	  args[j] := keywords[pointer-value(ndptr, index: i) - numpos];
	  args[j + 1] := dispparam-value(argptr, i);
	end for;
      end if;
      apply(function, instance, args)
    end if
  end if
end method;


define method make-dispparams( named-args :: <sequence>,
			      named-arg-ids :: <sequence>,
			      #rest positional-args )
 => params :: <LPDISPPARAMS>;

  let params :: <LPDISPPARAMS> = make(<LPDISPPARAMS>);
  let numnamed :: <integer> = size(named-args);
  let numpositional :: <integer> = size(positional-args);
  params.cNamedArgs-value := numnamed;
  let numargs :: <integer> = numnamed + numpositional;
  params.cArgs-value := numargs;
  if ( zero?(numargs) )
    params.rgvarg-value := null-pointer(<LPVARIANTARG>);
    params.rgdispidNamedArgs-value := null-pointer(<LPDISPID>);
  else
    unless ( numnamed = size(named-arg-ids) )
      error("make-dispparams: mismatched number of named values and IDs");
    end unless;
    let argptr :: <LPVARIANTARG> =
      make(<LPVARIANTARG>, element-count: numargs);
    params.rgvarg-value := argptr;
    for ( arg in positional-args,
	 i from (numargs - 1) by -1 )
      let arg-i-ptr :: <LPVARIANTARG> = indexed-variant(argptr,i);
      VariantInit(arg-i-ptr);
      pointer-value(arg-i-ptr) := arg;
    end for;
    if ( zero?(numnamed) )
      params.rgdispidNamedArgs-value := null-pointer(<LPDISPID>);
    else
      let idptr = make(<LPDISPID>, element-count: numnamed);
      params.rgdispidNamedArgs-value := idptr;
      for ( value in named-args,
	   id in named-arg-ids,
	   i from 0 by 1 )
	let arg-i-ptr :: <LPVARIANTARG> = indexed-variant(argptr,i);
	VariantInit(arg-i-ptr);
	pointer-value(arg-i-ptr) := value;
	pointer-value(idptr, index: i) := id;
      end for;
    end if;
  end if;
  params
end method make-dispparams;

define method destroy-dispparams (this :: <LPDISPPARAMS>) => ();
  let pos-arg = this.rgvarg-value;
  unless ( null-pointer?(pos-arg) )
    // `destroy' deallocates only the first element, so do any others here.
    for ( i from 1 below this.cArgs-value )
      VariantClear(indexed-variant(pos-arg,i));
    end for;
    destroy(pos-arg);
  end unless;
  let named-args = this.rgdispidNamedArgs-value;
  unless ( null-pointer?(named-args) )
    destroy(named-args);
  end unless;
  destroy(this);
end;


// Easy way to call a dispatch method which has only positional arguments
// and doesn't use the locale.
define method call-simple-method ( instance /* :: <LPDISPATCH> */,
				   dispid :: <disp-id>,
				   #rest args)
 => ( result :: <object> );
  apply(simple-invoke, instance, dispid, $DISPATCH-METHOD,
	$LOCALE-USER-DEFAULT, #f, args)
end method call-simple-method;

define method call-simple-method(instance /* :: <LPDISPATCH> */,
				 name :: <string>,
				 #rest args)
 => ( result :: <object> );
  let disp-id = get-id-of-name(instance, name, undefined-ok?: #f);
  apply(call-simple-method, instance, disp-id, args)
end method;

define function simple-invoke ( instance /* :: <LPDISPATCH> */,
			      dispid :: <disp-id>, flags :: <integer>,
			      locale, default, #rest args)
 => ( result :: <object> );
  let result = default;
  let dispparams :: <LPDISPPARAMS> = apply(make-dispparams, #(), #(), args);
  with-stack-structure( varResult :: <LPVARIANT> )
    VariantInit(varResult); // in case with-stack-structure doesn't initialize
    let ( status :: <HRESULT>, arg-err :: <integer> ) =
      IDispatch/Invoke(instance, dispid, $IID-NULL, locale, flags,
		       dispparams, varResult, $NULL-EXCEPINFO);
    if ( FAILED?(status) )
      if ( (status = $DISP-E-TYPEMISMATCH | status = $DISP-E-PARAMNOTFOUND)
	    & (arg-err >= 0) & (arg-err < size(args)) )
	// Show which value it doesn't like:
	ole-cerror(status, "Invoke", instance, dispid,
		   args[size(args) - 1 - arg-err]);
      else
	apply(ole-cerror, status, "Invoke", instance, dispid, args);
      end if;
    else
      unless ( empty?(varResult) )
	result := pointer-value(varResult);
      end unless;
    end if;
    // Don't call VariantClear(varResult) here because we don't want to 
    // deallocate the object that is being passed back as the `result'.
    varResult.vt-value := $VT-EMPTY;
  end with-stack-structure;
  destroy-dispparams(dispparams);
  result
end simple-invoke;

define variable empty-disp-params :: false-or(<LPDISPPARAMS>) = #f;

define method get-property(instance /* :: <LPDISPATCH> */,
			   dispid :: <disp-id>,
			   #key default = unsupplied(),
			        locale = $LOCALE-USER-DEFAULT,
			        index = unsupplied())
 => ( value :: <object> );

  if (supplied?(index))
    let value =
      if ( instance?(index, <sequence>) & ~ instance?(index, <string>) )
	apply(simple-invoke, instance, dispid, $DISPATCH-PROPERTYGET,
	      locale, default, index)
      else
	simple-invoke(instance, dispid, $DISPATCH-PROPERTYGET,
		      locale, default, index)
      end if;
    if (unsupplied?(value))
	error("No value for get-property(%=,%=, index: %=)",
	      instance, dispid, index);
    else
      value
    end if
  else
  let result = #f;
  with-stack-structure( varResult :: <LPVARIANT> )
    VariantInit(varResult);
    if ( empty-disp-params == #f )
      // create this structure the first time it is needed
      empty-disp-params := make-dispparams(#(), #());
    end if;
    let status :: <HRESULT> =
      IDispatch/Invoke(instance, dispid, $IID-NULL,
		       locale, $DISPATCH-PROPERTYGET,
		       empty-disp-params, varResult, $NULL-EXCEPINFO);
    if ( FAILED?(status) )
      if ( (status = $TYPE-E-ELEMENTNOTFOUND |
	    status = $DISP-E-MEMBERNOTFOUND )
	    & supplied?(default) )
	result := default;
      else
	ole-cerror(status, "$DISPATCH-PROPERTYGET", instance, dispid);
      end if;
    elseif ( empty?(varResult) )
      // This shouldn't happen, but we don't control what the server does.
      if (supplied?(default))
	result := default;
      else
	error("No value received for get-property(%=,%=)", instance, dispid);
      end if
    else
      result := pointer-value(varResult)
    end if;
    // Don't call VariantClear(varResult) here because we don't want to 
    // deallocate the object that is being passed back as the `result'.
    varResult.vt-value := $VT-EMPTY;
  end with-stack-structure;
  result
  end
end method get-property;

define method get-property(instance /* :: <LPDISPATCH> */,
			   name :: <string>,
			   #rest rest-args,
			   #key default = unsupplied(),
			   locale = $LOCALE-USER-DEFAULT,
			   #all-keys)
 => ( value :: <object> );
  
  let disp-id :: <disp-id> =
    get-id-of-name(instance, name, locale: locale,
		   undefined-ok?: supplied?(default));
  if ( disp-id = $DISPID-UNKNOWN )
    default
  else
    apply(get-property, instance, disp-id, rest-args)
  end if
end method;

define method get-property-setter(value :: <object>,
				  instance /* :: <LPDISPATCH> */,
				  dispid /* :: <disp-id> */,
				  #rest rest, #key)
 => ( value :: <object> );
  apply(set-property, instance, dispid, value, rest);
  value
end;

define constant $propput-string = "dispatch property put";

define method set-property(instance /* :: <LPDISPATCH> */,
			   dispid /* :: <disp-id> */,
			   value :: <object>,
			   #key locale = $LOCALE-USER-DEFAULT,
			   index = unsupplied(),
			   // the following option is internal; don't document.
			   invoke-flags :: <integer> = $DISPATCH-PROPERTYPUT)
		 => ();

  // should `default:' be supported here also???

  if (supplied?(index))
    if ( instance?(index, <sequence>) & ~ instance?(index, <string>) )
      // sequence of indexes
      apply(set-indexed-property, value, instance, dispid,
	    locale, invoke-flags, index);
    else // a single index
      set-indexed-property(value, instance, dispid,
			   locale, invoke-flags, index);
    end if;
  else
  with-stack-structure( params :: <LPDISPPARAMS> )
    with-stack-structure( arg :: <LPVARIANT> )
      VariantInit(arg);
      with-stack-structure ( idptr :: <LPDISPID> )
	pointer-value(arg) := value;
	params.rgvarg-value := arg;
        pointer-value(idptr) := $DISPID-PROPERTYPUT; 
	params.rgdispidNamedArgs-value := idptr;
	params.cArgs-value := 1; 
	params.cNamedArgs-value := 1; 
	let status =
	  IDispatch/Invoke(instance, dispid, $IID-NULL,
			   locale, invoke-flags,
			   params, $NULL-VARIANT, $NULL-EXCEPINFO);
	if ( FAILED?(status) )
	  ole-cerror(status, $propput-string, instance, dispid);
	end if;
	VariantClear(arg);
      end with-stack-structure;
    end with-stack-structure;
  end with-stack-structure;
  end;
  values()
end method set-property;

define method set-property(instance /* :: <LPDISPATCH> */,
			   name :: <string>,
			   value :: <object>,
			   #rest options,
			   #key locale = $LOCALE-USER-DEFAULT, #all-keys)
		 => ();
  let disp-id :: <disp-id> =
    get-id-of-name(instance, name, locale: locale);
  apply(set-property, instance, disp-id, value, options)
end method;

define method set-property-ref(instance /* :: <LPDISPATCH> */,
			       dispid :: <object>,
			       value :: <object>,
			       #rest options, #key, #all-keys)
 => ();
  apply(set-property, instance, dispid, value,
	invoke-flags: $DISPATCH-PROPERTYPUTREF, options)
end method set-property-ref;

define method get-id-of-name(instance :: <LPDISPATCH>,
			     name :: <string>,
			     #key locale = $LOCALE-USER-DEFAULT,
			     undefined-ok? :: <boolean> = #f)
 => ( dispid :: <disp-id>)

  // Returns the dispatch ID for a given name.  This works for either
  // methods or properties, but doesn't support mapping the names of the
  // arguments of methods.

  with-stack-structure( pname :: <LPLPOLESTR> )
    let ole-name :: <LPOLESTR> = as(<LPOLESTR>, name);
    pointer-value(pname) := ole-name;
    with-stack-structure( pdispid :: <LPDISPID> )
      let status :: <HRESULT> =
	IDispatch/GetIDsOfNames (instance, $IID-NULL,
				 pname, 1, locale, pdispid);
      unless ( ole-name == name )
	destroy(ole-name);
      end unless;
      if ( FAILED?(status) )
	if ( undefined-ok? & (status = $DISP-E-UNKNOWNNAME) )
	  $DISPID-UNKNOWN
	else
	  ole-error(status, "GetIDsOfNames", instance, name);
	end if
      else
	pointer-value(pdispid)
      end if
    end with-stack-structure
  end with-stack-structure
end method get-id-of-name;


define method get-indexed-property ( instance /* :: <LPDISPATCH> */,
				     dispid :: <disp-id>,
				     #rest indexes) => ( value :: <object> )

  apply(simple-invoke, instance, dispid, $DISPATCH-PROPERTYGET,
	$LOCALE-USER-DEFAULT, #f, indexes)
end method;

define method get-indexed-property-setter (new-value,
					   instance /* :: <LPDISPATCH> */,
					   dispid :: <disp-id>,
					   #rest indexes)
 => ( new-value :: <object> )
  apply(set-indexed-property, new-value, instance, dispid,
	$LOCALE-USER-DEFAULT, $DISPATCH-PROPERTYPUT, indexes)
end method;

// internal function
define method set-indexed-property (new-value,
				    instance /* :: <LPDISPATCH> */,
				    dispid :: <disp-id>,
				    locale,
				    invoke-flags :: <integer>,
				    #rest indexes)
 => ( new-value :: <object> )
  
  let dispparams :: <LPDISPPARAMS> =
    apply(make-dispparams, vector(new-value),
	  vector($DISPID-PROPERTYPUT), indexes);
  block ()
    let ( status :: <HRESULT>, arg-err :: <integer> ) =
      IDispatch/Invoke(instance, dispid, $IID-NULL, locale,
		       invoke-flags, dispparams,
		       $NULL-VARIANT, $NULL-EXCEPINFO);
    if ( FAILED?(status) )
      if ( status = $DISP-E-TYPEMISMATCH
	    & arg-err >= 0 & arg-err < size(dispparams) )
	// Show which value it doesn't like:
	ole-cerror(status, $propput-string, instance, dispid,
		   pointer-value(dispparams.rgvarg-value, index: arg-err));
      else
	apply(ole-cerror, status, $propput-string, instance,
	      dispid, indexes);
      end if;
    end if;
  cleanup
    destroy-dispparams(dispparams);
  end block;
  new-value
end method set-indexed-property;
