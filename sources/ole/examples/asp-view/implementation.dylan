Module: asp-view
Author:	Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro with-error-status
  { with-error-status ?:body end }
    =>
    { block (return)
	// Ignore warnings
	let handler <warning> = method (c :: <warning>, next-handler)
				  log("Warning: %=. Continuing.\n", c);
				  #f
				end;
	// Abort on error
	let handler <error> = method (c :: <error>, next-handler)
				log("Error: %=.\n", c);
				return($E-FAIL)
			      end;
	?body;
	$S-OK
      end }
end macro;

define method initialize (this :: <asp-view>, #key, #all-keys)
  log("Initializing.\n");
  next-method();
end method initialize;

define method on-start-page (this :: <asp-view>, sc :: <LPDISPATCH>)
 => (res :: <SCODE>)
  with-error-status
    log("OnStartPage.\n");
    this.context := make(<IScriptingContext>, disp-interface: sc);
    AddRef(this.context);
    log("OnStartPage done.\n");
  end
end;

define method on-end-page (this :: <asp-view>) => (res :: <SCODE>)
  with-error-status
    log("OnEndPage.\n");
    when (this.context)
      Release(this.context);
      this.context := #f;
    end;
    log("OnEndPage done.\n");
  end
end;

define method terminate (this :: <asp-view>) => ()
  log("Terminating.\n");
  when (this.context)
    Release(this.context);
    this.context := #f;
  end;
  next-method();
end method terminate ;

define method page-out (this :: <asp-view>, #rest format-args)
  let resp :: <IResponse> = this.context.IScriptingContext/Response;
  // Work around for "Bug 1000367: can't format non-byte-string strings"
  let format-args = map(method (arg)
			  if (instance?(arg, <string>))
			    as(<byte-string>, arg)
			  else
			    arg
			  end
			end, format-args);
  let str :: <string> = apply(format-to-string, format-args);
  IResponse/Write(resp, str);
end;

define method enum-keys (dsp :: <LPDISPATCH>) => (keys :: <sequence>)
  let enum-punk :: <lpunknown> = call-simple-method(dsp, $dispid-newenum);
  let (status, enum) = QueryInterface(enum-punk, $IID-IEnumVARIANT);
  check-ole-status(status, "QueryInterface(IEnumVARIANT)", enum-punk);
  Release(enum-punk);
  let keys = make(<stretchy-vector>);
  with-stack-structure (result :: <LPVARIANT>)  
    VariantInit(result);
    iterate loop ()
      let (status, num-fetched) = IEnumVARIANT/Next(enum, 1, result);
      check-ole-status(status, "IEnumVARIANT/Next", enum);
      select (num-fetched)
	0 => begin
	       Release(enum);
	       keys
	     end;
	1 => begin
	       add!(keys, pointer-value(result));
	       // Don't call VariantClear(result) because we don't want to
	       // deallocate the object that was collected in 'keys'
	       result.vt-value := $VT-EMPTY;
	       loop()
	     end;
      end select;
    end iterate;
  end with-stack-structure;
end enum-keys;

define method quotify (str :: <string>) => (str :: <string>)
  let n = find-key(str, method (c) member?(c, "<>") end);
  if (n)
    let rep = select (str[n])
		'<' => "&lt;";
		'>' => "&gt;";
	      end;
    quotify(replace-subsequence!(str, rep, start: n, end: n + 1))
  else
    str
  end;
end;

define method quotify (obj :: <object>) => (str :: <string>)
  quotify(format-to-string("%s", obj))
end;

define method show-dictionary (this :: <asp-view>,
			       dct :: type-union(<IRequestDictionary>,
						 <IVariantDictionary>))
  let keys = enum-keys(dct);
  if (empty?(keys))
    page-out(this, "<em>Empty</em><br>");
  else
    page-out(this, "<TABLE BORDER=1 COLS=2 FRAME=BOX RULES=ALL>"
	           "<THEAD><TR><TH>Key</TH><TH>Value</TH></TR>"
	           "<TBODY>\n");
    for (key in keys)
      let value = dct[key];
      if (instance?(value, <Interface>))
	// If it's a IStringList, we have a better way to display it...
	let (stat, sl) = QueryInterface(value,
					dispatch-client-uuid(<IStringList>));
	if (SUCCEEDED?(stat))
	  Release(value);
	  value := call-simple-method(sl, $dispid-value);
	  Release(sl);
	end;
      end;
      page-out(this, "<TR><TD>%s</TD><TD>%s</TD></TR>\n",
	             quotify(key), quotify(value));
    end;
    page-out(this, "</table>");
  end;
end;

define method show-form (this :: <asp-view>) => (res :: <SCODE>)
  with-error-status
    log("ShowForm.\n");
    let req :: <IRequest> = this.context.IScriptingContext/Request;
    page-out(this, "<H2>Form:</H2>");
    show-dictionary(this, req.IRequest/Form);
    log("ShowForm done.\n");
  end
end;

define method show-variables (this :: <asp-view>) => (res :: <SCODE>)
  with-error-status
    log("ShowVariables.\n");
    let req :: <IRequest> = this.context.IScriptingContext/Request;
    page-out(this, "<H2>ServerVariables:</H2>");
    show-dictionary(this, req.IRequest/ServerVariables);
    log("ShowVariables done.\n");
  end
end;

define method show-query (this :: <asp-view>) => (res :: <SCODE>)
  with-error-status
    log("ShowQuery.\n");
    let req :: <IRequest> = this.context.IScriptingContext/Request;
    page-out(this, "<H2>QueryString:</H2>");
    show-dictionary(this, req.IRequest/QueryString);
    log("ShowQuery done.\n");
  end
end;

define method show-cookies (this :: <asp-view>) => (res :: <SCODE>)
  with-error-status
    log("ShowCookies.\n");
    let req :: <IRequest> = this.context.IScriptingContext/Request;
    page-out(this, "<H2>Cookies:</H2>");
    show-dictionary(this, req.IRequest/Cookies);
    log("ShowCookies done.\n");
  end
end;

define method show-session (this :: <asp-view>) => (res :: <SCODE>)
  with-error-status
    log("ShowSession.\n");
    let ses :: <ISessionObject> = this.context.IScriptingContext/Session;
    page-out(this, "<H2>Session:</H2>");
    page-out(this, "Session ID=%=<br>", ses.ISessionObject/SessionID);
    page-out(this, "Timeout=%s mins<br>", ses.ISessionObject/Timeout);
    page-out(this, "Objects:<br>");
    show-dictionary(this, ses.ISessionObject/StaticObjects);
    page-out(this, "Contents:<br>");
    show-dictionary(this, ses.ISessionObject/Contents);
    log("ShowSession done.\n");
  end
end;

define method show-all (this :: <asp-view>) => (res :: <SCODE>)
  block (return)
    log("ShowAll.\n");
    let status = show-session(this);
    if (FAILED?(status)) return(status) end;
    page-out(this, "<HR>\n");
    let status = show-variables(this);
    if (FAILED?(status)) return(status) end;
    page-out(this, "<HR>\n");
    let status = show-cookies(this);
    if (FAILED?(status)) return(status) end;
    page-out(this, "<HR>\n");
    let status = show-query(this);
    if (FAILED?(status)) return(status) end;
    page-out(this, "<HR>\n");
    let status = show-form(this);
    if (FAILED?(status)) return(status) end;
    page-out(this, "<HR>\n");
    log("ShowAll done.\n");
    $S-OK
  end block;
end;
