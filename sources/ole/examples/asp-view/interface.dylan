Module: asp-view
Author:	Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define dispatch-interface <asp-view> (<simple-dispatch>)
  documentation "Dylan ASP Viewer";
  uuid "{836215C2-CCC6-11D2-A51C-00600808472F}";

  slot context :: false-or(<IScriptingContext>) = #f;

  // Standard ASP callbacks
  function on-start-page (sc :: <LPDISPATCH>) => (), name: "OnStartPage";
  function on-end-page () => (), name: "OnEndPage";

  // Functionality specific to this component
  function show-session () => (), name: "ShowSession";
  function show-variables () => (), name: "ShowVariables";
  function show-cookies () => (), name: "ShowCookies";
  function show-query () => (), name: "ShowQuery";
  function show-form () => (), name: "ShowForm";
  function show-all () => (), name: "ShowAll";
end;

define coclass $asp-view-component
  name "ASPView";
  documentation "Dylan ASP Viewer component";
  uuid "{22e89578-16cf-11d2-bd09-006097c9031d}";
  interface <asp-view>;
end;

in-process-automation-server(typeinfo: $asp-view-component,
			     prog-id: "HQN.ASPview",
			     versioned-prog-id: "HQN.ASPview.1");


