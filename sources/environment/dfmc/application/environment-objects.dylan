Module:    dfmc-application
Synopsis:  general environment object information from the application.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// GET-ENVIRONMENT-OBJECT-PRIMITIVE-NAME  (Environment Protocol Method)
//    Constructs a string that can be used in the environment to
//    name the object.

define method get-environment-object-primitive-name
    (application :: <dfmc-application>, obj :: <application-object>)
 => (name :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    application-proxy-primitive-name(application, proxy, decorate?: #t)
  end
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (application :: <dfmc-application>, obj :: <symbol-object>)
 => (name :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    application-proxy-primitive-name(application, proxy, decorate?: #f)
  end
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (application :: <dfmc-application>, obj :: <number-object>)
 => (name :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    application-proxy-primitive-name(application, proxy, decorate?: #f)
  end
end method get-environment-object-primitive-name;

define method number-object-to-string
    (application :: <dfmc-application>, obj :: <number-object>,
     #key format :: false-or(<symbol>) = #f,
          prefix? :: <boolean> = #t)
 => (name :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    let value = runtime-proxy-to-remote-value(application, proxy);
    let text = print-dylan-object(target, value, format: format);
    concatenate(if (prefix?)
		  select (format)
		    #"binary" => "#b";
		    #"octal"  => "#o";
		    #"hex"    => "#x";
		    otherwise => "";
		  end
		else
		  ""
		end,
		text)
  end
end method number-object-to-string;

define method get-environment-object-primitive-name
    (application :: <dfmc-application>, obj :: <character-object>)
 => (name :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    application-proxy-primitive-name(application, proxy, decorate?: #f)
  end
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (application :: <dfmc-application>, obj :: <string-object>)
 => (name :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    application-proxy-primitive-name(application, proxy, decorate?: #f)
  end
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (application :: <dfmc-application>, obj :: <boolean-object>)
 => (name :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = obj.application-object-proxy;
    application-proxy-primitive-name(application, proxy, decorate?: #f)
  end
end method get-environment-object-primitive-name;


///// ENVIRONMENT-OBJECT-SOURCE (Environment Protocol Method)
//    It's up to the compiler to provide this. We just return #f in all
//    cases.

define method environment-object-source
    (application :: <dfmc-application>, obj :: <environment-object>)
       => (source :: false-or(<string>))
  #f
end method;
