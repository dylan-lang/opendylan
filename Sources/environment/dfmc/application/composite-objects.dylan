Module:    dfmc-application
Synopsis:  Serving composite objects from the application.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// COMPOSITE-OBJECT-SIZE (Environment Protocol Method)
//    Returns the size (ie, the number of instance slots, including any
//    repeated slots) of a composite instance.

define method composite-object-size
    (application :: <dfmc-application>, ro :: <composite-object>,
     #key inherited? = #f)
        => (sz :: <integer>)
  let target = application.application-target-app;
  let sz = 0;

  // Within a debugger transaction, get the proxy for this composite
  // object, and call the DM to calculate the total number of slots
  // in the object.

  perform-debugger-transaction
    (target,
     method ()
       let proxy = ro.application-object-proxy;
       let proxy-value = runtime-proxy-to-remote-value(application, proxy);
       let (byte-size, fixed-count, element-count)
         = dylan-object-size(target, proxy-value);
       let (class, incarnation, current-incarnation, immediate?)
         = dylan-object-class(target, proxy-value);
       let class-proxy =
         exchange-value-proxy-for-browsable-class-proxy
           (application, proxy);
       if (class-proxy)
         let (slots, navigation, repeat, count-offset, element-size,
              element-offset, class-slot-count)
            = class-proxy-browser-information
                (application, class-proxy, incarnation: incarnation);
         sz := fixed-count + class-slot-count;
       else
         sz := fixed-count;
       end if;
     end method);

  sz;
end method;


///// COMPOSITE-OBJECT-CONTENTS (Environment Protocol Method)
//    Returns the slot names and values of a composite instance. All
//    of the values are wrapped up in <environment-object>s, whereas the
//    slot names are just returned as strings.

define method composite-object-contents
    (application :: <dfmc-application>, ro :: <composite-object>,
     #key inherited? = #f)
       => (names :: <sequence>, vals :: <sequence>)

  let target = application.application-target-app;
  let names = #[];
  let vals = #[];

  // Within a debugger transaction, get the proxy for this composite
  // object, and exchange it for its class proxy. Get the browser
  // navigation info from the class, and use that to unpick the object.

  perform-debugger-transaction
    (target,
     method ()
       let proxy = ro.application-object-proxy;
       let class-proxy =
         exchange-value-proxy-for-browsable-class-proxy
           (application, proxy);
       if (class-proxy)
         let proxy-value = runtime-proxy-to-remote-value(application, proxy);
         let (class, incarnation, current-incarnation, immediate?)
           = dylan-object-class(target, proxy-value);
         let (slots, navigation, repeat, count-offset, element-size,
              element-offset, class-slot-count)
            = class-proxy-browser-information
                (application, class-proxy, incarnation: incarnation);
         let class-object = 
           runtime-proxy-to-remote-value(application, class-proxy);
         let (byte-size, fixed-count, element-count)
            = dylan-object-size(target, proxy-value);
         let (class-slot-names, class-slots, class-slot-values)
           = dylan-class-slot-storage
                (target, class-object, use-incarnation: incarnation);
         if (repeat)
           names := make(<vector>, size: fixed-count + 1 + class-slot-count);
           vals := make(<vector>, size: fixed-count + 1 + class-slot-count);
         else
           names := make(<vector>, size: fixed-count + class-slot-count);
           vals := make(<vector>, size: fixed-count + class-slot-count);
         end if;
         let i = 0;
         for (j from 0 below class-slot-count)
           names[i] := class-slot-names[j];
           vals[i] := 
             make-environment-object-for-runtime-value
               (application, class-slot-values[i]);
           i := i + 1;
         end for;
         for (name-offset-pair in slots)
           let slot-name = head(name-offset-pair);
           let slot-value =
             read-dylan-value(target,
                              indexed-remote-value(proxy-value,
                                                   tail(name-offset-pair)));
           names[i] := slot-name;
           vals[i] :=
             make-environment-object-for-runtime-value
               (application, slot-value);
           i := i + 1;
         end for;
       end if
     end method);

  values(names, vals);
end method;

