Module:    win32-resource-database-internal 
Synopsis:  decoding windows resource id's
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function decode-resource(id :: <raw-resource-id>) 
 => (rid :: type-union(<integer>, <string>));
  let value = id.pointer-address;
//  debug-out("id: %d \n", as(<integer>, value));
  if(zero?(%logand(as(<machine-word>, #xFFFF0000), value)))
//    debug-out("integer resource\n");
    as(<integer>, id.pointer-address)
  else
//    debug-out("string resource\n");
    as(<byte-string>, id)
  end;
end;

// not used yet
define constant <predefined-resource-type> = 
// until singletons are implemented 
  <raw-resource-id>;
/*
  one-of($RT-ACCELERATOR,
	 $RT-BITMAP,
	 $RT-DIALOG,
	 $RT-FONT,
	 $RT-FONTDIR,
	 $RT-MENU,
	 $RT-RCDATA,
	 $RT-STRING,
	 $RT-MESSAGETABLE,
	 $RT-CURSOR,
	 $RT-GROUP-CURSOR,
	 $RT-ICON,
	 $RT-GROUP-ICON,
	 $RT-VERSION);
*/

define method encode-resource(r :: <integer>)
 => (rid :: <raw-resource-id>);
  MAKEINTRESOURCE(r)
end;

define method encode-resource(r :: <C-string>)
 => (rid :: <raw-resource-id>);
  as(<raw-resource-id>, r) // this should work
//  r
end;

define method encode-resource(r :: <C-unicode-string>)
 => (rid :: <raw-resource-id>);
  as(<raw-resource-id>, r)
end;

define method encode-resource(r :: <byte-string>)
 => (rid :: <raw-resource-id>);
  as(<raw-resource-id>, as(<C-string>, r))
end;

// control window classes
// not used yet
/*
define constant $button-class = #x0080;
define constant $edit-class = #x0081;
define constant $static-class = #x0082;
define constant $list-box-class = #x0083;
define constant $scroll-bar-class = #x0084;
define constant $combo-box-class = #x0085;
*/

define generic print-resource-id(id :: <resource-id>,
				 resource-name :: <string>) => ();

define method print-resource-id(id :: <integer>, resource-name :: <byte-string>) 
 => ();
  if(id = 0)
    debug-out("No %s resource present\n", resource-name);
  else
    debug-out("%s resource id = %d\n", resource-name, id);
  end;
end;

define method print-resource-id(id :: <string>, resource-name :: <byte-string>) 
 => ();
  debug-out("%s resource id = %s\n", resource-name, as(<byte-string>, id));
end;

define method print-resource-id(id :: <raw-resource-id>, 
				resource-name :: <byte-string>)
 => ();
  print-resource-id(decode-resource(id), resource-name);
end;

