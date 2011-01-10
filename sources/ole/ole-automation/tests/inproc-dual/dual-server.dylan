Module:    inproc-dual
Synopsis:  Test the "define dual-interface" macro.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $IID-IDonkey = 
  as(<REFCLSID>, "{08448354-265D-11D2-9A67-006097C90313}");

define constant $donkey-code = 192837;

define dual-interface <IDonkey> ( <simple-dispatch> )
  uuid $IID-IDonkey;
  name "Donkey";
  documentation "simple test of dual interface";
  major-version 1; minor-version 1;

  member-function IDonkey/Bar ( n :: <C-void*> ) => ();
  member-function IDonkey/Square ( n :: <integer> ) => (nn :: <integer>);
  virtual property IDonkey/Size :: <integer>, name: "size";

  constant property Donkey-const :: <single-float>, name: "const";
  constant property Donkey-code :: <integer> = $donkey-code,
	disp-id: $DISPID-VALUE;
end;  

define COM-interface <dll-donkey> ( <IDonkey> )
  slot IDonkey/Size :: <integer> = 0, init-keyword: size:;
end <dll-donkey>;

define method IDonkey/Bar (This :: <dll-donkey>, n :: <C-void*>)
 => (status :: <HRESULT>);
  if ( null-pointer?(n) )
    $S-FALSE
  else
    $S-OK
  end if
end method;

define method IDonkey/Square (This :: <dll-donkey>, n :: <integer>)
 => (status :: <HRESULT>, square :: <integer>);
  values( $S-OK, n * n )
end method;

define constant $Donkey-const = 8.75;

define method Donkey-const (This :: <dll-donkey>) => (value :: <single-float>)
  $Donkey-const
end;
