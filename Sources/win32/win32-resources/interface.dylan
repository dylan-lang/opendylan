Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Basic types and classes

define constant <raw-resource-id> = <LPTSTR>;

define constant <resource-id>
  = type-union(<raw-resource-id>, <unsigned-int>, <string>);

define constant <resource-type>
  = type-union(<raw-resource-id>, <unsigned-int>, <string>);

// define constant $button-class     :: <integer> = #x0080;
// define constant $edit-class       :: <integer> = #x0081;
// define constant $static-class     :: <integer> = #x0082;
// define constant $list-box-class   :: <integer> = #x0083;
// define constant $scroll-bar-class :: <integer> = #x0084;
// define constant $combo-box-class  :: <integer> = #x0085;


/// Basic protocols

define generic resource-id
    (resource :: <resource>) => (id :: <resource-id>);
define generic resource-type
    (resource :: <resource>) => (type :: <resource-type>);

define abstract class <resource> (<object>)
end class <resource>;


define generic window-class
    (resource :: <window-resource>) => (window-class :: <resource-id>);
define generic window-position
    (resource :: <window-resource>) => (x :: <integer>, y :: <integer>);
define generic window-size
    (resource :: <window-resource>) => (w :: <integer>, h :: <integer>);

define abstract class <window-resource> (<resource>) 
  slot window-class :: <resource-id>;
end class <window-resource>;


define generic gadget-count
    (resource :: <top-window-resource>) => (n :: <integer>);

define abstract class <top-window-resource> (<window-resource>)
end class <top-window-resource>;


define generic encode-resource
    (resource :: type-union(<resource-type>, <resource-id>)) => (id :: <raw-resource-id>);

define generic decode-resource
    (raw-id :: <raw-resource-id>) => (id :: type-union(<unsigned-int>, <string>));

define generic lookup-resource
    (type :: <resource-type>, id :: <resource-id>) => (resource :: <resource>);

define generic lookup-dialog
    (id :: <resource-id>) => (resource :: <top-window-resource>);

define generic lookup-control
    (window :: <window-resource>, id :: <resource-id>) => (resource :: <window-resource>);

define generic dialog-menu
    (dialog :: <top-window-resource>) => (id :: <resource-id>);


/// Describing resources

define generic describe-resource
    (resource :: false-or(<resource>)) => ();

define sealed method describe-resource
    (resource == #f) => ()
  #f
end method describe-resource;
