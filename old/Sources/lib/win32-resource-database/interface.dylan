Module:    win32-resource-database-internal 
Synopsis:  public interface to resource database
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <resource> (<object>)
//  virtual slot resource-id :: <resource-id>;
end;

// +++++ until we use signals use false-or
define generic describe-resource(r :: false-or(<resource>)) => ();

define method describe-resource(r == #f) => ();
end;

define abstract class <window-resource> (<resource>) 
  slot window-class :: <resource-id>;
//  keyword window-class;
end;

define generic window-position(r :: <window-resource>) 
 => (x :: <integer>, y :: <integer>);

define generic window-size(r :: <window-resource>) 
 => (width :: <integer>, height :: <integer>);

// top level window
define abstract class <top-window-resource> (<window-resource>)
//  virtual slot number-of-gadgets :: <integer>;
end;

define generic number-of-gadgets(r :: <top-window-resource>)
 => (number :: <integer>);

define constant <raw-resource-id> = <LPTSTR>;


define constant <resource-type> = type-union(<raw-resource-id>,
					     <integer>,
					     <string>);

define generic resource-type (r :: <resource>) => (type :: <resource-type>);

define constant <resource-id> = type-union(<raw-resource-id>,
					   <integer>, 
					   <string>);

define generic encode-resource(r :: type-union(<resource-type>, <resource-id>))
 => (id :: <raw-resource-id>);


define function load-default-resources()
  win32-load-app-instance-resources();
end;

//define method load-resources(handle :: <module-handle>) => ();
//end;

define generic lookup-resource(type :: <resource-type>,
			       id :: <resource-id>)
 => (resource :: <resource>);

define generic lookup-dialog(id :: <resource-id>)
 => (resource :: <top-window-resource>);

// In principle windows can have children too
define generic lookup-control(window :: <window-resource>, id :: <resource-id>)
 => (resource :: <window-resource>);

define generic dialog-menu(d :: <top-window-resource>)
 => (id :: <resource-id>);
