Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Resource lookup

define constant *grok-resource-table* :: <resource-table>
  = make(<resource-table>);


define sealed method lookup-resource
    (type :: <resource-type>, id :: <unsigned-int>)
 => (resource :: <resource>)
  let table    = *resource-database*.%resources[type];	// doesn't have to be encoded
  let wrapper  = table[encode-resource(id)];
  let resource = retrieve-resource(wrapper.%resource, *resource-database*);
  wrapper.%resource := resource;
  resource
end method lookup-resource;

define sealed method lookup-control
    (dialog :: <dialog-resource>, id :: <resource-id>)
 => (resource :: <control-resource>)
  let control = element(dialog-children(dialog), encode-resource(id), default: #f);
  control
  | error("No such control id %=", id)
end method lookup-control;


define sealed method retrieve-resource
    (resource :: <loaded-resource>, database :: <win32-resource-database>)
 => (resource :: <loaded-resource>)
  resource
end method retrieve-resource;

define sealed method retrieve-resource
    (resource :: <resource-description>, database :: <win32-resource-database>)
 => (resource :: <loaded-resource>)
  let grokker :: false-or(<function>)
    = element(*grok-resource-table*, resource-type(resource), default: #f);
  if (grokker)
    grokker(resource, database.%module)
  else
    error("Resource of type %= not supported yet", resource-type(resource))
  end
end method retrieve-resource;


/// Grokking of simple resources

//--- $RT-ACCELERATOR  not yet handled
//--- $RT-FONT         not yet handled
//--- $RT-FONTDIR      not yet handled
//--- $RT-MENU         not yet handled
//--- $RT-RCDATA       not yet handled
//--- $RT-STRING       not yet handled
//--- $RT-MESSAGETABLE not yet handled
//--- $RT-GROUP-CURSOR not yet handled
//--- $RT-GROUP-ICON   not yet handled
//--- $RT-VERSION      not yet handled

define function grok-bitmap
    (resource :: <resource-description>, handle :: <HANDLE>)
 => (bitmap :: <bitmap-resource>)
  // We already have the handle, but need to load the resource
  let bitmap = LoadBitmap(handle, resource-id(resource));
  make(<bitmap-resource>,
       resource-description: resource,
       memory-handle: bitmap)
end function grok-bitmap;

*grok-resource-table*[$RT-BITMAP] := grok-bitmap;

define method unload-resource
    (resource :: <bitmap-resource>) => ()
  DeleteObject(memory-handle(resource))
end method unload-resource;


define function grok-icon
    (resource :: <resource-description>, handle :: <HANDLE>)
 => (icon :: <icon-resource>)
  // We already have the handle, but need to load the resource
  let icon = LoadIcon(handle, resource-id(resource));
  make(<icon-resource>,
       resource-description: resource,
       memory-handle: icon)
end function grok-icon;

*grok-resource-table*[$RT-ICON] := grok-icon;

define method unload-resource
    (resource :: <icon-resource>) => ()
  DestroyIcon(memory-handle(resource))
end method unload-resource;


define function grok-cursor
    (resource :: <resource-description>, handle :: <HANDLE>)
 => (cursor :: <cursor-resource>)
  // We already have the handle, but need to load the resource
  let cursor = LoadCursor(handle, resource-id(resource));
  make(<cursor-resource>,
       resource-description: resource,
       memory-handle: cursor)
end function grok-cursor;

*grok-resource-table*[$RT-CURSOR] := grok-cursor;

define method unload-resource
    (resource :: <cursor-resource>) => ()
  DestroyCursor(memory-handle(resource))
end method unload-resource;
