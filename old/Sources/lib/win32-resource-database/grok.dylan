Module:    win32-resource-database-internal 
Synopsis:  grok simple reasources
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

*grok-resource-table*[$RT-BITMAP] := grok-bitmap;

define function grok-bitmap(r :: <resource-description>, m :: <HANDLE>)
 => (resource :: <dialog-resource>);
  // we already have the handle, but need to load the resource
  let bitmap-handle = LoadBitmap(m, r.resource-id);
  let new-bitmap = make(<simple-resource>,
			resource-description: r,
			memory-handle: bitmap-handle);
  new-bitmap;
end;

*grok-resource-table*[$RT-ICON] := grok-icon;

define function grok-icon(r :: <resource-description>, m :: <HANDLE>)
 => (resource :: <dialog-resource>);
  // we already have the handle, but need to load the resource
  let icon-handle = LoadIcon(m, r.resource-id);
  let new-icon = make(<simple-resource>,
		      resource-description: r,
		      memory-handle: icon-handle);
  new-icon;
end;

*grok-resource-table*[$RT-CURSOR] := grok-cursor;

define function grok-cursor(r :: <resource-description>, m :: <HANDLE>)
 => (resource :: <dialog-resource>);
  // we already have the handle, but need to load the resource
  let cursor-handle = LoadCursor(m, r.resource-id);
  let new-cursor = make(<simple-resource>,
			resource-description: r,
			memory-handle: cursor-handle);
  new-cursor;
end;

