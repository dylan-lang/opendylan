Module:    dylan-user
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module win32-resources
  create <resource-id>,
	 <resource-type>;

  create <resource>,
	 resource-id,
         resource-type;

  create <window-resource>,
	 window-class,
	 window-position, 
	 window-size;

  create <top-window-resource>,
	 gadget-count;

  create <dialog-resource>,
	 dialog-template,
	 dialog-menu,
	 dialog-title,
	 dialog-font-name,
	 dialog-font-size,
	 dialog-children,
	 control-parent,
	 control-text,
	 control-creation-data-size,
	 control-creation-data;

  create encode-resource,
	 decode-resource,
	 lookup-resource,
	 lookup-dialog,
	 lookup-control,
	 load-default-resources,
	 unload-resource;

  // Debugging support
  create describe-database,
	 describe-resource;
end module win32-resources;

define module win32-resources-internal
  use common-dylan,
    exclude: { debug-message };
  use threads;
  use table-extensions,
    exclude: { table };
  use machine-words;
  use c-ffi;
  use win32-common;
  use win32-user;
  use win32-gdi;
  use win32-kernel,
    exclude: { sleep };

  use simple-format;

  use win32-resources;
end module win32-resources-internal;
