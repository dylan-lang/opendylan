Resource Database library
-------------------------

There is no attempt to make it portable despite appearances. It works
only on win32.

The resource-database is a runtime support for groking resources
linked to the executable on Windows.

User model
----------

The library would be used with DUIM. The user creates dialogs, menus
and other resources with a gui builder. At this time we can use VC++
or the dialog editor and menu editor in the SDK.

The gui builder app generates .rc files. If more then one, the files
have to be combined into one using #include directives.

The .rc files are compiled into the .res file by the resource compiler
from the SDK.

The .res file is linked with the application. Note that .res files can
be linked with DLL's too.

While designing the dialogs and menus the user assignes symbolic (or
string) ID's to the resources (dialogs, gadgets, bitmaps etc). The
resource compiler generates a resource.h files which defines those
symbols. A script converts the resource.h file into resource.dylan
files which defines Dylan equivalents of those symbols.

DUIM accepts a keyword, say id: or resource-id:, in all of its frame
and gadget creation functions. The user uses the symbols defined in
resource.dylan in the creation functions. Note that if strings are
used for resource id's the resource.dylan files is
unnecessary. Otherwise resource.dylan file is made part of the
application's Dylan library. The user doesn't have to specify any
other attributes when calling DUIM creation functions.

DUIM looks up the frames and gadgets in the resource database and
creates them based on the info (details later).

DUIM may attempt to create itself the ui objects if they are missing
in the database.

All MS tools will be superceded by our GUI builder in the future.

So, the main communication channel between the UI designer and the
programmer is the set of symbolic or string ID's. The dialog editors
allow the user to edit many attributes which DUIM is not interested in
- which is perfectly OK. However, there are some attributes - e.g. if
it is a child window, or if the parent window should be notified about
some events - that might affect the DUIM's model of things which is
based on the code entered by the programmer. We could just treat it as
an error (if DUIM disagrees with the resource) being a result of
misscommunication between the UI designer and the programmer, or
attempt to fix it up under the scenes.

DUIM's model
------------

When I say DUIM below, it refers to the backend. When I say DUIM API I
mean the user level API. 

The dialogs can be created directly from the resources and Windows
handles all the details. In this case however the dialogs have default
window classes. The main window in an win32 application has to have a
custom window class. In this case DUIM could use a dialog resource
template to create a custom window. This may be accomplished by
modifying the template in runtime or by requesting that the UI
designer assignes a special class to top level DUIM frames.

When DUIM creates top level frames it looks for a dialog with some id
in the resource database. When it creates the gadgets in looks up
those gadgets in the dialog again by id. This process is to create the
DUIM data structures describing the frame.

In most cases the whole dialog including gadgets can be created based
on the resource template. The id's of command gadgets (those that send
command messages) have to be registered with a generic backend event
dispacher so they can be forwarded to DUIM for processing. 

Note that in this model DUIM doesn't know the id's of gadget windows -
this is OK for static dialogs. If DUIM needs to know the id's of
window it can use GetDlgItem function to retrieve them.

Finally window can be created step by step using the info from the
dialog template, but I don't think it will be necessary.

What about DUIM's geometry managers ? In most cases they are not
separate windows I understand. So, DUIM just wraps some gadgets in
those managers based on their id's. We probably need a manager that
accepts geometry as is, so the user may do the alignment and
positioning himself.

Dialogs can also be used as children of other windows. The MS tools
don't allow for this directly. The child has to be created
programmatically. We may need another keyword at the DUIM API level or
the database may look for a child dialog if a gadget of with an id
cannot be found in the dialog. (In other words one cannot generate a
dialog resource containing other dialogs).

The user may create dialogs with a custom window classes (by hand
editing the .rc file if using VC++ or directly when using the dialog
editor from the SDK). This might allow for special event processing
for this particular dialog (remember that when a window is created in
win32 one must specify its class). But this would require some
additional abstraction at the DUIM API level. An exception would be a
special predefined DUIM window class (known to DUIM) which the UI
designer would assign to some dialogs. This wouldn't require any DUIM
API changes.

DUIM may also use the resource database in a "half-way", by looking up
fonts, colors, bitmaps, strings, etc and handling the window creation
itself. In this case the user would specify attributes to DUIM
creation function by referring to them by their resopurce id's. I
wonder if this can be done without extending DUIM's API. For example
if one wants to specify text for a label gadget using a resource id,
can the same keyword be used ?

API
---

Initial implemented API. We may add more as needed. (I just noticed
that create-dialog() is missing)

define abstract class <resource> (<object>)
//  virtual slot resource-id :: <resource-id>;
end;

define abstract class <window-resource> (<resource>) 
  slot window-class :: <resource-id>;
//  keyword window-class;
end;

define generic x-position(r :: <window-resource>) => (_ :: <integer>);
define generic y-position(r :: <window-resource>) => (_ :: <integer>);
define generic width(r :: <window-resource>) => (_ :: <integer>);
define generic height(r :: <window-resource>) => (_ :: <integer>);

// top level window
define abstract class <top-window-resource> (<window-resource>)
//  virtual slot number-of-gadgets :: <integer>;
end;

define generic number-of-gadgets(r :: <top-window-resource>)
 => (_ :: <integer>);

define constant <raw-resource-id> = <LPTSTR>;


define constant <resource-type> = type-union(<raw-resource-id>,
					     <integer>,
					     <byte-string>);

define generic resource-type (r :: <resource>) => (_ :: <resource-type>);

define constant <resource-id> = type-union(<raw-resource-id>,
					   <integer>, 
					   <byte-string>);

define generic encode-resource(r :: type-union(<resource-type>, <resource-id>))
 => (_ :: <raw-resource-id>);


define function load-default-resources()
  win32-load-app-instance-resources();
end;

//define method load-resources(handle :: <module-handle>) => ();
//end;

define generic lookup-resource(type :: <resource-type>,
			       id :: <resource-id>)
 => (_ :: <resource>);

define generic lookup-dialog(id :: <resource-id>)
 => (_ :: <top-window-resource>);

// In principle windows can have children too
define generic lookup-control(window :: <window-resource>, id :: <resource-id>)
 => (_ :: <window-resource>);

define generic dialog-menu(d :: <top-window-resource>)
 => (_ :: <resource-id>);


