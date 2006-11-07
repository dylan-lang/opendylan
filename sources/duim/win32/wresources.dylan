Module:    Win32-duim
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline function pointer+
    (p :: <C-statically-typed-pointer>, offset :: <integer>)
 => (p :: <C-statically-typed-pointer>)
  pointer-value-address(p, index: offset)
end function pointer+;

define inline function pointer-address-16
    (p) => (p-16)
  %logand(\%+(p, 1), as(<machine-word>, #xFFFFFFFE))
end function pointer-address-16;

define inline function pointer-address-32
    (p) => (p-32)
  %logand(\%+(p, 3), as(<machine-word>, #xFFFFFFFC))
end function pointer-address-32;


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


define generic window-position
    (resource :: <window-resource>) => (x :: <integer>, y :: <integer>);
define generic window-size
    (resource :: <window-resource>) => (w :: <integer>, h :: <integer>);

define abstract class <window-resource> (<resource>) 
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

define generic lookup-control
    (window :: <window-resource>, id :: <resource-id>) => (resource :: <window-resource>);


/// Decoding

define sealed method decode-resource
    (raw-id :: <raw-resource-id>)
 => (resource-id :: type-union(<unsigned-int>, <string>))
  let value = pointer-address(raw-id);
  if (zero?(%logand(as(<machine-word>, #xFFFF0000), value)))
    as(<integer>, value)
  else
    as(<byte-string>, raw-id)
  end
end method decode-resource;


/// Encoding

define sealed method encode-resource
    (resource-id :: <unsigned-int>) => (raw-id :: <raw-resource-id>)
  MAKEINTRESOURCE(resource-id)
end method encode-resource;

define sealed method encode-resource
    (resource-id :: <C-string>) => (raw-id :: <raw-resource-id>)
  as(<raw-resource-id>, resource-id)            // this should work
end method encode-resource;

define sealed method encode-resource
    (resource-id :: <C-unicode-string>) => (raw-id :: <raw-resource-id>)
  as(<raw-resource-id>, resource-id)            // this should work
end method encode-resource;

define sealed method encode-resource
    (resource-id :: <byte-string>) => (raw-id :: <raw-resource-id>)
  as(<raw-resource-id>, as(<C-string>, resource-id))
end method encode-resource;


/// Resource classes

define sealed class <resource-wrapper>
    (<resource>)
  slot %resource :: <resource>,
    required-init-keyword: resource:;
end class <resource-wrapper>;

define sealed domain make (singleton(<resource-wrapper>));
define sealed domain initialize (<resource-wrapper>);


define abstract class <win32-resource>
    (<resource>)
  slot resource-handle :: <HANDLE>,
    init-keyword: resource-handle:;
  slot resource-id :: <raw-resource-id>,
    init-keyword: resource-id:;
  slot resource-size :: <integer>,
    init-keyword: resource-size:;
end class <win32-resource>;

// Stand-in for a loaded resource so we can load it lazily...
define sealed class <resource-description>
    (<win32-resource>) 
  constant slot resource-type-value :: <resource-type>, 
    required-init-keyword: resource-type:;
end class <resource-description>;

define sealed domain make (singleton(<resource-description>));
define sealed domain initialize (<resource-description>);

define method resource-type
    (resource :: <resource-description>) => (resource-type :: <resource-type>)
  resource-type-value(resource)
end method resource-type;


// A resource once it as been loaded into memory
define abstract class <loaded-resource>
    (<win32-resource>) 
  constant slot memory-handle :: <HANDLE>,
    init-keyword: memory-handle:;
end class <loaded-resource>;

// Not actually used....
ignore(memory-handle);

define method initialize
    (resource :: <loaded-resource>, #key resource-description)
  next-method();
  resource-handle(resource) := resource-handle(resource-description);
  resource-id(resource)     := resource-id(resource-description);
  resource-size(resource)   := resource-size(resource-description);
end method initialize;

define abstract class <pseudo-resource>
    (<win32-resource>)
end class <pseudo-resource>;


define sealed class <bitmap-resource>
    (<loaded-resource>)
end class <bitmap-resource>;

define sealed domain make (singleton(<bitmap-resource>));
define sealed domain initialize (<bitmap-resource>);


define sealed class <icon-resource>
    (<loaded-resource>)
end class <icon-resource>;

define sealed domain make (singleton(<icon-resource>));
define sealed domain initialize (<icon-resource>);


define sealed class <cursor-resource>
    (<loaded-resource>)
end class <cursor-resource>;

define sealed domain make (singleton(<cursor-resource>));
define sealed domain initialize (<cursor-resource>);


/// Dialog resources

define sealed class <dialog-resource>
    (<loaded-resource>, <top-window-resource>)
  constant slot dialog-template :: <LPDLGTEMPLATE>,
    required-init-keyword: template:;
  slot dialog-font-name :: <raw-resource-id> = encode-resource(0);
  slot dialog-font-size :: <integer> = 0;
  constant slot dialog-children = make(<resource-table>);
end class <dialog-resource>;

define sealed domain make (singleton(<dialog-resource>));
define sealed domain initialize (<dialog-resource>);


define sealed method window-position
    (dialog :: <dialog-resource>) => (x :: <integer>, y :: <integer>)
  values(dialog-template(dialog).x-value, dialog-template(dialog).y-value)
end method window-position;

define sealed method window-size
    (dialog :: <dialog-resource>) => (w :: <integer>, h :: <integer>)
  values(dialog-template(dialog).cx-value, dialog-template(dialog).cy-value)
end method window-size;

define sealed method gadget-count
    (dialog :: <dialog-resource>) => (n :: <integer>)
  dialog-template(dialog).cdit-value
end method gadget-count;


define generic register-child
    (dialog :: <dialog-resource>, child :: <window-resource>, id :: <resource-id>) => ();

define sealed method register-child
    (dialog :: <dialog-resource>, child :: <control-resource>, id :: <unsigned-int>) => ()
  dialog-children(dialog)[encode-resource(id)] := child;
end method register-child;

define sealed method register-child
    (dialog :: <dialog-resource>, child :: <control-resource>, id :: <byte-string>) => ()
  dialog-children(dialog)[encode-resource(id)] := child
end method register-child;



/// Control resources

define sealed class <control-resource> 
    (<pseudo-resource>, <window-resource>)
  constant slot control-template :: <LPDLGITEMTEMPLATE>, 
    required-init-keyword: template:;
end class <control-resource>;

define method initialize
    (resource :: <control-resource>, #key)
  next-method();
  resource-id(resource) := encode-resource(control-template(resource).id-value)
end method initialize;

define sealed domain make (singleton(<control-resource>));
define sealed domain initialize (<control-resource>);


define sealed method window-position
    (control :: <control-resource>) => (x :: <integer>, y :: <integer>)
  values(control-template(control).x-value, control-template(control).y-value)
end method window-position;

define sealed method window-size
    (control :: <control-resource>) => (w :: <integer>, h :: <integer>)
  values(control-template(control).cx-value, control-template(control).cy-value)
end method window-size;


define sealed method get-resource-id
    (control :: <control-resource>) => (id :: <resource-id>)
  control-template(control).id-value
end method get-resource-id;


/// Toolbar resources

define sealed class <toolbar-resource>
    (<loaded-resource>, <window-resource>)
end class <toolbar-resource>;

define sealed domain make (singleton(<toolbar-resource>));
define sealed domain initialize (<toolbar-resource>);


/// Resource tables

define sealed class <resource-table> (<table>)
end class <resource-table>;

define sealed domain make (singleton(<resource-table>));
define sealed domain initialize (<resource-table>);

define sealed method resource-equal
    (id1 :: <raw-resource-id>, id2 :: <raw-resource-id>) => (equal? :: <boolean>)
  pointer-address(id1) = pointer-address(id2)
end method resource-equal;

define sealed method resource-hash
    (id :: <raw-resource-id>, hash-state :: <hash-state>)
 => (hash-value, hash-state)
  let (value, state) = object-hash(decode-resource(id), hash-state);
  values(value, state)
end method resource-hash;

define sealed method table-protocol
    (table :: <resource-table>)
 => (test-function :: <function>, hash-function :: <function>)
  values(resource-equal, resource-hash)
end method table-protocol;


/// Resource databases

define abstract class <resource-database> (<object>)
end class <resource-database>;

define generic processing-type
    (database :: <resource-database>, type :: <resource-type>);

define generic store-resource-name
    (database :: <resource-database>, name :: <resource-id>) => ();

define generic store-resource-details
    (database :: <resource-database>,
     handle :: <HANDLE>, resource-size :: <integer>, language-id :: <integer>) => ();


define sealed class <win32-resource-database> (<resource-database>)
  constant slot %resources :: <resource-table> = make(<resource-table>);
  slot %module :: <HANDLE>;             // the current instance handle
end class <win32-resource-database>;

define sealed domain make (singleton(<win32-resource-database>));
define sealed domain initialize (<win32-resource-database>);


/// Initializing resource databases

// The single resource database for the current application
define variable *resource-database* :: <win32-resource-database>
  = make(<win32-resource-database>);

// These are used for booting the application
define thread variable *current-database*   = #f;
define thread variable *current-type*       = #f;
define thread variable *current-type-table* = #f;
define thread variable *current-resource*   = #f;

define function load-default-resources
    () => (status :: <boolean>)
  let hInstance = application-instance-handle();
  *resource-database*.%module := hInstance;
  enumerate-resources(hInstance, database: *resource-database*)
end function load-default-resources;

// Must be called with the four thread variables above bound...
define sealed method processing-type
    (database :: <win32-resource-database>, type :: <resource-type>)
  let raw-type = encode-resource(type);
  let type-db  = element(database.%resources, raw-type, default: #"not-found");
  when (type-db == #"not-found")
    type-db := make(<resource-table>);
    database.%resources[raw-type] := type-db
  end;
  *current-type*       := raw-type;
  *current-type-table* := type-db;
end method processing-type;


// Must be called with the four thread variables above bound...
define sealed method store-resource-name
    (database :: <win32-resource-database>, name :: <unsigned-int>) => ()
  store-new-resource(database, encode-resource(name))
end method store-resource-name;

define sealed method store-resource-name
    (database :: <win32-resource-database>, name :: <byte-string>) => ()
  store-new-resource(database, encode-resource(name))
end method store-resource-name;

define sealed method store-resource-name
    (database :: <win32-resource-database>, name :: <raw-resource-id>) => ()
  store-new-resource(database, name)
end method store-resource-name;

define sealed method store-new-resource
    (database :: <win32-resource-database>, name :: <raw-resource-id>) => ()
  let resource = make(<resource-description>, 
                      resource-id: name,
                      resource-type: *current-type*);
  let wrapper  = make(<resource-wrapper>,
                      resource: resource);
  *current-type-table*[name] := wrapper;
  *current-resource*         := resource;
end method store-new-resource;


// Must be called with the four thread variables above bound...
define sealed method store-resource-details
    (database :: <win32-resource-database>,
     handle :: <HANDLE>, resource-size :: <integer>, language-id :: <integer>) => ()
  resource-handle(*current-resource*) := handle;
  resource-size(*current-resource*)   := resource-size;
end method store-resource-details;

define sealed method enumerate-resources
    (handle :: <HINSTANCE>, 
     #key database :: false-or(<resource-database>) = #f)
 => (success? :: <boolean>)
  assert(~null-handle?(handle), "Invalid handle to resource module");
  assert(database, "No database supplied");
  dynamic-bind (*current-database*   = database,
                *current-type*       = #f,
                *current-type-table* = #f,
                *current-resource*   = #f)
    let success? = EnumResourceTypes(handle, EnumResTypeProc, 0);
    success?
  end
end method enumerate-resources;


define sealed method enumerate-resource-types
    (hModule :: <HANDLE>,               // module handle
     lpType  :: <LPTSTR>,               // address of resource type
     lParam  :: <lparam-type>)          // extra parameter, could be used for error checking
 => (value :: <boolean>)      
  unless (null-pointer?(lpType))
    processing-type(*current-database*, lpType);
    // Find the names of all resources of type lpType
    EnumResourceNames(hModule, lpType, EnumResNameProc, 0)
  end;
  #t
end method enumerate-resource-types;

define callback EnumResTypeProc :: <ENUMRESTYPEPROC> = enumerate-resource-types;
  

define sealed method enumerate-resource-names
    (hModule :: <HANDLE>,               // module handle
     lpType  :: <LPCTSTR>,              // address of resource type
     lpName  :: <LPTSTR>,               // address of resource name
     lParam  :: <lparam-type>)          // extra parameter, could be used for error checking
 => (value :: <boolean>)      
  unless (null-pointer?(lpName))
    store-resource-name(*current-database*, lpName);
    // Find the languages of all resources of type lpType and name lpName
    EnumResourceLanguages(hModule, lpType, lpName, EnumResLangProc, 0)
  end;
  #t
end method enumerate-resource-names;

define callback EnumResNameProc :: <ENUMRESNAMEPROC> = enumerate-resource-names;


define sealed method enumerate-resource-languages
    (hModule :: <HANDLE>,               // module handle
     lpType  :: <LPCTSTR>,              // address of resource type
     lpName  :: <LPCTSTR>,              // address of resource name
     wLang   :: <integer>,              // resource language
     lParam  :: <lparam-type>)          // extra parameter, could be used for error checking
 => (value :: <boolean>)
  let hResInfo :: <HANDLE> = 
    FindResourceEx(hModule, lpType, lpName, wLang);
  let resource-size = SizeofResource(hModule, hResInfo);
  store-resource-details(*current-database*, hResInfo, resource-size, wLang);
  #t
end method enumerate-resource-languages;

define callback EnumResLangProc :: <ENUMRESLANGPROC> = enumerate-resource-languages;

/// Resource lookup

define constant *grok-resource-table* :: <resource-table>
  = make(<resource-table>);


define sealed method lookup-resource
    (type :: <resource-type>, id :: <unsigned-int>)
 => (resource :: <resource>)
  let table    = *resource-database*.%resources[type];  // doesn't have to be encoded
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

define constant $dlg-template-size  :: <integer> = 18;
define constant $item-template-size :: <integer> = 18;

define function grok-dialog
    (resource :: <resource-description>, handle :: <HANDLE>)
 => (dialog :: <dialog-resource>)
  let resource-address
    = pointer-address(LoadResource(handle, resource-handle(resource)));
  let template :: <LPDLGTEMPLATE>
    = make(<LPDLGTEMPLATE>, address: resource-address);
  let dialog = make(<dialog-resource>, 
                    resource-description: resource,
                    template: template);
  let template-size :: <integer> = 0;
  unless (null-pointer?(template))
    let offset :: <integer> = $dlg-template-size;
    let menu-test-pointer :: <LPWORD>
      = make(<LPWORD>, address: \%+(resource-address, offset));
    let (menu-resource-id, menu-resource-id-size)
      = grok-resource-id(menu-test-pointer);
    offset := offset + menu-resource-id-size;
    let class-test-pointer :: <LPWORD>
      = make(<LPWORD>, address: \%+(resource-address, offset));
    let (class-resource-id, class-resource-id-size)
      = grok-resource-id(class-test-pointer);
    offset := offset + class-resource-id-size;
    let title-string-pointer :: <C-void*>
      = make(<C-void*>, address: \%+(resource-address, offset));
    let (title-string, title-size)
      = grok-resource-string(title-string-pointer);
    offset := offset + title-size;
    let font-resource-size :: <integer> = 0;
    when (logand(template.style-value, $DS-SETFONT) ~= 0)
      let font-size-pointer :: <LPWORD>
        = make(<LPWORD>, address: \%+(resource-address, offset));
      offset := offset + size-of(<WORD>);
      dialog-font-size(dialog) := pointer-value(font-size-pointer);
      let font-name-pointer :: <C-pointer>
        = pointer+(font-size-pointer, 1);
      let (font-name, font-name-size)
        = grok-resource-string(font-name-pointer);
      dialog-font-name(dialog) := encode-resource(font-name);
      offset := offset + font-name-size;
    end;
    let addr = \%+(resource-address, offset);
    for (i :: <integer> from 1 below template.cdit-value,
         item = make(<LPDLGITEMTEMPLATE>, address: pointer-address-32(addr))
           then make(<LPDLGITEMTEMPLATE>, address: pointer-address-32(addr)))
      let (item-size, control-resource)
        = grok-item-template(item);
      register-child(dialog, control-resource, get-resource-id(control-resource));
      addr := \%+(pointer-address(item), item-size);
    finally
      let (item-size, control-resource)
        = grok-item-template(item);
      register-child(dialog, control-resource, get-resource-id(control-resource));
      addr := \%+(pointer-address(item), item-size);
      template-size := as(<integer>, \%-(addr, resource-address));
    end
  end;
  assert(resource-size(dialog) = template-size,
         "Incorrect dialog resource size retrieved");
  dialog
end function grok-dialog;

*grok-resource-table*[$RT-DIALOG] := grok-dialog;

define method grok-item-template
    (template :: <LPDLGITEMTEMPLATE>)
 => (resource-size :: <integer>, resource :: <control-resource>)
  let control = make(<control-resource>,
                     template: template);
  let class-test-pointer :: <LPWORD>
    = make(<LPWORD>, address: \%+(pointer-address(template), $item-template-size));
  let (class-resource-id, class-resource-id-size)
    = grok-resource-id(class-test-pointer);
  let text-address
    = \%+(pointer-address(class-test-pointer), class-resource-id-size);
  let text-resource-pointer :: <LPWORD>
    = make(<LPWORD>, address: pointer-address-16(text-address));
  let (text-resource, text-size)
    = grok-resource-id(text-resource-pointer);
  let creation-data-pointer
    = make(<LPWORD>, address: \%+(text-address, text-size));
  let data-size = grok-creation-data(creation-data-pointer);
  let template-size
    = as(<integer>,             // skip overflow check
         \%-(\%+(pointer-address(creation-data-pointer), data-size),
             pointer-address(template)));
  resource-size(control) := template-size;
  values(template-size, control)
end method grok-item-template;

define method grok-resource-id
    (p :: <LPWORD>) 
 => (id :: type-union(<unsigned-int>, <string>), resource-id-size :: <integer>)
  let word-pointer = as(<LPWORD>, p);
  let word-value   = pointer-value(word-pointer);
  case
    word-value = #x0000 =>
      values(0, size-of(<WORD>));
    word-value = #xFFFF =>
      let id = pointer-value(word-pointer, index: 1);
      let sz = 2 * size-of(<WORD>);
      values(id, sz);
    otherwise =>
      grok-resource-string(p);
  end
end method grok-resource-id;

define method grok-resource-string
    (p :: <C-pointer>)
 => (string :: <string>, resource-size :: <integer>)
  let resource-string :: <C-unicode-string>
    = make(<C-unicode-string>, address: pointer-address(p));
  //---*** (size(resource-string) + 1) * size-of(referenced-type(resource-string));
  let resource-size :: <integer> = (size(resource-string) + 1) * 2;
  values(resource-string, resource-size)
end method grok-resource-string;

define method grok-creation-data
    (p :: <LPWORD>)
 => (data-size :: <integer>)
  let data-value :: <integer> = as(<integer>, pointer-value(p));
  if (data-value = 0)
    size-of(<WORD>)
  else
    size-of(<WORD>) + data-value
  end
end method grok-creation-data;


/*---*** Not ready for prime-time yet!

define constant $RT-TOOLBAR = MAKEINTRESOURCE(241);

define C-struct <CTOOLBARDATA>
  slot Version-value   :: <WORD>;
  slot Width-value     :: <WORD>;
  slot Height-value    :: <WORD>;
  slot ItemCount-value :: <WORD>;
  pointer-type-name: <LPDLGTEMPLATE>;
end C-struct <CTOOLBARDATA>;

struct CToolBarData
{
        WORD wVersion;
        WORD wWidth;
        WORD wHeight;
        WORD wItemCount;
        //WORD aItems[wItemCount]

        WORD* items()
                { return (WORD*)(this+1); }
};

*grok-resource-table*[$RT-TOOLBAR] := grok-toolbar;

*/
