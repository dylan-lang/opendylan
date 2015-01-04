Module:    system-internals
Synopsis:  Settings and user profiles, for Win32
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Settings implementation for Win32

define inline-only constant $DWORD-SIZE = raw-as-integer(primitive-word-size());

define constant <LONG> = <machine-word>;

define constant <HKEY> = <machine-word>;

define inline-only constant $ERROR-SUCCESS              = as(<LONG>, 0);

define inline-only constant $REG-SZ                     = as(<LONG>, 1);
define inline-only constant $REG-EXPAND-SZ              = as(<LONG>, 2);
define inline-only constant $REG-MULTI-SZ               = as(<LONG>, 7);

define inline-only constant $KEY-QUERY-VALUE            = #x0001;
define inline-only constant $KEY-SET-VALUE              = #x0002;
define inline-only constant $KEY-CREATE-SUB-KEY         = #x0004;
define inline-only constant $KEY-ENUMERATE-SUB-KEYS     = #x0008;

define inline-only constant $REG-OPTION-NON-VOLATILE    = as(<LONG>, #x00000000);


define inline-only constant $HKEY-CLASSES-ROOT          = as(<HKEY>, #x80000000);
define inline-only constant $HKEY-CURRENT-USER          = as(<HKEY>, #x80000001);
define inline-only constant $HKEY-LOCAL-MACHINE         = as(<HKEY>, #x80000002);
define inline-only constant $HKEY-USERS                 = as(<HKEY>, #x80000003);

define inline-only function RegQueryValueEx
    (hKey :: <HKEY>, value-name :: <byte-string>)
 => (data :: <byte-string>, type :: <LONG>, status :: <LONG>)
  let type-buffer :: <byte-string>
    = make(<byte-string>, size: $DWORD-SIZE, fill: '\0');
  let buffer-size-buffer :: <byte-string>
    = make(<byte-string>, size: $DWORD-SIZE, fill: '\0');
  let status
    = primitive-wrap-machine-word
        (%call-c-function ("RegQueryValueExA", c-modifiers: "__stdcall")
             (hKey :: <raw-c-pointer>, lpValueName :: <raw-byte-string>,
              lpReserved :: <raw-c-pointer>, lpType :: <raw-c-pointer>,
              lpData :: <raw-c-pointer>, lpcbData :: <raw-c-pointer>)
          => (success? :: <raw-c-signed-long>)
           (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)),
            primitive-string-as-raw(value-name),
            primitive-cast-raw-as-pointer(integer-as-raw(0)),
            primitive-cast-raw-as-pointer(primitive-string-as-raw(type-buffer)),
            primitive-cast-raw-as-pointer(integer-as-raw(0)),
            primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer-size-buffer)))
         end);
  if (status = $ERROR-SUCCESS)
    let buffer-size
      = raw-as-integer
          (primitive-c-unsigned-long-at
            (primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer-size-buffer)),
             integer-as-raw(0), integer-as-raw(0)));
    let buffer :: <byte-string>
      = make(<byte-string>, size: buffer-size, fill: '\0');
    let status
      = primitive-wrap-machine-word
          (%call-c-function ("RegQueryValueExA", c-modifiers: "__stdcall")
               (hKey :: <raw-c-pointer>, lpValueName :: <raw-byte-string>,
                lpReserved :: <raw-c-pointer>, lpType :: <raw-c-pointer>,
                lpData :: <raw-c-pointer>, lpcbData :: <raw-c-pointer>)
            => (success? :: <raw-c-signed-long>)
             (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)),
              primitive-string-as-raw(value-name),
              primitive-cast-raw-as-pointer(integer-as-raw(0)),
              primitive-cast-raw-as-pointer(primitive-string-as-raw(type-buffer)),
              primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
              primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer-size-buffer)))
           end);
    let type
      = primitive-wrap-machine-word
          (primitive-c-unsigned-long-at
             (primitive-cast-raw-as-pointer(primitive-string-as-raw(type-buffer)),
              integer-as-raw(0), integer-as-raw(0)));
    // NOTE: For registry entries, the returned buffer-size may include a
    //       trailing NUL character...
    values(if (status = $ERROR-SUCCESS)
             let amount-to-copy
               = if (type = $REG-SZ | type = $REG-EXPAND-SZ)
                   buffer-size - 1
                 elseif (type = $REG-MULTI-SZ)
                   buffer-size - 2
                 else
                   buffer-size
                 end;
             copy-sequence(buffer, end: amount-to-copy)
           else
             ""
           end,
           type,
           status)
  else
    values("", $REG-SZ, status)
  end
end function RegQueryValueEx;

define inline-only function RegSetValueEx
    (hKey :: <HKEY>, value-name :: <byte-string>, type :: <LONG>, data :: <byte-string>)
 => (status :: <LONG>)
  primitive-wrap-machine-word
    (%call-c-function ("RegSetValueExA", c-modifiers: "__stdcall")
         (hKey :: <raw-c-pointer>, lpValueName :: <raw-byte-string>,
          reserved :: <raw-c-signed-long>, dwType :: <raw-c-signed-long>,
          lpData :: <raw-byte-string>, cbData :: <raw-c-signed-long>)
      => (status :: <raw-c-signed-long>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)),
        primitive-string-as-raw(value-name),
        integer-as-raw(0),
        primitive-unwrap-machine-word(type),
        primitive-string-as-raw(data),
        integer-as-raw(size(data) + 1))
     end)
end function RegSetValueEx;

define inline-only function RegDeleteValue (hKey :: <HKEY>, value-name :: <byte-string>)
 => (status :: <LONG>)
  primitive-wrap-machine-word
    (%call-c-function ("RegDeleteValueA", c-modifiers: "__stdcall")
         (hKey :: <raw-c-pointer>, lpValueName :: <raw-byte-string>)
      => (status :: <raw-c-signed-long>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)),
        primitive-string-as-raw(value-name))
     end)
end function RegDeleteValue;

define inline-only function RegOpenKeyEx
    (hKey :: <HKEY>, subkey-name :: <byte-string>, desired-sam :: <LONG>)
 => (hSubKey :: <HKEY>, status :: <LONG>)
  let hSubKey-buffer :: <byte-string>
    = make(<byte-string>, size: $DWORD-SIZE, fill: '\0');
  let status
    = primitive-wrap-machine-word
        (%call-c-function ("RegOpenKeyExA", c-modifiers: "__stdcall")
             (hKey :: <raw-c-pointer>, lpSubKey :: <raw-byte-string>,
              dwOptions :: <raw-c-signed-long>, samDesired :: <raw-c-signed-long>,
              phkResult :: <raw-c-pointer>)
          => (status :: <raw-c-signed-long>)
           (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)),
            primitive-string-as-raw(subkey-name),
            integer-as-raw(0),
            primitive-unwrap-machine-word(desired-sam),
            primitive-cast-raw-as-pointer(primitive-string-as-raw(hSubKey-buffer)))
         end);
  values(primitive-wrap-machine-word
           (primitive-c-unsigned-long-at
              (primitive-cast-raw-as-pointer(primitive-string-as-raw(hSubKey-buffer)),
               integer-as-raw(0), integer-as-raw(0))),
         status)
end function RegOpenKeyEx;

define inline-only function RegCreateKeyEx
    (hKey :: <HKEY>, subkey-name :: <byte-string>, class :: <byte-string>,
     options :: <LONG>, desired-sam :: <LONG>)
 => (hSubKey :: <HKEY>, status :: <LONG>)
  let hSubKey-buffer :: <byte-string>
    = make(<byte-string>, size: $DWORD-SIZE, fill: '\0');
  let disposition-buffer :: <byte-string>
    = make(<byte-string>, size: $DWORD-SIZE, fill: '\0');
  let status
    = primitive-wrap-machine-word
        (%call-c-function ("RegCreateKeyExA", c-modifiers: "__stdcall")
             (hKey :: <raw-c-pointer>, lpSubKey :: <raw-byte-string>,
              reserved :: <raw-c-signed-long>, lpClass :: <raw-byte-string>,
              dwOptions :: <raw-c-signed-long>, samDesired :: <raw-c-signed-long>,
              lpSecurityAttributes :: <raw-c-pointer>, phkResult :: <raw-c-pointer>,
              lpdwDisposition :: <raw-c-pointer>)
          => (status :: <raw-c-signed-long>)
           (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)),
            primitive-string-as-raw(subkey-name),
            integer-as-raw(0),
            primitive-string-as-raw(class),
            primitive-unwrap-machine-word(options),
            primitive-unwrap-machine-word(desired-sam),
            primitive-cast-raw-as-pointer(integer-as-raw(0)),
            primitive-cast-raw-as-pointer(primitive-string-as-raw(hSubKey-buffer)),
            primitive-cast-raw-as-pointer(primitive-string-as-raw(disposition-buffer)))
         end);
  values(primitive-wrap-machine-word
           (primitive-c-unsigned-long-at
              (primitive-cast-raw-as-pointer(primitive-string-as-raw(hSubKey-buffer)),
               integer-as-raw(0), integer-as-raw(0))),
         status)
end function RegCreateKeyEx;

define inline-only function RegCloseKey (hKey :: <HKEY>) => (status :: <LONG>)
  primitive-wrap-machine-word
    (%call-c-function ("RegCloseKey", c-modifiers: "__stdcall")
         (hKey :: <raw-c-pointer>)
      => (status :: <raw-c-signed-long>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)))
     end)
end function RegCloseKey;

define inline-only function RegDeleteKey (hKey :: <HKEY>, subkey-name :: <byte-string>)
 => (status :: <LONG>)
  primitive-wrap-machine-word
    (%call-c-function ("RegDeleteKeyA", c-modifiers: "__stdcall")
         (hKey :: <raw-c-pointer>, lpSubKey :: <raw-byte-string>)
      => (status :: <raw-c-signed-long>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(hKey)),
        primitive-string-as-raw(subkey-name))
     end)
end function RegDeleteKey;


/// Reading, writing, and removing values

define inline-only function settings-key-handle
    (settings :: <settings>) => (hkey :: false-or(<HKEY>))
  settings-handle(settings)
end settings-key-handle;

// Binds 'data' to a byte string containing the key's value
define macro reading-value
  { reading-value (?data:name = ?settings:expression, ?key:expression)
      ?:body
    end }
  => { begin
         let _hKey = settings-key-handle(?settings);
         if (_hkey)
           let (_buffer, _type, _result) = RegQueryValueEx(_hKey, ?key);
           // If we got an error or the type is wrong, just say the key wasn't found
           //---*** Is this really what we want to do with all errors?
           if (_result ~= $ERROR-SUCCESS | _type ~= $REG-SZ)
             values(#f, #f)
           else
             let ?data = as(<byte-string>, _buffer);
             values(begin ?body end, #t)
           end
         else
           values(#f, #f)
         end
       end }
end macro reading-value;

// 'body' should evaluate to a byte string, which is written as the key's value
define macro writing-value
  { writing-value (?settings:expression, ?key:expression)
      ?:body
    end }
  => { begin
         let _string :: <byte-string> = begin ?body end;
         let _hKey = settings-key-handle(?settings);
         if (_hkey)
           let _result = RegSetValueEx(_hKey, ?key, $REG-SZ, _string);
           //---*** Is this really what we want to do with all errors?
           if (_result ~= $ERROR-SUCCESS)
             #f
           else
             #t
           end
         end
       end }
end macro writing-value;


define sealed method get-value
    (settings :: <settings>, key :: <byte-string>, type == <string>)
 => (value :: false-or(<string>), found? :: <boolean>)
  reading-value (data = settings, key)
    data
  end
end method get-value;

define sealed method set-value
    (value :: <string>, settings :: <settings>, key :: <byte-string>, type == <string>)
 => (success? :: <boolean>)
  writing-value (settings, key)
    value
  end
end method set-value;


define sealed method get-value
    (settings :: <settings>, key :: <byte-string>, type == <symbol>)
 => (value :: false-or(<symbol>), found? :: <boolean>)
  reading-value (data = settings, key)
    as(<symbol>, data)
  end
end method get-value;

define sealed method set-value
    (value :: <symbol>, settings :: <settings>, key :: <byte-string>, type == <symbol>)
 => (success? :: <boolean>)
  writing-value (settings, key)
    as(<byte-string>, value)
  end
end method set-value;


define sealed method get-value
    (settings :: <settings>, key :: <byte-string>, type == <integer>)
 => (value :: false-or(<integer>), found? :: <boolean>)
  reading-value (data = settings, key)
    string-to-integer(data)
  end
end method get-value;

define sealed method set-value
    (value :: <integer>, settings :: <settings>, key :: <byte-string>, type == <integer>)
 => (success? :: <boolean>)
  writing-value (settings, key)
    integer-to-string(value)
  end
end method set-value;


define sealed method get-value
    (settings :: <settings>, key :: <byte-string>, type == <boolean>)
 => (value :: false-or(<boolean>), found? :: <boolean>)
  reading-value (data = settings, key)
    let value = as-lowercase!(data);
    case
      value = "yes" => #t;
      value = "no"  => #f;
      otherwise     => #t;
    end
  end
end method get-value;

define sealed method set-value
    (value :: <boolean>, settings :: <settings>, key :: <byte-string>, type == <boolean>)
 => (success? :: <boolean>)
  writing-value (settings, key)
    if (value) "yes" else "no" end
  end
end method set-value;


define sealed method get-value
    (settings :: <settings>, key :: <byte-string>, type == <machine-word>)
 => (value :: false-or(<machine-word>), found? :: <boolean>)
  reading-value (data = settings, key)
    string-to-machine-word(data)
  end
end method get-value;

define sealed method set-value
    (value :: <machine-word>,
     settings :: <settings>, key :: <byte-string>, type == <machine-word>)
 => (success? :: <boolean>)
  writing-value (settings, key)
    machine-word-to-string(value)
  end
end method set-value;


define sealed method do-remove-value!
    (settings :: <settings>, key :: <byte-string>) => ()
  let hKey = settings-key-handle(settings);
  when (hKey)
    RegDeleteValue(hKey, key)
  end;
end method do-remove-value!;


/// Creating keys

define sealed method settings-key-name
    (settings :: <system-settings>) => (key-name :: <byte-string>)
  "HKEY_CLASSES_ROOT"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <system-settings>) => (handle :: <HKEY>)
  $HKEY-CLASSES-ROOT
end method settings-handle;

define sealed method settings-writable?
    (settings :: <system-settings>) => (writable? :: <boolean>)
  #t
end method settings-writable?;


define sealed method settings-key-name
    (settings :: <site-settings>) => (key-name :: <byte-string>)
  error("Site settings not supported under Windows")
end method settings-key-name;

define sealed method settings-key-name
    (settings :: <site-software-settings>) => (key-name :: <byte-string>)
  error("Site settings not supported under Windows")
end method settings-key-name;


define sealed method settings-key-name
    (settings :: <local-settings>) => (key-name :: <byte-string>)
  "HKEY_LOCAL_MACHINE"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <local-settings>) => (handle :: <HKEY>)
  $HKEY-LOCAL-MACHINE
end method settings-handle;

define sealed method settings-writable?
    (settings :: <local-settings>) => (writable? :: <boolean>)
  #t
end method settings-writable?;

define sealed method settings-key-name
    (settings :: <local-software-settings>) => (key-name :: <byte-string>)
  "Software"
end method settings-key-name;

define sealed method settings-key-name
    (settings :: <local-hardware-settings>) => (key-name :: <byte-string>)
  "Hardware"
end method settings-key-name;


define sealed method settings-key-name
    (settings :: <default-user-settings>) => (key-name :: <byte-string>)
  "HKEY_USERS"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <default-user-settings>) => (handle :: <HKEY>)
  $HKEY-USERS
end method settings-handle;

define sealed method settings-writable?
    (settings :: <default-user-settings>) => (writable? :: <boolean>)
  #t
end method settings-writable?;

define sealed method settings-key-name
    (settings :: <default-user-software-settings>) => (key-name :: <byte-string>)
  "Software"
end method settings-key-name;


define sealed method settings-key-name
    (settings :: <current-user-settings>) => (key-name :: <byte-string>)
  "HKEY_CURRENT_USER"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <current-user-settings>) => (handle :: <HKEY>)
  $HKEY-CURRENT-USER
end method settings-handle;

define sealed method settings-writable?
    (settings :: <current-user-settings>) => (writable? :: <boolean>)
  #t
end method settings-writable?;

define sealed method settings-key-name
    (settings :: <current-user-software-settings>) => (key-name :: <byte-string>)
  "Software"
end method settings-key-name;


/// Registering keys

define inline-only constant $READING-SAM
  = as(<LONG>, logior($KEY-QUERY-VALUE, $KEY-ENUMERATE-SUB-KEYS));

define inline-only constant $WRITING-SAM
  = as(<LONG>, logior($KEY-QUERY-VALUE, $KEY-SET-VALUE,
                      $KEY-CREATE-SUB-KEY, $KEY-ENUMERATE-SUB-KEYS));

define variable *settings-default-class* = "Open Dylan";

define sealed method initialize-settings
    (settings :: <settings>, for-writing? :: <boolean>) => ()
  local method open ()
          let parent  = element($settings-table, settings-parent(settings));
          initialize-settings(parent, for-writing?);
          let hKey    = settings-key-handle(parent);
          if (hKey)
            let key   = settings-key-name(settings);
            let class = *settings-default-class*;
            let (phkResult, result)
              = if (for-writing?)
                  RegCreateKeyEx(hKey, key, class, $REG-OPTION-NON-VOLATILE, $WRITING-SAM)
                else
                  RegOpenKeyEx(hKey, key, $READING-SAM)
                end;
            if (result ~= $ERROR-SUCCESS)
              //---*** What should we do with errors?
              #f
            else
              settings-writable?(settings) := for-writing?;
              settings-handle(settings)    := phkResult;
            end
          end
        end method;
  let handle = settings-key-handle(settings);
  if (handle)
    when (for-writing? & ~settings-writable?(settings))
      RegCloseKey(handle);
      invalidate-settings-caches(settings);
      open()
    end
  else
    open()
  end
end method initialize-settings;

define sealed method register-key
    (settings :: <settings>, key-name :: <byte-string>, for-writing? :: <boolean>)
 => (key :: <byte-string>)
  // No need to do anything except ensure that all the parent
  // settings have been initialized
  //---*** This should transmogrify '?' and '*' characters
  initialize-settings(settings, for-writing?);
  key-name
end method register-key;

define sealed method unregister-key
    (settings :: <settings>, key-name :: <byte-string>) => ()
  initialize-settings(settings, #t);
  let hKey = settings-key-handle(settings);
  when (hKey)
    RegDeleteKey(hKey, key-name)
  end;
end method unregister-key;
