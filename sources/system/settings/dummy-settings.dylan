Module:    system-internals
Synopsis:  Settings and user profiles, for the emulator
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Settings implementation for the emulator

/// Reading, writing, and removing values

// Binds 'data' to a byte string containing the key's value
//---*** Do this by reading from a file named by the key
define macro reading-value
  { reading-value (?data:name = ?settings:expression, ?key:expression)
      ?:body
    end }
  => { values(#f, #f) }
end macro reading-value;

// 'body' should evaluate to a byte string, which is written as the key's value
//---*** Do this by writing to a file named by the key
define macro writing-value
  { writing-value (?settings:expression, ?key:expression)
      ?:body
    end }
  => { #f }
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
  //---*** Do this by deleting the file named by the key
end method do-remove-value!;


/// Creating keys

define sealed method settings-key-name
    (settings :: <system-settings>) => (key-name :: <byte-string>)
  "System"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <system-settings>) => (handle :: <byte-string>)
  "~/.settings/System"
end method settings-handle;


define sealed method settings-key-name
    (settings :: <site-settings>) => (key-name :: <byte-string>)
  "Site"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <site-settings>) => (handle :: <byte-string>)
  "~/.settings/Site" 
end method settings-handle;

define sealed method settings-key-name
    (settings :: <site-software-settings>) => (key-name :: <byte-string>)
  "Software"
end method settings-key-name;


define sealed method settings-key-name
    (settings :: <local-settings>) => (key-name :: <byte-string>)
  "Local"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <local-settings>) => (handle :: <byte-string>)
  "~/.settings/Local" 
end method settings-handle;

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
  "Users"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <default-user-settings>) => (handle :: <byte-string>)
  "~/.settings/Users" 
end method settings-handle;

define sealed method settings-key-name
    (settings :: <default-user-software-settings>) => (key-name :: <byte-string>)
  "Software"
end method settings-key-name;


define sealed method settings-key-name
    (settings :: <current-user-settings>) => (key-name :: <byte-string>)
  "User"
end method settings-key-name;

define sealed method settings-handle
    (settings :: <current-user-settings>) => (handle :: <byte-string>)
  "~/.settings/Users" 
end method settings-handle;

define sealed method settings-key-name
    (settings :: <current-user-software-settings>) => (key-name :: <byte-string>)
  "Software"
end method settings-key-name;


/// Registering keys

define sealed method initialize-settings
    (settings :: <settings>, for-writing? :: <boolean>) => ()
  unless (settings-handle(settings))
    let parent = element($settings-table, settings-parent(settings));
	initialize-settings(parent, for-writing?);
    let handle  = settings-handle(parent);
    let key     = settings-key-name(settings);
    let path    = concatenate-as(<byte-string>, handle, "/", key);
    //---*** Create the directory given by 'path'
	settings-writable?(settings) := for-writing?;
    settings-handle(settings) := path
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
  //---*** What should this do?
end method unregister-key;
