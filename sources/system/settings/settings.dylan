Module:    system-internals
Synopsis:  Settings and user profiles
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Basic settings protocol

define open abstract primary class <settings> (<object>)
  constant slot settings-parent = #f,
    init-keyword: parent:;
  // Implementation-defined handle for this set of settings
  // Note that we don't hang on to the handles for leaf keys
  slot settings-handle = #f;
  slot settings-writable? :: <boolean> = #f;
end class <settings>;

define generic initialize-settings
    (settings :: <settings>, for-writing? :: <boolean>) => ();

define open generic invalidate-settings-caches
    (settings :: <settings>) => ();

// Back-end functions for reading and writing a value
define generic get-value
    (settings :: <settings>, key :: <byte-string>, type :: <type>)
 => (value, found? :: <boolean>);
define generic set-value
    (value, settings :: <settings>, key :: <byte-string>, type :: <type>)
 => (success? :: <boolean>);

// Front- and back-end functions for removing a value
define open generic remove-value!
    (settings :: <settings>, key :: <symbol>) => ();
define generic do-remove-value!
    (settings :: <settings>, key :: <byte-string>) => ();

// The key name for a <settings>
define open generic settings-key-name
    (settings :: <settings>) => (key-name :: <byte-string>);
define open generic settings-key-name-setter
    (key-name :: <byte-string>, settings :: <settings>) => (key-name :: <byte-string>);
define open generic %settings-key-name-setter
    (key-name :: <byte-string>, settings :: <settings>) => (key-name :: <byte-string>);

// The key name for a single "slot" within a <settings>
define open generic make-key
    (settings :: <settings>, key :: <symbol>, for-writing? :: <boolean>)
 => (key :: <byte-string>);

define generic register-key
    (settings :: <settings>, key :: <byte-string>, for-writing? :: <boolean>)
 => (key :: <byte-string>);
define generic unregister-key
    (settings :: <settings>, key :: <byte-string>) => ();


/// Settings defining macro

define variable $settings-table :: <object-table> = make(<object-table>);

// This macro has a syntax that makes it look like the parent class is
// being subclassed, but it really isn't.  We use something more akin
// to encapsulation so that settings handles and keys can be dynamically
// computed at run-time.
define macro settings-definer
  { define settings ?:name (?parent:expression) ?slots:* end }
    => { define settings-class ?name ?slots end;
         // Define a 'make' method that ensures that there is exactly
         // one instance of each class
         define sealed method make
             (class == ?name, #rest initargs, #key) => (settings :: ?name)
           element($settings-table, class, default: #f)
           | begin
               let object = apply(?=next-method, class, parent: ?parent, initargs);
               element($settings-table, class) := object;
               object
             end
         end method make;
         define invalidate-settings-caches-method ?name
           ?slots
         end;
         // Define the methods for each of the keys in this setting
         define settings-key ?name ?slots end;
         // Define the methods for each of the slots in this setting
         define settings-slots ?name ?slots end;
         // Make the setting; it will get initialized later
         make(?name); }
end macro settings-definer;

define constant $uninitialized  = #("uninitialized");
define constant <uninitialized> = singleton($uninitialized);

define inline function initialized?   (x) ~(x == $uninitialized) end;
define inline function uninitialized? (x)   x == $uninitialized  end;

define macro uninitialized-or
  { uninitialized-or (?type:expression) }
    => { type-union(false-or(?type), <uninitialized>) }
end macro uninitialized-or;

define macro settings-class-definer
  { define settings-class ?:name ?slots:* end }
    => { define sealed class ?name (<settings>) ?slots end }
 slots:
  { } => { }
  { ?slot:*; ... } => { ?slot ... }
 slot:
  { slot ?slot-name:name :: ?type:expression }
    => { slot "%" ## ?slot-name ## "-key"     :: false-or(<byte-string>) = #f;
         slot "%" ## ?slot-name ## "-value"   /* :: uninitialized-or(?type) */ = $uninitialized;
         constant slot "%" ## ?slot-name ## "-default" /* :: uninitialized-or(?type) */ = $uninitialized; }
  { slot ?slot-name:name :: ?type:expression, key: ?key:token }
    => { slot "%" ## ?slot-name ## "-key"     :: false-or(<byte-string>) = #f;
         slot "%" ## ?slot-name ## "-value"   /* :: uninitialized-or(?type) */ = $uninitialized;
         constant slot "%" ## ?slot-name ## "-default" /* :: uninitialized-or(?type) */ = $uninitialized; }
  { slot ?slot-name:name :: ?type:expression = ?default:expression }
    => { slot "%" ## ?slot-name ## "-key"     :: false-or(<byte-string>) = #f;
         slot "%" ## ?slot-name ## "-value"   /* :: uninitialized-or(?type) */ = $uninitialized;
         constant slot "%" ## ?slot-name ## "-default" /* :: uninitialized-or(?type) */ = begin ?default end; }
  { slot ?slot-name:name :: ?type:expression = ?default:expression, key: ?key:token }
    => { slot "%" ## ?slot-name ## "-key"     :: false-or(<byte-string>) = #f;
         slot "%" ## ?slot-name ## "-value"   /* :: uninitialized-or(?type) */ = $uninitialized;
         constant slot "%" ## ?slot-name ## "-default" /* :: uninitialized-or(?type) */ = begin ?default end; }
  { key-name ?key-name:expression }
    => { constant slot settings-key-name :: <byte-string> = ?key-name; }
  { variable key-name ?key-name:expression }
    => { slot settings-key-name :: <byte-string> = ?key-name,
           setter: %settings-key-name-setter; }
end macro settings-class-definer;

define macro settings-key-definer
  { define settings-key ?:name end }
    => { }
  { define settings-key ?:name
      slot ?slot-name:name :: ?type:name; ?more-slots:*
    end }
    => { define settings-key ?name ?more-slots end; }
  { define settings-key ?:name
      slot ?slot-name:name :: ?type:name, key: ?key:token; ?more-slots:*
    end }
    => { define settings-key ?name ?more-slots end; }
  { define settings-key ?:name
      slot ?slot-name:name :: ?type:name = ?default:expression; ?more-slots:*
    end }
    => { define settings-key ?name ?more-slots end; }
  { define settings-key ?:name
      slot ?slot-name:name :: ?type:name = ?default:expression, key: ?key:token; ?more-slots:*
    end }
    => { define settings-key ?name ?more-slots end; }
  { define settings-key ?:name
      key-name ?key-name:expression; ?more-slots:*
    end }
    => { define settings-key ?name ?more-slots end; }
  { define settings-key ?:name
      variable key-name ?key-name:expression; ?more-slots:*
    end }
    => { define method settings-key-name-setter
             (key-name :: <byte-string>, _settings :: ?name) => (key-name :: <byte-string>)
           invalidate-settings-caches(_settings);
           %settings-key-name(_settings) := key-name
         end method settings-key-name-setter;
         define settings-key ?name ?more-slots end; }
end macro settings-key-definer;

define macro settings-slots-definer
  { define settings-slots ?:name end }
    => { }
  { define settings-slots ?:name
      slot ?slot-name:name :: ?type:name; ?more-slots:*
    end }
    => { define settings-slot ?name
           slot ?slot-name :: ?type, ?"slot-name";
         end;
         define settings-slots ?name ?more-slots end; }
  { define settings-slots ?:name
      slot ?slot-name:name :: ?type:name, key: #f; ?more-slots:*
    end }
    => { define settings-slot ?name
           slot ?slot-name :: ?type, "";
         end;
         define settings-slots ?name ?more-slots end; }
  { define settings-slots ?:name
      slot ?slot-name:name :: ?type:name, key: ?key:token; ?more-slots:*
    end }
    => { define settings-slot ?name
           slot ?slot-name :: ?type, ?key;
         end;
         define settings-slots ?name ?more-slots end; }
  { define settings-slots ?:name
      slot ?slot-name:name :: ?type:name = ?default:expression; ?more-slots:*
    end }
    => { define settings-slot ?name
           slot ?slot-name :: ?type, ?"slot-name";
         end;
         define settings-slots ?name ?more-slots end; }
  { define settings-slots ?:name
      slot ?slot-name:name :: ?type:name = ?default:expression, key: #f; ?more-slots:*
    end }
    => { define settings-slot ?name
           slot ?slot-name :: ?type, "";
         end;
         define settings-slots ?name ?more-slots end; }
  { define settings-slots ?:name
      slot ?slot-name:name :: ?type:name = ?default:expression, key: ?key:token; ?more-slots:*
    end }
    => { define settings-slot ?name
           slot ?slot-name :: ?type, ?key;
         end;
         define settings-slots ?name ?more-slots end; }
  { define settings-slots ?:name
      key-name ?key-name:expression; ?more-slots:*
    end }
    => { define settings-slots ?name ?more-slots end; }
  { define settings-slots ?:name
      variable key-name ?key-name:expression; ?more-slots:*
    end }
    => { define settings-slots ?name ?more-slots end; }
end macro settings-slots-definer;

define macro settings-slot-definer
  { define settings-slot ?:name
      slot ?slot-name:name :: ?type:name, ?key:token;
    end }
    => { define sealed method ?slot-name
             (_settings :: ?name) => (_value :: false-or(?type))
           // If the slot has already been read, use the cached value,
           // otherwise read it from the registry and cache it
           if (initialized?(_settings."%" ## ?slot-name ## "-value"))
             _settings."%" ## ?slot-name ## "-value"
           else
             let _key = make-key(_settings, ?#"slot-name", #f);
             let (_value, _found) = get-value(_settings, _key, ?type);
             if (_found)
               _settings."%" ## ?slot-name ## "-value" := _value;
               _value
             else
               let _value = _settings."%" ## ?slot-name ## "-default";
               initialized?(_value) & _value
             end
           end
         end method ?slot-name;
         define sealed method ?slot-name ## "-setter"
             (_value :: ?type, _settings :: ?name) => (_value :: ?type)
           // Write the new value to the registry and cache it
           let _key = make-key(_settings, ?#"slot-name", #t);
           _settings."%" ## ?slot-name ## "-value" := _value;
           set-value(_value, _settings, _key, ?type);
           _value
         end method ?slot-name ## "-setter";
         define sealed method remove-value!
             (_settings :: ?name, _slot == ?#"slot-name") => ()
           let _key = make-key(_settings, ?#"slot-name", #t);
           _settings."%" ## ?slot-name ## "-value" := $uninitialized;
           do-remove-value!(_settings, _key)
         end method remove-value!;
         define sealed method make-key
             (_settings :: ?name, _slot == ?#"slot-name", for-writing? :: <boolean>)
          => (key :: <byte-string>)
           (if (for-writing?) settings-writable?(_settings) else #t end)
             & _settings."%" ## ?slot-name ## "-key"
           | begin
               let _key = register-key(_settings, ?key, for-writing?);
               _settings."%" ## ?slot-name ## "-key" := _key;
               _key
             end
         end method make-key; }
end macro settings-slot-definer;

define macro invalidate-settings-caches-method-definer
  { define invalidate-settings-caches-method  ?:name ?slots:* end }
    => { define method invalidate-settings-caches
             (_settings :: ?name) => ()
           settings-handle(_settings) := #f;
           ?slots
         end method invalidate-settings-caches; }
 slots:
  { } => { }
  { ?slot:*; ... } => { ?slot ... }
 slot:
  { slot ?slot-name:name :: ?type:expression }
    => { _settings."%" ## ?slot-name ## "-key"   := #f;
         _settings."%" ## ?slot-name ## "-value" := $uninitialized; }
  { slot ?slot-name:name :: ?type:expression, key: ?key:token }
    => { _settings."%" ## ?slot-name ## "-key"   := #f;
         _settings."%" ## ?slot-name ## "-value" := $uninitialized; }
  { slot ?slot-name:name :: ?type:expression = ?default:expression }
    => { _settings."%" ## ?slot-name ## "-key"   := #f;
         _settings."%" ## ?slot-name ## "-value" := $uninitialized; }
  { slot ?slot-name:name :: ?type:expression = ?default:expression, key: ?key:token }
    => { _settings."%" ## ?slot-name ## "-key"   := #f;
         _settings."%" ## ?slot-name ## "-value" := $uninitialized; }
  { key-name ?key-name:expression }
    => { }
  { variable key-name ?key-name:expression }
    => { }
end macro invalidate-settings-caches-method-definer;


/// Standard settings
/// NB: for all standard settings, key names are supplied by the back-end

// The settings for the entire system
define settings <system-settings> (<settings>)
end settings <system-settings>;


// The settings for the entire site
define settings <site-settings> (<settings>)
end settings <site-settings>;

define settings <site-software-settings> (<site-settings>)
end settings <site-software-settings>;


// The settings for this machine
define settings <local-settings> (<settings>)
end settings <local-settings>;

define settings <local-software-settings> (<local-settings>)
end settings <local-software-settings>;

define settings <local-hardware-settings> (<local-settings>)
end settings <local-hardware-settings>;


// The default settings for all users
define settings <default-user-settings> (<settings>)
end settings <default-user-settings>;

define settings <default-user-software-settings> (<default-user-settings>)
end settings <default-user-software-settings>;


// The settings for the current user
define settings <current-user-settings> (<settings>)
end settings <current-user-settings>;

define settings <current-user-software-settings> (<current-user-settings>)
end settings <current-user-software-settings>;

