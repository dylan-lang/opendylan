Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Mailto locators


/// Mail accounts

define open class <mail-account> (<object>) end;

define variable *default-account* :: false-or(<mail-account>) = #f;

define open generic send 
    (to :: <object>, object :: <object>, #key) 
 => ();

define open generic send-using
    (account :: <mail-account>, to :: <object>, object :: <object>, #key)
 => ();

define method send 
    (to :: <object>, object :: <object>, #rest options, #key, #all-keys) => ()
  assert(*default-account*, "Default outgoing mail account details installed.");
  start-sockets();
  apply(send-using, *default-account*, to, object, options);
end method;

/// SMTP mail

define class <smtp-account> (<mail-account>)
  constant slot account-host :: <byte-string>,
    required-init-keyword: host:;
  constant slot account-port :: <integer> = 25,
    init-keyword: port:;
  constant slot account-from-address :: <byte-string>,
    required-init-keyword: from-address:;
  constant slot account-from-name :: <byte-string> = "",
    init-keyword: from-name:;
end class;

// For installing SMTP as the default outgoing mail account
define method set-smtp-defaults (#rest options) => ()
  *default-account* := apply(make, <smtp-account>, options);
end method;

define class <message> (<object>)
  constant slot message-recipients = #f,
    init-keyword: recipients:;
  constant slot message-header = #f,
    init-keyword: header:;
  constant slot message-body,
    init-keyword: body:;

  slot message-parsed-header = #f,
    init-keyword: parsed-header:;
end class;

define method message-property
    (message :: <message>, property :: <symbol>)
 => (property :: false-or(<byte-string>))
  let parsed = message-parsed-header(message);
  if (~parsed)
    parsed 
      := message-parsed-header(message)
        := read-header-from-string(message-header(message));
  end;
  element(parsed, property, default: #f);
end method;

define method as 
    (class == <message>, string :: <byte-string>) => (message :: <message>)
  let (keys, lines, chars) = read-header-from-string(string);
  make(<message>,
       header: copy-sequence(string, end: chars),
       body:   copy-sequence(string, start: chars),
       parsed-header: keys);
end method;

define method as-recipients (object :: <mail-to-locator>) => (seq :: <sequence>)
  list(locator-address(object))
end method;

define method as-recipients (object :: <sequence>) => (seq :: <sequence>)
  assert(every?(rcurry(instance?, <mail-to-locator>), object),
         "All recipients mail locators");
  object
end method;

define method as-recipients-field 
    (recipients :: <sequence>) => (field :: <byte-string>)
  assert(~empty?(recipients), "Non empty recipients");
  reduce1(method (before :: <byte-string>, after :: <byte-string>)
            concatenate(before, ", ", after);
          end,
          recipients);
end method;

define method first-line 
    (string :: <byte-string>) => (substring :: <byte-string>)
  let newline-pos = subsequence-position(string, "\n");
  if (newline-pos)
    copy-sequence(string, end: newline-pos)
  else
    string
  end;
end method;

define method as-message
    (account :: <smtp-account>, string :: <byte-string>, to :: <object>)
 => (message :: <message>)
  let recipients = as-recipients(to);
  make(<message>,
       recipients: recipients,
       header: 
         format-to-string
           ("From: %s <%s>\nTo: %s\nSubject: %s\n",
            account-from-name(account),
            account-from-address(account),
            as-recipients-field(recipients),
            first-line(string)),
       body: string);
end method;

define method send-using
    (account :: <smtp-account>, to :: <object>, object :: <object>,
       #key) 
 => ()
  let message = as-message(account, object, to);
  with-smtp-stream 
      (stream to account-host(account), port: account-port(account))
    write-smtp-from(stream, account-from-address(account));
    for (recipient in message-recipients(message))
      write-smtp-recipient(stream, recipient);
    end;
    write-smtp-data-start(stream);
    write-new-line-terminated-text(stream, message-header(message));
    new-line(stream);
    write-new-line-terminated-text(stream, message-body(message));
    write-smtp-data-end(stream);
  end;
end method;

define method write-new-line-terminated-text
    (stream :: <stream>, text :: <byte-string>) => ()
  write-text(stream, text);
  if (last(text) ~== '\n')
    new-line(stream);
  end;
end method;

// eof

