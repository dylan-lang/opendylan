Module: orb-utilities
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <TICKET>
///
/// This is intended for use by invalidation mechanisms.
///
/// The producer of some information also provides a ticket
/// that consumers can store alongside their caches.
///
/// When there is new information the producer can invalidate the
/// cache by reissuing the ticket and publishing it alongside the new
/// information.

/// PROTOCOL

define constant <ticket> = <object>;

/// Checks if this ticket is valid, when compared to a know valid ticket.
define open generic valid-ticket? (ticket :: <ticket>, valid-ticket :: <ticket>)
 => (valid? :: <boolean>);

/// Returns a new valid ticket, invalidating the given old valid ticket.
define open generic reissue-ticket (ticket :: false-or(<ticket>))
 => (ticket);

/// Returns an initial valid ticket.
define open generic initial-ticket (type :: <type>)
 => (ticket);

/// Returns a known invalid ticket.
define open generic invalid-ticket (type :: <type>)
 => (ticket);

/// BOOLEAN TICKETS

define class <boolean-ticket> (<ticket>)
  sealed atomic slot %valid-ticket? :: <boolean> = #t, init-keyword: valid?:;
end class;

define sealed domain make (subclass(<boolean-ticket>));
define sealed domain initialize (<boolean-ticket>);

define method valid-ticket? (ticket :: <boolean-ticket>, valid-ticket :: <boolean-ticket>)
 => (valid? :: <boolean>)
  %valid-ticket?(ticket)
end method;

define method initial-ticket (class == <boolean-ticket>)
 => (ticket :: <boolean-ticket>)
  make(<boolean-ticket>)
end method;

define method reissue-ticket (ticket :: <boolean-ticket>)
 => (ticket :: <boolean-ticket>)
  let new-ticket = make(<boolean-ticket>);
  %valid-ticket?(ticket) := #f;
  new-ticket
end method;

define constant $invalid-ticket :: <boolean-ticket> = make(<boolean-ticket>, valid?: #f);

define method invalid-ticket (type == <boolean-ticket>)
 => (ticket :: <boolean-ticket>)
  $invalid-ticket
end method;

/// INTEGER TICKETS

define constant <integer-ticket> = <integer>;

define method valid-ticket? (ticket :: <integer-ticket>, valid-ticket :: <integer-ticket>)
 => (valid? :: <boolean>)
  ticket = valid-ticket
end method;

define method initial-ticket (type == <integer-ticket>)
 => (ticket :: <integer-ticket>)
  1
end method;

define method reissue-ticket (ticket :: <integer-ticket>)
 => (ticket :: <integer-ticket>)
  ticket + 1
end method;

define method invalid-ticket (type == <integer-ticket>)
 => (ticket :: <integer-ticket>)
  0
end method;

/*
/// Example

define class <producer> (<object>)
  slot producer-data;
  slot producer-data-ticket :: <boolean-ticket> = initial-ticket(<boolean-ticket>);
end class;

define class <consumer> (<object>)
  slot consumer-data;
  slot consumer-data-ticket :: <boolean-ticket> = invalid-ticket(<boolean-ticket>);
  slot consumer-producer;
end class;

define method produce (p :: <producer>)
  // atomically
  producer-data(p) := new-data;
  producer-data-ticket(p) := reissue-ticket(producer-data-ticket(p));
end method;

define method consume (c :: <consumer>)
  let p = consumer-producer(c);
  unless (valid-ticket?(consumer-data-ticket(c), producer-data-ticket(p)))
    /// atomically
    consumer-data(c) := producer-data(p);
    consumer-data-ticket(c) := producer-data-ticket(p);
  end unless;
end method;

*/