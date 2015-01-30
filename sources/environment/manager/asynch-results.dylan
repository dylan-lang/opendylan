Module:    asynchronous-results-implementation
Synopsis:  Environment Manager
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// -=- ID POOL -=- (internal)
///
/// This section implements a pool of unique IDs.  There are operations
/// to obtain an ID from the pool and to release one back to the pool.
/// (We could use the Strategy pattern here, to allow different ways
/// of getting IDs, e.g., using GUIDs under Win32.)
///
/// The pool is of finite size, so an attempt to obtain an ID when
/// all are in use will block until one becomes available.

/// *NEXT-ID*, $IDS-IN-USE (internal)
///
/// These variables keep track of which ID should be allocated next and
/// which have been allocated.  The *NEXT-ID* is not guaranteed to be
/// available, though it is likely to be (if IDs are returned to the
/// pool "often enough").  If it is claimed, the ID allocation code
/// will scan for the next unclaimed ID and assign that instead (and
/// update *NEXT-ID* accordingly).

define variable *next-id* :: <integer> = 0;
define constant $ids-in-use :: <table> = make(<table>);

/// $ID-POOL-LOCK (internal)
///
/// This lock is used to synchronise access to the pool of IDs.

define constant $id-pool-lock :: <simple-lock> = make(<simple-lock>);

/// $OBTAIN-ID-RETRY-INTERVAL
///
/// The number of seconds to wait before trying again to obtain an ID
/// from the pool, if the all IDs are in use.

define constant $OBTAIN-ID-RETRY-INTERVAL :: <integer> = 3;

/// OBTAIN-ID (internal)
///
/// Returns the next free ID, updating *NEXT-ID* to the next one after that.

define function obtain-id () => (id-string :: false-or(<string>))
  local method id-free? (id :: <integer>) => (free? :: <boolean>)
    ~element($ids-in-use, id, default: #f)
  end method;
  local method following-id (id :: <integer>) => (id+1 :: <integer>)
    if (id = $maximum-integer) 0 else id + 1 end
  end method;
  with-lock ($id-pool-lock)
    let free-id :: <integer>
      = if (id-free?(*next-id*))
          // Just use the *next-id*.
          *next-id*
        else
          // Scan all IDs after *next-id*, until we hit a free one or we get
          // all the way back round to *next-id*.  In the latter case, wait
          // for a while, then try again;
          let free-id = #f;
          until (free-id)
            for (id = following-id(*next-id*) then following-id(id),
                 until: (id-free?(id) | (id = *next-id*)))
            finally
              if (id = *next-id*)
                release($id-pool-lock);
                sleep($obtain-id-retry-interval);
                wait-for($id-pool-lock);
              else
                free-id := id
              end;
            end;
          end;
          free-id
        end;
    *next-id* := following-id(free-id);
    $ids-in-use[free-id] := #t;
    integer-to-string(free-id);
  end;
end function obtain-id;

/// RELEASE-ID (internal)

define function release-id (id-string :: <string>) => ()
  with-lock ($id-pool-lock)
    let id = string-to-integer(id-string, default: #f);
    when (id)
      //--- Maybe we should signal a condition for unconvertable strings?
      //--- OTOH, the only callers of this will already have ensured that
      //--- only valid IDs are passed in here.
      remove-key!($ids-in-use, id);
    end;
  end;
end function release-id;

/// -=- TOKEN CLASS -=-
///
/// Results protocol:
///   function get-results-id () => (id :: <string>)
///   function wait-for-results (id :: <string>) => (results :: <sequence>);
///    // May signal <serious-condition> subclass <timeout-awaiting-results>
///    //   with restarts <keep-waiting> and <assume-results>(results);
///    // or signal <error> subclass <invalid-results-id>
///    //   with no restarts.
///   function provide-results (id :: <string>, results :: <sequence>) => ();
///    // May signal <error> subclass <invalid-results-id>
///    //   with no restarts.
/// end protocol <<results-token>>;

/// <SIMPLE-CONDITION-WITH-ID> (internal)

define sealed abstract class <simple-condition-with-id> (<simple-condition>)
  required keyword id:;
end class <simple-condition-with-id>;

define sealed method make
    (class :: subclass(<simple-condition-with-id>),
     #rest initargs, #key id, #all-keys)
 => (condition :: <simple-condition-with-id>)
  apply(next-method, class, format-arguments: vector(id), initargs)
end method make;

/// <INVALID-RESULTS-ID> (asynchronous-results)

define sealed class <invalid-results-id> (<error>, <simple-condition-with-id>)
  keyword format-string: = "Attempted to wait for results on invalid ID %s";
end class <invalid-results-id>;

/// <TIMEOUT-AWAITING-RESULTS> (asynchronous-results)

define sealed class <timeout-awaiting-results>
    (<serious-condition>, <simple-condition-with-id>)
  keyword format-string: = "Timed out while waiting for results on ID %s";
end class <timeout-awaiting-results>;

/// <KEEP-WAITING> (asynchronous-results)

define sealed class <keep-waiting> (<simple-restart>)
  keyword format-string: = "Keep waiting for results";
end class <keep-waiting>;

/// <ASSUME-RESULTS> (asynchronous-results)

define sealed class <assume-results> (<simple-restart>)
  sealed constant slot results-to-assume :: false-or(<sequence>),
    required-init-keyword: assume:;
  keyword format-string: = "Assume results %=";
end class <assume-results>;

define sealed method make
    (class == <assume-results>,
     #rest initargs, #key assume, #all-keys)
 => (assume-results :: <assume-results>)
  apply(next-method, class, format-arguments: vector(assume), initargs)
end method make;

/// <RESULTS-TOKEN> (internal)

define sealed class <results-token> (<object>)
  sealed constant slot token-id :: <string> = obtain-id();
  sealed slot token-results :: false-or(<sequence>),
    init-value: #f;
  sealed constant slot token-notification :: <notification>
    = make(<notification>, lock: make(<lock>));
end class <results-token>;

/// $TOKEN-TABLE (internal)
///
/// This table maps TOKEN-IDs to <results-token>s.

define constant $token-table :: <table> = make(<table>);

/// INITIALIZE (dylan)

define sealed method initialize
    (token :: <results-token>, #key, #all-keys)
  next-method();
  $token-table[token.token-id] := token;
end method initialize;

/// GET-RESULTS-ID (asynchronous-results)

define function get-results-id () => (id :: <string>)
  make(<results-token>).token-id
end function get-results-id;

/// ID-RESULTS-TOKEN (internal)

define function id-results-token
    (id :: <string>, #key error? :: <boolean> = #t)
 => (token :: false-or(type-union(<results-token>, singleton(#"aborted"))))
  let token = element($token-table, id, default: #f);
  if (token)
    token
  else
    when (error?)
      error(make(<invalid-results-id>, id: id));
    end;
  end;
end function id-results-token;

/// WAIT-FOR-RESULTS (asynchronous-results)

define function wait-for-results
    (id :: <string>,
     #key timeout :: false-or(<integer>) = #f)
 => (results :: false-or(<sequence>))
  let token = id-results-token(id);
  let notification = token.token-notification;
  let results = #f;
  let got-notification? = #f;
  with-lock (notification.associated-lock)
    block ()
      unless (token.token-results)
        until (got-notification?)
          got-notification? := wait-for(notification, timeout: timeout);
          unless (got-notification?)
            block ()
              error(make(<timeout-awaiting-results>, id: id));
            exception (<keep-waiting>)
              // Okay, go back round the "until".
            end;
          end;
        end;
      end;
      results := token.token-results;
    cleanup
      when (got-notification?)
        abort-results(id);
        // And now the notification and lock will just be GC'd.
      end;
    exception (assumption :: <assume-results>)
      results := assumption.results-to-assume;
    end;
  end;
end function wait-for-results;

/// PROVIDE-RESULTS (asynchronous-results)

define function provide-results
    (id :: <string>, results :: <sequence>)
 => ()
  let token = id-results-token(id, error?: #f);
  when (token)
    let notification = token.token-notification;
    with-lock (notification.associated-lock)
      token.token-results := results;
      release-all(notification);
    end;
  end;
end function provide-results;

/// ABORT-RESULTS (asynchronous-results)

define function abort-results (id :: <string>) => ()
  remove-key!($token-table, id);
  release-id(id);
end function abort-results;

/// Utility macros

/// WITH-ASYNCHRONOUS-RESULTS (asynchronous-results)

define macro with-asynchronous-results
  { with-asynchronous-results
        (?:name, #key ?abort-on-condition?:expression = #t,
                      ?timeout:expression = #f)
      ?:body
    end }
 => { let _id = get-results-id();
      let ?name = _id;
      block ()
        ?body;
        wait-for-results(_id, timeout: ?timeout)
      cleanup
        when (?abort-on-condition?)
          abort-results(_id);
        end;
      end }
end macro with-asynchronous-results;
