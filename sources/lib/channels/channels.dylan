Module:       channels
Synopsis:     broadcast-calling library
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// CHANNELS
//
// ---*** ordering of calls to receivers in broadcasts?
// ---*** multi-threading for calls to receivers in broadcasts?
// ---*** use add-method to dynamically build gf for piggybacking on Dylan dispatching?
// 


/// <CHANNEL-OBJECT> (internal)

define class <channel-object> (<object>)
  constant slot channel-message? = unsupplied(), 
    init-keyword: message?:;
  constant slot channel-receiver? = unsupplied(), 
    init-keyword: receiver?:;
  constant slot channel-callback :: false-or(<function>) = #f,
    init-keyword: callback:;
end class; 


/// <CHANNEL> (channels)

define open class <channel> (<channel-object>)
  constant slot channel-lock :: <lock> = make(<lock>);
  constant slot channel-receivers :: <table> = make(<table>), 
    init-keyword: receivers:;
  constant slot channel-mapper :: <function> = do, 
    init-keyword: mapper:;
end class;


/// <RECEIVER> (internal)

define sealed class <receiver> (<channel-object>)
  sealed slot receiver-key :: <object>, required-init-keyword: key:;
  sealed slot receiver-message-type :: <type> = <object>, init-keyword: message-type:;
end class;


/// PROTOCOLS

define open generic broadcast (channel :: <channel>, message :: <object>, #rest args);

define open generic override-channel
    (channel :: <channel>, #rest args, #key message?, receiver?, callback, mapper) => (new-channel :: <channel>);

define open generic tune-in (channel :: <channel>, receiver-key :: <object>, #key, #all-keys) => ();

define open generic tune-out (channel :: <channel>, receiver-key :: <object>) => ();


/// OVERRIDE-CHANNEL (channels)

define method override-channel
    (channel :: <channel>, #rest args,
     #key message? = unsupplied(),
     receiver? = unsupplied(),
     callback = unsupplied(),
     mapper = #f)
 => (new-channel :: <channel>)
  let override :: <channel> = make(<channel>,
				   message?: any-supplied?(message?, channel.channel-message?),
				   receiver?: any-supplied?(receiver?, channel.channel-receiver?),
				   callback: any-supplied?(callback, channel.channel-callback),
				   mapper: mapper | channel.channel-mapper,
				   receivers: channel.channel-receivers);
  override
end method;


/// BROADCAST (channels)

define method broadcast (channel :: <channel>, message :: <object>, #rest args)
  local method do-receiver (receiver :: <receiver>)
	  if (instance?(message, receiver.receiver-message-type))
	    let callback :: <function> = receiver.channel-callback | channel.channel-callback | curry(error, "Missing channel callback");
	    let message? :: <boolean> = any-supplied?(receiver.channel-message?, channel.channel-message?, #t);
	    let receiver? :: <boolean> = any-supplied?(receiver.channel-receiver?, channel.channel-receiver?, #f);
	    let extra :: <list> = #();
	    if (receiver?) extra := add!(extra, receiver.receiver-key) end if;
	    if (message?) extra := add!(extra, message) end if;
	    apply(callback, concatenate(extra, args));
	  end if;
	end method;
  let lock = channel.channel-lock;
  if (owned?(lock))
    make(<thread>,
         name: "Re-entrant broadcast",
         function: method ()
		     apply(broadcast, channel, message, args)
		   end)
  else
    with-lock (lock)
      channel.channel-mapper(do-receiver, channel.channel-receivers);
    end
  end
end method;


/// TUNE-IN (channels)

define method tune-in (channel :: <channel>,
                       receiver-key :: <object>,
                       #rest args, #key callback, #all-keys)
 => ()
  let receiver :: <receiver> =
    if (instance?(receiver-key, <function>) & callback = #f)
      apply(make, <receiver>, key: receiver-key, callback: receiver-key, args);
    else
      apply(make, <receiver>, key: receiver-key, args);
    end if;
  let lock = channel.channel-lock;
  if (owned?(lock))
    make(<thread>,
         name: "Re-entrant tune-in",
         function: method ()
		     apply(tune-in, channel, receiver-key, args)
		   end)
  else
    with-lock (lock)
      channel.channel-receivers[receiver-key] := receiver;
    end
  end
end method;


/// TUNE-OUT (channels)

define method tune-out (channel :: <channel>, receiver-key :: <object>)
 => ()
  let lock = channel.channel-lock;
  if (owned?(lock))
    make(<thread>,
         name: "Re-entrant tune-out",
         function: method ()
		     tune-out(channel, receiver-key)
		   end)
  else
    with-lock (lock)
      remove-key!(channel.channel-receivers, receiver-key);
    end
  end
end method;


/// ANY-SUPPLIED? (internal)

define method any-supplied? (#rest values)
  block (return)
    for (v in values)
      if (supplied?(v))
	return(v);
      end if;
    end for;
    return(#f);
  end block;
end method;
