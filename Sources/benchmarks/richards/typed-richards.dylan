module: richards
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A direct translation of the Cecil Richards benchmark, with added type information
// but no other attempts to make it more dylan-like in style.

// some constants

define constant $device-packet-kind :: <integer> = 1;
define constant $work-packet-kind   :: <integer> = 2;

define constant $idler    :: <integer> = 1;
define constant $worker   :: <integer> = 2;
define constant $handlerA :: <integer> = 3;
define constant $handlerB :: <integer> = 4;
define constant $deviceA  :: <integer> = 5;
define constant $deviceB  :: <integer> = 6;

// global mutable state
define sealed class <state> (<object>)
  sealed constant slot task-table :: <simple-object-vector>
    = make(<vector>, size: 6, fill: $null-task);
  sealed slot task-list    :: <task-control-block> = $null-task;
  sealed slot current-task :: <task-control-block> = $null-task;
  sealed slot current-task-identity :: <integer> = 0;
  sealed slot layout                :: <integer> = 0;
  sealed slot queue-packet-count    :: <integer> = 0;
  sealed slot hold-count            :: <integer> = 0;
end class;

define sealed domain make (singleton(<state>));
define sealed domain initialize (<state>);


// control state
define variable *tracing* :: <boolean> = #f;

define method schedule ()
  let state :: <state> = *state*;
  state.current-task := state.task-list;
  while (state.current-task ~== $null-task)
    if (task-holding-or-waiting?(state.current-task))
      state.current-task := state.current-task.link;
    else
      state.current-task-identity := state.current-task.ident;
      if (*tracing*)
	format-out("%s\n", state.current-task-identity);
      end;
      state.current-task := run-task(state.current-task);
    end;
  end;
end method;

define method run-task (tcb :: <task-control-block>)
  let message = if (waiting-with-packet?(tcb))
		  let message = tcb.input;
		  tcb.input := message.link;
		  if (tcb.input == $null-packet)
		    mark-running(tcb);
		  else
		    mark-packet-now-pending(tcb);
		  end;
		  message
		else
		  $null-packet;
		end;
  run(tcb, message)
end method;
				      
define abstract primary class <task-state> (<object>)
  // Default initializers put the task in the Waiting state
  sealed slot packet-pending? :: <boolean> = #f;
  sealed slot task-waiting?   :: <boolean> = #f;
  sealed slot task-holding?   :: <boolean> = #t;
end class;

/*
define method mark-waiting (t :: <task-state>)
  t.packet-pending? := #f;
  t.task-holding?   := #f;
  t.task-waiting?   := #t;
end method method;
*/

define method mark-running (t :: <task-state>)
  t.packet-pending? := #f;
  t.task-waiting?   := #f;
  t.task-holding?   := #f;
end method;

/*
define method mark-waiting-with-packet (t :: <task-state>)
  t.packet-pending? := #t;
  t.task-waiting?   := #t;
  t.task-holding?   := #f;
end method;
*/

define method mark-packet-now-pending (t :: <task-state>)
  t.packet-pending? := #t;
  t.task-waiting?   := #f;
  t.task-holding?   := #f;
end method;

define inline method task-holding-or-waiting? (t :: <task-state>)
  t.task-holding? | ( ~t.packet-pending? & t.task-waiting? )
end method;

define inline method waiting-with-packet? (t :: <task-state>)
  t.packet-pending? & ( t.task-waiting? & ~t.task-holding?)
end method;

define sealed class <task-control-block> (<task-state>)
  sealed slot link /* :: <task-control-block> */ = $null-task,
    init-keyword: link:;
  sealed slot ident :: <integer> = 0,
    init-keyword: ident:;
  sealed constant slot priority = 0,
    init-keyword: priority:;
  sealed slot input :: <packet> = $null-packet,
    init-keyword: input:;
end class;

define sealed domain make (singleton(<task-control-block>));
define sealed domain initialize (<task-control-block>);

define method add-to-task-list (tcb :: <task-control-block>)
  let state :: <state> = *state*;
  tcb.link := state.task-list;
  state.task-list := tcb;
  state.task-table[tcb.ident - 1] := tcb;
end method;

define method add-input
    (tcb :: <task-control-block>, packet :: <packet>, old-task :: <task-state>)
 => (tcb :: <task-control-block>)
  if (tcb.input == $null-packet)
    tcb.input := packet;
    tcb.packet-pending? := #t;
    if (tcb.priority > old-task.priority)
      tcb
    else
      old-task
    end;
  else
    tcb.input := append-head(packet, tcb.input);
    old-task
  end;
end method;

define sealed class <packet> (<object>)
  sealed slot link /* :: <packet> */ = $null-packet,
    init-keyword: link:;
  sealed slot ident :: <integer> = 0,
    init-keyword: ident:;
  sealed constant slot kind :: <integer>,
    required-init-keyword: kind:;
  sealed slot datum :: <integer>,
    required-init-keyword: datum:;
  sealed constant slot data :: <vector>,
    required-init-keyword: data:;
end class;

define sealed domain make (singleton(<packet>));
define sealed domain initialize (<packet>);

define constant $null-packet /* :: <packet> */
  = make(<packet>,
	 ident: -1, kind: -1, datum: -1,
	 data: make(<vector>, size: 4, fill: 0));

define constant $null-task /* :: <task-control-block> */
  = make(<task-control-block>);

define sealed class <device-task-rec> (<task-control-block>)
  sealed slot pending :: <packet> = $null-packet,
    init-keyword: pending:;
end class;

define sealed domain make (singleton(<device-task-rec>));
define sealed domain initialize (<device-task-rec>);

define method run (t :: <device-task-rec>, work :: <packet>)
  if (work == $null-packet)
    let function-work = t.pending;
    if (function-work == $null-packet)
      wait()
    else
      t.pending := $null-packet;
      queue-packet(function-work);
    end;
  else
    t.pending := work;
    if (*tracing*) traceit(work.datum) end;
    hold-self();
  end;
end method;

define sealed class <idle-task-rec> (<task-control-block>)
  sealed slot control :: <integer> = 0,
    init-keyword: control:;
  sealed slot count :: <integer> = 0,
    init-keyword: count:;
end class;

define sealed domain make (singleton(<idle-task-rec>));
define sealed domain initialize (<idle-task-rec>);

define method run (t :: <idle-task-rec>, work :: <packet>)
  t.count := t.count - 1;
  if (t.count = 0)
    hold-self();
  else
    if (logand(t.control, 1) = 0)
      t.control := truncate/(t.control, 2);
      release($deviceA)
    else
      t.control := logxor(truncate/(t.control, 2), 53256);
      release($deviceB)
    end;
  end;
end method;

define sealed class <handler-task-rec> (<task-control-block>)
  sealed slot work-in   :: <packet> = $null-packet;
  sealed slot device-in :: <packet> = $null-packet;
end class;

define sealed domain make (singleton(<handler-task-rec>));
define sealed domain initialize (<handler-task-rec>);

define method run (t :: <handler-task-rec>, work :: <packet>)
  unless (work == $null-packet)
    if ($work-packet-kind == work.kind)
      work-in-add(t, work);
    else
      device-in-add(t, work);
    end;
  end;
  let work-packet = t.work-in;
  if (work-packet == $null-packet)
    wait()
  else
    let count = work-packet.datum;
    if (count > 4)
      t.work-in :=  work-packet.link;
      queue-packet(work-packet);
    else
      let device-packet = t.device-in;
      if (device-packet == $null-packet)
	wait()
      else
	t.device-in := device-packet.link;
	device-packet.datum := work-packet.data[count - 1];
	work-packet.datum := count + 1;
	queue-packet(device-packet)
      end;
    end;
  end;
end method;

define sealed class <worker-task-rec> (<task-control-block>)
  sealed slot count :: <integer> = 0,
    init-keyword: count:;
  sealed slot destination :: <integer> = 0,
    init-keyword: destination:;
end class;

define sealed domain make (singleton(<worker-task-rec>));
define sealed domain initialize (<worker-task-rec>);

define method run (t :: <worker-task-rec>, work :: <packet>)
  if (work == $null-packet)
    wait()
  else
    t.destination
      := if ($handlerA == t.destination) $handlerB else $handlerA end;
    work.ident := t.destination;
    work.datum := 1;
    for (i :: <integer> from 0 to 3)
      t.count := t.count + 1;
      if (t.count > 256) t.count := 1 end;
      work.data[i] := as(<integer>, 'A') + t.count - 1;
    end;
    queue-packet(work)
  end
end method;

define method create-packet (link, ident, kind) => (packet :: <packet>)
  make(<packet>,
       link: link, ident: ident, kind: kind, datum: 1,
       data: make(<vector>, size: 4, fill: 0));
end method;

define method create-idler (i, p, w, s) => (idler :: <idle-task-rec>)
  let id = make(<idle-task-rec>,
		ident: i, priority: p, input: w,
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?,
		control: 1,
		count: 10000);
  add-to-task-list(id);
  id
end method;

define method create-worker (i, p, w, s) => (worker :: <worker-task-rec>)
  let id = make(<worker-task-rec>,
		ident: i, priority: p, input: w, 
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?,
		destination: $handlerA,
		count: 0);
  add-to-task-list(id);
  id
end method;

define method create-handler (i, p, w, s) => (handler :: <handler-task-rec>)
  let id = make(<handler-task-rec>,
		ident: i, priority: p, input: w, 
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?);
  add-to-task-list(id);
  id
end method;

define method create-device (i, p, w, s) => (device :: <device-task-rec>)
  let id = make(<device-task-rec>,
		ident: i, priority: p, input: w, 
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?);
  add-to-task-list(id);
  id
end method;

define method richards ()
  *state*.queue-packet-count := 0;
  *state*.hold-count := 0;

  create-idler($idler, 0, $null-packet, running());

  let work-q = create-packet($null-packet, $worker, $work-packet-kind);
  work-q := create-packet(work-q, $worker, $work-packet-kind);
  create-worker($worker, 1000, work-q, waiting-with-packet());

  work-q := create-packet($null-packet, $deviceA, $device-packet-kind);
  work-q := create-packet(work-q, $deviceA, $device-packet-kind);
  work-q := create-packet(work-q, $deviceA, $device-packet-kind);
  create-handler($handlerA, 2000, work-q, waiting-with-packet());

  work-q := create-packet($null-packet, $deviceB, $device-packet-kind);
  work-q := create-packet(work-q, $deviceB, $device-packet-kind);
  work-q := create-packet(work-q, $deviceB, $device-packet-kind);
  create-handler($handlerB, 3000, work-q, waiting-with-packet());

  create-device($deviceA, 4000, $null-packet, waiting());
  create-device($deviceB, 5000, $null-packet, waiting());

  schedule();

  if (*state*.queue-packet-count == 23246 & *state*.hold-count == 9297)
    format-out("hold count = %s, queue packet count = %s\n"
	       "Richards results incorrect\n",
	       *state*.hold-count, *state*.queue-packet-count);
  elseif (*tracing*)
    format-out("Richards completed correctly\n");
  end;
end method;

     
define method find-task (identity :: <integer>) => (task :: <task-control-block>)
  let tk = *state*.task-table[identity - 1];
  if (tk == $null-task)
    format-out("find-task failed\n");
  end;
  tk
end method;

define method hold-self () => (tcb :: <task-control-block>)
  let state = *state*;
  state.hold-count := state.hold-count + 1;
  state.current-task.task-holding? := #t;
  state.current-task.link
end method;

define method queue-packet (packet :: <packet>) => (tcb :: <task-control-block>)
  let tk = find-task(packet.ident);
  if (tk == $null-task)
    $null-task
  else
    let state = *state*;
    state.queue-packet-count := state.queue-packet-count + 1;
    packet.link := $null-packet;
    packet.ident := state.current-task-identity;
    add-input(tk, packet, state.current-task)
  end;
end method;

define method release (identity :: <integer>) => (tcb :: <task-control-block>)
  let tk = find-task(identity);
  if (tk == $null-task)
    $null-task
  else
    let state = *state*;
    tk.task-holding? := #f;
    if (tk.priority > state.current-task.priority)
      tk
    else
      state.current-task
    end
  end;
end method;

define method traceit (id :: <integer>)
  let state = *state*;
  state.layout := state.layout - 1;
  if (state.layout == 0)
    format-out("\n");
    state.layout := 50;
  end;
  format-out("%s", id + 1);
end method;

define method wait ()
  let state = *state*;
  state.current-task.task-waiting? := #t;
  state.current-task
end method;

define inline method waiting-with-packet () => (tcb :: <task-control-block>)
  make(<task-control-block>,
       packet-pending?: #t, task-waiting?: #t, task-holding?: #f);
end method;

define inline method waiting () => (tcb :: <task-control-block>)
  make(<task-control-block>,
       packet-pending?: #f, task-holding?: #f, task-waiting?: #t);
end method;

define inline method running () => (tcb :: <task-control-block>)
  make(<task-control-block>,
       packet-pending?: #f, task-waiting?: #f, task-holding?: #f);
end method;

define inline method work-in-add (tk :: <task-control-block>, packet :: <packet>)
  tk.work-in := append-head(packet, tk.work-in);
  tk
end method;

define inline method device-in-add (tk :: <task-control-block>, packet :: <packet>)
  tk.device-in := append-head(packet, tk.device-in);
  tk
end method;

define method append-head (packet :: <packet>, queuehead :: <packet>)
  packet.link := $null-packet;
  if (queuehead == $null-packet)
    packet
  else
    let mouse :: <packet> = queuehead;
    let mouse-link = mouse.link;
    while (mouse-link ~== $null-packet)
      mouse := mouse-link;
      mouse-link := mouse.link;
    end;
    mouse.link := packet;
    queuehead
  end;
end method;

define constant *state* :: <state> = make(<state>);

define method run-richards ()
  // *tracing* := "Yy".includes(ask("Tracing? ").first);
  format-out("** Starting richards...\n");
  benchmark-closure(richards);
end;

run-richards();
