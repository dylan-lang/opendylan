module: richards
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A direct translation of the Cecil Richards benchmark, with no attempt
// to make it more dylan-like in efficiency or style.

// some constants

define constant $device-packet-kind = 1;
define constant $work-packet-kind = 2;

define constant $idler = 1;
define constant $worker = 2;
define constant $handlerA = 3;
define constant $handlerB = 4;
define constant $deviceA = 5;
define constant $deviceB = 6;

// global mutable state
define class <state> (<object>)
  constant slot task-table = make(<vector>, size: 6, fill: $null-task);
  slot task-list = $null-task;
  slot current-task = $null-task;
  slot current-task-identity = 0;
  slot layout = 0;
  slot queue-packet-count = 0;
  slot hold-count = 0;
end;

// control state
define variable *tracing* = #f;

define method schedule ()
  *state*.current-task := *state*.task-list;
  while (*state*.current-task ~== $null-task)
    if (task-holding-or-waiting?(*state*.current-task))
      *state*.current-task := *state*.current-task.link;
    else
      *state*.current-task-identity := *state*.current-task.ident;
      if (*tracing*)
	format-out("%s\n", *state*.current-task-identity);
      end;
      *state*.current-task := run-task(*state*.current-task);
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
				      
define abstract class <task-state> (<object>)
  // Default initializers put the task in the Waiting state
  slot packet-pending? = #f;
  slot task-waiting? = #f;
  slot task-holding? = #t;
end;

/*
define method mark-waiting (t :: <task-state>)
  t.packet-pending? := #f;
  t.task-holding? := #f;
  t.task-waiting? := #t;
end;
*/

define method mark-running (t :: <task-state>)
  t.packet-pending? := #f;
  t.task-waiting? := #f;
  t.task-holding? := #f;
end;

/*
define method mark-waiting-with-packet (t :: <task-state>)
  t.packet-pending? := #t;
  t.task-waiting? := #t;
  t.task-holding? := #f;
end;
*/

define method mark-packet-now-pending (t :: <task-state>)
  t.packet-pending? := #t;
  t.task-waiting? := #f;
  t.task-holding? := #f;
end;

define inline method task-holding-or-waiting? (t :: <task-state>)
  t.task-holding? | ( ~t.packet-pending? & t.task-waiting? )
end;

define inline method waiting-with-packet? (t :: <task-state>)
  t.packet-pending? & ( t.task-waiting? & ~t.task-holding?)
end;

define class <task-control-block> (<task-state>)
  slot link = $null-task, init-keyword: link:;
  slot ident = 0, init-keyword: ident:;
  constant slot priority = 0, init-keyword: priority:;
  slot input = $null-packet, init-keyword: input:;
end;

define method add-to-task-list (tcb :: <task-control-block>)
  tcb.link := *state*.task-list;
  *state*.task-list := tcb;
  *state*.task-table[tcb.ident - 1] := tcb;
end;

define method add-input (tcb :: <task-control-block>, packet, old-task)
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
end;

define constant $null-task = make(<task-control-block>);

define class <packet> (<object>)
  slot link = $null-packet, init-keyword: link:;
  slot ident, init-keyword: ident:;
  constant slot kind, init-keyword: kind:;
  slot datum, init-keyword: datum:;
  constant slot data, init-keyword: data:;
end;

define constant $null-packet = make(<packet>,
				    ident: -1,
				    kind: -1,
				    datum: -1,
				    data: make(<vector>, size: 4, fill: 0));

define class <device-task-rec> (<task-control-block>)
  slot pending = $null-packet, init-keyword: pending:;
end;

define method run (t :: <device-task-rec>, work)
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
end;

define class <idle-task-rec> (<task-control-block>)
  slot control, init-keyword: control:;
  slot count, init-keyword: count:;
end;

define method run (t :: <idle-task-rec>, work)
  t.count := t.count - 1;
  if (t.count == 0)
    hold-self();
  else
    if (logand(t.control, 1) == 0)
      t.control := t.control / 2;
      release($deviceA)
    else
      t.control := logxor((t.control / 2), 53256);
      release($deviceB)
    end;
  end;
end;

define class <handler-task-rec> (<task-control-block>)
  slot work-in = $null-packet;
  slot device-in = $null-packet;
end;

define method run(t :: <handler-task-rec>, work)
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
end;

define class <worker-task-rec> (<task-control-block>)
  slot count = 0, init-keyword: count:;
  slot destination, init-keyword: destination:;
end;

define method run (t :: <worker-task-rec>, work)
  if (work == $null-packet)
    wait()
  else
    t.destination
      := if ($handlerA == t.destination) $handlerB else $handlerA end;
    work.ident := t.destination;
    work.datum := 1;
    for (i from 0 to 3)
      t.count := t.count + 1;
      if (t.count > 256) t.count := 1 end;
      work.data[i] := as(<integer>, 'A') + t.count - 1;
    end;
    queue-packet(work)
  end
end;

define method create-packet (link, ident, kind)
  make(<packet>, link: link, ident: ident, kind: kind,
       datum: 1, data: make(<vector>, size: 4, fill: 0));
end;

define method create-idler (i, p, w, s)
  let id = make(<idle-task-rec>,
		ident: i, priority: p, input: w,
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?,
		control: 1,
		count: 10000);
  add-to-task-list(id);
  id
end;

define method create-worker (i, p, w, s)
  let id = make(<worker-task-rec>,
		ident: i, priority: p, input: w, 
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?,
		destination: $handlerA,
		count: 0);
  add-to-task-list(id);
  id
end;

define method create-handler (i, p, w, s)
  let id = make(<handler-task-rec>,
		ident: i, priority: p, input: w, 
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?);
  add-to-task-list(id);
  id
end;

define method create-device (i, p, w, s)
  let id = make(<device-task-rec>,
		ident: i, priority: p, input: w, 
		packet-pending?: s.packet-pending?,
		task-waiting?: s.task-waiting?,
		task-holding?: s.task-holding?);
  add-to-task-list(id);
  id
end;

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
end;

     
define method find-task (identity)
  let tk = *state*.task-table[identity - 1];
  if (tk == $null-task)
    format-out("find-task failed\n");
  end;
  tk
end;

define method hold-self ()
  *state*.hold-count := *state*.hold-count + 1;
  *state*.current-task.task-holding? := #t;
  *state*.current-task.link
end;

define method queue-packet (packet :: <packet>)
    let tk = find-task(packet.ident);
    if (tk == $null-task)
      $null-task
    else
      *state*.queue-packet-count := *state*.queue-packet-count + 1;
      packet.link := $null-packet;
      packet.ident := *state*.current-task-identity;
      add-input(tk, packet, *state*.current-task)
    end;
end;

define method release (identity)
  let tk = find-task(identity);
  if (tk == $null-task)
    $null-task
  else
    tk.task-holding? := #f;
    if (tk.priority > *state*.current-task.priority)
      tk
    else
      *state*.current-task
    end
  end;
end;

define method traceit (id)
  *state*.layout := *state*.layout - 1;
  if (*state*.layout == 0)
    format-out("\n");
    *state*.layout := 50;
  end;
  format-out("%s", id + 1);
end;

define method wait()
  *state*.current-task.task-waiting? := #t;
  *state*.current-task
end;

define method waiting-with-packet()
  make(<task-control-block>,
       packet-pending?: #t, task-waiting?: #t, task-holding?: #f);
end;

define method waiting ()
  make(<task-control-block>,
       packet-pending?: #f, task-holding?: #f, task-waiting?: #t);
end;

define method running()
  make(<task-control-block>,
       packet-pending?: #f, task-waiting?: #f, task-holding?: #f);
end;

define method work-in-add (tk :: <task-control-block>, packet)
  tk.work-in := append-head(packet, tk.work-in);
  tk
end;

define method device-in-add (tk :: <task-control-block>, packet)
  tk.device-in := append-head(packet, tk.device-in);
  tk
end;

define method append-head (packet :: <packet>, queuehead)
  packet.link := $null-packet;
  if (queuehead == $null-packet)
    packet
  else
    let mouse = queuehead;
    let mouse-link = mouse.link;
    while (mouse-link ~== $null-packet)
      mouse := mouse-link;
      mouse-link := mouse.link;
    end;
    mouse.link := packet;
    queuehead
  end;
end method;

define constant *state* = make(<state>);

define method run-richards ()
  // *tracing* := "Yy".includes(ask("Tracing? ").first);
  format-out("** Starting richards...\n");
  benchmark-closure(richards);
end;

run-richards();
