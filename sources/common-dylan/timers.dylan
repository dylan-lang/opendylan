Module:   common-dylan-internals
Synopsis: Basic timers.
Author:   Bruce Mitchener, Jr.
License:  See License.txt in this distribution for details.
Warranty: Distributed WITHOUT WARRANTY OF ANY KIND

define class <timer> (<object>)
  slot timer-running? :: <boolean> = #f;
  slot timer-started-seconds :: <machine-word>;
  slot timer-started-nanoseconds :: <machine-word>;
end class <timer>;

define method timer-start
    (timer :: <timer>)
 => ()
  timer.timer-running? := #t;
  let (sec, nsec) = %timer-current-time();
  timer.timer-started-seconds := sec;
  timer.timer-started-nanoseconds := nsec;
end;

define method timer-accumulated-time
    (timer :: <timer>)
 => (second :: <integer>, microseconds :: <integer>)
  if (timer.timer-running?)
    let (sec, nsec) = %timer-current-time();
    %timer-diff-times(timer.timer-started-seconds, timer.timer-started-nanoseconds, sec, nsec)
  else
    values(0, 0)
  end if
end;

define method timer-stop
    (timer :: <timer>)
 => (seconds :: <integer>, microseconds :: <integer>)
  if (timer.timer-running?)
    let (sec, nsec) = %timer-current-time();
    timer.timer-running? := #f;
    %timer-diff-times(timer.timer-started-seconds, timer.timer-started-nanoseconds, sec, nsec)
  else
    values(0, 0)
  end if
end;

define macro with-storage
  { with-storage (?:name, ?size:expression) ?:body end }
  => { begin
         let ?name = primitive-wrap-machine-word(integer-as-raw(0));
         block ()
           ?name := primitive-wrap-machine-word
                      (primitive-cast-pointer-as-raw
                         (%call-c-function ("malloc")
                            (nbytes :: <raw-c-unsigned-long>) => (p :: <raw-c-pointer>)
                            (integer-as-raw(?size))
                          end));
           if (primitive-machine-word-equal?
                 (primitive-unwrap-machine-word(?name), integer-as-raw(0)))
             error("unable to allocate %d bytes of storage", ?size);
           end;
           ?body
         cleanup
           if (primitive-machine-word-not-equal?
                 (primitive-unwrap-machine-word(?name), integer-as-raw(0)))
             %call-c-function ("free")
               (p :: <raw-c-pointer>) => (void :: <raw-c-void>)
                 (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(?name)))
             end;
             #f
           end
         end
       end }
end macro with-storage;

define inline function %timer-current-time
    ()
 => (secs :: <machine-word>, nsecs :: <machine-word>)
  let secs :: <machine-word> = primitive-wrap-machine-word(integer-as-raw(0));
  let nsecs :: <machine-word> = primitive-wrap-machine-word(integer-as-raw(0));
  with-storage (timeloc, 8)
    %call-c-function ("timer_get_point_in_time")
        (time :: <raw-c-pointer>)
     => (nothing :: <raw-c-void>)
      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(timeloc)))
    end;
    secs := primitive-wrap-machine-word(
              primitive-c-unsigned-int-at(primitive-unwrap-machine-word(timeloc),
                                          integer-as-raw(0),
                                          integer-as-raw(0)));
    nsecs := primitive-wrap-machine-word(
               primitive-c-unsigned-int-at(primitive-unwrap-machine-word(timeloc),
                                           integer-as-raw(1),
                                           integer-as-raw(0)));
  end with-storage;
  values(secs, nsecs)
end function;

define inline function %timer-diff-times
    (started-seconds :: <machine-word>, started-nanoseconds :: <machine-word>,
     stopped-seconds :: <machine-word>, stopped-nanoseconds :: <machine-word>)
 => (seconds :: <integer>, microseconds :: <integer>)
  let secs = 0;
  let nsecs = 0;
  with-storage (timeloc, 8)
    %call-c-function ("timer_accumulated_time")
        (starting-secs :: <raw-c-unsigned-int>,
         starting-nsecs :: <raw-c-unsigned-int>,
         stopping-secs :: <raw-c-unsigned-int>,
         stopping-nsecs :: <raw-c-unsigned-int>,
         time :: <raw-c-pointer>)
     => (nothing :: <raw-c-void>)
      (primitive-unwrap-machine-word(started-seconds),
       primitive-unwrap-machine-word(started-nanoseconds),
       primitive-unwrap-machine-word(stopped-seconds),
       primitive-unwrap-machine-word(stopped-nanoseconds),
       primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(timeloc)))
    end;
    secs := raw-as-integer(
              primitive-c-unsigned-int-at(primitive-unwrap-machine-word(timeloc),
                                          integer-as-raw(0),
                                          integer-as-raw(0)));
    nsecs := raw-as-integer(
               primitive-c-unsigned-int-at(primitive-unwrap-machine-word(timeloc),
                                           integer-as-raw(1),
                                           integer-as-raw(0)));
  end with-storage;
  values(secs, nsecs)
end;
