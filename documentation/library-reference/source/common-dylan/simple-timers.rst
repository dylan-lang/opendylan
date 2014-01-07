************************
The simple-timers Module
************************

.. current-library:: common-dylan
.. current-module:: simple-timers

Common Dylan provides a simple facility for timers to track intervals
of time via the *simple-timers* module.

Timers offer microsecond resolution on all supported platforms. Timers
attempt to be monotonic where that capability is supported by the operating
system.

.. class:: <profiling-timer>

   The timer class.

   :superclasses: <object>

   :description:

     The timer class. Timers start out stopped and must be started
     with :gf:`timer-start`.

.. generic-function:: timer-start

   Starts a timer.

   :signature: timer-start timer => ()

   :parameter timer: An instance of :class:`<profiling-timer>`.

   See also:

   * :gf:`timer-stop`

.. generic-function:: timer-stop

   Stops a timer and returns the elapsed time.

   :signature: timer-stop timer => (seconds, microseconds)

   :parameter timer: An instance of :class:`<profiling-timer>`.
   :value seconds: An instance of :drm:`<integer>`.
   :value microseconds: An instance of :drm:`<integer>`.

   See also:

   * :gf:`timer-start`

.. generic-function:: timer-accumulated-time

   Returns the time since the timer was started.

   :signature: timer-accumulated-time timer => (seconds, microseconds)

   :parameter timer: An instance of :class:`<profiling-timer>`.
   :value seconds: An instance of :drm:`<integer>`.
   :value microseconds: An instance of :drm:`<integer>`.

.. generic-function:: timer-running?

   Returns true if the timer is running.

   :signature: timer-running? timer => (running?)

   :parameter timer: An instance of :class:`<profiling-timer>`.
   :value running?: An instance of :drm:`<boolean>`.
