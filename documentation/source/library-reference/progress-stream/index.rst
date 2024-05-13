***************************
The progress-stream Library
***************************

.. current-library:: progress-stream
.. current-module:: progress-stream

Many text-mode programs that perform long-running, non-interactive
tasks (such as compilers) provide a running indication of what
fraction of the task is complete using a bar graph drawn using text
characters. This library implements a wrapper stream that interleaves
the progress bar graph with any status output. When output is not
directed to a console then progress updates are ignored.

.. class:: <progress-stream>
   :abstract:
   :instantiable:

   :superclasses: :class:`<wrapper-stream>`

   :keyword force?: If true, forces the wrapped stream to be treated as
                    a console, allowing textual progress bar output.

   :keyword: bar-width: An instance of :drm:`<integer>` representing
             the number of bar characters to be included in textual
             progress bar output. Defaults to 40.

   :keyword: line-width: An instance of :drm:`<integer>` representing
             the total line width (including both progress bar and label)
             in characters. Defaults to 79.

.. generic-function:: show-progress

   :signature: show-progress (stream position range #key label) => ()

   :parameter stream: An instance of :class:`<progress-stream>`.
   :parameter position: An instance of :drm:`<integer>`.
   :parameter range: An instance of :drm:`<integer>`.
   :parameter #key label: An instance of :drm:`<object>`.

   :description:

      Graphically displays the current progress, proportional to
      *position* divided by *range*, to the given *stream*. In the
      case where *stream* is a console (i.e., unredirected
      :var:`*standard-output*`), :gf:`show-progress` will draw a
      textual progress bar with overall width configured in the
      :class:`<progress-stream>` instance.

.. generic-function:: stream-supports-show-progress?

   :signature: stream-supports-show-progress? (stream) => (supports-show-progress?)

   :parameter stream: An instance of :class:`<stream>`.
   :value supports-show-progress?: An instance of :drm:`<boolean>`.

   :description:

      Returns a true value if the *stream* argument can usefully
      support the :gf:`show-progress` operation.
