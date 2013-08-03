Module:       Dylan-User
Synopsis:     Standalone wrapper for DUIM-Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module standalone-deuce
  create <deuce-editor>,
         <deuce-frame>,
         start-deuce,
         ensure-deuce-started;
end module standalone-deuce;

define module standalone-deuce-internals
  use common-dylan,
    exclude: { position, position-if, format-to-string };
  use threads;
  use plists;
  use streams-internals,
    rename: { <buffer>       => streams/<buffer>,
              read-character => streams/read-character };

  use format;
  use format-out;

  use operating-system;

  //---*** For the mail sending hack
  // use sockets;
  // use smtp-client;

  use duim;
  use duim-internals,
    import: { read-event,
              event-status-code,
              update-scroll-bar };
  use win32-duim,
    import: { <win32-icon> };
  use standalone-deuce,
    export: all;
  use deuce-internals,
    rename: { // Random DUIM clashes
              line-length        => deuce/line-length,
              node-children        => deuce/node-children,
              node-children-setter => deuce/node-children-setter,
              node-parent        => deuce/node-parent,
              execute-command        => deuce/execute-command,
              \command-definer  => \deuce/command-definer,
              <command-table>        => deuce/<command-table>,
              <standard-command-table> => deuce/<standard-command-table>,
              command-table-name  => deuce/command-table-name,
              undo-command        => deuce/undo-command,
              redo-command        => deuce/redo-command,
              $control-key        => deuce/$control-key,
              $meta-key                => deuce/$meta-key,
              $super-key        => deuce/$super-key,
              $shift-key        => deuce/$shift-key,
              $left-button        => deuce/$left-button,
              $middle-button        => deuce/$middle-button,
              $right-button        => deuce/$right-button,
              // Deuce window protocol
              window-size           => deuce/window-size,
              window-viewport-size => deuce/window-viewport-size,
              update-scroll-bar    => deuce/update-scroll-bar,
              scroll-position           => deuce/scroll-position,
              set-scroll-position  => deuce/set-scroll-position,
              display-message            => deuce/display-message,
              display-error-message => deuce/display-error-message,
              display-buffer-name   => deuce/display-buffer-name,
              draw-string        => deuce/draw-string,
              string-size        => deuce/string-size,
              draw-line                => deuce/draw-line,
              draw-rectangle        => deuce/draw-rectangle,
              draw-image        => deuce/draw-image,
              clear-area        => deuce/clear-area,
              copy-area                => deuce/copy-area,
              font-metrics        => deuce/font-metrics,
              choose-from-menu          => deuce/choose-from-menu,
              choose-from-dialog  => deuce/choose-from-dialog,
              cursor-position          => deuce/cursor-position,
              set-cursor-position => deuce/set-cursor-position,
              \with-busy-cursor          => \deuce/with-busy-cursor,
              do-with-busy-cursor => deuce/do-with-busy-cursor,
              caret-position          => deuce/caret-position,
              set-caret-position  => deuce/set-caret-position,
              caret-size          => deuce/caret-size,
              set-caret-size      => deuce/set-caret-size,
              show-caret        => deuce/show-caret,
              hide-caret        => deuce/hide-caret,
              <color>                => deuce/<color>,
              color-red                => deuce/color-red,
              color-green        => deuce/color-green,
              color-blue        => deuce/color-blue,
              $black                => deuce/$black,
              $white                => deuce/$white,
              $red                => deuce/$red,
              $green                => deuce/$green,
              $blue                => deuce/$blue,
              $cyan                => deuce/$cyan,
              $magenta                => deuce/$magenta,
              $yellow                => deuce/$yellow,
              command-enabled?        => deuce/command-enabled?,
              command-enabled?-setter => deuce/command-enabled?-setter };
  use duim-deuce-internals;
end module standalone-deuce-internals;
