Module:    environment-tools
Synopsis:  Environment Tools
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Image constants

//---*** hughg, 1998/11/02: This one really belongs in DUIM, but andrewa
// agrees this'll do for now (for the playground dialog).
define variable $check-bitmap :: <label-type> = "X";

define variable $splash-screen-bitmap :: <label-type> = #f;
define variable $about-box-bitmap     :: <label-type> = #f;

define variable $product-large-bitmap      :: <label-type> = #f;
define variable $tutorial-large-bitmap     :: <label-type> = "Tutorial Icon";
define variable $examples-large-bitmap     :: <label-type> = "Examples Icon";
define variable $playground-large-bitmap   :: <label-type> = "Playground Icon";
define variable $dylan-file-large-bitmap   :: <label-type> = "Dylan Icon";
define variable $project-file-large-bitmap :: <label-type> = "Project Icon";
define variable $text-file-large-bitmap    :: <label-type> = "Text Icon";
define variable $open-large-bitmap         :: <label-type> = "Open Icon";

define variable $application-bitmap      :: <label-type> = #f;
define variable $class-bitmap            :: <label-type> = #f;
define variable $constant-bitmap         :: <label-type> = #f;
define variable $default-bitmap          :: <label-type> = #f;
define variable $definition-bitmap       :: <label-type> = #f;
define variable $dylan-file-bitmap       :: <label-type> = #f;
define variable $canonical-source-bitmap :: <label-type> = #f;
define variable $current-source-bitmap   :: <label-type> = #f;
define variable $error-bitmap            :: <label-type> = #f;
define variable $foreign-bitmap          :: <label-type> = #f;
define variable $function-bitmap         :: <label-type> = #f;
define variable $generic-bitmap          :: <label-type> = #f;
define variable $library-bitmap          :: <label-type> = #f;
define variable $macro-bitmap            :: <label-type> = #f;
define variable $method-bitmap           :: <label-type> = #f;
define variable $module-bitmap           :: <label-type> = #f;
define variable $object-bitmap           :: <label-type> = #f;
define variable $project-bitmap          :: <label-type> = #f;
define variable $restart-bitmap          :: <label-type> = #f;
define variable $serious-warning-bitmap  :: <label-type> = #f;
define variable $slot-bitmap             :: <label-type> = #f;
define variable $stack-frame-bitmap      :: <label-type> = #f;
define variable $text-file-bitmap        :: <label-type> = #f;
define variable $threads-bitmap          :: <label-type> = #f;
define variable $unbound-bitmap          :: <label-type> = #f;
define variable $variable-bitmap         :: <label-type> = #f;
define variable $warning-bitmap          :: <label-type> = #f;
define variable $clients-folder-bitmap   :: <label-type> = #f;
define variable $uses-folder-bitmap      :: <label-type> = #f;


define variable $main-window-small-icon      :: <label-type> = #f;
define variable $project-window-small-icon   :: <label-type> = #f;
define variable $browser-window-small-icon   :: <label-type> = #f;
define variable $editor-window-small-icon    :: <label-type> = #f;
define variable $debugger-window-small-icon  :: <label-type> = #f;
define variable $describer-window-small-icon :: <label-type> = #f;
define variable $find-window-small-icon      :: <label-type> = #f;


/// environment-object-icon
///
/// NB these functions deal with environment objects in a loose sense:
/// objects visible in the environment. That is not just objects of type
/// <environment-object>.

define open generic environment-object-icon
    (project :: <project-object>, object :: <object>);

define method environment-object-icon
    (project :: <project-object>, object :: <object>)
  values(environment-object-small-icon(project, object),
         environment-object-large-icon(project, object))
end method environment-object-icon;


/// Large icons

define open generic environment-object-large-icon
    (project :: <project-object>, object :: <object>);

define method environment-object-large-icon
    (project :: <project-object>, object :: <object>)
  #f
end method environment-object-large-icon;


/// Small icons

define open generic environment-object-small-icon
    (project :: <project-object>, object :: <object>);

define method environment-object-small-icon
    (project :: <project-object>, object :: <object>)
  $default-bitmap
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, sr :: <source-record>)
  environment-object-small-icon(project, source-record-location(sr));
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, loc :: <source-location>)
  $definition-bitmap // ---*** $range-source-location-bitmap
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, obj :: <environment-object>)
  select (obj by instance?)
    <application>               => $application-bitmap;
    <condition-object>          => $error-bitmap;
    <class-object>              => $class-bitmap;
    <type-object>               => $class-bitmap;
    <type-expression-object>    => $class-bitmap;
    <foreign-object>            => $foreign-bitmap;
    <simple-function-object>    => $function-bitmap;
    <generic-function-object>   => $generic-bitmap;
    <macro-object>              => $macro-bitmap;
    <module-object>             => $module-bitmap;
    <library-object>            => $library-bitmap;
    <project-object>            => $project-bitmap;
    <slot-object>               => $slot-bitmap;
    <restart-object>            => $restart-bitmap;
    <unbound-object>            => $unbound-bitmap;
    <constant-object>           => $constant-bitmap;
    <variable-object>           => $variable-bitmap;
    <definition-object>         => $definition-bitmap;
    otherwise                   => $object-bitmap;
  end
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, object :: <method-object>)
  $method-bitmap
  /*---*** andrewa: this might be interesting, really we need a method-kind protocol
  if (method-generic-function(project, object))
    $method-bitmap
  else
    $function-bitmap
  end
  */
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, name :: <name-object>)
  let object = name-value(project, name);
  if (object)
    environment-object-small-icon(project, object)
  else
    next-method()
  end
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, obj :: <warning-object>)
  select (obj by instance?)
    <compiler-error-object>           => $error-bitmap;
    <project-warning-object>          => $serious-warning-bitmap;
    <serious-compiler-warning-object> => $serious-warning-bitmap;
    otherwise                         => $warning-bitmap;
  end
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, object :: <thread-object>)
  $threads-bitmap
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, object :: <stack-frame-object>)
  let type = stack-frame-type(project, object);
  select (type)
    #"dylan-call" =>
      let function = stack-frame-function(project, object);
      if (function)
        environment-object-small-icon(project, function);
      else
        $stack-frame-bitmap;
      end if;
    #"foreign-call" =>
      $stack-frame-bitmap; // ---*** $foreign-function-bitmap
    #"cleanup" =>
      $stack-frame-bitmap; // ---*** $cleanup-stack-frame-bitmap
    #"unknown" =>
      $stack-frame-bitmap; // ---*** $unknown-stack-frame-bitmap
  end select;
end method environment-object-small-icon;

define method environment-object-small-icon
    (project :: <project-object>, breakpoint :: <breakpoint-object>)
  case
    ~breakpoint =>
      $potential-breakpoint-image;
    breakpoint.breakpoint-current? =>
      $current-location-image;
    breakpoint.breakpoint-profile? =>
      $profile-point-image;
    breakpoint.breakpoint-transient? =>
      $step-breakpoint-image;
    breakpoint.breakpoint-stop? =>
      if (breakpoint-enabled?(breakpoint))
        $enabled-breakpoint-image
      else
        $disabled-breakpoint-image
      end;
    breakpoint.breakpoint-message? =>
      if (breakpoint-enabled?(breakpoint))
        $enabled-tracepoint-image
      else
        $disabled-tracepoint-image
      end;
    otherwise =>
      $potential-breakpoint-image;
  end
end method environment-object-small-icon;
