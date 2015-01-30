Module:    environment-profiler
Synopsis:  The profiling tool provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profiler

define frame <profiler-time-line> (<simple-frame>)
  pane time-line-page (frame)
    make(<tab-control-page>,
         label: "Time Line",
         id: #"time-line");
//  pane time-line-pane (frame)
//    make(<time-line-pane>);
end frame <profiler-time-line>;

define method refresh-profiler-page
    (frame :: <profiler-time-line>, page == #"time-line",
     #key clean? = #f, new-thread? = #t)
 => ()
/*
  let pane = frame.time-line-pane;
  let project = frame.ensure-frame-project;
  let thread = frame.frame-current-thread;
  let profile = project.project-last-profile;
  debug-out(#"environment-profiler", "Refreshing time line page (clean?: %=)", clean?);
  refresh-time-line-pane
    (pane, profile: profile, thread: thread, clean?: clean?)
*/
end method refresh-profiler-page;

define method invalidate-profiler-page
    (frame :: <profiler-time-line>, page == #"time-line") => ()
  //---*** Do something!
end method invalidate-profiler-page;

/*---*** Not ready!

/// Time line pane

define pane <time-line-pane> ()
  sealed slot time-line-profile :: false-or(<application-profile>) = #f;
  sealed slot time-line-thread :: false-or(<thread-object>) = #f;
  pane allocation-chart (pane)
    begin
      let thread = pane.time-line-thread;
      make(<equation-chart>,
           equation: thread & make-allocation-equation(pane, thread))
    end;
  pane page-faults-chart (pane)
    make(<equation-chart>,
         equation: make-page-faults-equation(pane));
  layout (pane)
    make(<column-splitter>,
         children: vector(pane.allocation-chart,
                          pane.page-faults-chart));
end pane <time-line-pane>;

define function make-time-line-equation
    (pane :: <time-line-pane>, value-function :: <function>)
 => (equation :: false-or(<equation>))
  let profile = pane.time-line-profile;
  if (profile)
    let snapshots = profile.application-profile-snapshots;
    let start-time :: <integer> = 0;
    let end-time   :: <integer> = 0;
    let min-value  :: <integer> = 0;
    let max-value  :: <integer> = 0;
    for (snapshot :: <application-snapshot> in snapshots)
      let wall-time = snapshot.application-snapshot-wall-time;
      let value = snapshot.value-function;
      max-allocation := max(max-allocation, allocation);
      end-time := end-time + wall-time
    end;
    make(<equation>,
         values: snapshots,
         x-function:
           method (snapshot :: <application-snapshot>, old-time :: <integer>)
             let wall-time = snapshot.application-snapshot-wall-time;
             wall-time + old-time
           end,
         y-function:
           method (snapshot :: <application-snapshot>, old-value :: <integer>)
             ignore(old-value);
             snapshot.value-function
           end,
         region:
           make-bounding-box(start-time, min-value, end-time, max-value))
  end
end function make-allocation-equation;

define function make-allocation-equation
    (pane :: <time-line-pane>, thread :: <thread-object>)
 => (equation :: false-or(<equation>))
  make-time-line-equation
    (pane,
     method (snapshot :: <application-snapshot>)
       let thead-snapshot
         = application-snapshot-thread-snapshot(snapshot, thread);
       thread-snapshot.thread-snapshot-allocation
     end)
end function make-allocation-equation;

define function make-page-faults-equation
    (pane :: <time-line-pane>) => (equation :: false-or(<equation>))
  make-time-line-equation(pane, application-snapshot-page-faults)
end function make-page-faults-equation;


/// Equation charting

define class <equation> (<object>)
  sealed slot equation-values :: <sequence>,
    required-init-keyword: values:;
  sealed slot equation-x-function :: <function>,
    required-init-keyword: x-function:;
  sealed slot equation-y-function :: <function>,
    required-init-keyword: y-function:;
  sealed slot equation-region :: <bounding-box>,
    required-init-keyword: region:;
end class <equation>;

define class <equation-chart> (<drawing-pane>)
  sealed slot chart-equation :: false-or(<equation>) = #f,
    init-keyword: equation:;
end class <equation-chart>;

define method handle-repaint
    (pane :: <equation-chart>, medium :: <medium>, region :: <region>)
 => ()
  let profile = pane.time-line-profile;
  if (profile)
    let (width, height) = sheet-size(pane);
    let total-wall-time = profile.application-total-wall-time;
    draw-chart(pane, medium, region, profile.application-profile-snapshots,
               application-snapshot-wall-time,   0, total-wall-time,
               application-snapshot-page-faults, 0, total-page-faults)
  end
end method handle-repaint;

define method draw-chart
    (pane :: <equation-chart>, medium :: <medium>, repaint-region :: <region>,
     equation :: <equation>)
 => ()
  let (pane-width, pane-height) = sheet-size(pane);
  let x-function = equation.equation-x-function;
  let y-function = equation.equation-y-function;
  let region = equation.equation-region;
  let (left,  top)    = box-position(region);
  let (width, height) = box-edges(region);
  let x-scale = (pane-width  * 1.0) / width;
  let y-scale = (pane-height * 1.0) / height;
  let transform
    = compose-scaling-with-transform
        (compose-transforms
           (make-reflection-transform(0, 0, 100, 0),
            make-translation-transform(left, top)),
         x-scale, y-scale);
  let x :: <real> = left;
  let y :: <real> = top;
  with-transform (medium, transform)
    let new-x = x-function(value, x);
    let new-y = y-function(value, y);
    draw-line(medium, x, y, new-x, new-y);
    x := new-x;
    y := new-y
  end
end method draw-chart;
*/
