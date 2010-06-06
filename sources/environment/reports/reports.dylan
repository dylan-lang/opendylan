Module:    environment-reports
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Report protocol

define constant $report-width = 72;

define constant $report-separator
  = make(<byte-string>, size: $report-width, fill: '-');

define constant <report-format> = one-of(#"text", #"html", #"xml");

define abstract class <report> (<object>)
  sealed constant slot report-format :: <report-format> = #"text",
    init-keyword: format:;
end class <report>;

define abstract class <multi-file-report> (<report>)
  slot report-directory :: false-or(<directory-locator>) = #f,
    init-keyword: directory:;
end class <multi-file-report>;

define sealed domain make (subclass(<report>));
define sealed domain initialize (<report>);

define generic write-report-as
    (stream :: <stream>, report :: <report>, format :: <report-format>)
 => ();
 
define generic create-multi-file-report-as
    (report :: <multi-file-report>, locator :: <directory-locator>, 
     format :: <report-format>)
 => (root-filename :: false-or(<file-locator>));
 

/// Some useful report subclasses

define abstract class <project-report> (<report>)
  sealed constant slot report-project :: <project-object>,
    required-init-keyword: project:;
end class <project-report>;


/// Implementation

define class <report-error> (<simple-error>)
  keyword format-string: = "unsupported report options";
end class <report-error>;

define method write-report-as
    (stream :: <stream>, report :: <report>, format :: <report-format>) => ()
  error(make(<report-error>));
end method write-report-as;

define function write-report
    (stream :: <stream>, report :: <report>)
 => ()
  write-report-as(stream, report, report.report-format)
end function write-report;

define function create-multi-file-report
    (report :: <report>, directory :: <directory-locator>)
 => (root-filename :: false-or(<file-locator>))
  create-multi-file-report-as(report, directory, report.report-format)
end function create-multi-file-report;

define function create-report-to-string
    (report :: <report>)
 => (string :: <string>)
  with-output-to-string (stream)
    let format = report.report-format;
    write-report-as(stream, report, format)
  end
end function create-report-to-string;


/// Installation

define constant $reports = make(<table>);

define class <report-info> (<object>)
  sealed constant slot report-info-name :: <symbol>,
    required-init-keyword: name:;
  sealed constant slot report-info-class :: subclass(<report>),
    required-init-keyword: class:;
  sealed constant slot report-info-title :: <string>,
    required-init-keyword: title:;
  sealed constant slot report-info-formats :: <sequence> = #[#"text"],
    init-keyword: formats:;
  sealed constant slot report-info-multi-file? :: <boolean> = #f,
    init-keyword: multi-file?:;
end class <report-info>;

define function report-info-format-name
    (info :: <report-info>, format :: <symbol>)
 => (name :: <string>)
  //---*** Make this extensible...
  select (format)
    #"text" => "Text";
    #"html" => "HTML";
    #"xml"  => "XML";
  end
end function report-info-format-name;

define function available-reports
    () => (reports :: <collection>)
  $reports
end function available-reports;

define function install-report
    (name :: <symbol>, title :: <string>, class :: subclass(<report>),
     #rest args, #key, #all-keys)
 => ()
  element($reports, name)
    := apply(make, <report-info>,
	     name:    name,
	     title:   title,
	     class:   class,
	     args)
end function install-report;

define function find-report-info
    (name :: <symbol>)
 => (info :: false-or(<report-info>))
  element($reports, name, default: #f)
end function find-report-info;
