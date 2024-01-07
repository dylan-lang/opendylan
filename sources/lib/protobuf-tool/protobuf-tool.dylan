Module: protobuf-tool
Synopsis: Support generating Dylan code from .proto files in the compiler.


// In addition to this code, the compiler executables have to "use protobuf-tool".

// TODO(cgay): It looks like clean-build? is never passed at all?

tool-register(#"protobuf", invoke-protobuf-tool);

define constant $proto-directory = #"directory";
define constant $proto-files     = #"files";
define constant $proto-library   = #"library";
define constant $all-headers
  = vector(#"origin", $proto-directory, $proto-files, $proto-library);

define method invoke-protobuf-tool
    (spec-file :: <file-locator>, project-file :: false-or(<file-locator>), prev-run :: false-or(<date>),
     #key clean-build? :: <boolean>)
 => (anything-modified? :: <boolean>, modified-projects :: <sequence> /* of: <locator> */)
  debug-out(#"protobuf-tool",
            "*** Protobuf tool invoked. Spec: %=, project: %=, prev: %=, clean: %=\n",
             spec-file, project-file, prev-run, clean-build?);
  if (~project-file)
    // Comments in tools-interface.dylan say project-file should only be #f when invoked
    // from the tool-invoke app (i.e., only during development of new plugins).
    tool-error("no project file passed to protobuf-tool??");
  end;
  // Note we don't do anything special for clean-build?: #t. Just re-gen the
  // Dylan files and let the compiler figure out if they've changed. Sufficient?

  let generator = generator-from-spec-file(spec-file, prev-run);
  if (~generator)
    values(#f, #())    // No generator indicates nothing changed since prev-run.
  else
    let (output-files, lid-file) = generate-dylan-code(generator);
    if (lid-file)
      maybe-update-subproject(project-file, output-files, lid-file, prev-run)
    else
      modify-current-project(project-file, output-files)
    end
  end
end method invoke-protobuf-tool;

define function maybe-update-subproject
    (project-file :: <file-locator>, output-files :: <sequence>, lid-file :: <file-locator>,
     prev-run :: false-or(<date>))
 => (any-change? :: <boolean>, modified-projects :: <sequence>)
  // Code was generated in its own library, so the only file that might be returned
  // is the LID file.
  if (~prev-run
        | file-modified?(lid-file, prev-run)
        | any?(rcurry(file-modified?, prev-run), output-files))
    add-subproject(project-file, lid-file);
    values(#t, vector(project-file))
  else
    values(#f, #())
  end
end function;

// Code was generated without a library definition or LID file so add the output files to
// the current project. The user might want to do this to add a method to a sealed
// generic function, for example.
//
// HACK: Insert the module files after the library file, which we assume to be the first
// file. When doing this, the user must define their library in a separate file from
// their modules so that the module files follow the generated protobuf module files and
// can therefore use the generated modules. What's a better way?
// Example: the project ends up looking like this:
//   Files: library.dylan
//          generated-pb-module1.dylan
//          generated-pb-module2.dylan
//          generated-pb.dylan
//          user-module.dylan
//          user-main.dylan
define function modify-current-project
    (project-file :: <file-locator>, output-files :: <sequence>)
 => (anything-modified? :: <boolean>, modified-projects :: <sequence> /* of: <locator> */)
  let project = read-project-file(project-file);
  let any-changed? = #f;
  for (output-file in reverse(output-files))
    let relative = relative-locator(output-file, project-file);
    let project-files = project.project-information-files;
    if (member?(output-file, project-files, test: \=))
      debug-out(#"protobuf-tool", "  File already in project: %=\n", output-file);
    else
      project.project-information-files
        := concatenate(list(project-files[0], output-file),
                       copy-sequence(project-files, start: 1));
      any-changed? := #t;
      debug-out(#"protobuf-tool", "  Adding file to project: %=\n", output-file);
      debug-out(#"protobuf-tool", "  Files: %=\n", project.project-information-files);
    end;
  end;
  if (any-changed?)
    write-project-file(project-file, project);
    values(#t, vector(project-file))
  else
    values(#f, #())
  end
end function modify-current-project;

/* Spec file format:
Origin: protobuf       // Invoke this tool.
Files: a.proto         // List of files to parse.
       b.proto
Library: foo-pb        // Name of library to create. If not specified then don't
                       // create a library; just generate module files.
Directory: generated   // Where to put generated files, relative to project dir.
*/

define function generator-from-spec-file
    (spec-file :: <file-locator>, prev-run :: false-or(<date>))
 => (gen :: false-or(<generator>))
  let spec :: <table> = read-file-header(spec-file);
  for (_ keyed-by k in spec)
    if (~member?(k, $all-headers))
      // TODO(cgay): how to just issue a warning?
      tool-error("unrecognized key %= in %s", k, spec-file)
    end;
  end;

  let proto-files = element(spec, $proto-files, default: #f)
    | tool-error("%s has no Files: header", spec-file);
  let proto-files = map(method (proto-file :: <string>)
                          merge-locators(as(<file-locator>, proto-file), spec-file)
                        end,
                        proto-files);
  if (~prev-run
        | file-modified?(spec-file, prev-run)
        | any?(rcurry(file-modified?, prev-run),
               proto-files))
    local method one-or-none (key)
            let v = element(spec, key, default: #f);
            if (v)
              if (v.size > 1)
                tool-error("The '%s:' header in %s must have exactly one value, got %=",
                           format-arguments: list(key, spec-file, v));
              end;
              v[0]
            end;
          end;
    let output-directory = spec-file.locator-directory;
    let dir = one-or-none($proto-directory);
    if (dir)
      output-directory := apply(subdirectory-locator, output-directory,
                                locator-path(as(<directory-locator>, dir)));
    end;
    make(<generator>,
         input-files: proto-files,
         output-directory: output-directory,
         library-name: one-or-none($proto-library))
  end if
end function;

define function file-modified? (locator :: <file-locator>, since :: <date>)
  block ()
    file-property(locator, #"modification-date") > since
  exception (err :: <file-system-error>)
    tool-error("could not open proto file %s: %=",
               format-arguments: list(locator, err));
  end
end function;

define function add-subproject
    (project-file :: <file-locator>, subproject :: <file-locator>) => ()
  let project = read-project-file(project-file);
  let loc = relative-locator(subproject, project-file);
  // Remove the file extension because it confuses the build system.
  let loc = make(<file-locator>,
                 directory: locator-directory(loc), // may be #f
                 base: locator-base(loc));
  project.project-information-subprojects
    := add-new!(project.project-information-subprojects,
                loc,
                test: \=);
  write-project-file(project-file, project);
end function;
