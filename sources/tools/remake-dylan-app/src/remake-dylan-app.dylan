module: remake-dylan-app
synopsis: A replacement for OpenDylan's original make-dylan-app
author: Rosario Raulin
copyright: (C) 2012 Rosario Raulin, see LICENSE file

define function main(app-name :: <string>, arguments :: <vector>) => ()
  let options = make(<remake-dylan-app-argparser>);

  if (arguments.size < 1)
    print-synopsis(options, *standard-error*);
    exit-application(1);
  else
    parse-arguments(options, arguments);

    for (name :: <string> in file-paths(options))
      block()
        make-dylan-app(name);
      exception (condition :: <condition>)
        format(*standard-error*, "error: %=\n", condition);
      end block;
    end for;
    exit-application(0);
  end if;
end function main;

main(application-name(), application-arguments());
