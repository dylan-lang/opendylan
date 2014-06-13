module: dfmc-testing

define function callback-handler (#rest args)
  format-out("%=\n", args);
end function callback-handler;
define function main()
  let project = find-project("dylan");
  open-project-compiler-database(project, 
                                 warning-callback: callback-handler,
                                 error-handler: callback-handler);

  run-typist-algebra-tests(safely?: #f);
  run-typist-inference-tests(safely?: #f);
end;

main();
