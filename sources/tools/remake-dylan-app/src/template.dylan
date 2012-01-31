module: remake-dylan-app

// The <template> class is used to encapuslate constant format strings
// ("templates") and its arguments.

define class <template> (<object>)
  slot constant-string, init-keyword: constant-string:, init-value: "";
  slot arguments, init-keyword: arguments:, init-value: #();
end class <template>;

define class <writable-template> (<template>)
  slot output-path, required-init-keyword: output-path:;
end class <writable-template>;

define function make-writable-template
    (path, constant-string, #rest args) => (template :: <writable-template>)
  make(<writable-template>, output-path: path,
       constant-string: constant-string, arguments: args);
end function make-writable-template;

define method as
    (class == <string>, template :: <template>) => (output :: <string>)
  apply(format-to-string, constant-string(template), arguments(template));
end method as;

define method print-object
    (template :: <template>, stream :: <stream>) => ()
  format(stream, "%s", as(<string>, template));
end method print-object;

define generic write-templates (#rest templates) => ();

define method write-templates(#rest templates :: <writable-template>) => ();
  for (template in templates)
    with-open-file (stream = output-path(template), direction: #"output",
		    if-does-not-exist: #"create")
      print-object(template, stream);
    end with-open-file;
  end for;
end method write-templates;
