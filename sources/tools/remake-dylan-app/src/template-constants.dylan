module: remake-dylan-app

define constant $main-template-simple :: <string>
  = "module: %s\nsynopsis: \nauthor: \ncopyright: \n\ndefine function main(name, arguments)\n  format-out(\"Hello, world!\\n\");\n  exit-application(0);\nend function main;\n\n// Invoke our main() function.\nmain(application-name(), application-arguments());\n";

define constant $lib-template-simple :: <string>
  = "module: dylan-user\n\ndefine library %s\n  use common-dylan;\n  use io;\nend library;\n\ndefine module %s\n  use common-dylan;\n  use format-out;\nend module;\n";

define constant $lid-template-simple :: <string>
  = "library: %s\nexecutable: %s\nfiles: %s\n  %s\n";
