Module: unicode-data-generator
Synopsis: 
Author: 
Copyright: 

define function message(#rest args)
 => ();
  apply(format-out, args);
  force-out();
end function;

define function main (name :: <string>, arguments :: <vector>)
  let ucd = make(<ucd-database>);

  // load currently-supported files
  ucd-load-blocks(ucd, "Blocks.txt");
  ucd-load-unicodedata(ucd, "UnicodeData.txt");
  ucd-load-proplist(ucd, "PropList.txt");
  ucd-load-proplist(ucd, "DerivedCoreProperties.txt");
  ucd-load-proplist(ucd, "DerivedNormalizationProps.txt");

  // resolve references
  ucd-resolve-simple-case-mappings(ucd);

  // optimize representations
  ucd-optimize-names(ucd);

  // generate output
  ucd-generate-character-name(ucd, "ud-names.dylan");

  exit-application(0);
end function main;

main(application-name(), application-arguments());
