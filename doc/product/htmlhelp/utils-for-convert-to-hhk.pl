#!/usr/local/bin/perl -I/u/andrews/include/htmlhelp
# usage: Not executable
#
# Utility routines used in /u/andrews/bin/convert-to-hhk.pl
#
# 09/02/98: Wrote this

sub unescape_html_symbols {
    local($label) = @_;
    $label =~ s/&gt;/>/g;
    $label =~ s/&lt;/</g;
    $label =~ s/\\/\\\\/g;
    $label =~ s/&quot;/\\\"/g;
    $label;
}


sub skippable {

    local($line) = @_; 
    local($can_be_skipped) = 1;

    SWITCH: {

	if ($line =~ /<H2 /) { last SWITCH; }
	if ($line =~ /<H3 /) { last SWITCH; }
	if ($line =~ /<!--/) { last SWITCH; }
	if ($line =~ /<DIV /) { last SWITCH; }
	if ($line =~ /<\DIV /) { last SWITCH; } 
	if ($line =~ /<PRE /) { last SWITCH; }
	if ($line =~ /<\PRE /) { last SWITCH; }

        $can_be_skipped = 0;

    }
   
    return $can_be_skipped;
}


# LIST OF TOPIC TYPES TO THROW AWAY
# This is a -- possibly temporary -- measure
# to overcome the unpleasantness of having 
# fifty thousand `classes, <foo> class' entries
# by throwing them away.  


sub can_be_thrown_away {

    local($text) = @_;
    local($e, @throwaway); 

    @throwaway = ("classes, ",
		  "variables, ",
		  "constants, ",
		  "generic functions, ",
		  "functions, ",
		  "methods, ",
		  "init-keywords, ",
		  "types, ",
		  "modules, ",
		  "libraries, ",
		  "operations, ",
		  "statement macros, ",
		  "definition macros, ",
		  "macros, ");

    foreach $e (@throwaway) {
	if ($text =~ /^$e/) { # Found something that should be thrown away
	    return 1;  
	} 
    }

    # Didn't find something that should be thrown away

    return 0; 

}

1;

