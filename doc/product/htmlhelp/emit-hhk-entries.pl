#!/usr/local/bin/perl -I/u/andrews/bin
# usage: Not executable.
#
# 09/02/98: General rewrite of topic emitter routines
# for convert-to-hhk.pl


require "utils-for-convert-to-hhk.pl";

# EMISSION 

$hhk_line_terminator = "
\n";


sub emit {

    local ($indents, $text) = @_;

    local ($tabs) = "";

    while ($indents >= 0) {
	$tabs = $tabs . "\t";
        $indents = $indents - 1;
    }

    print OUTPUT $tabs, $text, $hhk_line_terminator;

}

sub emit_header {

    # Emits the header information for an .HHK index file
    
    &emit(-1, "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">");
    &emit(-1, "<HTML>");
    &emit(-1, "<HEAD>");
    &emit(-1, "<meta name=\"GENERATOR\" content=\"Microsoft&reg; HTML Help Workshop 4.1\">");
    &emit(-1, "<!-- Sitemap 1.0 -->");
    &emit(-1, "<\HEAD>");
    &emit(-1, "<BODY>");
    &emit(-1, "<OBJECT type=\"text/site properties\">");
    &emit(-1, "\t<param name=\"FrameName\" value=\"right\">");
    &emit(-1, "</OBJECT>");

    # This first <UL> begins the index entry list  

    &emit(-1, "<UL>"); 

}

sub emit_footer {

    # Emits the footer information for an .HHK index file

    # The </UL> ends the index entry list

    &emit(-1, "</UL>");
    &emit(-1, "</BODY>");
    &emit(-1, "</HTML>");

}


sub emit_single_link_topic {

    # Emits a topic for a keyword with a single link 
    #   
    # Single-link topic look like this on paper
    #
    #  errors, 13
    #
    # and like this in WebMaker HTML
    # 
    #  _errors_  

    local ($directory_prefix, $text, $link, $nesting) = @_;

    $text = &unescape_html_symbols($text);

    # Throw entry away if it belongs to class of throway entries

    if (&can_be_thrown_away($text)) { return (); }

    # All single topic begin with an <LI> and sitemap <OBJECT> decl,

    &emit($nesting, "<LI> <OBJECT type=\"text/sitemap\">");

    # then the text of the topic as it should appear in the index,

    &emit($nesting + 1, "<param name=\"Name\" value=\"$text\">");

    # then the link to be associated with that text

    &emit($nesting + 1, "<param name=\"Local\" value=\"$directory_prefix\$link\">");

    # and then are terminated with a </OBJECT> 
 
    &emit($nesting + 1, "</OBJECT>");

}


sub emit_multiple_link_topic {

    # Emits a topic for a keyword with two or more links
    #   
    # Multiple-link topics look like this on paper
    #
    #  errors, 13, 25
    #
    # and like this in WebMaker HTML
    # 
    #  errors  _[1]_ _[2]_
    #
    # In the HTML Help index window, an _n_ (_n_ >= 2) link topic has
    # to be encoded as a single-line topic that, when you click on it,
    # presents a dialog of _n_ topic choices.  In a hand-written index
    # you could make the topics in this dialog say something useful,
    # like `handling errors' and `errors on streams', but since
    # WebMaker only gives the text `[1]', `[2]', etc., we can't do
    # this here.  So I have chosen to name each topic choice `Topic 1',
    # `Topic 2', etc.
    
    local ($directory_prefix, $text, $nesting, @links) = @_;

    $text = &unescape_html_symbols($text);

    if (&can_be_thrown_away($text)) { return (); }

    # $topic is used in generating the topic number (see above); $l is
    # a loop counter variable

    local($topic) = 0;
    local($l);  

    # All multiple-link topics begin with an <LI> and sitemap <OBJECT> decl,

    &emit($nesting, "<LI> <OBJECT type=\"text/sitemap\">");

    # then the text of the topic as it should appear in the index,

    &emit($nesting + 1, "<param name=\"Name\" value=\"$text\">");

    # and here's where we have to emit a sub-topic label (`Topic _n_')
    # for each link in the WebMaker HTML index entry 

    foreach $l (@links) {
        $topic = $topic + 1;
	&emit($nesting + 1, "<param name=\"Name\" value=\"Topic $topic\">");
	&emit($nesting + 1, "<param name=\"Local\" value=\"$directory_prefix\$l\">"); 
    }

    # The entry ends as usual with a </OBJECT>
    
    &emit($nesting + 1, "</OBJECT>");

}

1;




