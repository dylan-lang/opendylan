#!/usr/local/bin/perl
# usage: convert-to-hhc.pl input-file output-file title
# upgraded version for WebMaker 3.0.2 contents pages
# 3 nov 97: added flag for full TOC output or just LI-item generation
# 8 jan 98: upgraded it to emit new HTMLHelp 1.1 headings and footers

# global -- get the current dir


sub parsenumlabel { # works out heading level from its number

    local($number, $current_level) = @_;

  SWITCH: {
      if ($number =~ m/[A-Z0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\s*$/) { # 3Heading
	  $headcount = 3; $last_numbered = 1; last SWITCH; }
      if ($number =~ m/^[A-Z0-9]\.[0-9]+\.[0-9]+\s*$/) { # 2Heading 
	  $headcount = 2; $last_numbered = 1; last SWITCH; }
      if ($number =~ m/^[A-Z0-9]+\.[0-9]+\s*$/) { # 1Heading
	  $headcount = 1; $last_numbered = 1; last SWITCH; }
      if (($number =~ m/^[A-Z0-9]+\s*$/) ||  
	  ($number =~ m/^[A-Z0-9]+\.\s*$/)) { # Chapters
	  $headcount = 0; $last_numbered = 1; last SWITCH; }
      if ($number eq '') { # not numbered: could be Preface heading or REntry
	  if ($last_numbered eq 1) { # if last heading was numbered,
	      $headcount = $current_level + 1; # we want to indent this
	      $last_numbered = 0; } 
	  else {
	      $headcount = $current_level; # otherwise we don't
	  };

	  last SWITCH; }

      $headcount = -1;
  }
    return $headcount;
};

sub emit_header {

    # emits the header information for the .hhc contents file

    print OUTPUT "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n";
    print OUTPUT "<HTML>\n<HEAD>\n";
    print OUTPUT "<meta name=\"GENERATOR\" content=\"Microsoft&reg; HTML Help Workshop 4.1\">\n";
    print OUTPUT "<!-- Sitemap 1.0 -->\n";
    print OUTPUT "<\HEAD>\n<BODY>\n";
    print OUTPUT "<OBJECT type=\"text/site properties\">\n";
    print OUTPUT "\t<param name=\"FrameName\" value=\"content\">\n";
    print OUTPUT "</OBJECT>\n\n";
    print OUTPUT "<UL>\n"; 
}


sub emit_title_topic {

    local($directory_prefix, $title) = @_;

    print OUTPUT "\t<LI> <OBJECT type=\"text/sitemap\">\n";
    print OUTPUT "\t\t<param name=\"Name\" value=\"$title\">\n";
    print OUTPUT "\t\t<param name=\"Local\" value=\"$directory_prefix/index.htm\">\n";
    #    print OUTPUT "\t\t<param name=\"FrameName\" value=\"content\">\n";
    print OUTPUT "\t\t</OBJECT>\n";

}

sub emit_footer {

    print OUTPUT "</UL>\n";
    print OUTPUT "</BODY>\n";
    print OUTPUT "</HTML>\n";

}

# PRINT_INDENTED
#
# Syntax: print_indented(indent-level, text);
#
# Descriptions: Prints _text_ to the OUTPUT file stream, tabbed
# indent-level+1 tabs.  Not all parts of the script use this 
# function to print things, and that's because the script is a 
# hack.

sub print_indented {

    local($level, $text) = @_;

    while ($level >= 0) {
	print OUTPUT "\t";
	$level = $level - 1;
    }

    print OUTPUT $text;

}


# EMIT_STARTLIST and EMIT_ENDLIST
#
# Syntax: emit_[start|end]list(indent-level);
#
# Description: Emit the HTML code for the start/end of a TOC.


sub emit_startlist {

    local($level) = @_;

    &print_indented($level, "<UL>\n");
}

sub emit_endlist {

    local($level) = @_;

    &print_indented($level, "</UL>\n");

}


# EMIT_HEADING
#
# Syntax: emit_heading(directory-prefix, indent-level, heading-number, heading-text, URL)
#
# Description: Emits a TOC entry at _indent-level_, linked to _URL_.  
# Uses _heading-number_ and _heading-text_ to built the TOC entry label.  

sub emit_heading {

    local($directory_prefix, $level, $number, $text, $url) = @_;
    $text =~ s/^\s+- / /;
    local($label) = $number . $text;

    &print_indented($level, "\t<LI> <OBJECT type=\"text/sitemap\">\n");
    &print_indented($level, "\t\t<param name=\"Name\" value=\"$label\">\n");
    &print_indented($level, "\t\t<param name=\"Local\" value=\"$directory_prefix/$url\">\n");
#    &print_indented($level, "\t\t<param name=\"FrameName\" value=\"content\">\n");
    &print_indented($level, "\t     </OBJECT>\n");

}

# EMIT_TOC
#
# Description: This is an admin function to prepare the first level of 
# the TOC: TOCs are actually built up through recursion with TOC_EMITTER; 
# this function sets up the first call to that function with the right 
# indentation info.

sub emit_toc {

    local ($directory_prefix) = @_;
    local($current_level, $last_numbered);
    $current_level = 0;
    $last_numbered = 0;
    &emit_startlist($current_level);
    &toc_emitter($directory_prefix, $current_level, $last_numbered); 
    &emit_endlist($current_level);
}


# TOC_EMITTER
#
# Syntax: toc_emitter(directory_prefix, current_level, last_numbered)
#
# Description: The meat of the script.  A nasty mix of iteration
# and recursion that examines each line in input-file and decides 
# whether it is a heading that should be included in the HTMLHelp
# TOC.  
#
# If the line is a heading, the routine figures out whether it
# should be placed directly beneath the heading emitted last, or 
# whether it is a subheading that should be indented to a new level.  
# 
# A subheading is considered to be a `nested' TOC and is therefore
# produced via a recursive call to TOC emitter.

sub toc_emitter {


    local($directory_prefix, $current_level, $last_numbered) = @_;
    local($line, $heading, $heading_level);

    while (@LINES) {
	$line = shift(@LINES);

	next if ($line !~ /<DT>/); # skip line if not a heading

	($number, $text) = ($line =~ m+<B>(.*)</B>(.*)</A>+);

	$heading_level = &parsenumlabel($number, 
					$current_level,
					$last_numbered); # get heading number 

	next if ($heading_level eq -1); # if heading illegal, go to next line

	($url) = ($line =~ m+HREF=\"(.*)\"+);

	if ($heading_level > $current_level) { # if a subhead, nest a new TOC
	    &emit_startlist($heading_level);
	    &emit_heading($directory_prefix, $heading_level, 
			  $number, $text, $url);
	    &toc_emitter($directory_prefix, $heading_level, $last_numbered);
	    &emit_endlist($heading_level);
	}

	elsif ($heading_level eq $current_level) { # if same level, emit
	    &emit_heading($directory_prefix, $heading_level, $number, $text, $url);
	}

	elsif ($heading_level < $current_level) { # if a lower level, return
	    unshift(@LINES, $line);
	    return;
	}	    
    }

}


# MAIN
#
# Description: Gets the command-line input, opens all the file streams, 
# and builds up the HTMLHelp contents (HHC) file.

sub main { 

# Running on Windows or Unix?
  
  if (!($ENV{'COMSPEC'} eq '')) { $CURRENT_DIR = `cd`; }
  else { $CURRENT_DIR = `pwd`; };

  print STDOUT "Current dir is: ", $CURRENT_DIR;

  @path = split(m;/;, $CURRENT_DIR);

  $numelements = @path;

  chop($directory_prefix = $path[$numelements - 1]);

 ($input, $output, $title, $toc_or_element) = @ARGV;
 
 open(INPUT, $input) || 
     die("Error, could not open input file \"$input\": $@\n");


# I'm not pleased about putting all the lines of the file into an
# array, but toc_emitter unshifts lines back on to the list, and I
# couldn't find a way to unshift lines back on to the stream.  If
# there is a way of looking ahead on the stream, that might work, but
# I dunno.

 while(<INPUT>) {
     $line = $_;
     push(@LINES, $line);

 }

 close(INPUT);

 open(OUTPUT, ">$output") || 
     die("Error, could not open output file \"$output\": $@\n");

 if ($toc_or_element eq 'toc' ) { 
     &emit_header(); # emit the HTML file header info
     &emit_title_topic($directory_prefix, $title); # emit the titular topic
     &emit_toc(); # emit the TOC
     &emit_footer(); # emit the HTML file footer info

 } else {
     &emit_title_topic($directory_prefix, $title); # emit the titular topic
     &emit_toc($directory_prefix); # emit the TOC
 };


 close(OUTPUT);

}

&main(); 



