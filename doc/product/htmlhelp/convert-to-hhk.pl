#!/usr/local/bin/perl 
#
# usage: convert-to-hhk.pl input-file output-file
#
# NOTE: This script is intended to work on indexes only one level
# of nesting deep.
#
# 1999-05-28: add current directory to emitted URLs
# 09/02/98: General rewrite to simplify

# EMISSION 
push (@INC, "/u/andrews/include/htmlhelp") ;

require "utils-for-convert-to-hhk.pl";
require "emit-hhk-entries.pl";
require "decode-wm-index-entries.pl";

# GENERATE


sub build_hhk_index {

    # Main routine that builds an HHK index out of a WebMaker index

    local ($directory_prefix) = @_;
    local ($within_body_of_webmaker_index, @items, $i, $link, @links, $topic, $nesting);

    $nesting = 0;    

    local ($f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8, $f9, $f10, $f11);

    $within_body_of_webmaker_index = 0;

    # First, empty the @links list, which is used to build up a list of 
    # links in a multiple-link topic

    @links = ();

    local ($first) = 1;
  

    while (@LINES)  {

        # Extract a single line from the WebMaker index file

	$line = shift(@LINES); 

        # The useful part of a WebMaker index file is sandwiched between 
        # two horizontal rule statements.  So if the line is the wrong side of a 
        # horizontal rule, we skip it 

	if ($line =~ /<HR/) {  # used to be /<HR / 
	    print STDOUT "encountered a horizontal rule\n";
	    $within_body_of_webmaker_index = 
		1 - $within_body_of_webmaker_index;

	    next; # Added this line-punt because WM 3.0.2 inserts a
	          # dud <A NAME...> link immediately after first <HR>
	          # -- I dunno why 
	};

	next if (! $within_body_of_webmaker_index );


        # Even within the useful part of the WebMaker index file, there are some
        # useless HTML statements that we can skip
	print STDOUT "got to skippable call \n";
	next if (&skippable($line)); 

        # Only interesting cases remain

        # Now examine each case.  Unfortunately, we have to eliminate
        # certain cases before testing for others.
	print STDOUT "got to switch \n";

      SWITCH: {

        # Case 1: Single-link topic at top level of index
        # ^  <A ... and $nesting is 0

	if (($line =~ /^  <A /) && ($nesting eq 0)) { 

	    ($topic, $link) = &decode_single_link_wm_index_entry($line);

	    &emit_single_link_topic($directory_prefix, $topic, 
				    $link, $nesting);
	    $last_topic = $topic;
            last SWITCH; 
	}

        # Case 2: Single-link topic at first level of nesting.  
        # ^    <A ... and $nesting is 1

        if (($line =~ /^    <A /) && ($nesting eq 1)) {

	    ($topic, $link) = &decode_single_link_wm_index_entry($line);
	    &emit_single_link_topic($directory_prefix, 
				    $last_topic . ", " . $topic,
				    $link, $nesting);
	    last SWITCH;
	}


        # Case 3: Single-link topic that appears at top-level
        # immediately after a first-level nesting sequence.  By doing
        # so it terminates the nested sequence.
        # 
        # ^  <A ... and $nesting is 1

        if (($line =~ /^  <A /) && ($nesting eq 1)) { 

	    # Terminate the first-level nesting sequence

	    $nesting = 0;

	    # Now do the usual stuff

	    ($topic, $link) = &decode_single_link_wm_index_entry($line);
	    &emit_single_link_topic($directory_prefix, $topic, 
				    $link, $nesting);
	    $last_topic = $topic;
	    last SWITCH;
	}


        # Case 4: Nest-opening single-link topic

        if (($line =~ /^    <A /) && ($nesting eq 0)) { 


	    $nesting = 1;

	    ($topic, $link) = &decode_single_link_wm_index_entry($line);
	    &emit_single_link_topic($directory_prefix, 
				    $last_topic . ", " . $topic, 
				    $link, $nesting);
	    last SWITCH;
	}


        # Case 5: Nested multiple-link topic 

        if (($line =~ /^    (.*)( <A HREF.*)/) && ($nesting eq 1)) { 

            ($topic, @links) = &decode_multiple_link_wm_index_entry($line, $nesting);

	    &emit_multiple_link_topic($directory_prefix, 
				      $last_topic . ", " . $topic, 
				      $nesting, @links);
	    last SWITCH;
	}



        # Case 6: Nest-opening multiple-link topic 


	if (($line =~ /^    (.*)( <A HREF.*)/) && ($nesting eq 0)) { # multiple, open nest level

            # Open new nesting sequence

	    $nesting = 1;
	    
            # Get topic and links for this entry, then emit

            ($topic, @links) = &decode_multiple_link_wm_index_entry($line, $nesting);
	    &emit_multiple_link_topic($directory_prefix, 
				      $last_topic . ", " . $topic, 
				      $nesting, @links);
	    last SWITCH;
	}



        # Case 7: Nest-closing multiple-link topic 

	if (($line =~ /^  (.*)( <A HREF.*)/) && ($nesting eq 1)) { # multiple, close nest level

	    $nesting = 0;

            ($topic, @links) = &decode_multiple_link_wm_index_entry($line, $nesting);
	    &emit_multiple_link_topic($directory_prefix, $topic, 
				      $nesting, @links);
	    $last_topic = $topic;
	    last SWITCH;

	}

        # Case 8: Top-level multiple-link topic 
        #
        # Looks like this in the WebMaker HTML (`v' represents a
        # space; [text] any text string that does not begin with a space)
        #
        #  vv[text]
        #  vvtopicv<A HREF="link 1" CLASS=FOOTER>[1]</A> <A HREF="link 2" CLASS=FOOTER>[2]</A>  ...
        #  vv[text] or vvvv[text] or
        #
        # That is, these entries are top-level, and continue a
	# top-level sequence. 

	if (($line =~ /^  (.*)( <A HREF.*)/) && ($nesting eq 0)) { # multiple at top level

            ($topic, @links) = &decode_multiple_link_wm_index_entry($line);
	    &emit_multiple_link_topic($directory_prefix, $topic, 
				      $nesting, @links);
	    $last_topic = $topic;
	    last SWITCH;

	} 


        # Case 9: Top-level no-link entry that forms a $last_topic prefix in a flattened multi-liner
        
        if ($line =~ /^  (\w+)/) { # treat as multi-liner, top-level

	    ($topic) = ($line =~ m/^  (.*)/);

	    $last_topic = $topic;
	    last SWITCH;

	}

    }

    }
}





sub main {

# Running on Windows or Unix?
  
    if (!($ENV{'COMSPEC'} eq '')) { $CURRENT_DIR = `cd`; }
    else { $CURRENT_DIR = `pwd`; };

    @path = split(m;/;, $CURRENT_DIR);
    $numelements = @path;
    chop($directory_prefix = $path[$numelements - 1]);



    ($input, $output) = @ARGV;

    open(OUTPUT, ">$output") || 
	die("Error, could not open output file \"$output\": $@\n");

    open(INPUT, $input) || 
	die("Error, could not open input file \"$input\": $@\n");

# I'm not pleased about putting all the lines of the file into an
# array, but ix_emitter unshifts lines back on to the list, and I 
# couldn't find a way to unshift lines back on to the stream.  If there is a
# way of looking ahead on the stream, that might work.  

    @LINES = <INPUT>;

#    &emit_header();

    &build_hhk_index($directory_prefix); # do the work

#    &emit_footer();

    close(INPUT);

    close(OUTPUT);

}


&main();
