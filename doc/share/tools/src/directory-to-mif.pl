#!/usr/local/bin/perl 

# directory-to-mif.  last edit: 8 Nov 95
# Converts the FrameMaker files in one or more directories 
# to MIF format.  Backs up any old MIF versions of those files. 

# Usage: directory-to-mif directory [directories]

# $FMHOME is the FrameMaker 5 installation directory.

$FMHOME = $ENV{"FMHOME"} || "/usr/local/frame";

# $FMBATCH_COMMAND is the location of the FrameMaker batch
#  command, fmbatch.

$FMBATCH_COMMAND = "$FMHOME/bin/fmbatch";

# $FM_FILE_SUFFIXES is a colon-separated list of possible 
# FrameMaker document file suffixes.  Files with these suffixes
# will be saved as MIF.  Include the dot, and use a colon to 
# separate the suffixes.  
# NOTE: Don't add book suffixes to this variable -- they are
# covered by $FM_BOOK_SUFFIXES.

$FM_FILE_SUFFIXES = ".doc";

# $FM_BOOK_SUFFIXES is a colon-separated list of possible 
# FrameMaker book file suffixes.  Book files with these suffixes 
# will be saved as MIF.  Include the dot, and use a colon to 
# separate the suffixes.  

$FM_BOOK_SUFFIXES = ".book:.bk";


# $FMBATCH_FILE_SUFFIX provides the suffix used when saving
# an ordinary FrameMaker file as MIF.  
#
# $FMBATCH_BOOK_SUFFIX provides the suffix used when saving
# a FrameMaker book file as MIF.  
#
# Suffixes for the two must differ, since it is possible for a 
# book to share its file stem with one of the files it contains.
# Without a different suffix, a name clash would occur when 
# writing the MIF file.  

$FMBATCH_FILE_SUFFIX = ".MIF";
$FMBATCH_BOOK_SUFFIX = ".book.MIF";


############# DON'T EDIT ANYTHING BEYOND THIS POINT! #############

# $BASECONDCMD is used as the basis of shell calls invoking 
# Perl to do in-place editing of files.

$BASECMD = "perl -w -pi -e";

# If -delete is passed in the command-line arguments, $DELETE 
# is set to `y' and source files will be deleted after being
# converted to MIF.

$DELETE='n';


# SUB CONVERT_TO_MIF.  Converts FrameMaker files in a given 
# directory to MIF.  

sub convert_to_mif {

    local($DIRECTORY) = @_;
    local(%FILES, $FILE, $STEM, $SUFFIX, $BATCH_COMMAND);

    chdir($DIRECTORY);

    @FILES = &get_files($DIRECTORY, 
			join(':', $FM_FILE_SUFFIXES, $FM_BOOK_SUFFIXES));

    foreach $FILE (@FILES) {
	$BATCH_COMMAND = join(' ', $FMBATCH_COMMAND, &get_mif_batch_command($FILE));
	print STDOUT "Converting file ", $FILE, " to MIF...\n";
	system $BATCH_COMMAND; 
	if ($DELETE eq 'y') { unlink $FILE };
    }
}

# SUB GET_FILES.  Returns names of files with given suffixes
# in a given directory.

sub get_files {

    local($DIRECTORY, $SUFFIXES) = @_;
    local(@FILES);
    local(@FOUNDFILES);
    local($SUFFIX);

    local(@SUFFIXES) = split(/:/, $SUFFIXES);
#    print STDOUT $DIRECTORY;
    opendir(DIR,$DIRECTORY);
    @FILES = readdir(DIR); 
    closedir(DIR);

    foreach $SUFFIX (@SUFFIXES) {
	push (@FOUNDFILES, grep(/\w+$SUFFIX$/, @FILES)) unless (grep(/\w+$SUFFIX$/, @FOUNDFILES));
    }

    return @FOUNDFILES;
}

# SUB GET_MIF_BATCH_COMMAND.  Constructs FrameMaker batch command
# for dealing with a particular suffix.  Might consider cutting 
# the batch command up a bit more, into variables at the level
# $FMBATCH_OPENCOMMAND, $FMBATCH_SAVECOMMAND, but it hardly 
# seems worth it right now.

sub get_mif_batch_command {

    local ($FILE) = @_;

    local ($STEM, $SUFFIX) = split(/\./,$FILE);
    local ($BATCH_COMMAND); 

    if (grep(/$SUFFIX/, split(/:/, $FM_FILE_SUFFIXES))) { 

      $BATCH_COMMAND = "<<FM\nOpen $FILE\nSaveAs m $FILE $STEM$FMBATCH_FILE_SUFFIX\nQuit $FILE\nFM\n"; 

  } elsif (grep(/$SUFFIX/, split(/:/, $FM_BOOK_SUFFIXES))) {

      $BATCH_COMMAND = "<<FM\nOpen $FILE\nSaveAs m $FILE $STEM$FMBATCH_BOOK_SUFFIX\nQuit $FILE\nFM\n"; 

  } else { die "Error: Couldn't build batch command.  File suffix ($SUFFIX) was illegal.\n";};

    return $BATCH_COMMAND;  

}


# SUB MAIN.  Converts directory given on command-line to MIF.

sub main {

    local(@DIRECTORIES);

    if (!(@ARGV)) { die "Error: no arguments specified," };

    print STDOUT $ARGV{0};

    foreach (@ARGV) {

	if ($_ eq "-d") { $DELETE='y' }
	else { push(@DIRECTORIES, $_) };
	 
    };

    if (!(@DIRECTORIES)) { die "Error: no directory specified, " };

    foreach (@DIRECTORIES) {
	print STDOUT "Converting directory $_ to MIF:\n";
	&convert_to_mif($_);

    };

};



# Go ...

&main();

