#! /usr/local/bin/perl

# Script to synchronize HOPE compounds.
# Arguments, if any, are patterns for the HOPE "sync" command, used
# to construct the list of compounds to synchronize.
# If no arguments, use the default pattern: /^DOC/
#
# The environment variable HOPE_CMD can be set to run something other
# than "hope".
sub main {
    local (@HOPE_CMD) = split (/\s/, ($ENV{"HOPE_CMD"} || "hope"));
    local ($DEFAULT_PAT) = '/^DOC/';
    local ($COMPOUND_PAT);
    $|=1;
    if (! @ARGV) { unshift (@ARGV, "$DEFAULT_PAT"); }
    while (@ARGV) {
        $COMPOUND_PAT = shift (@ARGV);
        system (@HOPE_CMD, "sync", "-compound", "$COMPOUND_PAT");
    }
}

&main;
