##############################################################################
### This is a perl script which gets evaluated as a trigger within HOPE
###
### $HopeName: D!trigger.pl(trunk.13) $
### $Id: trigger.pl,v 1.1 2004/03/12 00:41:07 cgay Exp $
##############################################################################

$EmailChangesTo = 'jonathan, keith, gz';

@MyEmailText       = ();
@MyMiddleEmailText = ();
%MyEmailArgs       = ();
%MyTriggerArgs     = ();
$DoEmail           = 0;

&processObjects;

### Post trigger
if ($MyTriggerArgs{'TRIGGERphase'} eq 'post') {
    # Send the email
    &email(*MyEmailArgs, *MyEmailText) if ($DoEmail);

    &exitOK;

### Pre trigger
} else {
    &exitOK;
}

##############################################################################
#### parse all objects
##############################################################################

sub processObjects {
    local($key, $val);

    %MyTriggerArgs = (); # For safety, reset this as well

    while (&decodeTriggerArgs(*MyTriggerArgs)) {

        # Ignore some commands.
        next if ($MyTriggerArgs{'TRIGGERcommand'} eq 'claim'    ||
                 $MyTriggerArgs{'TRIGGERcommand'} eq 'checkout' ||
                 $MyTriggerArgs{'TRIGGERcommand'} eq 'abandon'
                 );

        $DoEmail = 1;

        # Munge arguments
        $MyTriggerArgs{'COMMANDbranch'} = 'trunk'
            if ($MyTriggerArgs{'COMMANDbranch'} eq '.');

        ## crude subject line experimental
        local($b,$c,$u,$v, $subj);
        $b = $MyTriggerArgs{'COMMANDbranch'};
        $c = $MyTriggerArgs{'COMMANDcompound'};
        $u = $MyTriggerArgs{'COMMANDunit'};
        $v = $MyTriggerArgs{'COMMANDunitversion'};
        $subj = "$MyTriggerArgs{'TRIGGERcommand'} of " .
                (length($u) ? "$c($b)!$u($v)" : "$c($b)") .
                " from $MyTriggerArgs{'HOPEservername'}";

        %MyEmailArgs = ('To'     , $EmailChangesTo,
                        'From'   , $MyTriggerArgs{'HOPEclientusername'},
                        'Subject', $subj);

        @MyMiddleEmailText = ();

        # Go over each repeating argument
        foreach $key (sort(keys(%MyTriggerArgs))) {

           # skip repeating arguments we are really not interested in
           next if ($key eq 'HOPEclientdir'      ||
                    $key eq 'HOPEclientname'     ||
                    $key eq 'HOPEclientos'       ||
                    $key eq 'HOPEclientusername' ||
                    $key eq 'HOPEservername'     ||
                    $key eq 'HOPEserverversion'  ||
                    $key eq 'TRIGGERcompound'    ||
                    $key eq 'TRIGGERcompounddir' ||
                    $key eq 'TRIGGERphase'       ||
                    $key eq 'TRIGGERrundir'      ||
                    $key eq 'TRIGGERcommand'     ||
                    $key eq 'TRIGGERtest'        ||
                    # the below are not that useful either!
                    $key eq 'COMMANDdate'        ||
                    $key eq 'COMMANDfilename'    ||
                    $key eq 'COMMANDlocal'       ||
                    $key eq 'COMMANDrecursive'   ||
                    # skip what we handle below
                    $key eq 'COMMANDbranch'      ||
                    $key eq 'COMMANDcompound'    ||
                    $key eq 'COMMANDunit'        ||
                    $key eq 'COMMANDreason'      ||
                    $key eq 'COMMANDunitversion'
                    );


           ##########################################
           ### Command switch
           ##########################################

           if ($MyTriggerArgs{'TRIGGERcommand'} eq 'checkin') {
           ##########################################

               # skip what we handle above
               next if ($key eq 'COMMANDbugnumber'      ||
                        $key eq 'COMMANDforcedcheckin'  ||
                        $key eq 'COMMANDuseclaimreason' ||
                        $key eq 'COMMANDdelete'         ||
                        $key eq 'COMMANDuser'
                       );


           } elsif ($MyTriggerArgs{'TRIGGERcommand'} eq 'add') {
           ##########################################


           } elsif ($MyTriggerArgs{'TRIGGERcommand'} eq 'set') {
           ##########################################

               local(%set_attr, $set_attr, $set_val);
               %set_attr = split(',', $MyTriggerArgs{'COMMANDattributes'});
               while (($set_attr, $set_val) = each %set_attr) {
                   push(@MyMiddleEmailText,
                        sprintf("    %-23s %s",$set_attr,$set_val));
               }

               # skip what we handle above
               next if ($key eq 'COMMANDattributes');

               # catch anything else we might have missed
               push(@MyMiddleEmailText,
                    sprintf("    %-23s %s",$key,$MyTriggerArgs{$key}));


           } elsif ($MyTriggerArgs{'TRIGGERcommand'} eq 'remove') {
           ##########################################


           } elsif ($MyTriggerArgs{'TRIGGERcommand'} eq 'branch') {
           ##########################################

           } elsif ($MyTriggerArgs{'TRIGGERcommand'} eq 'checkpoint') {
           ##########################################

           }

           ##########################################
           # catch anything else we might have missed! missing command or
           # even a field within a command! Make sure you leave this here! 
           push(@MyMiddleEmailText,
                sprintf("    %-23s %s",$key,$MyTriggerArgs{$key}));
           ##########################################
        } # End of command switch

        push(@MyEmailText, "o $MyTriggerArgs{'COMMANDcompound'}(" .
                             "$MyTriggerArgs{'COMMANDbranch'})!" .
                             "$MyTriggerArgs{'COMMANDunit'}(" .
                             "$MyTriggerArgs{'COMMANDunitversion'})");
        # shove in that reason
        if (defined $MyTriggerArgs{'COMMANDreason'}) {

            # indent the reason a little
            local(@TheReason) = &unpackLines($MyTriggerArgs{'COMMANDreason'});
            foreach $line (@TheReason) {
                push(@MyEmailText, "  $line");
            }
        }

        push(@MyEmailText, @MyMiddleEmailText);
        push(@MyEmailText, "\n\n");
    }

    # Insert some text at the beginning of of the email!
    unshift(@MyEmailText,
            "\n\n" .
            "$MyTriggerArgs{'TRIGGERcommand'} command executed by " .
            "$MyTriggerArgs{'HOPEclientusername'}" .
            "\n");
}


##############################################################################
#### support routines
##############################################################################

sub unpackLines {
  local($line) = @_;
  local(@lines, $oneline);

  while ($line =~ /([^\\])\\n/) {  # find a linebreak
     $oneline = "$`$1";
     $line = $';
     $oneline =~ s/\\\\/\\/g;
     push(@lines, $oneline);
  }
  push(@lines, $line);
  return (@lines);
}

sub exitOK {
    exit 0;
}
sub exitAbort {
    exit 1;
}

sub email {
    local (*emailargs, *text) = @_;
    local ($SendMail, *MAIL);

    $SendMail = '/usr/lib/sendmail';

    return undef unless open (MAIL, "| $SendMail -t");

    if (length($emailargs{'From'})) {
        print MAIL "From: $emailargs{'From'}\n";
    }
    if (length($emailargs{'To'})) {
        print MAIL "To: $emailargs{'To'}\n";
    } else {
        return undef;
    }
    if (length($emailargs{'Reply-To'})) {
        print MAIL "Reply-To: $emailargs{'Reply-To'}\n";
    }
    if (length($emailargs{'Cc'})) {
        print MAIL "Cc: $emailargs{'Cc'}\n";
    }
    if (length($emailargs{'Bcc'})) {
        print MAIL "Bcc: $emailargs{'Bcc'}\n";
    }
    if (length($emailargs{'Subject'})) {
        print MAIL "Subject: $emailargs{'Subject'}\n";
    }

    foreach (@text) {
        print MAIL "$_\n";
    }

    print MAIL "\n";
    close MAIL;

    return 1;
}

#############################################################################
###                                 END OF SCRIPT
#############################################################################
1;

### RCS $Log: trigger.pl,v $
### RCS Revision 1.1  2004/03/12 00:41:07  cgay
### RCS Initial revision
### RCS
### RCS Revision 1.14  1998/11/11 17:07:15  keith
### RCS Test trigger change.
### RCS
# Revision 1.13  1998/11/11  17:04:38  keith
# Remove Glenn from recipients list.
#
# Revision 1.12  1998/02/24  11:01:30  tony
# Remove myself from the trigger
#
# Revision 1.11  1997/05/06  23:48:12  gsb
# me too
#
# Revision 1.10  1996/12/18  16:46:06  rthor
# restore
#
# Revision 1.9  1996/12/16  19:39:49  rthor
# Temp removal of mail during copyright update
#
# Revision 1.8  1996/11/21  18:53:19  gz
# Add myself
#
# Revision 1.7  1996/06/10  20:39:05  rthor
# Remove some folks
#
# Revision 1.6  1996/05/20  19:03:18  rthor
# remove jh
#
# Revision 1.5  1996/02/09  20:10:32  rthor
# add jh to mail recipients
#
# Revision 1.4  1996/02/07  12:00:26  haahr
# Remove myself from the change mailing list.
#
# Revision 1.3  1996/01/16  16:05:13  rthor
# add arthor
#
# Revision 1.2  1995/10/24  16:15:16  simong
# Remove self from mailto list
#
# Revision 1.1  1995/07/28  14:47:02  haahr
# new unit
# Snarfed template from HOPE!trigger.pl
#
