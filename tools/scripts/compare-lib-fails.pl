#!/usr/local/bin/perl
#
# Script: compare-lib-fails.pl
#
# Required Args: <Host name>     ex.  <tigris> or <gamesh> ...
# Optional Args: <Date>          ex.  <May22>
#                <-nm>        turns off mailer.
#
#   this script finds todays and yesterdays library failures for a given 
#   host, and outputs to <STDOUT> new failures and successes.
#
#   note:  this script will look back no more than 30 days for a previous
#          library log file.
#
# Last modified by amit on July 28 1996 to emit a file containing the
# argument date's library failures under /logs. For more details look
# under ~dylan/tools/admin/README
#########################################################################

$no_mail = "false";
$log_dir = "/u/dylan/tools/admin/logs";
$host = shift(@ARGV);
$day_arg = "";

$platform = "unknown";

if ($host eq "")
  {
    print "  Error!  Improper usage.  Specify a host name.\n";
    exit 0;
  }

while (<@ARGV>)
  {
    if ($_ eq "-nm")
      {
	$no_mail = "true";
      } 
    else 
      {
	$day_arg = $_;
      }
  }

if ($day_arg eq "")
  {
    $today = `date +%h%d`;
    chop($today); 
  }
else
  {
    $today = $day_arg;
  }

$yesterday = $today;
$yesterday_file = "";

$back_days = 0;

while (($yesterday_file eq "") && ($back_days < 30))
  {
    opendir(LOGS, $log_dir);

    $yesterday = &subtract_day ($yesterday);
    $back_days++;

    foreach $i (readdir(LOGS))
      {
	$_ = $i;

	if (/libraries/ && /$today/ && /$host/)
	  {
            if (`/bin/grep -i 'too many failures' $log_dir/$i`)
	      {
	        $today_file = $i;
              }
	  }
	elsif (/libraries/ && /$yesterday/ && /$host/)
	  {
            if (`/bin/grep -i 'too many failures' $log_dir/$i`)
	      {
                $yesterday_file = $i;
              }
	  }
      }
    closedir(LOGS);
  }

if ($today_file ne "")
  {
    $platform = "";
    @split_file = split(/-/, $today_file);

    $word = -1;
    foreach $_ (@split_file)
      {
	$word++;
      }

    for ($i = 2; $i < $word; $i++)
      {
	$platform = $platform . $split_file[$i] . "-";
      }

    chop($platform);
  }

if ($yesterday_file eq "")
  {
    $yesterday = "DATE-NOT-FOUND";
  }

printf "\n    Comparing %s:%s libraries %s with %s\n\n", 
       $host, $platform, $today, $yesterday;

if ($today_file eq "")
  {
    print "  Error!  There is an error in the library log for $today\n\n";
    exit 0;
  }
elsif ($yesterday_file eq "")
  {
    print "  Error!  Could not find a library file before $today\n\n";
    exit 0;
  }

@todays_failures = &unique(&get_failures ($today_file));
@yesterdays_failures = &unique(&get_failures ($yesterday_file));


################################################################
####                                                        ####
####  This part of the script mails the list of failures to ####
####  the people listed in the uncles variable below.  Use  ####
####  the -nm option if this is not needed.                 ####
####                                                        ####
################################################################

#### The pathname of temp_file changed by amit so that this
#### info can be saved into logs. The mailing of the file is
#### still intact with the -nm option, I just moved the `if'
#### statement further down

    $temp_file = "$log_dir/lib-logs/$today_file";
    $subject = "$today_file";
    $uncles = `cat /u/dylan/tools/admin/build-uncles`;    

    open(FAILFILE, ">$temp_file");
    foreach $file (@todays_failures)
      {
        print FAILFILE "$file\n";
      }
    close(FAILFILE);

if ($no_mail eq "false")
  {
    system ("cat $temp_file \| /usr/ucb/mail \-s $subject $uncles");
  }


################################################################

$new_success = "0";
$new_failure = "0";

print "Libraries that no longer fail as of $yesterday:\n";

foreach $i (@yesterdays_failures)
  {
    $found = "false";

    foreach $j (@todays_failures)
      {
        if ($j eq $i)
          {
            $found = "true";
          }
      }

    if ($found eq "false")
      {
        print "  $i\n";
        $new_success++;
      }
  }

if ($new_success == 0)
  {
    print "  NONE\n";
  }

print "\nLibraries that now fail as of $today:\n";

foreach $i (@todays_failures)
  {
    $found = "false";

    foreach $j (@yesterdays_failures)
      {
        if ($j eq $i)
          {
            $found = "true";
          }
      }

    if ($found eq "false")
      {
        print "  $i\n";
        $new_failures++;
      }
  }

if ($new_failures == 0)
  {
    print "  NONE\n";
  }

printf "\nNew Successes: %d\n", $new_success;
printf "New Failures: %d\n\n", $new_failures;

############################################################

sub subtract_day  # pass it date to be subtracted return subtracted date
  {
    local ($date, @num_days, @months, %month_numbers, 
           $day, $month, $month_num);

    $date = @_[0];
    @num_days = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
    @months = ("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
	       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");
    %month_numbers = ("Jan", "0", "Feb", "1", "Mar", "2", "Apr", "3", 
                      "May", "4", "Jun", "5", "Jul", "6", "Aug", "7",
                      "Sep", "8", "Oct", "9", "Nov", "10", "Dec", "11");

    $day = $date;
    $day =~ tr/a-zA-Z/ /;
    $month = $date;
    $month =~ tr/0-9/ /;
    $month =~ s/\W.*//;
    $month_num = $month_numbers{$month};

    if ($day == 1)
      {
	if ($month_num == 0)
	  {
	    $month_num = 11;
	  }
	else
	  {
	    $month_num--;
	  }

	$day = $num_days[month_num];
      }
    else
      {
	$day--;
      }

    $yesterday = sprintf("%s%02d", $months[$month_num], $day);
  }

############################################################

sub get_failures   #pass it a library log file
  {
    local (@file_array, $file, $i, @failures);

    $file = @_[0];
    $i = 0;
    @file_array = `cat $log_dir/$file`;
    $length = sprintf("".@file_array);
    chop(@file_array);

    if (`grep -ni "too many failures" $log_dir/$file`)
      {
        $grep_str = `grep -ni "too many failures" $log_dir/$file`;
        @str_list = split(/:/, $grep_str);

        $i = $str_list[0];
      }
    else
      {
        $i = $length;
      }

    until ($i >= $length)
      {
        if ($file_array[$i] =~ /DYLAN::/)
          {
            $file_array[$i] =~ s/ //;
            push(@failures, $file_array[$i])
          }

        $i++;
        
      }
    
    @failures = sort(@failures);
  }


############################################################

sub unique
  {
    local($last, @list, @temp_list, $member);

    @list = @_;
    $last = "NULL";

    foreach $member (sort(@list))
      {
	if ($member ne $last)
	  {
	    $last = $member;
	    push(@temp_list, $member);
	  }
      }
    @list = @temp_list;
  }


#eof

