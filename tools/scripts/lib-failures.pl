#!/usr/local/bin/perl
#
# Perl script: lib-failures
#
# Optional Args: <Date>   ex.  <May22>  Default is today.
#                <-mail>  mails the failure chart to the people in 
#                         /u/dylan/tools/admin/build-uncles
#                <-save>  saves the failure chart in $summary_dir under the
#                         name failure_chart.$day
#
#   This script extracts all the library failures from logs it finds in 
# $log_dir, and produces a chart listing all failures and which machines 
# each failed on.  You should copy this file into your own /bin directory
# and customize the $log_dir and $summary_dir variables below.
#
# note:    The script /u/dylan/tools/scripts/compare-lib-fails.pl will mail
#        anybody a list of failures it finds during the night for each
#        machine.  Just add your username to the file:
#        '/u/admin/dylan/tools/admin/build-uncles' to recieve them.
#          This is very handy as it keeps you from having to ftp log files
#        from other sites.
#
#          Also, there is another script called extract-failures which will
#        extract out all of the library failures mail you got from 
#        compare-lib-fails.pl into files of their original log filenames.
#        once again you may want to copy it to your own /bin directory and
#        customize the $log_dir variable.
#
# Last modified by amit on July 28 1996 to incorporate this into nightly
# build so that the failure chart can be mailed to build-uncles. The
# source of the library files will now be under ~dylan/tools/admin/logs/
# lib-logs. For more details, see: ~dylan/tools/admin/README
#
# Last modified by amit on July 30 1996 to include the rsh part for other
# sites.
#########################################################################

$log_dir = "/u/dylan/tools/admin/logs/lib-logs";
$summary_dir = "$log_dir";
$day=`date +%h%d`; chop($day);
$site = `cat /u/dylan/local/site-name`; chop($site);
$save_file = "false";
$no_mail = "true";
$array_index = 0;

### Remote login to other 3 sites to copy their list of lib
### files into $log_dir
###

### In case a build fails there will be no such logfile, thus rcp
### generates a message to stderr. 2>&1 directs the stderr to stdout
### which is then captured in $junk

$junk=`rcp zaphod.long.harlequin.co.uk:$log_dir/*$day* $log_dir 2>&1`;
$junk=`rcp meteor.menlo.harlequin.com:$log_dir/*$day* $log_dir 2>&1`;
$junk=`rcp bright.seattle.harlequin.com:$log_dir/*$day* $log_dir 2>&1`;

while (<@ARGV>)
  {
    if ($_ eq "-mail")
      {
	$no_mail = "false";
      } 
    elsif ($_ eq "-save")
      {
        $save_file = "true";
      }
    else 
      {                         # assume its a date
	$day = $_;              # and put it in the wd
      }
  }

$summary = "$summary_dir/failure_chart.$day";

open (SMMRY, ">$summary");
opendir(LOGS,$log_dir);		# get all logs corresponding to $day

print SMMRY "\nFiles used:\n";
foreach $i (readdir(LOGS)) 
  {
    if (($i =~ /$day.log/) && ($i =~ /libraries/)) 
      {
        @file_list = split(/-/, $i);
        $machine = $file_list[1];

        print SMMRY "  $i\n";

	push (@logs,$i);

        if ($i =~ /sparc/)
          {
            push (@sparcs, $machine)
          }
        elsif ($i =~ /alpha/)
          {
            push (@alphas, $machine)
          }
        elsif ($i =~ /irix/)
          {
            push (@irixs, $machine)
          }
        elsif ($i =~ /alpha/)
          {
            push (@sparcs, $machine)
          }
      } 
  }
closedir(LOGS);

print SMMRY "\n";

unless (@logs)
  {
    print "\n  No library logs found for $day\n\n";
    exit 0;
  }

foreach $i (@logs)
  {
    &get_failures($i);
  }

$last = "NULL";
$total_count = 0;

foreach $i (sort(@total_failures))
  {
    if ($i ne $last)
      {
        $last = $i;
        push(@temp_list, $i);
        $total_count++;
      }
  }

@total_failures = @temp_list;

$max_size = 12;
foreach $i (@total_failures) 
  {
    $fail_size = &string_len($i);
    if ($fail_size > $max_size)
      {
        $max_size = $fail_size;
      }
  }
$max_size += 2;

$mach_line = "";
$plat_line = "";

$count = 0;
foreach $i (@sparcs)
  {
    push(@machines, $i);
    $mach_line = $mach_line . sprintf("%-10s", $i);
    if ($count == 0)
      {
        $plat_line = $plat_line . "Sparc:    ";
        $count++;
      }
    else
      {
        $plat_line = $plat_line . "          ";
      }
  }

$count = 0;
foreach $i (@alphas)
  {
    push(@machines, $i);
    $mach_line = $mach_line . sprintf("%-10s", $i);
    if ($count == 0)
      {
        $plat_line = $plat_line . "Alpha:    ";
        $count++;
      }
    else
      {
        $plat_line = $plat_line . "          ";
      }
  }

$count = 0;
foreach $i (@irixs)
  {
    push(@machines, $i);
    $mach_line = $mach_line . sprintf("%-10s", $i);
    if ($count == 0)
      {
        $plat_line = $plat_line . "Irix:     ";
        $count++;
      }
    else
      {
        $plat_line = $plat_line . "          ";
      }
  }

printf SMMRY "%$max_size s  $plat_line\n", " ";
printf SMMRY "%$max_size s  $mach_line\n", " ";

printf SMMRY "%$max_size s", " ";

foreach $i (@machines)
  {
    print SMMRY "----------";
  }

print SMMRY "-\n";
 
foreach $i (@total_failures) 
  {
    printf SMMRY "%-$max_size s", $i;

    foreach $machine (@machines)
      {
        $found = "false";
        $machine_count{$machine} = $machine_index_end{$machine} -
                                   $machine_index{$machine} + 1; 
        
        for ($location = $machine_index{$machine}; 
             $location <= $machine_index_end{$machine}; $location++)
          {
            if ($failures[$location] eq $i)
              {
                $found = "true";
              }
          }

        if ($found eq "true")
          {
            print SMMRY "|    x    ";
          }
        else
          {
            print SMMRY "|         ";
          }

      }

    print SMMRY "|\n";
  }

printf SMMRY "%$max_size s", " ";
foreach $i (@machines)
  {
    print SMMRY "----------";
  }

$total_string = " Total: $total_count";

printf SMMRY "-\n%-$max_size s", $total_string;

foreach $machine (@machines)
  {
    printf SMMRY "|  %3d    ", $machine_count{$machine};
  }

print SMMRY "|\n\n";



close (SMMRY);

if ($no_mail eq "true") 
  {
    system("cat $summary | more");
  } 
else
  {			 
    $subject = "\"Failure chart for $day\"";
    $uncles = `cat /u/dylan/tools/admin/build-uncles`;

    system ("cat $summary \| /usr/ucb/mail \-s $subject $uncles");
  }

if ($save_file eq "false")
  {
    `rm $summary`;
  }


############################################################

sub get_failures   #pass it a library log file
  {
    local (@file_array, $file);

    $file = @_[0];
    @file_list = split(/-/, $file);
    $machine = $file_list[1];

    $machine_index{$machine} = $array_index;  #assoc. array with index ref
    @file_array = `cat $log_dir/$file`;
    $length = sprintf("".@file_array);
    chop(@file_array);

    if (`grep -ni "too many failures" $log_dir/$file`)
      {
        $grep_str = `grep -ni "too many failures" $log_dir/$file`;
        @str_list = split(/:/, $grep_str);

        $j = $str_list[0];
      }
    elsif ($length > 150)
      {
        $j = $length;
      }
    else
      {
        $j = 0;
      }

    until ($j >= $length)
      {
        if ($file_array[$j] =~ /DYLAN::/)
          {
            $file_array[$j] =~ s/ //;
            $file_array[$j] =~ s/DYLAN:://;

            push(@failures, $file_array[$j]);
            $machine_index_end{$machine} = $array_index;
            $array_index++;

            push(@total_failures, $file_array[$j]);  #array for all failures
          }

        $j++;
        
      }

  }

############################################################

sub string_len
  {
    local($temp_str, $counter);

    $counter = 0;

    $temp_str = @_[0];

    while($temp_str)
      {
        chop($temp_str);
        $counter++;
      }

    sprintf("%d", $counter);
  }

#eof