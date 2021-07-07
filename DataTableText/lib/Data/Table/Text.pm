#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
#-------------------------------------------------------------------------------
# Write data in tabular text format.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc, 2016-2020
#-------------------------------------------------------------------------------
# podDocumentation
# cd /home/phil/perl/cpan/DataTableText/; perl Build.PL && perl Build test && sudo perl Build install
# To escape an open parenthesis in a regular expression use: \x28, for close use: \x29
# E for exportable methods
# write binary data without complaints about wide characters
# formatTableHH hash with sub hash of {} fails to print see svgToDita
# runInParallel - processing statistics
# formatTable should optionally clear left columns identical to previous line
# checkKeys information should be formatted so it can be referred to in sub descriptions
# updateDocumentation - mark synopsis tests with #S an place in synopsis
package Data::Table::Text;
use v5.26;
our $VERSION = 20210629;                                                        # Version
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess carp cluck);
use Cwd;
use Digest::MD5 qw(md5_hex);
use File::Path qw(make_path);
use File::Glob qw(:bsd_glob);
use File::Temp qw(tempfile tempdir);
use POSIX qw(:sys_wait_h strftime);                                             # http://www.cplusplus.com/reference/ctime/strftime/
use Data::Dump qw(dump);
use IO::Socket::UNIX;
use JSON;
use MIME::Base64;
use Scalar::Util qw(blessed reftype looks_like_number);
use Storable qw(store retrieve dclone);
use Time::HiRes qw(time gettimeofday);
use B;
use utf8;

#D1 Time stamps                                                                 # Date and timestamps as used in logs of long running commands.

sub dateTimeStamp                                                               #I Year-monthNumber-day at hours:minute:seconds
 {strftime('%Y-%m-%d at %H:%M:%S', localtime)
 }

sub dateTimeStampName                                                           # Date time stamp without white space.
 {strftime('_on_%Y_%m_%d_at_%H_%M_%S', localtime)
 }

sub dateStamp                                                                   # Year-monthName-day
 {strftime('%Y-%b-%d', localtime)
 }

sub versionCode                                                                 # YYYYmmdd-HHMMSS
 {strftime('%Y%m%d-%H%M%S', localtime)
 }

sub versionCodeDashed                                                           # YYYY-mm-dd-HH:MM:SS
 {strftime('%Y-%m-%d-%H:%M:%S', localtime)
 }

sub timeStamp                                                                   # hours:minute:seconds
 {strftime('%H:%M:%S', localtime)
 }

sub microSecondsSinceEpoch                                                      # Micro seconds since unix epoch.
 {my ($s, $u) = gettimeofday();
  $s*1e6 + $u
 }

#D1 Command execution                                                           # Various ways of processing commands and writing results.

sub ddd(@)                                                                      # Dump data
 {my (@data) = @_;                                                              # Messages
  my $m = dump(@_);                                                             # Dump data

  unless(&onAws)                                                                # Not on AWS
   {my ($p, $f, $l) = caller();
    my $L = " at $f line $l";                                                   # Message source location
    if ($m  =~ m(\A(.*?)\n(.*\Z))s)                                             # Move and align location to make messages more readable
     {$m = pad($1, 80)."$L\n$2";                                                # Location at end of first line
     }
    else
     {$m = pad($m, 80).$L                                                       # Location at end of only line
     }
   }

  say STDERR $m;                                                                # Say message
  $m                                                                            # Return message produced
 }

sub fff($$@)                                                                    # Confess a message with a line position and a file that Geany will jump to if clicked on.
 {my ($line, $file, @m) = @_;                                                   # Line, file, messages

  my $m = join ' ', @m;                                                         # Time stamp each message
  return unless $m =~ m(\S)s;
# $m =~ s(\n) ( )gs;
  $m .= " called at $file line $line";
  confess "$m\n";                                                               # Confess
 }

my $mmm = 0;                                                                    # Time of last message

sub lll(@)                                                                      # Log messages with a time stamp and originating file and line number.
 {my (@messages) = @_;                                                          # Messages
  my @m = map {defined ? $_ : q(undef)} @_;
  return unless (join '', @m) =~ m(\S)s;

  my $m = join '', map {m(\s\Z) ? $_ : qq($_ )} timeStamp, @m;                  # Time stamp each message
  $mmm = time;                                                                  # Update time of last message

  unless(&onAws)                                                                # Not on AWS
   {my ($p, $f, $l) = caller();
    my $L = " at $f line $l";                                                   # Message source location
    if ($m  =~ m(\A(.*?)\n(.*\Z))s)                                             # Move and align location to make messages more readable
     {$m = pad($1, 80)."$L\n$2";                                                # Location at end of first line
     }
    else
     {$m = pad($m, 80).$L                                                       # Location at end of only line
     }
   }

  say STDERR $m;                                                                # Say message
  $m                                                                            # Return message produced
 }

sub mmm(@)                                                                      # Log messages with a differential time in milliseconds and originating file and line number.
 {my (@messages) = @_;                                                          # Messages
  my (@m) = map {defined ? $_ : q(undef)} @_;

  my $t = $mmm ? sprintf("%8.3f", time - $mmm) : timeStamp;                     # Time at start, delta there after.
          $mmm = time;                                                          # Update time of last message

  my $m = join '', map {m(\s\Z) ? $_ : qq($_ )} $t, @_ ;                        # Time stamp each message

  unless(&onAws)                                                                # Not on AWS
   {my ($p, $f, $l) = caller();
    my $L = " at $f line $l";                                                   # Message source location
    if ($m  =~ m(\A(.*?)\n(.*\Z))s)                                             # Move and align location to make messages more readable
     {$m = pad($1, 80)."$L\n$2";                                                # Location at end of first line
     }
    else
     {$m = pad($m, 80).$L                                                       # Location at end of only line
     }
   }

  say STDERR $m;                                                                # Say message
  $m                                                                            # Return message produced
 }

sub xxx(@)                                                                      # Execute a shell command optionally checking its response. The command to execute is specified as one or more strings which are joined together after removing any new lines. Optionally the last string can be a regular expression that is used to test any non blank output generated by the execution of the command: if the regular expression fails the command and the command output are printed, else it is suppressed as being uninteresting. If such a regular expression is not supplied then the command and its non blank output lines are always printed.
 {my (@cmd) = @_;                                                               # Command to execute followed by an optional regular expression to test the results
  @cmd or confess "No command\n";                                               # Check that there is a command to execute
  $_ or confess "Missing command component\n" for @cmd;                         # Check that there are no undefined command components
  my $success = $cmd[-1];                                                       # Error check if present
  my $check = ref($success) =~ /RegExp/i;                                       # Check for error check
  pop @cmd if $check;                                                           # Remove check from command
  my $cmd = join ' ', @cmd;                                                     # Command to execute
  say STDERR $cmd unless $check;                                                # Print the command unless there is a check in place
  my $response = eval {qx($cmd 2>&1)} // "No such command";                     # Execute command
  $response =~ s/\s+\Z//s;                                                      # Remove trailing white space from response
  say STDERR $response if $response and !$check;                                # Print non blank error message
  confess $response if $response and $check and $response !~ m/$success/;       # Error check if an error checking regular expression has been supplied
  confess $response if $response and $response =~ m/Syntax error:.*unexpected/; # Check for a particularly annoying error
  $response
 } # xxx

sub xxxr($;$)                                                                   #I Execute a command B<$cmd> via bash on the server whose ip address is specified by B<$ip> or returned by L<awsIp>. The command will be run using the userid listed in F<.ssh/config>
 {my ($cmd, $ip) = @_;                                                          # Command string, optional ip address
  my $i = $ip // &awsIp;                                                        # Ip address
  return undef unless confirmHasCommandLineCommand(q(ssh));                     # Confirm we have ssh
  my $c = qq(ssh $i "$cmd 2>&1");                                                # Command
  lll $c;
  my $r = eval {qx($c)};                                                        # Execute command remotely
  lll $r if $r;
  $r
 } # xxxr

sub yyy($)                                                                      # Execute a block of shell commands line by line after removing comments - stop if there is a non zero return code from any command.
 {my ($cmd) = @_;                                                               # Commands to execute separated by new lines
  for(split /\n/, $cmd)                                                         # Split commands on new lines
   {s(#.*\Z)()gs;                                                               # Remove comments
    next if !$_ or m(\A\s*\Z);                                                  # Skip blank lines
    lll $_;                                                                     # Say command
    print STDERR $_ for qx($_);                                                 # Execute command
    say STDERR '';
   }
 } # yyy

sub zzz($;$$$)                                                                  # Execute lines of commands after replacing new lines with && then check that the pipeline execution results in a return code of zero and that the execution results match the optional regular expression if one has been supplied; confess() to an error if either check fails. To execute remotely, add "ssh ... 'echo start" as the first line and "echo end'" as the last line with the commands to be executed on the lines in between.
 {my ($cmd, $success, $returnCode, $message) = @_;                              # Commands to execute - one per line with no trailing &&, optional regular expression to check for acceptable results, optional regular expression to check the acceptable return codes, message of explanation if any of the checks fail
  $cmd or confess "No command\n";                                               # Check that there is a command to execute
  my @c;                                                                        # Commands
  for(split /\n/, $cmd)                                                         # Split commands on new lines
   {s(#.*\Z)()gs;                                                               # Remove comments
    next unless m(\S);                                                          # Skip blank lines
    push @c, $_;                                                                # Save command
   }
  my $c = join ' && ', @c;                                                      # Command string to execute
  my $r = qx($c 2>&1);                                                          # Execute command
  my $R = $?;
  $r =~ s/\s+\Z//s;                                                             # Remove trailing white space from response

  confess "Error:\n".                                                           # Check the error code and results
    ($message ? "$message\n" : '').                                             # Explanation if supplied
    "$cmd\n".                                                                   # Commands being executed
    "Return code: $R\n".                                                        # Return code
    "Result:\n$r\n" if                                                          # Output from commands so far
    $R && (!$returnCode or $R !~ /$returnCode/) or                              # Return code not zero and either no return code check or the return code checker failed
    $success && $r !~ m/$success/s;                                             # Results check failed
  $r
 } # zzz

sub execPerlOnRemote($;$)                                                       #I Execute some Perl B<$code> on the server whose ip address is specified by B<$ip> or returned by L<awsIp>.
 {my ($code, $ip) = @_;                                                         # Code to execute, optional ip address
  my $file = writeFile(fpe(&temporaryFolder, qw(code pl)),  $code);             # Create code file
  copyFileToRemote($file);                                                      # Copy code to server
  say STDERR xxxr(qq(perl $file 2>&1));                                         # Execute code on server and return its output
 }

sub parseCommandLineArguments(&$;$)                                             # Call the specified B<$sub> after classifying the specified array of [arguments] in B<$args> into positional and keyword parameters. Keywords are always preceded by one or more B<-> and separated from their values by B<=>. $sub([$positional], {keyword=>value}) will be called  with a reference to an array of positional parameters followed by a reference to a hash of keywords and their values. The value returned by $sub will be returned to the caller. The keywords names will be validated if B<$valid> is either a reference to an array of valid keywords names or a hash of {valid keyword name => textual description}. Confess with a table of valid keywords definitions if $valid is specified and an invalid keyword argument is presented.
 {my ($sub, $args, $valid) = @_;                                                # Sub to call, list of arguments to parse, optional list of valid parameters else all parameters will be accepted

  my %valid = sub                                                               # Valid keywords
    {return () unless $valid;                                                   # No keywords definitions
     return map {lc($_)=>0} @$valid if ref($valid) =~ m(array)is;               # Keyword names as an array but with no explanation
     %$valid                                                                    # Hash of keyword name=>explanation
    }->();

  my %keywords;
  my @positionals;
  for my $arg(@$args)                                                           # Each arg
   {if ($arg =~ m/\A-+(\S+?)\s*(=\s*(.+)\s*)?\Z/)                               # Keyword parameters with leading and trailing blanks removed
     {if ($valid and !defined($valid{lc($1)}))                                  # Validate keyword name
       {my @s;
        for my $k(sort keys %valid)                                             # Create a table of valid keywords
         {if (my $v = $valid{$k})
           {push @s, [$k, $v];
           }
          else
           {push @s, [$k];
           }
         }
        if (@s)                                                                 # Format error message
         {my $s = formatTable(\@s, [qw(Keyword Description)]);
          confess "Invalid parameter: $arg\nValid keyword parameters are:\n$s\n";
         }
        else
         {confess "Invalid parameter: $arg\n";
         }
       }
      $keywords{lc($1)} = $3;                                                   # Save valid keyword parameter
     }
    else                                                                        # Positional parameter
     {push @positionals, $arg;
     }
   }
  $sub->([@positionals], {%keywords})
 } # parseCommandLineArguments

sub call(&;@)                                                                   # Call the specified B<$sub> in a separate child process, wait for it to complete, then copy back the named B<@our> variables from the child process to the calling parent process effectively freeing any memory used during the call.
 {my ($sub, @our) = @_;                                                         # Sub to call, names of our variable names with preceding sigils to copy back
  my ($package)   = caller;                                                     # Caller's package
  my $folder      = &temporaryFolder;                                           # Folder for returned data files
  my $pid  = fork;                                                              # Fork
  if (!defined($pid))                                                           # Fork failed
   {confess "Unable to fork!\n";
   }
  elsif ($pid == 0)                                                             # Fork - child
   {&$sub;                                                                      # Execute the sub
    my @save = '';                                                              # Code to copy back our variables
    for my $o(@our)                                                             # Each variable
     {my ($sigil, $var) = $o =~ m(\A(.)(.+)\Z)s;                                # Sigil, variable name
      my $our  = $sigil.$package.q(::).$var;                                    # Add caller's package to variable name
      my $char = ord($sigil);                                                   # Differentiate between variables with the same type but different sigils
      my $file = fpe($folder, qq(${$}$var$char), q(data));                      # File for this variable
      push @save, <<END                                                         # Save each our variable in a file
store \\$our, q($file);
END
     }
    my $save = join "\n", @save;                                                # Perl code to store our variables
    eval $save;                                                                 # Evaluate code to store our variables
    confess $@ if $@;                                                           # Confess any errors
    exit;                                                                       # End of child process
   }
  else                                                                          # Fork - parent
   {waitpid $pid,0;                                                             # Wait for child
    my @save = '';                                                              # Code to retrieve our variables
    my @file;                                                                   # Transfer files
    for my $o(@our)
     {my ($sigil, $var) = $o =~ m(\A(.)(.+)\Z)s;                                # Sigil, variable name
      my $our  = $sigil.$package.q(::).$var;                                    # Add caller's package to variable name
      my $char = ord($sigil);                                                   # Differentiate between variables with the same type but different sigils
      my $file = fpe($folder, qq($pid$var$char), q(data));                      # Save file
      push @save, <<END;                                                        # Perl code to retrieve our variables
$our = ${sigil}{retrieve q($file)};
END
      push @file, $file;                                                        # Remove transfer files
     }
    my $save = join "\n", @save;
    eval $save;                                                                 # Evaluate perl code
    my $r = $@;                                                                 # Save result
    clearFolder($folder, scalar(@our)+1);                                        # Remove transfer files
    confess "$r\n$save\n" if $r;                                                # Confess to any errors
   }
 } # call

#D1 Files and paths                                                             # Operations on files and paths.
#D2 Statistics                                                                  # Information about each file.

sub fileSize($)                                                                 # Get the size of a B<$file> in bytes.
 {my ($file) = @_;                                                              # File name
  return (stat($file))[7] if -e $file;                                          # Size if file exists
  undef                                                                         # File does not exist
 }

sub fileLargestSize(@)                                                          # Return the largest B<$file>.
 {my (@files) = @_;                                                             # File names
  my ($l) = map {$$_[1]} sort {$$b[0] <=> $$a[0]}                               # Largest file
            map {[fileSize($_)//0, $_]} @files;
  $l
 }

sub folderSize($)                                                               # Get the size of a B<$folder> in bytes.
 {my ($folder) = @_;                                                            # Folder name
  return undef unless -d $folder;                                               # Not a folder
  return undef unless confirmHasCommandLineCommand(q(du));                      # Confirm we have the disk used command
  my $s = qx(du -s $folder);                                                    # Folder size
  $s =~ s(\s.*\Z) ()gsr                                                         # nnnn folder
 }

sub fileMd5Sum($)                                                               # Get the Md5 sum of the content of a B<$file>.
 {my ($file) = @_;                                                              # File or string
  if ($file !~ m(\0|\n|\A\.|\A\/\Z)s and -e $file)                              # From file - this is not entirely satisfactory.
   {my $s = readBinaryFile($file);
    return md5_hex($s);
   }
  else                                                                          # From string - convoluted but necessary to avoid L<utf8> problems
   {cluck "Deprecated: use stringMd5Sum instead";
    return stringMd5Sum($file);
   }
 }

sub guidFromMd5($)                                                              # Create a guid from an md5 hash.
 {my ($m) = @_;                                                                 # Md5 hash
  length($m) == 32 or confess "Not an md5 hash: ". ($m//"undef");
  join '-', q(GUID), substr($m, 0, 8),  substr($m, 8, 4), substr($m, 12, 4),    # Uppercase might be needed to meet the strictest definition of a GUID
                     substr($m, 16, 4), substr($m, 20);
 }

sub md5FromGuid($)                                                              # Recover an md5 sum from a guid.
 {my ($G) = @_;                                                                 # Guid
  length($G) >= 41 or confess "Incorrect length for guid: $G";                  # Check guid
  my $g = substr($G, 0, 41);
  return $g =~ s(guid|-) ()igsr if $g =~ m(\AGUID-[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}\Z)is;
  confess "Incorrect format for guid: $g";
 }

sub guidFromString($)                                                           # Create a guid representation of the L<md5> of the content of a string.
 {my ($string) = @_;                                                            # String
  guidFromMd5 &stringMd5Sum($string);
 }

sub fileModTime($)                                                              # Get the modified time of a B<$file> as seconds since the epoch.
 {my ($file) = @_;                                                              # File name
  (stat($file))[9] // 0
 }

sub fileOutOfDate(&$@)                                                          # Calls the specified sub B<$make> for each source file that is missing and then again against the B<$target> file if any of the B<@source> files were missing or the $target file is older than any of the @source files or if the target does not exist. The file name is passed to the sub each time in $_. Returns the files to be remade in the order they should be made.
 {my ($make, $target, @source) = @_;                                            # Make with this sub, target file, source files
  my $exists = -e $target;                                                      # Existence of target
  my @missing = grep {!-e $_} @source;                                          # Missing files that do not exist will need to be remade
  push @missing, $target unless $exists and !@missing;                          # Add the target if there were missing files
  if (!@missing)                                                                # If there were no missing files that forced a remake, then check for a source file younger than the target that would force a remake of the target
   {my $t = fileModTime($target);                                               # Time of target
    if (grep {-e $$_[0] and $$_[0] ne $target and $$_[1] > $t}                  # Target will have to be remade if there are younger source files
        map {[$_, fileModTime($_)]}
        @source)
     {@missing = $target;
     }
   }
  my %remade;                                                                   # Files that have been remade
  my @order;                                                                    # Files that have been remade in make order
  for(@missing)
   {&$make, push @order, $_ unless $remade{$_}++;                               # Make each missing file once and then the target file
   }
  @order                                                                        # Return a list of the files that were remade
 } # fileOutOfDate

sub firstFileThatExists(@)                                                      # Returns the name of the first file from B<@files> that exists or B<undef> if none of the named @files exist.
 {my (@files) = @_;                                                             # Files to check
  for(@files)
   {return $_ if -e $_;
   }
  undef                                                                         # No such file
 } # firstFileThatExists

sub fileInWindowsFormat($)                                                      # Convert a unix B<$file> name to windows format
 {my ($file) = @_;                                                              # File
  $file =~ s(\/) (\\)gsr
 }

#D2 Components                                                                  # File names and components.

#D3 Fusion                                                                      # Create file names from file name components.

sub onWindows                                                                   #P Are we on windows
 {$^O =~ m(MSWin32)
 }

sub filePathSeparatorChar                                                       #P File path separator
 {onWindows ? '\\' : '/';
 }

sub denormalizeFolderName($)                                                    #P Remove any trailing folder separator from a folder name.
 {my ($name) = @_;                                                              # Folder name
  $name =~ s([\/\\]+\Z) ()gsr;
 }

sub renormalizeFolderName($)                                                    #P Normalize a folder name by ensuring it has a single trailing directory separator.
 {my ($name) = @_;                                                              # Name
  ($name =~ s([\/\\]+\Z) ()gsr).filePathSeparatorChar;                          # Put a trailing / on the folder name
 }

sub prefferedFileName($)                                                        #P Normalize a file name
 {my ($name) = @_;                                                              # Name
  onWindows ? $name =~ s([\/\\]+) (\\)gsr :
              $name =~ s([\/\\]+)  (/)gsr ;
 }

sub filePath(@)                                                                 # Create a file name from a list of  names. Identical to L<fpf|/fpf>.
 {my (@file) = @_;                                                              # File name components
  defined($_) or confess "Missing file component\n" for @file;                  # Check that there are no undefined file components
  my @components = grep {$_} map {denormalizeFolderName($_)} @file;             # Skip blank components
  return '' unless @components;                                                 # No components resolves to '' rather than '/'
  prefferedFileName join '/', @components;                                      # Join separate components
 }

sub filePathDir(@)                                                              # Create a folder name from a list of  names. Identical to L<fpd|/fpd>.
 {my (@file) = @_;                                                              # Directory name components
  my $file = filePath(@_);
  return '' unless $file;                                                       # No components resolves to '' rather than '/'
  renormalizeFolderName($file)                                                  # Normalize with trailing separator
 }

sub filePathExt(@)                                                              #I Create a file name from a list of  names the last of which is assumed to be the extension of the file name. Identical to L<fpe|/fpe>.
 {my (@File) = @_;                                                              # File name components and extension
  my @file = grep{defined and /\S/} @_;                                         # Remove undefined and blank components
  @file > 1 or confess "At least two non blank file name components required\n";
  my $x = pop @file;
  my $n = pop @file;
  my $f = "$n.$x";
  return $f unless @file;
  filePath(@file, $f)
 }

BEGIN{*fpd=*filePathDir}
BEGIN{*fpe=*filePathExt}
BEGIN{*fpf=*filePath}

#D3 Fission                                                                     # Get file name components from a file name.

sub fp($)                                                                       # Get the path from a file name.
 {my ($file) = @_;                                                              # File name
  $file or confess "File required";
  if (onWindows)
   {return '' unless $file =~ m(\\);                                            # Must have a \ in it else no path
    $file =~ s([^\\]*\Z) ()gsr
   }
  else
   {return '' unless $file =~ m(/);                                             # Must have a / in it else no path
    $file =~ s([^/]*\Z) ()gsr
   }
 }

sub fpn($)                                                                      # Remove the extension from a file name.
 {my ($file) = @_;                                                              # File name
  $file or confess "File required";
  if (onWindows)
   {return '' unless $file =~ m(\\);                                            # Must have a \ in it else no path
   }
  else
   {return '' unless $file =~ m(/);                                             # Must have a / in it else no path
   }
  $file =~ s(\.[^.]+?\Z) ()gsr
 }

sub fn($)                                                                       #I Remove the path and extension from a file name.
 {my ($file) = @_;                                                              # File name
  $file or confess "File required";
  if (onWindows)
   {$file =~ s(\A.*\\) ()gsr =~ s(\.[^.]+?\Z) ()gsr
   }
  else
   {$file =~ s(\A.*/) ()gsr =~ s(\.[^.]+?\Z) ()gsr
   }
 }

sub fne($)                                                                      # Remove the path from a file name.
 {my ($file) = @_;                                                              # File name
  $file or confess "File required";
  if (onWindows)
   {$file =~ s(\A.*\\) ()gsr;
   }
  else
   {$file =~ s(\A.*/) ()gsr;
   }
 }

sub fe($)                                                                       # Get the extension of a file name.
 {my ($file) = @_;                                                              # File name
  $file or confess "File required";
  return '' unless $file =~ m(\.)s;                                             # Must have a period
  my $f = $file =~ s(\.[^.]*?\Z) ()gsr;
  substr($file, length($f)+1)
 }

sub checkFile($)                                                                # Return the name of the specified file if it exists, else confess the maximum extent of the path that does exist.
 {my ($file) = @_;                                                              # File to check
  unless(-e $file)
   {confess "Can only find the prefix (below) of the file (further below):\n".
      matchPath($file)."\n$file\n";
   }
  $file
 }

sub quoteFile($)                                                                # Quote a file name.
 {my ($file) = @_;                                                              # File name
  $file or confess "Undefined file to quote";
  $file =~ s(")  (\\\")gs;
  $file =~ s(\$) (\\\$)gs;
  qq(\"$file\")
 }

sub removeFilePrefix($@)                                                        # Removes a file B<$prefix> from an array of B<@files>.
 {my ($prefix, @files) = @_;                                                    # File prefix, array of file names
  my @f = map {s(\A$prefix) ()r} @files;
  return $f[0] if @f == 1 and !wantarray;                                       # Special case of wanting one file in scalar context
  @f
 }

sub swapFilePrefix($$;$)                                                        # Swaps the start of a B<$file> name from a B<$known> name to a B<$new> one if the file does in fact start with the $known name otherwise returns the original file name as it is. If the optional $new prefix is omitted then the $known prefix is removed from the $file name.
 {my ($file, $known, $new) = @_;                                                # File name, existing prefix, optional new prefix defaults to q()
  my $L = length($file);
  my $l = length($known);
  if ($L >= $l)
   {if (substr($file, 0, $l) eq $known)
     {return ($new//q()).substr($file, $l);
     }
    return $file;
   }
  confess "Known $l longer than file name $L:\n$known\n$file\n";
 } # swapFilePrefix

sub setFileExtension($;$)                                                       # Given a B<$file>, change its extension to B<$extension>. Removes the extension if no $extension is specified.
 {my ($file, $extension) = @_;                                                  # File name, optional new extension
  return $file =~ s(\.\w+\Z) ()sr unless defined $extension;                    # Remove extension
  my $ext = $extension =~ s(\A\.+) ()gsr;                                       # Remove leading dots
  return $file                    unless $ext;                                  # No extension after dot removal
  ($file =~ s(\.\w+\Z) ()gsr).q(.).$ext;                                        # Change extension
 } # setFileExtension

sub swapFolderPrefix($$$)                                                       # Given a B<$file>, swap the folder name of the $file from B<$known> to B<$new> if the file $file starts with the $known folder name else return the $file as it is.
 {my ($file, $known, $new) = @_;                                                # File name, existing prefix, new prefix
  swapFilePrefix($file, fpd($known), fpd($new));
 } # swapFolderPrefix

sub fullyQualifiedFile($;$)                                                     # Check whether a B<$file> name is fully qualified or not and, optionally, whether it is fully qualified with a specified B<$prefix> or not.
 {my ($file, $prefix) = @_;                                                     # File name to test, file name prefix
  return $file =~ m(\A/)s unless $prefix;                                       # Check against /
  index($file, $prefix) == 0                                                    # Check against  supplied prefix
 } # fullyQualifiedFile

sub fullyQualifyFile($)                                                         # Return the fully qualified name of a file.
 {my ($file) = @_;                                                              # File name
  return $file if fullyQualifiedFile($file);                                    # File is already fully qualified
  absFromAbsPlusRel(&currentDirectory, $file);                                  # Fully qualify file name
 } # fullyQualifyFile

sub removeDuplicatePrefixes($)                                                  # Remove duplicated leading directory names from a file name.
 {my ($file) = @_;                                                              # File name
  return $file unless $file =~ m(/)s;                                           # No path to deduplicate
  return $file if $file =~ m(\A[/.]);                                           # Later
  my ($p, @p) = split m(/), $file;
  shift @p while @p && $p[0] eq $p;
  join "/", $p, @p;
 } # removeDuplicatePrefixes

sub containingFolderName($)                                                     # The name of a folder containing a file
 {my ($file) = @_;                                                              # File name
  my @p = split m(/), $file;
  return $p[-2] if @p > 1;
  confess "No folder name provided";
 } # containingFolderName
#D2 Position                                                                    # Position in the file system.

sub currentDirectory                                                            # Get the current working directory.
 {renormalizeFolderName(getcwd)
 } # currentDirectory

sub currentDirectoryAbove                                                       # Get the path to the folder above the current working folder.
 {my $path = currentDirectory;
  my @path = split m(/)s, $path;
  shift @path if @path and $path[0] =~ m/\A\s*\Z/;
  @path or confess "No directory above\n:".currentDirectory, "\n";
  pop @path;
  my $r = shift @path;
  filePathDir("/$r", @path);
 } # currentDirectoryAbove

sub parseFileName($)                                                            # Parse a file name into (path, name, extension) considering .. to be always part of the path and using B<undef> to mark missing components.  This differs from (fp, fn, fe) which return q() for missing components and do not interpret . or .. as anything special
 {my ($file) = @_;                                                              # File name to parse
  defined($file) or confess "File required";
  return ($file) if $file =~ m{\/\Z}s or $file =~ m/\.\.\Z/s;                   # Its a folder
  if ($file =~ m/\.[^\/]+?\Z/s)                                                 # The file name has an extension
   {if ($file =~ m/\A.+[\/]/s)                                                  # The file name has a preceding path
     {my @f = $file =~ m/(\A.+[\/])([^\/]*)\.([^\/]+?)\Z/s;                     # File components
      return @f;
     }
    else                                                                        # There is no preceding path
     {my @f = $file =~ m/(\A.+)\.([^\/]+?)\Z/s;                                 # File components
      return (undef, @f)
     }
   }
  else                                                                          # The file name has no extension
   {if ($file =~ m/\A.+[\/]/s)                                                  # The file name has a preceding path
     {my @f = $file =~ m/(\A.+\/)([^\/]+?)\Z/s;                                 # File components
      return @f;
     }
    elsif ($file =~ m/\A[\/]./s)                                                # The file name has a single preceding /
     {return (q(/), substr($file, 1));
     }
    elsif ($file =~ m/\A[\/]\Z/s)                                               # The file name is a single /
     {return (q(/));
     }
    else                                                                        # There is no preceding path
     {return (undef, $file)
     }
   }
 } # parseFileName

sub fullFileName                                                                # Full name of a file.
 {my ($file) = @_;                                                              # File name
  return $file if fullyQualifiedFile $file;                                     # Already a full file name
  absFromAbsPlusRel(currentDirectory, $file);                                   # Relative to current folder
 } # fullFileName

sub relFromAbsAgainstAbs($$)                                                    #I Relative file from one absolute file B<$a> against another B<$b>.
 {my ($a, $b) = @_;                                                             # Absolute file to be made relative, against this absolute file.

  my $m = length($a) < length($b) ? length($a) : length($b);                    # Shortest length

  $a =~ m(\A/) or confess "$a is not absolute";                                 # Require absolute file names
  $b =~ m(\A/) or confess "$b is not absolute";
  $b =~ s([^/]+\Z) ();                                                          # Make the against file into a folder

  my $s = 0;                                                                    # Position of last matching /

  for my $i(1..$m-1)                                                            # Locate first non matching character - the first character of both file names is / which matches
   {if (substr($a, $i, 1) ne substr($b, $i, 1))                                 # First mismatch
     {my $u = 0;                                                                # Number of jumps up from $b
      my $p = $s;                                                               # Last /
      ++$u while(($p = index($b, q(/), $p+1)) > -1);                            # Number of / to jump up
      return ((q(../) x $u).substr($a, $s+1)) =~ s(\A\Z) (./)gsr;               # Jumps up from $b plus remainder of $a avoiding a blank result
     }
    elsif (substr($a, $i, 1) eq q(/))                                           # Agree up to this / at least
     {$s = $i;
     }
   }
  my $u = 0;                                                                    # Number of jumps up from $b
  my $p = $s;                                                                   # Last /
  ++$u while(($p = index($b, q(/), $p+1)) > -1);                                # Number of / to jump up
  ((q(../) x $u).substr($a, $s+1)) =~ s(\A\Z) (./)gsr;                          # Jumps up from $b plus remainder of $a avoiding a blank result
 }

sub absFromAbsPlusRel($$)                                                       #I Absolute file from an absolute file B<$a> plus a relative file B<$r>. In the event that the relative file $r is, in fact, an absolute file then it is returned as the result.
 {my ($a, $r) = @_;                                                             # Absolute file, relative file

  return $r if $r =~ m(\A/);                                                    # Return absolute file if such is supplied
  $a =~ m(\A/) or confess "$a is not absolute";                                 # Require absolute file name
  $a =~ s([^/]+\Z) ();                                                          # Make the absolute file into a folder
  $r =~ s(\A\./)   ();                                                          # Remove any leading ./ from relative file
  $r =~ s(\.\.\Z)  (../);                                                       # Make trailing .. into a folder

  my $R = qq($a$r);                                                             # Combine and ...
  undef while $R =~ s([^/]+/\.\./) ();                                          # Squeeze out jumps

  $R
 }

sub absFile($)                                                                  # Return the name of the given file if it a fully qualified file name else returns B<undef>. See: L<fullyQualifiedFile> to check the initial prefix of the file name as well.
 {my ($file) = @_;                                                              # File to test
  return $file if $file =~ m(\A/);
  undef
 }

sub sumAbsAndRel(@)                                                             # Combine zero or more absolute and relative names of B<@files> starting at the current working folder to get an absolute file name.
 {my (@files) = @_;                                                             # Absolute and relative file names
  unshift @files, currentDirectory;
  while(@files > 1)
   {my $a = shift @files;
    my $b = shift @files;
    unshift @files, absFile($b) ? $b : absFromAbsPlusRel($a, $b);
   }
  $files[0]
 } # sumAbsAndRel

#D2 Temporary                                                                   # Temporary files and folders

sub temporaryFile                                                               # Create a new, empty, temporary file.
 {my ($fh, $filename) = tempfile;
  $filename
 }# temporaryFile

sub temporaryFolder                                                             # Create a new, empty, temporary folder.
 {my $d = tempdir();
     $d =~ s/[\/\\]+\Z//s;
  $d.filePathSeparatorChar;
 } # temporaryFolder

BEGIN{*temporaryDirectory=*temporaryFolder}

#D2 Find                                                                        # Find files and folders below a folder.

sub findAllFilesAndFolders($$)                                                  #P Find all the files and folders under a folder.
 {my ($folder, $dirs) = @_;                                                     # Folder to start the search with, true if only folders are required
  my @files;                                                                    # Files

  if (onWindows)                                                                # windows
   {my $c = qq(powershell Get-ChildItem -Recurse -Name $folder ).
     ($dirs ? '-Directory' : '-File');
    my $r = qx($c);
       $r =~ s(\\) (/)g;
    my @r = map {qq($folder$_)} split /\n/, $r;
    @r = map {$_.filePathSeparatorChar} @r if $dirs;
    unshift @r, $folder;                                                        # Find includes the start folder but windows does not
    return sort @r;
   }

  return undef unless confirmHasCommandLineCommand(q(find));                    # Confirm we have find
  my $c   = qq(find "$folder" -print0 -type ).($dirs ? 'd' : 'f');              # Use find command to find files
  my $res = qx($c);                                                             # Execute find command
  defined($res) or confess "No result from find command below\n$c\n";           # Find failed for some reason
  utf8::decode($res);                                                           # Decode unicode file names
  sort split /\0/, $res                                                         # Split out file names on \0
 } # findAllFilesAndFolders

sub findFiles($;$)                                                              # Find all the files under a B<$folder> and optionally B<$filter> the selected files with a regular expression.
 {my ($folder, $filter) = @_;                                                   # Folder to start the search with, optional regular expression to filter files
  my @files;                                                                    # Files
  for(findAllFilesAndFolders($folder, 0))                                       # All files and folders
   {next if -d $_;                                                              # Do not include folder names
    next if $filter and $filter and !m($filter)s;                               # Filter out files that do not match the regular expression
    push @files, $_;
   }
  @files
 } # findFiles

sub findDirs($;$)                                                               # Find all the folders under a B<$folder> and optionally B<$filter> the selected folders with a regular expression.
 {my ($folder, $filter) = @_;                                                   # Folder to start the search with, optional regular expression to filter files
  return findAllFilesAndFolders($folder, 1) if onWindows;                       # All folders if on windows

  my @dir;                                                                      # Directories
  for(findAllFilesAndFolders($folder, 1))                                       # All files and folders
   {next unless -d $_;                                                          # Include only folders
    next if $filter and $filter and !m($filter)s;                               # Filter out directories that do not match the regular expression
    push @dir, fpd($_);
   }
  @dir
 } # findDirs

sub fileList($)                                                                 # Files that match a given search pattern interpreted by L<perlfunc/bsd_glob>.
 {my ($pattern) = @_;                                                           # Search pattern
  bsd_glob($pattern, GLOB_MARK | GLOB_TILDE)
 } # fileList

sub searchDirectoryTreesForMatchingFiles(@)                                     #I Search the specified directory trees for the files (not folders) that match the specified extensions. The argument list should include at least one path name to be useful. If no file extensions are supplied then all the files below the specified paths are returned.  Arguments wrapped in [] will be unwrapped.
 {my (@FoldersandExtensions) = @_;                                              # Mixture of folder names and extensions
  my (@foldersandExtensions) = map {ref($_) ? @$_ : $_} @_;

  my  @extensions = grep {$_ and !-d $_ and !m([\/])} @_;                       # Extensions are not directories
  for(@extensions)                                                              # Prefix period to extension of not all ready there - however this can lead to errors if there happens to be a folder with the same name as an undotted extension.
   {$_ = qq(\.$_) unless m(\A\.)s
   }

  my $ext = @extensions ? join '|', @extensions : undef;                        # Extensions
  my @file;                                                                     # Files

  for my $dir(@_)                                                               # Directories
   {next unless $dir && -d $dir;                                                # Do not include folder names

    for my $d(findAllFilesAndFolders($dir, 0))                                  # All files and folders beneath each folder
     {next if -d $d;                                                            # Do not include folder names
      push @file, $d if !$ext or $d =~ m(($ext)\Z)is;                           # Filter by extension if requested.
     }
   }
  @file                                                                         # Return files
 } # searchDirectoryTreesForMatchingFiles

sub searchDirectoryTreeForSubFolders($)                                         #I Search the specified directory under the specified folder for sub folders
 {my ($folder) = @_;                                                            # The folder at which to start the search
  my @f;                                                                        # Folders found
  for my $d(findAllFilesAndFolders($folder, 0))                                 # All files and folders beneath the start folder
   {push @f, $d if -d $d;                                                       # Do not include file names
   }
  @f                                                                            # Return folder names
 } # searchDirectoryTreeForSubFolders

sub hashifyFolderStructure(@)                                                   # Hashify a list of file names to get the corresponding folder structure.
 {my (@files) = @_;                                                             # File names
  my %h;
  for my $f(@files)                                                             # Map each file
   {my @f = split m(/), $f;
    my $s = join '', map {q({).dump($_).q(})} @f;                               # Hashify directory structure
    my $c = "\$h$s = ".dump($f);                                                # Load targets
    eval $c;
    confess $@ if $@;
   }
  \%h
 } # hashifyFolderStructure

sub countFileExtensions(@)                                                      # Return a hash which counts the file extensions in and below the folders in the specified list.
 {my (@folders) = @_;                                                           # Folders to search
  my %ext;
  for my $dir(@folders)                                                         # Directories
   {next unless -d $dir;
    for my $file(findAllFilesAndFolders($dir, 0))                               # All files and folders under the current folder
     {next if -d $file;                                                         # Do not include folder names
      $ext{fe $file}++;
     }
   }
  \%ext                                                                         # Return extension counts
 } # countFileExtensions

sub countFileTypes($@)                                                          # Return a hash which counts, in parallel with a maximum number of processes: B<$maximumNumberOfProcesses>, the results of applying the B<file> command to each file in and under the specified B<@folders>.
 {my ($maximumNumberOfProcesses, @folders) = @_;                                # Maximum number of processes to run in parallel, Folders to search

  return undef unless confirmHasCommandLineCommand(q(file));                    # Confirm we have file command

  my %ext;
  my @files = squareArray(searchDirectoryTreesForMatchingFiles(@folders));      # Find files

  my $p = newProcessStarter($maximumNumberOfProcesses);                         # Process starter
     $p->totalToBeStarted  = scalar @files;

  for my $block(@files)                                                         # Apply file to each file
   {$p->start(sub
     {my @r;
      for my $file(@$block)
       {my $f = quoteFile($file);
        my $r = qx(file $f);
        push @r, trim(swapFilePrefix($r, $file.q(:), q()));                     # Remove file name from output
       }
      [@r]
     });
   }

  for my $type(deSquareArray($p->finish))                                       # Consolidate results
   {$ext{$type}++;
   }

  \%ext
 } # countFileTypes

sub matchPath($)                                                                # Return the deepest folder that exists along a given file name path.
 {my ($file) = @_;                                                              # File name
  return $file if -e $file;                                                     # File exists so nothing more to match
  my @path = split /[\/\\]/, $file;                                             # Split path into components
  while(@path)                                                                  # Remove components one by one
   {pop @path;                                                                  # Remove deepest component and try again
    my $path = join filePathSeparatorChar, @path, '';                           # Containing folder
    return $path if -d $path;                                                   # Containing folder exists
   }
  ''                                                                            # Nothing matches
 } # matchPath

sub findFileWithExtension($@)                                                   # Find the first file that exists with a path and name of B<$file> and an extension drawn from <@ext>.
 {my ($file, @ext) = @_;                                                        # File name minus extensions, possible extensions
  for my $ext(@ext)                                                             # Each extension
   {my $f = fpe($file, $ext);                                                   # Possible file
    return $ext if -e $f;                                                       # First matching file
   }
  undef                                                                         # No matching file
 } # findFileWithExtension

sub clearFolder($$;$)                                                           #I Remove all the files and folders under and including the specified B<$folder> as long as the number of files to be removed is less than the specified B<$limitCount>. Sometimes the folder can be emptied but not removed - perhaps because it a link, in this case a message is produced unless suppressed by the optional B<$nomsg> parameter.
 {my ($folder, $limitCount, $noMsg) = @_;                                       # Folder, maximum number of files to remove to limit damage, no message if the folder cannot be completely removed.
  return unless -d $folder;                                                     # Only works on a folder that exists
  my @files = findFiles($folder);                                               # Find files to be removed
  if (@files > $limitCount)                                                     # Limit the number of files that can be deleted to limit potential opportunity for damage
   {my $f = @files;
    confess "Limit is $limitCount, but $f files under folder:\n$folder\n";
   }
  my @dirs = findDirs($folder);                                                 # These directories should be empty and thus removable after removing the files
  unlink $_ for @files;                                                         # Remove files
  rmdir $_  for reverse @dirs;                                                  # Remove empty folders
  unless($noMsg or onWindows)
   {-e $folder and carp "Unable to completely remove folder:\n$folder\n";       # Complain if the folder still exists
   }
 } # clearFolder

#D2 Read and write files                                                        # Read and write strings from and to files creating paths to any created files as needed.

sub readFile($)                                                                 #I Return the content of a file residing on the local machine interpreting the content of the file as L<utf8>.
 {my ($file) = @_;                                                              # Name of file to read
  defined($file) or
    confess "Cannot read undefined file\n";
  $file =~ m(\n|\r) and
    confess "File name contains a new line:\n=$file=\n";
  -e $file or
    confess "Cannot read file because it does not exist, file:\n$file\n";
  open(my $F, "<:encoding(UTF-8)", $file) or
    confess "Cannot open file for unicode input, file:\n$file\n$!\n";
  if (wantarray)                                                                # Read as an array
   {my @string = eval {<$F>};
    $@ and confess "$@ reading file:\n$file\n";
    return @string;
   }
  else                                                                          # Read as a string
   {local $/ = undef;
    my $string = eval {<$F>};
    $@ and confess "$@ reading file:\n$file\n";
    return $string;
   }
 } # readFile

sub readStdIn                                                                   # Return the contents of STDIN and return the results as either an array or a string. Terminate with Ctrl-D if testing manually - STDIN remains open allowing this method to be called again to receive another block of data.
 {if (wantarray)                                                                # Read as an array
   {my @string = eval {<STDIN>};
    $@ and confess "$@ reading STDIN\n";
    return @string;
   }
  else                                                                          # Read as a string
   {local $/ = undef;
    my $string = eval {<STDIN>};
    $@ and confess "$@ reading STDIN\n";
    return $string;
   }
 } # readStdIn

sub readFileFromRemote($;$)                                                     #I Copy and read a B<$file> from the remote machine whose ip address is specified by B<$ip> or returned by L<awsIp> and return the content of $file interpreted as utf8 .
 {my ($file, $ip) = @_;                                                         # Name of file to read, optional ip address of server
  copyFileFromRemote($file, $ip // &awsIp);                                     # Read from specified remote instance
  if (wantarray)
   {my @r = readFile($file);
    return @r;
   }
  else
   {my $r = readFile($file);
    return $r;
   }
 } # readFileFromRemote

sub evalFile($)                                                                 # Read a file containing L<unicode> content represented as L<utf8>, L<perlfunc/eval> the content, confess to any errors and then return any result with L<lvalueMethod> methods to access each hash element.
 {my ($file) = @_;                                                              # File to read
  my $string = readFile($file);
  my $res = eval $string;
  $@ and confess "$@\nin file:\n$file\n";
  reloadHashes($res);
  $res
 } # evalFile

sub evalFileAsJson($)                                                           # Read a B<$file> containing L<json> and return the corresponding L<Perl> data structure.
 {my ($file) = @_;                                                              # File to read
  my $string = readFile($file);
  decodeJson($string);
 } # evalFileAsJson

sub evalGZipFile($)                                                             # Read a file compressed with L<gzip> containing L<unicode> content represented as L<utf8>, L<perlfunc/eval> the content, confess to any errors and then return any result with L<lvalueMethod> methods to access each hash element. This is slower than using L<Storable> but does produce much smaller files, see also: L<dumpGZipFile|/dumpGZipFile>.
 {my ($file) = @_;                                                              # File to read
  my $string = readGZipFile($file);
  my $res = eval $string;
  $@ and confess "$@\n";
  reloadHashes($res);
 } # evalGZipFile

sub retrieveFile($)                                                             # Retrieve a B<$file> created via L<Storable>.  This is much faster than L<evalFile|/evalFile> as the stored data is not in text format.
 {my ($file) = @_;                                                              # File to read
  -e $file or confess "No such file: $file\n";                                  # Check file exists
  my $res = retrieve $file;                                                     # Retrieve file
# reloadHashes($res); ####TEST#### Causing problems when we try to reload large structures like Xref                                                          # Reload access methods
  $res
 } # evalFile

sub readUtf16File($)                                                            #P Read a file containing L<unicode> encoded in utf-16.
 {my ($file) = @_;                                                              # Name of file to read
  defined($file) or
    confess "Cannot read undefined file\n";
  $file =~ m(\n|\r) and
    confess "File name contains a new line:\n=$file=\n";
  -e $file or
    confess "Cannot read file because it does not exist, file:\n$file\n";
  open(my $F, "<:encoding(UTF-16)", $file) or confess
    "Cannot open file for utf16 input, file:\n$file\n$!\n";
  local $/ = undef;
  my $s = eval {<$F>};
  $@ and confess $@;
  $s
 }

sub readBinaryFile($)                                                           # Read a binary file on the local machine.
 {my ($file) = @_;                                                              # File to read
  -e $file or
    confess "Cannot read binary file because it does not exist:\n$file\n";
  open my $F, "<$file" or
    confess "Cannot open binary file for input:\n$file\n$!\n";
  binmode $F;
  local $/ = undef;
  <$F>;
 } # readBinaryFile

sub readGZipFile($)                                                             # Read the specified file containing compressed L<unicode> content represented as L<utf8> through L<gzip>.
 {my ($file) = @_;                                                              # File to read.
  defined($file) or
    confess "Cannot read undefined file\n";
  $file =~ m(\n|\r) and
    confess "File name contains a new line:\n=$file=\n";
  -e $file or
    confess "Cannot read file because it does not exist, file:\n$file\n";
  return undef unless confirmHasCommandLineCommand(q(gunzip));                  # Confirm we have gunzip
  open(my $F, "gunzip < $file|") or                                             # Unzip input file
    confess "Cannot open file for input, file:\n$file\n$!\n$?\n";
  binmode($F, "encoding(UTF-8)");
  local $/ = undef;
  my $string = <$F>;
  $string                                                                       # Resulting string
 } # readGZipFile

sub makePath($)                                                                 # Make the path for the specified file name or folder on the local machine. Confess to any failure.
 {my ($file) = @_;                                                              # File or folder name
  my @path = split /[\\\/]+/, $file;
  return undef unless @path > 1;                                                # Its just a file
  pop @path unless $file =~ /[\\\/]\Z/;                                         # Remove file component allowing us to present files as well as folders
  my $path = join filePathSeparatorChar, @path;
  return undef if -d $path;
  eval {make_path($path)};
  return $file if -d $path;                                                     # Success
  confess "Cannot make path with make_path: because:\n$path\n$@\n";
 } # makePath

sub makePathRemote($;$)                                                         # Make the path for the specified B<$file> or folder on the L<aws> instance whose ip address is specified by B<$ip> or returned by L<awsIp>. Confess to any failures.
 {my ($file, $ip) = @_;                                                         # File or folder name, optional ip address
  my @path = split /[\\\/]+/, $file;
  return undef unless @path > 1;                                                # Its just a file
  pop @path unless $file =~ /[\\\/]\Z/;                                         # Remove file component allowing us to present files as well as folders. Split is asymmetric - trailing zero length strings are removed from the results array whilst leading zero length strings are not.
  my $path = join filePathSeparatorChar, @path;

  my $i = $ip // &awsIp;                                                        # Server ip address
  my $c = qq(ssh $i "mkdir -p '$path'; ls -lad '$path'");                       # Make path and list it to confirm
  my $r = qx($c);                                                               # Execute
  return $path if $r =~ m(\Ad);                                                 # Check we have a folder
  confess "Unable to create folder $path on $i\n"                               # Report failure
 } # makePathRemote

sub overWriteFile($$)                                                           # Write to a B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<unicode> content encoded as L<utf8>. Return the name of the $file on success else confess to any failures. If the file already exists it will be overwritten.
 {my ($file, $string) = @_;                                                     # File to write to or B<undef> for a temporary file, unicode string to write
  $file //= temporaryFile;
  $file =~ m(\n|\r)s and confess "File name contains a new line:\n=$file=\n";
  defined($string) or cluck "No string for file:\n$file\n";
  makePath($file);
  open my $F, ">$file" or
          confess "Cannot open file for write because:\n$file\n$!\n";
  binmode($F, ":utf8");
  print  {$F} $string;
  close  ($F) or confess "Could not close file:\n$file\n$!\n";;
  -e $file or confess "Failed to write to file:\n$file\n";
  $file
 } # overWriteFile

BEGIN{*owf=*overWriteFile}                                                      # Short form of overwrite file

sub writeFile($$)                                                               #I Write to a new B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<unicode> content encoded as L<utf8>. Return the name of the $file written to on success else confess if the file already exists or any other error occurs.
 {my ($file, $string) = @_;                                                     # New file to write to or B<undef> for a temporary file,  string to write
  if (defined $file)
   {-e $file and confess "File already exists:\n$file\n";
   }
  &overWriteFile(@_);
 } # writeFile

sub writeTempFile(@)                                                            # Write an array of strings as lines to a temporary file and return the file name.
 {my (@strings) = @_;                                                           # Array of lines
  overWriteFile(undef, join '', map{"$_\n"} @strings);
 } # writeTempFile

sub writeFileToRemote($$;$)                                                     #I Write to a new B<$file>, after creating a path to the file with L<makePath> if necessary, a B<$string> of L<unicode> content encoded as L<utf8> then copy the $file to the remote server whose ip address is specified by B<$ip> or returned by L<awsIp>. Return the name of the $file on success else confess if the file already exists or any other error occurs.
 {my ($file, $string, $ip) = @_;                                                # New file to write to or B<undef> for a temporary file,  string to write, optional ip address
  my $f = writeFile($file, $string);                                            # Create file locally
  copyFileToRemote($f, $ip);                                                    # Copy file created to remote
  $f                                                                            # Return local file name
 } # writeFileToRemote

sub overWriteBinaryFile($$)                                                     # Write to B<$file>, after creating a path to the file with L<makePath> if necessary, the binary content in B<$string>. If the $file already exists it is overwritten. Return the name of the $file on success else confess.
 {my ($file, $string) = @_;                                                     # File to write to or B<undef> for a temporary file, L<unicode> string to write
  $file //= temporaryFile;
  $file =~ m(\n|\r)s and confess "File name contains a new line:\n=$file=\n";
  $string or carp "No string for binary write to file:\n$file\n";
  makePath($file);
  open my $F, ">$file" or
    confess "Cannot open file for binary write because:\n$file\n$!\n";
  binmode($F);
  print  {$F} $string;
  close  ($F);
  -e $file or confess "Failed to write in binary to file:\n=$file=\n$!\n";
  $file
 }

sub writeBinaryFile($$)                                                         # Write to a new B<$file>, after creating a path to the file with L<makePath> if necessary, the binary content in B<$string>. Return the name of the $file on success else confess if the file already exists or any other error occurs.
 {my ($file, $string) = @_;                                                     # New file to write to or B<undef> for a temporary file,  string to write
  if (defined $file)
   {-e $file and confess "Binary file already exists:\n$file\n";
   }
  &overWriteBinaryFile(@_);
 }

sub dumpFile($$)                                                                # Dump to a B<$file> the referenced data B<$structure>.
 {my ($file, $structure) = @_;                                                  # File to write to or B<undef> for a temporary file,  address of data structure to write
  overWriteFile($file, dump($structure));
 } # dumpFile

sub dumpTempFile($)                                                             # Dump a data structure to a temporary file and return the name of the file created
 {my ($structure) = @_;                                                         # Data structure to write
  writeFile(undef, dump($structure));
 } # dumpTempFile

sub dumpFileAsJson($$)                                                          # Dump to a B<$file> the referenced data B<$structure> represented as L<json> string.
 {my ($file, $structure) = @_;                                                  # File to write to or B<undef> for a temporary file,  address of data structure to write
  overWriteFile($file, encodeJson($structure));
 } # dumpFileAsJson

sub dumpTempFileAsJson($)                                                       # Dump a data structure represented as L<json> string to a temporary file and return the name of the file created.
 {my ($structure) = @_;                                                         # Data structure to write
  writeFile(undef, encodeJson($structure));
 } # dumpTempFileAsJson

sub storeFile($$)                                                               # Store into a B<$file>, after creating a path to the file with L<makePath> if necessary, a data B<$structure> via L<Storable>.  This is much faster than L<dumpFile|/dumpFile> but the stored results are not easily modified.
 {my ($file, $structure) = @_;                                                  # File to write to or B<undef> for a temporary file,  address of data structure to write
  if (!$file)                                                                   # Use a temporary file or create a path to the named file
   {$file //= temporaryFile;
   }
  else
   {makePath($file);
   }
  ref($structure) or confess "Reference required for structure parameter";
  store $structure, $file;
  $file
 } # writeFile

sub writeGZipFile($$)                                                           # Write to a B<$file>, after creating a path to the file with L<makePath> if necessary, through L<gzip> a B<$string> whose content is encoded as L<utf8>.
 {my ($file, $string) = @_;                                                     # File to write to, string to write
  makePath($file);
  open my $F, "| gzip>$file" or                                                 # Compress via gzip
    confess "Cannot open file for write because:\n$file\n$!\n";
  binmode($F, ":utf8");                                                         # Input to gzip encoded as utf8
  print  {$F} $string;
  close  ($F);
  -e $file or confess "Failed to write to file:\n$file\n";
  $file
 } # writeGZipFile

sub dumpGZipFile($$)                                                            # Write to a B<$file> a data B<$structure> through L<gzip>. This technique produces files that are a lot more compact files than those produced by L<Storable>, but the execution time is much longer. See also: L<evalGZipFile|/evalGZipFile>.
 {my ($file, $structure) = @_;                                                  # File to write, reference to data
  ref($structure) or confess "\$structure must contain a reference to data, not a scalar";
  writeGZipFile($file, dump($structure));
 } # dumpGZipFile

sub writeFiles($;$$)                                                            # Write the values of a B<$hash> reference into files identified by the key of each value using L<overWriteFile|/overWriteFile> optionally swapping the prefix of each file from B<$old> to B<$new>.
 {my ($hash, $old, $new) = @_;                                                  # Hash of key value pairs representing files and data, optional old prefix, new prefix
  for my $file(sort keys %$hash)                                                # Write file data for each hash key
   {my $target = $old && $new ? swapFilePrefix($file, $old, $new) : $file;      # Optionally swap file prefix
    overWriteFile($file, $hash->{$file})
   }
 } # writeFiles

sub readFiles(@)                                                                # Read all the files in the specified list of folders into a hash.
 {my (@folders) = @_;                                                           # Folders to read
  my %h;
  for my $file(searchDirectoryTreesForMatchingFiles(@folders))                  # Files
   {eval {$h{$file} = readFile($file)};
   }
  \%h
 } # readFiles

sub appendFile($$)                                                              # Append to B<$file> a B<$string> of L<unicode> content encoded with L<utf8>, creating the $file first if necessary. Return the name of the $file on success else confess. The $file being appended to is locked before the write with L<perlfunc/flock> to allow  multiple processes to append linearly to the same file.
 {my ($file, $string) = @_;                                                     # File to append to, string to append
  $file or confess "No file name supplied\n";
  $string or carp "No string for file:\n$file\n";
  makePath($file);
  open my $F, ">>$file" or
    confess "Cannot open file for write file:\n$file\n$!\n";
  binmode($F, ":utf8");
  flock($F, 2);
  print  {$F} $string;
  close  ($F);
  -e $file or confess "Failed to write to file:\n$file\n";
  $file
 } # appendFile

sub createEmptyFile($)                                                          # Create an empty file unless the file already exists and return the name of the file else confess if the file cannot be created.
 {my ($file) = @_;                                                              # File to create or B<undef> for a temporary file
  $file //= temporaryFile;
  return $file if -e $file;                                                     # Return file name as proxy for success if file already exists
  makePath($file);
  open my $F, ">$file" or confess "Cannot create empty file:\n$file\n$!\n";
  binmode($F);
  print  {$F} '';
  close  ($F);
  -e $file or confess "Failed to create empty file:\n$file\n";
  $file                                                                         # Return file name on success
 } # createEmptyFile

sub binModeAllUtf8                                                              #P Set STDOUT and STDERR to accept utf8 without complaint.
 {binmode $_, ":utf8" for *STDOUT, *STDERR;
 }

sub setPermissionsForFile($$)                                                   # Apply L<chmod> to a B<$file> to set its B<$permissions>.
 {my ($file, $permissions) = @_;                                                # File, permissions settings per chmod
  return undef unless confirmHasCommandLineCommand(q(chmod));                   # Confirm we have chmod
  qx(chmod $permissions $file);                                                 # Use chmod to set permissions
 }

sub numberOfLinesInFile($)                                                      # Return the number of lines in a file.
 {my ($file) = @_;                                                              # File
  scalar split /\n/, readFile($file);                                           # Number of lines
 } # numberOfLinesInFile

sub overWriteHtmlFile($$)                                                       # Write an L<html> file to /var/www/html and make it readable
 {my ($file, $data) = @_;                                                       # Target file relative to /var/www/html, data to write
  my $s = writeTempFile($data);
  my $t = fpf(q(/var/www/html/), $file);
  xxx qq(sudo mv $s $t; chmod o+r $t);
  unlink $s;
 }

sub overWritePerlCgiFile($$)                                                    # Write a L<Perl> file to /usr/lib/cgi-bin and make it executable after checking it for syntax errors
 {my ($file, $data) = @_;                                                       # Target file relative to /var/www/html, data to write
  my $s = writeTempFile($data);
  my $r = qx(perl -c $s 2>&1);
  if ($r =~ m(syntax OK)si)
   {my $t = fpf(q(/usr/lib/cgi-bin/), $file);
    say STDERR qx(sudo mv $s $t; chmod o+rx $t);
   }
  else
   {my @data = map {[$_]} split m/\n/, $data;
    say STDERR formatTable([@data]);
    confess "Perl error:\n$r\n";
   }
  unlink $s;
 }

#D2 Copy                                                                        # Copy files and folders. The B<\Acopy.*Md5Normalized.*\Z> methods can be used to ensure that files have collision proof names that collapse duplicate content even when copied to another folder.

sub copyFile($$)                                                                # Copy the B<$source> file encoded in utf8 to the specified B<$target> file in and return $target.
 {my ($source, $target) = @_;                                                   # Source file, target file
  owf($target, readFile($source));
  my $s = fileSize($source);
  my $t = fileSize($target);
  $s eq $t or lll
    "Copied file has a different size\n".formatTable
    ([[$s, $source], [$t, $target]], <<END);
Size Size of file
File Name of file
END
  $target                                                                       # Return target file name
 }

sub moveFileNoClobber($$)                                                       # Rename the B<$source> file, which must exist, to the B<$target> file but only if the $target file does not exist already.  Returns 1 if the $source file was successfully renamed to the $target file else 0.
 {my ($source, $target) = @_;                                                   # Source file, target file
  if (-e $source and !-e $target)                                               # Rename possible
   {rename $source, $target;
    return 1;
   }
  0                                                                             # Rename not possible
 }

sub moveFileWithClobber($$)                                                     # Rename the B<$source> file, which must exist, to the B<$target> file but only if the $target file does not exist already.  Returns 1 if the $source file was successfully renamed to the $target file else 0.
 {my ($source, $target) = @_;                                                   # Source file, target file
  if (-e $source)                                                               # Source file exists so rename
   {unlink $target;
    rename $source, $target;
    return 1;
   }
  0                                                                             # No such source file
 }

sub copyFileToFolder($$)                                                        # Copy the file named in B<$source> to the specified B<$targetFolder/> or if $targetFolder/ is in fact a file into the folder containing this file and return the target file name. Confesses instead of copying if the target already exists.
 {my ($source, $targetFolder) = @_;                                             # Source file, target folder
  writeFile fpf(fp($targetFolder), fne($source)), readFile $source;
 }

sub nameFromStringMaximumLength {128}                                           #P Maximum length of a name generated from a string

sub nameFromString($%)                                                          # Create a readable name from an arbitrary string of text.
 {my ($string, %options) = @_;                                                  # String, options

  my @name;
  if ($string =~ m(<(bookmap))s)                                                # The ghastly compromise
   {push @name, q(bm);
   }
  elsif ($string =~ m(<(bookmap|concept|glossentry|html|map|reference|task))s)  # The correct solution
   {push @name, substr($1, 0, 1);
   }

  $string =~ s(<[^>]*>) (_)gs;                                                  # Remove xml/html tags
  $string =~ s([^a-z0-9_])(_)gis;                                               # Reduce character set to produce a readable name
  push @name, $string;

  my $name = join q(_), @name;
     $name =~ s(_+)(_)gs;                                                       # Remove runs of underscores
     $name =~ s((\A_+|_+\Z)) ()gs;                                              # Remove leading and trailing underscores

  firstNChars($name, $options{maximumLength} // nameFromStringMaximumLength);   # Limit the name length
 }

sub nameFromStringRestrictedToTitle($%)                                         # Create a readable name from a string of text that might contain a title tag - fall back to L<nameFromString|/nameFromString> if that is not possible.
 {my ($string, %options) = @_;                                                  # String, options
  my @name;
  if ($string =~ m(<(bookmap))s)                                                # The ghastly compromise
   {push @name, q(bm);
   }
  elsif ($string =~ m(<(bookmap|concept|glossentry|html|map|reference|task))s)  # The correct solution
   {push @name, substr($1, 0, 1);
   }

  for my $t(qw(title mainbooktitle booktitlealt ))                              # Various title tags
   {if ($string =~ m(<$t[^>]*>([^<]*)</$t>)is)
     {push @name, $1;
     }
   }

  my $name = lc join '_', @name;                                                # Mim believes in lc
  $name =~ s(<[^>]*>) (_)gs;                                                    # Remove xml/html tags
  $name =~ s([^a-z0-9_])(_)gis;                                                 # Reduce character set to produce a readable name
  $name =~ s(_+)(_)gs;                                                          # Remove runs of underscores
  $name =~ s((\A_+|_+\Z)) ()gs;                                                 # Remove leading and trailing underscores

  firstNChars($name, $options{maximumLength} // nameFromStringMaximumLength);   # Limit the name length
 }

sub uniqueNameFromFile($)                                                       # Create a unique name from a file name and the md5 sum of its content
 {my ($source) = @_;                                                            # Source file
  my $sourceFile = fn $source;                                                  # File name component
  return fne($source) if $sourceFile =~ m([0-9a-z]{32}\Z)is;                    # Name already normalized
  my $sourceFileLimited = nameFromString($sourceFile);                          # File name with limited character set
  my $md5 = fileMd5Sum($source);                                                # Normalizing Md5 sum
  fpe($sourceFileLimited.q(_).$md5, fe $source);                                # Normalized name
 }

sub nameFromFolder($)                                                           # Create a name from the last folder in the path of a file name.  Return undef if the file does not have a path.
 {my ($file) = @_;                                                              # File name
  my $p = fp $file;
  my @p = onWindows ? split m(\\), $p : split m(/), $p;
  return $p[-1] if @p;
  undef
 }

sub copyFileMd5Normalized(;$$)                                                  # Normalize the name of the specified B<$source> file to the md5 sum of its content, retaining its current extension, while placing the original file name in a companion file if the companion file does not already exist.  If no B<$target> folder is supplied the file is renamed to its normalized form in situ, otherwise it is copied to the target folder and renamed there. A companion file for the B<$source> file is created by removing the extension of the normalized file and writing the original B<$source> file name to it unless such a file already exists as we assume that it contains the 'original' original name of the B<$source> file. If the B<$source> file is copied to a new location then the companion file is copied as well to maintain the link back to the original name of the file.
 {my ($source, $Target) = @_;                                                   # Source file, target folder or a file in the target folder
  warn "Deprecated in favor of Dita::GB::Standard";
  -e $source && !-d $source or
    confess "Source file to normalize does not exist:\n$source";
  my $target = fp($Target // $source);                                          # Target folder
  my $sourceFile = fn $source;                                                  # File name component

  if ($sourceFile =~ m([0-9a-z]{32}\Z)is)                                       # Name already normalized
   {if (@_== 2)                                                                 # Copy source to new folder if necessary
     {my $target = fpf(fp($Target), fne($source));
      copyFile($source, $target);
      my $id = setFileExtension($source);
      my $od = setFileExtension($target);
      if (!-e $od)
       {if (-e $id)
         {copyFile($id, $od);
         }
        else
         {owf($od, $source);
         }
       }
      return $target;                                                           # Normalized target
     }
    return $source;                                                             # File is already normalized
   }

  my $out = fpe($target, nameFromString(readFile($source)));                    # Create normalized name in new folder depending only on the content of the source file
  my $id  = setFileExtension($source);                                          # Source companion file carrying original name
  my $od  = setFileExtension($out);                                             # Target companion file carrying original name

  if (!-e $out)                                                                 # Copy file unless it is already there - we know the target is correct because its name is normalized
   {copyFile($source, $out);                                                    # Copy source to normalized target
    if (-e $id)                                                                 # Copy or create companion file
     {copyFile($id, $od);
     }
    elsif (!-e $od)
     {owf($od, $source);                                                        # Create a companion file as none exists
     }
   }
  $out                                                                          # Return normalized image file name
 }

sub copyFileMd5NormalizedName($$@)                                              # Name a file using the GB Standard
 {my ($content, $extension, %options) = @_;                                     # Content, extension, options
warn "Deprecated in favor of Dita::GB::Standard";
  defined($content) or
    confess "Content must be defined";
  defined($extension) && $extension =~ m(\A\S{2,}\Z)s or
    confess "Extension must be non blank and at least two characters long";
  my $name   = nameFromString($content);                                        # Human readable component
     $name   = nameFromStringRestrictedToTitle($content) if $options{titleOnly};# Not entirely satisfactory
  my $md5    = stringMd5Sum($content);                                          # Md5 sum
  fpe($name.q(_).$md5, $extension)                                              # Add extension
 }

sub copyFileMd5NormalizedCreate($$$$@)                                          # Create a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content>, whose extension is B<$extension> and which has a companion file with the same name minus the extension which contains the specified B<$companionContent>.  Such a file can be copied multiple times by L<copyFileMd5Normalized|/copyFileMd5Normalized> regardless of the other files in the target folders.
 {my ($Folder, $content, $extension, $companionContent, %options) = @_;         # Target folder or a file in that folder, content of the file, file extension, contents of the companion file, options.
warn "Deprecated in favor of Dita::GB::Standard";
  my $folder = fp $Folder;                                                      # Normalized folder name
  my $name   = nameFromString($content);                                        # Entirely satisfactory
     $name   = nameFromStringRestrictedToTitle($content) if $options{titleOnly};# Not entirely satisfactory
  my $md5    = stringMd5Sum($content);
  my $od     = fpf($folder, $name.q(_).$md5);                                   # Companion file
  my $out    = fpe($od, $extension);                                            # Normalized file
  owf($out, $content);                                                          # Write file content
  owf($od,  $companionContent );                                                # Write companion file
  $out
 }

sub copyFileMd5NormalizedGetCompanionContent($)                                 # Return the content of the companion file to the specified B<$source> file after it has been normalized via L<copyFileMd5Normalized|/copyFileMd5Normalized> or L<copyFileMd5NormalizedCreate|/copyFileMd5NormalizedCreate> or return B<undef> if the corresponding companion file does not exist.
 {my ($source) = @_;                                                            # Source file.
warn "Deprecated in favor of Dita::GB::Standard";
  my $id = setFileExtension($source);
  -e $source && -e $id ? readFile($id) : undef
 }

sub copyFileMd5NormalizedDelete($)                                              # Delete a normalized and its companion file
 {my ($file) = @_;                                                              # File
warn "Deprecated in favor of Dita::GB::Standard";
  my $companion = setFileExtension($file);
  unlink $_ for $companion, $file;
 }

sub copyBinaryFile($$)                                                          # Copy the binary file B<$source> to a file named <%target> and return the target file name,
 {my ($source, $target) = @_;                                                   # Source file, target file
  overWriteBinaryFile($target, readBinaryFile($source));
#  my $s = fileSize($source);                                                   # Appears to be unreliable across multiple CPUs
#  my $t = fileSize($target);
#  $s eq $t or confess
#    "Copied binary file has a different size\n".formatTable
#    ([[$s, $source], [$t, $target]], <<END);
#Size Size of file
#File Name of file
#END
  $target
 }

sub copyBinaryFileMd5Normalized($;$)                                            # Normalize the name of the specified B<$source> file to the md5 sum of its content, retaining its current extension, while placing the original file name in a companion file if the companion file does not already exist.  If no B<$target> folder is supplied the file is renamed to its normalized form in situ, otherwise it is copied to the target folder and renamed there. A companion file for the B<$source> file is created by removing the extension of the normalized file and writing the original B<$source> file name to it unless such a file already exists as we assume that it contains the 'original' original name of the B<$source> file. If the B<$source> file is copied to a new location then the companion file is copied as well to maintain the link back to the original name of the file.
 {my ($source, $Target) = @_;                                                   # Source file, target folder or a file in the target folder
warn "Deprecated in favor of Dita::GB::Standard";
  -e $source or confess "File does not exist:\n$source\n";

  return $source if fn($source) =~ m([0-9a-z]{32}\Z)is and @_ == 1;             # Name already normalized and no target

  my $target = fp($Target);                                                     # Target folder
  my $ext = fe($source);                                                        # Extension
  my $out = fpe($target, $ext.q(_).fileMd5Sum($source), $ext);                  # Normalized name in new folder
  my $id  = setFileExtension($source);                                          # Source companion file carrying original name
  my $od  = setFileExtension($out);                                             # Target companion file carrying original name

  if (!-e $out)                                                                 # Copy file unless it is already there - we know the target is correct because its name is normalized
   {overWriteBinaryFile($out, readBinaryFile($source));
    if (-e $id)                                                                 # Copy or create companion file
     {copyFile($id, $od);
     }
    elsif (!-e $od)
     {owf($od, $source);
     }
   }
  $out                                                                          # Return normalized image file name
 }

sub copyBinaryFileMd5NormalizedCreate($$$$)                                     # Create a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content>, whose extension is B<$extension> and which has a companion file with the same name minus the extension  which contains the specified B<$companionContent>.  Such a file can be copied multiple times by L<copyBinaryFileMd5Normalized|/copyBinaryFileMd5Normalized> regardless of the other files in the target folders while retaining the original name information.
 {my ($Folder, $content, $extension, $companionContent) = @_;                   # Target folder or a file in that folder, content of the file, file extension, optional content of the companion file.
warn "Deprecated in favor of Dita::GB::Standard";
  my $folder = fp $Folder;                                                      # Normalized folder name
  my $md5    = fileMd5Sum($content);                                            # Md5 sum of content
  my $od     = fpf($folder, $extension.q(_).$md5);                              # Companion file
  my $out    = fpe($od, $extension);                                            # Normalized file
  owf($out, $content);                                                          # Write file content
  owf($od, $companionContent);                                                  # Write companion file
  -e $out or confess "Failed to create file $out";
  -e $od  or confess "Failed to create companion file $od";
  $out
 }

sub copyBinaryFileMd5NormalizedGetCompanionContent($)                           # Return the original name of the specified B<$source> file after it has been normalized via L<copyBinaryFileMd5Normalized|/copyBinaryFileMd5Normalized> or L<copyBinaryFileMd5NormalizedCreate|/copyBinaryFileMd5NormalizedCreate> or return B<undef> if the corresponding companion file does not exist.
 {my ($source) = @_;                                                            # Source file.
warn "Deprecated in favor of Dita::GB::Standard";
  my $id = setFileExtension($source);
  -e $source && -e $id ? readFile($id) : undef
 }

sub copyFileToRemote($;$)                                                       # Copy the specified local B<$file> to the server whose ip address is specified by B<$ip> or returned by L<awsIp>.
 {my ($file, $ip) = @_;                                                         # Source file, optional ip address
  my $f = fullyQualifyFile($file);                                              # Fully qualify source file
  -f $file or confess "No such file:\n$file\n";                                 # Check source file exists
  -f $f or confess "No such file:\n$f\n";                                       # Check source file exists
  my $i = $ip // &awsIp;                                                        # Ip of server
  my $d = fp $f;                                                                # Folder to create if necessary
  makePathRemote($f, $i);                                                       # Create folder on remote
  my $c = qq(rsync -mpqrt --del $f $i:$f);                                      # Transfer file
# lll $c;
  xxx $c, qr(\A\s*\Z);                                                          # Execute and expect no messages
 }

sub copyFileFromRemote($;$)                                                     # Copy the specified B<$file> from the server whose ip address is specified by B<$ip> or returned by L<awsIp>.
 {my ($file, $ip) = @_;                                                         # Source file, optional ip address
  my $f = fullyQualifyFile($file);                                              # Fully qualify source file
  my $i = $ip // &awsIp;                                                        # Ip of server
  my $d = fp $f;                                                                # Folder to create if necessary
  makePath($d);                                                                 # Create folder
  my $c = qq(rsync -mpqrt $i:$f $f);                                            # Transfer file
 #lll $c;
  xxx $c, qr(\A\s*\Z);
 }

sub copyFolder($$)                                                              # Copy the B<$source> folder to the B<$target> folder after clearing the $target folder.
 {my ($source, $target) = @_;                                                   # Source file, target file
  -d $source or confess "No such folder:\n$source\n";
  my $s = fpd($source);
  my $t = fpd($target);
  makePath($t);
  my $c = qq(rsync -r --del $s $t), qr(\A\s*\Z);                                # Suppress command printing by supplying a regular expression to test the command output
 #lll $c;
  xxx $c, qr(\A\s*\Z);
 }

sub mergeFolder($$)                                                             # Copy the B<$source> folder into the B<$target> folder retaining any existing files not replaced by copied files.
 {my ($source, $target) = @_;                                                   # Source file, target file
  -d $source or confess "No such folder:\n$source\n";
  my $s = fpd($source);
  my $t = fpd($target);
  makePath($t);
  my $c = qq(rsync -r $s $t);
 #lll $c;
  xxx $c, qr(\A\s*\Z);
 }

sub copyFolderToRemote($;$)                                                     # Copy the specified local B<$Source> folder to the corresponding remote folder on the server whose ip address is specified by B<$ip> or returned by L<awsIp>. The default userid supplied by F<.ssh/config> will be used on the remote server.
 {my ($Source, $ip) = @_;                                                       # Source file, optional ip address of server
  my $source = fullyQualifyFile($Source);                                       # Fully qualify source folder
  -d $Source or confess "No such folder:\n$Source\n";                           # Check source exists
  -d $source or confess "No such folder:\n$source\n";                           # Check source exists
  my $i = $ip // &awsIp;                                                        # Ip of server
  my $s = fpd($source);                                                         # Normalize folder name
  makePathRemote($s, $i);                                                       # Create folder on target
  my $c = qq(rsync -mpqrt --del $s $i:$s);                                      # Transfer files
  #lll $c;
  xxx($c, qr(\A\s*\Z));                                                         # Execute and expect no messages
 }

sub mergeFolderFromRemote($;$)                                                  # Merge the specified B<$Source> folder from the corresponding remote folder on the server whose ip address is specified by B<$ip> or returned by L<awsIp>. The default userid supplied by F<.ssh/config> will be used on the remote server.
 {my ($Source, $ip) = @_;                                                       # Source file, optional ip address of server
  my $source = fullyQualifyFile($Source);                                       # Fully qualify source folder
  my $i = $ip // &awsIp;                                                        # Ip of server
  my $s = fpd($source);                                                         # Normalize folder name
  makePath($s);                                                                 # Create folder locally to receive results
  makePathRemote($s, $i);                                                       # Create folder on target so that rsync does not complain if it is not there == empty
  my $c  = qq(rsync -mpqrt $i:$s $s);                                           # Transfer files
 #lll $c;
  xxx($c, qr(\A\s*\Z));                                                         # Execute and expect no messages
 }

#D1 Testing                                                                     # Methods to assist with testing

sub removeFilePathsFromStructure($)                                             # Remove all file paths from a specified B<$structure> to make said $structure testable with L<Test::More/is_deeply>.
 {my ($structure) = @_;                                                         # Data structure reference
  my $s = dump($structure);                                                     # Dump structure
  $s =~ s("[^"]*/) (")gs;                                                       # Remove file prefixes in strings
  my $r = eval $s;                                                              # New version of structure
  confess "Unable to remove file prefixes from structure\n$@\n$s\n" if $@;      # Complain if removal fails
  $r                                                                            # Return new structure
 }

sub writeStructureTest($$)                                                      # Write a test for a data B<$structure> with file names in it.
 {my ($structure, $expr) = @_;                                                  # Data structure reference, expression
  my $s = nws(dump($structure));                                                # Dump structure
     $s =~ s("[^"]*/) (")gs;                                                    # Remove file prefixes in strings
     $s =~ s(\],) (],\n    )gs;                                                 # Reinsert new lines
     $s =~ s(\},) (},\n    )gs;
  <<END;                                                                        # Format test
  is_deeply removeFilePathsFromStructure($expr),\n   $s;
END
 }

#D1 Images                                                                      # Image operations.

sub imageSize($)                                                                # Return (width, height) of an B<$image>.
 {my ($image) = @_;                                                             # File containing image
  -e $image or confess
    "Cannot get size of image as file does not exist:\n$image\n";
  return undef unless confirmHasCommandLineCommand(q(identify));                # Confirm we have identify
  my $s = qx(identify -verbose "$image");
  if ($s =~ /Geometry: (\d+)x(\d+)/s)
   {return ($1, $2);
   }
  else
   {confess "Cannot get image size for file:\n$image\nfrom:\n$s\n";
   }
 }

sub convertImageToJpx690($$;$$)                                                 #P Convert a B<$source> image to a B<$target> image in jpx format using versions of L<Imagemagick> version 6.9.0 and above. The size in pixels of each jpx tile may be specified by the optional B<$Size> parameter which defaults to B<256>. B<$Tiles> optionally provides an upper limit on the number of each tiles in each dimension.
 {my ($Source, $target, $Size, $Tiles) = @_;                                    # Source file, target folder (as multiple files will be created),  optional size of each tile - defaults to 256, optional limit on the number of tiles in either dimension
  my $source = $Source;
  my $size = $Size // 256;                                                      # Size of each tile
  my $N    = 4;                                                                 # Power of ten representing the maximum number of tiles
  -e $source or confess "Image file does not exist:\n$source\n";                # Check source
  $target  = fpd($target);                                                      # Make sure the target is a folder
  makePath($target);                                                            # Make target folder

  if ($Tiles)                                                                   # Restrict the converted image to a maximum number of tiles if requested
   {my $s = quoteFile($source);
    my $t = temporaryFile;
    my $n = $Size*$Tiles;
    my $c = qq(convert $s -resize ${n}x${n}\\> $t);
    lll $_ for qx($c 2>&1);
    $source = $t;                                                               # Resized file is now source
   }

  my ($w, $h) = imageSize($source);                                             # Image size
  my $W = int($w/$size); ++$W if $w % $size;                                    # Image size in tiles
  my $H = int($h/$size); ++$H if $h % $size;
  writeFile(filePath($target, "jpx.data"), <<END);                              # Write jpx header
version 1
type    jpx
size    $size
source  $Source
width   $w
height  $h
END

  if (1)                                                                        # Create tiles
   {my $s = quoteFile($source);
    my $t = quoteFile($target."%0${N}d.jpg");
    my $c = qq(convert $s -crop ${size}x${size} $t);
    lll $_ for qx($c 2>&1);
   }

  if (1)                                                                        # Rename tiles in two dimensions
   {my $W = int($w/$size); ++$W if $w % $size;
    my $H = int($h/$size); ++$H if $h % $size;
    my $k = 0;
    for   my $Y(1..$H)
     {for my $X(1..$W)
       {my $s = sprintf("${target}%0${N}d.jpg", $k++);
        my $t = "${target}/${Y}_${X}.jpg";
        rename $s, $t or confess "Cannot rename file:\n$s\nto:\n$t\n";
        -e $t or confess "Cannot create file:\n$t\n";
       }
     }
   }
 }

sub convertImageToJpx($$;$$)                                                    #P Convert a B<$source> image to a B<$target> image in jpx format. The size in pixels of each jpx tile may be specified by the optional B<$Size> parameter which defaults to B<256>. B<$Tiles> optionally provides an upper limit on the number of each tiles in each dimension.
 {my ($Source, $target, $Size, $Tiles) = @_;                                    # Source file, target folder (as multiple files will be created),  optional size of each tile - defaults to 256, optional limit in either direction on the number of tiles
  my $source = $Source;

  return undef unless confirmHasCommandLineCommand(q(convert));                 # Confirm we have convert

  if (1)
   {my $r = qx(convert --version);
    if ($r =~ m(\AVersion: ImageMagick ((\d|\.)+)))
     {my $version = join '', map {sprintf("%04d", $_)} split /\./, $1;
      return &convertImageToJpx690(@_) if $version >= 600090000;
     }
    else {confess "Please install Imagemagick:\nsudo apt install imagemagick\n"}
   }

  -e $source or confess "Image file does not exist:\n$source\n";
  my $size = $Size // 256;

  makePath($target);

  if ($Tiles)                                                                   # Restrict the converted image to a maximum number of tiles if requested
   {my $s = quoteFile($source);
    my $t = temporaryFile;
    my $n = $Size*$Tiles;
    my $c = qq(convert $s -resize ${n}x${n}\\> $t);
    lll $_ for qx($c 2>&1);
    $source = $t;                                                               # Resized file is now source
   }

  my ($w, $h) = imageSize($source);                                             # Write Jpx header
  writeFile(filePath($target, "jpx.data"), <<END);
version 1
type    jpx
size    $size
source  $Source
width   $w
height  $h
END

  if (1)                                                                        # Create tiles
   {my $s = quoteFile($source);
    my $t = quoteFile($target);
    my $c = qq(convert $s -crop ${size}x${size} $t);
    lll $_ for qx($c 2>&1);
   }

  if (1)                                                                        # Rename tiles in two dimensions
   {my $W = int($w/$size); ++$W if $w % $size;
    my $H = int($h/$size); ++$H if $h % $size;
    my $k = 0;
    for   my $Y(1..$H)
     {for my $X(1..$W)
       {my $s = "${target}-$k";
        my $t = "${target}/${Y}_${X}.jpg";
        rename $s, $t or confess "Cannot rename file:\n$s\nto:\n$t\n";
        -e $t or confess "Cannot create file:\n$t\n";
        ++$k;
       }
     }
   }
 }

sub convertDocxToFodt($$)                                                       # Convert a I<docx> B<$inputFile> file to a I<fodt> B<$outputFile> using B<unoconv> which must not be running elsewhere at the time.  L<Unoconv|/https://github.com/dagwieers/unoconv> can be installed via:\m  sudo apt install sharutils unoconv\mParameters:
 {my ($inputFile, $outputFile) = @_;                                            # Input file, output file
  return undef unless confirmHasCommandLineCommand(q(unoconv));                 # Confirm we have unoconv
  my $r = qx(unoconv -f fodt -o "$outputFile" "$inputFile");                    # Perform conversion
  !$r or confess "unoconv failed, try closing libreoffice if it is open\n". $r;
 }

# Tests in: /home/phil/perl/z/unoconv/testCutOutImagesInFodtFile.pl
sub cutOutImagesInFodtFile($$$)                                                 # Cut out the images embedded in a B<fodt> file, perhaps produced via L<convertDocxToFodt|/convertDocxToFodt>, placing them in the specified folder and replacing them in the source file with:\m  <image href="$imageFile" outputclass="imageType">.\mThis conversion requires that you have both L<Imagemagick> and L<unoconv|/https://github.com/dagwieers/unoconv> installed on your system:\m    sudo apt install sharutils  imagemagick unoconv\mParameters:
 {my ($inputFile, $outputFolder, $imagePrefix) = @_;                            # Input file,  output folder for images, a prefix to be added to image file names
  my $source = readFile($inputFile);                                            # Read .fodt file
  lll "Start image location in string of ", length($source);

  my @p;
  my $p = 0;
  my ($s1, $s2) = ('<office:binary-data>', '</office:binary-data>');
  for(;;)                                                                       # Locate images
   {my $q = index($source, $s1, $p);  last if $q < 0;
    my $Q = index($source, $s2, $q);  last if $Q < 0;
    push @p, [$q+length($s1), $Q-$q-length($s1)];
    $p = $Q;
   }
  lll "Cutting out ", scalar(@p), " images";                                    # Cut out images

  my $imageNumber = @p;                                                         # Number the image files

  for(reverse @p)                                                               # We cut out in reverse to preserve the offsets of the images yet to be cut out
   {my ($p, $l) = @$_;                                                          # Position, length of image

    my $i = substr($source, $p, $l);                                            # Image text uuencoded
       $i =~ s/ //g;                                                            # Remove leading spaces on each line

    my ($ext, $type, $im) =                                                     # Decide on final image type, possibly via an external imagemagick conversion on windows, or an internal imagemagick conversion locally
      $i =~ m/\AiVBOR/    ? ('png')            :
      $i =~ m/\AAQAAAG/   ? ('png', 'emf')     :
      $i =~ m/\AVkNMT/    ? ('png', 'svm')     :
      $i =~ m/\A183G/     ? ('png', '', 'wmf') :
      $i =~ m/\A\/9j/     ? ('jpg')            :
      $i =~ m/\AR0lGODlh/ ? ('gif')            :
      confess "Unknown image type: ". substr($i, 0, 16)."\n";

    lll "$imageNumber cut $ext from $p for $l";

    my $imageBinary = decodeBase64($i);                                         # Decode image
    my $imageFile =                                                             # Image file name
      fpe($outputFolder, join(q(), $imagePrefix, q(_), $imageNumber), $ext);

    if (!$type)
     {writeBinaryFile($imageFile, $imageBinary);
     }

    my $xml = "<image href=\"$imageFile\" outputclass=\"$ext\"\/>";             # Create image command
    substr($source, $p, $l) = $xml;                                             # Replace the image source with an image command
    $imageNumber--;
   }
  $source
 }

#D1 Encoding and Decoding                                                       # Encode and decode using L<Json> and Mime.

sub unbless($)                                                                  # Remove the effects of bless from a L<Perl> data B<$structure> enabling it to be converted to L<JSON> or compared with L<Test::More::is_deeply>.
 {my ($d) = @_;                                                                 # Unbless a L<Perl> data structure.
  return $d unless ref $d;
  my $r = reftype $d;
  return [map {      __SUB__->(    $_ )}      @$d] if $r eq q(ARRAY);
  return {map {$_ => __SUB__->($$d{$_})} keys %$d} if $r eq q(HASH);
  confess "Unknown container: $r\n";
 }

sub encodeJson($)                                                               # Convert a L<Perl> data B<$structure> to a L<Json> string.
 {my ($structure) = @_;                                                         # Data to encode
  JSON->new->utf8->allow_blessed->pretty->canonical->encode(unbless $structure)
 }

sub decodeJson($)                                                               # Convert a L<Json> B<$string> to a L<Perl> data structure.
 {my ($string) = @_;                                                            # Data to decode
  JSON->new->utf8->pretty->canonical->decode($string)
 }

sub encodeBase64($)                                                             # Encode an L<ascii> B<$string> in base 64.
 {my ($string) = @_;                                                            # String to encode
  my $s = eval {encode_base64($string, '')};
  confess $@ if $@;                                                             # So we get a trace back
  $s
 }

sub decodeBase64($)                                                             # Decode an L<ascii> B<$string> in base 64.
 {my ($string) = @_;                                                            # String to decode
  my $s   = eval {decode_base64($string)};
  confess $@ if $@;                                                             # So we get a trace back
  $s
 }

sub convertUnicodeToXml($)                                                      # Convert a B<$string> with L<unicode> code points that are not directly representable in L<ascii> into string that replaces these code points with their representation in L<Xml> making the string usable in L<Xml> documents.
 {my ($string) = @_;                                                            # String to convert
  my $t = '';
  for(split //, $string)                                                        # Each letter in the source
   {my $n = ord($_);
    my $c = $n > 127 ? "&#$n;" : $_;                                            # Use xml representation beyond u+127
    $t .= $c;
   }
  $t                                                                            # Return resulting string
 }

sub asciiToHexString($)                                                         # Encode an L<ascii> string as a string of L<hexadecimal> digits.
 {my ($ascii) = @_;                                                             # Ascii string
  my $c = '';                                                                   # Result
  for my $a(split //, $ascii)                                                   # Each ascii character
   {$c .= sprintf("%x", ord $a)                                                 # Format as hex
   }
  $c                                                                            # Return string of hexadecimal digits
 }

sub hexToAsciiString($)                                                         # Decode a string of L<hexadecimal> digits as an L<ascii> string.
 {my ($hex) = @_;                                                               # Hexadecimal string
  my @c = grep {m/[0-9a-f]/i} split //, $hex;                                   # Each hexadecimal digit
  my $c = '';                                                                   # Result
  for my $i(keys @c)                                                            # Index of each hexadecimal digit
   {if ($i % 2 == 1)                                                            # End of latest pair
     {$c .= chr hex $c[$i-1].$c[$i];                                            # Convert to character
     }
   }
  $c                                                                            # Return result
 }

my @translatePercentEncoding =
 (qq(\n)=>q(%0A),
  qq( ) =>q(%20),
  qq(\")=>q(%22),
  qq(\%)=>q(%25),
  qq(\-)=>q(%2d),
  qq(\.)=>q(%2e),
  qq(\<)=>q(%3c),
  qq(\>)=>q(%3e),
  qq(\\)=>q(%5c),
  qq(\^)=>q(%5e),
  qq(\_)=>q(%5f),
  qq(\`)=>q(%60),
  qq(\{)=>q(%7b),
  qq(\|)=>q(%7c),
  qq(\})=>q(%7d),
  qq(\~)=>q(%7e),
 );

my %translatePercentEncoding =         @translatePercentEncoding;
my %TranslatePercentEncoding = reverse @translatePercentEncoding;

sub wwwEncode($)                                                                # Percent encode a L<url> per: https://en.wikipedia.org/wiki/Percent-encoding#Percent-encoding_reserved_characters
 {my ($string) = @_;                                                            # String
  join '', map {$translatePercentEncoding{$_}//$_} split //, $string
 }

sub wwwDecode($)                                                                # Percent decode a L<url> B<$string> per: https://en.wikipedia.org/wiki/Percent-encoding#Percent-encoding_reserved_characters
 {my ($string) = @_;                                                            # String
  my $r = '';
  my @s = split //, $string;
  while(@s)
   {my $c = shift @s;
    if ($c eq q(%) and @s >= 2)
     {$c .= shift(@s).shift(@s);
      $r .= $TranslatePercentEncoding{$c}//$c;
     }
    else
     {$r .= $c;
     }
   }
  $r =~ s(%0d0a) (\n)gs;                                                        # Awkward characters that appear in urls
  $r =~ s(\+)     ( )gs;
  $r
 }

sub printPerlDataAsXml($;$)                                                     # Print a Perl data structure as xml
 {my ($data, $width) = @_;                                                      # Perl data structure, ideal width for Xml - default is 80
  $width //= 80;                                                                # Default width

  sub                                                                           # Dump a level of a Perl data structure as xml
   {my ($data, $depth) = @_;                                                    # Perl data structure, depth
    my $r = reftype($data) // '';
    my @s;                                                                      # Type of data

    my sub format(@)                                                            # Format an array of tags
     {my (@s) = @_;
      my $s = join '', @s;

      my $d = '  ' x $depth;
      @s < 2 || length($s) < $width ? $s : join "\n", shift(@s), map {"$d$_"} @s;
     };

    if ($r =~ m(HASH)i)                                                         # Hash
     {my   @t;
      push @t, qq(<hash>);
      for my $k(sort keys %$data)
       {my @u;
        push @u, qq(<$k>);
        push @u, __SUB__->($$data{$k}, $depth+2);
        push @u, qq(</$k>);
        push @t, format(@u);
       }
      push @t, qq(</hash>);
      push @s, format(@t);
     }
    elsif ($r =~ m(ARRAY)i)                                                     # Array
     {my   @t;
      push @t, qq(<array>);
      for my $i(keys @$data)
       {my   @u;
        push @u, qq(<e>);
        push @u, __SUB__->($$data[$i], $depth+2);
        push @u, qq(</e>);
        push @t, format(@u);
       }
      push @t, qq(</array>);
      push @s, format(@t);
     }
    else                                                                        # Scalar
     {push @s, qq($data);
     }

    format(@s)
   }->($data, 0);
 }

#D1 Numbers                                                                     # Numeric operations,

sub powerOfTwo($)                                                               # Test whether a number B<$n> is a power of two, return the power if it is else B<undef>.
 {my ($n) = @_;                                                                 # Number to check
  for(0..128)
   {return $_  if 1<<$_ == $n;
    last       if 1<<$_ >  $n;
   }
  undef
 }

sub containingPowerOfTwo($)                                                     # Find log two of the lowest power of two greater than or equal to a number B<$n>.
 {my ($n) = @_;                                                                 # Number to check
  for(0..128)
   {return $_  if $n <= 1<<$_;
   }
  undef
 }

#D2 Minima and Maxima                                                           # Find the smallest and largest elements of arrays.

sub min(@)                                                                      # Find the minimum number in a list of numbers confessing to any ill defined values.
 {my (@m) = @_;                                                                 # Numbers
  my @n = grep {defined($_) and looks_like_number($_)} @_;
  @_ == @n or confess q(Undefined or non numeric parameters present);
  return undef unless @n;
  my $M = shift;
  for(@n)
   {$M = $_ if $_ < $M;
   }
  $M
 }

sub indexOfMin(@)                                                               # Find the index of the minimum number in a list of numbers confessing to any ill defined values.
 {my (@m) = @_;                                                                 # Numbers
  my @n = grep {defined($_) and looks_like_number($_)} @_;
  @_ == @n or confess q(Undefined or non numeric parameters present);
  return undef unless @n;
  my $M = 0;
  for my $i(keys @n)
   {my $n = $n[$i];
    $M = $i if $n < $n[$M];
   }
  $M
 }

sub max(@)                                                                      # Find the maximum number in a list of numbers confessing to any ill defined values.
 {my (@m) = @_;                                                                 # Numbers
  my @n = grep {defined($_) and looks_like_number($_)} @_;
  @_ == @n or confess q(Undefined or non numeric parameters present);
  return undef unless @n;
  my $M = shift;
  for(@n)
   {$M = $_ if $_ > $M;
   }
  $M
 }

sub indexOfMax(@)                                                               # Find the index of the maximum number in a list of numbers confessing to any ill defined values.
 {my (@m) = @_;                                                                 # Numbers
  my @n = grep {defined($_) and looks_like_number($_)} @_;
  @_ == @n or confess q(Undefined or non numeric parameters present);
  return undef unless @n;
  my $M = 0;
  for my $i(keys @n)
   {my $n = $n[$i];
    $M = $i if $n > $n[$M];
   }
  $M
 }

sub arraySum(@)                                                                 # Find the sum of any strings that look like numbers in an array.
 {my (@a) = @_;                                                                 # Array to sum
  my @n = grep {defined($_) and looks_like_number($_)} @_;
  @_ == @n or confess q(Undefined or non numeric parameters present);
  my $sum = 0; $sum += $_ for @n;
  $sum
 }

sub arrayProduct(@)                                                             # Find the product of any strings that look like numbers in an array.
 {my (@a) = @_;                                                                 # Array to multiply
  my @n = grep {defined($_) and looks_like_number($_)} @_;
  @_ == @n or confess q(Undefined or non numeric parameters present);
  my $product = 1; $product *= $_ for @n;
  $product
 }

sub arrayTimes($@)                                                              # Multiply by B<$multiplier> each element of the array B<@a> and return as the result.
 {my ($multiplier, @a) = @_;                                                    # Multiplier, array to multiply and return
  map {$multiplier * $_} @a
 }

#D1 Sets                                                                        # Set operations.

sub mergeHashesBySummingValues(@)                                               # Merge a list of hashes B<@h> by summing their values
 {my (@h) = @_;                                                                 # List of hashes to be summed
  my %h;
  for my $h(@h)
   {$h{$_} += $$h{$_} for sort keys %$h;
   }
  \%h
 }

sub invertHashOfHashes(@)                                                       # Invert a hash of hashes: given {a}{b} = c return {b}{c} = c
 {my ($h) = @_;                                                                 # Hash of hashes
  my %i;                                                                        # Resulting inverted hash of hashes
  for   my $a(keys $h->%*)
   {for my $b(keys $$h{$a}->%*)
     {$i{$b}{$a} = $$h{$a}{$b};
     }
   }

  \%i                                                                           # Inverted hashes
 }

sub unionOfHashKeys(@)                                                          # Form the union of the keys of the specified hashes B<@h> as one hash whose keys represent the union.
 {my (@h) = @_;                                                                 # List of hashes to be united
  return {}    unless @h;
  return $h[0] if @h == 1;
  my %u;                                                                        # Union
  for my $h(@h)                                                                 # Each hash to be united
   {for my $k(keys %$h)                                                         # Keys in current hash
     {$u{$k}++;                                                                 # Add value to union array
     }
   }

  \%u                                                                           # Union of all hashes
 }

sub intersectionOfHashKeys(@)                                                   # Form the intersection of the keys of the specified hashes B<@h> as one hash whose keys represent the intersection.
 {my (@h) = @_;                                                                 # List of hashes to be intersected
  return {}    unless @h;
  return $h[0] if @h == 1;

  my $u = unionOfHashKeys(@h);                                                  # Union
  my $N = @h;                                                                   # Number of hashes
  my %i;                                                                        # Intersection
  for my $k(keys %$u)                                                           # Each key
   {if ($$u{$k} == $N)                                                          # Key present in all hashes
     {$i{$k}++                                                                  # Add hash value to intersection
     }
   }

  \%i                                                                           # Intersection of all hashes
 }

sub unionOfHashesAsArrays(@)                                                    # Form the union of the specified hashes B<@h> as one hash whose values are a array of corresponding values from each hash
 {my (@h) = @_;                                                                 # List of hashes to be united
  my %u;                                                                        # Union
  for my $i(keys @h)                                                            # Each hash to be united
   {my $h = $h[$i];                                                             # Current hash
    for my $k(keys %$h)                                                         # Keys in current hash
     {if (defined(my $v = $$h{$k}))                                             # Value defined at current key
       {$u{$k}[$i] = $v;                                                        # Add value to union array
       }
     }
   }
  \%u                                                                           # Union of all hashes
 }

sub intersectionOfHashesAsArrays(@)                                             # Form the intersection of the specified hashes B<@h> as one hash whose values are an array of corresponding values from each hash
 {my (@h) = @_;                                                                 # List of hashes to be intersected
  my $N   = @h;                                                                 # Number of hashes
  my %n;                                                                        # Count of number of hashes that have each key
  for my $h(@h)                                                                 # Each hash
   {defined($$h{$_}) ? ++$n{$_} : undef for keys %$h                            # Count the number of hashes that have this key
   }

  my %i;                                                                        # Intersection
  for my $k(keys %n)                                                            # Each key
   {if ($n{$k} == $N)                                                           # Key present in all hashes
     {$i{$k}[$_] = $h[$_]{$k} for keys @h                                       # Add hash value to intersection array
     }
   }

  \%i                                                                           # Intersection of all hashes
 }

sub setCombination(@)                                                           #P Count the elements in sets B<@s> represented as arrays of strings and/or the keys of hashes
 {my (@s) = @_;                                                                 # Array of arrays of strings and/or hashes
  my %e;
  for my $s(@s)                                                                 # Intersect each set
   {my $t = reftype($s);
    if (!defined $t)                                                            # Scalar as a set of one
     {$e{$s}++
     }
    elsif ($t =~ m(array)is)                                                    # Intersect array of strings
     {for my $e(@$s)                                                            # Count instances of each string
       {$e{$e}++
       }
     }
    elsif ($t =~ m(hash)is)                                                     # Intersect keys of hash
     {for my $e(keys %$s)                                                       # Count instances of each key
       {$e{$e}++
       }
     }
    else                                                                        # Unknown set type
     {confess "Unknown set type: $t";
     }
   }
  \%e                                                                           # Count of each set member
 }

sub setUnion(@)                                                                 # Union of sets B<@s> represented as arrays of strings and/or the keys of hashes
 {my (@s) = @_;                                                                 # Array of arrays of strings and/or hashes
  my $e = setCombination(@_);
  sort keys %$e                                                                 # Return words in union
 }

sub setIntersection(@)                                                          # Intersection of sets B<@s> represented as arrays of strings and/or the keys of hashes
 {my (@s) = @_;                                                                 # Array of arrays of strings and/or hashes
  my $e = setCombination(@_);
  my $S = @s;                                                                   # Set count
  grep {$e->{$_} == $S} sort keys %$e                                           # Return words that appear in all the sets
 }

sub setIntersectionOverUnion(@)                                                 # Returns the size of the intersection over the size of the union of one or more sets B<@s> represented as arrays and/or hashes
 {my (@s) = @_;                                                                 # Array of arrays of strings and/or hashes
  my $e = setCombination(@_);                                                   # Set element count
  my $u = keys %$e;                                                             # Union size
  $u == 0 and confess "Empty union";                                            # 0/0 can be anything
  my $S = @s;                                                                   # Set count
  my $i = grep {$e->{$_} == $S} keys %$e;                                       # Intersection size
  $i/$u                                                                         # Return ratio
 }

sub setPartitionOnIntersectionOverUnion($@)                                     # Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@sets> so that within each partition the L<setIntersectionOverUnion|/setIntersectionOverUnion> of any two sets in the partition is never less than the specified level of I<$confidence**2>
 {my ($confidence, @sets) = @_;                                                 # Minimum setIntersectionOverUnion, array of arrays of strings and/or hashes representing sets
  my @s = sort {scalar(@$b) <=> scalar(@$a)} map {[setUnion($_)]} @sets;        # Input sets as arrays in descending order of length

  my @partition;
  while(@s)                                                                     # The proposed partition
   {my $base = shift @s;                                                        # Each set starting with the largest
    next unless defined $base;                                                  # No longer present
    my @base = ($base);                                                         # Create set of elements congruent with the base set
    for my $i(keys @s)                                                          # Each remaining set
     {my $s = $s[$i];                                                           # Current set to compare with base set
      next unless defined $s;                                                   # Current set has already been classified
      last if scalar(@$s) < scalar(@$base) * $confidence;                       # Too small in comparison to the base and the sets are in descending order of size so all the remainder will have the same problem
      my $o = setIntersectionOverUnion($base, $s);                              # Overlap
      if ($o > $confidence)                                                     # Overlap is better than confidence
       {push @base, $s;                                                         # Include in partition
        $s[$i] = undef;                                                         # Remove from further consideration
       }
     }
    push @partition, \@base;                                                    # Save partition
   }
  @partition;                                                                   # Return partitions
 }

sub setPartitionOnIntersectionOverUnionOfSetsOfWords($@)                        # Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@sets> of words so that within each partition the L<setIntersectionOverUnion|/setIntersectionOverUnion> of any two sets of words in the partition is never less than the specified I<$confidence**2>
 {my ($confidence, @sets) = @_;                                                 # Minimum setIntersectionOverUnion, array of arrays of strings and/or hashes representing sets

  my %u;                                                                        # Normalized set to input sets with this normalization
  for my $s(@sets)                                                              # Each set
   {push @{$u{join ' ', setUnion($s)}}, $s;                                     # Normalized set back to each input set of words
   }
  my @partition = setPartitionOnIntersectionOverUnion($confidence,              # Partition normalized sets
    map {[split /\s+/, $_]} sort keys %u);

  my @P;
  for my $partition(@partition)                                                 # Each partition
   {my @p;
    for my $set(@$partition)                                                    # Each set in the current partition
     {push @p, @{$u{join ' ',  @$set}};
     }

    push @P, \@p;
   }
  @P
 }

sub setPartitionOnIntersectionOverUnionOfStringSets($@)                         # Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@strings>, each set represented by a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified I<$confidence**2>
 {my ($confidence, @strings) = @_;                                              # Minimum setIntersectionOverUnion, sets represented by strings

  my %u;                                                                        # Normalized set to input sets with this normalization
  for my $s(@strings)                                                           # Each set
   {my $n = nws($s =~ s([^a-z ]) ()girs);
    push @{$u{$n}}, $s;                                                         # Normalized set back to each input set of words
   }

  my @partition = setPartitionOnIntersectionOverUnionOfSetsOfWords($confidence, # Partition normalized strings
    map {[split /\s+/, $_]} sort {length($a) <=> length($b)} sort keys %u);

  my @P;                                                                        # Partition of strings
  for my $partition(@partition)                                                 # Each partition
   {my @p;
    for my $set(@$partition)                                                    # Each set in the current partition
     {push @p, @{$u{join ' ',  @$set}};
     }

    push @P, \@p;
   }
  @P
 }

sub setPartitionOnIntersectionOverUnionOfHashStringSets($$)                     # Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<$hashSet> represented by a hash, each hash value being a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified B<$confidence**2> and the partition entries are the hash keys of the string sets.
 {my ($confidence, $hashSet) = @_;                                              # Minimum setIntersectionOverUnion, sets represented by the hash value strings
  reftype($hashSet) =~ m(hash)is or confess "Second parameter must be a hash";

  my %u;                                                                        # Invert the hash so we can present the partitions by hash key
  for my $s(sort keys %$hashSet)                                                # Each set
   {push @{$u{$$hashSet{$s}}}, $s;                                              # Invert
   }

  my @partition = setPartitionOnIntersectionOverUnionOfStringSets($confidence,  # Partition strings
    sort {length($a) <=> length($b)} sort values %$hashSet);

  my @P;                                                                        # Partition of strings
  for my $partition(@partition)                                                 # Each partition
   {my @p;
    my %p;                                                                      # If n sets are identical we get n repetitions - this hash prevents that.
    for my $set(@$partition)                                                    # Each set in the current partition
     {if (my $u = $u{$set})
       {for my $U(@$u)
         {push @p, $U unless $p{$U}++;
         }
       }
     }

    push @P, [sort @p];
   }
  sort {scalar(@$b) <=> scalar(@$a)} @P
 }

sub setPartitionOnIntersectionOverUnionOfHashStringSetsInParallel($$)           # Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<$hashSet> represented by a hash, each hash value being a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified B<$confidence**2> and the partition entries are the hash keys of the string sets. The partition is performed in square root parallel.
 {my ($confidence, $hashSet) = @_;                                              # Minimum setIntersectionOverUnion, sets represented by the hash value strings
  reftype($hashSet) =~ m(hash)is or confess "Second parameter must be a hash";

  my %u;                                                                        # Invert the hash so we can present the partitions by hash key
  for my $s(sort keys %$hashSet)                                                # Each set
   {push @{$u{$$hashSet{$s}}}, $s;                                              # Invert
   }

  my @strings = sort {length($a) <=> length($b)} sort values %$hashSet;         # Strings in length order
  my @square  = squareArray(@strings);

  my @partition;

  &runInParallel(&numberOfCpus(8),                                              # Partition strings in square root blocks in parallel
  sub
   {[setPartitionOnIntersectionOverUnionOfStringSets($confidence, $_[0]->@*)];  # Partition strings
   },
  sub                                                                           # Consolidate partitions
   {for my $p(@_)
     {push @partition, @$p;
     }
   }, @square);

  my @P;                                                                        # Partition of strings
  for my $partition(@partition)                                                 # Each partition
   {my @p;
    my %p;                                                                      # If n sets are identical we get n repetitions - this hash prevents that.
    for my $set(@$partition)                                                    # Each set in the current partition
     {if (my $u = $u{$set})
       {for my $U(@$u)
         {push @p, $U unless $p{$U}++;
         }
       }
     }

    push @P, [sort @p];
   }
  sort {scalar(@$b) <=> scalar(@$a)} @P
 }

sub contains($@)                                                                # Returns the indices at which an B<$item> matches elements of the specified B<@array>. If the item is a regular expression then it is matched as one, else it is a number it is matched as a number, else as a string.
 {my ($item, @array) = @_;                                                      # Item, array
  my @r;
  if (ref($item) =~ m(Regexp))                                                  # Match via a regular expression
   {for(keys @array)
     {push @r, $_ if $array[$_] =~ m($item)s;
     }
   }
  elsif (looks_like_number($item))                                              # Match as a number
   {for(keys @array)
     {push @r, $_ if $array[$_]+0 == $item;
     }
   }
  else                                                                          # Match as a string
   {for(keys @array)
     {push @r, $_ if $array[$_] eq $item;
     }
   }
  @r
 }

sub countOccurencesInString($$)                                                 # Returns the number of occurrences in B<$inString> of B<$searchFor>.
 {my ($inString, $searchFor) = @_;                                              # String to search in, string to search for.
  my $n = 0;
  length($inString) >= length($searchFor) or
    confess "String to search must be longer than string to look for";
  my $p = -1;
  ++$n while(($p = index($inString, $searchFor, $p+1)) > -1);
  $n
 }

sub partitionStringsOnPrefixBySize                                              # Partition a hash of strings and associated sizes into partitions with either a maximum size B<$maxSize> or only one element; the hash B<%Sizes> consisting of a mapping {string=>size}; with each partition being named with the shortest string prefix that identifies just the strings in that partition. Returns a list of {prefix => size}... describing each partition.
 {my ($maxSize, %Sizes) = @_;                                                   # Maximum size of a partition, {string=>size}... hash to be partitioned

  my %paths;                                                                    # Path to each character in each string
  my %sizes;                                                                    # Size associate with each path
  for my $string(sort keys %Sizes)                                              # Create a path of hashes with single character keys
   {my $size = $Sizes{$string};                                                 # Size associated with the string
    my $paths = '';
    my @s = split m(), $string;                                                 # String as single characters
    while(@s)                                                                   # Shorten path
     {my $k = join '', map {qq({'$_'})} @s;                                     # Path of hashes with single character keys
      $paths .= qq(\$paths$k //= {};\n);                                        # Auto vivify
      my $d =  join '', @s;                                                     # Path name
      $sizes{$d} += $size;                                                      # Aggregate size
      pop @s;                                                                   # Move up one level
     }
    $sizes{q()} += $size;                                                       # Total size
    eval $paths;                                                                # Create paths - this level of aggregation seems to give the fastest overall response
    confess "$paths\n$@\n" if $@;                                               # Unable to create path
   }

  my %partition;                                                                # Partition the paths

  my $partition; $partition = sub                                               # Partition paths at the current level
   {my ($paths, @path) = @_;                                                    # Path at this level, path to this level

    my $p = join '', @path;                                                     # Path name
    my $s = $sizes{$p};                                                         # Size of path

    if ($s <= $maxSize or !keys %$paths)                                        # Small enough or complete path
     {$partition{$p} = $s;                                                      # Path => size
     }
    else                                                                        # Still too big
     {for my $d(sort keys %$paths)                                              # Next level
       {&$partition($$paths{$d}, @path, $d);                                    # Try at the next level
       }
     }
   };

  &$partition(\%paths) if keys %paths;                                          # Partition from the top

  %partition
 }

sub transitiveClosure($)                                                        # Transitive closure of a hash of hashes
 {my ($h) = @_;                                                                 # Hash of hashes

  my %keys = arrayToHash(keys %$h)->%*;                                         # Find all the keys to consider
  for my $i(keys %$h)
   {my $value = $$h{$i};
    if (reftype($value) =~ m(hash)i)
     {%keys = (%keys, arrayToHash(keys %$value)->%*);                           # Just the sub keys
     }
   }

  my %t;                                                                        # Transitive closure
  for   my $a(keys %keys)
   {my $i = $$h{$a};
    if ($i and reftype($i) =~ m(hash)i)
     {for my $b(keys %keys)
       {$t{$a}{$b} = 1 if $$i{$b}
       }
     }
   }

  for(1..100)
   {my $changes = 0;
    for   my $a(keys %keys)
     {for my $b(keys %keys)
       {if ($t{$a}{$b})
         {$t{$b}{$_} and !$t{$a}{$_}++ and ++$changes for keys %keys            # a=>b and b=>c so a=>c
         }
       }
     }
    last unless $changes;
   }

  for my $s(keys %t)                                                            # Remove empty hashes
   {delete $t{$s} unless keys $t{$s}->%*;
   }

  my %s;
  my @s;
  for my $s(sort keys %t)                                                       # Compress by creating soft pointers to common key sequences
   {my $k = join ' ', sort keys $t{$s}->%*;
    if (defined(my $i = $s{$k}))                                                # Reuse a matching entry indexed from zero
     {$t{$s} = $i
     }
    else                                                                        # Create a new entry
     {push @s, $t{$s}; $t{$s} = $s{$k} = @s - 1;
     }
   }

  genHash(q(Data::Table::Text::TransitiveClosure),
    start => \%t,
    end   => \@s,
   )
 } # transitiveClosure

#D1 Format                                                                      # Format data structures as tables.

sub maximumLineLength($)                                                        # Find the longest line in a B<$string>.
 {my ($string) = @_;                                                            # String of lines of text
  max(map {length($_)} split /\n/, ($string//'')) // 0                          # Length of longest line
 }

sub formatTableMultiLine($;$)                                                   #P Tabularize text that has new lines in it.
 {my ($data, $separator) = @_;                                                  # Reference to an array of arrays of data to be formatted as a table, optional line separator to use instead of new line for each row.
  ref($data) =~ /array/i or
    confess "Array reference required not:\n".dump($data)."\n";

  my @width;                                                                    # Maximum width of each column
  for my $row(@$data)                                                           # Find maximum width of each column
   {ref($row) =~ /array/i or
      confess "Array reference required not:\n".dump($row)."\n";
    for my $col(0..$#$row)                                                      # Each column index
     {my $a = $width[$col] // 0;                                                # Maximum length of data so far
      my $b = maximumLineLength($row->[$col]);                                  # Length of longest line in current item
      $width[$col] = ($a > $b ? $a : $b);                                       # Update maximum length
     }
   }

  my @text;                                                                     # Formatted data
  for   my $row(@$data)                                                         # Each row
   {my @row;                                                                    # Laid out text
    for my $col(0..$#$row)                                                      # Each column
     {my $m = $width[$col];                                                     # Maximum width
      for my $i(split /\n/, $row->[$col]//'')                                   # Each line of item
       {if ($i !~ /\A\s*[-+]?\s*(\d|[,])+(\.\d+)?([Ee]\s*[-+]?\s*\d+)?\s*\Z/)   # Not a number - left justify
         {push @{$row[$col]}, substr($i.(' 'x$m), 0, $m);
         }
        else                                                                    # Number - right justify
         {push @{$row[$col]}, substr((' 'x$m).$i, -$m);
         }
       }
     }

    my $n = max(map {scalar @{$_//[]}} @row)//0;                                # Maximum number of rows

    for my $r(1..$n)                                                            # Each row of the items
     {my $text = '';
      for my $col(0..$#$row)                                                    # Each item
       {$text .= ($row[$col][$r-1] // (q( ) x $width[$col])).q(  );
       }
      $text =~ s(\s*\Z) ()s;                                                    # Strip trailing blanks as they are not needed for padding
      push @text, $text;
     }
   }

  my $s = $separator//"\n";
  join($s, @text).$s
 }

sub formatTableBasic($)                                                         # Tabularize an array of arrays of text.
 {my ($data) = @_;                                                              # Reference to an array of arrays of data to be formatted as a table.
  ref($data) =~ /array/i or                                                     # Must be an array
    confess "Array reference required not:\n".dump($data)."\n";
  my @width;                                                                    # Maximum width of each column

  for   my $row(@$data)                                                         # Each row
   {for my $col(0..$#$row)                                                      # Each column index
     {my $text  = $row->[$col] // '';                                           # Text of current line
      return &formatTableMultiLine(@_) if $text =~ m(\n);                       # Element has a new line in it
      my $a  = $width[$col] // 0;                                               # Maximum length of data so far
      my $b  = length($text);                                                   # Length of longest line in current item
      $width[$col] = ($a > $b ? $a : $b);                                       # Update maximum length
     }
   }

  my @text;                                                                     # Formatted data
  for my $row(@$data)
   {my $text = '';                                                              # Formatted text
    for my $col(0..$#$row)
     {my $m = $width[$col];                                                     # Maximum width
      my $i = $row->[$col]//'';                                                 # Current item
      if ($i !~ /\A\s*[-+]?\s*(\d|[,])+(\.\d+)?([Ee]\s*[-+]?\s*\d+)?\s*\Z/)     # Not a number - left justify
       {$text .= substr($i.(' 'x$m), 0, $m)."  ";
       }
      else                                                                      # Number - right justify
       {$text .= substr((' 'x$m).$i, -$m)."  ";
       }
     }
    $text =~ s(\s*\Z) ()s;                                                      # Strip trailing blanks as they are not needed for padding
    push @text, $text;
   }

  join("\n", @text)."\n"
 }

sub formatTableClearUpLeft($)                                                   #P Blank identical column values up and left
 {my ($data) = @_;                                                              # Array of arrays

  for   my $row(1..@$data)                                                      # Each row from last to first
   {my $d = $$data[-$row];
    last if $row == @$data;

    my $p = $row+1;
    for my $c(reverse 1..@$d)                                                   # Compare left values in current row to previous row
     {next unless my $dc = $$d[-$c];
      next unless my $pc = $$data[-$p][-$c];
      if ($dc eq $pc)                                                           # Blank equal value
       {$$d[-$c] = q();
       }
      else                                                                      # Values not equal terminates equal valued column suppression
       {last;
       }
     }
   }
 }

sub formatTableAA($$%)                                                          #P Tabularize an array of arrays.
 {my ($data, $title, %options) = @_;                                            # Data to be formatted, reference to an array of titles, options
  return dump($data) unless ref($data) =~ /array/i and @$data;

  my $d;                                                                        # Copy of the input data because we are going to modify it
  for   my $row(@$data)                                                         # Each row
   {ref($row) =~ /array/i or                                                    # Each row must be an array
      confess "Array reference required not:\n".dump($row)."\n";
    push @$d, [q(), @$row];                                                     # Copy each row with space for a row number
   }

  if (my $w = $options{maximumColumnWidth})                                     # Apply maximum column width if supplied
   {for my $r(@$d)
     {for(@$r)
       {$_ = substr($_, 0, $w) if length > $w;
       }
     }
   }

  formatTableClearUpLeft($d) if $options{clearUpLeft};                          # Clear up and left if requested
  $$d[$_-1][0] = $_ for 1..@$data;                                              # Number each row now that we have suppressed duplicates
  unshift @$d, ['', @$title] if $title;                                         # Add title
  formatTableBasic($d);                                                         # Format array
 }

sub formatTableHA($;$)                                                          #P Tabularize a hash of arrays.
 {my ($data, $title) = @_;                                                      # Data to be formatted, optional titles
  return dump($data) unless ref($data) =~ /hash/i and keys %$data;
  my $d;
  push @$d, $title if $title;
  push @$d, [$_, @{$data->{$_}}] for sort keys %$data;
  formatTableBasic($d);
 }

sub formatTableAH($)                                                            #P Tabularize an array of hashes.
 {my ($data) = @_;                                                              # Data to be formatted
  return dump($data) unless ref($data) =~ /array/i and @$data;

  my %k; @k{keys %$_}++ for @$data;                                             # Column headers
  my @k = sort keys %k;
  $k{$k[$_-1]} = $_ for 1..@k;

  my $d = [['', @k]];
  for(1..@$data)
   {push @$d, [$_];
    my %h = %{$data->[$_-1]};
    $d->[-1][$k{$_}] = $h{$_} for keys %h;
   }
  formatTableBasic($d);
 }

sub formatTableHH($)                                                            #P Tabularize a hash of hashes.
 {my ($data) = @_;                                                              # Data to be formatted
  return dump($data) unless ref($data) =~ /hash/i and keys %$data;

  my %k; @k{keys %$_}++ for values %$data;                                      # Column headers
  my @k = sort keys %k;
  $k{$k[$_-1]} = $_ for 1..@k;

  my $d = [['', @k]];
  for(sort keys %$data)
   {push @$d, [$_];
    my %h = %{$data->{$_}};
    $d->[-1][$k{$_}] = $h{$_} for keys %h;
   }
  formatTableBasic($d);
 }

sub formatTableA($;$)                                                           #P Tabularize an array.
 {my ($data, $title) = @_;                                                      # Data to be formatted, optional title
  return dump($data) unless ref($data) =~ /array/i and @$data;

  my $d;
  push @$d, $title if $title;
  for(keys @$data)
   {push @$d, @$data > 1 ? [$_, $data->[$_]] : [$data->[$_]];                   # Skip line number if the array is degenerate
   }
  formatTableBasic($d);
 }

sub formatTableH($;$)                                                           #P Tabularize a hash.
 {my ($data, $title) = @_;                                                      # Data to be formatted, optional title

  return dump($data) unless ref($data) =~ /hash/i and keys %$data;

  my $d;
  push @$d, $title if $title;
  for(sort keys %$data)
   {push @$d, [$_, $data->{$_}];
   }
  formatTableBasic($d);
 }

our @formatTables;                                                              # tttt Report of all the reports that have been created

sub formatTableCheckKeys                                                        #P Options available for formatting tables
 {{title       => <<'END',
Title for the table
END
     head      => <<'END',
Header text which will preceed the formatted table.
DDDD will be replaced with the current date and time.
NNNN will be replaced with the number of rows in the table.
TTTT will be replaced with the title from the title keyword
END
     columns   => <<'END',
Definition of each column one per line: the first word is the name of the column, while subsequent words describe the column.
END
     foot      => <<'END',
Footer text which will follow the table
END
     summarize => <<'END',
If true, each column of an array of arrays will be summarized by printing its
distinct values and a count of how often each value occurs in a series of
smaller tables following the main table.
END
     clearUpLeft => <<'END',
If numeric +/-\$N, replace any left hand column values repeated in the
following row with white space to make it easier to follow the range of keys.
If a positive count is given the clearing will always be stopped after the
numbered column (based from 1) if negative, then clearing will be stopped after
the column obtained by counting back counting 1-\$N columns from the last
column. Thus a value of -1 will stop clearing after the final column which
could potentially produce a blank row if there are two duplicate rows in
sequence.
END
     file   => q(The name of a file to which to write the formatted table.),
     rows   => q(The number of rows in the report),
     zero   => q(Write the report even if the table is empty.),
     wide   => q(Write a note explaining the need to scroll to the right if true),
     msg    => q(Write a message to STDERR summarizing the situation if true),
     csv    => q(Write a csv version of the report if true),
     indent => q(Number of spaces to be used to indent the table, defaults to zero),
     debug  => q(Debug table processing),
     facet  => <<END,
Counts in html reports with the same facet will be plotted on the same chart to
provide a visual indication of their relative sizes.
END
     aspectColor => <<END,
The color in which to draw this aspect on charts and graphs.
END
     maximumColumnWidth => <<END,
The maximum width permitted for a column, defaults to unlimited.
END
 }} # formatTableCheckKeys

sub formatTable($;$%)                                                           #I Format various B<$data> structures as a table with titles as specified by B<$columnTitles>: either a reference to an array of column titles or a string each line of which contains the column title as the first word with the rest of the line describing that column.\mOptionally create a report from the table using the report B<%options> described in L<formatTableCheckKeys>
 {my ($data, $columnTitles, @options) = @_;                                     # Data to be formatted, optional reference to an array of titles or string of column descriptions, options

  my %options = sub                                                             # Make column titles an option so that the options list is easily reused. The original arrangement where column titles were a separate (optional) parameter will eventually be deprecated. To make this work, columns=> has to be the first option.
   {if ($columnTitles and !ref($columnTitles) and
        $columnTitles eq q(columns) and scalar(@options) % 2 == 1)
     {my %o = ($columnTitles, @options);
      $columnTitles = $o{columns};
      return %o;
     }
    scalar(@options) % 2 and confess "Options fail to pair";
    @options
   }->();

  checkKeys(\%options, formatTableCheckKeys);                                   # Check report options

  my ($titleString, $title) = sub                                               # Title string, column headers
   {return (undef, undef) unless defined $columnTitles;                         # No titles
    if (my $r = reftype $columnTitles)                                          # Array of column titles
     {return (undef, $columnTitles) if $r =~ m(\Aarray\Z)si;
     }
    return (q(), q()) unless $columnTitles;                                     # Column titles are not required for hash of hashes
    my @c = map {[split m(\s+), $_, 2]} split "\n", $columnTitles;              # Column definitions
    my $s = &formatTable(\@c, [qw(Column Description)]);                        # Column definitions descriptions table
   ($s, [map {$$_[0]} @c])
   }->();

  my ($a, $h, $o) = (0, 0, 0);                                                  # Check structure of input data tttt
  my $checkStructure = sub
   {for(@_)
     {my $r = reftype($_);                                                      # Process arrays and hashes or objects built on them
      if ($r)
       {if ($r =~ /array/i)   {++$a}
        elsif ($r =~ /hash/i) {++$h}
        else                  {++$o}
       }
      else                    {++$o}
     }
   };

  my $formattedTable = sub                                                      # Format table
   {if    (reftype($data) =~ /array/i)
     {$checkStructure->(       @$data);
      return formatTableAA($data, $title, %options) if  $a and !$h and !$o;
      return formatTableAH($data)                   if !$a and  $h and !$o;
      return formatTableA ($data, $title);
     }
    elsif (reftype($data) =~ /hash/i)
     {$checkStructure->(values %$data);
      return formatTableHA($data, $title) if  $a and !$h and !$o;
      return formatTableHH($data)         if !$a and  $h and !$o;
      return formatTableH ($data, $title);
     }
   }->();

  return $formattedTable unless keys %options;                                  # Return table as is unless report requested

  my ($Title, $head, $foot, $file, $zero, $summarize, $wide, $msg, $csv, $zwsp, $indent) = map{$options{$_}} # Options requested
    qw(title   head   foot   file   zero   summarize   wide   msg   csv   zwsp   indent);

  my @report;
  my $date = dateTimeStamp;
  my $N    = keyCount(1, $data);
  my $H    = ($head//'') =~ s(DDDD) ($date)gr =~ s(NNNN) ($N)gr;
     $H    =~ s(TTTT) ($title)gs          if $Title;
  push @report, $Title                    if $Title;
  push @report, $H                        if $head;
  push @report, qq(This file: $file)      if $file;
  push @report, $titleString              if $titleString;
  push @report, <<END                     if $wide;
Please note that this is a wide report: you might have to scroll
a long way to the right to see all the columns of data available!
END
  push @report, <<END                     if $summarize;
Summary_of_column                - Count of unique values found in each column                     Use the Geany flick capability by placing your cursor on the first word
Comma_Separated_Values_of_column - Comma separated list of the unique values found in each column  of these lines and pressing control + down arrow to see each sub report.
END

  push @report, $formattedTable;
  push @report, $foot                     if $foot;

  push @formatTables, [$N, $Title//nws($H, 80), $file];                         # Report of all the reports created

  if ($msg and $file and $head)
   {lll $H =~ s(\n.*\Z) ()gsr;
    lll qq(See file: $file);
   }

  if ($summarize)                                                               # Summarize an array of arrays if requested
   {my $s = '';
    if (reftype($data) =~ /array/i)
     {if ($a and !$h and !$o)
       {for my $col(1..@$title)
         {my $n = $title->[$col-1];
          my $c = qq(Summary_of_column_$n);
          my @s = summarizeColumn($data, $col-1);
          my $t = &formatTable(\@s, [q(Count), $n]);
          $s .= qq($c\n$t\n);
          if (1)
           {my $v = join ",", sort map {dump($$_[1])} @s;
            $s .= "Comma_Separated_Values_of_column_$n: $v\n\n";
           }
         }
       }
     }
    push @report, $s;
   }

  if ($file)                                                                    # Write a csv version of the report (Sabine)
   {if (reftype($data) =~ /array/i)
     {if ($a && !$h && !$o or $zero)
       {my @s;

        if ($title)                                                             # Column headers
         {my $r = join ',', map {defined($_) ? $_ : q(unknown)} @$title;
          push @s, $r;
         }

        for my $d(@$data)
         {push @s, join ',', map{dump($_)} @$d;
         }
        my $csvFile = setFileExtension($file, q(csv));
        my $csvData = join "\n", @s;
        overWriteFile($csvFile, "$csvData\n");
       }
     }
   }

  my $report = join "\n\n", @report;                                            # Create report

  overWriteFile($file, $report) if $file and $a+$h+$o || $zero;                 # Only write the report if there is some data in it or the zero option has been specified to write it regardless.

  $report = indentString($report, $indent) if $indent;                          # Indent table if requested

  $report
 } # formatTable

sub formattedTablesReport(@)                                                    # Report of all the reports created. The optional parameters are the same as for L<formatTable|/formatTable>
 {my (@options) = @_;                                                           # Options

  formatTable([sort {($a->[1]//'') cmp ($b->[1]//'')} @formatTables], <<END,
Rows   Number of entries in table
Title  Title of the report
File   File containing the report
END
    @options);
 }

sub summarizeColumn($$)                                                         # Count the number of unique instances of each value a column in a table assumes.
 {my ($data, $column) = @_;                                                     # Table == array of arrays, column number to summarize.
  my @data = map {$$_[$column]} @$data;
  my %data;
  for my $d(@data)
   {$data{$d}++ if defined $d;
   }
  sort {return $$a[1] cmp $$b[1] if $$b[0] == $$a[0];                           # Return array of [count, key]
        return $$b[0] <=> $$a[0]} map {[$data{$_}, $_]} sort keys %data;
 }

sub keyCount($$)                                                                # Count keys down to the specified level.
 {my ($maxDepth, $ref) = @_;                                                    # Maximum depth to count to, reference to an array or a hash
  my $n = 0;
  my $count;
  $count = sub
   {my ($ref, $currentDepth) = @_;
    if (ref($ref) =~ /array/i)
     {if ($maxDepth == $currentDepth) {$n += scalar(@$ref)}
      else {$count->($_, ++$currentDepth)       for @$ref}
     }
    elsif (ref($ref) =~ /hash/i)
     {if ($maxDepth == $currentDepth)   {$n += scalar(keys %$ref)}
      else {$count->($ref->{$_}, ++$currentDepth) for keys %$ref}
     }
    else {++$n}
   };
  $count->($ref, 1);
  $n
 }

sub formatHtmlTable($%)                                                         # Format an array of arrays of scalars as an html table using the  B<%options> described in L<formatTableCheckKeys>.
 {my ($data, %options) = @_;                                                    # Data to be formatted, options
  my $rows = $data ? scalar(@$data) : 0;                                        # The number of rows in the report

  checkKeys(\%options, formatTableCheckKeys);                                   # Check report options

  if (!$options{zero} and $data and ref($data) =~ m(array)i and !@$data)        # Return empty string if the table is empty unless the zero option has been supplied
   {return q()
   }

  my @html;                                                                     # Generated html
  my $cl = q();                                                                 # Table column names
  my $ct = q();                                                                 # Columns description table if present

  if (my $columns = $options{columns})                                          # Column headers
   {ref($columns) and confess <<END;                                            # Describe column option
Expected one line per column wiith the forst weor dbeing teh column name and
the remainder being a comment describing the comment.
END
    my @c = map {[split m(\s+), $_, 2]} split "\n", $columns;                   # Parse column headers
    $cl = join '', q(<tr><th>), join q(<th>),
      map {my ($c, $d) = @$_; qq(<span title="$d">$c</span>)} @c;               # Column line with tool tips
    $ct = join "\n", q(<p><pre>), formatTableBasic([@c]), qq(</pre></p>\n);     # Column format
   }

  if (my $title = $options{title})                                              # Title
   {push @html, <<END;
<h1>$title</h1>
END
   }

  my $hf = sub                                                                  # Header / Footer
   {my ($text) = @_;                                                            # Text of header or footer
    my $d = dateTimeStamp;
    my $t = ($text//'') =~ s(DDDD) ($d)gr =~ s(NNNN) ($rows)gr;                 # Edit in NNNN and DDDD fields

    push @html, <<END;
<p>$t</p>
END
   };

  if (my $head = $options{head})                                                # Header
   {&$hf($head);
   }

  push @html, <<END;                                                            # Table start
<p><table borders="0" cellpadding="10" cellspacing="5">
END

  push @html, $cl if $cl;                                                       # Column headers

  if ($data)                                                                    # Table data
   {for my $data(@$data)
     {push @html, join '', q(<tr><td>), join q(<td>), map {$_//q()} @$data;
     }
   }

  push @html, <<END;                                                            # Table end
</table></p>
END

  push @html, $ct if $ct;                                                       # Column descriptions block

  if (my $foot = $options{foot})                                                # Footer
   {&$hf($foot);
   }

  if (1)                                                                        # Record options invisibly
   {my $options = dump({%options, rows=>$rows});
    push @html, qq(<span class="options" style="display: none">$options</span>);
   }

  my $html = join "\n", @html;                                                  # Create html
  if (my $file = $options{file})
   {my $html = join "\n", @html;
    overWriteFile($file, $html);
   }

  $html
 } # formatHtmlTable

sub formatHtmlTablesIndex($$$;$)                                                # Create an index of html reports.
 {my ($reports, $title, $url, $columns) = @_;                                   # Reports folder, title of report of reports, $url to get files, number of columns - defaults to 1
  $columns //= 1;

  my %reports = sub                                                             # {file=>options} for each html report
   {my @r = searchDirectoryTreesForMatchingFiles($reports, q(.html));           # Find all html reports
    my %r;
    for my $r(@r)                                                               # Each html report
     {my $t = readFile($r);
      if ($t =~ m(<span class="options" style="display: none">(.*?)</span>)s)   # Extract report meta data
       {my $d = eval $1;
        $@ and confess "Cannot retrieve report metadata:\n$r\n$@\n";
        if (my $t = $$d{title})
         {$r{$t} = $d;
         }
        else
         {cluck "No title in file:\n$r\n";
         }
       }
     }
    %r
   }->();

  my @toc; my %class;                                                           # List of reports
  for my $title(sort keys %reports)                                             # Each report
   {my $options = $reports{$title};
    my $rows    = $$options{rows};
    next unless my $file = $$options{file};
    my $class   = containingFolderName($file); $class{$class}++;                # Classification for report
    my $href    = qq($url$file);
    my $link    = qq(<a href="$href">$title</a>);
    my $tick    = sub                                                           # Flag items that we would like to be zero
     {return q() unless $file =~ m(/bad/) and $rows;
      q(<svg width="1em" height="1em"><circle cx="50%" cy="50%" r="50%" fill="red" width="1em" height="1em"/></svg>)
     }->();

    my $c = qq( class="report report_$class");                                  # Classification

    push @toc, join '', qq(<td $c align="right">),
               join(    qq(<td $c>), $rows, $tick, $link);
   }

  my $tocs    = @toc;
# my @tocs    = rectangularArray(int(@toc / $columns), @toc);                   # Divide into columns
  my @tocs    = rectangularArray2($columns, @toc);                               # Divide into columns
  my $toc     = join "\n", map {q(<tr>).join ' ', @$_} @tocs;
  my $dt      = dateTimeStamp;                                                  # Date of run
  my $t       = $title ? qq(<h1>$title</h1>) : q();                             # Title if present

  my $groups  = join ', ', map {qq("$_")} sort keys %class;                     # Groups
  my $select  = join '',   map {<<END}    sort keys %class;                     # Group select buttons
<td><a class="select select_$_ sizeLargeBold" onclick='show_report("$_")'>$_</a>
END

  push my @html, <<END;                                                         # Html
<style>
.sizeLargeBold
 {font-size: 1.2em;
  font-weight: bold;
 }
</style>

<table border="0" cellpadding="10">
<tr>
<td><big><b>$tocs</b> reports available on $dt</big>
<td><a class="sizeLargeBold" onclick='resetAllVisible(false)'>Hide All</a>
$select
<td><a class="sizeLargeBold" onclick='resetAllVisible(true)' >Show All</a>
</table>
<p><table border="0" cellpadding="10">
$toc
</table></p>
<script>
const groups = [$groups];                                                       // Groups available
const hidden = {};                                                              // Groups that hav been hidden

function setGroupEmphasis()                                                     // Set the emphasis for each group depending on whether we are showing this group or not
 {for (let group of groups)
   {const S = document.getElementsByClassName("select_"+group);
    for(let s of S)
     {if (hidden[group])
       {s.classList.remove('sizeLargeBold');
       }
      else
       {s.classList.add('sizeLargeBold');
       }
     }
   }
 }

function show_report(group)                                                     // Toggle the visibility of a group
 {hidden[group] = !hidden[group];
  const h = hidden[group] ? 'hidden' :'visible';
  setGroupEmphasis();
  const reports = document.getElementsByClassName("report");
  for (let r of reports)
   {if (r.classList.contains('report_'+group))
     {r.style.visibility = h;
     }
   }
 }

function resetAllVisible(show)
 {for (let g of groups)
   {hidden[g] = show;
    show_report(g);
   }
 }
</script>
END

  my $html = join "\n", @html;                                                  # Create html

  if (my $out = fpe($reports, qw(index_of_reports html)))
   {owf($out, $html);
   }

  $html                                                                         # Return the html so created
 } # formatHtmlTablesIndex

my @formatHtmlAndTextTablesPids;                                                # Pids used to format tables in parallel

sub formatHtmlAndTextTablesWaitPids                                             # Wait on all table formatting pids to complete
 {waitpid $_, 0 for @formatHtmlAndTextTablesPids;
 }

sub formatHtmlAndTextTables($$$$$%)                                             # Create text and html versions of a tabular report
 {my ($reports, $html, $getFile, $filePrefix, $data, %options) = @_;            # Folder to contain text reports, folder to contain html reports, L<url> to get files, file prefix to be removed from file entries or array of file prefixes, data, options

  my @prefix  = ref($filePrefix) ? @$filePrefix : $filePrefix;                  # Flatten file prefixes into array
  my $file    = $options{file};                                                 # Relative report file
  my $columns = $options{columns};                                              # Columns must come first for the moment

  if ($reports)                                                                 # Format table as text
   {push @formatHtmlAndTextTablesPids, my $pid = fork;
    unless($pid)
     {my $out = setFileExtension fpf($reports, $file), q(txt);                  # Output file name
      formatTable($data, columns=>$columns, %options, file=>$out);
      exit;
     }
   }

  if ($html)                                                                    # Format table as html
   {push @formatHtmlAndTextTablesPids, my $pid = fork;
    unless($pid)
     {my $out = setFileExtension fpf($html, $file), q(html);                    # Output file name
      my $start = time;
      my $h   = sub                                                             # Turn file names into links in a table of scalars
       {my @r;
        for my $row(@$data)
         {my @c;
          for my $col(@$row)
           {push @c, sub
             {for my $filePrefix(@prefix)                                       # Try each file prefix
               {if ($col and $col =~ m(\A$filePrefix)s)
                 {return qq(<a href="$getFile$col">).
                    swapFilePrefix($col, $filePrefix).q(</a>);
                 }
               }
              $col                                                              # Use plain file name as no prefix matched
             }->();
           }
          push @r, \@c;
         }
        \@r                                                                     # Return edited rows as a reference for convenient use with formatTable
       }->();

      formatHtmlTable($h, %options, file => $out);                              # Format table as html
      exit;
     }
   }
 }  # formatHtmlAndTextTables

#D1 Lines                                                                       # Load data structures from lines.

sub loadArrayFromLines($)                                                       # Load an array from lines of text in a string.
 {my ($string) = @_;                                                            # The string of lines from which to create an array
  [grep {!/\A#/} split "\n", $string]
 }

sub loadHashFromLines($)                                                        # Load a hash: first word of each line is the key and the rest is the value.
 {my ($string) = @_;                                                            # The string of lines from which to create a hash
  +{map{split /\s+/, $_, 2} split "\n", $string}
 }

sub loadArrayArrayFromLines($)                                                  # Load an array of arrays from lines of text: each line is an array of words.
 {my ($string) = @_;                                                            # The string of lines from which to create an array of arrays
  [map{[split /\s+/]} split "\n", $string]
 }

sub loadHashArrayFromLines($)                                                   # Load a hash of arrays from lines of text: the first word of each line is the key, the remaining words are the array contents.
 {my ($string) = @_;                                                            # The string of lines from which to create a hash of arrays
  +{map{my @a = split /\s+/; (shift @a, [@a])} split "\n", $string}
 }

sub loadArrayHashFromLines($)                                                   # Load an array of hashes from lines of text: each line is a hash of words.
 {my ($string) = @_;                                                            # The string of lines from which to create an array of arrays
  [map {+{split /\s+/}} split /\n/, $string]
 }

sub loadHashHashFromLines($)                                                    # Load a hash of hashes from lines of text: the first word of each line is the key, the remaining words are the sub hash contents.
 {my ($string) = @_;                                                            # The string of lines from which to create a hash of arrays
  +{map{my ($a, @a) = split /\s+/; ($a=>{@a})} split "\n", $string}
 }

sub checkKeys($$)                                                               # Check the keys in a B<hash> confirm to those B<$permitted>.
 {my ($hash, $permitted) = @_;                                                  # The hash to test, a hash of the permitted keys and their meanings

  ref($hash)      =~ /hash/igs or                                               # Check parameters
    confess "Hash reference required for first parameter\n";
  ref($permitted) =~ /hash/igs or
    confess "Hash reference required for second parameter\n";

  my %parms = %$hash;                                                           # Copy keys supplied
  delete $parms{$_} for keys %$permitted;                                       # Remove permitted keys
  return '' unless keys %parms;                                                 # Success - all the keys in the test hash are permitted

  confess join "\n",                                                            # Failure - explain what went wrong
   "Invalid options chosen:",
    indentString(formatTable([sort keys %parms]), '  '),
   "",
   "Permitted options are:",
    indentString(formatTable($permitted),         '  '),
   "";
 }

#D1 LVALUE methods                                                              # Replace $a->{B<value>} = $b with $a->B<value> = $b which reduces the amount of typing required, is easier to read and provides a hard check that {B<value>} is spelled correctly.

sub genLValueScalarMethods(@)                                                   # Generate L<lvalueMethod> scalar methods in the current package, A method whose value has not yet been set will return a new scalar with value B<undef>. Suffixing B<X> to the scalar name will confess if a value has not been set.
 {my (@names) = @_;                                                             # List of method names
  my ($package) = caller;                                                       # Package
  for my $m(@_)                                                                 # Name each method
   {my $s;
    if ($m =~ m(::)s)                                                           # Package name supplied in name
     {my $M = $m =~ s(\A.*:) ()r;                                               # Remove package
      $s =
       'sub '.$m. ':lvalue {$_[0]{"'.$M.'"}}'.                                  # LValue version for get and set
       'sub '.$m.'X        {$_[0]{"'.$M.'"} // q()}';                           # Non lvalue version for get only returning q() instead of B<undef>
     }
    else                                                                        # Use package of caller
     {$s =
       'sub '.$package.'::'.$m. ':lvalue {$_[0]{"'.$m.'"}}'.                    # LValue version for get and set
       'sub '.$package.'::'.$m.'X        {$_[0]{"'.$m.'"} // q()}';             # Non lvalue version for get only returning q() instead of undef
     }
 #   'sub '.$package.'::'.$_. ':lvalue {my $v;       $_[0]{"'.$_.'"} //= $v}'.
 #   'sub '.$package.'::'.$_.'X:lvalue {my $v = q(); $_[0]{"'.$_.'"} //= $v}';
 #   'sub '.$package.'::'.$_.'X:lvalue {my $v =      $_[0]{"'.$_.'"}; confess q(No value supplied for "'.$_.'") unless defined($v); $v}';
    eval $s;
    confess "Unable to create LValue scalar method for: '$m' because\n$@\n" if $@;
   }
 }

sub addLValueScalarMethods(@)                                                   # Generate L<lvalueMethod> scalar methods in the current package if they do not already exist. A method whose value has not yet been set will return a new scalar with value B<undef>. Suffixing B<X> to the scalar name will confess if a value has not been set.
 {my (@names) = @_;                                                             # List of method names
  my ($package) = caller;                                                       # Package
  for my $m(@_)                                                                 # Name each method
   {my $M = $m =~ m(::)s ? $m : $package.'::'.$m;
    next if defined &$M;
    genLValueScalarMethods($M);
   }
 }

sub genLValueScalarMethodsWithDefaultValues(@)                                  # Generate L<lvalueMethod> scalar methods with default values in the current package. A reference to a method whose value has not yet been set will return a scalar whose value is the name of the method.
 {my (@names) = @_;                                                             # List of method names
  my ($package) = caller;                                                       # Package
  for(@_)                                                                       # Name each method
   {my $s = 'sub '.$package.'::'.$_.':lvalue {my $v = "'.$_.'"; $_[0]{"'.$_.'"} //= $v}';
    eval $s;
    confess "Unable to create LValue scalar method for: '$_' because\n$@\n" if $@;
   }
 }

sub genLValueArrayMethods(@)                                                    # Generate L<lvalueMethod> array methods in the current package. A reference to a method that has no yet been set will return a reference to an empty array.
 {my (@names) = @_;                                                             # List of method names
  my ($package) = caller;                                                       # Package
  for(@_)                                                                       # Name each method
   {my $s = 'sub '.$package.'::'.$_.':lvalue {$_[0]{"'.$_.'"} //= []}';
    eval $s;
    confess "Unable to create LValue array method for: '$_' because\n$@\n" if $@;
   }
 }

sub genLValueHashMethods(@)                                                     # Generate L<lvalueMethod> hash methods in the current package. A reference to a method that has no yet been set will return a reference to an empty hash.
 {my (@names) = @_;                                                             # Method names
  my ($package) = caller;                                                       # Package
  for(@_)                                                                       # Name each method
   {my $s = 'sub '.$package.'::'.$_.':lvalue {$_[0]{"'.$_.'"} //= {}}';
    eval $s;
    confess "Unable to create LValue hash method for: '$_' because\n$@\n" if $@;
   }
 }

my %genHash;                                                                    # Hash of methods created by genHash - these methods can be reused - others not so created cannot.

sub genHash($%)                                                                 #I Return a B<$bless>ed hash with the specified B<$attributes> accessible via L<lvalueMethod> method calls. L<updateDocumentation|/updateDocumentation> will generate documentation at L<Hash Definitions> for the hash defined by the call to L<genHash|/genHash> if the call is laid out as in the example below.
 {my ($bless, %attributes) = @_;                                                # Package name, hash of attribute names and values
  my $h = \%attributes;
  bless $h, $bless;
  for my $m(sort keys %attributes)                                              # Add any attributes not already present
   {unless ($m =~ m(\A[a-z_](\w|:)*\Z)is)                                       # Silently skip anything we could not reasonably use as an attribute name
     {confess qq(Implausibly named attribute: "$m"\n);
     }

    my $M = $bless.q(::).$m;                                                    # The full name of the attribute

    if ($h->can($m))                                                            # Skip any methods that are already defined
     {say STDERR dump(\%genHash, $m, $M) unless $genHash{$M};

      confess "Cannot define attribute because there is already ".
              "a method with the same name: $m\n" unless $genHash{$M};

      next;
     }

    if ($h->can($m.q(X)))                                                       # Confess to any methods that collide with X names
     {confess "Cannot define attribute because there is already ".
              "an X method with the same name: $m\n";
     }

    my $R = reftype($attributes{$m});                                           # Type of thing referred to
    my $r = !defined($R) ? q() : $R =~ m(array)i ? q( //= []) : q( //= {});     # Empty return type
    my $s = '';
    $s .= 'sub '.$bless.'::'.$m. ':lvalue {$_[0]{"'.$m.qq("}$r})."\n";          # LValue version for get and set
    $s .= 'sub '.$bless.'::'.$m. 'X       {$_[0]{"'.$m.'"}//q()}'."\n";         # Default to blank for get
    if ($s)                                                                     # Add any new methods needed
     {eval $s;
      confess "$@\n$s\n$@" if $@;
     }
    $genHash{$M}++;                                                             # Record attribute as being created by genHash
   }

  $h
 }

sub loadHash($%)                                                                # Load the specified blessed B<$hash> generated with L<genHash|/genHash> with B<%attributes>. Confess to any unknown attribute names.
 {my ($hash, %attributes) = @_;                                                 # Hash, hash of attribute names and values to be loaded
  for my $m(sort keys %attributes)                                              # Add any attributes not already present
   {$hash->can($m) or confess "Cannot load attribute: $m\n";                    # Unknown attribute
    $hash->{$m} = $attributes{$m};                                              # Load known attribute
   }
  $hash                                                                         # Return loaded hash
 }

my $reloadHashesCount = 0;                                                      # Generate names for reloaded hashes that are not already blessed

sub reloadHashes2($$)                                                           #P Ensures that all the hashes within a tower of data structures have LValue methods to get and set their current keys.
 {my ($d, $progress) = @_;                                                      # Data structure, progress
  return unless my $r = reftype($d);
  return if $$progress{$d};
  if ($d =~ m(array)is)                                                         # Array
   {$$progress{$d}++;
    &reloadHashes2($_, $progress) for @$d;
   }
  elsif ($d =~ m(hash)is)                                                       # Hash
   {$$progress{$d}++;
    &reloadHashes2($_, $progress) for values %$d;
    if (my $b = blessed($d))                                                    # Already blessed
     {genHash($b, %$d);
     }
    else                                                                        # Create a name
     {my $b = q(reloadHash_).++$reloadHashesCount;
      bless($d, $b);                                                            # Bless hash
      genHash($b, %$d);
     }
   }
 }

sub reloadHashes($)                                                             # Ensures that all the hashes within a tower of data structures have LValue methods to get and set their current keys.
 {my ($d) = @_;                                                                 # Data structure
  reloadHashes2($d, {});
  $d
 }

sub showHashes2($$$)                                                            #P Create a map of all the keys within all the hashes within a tower of data structures.
 {my ($d, $keys, $progress) = @_;                                               # Data structure, keys found, progress
  return unless my $r = reftype($d);
  return if $$progress{$d};
  if ($d =~ m(array)is)
   {$$progress{$d}++;
    &showHashes2($_, $keys, $progress) for @$d;
   }
  elsif ($d =~ m(hash)is)
   {$$progress{$d}++;
    &showHashes2($_, $keys, $progress) for values %$d;
    if (my $b = blessed($d))
     {for my $k(keys %$d)
       {$keys->{$b}{$k}++
       }
     }
   }
 }

sub showHashes($)                                                               #P Create a map of all the keys within all the hashes within a tower of data structures.
 {my ($d) = @_;                                                                 # Data structure
  showHashes2($d, my $keys = {}, {});
  $keys
 }

my %packageSearchOrder;                                                         # Method to package map

sub setPackageSearchOrder($@)                                                   # Set a package search order for methods requested in the current package via AUTOLOAD.
 {my ($set, @search) = @_;                                                      # Package to set, package names in search order.
  %packageSearchOrder = ();                                                     # Reset method to package map

  my $c  = <<'END';
if (1)
 {package $set;
  our $AUTOLOAD;                                                                # Method requested
  BEGIN{undef &AUTOLOAD};                                                       # Replace autoload
  sub AUTOLOAD
   {my $s = $AUTOLOAD;
    return if $s =~ m(Destroy)is;
    if (my $t = $packageSearchOrder{$s})                                        # Reuse a cached method if possible
     {goto &$t;
     }
    else                                                                        # Search for the first package that can provide the requested method
     {for my $package(@search)
       {my $t = $s =~ s(\A.+::) (${package}::)grs;
        if (defined &$t)
         {$packageSearchOrder{$s} = $t;
          goto &$t;
         }
       }
      confess "Cannot find a method implementing $s";                           # No package supports the requested method
     }
   }
 }
END
  my $search = q/qw(/.join(' ', @search).q/)/;                                  # Set search order
  $c =~ s(\$set)    ($set)gs;
  $c =~ s(\@search) ($search)gs;
  eval $c;
  confess "$c\n$@\n" if $@;
 }

sub isSubInPackage($$)                                                          # Test whether the specified B<$package> contains the subroutine <$sub>.
 {my ($package, $sub) = @_;                                                     # Package name, subroutine name
  my $r = eval qq(defined(&${package}::${sub}));                                # hasSub containsSub
  $@ and confess $@;
  $r
 }

sub overrideMethods($$@)                                                        #S For each method, if it exists in package B<$from> then export it to package B<$to> replacing any existing method in B<$to>, otherwise export the method from package B<$to> to package B<$from> in order to merge the behavior of the B<$from> and B<$to> packages with respect to the named methods with duplicates resolved if favour of package B<$from>.
 {my ($from, $to, @methods) = @_;                                               # Name of package from which to import methods, package into which to import the methods, list of methods to try importing.
  my @s;
  for my $method(setUnion @methods)                                             # Replaceable methods
   {push @s, <<"END";
if (isSubInPackage(q($from), q($method)))
 {undef &${to}::$method;
  *${to}::$method = *${from}::$method;
 }
else
 {undef &${from}::$method;
  *${from}::$method = *${to}::$method;
 }
END
   }
  my $s = join "\n", @s;                                                        # Replace methods
  eval $s;
  confess $@ if $@;
 }

sub overrideAndReabsorbMethods(@)                                               #S Override methods down the list of B<@packages> then reabsorb any unused methods back up the list of packages so that all the packages have the same methods as the last package with methods from packages mentioned earlier overriding methods from packages mentioned later.  The methods to override and reabsorb are listed by the sub B<overridableMethods> in the last package in the packages list. Confess to any errors.
 {my (@packages) = @_;                                                          # List of packages
  @packages or confess "No packages supplied";                                  # Check we have some packages
  my $base    = $packages[-1];                                                  # The last package
  my $om      = qq(&${base}::overrideableMethods);                              # Sub to supply replaceable methods
  my @methods = eval $om;                                                       # Retrieve replaceable methods
  $@ and confess "Cannot retrieve replaceable methods via sub $om\n$@\n";

  my @s;                                                                        # Replacement code

  for my $i(keys @packages)                                                     # Push methods down through the packages
   {last if $i == $#packages;
    my $from   = $packages[$i];
    my $to     = $packages[$i+1];
    for my $method(@methods)                                                    # Push each method down one level if possible
     {push @s, <<"END";
if (isSubInPackage(q($from), q($method)))
 {undef &${to}::$method;
  *${to}::$method = *${from}::$method;
 }
END
     }
   }

  for my $i(reverse keys @packages)                                             # Pull methods up through the packages
   {next unless $i;
    my $from   = $packages[$i];
    my $to     = $packages[$i-1];
    for my $method(@methods)                                                    # Pull each method up one level if possible
     {push @s, <<"END";
if (isSubInPackage(q($from), q($method)) && !isSubInPackage(q($to), q($method)))
 {undef &${to}::$method;
  *${to}::$method = *${from}::$method;
 }
END
     }
   }

  my $s = join "\n", @s;                                                        # Replace methods
  eval $s;
  confess "$@\n$s\n" if $@;
 }

sub assertPackageRefs($@)                                                       # Confirm that the specified references are to the specified package
 {my ($package, @refs) = @_;                                                    # Package, references
  for(@refs)                                                                    # Check each reference
   {my $r = ref($_);
    $r && $r eq $package or confess "Wanted reference to $package, but got $r\n";
   }
  1
 }

sub assertRef(@)                                                                # Confirm that the specified references are to the package into which this routine has been exported.
 {my (@refs) = @_;                                                              # References
  my ($package) = caller;                                                       # Package
  for(@_)                                                                       # Check each reference
   {my $r = ref($_);
    $r && $r eq $package or confess "Wanted reference to $package, but got $r\n";
   }
  1
 }

sub arrayToHash(@)                                                              # Create a hash reference from an array
 {my (@array) = @_;                                                             # Array
 +{map{$_=>1} @array}
 }

sub flattenArrayAndHashValues(@)                                                # Flatten an array of scalars, array and hash references to make an array of scalars by flattening the array references and hash values.
 {my (@array) = @_;                                                             # Array to flatten
  my @a;
  for my $a(@array)
   {if    (ref($a) =~ m(\Aarray\Z)i)
     {push @a, &flattenArrayAndHashValues(@$a);
     }
    elsif (ref($a) =~ m(\Ahash\Z)i)
     {push @a, &flattenArrayAndHashValues(map {$$a{$_}} sort keys %$a);
     }
    else
     {push @a, $a;
     }
   }
  @a                                                                            # Flattened array
 }

sub getSubName($)                                                               # Returns the (package, name, file, line) of a perl B<$sub> reference.
 {my ($sub) = @_;                                                               # Reference to a sub with a name.
  if (my $b = B::svref_2object($sub))
   {my $r = ref($b);
    if ($r =~ m(B::CV)i)
     {if (my $g = $b->GV)
       {return ($g->STASH->NAME, $g->NAME, $g->FILE, $g->LINE);                 # Package, name, file, line in file
       }
     }
   }
  confess "Unable to get name of sub referenced by $sub";
 }

#D1 Strings                                                                     # Actions on strings.

sub stringMd5Sum($)                                                             # Get the Md5 sum of a B<$string> that might contain L<utf8> code points.
 {my ($string) = @_;                                                            # String
  my $f = writeFile(undef, $string);                                            # Write into a file
  my $s = readBinaryFile($f);                                                   # Read as binary
  my $m = md5_hex($s);                                                          # Md5sum of bytes
  unlink $f;
  $m;
 }

sub indentString($$)                                                            # Indent lines contained in a string or formatted table by the specified string.
 {my ($string, $indent) = @_;                                                   # The string of lines to indent, the indenting string
  join "\n", map {$indent.$_} split m(\n+), (ref($string) ? $$string  : $string)
 }

sub replaceStringWithString($$$)                                                # Replace all instances in B<$string> of B<$source> with B<$target>
 {my ($string, $source, $target) = @_;                                          # String in which to replace substrings, the string to be replaced, the replacement string
  for(1..(1+length($string)/ (length($source)+1)))                              # Avoid too much recursive expansion
   {my $i = index($string, $source);
    if ($i >= 0)
     {substr($string, $i, length($source)) = $target;
      next;
     }
    last;
   }
  $string
 }

sub formatString($$)                                                            # Format the specified B<$string> so it can be displayed in B<$width> columns.
 {my ($string, $width) = @_;                                                    # The string of text to format, the formatted width.

  $string =~ s(\\m) (\n\n)gs;                                                   # Expand \m introduced by update documentation

  for(1..9)
   {if ($string =~ m((B<([^>]*)>))s)
     {$string = replaceStringWithString(my $s = $string, $1, boldString($2));
      last if $s eq $string;
     }
   }

  my @f;
  my @w = split m/\s+/, $string;                                                # Parse string into words
  for my $w(@w)                                                                 # Bold B<string>
   {if (!$f[-1]) {push @f, $w}
    else
     {my $l = $f[-1].qq( $w);
      if (length($l) > $width)
       {push @f, $w;
       }
      else
       {$f[-1] = $l;
       }
     }
   }

  my $t = join "\n", @f;                                                        # Format punctuation
     $t =~ s(\s*([,;.!?]))           ($1)gs;
     $t =~ s(\s*\Z)                  ()s;

  "$t\n"
 }

sub isBlank($)                                                                  # Test whether a string is blank.
 {my ($string) = @_;                                                            # String
  $string =~ m/\A\s*\Z/
 }

sub trim($)                                                                     # Remove any white space from the front and end of a string.
 {my ($string) = @_;                                                            # String
  $string =~ s/\A\s+//r =~ s/\s+\Z//r
 }

sub pad($$;$)                                                                   # Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.
 {my ($string, $length, $padding) = @_;                                         # String, tab width, padding string
  defined($string) or confess "String required\n";
  $string =~ s/\s+\Z//;
  $padding //= q( );
  my $l = length($string);
  return $string if $l % $length == 0;
  my $p = $length - $l % $length;
  $string .= $padding x $p;
 }

sub lpad($$;$)                                                                  # Left Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.
 {my ($string, $length, $padding) = @_;                                         # String, tab width, padding string
  defined($string) or confess "String required\n";
  $string =~ s/\s+\Z//;
  $padding //= q( );
  my $l = length($string);
  return $string if $l % $length == 0;
  my $p = $length - $l % $length;
  ($padding x $p).$string;
 }

sub ppp($$;$)                                                                   # Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.
 {my ($length, $string, $padding) = @_;                                         # Tab width, string, padding string
  defined($string) or confess "String required\n";
  $string =~ s/\s+\Z//;
  $padding //= q( );
  my $l = length($string);
  return $string if $l % $length == 0;
  my $p = $length - $l % $length;
  $string .= $padding x $p;
 }

sub firstNChars($$)                                                             # First N characters of a string.
 {my ($string, $length) = @_;                                                   # String, length
  return $string if !$length or length($string) < $length;
  substr($string, 0, $length);
 }

sub nws($;$)                                                                    # Normalize white space in a string to make comparisons easier. Leading and trailing white space is removed; blocks of white space in the interior are reduced to a single space.  In effect: this puts everything on one long line with never more than one space at a time. Optionally a maximum length is applied to the normalized string.
 {my ($string, $length) = @_;                                                   # String to normalize, maximum length of result
  my $s = $string =~ s((\x{200b}|\A\s+|\s+\Z)) ()gr =~ s/\s+/ /gr;
  firstNChars($s, $length)                                                      # Apply maximum length if requested
 }

sub deduplicateSequentialWordsInString($)                                       # Remove sequentially duplicate words in a string
 {my ($s) = @_;                                                                 # String to deduplicate
  my %a = map {$_=>1} grep {$_} split /\W+/, $s;                                # Split into words
  for my $w(sort keys %a)
   {1 while $s =~ s($w\s+$w) ($w)gs;
   }
  $s
 }

sub detagString($)                                                              # Remove L<html> or L<xml> tags from a string
 {my ($string) = @_;                                                            # String to detag
  $string =~ s(<[^>]*>) ()gsr                                                   # Remove xml/html tags
 }

sub parseIntoWordsAndStrings($)                                                 # Parse a B<$string> into words and quoted strings. A quote following a space introduces a string, else a quote is just part of the containing word.
 {my ($string) = @_;                                                            # String to parse
  return () unless $string;

  my $s = 0;                                                                    # 0 - look for word or quote, 1 in word, 2 in ' string, 3 - in " string
  my @r;
  my $r;

  my $accept = sub                                                              # Accept a word or string
   {push @r, $r; $s = 0;
   };

  for my $c(split m//, $string)                                                 # Each character in the string
   {next if $s == 0 and $c =~ m(\s);                                            # Skip spaces while looking for a word or string

    if ($s == 0)                                                                # String
     {if    ($c =~ m('))                                                        # ' string
       {$r = ''; $s = 2;
       }
      elsif ($c =~ m("))                                                        # " string
       {$r = ''; $s = 3;
       }
      else                                                                      # Word
       {$r = $c; $s = 1;
       }
     }
    elsif ($s == 1)                                                             # In word
     {if ($c =~ m(\s))
       {&$accept;
       }
      else
       {$r .=  $c;
       }
     }
    elsif ($s == 2)                                                             # In ' string
     {if ($c =~ m('))
       {&$accept;
       }
      else
       {$r .=  $c;
       }
     }
    elsif ($s == 3)                                                             # In " string
     {if ($c =~ m("))
       {&$accept;
       }
      else
       {$r .=  $c;
       }
     }
   }
  &$accept;
  @r
 } # parseIntoWordsAndStrings

sub stringsAreNotEqual($$)                                                      # Return the common start followed by the two non equal tails of two non equal strings or an empty list if the strings are equal.
 {my ($a, $b) = @_;                                                             # First string, second string
  my @a = split //, $a;
  my @b = split //, $b;
  my @c;
  while(@a and @b and $a[0] eq $b[0])
   {shift @a; push @c, shift @b;
   }
  (join(q(), @c), join(q(), @a), join(q(), @b))
 }

sub showGotVersusWanted($$)                                                     # Show the difference between the wanted string and the wanted string
 {my ($g, $e) = @_;                                                             # First string, second string
  my @s;
  if ($g ne $e)
   {my ($s, $G, $E) = stringsAreNotEqual($g, $e);
    if (length($s))
     {my $line = 1 + length($s =~ s([^\n])  ()gsr);
      my $char = 1 + length($s =~ s(\A.*\n) ()sr);
      push @s, "Comparing wanted with got failed at line: $line, character: $char";
      push @s, "Start:\n$s";
     }
    my $b1 = '+' x 80;
    my $b2 = '_' x 80;
    push @s,  "Want $b1\n", firstNChars($E, 80);
    push @s,  "Got  $b2\n", firstNChars($G, 80);
    return join "\n", @s;
   }
  undef
 }

sub printQw(@)                                                                  # Print an array of words in qw() format.
 {my (@words) = @_;                                                             # Array of words
  'qw('.join(' ', @words).')'
 }

sub numberOfLinesInString($)                                                    # The number of lines in a string.
 {my ($string) = @_;                                                            # String
  scalar split /\n/, $string;
 }

sub javaPackage($)                                                              # Extract the package name from a java string or file.
 {my ($java) = @_;                                                              # Java file if it exists else the string of java

  my $s = sub
   {return readFile($java) if $java !~ m/\n/s and -e $java;                     # Read file of java
    $java                                                                       # Java string
   }->();

  my ($package) = $s =~ m(package\s+(\S+)\s*;);
  $package
 }

sub javaPackageAsFileName($)                                                    # Extract the package name from a java string or file and convert it to a file name.
 {my ($java) = @_;                                                              # Java file if it exists else the string of java

  if (my $package = javaPackage($java))
   {return $package =~ s/\./\//gr;
   }
  undef
 }

sub perlPackage($)                                                              # Extract the package name from a perl string or file.
 {my ($perl) = @_;                                                              # Perl file if it exists else the string of perl
  my $p = javaPackage($perl);                                                   # Use same technique as Java
  defined($p) or confess "There is no Perl module in file: $perl";
  $p
 }

sub javaScriptExports($)                                                        # Extract the Javascript functions marked for export in a file or string.  Functions are marked for export by placing function in column 1 followed by //E on the same line.  The end of the exported function is located by \n }
 {my ($fileOrString) = @_;                                                      # File or string
  my $s = $fileOrString =~ m(\n) ? $fileOrString : readFile($fileOrString);
  my @s;
  my $state = 0;
  for my $line(split /\n/, $s)
   {if      ($state == 0)
     {if ($line =~ m(\Afunction.*\/\/E))
       {$state = 1;
        push @s, q(), $line;
       }
     }
    elsif ($state == 1)
     {$state = 0 if $line =~ m(\A \});
      push @s, $line;
     }
   }
  join "\n", @s, '';
 }

sub chooseStringAtRandom(@)                                                     # Choose a string at random from the list of B<@strings> supplied.
 {my (@strings) = @_;                                                           # Strings to chose from
  my $r = int((rand() * @strings)) % @strings;
  $strings[$r]
 }

sub randomizeArray(@)                                                           # Randomize an array
 {my (@a) = @_;                                                                 # Array to randomize
  for my $i(keys @a)
   {my $r = int(rand() * ($i+1));                                               # Uniform randomization
    my $s = $a[$i];
    my $t = $a[$r];
            $a[$i] = $t;
            $a[$r] = $s;
   }
  @a
 }

#D1 Arrays and Hashes                                                           # Operations on arrays and hashes and array of of hashesh and ghashes of arrays and  so on a infinitum.

sub lengthOfLongestSubArray($)                                                  # Given an array of arrays find the length of the longest sub array.
 {my ($a) = @_;                                                                 # Array reference
  max map{scalar @$_} @$a
 }

sub cmpArrays($$)                                                               # Compare two arrays of strings
 {my ($a, $b) = @_;                                                             # Array A, array B
  my @a = @$a;
  my @b = @$b;
  while(@a and @b and !($a[0] cmp $b[0]))
   {shift @a; shift @b;
   }
  return $a[0] cmp $b[0] if @a and @b;
  return -1 if @b;
  return +1 if @a;
  0
 }

sub forEachKeyAndValue(&%)                                                      # Iterate over a hash for each key and value
 {my ($body, %hash) = @_;                                                       # Body to be executed, hash to be iterated
  &$body($_, $hash{$_}) for sort keys %hash;
 }

#D1 Unicode                                                                     # Translate L<ascii> alphanumerics in strings to various L<unicode> blocks.

my $normalString                    = join '', 'A'..'Z', 'a'..'z', '0'..'9';
my $normalAlphaString               = join '', 'A'..'Z', 'a'..'z';
my $boldString                      = q(𝗔𝗕𝗖𝗗𝗘𝗙𝗚𝗛𝗜𝗝𝗞𝗟𝗠𝗡𝗢𝗣𝗤𝗥𝗦𝗧𝗨𝗩𝗪𝗫𝗬𝗭𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇𝟬𝟭𝟮𝟯𝟰𝟱𝟲𝟳𝟴𝟵);
my $squareString                    = q(🄰🄱🄲🄳🄴🄵🄶🄷🄸🄹🄺🄻🄼🄽🄾🄿🅀🅁🅂🅃🅄🅅🅆🅇🅈🅉🄰🄱🄲🄳🄴🄵🄶🄷🄸🄹🄺🄻🄼🄽🄾🄿🅀🅁🅂🅃🅄🅅🅆🅇🅈🅉0123456789);
my $circleString                    = q(ⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ⓪①②③④⑤⑥⑦⑧⑨);
my $darkString                      = q(🅐🅑🅒🅓🅔🅕🅖🅗🅘🅙🅚🅛🅜🅝🅞🅟🅠🅡🅢🅣🅤🅥🅦🅧🅨🅩🅐🅑🅒🅓🅔🅕🅖🅗🅘🅙🅚🅛🅜🅝🅞🅟🅠🅡🅢🅣🅤🅥🅦🅧🅨🅩⓿➊➋➌➍➎➏➐➑➒);
my $superString                     = q(ᴬᴮCᴰᴱFᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾQᴿSᵀᵁⱽᵂXYZᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖqʳˢᵗᵘᵛʷˣʸᶻ⁰¹²³⁴⁵⁶⁷⁸⁹);
my $lowsubString                    = q(ₐbcdₑfgₕᵢⱼₖₗₘₙₒₚqᵣₛₜᵤᵥwₓyz₀₁₂₃₄₅₆₇₈₉);
my $lowerString                     = join '', 'a'..'z', '0'..'9';
my $mathematicalItalic              = '𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍𝑎𝑏𝑐𝑑𝑒𝑓𝑔𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧';
my $mathematicalBold                = '𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳';
my $mathematicalBoldItalic          = '𝑨𝑩𝑪𝑫𝑬𝑭𝑮𝑯𝑰𝑱𝑲𝑳𝑴𝑵𝑶𝑷𝑸𝑹𝑺𝑻𝑼𝑽𝑾𝑿𝒀𝒁𝒂𝒃𝒄𝒅𝒆𝒇𝒈𝒉𝒊𝒋𝒌𝒍𝒎𝒏𝒐𝒑𝒒𝒓𝒔𝒕𝒖𝒗𝒘𝒙𝒚𝒛';
my $mathematicalSansSerif           = '𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓';
my $mathematicalSansSerifBold       = '𝗔𝗕𝗖𝗗𝗘𝗙𝗚𝗛𝗜𝗝𝗞𝗟𝗠𝗡𝗢𝗣𝗤𝗥𝗦𝗧𝗨𝗩𝗪𝗫𝗬𝗭𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇';
my $mathematicalSansSerifItalic     = '𝘈𝘉𝘊𝘋𝘌𝘍𝘎𝘏𝘐𝘑𝘒𝘓𝘔𝘕𝘖𝘗𝘘𝘙𝘚𝘛𝘜𝘝𝘞𝘟𝘠𝘡𝘢𝘣𝘤𝘥𝘦𝘧𝘨𝘩𝘪𝘫𝘬𝘭𝘮𝘯𝘰𝘱𝘲𝘳𝘴𝘵𝘶𝘷𝘸𝘹𝘺𝘻';
my $mathematicalSansSerifBoldItalic = '𝘼𝘽𝘾𝘿𝙀𝙁𝙂𝙃𝙄𝙅𝙆𝙇𝙈𝙉𝙊𝙋𝙌𝙍𝙎𝙏𝙐𝙑𝙒𝙓𝙔𝙕𝙖𝙗𝙘𝙙𝙚𝙛𝙜𝙝𝙞𝙟𝙠𝙡𝙢𝙣𝙤𝙥𝙦𝙧𝙨𝙩𝙪𝙫𝙬𝙭𝙮𝙯';
my $mathematicalMonoSpace           = '𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚇𝚈𝚉𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠𝚡𝚢𝚣';

sub mathematicalItalicString($)                                                 # Convert alphanumerics in a string to L<unicode> Mathematical Italic.
 {my ($string) = @_;                                                            # String to convert
  my $h = $normalAlphaString =~ s(h) ()r;                                       # Unicode does not have a small mathematical italic h
  eval qq(\$string =~ tr($h) ($mathematicalItalic));
  $string
 }

sub mathematicalBoldString($)                                                   # Convert alphanumerics in a string to L<unicode> Mathematical Bold.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalAlphaString) ($mathematicalBold));
  $string
 }

sub mathematicalBoldStringUndo($)                                               # Undo alphanumerics in a string to L<unicode> Mathematical Bold..
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($mathematicalBold) ($normalAlphaString));
  $string
 }

sub mathematicalBoldItalicString($)                                             # Convert alphanumerics in a string to L<unicode> Mathematical Bold Italic.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalAlphaString) ($mathematicalBoldItalic));
  $string
 }

sub mathematicalBoldItalicStringUndo($)                                         # Undo alphanumerics in a string to L<unicode> Mathematical Bold Italic.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($mathematicalBoldItalic) ($normalAlphaString));
  $string
 }

sub mathematicalSansSerifString($)                                              # Convert alphanumerics in a string to L<unicode> Mathematical Sans Serif.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalAlphaString) ($mathematicalSansSerif));
  $string
 }

sub mathematicalSansSerifStringUndo($)                                          # Undo alphanumerics in a string to L<unicode> Mathematical Sans Serif.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($mathematicalSansSerif) ($normalAlphaString));
  $string
 }

sub mathematicalSansSerifBoldString($)                                          # Convert alphanumerics in a string to L<unicode> Mathematical Sans Serif Bold.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalAlphaString) ($mathematicalSansSerifBold));
  $string
 }

sub mathematicalSansSerifBoldStringUndo($)                                      # Undo alphanumerics in a string to L<unicode> Mathematical Sans Serif Bold.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($mathematicalSansSerifBold) ($normalAlphaString));
  $string
 }

sub mathematicalSansSerifItalicString($)                                        # Convert alphanumerics in a string to L<unicode> Mathematical Sans Serif Italic.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalAlphaString) ($mathematicalSansSerifItalic));
  $string
 }

sub mathematicalSansSerifItalicStringUndo($)                                    # Undo alphanumerics in a string to L<unicode> Mathematical Sans Serif Italic.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($mathematicalSansSerifItalic) ($normalAlphaString));
  $string
 }

sub mathematicalSansSerifBoldItalicString($)                                    # Convert alphanumerics in a string to L<unicode> Mathematical Sans Serif Bold Italic.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalAlphaString) ($mathematicalSansSerifBoldItalic));
  $string
 }

sub mathematicalSansSerifBoldItalicStringUndo($)                                # Undo alphanumerics in a string to L<unicode> Mathematical Sans Serif Bold Italic.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($mathematicalSansSerifBoldItalic) ($normalAlphaString));
  $string
 }

sub mathematicalMonoSpaceString($)                                              # Convert alphanumerics in a string to L<unicode> Mathematical MonoSpace.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalAlphaString) ($mathematicalMonoSpace));
  $string
 }

sub mathematicalMonoSpaceStringUndo($)                                          # Undo alphanumerics in a string to L<unicode> Mathematical MonoSpace.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($mathematicalMonoSpace) ($normalAlphaString));
  $string
 }

sub boldString($)                                                               # Convert alphanumerics in a string to bold.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalString) ($boldString));                         # Some Perls cannot do this and complain but I want to avoid excluding all the other methods in this file just because some perls cannot do this one operation.
  $string
 }

sub boldStringUndo($)                                                           # Undo alphanumerics in a string to bold.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($boldString) ($normalString));
  $string
 }

sub enclosedString($)                                                           # Convert alphanumerics in a string to enclosed alphanumerics.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalString) ($circleString));
  $string
 }

sub enclosedStringUndo($)                                                       # Undo alphanumerics in a string to enclosed alphanumerics.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($circleString) ($normalString));
  $string
 }

sub enclosedReversedString($)                                                   # Convert alphanumerics in a string to enclosed reversed alphanumerics.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalString) ($darkString));
  $string
 }

sub enclosedReversedStringUndo($)                                               # Undo alphanumerics in a string to enclosed reversed alphanumerics.
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($darkString)   ($normalString));
  $string
 }

sub superScriptString($)                                                        # Convert alphanumerics in a string to super scripts
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($normalString) ($superString));
  $string
 }

sub superScriptStringUndo($)                                                    # Undo alphanumerics in a string to super scripts
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($superString)  ($normalString));
  $string
 }

sub subScriptString($)                                                          # Convert alphanumerics in a string to sub scripts
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($lowerString)  ($lowsubString));
  $string
 }

sub subScriptStringUndo($)                                                      # Undo alphanumerics in a string to sub scripts
 {my ($string) = @_;                                                            # String to convert
  eval qq(\$string =~ tr($lowsubString) ($lowerString));
  $string
 }

sub isFileUtf8($)                                                               # Return the file name quoted if its contents are in utf8 else return undef
 {my ($file) = @_;                                                              # File to test
  my $f = quoteFile($file);

  return undef unless confirmHasCommandLineCommand(q(isutf8));                  # Confirm we have isutf8

  qx(isutf8 -q $f);                                                             # Test
  return $f unless $?;                                                          # File is utf8
  undef                                                                         # File is not utf8
 }

#D1 Unix domain communications                                                  # Send messages between processes via a unix domain socket.

sub newUdsr(@)                                                                  #P Create a communicator - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.
 {my (@parms) = @_;                                                             # Attributes per L<Udsr Definition|/Udsr Definition>
   my $u = genHash(q(Udsr),                                                     # Package name
    client       => undef,                                                      # Client socket and connection socket
    headerLength => 8,                                                          #I Length of fixed header which carries the length of the following message

    serverAction => undef,                                                      #I Server action sub, which receives a communicator every time a client creates a new connection. If this server is going to be started by systemd  as a service with the specified L<serverName> then this is the a actual text of the code that will be installed as a CGI script and run in response to an incoming transaction in a separate process with the userid set to L<serviceUser>. It receives the text of the http request from the browser as parameter 1 and should return the text to be sent back to the browser.

    serverPid    => undef,                                                      # Server pid which can be used to kill the server via kill q(kill), $pid
    socketPath   => q(unix-domain-socket-test.sock),                            #I Socket file

    serviceName  => q(zzz),                                                     #I Service name for install by systemd
    serviceUser  => q(),                                                        #I Userid for service
    @_
   );
 }

sub newUdsrServer(@)                                                            # Create a communications server - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.
 {my (@parms) = @_;                                                             # Attributes per L<Udsr Definition|/Udsr Definition>
  my $u = newUdsr(@_);
  my $f = $u->socketPath;
  unlink $f;
  my $s = IO::Socket::UNIX->new(Type=>SOCK_STREAM(), Local=>$f, Listen=>1);     # Create socket
  xxx(qq(chmod ugo=rwx $f));                                                    # Ensure that www-data can read and write to the socket
# lll "Created unix domain socket as user:", qx(/usr/bin/whoami);
  if (my $pid = fork)                                                           # Run the server in a process by itself
   {$u->serverPid = $pid;                                                       # Record server pid so it can be killed
    return $u;
   }
  else                                                                          # Run the server action on a client connection
   {while (my $con = $s->accept())
     {$u->client = $con;
      call sub{$u->serverAction->($u)};                                         # The server action sub should use the read and write routines in the passed communicator to interact with the client .
      $con->close;
     }
    exit;
   }
 }

sub newUdsrClient(@)                                                            # Create a new communications client - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.
 {my (@parms) = @_;                                                             # Attributes per L<Udsr Definition|/Udsr Definition>
  my $u = newUdsr(@_);
  my $s = $u->client = IO::Socket::UNIX->new(Type=>SOCK_STREAM(), Peer => $u->socketPath);
  my $r1 = $!; my $r2 = $?;
  $s or confess join "\n", "Cannot create unix domain socket:",
    dump($u), dump({q($!)=>$r1, q($?)=>$r2, q(userId)=>qx(/usr/bin/whoami)});
  $u
 }

sub Udsr::write($$)                                                             # Write a communications message to the L<newUdsrServer|/newUdsrServer> or the L<newUdsrClient|/newUdsrClient>.
 {my ($u, $msg) = @_;                                                           # Communicator, message
  my $con = $u->client;
# $msg //= '';                                                                  # undef seems to get reported as wide char
  my $m = pad(length($msg), $u->headerLength).$msg;
  $con or confess "No unix domain socket:\n". dump($u);                         # Complain if the socket has not been created
  $con->print($m);
  $u
 }

sub Udsr::read($)                                                               # Read a message from the L<newUdsrServer|/newUdsrServer> or the L<newUdsrClient|/newUdsrClient>.
 {my ($u) = @_;                                                                 # Communicator
  my $con = $u->client;
  $con->read(my $length, $u->headerLength);
  $con->read(my $data,   $length);
  $data
 }

sub Udsr::kill($)                                                               # Kill a communications server.
 {my ($u) = @_;                                                                 # Communicator
  my $p = $u->serverPid;                                                        # Server Pid
  kill 'KILL', $p if $p;                                                        # Kill server
  $u->serverPid = undef;                                                        # Server Pid
  unlink $u->socketPath;                                                        # Remove socket
  $u
 }

sub Udsr::webUser($$)                                                           # Create a systemd installed server that processes http requests using a specified userid. The systemd and CGI files plus an installation script are written to the specified folder after it has been cleared. The L<serverAction> attribute contains the code to be executed by the server: it should contain a L<sub> B<genResponse($hash)> which will be called with a hash of the CGI variables. This L<sub> should return the response to be sent back to the client. Returns the installation script file name.
 {my ($u, $folder) = @_;                                                        # Communicator, folder to contain server code

  clearFolder($folder, 9);                                                      # Clear the output folder

  my $parms = join ', ',                                                        # Parameters to hand to server and client
    map {my $v = $$u{$_}; defined($v) ? qq($_ => q($v)) : ()}
    grep {!m/serverAction/} keys %$u;

  my $user = $u->serviceUser;                                                   # Communicator details
  my $code = $u->serverAction;                                                  # Server code minus
     $code =~ s(if \(!caller\).*\Z) ()s;                                        # Remove initiator at end
     $code =~ s(##.*?\n) ()gs;                                                  # Remove some spare blank lines so line numbers match

  my $perlParameters = sub                                                      # Get perl parameters
   {if ($code =~ m(\A#!.*?perl\s*(.*?)\n)is)
     {my $p = $1;
      return $p;
     }
    q()
   }->();

  my $name = $u->serviceName;

  my $ssdt = fpe(qw(/etc systemd system), $name, q(service));                   # Systemd folder

  my $cgif = fpd(qw(/usr lib cgi-bin),    $name);                               # Cgi folder
  my $cgst = fpe($cgif, q(server), q(pl));                                      # Cgi server
  my $cgct = fpe($cgif, q(client), q(pl));                                      # Cgi client

  my $inst = fpe($folder, qw(install sh));                                      # Install script
  my $ssdl = fpe($folder, qw(service txt));
  my $cgsl = fpe($folder, q(server), q(pl));
  my $cgcl = fpe($folder, q(client), q(pl));

  owf($ssdl, <<END);                                                            # Systemd definition
[Unit]
Description=Http to unix domain socket server

[Service]
Type=forking
ExecStart=/usr/lib/cgi-bin/$name/server.pl
User=$user

[Install]
WantedBy=multi-user.target
END
# setPermissionsForFile($ssdl, q(ugo=rx));
  setPermissionsForFile($ssdl, q(ugo=r));                                       # Permissions will be copied to server if the file does not exist on the server

  my $server = join '', <<END,                                                  # Server definition
#!/usr/bin/perl $perlParameters
END
<<'END';
#-------------------------------------------------------------------------------
# Http to unix domain socket server
#-------------------------------------------------------------------------------
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use utf8;
use feature qw(say current_sub);

makeDieConfess;

# Server code which should contain a sub genResponse($hash) which returns the response to be sent to the client
<code>

my $parms = newUdsr(<parms>);

$parms->serverAction = sub                                                      # Perform server action
 {my ($c) = @_;                                                                 # Communicator
  my $parms = $c->read;                                                         # Parameter string from client
  my $data = $parms ? eval $parms : undef;                                      # Decode parameter string
  $@ and confess "Unable to decode webUser request:\n$parms\n";                 # Complain about parameter string
  my $resp = genResponse($data);                                                # Execute server action and capture returned value
  $c->write($resp);                                                             # Write back to the client
 };

unlink $parms->socketPath;
newUdsrServer(%$parms);
END
  $server =~ s(<parms>) ($parms)s;
  $server =~ s(<code>)  ($code)s;
  owf($cgsl, $server);
  setPermissionsForFile($cgsl, q(ugo=rx));

  my $client = <<'END';
#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
#-------------------------------------------------------------------------------
# Http to unix domain socket client
#-------------------------------------------------------------------------------
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use CGI;
use utf8;
use feature qw(say current_sub);

makeDieConfess;

my $cgi = CGI->new;

my %v = $cgi->Vars;
if (my $j = $cgi->param(q(POSTDATA)))                                           # Load POST data
 {$v{POSTDATA} = $j;
  if (my $p = decodeJson($j))
   {if (ref($p) =~ m(hash)i)
     {%v = (%v, %$p);
     }
   }
 }
#for my $k(keys %v)
# {$v{$k} = wwwDecode($v{$k}) // q();
# }

my $parms = newUdsr(<parms>);
my $c     = newUdsrClient(%$parms);
say $c->read($c->write(dump({%v})));
END
  $client =~ s(<parms>) ($parms)s;
  owf($cgcl, $client);
  setPermissionsForFile($cgcl, q(ugo=rx));

  owf($inst, <<END);
sudo rm $ssdt $cgst $cgct
sudo mkdir -p $cgif
sudo cp $ssdl $ssdt
sudo cp $cgsl $cgst
sudo cp $cgcl $cgct
sudo systemctl daemon-reload; sudo systemctl enable $name; sudo systemctl restart $name; sudo systemctl status $name
END

  setPermissionsForFile $inst, q(u+x);

#  if (!$noInstall)                                                             # Install on server if available
#   {copyFolderToRemote($folder);                                               # Copy code created locally to remote server
#    xxxr(qq(bash -x $inst));                                                   # Install system by executing install procedure remotely
#   }

  lll <<END;
See status with:

  sudo systemctl status $name

Install with:

  $inst

Remove with:

  sudo rm $ssdt $cgst $cgct

Access via:

  http://localhost/cgi-bin/$name/client.pl

END

  $inst                                                                         # Install script
 }

#D2 www                                                                         # Web processing

sub wwwHeader {say STDOUT qq(Content-Type: text/html;charset=UTF-8\n\n)}        # Html header

sub wwwGitHubAuth(&$$$$)                                                        # Logon as a L<GitHub> L<OAuth> app per: L<https://github.com/settings/developers>. If no L<OAuth> code is supplied then a web page is printed that allows the user to request that such a code be sent to the server.  If a valid code is received, by the server then it is converted to a L<OAuth> token which is handed to L<sub> L<saveUserDetails>.
 {my ($saveUserDetails, $clientId, $clientSecret, $code, $state) = @_;          # Process user token once obtained from GitHub, Client id, client secret, authorization code, random string

  if (!$code)                                                                   # Show logon page if no code has been supplied
   {my $r = rand =~ s(\A0.) ()r;
    say STDOUT <<HTML;                                                          # Logon page
<html>
<meta charset="utf-8"/>
<body>
<p>Logon with <a href="https://github.com/login/oauth/authorize?client_id=$clientId&state=$r&scope=repo">GitHub</a></p>
<script>
window.localStorage.setItem('randomUserid', "$r");
</script>
</body>
</html>
HTML
   }
  else                                                                          # Get userid
   {my $s = qq(wget -q -O- "https://github.com/login/oauth/access_token)        # Get the token - Wget works, Curl does not
     .qq(?code=$code&state=$state)
     .qq(&client_id=$clientId&client_secret=$clientSecret");

    if (my $r = qx($s))                                                         # Get user details
     {if ($r =~ m(\Aaccess_token=(.*?)&scope=(.*?)&token_type=(.*?)\Z))
       {my ($token, $scope, $type) = ($1, $2, $3);
        my $c = qq(wget -q -O- --header="Authorization: token $token")
               .qq( https://api.github.com/user);
        my $j = qx($c 2>&1);
        my $user = decodeJson($j);
        $saveUserDetails->($user, $state, $token, $scope, $type);
       }
     }
   }
 } # wwwGitHubAuth

#D1 Cloud Cover                                                                 # Useful for operating across the cloud.

sub makeDieConfess                                                              # Force die to confess where the death occurred
 {$SIG{__DIE__} = sub
   {local $SIG{__DIE__} = undef;
    confess shift;
   };
 }

sub ipAddressOfHost($)                                                          # Get the first ip address of the specified host via Domain Name Services
 {my ($host) = @_;                                                              # Host name
  my $i = inet_aton $host;
  confess "Unable to get ip address of hist: $host\n" unless $i;
  return inet_ntoa $i;
 }

sub awsIpFile {q(/tmp/awsPrimaryInstanceIpAddress.data)}                        #P File in which to save IP address of primary instance on Aws
sub awsEc2DescribeInstancesCache {q(/tmp/awsEc2DescribeInstancesCache.data)}    #P File in which to cache latest results from describe instances to avoid being throttled

sub awsIp                                                                       # Get ip address of server at L<AWS>.
 {for(1..2)
   {if (-e awsIpFile)
     {if (my $d = eval {retrieveFile(awsIpFile)})
       {if ($d->{time} + 180 > time)
         {return $d->{ip};
         }
       }
     }
    &awsParallelPrimaryInstanceId();
   }
  confess "Unable to get primary instance IP address\n";
 }

sub saveAwsIp                                                                   # Make the server at L<aws> with the given IP address the default primary server as used by all the methods whose names end in B<r> or B<Remote>. Returns the given IP address.
 {my ($ip) = @_;                                                                # Ip address of chosen server on L<aws>
  storeFile(awsIpFile, {ip=>$ip, time=>time});
  $ip
 }

sub saveAwsDomain                                                               # Make the server at L<aws> with the given domain name the default primary server as used by all the methods whose names end in B<r> or B<Remote>. Returns the given IP address.
 {my ($host) = @_;                                                              # Host domain name
  saveAwsIp ipAddressOfHost $host;
 }

sub awsMetaData($)                                                              # Get an item of meta data for the L<AWS> server we are currently running on if we are running on an L<AWS> server else return a blank string.
 {my ($item) = @_;                                                              # Meta data field
  return q() unless &onAws;                                                     # We are not on Aws
  return undef unless confirmHasCommandLineCommand(q(curl));                    # Confirm we have curl
  my $c = qq(curl -m 0 -s http://169.254.169.254/latest/meta-data/$item/);      # Command
  qx($c)
 }

my       $awsCurrentIp;                                                         # Server IP address if running on L<AWS>
sub       awsCurrentIp                                                          # Get the ip address of the AWS server we are currently running on if we are running on an L<AWS> server else return a blank string.
 {return $awsCurrentIp if defined $awsCurrentIp;
         $awsCurrentIp = awsMetaData q(public-ipv4);
 }

my       $awsCurrentInstanceId;                                                 # Server instance id
sub       awsCurrentInstanceId                                                  # Get the instance id of the L<AWS> server we are currently running on if we are running on an L<AWS> server else return a blank string.
 {return $awsCurrentInstanceId if defined $awsCurrentInstanceId;
         $awsCurrentInstanceId = awsMetaData q(instance-id)
 }

my       $awsCurrentAvailabilityZone;                                           # Availability zone
sub       awsCurrentAvailabilityZone                                            # Get the availability zone of the L<AWS> server we are currently running on if we are running on an L<AWS> server else return a blank string.
 {return $awsCurrentAvailabilityZone if defined $awsCurrentAvailabilityZone;
         $awsCurrentAvailabilityZone = awsMetaData(q(placement/availability-zone))
 }

my       $awsCurrentRegion;                                                     # Server region
sub       awsCurrentRegion                                                      # Get the region of the L<AWS> server we are currently running on if we are running on an L<AWS> server else return a blank string.
 {if (my $a = awsCurrentAvailabilityZone)
   {return $a =~ s(.\Z) ()sr
   }
  q()
 }

my       $awsCurrentInstanceType;                                               # Instance type
sub       awsCurrentInstanceType                                                # Get the instance type of the L<AWS> server if we are running on an L<AWS> server else return a blank string.
 {return $awsCurrentInstanceType if defined $awsCurrentInstanceType ;
         $awsCurrentInstanceType = awsMetaData(q(instance-type))
 }

sub awsInstanceId(%)                                                            #P Create an instance-id from the specified B<%options>
 {my (%options) = @_;                                                           # Options
  return q() unless my $i = $options{instanceId} // awsCurrentInstanceId;       # Instance id if supplied or we are on AWS
  qq( --instance-id $i );                                                       # instance-id keyword
 }

sub awsProfile(%)                                                               #P Create a profile keyword from the specified B<%options>
 {my (%options) = @_;                                                           # Options
  return q() unless my $p = $options{profile};                                  # Profile value
  qq( --profile $p );                                                           # Profile keyword
 }

sub awsRegion(%)                                                                #P Create a region keyword from the specified B<%options>
 {my (%options) = @_;                                                           # Options
  return q() unless my $r = $options{region} // awsCurrentRegion;               # Region value if supplied or we are on AWS
  qq( --region $r );                                                            # Region keyword
 }

sub awsExecCli($%)                                                              # Execute an AWs command and return its response
 {my ($command, %options) = @_;                                                 # Command to execute, aws cli options
  $command =~ s(\n) ( )gs;                                                      # Make command into one line
  my $p = awsProfile(%options);                                                 # Profile to use
  my $r = awsRegion (%options);                                                 # Region to use
  my $c = qq($command $r $p);                                                   # Command
  say STDERR $c;
  qx($c 2>&1);                                                                  # Execute
 }

sub awsExecCliJson($%)                                                          # Execute an AWs command and decode the json so produced
 {my ($command, %options) = @_;                                                 # Command to execute, aws cli options
  $command =~ s(\n) ( )gs;                                                      # Make command into one line
  my $p = awsProfile(%options);                                                 # Profile to use
  my $r = awsRegion (%options);                                                 # Region to use
  my $c = qq($command $r $p);                                                   # Command
  say STDERR $c;
  my $j = qx($c);                                                               # Retrieve json
  reloadHashes decodeJson($j);                                                  # Decode json to Perl
 }

sub awsEc2DescribeInstances(%)                                                  # Describe the L<AWS> instances running in a B<$region>.
 {my (%options) = @_;                                                           # Options

  my $region = $options{region} // q();
  if (-e awsEc2DescribeInstancesCache)                                          # Use cached value if possible
   {if (  my $D = eval {retrieveFile(awsEc2DescribeInstancesCache)})
     {if (my $d = $D->{$region})
       {return $d->{results} if $d->{time} + 20 > time;
       }
     }
   }

  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $p = awsProfile(%options);                                                 # Profile to use
  my $r = awsRegion(%options);                                                  # Region to use
  my $c = qq(aws ec2 describe-instances $r $p);                                 # Command
  my $j = qx($c);                                                               # Retrieve json
  my $d = decodeJson($j);                                                       # Decode json to Perl

  storeFile(awsEc2DescribeInstancesCache, {$region=>{time=>time, results=>$d}});# Cache results by region
  $d
 }

sub awsEc2DescribeInstancesGetIPAddresses(%)                                    # Return a hash of {instanceId => public ip address} for all running instances on L<AWS> with ip addresses.
 {my (%options) = @_;                                                           # Options

  my $d = awsEc2DescribeInstances(%options);                                    # Refresh with latest data
  my %i;
  for   my $r($d->{Reservations}->@*)
   {for my $i($r->{Instances}->@*)
     {if ($$i{State}{Name} =~ m(running)i)
       {my $id = $$i{InstanceId};
        $i{$id} = $i->{PublicIpAddress};
       }
     }
   }

  \%i                                                                           # Return {instanceId => public ip address}
 }

sub awsEc2InstanceIpAddress($%)                                                 # Return the IP address of a named instance on L<AWS> else return B<undef>.
 {my ($instanceId, %options) = @_;                                              # Instance id, options
  my $p = awsProfile(%options);                                                 # Profile to use
  my $r = awsRegion(%options);                                                  # Region to use
  my $c = qq(aws ec2 describe-instances --instance-ids $instanceId $r $p);      # Command
  my $j = qx($c);                                                               # Retrieve json
  my $d = decodeJson($j);                                                       # Decode json
  for   my $R($d->{Reservations}->@*)
   {for my $i($R->{Instances}->@*)
     {if (my $id = $$i{InstanceId})
       {if ($id eq $instanceId)
         {for my $I($i->{NetworkInterfaces}->@*)
           {if (my $ip = $$I{Association}{PublicIp})
             {return $ip                                                        # Return first ip address
             }
           }
         }
       }
     }
   }
  undef                                                                         # No ip address found
 }

sub awsEc2CreateImage($%)                                                       # Create an image snap shot with the specified B<$name> of the AWS server we are currently running on if we are running on an AWS server else return false. It is safe to shut down the instance immediately after initiating the snap shot - the snap continues even though the instance has terminated.
 {my ($name, %options) = @_;                                                    # Image name, options
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $i = awsInstanceId(%options);                                              # Instance id
  my $p = awsProfile(%options);                                                 # Profile
  my $r = awsRegion(%options);                                                  # Region
  my $c = qq(aws ec2 create-image --name "$name" $i $p $r);
  xxx($c);
 } # awsEc2CreateImage

sub awsEc2FindImagesWithTagValue($%)                                            # Find images with a tag that matches the specified regular expression B<$value>.
 {my ($value, %options) = @_;                                                   # Regular expression, Options
  my @images = awsEc2DescribeImages(%options);
  my @i;
  for my $i(@images)                                                            # Each image
   {if (my $tags = $i->{Tags})
     {for my $t(@$tags)                                                         # Each tag
       {next unless $t->{Value} =~ m($value);
        push @i, $i->{ImageId};
        last;
       }
     }
   }

  @i                                                                            # Matching images
 } # awsEc2FindImagesWithTagValue

sub awsEc2DescribeImages(%)                                                     # Describe images available.
 {my (%options) = @_;                                                           # Options
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $p = awsProfile(%options);                                                 # Profile
  my $r = awsRegion (%options);                                                 # Region
  my $c = qq(aws ec2 describe-images --owners self $p $r);
  my $j = qx($c);
  map {reloadHashes $_}
    sort {$$b{CreationDate} cmp $$a{CreationDate}}
      @{decodeJson($j)->{Images}}                                               # Decode json, sort into descending date order and return
 }

my $awsCurrentLinuxSpotPrices;                                                  # Prices do not change very rapidly on the whole
sub awsCurrentLinuxSpotPrices(%)                                                # Return {instance type} = cheapest spot price in dollars per hour for the given region
 {my (%options) = @_;                                                           # Options
  return $awsCurrentLinuxSpotPrices if $awsCurrentLinuxSpotPrices;              # Return cached set

  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $p = awsProfile(%options);                                                 # Profile
  my $r = awsRegion (%options);                                                 # Region

  my $t = int time();
  my $c = qq(aws ec2 describe-spot-price-history --start-time=$t $p $r ).
   qq(--product-descriptions="Linux/UNIX" --query 'SpotPriceHistory[*]');

  my $j = qx($c);
  my $d = decodeJson($j);

  my %h;                                                                        # {instance type} = cheapest spot
  for my $s(@$d)
   {my $i = $s->{InstanceType};
    my $p = $s->{SpotPrice};
    $h{$i} = min($h{$i}//$p, $p);
   }

  $awsCurrentLinuxSpotPrices = \%h                                              # Cache results
 }

my %awsEc2DescribeInstanceType;                                                 # Cache instance type details
sub awsEc2DescribeInstanceType($%)                                              # Return details of the specified instance type.
 {my ($instanceType, %options) = @_;                                            # Instance type name, options
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $i = $instanceType;                                                        # Instance type name
  my $p = awsProfile(%options);                                                 # Profile
  my $r = awsRegion(%options);                                                  # Region
  my $cached = $awsEc2DescribeInstanceType{$r}{$i};                             # Cached instance type
  return $cached if $cached;
  my $c = qq(aws ec2 describe-instance-types $p $r --instance-types "$i");
  my $j = qx($c);
  my $d = decodeJson($j);
  $awsEc2DescribeInstanceType{$r}{$i} = $d->{InstanceTypes}[0];                 # Cache instance type
 }

sub awsEc2ReportSpotInstancePrices($%)                                          # Report the prices of all the spot instances whose type matches a regular expression B<$instanceTypeRe>. The report is sorted by price in millidollars per cpu ascending.
 {my ($instanceTypeRe, %options) = @_;                                          # Regular expression for instance type name, options
  my $spots = awsCurrentLinuxSpotPrices(%options);                              # Spot prices
  my @r;
  my $cit; my $pc;                                                              # Cheapest instance type, cheapest instance cost per cpu
  my sub formatPrice($)
   {my ($p) = @_;
    sprintf("%.2f", $p)
   };

  for my $s(sort keys %$spots)
   {next unless $s =~ m($instanceTypeRe)i;
    my $t = awsEc2DescribeInstanceType($s, %options);                           # Instance type details for spot instance
    next unless grep {m(spot)i} $t->{SupportedUsageClasses}->@*;                # Instance type allows spot instances
    my $price = $$spots{$s} * 1e3;
    my $cpus  = $$t{VCpuInfo}{DefaultVCpus};
    my $pricePerCpu = $price / $cpus;
    my $pf          = sprintf("%.2f", $pricePerCpu);
    push @r, [$s, int($price), $cpus, formatPrice($pf)];
    if (!defined($cit) or $pricePerCpu < $pc)                                   # Cheapest so far per CPU
     {$cit = $s;
      $pc  = $pricePerCpu;
     }
   }

  my $p = formatPrice($pc);
  my $r = formatTable([sort {$$a[-1] <=> $$b[-1]} @r], <<END,                   # Report
Instance_Type  Instance type name
Price          Price in millidollars per hour
CPUs           Number of Cpus
Price_per_CPU  The price per CPU in millidollars per hour
END
      title => q(CPUs by price),
      head  => <<END,
NNNN instances types found on DDDD

Cheapest Instance Type: $cit
Price Per Cpu hour    : $p      in millidollars per hour
END
  );

  genHash(q(Data::Table::Text::AwsEc2Price),                                    # Prices of selected aws elastic compute instance types
    cheapestInstance => $cit,                                                   # The instance type that has the lowest CPU cost
    pricePerCpu      => $pc,                                                    # The cost of the cheapest CPU In millidollars per hour
    report           => $r,                                                     # Report showing the cost of other selected instances
   );
 }

sub awsEc2RequestSpotInstances($$$$$$%)                                         # Request spot instances as long as they can be started within the next minute. Return a list of spot instance request ids one for each instance requested.
 {my ($count, $instanceType, $ami, $price, $securityGroup, $key, %options) = @_;# Number of instances, instance type, AMI, price in dollars per hour, security group, key name, options.
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $p = awsProfile(%options);                                                 # Profile
  my $r = awsRegion(%options);                                                  # Region
  my $t = qq( --valid-until ).(int time + 60);                                  # Limit the duration to one minute - i.e. launch now or not at all.

  my $j = <<END;
 {"DryRun"              : false,
  "InstanceCount"       : $count,
  "LaunchSpecification" :
    {"SecurityGroupIds" : ["$securityGroup"],
     "ImageId"          : "$ami",
     "InstanceType"     : "$instanceType",
     "KeyName"          : "$key"
    },
  "SpotPrice"           : "$price",
  "Type"                : "one-time"
 }
END
  my $f = writeFile(undef, $j);
  my $c = qq(aws ec2 request-spot-instances --cli-input-json file://$f $p $r $t);
  my $k = qx($c);
  my $d = decodeJson($k);
  map {$_->{SpotInstanceRequestId}=>1} $d->{SpotInstanceRequests}->@*           # List of spot instances request ids - one for each instance requested. I.e. if $count == 2 then two spot instance request ids will be returned.
 }

sub awsEc2DescribeSpotInstances(%)                                              # Return a hash {spot instance request => spot instance details} describing the status of active spot instances.
 {my (%options) = @_;                                                           # Options.
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $p = awsProfile(%options);                                                 # Profile
  my $r = awsRegion(%options);                                                  # Region
  my $c = qq(aws ec2 describe-spot-instance-requests $p $r);
  my $j = qx($c);
  my $d = decodeJson($j);
  my @r = $d->{SpotInstanceRequests}->@*;
  my %r = map {$_->{SpotInstanceRequestId}=>$_} @r;                             # Hash of spot instance requests
  \%r
 }

sub awsR53a($$$%)                                                               # Create/Update a B<A> L<DNS> record for the specified server.
 {my ($zone, $server, $ip, %options) = @_;                                      # Zone id from R53, fully qualified domain name, ip address, AWS CLI global options
  my $t = writeTempFile(<<END);                                                 # Create description of update as JSON
{ "Changes": [
    {"Action": "UPSERT",
    "ResourceRecordSet":
       {"Name": "$server", "Type": "A", "TTL": 300,
        "ResourceRecords": [{"Value": "$ip"}]
       }
    }
  ]
}
END
  my $p = awsProfile(%options);                                                 # Profile
  my $s = xxx qq(aws route53 change-resource-record-sets --hosted-zone-id )     # Execute command
             .qq($zone --change-batch file://$t $p),
              qr(ChangeInfo);
  unlink $t;
  $s
 }

sub awsR53aaaa($$$%)                                                            # Create/Update a B<AAAA> L<DNS> record for the specified server.
 {my ($zone, $server, $ip, %options) = @_;                                      # Zone id from R53, fully qualified domain name, ip6 address, AWS CLI global options
  my $t = writeTempFile(<<END);                                                 # Create description of update as JSON
{ "Changes": [
    {"Action": "UPSERT",
    "ResourceRecordSet":
       {"Name": "$server", "Type": "AAAA", "TTL": 300,
        "ResourceRecords": [{"Value": "$ip"}]
       }
    }
  ]
}
END
  my $p = awsProfile(%options);                                                 # Profile
  my $s = xxx qq(aws route53 change-resource-record-sets --hosted-zone-id )     # Execute command
             .qq($zone --change-batch file://$t $p),
              qr(ChangeInfo);
  unlink $t;
  $s
 }

sub awsEc2Tag($$$%)                                                             # Tag an elastic compute resource with the supplied tags.
 {my ($resource, $name, $value, %options) = @_;                                 # Resource, tag name, tag value, options.
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws cli
  my $p = awsProfile(%options);                                                 # Profile
  my $r = awsRegion(%options);                                                  # Region
  my $c = qq(aws ec2 create-tags --resources $resource ).
          qq( --tags Key=$name,Value=$value $r $p);
  xxx $c;
 }

my %confirmHasCommandLineCommand;                                               # Cache responses from which
sub confirmHasCommandLineCommand($)                                             # Check that the specified b<$cmd> is present on the current system.  Use $ENV{PATH} to add folders containing commands as necessary.
 {my ($cmd) = @_;                                                               # Command to check for
  return 1 if $confirmHasCommandLineCommand{$cmd};                              # Use cache if possible

  my $c = qx(which $cmd);                                                       # Check for command
  if ($c =~ m(/)s)
   {return ++$confirmHasCommandLineCommand{$cmd};
   }

  cluck "Unable to confirm presence of command: $cmd\n";                        # Complain if the command is not available
  undef;
 }

sub getNumberOfCpus                                                             #P Number of cpus
 {return 1 if $^O !~ m(linux)i;                                                 # Presumably there is at least 1
  my $n = confirmHasCommandLineCommand(q(nproc)) ? qx(nproc) : undef;           # nproc
  return 1 unless $n;                                                           # We must have at least 1
  $n =~ s(\s+\Z) ()r;
 }

my $numberOfCpus;                                                               # Number of cpus cache

sub numberOfCpus(;$)                                                            # Number of cpus scaled by an optional factor - but only if you have nproc. If you do not have nproc but do have a convenient way for determining the number of cpus on your system please let me know.
 {my ($scale) = @_;                                                             # Scale factor
  my $n = $numberOfCpus //= getNumberOfCpus;                                    # Cache the number of cpus as it will not change
  return $n * $scale          if $scale and $scale == int($scale);
  return int(1 + $n * $scale) if $scale and $scale != int($scale);
  $n
 }

sub ipAddressViaArp($)                                                          # Get the ip address of a server on the local network by hostname via arp
 {my ($hostName) = @_;                                                          # Host name
  return undef unless confirmHasCommandLineCommand(q(arp));                     # Confirm we have arp

  my ($line) = grep {/$hostName/i} qx(arp -a 2>&1);                             # Search for host name in arp output
  return undef unless $line;                                                    # No such host
  my (undef, $ip) = split / /, $line;                                           # Get ip address
  $ip =~ s(\x28|\x29) ()gs;                                                     # Remove brackets around ip address
  $ip                                                                           # Return ip address
 }

sub parseS3BucketAndFolderName($)                                               # Parse an L<s3> bucket/folder name into a bucket and a folder name removing any initial s3://.
 {my ($name) = @_;                                                              # Bucket/folder name

  $name = $name =~ s(s3://) ()gsr =~ s(\A\s*|\s*\Z) ()gsr;
  if ($name =~ m(\A([^/]*)/\Z)s)
   {return ($1, q())
   }
  if ($name =~ m(\A(.*?)/(.*)\Z)s)
   {return ($1, $2)
   }
 ($name, q())
 }

sub saveCodeToS3($$$$;$)                                                        # Save source code every B<$saveCodeEvery> seconds by zipping folder B<$folder> to zip file B<$zipFileName> then saving this zip file in the specified L<S3> B<$bucket> using any additional L<s3> parameters in B<$S3Parms>.
 {my ($saveCodeEvery, $folder, $zipFileName, $bucket, $S3Parms) = @_;           # Save every seconds, folder to save, zip file name, bucket/key, additional S3 parameters like profile or region as a string
  @_ == 5 or confess "Five parameters required";
  return undef unless confirmHasCommandLineCommand(q(zip));                     # Confirm we have zip
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws

  my $saveTimeFile = fpe($folder, q(codeSaveTimes));                            # Get last save time if any
  my $lastSaveTime = -e $saveTimeFile ? retrieve($saveTimeFile) : undef;        # Get last save time
  return if $lastSaveTime and $lastSaveTime->[0] > time - $saveCodeEvery;       # Too soon

  return if fork;                                                               # Fork zip upload
  my $target = fpe($bucket, $zipFileName, q(zip));                              # Target on S3
  lll "Saving latest version of code in $folder to s3://$target";

  my $z = fpe($folder, $zipFileName, q(zip));                                   # Zip file
  unlink $z;                                                                    # Remove old zip file

  if (my $c = qq(cd $folder; zip -qr $z * -x "*.zip" -x "*.gz" -x "*/blib/*" -x "*/[._]*"))  # Zip command
   {my $r = qx($c);
    confess "$c\n$r\n" if $r =~ m(\S);                                          # Confirm zip
   }

  my $s3Parms = $S3Parms // '';
  if (my $c = "aws s3 cp $z s3://$target $s3Parms")                             # Upload zip
   {my $r = qx($c);
    confess "$c\n$r\n" if $r =~ m(\S);                                          # Confirm upload
   }

  store([time], $saveTimeFile);                                                 # Save last save time
  unlink $z;                                                                    # Remove old zip file
  lll "Saved latest version of code from $folder to s3://$target";
  exit;
 }

sub saveSourceToS3($;$)                                                         #P Save source code.
 {my ($aws, $saveIntervalInSeconds) = @_;                                       # Aws target file and keywords, save internal
  $saveIntervalInSeconds //= 1200;                                              # Default save time
  warn "saveSourceToS3 is deprecated, please use saveCodeToS3 instead";
  return undef unless confirmHasCommandLineCommand(q(zip));                     # Confirm we have zip
  return undef unless confirmHasCommandLineCommand(q(aws));                     # Confirm we have aws

  unless(fork())
   {my $saveTime = "/tmp/saveTime/$0";                                          # Get last save time if any
    makePath($saveTime);

    if (my $lastSaveTime = fileModTime($saveTime))                              # Get last save time
     {return if $lastSaveTime > time - $saveIntervalInSeconds;                  # Already saved
     }

    lll "Saving latest version of code to S3";
    unlink my $z = qq(/tmp/DataTableText/save/$0.zip);                          # Zip file
    makePath($z);                                                               # Zip file folder
    return undef unless confirmHasCommandLineCommand(q(zip));                   # Confirm we have zip
    my $c = qq(zip -r $z $0);                                                   # Zip command
    print STDERR $_ for qx($c);                                                 # Zip file to be saved

    return undef unless confirmHasCommandLineCommand(q(aws));                   # Confirm we have aws
    my $a = qq(aws s3 cp $z $aws);                                              # Aws command
    my $r = qx($a);                                                             # Copy zip to S3
    #!$r or confess $r;
    writeFile($saveTime, time);                                                 # Save last save time
    lll "Saved latest version of code to S3";
    exit;
   }
 }

sub addCertificate($)                                                           # Add a certificate to the current ssh session.
 {my ($file) = @_;                                                              # File containing certificate
  return undef unless confirmHasCommandLineCommand(q(ssh-add));                 # Confirm we have ssh-add
  qx(ssh-add -t 100000000 $file 2>/dev/null);
 }

my $hostName;                                                                   # Host name cache.
sub hostName                                                                    # The name of the host we are running on.
 {return undef unless confirmHasCommandLineCommand(q(hostname));                # Confirm we have hostname
  $hostName //= trim(qx(hostname))
 }

my $userid;                                                                     # User name cache.
sub userId(;$)                                                                  # Get or confirm the userid we are currently running under.
 {my ($user) = @_;                                                              # Userid to confirm
  return $user if $user and $userid and $user eq $userid;                       # Confirm userid via cache
  return undef unless confirmHasCommandLineCommand(q(whoami));                  # Confirm we have whoami
  $userid //= trim(qx(whoami));                                                 # Cache result if necessary
  return undef if $user and $user ne $userid;                                   # Confirm userid via latest value
  $userid
 }

sub awsTranslateText($$$;$)                                                     # Translate B<$text> from English to a specified B<$language> using AWS Translate with the specified global B<$options> and return the translated string.  Translations are cached in the specified B<$cacheFolder> for reuse where feasible.
 {my ($string, $language, $cacheFolder, $Options) = @_;                         # String to translate, language code, cache folder, aws global options string

  $language =~ m(\A(ar|zh|zh\-TW|cs|da|nl|en|fi|fr|de|he|id|it|ja|ko|pl|pt|ru|es|sv|tr)\Z)i or
  confess "Language code must be one of:\n".
    formatTable([map {split /\s+/, 2} split /\n/, <<END],
Arabic     ar
Chinese-Simplified  zh
Chinese-Traditional zh-TW
Czech      cs
Danish     da
Dutch      nl
English    en
Finnish    fi
French     fr
German     de
Hebrew     he
Indonesian id
Italian    it
Japanese   ja
Korean     ko
Polish     pl
Portuguese pt
Russian    ru
Spanish    es
Swedish    sv
Turkish    tr
END
<<END
Language  Name of the language
Code      Code used to describe language
END
);
  my $name = lc nameFromString($string);                                        # Cache name from input string
  my $cached = fpe($cacheFolder, $language, $name, q(txt));                     # Cache file
  return readFile($cached) if -e $cached;                                       # Assume that what is in the cache file is a reasonable translation.

  my $options = $Options // '';
  my $c = <<END =~ s(\n) ( )gsr;
aws translate translate-text
  --text "$string"
 --source-language-code "en"
 --target-language-code "$language"
 --region "us-east-1"
 $options
END

  if (my $J = qx($c))                                                           # Translate
   {my $p = decodeJson($J);                                                     # Decode json response
    if (my $t = $p->{TranslatedText})                                           # Get translation
     {owf($cached, $t);                                                         # Cache result
      return $t;                                                                # Return translation
     }
   }
  confess "Unable to perform translation";                                      # No useful response from Aws
 }

#D1 AWS parallel                                                                # Parallel computing across multiple instances running on L<AWS>.

my $onAws;                                                                      # Cache results of L<onAws>.
sub onAws                                                                       # Returns 1 if we are on AWS else return 0.
 {return $onAws if defined $onAws;
  $onAws = -e q(/home/ubuntu/) ? 1 : 0
 }

sub onAwsPrimary                                                                # Return 1 if we are on L<AWS> and we are on the primary session instance as defined by L<awsParallelPrimaryInstanceId>, return 0 if we are on a secondary session instance, else return B<undef> if we are not on L<AWS>.
 {return undef unless onAws;                                                    # Not on Aws
  my $i = &awsCurrentInstanceId;                                                # Instance id
  my $I = &awsParallelPrimaryInstanceId;                                        # Primary instance id
  $I eq $i ? 1 : 0
 }

sub onAwsSecondary                                                              # Return 1 if we are on L<AWS> but we are not on the primary session instance as defined by L<awsParallelPrimaryInstanceId>, return 0 if we are on the primary session instance, else return B<undef> if we are not on L<AWS>.
 {return undef unless onAws;                                                    # Not on Aws
  my $i = &awsCurrentInstanceId;                                                # Instance id
  my $I = &awsParallelPrimaryInstanceId;                                        # Primary instance id
  $I ne $i ? 1 : 0
 }

sub awsParallelPrimaryInstanceId(%)                                             # Return the instance id of the primary instance. The primary instance is the instance at L<AWS> that we communicate with - it controls all the secondary instances that form part of the parallel session. The primary instance is located by finding the first running instance in instance Id order whose Name tag contains the word I<primary>. If no running instance has been identified as the primary instance, then the first viable instance is made the primary. The ip address of the primary is recorded in F</tmp/awsPrimaryIpAddress.data> so that it can be quickly reused by L<xxxr>, L<copyFolderToRemote>, L<mergeFolderFromRemote> etc. Returns the instanceId of the primary instance or B<undef> if no suitable instance exists.
 {my (%options) = @_;                                                           # Options

  my $d = awsEc2DescribeInstances(%options);                                    # Available instances
  my @id;                                                                       # Instance Ids
  for   my $r($d->{Reservations}->@*)                                           # Check instances for an existing primary instance
   {for my $i($r->{Instances}->@*)
     {if (my $s = $$i{State}{Name})                                             # Running instances
       {if ($s =~ m(running)i)
         {push @id, my $id = $$i{InstanceId};
          for my $t($$i{Tags}->@*)                                              # Tags
           {if (my $v = $$t{Value})
             {if  ($v =~ m(SessionLeader|Primary)i)
               {for my $I($i->{NetworkInterfaces}->@*)                          # Save first public Ip address in a well known location
                 {my $ip = $$I{Association}{PublicIp};
                  saveAwsIp($ip);                                               # Save ip address
                  last;
                 }
                return $id;                                                     # Return existing primary instance
               }
             }
           }
         }
       }
     }
   }

  if (my ($id) = @id)                                                           # No instance marked as primary but running instances available
   {awsEc2Tag($id, Name=>q(Primary), %options);
    return  $id;
   }

  confess "No instances running"                                                # No running instances
 }

sub awsParallelSpreadFolder($%)                                                 # On L<aws>: copies a specified B<$folder> from the primary instance, see: L<awsParallelPrimaryInstanceId>, in parallel, to all the secondary instances in the session. If running locally: copies the specified folder to all L<aws> session instances both primary and secondary.
 {my ($folder, %options) = @_;                                                  # Fully qualified folder name, options
  -d $folder or confess "No such folder:\n$folder\n";                           # Check source exists
  my $f = fpd($folder);                                                         # Normalize the folder name

  my sub spread(@)                                                              # Spread folder to the specified ip addresses
   {my (@i) = @_;                                                               # Ip addresses
    my @pid;
    for my $i(@i)                                                               # Each secondary
     {if (my $pid = fork)
       {push @pid, $pid;
       }
      else
       {makePathRemote($f, $i);                                                 # Create remote folder so rsync does not complain
        copyFolderToRemote($f, $i);                                             # Copy folder to remote
        exit;
       }
     }
    waitpid $_, 0 for @pid;
   }

  if (onAwsPrimary)                                                             # Running on Aws primary - merge folders from secondary instances
   {spread(awsParallelSecondaryIpAddresses(%options));
   }
  elsif (!onAws)                                                                # Running locally - merge folders from all instances
   {spread(awsParallelIpAddresses(%options));
   }
  else                                                                          # Unknown location
   {confess "Running somewhere other than locally or on aws primary\n";
   }
 }

sub awsParallelGatherFolder($%)                                                 # On L<aws>: merges all the files in the specified B<$folder> on each secondary instance to the corresponding folder on the primary instance in parallel.  If running locally: merges all the files in the specified folder on each L<aws> session instance (primary and secondary) to the corresponding folder on the local machine.  The folder merges are done in parallel which makes it impossible to rely on the order of the merges.
 {my ($folder, %options) = @_;                                                  # Fully qualified folder name, options
  my $f = fpd($folder);                                                         # Normalize the folder name
  makePath($f);                                                                 # Create target folder

  my sub gather(@)                                                              # Gather folder from specified ip addresses
   {my (@i) = @_;                                                               # Ip addresses
    my @pid;
    for my $i(@i)                                                               # Each secondary
     {if (my $pid = fork)
       {push @pid, $pid;
       }
      else
       {makePathRemote($f, $i);                                                 # Create remote folder so rsync does not complain
        mergeFolderFromRemote($f, $i);                                          # Merge folder from remote
        exit;
       }
     }
    waitpid $_, 0 for @pid;
   }

  if (onAwsPrimary)                                                             # Running on Aws primary
   {gather(awsParallelSecondaryIpAddresses(%options));
   }
  elsif (!onAws)                                                                # Running locally
   {if (my $i = awsParallelPrimaryIpAddress(%options))
     {gather($i, awsParallelSecondaryIpAddresses(%options));
     }
   }
  else                                                                          # Unknown location
   {confess "Running somewhere other than locally or on aws primary\n";
   }
 } # awsParallelGatherFolder

sub awsParallelPrimaryIpAddress(%)                                              # Return the IP addresses of any primary instance on L<aws>.
 {my (%options) = @_;                                                           # Options

  my $s = awsParallelPrimaryInstanceId(%options);                               # Instance id of primary instance
  if (my $instanceIds = awsEc2DescribeInstancesGetIPAddresses(%options))        # {instance id => instance ip }
   {return $$instanceIds{$s};                                                   # Ip address of primary
   }

  undef
 }

sub awsParallelSecondaryIpAddresses(%)                                          # Return a list containing the IP addresses of any secondary instances on L<aws>.
 {my (%options) = @_;                                                           # Options

  my @i;
  my $s = awsParallelPrimaryInstanceId(%options);                               # Instance id of primary instance
  if (my $instanceIds = awsEc2DescribeInstancesGetIPAddresses(%options))        # {instance id => instance ip }
   {for my $id(sort keys %$instanceIds)                                         # Each running instance
     {next if $id eq $s;                                                        # Skip primary instance
      push @i, $$instanceIds{$id};                                              # Save ip address of secondary instance
     }
   }

  @i                                                                            # Ip addresses of any secondary instances
 }

sub awsParallelIpAddresses(%)                                                   # Return the IP addresses of all the L<aws> session instances.
 {my (%options) = @_;                                                           # Options

  my @i;
  my $s = awsParallelPrimaryInstanceId(%options);                               # Instance id of primary instance
  if (my $instanceIds = awsEc2DescribeInstancesGetIPAddresses(%options))        # {instance id => instance ip }
   {for my $id(sort keys %$instanceIds)                                         # Each running instance
     {push @i, $$instanceIds{$id};                                              # Save ip address of secondary instance
     }
   }

  @i                                                                            # Ip addresses of all instances
 }

sub getCodeContext($)                                                           # Recreate the code context for a referenced sub
 {my ($sub) = @_;                                                               # Sub reference
  my @l = readFile($0);
  my @c;
  for my $i(keys @l)
   {my $l = $l[$i];
    last if $i and $l =~ m/\A#!/;
    push @c, $l if $l =~ m/\A(#!|use )/;
   }
   if ($0 =~ m(\.pm\Z)i and $0 !~ m(DataTableText)i)                            # If we were started from a pm file we include the pm file as well as there will be no "use" to bring it in. "do" is use in preference to "use" as we want the same context as if we were in the module
    {push @c, <<END;
if (1)
 {use Data::Table::Text qw(readFile);
  my \$s = Data::Table::Text::readFile(q($0));
  eval \$s;
  confess "\$s\n\$@\n" if \$@;
 }
END
    }
  join q(), @c;
 }

sub awsParallelProcessFiles($$$$%)                                              #I Process files in parallel across multiple L<aws> instances if available or in series if not.  The data located by B<$userData> is transferred from the primary instance, as determined by L<awsParallelPrimaryInstanceId>, to all the secondary instances. B<$parallel> contains a reference to a sub, parameterized by array @_ = (a copy of the user data, the name of the file to process), which will be executed upon each session instance including the primary instance to update $userData. B<$results> contains a reference to a sub, parameterized by array @_ = (the user data, an array of results returned by each execution of $parallel), that will be called on the primary instance to process the results folders from each instance once their results folders have been copied back and merged into the results folder of the primary instance. $results should update its copy of $userData with the information received from each instance. B<$files> is a reference to an array of the files to be processed: each file will be copied from the primary instance to each of the secondary instances before parallel processing starts. B<%options> contains any parameters needed to interact with L<ec2>  via the L<awscli>.  The returned result is that returned by sub $results.
 {my ($userData, $parallel, $results, $files, %options) = @_;                   # User data or undef, parallel sub reference, series sub reference, [files to process], aws cli options.
  $userData //= {};                                                             # Default value for user data else storable will complain
  my $d = temporaryFolder;                                                      # Temporary folder containing a description of what needs to be done
  my $r = fpd($d, q(out));                                                      # Results folder

  $options{region} //= awsCurrentRegion;                                        # Default region

  if (onAws and my @i = awsParallelSecondaryIpAddresses(%options))              # Process across multiple session instances on AWS
   {my @buckets = packBySize(@i+0, map {[fileSize($_), $_]} @$files);           # Pack files into buckets for each secondary instance

    for my $i(keys @i)                                                          # Each other session instance
     {storeFile(my $f = fpe($d, $i[$i], qw(files data)), $buckets[$i]);         # Save files to be processed on each of the other session instance
     }

    my $parallelSubName = join '::', (getSubName($parallel))[0,1];              # Get name of parallel sub
    my $resultsSubName  = join '::', (getSubName($results)) [0,1];              # Get name of results sub
    my $codeContext     = getCodeContext($parallel);                            # Get context of parallel sub

    my $userDataFile = fpe($d, qw(user data));                                  # Save user data in this file so we get a fresh copy each time effectively making it read only
    storeFile($userDataFile, $userData);                                        # Save user data

    my $c = writeFile(fpe($d, qw(code pl)), <<END);                             # Code to be executed in parallel on the secondary instances
$codeContext

my \$folder   = fp(\$0);
my \$files    = retrieveFile(fpe(\$folder, awsCurrentIp, qw(files data)));
my \$userData = retrieveFile(fpe(\$folder, qw(user data)));

processFilesInParallel
 (sub
   {my (\$file) = \@_;
    $parallelSubName(\$userData, \$file)
   },
  sub
   {my \$r = $resultsSubName(\$userData, \@_);
    my \$f = fpe(q($r), awsCurrentIp, q(data));
    storeFile(\$f, \$r);
    \$r
   },
  @\$files,
 );
END

    xxx qq(perl -c $c), qr(syntax OK);                                          # Syntax perl code before we ship it off to the secondary instances for execution
    awsParallelSpreadFolder($d, %options);                                      # Save processing request to on each of the other session instance

    if (1)                                                                      # Spread folders containing all the input files to be processed in parallel across each of the secondary instances so that they have a complete copy of the data to be processed
     {my %f = map {fp($_)=>1} @$files;
      processFilesInParallel(sub
       {my ($f) = @_;
        awsParallelSpreadFolder($f, %options);
       },
      undef, sort keys %f);
     }

    my @c;                                                                      # Commands to process on each of the secondary instances
    for my $i(@i)                                                               # Each of the secondary instances available for processing
     {push @c, <<END =~ s(\n) ( )gsr;                                           # Execute code, retrieve results from each of the secondary instances
ssh $i "perl $c                 2>&1" ;
rsync -mpqrt       '$i:$d' '$d' 2>&1
END
     }

    if (my $pid = fork)                                                         # Parent: merge results from each secondary instance
     {waitpid $pid, 0;                                                          # Wait for the secondary instances to finish
      return &$results($userData,                                               # Combine primary instance and secondary instance results with the user data
        map {retrieveFile($_)} searchDirectoryTreesForMatchingFiles($r));       # Merge data from each secondary instances
     }
    else                                                                        # Child: Execute on the secondary instances in parallel
     {my $cmd = join ' & ', map {qq/( $_ )/} @c;
     #lll $cmd;
      lll qx($cmd);
      exit;
     }
   }

  else                                                                          # Run on local computer or on a single Aws instance
   {return processFilesInParallel                                               # Process bucket[0] on primary instance
     (sub
       {my ($file) = @_;
        &$parallel($userData, $file)
       },
      sub
       {&$results($userData, @_);
       },
      @$files,
     );
   }
 } # awsParallelProcessFiles

sub awsParallelProcessFilesTestParallel($$)                                     #P Test running on L<AWS> in parallel.
 {my ($userData, $file) = @_;                                                   # User data, file to process.
  my $i = &awsCurrentIp||q(localHost);
  $userData->{files}{$file} = fileMd5Sum($file);
  $userData->{ip}    {$i}   = 1;                                                # UserData is reused each time so we cannot ++
  $userData->{ipFile}{$i}{$file}++;
  $userData;
 }

sub awsParallelProcessFilesTestResults($@)                                      #P Test results of running on L<AWS> in parallel.
 {my ($userData, @results) = @_;                                                # User data from primary instance instance or process, results from each parallel instance or process

  for   my $x(@results)
   {for my $f(sort keys $x->{files}->%*)
     {$userData->{files}{$f} = $x->{files}{$f};
     }
    for my $i(sort keys $x->{ip}->%*)
     {$userData->{ip}{$i}   += $x->{ip}{$i};
     }
    for   my $i(sort keys $x->{ipFile}    ->%*)
     {for my $f(sort keys $x->{ipFile}{$i}->%*)
       {$userData->{ipFile}{$i}{$f} = $x->{ipFile}{$i}{$f};
       }
     }
    $userData->{merge} += $x->{merge}//0;                                       # Merges done else where
   }

  $userData->{merge}++;                                                         # This merge
  $userData
 }

#D1 S3                                                                          # Work with S3 as if it were a file system.

sub s3Profile(%)                                                                #P Return an S3 profile keyword from an S3 option set
 {my (%options) = @_;                                                           # Options
  my $p = $options{profile};                                                    # Profile option
  $p ? qq( --profile $p) : q()                                                  # Return profile keyword if profile specified
 }

sub s3Delete(%)                                                                 #P Return an S3 --delete keyword from an S3 option set
 {my (%options) = @_;                                                           # Options
  my $p = $options{delete};                                                     # Delete option
  $p ? qq( --delete) : q()                                                      # Return delete keyword if profile specified
 }

sub s3ListFilesAndSizes($%)                                                     # Return {file=>size} for all the files in a specified B<$folderOrFile> on S3 using the specified B<%options> if any.
 {my ($folderOrFile, %options) = @_;                                            # Source on S3 - which will be truncated to a folder name, options
  my ($bucket, $folder) = parseS3BucketAndFolderName($folderOrFile);            # Parse an L<s3> bucket/folder name into a bucket and a folder name removing any initial s3://.
  my $profile = s3Profile(%options);                                            # Add profile if specified
  my $getCmd  = qq(aws s3 ls s3://$bucket/$folder $profile --recursive);        # Command to get the sizes of the files to download
  my $files   = qx($getCmd);                                                    # Get the sizes of the files to download
  my @files   = map {my @a = split m/\s+/, $_, 4; [@a[-1, -2, 0, 1]]}           # Files and sizes
                split m/\n/, $files;
  {map {q(s3://).fpf($bucket, $$_[0]) => $_} @files}                            # Hash {file=>[name, size, modified date, modified time]}
 }

sub s3FileExists($%)                                                            # Return (name, size, date, time) for a B<$file> that exists on S3 else () using the specified B<%options> if any.
 {my ($file, %options) = @_;                                                    # File on S3 - which will be truncated to a folder name, options
  my %files = s3ListFilesAndSizes($file, %options);                             # Details of files with that prefix
  return () unless keys %files == 1;                                            # Only one file expected
  my ($f)   = keys %files;                                                      # File name
  my $d     = $files{$f};                                                       # Details of the one file
  return () unless $$d[3];                                                      # All details present
  @$d                                                                           # Return details of one file
 }

sub s3WriteFile($$%)                                                            # Write to a file B<$fileS3> on S3 the contents of a local file B<$fileLocal> using the specified B<%options> if any.  $fileLocal will be removed if %options contains a key cleanUp with a true value.
 {my ($fileS3, $fileLocal, %options) = @_;                                      # File to write to on S3, string to write into file,  options
  my ($bucket, $folder) = parseS3BucketAndFolderName($fileS3);                  # Parse an L<s3> bucket/folder name into a bucket and a folder name removing any initial s3://.
  my $profile = s3Profile(%options);                                            # Add profile if specified
  my $f       = pad($fileLocal,               32);
  my $s       = pad(qq(s3://$bucket/$folder), 32);
  my $cmd     = qq(aws s3 cp $f $s $profile --quiet);                           # Command to write the temporary file into S3 with the specified file name
  xxx $cmd;                                                                     # Execute and print command
# unlink $fileLocal if $options{cleanUp};                                       # Remove local file after upload if requested
 }

sub s3WriteString($$%)                                                          # Write to a B<$file> on S3 the contents of B<$string> using the specified B<%options> if any.
 {my ($file, $string, %options) = @_;                                           # File to write to on S3, string to write into file,  options
  my ($bucket, $folder) = parseS3BucketAndFolderName($file);                    # Parse an L<s3> bucket/folder name into a bucket and a folder name removing any initial s3://.
  my $profile = s3Profile(%options);                                            # Add profile if specified
  my $temp    = writeFile(undef, $string);                                      # Write the string to a temporary file
  my $f       = pad($temp,                    32);
  my $s       = pad(qq(s3://$bucket/$folder), 32);
  my $cmd     = qq(aws s3 cp $f $s $profile --quiet);                           # Command to write the temporary file into S3 with the specified file name
  xxx $cmd;                                                                     # Execute and print command
  unlink $temp;
 }

sub s3ReadFile($$%)                                                             # Read from a B<$file> on S3 and write the contents to a local file B<$local> using the specified B<%options> if any.  Any pre existing version of the local file $local will be deleted.  Returns whether the local file exists after completion of the download.
 {my ($file, $local, %options) = @_;                                            # File to read from on S3, local file to write to,  options
  my ($bucket, $folder) = parseS3BucketAndFolderName($file);                    # Parse an L<s3> bucket/folder name into a bucket and a folder name removing any initial s3://.
  my $profile = s3Profile(%options);                                            # Add profile if specified
  my $quiet   = $file =~ m(pcd\Z)i ? q() : q( --quiet);                         # Watch certain important files
  my $d       = temporaryFolder;
  my $F       = fpe(temporaryFile, qw(download txt));
  my $f       = pad($F,                       32);
  my $s       = pad(qq(s3://$bucket/$folder), 32);
  my $cmd     = qq(aws s3 cp $s $f $profile $quiet);                            # Command to write the temporary file into S3 with the specified file name
  lll $cmd;
  xxx $cmd;                                                                     # Download
  moveFileWithClobber($f, $local);                                              # Update local file if a file was in fact downloaded
  clearFolder($d, 11);
  -f $local
 }

sub s3ReadString($%)                                                            # Read from a B<$file> on S3 and return the contents as a string using specified B<%options> if any.  Any pre existing version of $local will be deleted.  Returns whether the local file exists after completion of the download.
 {my ($file, %options) = @_;                                                    # File to read from on S3, options
  my ($bucket, $folder) = parseS3BucketAndFolderName($file);                    # Parse an L<s3> bucket/folder name into a bucket and a folder name removing any initial s3://.
  my $profile = s3Profile(%options);                                            # Add profile if specified
  my $local   = temporaryFile;                                                  # Temporary file to hold download
  my $f       = pad($local,                   32);
  my $s       = pad(qq(s3://$bucket/$folder), 32);
  my $cmd     = qq(aws s3 cp $s $f $profile --quiet);                           # Command to write the temporary file into S3 with the specified file name
  xxx $cmd;                                                                     # Execute and print command
  if (-f $local)                                                                # Retrieve string from temporary file
   {my $s = readFile($local);                                                   # Read temporary file
    unlink $local;                                                              # Remove temporary file
    return $s;                                                                  # Return contend downloaded from S3
   }
  undef                                                                         # No such file accessible on S3
 }

sub s3DownloadFolder($$%)                                                       # Download a specified B<$folder> on S3 to a B<$local> folder using the specified B<%options> if any.  Any existing data in the $local folder will be will be deleted if delete=>1 is specified as an option. Returns B<undef on failure> else the name of the B<$local> on success.
 {my ($folder, $local, %options) = @_;                                          # Folder to read from on S3, local folder to write to,  options
  $folder =~ s(\As3://) ();                                                     # Normalize folder name
  makePath($local);                                                             # Create local path if necessary
  my $profile = s3Profile(%options);                                            # Add profile if specified
  my $delete  = s3Delete (%options);                                            # Add delete if specified
  my $f       = pad($local,           32);
  my $s       = pad(qq(s3://$folder), 32);
  my $cmd     = qq(aws s3 sync $s $f $profile $delete);                         # Command to copy the folder on S3 to the local folder
  xxx $cmd;                                                                     # Download
  -f $local                                                                     # Test for local file after download
 }

sub s3ZipFolder($$%)                                                            # Zip the specified B<$source> folder and write it to the named B<$target> file on S3.
 {my ($source, $target, %options) = @_;                                         # Source folder, target file on S3, S3 options
  unless(-d $source)                                                            # Check the folder exists
   {confess "No such folder: $source";
   }
  return undef unless confirmHasCommandLineCommand(q(zip));                     # Confirm we have zip
  my $z = fpe(temporaryFile, q(zip));                                           # Local zip file
  my $c = qq(cd $source; zip -qr $z .);                                         # Zip command
  xxx $c, qr(\A\s*\Z);
  my $r = s3WriteFile($target, $z, %options);                                   # Upload to S3
  unlink $z;
  $r
 }

sub s3ZipFolders($%)                                                            # Zip local folders and upload them to S3 in parallel.  B<$map> maps source folder names on the local machine to target folders on S3. B<%options> contains any additional L<AWS> cli options.
 {my ($map, %options) = @_;                                                     # Source folder to S3 mapping, S3 options

  &runInParallel(&numberOfCpus(8), sub                                          # Upload in parallel
   {my ($r) = @_;
    &s3ZipFolder(@$r, %options);
   },
  sub {},
  map{[$_, $$map{$_}]} sort keys %$map);
 }

#D1 GitHub                                                                      # Simple interactions with L<GitHub> - for more complex interactions please use L<GitHub::Crud>.

sub downloadGitHubPublicRepo($$)                                                # Get the contents of a public repo on GitHub and place them in a temporary folder whose name is returned to the caller or confess if no such repo exists.
 {my ($user, $repo) = @_;                                                       # GitHub user, GitHub repo
  my $t = temporaryFolder;                                                      # Folder to download to
  my $z = fpe($t, qw(gh zip));                                                  # Zip file
  my $s = fpe(q(https://github.com/), $user, $repo, qw(archive master zip));    # L<url> to GitHub to retrieve zipped repository
  confirmHasCommandLineCommand(q(wget));                                        # Conform we have wget
  my $d = xxx qq(wget -O $z $s), qr(200 OK);                                    # Run download
     $d =~ m(ERROR 404: Not Found)s || !-e $z || fileSize($z) < 1e2 and         # Make sure we got a zip file
     confess "No such user/repo on GitHub or repo too small:\n$d\n";
  xxx qq(cd $t; unzip $z; rm $z; ls -lah), qr();                                # Unzip the zip file
  $t                                                                            # Return the folder containing the unzipped files
 }

sub downloadGitHubPublicRepoFile($$$)                                           # Get the contents of a B<$user> B<$repo> B<$file> from  a public repo on GitHub and return them as a string.
 {my ($user, $repo, $file) = @_;                                                # GitHub user, GitHub repository, file name in repository
  my $s = fpf(q(https://raw.githubusercontent.com/), $user, $repo, q(master), $file);
  my $t = temporaryFile;                                                        # File to download into
  my $d = xxx qq(wget -O $t $s), qr(200 OK);                                    # Run download
     $d =~ m(ERROR 404: Not Found)s and                                         # Make sure we got the file
     confess "No such user/repo/file on GitHub:\n$d\n";
  -f $t or confess "No output from user/repo/file on GitHub";                   # Check we got a result
  my $r = readFile($t);                                                         # Read results
  unlink $t;                                                                    # Remove temporary output file
  $r                                                                            # Return data read from github
 }

#D1 Processes                                                                   # Start processes, wait for them to terminate and retrieve their results

sub startProcess(&\%$)                                                          # Start new processes while the number of child processes recorded in B<%$pids> is less than the specified B<$maximum>.  Use L<waitForAllStartedProcessesToFinish|/waitForAllStartedProcessesToFinish> to wait for all these processes to finish.
 {my ($sub, $pids, $maximum) = @_;                                              # Sub to start, hash in which to record the process ids, maximum number of processes to run at a time
  warn "Deprecated in favor of newProcessStarter";
  while(keys(%$pids) >= $maximum)                                               # Wait for enough processes to terminate to bring us below the maximum number of processes allowed.
   {my $p = waitpid 0,0;
#   $$pids{$p} or confess "Pid $p not defined in ".dump($pids)."\n";
    delete $$pids{$p}
   }

  if (my $pid = fork)                                                           # Create new process
   {$$pids{$pid}++                                                              # Update pids
   }
  else                                                                          # Run sub in new process
   {&$sub;
    exit;
   }
 }

sub waitForAllStartedProcessesToFinish(\%)                                      # Wait until all the processes started by L<startProcess|/startProcess> have finished.
 {my ($pids) = @_;                                                              # Hash of started process ids
  warn "Deprecated in favor of newProcessStarter";
  while(keys %$pids)                                                            # Remaining processes
   {my $p = waitpid 0,0;
#   $$pids{$p} or cluck "Pid $p not defined in ".dump($pids)."\n";
    delete $$pids{$p}
   }
 }

sub newProcessStarter($%)                                                       # Create a new L<process starter|/Data::Table::Text::Starter Definition> with which to start parallel processes up to a specified B<$maximumNumberOfProcesses> maximum number of parallel processes at a time, wait for all the started processes to finish and then optionally retrieve their saved results as an array from the folder named by B<$transferArea>.
 {my ($maximumNumberOfProcesses, %options) = @_;                                # Maximum number of processes to start, options
  my $h = genHash(q(Data::Table::Text::Starter),                                # Process starter definition.
    transferArea             => temporaryFolder,                                # The name of the folder in which files transferring results from the child to the parent process will be stored.
    autoRemoveTransferArea   => 1,                                              # If true then automatically clear the transfer area at the end of processing.
    maximumNumberOfProcesses => $maximumNumberOfProcesses // 8,                 # The maximum number of processes to start in parallel at one time. If this limit is exceeded, the start of subsequent processes will be delayed until processes started earlier have finished.
    pids                     => {},                                             # A hash of pids representing processes started but not yet completed.
    resultsArray             => [],                                             # Consolidated array of results.
    processingTitle          => undef,                                          #I Optional: title describing the processing being performed.
    processingLogFile        => undef,                                          #I Optional: name of a file to which process start and end information should be appended
    processingLogFileHandle  => undef,                                          # Handle for log file if a log file was supplied
    totalToBeStarted         => undef,                                          #I Optionally: the total number of processes to be started - if this is supplied then an estimate of the finish time for this processing is printed to the log file every time a process starts or finishes.
    processStartTime         => {},                                             # {pid} == time the process was started.
    processFinishTime        => {},                                             # {pid} == time the process finished.
    startTime                => time,                                           # Start time
   );

  loadHash($h, %options);                                                       # Load and validate the options
 }

sub Data::Table::Text::Starter::logEntry($$)                                    #P Create a log entry showing progress and eta.
 {my ($starter, $finish) = @_;                                                  # Starter, 0 - start; 1 - finish
  if (my $l = $starter->processingLogFile)                                      # Write a log entry if a log file has been supplied
   {my $t   = $starter->processingTitle // '';                                  # Title of processing
    my $sf  = $finish ? q(F) : q(S);                                            # Whether we are starting or finishing
    my $N   = $starter->totalToBeStarted;                                       # Total number to be started if known
    my $M   = $starter->maximumNumberOfProcesses // 1;                          # Maximum number of processes in parallel
    my $started  = keys %{$starter->processStartTime};                          # Number of processes started
    my $finished = keys %{$starter->processFinishTime};                         # Number of processes finished

    if (!$finish and $started == 1 and $t)                                      # Title message
     {my $n = $N ? qq(Start $N processes in parallel upto $M for:) :
                   qq(Process in parallel upto $M:);
      $starter->say(join " ", timeStamp, "$n $t");
     }

    my $eta = sub                                                               # Estimate finish time
     {if ($N and $finished)                                                     # Expected number of starts has been supplied and at least one process has finished
       {my $avgExecTime = $starter->averageProcessTime;                         # Average execution time process
        my $toGo        = ($N - $finished) * $avgExecTime / $M;                 # Time to go not with standing Amdahl's law.
        my @finishAt    = localtime(time + $toGo);                              # Finish time
        my $finishTime  = strftime('%H:%M:%S', @finishAt);                      # Format finish time
        return sprintf("eta: %.2f seconds at $finishTime", $toGo);              # Finish time message
       }
      q()                                                                       # No estimate available for finish time
     }->();

    my $w = $N ? length($N) : 0;                                                # Width of output field
    my $p = $N == 0 ? q()   :                                                   # Progress indicator
      sprintf("%${w}d", $finish ? $finished : $started).q(/).$N;

    $starter->say(join " ", timeStamp, $sf, $p, $eta, $t);
   }
 }

sub Data::Table::Text::Starter::averageProcessTime($)                           #P Average elapsed time spent by each process
 {my ($starter) = @_;                                                           # Starter
  my $execTime = 0;                                                             # Total execution time for all processes that have finished so far
  for my $finish(sort keys %{$starter->processFinishTime})                      # Sum execution time over all processes that have finished
   {my $f =                  $starter->processFinishTime->{$finish} // 0;       # Finish time
    my $s =                  $starter->processStartTime ->{$finish} // 0;       # Start time
    $execTime += $f - $s;                                                       # Execution time
   }
  my $finished = keys %{$starter->processFinishTime} || 1;                      # Number of processes finished
  $execTime / $finished;                                                        # Average execution time process
 }

sub Data::Table::Text::Starter::say($@)                                         #P Write to the log file if it is available.
 {my ($starter, @message) = @_;                                                 # Starter, text to write to log file.
  return unless my $F = $starter->processingLogFileHandle;                      # Number of processes started
  flock($F, 2);
  print {$F} join '', @message, "\n";
 }

sub Data::Table::Text::Starter::start($$)                                       # Start a new process to run the specified B<$sub>.
 {my ($starter, $sub) = @_;                                                     # Starter, sub to be run.

  my $started  = keys %{$starter->processStartTime};                            # Number of processes started

  if ($started == 0)                                                            # Create a log file if logging requested and no processes have been started yet
   {if (my $file = $starter->processingLogFile)
     {makePath($file);
      open my $F, ">>$file" or
        confess "Cannot open file for write, file:\n$file\n$!\n";
      binmode($F, ":utf8");
      $starter->processingLogFileHandle = $F;
     }
   }

  while(keys(%{$starter->pids}) >= $starter->maximumNumberOfProcesses)          # Wait for enough processes to terminate to bring us below the maximum number of processes allowed.
   {$starter->waitOne;
   }

  if (my $pid = fork)                                                           # Create new process
   {my $startTime = time;
    $starter->pids->{$pid}++;                                                   # Update pids
    $starter->processStartTime->{$pid} = time;                                  # Time process was started
    $starter->logEntry;                                                         # Write a log entry
   }
  else                                                                          # Run sub in new process
   {#setpriority(0, 0, +1);                                                     # Run at a slightly lower priority to make sure the parent can reap zombies as quickly as possible - questionable and does not work on "haiku"
    my $results = &$sub;                                                        # Execute sub and address results
    if (my $t = $starter->transferArea)                                         # Transfer folder
     {my $f = fpe($t, $$, q(data));                                             # Transfer file in transfer folder
      makePath($f);                                                             # Make path for transfer file folder
      eval {store [$results], $f};                                              # Store data
      $@ and confess "$@\n";                                                    # Confess to any errors
     }
    exit;
   }
 }

sub Data::Table::Text::Starter::waitOne($)                                      #P Wait for at least one process to finish and consolidate its results.
 {my ($starter) = @_;                                                           # Starter
  my $select = 0;                                                               # Must wait for at least one process to finish
  my $startTime = time;

  while(keys(%{$starter->pids}) and my $p = waitpid 0, $select)                 # Wait for a process to finish - get its pid
   {if ($starter->pids->{$p})                                                   # One of ours and it has data to transfer
     {if (my $t = $starter->transferArea)                                       # Transfer folder
       {my $f = fpe($t, $p, q(data));                                           # Transfer file in transfer folder
        if (-e $f)
         {my $size = fileSize($f);
          my $big  = $size > 1e9;
          lll "Retrieve $f start size=$size " if $big;
          if (my $d = eval {retrieve $f})                                       # Retrieve data
           {if (ref($d) =~ m(array)is)                                          # Check we got an array reference
             {if (@$d == 1)                                                     # array should have just one element
               {push @{$starter->resultsArray}, $$d[0];                         # Save data in parent
               }
              else
               {confess "Too many process results returned";
               }
             }
            else
             {confess "Expected an of process array";
             }
           }
          else
           {cluck "Unable to retrieve process results";
           }
          mmm "Retrieve $f end" if $big;
         }
        else
         {die "No such process file: $f\n";
         }
       }
     }

    $starter->processFinishTime->{$p} = time;                                   # Approximate time process ended
    $starter->logEntry(1);                                                      # Write a log entry
    delete $starter->pids->{$p};                                                # Remove pid from consideration
    $select = WNOHANG;                                                          # Subsequent waits do not, in fact, wait - if more finished processes are immediately available then they will be harvested, otherwise no outstanding finished processes are available to harvest and the while loop terminates.
   }
 }

sub Data::Table::Text::Starter::finish($)                                       # Wait for all started processes to finish and return their results as an array.
 {my ($starter) = @_;                                                           # Starter

  while(keys(%{$starter->pids}) > 0)                                            # Wait for all started processes to terminate
   {$starter->waitOne;
   }

  my @r = @{$starter->resultsArray};                                            # Return results

  if (my $l = $starter->processingLogFile)                                      # Log file provided
   {my $t        = $starter->processingTitle // '';                             # Title of processing
    my $N        = $starter->totalToBeStarted;                                  # Total number to be started if known
    my $started  = keys %{$starter->processStartTime};                          # Number of processes started
    my $finished = keys %{$starter->processFinishTime};                         # Number of processes finished

    my @m;
    if ($t)
     {push @m, timeStamp. " Finished $finished processes for: $t"
     }
    else
     {push @m, timeStamp. " Finished $finished processes"
     }

    push @m, "Elapsed time: ".
     sprintf("%.2f seconds", time - $starter->startTime);

    push @m, "Average process execution time: ".
      sprintf("%.2f seconds", $starter->averageProcessTime);

    my $but = qq(Started $started processes but);                               # Complain if not enough processes finished
    if ($started != @r)
     {my $r = @r;
      push @m, "$but only received results from $r";
     }
    if ($started != $finished)
     {push @m, "$but only $finished finished";
     }
    if ($started != $N)
     {push @m, "$but totalToBeStarted=>$N was specified";
     }
    if (my $F = $starter->processingLogFileHandle)                              # Log
     {$starter->say(join "\n", @m);                                             # Log message
      $starter->processingLogFileHandle = undef;
      close($F);                                                                # Close log
     }
   }

  if ($starter->autoRemoveTransferArea)                                         # Clear the transfer area if requested
   {clearFolder($starter->transferArea, scalar(@r)+1)
   }

  @r                                                                            # Return results
 }

sub squareArray(@)                                                              # Create a two dimensional square array from a one dimensional linear array.
 {my (@array) = @_;                                                             # Array
  my $N = @array;                                                               # Size of linear array
  my $n = int sqrt $N;                                                          # Dimension of square array
  ++$n unless $n*$n == $N;                                                      # Adjust up unless perfect square
  my @s;                                                                        # Square array
  my $i = 0; my $j = 0;                                                         # Current coordinates in square array
  for my $e(@array)                                                             # Load square array from linear array
   {$s[$j][$i] = $e;                                                            # Current element
    ++$i;                                                                       # Next minor coordinate
    ++$j, $i = 0 if $i >= $n;                                                   # Next major coordinate
   }
  @s                                                                            # Resulting square array
 }

sub deSquareArray(@)                                                            # Create a one dimensional array from a two dimensional array of arrays
 {my (@square) = @_;                                                            # Array of arrays
  my @a;
  for my $r(@square)                                                            # Each row
   {ref($r) =~ m(array)is or confess "Not an array reference";
    push @a, @$r;                                                               # Push row contents
   }
  @a                                                                            # Linear array
 }

sub countSquareArray(@)                                                         #P Count the number of elements in a square array
 {my (@square) = @_;                                                            # Array of arrays
  my $a = 0;
  for my $r(@square)                                                            # Each row
   {ref($r) =~ m(array)is or confess "Not an array reference";
    $a += scalar(@$r);                                                          # Push row contents
   }
  $a                                                                            # Count
 }

sub rectangularArray($@)                                                        # Create a two dimensional rectangular array whose first dimension is B<$first> from a one dimensional linear array.
 {my ($first, @array) = @_;                                                     # First dimension size, array
  my $N = @array;                                                               # Size of linear array
  return @array if $N < 2;                                                      # Data is already a 1 x N rectangle
  my @r;                                                                        # Rectangular array
  for my $i(keys @array)                                                        # Load rectangular array from linear array
   {push $r[$i % $first]->@*, $array[$i];
   }

  @r                                                                            # Resulting rectangular array
 }

sub rectangularArray2($@)                                                       # Create a two dimensional rectangular array whose second dimension is B<$second> from a one dimensional linear array.
 {my ($second, @array) = @_;                                                    # Second dimension size, array
  my $N = @array;                                                               # Size of linear array
  my @r;                                                                        # Rectangular array
  for my $i(keys @array)                                                        # Load rectangular array from linear array
   {my $r = $i % $second;
    my $j = ($i - $r) / $second;
    $r[$j][$r] = $array[$i];
   }

  @r                                                                            # Resulting rectangular array
 }

sub callSubInParallel(&)                                                        # Call a sub reference in parallel to avoid memory fragmentation and return its results.
 {my ($sub) = @_;                                                               # Sub reference

  my $file = temporaryFile;                                                     # Temporary file to receive results

  if (my $pid = fork)                                                           # Parent: wait for child Xref to finish
   {waitpid $pid, 0;                                                            # Wait for results
    my $x = retrieveFile($file);                                                # Retrieve results
    unlink $file;                                                               # Remove results file
    return @$x if wantarray;                                                    # Return results as an array
    $$x[0];                                                                     # Return results
   }
  else                                                                          # Child: call in a separate process to avoid memory fragmentation in parent
   {storeFile($file, [&$sub]);                                                  # Execute child and return results
    exit;
   }
 }

sub callSubInOverlappedParallel(&&)                                             # Call the B<$child> sub reference in parallel in a separate child process and ignore its results while calling the B<$parent> sub reference in the parent process and returning its results.
 {my ($child, $parent) = @_;                                                    # Sub reference to call in child process, sub reference to call in parent process

  if (my $pid = fork)                                                           # Parent
   {my $r = [&$parent];                                                         # Parent sub
    waitpid $pid, 0;                                                            # Wait for child
    return @$r if wantarray;                                                    # Return results as an array
    $$r[0];                                                                     # Return results
   }
  else                                                                          # Child
   {&$child;                                                                    # Ignore results
    exit;
   }
 }

sub runInParallel($$$@)                                                         #I Process the elements of an array in parallel using a maximum of B<$maximumNumberOfProcesses> processes. sub B<&$parallel> is forked to process each array element in parallel. The results returned by the forked copies of &$parallel are presented as a single array to sub B<&$results> which is run in series. B<@array> contains the elements to be processed. Returns the result returned by &$results.
 {my ($maximumNumberOfProcesses, $parallel, $results, @array) = @_;             # Maximum number of processes, parallel sub, results sub, array of items to process

  my $p = newProcessStarter($maximumNumberOfProcesses);                         # Process starter

  for my $s(@array)                                                             # Process each element of the array
   {$p->start(sub{&$parallel($s)});
   }

  my @r = $p->finish;
  return &$results(@r) if $results;                                             # Consolidate results if requested
  undef
 } # runInParallel

sub runInSquareRootParallel($$$@)                                               # Process the elements of an array in square root parallel using a maximum of B<$maximumNumberOfProcesses> processes. sub B<&$parallel> is forked to process each block of array elements in parallel. The results returned by the forked copies of &$parallel are presented as a single array to sub B<&$results> which is run in series. B<@array> contains the elements to be processed. Returns the result returned by &$results..
 {my ($maximumNumberOfProcesses, $parallel, $results, @array) = @_;             # Maximum number of processes, parallel sub, results sub, array of items to process

  my @s = squareArray(@array);                                                  # Square array of processes
  my $p = newProcessStarter($maximumNumberOfProcesses);                         # Process starter

  for my $row(@s)                                                               # Process each row of the square
   {$p->start(sub
     {my @r;
      for my $s(@$row)                                                          # Process each element of each row and consolidate the results
       {push @r, &$parallel($s);
       }
      [@r]
     });
   }

  my @r = deSquareArray $p->finish;
  return &$results(@r) if $results;                                             # Consolidate results
  undef
 } # runInSquareRootParallel

sub packBySize($@)                                                              # Given B<$N> buckets and a list B<@sizes> of ([size of file, name of file]...) pack the file names into buckets so that each bucket contains approximately the same number of bytes.  In general this is an NP problem.  Packing largest first into emptiest bucket produces an N**2 heuristic if the buckets are scanned linearly, or N*log(N) if a binary tree is used.  This solution is a compromise at N**3/2 which has the benefits of simple code yet good performance.  Returns ([file names ...]).
 {my ($N, @sizes) = @_;                                                         # Number of buckets, sizes
  return [map {$$_[1]} @sizes] if $N < 2;                                       # Put all the files in the first bucket unless a plurality of buckets was specified
  my $step        = int sqrt($N);                                               # Divide the buckets up into sequences of square root length
  my $sequence    = 0;                                                          # Current sequence
  my @buckets     = map {[]} 1..$N;                                             # Buckets representing the work to be done by each process
  my @bucketSizes = ((0) x $N);                                                 # Sum of sizes of files allocated to this bucket

  for my $size(sort {$$b[0] <=> $$a[0]} @sizes)                                 # Push files in descending order of size onto the smallest bucket in the current sequence
   {my $mb = $sequence++ % $step;                                               # Start of sequence we are on
    my $ms = $bucketSizes[$mb];                                                 # Smallest bucket so far in sequence

    for(my $b = $mb+$step; $b < $N; $b += $step)                                # Look through remainder of sequence
     {$ms = $bucketSizes[$mb = $b] if $bucketSizes[$b] < $ms;                   # Smallest bucket so far
     }

    $bucketSizes   [$mb] += $$size[0];                                          # Update bucket size
    push @{$buckets[$mb]},  $$size[1];                                          # Add file to bucket
   }

  @buckets                                                                      # ([file names ...]...) so that each bucket has the approximately the same number of bytes summed over the files in the bucket
 }

sub processSizesInParallelN($$$@)                                               #P Process items of known size in parallel using the specified number B<$N> processes with the process each file is assigned to depending on the size of the file so that each process is loaded with approximately the same number of bytes of data in total from the files it processes. \mEach file is processed by sub B<$parallel> and the results of processing all files is processed by B<$results> where the files are taken from B<@files>. Each B<&$parallel> receives a file from B<@files>. B<&$results> receives an array of all the results returned by B<&$parallel>.
 {my ($N, $parallel, $results, @sizes) = @_;                                    # Number of processes, Parallel sub, results sub, array of [size; item] to process by size

  return &$results() if @sizes == 0 and $results;                               # Nothing to do - report same to results sub!
  return ()      unless @sizes;                                                 # Nothing to do - really!

  return runInParallel($N, $parallel, $results // sub{@_},                      # One process per item
    map {$$_[1]} @sizes) if @sizes <= $N;

#  my @buckets     = map {[]} 1..$N;                                             # Buckets representing the work to be done by each process
#  my @bucketSizes = ((0) x $N);                                                 # Sum of sizes of files allocated to this bucket

#  for my $size(sort {$$b[0] <=> $$a[0]} @sizes)                                 # Push files in descending order of size onto the smallest bucket
#   {my $mb = 0; my $ms = $bucketSizes[0];                                       # Smallest bucket so far
#    for(keys @buckets)                                                          # Find smallest bucket - sort in place is slower
#     {$ms = $bucketSizes[$mb = $_] if $bucketSizes[$_] < $ms;                   # Smallest bucket so far
#     }
#    $bucketSizes[$mb]   += $$size[0];                                           # Update bucket size
#    push @{$buckets[$mb]}, $$size[1];                                           # Add file to bucket
#   }

  my @buckets = packBySize($N, @sizes);                                         # Pack files by size
  my $p = newProcessStarter($N);                                                # Process starter
  for my $bucket(@buckets)                                                      # Process each bucket
   {$p->start(sub                                                               # Multiverse
     {my @r;
      for my $file(@$bucket)                                                    # Process each element of each row and consolidate the results
       {push @r, &$parallel($file);
       }
      [@r]
     });
   }

  my @p = $p->finish;                                                           # Consolidate results in universe
  my @r = deSquareArray @p;

  return &$results(@r) if $results;                                             # Post process results
  @r                                                                            # Return results if no post processor
 } # processSizesInParallel

sub processSizesInParallel($$@)                                                 # Process items of known size in parallel using (8 * the number of CPUs) processes with the process each item is assigned to depending on the size of the item so that each process is loaded with approximately the same number of bytes of data in total from the items it processes. \mEach item is processed by sub B<$parallel> and the results of processing all items is processed by B<$results> where the items are taken from B<@sizes>. Each &$parallel() receives an item from @files. &$results() receives an array of all the results returned by &$parallel().
 {my ($parallel, $results, @sizes) = @_;                                        # Parallel sub, results sub, array of [size; item] to process by size
  my $N = sub                                                                   # Heuristically scale the number of cpus by the instance type
   {return  4 unless onAws;
    my $i = awsCurrentInstanceType;
    return  4 if $i =~ m(\Am)i;
    return  8 if $i =~ m(\Ar)i;
    return 16 if $i =~ m(\Ax)i;
            2
   }->();
  processSizesInParallelN(numberOfCpus($N), $parallel, $results, @sizes);       # Process in parallel
 } # processSizesInParallel

sub processFilesInParallel($$@)                                                 # Process files in parallel using (8 * the number of CPUs) processes with the process each file is assigned to depending on the size of the file so that each process is loaded with approximately the same number of bytes of data in total from the files it processes. \mEach file is processed by sub B<$parallel> and the results of processing all files is processed by B<$results> where the files are taken from B<@files>. Each B<&$parallel> receives a file from B<@files>. B<&$results> receives an array of all the results returned by B<&$parallel>.
 {my ($parallel, $results, @files) = @_;                                        # Parallel sub, results sub, array of files to process by size
  processSizesInParallel $parallel, $results, map {[fileSize($_), $_]} @files;  # Process in parallel packing files to achieve as equal as possibly sized processes
 } # processFilesInParallel

sub processJavaFilesInParallel($$@)                                             # Process java files of known size in parallel using (the number of CPUs) processes with the process each item is assigned to depending on the size of the java item so that each process is loaded with approximately the same number of bytes of data in total from the java files it processes. \mEach java item is processed by sub B<$parallel> and the results of processing all java files is processed by B<$results> where the java files are taken from B<@sizes>. Each &$parallel() receives a java item from @files. &$results() receives an array of all the results returned by &$parallel().
 {my ($parallel, $results, @files) = @_;                                        # Parallel sub, results sub, array of [size; java item] to process by size
  my @sizes = map {[fileSize($_), $_]} @files;                                  # Process in parallel packing files to achieve as equal as possibly sized processes
  processSizesInParallelN(numberOfCpus(1/2), $parallel, $results, @sizes);      # Process in parallel
 } # processJavaFilesInParallel

sub syncFromS3InParallel($$$;$$)                                                # Download from L<S3> by using "aws s3 sync --exclude '*' --include '...'" in parallel to sync collections of two or more files no greater then B<$maxSize> or single files greater than $maxSize from the B<$source> folder on L<S3> to the local folder B<$target> using the specified B<$Profile> and B<$options> - then execute the entire command again without the --exclude and --include options in series which might now run faster due to the prior downloads.
 {my ($maxSize, $source, $target, $Profile, $options) = @_;                     # The maximum collection size, the source folder on S3, the target folder locally, aws cli profile, aws cli options
                                                                                # See: /home/phil/r/z/partitionStrings.pl for standalone tests
  my ($bucket, $folder) = parseS3BucketAndFolderName($source);                  # Parse an L<s3> bucket/folder name into a bucket and a folder name removing any initial s3://.

  my $profile = $Profile ? qq( --profile $Profile) : q();                       # Add profile if specified
  $options  //= q();                                                            # Default options

  my $getCmd  = qq(aws s3 ls s3://$bucket/$folder $profile --recursive);        # Command to get the sizes of the files to download
  my $files   = qx($getCmd);                                                    # Get the sizes of the files to download
  my @files   = map {my @a = split m/\s+/, $_, 4; [@a[-1, -2]]}                 # Files and sizes
                split m/\n/, $files;
  return unless @files;                                                         # No files to download

  call sub                                                                      # Partition likely to cause a lot of memory fragmentation
   {my %partition = partitionStringsOnPrefixBySize($maxSize, map {@$_} @files); # Partition the download into collections no larger than the specified size

    processSizesInParallel(                                                     # Download folders packing by size
      sub
       {my ($P) = @_;                                                           # Path to folder to download
        return unless keys %partition > 1;                                      # Process in parallel only if there is more than one partition
        my $p = swapFilePrefix($P, $folder);                                    # Remove the folder because it will be added back by the sync command, see:
        my $c   = join ' ', map {pad($_, 32)}                                   # Download in parallel command
                  qq(aws s3 sync "s3://$bucket/$folder"), qq("$target"),
                  qq(--exclude "*" --include "$p*"),
                  $options, $profile, q(--quiet);
        #lll $c;
        xxx $c, qr(\A\s*\Z);
       },
      sub                                                                       # Now execute the original command which should require less processing because of the prior downloads in parallel
       {my $c   = join ' ', map {pad($_, 32)}                                   # Down load in series command
                  qq(aws s3 sync "s3://$bucket/$folder"), qq("$target"),
                  $options, $profile, q(--quiet);
        #lll $c;
        xxx $c, qr(\A\s*\Z);
       }, map {[$partition{$_}, $_]} sort keys %partition);
   };
 } # syncFromS3InParallel

sub syncToS3InParallel($$$;$$)                                                  # Upload to L<S3> by using "aws s3 sync --exclude '*' --include '...'" in parallel to sync collections of two or more files no greater then B<$maxSize> or single files greater than $maxSize from the B<$source> folder locally to the target folder B<$target> on L<S3> using the specified B<$Profile> and B<$options> - then execute the entire command again without the --exclude and --include options in series which might now run faster due to the prior uploads.
 {my ($maxSize, $source, $target, $Profile, $options) = @_;                     # The maximum collection size, the target folder locally, the source folder on S3, aws cli profile, aws cli options

  $target =~ s(\As3://) ();                                                     # Remove S3 prefix if present

  my $profile = $Profile ? qq( --profile $Profile) : q();                       # Add profile if specified
  $options  //= q();                                                            # Default options

  my @files   = map {[$_=>fileSize $_]}                                         # Files and sizes
                searchDirectoryTreesForMatchingFiles($source);
  return unless @files;                                                         # No files to download

  $$_[0] = swapFilePrefix($$_[0], $source) for @files;                          # Remove folder prefix

  call sub                                                                      # Partition likely to cause a lot of memory fragmentation
   {my %partition = partitionStringsOnPrefixBySize($maxSize, map {@$_} @files); # Partition the download into collections no larger than the specified size

    processSizesInParallel(                                                     # Download folders packing by size
      sub
       {my ($p) = @_;                                                           # Path to folder to download
        return unless keys %partition > 1;                                      # Process in parallel only if there is more than one partition
        my $c   = join ' ', map {pad($_, 32)}
                  qq(aws s3 sync "$source"), qq("s3://$target"),
                  qq(--exclude "*" --include "$p*"),
                  $options, $profile, q(--quiet);
        #lll $c;
        xxx $c, qr(\A\s*\Z);
       },
      sub                                                                       # Now execute the original command which should require less processing because of the prior downloads in parallel
       {my $c   = join ' ', map {pad($_, 32)}
                  qq(aws s3 sync "$source"), qq("s3://$target"),
                  $options, $profile, q(--quiet);
        #lll $c;
        xxx $c, qr(\A\s*\Z);
       }, map {[$partition{$_}, $_]} sort keys %partition);
   };
 } # syncToS3InParallel

sub childPids($)                                                                # Recursively find the pids of all the sub processes of a B<$process> and all their sub processes and so on returning the specified pid and all its child pids as a list.
 {my ($p) = @_;                                                                 # Process
  confirmHasCommandLineCommand(q(pstree));                                      # Use pstree
  qx(pstree -p $p) =~ m(\((\d+)\))g;                                            # Extract the pids
 }

sub newServiceIncarnation($;$)                                                  # Create a new service incarnation to record the start up of a new instance of a service and return the description as a L<Data::Exchange::Service Definition hash|/Data::Exchange::Service Definition>.
 {my ($service, $file) = @_;                                                    # Service name, optional details file
  $file ||= fpe($ENV{HOME},                                                     # File to log service details in
    qw(.config com.appaapps services), $service, q(txt));                       # Service specification file
  my $t = genHash(q(Data::Exchange::Service),                                   # Service details.
    service=> $service,                                                         # The name of the service.
    start  => int(time) + (-e $file ? 1 : 0),                                   # The time this service was started time plus a minor hack to simplify testing.
    file   => $file,                                                            # The file in which the service start details is being recorded.
   );
  dumpFile($file, $t);                                                          # Write details
  $t                                                                            # Return service details
 }

sub Data::Exchange::Service::check($$)                                          # Check that we are the current incarnation of the named service with details obtained from L<newServiceIncarnation|/newServiceIncarnation>. If the optional B<$continue> flag has been set then return the service details if this is the current service incarnation else B<undef>. Otherwise if the B<$continue> flag is false confess unless this is the current service incarnation thus bringing the earlier version of this service to an abrupt end.
 {my ($service, $continue) = @_;                                                # Current service details, return result if B<$continue> is true else confess if the service has been replaced
  my $t = evalFile($service->file);                                             # Latest service details
  return $t if $t->start   == $service->start   and                             # Check service details match
               $t->service eq $service->service and
               $t->file    eq $t->file;
  confess $t->service. " replaced by a newer version\n" unless $continue;       # Replaced by a newer incarnation
  undef                                                                         # Not the current incarnation but continue specified
 }

#D1 Conversions                                                                 # Perform various conversions from STDIN to STDOUT

sub convertPerlToJavaScript(;$$)                                                # Convert Perl to Javascript
 {my ($in, $out) = @_;                                                          # Input file name or STDIN if undef, output file name or STDOUT if undefined
  my @lines = $in ? readFile($in) : readStdIn;                                  # Read file or STDIN

  for my $i(keys @lines)                                                        # Parameters
   {if ($lines[$i] =~ m(\Asub\s*(\w+)\s*\((.*?)\)(.*)\Z)i)
     {my ($sub, $parms, $comment) = ($1, $2, $3);
      my $j = $i + 1;

      if ($lines[$j] =~ m(\A(\s*\{)my\s*\((.*?)\)\s*=\s*\@_)i)
       {my ($lead, $my) = ($1, $2);
        $my =~ s(\$) ()gs;
        $lines[$i] = qq(function $sub($my)$comment\n$lead);
        $lines[$j] = '';
       }
     }
   }

  for my $i(keys @lines)                                                        # if(my
   {if ($lines[$i] =~ m(\A(\s*)if\s*\(my\s*\$(\w+))i)
     {my ($lead, $var) = ($1, $2);
      my $l = $lines[$i];
         $l =~ s(if\s*\(my) ();
         $l =~ s(\)\s*\Z)   ();

      $lines[$i] = qq($l\nif ($var)\n)
     }
   }

  for my $i(keys @lines)                                                        # if(defined $x)
   {if ($lines[$i] =~ m(\A(\s*)if\s*\(defined\s*\$(\w+)\s*\)(.*)\Z)i)
     {my ($lead, $var, $trail) = ($1, $2);
      $lines[$i] = qq(${lead}if ($var !== undefined)\n)
     }
   }

  for my $i(keys @lines)                                                        # for my $var(
   {if ($lines[$i] =~ m(\A(\s*)for\s*my\s*(\w+)(.*)\Z)i)
     {my ($lead, $var, $rest) = ($1, $2, $3);
      $lines[$i] = qq/${lead}for(const $var of $rest/
     }
   }

  if (1)                                                                        # In place changes
   {for(@lines)
     {s(#) (//)gs;
      s(\Asub ) (function)gs;
      s(my\s*@(\w+)\s*;)        (const $1 = new Array())gs;
      s(my\s*%(\w+)\s*;)        (const $1 = new Map())gs;
      s(my \$)                  (const )gs;
      s(->[@%]\*)               ()gs;
      s(\{(\w+)\})              (.$1)gs;                                        # Hash constant lookup
      s(\{\$(\w+)\})            (\[$1\])gs;                                     # Hash variable lookup
      s(\s+(or)\s+)             ( || )gs;                                       # Or
      s(\s+(and)\s+)            ( && )gs;                                       # And
      s(sort keys\s*%\$(\w+))   (Object.keys($1).sort())gs;                     # sort keys %$t
      s(keys\s*%\$(\w+))        (Object.keys($1))gs;                            # keys %$t
      s(\Ause)                  (require);                                      # use ...
      s(\A(\s*)ok\s+(.*);\s*\Z) (${1}assert($2)\n);                             # ok
      s(\A(\s*)is_deeply)       (${1}assert.deepEquals\();                      # is_deeply
      s(qq\((.*?)\))            (`$1`)gs;                                       # double quoted strings
      s(q\((.*?)\))             ('$1')gs;                                       # single quoted strings
      s([\$\@&#])               ()gs;                                           # Sigils
     }
   }

  if (1)                                                                        # ->
   {for(@lines)
     {s(->\[) ([)gs;
      s(->)   (\.)gs;
     }
   }

  if (1)                                                                        # Specifics
   {for(@lines)
     {s(\.\.)  (.)gs;
      s(\$ssv) (ditaJs.ssv)gs;
     }
   }

  my @comments = split /\n/, join '', @lines;                                   # Reparse
  if (1)                                                                        # Comment position
   {for my $i(keys @comments)
     {next if $comments[$i] =~ m(\A//);
      if ($comments[$i] =~ m(\A(.*)(//.*)\Z))
       {my ($code, $comment) = ($1, $2);
        if (length($code) > 80)
         {my $a = substr($code, 0, 80);
          my $b = substr($code, 80);
             $b =~ s(\s+\Z) ();
          $code = qq($a$b);
         }
        elsif (length($code) < 80)
         {$code = substr($code.(' ' x 80), 0, 80);
         }
        $comments[$i] = qq($code$comment)
       }
     }
   }

  my $text = join "\n", @comments, '';
     $text =~ s((\n=pod\n.*?\n=cut\n)) (`$1`)gs;                                # Pod as comment string

  $out ? owf($out, $text) : (say STDOUT $text)                                  # Write results to file or STDOUT
 } # convertPerlToJavaScript

#D1 Documentation                                                               # Extract, format and update documentation for a perl module.

sub parseDitaRef($;$$)                                                          # Parse a dita reference B<$ref> into its components (file name, topic id, id) . Optionally supply a base file name B<$File>> to make the the file component absolute and/or a a default the topic id B<$TopicId> to use if the topic id is not present in the reference.
 {my ($ref, $File, $TopicId) = @_;                                              # Reference to parse, default absolute file, default topic id
  return (q()) x 3 unless $ref and $ref =~ m(\S)s;

  my ($file, $rest)  = split /#/, $ref, 2;

  $file    = $File && $file ? sumAbsAndRel($File, $file) : $File || $file||q(); # Full file path if possible

  if (!$rest)                                                                   # file
   {return ($file, q(), q())
   }

  if ($rest !~ m(/)s)                                                           # file#id
   {return ($file, q(), $rest)
   }

  if ($rest =~ m(\A\./)s)                                                       # file#./id
   {return ($file, $TopicId || q(), $rest =~ s(\A\./) ()r)
   }

  my ($topicId, $id) = split m(/), $rest, 2;
  $topicId = $topicId || $TopicId || q();
  $topicId = $TopicId if $TopicId and $topicId =~ m(\A(\s*|\.)\Z);
  $id    ||= q();

  ($file, $topicId, $id)
 }

sub parseXmlDocType($)                                                          # Parse an L<xml> DOCTYPE and return a hash indicating its components
 {my ($string) = @_;                                                            # String containing a DOCTYPE

  if ($string =~ m(<!DOCTYPE\s+(\w+)\s+PUBLIC\s+"([^"]+)"\s+"([^"]+)")s)        # Parse DOCTYPE PUBLIC
   {return genHash(q(DocType),
      root     => $1,
      public   => 1,
      publicId => $2,
      localDtd => $3);
   }
  elsif ($string =~ m(<!DOCTYPE\s+(\w+)\s+SYSTEM\s+([a-z0-9.]+)?)s)             # Parse DOCTYPE SYSTEM
   {return genHash(q(DocType),
      root     => $1,
      public   => 0,
      localDtd => $2);
   }
  undef
 }

sub reportSettings($;$)                                                         # Report the current values of parameterless subs.
 {my ($sourceFile, $reportFile) = @_;                                           # Source file, optional report file
  warn "Deprecated, please use reportAttributeSettings instead";
  my $s = readFile($sourceFile);

  my %s;
  for my $l(split /\n/, $s)                                                     # Find the attribute subs
   {if ($l =~ m(\Asub\s*(\w+)\s*\{.*?#\s+(.*)\Z))
     {$s{$1} = $2;
     }
   }

  my @r;
  for my $s(sort keys %s)                                                       # Evaluate each sub
   {my ($package, $filename, $line) = caller;                                   # Callers context
    my $v = eval q(&).$package.q(::).$s;                                        # Current value in callers context
    my $r = $@ // '';                                                           # Failure description
    push @r, [$s, $v, $r, $s{$s}];                                              # Table entry of sub name, sub value, reason why there is no value, comment
   }

  formatTable(\@r, <<END,                                                       # Format table
Attribute The name of the program attribute
Value     The current value of the program attribute
END
    head      => qq(Found NNNN parameters on DDDD),
    title     => qq(Attributes in program: $sourceFile),
    summarize => 1,
    $reportFile ? (file=>$reportFile) : ());
 }

sub reportAttributes($)                                                         # Report the attributes present in a B<$sourceFile>
 {my ($sourceFile) = @_;                                                        # Source file
  my $s = readFile($sourceFile);
  my %s;
  for my $l(split /\n/, $s)                                                     # Find the attribute subs
   {if ($l =~ m(\Asub\s*(\w+)\s*\{.*?#\w*\s+(.*)\Z))
     {$s{$1} = $2;
     }
   }
  \%s
 }

sub reportAttributeSettings(;$)                                                 # Report the current values of the attribute methods in the calling file and optionally write the report to B<$reportFile>. Return the text of the report.
 {my ($reportFile) = @_;                                                        # Optional report file
  my ($package, $sourceFile, $line) = caller;                                   # Callers context

  my $a = reportAttributes($sourceFile);                                        # Attribute methods in calling file

  my @r;
  for my $s(sort keys %$a)                                                      # Evaluate each sub
   {my $v = eval q(&).$package.q(::).$s;                                        # Current value in callers context
    my $r = $@ // '';                                                           # Failure description
    push @r, [$s, $v, $r, $$a{$s}];                                             # Table entry of sub name, sub value, reason why there is no value, comment
   }

  formatTable(\@r, <<END,                                                       # Format table
Attribute The name of the program attribute
Value     The current value of the program attribute
END
    head      => qq(Found NNNN parameters on DDDD),
    title     => qq(Attributes in program: $sourceFile),
    summarize => 1,
    $reportFile ? (file=>$reportFile) : ());

  \@r
 }

sub reportReplacableMethods($)                                                  # Report the replaceable methods marked with #r in a B<$sourceFile>
 {my ($sourceFile) = @_;                                                        # Source file
  my $s = readFile($sourceFile);
  my %s;
  for my $l(split /\n/, $s)                                                     # Find the attribute subs
   {if ($l =~ m(\Asub\s*(\w+).*?#\w*r\w*\s+(.*)\Z))
     {$s{$1} = $2;
     }
   }
  \%s
 }

sub reportExportableMethods($)                                                  # Report the exportable methods marked with #e in a B<$sourceFile>
 {my ($sourceFile) = @_;                                                        # Source file
  my $s = readFile($sourceFile);
  my %s;
  for my $l(split /\n/, $s)                                                     # Find the attribute subs
   {if ($l =~ m(\Asub\s*(\w+).*?#\w*e\w*\s+(.*)\Z))
     {$s{$1} = $2;
     }
   }
  \%s
 }

sub htmlToc($@)                                                                 # Generate a table of contents for some html.
 {my ($replace, $html) = @_;                                                    # Sub-string within the html to be replaced with the toc, string of html
  my @toc;
  my %toc;

  for(split /\n/, $html)
   {next unless /\A\s*<h(\d)\s+id="(.+?)"\s*>(.+?)<\/h\d>\s*\Z/;
    confess "Duplicate id $2\n" if $toc{$2}++;
    push @toc, [$1, $2, $3];
   }

  my @h;
  for my $head(keys @toc)
   {my ($level, $id, $title) = @{$toc[$head]};
    my $spacer = '&nbsp;' x (4*$level);
    push @h, <<END if $level < 2;
<tr><td>&nbsp;
END
    my $n = $head+1;
    push @h, <<END;
<tr><td align=right>$n<td>$spacer<a href="#$id">$title</a>
END
   }

  my $h = <<END.join "\n", @h, <<END;
<table cellspacing=10 border=0>
END
</table>
END

  $html =~ s($replace) ($h)gsr;
 }

sub wellKnownUrls                                                               #P Short names for some well known urls
 {genHash(q(Short_Names_For_Well_Known_Urls),                                   #  Short names for some well known urls
    ab              => [q(Android Build),                                       "https://metacpan.org/pod/Android::Build"                                                                                         ], #
    alva            => [q(Rio Alva),                                            "https://duckduckgo.com/?t=canonical&q=rio+alva&iax=images&ia=images"                                                             ], #
    ami             => [q(Amazon Machine Image),                                "https://en.wikipedia.org/wiki/Amazon_Machine_Image"                                                                              ], #
    apache          => [q(Apache Web Server),                                   "https://en.wikipedia.org/wiki/Apache_HTTP_Server"                                                                                ], #
    aas             => [q(Amazon App Store),                                    "https://www.amazon.com/s?k=appaapps"                                                                                             ], #
    appaapps        => [q(www.appaapps.com),                                    "http://www.appaaps.com"                                                                                                          ], #
    aramco          => [q(Saudi Aramco),                                        "https://en.wikipedia.org/wiki/Saudi_Aramco"                                                                                      ], #
    arena           => [q(arena),                                               "https://en.wikipedia.org/wiki/Region-based_memory_management"                                                                    ], #
    ascii           => [q(Ascii),                                               "https://en.wikipedia.org/wiki/ASCII"                                                                                             ], #
    awsami          => [q(Amazon Web Services - Amazon Machine Image),          "https://en.wikipedia.org/wiki/Amazon_Machine_Image"                                                                              ], #
    awscli          => [q(Amazon Web Services Command Line Interface),          "https://aws.amazon.com/cli/"                                                                                                     ], #
    awsforecast     => [q(Amazon Web Services Forecast),                        "https://eu-west-1.console.aws.amazon.com/forecast"                                                                               ], #
    aws             => [q(Amazon Web Services),                                 "http://aws.amazon.com"                                                                                                           ], #
    avx             => [q(Advanced Vector Extensions),                          "https://en.wikipedia.org/wiki/AVX-512"                                                                                           ], #
    avx512          => [q(Advanced Vector Extensions),                          "https://en.wikipedia.org/wiki/AVX-512"                                                                                           ], #
    azure           => [q(Azure),                                               "https://en.wikipedia.org/wiki/Microsoft_Azure"                                                                                   ], #
    backend         => [q(back end),                                            "https://en.wikipedia.org/wiki/Front_end_and_back_end"                                                                            ], #
    bash            => [q(Bash),                                                "https://en.wikipedia.org/wiki/Bash_(Unix_shell)"                                                                                 ], #
    ban             => [q(Briana Ashley Nevarez),                               "https://www.linkedin.com/in/briana-nevarez-b66b621b0/"                                                                           ], #
    bandwidth       => [q(Bandwidth),                                           "https://en.wikipedia.org/wiki/Bandwidth_(computing)"                                                                             ], #
    binarysearch    => [q(Binary Search),                                       "https://metacpan.org/release/Binary-Heap-Search"                                                                                 ], #
    bitterend       => [q(Bitter End),                                          "https://en.wikipedia.org/wiki/Knot#Bitter_end"                                                                                   ], #
    blob            => [q(blob),                                                "https://en.wikipedia.org/wiki/Binary_large_object"                                                                               ], #
    boson           => [q(Boson),                                               "https://en.wikipedia.org/wiki/Boson"                                                                                             ], #
    browser         => [q(web browser),                                         "https://en.wikipedia.org/wiki/Web_browser"                                                                                       ], #
    bulktreeg       => [q(Bulk Tree),                                           "https://github.com/philiprbrenan/TreeBulk"                                                                                       ], #
    button          => [q(Button),                                              "https://en.wikipedia.org/wiki/Button_(computing)"                                                                                ], #
    code            => [q(code),                                                "https://en.wikipedia.org/wiki/Computer_program"                                                                                  ], #
    certbot         => [q(Certbot),                                             "https://certbot.eff.org/lets-encrypt/ubuntufocal-apache"                                                                         ], #
    cgi             => [q(Common Gateway Interface),                            "https://en.wikipedia.org/wiki/Common_Gateway_Interface"                                                                          ], #
    chmod           => [q(chmod),                                               "https://linux.die.net/man/1/chmod"                                                                                               ], #
    chown           => [q(chown),                                               "https://linux.die.net/man/1/chown"                                                                                               ], #
    cicero          => [q("The sinews of war are an infinite supply of money"), "https://en.wikipedia.org/wiki/Cicero#Legacy"                                                                                     ], #
    cicd            => [q(CI/CD),                                               "https://en.wikipedia.org/wiki/Continuous_integration"                                                                            ], #
    cl              => [q(command line),                                        "https://en.wikipedia.org/wiki/Command-line_interface"                                                                            ], #
    cm              => [q(Codementor),                                          'https://www.codementor.io/@philiprbrenan'                                                                                        ], #
    codementor      => [q(Codementor),                                          'https://www.codementor.io/@philiprbrenan'                                                                                        ], #
    commandline     => [q(command line),                                        "https://en.wikipedia.org/wiki/Command-line_interface"                                                                            ], #
    comment         => [q(comment),                                             "https://en.wikipedia.org/wiki/Comment_(computer_programming)"                                                                    ], #
    computer        => [q(computer),                                            "https://en.wikipedia.org/wiki/Computer"                                                                                          ], #
    concept         => [q(concept),                                             "http://docs.oasis-open.org/dita/dita/v1.3/errata02/os/complete/part3-all-inclusive/langRef/technicalContent/concept.html#concept"], #
    confess         => [q(confess),                                             "http://perldoc.perl.org/Carp.html#SYNOPSIS/"                                                                                     ], #
    conref          => [q(conref),                                              "http://docs.oasis-open.org/dita/dita/v1.3/errata02/os/complete/part3-all-inclusive/archSpec/base/conref.html#conref"             ], #
    cookie          => [q(cookie),                                              "https://en.wikipedia.org/wiki/Cookie"                                                                                            ], #
    corpus          => [q(corpus),                                              "https://en.wikipedia.org/wiki/Text_corpus"                                                                                       ], #
    coverage        => [q(coverage),                                            "https://en.wikipedia.org/wiki/Code_coverage"                                                                                     ], #
    cpan            => [q(CPAN),                                                "https://metacpan.org/author/PRBRENAN"                                                                                            ], #
    cpu             => [q(CPU),                                                 "https://en.wikipedia.org/wiki/Central_processing_unit"                                                                           ], #
    c               => [q(The C Programming Language),                          "https://1lib.eu/book/633119/db5c78"                                                                                              ], #
    co2             => [q(Carbon Dioxide),                                      "https://en.wikipedia.org/wiki/Carbon_dioxide"                                                                                    ], #
    css             => [q(Cascading Style Sheets),                              "https://en.wikipedia.org/wiki/CSS"                                                                                               ], #
    csv             => [q(csv),                                                 "https://en.wikipedia.org/wiki/Comma-separated_values"                                                                            ], #
    cv              => [q(Curriculum Vitae),                                    "https://en.wikipedia.org/wiki/Curriculum_vitae"                                                                                  ], #
    curl            => [q(curl),                                                "https://linux.die.net/man/1/curl"                                                                                                ], #
    dataStructure   => [q(data structure),                                      "https://en.wikipedia.org/wiki/Data_structure"                                                                                    ], #
    db2             => [q(DB2),                                                 "https://en.wikipedia.org/wiki/IBM_Db2_Family"                                                                                    ], #
    dbi             => [q(DBI),                                                 "https://dbi.perl.org/"                                                                                                           ], #
    dd              => [q(Daily Diary),                                         "http://philiprbrenan.appaapps.com.s3-website-eu-west-1.amazonaws.com/index.html"                                                 ], #
    ddt             => [q(Data::Table::Text),                                   "https://metacpan.org/pod/Data::Table::Text"                                                                                      ], #
    dependencies    => [q(dependencies),                                        "https://en.wikipedia.org/wiki/Coupling_(computer_programming)"                                                                   ], #
    dexl            => [q(Data::Edit::Xml::Lint),                               "https://metacpan.org/release/Data-Edit-Xml-Lint"                                                                                 ], #
    dex             => [q(Data::Edit::Xml),                                     "https://metacpan.org/pod/Data::Edit::Xml"                                                                                        ], #
    dexr            => [q(Data::Edit::Xml::Reuse),                              "https://metacpan.org/release/Data-Edit-Xml-Reuse"                                                                                ], #
    dexx            => [q(Data::Edit::Xml::Xref),                               "https://metacpan.org/release/Data-Edit-Xml-Xref"                                                                                 ], #
    dhahran         => [q(Dhahran),                                             "https://en.wikipedia.org/wiki/Dhahran"                                                                                           ], #
    ddg             => [q(DuckDuckGo),                                          "https://www.duckduckgo.com"                                                                                                      ], #
    dfa             => [q(DFA),                                                 "https://metacpan.org/pod/Data::DFA"                                                                                              ], #
    die             => [q(die),                                                 "http://perldoc.perl.org/functions/die.html"                                                                                      ], #
    diff            => [q(diff),                                                "https://en.wikipedia.org/wiki/Diff"                                                                                              ], #
    diospiros       => [q(diospiros),                                           "https://en.wikipedia.org/wiki/Persimmon"                                                                                         ], #
    diskdrive       => [q(disk drive),                                          "https://en.wikipedia.org/wiki/Solid-state_drive"                                                                                 ], #
    ditaot          => [q(DITA Open ToolKit),                                   "http://www.dita-ot.org/download"                                                                                                 ], #
    dita            => [q(Dita),                                                "http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html"                            ], #
    divtag          => [q(Div tag),                                             "https://en.wikipedia.org/wiki/Span_and_div"                                                                                      ], #
    dns             => [q(Domain Name System),                                  "https://en.wikipedia.org/wiki/Domain_Name_System"                                                                                ], #
    docbook         => [q(DocBook),                                             "https://tdg.docbook.org/tdg/5.1/"                                                                                                ], #
    documentation   => [q(documentation),                                       "https://en.wikipedia.org/wiki/Software_documentation"                                                                            ], #
    domain          => [q(domain name),                                         "https://en.wikipedia.org/wiki/Domain_name"                                                                                       ], #
    dom             => [q(Document Object Model),                               "https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model"                                                          ], #
    dtd             => [q(DTD),                                                 "https://en.wikipedia.org/wiki/Document_type_definition"                                                                          ], #
    dtt             => [q(Data::Table::Text),                                   "https://metacpan.org/pod/Data::Table::Text"                                                                                      ], #
    dttg            => [q(Data::Table::Text),                                   "https://github.com/philiprbrenan/DataTableText"                                                                                  ], #
    ec2Console      => [q(EC2 Console),                                         "https://us-east-1.console.aws.amazon.com/ec2/"                                                                                   ], #
    ec2             => [q(EC2),                                                 "https://aws.amazon.com/ec2/"                                                                                                     ], #
    eff             => [q(The Electronic Frontier Foundation),                  "https://en.wikipedia.org/wiki/Electronic_Frontier_Foundation"                                                                    ], #
    electrons       => [q(electrons),                                           "https://en.wikipedia.org/wiki/Electron"                                                                                          ], #
    english         => [q(English),                                             "https://en.wikipedia.org/wiki/English_language"                                                                                  ], #
    eval            => [q(eval),                                                "http://perldoc.perl.org/functions/eval.html"                                                                                     ], #
    extensions      => [q(file name extensions),                                "https://en.wikipedia.org/wiki/List_of_filename_extensions"                                                                       ], #
    fail            => [q(fail),                                                "https://1lib.eu/book/2468851/544b50"                                                                                             ], #
    file            => [q(file),                                                "https://en.wikipedia.org/wiki/Computer_file"                                                                                     ], #
    fileutility     => [q(File  utility),                                       "https://www.man7.org/linux/man-pages/man1/file.1.html"                                                                           ], #
    find            => [q(find),                                                "https://en.wikipedia.org/wiki/Find_(Unix)"                                                                                       ], #
    foehn           => [q(Foehn),                                               "https://en.wikipedia.org/wiki/Foehn_wind"                                                                                        ], #
    folder          => [q(folder),                                              "https://en.wikipedia.org/wiki/File_folder"                                                                                       ], #
    fork            => [q(fork),                                                "https://en.wikipedia.org/wiki/Fork_(system_call)"                                                                                ], #
    frontend        => [q(front end),                                           "https://en.wikipedia.org/wiki/Front_end_and_back_end"                                                                            ], #
    fsf             => [q(Free Software Foundation),                            "https://www.fsf.org/"                                                                                                            ], #
    fusion          => [q(fusion),                                              "https://en.wikipedia.org/wiki/Nuclear_fusion"                                                                                    ], #
    future          => [q(future),                                              "https://en.wikipedia.org/wiki/Future"                                                                                            ], #
    gbstandard      => [q(GB Standard),                                         "http://metacpan.org/pod/Dita::GB::Standard"                                                                                      ], #
    gdpr            => [q(European Directive on Data Protection),               "https://gdpr.eu"                                                                                                                 ], #
    geany           => [q(Geany),                                               "https://www.geany.org"                                                                                                           ], #
    ghc             => [q(Github Automation),                                   "https://metacpan.org/release/GitHub-Crud"                                                                                        ], #
    ghe             => [q(Github Edit)      ,                                   "https://github.com/ricksnp/github-editor"                                                                                        ], #
    gigabit         => [q(gigabit),                                             "https://en.wikipedia.org/wiki/Gigabit_Ethernet"                                                                                  ], #
    gigabyte        => [q(gigabyte),                                            "https://en.wikipedia.org/wiki/Gigabyte"                                                                                          ], #
    gitHubAction    => [q(GitHub Action),                                       "https://docs.github.com/en/free-pro-team\@latest/actions/quickstart"                                                             ], #
    githubaction    => [q(GitHub Action),                                       "https://docs.github.com/en/free-pro-team\@latest/actions/quickstart"                                                             ], #
    gitHubCrud      => [q(GitHub::Crud),                                        "https://metacpan.org/pod/GitHub::Crud"                                                                                           ], #
    gitHub          => [q(GitHub),                                              "https://github.com/philiprbrenan"                                                                                                ], #
    github          => [q(GitHub),                                              "https://github.com/philiprbrenan"                                                                                                ], #
    gitHubDP        => [q(GitHub Developer Program),                            "https://github.com/philiprbrenan"                                                                                                ], #
    githubdp        => [q(GitHub Developer Program),                            "https://github.com/philiprbrenan"                                                                                                ], #
    gmt             => [q(Greenwich Mean Time),                                 "https://en.wikipedia.org/wiki/Greenwich_Mean_Time"                                                                               ], #
    gnufdl          => [q(GNU Free Documentation License),                      "https://en.wikipedia.org/wiki/Wikipedia:Text_of_the_GNU_Free_Documentation_License"                                              ], #
    gowest          => [q("Go West young man"),                                 "https://en.wikipedia.org/wiki/Go_West,_young_man"                                                                                ], #
    grep            => [q(grep),                                                "https://en.wikipedia.org/wiki/Grep"                                                                                              ], #
    guid            => [q(guid),                                                "https://en.wikipedia.org/wiki/Universally_unique_identifier"                                                                     ], #
    gui             => [q(graphical user interface),                            "https://en.wikipedia.org/wiki/Graphical_user_interface"                                                                          ], #
    gunzip          => [q(gunzip),                                              "https://en.wikipedia.org/wiki/Gunzip"                                                                                            ], #
    gzip            => [q(gzip),                                                "https://en.wikipedia.org/wiki/Gzip"                                                                                              ], #
    hacker          => [q(hacker),                                              "https://1lib.eu/book/643342/813ee7"                                                                                              ], #
    heapsLaw        => [q(Heap's Law),                                          "https://en.wikipedia.org/wiki/Heaps%27_law"                                                                                      ], #
    help            => [q(help),                                                "https://en.wikipedia.org/wiki/Online_help"                                                                                       ], #
    hexadecimal     => [q(hexadecimal),                                         "https://en.wikipedia.org/wiki/Hexadecimal"                                                                                       ], #
    hipaa           => [q(HIPAA),                                               "https://en.wikipedia.org/wiki/Health_Insurance_Portability_and_Accountability_Act"                                               ], #
    hpe             => [q(Hewlett Packard Enterprise),                          "https://www.hpe.com/us/en/home.html"                                                                                             ], #
    html            => [q(HTML),                                                "https://en.wikipedia.org/wiki/HTML"                                                                                              ], #
    http            => [q(HTTP),                                                "https://en.wikipedia.org/wiki/HTTP"                                                                                              ], #
    https           => [q(HTTPS),                                               "https://en.wikipedia.org/wiki/HTTPS"                                                                                             ], #
    htmltable       => [q(html table),                                          "https://www.w3.org/TR/html52/tabular-data.html#the-table-element"                                                                ], #
    hxnormalize     => [q(hxnormalize),                                         "https://www.w3.org/Tools/HTML-XML-utils/man1/hxnormalize.html"                                                                   ], #
    ibm             => [q(IBM),                                                 "https://en.wikipedia.org/wiki/IBM"                                                                                               ], #
    iconv           => [q(iconv),                                               "https://linux.die.net/man/1/iconv"                                                                                               ], #
    ide             => [q(Integrated Development Environment),                  "https://en.wikipedia.org/wiki/Integrated_development_environment"                                                                ], #
    ietf            => [q(Internet Engineering Task Force),                     "https://en.wikipedia.org/wiki/Internet_Engineering_Task_Force"                                                                   ], #
    imagemagick     => [q(Imagemagick),                                         "https://www.imagemagick.org/script/index.php"                                                                                    ], #
    infix           => [q(infix),                                               "https://en.wikipedia.org/wiki/Infix_notation"                                                                                    ], #
    install         => [q(install),                                             "https://en.wikipedia.org/wiki/Installation_(computer_programs)"                                                                  ], #
    intelsde        => [q(Intel Software Development Emulator),                 "https://software.intel.com/content/www/us/en/develop/articles/intel-software-development-emulator.html"                          ], #
    internet        => [q(Internet),                                            "https://en.wikipedia.org/wiki/Internet"                                                                                          ], #
    ip6             => [q(IPv6 address),                                        "https://en.wikipedia.org/wiki/IPv6"                                                                                              ], #
    ip              => [q(IP address),                                          "https://en.wikipedia.org/wiki/IP_address"                                                                                        ], #
    ipaddress       => [q(IP address),                                          "https://en.wikipedia.org/wiki/IP_address"                                                                                        ], #
    java            => [q(Java),                                                "https://en.wikipedia.org/wiki/Java_(programming_language)"                                                                       ], #
    jet             => [q(Joint European Torus),                                "https://en.wikipedia.org/wiki/Joint_European_Torus"                                                                              ], #
    jetni           => [q(Physics design calculations for the JET neutral injectors),"https://www.sciencedirect.com/science/article/pii/B978008025697950052X"                                                     ], #
    javascript      => [q(JavaScript),                                          "https://en.wikipedia.org/wiki/JavaScript"                                                                                        ], #
    jpg             => [q(JPG),                                                 "https://en.wikipedia.org/wiki/JPEG"                                                                                              ], #
    json            => [q(Json),                                                "https://en.wikipedia.org/wiki/JSON"                                                                                              ], #
    keyboard        => [q(keyboard),                                            "https://en.wikipedia.org/wiki/Computer_keyboard"                                                                                 ], #
    killarney       => [q(Killarney),                                           "https://en.wikipedia.org/wiki/Killarney"                                                                                         ], #
    kubuntu         => [q(Kubuntu),                                             "https://kubuntu.org/"                                                                                                            ], #
    laser           => [q(laser),                                               "https://en.wikipedia.org/wiki/Laser"                                                                                             ], #
    learningCurve   => [q(learning curve),                                      "https://en.wikipedia.org/wiki/Learning_curve"                                                                                    ], #
    libpq           => [q(libpq),                                               "https://www.postgresql.org/docs/13/libpq.html"                                                                                   ], #
    libreoffice     => [q(LibreOffice),                                         "https://www.libreoffice.org/"                                                                                                    ], #
    lint            => [q(lint),                                                "http://xmlsoft.org/xmllint.html"                                                                                                 ], #
    linting         => [q(linting),                                             "https://en.wikipedia.org/wiki/Lint_(software)"                                                                                   ], #
    linux           => [q(Linux),                                               "https://en.wikipedia.org/wiki/Linux"                                                                                             ], #
    liseMeitner     => [q(Lise Meitner),                                        "https://en.wikipedia.org/wiki/Lise_Meitner"                                                                                      ], #
    list            => [q(list),                                                "https://en.wikipedia.org/wiki/Linked_list"                                                                                       ], #
    log             => [q(log),                                                 "https://en.wikipedia.org/wiki/Log_file"                                                                                          ], #
    lunchclub       => [q(LunchClub),                                           "https://lunchclub.com/?invite_code=philipb4"                                                                                     ], #
    lvaluemethod    => [q(lvalue method),                                       "http://perldoc.perl.org/perlsub.html#Lvalue-subroutines"                                                                         ], #
    maze            => [q(Maze),                                                "https://github.com/philiprbrenan/maze"                                                                                           ], #
    md5             => [q(MD5 sum),                                             "https://en.wikipedia.org/wiki/MD5"                                                                                               ], #
    md              => [q(Mark Down),                                           "https://en.wikipedia.org/wiki/Markdown"                                                                                          ], #
    mdnfetch        => [q(the Javascript Fetch API),                            "https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API"                                                                      ], #
    mideast         => [q(Middle East),                                         "https://en.wikipedia.org/wiki/Middle_East"                                                                                       ], #
    meme            => [q(Meme),                                                "https://en.wikipedia.org/wiki/Meme"                                                                                              ], #
    memory          => [q(memory),                                              "https://en.wikipedia.org/wiki/Computer_memory"                                                                                   ], #
    mentor          => [q(mentor),                                              "https://en.wikipedia.org/wiki/Mentorship"                                                                                        ], #
    metadata        => [q(metadata),                                            "https://en.wikipedia.org/wiki/Metadata"                                                                                          ], #
    mfa             => [q(Multi-factor authentication),                         "https://en.wikipedia.org/wiki/Multi-factor_authentication"                                                                       ], #
    minimalism      => [q(minimalism),                                          "https://en.wikipedia.org/wiki/Minimalism_(computing)"                                                                            ], #
    mod_shib        => [q(mod_shib),                                            "https://wiki.shibboleth.net/confluence/display/SP3/Apache"                                                                       ], #
    module          => [q(module),                                              "https://en.wikipedia.org/wiki/Modular_programming"                                                                               ], #
    mopc            => [q(mop-c),                                               "https://metacpan.org/pod/Preprocess::Ops"                                                                                        ], #
    mvp             => [q(Minimal Viable Product),                              "https://en.wikipedia.org/wiki/Minimum_viable_product"                                                                            ], #
    murphyslaw      => [q(Murphy's Law),                                        "https://en.wikipedia.org/wiki/Murphy%27s_law"                                                                                    ], #
    mysqlMan        => [q(MySql manual),                                        "https://dev.mysql.com/doc/refman/8.0/en/"                                                                                        ], #
    mysql           => [q(MySql),                                               "https://en.wikipedia.org/wiki/MySQL"                                                                                             ], #
    nasm            => [q(nasm),                                                "https://github.com/netwide-assembler/nasm"                                                                                       ], #
    nasmx86         => [q(NasmX86),                                             "https://github.com/philiprbrenan/NasmX86"                                                                                        ], #
    nfa             => [q(NFA),                                                 "https://metacpan.org/pod/Data::NFA"                                                                                              ], #
    ni              => [q(Neutral Beam Injection),                              "https://en.wikipedia.org/wiki/Neutral-beam_injection"                                                                            ], #
    nodejs          => [q(NodeJs),                                              "https://en.wikipedia.org/wiki/NodeJs"                                                                                            ], #
    oauth           => [q(Oauth),                                               "https://en.wikipedia.org/wiki/OAuth"                                                                                             ], #
    openoffice      => [q(Apache Open Office),                                  "https://www.openoffice.org/download/index.html"                                                                                  ], #
    openssl         => [q(Open SSL),                                            "https://www.openssl.org/"                                                                                                        ], #
    othermeta       => [q(othermeta),                                           "http://docs.oasis-open.org/dita/dita/v1.3/errata02/os/complete/part3-all-inclusive/contentmodels/cmlto.html#cmlto__othermeta"    ], #
#   our             => [q(our),                                                 "https://perldoc.perl.org/functions/our.html"                                                                                     ], #
    oxygenformat    => [q(Oxygen Format),                                       "https://www.oxygenxml.com/doc/versions/20.1/ug-author/topics/linked-output-messages-of-external-engine.html"                     ], #
    oxygenworkshop  => [q(Oxygen Workshop),                                     "http://github.com/philiprbrenan/oxygenWorkShop"                                                                                  ], #
    pairprogram     => [q(pair program),                                        "https://en.wikipedia.org/wiki/Pair_programming"                                                                                  ], #
    pairprograming  => [q(pair programing),                                     "https://en.wikipedia.org/wiki/Pair_programming"                                                                                  ], #
    parkinson       => [q(Parkinson's law: work expands to fill the time available), "https://en.wikipedia.org/wiki/Parkinson%27s_law"                                                                            ], #
    parse           => [q(parse),                                               "https://en.wikipedia.org/wiki/Parsing"                                                                                           ], #
    pcdInstall      => [q(PCD installation),                                    "https://github.com/philiprbrenan/philiprbrenan.github.io/blob/master/pcd_installation.md"                                        ], #
    pcdLang         => [q(PCD),                                                 "https://philiprbrenan.github.io/data_edit_xml_edit_commands.html"                                                                ], #
    pcd             => [q(Dita::Pcd),                                           "https://metacpan.org/pod/Dita::PCD"                                                                                              ], #
    pdf             => [q(PDF),                                                 "https://en.wikipedia.org/wiki/PDF"                                                                                               ], #
    people          => [q(people),                                              "https://en.wikipedia.org/wiki/Person"                                                                                            ], #
    perlal          => [q(Perl Artistic Licence),                               "https://dev.perl.org/licenses/artistic.html"                                                                                     ], #
    perl            => [q(Perl),                                                "http://www.perl.org/"                                                                                                            ], #
    pg              => [q(Postgres database),                                   "https://www.postgresql.org/"                                                                                                     ], #
    pi              => [q(𝝿),                                                   "https://en.wikipedia.org/wiki/Pi"                                                                                                ], #
    philCpan        => [q(CPAN),                                                "https://metacpan.org/author/PRBRENAN"                                                                                            ], #
    photoApp        => [q(AppaApps Photo App),                                  "https://github.com/philiprbrenan/AppaAppsGitHubPhotoApp"                                                                         ], #
    php             => [q(PHP),                                                 "https://en.wikipedia.org/wiki/PHP"                                                                                               ], #
    pl              => [q(programming language),                                "https://en.wikipedia.org/wiki/Programming_language"                                                                              ], #
    plasma          => [q(plasma),                                              "https://en.wikipedia.org/wiki/Plasma_(physics)"                                                                                  ], #
    pli             => [q(Programming Language One),                            "https://en.wikipedia.org/wiki/PL/I"                                                                                              ], #
    pod             => [q(POD),                                                 "https://perldoc.perl.org/perlpod.html"                                                                                           ], #
    poppler         => [q(Poppler),                                             "https://poppler.freedesktop.org/"                                                                                                ], #
    portugal        => [q(Portugal),                                            "https://en.wikipedia.org/wiki/Portugal"                                                                                          ], #
    postgres        => [q(Postgres),                                            "https://github.com/philiprbrenan/postgres"                                                                                       ], #
    prb             => [q(philip r brenan),                                     "https://philiprbrenan.neocities.org/"                                                                                            ], #
    preprocessor    => [q(preprocessor),                                        "https://en.wikipedia.org/wiki/Preprocessor"                                                                                      ], #
    process         => [q(process),                                             "https://en.wikipedia.org/wiki/Process_management_(computing)"                                                                    ], #
    procfs          => [q(Process File System),                                 "https://en.wikipedia.org/wiki/Procfs"                                                                                            ], #
    program         => [q(program),                                             "https://en.wikipedia.org/wiki/Computer_program"                                                                                  ], #
    python          => [q(Python),                                              "https://www.python.org/"                                                                                                         ], #
    quicksort       => [q(Quick Sort),                                          "https://github.com/philiprbrenan/QuickSort"                                                                                      ], #
    rackspace       => [q(Rackspace),                                           "https://www.rackspace.com/"                                                                                                      ], #
    recursively     => [q(recursively),                                         "https://en.wikipedia.org/wiki/Recursion"                                                                                         ], #
    recursive       => [q(recursive),                                           "https://en.wikipedia.org/wiki/Recursion"                                                                                         ], #
    relocatable     => [q(relocatable),                                         "https://en.wikipedia.org/wiki/Relocation_%28computing%29"                                                                        ], #
    rrr             => [q(The R Programming Language),                          "https://en.wikipedia.org/wiki/R_(programming_language)"                                                                          ], #
    riyadh          => [q(Riyadh),                                              "https://en.wikipedia.org/wiki/Riyadh"                                                                                            ], #
    rfp             => [q(Request For Proposal),                                "https://en.wikipedia.org/wiki/Request_for_proposal"                                                                              ], #
    r53             => [q(Route 53),                                            "https://console.aws.amazon.com/route53"                                                                                          ], #
    rsa             => [q(RSA),                                                 "https://en.wikipedia.org/wiki/RSA_(cryptosystem)"                                                                                ], #
    rsync           => [q(rsync),                                               "https://linux.die.net/man/1/rsync"                                                                                               ], #
    s3Console       => [q(S3 Console),                                          "https://s3.console.aws.amazon.com/s3/home"                                                                                       ], #
    s3              => [q(S3),                                                  "https://aws.amazon.com/s3/"                                                                                                      ], #
    s390            => [q(IBM System 390),                                      "https://en.wikipedia.org/wiki/IBM_System/390"                                                                                    ], #
    saml            => [q(Security Assertion Markup Language),                  "https://en.wikipedia.org/wiki/Security_Assertion_Markup_Language"                                                                ], #
    samltest        => [q(SAML test),                                           "https://samltest.id/"                                                                                                            ], #
    samltools       => [q(SAML tools),                                          "https://www.samltool.com/sp_metadata.php"                                                                                        ], #
    sas             => [q(SAS Institute),                                       "https://en.wikipedia.org/wiki/SAS_Institute"                                                                                     ], #
    securityGroup   => [q(security group),                                      "https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/working-with-security-groups.html"                                           ], #
    selfaware       => [q(self aware),                                          "https://en.wikipedia.org/wiki/Self-awareness"                                                                                    ], #
    server          => [q(server),                                              "https://en.wikipedia.org/wiki/Server_(computing)"                                                                                ], #
    sevenZ          => [q(7z),                                                  "https://en.wikipedia.org/wiki/7z"                                                                                                ], #
    sha             => [q(SHA),                                                 "https://en.wikipedia.org/wiki/SHA-1"                                                                                             ], #
    shell           => [q(shell),                                               "https://en.wikipedia.org/wiki/Shell_(computing)"                                                                                 ], #
    shib            => [q(Shibboleth),                                          "https://www.shibboleth.net/"                                                                                                     ], #
    simd            => [q(SIMD),                                                "https://www.officedaytime.com/simd512e/"                                                                                         ], #
    smartmatch      => [q(smartmatch),                                          "https://perldoc.perl.org/perlop.html#Smartmatch-Operator"                                                                        ], #
    snake_case      => [q(snake_case),                                          "https://en.wikipedia.org/wiki/Snake_case"                                                                                        ], #
    camelCase       => [q(camelCase),                                           "https://en.wikipedia.org/wiki/Camel_case"                                                                                        ], #
    sow             => [q(Shibboleth on Windows),                               "http://philiprbrenan.appaapps.com/ShibbolethOnWindows"                                                                           ], #
    spot            => [q(spot),                                                "https://aws.amazon.com/ec2/spot/"                                                                                                ], #
    spreedsheet     => [q(Spreadsheet),                                         "https://en.wikipedia.org/wiki/Spreadsheet"                                                                                       ], #
    sql             => [q(Structured Query Language),                           "https://en.wikipedia.org/wiki/SQL"                                                                                               ], #
    squareroot      => [q(Square Root),                                         "https://en.wikipedia.org/wiki/Square_root"                                                                                       ], #
    ssh             => [q(Secure Shell),                                        "https://www.ssh.com/ssh"                                                                                                         ], #
    ssxr            => [q(Self Xref),                                           "https://philiprbrenan.github.io/selfServiceXref.pdf"                                                                             ], #
    stdin           => [q(stdin),                                               "https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)"                                                           ], #
    stderr          => [q(stderr),                                              "https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)"                                                           ], #
    stdout          => [q(stdout),                                              "https://en.wikipedia.org/wiki/Standard_streams#Standard_input_(stdin)"                                                           ], #
    step            => [q(step),                                                "http://docs.oasis-open.org/dita/dita/v1.3/errata02/os/complete/part3-all-inclusive/contentmodels/cmlts.html#cmlts__step"         ], #
    steps           => [q(steps),                                               "http://docs.oasis-open.org/dita/dita/v1.3/errata02/os/complete/part3-all-inclusive/contentmodels/cmlts.html#cmlts__steps"        ], #
    stopwords       => [q(stopwords),                                           "https://metacpan.org/pod/Storable"                                                                                               ], #
    storable        => [q(Storable),                                            "https://metacpan.org/pod/Storable"                                                                                               ], #
    sub             => [q(sub),                                                 "https://perldoc.perl.org/perlsub.html"                                                                                           ], #
    substeps        => [q(substeps),                                            "http://docs.oasis-open.org/dita/dita/v1.3/errata02/os/complete/part3-all-inclusive/contentmodels/cmlts.html#cmlts__substeps"     ], #
    sws             => [q(Sir Walter Scott),                                    "https://en.wikipedia.org/wiki/Walter_Scott"                                                                                      ], #
    ta              => [q(Transamerica),                                        "https://en.wikipedia.org/wiki/Transamerica_Corporation"                                                                          ], #
    transamerica    => [q(Transamerica),                                        "https://en.wikipedia.org/wiki/Transamerica_Corporation"                                                                          ], #
    table           => [q(table of information),                                "https://en.wikipedia.org/wiki/Table_(information)"                                                                               ], #
    tab             => [q(tab),                                                 "https://en.wikipedia.org/wiki/Tab_key"                                                                                           ], #
    taocp           => [q(The Art of Computer Programming),                     "https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming"                                                                   ], #
    task            => [q(task),                                                "http://docs.oasis-open.org/dita/dita/v1.3/errata02/os/complete/part3-all-inclusive/langRef/technicalContent/task.html#task"      ], #
    tdd             => [q(test driven development),                             "https://en.wikipedia.org/wiki/Test-driven_development"                                                                           ], #
    test            => [q(test),                                                "https://en.wikipedia.org/wiki/Software_testing"                                                                                  ], #
    textmatch       => [q(text matching),                                       "https://metacpan.org/pod/Text::Match"                                                                                            ], #
    thp             => [q(Theoretical Computational Physics),                   "https://en.wikipedia.org/wiki/Theoretical_physics"                                                                               ], #
    tls             => [q(TLS),                                                 "https://en.wikipedia.org/wiki/Transport_Layer_Security"                                                                          ], #
    tree            => [q(tree),                                                "https://en.wikipedia.org/wiki/Tree_(data_structure)"                                                                             ], #
    tritium         => [q(tritium),                                             "https://en.wikipedia.org/wiki/Tritium"                                                                                           ], #
    ubuntu          => [q(Ubuntu),                                              "https://ubuntu.com/download/desktop"                                                                                             ], #
    ucla            => [q(University of California at Los Angeles),             "https://en.wikipedia.org/wiki/University_of_California,_Los_Angeles"                                                             ], #
    udel            => [q(University of Delaware),                              "https://www.udel.edu/"                                                                                                           ], #
    udp             => [q(User Datagram Protocol),                              "https://en.wikipedia.org/wiki/User_Datagram_Protocol"                                                                            ], #
    ul              => [q(University of Lancaster),                             "https://en.wikipedia.org/wiki/Lancaster_University"                                                                              ], #
    undef           => [q(undef),                                               "https://perldoc.perl.org/functions/undef.html"                                                                                   ], #
    unicode         => [q(Unicode),                                             "https://en.wikipedia.org/wiki/Unicode"                                                                                           ], #
    unixHaters      => [q(Unix Haters Handbook),                                "https://1lib.eu/book/750790/8f3128"                                                                                              ], #
    universe        => [q(Universe),                                            "https://en.wikipedia.org/wiki/Universe"                                                                                          ], #
    unix            => [q(Unix),                                                "https://en.wikipedia.org/wiki/Unix"                                                                                              ], #
    unoconv         => [q(unoconv),                                             "https://github.com/unoconv/unoconv"                                                                                              ], #
    uow             => [q(Ubuntu on Windows),                                   "http://philiprbrenan.appaapps.com/UbuntuOnWindows"                                                                               ], #
    url             => [q(url),                                                 "https://en.wikipedia.org/wiki/URL"                                                                                               ], #
    user            => [q(user),                                                "https://en.wikipedia.org/wiki/User_(computing)"                                                                                  ], #
    usa             => [q(United States of America),                            "https://en.wikipedia.org/wiki/United_States"                                                                                     ], #
    uk              => [q(United Kingdom),                                      "https://en.wikipedia.org/wiki/United_Kingdom"                                                                                    ], #
    unexaminedlife  => [q(Socrates: "The unexamined life is not worth living"), "https://en.wikipedia.org/wiki/The_unexamined_life_is_not_worth_living"                                                           ], #
    upload          => [q(upload),                                              "https://en.wikipedia.org/wiki/Upload"                                                                                            ], #
    usa             => [q(United States),                                       "https://en.wikipedia.org/wiki/United_States"                                                                                     ], #
    uk              => [q(United Kingdom),                                      "https://en.wikipedia.org/wiki/United_Kingdom"                                                                                    ], #
    uspto           => [q(United States Patent and Trademark Office),           "https://en.wikipedia.org/wiki/USPTO"                                                                                             ], #
    utf8            => [q(utf8),                                                "https://en.wikipedia.org/wiki/UTF-8"                                                                                             ], #
    v2              => [q(Vectors In Two Dimensions),                           "https://pypi.org/project/Vector2/"                                                                                               ], #
    verify          => [q(verify),                                              "https://en.wikipedia.org/wiki/Software_verification_and_validation"                                                              ], #
    vhdl            => [q(VHDL),                                                "https://ghdl.readthedocs.io/en/latest/about.html"                                                                                ], #
    vi              => [q(vi),                                                  "https://www.vim.org/"                                                                                                            ], #
    website         => [q(web site),                                            "https://en.wikipedia.org/wiki/Website"                                                                                           ], #
    webpage         => [q(web page),                                            "https://en.wikipedia.org/wiki/Web_page"                                                                                          ], #
    whitespace      => [q(white space),                                         "https://en.wikipedia.org/wiki/Whitespace_character"                                                                              ], #
    whp             => [q(Whp),                                                 "https://www.whp.net/en/"                                                                                                         ], #
    widget          => [q(widget),                                              "https://en.wikipedia.org/wiki/Graphical_widget"                                                                                  ], #
    wikipedia       => [q(Wikipedia),                                           "https://en.wikipedia.org"                                                                                                        ], #
    word            => [q(word),                                                "https://en.wikipedia.org/wiki/Doc_(computing)"                                                                                   ], #
    x64             => [q(x64),                                                 "https://en.wikipedia.org/wiki/X86-64"                                                                                            ], #
    xmllint         => [q(Xml Lint),                                            "http://xmlsoft.org/xmllint.html"                                                                                                 ], #
    xmlparser       => [q(Xml parser),                                          "https://metacpan.org/pod/XML::Parser/"                                                                                           ], #
    xml             => [q(Xml),                                                 "https://en.wikipedia.org/wiki/XML"                                                                                               ], #
    xref            => [q(Xref),                                                "https://metacpan.org/pod/Data::Edit::Xml::Xref"                                                                                  ], #
    youngtableaug   => [q(Young Tableau on GitHub),                             "https://github.com/philiprbrenan/youngTableauSort/"                                                                              ], #
    youngtableau    => [q(Young Tableau),                                       "https://en.wikipedia.org/wiki/Young_tableau"                                                                                     ], #
    zerowidthspace  => [q(zero width space),                                    "https://en.wikipedia.org/wiki/Zero-width_space"                                                                                  ], #
    zip             => [q(zip),                                                 "https://linux.die.net/man/1/zip"                                                                                                 ], #
    zoom            => [q(Zoom),                                                "https://zoom.us/"                                                                                                                ], #
   );
 } # wellKnownUrls

sub expandWellKnownWordsAsUrlsInHtmlFormat($)                                   # Expand words found in a string using the html B<a> tag to supply a definition of that word.
 {my ($string)  = @_;                                                           # String containing url names to expand
  my $wellKnown = wellKnownUrls;                                                # Well known urls to expand

  for my $w(sort keys %$wellKnown)                                              # Expand well known words (lowercased) as html links
   {my ($t, $u) = @{$$wellKnown{$w}};
    $string =~ s(L\[$w\]) (<a href="$u">$t</a>)gis;                             # Explicit link
    $string =~ s(\s$w([.,;:'"]*)\s) ( <a href="$u">$t</a>$1 )gs;                # Word that matches
   }

  $string =~ s(W\[(\w+)\]) (<code>$1</code>)gs;                                 # W[...] wraps words with definitions we wish to stress
  $string =~ s(w\[(\w+)\]) ($1)gsr;                                             # w[...] wraps words with definitions we wish to keep as is
 }

sub expandWellKnownWordsAsUrlsInMdFormat($)                                     # Expand words found in a string using the md url to supply a definition of that word.
 {my ($string)  = @_;                                                           # String containing url names to expand
  my $wellKnown = wellKnownUrls;                                                # Well known urls to expand

  for my $w(sort keys %$wellKnown)                                              # Expand well known words (lowercased) as html links
   {my ($t, $u) = @{$$wellKnown{$w}};
#   $string =~ s(L\[$w\])            (\![$t]($u))gis;                           # Explicit link
#   $string =~ s(\s$w([.,;:'"]*)\s) ( \![$t]($u)$1 )gs;                         # Word that matches
    $string =~ s(L\[$w\])            ([$t]($u))gis;                             # Explicit link
    $string =~ s(\s$w([.,;:'"]*)\s) ( [$t]($u)$1 )gs;                           # Word that matches
   }

  $string =~ s(W\[(\w+)\]) (```$1```)gs;                                        # W[...] wraps words with definitions we wish to stress
  $string =~ s(w\[(\w+)\]) ($1)gsr;                                             # w[...] wraps words with definitions we wish to keep as is
 }

sub reinstateWellKnown($)                                                       #P Contract references to well known Urls to their abbreviated form
 {my ($string)  = @_;                                                           # Source string
  my $wellKnown = wellKnownUrls;                                                # Well known urls to contract

  for my $w(sort keys %$wellKnown)
   {my ($t, $u) = @{$$wellKnown{$w}};
    $string =~ s(L<$t\|$u>) (L<$t>)gis;
   }

  $string                                                                       # Result
 }

sub expandWellKnownUrlsInPerlFormat($)                                          # Expand short L<url> names found in a string in the format LE<lt>url-nameE<gt> using the Perl POD syntax
 {my ($string)  = @_;                                                           # String containing url names to expand
  my $wellKnown = wellKnownUrls;                                                # Well known urls to expand

  for my $w(sort keys %$wellKnown)
   {my ($t, $u) = @{$$wellKnown{$w}};
    $string =~ s(L\<$w\>) (L<$t|$u>)gis;
   }

  $string                                                                       # Result
 }

sub expandWellKnownUrlsInHtmlFormat($)                                          # Expand short L<url> names found in a string in the format L[url-name] using the html B<a> tag.
 {my ($string)  = @_;                                                           # String containing url names to expand
  my $wellKnown = wellKnownUrls;                                                # Well known urls to expand

  for my $w(sort keys %$wellKnown)                                              # Expand well known urls as html a links
   {my ($t, $u) = @{$$wellKnown{$w}};
    $string =~ s(L\[$w\]) (<a format="html" href="$u">$t</a>)gis;
   }

  if (my @e = $string =~ m(L\[(\w+)\])gs)                                       # Check for expansion failures
   {say STDERR "Failed to find url expansions for these words:\n", dump(\@e);
   }

  $string                                                                       # Result
 }

sub expandWellKnownUrlsInHtmlFromPerl($)                                        # Expand short L<url> names found in a string in the format L[url-name] using the html B<a> tag.
 {my ($string)  = @_;                                                           # String containing url names to expand
  my $wellKnown = wellKnownUrls;                                                # Well known urls to expand

  for my $w(sort keys %$wellKnown)                                              # Expand well known urls as html a links
   {my ($t, $u) = @{$$wellKnown{$w}};
    my $s = qq(<a format="html" href="$u">$t</a>);
    $string =~ s(L\<$w\>) ($s)gis;
   }

  $string                                                                       # Result
 }

sub expandWellKnownUrlsInPod2Html($)                                            # Expand short L<url> names found in a string in the format =begin html format
 {my ($string)  = @_;                                                           # String containing url names to expand
  my $wellKnown = wellKnownUrls;                                                # Well known urls to expand

  for my $w(sort keys %$wellKnown)                                              # Expand well known urls as html a links
   {my ($t, $u) = @{$$wellKnown{$w}};
    my $r = <<END =~ s(`) (=)gr;
`begin HTML

<aaaa format="html" href="$u">$t</aaaa>

`end   HTML
END
    $string =~ s(\s*L\<$w\>\s*) (\n\n$r\n\n)gis;
   }

  $string                                                                       # Result
 }

sub expandWellKnownUrlsInDitaFormat($)                                          # Expand short L<url> names found in a string in the format L[url-name] in the L[Dita] B<xref>format.
 {my ($string)  = @_;                                                           # String containing url names to expand
  my $wellKnown = wellKnownUrls;                                                # Well known urls to expand

  for my $w(sort keys %$wellKnown)
   {my ($t, $u) = @{$$wellKnown{$w}};
    $string =~ s(L\[$w\]) (<xref scope="external" format="html" href="$u">$t</xref>)gis;
   }

  $string                                                                       # Result
 }

sub formatSourcePodAsHtml                                                       #P Format the L<pod> in the current source file as L<html>.
 {my $s1 = readFile $0;                                                         # Read source file
  my $s2 = expandWellKnownUrlsInPerlFormat $s1;                                 # Expand Perl links
  my $s3 = expandWellKnownUrlsInHtmlFormat $s2;                                 # Expand Html links
     $s3 =~ s(<th>) (<th align="left">)g;                                       # Align headers
  my $s  = writeTempFile $s3;                                                   # Write expanded source to temporary file
  my $t  = setFileExtension $0, q(html);

  lll qx(pod2html --infile $s --outfile $t; rm pod2htmd.tmp);                   # Format expanded source as HTML
  lll qx(opera $t);                                                             # Show HTML
 }

sub expandNewLinesInDocumentation($)                                            # Expand new lines in documentation, specifically \n for new line and \m for two new lines.
 {my ($s) = @_;                                                                 # String to be expanded
  $s =~ s(\\m) (\n\n)gs;                                                        # Double new line
  $s =~ s(\\n) (\n)gs;                                                          # Single new line
  $s
 }

sub extractTest($)                                                              #P Remove example markers from test code.
 {my ($string) = @_;                                                            # String containing test line
 #$string =~ s/\A\s*{?(.+?)\s*#.*\Z/$1/;                                        # Remove any initial white space and possible { and any trailing white space and comments
  $string =~ s(#T(\w|:)+) ()gs;                                                 # Remove test tags from line
  $string
 }

sub extractCodeBlock($;$)                                                       # Extract the block of code delimited by B<$comment>, starting at qq($comment-begin), ending at qq($comment-end) from the named B<$file> else the current Perl program $0 and return it as a string or confess if this is not possible.
 {my ($comment, $file) = @_;                                                    # Comment delimiting the block of code, file to read from if not $0
  my $s = readFile($file//$0);
  if ($s =~ m($comment-begin\s*\n(.*?)$comment-end)is)
   {my $c = $1;
    $c =~ s(\s+\Z) ()s;
    return qq($c\n);
   }
  confess "Unable to locate code delimited by $comment in $0\n";                #CODEBLOCK-begin
  my $a = 1;
  my $b = 2;                                                                    #CODEBLOCK-end
 }

sub updateDocumentation(;$)                                                     # Update the documentation for a Perl module from the comments in its source code. Comments between the lines marked with:\m  #Dn title # description\mand:\m  #D\mwhere n is either 1, 2 or 3 indicating the heading level of the section and the # is in column 1.\mMethods are formatted as:\m  sub name(signature)      #FLAGS comment describing method\n   {my ($parameters) = @_; # comments for each parameter separated by commas.\mFLAGS can be chosen from:\m=over\m=item I\mmethod of interest to new users\m=item P\mprivate method\m=item r\moptionally replaceable method\m=item R\mrequired replaceable method\m=item S\mstatic method\m=item X\mdie rather than received a returned B<undef> result\m=back\mOther flags will be handed to the method extractDocumentationFlags(flags to process, method name) found in the file being documented, this method should return [the additional documentation for the method, the code to implement the flag].\mText following 'E\xxample:' in the comment (if present) will be placed after the parameters list as an example. Lines containing comments consisting of '#T'.methodName will also be aggregated and displayed as examples for that method.\mLines formatted as:\m  BEGIN{*source=*target}\mstarting in column 1 will define a synonym for a method.\mLines formatted as:\m  #C emailAddress text\mwill be aggregated in the acknowledgments section at the end of the documentation.\mThe character sequence B<\\xn> in the comment will be expanded to one new line, B<\\xm> to two new lines and B<L>B<<$_>>,B<L>B<<confess>>,B<L>B<<die>>,B<L>B<<eval>>,B<L>B<<lvalueMethod>> to links to the perl documentation.\mSearch for '#D1': in L<https://metacpan.org/source/PRBRENAN/Data-Table-Text-20180810/lib/Data/Table/Text.pm> to see  more examples of such documentation in action - although it is quite difficult to see as it looks just like normal comments placed in the code.\mParameters:\n
 {my ($perlModule) = @_;                                                        # Optional file name with caller's file being the default
  $perlModule //= $0;                                                           # Extract documentation from the caller if no perl module is supplied
  my $package = perlPackage($perlModule);                                       # Package name
  my $maxLinesInExample = 500;                                                  # Maximum number of lines in an example
  my %attributes;                                                               # Attributes defined in this package, the values of this hash are the flags for the attribute
  my %attributeDescription;                                                     # Description of each attribute
  my %collaborators;                                                            # Collaborators #C pause-id  comment
  my %comment;                                                                  # The line comment associated with a method
  my %examples;                                                                 # Examples for each method
  my %exported;                                                                 # Exported methods
  my %genHashFlags;                                                             # Flags on attributes in objects defined by genHash
  my %genHashs;                                                                 # Attributes in objects defined by genHash
  my %genHash;                                                                  # Attributes in objects defined by genHash
  my %genHashPackage;                                                           # Packages defined by genHash
  my %isUseful;                                                                 # Immediately useful methods
  my %methods;                                                                  # Methods that have been coded as opposed to being generated
  my %methodParms;                                                              # Method names including parameters
  my %methodX;                                                                  # Method names for methods that have an version suffixed with X that die rather than returning B<undef>
  my %private;                                                                  # Private methods
  my %replace;                                                                  # Optional replaceable methods
  my %Replace;                                                                  # Required replaceable methods
  my %signatureNames;                                                           # Signature using parameter names
  my %static;                                                                   # Static methods
  my %synonymTargetSource;                                                      # Synonyms from source to target - {$source}{$target} = 1 - can be several
  my %synonymTarget;                                                            # Synonym target - confess is more than one
  my @synopsis;                                                                 # External synopsis to allow L<symbol> to be expanded
  my %title;                                                                    # Method to title of section describing method
  my %userFlags;                                                                # User flags
  my $oneLineDescription = qq(\n);                                              # One line description from =head1 Name
  my $install = '';                                                             # Additional installation notes
  my @doc;                                                                      # Documentation
  my @private;                                                                  # Documentation of private methods
  my $level = 0; my $off = 0;                                                   # Header levels
  my %unitary;                                                                  # A unitary method - all of its parameters other than the first are strings or numbers
  my $version;                                                                  # Version of package being documented
  my @ctags;                                                                    # Ctags file in pipe format for each sub
  my %moduleDescription;                                                        # {section}{method}{detail}=value

  my $sourceIsString = $perlModule =~ m(\n)s;                                   # Source of documentation is a string not a file
  my $Source = my $source = $sourceIsString ? $perlModule:readFile($perlModule);# Read the perl module from a file unless it is a string not a file

  if ($source =~ m(our\s+\$VERSION\s*=\s*(\S+)\s*;)s)                           # Update references to examples so we can include html and images etc. in the module
   {my $V = $version = $1;                                                      # Quoted version
    if (my $v = eval $V)                                                        # Remove any quotes
     {my $s = $source;
      $source =~                                                                # Replace example references in source
        s((https://metacpan\.org/source/\S+?-)(\d+)(/examples/))
         ($1$v$3)gs;
      $moduleDescription{version} = $v;                                         # Record version in module description
     }
   }

  if ($source =~ m(\n=head1\s+Name\s+(?:\w|:)+\s+(.+?)\n)s)                     # Extract one line description from =head1 Name ... Module name ... one line description
   {my $s = $1;
    $s =~ s(\A\s*-\s*) ();                                                      # Remove optional leading -
    $s =~ s(\s+\Z)     ();                                                      # Remove any trailing spaces
    $oneLineDescription = "\n$s\n";                                             # Save description
    $moduleDescription{oneLineDescription} = $oneLineDescription;               # Record one line description in module description
   }

  if (1)                                                                        # Document description
   {my $v = $version ? "\n\nVersion $version.\n" : "";
    push @doc, <<"END";
`head1 Description
$oneLineDescription$v

The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.

END
   }

  my @lines = split /\n/, $source;                                              # Split source into lines

  for my $l(keys @lines)                                                        # Tests associated with each method
   {my $line = $lines[$l];
    if (my @tags = $line =~ m/(?:\s#T((?:\w|:)+))/g)
     {my %tags; $tags{$_}++ for @tags;

      for(grep {$tags{$_} > 1} sort keys %tags)                                 # Check for duplicate example names on the same line
       {warn "Duplicate example name $_ on line $l";
       }

      my @testLines = (extractTest($line));

      if ($line =~ m/<<(END|'END'|"END")/)                                      # Process here documents
       {for(my $L = $l + 1; $L < @lines; ++$L)
         {my $nextLine = $lines[$L];
          push @testLines, extractTest($nextLine);
          last if $nextLine =~ m/\AEND/;                                        # Finish on END
         }
       }

      if ($line =~ m(\A(\s*)if\s*\x28(\d+)\x29))                                # Process "if (\d+)" recording leading spaces
       {my $S = $1; my $minimumNumberOfLines = $2;                              # Leading spaces so we can balance the indentation of the closing curly bracket. Start testing for the closing } after this many lines
        my $M = $maxLinesInExample;
        for(my ($L, $N) = ($l + 1, 0); $L < @lines; ++$L, ++$N)
         {my $nextLine = $lines[$L];
          push @testLines, extractTest($nextLine);
          if ($N >= $minimumNumberOfLines and $nextLine =~ m/\A$S }/)           # Finish on closing brace in column 2
           {#say STDERR "End of example";
            last;
           }
          else
           {#say STDERR "$N  ", $nextLine;
           }
          my $L = $l + 1;
          $N < $M or fff($L, $perlModule, "Too many lines in example");         # Prevent overruns
         }

        if (@testLines > 1)                                                     # Remove leading and trailing 'if' if possible
         {if ($testLines[0] =~ m(\A\s*if\s*\x{28}\d\x{29}\s*{)i)
           {pop @testLines; shift @testLines;
           }
         }
       }

      push @testLines, '';                                                      # Blank line between each test line

      for my $testLine(@testLines)                                              # Save test lines
       {for my $t(sort keys %tags)
         {$testLine =~ s(!) (#)g if $t =~ m(\AupdateDocumentation\Z)s;          # To prevent the example documentation using this method showing up for real.
          push @{$examples{$t}}, $testLine;
         }
       }
      push @{$moduleDescription{tests}}, [\@tags, \@testLines];                 # Record tests in module description
     }
   }

  for my $l(keys @lines)                                                        # Tests associated with replaceable methods
   {my $M = $maxLinesInExample;
    my $line = $lines[$l];
    if ($line =~ m(\Asub\s+((\w|:)+).*#(\w*)[rR]))
     {my $sub = $1;
      my @testLines = ($line =~ s(\s#.*\Z) ()r);
      for(my ($L, $N) = ($l + 1, 0); $L < @lines; ++$L, ++$N)
       {my $nextLine = $lines[$L];
        push @testLines, extractTest($nextLine);
        last if $nextLine =~ m/\A }/;                                           # Finish on closing brace in column 2
        my $L = $l + 1;
        $N < $M or fff($L, $perlModule, "Too many lines in test");              # Prevent overruns
       }
      push @testLines, '';                                                      # Blank line between each test line

      for my $testLine(@testLines)                                              # Save test lines
       {push @{$examples{$sub}}, $testLine;
       }
     }
   }

  for my $l(keys @lines)                                                        # Generated objects
   {my $M = $maxLinesInExample;
    my $line = $lines[$l];
    if ($line =~ m(genHash\s*\x28\s*(q\x28.+\x29|__PACKAGE__).+?# (.+)\Z))
     {my $p = $1; my $c = $2;
         $p = $p =~ s(q[qw]?\x28|\x29) ()gsr =~ s(__PACKAGE__) ($package)gsr;
      $genHashPackage{$p} = $c;
      for(my ($L, $N) = ($l + 1, 0); $L < @lines; ++$L, ++$N)
       {my $nextLine = $lines[$L];
        if ($nextLine =~ m(\A\s+(\w+)\s*=>\s*.+?#(\w*)\s+(.*)\Z))
         {my $flags = $genHashFlags{$p}{$1} = $2;
                      $genHashs    {$p}{$1} = $3;
          if (my $invalidFlags = $flags =~ s([I]) ()gsr)
           {confess "Invalid flags $invalidFlags on line $L:\n$nextLine";
           }
         }
        last if $nextLine =~ m/\A\s*\);/;                                       # Finish on closing bracket
        $N < $M or confess                                                      # Prevent overruns
          "More than $M line genHash definition at line $l\n".
          join("\n", @lines[$l..$L]);
       }
     }
   }

  for my $l(keys @lines)                                                        # Place the synopsis in a here doc block starting with my $documentationSynopsis = <<END; if this text contains L<symbol> that should be expanded. If present, the generated text will be used to generate a =head1 Synopsis section just before the description
   {my $line = $lines[$l];
    if ($line =~ m(\Amy \$documentationSynopsis = <<END;))
     {for(my ($L, $N) = ($l + 1, 0); $L < @lines; ++$L, ++$N)
       {my $nextLine = $lines[$L];
        last if $nextLine =~ m(\AEND\Z);
        push @synopsis, $nextLine;
       }
     }
   }

  if (1)                                                                        # Offset method name in examples to make it easier to pick out.
   {my $mark = boldString('  # Example');                                       # Marker to highlight the method being described
     for my $m(sort keys %examples)
     {my $L = $examples{$m};
      for my $i(keys @$L)
#      {if (index($$L[$i], $m) > -1)
       {if ($$L[$i] =~ m(\b$m\b))
         {$$L[$i] = join "\n", '', '  '.$$L[$i].$mark, '';
         }
       }
     }
   }

  for my $l(keys @lines)                                                        # Extract synonyms
   {my $line = $lines[$l];
    if ($line =~ m(\ABEGIN\{\*(\w+)=\*(\w+)\}))
     {my ($source, $target) = ($1, $2);
      $synonymTargetSource{$target}{$source} = 1;                               # Multiple synonyms for a method are allowed
      confess "Multiple targets for synonym: $source\n"                         # Only one method can be associated with each synonym
        if $synonymTarget{$target} and $synonymTarget{$target} ne $source;
      $synonymTarget{$source} = $target;
      $moduleDescription{methods}{$target}{synonyms}{$source} = 1;              # Include synonyms in module description
     }
   }

  unless($perlModule =~ m(\A(Text.pm|Doc.pm)\Z)s)                               # Load the module being documented so that we can call its extractDocumentationFlags method if needed to process user flags, we do not need to load these modules as they are already loaded
   {do "./$perlModule";
    confess $@ if $@;
   }

  for my $l(keys @lines)                                                        # Extract documentation from comments
   {my $L = $l + 1;                                                             # Line number
    my $line     = $lines[$l];                                                  # This line
    my $nextLine = $lines[$l+1];                                                # The next line
    if ($line =~ /\A#D(\d)\s+(.*?)\s*(#\s*(.+)\s*)?\Z/)                         # Sections are marked with #Dn in column 1-3 followed by title followed by optional text
     {$level = $1;
      my $headLevel = $level+$off;
      push @doc, "\n=head$headLevel $2" if $level;                              # Heading
      push @doc, "\n$4"                 if $level and $4;                       # Text of section
     }
    elsif ($line =~ /\A#C(?:ollaborators)?\s+(\S+)\s+(.+?)\s*\Z/)               # Collaborators
     {$collaborators{$1} = $2;
     }
    elsif ($line =~ /\A#I(?:nstall(?:ation)?)?\s+(.+)\Z/)                       # Extra install instructions
     {$install = "\\m$1\\m";
     }
    elsif ($line =~ /\A#D(off)?/)                                               # Switch documentation off
     {$level = 0;
     }
    elsif ($level and $line =~                                                  # Documentation for a generated lvalue * method = sub name comment
     /\Asub\s*(\w+)\s*{.*}\s*#(\w*)\s+(.*)\Z/)
     {my ($name, $flags, $description) = ($1, $2, $3);                          # Name of attribute, flags, description from comment
      $attributes{$name}           = $flags;
      $attributeDescription{$name} = $description;
     }
    elsif ($level and $line =~                                                  # Documentation for a method
     /\Asub\b\s*(.*?)?(\s*:lvalue)?\s*#(\w*)\s+(.+?)\s*\Z/)
     {my ($sub, $lvalue, $flags, $comment, $example, $produces) =               # Name from sub, flags, description
         ($1, $2, $3, $4);
      $flags //= '';                                                            # No flags found

      if ($comment =~ m/\A(.*)Example:(.+?)\Z/is)                               # Extract example - in comment examples are now deprecated in favor of using tests as examples
       {$comment = $1;
       ($example, $produces) = split /:/, $2, 2;
       }

      if ($comment !~ m(\.\s*\Z)is)                                             # Check for closing full stop
       {#fff $L, $perlModule, "Comment on line: $L does not end in a full stop\n$comment";
       }

      my $signature = $sub =~ s/\A\s*(\w|:)+//gsr =~                            # Signature
                              s/\A\x28//gsr     =~
                              s/\x29\s*(:lvalue\s*)?\Z//gsr;
      my $name      = $sub =~ s/\x28.*?\x29//r;                                 # Method name after removing parameters

      my $methodX   = $flags =~ m/X/;                                           # Die rather than return undef
      my $private   = $flags =~ m/P/;                                           # Private
      my $static    = $flags =~ m/S/;                                           # Static
      my $isUseful  = $flags =~ m/I/;                                           # Immediately useful
      my $unitary   = $flags =~ m/U/;                                           # Unitary method - the parameters, other than the first, are strings or numbers
      my $exported  = $flags =~ m/E/;                                           # Exported
      my $replace   = $flags =~ m/r/;                                           # Optionally replaceable
      my $Replace   = $flags =~ m/R/;                                           # Required replaceable
      my $userFlags = $flags =~ s/[EIPrRSX]//gsr;                               # User flags == all flags minus the known flags

      confess "(P)rivate and (rR)eplacable are incompatible on method $name\n"
        if $private and $replace || $Replace;
      confess "(S)tatic and (rR)eplacable are incompatible on method $name\n"
        if $static and $replace || $Replace;
      confess "(E)xported and (rR)eplacable are incompatible on method $name\n"
        if $exported and $replace || $Replace;
      confess "(E)xported and (S)tatic are incompatible on method $name\n"
        if $exported and $static;

      $methodX {$name} = $methodX  if $methodX;                                 # MethodX
      $private {$name} = $private  if $private;                                 # Private
      $replace {$name} = $replace  if $replace;                                 # Optionally replace
      $Replace {$name} = $Replace  if $Replace;                                 # Required replace
      $static  {$name} = $static   if $static;                                  # Static
      $isUseful{$name} = $comment  if $isUseful;                                # Immediately useful
      $exported{$name} = $exported if $exported;                                # Exported
      $unitary {$name} = $unitary  if $unitary;                                 # Unitary method
      $comment {$name} = $comment;                                              # Comment describing method

      for my $field                                                             # Include method details in module description
       (qw(methodX private replace Replace static isUseful
           exported unitary comment signature name flags userFlags))
       {my $v = eval q($).$field;
        next if $@;
        next unless $v;
        $moduleDescription{methods}{$name}{$field} = $v;
       }

      $userFlags{$name} =                                                       # Process user flags
        &docUserFlags($userFlags, $perlModule, $package, $name)
        if $userFlags;

      my ($parmNames, $parmDescriptions);
      if ($signature)                                                           # Parameters, parameter descriptions from comment
       {($parmNames, $parmDescriptions) =
         $nextLine =~ /\A\s*(.+?)\s*#\s*(.+?)\s*\Z/;
       }
      $parmNames //= ''; $parmDescriptions //= '';                              # No parameters

      my @parameters = split /,\s*/,                                            # Parameter names
        $parmNames =~ s/\A\s*\{my\s*\x28//r =~ s/\x29\s*=\s*\@_.*//r;           # Names inside parenthesis

      my $signatureNames = join ', ', @parameters;                              # Signature using parameter names
         $signatureNames{$name} = $signatureNames;

      my $signatureLength = length($signature =~ s([;\\]) ()gsr);               # Number of parameters in signature
      @parameters == $signatureLength or                                        # Check signature length
        confess "Wrong number of parameter descriptions for method: ".
          "$name($signature)\n";

      my @parmDescriptions = map {ucfirst()} split /,\s*/, $parmDescriptions;   # Parameter descriptions with first letter uppercased

      if (1)                                                                    # Check parameters comment
       {my $p = @parmDescriptions;
        my $l = $signatureLength;
        $p == $l or fff $L, $perlModule, <<"END";
Method:

  $name($signature)

The comment describing the parameters for this
method has descriptions for $p parameters but the signature suggests that there
are $l parameters.

The comment is split on /,/ to divide the comment into descriptions of each
parameter.

The comment supplied is:
$parmDescriptions
END
       }

      for my $p(keys @parameters)                                               # Record parameters in module description
       {my $d = [$parameters[$p], $parmDescriptions[$p]];
        push @{$moduleDescription{methods}{$name}{parameters}}, $d;
       }

      my $parametersAsString = join ', ', @parameters;                          # Parameters as a comma separated string
      my $headLevel = $level+$off+1;                                            # Heading level
#     my $methodSignature = "$name($parametersAsString)";                       # Method(signature)

      $methods{$name}++;                                                        # Methods that have been coded as opposed to being generated
      $methodParms{$name} = $name;                                              # Method names not including parameters
      $methodParms{$name.'X'} = $name if $methodX;                              # Method names not including parameters
      $methodX{$name}++ if $methodX;                                            # Method names that have an X version
      if (my $u = $userFlags{$name})                                            # Add names of any generated methods
       {$methodParms{$_} = $name for @{$u->[2]};                                # Generated names array
       }

      my @method;                                                               # Accumulate method documentation

      if (1)                                                                    # Section title
       {my $h = $private ? 2 : $headLevel;
        my $title = $title{$name} = qq($name($signatureNames));                 # Method title
        push @method, "\n=head$h $title\n\n$comment\n";                         # Method description
       }

      push @method, indentString(formatTable
       ([map{[$parameters[$_], $parmDescriptions[$_]]} keys @parameters],
        [qw(Parameter Description)]), '  ')
        if $parmNames and $parmDescriptions and $parmDescriptions !~ /\A#/;     # Add parameter description if present

      push @method,                                                             # Add user documentation
       "\n".$userFlags{$name}[0]."\n"          if $userFlags{$name}[0];

      push @method,                                                             # Add example
       "\nB<Example:>\n\n  $example"           if $example;

      push @method,                                                             # Produces
       "\n$produces"                           if $produces;

      if (my $examples = $examples{$name})                                      # Format examples
       {if (my @examples = @$examples)
         {push @method, '\nB<Example:>\m', map {"  $_"} @examples;
         }
       }

      push @method, <<END if $replace;                                          # Optionally replaceable

You can provide you own implementation of this method in your calling package
via:

  sub $name {...}

if you wish to override the default processing supplied by this method.

END


      push @method, <<END if $Replace;                                          # Required replaceable

You must supply an implementation of this method in your package via:

  sub $name {...}

END

      push @method,                                                             # Add a note about the availability of an X method
       "\nUse B<${name}X> to execute L<$name|/$name> but B<die> '$name'".
       " instead of returning B<undef>"        if $methodX;

      push @method,                                                             # Static method
       "\nThis is a static method and so should either be imported or invoked as:\n\n".
       "  $package\:\:$name\n"                 if $static;

      push @method,                                                             # Exported
       "\nThis method can be imported via:\n\n".
       "  use $package qw($name)\n"            if $exported;

      if (my $s = $synonymTargetSource{$name})                                  # Synonym
       {if (keys %$s)
         {for my $source(sort keys %$s)
           {push @method, "\nB<$source> is a synonym for L<$name|/$name>.\n";
           }
         }
       }

      push @{$private ? \@private : \@doc}, @method;                            # Save method documentation in correct section
      push @ctags, join "|", $name, qq/($signatureNames)/,                      # Ctags line
                             q(: ).$comment =~ s(\|) (_)gr, q();
     }
    elsif ($level and $line =~                                                  # Documentation for a generated lvalue * method = sub name comment
     /\A\s*genLValue(?:\w+?)Methods\s*\x28q(?:w|q)?\x28(\w+)\x29\x29;\s*#\s*(.+?)\s*\Z/)
     {my ($name, $description) = ($1, $2);                                      # Name from sub, description from comment
      next if $description =~ /\A#/;                                            # Private method if #P
      my $headLevel = $level+$off+1;                                            # Heading level
      $methodParms{$name} = $name;                                              # Method names not including parameters
      $comment    {$name} = $description =~ s(\A#) ()gsr;                       # Description of method
      push @doc, "\n=head$headLevel $name :lvalue\n\n$description\n";           # Method description
     }
   }

  if (isSubInPackage($package, q(processModuleDescription)))                    # Process module description
   {my $s = $package.q(::processModuleDescription);
#   my $c = qq(\&$s(reloadHashes(\\%moduleDescription)));                       # Fails with Data::Edit::Xml
    my $c = qq(\&$s(\\%moduleDescription));
    eval qq($c);
    cluck $@ if $@;
   }

  if (1)                                                                        # Write ctags for Geany
   {my $c = join "\n", "# format=pipe", sort @ctags;
    my $h = $ENV{HOME};
    my $p = $package;
    my $f = fpe($h, qw(.config geany tags), $p.q(.pl), q(tags));
    eval {owf($f, $c)};
    eval {dumpFile(fpe($h, qw(.config help), $p, q(txt)), \%moduleDescription)};# Write module description so it can be reused elsewhere
   }

  if (keys %genHashs)                                                           # Document generated objects
   {push @doc, qq(\n), qq(=head1 Hash Definitions), qq(\n);
    for   my $package  (sort keys % genHashs)
     {my @i; my @o;                                                             # Input and output attributes
      for my $attribute(sort keys %{$genHashs{$package}})
       {my $comment = $genHashs{$package}{$attribute}     // q();
        my $flags   = $genHashFlags{$package}{$attribute} // q();

#       my $a = qq(B<$attribute> - $comment\n);                                 # Attribute description
        my $a = qq(=head4 $attribute\n\n$comment\n);                            # Attribute description
        push @{$flags =~ m(I)s ? \@i : \@o}, $a;

        if ($title{$attribute})                                                 # Record the title of the attribute so we can link to it via L[name].
         {lll "Attribute: $attribute defined more than once"
         }
        else
         {$title{$attribute} = $attribute;
         }
       }

      push @doc, qq(\n), qq(=head2 $package Definition), qq(\n),                # Attributes header
                 $genHashPackage{$package}, qq(\n);

      if (@i)                                                                   # Input fields
       {push @doc, qq(\n), qq(=head3 Input fields), qq(\n), @i;
       }

      if (@o)                                                                   # Output fields
       {push @doc, qq(\n), qq(=head3 Output fields), qq(\n), @o;
       }
     }
   }

  if (my @a = sort keys %attributes)
   {push my @d, qq(\n), qq(=head1 Attributes\n\n);
    push @d, <<"END";
The following is a list of all the attributes in this package.  A method coded
with the same name in your package will over ride the method of the same name
in this package and thus provide your value for the attribute in place of the
default value supplied for this attribute by this package.

`head2 Replaceable Attribute List

END
    push @d, join ' ', @a, "\n\n";
    for my $name(@a)
     {my $d = $attributeDescription{$name};
      push @d, qq(=head2 $name\n\n$d\n\n);
     }
    push @doc, @d;
   }

  if (my @r = sort keys %replace)
   {push @doc, qq(\n), <<END;
`head1 Optional Replace Methods

The following is a list of all the optionally replaceable methods in this
package.  A method coded with the same name in your package will over ride the
method of the same name in this package providing your preferred processing for
the replaced method in place of the default processing supplied by this
package. If you do not supply such an over riding method, the existing method
in this package will be used instead.

`head2 Replaceable Method List

END
    push @doc, join ' ', @r, "\n\n";
   }

  if (1)                                                                        # Alphabetic listing of methods that still need examples
   {my %m = %methods;
    delete @m{$_, "$_ :lvalue"} for keys %examples;
    delete @m{$_, "$_ :lvalue"} for keys %private;
    my $n = keys %m;
    my $N = keys %methods;
    say STDERR formatTable(\%m), "\n$n of $N methods still need tests" if $n;
   }

  if (keys %isUseful)                                                           # Alphabetic listing of immediately useful methods
    {my @d;
     push @d, <<END;

`head1 Immediately useful methods

These methods are the ones most likely to be of immediate use to anyone using
this module for the first time:

END
    for my $m(sort {lc($a) cmp lc($b)} keys %isUseful)
     {my $c = $isUseful{$m};
      my $s = $signatureNames{$m};
      my $n = $m.($s ? qq/($s)/ : q());
      push @d, "L<$n|/$n>\n\n$c\n"
     }
    push @d, <<END;

END
    unshift @doc, (shift @doc, @d)                                              # Put first after title
   }

  push @doc, qq(\n\n=head1 Private Methods), @private if @private;              # Private methods in a separate section if there are any

  if (keys %synonymTarget)                                                      # Synonyms
   {my @s;
    my $line;
    for my $source(sort keys %synonymTarget)
     {my $target  = $synonymTarget{$source};
      my $comment = $comment{$target} // confess "No comment for $target\n";
         $comment =~ s(\..*\Z) (\.)s;
      push @s, qq(B<$source> is a synonym for L<$target|/$target> - $comment);
     }
    my $s = join q(\n\n), @s;
    push @doc, qq(\n\n=head1 Synonyms\n\n$s\n);
   }

  push @doc, qq(\n\n=head1 Index\n\n);
  if (1)
   {my $n = 0;
    for my $s(sort {lc($a) cmp lc($b)} keys %methodParms)                       # Alphabetic listing of methods
     {my $t = $methodParms{$s};
      my $c = $comment{$s};
      if ($c and $t)
       {$c =~ s(\..*\Z) (\.)s;
        push @doc, ++$n.qq( L<$s|/$t> - $c\n);
       }
     }
   }

  if (keys %exported)                                                           # Exported methods available
   {push @doc, <<"END";


`head1 Exports

All of the following methods can be imported via:

  use $package qw(:all);

Or individually via:

  use $package qw(<method>);


END

    my $n = 0;
    for my $s(sort {lc($a) cmp lc($b)} keys %exported)                          # Alphabetic listing of exported methods
     {push @doc, ++$n." L<$s|/$s>\n"
     }
   }

  push @doc, <<END;                                                             # Standard stuff
`head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install $package

`head1 Author

L<philiprbrenan\@gmail.com|mailto:philiprbrenan\@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

`head1 Copyright

Copyright (c) 2016-2021 Philip R Brenan.

This module is free software. It may be used, redistributed and/or modified
under the same terms as Perl itself.
END

  if (keys %collaborators)                                                      # Acknowledge any collaborators
   {push @doc,
     '\n=head1 Acknowledgements\m'.
     'Thanks to the following people for their help with this module:\m'.
     '=over\m';
    for(sort keys %collaborators)
     {my $p = "L<$_|mailto:$_>";
      my $r = $collaborators{$_};
      push @doc, "=item $p\n\n$r\n\n";
     }
    push @doc, '=back\m';
   }

  push @doc, '=cut\m';                                                          # Finish documentation

  if (keys %methodX)                                                            # Insert X method definitions
   {my @x;
    for my $x(sort keys %methodX)
     {push @x, ["sub ${x}X", "{&$x", "(\@_) || die '$x'}"];
     }
    push @doc, formatTableBasic(\@x);
   }

  for my $name(sort keys %userFlags)                                            # Insert generated method definitions
   {if (my $doc = $userFlags{$name})
     {push @doc, $doc->[1] if $doc->[1];
     }
   }

  push @doc, <<'END';                                                           # Standard test sequence

# Tests and documentation

sub test
 {my $p = __PACKAGE__;
  binmode($_, ":utf8") for *STDOUT, *STDERR;
  return if eval "eof(${p}::DATA)";
  my $s = eval "join('', <${p}::DATA>)";
  $@ and die $@;
  eval $s;
  $@ and die $@;
  1
 }

test unless caller;
END

  if (@synopsis)                                                                # Add the generated synopsis at the front if present       }
   {unshift @doc, q(=head1 Synopsis), @synopsis;
   }

  for(@doc)                                                                     # Expand snippets in documentation
   {s/\\m/\n\n/gs;                                                              # Double new line
    s/\\n/\n/gs;                                                                # Single new line
    s/\\x//gs;                                                                  # Break
    s/`/=/gs;
   }

  my $doc = expandWellKnownUrlsInPerlFormat(join "\n", @doc);                   # Create documentation

  for my $m(sort keys %title)                                                   # Links to titles
   {my $t = $title{$m};
    $doc =~ s(L\[$m\]) (L<$m|/"$t">)gs;
   }

  unless($sourceIsString)                                                       # Update source file
   {if (@synopsis)                                                              # Remove existing synopsis if adding a generated one
     {$source =~ s(=head1 Synopsis.*?(=head1 Description)) ($1)s;
     }

    $source =~ s/\n+=head1 Description.+?\n+1;\n+/\n\n$doc\n1;\n/gs;            # Edit module source from =head1 description to final 1;

    if ($source ne $Source)                                                     # Save source only if it has changed and came from a file
     {overWriteFile(filePathExt($perlModule, qq(backup)), $Source);             # Backup module source
      overWriteFile($perlModule, $source);                                      # Write updated module source
     }
   }

  $doc
 } # updateDocumentation

sub docUserFlags($$$$)                                                          #P Generate documentation for a method by calling the extractDocumentationFlags method in the package being documented, passing it the flags for a method and the name of the method. The called method should return the documentation to be inserted for the named method.
 {my ($flags, $perlModule, $package, $name) = @_;                               # Flags, file containing documentation, package containing documentation, name of method to be processed
  my $s = <<END;
${package}::extractDocumentationFlags("$flags", "$name");
END

  use Data::Dump qw(dump);
  my $r = eval $s;
  confess "$s\n". dump($@, $!) if $@;
  $r
 }

sub updatePerlModuleDocumentation($)                                            #P Update the documentation in a B<$perlModule> and display said documentation in a web browser.
 {my ($perlModule) = @_;                                                        # File containing the code of the perl module
  -e $perlModule or confess "No such file:\n$perlModule\n";
  updateDocumentation($perlModule);                                             # Update documentation

  zzz("pod2html --infile=$perlModule --outfile=zzz.html && ".                   # View documentation
      " opera zzz.html && ".
      " (sleep 5 && rm zzz.html pod2htmd.tmp) &");
 }

sub extractPythonDocumentationFromFiles(@)                                      #P Extract python documentation from the specified files
 {my (@sources) = @_;                                                           # Python source files

  my $docRe = qr(['"]{3});                                                      # Doc string marker

  my sub formatDocString($)                                                     # Format a doc string
   {my ($s) = @_;                                                               # String
    return $s;
    return '' unless $s;
    $s =~ s(input\s*:)  (<p><b>Input</b>:)gsi;
    $s =~ s(output\s*:) (<p><b>Output</b>:)gsi;
    $s =~ s(return\s*:) (<p><b>Return</b>:)gsi;
    $s =~ s(Parameters\s*\-+) (<p><b>Parameters</b>:)gsi;
    $s =~ s(Returns\s*\-+)    (<p><b>Returns</b>:)gsi;
    $s =~ s(\.?\s*\Z) (.)s;
    $s
   };

  my %parameters;                                                               # Parameters for each def
  my %comments;                                                                 # Comments for each def
  my %tests;                                                                    # Tests for each def
  my %testsCommon;                                                              # Common line for tests
  my %classDefinitions;                                                         # Class definitions
  my %classFiles;                                                               # Class files
  my %errors;                                                                   # Errors by source file

  for my $source(@sources)                                                      # Each source file
   {my @text = readFile($source);                                               # Read source file
    my $lines = @text;
    my $class = fne $source;

    my sub currentLine {$lines - @text};                                        # Current line number

    my sub getDocString                                                         # Get a doc string
     {my @c;

      my sub strip                                                              # Strip leading and trailing quotes
       {return unless @c;
        $c[0]  =~ s(\A\s*$docRe) ();
        $c[-1] =~ s($docRe\s*\Z) ();
        $c[-1] =~ s(\.?\s*\Z) (.);
        join "\n", @c
       };

      if (my $c = shift @text)                                                  # Doc string
       {if ($c =~ m(\A\s*$docRe.*\S))                                           # Quotes and text on same line
         {@c = $c;
          while(@text and $c !~ m($docRe\s*\Z)i)
           {push @c, $c = shift @text;
           }
          return strip
         }
        elsif ($c =~ m(\A\s*$docRe\s*\Z))                                       # Just quotes
         {@c = $c;
          while(@text and $text[0] !~ m($docRe\s*\Z)i)
           {push @c, shift @text;
           }
          return strip
         }
       }
      q()
     };

    my sub error(@)                                                             # Record an error
     {my (@e) = @_;                                                             # Error strings
      push $errors{$source}->@*, join ' ', @e;
     };

    while(@text)                                                                # Parse text of module
     {my $text = shift @text;

      if ($text =~ m(\A\s*def\s+(.*?)\((.*?)\)\s*:.*?#(\w*)\s+(.*))i)           # Def  function(parameter1 =1, parameter2 = 2) :  # first, second
       {my ($def, $parameters, $attributes, $parameterDefinitions) = @{^CAPTURE};

        my @p = split m/\s*,\s*/, $parameters;
        my @d = split m/\s*,\s*/, $parameterDefinitions;
        my $p = @p; my $d = @d;
        if ($p != $d)
         {my $l = currentLine;
          error qq(Number of parameters specified: $d does not equal),
                qq(number of parameters documented: $d on line: $l)
         }
        else
         {for my $p(@p)
           {my $c = ucfirst shift @d;
               $c =~ s(\.?\s*\Z) ()s;
            push $parameters{$class}{$def}->@*, [$p, $c];
           }
         }

        $comments{$class}{$def} = getDocString
       }
      elsif ($text =~ m(\A\s*def\s+(.*?)\((.*?)\)\s*:)i)                        # Def  function(parameter1 =1, parameter2 = 2) :
       {my ($def, $parameters) = @{^CAPTURE};
        my $doc = $comments{$class}{$def} = getDocString;

        my @p = split m/\s*,\s*/, $parameters;                                  # Parameters defined by a Python subroutine
        my %p;

        for my $line(split m/\n/, $doc)                                         # Check for parameter definitions
         {if ($line =~ m(\A\s*:\s*param\s*(.*?)\s*:\s*(.*?)\s*\Z))
           {my ($parm, $comment) = @{^CAPTURE};
            push $parameters{$class}{$def}->@*, [$parm, $comment];
            $parm =~ s(\A\s*(bool|str)\s*) ()s;                                 # Remove parameter type when present to get parameter name
            $p{$parm} = $comment;
           }
         }

        if (keys %p)                                                            # Use parameter definitions if present
         {if (@p != keys %p)
           {error q(Differing numbers of parameters described in comment and code);
           }
          for my $p(@p)
           {if (!$p{$p})
             {error qq(Parameter $p not described by :param);
             }
            delete $p{$p}
           }
          if (keys %p)
           {my $b = join ', ', sort keys %p;
            error qq(Parameters $b defined by :param but not present in defn);
           }
         }
        else                                                                    # Use parameter definitions from a Python subroutine
         {push $parameters{$class}{$def}->@*, [@p];
         }
        error qq(No parameter definitions for $class.$def)
       }
      elsif ($text =~ m(\A\s*class\s+(.*?)\s*:))                                # Class - assume there is no more than one class per file for the moment
       {$classFiles{$class}       = $class = $1;
        $classDefinitions{$class} = getDocString
       }
      elsif ($text =~ m(\A\s*if\s+1\s*:\s*#T(\w+)))                             # Test as if 1: statement
       {my $test = $1;
        my @test;
        while(@text and $text[0] !~ m(\A\s*\Z))
         {push @test, trim shift @text;
         }
        push $tests{$class}{$test}->@*, @test;
       }
      elsif ($text =~ m(\A(.*?)#T(\w+)))                                        # Test on a single line
       {my ($text, $test) = @{^CAPTURE};;
        push @{$testsCommon{$test}}, $text;
       }
     }
    error qq(No class in file $source) unless $class
   }

  my $d = genHash(q(Data::Table::Text::Python::Documentation),                  # Documentation extracted from Python source files
    parameters       => \%parameters,                                           # Parameters for each def
    comments         => \%comments,                                             # Comments for each def
    tests            => \%tests,                                                # Tests for each def
    testsCommon      => \%testsCommon,                                          # Common line for tests
    classDefinitions => \%classDefinitions,                                     # Class definitions
    classFiles       => \%classFiles,                                           # Class files
    errors           => \%errors,                                               # Errors encountered
   );

  my %opCodes =                                                                 # Translate these opcodes
   (neg      => q(- ) ,
    abs      => q(abs),
    eq       => q(==) ,
    iadd     => q(+=) ,
    add      => q(+ ) ,
    isub     => q(-=) ,
    sub      => q(- ) ,
    imul     => q(*=) ,
    mul      => q(* ) ,
    itruediv => q(/=) ,
    truediv  => q(/ ) );

  my sub classComment($)                                                        # Comment describing a class
   {my ($class) = @_;                                                           # Class
    $d->classDefinitions->{$class} // q()
   };

  my @h;                                                                        # Generated mark down

  push @h, <<END;
<h1>Table of contents</h1>
<p><table cellpadding=10>
END
  for my $class(sort keys $d->parameters->%*)                                   # Table of contents
   {my $comment = formatDocString classComment($class);
    my $m = stringMd5Sum $class;
    push @h, <<END;
<tr><td><a href="#$m">$class</a><td>$comment
END
   }
  push @h, <<END;
</table>
END

  for my $class(sort keys $d->parameters->%*)                                   # Each class
   {my $comment = formatDocString classComment $class;
    my $m = stringMd5Sum $class;
    push @h, <<END;
<h1 id="$m">Class: $class</h1>
<p>$comment</p>
END

    for my $defn(sort keys $d->parameters->{$class}->%*)                        # Each class method
     {my $comment = formatDocString $d->comments->{$class}{$defn};
      my $title = $defn;
      my $shortOp = $defn =~ s(_) ()gr;
      if (my $op = $opCodes{$shortOp})
       {$title .= " **$op**" unless $op eq $shortOp;
       }
      push @h, trim <<END;
<h2>$title</h2>
$comment
<h4>Parameters</h4>
<table cellpadding=10>
<tr><th>Name<th>Description
END
      if (my $parameters = $d->parameters->{$class}{$defn})                     # Parameters
       {for my $p(@$parameters)
         {my ($n, $c) = (@$p, (q()) x 2);
          push @h, <<END;
<tr><td><b><i>$n</i></b><td>$c
END
         }
       }
      push @h, <<END;
</table>
END
      my $examples = join "\n", map {nws $_} $d->tests->{$class}{$defn}->@*;

      push @h, trim <<END;                                                      # Example
<h4>Examples</h4>
<pre language="python">
$examples
</pre>
<hr>
END
     }
   }

  if (my $errors = $d->errors)                                                  # Errors by source file
   {push @h, q(<h1>Possible improvements to documentation</h1>);

    for my $file(sort keys %$errors)                                            # Each file with errors
     {push @h, <<END;
<h2>$file</h2>
<p><table cellpadding=10>
END
      for my $error($$errors{$file}->@*)
       {push @h, <<END;
<tr><td>$error
END
       }
      push @h, <<END;
</table>
END
     }
   }

  join "\n", @h;
 } # extractPythonDocumentationFromFiles

#-------------------------------------------------------------------------------
# Export - eeee
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

# containingFolder

@ISA          = qw(Exporter);
@EXPORT       = qw(formatTable);
@EXPORT_OK    = qw(
 absFromAbsPlusRel addCertificate addLValueScalarMethods adopt appendFile
 arrayProduct arraySum arrayTimes arrayToHash asciiToHexString assertPackageRefs
 assertRef awsCurrentAvailabilityZone awsCurrentInstanceId
 awsCurrentInstanceType awsCurrentIp awsCurrentLinuxSpotPrices awsCurrentRegion
 awsEc2DescribeInstancesGetIPAddresses
 awsEc2CreateImage awsEc2DescribeImages awsEc2DescribeInstanceType
 awsEc2DescribeInstances awsEc2DescribeInstancesGetIPAddresses
 awsEc2DescribeSpotInstances awsEc2FindImagesWithTagValue
 awsEc2InstanceIpAddress awsEc2ReportSpotInstancePrices
 awsEc2RequestSpotInstances awsExecCli awsExecCliJson
 awsIp awsMetaData awsParallelGatherFolder
 awsParallelIpAddresses awsParallelPrimaryInstanceId awsParallelPrimaryIpAddress
 awsParallelProcessFiles awsParallelSecondaryIpAddresses awsParallelSpreadFolder
 awsR53a awsR53aaaa
 binModeAllUtf8 boldString boldStringUndo
 call callSubInOverlappedParallel callSubInParallel checkFile checkFilePath
 checkFilePathDir checkFilePathExt checkKeys childPids chooseStringAtRandom
 clearFolder cmpArrays confirmHasCommandLineCommand
 containingFolderName containingPowerOfTwo contains
 convertDocxToFodt convertImageToJpx convertPerlToJavaScript convertUnicodeToXml copyBinaryFile
 copyBinaryFileMd5Normalized copyBinaryFileMd5NormalizedCreate
 copyBinaryFileMd5NormalizedGetCompanionContent copyFile copyFileFromRemote
 copyFileMd5Normalized copyFileMd5NormalizedCreate copyFileMd5NormalizedDelete
 copyFileMd5NormalizedGetCompanionContent copyFileMd5NormalizedName
 copyFileToFolder copyFileToRemote copyFolder copyFolderToRemote
 countFileExtensions countFileTypes countOccurencesInString createEmptyFile
 currentDirectory currentDirectoryAbove cutOutImagesInFodtFile
 dateStamp dateTimeStamp dateTimeStampName ddd deSquareArray decodeBase64
 decodeJson deduplicateSequentialWordsInString detagString
 downloadGitHubPublicRepo dumpFile dumpFileAsJson dumpGZipFile
 dumpTempFile dumpTempFileAsJson
 enclosedReversedString enclosedReversedStringUndo enclosedString
 enclosedStringUndo encodeBase64 encodeJson evalFile evalGZipFile
 execPerlOnRemote expandNewLinesInDocumentation expandWellKnownUrlsInDitaFormat
 expandWellKnownUrlsInHtmlFormat expandWellKnownWordsAsUrlsInHtmlFormat expandWellKnownWordsAsUrlsInMdFormat
 expandWellKnownUrlsInHtmlFromPerl expandWellKnownUrlsInPod2Html
 expandWellKnownUrlsInPerlFormat extractCodeBlock
 extractPythonDocumentationFromFiles evalFileAsJson
 fe fff fileInWindowsFormat fileLargestSize fileList fileMd5Sum fileModTime
 fileOutOfDate filePath filePathDir filePathExt fileSize findDirs
 findFileWithExtension findFiles firstFileThatExists firstNChars
 flattenArrayAndHashValues fn fne folderSize formatHtmlAndTextTables
 forEachKeyAndValue
 formatHtmlAndTextTablesWaitPids formatHtmlTable formatHtmlTablesIndex
 formatSourcePodAsHtml
 formatString formatTableBasic formattedTablesReport fp fpd fpe fpf fpn
 fullFileName fullyQualifiedFile fullyQualifyFile
 genClass genHash genLValueArrayMethods genLValueHashMethods
 genLValueScalarMethods genLValueScalarMethodsWithDefaultValues getSubName
 guidFromMd5 guidFromString
 hexToAsciiString hostName htmlToc
 imageSize indentString indexOfMax indexOfMin intersectionOfHashKeys
 intersectionOfHashesAsArrays invertHashOfHashes ipAddressViaArp isBlank
 ipAddressOfHost
 isFileUtf8 isSubInPackage
 javaPackage javaPackageAsFileName javaScriptExports
 keyCount
 lll loadArrayArrayFromLines loadArrayFromLines loadArrayHashFromLines loadHash
 loadHashArrayFromLines loadHashFromLines loadHashHashFromLines
 lengthOfLongestSubArray lpad
 makeDieConfess makePath makePathRemote matchPath mathematicalBoldItalicString
 mathematicalBoldItalicStringUndo mathematicalBoldString
 mathematicalBoldStringUndo mathematicalItalicString mathematicalMonoSpaceString
 mathematicalMonoSpaceStringUndo mathematicalSansSerifBoldItalicString
 mathematicalSansSerifBoldItalicStringUndo mathematicalSansSerifBoldString
 mathematicalSansSerifBoldStringUndo mathematicalSansSerifItalicString
 mathematicalSansSerifItalicStringUndo mathematicalSansSerifString
 mathematicalSansSerifStringUndo max md5FromGuid mergeFolder
 mergeFolderFromRemote microSecondsSinceEpoch min mmm
 moveFileNoClobber moveFileWithClobber
 nameFromFolder nameFromString nameFromStringRestrictedToTitle newProcessStarter
 newServiceIncarnation newUdsr newUdsrClient newUdsrServer numberOfCpus
 numberOfLinesInFile numberOfLinesInString nws
 onAws onAwsPrimary onAwsSecondary overWriteBinaryFile overWriteFile
 overrideAndReabsorbMethods owf
 overWriteHtmlFile overWritePerlCgiFile
 pad ppp parseCommandLineArguments parseDitaRef parseFileName
 parseIntoWordsAndStrings parseXmlDocType partitionStringsOnPrefixBySize
 powerOfTwo printQw processFilesInParallel processJavaFilesInParallel
 printPerlDataAsXml
 processSizesInParallel
 quoteFile
 randomizeArray
 readBinaryFile readFile readFileFromRemote readFiles readGZipFile readStdIn readUtf16File
 rectangularArray rectangularArray2
 relFromAbsAgainstAbs reloadHashes removeBOM removeDuplicatePrefixes
 removeFilePathsFromStructure removeFilePrefix removeFoldersFromDataStructure
 replaceStringWithString reportAttributeSettings reportAttributes
 reportExportableMethods reportReplacableMethods reportSettings retrieveFile
 runInParallel runInSquareRootParallel
 s3DownloadFolder s3FileExists s3ListFilesAndSizes s3ReadFile s3ReadString
 s3WriteFile s3WriteString s3ZipFolder s3ZipFolders saveCodeToS3 saveSourceToS3
 saveAwsDomain saveAwsIp
 searchDirectoryTreesForMatchingFiles searchDirectoryTreeForSubFolders setFileExtension setIntersection
 setIntersectionOfArraysOfStrings setIntersectionOverUnion setPackageSearchOrder
 setPartitionOnIntersectionOverUnion
 setPartitionOnIntersectionOverUnionOfHashStringSets
 setPartitionOnIntersectionOverUnionOfHashStringSetsInParallel
 setPartitionOnIntersectionOverUnionOfSetsOfWords
 setPartitionOnIntersectionOverUnionOfStringSets setPermissionsForFile setUnion
 showGotVersusWanted
 squareArray startProcess storeFile stringMd5Sum
 stringsAreNotEqual subScriptString subScriptStringUndo sumAbsAndRel
 summarizeColumn superScriptString superScriptStringUndo swapFilePrefix
 swapFolderPrefix syncFromS3InParallel syncToS3InParallel
 temporaryDirectory temporaryFile temporaryFolder timeStamp
 transitiveClosure trim
 unbless
 unionOfHashKeys unionOfHashesAsArrays uniqueNameFromFile updateDocumentation
 updatePerlModuleDocumentation userId
 versionCode versionCodeDashed
 waitForAllStartedProcessesToFinish writeBinaryFile writeFile writeTempFile writeFileToRemote
 writeFiles writeGZipFile writeStructureTest wwwDecode wwwEncode
 wwwHeader wwwGitHubAuth
 xxx xxxr
 yyy
 zzz
 );

if (0)                                                                          # Format exports
 {my $width = 80;
  binModeAllUtf8;
  my %e = map {$_=>1} @EXPORT_OK;
  my @e = sort keys %e;
  my @r = '';
  for my $i(keys @e)
   {my $e =  $e[$i];
    my $E = $i ? $e[$i-1] : q( );
    if (length($r[-1]) + 1 + length($e) > $width or
        substr($e, 0, 1) ne substr($E, 0, 1))
     {push @r, '';
     }
    $r[-1] .= qq( $e);
   }
  say STDERR "qw(", join("\n", @r);
  exit;
 }

%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

#D
# podDocumentation
#C mim@cpan.org Testing on windows

=pod

=encoding utf-8

=head1 Name

Data::Table::Text - Write data in tabular text format.

=for html
<p><a href="https://github.com/philiprbrenan/DataTableText"><img src="https://github.com/philiprbrenan/DataTableText/workflows/Test/badge.svg"></a>

=head1 Synopsis

  use Data::Table::Text;

# Print a table:

  my $d =
   [[qq(a), qq(b\nbb), qq(c\ncc\nccc\n)],
    [qq(1), qq(1\n22), qq(1\n22\n333\n)],
   ];

  my $t = formatTable($d, [qw(A BB CCC)]);

  ok $t eq <<END;
     A  BB  CCC
  1  a  b   c
        bb  cc
            ccc
  2  1   1    1
        22   22
            333
  END

# Print a table containing tables and make it into a report:

  my $D = [[qq(See the\ntable\nopposite), $t],
           [qq(Or\nthis\none),            $t],
          ];


  my $T = formatTable($D, [qw(Description Table)], head=><<END);
  Table of Tables.

  Table has NNNN rows each of which contains a table.
  END

  ok $T eq <<END;
  Table of Tables.

  Table has 2 rows each of which contains a table.


     Description  Table
  1  See the         A  BB  CCC
     table        1  a  b   c
     opposite           bb  cc
                            ccc
                  2  1   1    1
                        22   22
                            333
  2  Or              A  BB  CCC
     this         1  a  b   c
     one                bb  cc
                            ccc
                  2  1   1    1
                        22   22
                            333
  END

# Print an array of arrays:

  my $aa = formatTable
   ([[qw(A   B   C  )],
     [qw(AA  BB  CC )],
     [qw(AAA BBB CCC)],
     [qw(1   22  333)]],
     [qw (aa  bb  cc)]);

  ok $aa eq <<END;
     aa   bb   cc
  1  A    B    C
  2  AA   BB   CC
  3  AAA  BBB  CCC
  4    1   22  333
  END

# Print an array of hashes:

  my $ah = formatTable
   ([{aa=> "A",   bb => "B",   cc => "C" },
     {aa=> "AA",  bb => "BB",  cc => "CC" },
     {aa=> "AAA", bb => "BBB", cc => "CCC" },
     {aa=> 1,     bb => 22,    cc => 333 }]);

  ok $ah eq <<END;
     aa   bb   cc
  1  A    B    C
  2  AA   BB   CC
  3  AAA  BBB  CCC
  4    1   22  333
  END

# Print a hash of arrays:

  my $ha = formatTable
   ({""     => ["aa",  "bb",  "cc"],
     "1"    => ["A",   "B",   "C"],
     "22"   => ["AA",  "BB",  "CC"],
     "333"  => ["AAA", "BBB", "CCC"],
     "4444" => [1,      22,    333]},
     [qw(Key A B C)]
     );

  ok $ha eq <<END;
  Key   A    B    C
        aa   bb   cc
     1  A    B    C
    22  AA   BB   CC
   333  AAA  BBB  CCC
  4444    1   22  333
  END

# Print a hash of hashes:

  my $hh = formatTable
   ({a    => {aa=>"A",   bb=>"B",   cc=>"C" },
     aa   => {aa=>"AA",  bb=>"BB",  cc=>"CC" },
     aaa  => {aa=>"AAA", bb=>"BBB", cc=>"CCC" },
     aaaa => {aa=>1,     bb=>22,    cc=>333 }});

  ok $hh eq <<END;
        aa   bb   cc
  a     A    B    C
  aa    AA   BB   CC
  aaa   AAA  BBB  CCC
  aaaa    1   22  333
  END

# Print an array of scalars:

  my $a = formatTable(["a", "bb", "ccc", 4], [q(#), q(Col)]);

  ok $a eq <<END;
  #  Col
  0  a
  1  bb
  2  ccc
  3    4
  END

# Print a hash of scalars:

  my $h = formatTable({aa=>"AAAA", bb=>"BBBB", cc=>"333"}, [qw(Key Title)]);

  ok $h eq <<END;
  Key  Title
  aa   AAAA
  bb   BBBB
  cc     333
  END

=head1 Description

Write data in tabular text format.


Version 20210629.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Immediately useful methods

These methods are the ones most likely to be of immediate use to anyone using
this module for the first time:


L<absFromAbsPlusRel($a, $r)|/absFromAbsPlusRel($a, $r)>

Absolute file from an absolute file B<$a> plus a relative file B<$r>. In the event that the relative file $r is, in fact, an absolute file then it is returned as the result.

L<awsParallelProcessFiles($userData, $parallel, $results, $files, %options)|/awsParallelProcessFiles($userData, $parallel, $results, $files, %options)>

Process files in parallel across multiple L<Amazon Web Services|http://aws.amazon.com> instances if available or in series if not.  The data located by B<$userData> is transferred from the primary instance, as determined by L<awsParallelPrimaryInstanceId>, to all the secondary instances. B<$parallel> contains a reference to a sub, parameterized by array @_ = (a copy of the user data, the name of the file to process), which will be executed upon each session instance including the primary instance to update $userData. B<$results> contains a reference to a sub, parameterized by array @_ = (the user data, an array of results returned by each execution of $parallel), that will be called on the primary instance to process the results folders from each instance once their results folders have been copied back and merged into the results folder of the primary instance. $results should update its copy of $userData with the information received from each instance. B<$files> is a reference to an array of the files to be processed: each file will be copied from the primary instance to each of the secondary instances before parallel processing starts. B<%options> contains any parameters needed to interact with L<EC2|https://aws.amazon.com/ec2/>  via the L<Amazon Web Services Command Line Interface|https://aws.amazon.com/cli/>.  The returned result is that returned by sub $results.

L<clearFolder($folder, $limitCount, $noMsg)|/clearFolder($folder, $limitCount, $noMsg)>

Remove all the files and folders under and including the specified B<$folder> as long as the number of files to be removed is less than the specified B<$limitCount>. Sometimes the folder can be emptied but not removed - perhaps because it a link, in this case a message is produced unless suppressed by the optional B<$nomsg> parameter.

L<dateTimeStamp|/dateTimeStamp>

Year-monthNumber-day at hours:minute:seconds

L<execPerlOnRemote($code, $ip)|/execPerlOnRemote($code, $ip)>

Execute some Perl B<$code> on the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

L<filePathExt(@File)|/filePathExt(@File)>

Create a file name from a list of  names the last of which is assumed to be the extension of the file name. Identical to L<fpe|/fpe>.

L<fn($file)|/fn($file)>

Remove the path and extension from a file name.

L<formatTable($data, $columnTitles, @options)|/formatTable($data, $columnTitles, @options)>

Format various B<$data> structures as a table with titles as specified by B<$columnTitles>: either a reference to an array of column titles or a string each line of which contains the column title as the first word with the rest of the line describing that column.

Optionally create a report from the table using the report B<%options> described in L<formatTableCheckKeys>

L<genHash($bless, %attributes)|/genHash($bless, %attributes)>

Return a B<$bless>ed hash with the specified B<$attributes> accessible via L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> method calls. L<updateDocumentation|/updateDocumentation> will generate documentation at L<Hash Definitions> for the hash defined by the call to L<genHash|/genHash> if the call is laid out as in the example below.

L<readFile($file)|/readFile($file)>

Return the content of a file residing on the local machine interpreting the content of the file as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

L<readFileFromRemote($file, $ip)|/readFileFromRemote($file, $ip)>

Copy and read a B<$file> from the remote machine whose ip address is specified by B<$ip> or returned by L<awsIp> and return the content of $file interpreted as utf8 .

L<relFromAbsAgainstAbs($a, $b)|/relFromAbsAgainstAbs($a, $b)>

Relative file from one absolute file B<$a> against another B<$b>.

L<runInParallel($maximumNumberOfProcesses, $parallel, $results, @array)|/runInParallel($maximumNumberOfProcesses, $parallel, $results, @array)>

Process the elements of an array in parallel using a maximum of B<$maximumNumberOfProcesses> processes. sub B<&$parallel> is forked to process each array element in parallel. The results returned by the forked copies of &$parallel are presented as a single array to sub B<&$results> which is run in series. B<@array> contains the elements to be processed. Returns the result returned by &$results.

L<searchDirectoryTreeForSubFolders($folder)|/searchDirectoryTreeForSubFolders($folder)>

Search the specified directory under the specified folder for sub folders

L<searchDirectoryTreesForMatchingFiles(@FoldersandExtensions)|/searchDirectoryTreesForMatchingFiles(@FoldersandExtensions)>

Search the specified directory trees for the files (not folders) that match the specified extensions. The argument list should include at least one path name to be useful. If no file extensions are supplied then all the files below the specified paths are returned.  Arguments wrapped in [] will be unwrapped.

L<writeFile($file, $string)|/writeFile($file, $string)>

Write to a new B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>. Return the name of the $file written to on success else confess if the file already exists or any other error occurs.

L<writeFileToRemote($file, $string, $ip)|/writeFileToRemote($file, $string, $ip)>

Write to a new B<$file>, after creating a path to the file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8> then copy the $file to the remote server whose ip address is specified by B<$ip> or returned by L<awsIp>. Return the name of the $file on success else confess if the file already exists or any other error occurs.

L<xxxr($cmd, $ip)|/xxxr($cmd, $ip)>

Execute a command B<$cmd> via bash on the server whose ip address is specified by B<$ip> or returned by L<awsIp>. The command will be run using the userid listed in F<.ssh/config>




=head1 Time stamps

Date and timestamps as used in logs of long running commands.

=head2 dateTimeStamp()

Year-monthNumber-day at hours:minute:seconds


B<Example:>



  ok dateTimeStamp     =~ m(\A\d{4}-\d\d-\d\d at \d\d:\d\d:\d\d\Z), q(dts);         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 dateTimeStampName()

Date time stamp without white space.


B<Example:>



  ok dateTimeStampName =~ m(\A_on_\d{4}_\d\d_\d\d_at_\d\d_\d\d_\d\d\Z);             # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 dateStamp()

Year-monthName-day


B<Example:>



  ok dateStamp         =~ m(\A\d{4}-\w{3}-\d\d\Z);                                  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 versionCode()

YYYYmmdd-HHMMSS


B<Example:>



  ok versionCode       =~ m(\A\d{8}-\d{6}\Z);                                       # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 versionCodeDashed()

YYYY-mm-dd-HH:MM:SS


B<Example:>



  ok versionCodeDashed =~ m(\A\d{4}-\d\d-\d\d-\d\d:\d\d:\d\d\Z);                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 timeStamp()

hours:minute:seconds


B<Example:>



  ok timeStamp         =~ m(\A\d\d:\d\d:\d\d\Z);                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 microSecondsSinceEpoch()

Micro seconds since unix epoch.


B<Example:>



  ok microSecondsSinceEpoch > 47*365*24*60*60*1e6;                                  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 Command execution

Various ways of processing commands and writing results.

=head2 ddd(@data)

Dump data

     Parameter  Description
  1  @data      Messages

B<Example:>



  ddd "Hello";                                                                      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 fff($line, $file, @m)

Confess a message with a line position and a file that Geany will jump to if clicked on.

     Parameter  Description
  1  $line      Line
  2  $file      File
  3  @m         Messages

B<Example:>



  fff __LINE__, __FILE__, "Hello world";                                            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 lll(@messages)

Log messages with a time stamp and originating file and line number.

     Parameter  Description
  1  @messages  Messages

B<Example:>



  lll "Hello world";                                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mmm(@messages)

Log messages with a differential time in milliseconds and originating file and line number.

     Parameter  Description
  1  @messages  Messages

B<Example:>



  mmm "Hello world";                                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 xxx(@cmd)

Execute a shell command optionally checking its response. The command to execute is specified as one or more strings which are joined together after removing any new lines. Optionally the last string can be a regular expression that is used to test any non blank output generated by the execution of the command: if the regular expression fails the command and the command output are printed, else it is suppressed as being uninteresting. If such a regular expression is not supplied then the command and its non blank output lines are always printed.

     Parameter  Description
  1  @cmd       Command to execute followed by an optional regular expression to test the results

B<Example:>



   {ok xxx("echo aaa")       =~ /aaa/;                                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 xxxr($cmd, $ip)

Execute a command B<$cmd> via bash on the server whose ip address is specified by B<$ip> or returned by L<awsIp>. The command will be run using the userid listed in F<.ssh/config>

     Parameter  Description
  1  $cmd       Command string
  2  $ip        Optional ip address

B<Example:>


  if (0)

   {ok xxxr q(pwd);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head2 yyy($cmd)

Execute a block of shell commands line by line after removing comments - stop if there is a non zero return code from any command.

     Parameter  Description
  1  $cmd       Commands to execute separated by new lines

B<Example:>



    ok !yyy <<END;                                                                  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  echo aaa
  echo bbb
  END


=head2 zzz($cmd, $success, $returnCode, $message)

Execute lines of commands after replacing new lines with && then check that the pipeline execution results in a return code of zero and that the execution results match the optional regular expression if one has been supplied; confess() to an error if either check fails. To execute remotely, add "ssh ... 'echo start" as the first line and "echo end'" as the last line with the commands to be executed on the lines in between.

     Parameter    Description
  1  $cmd         Commands to execute - one per line with no trailing &&
  2  $success     Optional regular expression to check for acceptable results
  3  $returnCode  Optional regular expression to check the acceptable return codes
  4  $message     Message of explanation if any of the checks fail

B<Example:>



  ok zzz(<<END, qr(aaa\s*bbb)s);                                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  echo aaa
  echo bbb
  END


=head2 execPerlOnRemote($code, $ip)

Execute some Perl B<$code> on the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

     Parameter  Description
  1  $code      Code to execute
  2  $ip        Optional ip address

B<Example:>



    ok execPerlOnRemote(<<'END') =~ m(Hello from: t2.micro)i;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  #!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
  use Data::Table::Text qw(:all);

  say STDERR "Hello from: ", awsCurrentInstanceType;
  END


=head2 parseCommandLineArguments($sub, $args, $valid)

Call the specified B<$sub> after classifying the specified array of [arguments] in B<$args> into positional and keyword parameters. Keywords are always preceded by one or more B<-> and separated from their values by B<=>. $sub([$positional], {keyword=>value}) will be called  with a reference to an array of positional parameters followed by a reference to a hash of keywords and their values. The value returned by $sub will be returned to the caller. The keywords names will be validated if B<$valid> is either a reference to an array of valid keywords names or a hash of {valid keyword name => textual description}. Confess with a table of valid keywords definitions if $valid is specified and an invalid keyword argument is presented.

     Parameter  Description
  1  $sub       Sub to call
  2  $args      List of arguments to parse
  3  $valid     Optional list of valid parameters else all parameters will be accepted

B<Example:>



    my $r = parseCommandLineArguments {[@_]}  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     [qw( aaa bbb -c --dd --eee=EEEE -f=F), q(--gg=g g), q(--hh=h h)];
    is_deeply $r,
      [["aaa", "bbb"],
       {c=>undef, dd=>undef, eee=>"EEEE", f=>"F", gg=>"g g", hh=>"h h"},
      ];

  if (1)

   {my $r = parseCommandLineArguments  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     {ok 1;
      $_[1]
     }
     [qw(--aAa=AAA --bbB=BBB)], [qw(aaa bbb ccc)];
    is_deeply $r, {aaa=>'AAA', bbb=>'BBB'};
   }


=head2 call($sub, @our)

Call the specified B<$sub> in a separate child process, wait for it to complete, then copy back the named B<@our> variables from the child process to the calling parent process effectively freeing any memory used during the call.

     Parameter  Description
  1  $sub       Sub to call
  2  @our       Names of our variable names with preceding sigils to copy back

B<Example:>


    our $a = q(1);
    our @a = qw(1);
    our %a = (a=>1);
    our $b = q(1);
    for(2..4) {

      call {$a = $_  x 1e3; $a[0] = $_ x 1e2; $a{a} = $_ x 1e1; $b = 2;} qw($a @a %a);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      ok $a    == $_ x 1e3;
      ok $a[0] == $_ x 1e2;
      ok $a{a} == $_ x 1e1;
      ok $b    == 1;
     }


=head1 Files and paths

Operations on files and paths.

=head2 Statistics

Information about each file.

=head3 fileSize($file)

Get the size of a B<$file> in bytes.

     Parameter  Description
  1  $file      File name

B<Example:>


    my $f = writeFile("zzz.data", "aaa");


    ok fileSize($f) == 3;                                                           # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 fileLargestSize(@files)

Return the largest B<$file>.

     Parameter  Description
  1  @files     File names

B<Example:>


    my $d = temporaryFolder;
    my @f = map {owf(fpe($d, $_, q(txt)), 'X' x ($_ ** 2 % 11))} 1..9;


    my $f = fileLargestSize(@f);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok fn($f) eq '3', 'aaa';

  #  my $b = folderSize($d);                                                       # Needs du
  #  ok $b > 0, 'bbb';

    my $c = processFilesInParallel(
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12);

    ok 108 == $c, 'cc11';

    my $C = processSizesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, map {[fileSize($_), $_]} (@f) x 12;

    ok 108 == $C, 'cc2';

    my $J = processJavaFilesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12;

    ok 108 == $J, 'cc3';

    clearFolder($d, 12);


=head3 folderSize($folder)

Get the size of a B<$folder> in bytes.

     Parameter  Description
  1  $folder    Folder name

B<Example:>


    my $d = temporaryFolder;
    my @f = map {owf(fpe($d, $_, q(txt)), 'X' x ($_ ** 2 % 11))} 1..9;

    my $f = fileLargestSize(@f);
    ok fn($f) eq '3', 'aaa';


  #  my $b = folderSize($d);                                                       # Needs du  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  #  ok $b > 0, 'bbb';

    my $c = processFilesInParallel(
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12);

    ok 108 == $c, 'cc11';

    my $C = processSizesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, map {[fileSize($_), $_]} (@f) x 12;

    ok 108 == $C, 'cc2';

    my $J = processJavaFilesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12;

    ok 108 == $J, 'cc3';

    clearFolder($d, 12);


=head3 fileMd5Sum($file)

Get the Md5 sum of the content of a B<$file>.

     Parameter  Description
  1  $file      File or string

B<Example:>



    fileMd5Sum(q(/etc/hosts));                                                      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $s = join '', 1..100;
    my $m = q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum($s) eq $m;

    my $f = writeFile(undef, $s);

    ok fileMd5Sum($f) eq $m;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $f;

    ok guidFromString(join '', 1..100) eq
       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

    ok guidFromMd5(stringMd5Sum(join('', 1..100))) eq
       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

    ok md5FromGuid(q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de)) eq
                        q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum(q(𝝰 𝝱 𝝲)) eq q(3c2b7c31b1011998bd7e1f66fb7c024d);
  }

  if (1)
   {ok arraySum   (1..10) ==  55;
    ok arrayProduct(1..5) == 120;
    is_deeply[arrayTimes(2, 1..5)], [qw(2 4 6 8 10)];


=head3 guidFromMd5($m)

Create a guid from an md5 hash.

     Parameter  Description
  1  $m         Md5 hash

B<Example:>


    my $s = join '', 1..100;
    my $m = q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum($s) eq $m;

    my $f = writeFile(undef, $s);
    ok fileMd5Sum($f) eq $m;
    unlink $f;

    ok guidFromString(join '', 1..100) eq
       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);


    ok guidFromMd5(stringMd5Sum(join('', 1..100))) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

    ok md5FromGuid(q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de)) eq
                        q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum(q(𝝰 𝝱 𝝲)) eq q(3c2b7c31b1011998bd7e1f66fb7c024d);
  }

  if (1)
   {ok arraySum   (1..10) ==  55;
    ok arrayProduct(1..5) == 120;
    is_deeply[arrayTimes(2, 1..5)], [qw(2 4 6 8 10)];


=head3 md5FromGuid($G)

Recover an md5 sum from a guid.

     Parameter  Description
  1  $G         Guid

B<Example:>


    my $s = join '', 1..100;
    my $m = q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum($s) eq $m;

    my $f = writeFile(undef, $s);
    ok fileMd5Sum($f) eq $m;
    unlink $f;

    ok guidFromString(join '', 1..100) eq
       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

    ok guidFromMd5(stringMd5Sum(join('', 1..100))) eq
       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);


    ok md5FromGuid(q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de)) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

                        q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum(q(𝝰 𝝱 𝝲)) eq q(3c2b7c31b1011998bd7e1f66fb7c024d);
  }

  if (1)
   {ok arraySum   (1..10) ==  55;
    ok arrayProduct(1..5) == 120;
    is_deeply[arrayTimes(2, 1..5)], [qw(2 4 6 8 10)];


=head3 guidFromString($string)

Create a guid representation of the L<MD5 sum|https://en.wikipedia.org/wiki/MD5> of the content of a string.

     Parameter  Description
  1  $string    String

B<Example:>


    my $s = join '', 1..100;
    my $m = q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum($s) eq $m;

    my $f = writeFile(undef, $s);
    ok fileMd5Sum($f) eq $m;
    unlink $f;


    ok guidFromString(join '', 1..100) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

    ok guidFromMd5(stringMd5Sum(join('', 1..100))) eq
       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

    ok md5FromGuid(q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de)) eq
                        q(ef69caaaeea9c17120821a9eb6c7f1de);

    ok stringMd5Sum(q(𝝰 𝝱 𝝲)) eq q(3c2b7c31b1011998bd7e1f66fb7c024d);
  }

  if (1)
   {ok arraySum   (1..10) ==  55;
    ok arrayProduct(1..5) == 120;
    is_deeply[arrayTimes(2, 1..5)], [qw(2 4 6 8 10)];


=head3 fileModTime($file)

Get the modified time of a B<$file> as seconds since the epoch.

     Parameter  Description
  1  $file      File name

B<Example:>



  ok fileModTime($0) =~ m(\A\d+\Z)s;                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 fileOutOfDate($make, $target, @source)

Calls the specified sub B<$make> for each source file that is missing and then again against the B<$target> file if any of the B<@source> files were missing or the $target file is older than any of the @source files or if the target does not exist. The file name is passed to the sub each time in $_. Returns the files to be remade in the order they should be made.

     Parameter  Description
  1  $make      Make with this sub
  2  $target    Target file
  3  @source    Source files

B<Example:>


    my @Files = qw(a b c);
    my @files = (@Files, qw(d));
    writeFile($_, $_), sleep 1 for @Files;

    my $a = '';

    my @a = fileOutOfDate {$a .= $_} q(a), @files;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $a eq 'da';
    is_deeply [@a], [qw(d a)];

    my $b = '';

    my @b = fileOutOfDate {$b .= $_} q(b), @files;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $b eq 'db';
    is_deeply [@b], [qw(d b)];

    my $c = '';

    my @c = fileOutOfDate {$c .= $_} q(c), @files;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $c eq 'dc';
    is_deeply [@c], [qw(d c)];

    my $d = '';

    my @d = fileOutOfDate {$d .= $_} q(d), @files;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $d eq 'd';
    is_deeply [@d], [qw(d)];


    my @A = fileOutOfDate {} q(a), @Files;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my @B = fileOutOfDate {} q(b), @Files;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my @C = fileOutOfDate {} q(c), @Files;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply [@A], [qw(a)];
    is_deeply [@B], [qw(b)];
    is_deeply [@C], [];
    unlink for @Files;


=head3 firstFileThatExists(@files)

Returns the name of the first file from B<@files> that exists or B<undef> if none of the named @files exist.

     Parameter  Description
  1  @files     Files to check

B<Example:>


    my $d = temporaryFolder;


    ok $d eq firstFileThatExists("$d/$d", $d);                                      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 fileInWindowsFormat($file)

Convert a unix B<$file> name to windows format

     Parameter  Description
  1  $file      File

B<Example:>


  if (1)

   {ok fileInWindowsFormat(fpd(qw(/a b c d))) eq q(\a\b\c\d\\);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head2 Components

File names and components.

=head3 Fusion

Create file names from file name components.

=head4 filePath(@file)

Create a file name from a list of  names. Identical to L<fpf|/fpf>.

     Parameter  Description
  1  @file      File name components

B<Example:>



    is_deeply filePath   (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply filePathDir(qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";
    is_deeply filePathDir('', qw(aaa))              , prefferedFileName "aaa/";
    is_deeply filePathDir('')                       , prefferedFileName "";
    is_deeply filePathExt(qw(aaa xxx))              , prefferedFileName "aaa.xxx";
    is_deeply filePathExt(qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";

    is_deeply fpd        (qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";
    is_deeply fpf        (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";
    is_deeply fpe        (qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";


B<fpf> is a synonym for L<filePath|/filePath>.


=head4 filePathDir(@file)

Create a folder name from a list of  names. Identical to L<fpd|/fpd>.

     Parameter  Description
  1  @file      Directory name components

B<Example:>


    is_deeply filePath   (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";

    is_deeply filePathDir(qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply filePathDir('', qw(aaa))              , prefferedFileName "aaa/";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply filePathDir('')                       , prefferedFileName "";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply filePathExt(qw(aaa xxx))              , prefferedFileName "aaa.xxx";
    is_deeply filePathExt(qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";

    is_deeply fpd        (qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";
    is_deeply fpf        (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";
    is_deeply fpe        (qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";


B<fpd> is a synonym for L<filePathDir|/filePathDir>.


=head4 filePathExt(@File)

Create a file name from a list of  names the last of which is assumed to be the extension of the file name. Identical to L<fpe|/fpe>.

     Parameter  Description
  1  @File      File name components and extension

B<Example:>


    is_deeply filePath   (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";
    is_deeply filePathDir(qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";
    is_deeply filePathDir('', qw(aaa))              , prefferedFileName "aaa/";
    is_deeply filePathDir('')                       , prefferedFileName "";

    is_deeply filePathExt(qw(aaa xxx))              , prefferedFileName "aaa.xxx";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply filePathExt(qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply fpd        (qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";
    is_deeply fpf        (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";
    is_deeply fpe        (qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";


B<fpe> is a synonym for L<filePathExt|/filePathExt>.


=head3 Fission

Get file name components from a file name.

=head4 fp($file)

Get the path from a file name.

     Parameter  Description
  1  $file      File name

B<Example:>



  ok fp (prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(a/b/);                               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 fpn($file)

Remove the extension from a file name.

     Parameter  Description
  1  $file      File name

B<Example:>



  ok fpn(prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(a/b/c.d);                            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 fn($file)

Remove the path and extension from a file name.

     Parameter  Description
  1  $file      File name

B<Example:>



  ok fn (prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(c.d);                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 fne($file)

Remove the path from a file name.

     Parameter  Description
  1  $file      File name

B<Example:>



  ok fne(prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(c.d.e);                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 fe($file)

Get the extension of a file name.

     Parameter  Description
  1  $file      File name

B<Example:>



  ok fe (prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(e);                                  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 checkFile($file)

Return the name of the specified file if it exists, else confess the maximum extent of the path that does exist.

     Parameter  Description
  1  $file      File to check

B<Example:>


    my $d = filePath   (my @d = qw(a b c d));

    my $f = filePathExt(qw(a b c d e x));

    my $F = filePathExt(qw(a b c e d));

    createEmptyFile($f);


    ok  eval{checkFile($d)};                                                        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    ok  eval{checkFile($f)};                                                        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 quoteFile($file)

Quote a file name.

     Parameter  Description
  1  $file      File name

B<Example:>



  is_deeply quoteFile(fpe(qw(a "b" c))), onWindows ? q("a\\\"b\".c") : q("a/\"b\".c");   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 removeFilePrefix($prefix, @files)

Removes a file B<$prefix> from an array of B<@files>.

     Parameter  Description
  1  $prefix    File prefix
  2  @files     Array of file names

B<Example:>



  is_deeply [qw(a b)], [&removeFilePrefix(qw(a/ a/a a/b))];                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  is_deeply [qw(b)],   [&removeFilePrefix("a/", "a/b")];                            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 swapFilePrefix($file, $known, $new)

Swaps the start of a B<$file> name from a B<$known> name to a B<$new> one if the file does in fact start with the $known name otherwise returns the original file name as it is. If the optional $new prefix is omitted then the $known prefix is removed from the $file name.

     Parameter  Description
  1  $file      File name
  2  $known     Existing prefix
  3  $new       Optional new prefix defaults to q()

B<Example:>



  ok swapFilePrefix(q(/aaa/bbb.txt), q(/aaa/), q(/AAA/)) eq q(/AAA/bbb.txt);        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 setFileExtension($file, $extension)

Given a B<$file>, change its extension to B<$extension>. Removes the extension if no $extension is specified.

     Parameter   Description
  1  $file       File name
  2  $extension  Optional new extension

B<Example:>



  ok setFileExtension(q(.c),     q(d)) eq q(.d);                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok setFileExtension(q(b.c),    q(d)) eq q(b.d);                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok setFileExtension(q(/a/b.c), q(d)) eq q(/a/b.d);                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 swapFolderPrefix($file, $known, $new)

Given a B<$file>, swap the folder name of the $file from B<$known> to B<$new> if the file $file starts with the $known folder name else return the $file as it is.

     Parameter  Description
  1  $file      File name
  2  $known     Existing prefix
  3  $new       New prefix

B<Example:>


    my $g = fpd(qw(a b c d));
    my $h = fpd(qw(a b cc dd));
    my $i = fpe($g, qw(aaa txt));


    my $j = swapFolderPrefix($i, $g, $h);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $j =~ m(a/b/cc/dd/)s     unless onWindows;
    ok $j =~ m(a\\b\\cc\\dd\\)s if     onWindows;


=head4 fullyQualifiedFile($file, $prefix)

Check whether a B<$file> name is fully qualified or not and, optionally, whether it is fully qualified with a specified B<$prefix> or not.

     Parameter  Description
  1  $file      File name to test
  2  $prefix    File name prefix

B<Example:>



  ok  fullyQualifiedFile(q(/a/b/c.d));                                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok  fullyQualifiedFile(q(/a/b/c.d), q(/a/b));                                     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok !fullyQualifiedFile(q(/a/b/c.d), q(/a/c));                                     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok !fullyQualifiedFile(q(c.d));                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 fullyQualifyFile($file)

Return the fully qualified name of a file.

     Parameter  Description
  1  $file      File name

B<Example:>


  if (0)

   {ok fullyQualifyFile(q(perl/cpan)) eq q(/home/phil/perl/cpan/);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head4 removeDuplicatePrefixes($file)

Remove duplicated leading directory names from a file name.

     Parameter  Description
  1  $file      File name

B<Example:>



  ok q(a/b.c) eq removeDuplicatePrefixes("a/a/b.c");                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok q(a/b.c) eq removeDuplicatePrefixes("a/b.c");                                  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok q(b.c) eq removeDuplicatePrefixes("b.c");                                      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head4 containingFolderName($file)

The name of a folder containing a file

     Parameter  Description
  1  $file      File name

B<Example:>



  ok containingFolderName(q(/a/b/c.d)) eq q(b);                                     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 Position

Position in the file system.

=head3 currentDirectory()

Get the current working directory.


B<Example:>



    currentDirectory;                                                               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 currentDirectoryAbove()

Get the path to the folder above the current working folder.


B<Example:>



    currentDirectoryAbove;                                                          # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 parseFileName($file)

Parse a file name into (path, name, extension) considering .. to be always part of the path and using B<undef> to mark missing components.  This differs from (fp, fn, fe) which return q() for missing components and do not interpret . or .. as anything special

     Parameter  Description
  1  $file      File name to parse

B<Example:>


  if (1)

   {is_deeply [parseFileName "/home/phil/test.data"], ["/home/phil/", "test", "data"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "/home/phil/test"],      ["/home/phil/", "test"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "phil/test.data"],       ["phil/",       "test", "data"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "phil/test"],            ["phil/",       "test"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "test.data"],            [undef,         "test", "data"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "phil/"],                [qw(phil/)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "/phil"],                [qw(/ phil)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "/"],                    [qw(/)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "/var/www/html/translations/"], [qw(/var/www/html/translations/)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "a.b/c.d.e"],            [qw(a.b/ c.d e)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "./a.b"],                [qw(./ a b)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseFileName "./../../a.b"],          [qw(./../../ a b)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head3 fullFileName()

Full name of a file.


B<Example:>



    fullFileName(fpe(qw(a txt)));                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 relFromAbsAgainstAbs($a, $b)

Relative file from one absolute file B<$a> against another B<$b>.

     Parameter  Description
  1  $a         Absolute file to be made relative
  2  $b         Against this absolute file.

B<Example:>



  ok "bbb.pl"                 eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/perl/aaa.pl");    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok "../perl/bbb.pl"         eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/java/aaa.jv");    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 absFromAbsPlusRel($a, $r)

Absolute file from an absolute file B<$a> plus a relative file B<$r>. In the event that the relative file $r is, in fact, an absolute file then it is returned as the result.

     Parameter  Description
  1  $a         Absolute file
  2  $r         Relative file

B<Example:>



  ok "/home/la/perl/aaa.pl"   eq absFromAbsPlusRel("/home/la/perl/bbb",      "aaa.pl");                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok "/home/la/perl/aaa.pl"   eq absFromAbsPlusRel("/home/il/perl/bbb.pl",   "../../la/perl/aaa.pl");      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 absFile($file)

Return the name of the given file if it a fully qualified file name else returns B<undef>. See: L<fullyQualifiedFile> to check the initial prefix of the file name as well.

     Parameter  Description
  1  $file      File to test

B<Example:>



  ok "/aaa/"                  eq absFile(qw(/aaa/));                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 sumAbsAndRel(@files)

Combine zero or more absolute and relative names of B<@files> starting at the current working folder to get an absolute file name.

     Parameter  Description
  1  @files     Absolute and relative file names

B<Example:>



  ok "/aaa/bbb/ccc/ddd.txt"   eq sumAbsAndRel(qw(/aaa/AAA/ ../bbb/bbb/BBB/ ../../ccc/ddd.txt));   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 Temporary

Temporary files and folders

=head3 temporaryFile()

Create a new, empty, temporary file.


B<Example:>


    my $d = fpd(my $D = temporaryDirectory, qw(a));
    my $f = fpe($d, qw(bbb txt));
    ok !-d $d;
    eval q{checkFile($f)};
    my $r = $@;
    my $q = quotemeta($D);
    ok nws($r) =~ m(Can only find.+?: $q)s;
    makePath($f);
    ok -d $d;
    ok -d $D;
    rmdir $_ for $d, $D;

    my $e = temporaryFolder;                                                      # Same as temporyDirectory
    ok -d $e;
    clearFolder($e, 2);


    my $t = temporaryFile;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok  -f $t;
    unlink $t;
    ok !-f $t;

    if (0)
     {makePathRemote($e);                                                         # Make a path on the remote system
     }


=head3 temporaryFolder()

Create a new, empty, temporary folder.


B<Example:>



    my $D = temporaryFolder;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;

    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);
    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];

    my @D = findDirs($D);
    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];

    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
              ["a.txt", "b.txt", "c.txt"];

    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;

    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;

    my @g = fileList(qq($D/*/*.txt));
    ok @g == 3;

    clearFolder($D, 5);
    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;

    my $d = fpd(my $D = temporaryDirectory, qw(a));
    my $f = fpe($d, qw(bbb txt));
    ok !-d $d;
    eval q{checkFile($f)};
    my $r = $@;
    my $q = quotemeta($D);
    ok nws($r) =~ m(Can only find.+?: $q)s;
    makePath($f);
    ok -d $d;
    ok -d $D;
    rmdir $_ for $d, $D;


    my $e = temporaryFolder;                                                      # Same as temporyDirectory  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok -d $e;
    clearFolder($e, 2);

    my $t = temporaryFile;
    ok  -f $t;
    unlink $t;
    ok !-f $t;

    if (0)
     {makePathRemote($e);                                                         # Make a path on the remote system
     }


B<temporaryDirectory> is a synonym for L<temporaryFolder|/temporaryFolder>.


=head2 Find

Find files and folders below a folder.

=head3 findFiles($folder, $filter)

Find all the files under a B<$folder> and optionally B<$filter> the selected files with a regular expression.

     Parameter  Description
  1  $folder    Folder to start the search with
  2  $filter    Optional regular expression to filter files

B<Example:>


    my $D = temporaryFolder;
    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;

    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);

    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my @D = findDirs($D);
    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];

    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
              ["a.txt", "b.txt", "c.txt"];

    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;

    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;

    my @g = fileList(qq($D/*/*.txt));
    ok @g == 3;

    clearFolder($D, 5);
    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;


=head3 findDirs($folder, $filter)

Find all the folders under a B<$folder> and optionally B<$filter> the selected folders with a regular expression.

     Parameter  Description
  1  $folder    Folder to start the search with
  2  $filter    Optional regular expression to filter files

B<Example:>


    my $D = temporaryFolder;
    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;

    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);
    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];


    my @D = findDirs($D);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];

    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
              ["a.txt", "b.txt", "c.txt"];

    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;

    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;

    my @g = fileList(qq($D/*/*.txt));
    ok @g == 3;

    clearFolder($D, 5);
    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;


=head3 fileList($pattern)

Files that match a given search pattern interpreted by L<perlfunc/bsd_glob>.

     Parameter  Description
  1  $pattern   Search pattern

B<Example:>


    my $D = temporaryFolder;
    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;

    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);
    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];

    my @D = findDirs($D);
    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];

    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
              ["a.txt", "b.txt", "c.txt"];


    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;

    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;


    my @g = fileList(qq($D/*/*.txt));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok @g == 3;

    clearFolder($D, 5);
    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;


=head3 searchDirectoryTreesForMatchingFiles(@FoldersandExtensions)

Search the specified directory trees for the files (not folders) that match the specified extensions. The argument list should include at least one path name to be useful. If no file extensions are supplied then all the files below the specified paths are returned.  Arguments wrapped in [] will be unwrapped.

     Parameter              Description
  1  @FoldersandExtensions  Mixture of folder names and extensions

B<Example:>


    my $D = temporaryFolder;
    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;

    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);
    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];

    my @D = findDirs($D);
    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];


    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              ["a.txt", "b.txt", "c.txt"];

    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;

    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;

    my @g = fileList(qq($D/*/*.txt));
    ok @g == 3;

    clearFolder($D, 5);
    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;


=head3 searchDirectoryTreeForSubFolders($folder)

Search the specified directory under the specified folder for sub folders

     Parameter  Description
  1  $folder    The folder at which to start the search

B<Example:>


    my $D = temporaryFolder;
    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;

    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);
    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];

    my @D = findDirs($D);
    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];

    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
              ["a.txt", "b.txt", "c.txt"];

    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;


    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my @g = fileList(qq($D/*/*.txt));
    ok @g == 3;

    clearFolder($D, 5);
    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;


=head3 hashifyFolderStructure(@files)

Hashify a list of file names to get the corresponding folder structure.

     Parameter  Description
  1  @files     File names

B<Example:>



    is_deeply hashifyFolderStructure(qw(/a/a/a /a/a/b /a/b/a /a/b/b)),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     {"" => {a => {a => { a => "/a/a/a", b => "/a/a/b" },
                   b => { a => "/a/b/a", b => "/a/b/b" },
                  },
            },
     };


=head3 countFileExtensions(@folders)

Return a hash which counts the file extensions in and below the folders in the specified list.

     Parameter  Description
  1  @folders   Folders to search

B<Example:>



    countFileExtensions(q(/home/phil/perl/));                                       # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 countFileTypes($maximumNumberOfProcesses, @folders)

Return a hash which counts, in parallel with a maximum number of processes: B<$maximumNumberOfProcesses>, the results of applying the B<file> command to each file in and under the specified B<@folders>.

     Parameter                  Description
  1  $maximumNumberOfProcesses  Maximum number of processes to run in parallel
  2  @folders                   Folders to search

B<Example:>



    countFileTypes(4, q(/home/phil/perl/));                                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 matchPath($file)

Return the deepest folder that exists along a given file name path.

     Parameter  Description
  1  $file      File name

B<Example:>


    my $d = filePath   (my @d = qw(a b c d));


    ok matchPath($d) eq $d;                                                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 findFileWithExtension($file, @ext)

Find the first file that exists with a path and name of B<$file> and an extension drawn from <@ext>.

     Parameter  Description
  1  $file      File name minus extensions
  2  @ext       Possible extensions

B<Example:>


    my $f = createEmptyFile(fpe(my $d = temporaryFolder, qw(a jpg)));


    my $F = findFileWithExtension(fpf($d, q(a)), qw(txt data jpg));                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok $F eq "jpg";


=head3 clearFolder($folder, $limitCount, $noMsg)

Remove all the files and folders under and including the specified B<$folder> as long as the number of files to be removed is less than the specified B<$limitCount>. Sometimes the folder can be emptied but not removed - perhaps because it a link, in this case a message is produced unless suppressed by the optional B<$nomsg> parameter.

     Parameter    Description
  1  $folder      Folder
  2  $limitCount  Maximum number of files to remove to limit damage
  3  $noMsg       No message if the folder cannot be completely removed.

B<Example:>


    my $D = temporaryFolder;
    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;

    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);
    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];

    my @D = findDirs($D);
    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];

    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
              ["a.txt", "b.txt", "c.txt"];

    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;

    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;

    my @g = fileList(qq($D/*/*.txt));
    ok @g == 3;


    clearFolder($D, 5);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;


=head2 Read and write files

Read and write strings from and to files creating paths to any created files as needed.

=head3 readFile($file)

Return the content of a file residing on the local machine interpreting the content of the file as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

     Parameter  Description
  1  $file      Name of file to read

B<Example:>


    my $f = writeFile(undef,  "aaa");

    is_deeply [readFile $f], ["aaa"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    appendFile($f, "bbb");

    is_deeply [readFile $f], ["aaabbb"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $F = writeTempFile(qw(aaa bbb));

    is_deeply [readFile $F], ["aaa
", "bbb
"];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    eval {writeFile($f,  q(ccc))};
    ok $@ =~ m(File already exists:)i;

    overWriteFile($F,    q(ccc));

    ok   readFile($F) eq q(ccc);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    unlink $f, $F;


=head3 readStdIn()

Return the contents of STDIN and return the results as either an array or a string. Terminate with Ctrl-D if testing manually - STDIN remains open allowing this method to be called again to receive another block of data.


B<Example:>


    my $d = qq(aaaa);
    open(STDIN, "<", writeTempFile($d));

    ok qq($d
) eq readStdIn;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 readFileFromRemote($file, $ip)

Copy and read a B<$file> from the remote machine whose ip address is specified by B<$ip> or returned by L<awsIp> and return the content of $file interpreted as utf8 .

     Parameter  Description
  1  $file      Name of file to read
  2  $ip        Optional ip address of server

B<Example:>



    my $f = writeFileToRemote(undef, q(aaaa));
    unlink $f;

    ok readFileFromRemote($f) eq q(aaaa);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $f;


=head3 evalFile($file)

Read a file containing L<Unicode|https://en.wikipedia.org/wiki/Unicode> content represented as L<utf8|https://en.wikipedia.org/wiki/UTF-8>, L<perlfunc/eval> the content, confess to any errors and then return any result with L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> methods to access each hash element.

     Parameter  Description
  1  $file      File to read

B<Example:>


    my $d = [qw(aaa bbb ccc), [{aaa=>'AAA', bbb=>'BBB'}]];
    my $f = dumpFile(undef, $d);

    is_deeply evalFile($f), $d;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply evalFile(my $F = dumpTempFile($d)), $d;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $f, $F;

    my $j = dumpFileAsJson(undef, $d);
    is_deeply evalFileAsJson($j), $d;
    is_deeply evalFileAsJson(my $J = dumpTempFileAsJson($d)), $d;
    unlink $j, $J;


=head3 evalFileAsJson($file)

Read a B<$file> containing L<Json|https://en.wikipedia.org/wiki/JSON> and return the corresponding L<Perl|http://www.perl.org/> data structure.

     Parameter  Description
  1  $file      File to read

B<Example:>


    my $d = [qw(aaa bbb ccc), [{aaa=>'AAA', bbb=>'BBB'}]];
    my $f = dumpFile(undef, $d);
    is_deeply evalFile($f), $d;
    is_deeply evalFile(my $F = dumpTempFile($d)), $d;
    unlink $f, $F;

    my $j = dumpFileAsJson(undef, $d);

    is_deeply evalFileAsJson($j), $d;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply evalFileAsJson(my $J = dumpTempFileAsJson($d)), $d;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $j, $J;


=head3 evalGZipFile($file)

Read a file compressed with L<gzip|https://en.wikipedia.org/wiki/Gzip> containing L<Unicode|https://en.wikipedia.org/wiki/Unicode> content represented as L<utf8|https://en.wikipedia.org/wiki/UTF-8>, L<perlfunc/eval> the content, confess to any errors and then return any result with L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> methods to access each hash element. This is slower than using L<Storable|https://metacpan.org/pod/Storable> but does produce much smaller files, see also: L<dumpGZipFile|/dumpGZipFile>.

     Parameter  Description
  1  $file      File to read

B<Example:>


    my $d = [1, 2, 3=>{a=>4, b=>5}];
    my $file = dumpGZipFile(q(zzz.zip), $d);
    ok -e $file;

    my $D = evalGZipFile($file);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $d, $D;
    unlink $file;


=head3 retrieveFile($file)

Retrieve a B<$file> created via L<Storable|https://metacpan.org/pod/Storable>.  This is much faster than L<evalFile|/evalFile> as the stored data is not in text format.

     Parameter  Description
  1  $file      File to read

B<Example:>


    my $f = storeFile(undef, my $d = [qw(aaa bbb ccc)]);

    my $s = retrieveFile($f);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $s, $d;
    unlink $f;


=head3 readBinaryFile($file)

Read a binary file on the local machine.

     Parameter  Description
  1  $file      File to read

B<Example:>


    my $f = writeBinaryFile(undef, 0xff x 8);


    my $s = readBinaryFile($f);                                                      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok $s eq 0xff x 8;


=head3 readGZipFile($file)

Read the specified file containing compressed L<Unicode|https://en.wikipedia.org/wiki/Unicode> content represented as L<utf8|https://en.wikipedia.org/wiki/UTF-8> through L<gzip|https://en.wikipedia.org/wiki/Gzip>.

     Parameter  Description
  1  $file      File to read.

B<Example:>


    my $s = '𝝰'x1e3;
    my $file = writeGZipFile(q(zzz.zip), $s);
    ok -e $file;

    my $S = readGZipFile($file);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $s eq $S;
    ok length($s) == length($S);
    unlink $file;


=head3 makePath($file)

Make the path for the specified file name or folder on the local machine. Confess to any failure.

     Parameter  Description
  1  $file      File or folder name

B<Example:>


    my $d = fpd(my $D = temporaryDirectory, qw(a));
    my $f = fpe($d, qw(bbb txt));
    ok !-d $d;
    eval q{checkFile($f)};
    my $r = $@;
    my $q = quotemeta($D);
    ok nws($r) =~ m(Can only find.+?: $q)s;

    makePath($f);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok -d $d;
    ok -d $D;
    rmdir $_ for $d, $D;

    my $e = temporaryFolder;                                                      # Same as temporyDirectory
    ok -d $e;
    clearFolder($e, 2);

    my $t = temporaryFile;
    ok  -f $t;
    unlink $t;
    ok !-f $t;

    if (0)
     {makePathRemote($e);                                                         # Make a path on the remote system
     }


=head3 makePathRemote($file, $ip)

Make the path for the specified B<$file> or folder on the L<Amazon Web Services|http://aws.amazon.com> instance whose ip address is specified by B<$ip> or returned by L<awsIp>. Confess to any failures.

     Parameter  Description
  1  $file      File or folder name
  2  $ip        Optional ip address

B<Example:>


    my $d = fpd(my $D = temporaryDirectory, qw(a));
    my $f = fpe($d, qw(bbb txt));
    ok !-d $d;
    eval q{checkFile($f)};
    my $r = $@;
    my $q = quotemeta($D);
    ok nws($r) =~ m(Can only find.+?: $q)s;
    makePath($f);
    ok -d $d;
    ok -d $D;
    rmdir $_ for $d, $D;

    my $e = temporaryFolder;                                                      # Same as temporyDirectory
    ok -d $e;
    clearFolder($e, 2);

    my $t = temporaryFile;
    ok  -f $t;
    unlink $t;
    ok !-f $t;

    if (0)

     {makePathRemote($e);                                                         # Make a path on the remote system  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     }


=head3 overWriteFile($file, $string)

Write to a B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>. Return the name of the $file on success else confess to any failures. If the file already exists it will be overwritten.

     Parameter  Description
  1  $file      File to write to or B<undef> for a temporary file
  2  $string    Unicode string to write

B<Example:>


    my $f = writeFile(undef,  "aaa");
    is_deeply [readFile $f], ["aaa"];

    appendFile($f, "bbb");
    is_deeply [readFile $f], ["aaabbb"];

    my $F = writeTempFile(qw(aaa bbb));
    is_deeply [readFile $F], ["aaa
", "bbb
"];

    eval {writeFile($f,  q(ccc))};
    ok $@ =~ m(File already exists:)i;


    overWriteFile($F,    q(ccc));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok   readFile($F) eq q(ccc);

    unlink $f, $F;


B<owf> is a synonym for L<overWriteFile|/overWriteFile>.


=head3 writeFile($file, $string)

Write to a new B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>. Return the name of the $file written to on success else confess if the file already exists or any other error occurs.

     Parameter  Description
  1  $file      New file to write to or B<undef> for a temporary file
  2  $string    String to write

B<Example:>



    my $f = writeFile(undef,  "aaa");  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply [readFile $f], ["aaa"];

    appendFile($f, "bbb");
    is_deeply [readFile $f], ["aaabbb"];

    my $F = writeTempFile(qw(aaa bbb));
    is_deeply [readFile $F], ["aaa
", "bbb
"];


    eval {writeFile($f,  q(ccc))};  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $@ =~ m(File already exists:)i;

    overWriteFile($F,    q(ccc));
    ok   readFile($F) eq q(ccc);

    unlink $f, $F;


=head3 writeTempFile(@strings)

Write an array of strings as lines to a temporary file and return the file name.

     Parameter  Description
  1  @strings   Array of lines

B<Example:>


    my $f = writeFile(undef,  "aaa");
    is_deeply [readFile $f], ["aaa"];

    appendFile($f, "bbb");
    is_deeply [readFile $f], ["aaabbb"];


    my $F = writeTempFile(qw(aaa bbb));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply [readFile $F], ["aaa
", "bbb
"];

    eval {writeFile($f,  q(ccc))};
    ok $@ =~ m(File already exists:)i;

    overWriteFile($F,    q(ccc));
    ok   readFile($F) eq q(ccc);

    unlink $f, $F;


=head3 writeFileToRemote($file, $string, $ip)

Write to a new B<$file>, after creating a path to the file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8> then copy the $file to the remote server whose ip address is specified by B<$ip> or returned by L<awsIp>. Return the name of the $file on success else confess if the file already exists or any other error occurs.

     Parameter  Description
  1  $file      New file to write to or B<undef> for a temporary file
  2  $string    String to write
  3  $ip        Optional ip address

B<Example:>




    my $f = writeFileToRemote(undef, q(aaaa));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $f;
    ok readFileFromRemote($f) eq q(aaaa);
    unlink $f;


=head3 overWriteBinaryFile($file, $string)

Write to B<$file>, after creating a path to the file with L<makePath> if necessary, the binary content in B<$string>. If the $file already exists it is overwritten. Return the name of the $file on success else confess.

     Parameter  Description
  1  $file      File to write to or B<undef> for a temporary file
  2  $string    L<Unicode|https://en.wikipedia.org/wiki/Unicode> string to write

B<Example:>


  if (1)
   {vec(my $a, 0, 8) = 254;
    vec(my $b, 0, 8) = 255;
    ok dump($a) eq dump("FE");
    ok dump($b) eq dump("FF");
    ok length($a) == 1;
    ok length($b) == 1;

    my $s = $a.$a.$b.$b;
    ok length($s) == 4;

    my $f = eval {writeFile(undef, $s)};
    ok fileSize($f) == 8;

    eval {writeBinaryFile($f, $s)};
    ok $@ =~ m(Binary file already exists:)s;


    eval {overWriteBinaryFile($f, $s)};  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok !$@;
    ok fileSize($f) == 4;

    ok $s eq eval {readBinaryFile($f)};

    copyBinaryFile($f, my $F = temporaryFile);
    ok $s eq readBinaryFile($F);
    unlink $f, $F;
   }


=head3 writeBinaryFile($file, $string)

Write to a new B<$file>, after creating a path to the file with L<makePath> if necessary, the binary content in B<$string>. Return the name of the $file on success else confess if the file already exists or any other error occurs.

     Parameter  Description
  1  $file      New file to write to or B<undef> for a temporary file
  2  $string    String to write

B<Example:>



    my $f = writeBinaryFile(undef, 0xff x 8);                                        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $s = readBinaryFile($f);

    ok $s eq 0xff x 8;

  if (1)
   {vec(my $a, 0, 8) = 254;
    vec(my $b, 0, 8) = 255;
    ok dump($a) eq dump("FE");
    ok dump($b) eq dump("FF");
    ok length($a) == 1;
    ok length($b) == 1;

    my $s = $a.$a.$b.$b;
    ok length($s) == 4;

    my $f = eval {writeFile(undef, $s)};
    ok fileSize($f) == 8;


    eval {writeBinaryFile($f, $s)};  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $@ =~ m(Binary file already exists:)s;

    eval {overWriteBinaryFile($f, $s)};
    ok !$@;
    ok fileSize($f) == 4;

    ok $s eq eval {readBinaryFile($f)};

    copyBinaryFile($f, my $F = temporaryFile);
    ok $s eq readBinaryFile($F);
    unlink $f, $F;
   }


=head3 dumpFile($file, $structure)

Dump to a B<$file> the referenced data B<$structure>.

     Parameter   Description
  1  $file       File to write to or B<undef> for a temporary file
  2  $structure  Address of data structure to write

B<Example:>


    my $d = [qw(aaa bbb ccc), [{aaa=>'AAA', bbb=>'BBB'}]];

    my $f = dumpFile(undef, $d);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply evalFile($f), $d;
    is_deeply evalFile(my $F = dumpTempFile($d)), $d;
    unlink $f, $F;

    my $j = dumpFileAsJson(undef, $d);
    is_deeply evalFileAsJson($j), $d;
    is_deeply evalFileAsJson(my $J = dumpTempFileAsJson($d)), $d;
    unlink $j, $J;


=head3 dumpTempFile($structure)

Dump a data structure to a temporary file and return the name of the file created

     Parameter   Description
  1  $structure  Data structure to write

B<Example:>


    my $d = [qw(aaa bbb ccc), [{aaa=>'AAA', bbb=>'BBB'}]];
    my $f = dumpFile(undef, $d);
    is_deeply evalFile($f), $d;

    is_deeply evalFile(my $F = dumpTempFile($d)), $d;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $f, $F;

    my $j = dumpFileAsJson(undef, $d);
    is_deeply evalFileAsJson($j), $d;
    is_deeply evalFileAsJson(my $J = dumpTempFileAsJson($d)), $d;
    unlink $j, $J;


=head3 dumpFileAsJson($file, $structure)

Dump to a B<$file> the referenced data B<$structure> represented as L<Json|https://en.wikipedia.org/wiki/JSON> string.

     Parameter   Description
  1  $file       File to write to or B<undef> for a temporary file
  2  $structure  Address of data structure to write

B<Example:>


    my $d = [qw(aaa bbb ccc), [{aaa=>'AAA', bbb=>'BBB'}]];
    my $f = dumpFile(undef, $d);
    is_deeply evalFile($f), $d;
    is_deeply evalFile(my $F = dumpTempFile($d)), $d;
    unlink $f, $F;


    my $j = dumpFileAsJson(undef, $d);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply evalFileAsJson($j), $d;
    is_deeply evalFileAsJson(my $J = dumpTempFileAsJson($d)), $d;
    unlink $j, $J;


=head3 dumpTempFileAsJson($structure)

Dump a data structure represented as L<Json|https://en.wikipedia.org/wiki/JSON> string to a temporary file and return the name of the file created.

     Parameter   Description
  1  $structure  Data structure to write

B<Example:>


    my $d = [qw(aaa bbb ccc), [{aaa=>'AAA', bbb=>'BBB'}]];
    my $f = dumpFile(undef, $d);
    is_deeply evalFile($f), $d;
    is_deeply evalFile(my $F = dumpTempFile($d)), $d;
    unlink $f, $F;

    my $j = dumpFileAsJson(undef, $d);
    is_deeply evalFileAsJson($j), $d;

    is_deeply evalFileAsJson(my $J = dumpTempFileAsJson($d)), $d;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $j, $J;


=head3 storeFile($file, $structure)

Store into a B<$file>, after creating a path to the file with L<makePath> if necessary, a data B<$structure> via L<Storable|https://metacpan.org/pod/Storable>.  This is much faster than L<dumpFile|/dumpFile> but the stored results are not easily modified.

     Parameter   Description
  1  $file       File to write to or B<undef> for a temporary file
  2  $structure  Address of data structure to write

B<Example:>



    my $f = storeFile(undef, my $d = [qw(aaa bbb ccc)]);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $s = retrieveFile($f);
    is_deeply $s, $d;
    unlink $f;


=head3 writeGZipFile($file, $string)

Write to a B<$file>, after creating a path to the file with L<makePath> if necessary, through L<gzip|https://en.wikipedia.org/wiki/Gzip> a B<$string> whose content is encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

     Parameter  Description
  1  $file      File to write to
  2  $string    String to write

B<Example:>


    my $s = '𝝰'x1e3;

    my $file = writeGZipFile(q(zzz.zip), $s);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok -e $file;
    my $S = readGZipFile($file);
    ok $s eq $S;
    ok length($s) == length($S);
    unlink $file;


=head3 dumpGZipFile($file, $structure)

Write to a B<$file> a data B<$structure> through L<gzip|https://en.wikipedia.org/wiki/Gzip>. This technique produces files that are a lot more compact files than those produced by L<Storable|https://metacpan.org/pod/Storable>, but the execution time is much longer. See also: L<evalGZipFile|/evalGZipFile>.

     Parameter   Description
  1  $file       File to write
  2  $structure  Reference to data

B<Example:>


    my $d = [1, 2, 3=>{a=>4, b=>5}];

    my $file = dumpGZipFile(q(zzz.zip), $d);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok -e $file;
    my $D = evalGZipFile($file);
    is_deeply $d, $D;
    unlink $file;


=head3 writeFiles($hash, $old, $new)

Write the values of a B<$hash> reference into files identified by the key of each value using L<overWriteFile|/overWriteFile> optionally swapping the prefix of each file from B<$old> to B<$new>.

     Parameter  Description
  1  $hash      Hash of key value pairs representing files and data
  2  $old       Optional old prefix
  3  $new       New prefix

B<Example:>


    my $d = temporaryFolder;
    my $a = fpd($d, q(aaa));
    my $b = fpd($d, q(bbb));
    my $c = fpd($d, q(ccc));
    my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
    my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
    my $files = {$a1 => "1111", $a2 => "2222"};


    writeFiles($files);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $ra = readFiles($a);
    is_deeply $files, $ra;
    copyFolder($a, $b);
    my $rb = readFiles($b);
    is_deeply [sort values %$ra], [sort values %$rb];

    unlink $a2;
    mergeFolder($a, $b);
    ok -e $b1; ok  -e $b2;

    copyFolder($a, $b);
    ok -e $b1; ok !-e $b2;

    copyFile($a1, $a2);
    ok readFile($a1) eq readFile($a2);


    writeFiles($files);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok !moveFileNoClobber  ($a1, $a2);
    ok  moveFileWithClobber($a1, $a2);
    ok !-e $a1;
    ok readFile($a2) eq q(1111);
    ok  moveFileNoClobber  ($a2, $a1);
    ok !-e $a2;
    ok readFile($a1) eq q(1111);

    clearFolder(q(aaa), 11);
    clearFolder(q(bbb), 11);


=head3 readFiles(@folders)

Read all the files in the specified list of folders into a hash.

     Parameter  Description
  1  @folders   Folders to read

B<Example:>


    my $d = temporaryFolder;
    my $a = fpd($d, q(aaa));
    my $b = fpd($d, q(bbb));
    my $c = fpd($d, q(ccc));
    my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
    my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
    my $files = {$a1 => "1111", $a2 => "2222"};

    writeFiles($files);

    my $ra = readFiles($a);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $files, $ra;
    copyFolder($a, $b);

    my $rb = readFiles($b);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply [sort values %$ra], [sort values %$rb];

    unlink $a2;
    mergeFolder($a, $b);
    ok -e $b1; ok  -e $b2;

    copyFolder($a, $b);
    ok -e $b1; ok !-e $b2;

    copyFile($a1, $a2);
    ok readFile($a1) eq readFile($a2);

    writeFiles($files);
    ok !moveFileNoClobber  ($a1, $a2);
    ok  moveFileWithClobber($a1, $a2);
    ok !-e $a1;
    ok readFile($a2) eq q(1111);
    ok  moveFileNoClobber  ($a2, $a1);
    ok !-e $a2;
    ok readFile($a1) eq q(1111);

    clearFolder(q(aaa), 11);
    clearFolder(q(bbb), 11);


=head3 appendFile($file, $string)

Append to B<$file> a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded with L<utf8|https://en.wikipedia.org/wiki/UTF-8>, creating the $file first if necessary. Return the name of the $file on success else confess. The $file being appended to is locked before the write with L<perlfunc/flock> to allow  multiple processes to append linearly to the same file.

     Parameter  Description
  1  $file      File to append to
  2  $string    String to append

B<Example:>


    my $f = writeFile(undef,  "aaa");
    is_deeply [readFile $f], ["aaa"];


    appendFile($f, "bbb");  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply [readFile $f], ["aaabbb"];

    my $F = writeTempFile(qw(aaa bbb));
    is_deeply [readFile $F], ["aaa
", "bbb
"];

    eval {writeFile($f,  q(ccc))};
    ok $@ =~ m(File already exists:)i;

    overWriteFile($F,    q(ccc));
    ok   readFile($F) eq q(ccc);

    unlink $f, $F;


=head3 createEmptyFile($file)

Create an empty file unless the file already exists and return the name of the file else confess if the file cannot be created.

     Parameter  Description
  1  $file      File to create or B<undef> for a temporary file

B<Example:>


    my $D = temporaryFolder;
    ok  -d $D;

    my $d = fpd($D, q(ddd));
    ok !-d $d;


    my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];

    my @D = findDirs($D);
    my @e = ($D, $d);
    my @E = sort @e;
    is_deeply [@D], [@E];

    is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
              ["a.txt", "b.txt", "c.txt"];

    is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
              ["a.txt", "b.txt", "c.txt"];

    ok -e $_ for @f;

    is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;

    my @g = fileList(qq($D/*/*.txt));
    ok @g == 3;

    clearFolder($D, 5);
    ok onWindows ? 1 : !-e $_ for @f;
    ok onWindows ? 1 : !-d $D;


=head3 setPermissionsForFile($file, $permissions)

Apply L<chmod|https://linux.die.net/man/1/chmod> to a B<$file> to set its B<$permissions>.

     Parameter     Description
  1  $file         File
  2  $permissions  Permissions settings per chmod

B<Example:>


  if (1)
   {my $f = temporaryFile();

    setPermissionsForFile($f, q(ugo=r));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $a = qx(ls -la $f);
    ok $a =~ m(-r--r--r--)s;

    setPermissionsForFile($f, q(u=rwx));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $b = qx(ls -la $f);
    ok $b =~ m(-rwxr--r--)s;
   }


=head3 numberOfLinesInFile($file)

Return the number of lines in a file.

     Parameter  Description
  1  $file      File

B<Example:>


    my $f = writeFile(undef, "a
b
");


    ok numberOfLinesInFile($f) == 2;                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 overWriteHtmlFile($file, $data)

Write an L<HTML|https://en.wikipedia.org/wiki/HTML> file to /var/www/html and make it readable

     Parameter  Description
  1  $file      Target file relative to /var/www/html
  2  $data      Data to write

B<Example:>



    overWriteHtmlFile   (q(index.html), q(<html><h1>Hello</h1></html>));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    overWritePerlCgiFile(q(gen.pl),     q(...));


=head3 overWritePerlCgiFile($file, $data)

Write a L<Perl|http://www.perl.org/> file to /usr/lib/cgi-bin and make it executable after checking it for syntax errors

     Parameter  Description
  1  $file      Target file relative to /var/www/html
  2  $data      Data to write

B<Example:>


    overWriteHtmlFile   (q(index.html), q(<html><h1>Hello</h1></html>));

    overWritePerlCgiFile(q(gen.pl),     q(...));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 Copy

Copy files and folders. The B<\Acopy.*Md5Normalized.*\Z> methods can be used to ensure that files have collision proof names that collapse duplicate content even when copied to another folder.

=head3 copyFile($source, $target)

Copy the B<$source> file encoded in utf8 to the specified B<$target> file in and return $target.

     Parameter  Description
  1  $source    Source file
  2  $target    Target file

B<Example:>


    my $d = temporaryFolder;
    my $a = fpd($d, q(aaa));
    my $b = fpd($d, q(bbb));
    my $c = fpd($d, q(ccc));
    my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
    my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
    my $files = {$a1 => "1111", $a2 => "2222"};

    writeFiles($files);
    my $ra = readFiles($a);
    is_deeply $files, $ra;
    copyFolder($a, $b);
    my $rb = readFiles($b);
    is_deeply [sort values %$ra], [sort values %$rb];

    unlink $a2;
    mergeFolder($a, $b);
    ok -e $b1; ok  -e $b2;

    copyFolder($a, $b);
    ok -e $b1; ok !-e $b2;


    copyFile($a1, $a2);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readFile($a1) eq readFile($a2);

    writeFiles($files);
    ok !moveFileNoClobber  ($a1, $a2);
    ok  moveFileWithClobber($a1, $a2);
    ok !-e $a1;
    ok readFile($a2) eq q(1111);
    ok  moveFileNoClobber  ($a2, $a1);
    ok !-e $a2;
    ok readFile($a1) eq q(1111);

    clearFolder(q(aaa), 11);
    clearFolder(q(bbb), 11);


=head3 moveFileNoClobber($source, $target)

Rename the B<$source> file, which must exist, to the B<$target> file but only if the $target file does not exist already.  Returns 1 if the $source file was successfully renamed to the $target file else 0.

     Parameter  Description
  1  $source    Source file
  2  $target    Target file

B<Example:>


    my $d = temporaryFolder;
    my $a = fpd($d, q(aaa));
    my $b = fpd($d, q(bbb));
    my $c = fpd($d, q(ccc));
    my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
    my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
    my $files = {$a1 => "1111", $a2 => "2222"};

    writeFiles($files);
    my $ra = readFiles($a);
    is_deeply $files, $ra;
    copyFolder($a, $b);
    my $rb = readFiles($b);
    is_deeply [sort values %$ra], [sort values %$rb];

    unlink $a2;
    mergeFolder($a, $b);
    ok -e $b1; ok  -e $b2;

    copyFolder($a, $b);
    ok -e $b1; ok !-e $b2;

    copyFile($a1, $a2);
    ok readFile($a1) eq readFile($a2);

    writeFiles($files);

    ok !moveFileNoClobber  ($a1, $a2);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok  moveFileWithClobber($a1, $a2);
    ok !-e $a1;
    ok readFile($a2) eq q(1111);

    ok  moveFileNoClobber  ($a2, $a1);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok !-e $a2;
    ok readFile($a1) eq q(1111);

    clearFolder(q(aaa), 11);
    clearFolder(q(bbb), 11);


=head3 moveFileWithClobber($source, $target)

Rename the B<$source> file, which must exist, to the B<$target> file but only if the $target file does not exist already.  Returns 1 if the $source file was successfully renamed to the $target file else 0.

     Parameter  Description
  1  $source    Source file
  2  $target    Target file

B<Example:>


    my $d = temporaryFolder;
    my $a = fpd($d, q(aaa));
    my $b = fpd($d, q(bbb));
    my $c = fpd($d, q(ccc));
    my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
    my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
    my $files = {$a1 => "1111", $a2 => "2222"};

    writeFiles($files);
    my $ra = readFiles($a);
    is_deeply $files, $ra;
    copyFolder($a, $b);
    my $rb = readFiles($b);
    is_deeply [sort values %$ra], [sort values %$rb];

    unlink $a2;
    mergeFolder($a, $b);
    ok -e $b1; ok  -e $b2;

    copyFolder($a, $b);
    ok -e $b1; ok !-e $b2;

    copyFile($a1, $a2);
    ok readFile($a1) eq readFile($a2);

    writeFiles($files);
    ok !moveFileNoClobber  ($a1, $a2);

    ok  moveFileWithClobber($a1, $a2);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok !-e $a1;
    ok readFile($a2) eq q(1111);
    ok  moveFileNoClobber  ($a2, $a1);
    ok !-e $a2;
    ok readFile($a1) eq q(1111);

    clearFolder(q(aaa), 11);
    clearFolder(q(bbb), 11);


=head3 copyFileToFolder($source, $targetFolder)

Copy the file named in B<$source> to the specified B<$targetFolder/> or if $targetFolder/ is in fact a file into the folder containing this file and return the target file name. Confesses instead of copying if the target already exists.

     Parameter      Description
  1  $source        Source file
  2  $targetFolder  Target folder

B<Example:>


    my $sd = temporaryFolder;
    my $td = temporaryFolder;
    my $sf = writeFile fpe($sd, qw(test data)), q(aaaa);

    my $tf = copyFileToFolder($sf, $td);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readFile($tf) eq q(aaaa);
    ok fp ($tf) eq $td;
    ok fne($tf) eq q(test.data);


=head3 nameFromString($string, %options)

Create a readable name from an arbitrary string of text.

     Parameter  Description
  1  $string    String
  2  %options   Options

B<Example:>



  ok q(help) eq nameFromString(q(!@#$%^help___<>?><?>));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


  ok q(bm_The_skyscraper_analogy) eq nameFromString(<<END);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  <bookmap id="b1">
  <title>The skyscraper analogy</title>
  </bookmap>
  END

  ok q(bm_The_skyscraper_analogy_An_exciting_tale_of_two_skyscrapers_that_meet_in_downtown_Houston)

     eq nameFromString(<<END);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  <bookmap id="b1">
  <title>The skyscraper analogy</title>
  An exciting tale of two skyscrapers that meet in downtown Houston
  <concept><html>
  </bookmap>
  END

  ok q(bm_the_skyscraper_analogy) eq nameFromStringRestrictedToTitle(<<END);
  <bookmap id="b1">
  <title>The skyscraper analogy</title>
  An exciting tale of two skyscrapers that meet in downtown Houston
  <concept><html>
  </bookmap>
  END


=head3 nameFromStringRestrictedToTitle($string, %options)

Create a readable name from a string of text that might contain a title tag - fall back to L<nameFromString|/nameFromString> if that is not possible.

     Parameter  Description
  1  $string    String
  2  %options   Options

B<Example:>


  ok q(help) eq nameFromString(q(!@#$%^help___<>?><?>));
  ok q(bm_The_skyscraper_analogy) eq nameFromString(<<END);
  <bookmap id="b1">
  <title>The skyscraper analogy</title>
  </bookmap>
  END

  ok q(bm_The_skyscraper_analogy_An_exciting_tale_of_two_skyscrapers_that_meet_in_downtown_Houston)
     eq nameFromString(<<END);
  <bookmap id="b1">
  <title>The skyscraper analogy</title>
  An exciting tale of two skyscrapers that meet in downtown Houston
  <concept><html>
  </bookmap>
  END


  ok q(bm_the_skyscraper_analogy) eq nameFromStringRestrictedToTitle(<<END);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  <bookmap id="b1">
  <title>The skyscraper analogy</title>
  An exciting tale of two skyscrapers that meet in downtown Houston
  <concept><html>
  </bookmap>
  END


=head3 uniqueNameFromFile($source)

Create a unique name from a file name and the md5 sum of its content

     Parameter  Description
  1  $source    Source file

B<Example:>


    my $f = owf(q(test.txt), join "", 1..100);

    ok uniqueNameFromFile($f) eq q(test_ef69caaaeea9c17120821a9eb6c7f1de.txt);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $f;


=head3 nameFromFolder($file)

Create a name from the last folder in the path of a file name.  Return undef if the file does not have a path.

     Parameter  Description
  1  $file      File name

B<Example:>



    ok nameFromFolder(fpe(qw( a b c d e))) eq q(c);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 copyFileMd5Normalized($source, $Target)

Normalize the name of the specified B<$source> file to the md5 sum of its content, retaining its current extension, while placing the original file name in a companion file if the companion file does not already exist.  If no B<$target> folder is supplied the file is renamed to its normalized form in situ, otherwise it is copied to the target folder and renamed there. A companion file for the B<$source> file is created by removing the extension of the normalized file and writing the original B<$source> file name to it unless such a file already exists as we assume that it contains the 'original' original name of the B<$source> file. If the B<$source> file is copied to a new location then the companion file is copied as well to maintain the link back to the original name of the file.

     Parameter  Description
  1  $source    Source file
  2  $Target    Target folder or a file in the target folder

B<Example:>


    my $dir = temporaryFolder;
    my $a = fpe($dir, qw(a a jpg));
    my $b = fpe($dir, qw(b a jpg));
    my $c = fpe($dir, qw(c a jpg));

    my $content = join '', 1..1e3;

    my $A = copyFileMd5NormalizedCreate($a, $content, q(jpg), $a);
    ok readFile($A) eq $content;

    ok $A eq copyFileMd5Normalized($A);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    my $B = copyFileMd5Normalized($A, $b);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readFile($B) eq $content;

    ok $B eq copyFileMd5Normalized($B);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    my $C = copyFileMd5Normalized($B, $c);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readFile($C) eq $content;

    ok $C eq copyFileMd5Normalized($C);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok fne($A) eq fne($_) for $B, $C;
    ok readFile($_) eq $content for $A, $B, $C;
    ok copyFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;

    ok 6 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($A);
    ok 4 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($B);
    ok 2 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($C);
    ok 0 == searchDirectoryTreesForMatchingFiles($dir);

    clearFolder($dir, 10);
    ok 0 == searchDirectoryTreesForMatchingFiles($dir);


=head3 copyFileMd5NormalizedName($content, $extension, %options)

Name a file using the GB Standard

     Parameter   Description
  1  $content    Content
  2  $extension  Extension
  3  %options    Options

B<Example:>



    ok copyFileMd5NormalizedName(<<END, q(txt)) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  <p>Hello<b>World</b></p>
  END
  q(Hello_World_6ba23858c1b4811660896c324acac6fa.txt);


=head3 copyFileMd5NormalizedCreate($Folder, $content, $extension, $companionContent, %options)

Create a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content>, whose extension is B<$extension> and which has a companion file with the same name minus the extension which contains the specified B<$companionContent>.  Such a file can be copied multiple times by L<copyFileMd5Normalized|/copyFileMd5Normalized> regardless of the other files in the target folders.

     Parameter          Description
  1  $Folder            Target folder or a file in that folder
  2  $content           Content of the file
  3  $extension         File extension
  4  $companionContent  Contents of the companion file
  5  %options           Options.

B<Example:>


    my $dir = temporaryFolder;
    my $a = fpe($dir, qw(a a jpg));
    my $b = fpe($dir, qw(b a jpg));
    my $c = fpe($dir, qw(c a jpg));

    my $content = join '', 1..1e3;


    my $A = copyFileMd5NormalizedCreate($a, $content, q(jpg), $a);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readFile($A) eq $content;
    ok $A eq copyFileMd5Normalized($A);

    my $B = copyFileMd5Normalized($A, $b);
    ok readFile($B) eq $content;
    ok $B eq copyFileMd5Normalized($B);

    my $C = copyFileMd5Normalized($B, $c);
    ok readFile($C) eq $content;
    ok $C eq copyFileMd5Normalized($C);

    ok fne($A) eq fne($_) for $B, $C;
    ok readFile($_) eq $content for $A, $B, $C;
    ok copyFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;

    ok 6 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($A);
    ok 4 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($B);
    ok 2 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($C);
    ok 0 == searchDirectoryTreesForMatchingFiles($dir);

    clearFolder($dir, 10);
    ok 0 == searchDirectoryTreesForMatchingFiles($dir);


=head3 copyFileMd5NormalizedGetCompanionContent($source)

Return the content of the companion file to the specified B<$source> file after it has been normalized via L<copyFileMd5Normalized|/copyFileMd5Normalized> or L<copyFileMd5NormalizedCreate|/copyFileMd5NormalizedCreate> or return B<undef> if the corresponding companion file does not exist.

     Parameter  Description
  1  $source    Source file.

B<Example:>


    my $dir = temporaryFolder;
    my $a = fpe($dir, qw(a a jpg));
    my $b = fpe($dir, qw(b a jpg));
    my $c = fpe($dir, qw(c a jpg));

    my $content = join '', 1..1e3;

    my $A = copyFileMd5NormalizedCreate($a, $content, q(jpg), $a);
    ok readFile($A) eq $content;
    ok $A eq copyFileMd5Normalized($A);

    my $B = copyFileMd5Normalized($A, $b);
    ok readFile($B) eq $content;
    ok $B eq copyFileMd5Normalized($B);

    my $C = copyFileMd5Normalized($B, $c);
    ok readFile($C) eq $content;
    ok $C eq copyFileMd5Normalized($C);

    ok fne($A) eq fne($_) for $B, $C;
    ok readFile($_) eq $content for $A, $B, $C;

    ok copyFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok 6 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($A);
    ok 4 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($B);
    ok 2 == searchDirectoryTreesForMatchingFiles($dir);
    copyFileMd5NormalizedDelete($C);
    ok 0 == searchDirectoryTreesForMatchingFiles($dir);

    clearFolder($dir, 10);
    ok 0 == searchDirectoryTreesForMatchingFiles($dir);


=head3 copyFileMd5NormalizedDelete($file)

Delete a normalized and its companion file

     Parameter  Description
  1  $file      File

B<Example:>


    my $dir = temporaryFolder;
    my $a = fpe($dir, qw(a a jpg));
    my $b = fpe($dir, qw(b a jpg));
    my $c = fpe($dir, qw(c a jpg));

    my $content = join '', 1..1e3;

    my $A = copyFileMd5NormalizedCreate($a, $content, q(jpg), $a);
    ok readFile($A) eq $content;
    ok $A eq copyFileMd5Normalized($A);

    my $B = copyFileMd5Normalized($A, $b);
    ok readFile($B) eq $content;
    ok $B eq copyFileMd5Normalized($B);

    my $C = copyFileMd5Normalized($B, $c);
    ok readFile($C) eq $content;
    ok $C eq copyFileMd5Normalized($C);

    ok fne($A) eq fne($_) for $B, $C;
    ok readFile($_) eq $content for $A, $B, $C;
    ok copyFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;

    ok 6 == searchDirectoryTreesForMatchingFiles($dir);

    copyFileMd5NormalizedDelete($A);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok 4 == searchDirectoryTreesForMatchingFiles($dir);

    copyFileMd5NormalizedDelete($B);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok 2 == searchDirectoryTreesForMatchingFiles($dir);

    copyFileMd5NormalizedDelete($C);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok 0 == searchDirectoryTreesForMatchingFiles($dir);

    clearFolder($dir, 10);
    ok 0 == searchDirectoryTreesForMatchingFiles($dir);


=head3 copyBinaryFile($source, $target)

Copy the binary file B<$source> to a file named <%target> and return the target file name,

     Parameter  Description
  1  $source    Source file
  2  $target    Target file

B<Example:>


  if (1)
   {vec(my $a, 0, 8) = 254;
    vec(my $b, 0, 8) = 255;
    ok dump($a) eq dump("FE");
    ok dump($b) eq dump("FF");
    ok length($a) == 1;
    ok length($b) == 1;

    my $s = $a.$a.$b.$b;
    ok length($s) == 4;

    my $f = eval {writeFile(undef, $s)};
    ok fileSize($f) == 8;

    eval {writeBinaryFile($f, $s)};
    ok $@ =~ m(Binary file already exists:)s;

    eval {overWriteBinaryFile($f, $s)};
    ok !$@;
    ok fileSize($f) == 4;

    ok $s eq eval {readBinaryFile($f)};


    copyBinaryFile($f, my $F = temporaryFile);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $s eq readBinaryFile($F);
    unlink $f, $F;
   }


=head3 copyBinaryFileMd5Normalized($source, $Target)

Normalize the name of the specified B<$source> file to the md5 sum of its content, retaining its current extension, while placing the original file name in a companion file if the companion file does not already exist.  If no B<$target> folder is supplied the file is renamed to its normalized form in situ, otherwise it is copied to the target folder and renamed there. A companion file for the B<$source> file is created by removing the extension of the normalized file and writing the original B<$source> file name to it unless such a file already exists as we assume that it contains the 'original' original name of the B<$source> file. If the B<$source> file is copied to a new location then the companion file is copied as well to maintain the link back to the original name of the file.

     Parameter  Description
  1  $source    Source file
  2  $Target    Target folder or a file in the target folder

B<Example:>


    my $dir = temporaryFolder;
    my $a = fpe($dir, qw(a a jpg));
    my $b = fpe($dir, qw(b a jpg));
    my $c = fpe($dir, qw(c a jpg));

    my $content = join '', 1..1e3;

    my $A = copyBinaryFileMd5NormalizedCreate($a, $content, q(jpg), $a);
    ok readBinaryFile($A) eq $content;

    ok $A eq copyBinaryFileMd5Normalized($A);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    my $B = copyBinaryFileMd5Normalized($A, $b);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readBinaryFile($B) eq $content;

    ok $B eq copyBinaryFileMd5Normalized($B);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    my $C = copyBinaryFileMd5Normalized($B, $c);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readBinaryFile($C) eq $content;

    ok $C eq copyBinaryFileMd5Normalized($C);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok fne($A) eq fne($_) for $B, $C;
    ok readBinaryFile($_) eq $content for $A, $B, $C;
    ok copyBinaryFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;

    ok 6 == searchDirectoryTreesForMatchingFiles($dir);
    clearFolder($dir, 10);


=head3 copyBinaryFileMd5NormalizedCreate($Folder, $content, $extension, $companionContent)

Create a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content>, whose extension is B<$extension> and which has a companion file with the same name minus the extension  which contains the specified B<$companionContent>.  Such a file can be copied multiple times by L<copyBinaryFileMd5Normalized|/copyBinaryFileMd5Normalized> regardless of the other files in the target folders while retaining the original name information.

     Parameter          Description
  1  $Folder            Target folder or a file in that folder
  2  $content           Content of the file
  3  $extension         File extension
  4  $companionContent  Optional content of the companion file.

B<Example:>


    my $dir = temporaryFolder;
    my $a = fpe($dir, qw(a a jpg));
    my $b = fpe($dir, qw(b a jpg));
    my $c = fpe($dir, qw(c a jpg));

    my $content = join '', 1..1e3;


    my $A = copyBinaryFileMd5NormalizedCreate($a, $content, q(jpg), $a);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok readBinaryFile($A) eq $content;
    ok $A eq copyBinaryFileMd5Normalized($A);

    my $B = copyBinaryFileMd5Normalized($A, $b);
    ok readBinaryFile($B) eq $content;
    ok $B eq copyBinaryFileMd5Normalized($B);

    my $C = copyBinaryFileMd5Normalized($B, $c);
    ok readBinaryFile($C) eq $content;
    ok $C eq copyBinaryFileMd5Normalized($C);

    ok fne($A) eq fne($_) for $B, $C;
    ok readBinaryFile($_) eq $content for $A, $B, $C;
    ok copyBinaryFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;

    ok 6 == searchDirectoryTreesForMatchingFiles($dir);
    clearFolder($dir, 10);


=head3 copyBinaryFileMd5NormalizedGetCompanionContent($source)

Return the original name of the specified B<$source> file after it has been normalized via L<copyBinaryFileMd5Normalized|/copyBinaryFileMd5Normalized> or L<copyBinaryFileMd5NormalizedCreate|/copyBinaryFileMd5NormalizedCreate> or return B<undef> if the corresponding companion file does not exist.

     Parameter  Description
  1  $source    Source file.

B<Example:>


    my $dir = temporaryFolder;
    my $a = fpe($dir, qw(a a jpg));
    my $b = fpe($dir, qw(b a jpg));
    my $c = fpe($dir, qw(c a jpg));

    my $content = join '', 1..1e3;

    my $A = copyBinaryFileMd5NormalizedCreate($a, $content, q(jpg), $a);
    ok readBinaryFile($A) eq $content;
    ok $A eq copyBinaryFileMd5Normalized($A);

    my $B = copyBinaryFileMd5Normalized($A, $b);
    ok readBinaryFile($B) eq $content;
    ok $B eq copyBinaryFileMd5Normalized($B);

    my $C = copyBinaryFileMd5Normalized($B, $c);
    ok readBinaryFile($C) eq $content;
    ok $C eq copyBinaryFileMd5Normalized($C);

    ok fne($A) eq fne($_) for $B, $C;
    ok readBinaryFile($_) eq $content for $A, $B, $C;

    ok copyBinaryFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok 6 == searchDirectoryTreesForMatchingFiles($dir);
    clearFolder($dir, 10);


=head3 copyFileToRemote($file, $ip)

Copy the specified local B<$file> to the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

     Parameter  Description
  1  $file      Source file
  2  $ip        Optional ip address

B<Example:>


  if (0)

   {copyFileToRemote     (q(/home/phil/perl/cpan/aaa.txt));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    copyFileFromRemote   (q(/home/phil/perl/cpan/aaa.txt));
    copyFolderToRemote   (q(/home/phil/perl/cpan/));
    mergeFolderFromRemote(q(/home/phil/perl/cpan/));
   }


=head3 copyFileFromRemote($file, $ip)

Copy the specified B<$file> from the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

     Parameter  Description
  1  $file      Source file
  2  $ip        Optional ip address

B<Example:>


  if (0)
   {copyFileToRemote     (q(/home/phil/perl/cpan/aaa.txt));

    copyFileFromRemote   (q(/home/phil/perl/cpan/aaa.txt));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    copyFolderToRemote   (q(/home/phil/perl/cpan/));
    mergeFolderFromRemote(q(/home/phil/perl/cpan/));
   }


=head3 copyFolder($source, $target)

Copy the B<$source> folder to the B<$target> folder after clearing the $target folder.

     Parameter  Description
  1  $source    Source file
  2  $target    Target file

B<Example:>


    my $d = temporaryFolder;
    my $a = fpd($d, q(aaa));
    my $b = fpd($d, q(bbb));
    my $c = fpd($d, q(ccc));
    my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
    my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
    my $files = {$a1 => "1111", $a2 => "2222"};

    writeFiles($files);
    my $ra = readFiles($a);
    is_deeply $files, $ra;

    copyFolder($a, $b);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $rb = readFiles($b);
    is_deeply [sort values %$ra], [sort values %$rb];

    unlink $a2;
    mergeFolder($a, $b);
    ok -e $b1; ok  -e $b2;


    copyFolder($a, $b);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok -e $b1; ok !-e $b2;

    copyFile($a1, $a2);
    ok readFile($a1) eq readFile($a2);

    writeFiles($files);
    ok !moveFileNoClobber  ($a1, $a2);
    ok  moveFileWithClobber($a1, $a2);
    ok !-e $a1;
    ok readFile($a2) eq q(1111);
    ok  moveFileNoClobber  ($a2, $a1);
    ok !-e $a2;
    ok readFile($a1) eq q(1111);

    clearFolder(q(aaa), 11);
    clearFolder(q(bbb), 11);


=head3 mergeFolder($source, $target)

Copy the B<$source> folder into the B<$target> folder retaining any existing files not replaced by copied files.

     Parameter  Description
  1  $source    Source file
  2  $target    Target file

B<Example:>


    my $d = temporaryFolder;
    my $a = fpd($d, q(aaa));
    my $b = fpd($d, q(bbb));
    my $c = fpd($d, q(ccc));
    my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
    my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
    my $files = {$a1 => "1111", $a2 => "2222"};

    writeFiles($files);
    my $ra = readFiles($a);
    is_deeply $files, $ra;
    copyFolder($a, $b);
    my $rb = readFiles($b);
    is_deeply [sort values %$ra], [sort values %$rb];

    unlink $a2;

    mergeFolder($a, $b);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok -e $b1; ok  -e $b2;

    copyFolder($a, $b);
    ok -e $b1; ok !-e $b2;

    copyFile($a1, $a2);
    ok readFile($a1) eq readFile($a2);

    writeFiles($files);
    ok !moveFileNoClobber  ($a1, $a2);
    ok  moveFileWithClobber($a1, $a2);
    ok !-e $a1;
    ok readFile($a2) eq q(1111);
    ok  moveFileNoClobber  ($a2, $a1);
    ok !-e $a2;
    ok readFile($a1) eq q(1111);

    clearFolder(q(aaa), 11);
    clearFolder(q(bbb), 11);


=head3 copyFolderToRemote($Source, $ip)

Copy the specified local B<$Source> folder to the corresponding remote folder on the server whose ip address is specified by B<$ip> or returned by L<awsIp>. The default userid supplied by F<.ssh/config> will be used on the remote server.

     Parameter  Description
  1  $Source    Source file
  2  $ip        Optional ip address of server

B<Example:>


  if (0)
   {copyFileToRemote     (q(/home/phil/perl/cpan/aaa.txt));
    copyFileFromRemote   (q(/home/phil/perl/cpan/aaa.txt));

    copyFolderToRemote   (q(/home/phil/perl/cpan/));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    mergeFolderFromRemote(q(/home/phil/perl/cpan/));
   }


=head3 mergeFolderFromRemote($Source, $ip)

Merge the specified B<$Source> folder from the corresponding remote folder on the server whose ip address is specified by B<$ip> or returned by L<awsIp>. The default userid supplied by F<.ssh/config> will be used on the remote server.

     Parameter  Description
  1  $Source    Source file
  2  $ip        Optional ip address of server

B<Example:>


  if (0)
   {copyFileToRemote     (q(/home/phil/perl/cpan/aaa.txt));
    copyFileFromRemote   (q(/home/phil/perl/cpan/aaa.txt));
    copyFolderToRemote   (q(/home/phil/perl/cpan/));

    mergeFolderFromRemote(q(/home/phil/perl/cpan/));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head1 Testing

Methods to assist with testing

=head2 removeFilePathsFromStructure($structure)

Remove all file paths from a specified B<$structure> to make said $structure testable with L<Test::More/is_deeply>.

     Parameter   Description
  1  $structure  Data structure reference

B<Example:>


  if (1)
   {my $d = {"/home/aaa/bbb.txt"=>1, "ccc/ddd.txt"=>2, "eee.txt"=>3};

    my $D = removeFilePathsFromStructure($d);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    is_deeply removeFilePathsFromStructure($d),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     {"bbb.txt"=>1, "ddd.txt"=>2, "eee.txt"=>3};

    ok writeStructureTest($d, q($d)) eq <<'END';

    is_deeply removeFilePathsFromStructure($d),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     { "bbb.txt" => 1, "ddd.txt" => 2, "eee.txt" => 3 };
  END
   }


=head2 writeStructureTest($structure, $expr)

Write a test for a data B<$structure> with file names in it.

     Parameter   Description
  1  $structure  Data structure reference
  2  $expr       Expression

B<Example:>


  if (1)
   {my $d = {"/home/aaa/bbb.txt"=>1, "ccc/ddd.txt"=>2, "eee.txt"=>3};
    my $D = removeFilePathsFromStructure($d);

    is_deeply removeFilePathsFromStructure($d),
     {"bbb.txt"=>1, "ddd.txt"=>2, "eee.txt"=>3};


    ok writeStructureTest($d, q($d)) eq <<'END';  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply removeFilePathsFromStructure($d),
     { "bbb.txt" => 1, "ddd.txt" => 2, "eee.txt" => 3 };
  END
   }


=head1 Images

Image operations.

=head2 imageSize($image)

Return (width, height) of an B<$image>.

     Parameter  Description
  1  $image     File containing image

B<Example:>



    my ($width, $height) = imageSize(fpe(qw(a image jpg)));                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 convertDocxToFodt($inputFile, $outputFile)

Convert a I<docx> B<$inputFile> file to a I<fodt> B<$outputFile> using B<unoconv> which must not be running elsewhere at the time.  L<Unoconv|/https://github.com/dagwieers/unoconv> can be installed via:

  sudo apt install sharutils unoconv

Parameters:

     Parameter    Description
  1  $inputFile   Input file
  2  $outputFile  Output file

B<Example:>



    convertDocxToFodt(fpe(qw(a docx)), fpe(qw(a fodt)));                            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 cutOutImagesInFodtFile($inputFile, $outputFolder, $imagePrefix)

Cut out the images embedded in a B<fodt> file, perhaps produced via L<convertDocxToFodt|/convertDocxToFodt>, placing them in the specified folder and replacing them in the source file with:

  <image href="$imageFile" outputclass="imageType">.

This conversion requires that you have both L<Imagemagick|https://www.imagemagick.org/script/index.php> and L<unoconv|/https://github.com/dagwieers/unoconv> installed on your system:

    sudo apt install sharutils  imagemagick unoconv

Parameters:

     Parameter      Description
  1  $inputFile     Input file
  2  $outputFolder  Output folder for images
  3  $imagePrefix   A prefix to be added to image file names

B<Example:>



    cutOutImagesInFodtFile(fpe(qw(source fodt)), fpd(qw(images)), q(image));        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 Encoding and Decoding

Encode and decode using L<Json|https://en.wikipedia.org/wiki/JSON> and Mime.

=head2 unbless($d)

Remove the effects of bless from a L<Perl|http://www.perl.org/> data B<$structure> enabling it to be converted to L<Json|https://en.wikipedia.org/wiki/JSON> or compared with L<Test::More::is_deeply>.

     Parameter  Description
  1  $d         Unbless a L<Perl|http://www.perl.org/> data structure.

B<Example:>


  if (1)
   {my $a = {};
    ok ref($a)      eq  q(HASH);
    my $b =   bless $a, q(aaaa);
    ok ref($a)      eq  q(aaaa);

    my $c = unbless $b;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok ref($c)      eq  q(HASH);
   }


=head2 encodeJson($structure)

Convert a L<Perl|http://www.perl.org/> data B<$structure> to a L<Json|https://en.wikipedia.org/wiki/JSON> string.

     Parameter   Description
  1  $structure  Data to encode

B<Example:>



    my $A = encodeJson(my $a = {a=>1,b=>2, c=>[1..2]});  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $b = decodeJson($A);
    is_deeply $a, $b;


=head2 decodeJson($string)

Convert a L<Json|https://en.wikipedia.org/wiki/JSON> B<$string> to a L<Perl|http://www.perl.org/> data structure.

     Parameter  Description
  1  $string    Data to decode

B<Example:>


    my $A = encodeJson(my $a = {a=>1,b=>2, c=>[1..2]});

    my $b = decodeJson($A);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $a, $b;


=head2 encodeBase64($string)

Encode an L<Ascii|https://en.wikipedia.org/wiki/ASCII> B<$string> in base 64.

     Parameter  Description
  1  $string    String to encode

B<Example:>



    my $A = encodeBase64(my $a = "Hello World" x 10);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $b = decodeBase64($A);
    ok $a eq $b;


=head2 decodeBase64($string)

Decode an L<Ascii|https://en.wikipedia.org/wiki/ASCII> B<$string> in base 64.

     Parameter  Description
  1  $string    String to decode

B<Example:>


    my $A = encodeBase64(my $a = "Hello World" x 10);

    my $b = decodeBase64($A);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $a eq $b;


=head2 convertUnicodeToXml($string)

Convert a B<$string> with L<Unicode|https://en.wikipedia.org/wiki/Unicode> code points that are not directly representable in L<Ascii|https://en.wikipedia.org/wiki/ASCII> into string that replaces these code points with their representation in L<Xml|https://en.wikipedia.org/wiki/XML> making the string usable in L<Xml|https://en.wikipedia.org/wiki/XML> documents.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok convertUnicodeToXml('setenta e três') eq q(setenta e tr&#234;s);               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 asciiToHexString($ascii)

Encode an L<Ascii|https://en.wikipedia.org/wiki/ASCII> string as a string of L<hexadecimal|https://en.wikipedia.org/wiki/Hexadecimal> digits.

     Parameter  Description
  1  $ascii     Ascii string

B<Example:>



    ok asciiToHexString("Hello World!") eq                  "48656c6c6f20576f726c6421";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok                  "Hello World!"  eq hexToAsciiString("48656c6c6f20576f726c6421");


=head2 hexToAsciiString($hex)

Decode a string of L<hexadecimal|https://en.wikipedia.org/wiki/Hexadecimal> digits as an L<Ascii|https://en.wikipedia.org/wiki/ASCII> string.

     Parameter  Description
  1  $hex       Hexadecimal string

B<Example:>


    ok asciiToHexString("Hello World!") eq                  "48656c6c6f20576f726c6421";

    ok                  "Hello World!"  eq hexToAsciiString("48656c6c6f20576f726c6421");  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 wwwEncode($string)

Percent encode a L<url|https://en.wikipedia.org/wiki/URL> per: https://en.wikipedia.org/wiki/Percent-encoding#Percent-encoding_reserved_characters

     Parameter  Description
  1  $string    String

B<Example:>



    ok wwwEncode(q(a  {b} <c>)) eq q(a%20%20%7bb%7d%20%3cc%3e);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok wwwEncode(q(../))        eq q(%2e%2e/);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok wwwDecode(wwwEncode $_)  eq $_ for q(a  {b} <c>), q(a  b|c),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(%), q(%%), q(%%.%%);


  sub wwwEncode($)                                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   {my ($string) = @_;                                                            # String
    join '', map {$translatePercentEncoding{$_}//$_} split //, $string
   }


=head2 wwwDecode($string)

Percent decode a L<url|https://en.wikipedia.org/wiki/URL> B<$string> per: https://en.wikipedia.org/wiki/Percent-encoding#Percent-encoding_reserved_characters

     Parameter  Description
  1  $string    String

B<Example:>


    ok wwwEncode(q(a  {b} <c>)) eq q(a%20%20%7bb%7d%20%3cc%3e);
    ok wwwEncode(q(../))        eq q(%2e%2e/);

    ok wwwDecode(wwwEncode $_)  eq $_ for q(a  {b} <c>), q(a  b|c),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(%), q(%%), q(%%.%%);


  sub wwwDecode($)                                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   {my ($string) = @_;                                                            # String
    my $r = '';
    my @s = split //, $string;
    while(@s)
     {my $c = shift @s;
      if ($c eq q(%) and @s >= 2)
       {$c .= shift(@s).shift(@s);
        $r .= $TranslatePercentEncoding{$c}//$c;
       }
      else
       {$r .= $c;
       }
     }
    $r =~ s(%0d0a) (
)gs;                                                        # Awkward characters that appear in urls
    $r =~ s(\+)     ( )gs;
    $r
   }


=head2 printPerlDataAsXml($data, $width)

Print a Perl data structure as xml

     Parameter  Description
  1  $data      Perl data structure
  2  $width     Ideal width for Xml - default is 80

B<Example:>


  my $perlData = {a=>1, b=>[{c=>[3,4]}, {d=>[5,6]}, {e=>7}], f=>8};

  my $xml = printPerlDataAsXml($perlData);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


  is_deeply $xml, trim(<<END);
  <hash>
      <a>1</a>
      <b>
      <array>
                  <e><hash><c><array><e>3</e><e>4</e></array></c></hash></e>
                  <e><hash><d><array><e>5</e><e>6</e></array></d></hash></e>
                  <e><hash><e>7</e></hash></e>
                  </array>
      </b>
      <f>8</f>
      </hash>
  END


=head1 Numbers

Numeric operations,

=head2 powerOfTwo($n)

Test whether a number B<$n> is a power of two, return the power if it is else B<undef>.

     Parameter  Description
  1  $n         Number to check

B<Example:>



  ok  powerOfTwo(1) == 0;                                                           # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok  powerOfTwo(2) == 1;                                                           # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok !powerOfTwo(3);                                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok  powerOfTwo(4) == 2;                                                           # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 containingPowerOfTwo($n)

Find log two of the lowest power of two greater than or equal to a number B<$n>.

     Parameter  Description
  1  $n         Number to check

B<Example:>



  ok  containingPowerOfTwo(1) == 0;                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok  containingPowerOfTwo(2) == 1;                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok  containingPowerOfTwo(3) == 2;                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok  containingPowerOfTwo(4) == 2;                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 Minima and Maxima

Find the smallest and largest elements of arrays.

=head3 min(@m)

Find the minimum number in a list of numbers confessing to any ill defined values.

     Parameter  Description
  1  @m         Numbers

B<Example:>


    ok !max;
    ok max(1) == 1;
    ok max(1,4,2,3) == 4;


    ok min(1) == 1;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok min(5,4,2,3) == 2;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 indexOfMin(@m)

Find the index of the minimum number in a list of numbers confessing to any ill defined values.

     Parameter  Description
  1  @m         Numbers

B<Example:>



    ok indexOfMin(qw(2 3 1 2)) == 2;                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 max(@m)

Find the maximum number in a list of numbers confessing to any ill defined values.

     Parameter  Description
  1  @m         Numbers

B<Example:>



    ok !max;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok max(1) == 1;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok max(1,4,2,3) == 4;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok min(1) == 1;
    ok min(5,4,2,3) == 2;


=head3 indexOfMax(@m)

Find the index of the maximum number in a list of numbers confessing to any ill defined values.

     Parameter  Description
  1  @m         Numbers

B<Example:>



   {ok indexOfMax(qw(2 3 1 2)) == 1;                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 arraySum(@a)

Find the sum of any strings that look like numbers in an array.

     Parameter  Description
  1  @a         Array to sum

B<Example:>



   {ok arraySum   (1..10) ==  55;                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 arrayProduct(@a)

Find the product of any strings that look like numbers in an array.

     Parameter  Description
  1  @a         Array to multiply

B<Example:>



    ok arrayProduct(1..5) == 120;                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head3 arrayTimes($multiplier, @a)

Multiply by B<$multiplier> each element of the array B<@a> and return as the result.

     Parameter    Description
  1  $multiplier  Multiplier
  2  @a           Array to multiply and return

B<Example:>



    is_deeply[arrayTimes(2, 1..5)], [qw(2 4 6 8 10)];                               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 Sets

Set operations.

=head2 mergeHashesBySummingValues(@h)

Merge a list of hashes B<@h> by summing their values

     Parameter  Description
  1  @h         List of hashes to be summed

B<Example:>


    is_deeply +{a=>1, b=>2, c=>3},

      mergeHashesBySummingValues  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

        +{a=>1,b=>1, c=>1}, +{b=>1,c=>1}, +{c=>1};


=head2 invertHashOfHashes($h)

Invert a hash of hashes: given {a}{b} = c return {b}{c} = c

     Parameter  Description
  1  $h         Hash of hashes

B<Example:>


    my $h =  {a=>{A=>q(aA), B=>q(aB)}, b=>{A=>q(bA), B=>q(bB)}};
    my $g =  {A=>{a=>q(aA), b=>q(bA)}, B=>{a=>q(aB), b=>q(bB)}};


    is_deeply invertHashOfHashes($h), $g;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply invertHashOfHashes($g), $h;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 unionOfHashKeys(@h)

Form the union of the keys of the specified hashes B<@h> as one hash whose keys represent the union.

     Parameter  Description
  1  @h         List of hashes to be united

B<Example:>


  if (1)

   {is_deeply  unionOfHashKeys  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ({a=>1,b=>2}, {b=>1,c=>1}, {c=>2}),
      {a=>1, b=>2, c=>2};

    is_deeply  intersectionOfHashKeys
     ({a=>1,b=>2},{b=>1,c=>1},{b=>3,c=>2}),
      {b=>1};
   }


=head2 intersectionOfHashKeys(@h)

Form the intersection of the keys of the specified hashes B<@h> as one hash whose keys represent the intersection.

     Parameter  Description
  1  @h         List of hashes to be intersected

B<Example:>


  if (1)
   {is_deeply  unionOfHashKeys
     ({a=>1,b=>2}, {b=>1,c=>1}, {c=>2}),
      {a=>1, b=>2, c=>2};


    is_deeply  intersectionOfHashKeys  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ({a=>1,b=>2},{b=>1,c=>1},{b=>3,c=>2}),
      {b=>1};
   }


=head2 unionOfHashesAsArrays(@h)

Form the union of the specified hashes B<@h> as one hash whose values are a array of corresponding values from each hash

     Parameter  Description
  1  @h         List of hashes to be united

B<Example:>


  if (1)

   {is_deeply  unionOfHashesAsArrays  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ({a=>1,b=>2}, {b=>1,c=>1}, {c=>2}),
      {a=>[1], b=>[2,1], c=>[undef,1,2]};

    is_deeply  intersectionOfHashesAsArrays
     ({a=>1,b=>2},{b=>1,c=>1},{b=>3,c=>2}),
      {b=>[2,1,3]};
   }


=head2 intersectionOfHashesAsArrays(@h)

Form the intersection of the specified hashes B<@h> as one hash whose values are an array of corresponding values from each hash

     Parameter  Description
  1  @h         List of hashes to be intersected

B<Example:>


  if (1)
   {is_deeply  unionOfHashesAsArrays
     ({a=>1,b=>2}, {b=>1,c=>1}, {c=>2}),
      {a=>[1], b=>[2,1], c=>[undef,1,2]};


    is_deeply  intersectionOfHashesAsArrays  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ({a=>1,b=>2},{b=>1,c=>1},{b=>3,c=>2}),
      {b=>[2,1,3]};
   }


=head2 setUnion(@s)

Union of sets B<@s> represented as arrays of strings and/or the keys of hashes

     Parameter  Description
  1  @s         Array of arrays of strings and/or hashes

B<Example:>



    is_deeply [qw(a b c)],     [setUnion(qw(a b c a a b b b))];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [qw(a b c d e)], [setUnion {a=>1, b=>2, e=>3}, [qw(c d e)], qw(e)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 setIntersection(@s)

Intersection of sets B<@s> represented as arrays of strings and/or the keys of hashes

     Parameter  Description
  1  @s         Array of arrays of strings and/or hashes

B<Example:>



    is_deeply [qw(a b c)], [setIntersection[qw(e f g a b c )],[qw(a A b B c C)]];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [qw(e)],   [setIntersection {a=>1, b=>2, e=>3}, [qw(c d e)], qw(e)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 setIntersectionOverUnion(@s)

Returns the size of the intersection over the size of the union of one or more sets B<@s> represented as arrays and/or hashes

     Parameter  Description
  1  @s         Array of arrays of strings and/or hashes

B<Example:>



    my $f = setIntersectionOverUnion {a=>1, b=>2, e=>3}, [qw(c d e)], qw(e);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $f > 0.199999 && $f < 0.200001;


=head2 setPartitionOnIntersectionOverUnion($confidence, @sets)

Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@sets> so that within each partition the L<setIntersectionOverUnion|/setIntersectionOverUnion> of any two sets in the partition is never less than the specified level of I<$confidence**2>

     Parameter    Description
  1  $confidence  Minimum setIntersectionOverUnion
  2  @sets        Array of arrays of strings and/or hashes representing sets

B<Example:>



    is_deeply [setPartitionOnIntersectionOverUnion  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (0.80,
       [qw(a A   b c d e)],
       [qw(a A B b c d e)],
       [qw(a A B C b c d)],
     )],
    [[["A", "B", "a".."e"],
      ["A",      "a".."e"]],
     [["A".."C", "a".."d"]],
    ];
  }




  if (1) {
  is_deeply [setPartitionOnIntersectionOverUnionOfSetsOfWords
     (0.80,
       [qw(a A   b c d e)],
       [qw(a A B b c d e)],
       [qw(a A B C b c d)],
     )],
   [[["a", "A", "B", "C", "b", "c", "d"]],
    [["a", "A", "B", "b" .. "e"], ["a", "A", "b" .. "e"]],
   ];


=head2 setPartitionOnIntersectionOverUnionOfSetsOfWords($confidence, @sets)

Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@sets> of words so that within each partition the L<setIntersectionOverUnion|/setIntersectionOverUnion> of any two sets of words in the partition is never less than the specified I<$confidence**2>

     Parameter    Description
  1  $confidence  Minimum setIntersectionOverUnion
  2  @sets        Array of arrays of strings and/or hashes representing sets

B<Example:>



  is_deeply [setPartitionOnIntersectionOverUnionOfSetsOfWords  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (0.80,
       [qw(a A   b c d e)],
       [qw(a A B b c d e)],
       [qw(a A B C b c d)],
     )],
   [[["a", "A", "B", "C", "b", "c", "d"]],
    [["a", "A", "B", "b" .. "e"], ["a", "A", "b" .. "e"]],
   ];


=head2 setPartitionOnIntersectionOverUnionOfStringSets($confidence, @strings)

Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@strings>, each set represented by a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified I<$confidence**2>

     Parameter    Description
  1  $confidence  Minimum setIntersectionOverUnion
  2  @strings     Sets represented by strings

B<Example:>



  is_deeply [setPartitionOnIntersectionOverUnionOfStringSets  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (0.80,
       q(The Emu            are seen here sometimes.),
       q(The Emu, Gnu       are seen here sometimes.),
       q(The Emu, Gnu, Colt are seen here.),
     )],
   [["The Emu, Gnu, Colt are seen here."],
    ["The Emu, Gnu       are seen here sometimes.",
     "The Emu            are seen here sometimes.",
    ]];


=head2 setPartitionOnIntersectionOverUnionOfHashStringSets($confidence, $hashSet)

Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<$hashSet> represented by a hash, each hash value being a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified B<$confidence**2> and the partition entries are the hash keys of the string sets.

     Parameter    Description
  1  $confidence  Minimum setIntersectionOverUnion
  2  $hashSet     Sets represented by the hash value strings

B<Example:>



    is_deeply [setPartitionOnIntersectionOverUnionOfHashStringSets  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (0.80,
       {e  =>q(The Emu            are seen here sometimes.),
        eg =>q(The Emu, Gnu       are seen here sometimes.),
        egc=>q(The Emu, Gnu, Colt are seen here.),
       }
     )],
   [["e", "eg"], ["egc"]];


=head2 setPartitionOnIntersectionOverUnionOfHashStringSetsInParallel($confidence, $hashSet)

Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<$hashSet> represented by a hash, each hash value being a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified B<$confidence**2> and the partition entries are the hash keys of the string sets. The partition is performed in square root parallel.

     Parameter    Description
  1  $confidence  Minimum setIntersectionOverUnion
  2  $hashSet     Sets represented by the hash value strings

B<Example:>


    my $N = 8;
    my %s;
    for     my $a('a'..'z')
     {my @w;
      for   my $b('a'..'e')
       {for my $c('a'..'e')
         {push @w, qq($a$b$c);
         }
       }

      for   my $i(1..$N)
       {$s{qq($a$i)} = join ' ', @w;
       }
     }

    my $expected =
     [["a1" .. "a8"],
      ["b1" .. "b8"],
      ["c1" .. "c8"],
      ["d1" .. "d8"],
      ["e1" .. "e8"],
      ["f1" .. "f8"],
      ["g1" .. "g8"],
      ["h1" .. "h8"],
      ["i1" .. "i8"],
      ["j1" .. "j8"],
      ["k1" .. "k8"],
      ["l1" .. "l8"],
      ["m1" .. "m8"],
      ["n1" .. "n8"],
      ["o1" .. "o8"],
      ["p1" .. "p8"],
      ["q1" .. "q8"],
      ["r1" .. "r8"],
      ["s1" .. "s8"],
      ["t1" .. "t8"],
      ["u1" .. "u8"],
      ["v1" .. "v8"],
      ["w1" .. "w8"],
      ["x1" .. "x8"],
      ["y1" .. "y8"],
      ["z1" .. "z8"],
     ];

    is_deeply $expected,
     [setPartitionOnIntersectionOverUnionOfHashStringSets          (0.50, \%s)];

    my $expectedInParallel =
     ["a1 a2 a3 a4 a5 a6 a7 a8",                                                  # Same strings in multiple parallel processes
      "b1 b2 b3 b4 b5 b6 b7 b8",
      "b1 b2 b3 b4 b5 b6 b7 b8",
      "c1 c2 c3 c4 c5 c6 c7 c8",
      "d1 d2 d3 d4 d5 d6 d7 d8",
      "d1 d2 d3 d4 d5 d6 d7 d8",
      "e1 e2 e3 e4 e5 e6 e7 e8",
      "f1 f2 f3 f4 f5 f6 f7 f8",
      "f1 f2 f3 f4 f5 f6 f7 f8",
      "g1 g2 g3 g4 g5 g6 g7 g8",
      "h1 h2 h3 h4 h5 h6 h7 h8",
      "h1 h2 h3 h4 h5 h6 h7 h8",
      "i1 i2 i3 i4 i5 i6 i7 i8",
      "j1 j2 j3 j4 j5 j6 j7 j8",
      "j1 j2 j3 j4 j5 j6 j7 j8",
      "k1 k2 k3 k4 k5 k6 k7 k8",
      "l1 l2 l3 l4 l5 l6 l7 l8",
      "l1 l2 l3 l4 l5 l6 l7 l8",
      "m1 m2 m3 m4 m5 m6 m7 m8",
      "n1 n2 n3 n4 n5 n6 n7 n8",
      "n1 n2 n3 n4 n5 n6 n7 n8",
      "o1 o2 o3 o4 o5 o6 o7 o8",
      "p1 p2 p3 p4 p5 p6 p7 p8",
      "q1 q2 q3 q4 q5 q6 q7 q8",
      "q1 q2 q3 q4 q5 q6 q7 q8",
      "r1 r2 r3 r4 r5 r6 r7 r8",
      "s1 s2 s3 s4 s5 s6 s7 s8",
      "s1 s2 s3 s4 s5 s6 s7 s8",
      "t1 t2 t3 t4 t5 t6 t7 t8",
      "u1 u2 u3 u4 u5 u6 u7 u8",
      "u1 u2 u3 u4 u5 u6 u7 u8",
      "v1 v2 v3 v4 v5 v6 v7 v8",
      "w1 w2 w3 w4 w5 w6 w7 w8",
      "w1 w2 w3 w4 w5 w6 w7 w8",
      "x1 x2 x3 x4 x5 x6 x7 x8",
      "y1 y2 y3 y4 y5 y6 y7 y8",
      "y1 y2 y3 y4 y5 y6 y7 y8",
      "z1 z2 z3 z4 z5 z6 z7 z8",
     ];

    if (1)

     {my @p = setPartitionOnIntersectionOverUnionOfHashStringSetsInParallel  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       (0.50, \%s);

      is_deeply $expectedInParallel, [sort map {join ' ', @$_} @p];
     }


=head2 contains($item, @array)

Returns the indices at which an B<$item> matches elements of the specified B<@array>. If the item is a regular expression then it is matched as one, else it is a number it is matched as a number, else as a string.

     Parameter  Description
  1  $item      Item
  2  @array     Array

B<Example:>



  is_deeply [1],       [contains(1,0..1)];                                          # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  is_deeply [1,3],     [contains(1, qw(0 1 0 1 0 0))];                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  is_deeply [0, 5],    [contains('a', qw(a b c d e a b c d e))];                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  is_deeply [0, 1, 5], [contains(qr(a+), qw(a baa c d e aa b c d e))];              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 countOccurencesInString($inString, $searchFor)

Returns the number of occurrences in B<$inString> of B<$searchFor>.

     Parameter   Description
  1  $inString   String to search in
  2  $searchFor  String to search for.

B<Example:>


  if (1)

   {ok countOccurencesInString(q(a<b>c<b><b>d), q(<b>)) == 3;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head2 partitionStringsOnPrefixBySize()

Partition a hash of strings and associated sizes into partitions with either a maximum size B<$maxSize> or only one element; the hash B<%Sizes> consisting of a mapping {string=>size}; with each partition being named with the shortest string prefix that identifies just the strings in that partition. Returns a list of {prefix => size}... describing each partition.


B<Example:>


  if (1)

   {my $ps = \&partitionStringsOnPrefixBySize;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply {&$ps(1)}, {};
    is_deeply {&$ps(1, 1=>0)},      {q()=>0};
    is_deeply {&$ps(1, 1=>1)},      {q()=>1};
    is_deeply {&$ps(1, 1=>2)},      {1=>2};
    is_deeply {&$ps(1, 1=>1,2=>1)}, {1=>1,2=>1};
    is_deeply {&$ps(2, 11=>1,12=>1, 21=>1,22=>1)}, {1=>2, 2=>2};
    is_deeply {&$ps(2, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>1)}, { 111 => 1, 112 => 1, 113 => 1, 121 => 1, 122 => 1, 123 => 1, 131 => 1, 132 => 1, 133 => 1 };

    for(3..8)
     {is_deeply {&$ps($_, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>1)}, { 11 => 3, 12 => 3, 13 => 3 };
     }

    is_deeply {&$ps(9, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>1)}, { q()=> 9};
    is_deeply {&$ps(3, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>2)}, { 11 => 3, 12 => 3, 131 => 1, 132 => 1, 133 => 2 };
    is_deeply {&$ps(4, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>2)}, { 11 => 3, 12 => 3, 13 => 4 };

   }


=head2 transitiveClosure($h)

Transitive closure of a hash of hashes

     Parameter  Description
  1  $h         Hash of hashes

B<Example:>


  if (1)

   {is_deeply transitiveClosure({a=>{b=>1, c=>2}, b=>{d=>3}, c=>{d=>4}}),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     {end => [{ b => 1, c => 1, d => 4 }, { d => 1 }],
      start => { a => 0, b => 1, c => 1 },
     };
   }


=head1 Format

Format data structures as tables.

=head2 maximumLineLength($string)

Find the longest line in a B<$string>.

     Parameter  Description
  1  $string    String of lines of text

B<Example:>



  ok 3 == maximumLineLength(<<END);                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a
  bb
  ccc
  END


=head2 formatTableBasic($data)

Tabularize an array of arrays of text.

     Parameter  Description
  1  $data      Reference to an array of arrays of data to be formatted as a table.

B<Example:>


    my $d = [[qw(a 1)], [qw(bb 22)], [qw(ccc 333)], [qw(dddd 4444)]];

    ok formatTableBasic($d) eq <<END, q(ftb);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a        1
  bb      22
  ccc    333
  dddd  4444
  END
    }

  if (0) {
    my %pids;
    sub{startProcess {} %pids, 1; ok 1 >= keys %pids}->() for 1..8;
    waitForAllStartedProcessesToFinish(%pids);
    ok !keys(%pids)


=head2 formatTable($data, $columnTitles, @options)

Format various B<$data> structures as a table with titles as specified by B<$columnTitles>: either a reference to an array of column titles or a string each line of which contains the column title as the first word with the rest of the line describing that column.

Optionally create a report from the table using the report B<%options> described in L<formatTableCheckKeys>

     Parameter      Description
  1  $data          Data to be formatted
  2  $columnTitles  Optional reference to an array of titles or string of column descriptions
  3  @options       Options

B<Example:>



  ok formatTable                                                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


   ([[qw(A    B    C    D   )],

     [qw(AA   BB   CC   DD  )],

     [qw(AAA  BBB  CCC  DDD )],

     [qw(AAAA BBBB CCCC DDDD)],

     [qw(1    22   333  4444)]], [qw(aa bb cc)]) eq <<END;
     aa    bb    cc
  1  A     B     C     D
  2  AA    BB    CC    DD
  3  AAA   BBB   CCC   DDD
  4  AAAA  BBBB  CCCC  DDDD
  5     1    22   333  4444
  END


  ok formatTable                                                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


   ([[qw(1     B   C)],

     [qw(22    BB  CC)],

     [qw(333   BBB CCC)],

     [qw(4444  22  333)]], [qw(aa bb cc)]) eq <<END;
     aa    bb   cc
  1     1  B    C
  2    22  BB   CC
  3   333  BBB  CCC
  4  4444   22  333
  END


  ok formatTable                                                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


   ([{aa=>'A',   bb=>'B',   cc=>'C'},

     {aa=>'AA',  bb=>'BB',  cc=>'CC'},

     {aa=>'AAA', bb=>'BBB', cc=>'CCC'},

     {aa=>'1',   bb=>'22',  cc=>'333'}

     ]) eq <<END;
     aa   bb   cc
  1  A    B    C
  2  AA   BB   CC
  3  AAA  BBB  CCC
  4    1   22  333
  END


  ok formatTable                                                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


   ({''=>[qw(aa bb cc)],

      1=>[qw(A B C)],

      22=>[qw(AA BB CC)],

      333=>[qw(AAA BBB CCC)],

      4444=>[qw(1 22 333)]}) eq <<END;
        aa   bb   cc
     1  A    B    C
    22  AA   BB   CC
   333  AAA  BBB  CCC
  4444    1   22  333
  END


  ok formatTable                                                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


   ({1=>{aa=>'A', bb=>'B', cc=>'C'},

     22=>{aa=>'AA', bb=>'BB', cc=>'CC'},

     333=>{aa=>'AAA', bb=>'BBB', cc=>'CCC'},

     4444=>{aa=>'1', bb=>'22', cc=>'333'}}) eq <<END;
        aa   bb   cc
     1  A    B    C
    22  AA   BB   CC
   333  AAA  BBB  CCC
  4444    1   22  333
  END


  ok formatTable({aa=>'A', bb=>'B', cc=>'C'}, [qw(aaaa bbbb)]) eq <<END;            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  aaaa  bbbb
  aa    A
  bb    B
  cc    C
  END

    my $d = temporaryFolder;
    my $f = fpe($d, qw(report txt));                                              # Create a report

    my $t = formatTable  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ([["a",undef], [undef, "b0ac"]],                                           # Data - please replace 0a with a new line
      [undef, "BC"],                                                              # Column titles
      file=>$f,                                                                   # Output file
      head=><<END);                                                               # Header
  Sample report.

  Table has NNNN rows.
  END
    ok -e $f;

    ok readFile($f) eq $t;
    is_deeply nws($t), nws(<<END);
  Sample report.

  Table has 2 rows.

  This file: ${d}report.txt

        BC
  1  a
  2     b
        c
  END
    clearFolder($d, 2);


=head2 formattedTablesReport(@options)

Report of all the reports created. The optional parameters are the same as for L<formatTable|/formatTable>

     Parameter  Description
  1  @options   Options

B<Example:>


    @formatTables = ();

    for my $m(2..8)
     {formatTable([map {[$_, $_*$m]} 1..$m], [q(Single), qq(* $m)],
        title=>qq(Multiply by $m));
     }


    ok nws(formattedTablesReport) eq nws(<<END);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     Rows  Title          File
  1     2  Multiply by 2
  2     3  Multiply by 3
  3     4  Multiply by 4
  4     5  Multiply by 5
  5     6  Multiply by 6
  6     7  Multiply by 7
  7     8  Multiply by 8
  END


=head2 summarizeColumn($data, $column)

Count the number of unique instances of each value a column in a table assumes.

     Parameter  Description
  1  $data      Table == array of arrays
  2  $column    Column number to summarize.

B<Example:>


    is_deeply

     [summarizeColumn([map {[$_]} qw(A B D B C D C D A C D C B B D)], 0)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     [[5, "D"], [4, "B"], [4, "C"], [2, "A"]];

    ok nws(formatTable
     ([map {[split m//, $_]} qw(AA CB CD BC DC DD CD AD AA DC CD CC BB BB BD)],
      [qw(Col-1 Col-2)],
       summarize=>1)) eq nws(<<'END');

  Summary_of_column                - Count of unique values found in each column                     Use the Geany flick capability by placing your cursor on the first word
  Comma_Separated_Values_of_column - Comma separated list of the unique values found in each column  of these lines and pressing control + down arrow to see each sub report.

      Col-1  Col-2
   1  A      A
   2  C      B
   3  C      D
   4  B      C
   5  D      C
   6  D      D
   7  C      D
   8  A      D
   9  A      A
  10  D      C
  11  C      D
  12  C      C
  13  B      B
  14  B      B
  15  B      D

  Summary_of_column_Col-1
     Count  Col-1
  1      5  C
  2      4  B
  3      3  A
  4      3  D

  Comma_Separated_Values_of_column_Col-1: "A","B","C","D"

  Summary_of_column_Col-2
     Count  Col-2
  1      6  D
  2      4  C
  3      3  B
  4      2  A

  Comma_Separated_Values_of_column_Col-2: "A","B","C","D"
  END


=head2 keyCount($maxDepth, $ref)

Count keys down to the specified level.

     Parameter  Description
  1  $maxDepth  Maximum depth to count to
  2  $ref       Reference to an array or a hash

B<Example:>


    my $a = [[1..3],       {map{$_=>1} 1..3}];

    my $h = {a=>[1..3], b=>{map{$_=>1} 1..3}};


    ok keyCount(2, $a) == 6;                                                        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    ok keyCount(2, $h) == 6;                                                        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 formatHtmlTable($data, %options)

Format an array of arrays of scalars as an html table using the  B<%options> described in L<formatTableCheckKeys>.

     Parameter  Description
  1  $data      Data to be formatted
  2  %options   Options

B<Example:>


  if (1)

   {my $t = formatHtmlTable  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ([
        [qw(1 a)],
        [qw(2 b)],
      ],
     title  => q(Sample html table),
     head   => q(Head NNNN rows),
     foot   => q(Footer),
     columns=> <<END,
  source The source number
  target The target letter
  END
     );

    my $T = <<'END';
  <h1>Sample html table</h1>

  <p>Head 2 rows</p>

  <p><table borders="0" cellpadding="10" cellspacing="5">

  <tr><th><span title="The source number">source</span><th><span title="The target letter">target</span>
  <tr><td>1<td>a
  <tr><td>2<td>b
  </table></p>

  <p><pre>
  source  The source number
  target  The target letter

  </pre></p>

  <p>Footer</p>

  <span class="options" style="display: none">{
    columns => "source The source number
target The target letter
",
    foot    => "Footer",
    head    => "Head NNNN rows",
    rows    => 2,
    title   => "Sample html table",
  }</span>
  END

    ok "$t
" eq $T;
   }


=head2 formatHtmlTablesIndex($reports, $title, $url, $columns)

Create an index of html reports.

     Parameter  Description
  1  $reports   Reports folder
  2  $title     Title of report of reports
  3  $url       $url to get files
  4  $columns   Number of columns - defaults to 1

B<Example:>


  if (1)
   {my $reports = temporaryFolder;

    formatHtmlAndTextTables
     ($reports, $reports, q(/cgi-bin/getFile.pl?), q(/a/),
       [[qw(1 /a/a)],
        [qw(2 /a/b)],
       ],
     title   => q(Bad files),
     head    => q(Head NNNN rows),
     foot    => q(Footer),
     file    => q(bad.html),
     facet   => q(files), aspectColor => "red",
     columns => <<END,
  source The source number
  target The target letter
  END
     );

    formatHtmlAndTextTables
     ($reports, $reports, q(/cgi-bin/getFile.pl?file=), q(/a/),
       [[qw(1 /a/a1)],
        [qw(2 /a/b2)],
        [qw(3 /a/b3)],
       ],
     title   => q(Good files),
     head    => q(Head NNNN rows),
     foot    => q(Footer),
     file    => q(good.html),
     facet   => q(files), aspectColor => "green",
     columns => <<END,
  source The source number
  target The target letter
  END
     );

    formatHtmlAndTextTablesWaitPids;


    my $result = formatHtmlTablesIndex($reports, q(TITLE), q(/cgi-bin/getFile.pl?file=));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $result =~ m(3.*Good files);
    ok $result =~ m(2.*Bad files);
  #  ok $result =~ m(green.*>3<.*>Good files);
  #  ok $result =~ m(red.*>2<.*>Bad files);

    clearFolder($reports, 11);
   }


=head2 formatHtmlAndTextTablesWaitPids()

Wait on all table formatting pids to complete


B<Example:>


  if (1)
   {my $reports = temporaryFolder;

    formatHtmlAndTextTables
     ($reports, $reports, q(/cgi-bin/getFile.pl?), q(/a/),
       [[qw(1 /a/a)],
        [qw(2 /a/b)],
       ],
     title   => q(Bad files),
     head    => q(Head NNNN rows),
     foot    => q(Footer),
     file    => q(bad.html),
     facet   => q(files), aspectColor => "red",
     columns => <<END,
  source The source number
  target The target letter
  END
     );

    formatHtmlAndTextTables
     ($reports, $reports, q(/cgi-bin/getFile.pl?file=), q(/a/),
       [[qw(1 /a/a1)],
        [qw(2 /a/b2)],
        [qw(3 /a/b3)],
       ],
     title   => q(Good files),
     head    => q(Head NNNN rows),
     foot    => q(Footer),
     file    => q(good.html),
     facet   => q(files), aspectColor => "green",
     columns => <<END,
  source The source number
  target The target letter
  END
     );


    formatHtmlAndTextTablesWaitPids;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $result = formatHtmlTablesIndex($reports, q(TITLE), q(/cgi-bin/getFile.pl?file=));
    ok $result =~ m(3.*Good files);
    ok $result =~ m(2.*Bad files);
  #  ok $result =~ m(green.*>3<.*>Good files);
  #  ok $result =~ m(red.*>2<.*>Bad files);

    clearFolder($reports, 11);
   }


=head2 formatHtmlAndTextTables($reports, $html, $getFile, $filePrefix, $data, %options)

Create text and html versions of a tabular report

     Parameter    Description
  1  $reports     Folder to contain text reports
  2  $html        Folder to contain html reports
  3  $getFile     L<url|https://en.wikipedia.org/wiki/URL> to get files
  4  $filePrefix  File prefix to be removed from file entries or array of file prefixes
  5  $data        Data
  6  %options     Options

B<Example:>


  if (1)
   {my $reports = temporaryFolder;


    formatHtmlAndTextTables  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ($reports, $reports, q(/cgi-bin/getFile.pl?), q(/a/),
       [[qw(1 /a/a)],
        [qw(2 /a/b)],
       ],
     title   => q(Bad files),
     head    => q(Head NNNN rows),
     foot    => q(Footer),
     file    => q(bad.html),
     facet   => q(files), aspectColor => "red",
     columns => <<END,
  source The source number
  target The target letter
  END
     );


    formatHtmlAndTextTables  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ($reports, $reports, q(/cgi-bin/getFile.pl?file=), q(/a/),
       [[qw(1 /a/a1)],
        [qw(2 /a/b2)],
        [qw(3 /a/b3)],
       ],
     title   => q(Good files),
     head    => q(Head NNNN rows),
     foot    => q(Footer),
     file    => q(good.html),
     facet   => q(files), aspectColor => "green",
     columns => <<END,
  source The source number
  target The target letter
  END
     );

    formatHtmlAndTextTablesWaitPids;

    my $result = formatHtmlTablesIndex($reports, q(TITLE), q(/cgi-bin/getFile.pl?file=));
    ok $result =~ m(3.*Good files);
    ok $result =~ m(2.*Bad files);
  #  ok $result =~ m(green.*>3<.*>Good files);
  #  ok $result =~ m(red.*>2<.*>Bad files);

    clearFolder($reports, 11);
   }


=head1 Lines

Load data structures from lines.

=head2 loadArrayFromLines($string)

Load an array from lines of text in a string.

     Parameter  Description
  1  $string    The string of lines from which to create an array

B<Example:>



    my $s = loadArrayFromLines <<END;                                               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a a
  b b
  END

    is_deeply $s, [q(a a), q(b b)];

    ok formatTable($s) eq <<END;
  0  a a
  1  b b
  END


=head2 loadHashFromLines($string)

Load a hash: first word of each line is the key and the rest is the value.

     Parameter  Description
  1  $string    The string of lines from which to create a hash

B<Example:>



    my $s = loadHashFromLines <<END;                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a 10 11 12
  b 20 21 22
  END

    is_deeply $s, {a => q(10 11 12), b =>q(20 21 22)};

    ok formatTable($s) eq <<END;
  a  10 11 12
  b  20 21 22
  END


=head2 loadArrayArrayFromLines($string)

Load an array of arrays from lines of text: each line is an array of words.

     Parameter  Description
  1  $string    The string of lines from which to create an array of arrays

B<Example:>



    my $s = loadArrayArrayFromLines <<END;                                          # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  A B C
  AA BB CC
  END

    is_deeply $s, [[qw(A B C)], [qw(AA BB CC)]];

    ok formatTable($s) eq <<END;
  1  A   B   C
  2  AA  BB  CC
  END


=head2 loadHashArrayFromLines($string)

Load a hash of arrays from lines of text: the first word of each line is the key, the remaining words are the array contents.

     Parameter  Description
  1  $string    The string of lines from which to create a hash of arrays

B<Example:>



    my $s = loadHashArrayFromLines <<END;                                           # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a A B C
  b AA BB CC
  END

    is_deeply $s, {a =>[qw(A B C)], b => [qw(AA BB CC)] };

    ok formatTable($s) eq <<END;
  a  A   B   C
  b  AA  BB  CC
  END


=head2 loadArrayHashFromLines($string)

Load an array of hashes from lines of text: each line is a hash of words.

     Parameter  Description
  1  $string    The string of lines from which to create an array of arrays

B<Example:>



    my $s = loadArrayHashFromLines <<END;                                           # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  A 1 B 2
  AA 11 BB 22
  END

    is_deeply $s, [{A=>1, B=>2}, {AA=>11, BB=>22}];

    ok formatTable($s) eq <<END;
     A  AA  B  BB
  1  1      2
  2     11     22
  END


=head2 loadHashHashFromLines($string)

Load a hash of hashes from lines of text: the first word of each line is the key, the remaining words are the sub hash contents.

     Parameter  Description
  1  $string    The string of lines from which to create a hash of arrays

B<Example:>



    my $s = loadHashHashFromLines <<END;                                            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a A 1 B 2
  b AA 11 BB 22
  END

    is_deeply $s, {a=>{A=>1, B=>2}, b=>{AA=>11, BB=>22}};

    ok formatTable($s) eq <<END;
     A  AA  B  BB
  a  1      2
  b     11     22
  END


=head2 checkKeys($hash, $permitted)

Check the keys in a B<hash> conform to those B<$permitted>.

     Parameter   Description
  1  $hash       The hash to test
  2  $permitted  A hash of the permitted keys and their meanings

B<Example:>



    eval q{checkKeys({a=>1, b=>2, d=>3}, {a=>1, b=>2, c=>3})};                      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok nws($@) =~ m(\AInvalid options chosen: d Permitted.+?: a 1 b 2 c 3);


=head1 LVALUE methods

Replace $a->{B<value>} = $b with $a->B<value> = $b which reduces the amount of typing required, is easier to read and provides a hard check that {B<value>} is spelled correctly.

=head2 genLValueScalarMethods(@names)

Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> scalar methods in the current package, A method whose value has not yet been set will return a new scalar with value B<undef>. Suffixing B<X> to the scalar name will confess if a value has not been set.

     Parameter  Description
  1  @names     List of method names

B<Example:>


    package Scalars;

    my $a = bless{};


    Data::Table::Text::genLValueScalarMethods(qw(aa bb cc));                        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    $a->aa = 'aa';

    Test::More::ok  $a->aa eq 'aa';

    Test::More::ok !$a->bb;

    Test::More::ok  $a->bbX eq q();

    $a->aa = undef;

    Test::More::ok !$a->aa;


=head2 addLValueScalarMethods(@names)

Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> scalar methods in the current package if they do not already exist. A method whose value has not yet been set will return a new scalar with value B<undef>. Suffixing B<X> to the scalar name will confess if a value has not been set.

     Parameter  Description
  1  @names     List of method names

B<Example:>


    my $class = "Data::Table::Text::Test";

    my $a = bless{}, $class;


    addLValueScalarMethods(qq(${class}::$_)) for qw(aa bb aa bb);                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    $a->aa = 'aa';

    ok  $a->aa eq 'aa';

    ok !$a->bb;

    ok  $a->bbX eq q();

    $a->aa = undef;

    ok !$a->aa;


=head2 genLValueScalarMethodsWithDefaultValues(@names)

Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> scalar methods with default values in the current package. A reference to a method whose value has not yet been set will return a scalar whose value is the name of the method.

     Parameter  Description
  1  @names     List of method names

B<Example:>


    package ScalarsWithDefaults;

    my $a = bless{};


    Data::Table::Text::genLValueScalarMethodsWithDefaultValues(qw(aa bb cc));       # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    Test::More::ok $a->aa eq 'aa';


=head2 genLValueArrayMethods(@names)

Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> array methods in the current package. A reference to a method that has no yet been set will return a reference to an empty array.

     Parameter  Description
  1  @names     List of method names

B<Example:>


    package Arrays;

    my $a = bless{};


    Data::Table::Text::genLValueArrayMethods(qw(aa bb cc));                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    $a->aa->[1] = 'aa';

    Test::More::ok $a->aa->[1] eq 'aa';


=head2 genLValueHashMethods(@names)

Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> hash methods in the current package. A reference to a method that has no yet been set will return a reference to an empty hash.

     Parameter  Description
  1  @names     Method names

B<Example:>


    package Hashes;

    my $a = bless{};


    Data::Table::Text::genLValueHashMethods(qw(aa bb cc));                          # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    $a->aa->{a} = 'aa';

    Test::More::ok $a->aa->{a} eq 'aa';


=head2 genHash($bless, %attributes)

Return a B<$bless>ed hash with the specified B<$attributes> accessible via L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> method calls. L<updateDocumentation|/updateDocumentation> will generate documentation at L<Hash Definitions> for the hash defined by the call to L<genHash|/genHash> if the call is laid out as in the example below.

     Parameter    Description
  1  $bless       Package name
  2  %attributes  Hash of attribute names and values

B<Example:>



    my $o = genHash(q(TestHash),                                                  # Definition of a blessed hash.  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

        a=>q(aa),                                                                 # Definition of attribute aa.
        b=>q(bb),                                                                 # Definition of attribute bb.
       );
    ok $o->a eq q(aa);
    is_deeply $o, {a=>"aa", b=>"bb"};

    my $p = genHash(q(TestHash),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      c=>q(cc),                                                                   # Definition of attribute cc.
     );
    ok $p->c eq q(cc);
    ok $p->a =  q(aa);
    ok $p->a eq q(aa);
    is_deeply $p, {a=>"aa", c=>"cc"};

    loadHash($p, a=>11, b=>22);                                                   # Load the hash
    is_deeply $p, {a=>11, b=>22, c=>"cc"};

    my $r = eval {loadHash($p, d=>44)};                                           # Try to load the hash
    ok $@ =~ m(Cannot load attribute: d);


=head2 loadHash($hash, %attributes)

Load the specified blessed B<$hash> generated with L<genHash|/genHash> with B<%attributes>. Confess to any unknown attribute names.

     Parameter    Description
  1  $hash        Hash
  2  %attributes  Hash of attribute names and values to be loaded

B<Example:>


    my $o = genHash(q(TestHash),                                                  # Definition of a blessed hash.
        a=>q(aa),                                                                 # Definition of attribute aa.
        b=>q(bb),                                                                 # Definition of attribute bb.
       );
    ok $o->a eq q(aa);
    is_deeply $o, {a=>"aa", b=>"bb"};
    my $p = genHash(q(TestHash),
      c=>q(cc),                                                                   # Definition of attribute cc.
     );
    ok $p->c eq q(cc);
    ok $p->a =  q(aa);
    ok $p->a eq q(aa);
    is_deeply $p, {a=>"aa", c=>"cc"};


    loadHash($p, a=>11, b=>22);                                                   # Load the hash  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $p, {a=>11, b=>22, c=>"cc"};


    my $r = eval {loadHash($p, d=>44)};                                           # Try to load the hash  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $@ =~ m(Cannot load attribute: d);


=head2 reloadHashes($d)

Ensures that all the hashes within a tower of data structures have LValue methods to get and set their current keys.

     Parameter  Description
  1  $d         Data structure

B<Example:>


  if (1)
   {my $a = bless [bless {aaa=>42}, "AAAA"], "BBBB";
    eval {$a->[0]->aaa};
    ok $@ =~ m(\ACan.t locate object method .aaa. via package .AAAA.);

    reloadHashes($a);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $a->[0]->aaa == 42;
   }

  if (1)
   {my $a = bless [bless {ccc=>42}, "CCCC"], "DDDD";
    eval {$a->[0]->ccc};
    ok $@ =~ m(\ACan.t locate object method .ccc. via package .CCCC.);

    reloadHashes($a);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $a->[0]->ccc == 42;
   }


=head2 setPackageSearchOrder($set, @search)

Set a package search order for methods requested in the current package via AUTOLOAD.

     Parameter  Description
  1  $set       Package to set
  2  @search    Package names in search order.

B<Example:>


  if (1)
   {if (1)
     {package AAAA;

      sub aaaa{q(AAAAaaaa)}
      sub bbbb{q(AAAAbbbb)}
      sub cccc{q(AAAAcccc)}
     }
    if (1)
     {package BBBB;

      sub aaaa{q(BBBBaaaa)}
      sub bbbb{q(BBBBbbbb)}
      sub dddd{q(BBBBdddd)}
     }
    if (1)
     {package CCCC;

      sub aaaa{q(CCCCaaaa)}
      sub dddd{q(CCCCdddd)}
      sub eeee{q(CCCCeeee)}
     }


    setPackageSearchOrder(__PACKAGE__, qw(CCCC BBBB AAAA));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok &aaaa eq q(CCCCaaaa);
    ok &bbbb eq q(BBBBbbbb);
    ok &cccc eq q(AAAAcccc);

    ok &aaaa eq q(CCCCaaaa);
    ok &bbbb eq q(BBBBbbbb);
    ok &cccc eq q(AAAAcccc);

    ok &dddd eq q(CCCCdddd);
    ok &eeee eq q(CCCCeeee);


    setPackageSearchOrder(__PACKAGE__, qw(AAAA BBBB CCCC));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok &aaaa eq q(AAAAaaaa);
    ok &bbbb eq q(AAAAbbbb);
    ok &cccc eq q(AAAAcccc);

    ok &aaaa eq q(AAAAaaaa);
    ok &bbbb eq q(AAAAbbbb);
    ok &cccc eq q(AAAAcccc);

    ok &dddd eq q(BBBBdddd);
    ok &eeee eq q(CCCCeeee);
   }


=head2 isSubInPackage($package, $sub)

Test whether the specified B<$package> contains the subroutine <$sub>.

     Parameter  Description
  1  $package   Package name
  2  $sub       Subroutine name

B<Example:>


  if (1)
   {sub AAAA::Call {q(AAAA)}

    sub BBBB::Call {q(BBBB)}
    sub BBBB::call {q(bbbb)}

    if (1)
     {package BBBB;
      use Test::More;
      *ok = *Test::More::ok;

      *isSubInPackage = *Data::Table::Text::isSubInPackage;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok  isSubInPackage(q(AAAA), q(Call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok !isSubInPackage(q(AAAA), q(call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok  isSubInPackage(q(BBBB), q(Call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok  isSubInPackage(q(BBBB), q(call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      ok Call eq q(BBBB);
      ok call eq q(bbbb);
      &Data::Table::Text::overrideMethods(qw(AAAA BBBB Call call));

      *isSubInPackage = *Data::Table::Text::isSubInPackage;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok  isSubInPackage(q(AAAA), q(Call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok  isSubInPackage(q(AAAA), q(call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok  isSubInPackage(q(BBBB), q(Call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      ok  isSubInPackage(q(BBBB), q(call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      ok Call eq q(AAAA);
      ok call eq q(bbbb);
      package AAAA;
      use Test::More;
      *ok = *Test::More::ok;
      ok  Call eq q(AAAA);
      ok &call eq q(bbbb);
     }
   }


=head2 overrideMethods($from, $to, @methods)

For each method, if it exists in package B<$from> then export it to package B<$to> replacing any existing method in B<$to>, otherwise export the method from package B<$to> to package B<$from> in order to merge the behavior of the B<$from> and B<$to> packages with respect to the named methods with duplicates resolved if favour of package B<$from>.

     Parameter  Description
  1  $from      Name of package from which to import methods
  2  $to        Package into which to import the methods
  3  @methods   List of methods to try importing.

B<Example:>


  if (1)
   {sub AAAA::Call {q(AAAA)}

    sub BBBB::Call {q(BBBB)}
    sub BBBB::call {q(bbbb)}

    if (1)
     {package BBBB;
      use Test::More;
      *ok = *Test::More::ok;
      *isSubInPackage = *Data::Table::Text::isSubInPackage;
      ok  isSubInPackage(q(AAAA), q(Call));
      ok !isSubInPackage(q(AAAA), q(call));
      ok  isSubInPackage(q(BBBB), q(Call));
      ok  isSubInPackage(q(BBBB), q(call));
      ok Call eq q(BBBB);
      ok call eq q(bbbb);

      &Data::Table::Text::overrideMethods(qw(AAAA BBBB Call call));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      *isSubInPackage = *Data::Table::Text::isSubInPackage;
      ok  isSubInPackage(q(AAAA), q(Call));
      ok  isSubInPackage(q(AAAA), q(call));
      ok  isSubInPackage(q(BBBB), q(Call));
      ok  isSubInPackage(q(BBBB), q(call));
      ok Call eq q(AAAA);
      ok call eq q(bbbb);
      package AAAA;
      use Test::More;
      *ok = *Test::More::ok;
      ok  Call eq q(AAAA);
      ok &call eq q(bbbb);
     }
   }


This is a static method and so should either be imported or invoked as:

  Data::Table::Text::overrideMethods


=head2 overrideAndReabsorbMethods(@packages)

Override methods down the list of B<@packages> then reabsorb any unused methods back up the list of packages so that all the packages have the same methods as the last package with methods from packages mentioned earlier overriding methods from packages mentioned later.  The methods to override and reabsorb are listed by the sub B<overridableMethods> in the last package in the packages list. Confess to any errors.

     Parameter  Description
  1  @packages  List of packages

B<Example:>



    ok overrideAndReabsorbMethods(qw(main Edit::Xml Data::Edit::Xml));              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



This is a static method and so should either be imported or invoked as:

  Data::Table::Text::overrideAndReabsorbMethods


=head2 assertPackageRefs($package, @refs)

Confirm that the specified references are to the specified package

     Parameter  Description
  1  $package   Package
  2  @refs      References

B<Example:>



    eval q{assertPackageRefs(q(bbb), bless {}, q(aaa))};  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $@ =~ m(\AWanted reference to bbb, but got aaa);


=head2 assertRef(@refs)

Confirm that the specified references are to the package into which this routine has been exported.

     Parameter  Description
  1  @refs      References

B<Example:>



    eval q{assertRef(bless {}, q(aaa))};  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $@ =~ m(\AWanted reference to Data::Table::Text, but got aaa);


=head2 arrayToHash(@array)

Create a hash reference from an array

     Parameter  Description
  1  @array     Array

B<Example:>



  is_deeply arrayToHash(qw(a b c)), {a=>1, b=>1, c=>1};                             # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 flattenArrayAndHashValues(@array)

Flatten an array of scalars, array and hash references to make an array of scalars by flattening the array references and hash values.

     Parameter  Description
  1  @array     Array to flatten

B<Example:>



  is_deeply [1..5], [flattenArrayAndHashValues([1], [[2]], {a=>3, b=>[4, [5]]})], 'ggg';   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 getSubName($sub)

Returns the (package, name, file, line) of a perl B<$sub> reference.

     Parameter  Description
  1  $sub       Reference to a sub with a name.

B<Example:>



  is_deeply [(getSubName(\&dateTime))[0,1]], ["Data::Table::Text", "dateTime"];     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 Strings

Actions on strings.

=head2 stringMd5Sum($string)

Get the Md5 sum of a B<$string> that might contain L<utf8|https://en.wikipedia.org/wiki/UTF-8> code points.

     Parameter  Description
  1  $string    String

B<Example:>


    my $s = join '', 1..100;
    my $m = q(ef69caaaeea9c17120821a9eb6c7f1de);


    ok stringMd5Sum($s) eq $m;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $f = writeFile(undef, $s);
    ok fileMd5Sum($f) eq $m;
    unlink $f;

    ok guidFromString(join '', 1..100) eq
       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);


    ok guidFromMd5(stringMd5Sum(join('', 1..100))) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

    ok md5FromGuid(q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de)) eq
                        q(ef69caaaeea9c17120821a9eb6c7f1de);


    ok stringMd5Sum(q(𝝰 𝝱 𝝲)) eq q(3c2b7c31b1011998bd7e1f66fb7c024d);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  }

  if (1)
   {ok arraySum   (1..10) ==  55;
    ok arrayProduct(1..5) == 120;
    is_deeply[arrayTimes(2, 1..5)], [qw(2 4 6 8 10)];


=head2 indentString($string, $indent)

Indent lines contained in a string or formatted table by the specified string.

     Parameter  Description
  1  $string    The string of lines to indent
  2  $indent    The indenting string

B<Example:>


    my $t = [qw(aa bb cc)];
    my $d = [[qw(A B C)], [qw(AA BB CC)], [qw(AAA BBB CCC)],  [qw(1 22 333)]];

    my $s = indentString(formatTable($d), '  ')."
";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok $s eq <<END;
    1  A    B    C
    2  AA   BB   CC
    3  AAA  BBB  CCC
    4    1   22  333
  END


=head2 replaceStringWithString($string, $source, $target)

Replace all instances in B<$string> of B<$source> with B<$target>

     Parameter  Description
  1  $string    String in which to replace substrings
  2  $source    The string to be replaced
  3  $target    The replacement string

B<Example:>



  ok replaceStringWithString(q(abababZ), q(ab), q(c)) eq q(cccZ), 'eee';            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 formatString($string, $width)

Format the specified B<$string> so it can be displayed in B<$width> columns.

     Parameter  Description
  1  $string    The string of text to format
  2  $width     The formatted width.

B<Example:>



  ok formatString(<<END, 16) eq  <<END, 'fff';                                      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  Now is the time for all
  good men to come to the rescue
  of the ailing B<party>.
  END


=head2 isBlank($string)

Test whether a string is blank.

     Parameter  Description
  1  $string    String

B<Example:>



  ok isBlank("");                                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok isBlank("
 ");                                                               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 trim($string)

Remove any white space from the front and end of a string.

     Parameter  Description
  1  $string    String

B<Example:>



  ok trim(" a b ") eq join ' ', qw(a b);                                            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 pad($string, $length, $padding)

Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.

     Parameter  Description
  1  $string    String
  2  $length    Tab width
  3  $padding   Padding string

B<Example:>



    is_deeply pad('abc  ', 2).'='         , "abc =";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply pad('abc  ', 3).'='         , "abc=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply pad('abc  ', 4, q(.)).'='   , "abc.=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply pad('abc  ', 5).'='         , "abc  =";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply pad('abc  ', 6).'='         , "abc   =";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply  ppp(2, 'abc  ').'='        , "abc =";
    is_deeply  ppp(3, 'abc  ').'='        , "abc=";
    is_deeply  ppp(4, 'abc  ', q(.)).'='  , "abc.=";
    is_deeply  ppp(5, 'abc  ').'='        , "abc  =";
    is_deeply  ppp(6, 'abc  ').'='        , "abc   =";

    is_deeply lpad('abc  ', 2).'='        , " abc=";
    is_deeply lpad('abc  ', 3).'='        , "abc=";
    is_deeply lpad('abc  ', 4, q(.)).'='  , ".abc=";
    is_deeply lpad('abc  ', 5).'='        , "  abc=";
    is_deeply lpad('abc  ', 6).'='        , "   abc=";


=head2 lpad($string, $length, $padding)

Left Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.

     Parameter  Description
  1  $string    String
  2  $length    Tab width
  3  $padding   Padding string

B<Example:>


    is_deeply pad('abc  ', 2).'='         , "abc =";
    is_deeply pad('abc  ', 3).'='         , "abc=";
    is_deeply pad('abc  ', 4, q(.)).'='   , "abc.=";
    is_deeply pad('abc  ', 5).'='         , "abc  =";
    is_deeply pad('abc  ', 6).'='         , "abc   =";

    is_deeply  ppp(2, 'abc  ').'='        , "abc =";
    is_deeply  ppp(3, 'abc  ').'='        , "abc=";
    is_deeply  ppp(4, 'abc  ', q(.)).'='  , "abc.=";
    is_deeply  ppp(5, 'abc  ').'='        , "abc  =";
    is_deeply  ppp(6, 'abc  ').'='        , "abc   =";


    is_deeply lpad('abc  ', 2).'='        , " abc=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply lpad('abc  ', 3).'='        , "abc=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply lpad('abc  ', 4, q(.)).'='  , ".abc=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply lpad('abc  ', 5).'='        , "  abc=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply lpad('abc  ', 6).'='        , "   abc=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 ppp($length, $string, $padding)

Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.

     Parameter  Description
  1  $length    Tab width
  2  $string    String
  3  $padding   Padding string

B<Example:>


    is_deeply pad('abc  ', 2).'='         , "abc =";
    is_deeply pad('abc  ', 3).'='         , "abc=";
    is_deeply pad('abc  ', 4, q(.)).'='   , "abc.=";
    is_deeply pad('abc  ', 5).'='         , "abc  =";
    is_deeply pad('abc  ', 6).'='         , "abc   =";


    is_deeply  ppp(2, 'abc  ').'='        , "abc =";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply  ppp(3, 'abc  ').'='        , "abc=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply  ppp(4, 'abc  ', q(.)).'='  , "abc.=";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply  ppp(5, 'abc  ').'='        , "abc  =";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply  ppp(6, 'abc  ').'='        , "abc   =";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply lpad('abc  ', 2).'='        , " abc=";
    is_deeply lpad('abc  ', 3).'='        , "abc=";
    is_deeply lpad('abc  ', 4, q(.)).'='  , ".abc=";
    is_deeply lpad('abc  ', 5).'='        , "  abc=";
    is_deeply lpad('abc  ', 6).'='        , "   abc=";


=head2 firstNChars($string, $length)

First N characters of a string.

     Parameter  Description
  1  $string    String
  2  $length    Length

B<Example:>



  ok firstNChars(q(abc), 2) eq q(ab);                                               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



  ok firstNChars(q(abc), 4) eq q(abc);                                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 nws($string, $length)

Normalize white space in a string to make comparisons easier. Leading and trailing white space is removed; blocks of white space in the interior are reduced to a single space.  In effect: this puts everything on one long line with never more than one space at a time. Optionally a maximum length is applied to the normalized string.

     Parameter  Description
  1  $string    String to normalize
  2  $length    Maximum length of result

B<Example:>



  ok nws(qq(a  b    c)) eq q(a b c);                                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 deduplicateSequentialWordsInString($s)

Remove sequentially duplicate words in a string

     Parameter  Description
  1  $s         String to deduplicate

B<Example:>



    ok deduplicateSequentialWordsInString(<<END) eq qq(\(aa \[bb \-cc dd ee
);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  (aa [bb bb -cc cc dd dd dd dd ee ee ee ee
  END


=head2 detagString($string)

Remove L<HTML|https://en.wikipedia.org/wiki/HTML> or L<Xml|https://en.wikipedia.org/wiki/XML> tags from a string

     Parameter  Description
  1  $string    String to detag

B<Example:>



  ok detagString(q(<a><a href="aaaa">a </a><a/>b </a>c)) eq q(a b c), 'hhh';        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 parseIntoWordsAndStrings($string)

Parse a B<$string> into words and quoted strings. A quote following a space introduces a string, else a quote is just part of the containing word.

     Parameter  Description
  1  $string    String to parse

B<Example:>


  if (1)
   {is_deeply

     [parseIntoWordsAndStrings(  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  q( aa12!    a'b   "aa !! ++ bb"  '  ',      '"'  "'"  ""   ''.))  ],
   ["aa12!", "a'b", "aa !! ++ bb", "  ", ",", '"', "'", "",  "", '.'];
   }


=head2 stringsAreNotEqual($a, $b)

Return the common start followed by the two non equal tails of two non equal strings or an empty list if the strings are equal.

     Parameter  Description
  1  $a         First string
  2  $b         Second string

B<Example:>



    ok        !stringsAreNotEqual(q(abc), q(abc));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok         stringsAreNotEqual(q(abc), q(abd));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [stringsAreNotEqual(q(abc), q(abd))], [qw(ab c d)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [stringsAreNotEqual(q(ab),  q(abd))], [q(ab), '', q(d)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply showGotVersusWanted("aaaa
bbbb
cccc
dddd
",
                                  "aaaa
bbbb
ccee
ffff
"), <<END;
  Comparing wanted with got failed at line: 3, character: 3
  Start:
  aaaa
  bbbb
  cc
  Want ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ee
  ffff

  Got  ________________________________________________________________________________

  cc
  dddd
  END


=head2 showGotVersusWanted($g, $e)

Show the difference between the wanted string and the wanted string

     Parameter  Description
  1  $g         First string
  2  $e         Second string

B<Example:>


    ok        !stringsAreNotEqual(q(abc), q(abc));
    ok         stringsAreNotEqual(q(abc), q(abd));
    is_deeply [stringsAreNotEqual(q(abc), q(abd))], [qw(ab c d)];
    is_deeply [stringsAreNotEqual(q(ab),  q(abd))], [q(ab), '', q(d)];

    is_deeply showGotVersusWanted("aaaa
bbbb
cccc
dddd
",  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

                                  "aaaa
bbbb
ccee
ffff
"), <<END;
  Comparing wanted with got failed at line: 3, character: 3
  Start:
  aaaa
  bbbb
  cc
  Want ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ee
  ffff

  Got  ________________________________________________________________________________

  cc
  dddd
  END


=head2 printQw(@words)

Print an array of words in qw() format.

     Parameter  Description
  1  @words     Array of words

B<Example:>



  is_deeply       printQw(qw(a b c)),    q(qw(a b c));                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 numberOfLinesInString($string)

The number of lines in a string.

     Parameter  Description
  1  $string    String

B<Example:>



    ok numberOfLinesInString("a
b
") == 2;                                        # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 javaPackage($java)

Extract the package name from a java string or file.

     Parameter  Description
  1  $java      Java file if it exists else the string of java

B<Example:>


    my $j = writeFile(undef, <<END);
  // Test
  package com.xyz;
  END

    ok javaPackage($j)           eq "com.xyz";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok javaPackageAsFileName($j) eq "com/xyz";
    unlink $j;

    my $p = writeFile(undef, <<END);
  package a::b;
  END
    ok perlPackage($p)           eq "a::b";
    unlink $p;


=head2 javaPackageAsFileName($java)

Extract the package name from a java string or file and convert it to a file name.

     Parameter  Description
  1  $java      Java file if it exists else the string of java

B<Example:>


    my $j = writeFile(undef, <<END);
  // Test
  package com.xyz;
  END
    ok javaPackage($j)           eq "com.xyz";

    ok javaPackageAsFileName($j) eq "com/xyz";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $j;

    my $p = writeFile(undef, <<END);
  package a::b;
  END
    ok perlPackage($p)           eq "a::b";
    unlink $p;


=head2 perlPackage($perl)

Extract the package name from a perl string or file.

     Parameter  Description
  1  $perl      Perl file if it exists else the string of perl

B<Example:>


    my $j = writeFile(undef, <<END);
  // Test
  package com.xyz;
  END
    ok javaPackage($j)           eq "com.xyz";
    ok javaPackageAsFileName($j) eq "com/xyz";
    unlink $j;

    my $p = writeFile(undef, <<END);
  package a::b;
  END

    ok perlPackage($p)           eq "a::b";                                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    unlink $p;

    my $p = writeFile(undef, <<END);
  package a::b;
  END


    ok perlPackage($p)           eq "a::b";                                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 javaScriptExports($fileOrString)

Extract the Javascript functions marked for export in a file or string.  Functions are marked for export by placing function in column 1 followed by //E on the same line.  The end of the exported function is located by
 }

     Parameter      Description
  1  $fileOrString  File or string

B<Example:>



    ok javaScriptExports(<<END) eq <<END;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  function aaa()            //E
   {console.log('aaa');


=head2 chooseStringAtRandom(@strings)

Choose a string at random from the list of B<@strings> supplied.

     Parameter  Description
  1  @strings   Strings to chose from

B<Example:>



  ok q(a) eq chooseStringAtRandom(qw(a a a a));                                     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 randomizeArray(@a)

Randomize an array

     Parameter  Description
  1  @a         Array to randomize

B<Example:>



  is_deeply [randomizeArray(qw(a a a a))], [qw(a a a a)];                             # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 Arrays and Hashes

Operations on arrays and hashes and array of of hashesh and ghashes of arrays and  so on a infinitum.

=head2 lengthOfLongestSubArray($a)

Given an array of arrays find the length of the longest sub array.

     Parameter  Description
  1  $a         Array reference

B<Example:>


  if (1)

   {ok 3 == lengthOfLongestSubArray [[1..2], [1..3], [1..3], []];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head2 cmpArrays($a, $b)

Compare two arrays of strings

     Parameter  Description
  1  $a         Array A
  2  $b         Array B

B<Example:>



    ok cmpArrays([qw(a b)],   [qw(a a)])   == +1;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok cmpArrays([qw(a b)],   [qw(a c)])   == -1;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok cmpArrays([qw(a b)],   [qw(a b a)]) == -1;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok cmpArrays([qw(a b a)], [qw(a b)])   == +1;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok cmpArrays([qw(a b)],   [qw(a b)])   ==  0;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 forEachKeyAndValue($body, %hash)

Iterate over a hash for each key and value

     Parameter  Description
  1  $body      Body to be executed
  2  %hash      Hash to be iterated

B<Example:>



    forEachKeyAndValue  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     {my ($letter, $number) = @_;
      push @t,  "Letter=$letter, number=$number";
     } %h;

    is_deeply join("
", @t, ''), <<END;
  Letter=a, number=1
  Letter=b, number=2
  Letter=c, number=3
  END
  }

  if ($localTest)
   {say STDERR "DTT finished in ", (time() - $timeStart), " seconds";


=head1 Unicode

Translate L<Ascii|https://en.wikipedia.org/wiki/ASCII> alphanumerics in strings to various L<Unicode|https://en.wikipedia.org/wiki/Unicode> blocks.

=head2 mathematicalItalicString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Italic.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalItalicString                 (q(APPLES and ORANGES)) eq q(𝐴𝑃𝑃𝐿𝐸𝑆 𝑎𝑛𝑑 𝑂𝑅𝐴𝑁𝐺𝐸𝑆);    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalBoldString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalBoldString                   (q(APPLES and ORANGES)) eq q(𝐀𝐏𝐏𝐋𝐄𝐒 𝐚𝐧𝐝 𝐎𝐑𝐀𝐍𝐆𝐄𝐒);    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalBoldStringUndo($string)

Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold..

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalBoldStringUndo               (q(𝐀𝐏𝐏𝐋𝐄𝐒 𝐚𝐧𝐝 𝐎𝐑𝐀𝐍𝐆𝐄𝐒)) eq q(APPLES and ORANGES);    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalBoldItalicString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold Italic.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalBoldItalicString             (q(APPLES and ORANGES)) eq q(𝑨𝑷𝑷𝑳𝑬𝑺 𝒂𝒏𝒅 𝑶𝑹𝑨𝑵𝑮𝑬𝑺);     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalBoldItalicStringUndo($string)

Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold Italic.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalBoldItalicStringUndo         (q(𝑨𝑷𝑷𝑳𝑬𝑺 𝒂𝒏𝒅 𝑶𝑹𝑨𝑵𝑮𝑬𝑺))  eq q(APPLES and ORANGES);    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifString              (q(APPLES and ORANGES)) eq q(𝖠𝖯𝖯𝖫𝖤𝖲 𝖺𝗇𝖽 𝖮𝖱𝖠𝖭𝖦𝖤𝖲);     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifStringUndo($string)

Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifStringUndo          (q(𝖠𝖯𝖯𝖫𝖤𝖲 𝖺𝗇𝖽 𝖮𝖱𝖠𝖭𝖦𝖤𝖲))  eq q(APPLES and ORANGES);    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifBoldString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifBoldString          (q(APPLES and ORANGES)) eq q(𝗔𝗣𝗣𝗟𝗘𝗦 𝗮𝗻𝗱 𝗢𝗥𝗔𝗡𝗚𝗘𝗦);     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifBoldStringUndo($string)

Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifBoldStringUndo      (q(𝗔𝗣𝗣𝗟𝗘𝗦 𝗮𝗻𝗱 𝗢𝗥𝗔𝗡𝗚𝗘𝗦)) eq q(APPLES and ORANGES);     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifItalicString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Italic.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifItalicString        (q(APPLES and ORANGES)) eq q(𝘈𝘗𝘗𝘓𝘌𝘚 𝘢𝘯𝘥 𝘖𝘙𝘈𝘕𝘎𝘌𝘚);      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifItalicStringUndo($string)

Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Italic.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifItalicStringUndo    (q(𝘈𝘗𝘗𝘓𝘌𝘚 𝘢𝘯𝘥 𝘖𝘙𝘈𝘕𝘎𝘌𝘚)) eq q(APPLES and ORANGES);      # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifBoldItalicString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold Italic.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifBoldItalicString    (q(APPLES and ORANGES)) eq q(𝘼𝙋𝙋𝙇𝙀𝙎 𝙖𝙣𝙙 𝙊𝙍𝘼𝙉𝙂𝙀𝙎);     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalSansSerifBoldItalicStringUndo($string)

Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold Italic.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalSansSerifBoldItalicStringUndo(q(𝘼𝙋𝙋𝙇𝙀𝙎 𝙖𝙣𝙙 𝙊𝙍𝘼𝙉𝙂𝙀𝙎)) eq q(APPLES and ORANGES);     # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalMonoSpaceString($string)

Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical MonoSpace.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalMonoSpaceString              (q(APPLES and ORANGES)) eq q(𝙰𝙿𝙿𝙻𝙴𝚂 𝚊𝚗𝚍 𝙾𝚁𝙰𝙽𝙶𝙴𝚂);    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 mathematicalMonoSpaceStringUndo($string)

Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical MonoSpace.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok mathematicalMonoSpaceStringUndo          (q(𝙰𝙿𝙿𝙻𝙴𝚂 𝚊𝚗𝚍 𝙾𝚁𝙰𝙽𝙶𝙴𝚂)) eq q(APPLES and ORANGES);    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 boldString($string)

Convert alphanumerics in a string to bold.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok boldString(q(zZ)) eq q(𝘇𝗭);                                                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 boldStringUndo($string)

Undo alphanumerics in a string to bold.

     Parameter  Description
  1  $string    String to convert

B<Example:>


  if (1)
   {my $n = 1234567890;

    ok boldStringUndo            (boldString($n))             == $n;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok enclosedStringUndo        (enclosedString($n))         == $n;
    ok enclosedReversedStringUndo(enclosedReversedString($n)) == $n;
    ok superScriptStringUndo     (superScriptString($n))      == $n;
    ok subScriptStringUndo       (subScriptString($n))        == $n;
   }


=head2 enclosedString($string)

Convert alphanumerics in a string to enclosed alphanumerics.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok enclosedString(q(hello world 1234)) eq q(ⓗⓔⓛⓛⓞ ⓦⓞⓡⓛⓓ ①②③④);           # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 enclosedStringUndo($string)

Undo alphanumerics in a string to enclosed alphanumerics.

     Parameter  Description
  1  $string    String to convert

B<Example:>


  if (1)
   {my $n = 1234567890;
    ok boldStringUndo            (boldString($n))             == $n;

    ok enclosedStringUndo        (enclosedString($n))         == $n;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok enclosedReversedStringUndo(enclosedReversedString($n)) == $n;
    ok superScriptStringUndo     (superScriptString($n))      == $n;
    ok subScriptStringUndo       (subScriptString($n))        == $n;
   }


=head2 enclosedReversedString($string)

Convert alphanumerics in a string to enclosed reversed alphanumerics.

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok enclosedReversedString(q(hello world 1234)) eq q(🅗🅔🅛🅛🅞 🅦🅞🅡🅛🅓 ➊➋➌➍);   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 enclosedReversedStringUndo($string)

Undo alphanumerics in a string to enclosed reversed alphanumerics.

     Parameter  Description
  1  $string    String to convert

B<Example:>


  if (1)
   {my $n = 1234567890;
    ok boldStringUndo            (boldString($n))             == $n;
    ok enclosedStringUndo        (enclosedString($n))         == $n;

    ok enclosedReversedStringUndo(enclosedReversedString($n)) == $n;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok superScriptStringUndo     (superScriptString($n))      == $n;
    ok subScriptStringUndo       (subScriptString($n))        == $n;
   }


=head2 superScriptString($string)

Convert alphanumerics in a string to super scripts

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok superScriptString(1234567890) eq q(¹²³⁴⁵⁶⁷⁸⁹⁰);                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 superScriptStringUndo($string)

Undo alphanumerics in a string to super scripts

     Parameter  Description
  1  $string    String to convert

B<Example:>


  if (1)
   {my $n = 1234567890;
    ok boldStringUndo            (boldString($n))             == $n;
    ok enclosedStringUndo        (enclosedString($n))         == $n;
    ok enclosedReversedStringUndo(enclosedReversedString($n)) == $n;

    ok superScriptStringUndo     (superScriptString($n))      == $n;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok subScriptStringUndo       (subScriptString($n))        == $n;
   }


=head2 subScriptString($string)

Convert alphanumerics in a string to sub scripts

     Parameter  Description
  1  $string    String to convert

B<Example:>



  ok subScriptString(1234567890)   eq q(₁₂₃₄₅₆₇₈₉₀);                                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 subScriptStringUndo($string)

Undo alphanumerics in a string to sub scripts

     Parameter  Description
  1  $string    String to convert

B<Example:>


  if (1)
   {my $n = 1234567890;
    ok boldStringUndo            (boldString($n))             == $n;
    ok enclosedStringUndo        (enclosedString($n))         == $n;
    ok enclosedReversedStringUndo(enclosedReversedString($n)) == $n;
    ok superScriptStringUndo     (superScriptString($n))      == $n;

    ok subScriptStringUndo       (subScriptString($n))        == $n;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head2 isFileUtf8($file)

Return the file name quoted if its contents are in utf8 else return undef

     Parameter  Description
  1  $file      File to test

B<Example:>


    my $f = writeFile(undef, "aaa");

    ok isFileUtf8 $f;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 Unix domain communications

Send messages between processes via a unix domain socket.

=head2 newUdsrServer(@parms)

Create a communications server - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.

     Parameter  Description
  1  @parms     Attributes per L<Udsr Definition|/Udsr Definition>

B<Example:>


    my $N = 20;

    my $s = newUdsrServer(serverAction=>sub  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     {my ($u) = @_;
      my $r = $u->read;
      $u->write(qq(Hello from server $r));
     });

    my $p = newProcessStarter(min(100, $N));                                      # Run some clients
    for my $i(1..$N)
     {$p->start(sub
       {my $count = 0;
        for my $j(1..$N)
         {my $c = newUdsrClient;
          my $m = qq(Hello from client $i x $j);
          $c->write($m);
          my $r = $c->read;
          ++$count if $r eq qq(Hello from server $m);
         }
        [$count]
       });
     }

    my $count;
    for my $r($p->finish)                                                         # Consolidate results
     {my ($c) = @$r;
      $count += $c;
     }

    ok $count == $N*$N;                                                           # Check results and kill
    $s->kill;


=head2 newUdsrClient(@parms)

Create a new communications client - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.

     Parameter  Description
  1  @parms     Attributes per L<Udsr Definition|/Udsr Definition>

B<Example:>


    my $N = 20;
    my $s = newUdsrServer(serverAction=>sub
     {my ($u) = @_;
      my $r = $u->read;
      $u->write(qq(Hello from server $r));
     });

    my $p = newProcessStarter(min(100, $N));                                      # Run some clients
    for my $i(1..$N)
     {$p->start(sub
       {my $count = 0;
        for my $j(1..$N)

         {my $c = newUdsrClient;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

          my $m = qq(Hello from client $i x $j);
          $c->write($m);
          my $r = $c->read;
          ++$count if $r eq qq(Hello from server $m);
         }
        [$count]
       });
     }

    my $count;
    for my $r($p->finish)                                                         # Consolidate results
     {my ($c) = @$r;
      $count += $c;
     }

    ok $count == $N*$N;                                                           # Check results and kill
    $s->kill;


=head2 Udsr::write($u, $msg)

Write a communications message to the L<newUdsrServer|/newUdsrServer> or the L<newUdsrClient|/newUdsrClient>.

     Parameter  Description
  1  $u         Communicator
  2  $msg       Message

B<Example:>


    my $N = 20;
    my $s = newUdsrServer(serverAction=>sub
     {my ($u) = @_;
      my $r = $u->read;
      $u->write(qq(Hello from server $r));
     });

    my $p = newProcessStarter(min(100, $N));                                      # Run some clients
    for my $i(1..$N)
     {$p->start(sub
       {my $count = 0;
        for my $j(1..$N)
         {my $c = newUdsrClient;
          my $m = qq(Hello from client $i x $j);
          $c->write($m);
          my $r = $c->read;
          ++$count if $r eq qq(Hello from server $m);
         }
        [$count]
       });
     }

    my $count;
    for my $r($p->finish)                                                         # Consolidate results
     {my ($c) = @$r;
      $count += $c;
     }

    ok $count == $N*$N;                                                           # Check results and kill
    $s->kill;


=head2 Udsr::read($u)

Read a message from the L<newUdsrServer|/newUdsrServer> or the L<newUdsrClient|/newUdsrClient>.

     Parameter  Description
  1  $u         Communicator

B<Example:>


    my $N = 20;
    my $s = newUdsrServer(serverAction=>sub
     {my ($u) = @_;
      my $r = $u->read;
      $u->write(qq(Hello from server $r));
     });

    my $p = newProcessStarter(min(100, $N));                                      # Run some clients
    for my $i(1..$N)
     {$p->start(sub
       {my $count = 0;
        for my $j(1..$N)
         {my $c = newUdsrClient;
          my $m = qq(Hello from client $i x $j);
          $c->write($m);
          my $r = $c->read;
          ++$count if $r eq qq(Hello from server $m);
         }
        [$count]
       });
     }

    my $count;
    for my $r($p->finish)                                                         # Consolidate results
     {my ($c) = @$r;
      $count += $c;
     }

    ok $count == $N*$N;                                                           # Check results and kill
    $s->kill;


=head2 Udsr::kill($u)

Kill a communications server.

     Parameter  Description
  1  $u         Communicator

B<Example:>


    my $N = 20;
    my $s = newUdsrServer(serverAction=>sub
     {my ($u) = @_;
      my $r = $u->read;
      $u->write(qq(Hello from server $r));
     });

    my $p = newProcessStarter(min(100, $N));                                      # Run some clients
    for my $i(1..$N)
     {$p->start(sub
       {my $count = 0;
        for my $j(1..$N)
         {my $c = newUdsrClient;
          my $m = qq(Hello from client $i x $j);
          $c->write($m);
          my $r = $c->read;
          ++$count if $r eq qq(Hello from server $m);
         }
        [$count]
       });
     }

    my $count;
    for my $r($p->finish)                                                         # Consolidate results
     {my ($c) = @$r;
      $count += $c;
     }

    ok $count == $N*$N;                                                           # Check results and kill
    $s->kill;


=head2 Udsr::webUser($u, $folder)

Create a systemd installed server that processes http requests using a specified userid. The systemd and CGI files plus an installation script are written to the specified folder after it has been cleared. The L<serverAction> attribute contains the code to be executed by the server: it should contain a L<sub|https://perldoc.perl.org/perlsub.html> B<genResponse($hash)> which will be called with a hash of the CGI variables. This L<sub|https://perldoc.perl.org/perlsub.html> should return the response to be sent back to the client. Returns the installation script file name.

     Parameter  Description
  1  $u         Communicator
  2  $folder    Folder to contain server code

B<Example:>


  if (0)
   {my $fold = fpd(qw(/home phil zzz));                                           # Folder to contain server code
    my $name = q(test);                                                           # Service
    my $user = q(phil);                                                           # User

    my $udsr = newUdsr                                                            # Create a Udsr parameter list
     (serviceName => $name,
      serviceUser => $user,
      socketPath  => qq(/home/phil/$name.socket),
      serverAction=> <<'END'
  my $user = userId;
  my $list = qx(ls -l);
  my $dtts = dateTimeStamp;
  return <<END2;
  Content-type: text/html

  <h1>Hello World to you $user on $dtts!</h1>

  <pre>
  $list
  </pre>
  END2
  END
     );


    Udsr::webUser($udsr, $fold);                                                  # Create and install web service interface  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $ip = awsIp;
    say STDERR qx(curl http://$ip/cgi-bin/$name/client.pl);                       # Enable port 80 on AWS first
   }


=head2 www

Web processing

=head3 wwwGitHubAuth($saveUserDetails, $clientId, $clientSecret, $code, $state)

Logon as a L<GitHub|https://github.com/philiprbrenan> L<Oauth|https://en.wikipedia.org/wiki/OAuth> app per: L<https://github.com/settings/developers>. If no L<Oauth|https://en.wikipedia.org/wiki/OAuth> code is supplied then a web page is printed that allows the user to request that such a code be sent to the server.  If a valid code is received, by the server then it is converted to a L<Oauth|https://en.wikipedia.org/wiki/OAuth> token which is handed to L<sub|https://perldoc.perl.org/perlsub.html> L<saveUserDetails>.

     Parameter         Description
  1  $saveUserDetails  Process user token once obtained from GitHub
  2  $clientId         Client id
  3  $clientSecret     Client secret
  4  $code             Authorization code
  5  $state            Random string

B<Example:>


    wwwHeader;


    wwwGitHubAuth  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     {my ($user, $state, $token, $scope, $type) = @_;
     }
    q(12345678901234567890), q(1234567890123456789012345678901234567890),
    q(12345678901234567890123456789012), q(12345678901234567890);


=head1 Cloud Cover

Useful for operating across the cloud.

=head2 makeDieConfess()

Force die to confess where the death occurred


B<Example:>



    makeDieConfess                                                                  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 ipAddressOfHost($host)

Get the first ip address of the specified host via Domain Name Services

     Parameter  Description
  1  $host      Host name

B<Example:>


    ok saveAwsIp(q(0.0.0.0)) eq awsIp;
    ok saveAwsIp(q(example.org));
    ok saveAwsDomain(q(example.org));
    ok awsR53a   (q(XXXXX), q(www.example.org), q(22.12.232.1));
    ok awsR53aaaa(q(XXXXX), q(www.example.org), q([1232:1232:1232:1232:1232:1232:1232:1232:]));


=head2 awsIp()

Get ip address of server at L<Amazon Web Services|http://aws.amazon.com>.


B<Example:>



    ok saveAwsIp(q(0.0.0.0)) eq awsIp;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok saveAwsIp(q(example.org));
    ok saveAwsDomain(q(example.org));
    ok awsR53a   (q(XXXXX), q(www.example.org), q(22.12.232.1));
    ok awsR53aaaa(q(XXXXX), q(www.example.org), q([1232:1232:1232:1232:1232:1232:1232:1232:]));


=head2 saveAwsIp()

Make the server at L<Amazon Web Services|http://aws.amazon.com> with the given IP address the default primary server as used by all the methods whose names end in B<r> or B<Remote>. Returns the given IP address.


B<Example:>



    ok saveAwsIp(q(0.0.0.0)) eq awsIp;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok saveAwsIp(q(example.org));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok saveAwsDomain(q(example.org));
    ok awsR53a   (q(XXXXX), q(www.example.org), q(22.12.232.1));
    ok awsR53aaaa(q(XXXXX), q(www.example.org), q([1232:1232:1232:1232:1232:1232:1232:1232:]));


=head2 saveAwsDomain()

Make the server at L<Amazon Web Services|http://aws.amazon.com> with the given domain name the default primary server as used by all the methods whose names end in B<r> or B<Remote>. Returns the given IP address.


B<Example:>


    ok saveAwsIp(q(0.0.0.0)) eq awsIp;
    ok saveAwsIp(q(example.org));

    ok saveAwsDomain(q(example.org));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok awsR53a   (q(XXXXX), q(www.example.org), q(22.12.232.1));
    ok awsR53aaaa(q(XXXXX), q(www.example.org), q([1232:1232:1232:1232:1232:1232:1232:1232:]));


=head2 awsMetaData($item)

Get an item of meta data for the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.

     Parameter  Description
  1  $item      Meta data field

B<Example:>



    ok awsMetaData(q(instance-id))    eq q(i-06a4b221b30bf7a37);                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsCurrentIp()

Get the ip address of the AWS server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.


B<Example:>



    awsCurrentIp;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    confirmHasCommandLineCommand(q(find));


    ok awsCurrentIp                   eq q(31.41.59.26);                            # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsCurrentInstanceId()

Get the instance id of the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.


B<Example:>



    ok awsCurrentInstanceId           eq q(i-06a4b221b30bf7a37);                    # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsCurrentAvailabilityZone()

Get the availability zone of the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.


B<Example:>



    ok awsCurrentAvailabilityZone     eq q(us-east-2a);                             # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsCurrentRegion()

Get the region of the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.


B<Example:>



    ok awsCurrentRegion               eq q(us-east-2);                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsCurrentInstanceType()

Get the instance type of the L<Amazon Web Services|http://aws.amazon.com> server if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.


B<Example:>



    ok awsCurrentInstanceType         eq q(r4.4xlarge);                             # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsExecCli($command, %options)

Execute an AWs command and return its response

     Parameter  Description
  1  $command   Command to execute
  2  %options   Aws cli options

B<Example:>



    ok awsExecCli(q(aws s3 ls)) =~ m(ryffine)i;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $p = awsExecCliJson(q(aws ec2 describe-vpcs), region=>q(us-east-1));
    ok $p->Vpcs->[0]->VpcId =~ m(\Avpc-)i;


=head2 awsExecCliJson($command, %options)

Execute an AWs command and decode the json so produced

     Parameter  Description
  1  $command   Command to execute
  2  %options   Aws cli options

B<Example:>


    ok awsExecCli(q(aws s3 ls)) =~ m(ryffine)i;

    my $p = awsExecCliJson(q(aws ec2 describe-vpcs), region=>q(us-east-1));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $p->Vpcs->[0]->VpcId =~ m(\Avpc-)i;


=head2 awsEc2DescribeInstances(%options)

Describe the L<Amazon Web Services|http://aws.amazon.com> instances running in a B<$region>.

     Parameter  Description
  1  %options   Options

B<Example:>


    my %options = (region => q(us-east-2), profile=>q(fmc));

    my $r = awsEc2DescribeInstances              (%options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my %i = awsEc2DescribeInstancesGetIPAddresses(%options);
    is_deeply \%i, { "i-068a7176ba9140057" => { "18.221.162.39" => 1 } };


=head2 awsEc2DescribeInstancesGetIPAddresses(%options)

Return a hash of {instanceId => public ip address} for all running instances on L<Amazon Web Services|http://aws.amazon.com> with ip addresses.

     Parameter  Description
  1  %options   Options

B<Example:>


    my %options = (region => q(us-east-2), profile=>q(fmc));
    my $r = awsEc2DescribeInstances              (%options);

    my %i = awsEc2DescribeInstancesGetIPAddresses(%options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply \%i, { "i-068a7176ba9140057" => { "18.221.162.39" => 1 } };


=head2 awsEc2InstanceIpAddress($instanceId, %options)

Return the IP address of a named instance on L<Amazon Web Services|http://aws.amazon.com> else return B<undef>.

     Parameter    Description
  1  $instanceId  Instance id
  2  %options     Options

B<Example:>



    ok q(3.33.133.233) eq awsEc2InstanceIpAddress  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      ("i-xxx", region => q(us-east-2), profile=>q(fmc));


=head2 awsEc2CreateImage($name, %options)

Create an image snap shot with the specified B<$name> of the AWS server we are currently running on if we are running on an AWS server else return false. It is safe to shut down the instance immediately after initiating the snap shot - the snap continues even though the instance has terminated.

     Parameter  Description
  1  $name      Image name
  2  %options   Options

B<Example:>



       awsEc2CreateImage(q(099 Gold));                                              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsEc2FindImagesWithTagValue($value, %options)

Find images with a tag that matches the specified regular expression B<$value>.

     Parameter  Description
  1  $value     Regular expression
  2  %options   Options

B<Example:>


    is_deeply

     [awsEc2FindImagesWithTagValue(qr(boot)i, region=>'us-east-2',  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      profile=>'fmc')],
     ["ami-011b4273c6123ae76"];


=head2 awsEc2DescribeImages(%options)

Describe images available.

     Parameter  Description
  1  %options   Options

B<Example:>



    awsEc2DescribeImages(region => q(us-east-2), profile=>q(fmc));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsCurrentLinuxSpotPrices(%options)

Return {instance type} = cheapest spot price in dollars per hour for the given region

     Parameter  Description
  1  %options   Options

B<Example:>



     awsCurrentLinuxSpotPrices(region => q(us-east-2), profile=>q(fmc));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsEc2DescribeInstanceType($instanceType, %options)

Return details of the specified instance type.

     Parameter      Description
  1  $instanceType  Instance type name
  2  %options       Options

B<Example:>



    my $i = awsEc2DescribeInstanceType  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ("m4.large", region=>'us-east-2', profile=>'fmc');

    is_deeply $i->{VCpuInfo},
     {DefaultCores          => 1,
      DefaultThreadsPerCore => 2,
      DefaultVCpus          => 2,
      ValidCores            => [1],
      ValidThreadsPerCore   => [1, 2],
      };


=head2 awsEc2ReportSpotInstancePrices($instanceTypeRe, %options)

Report the prices of all the spot instances whose type matches a regular expression B<$instanceTypeRe>. The report is sorted by price in millidollars per cpu ascending.

     Parameter        Description
  1  $instanceTypeRe  Regular expression for instance type name
  2  %options         Options

B<Example:>



    my $a = awsEc2ReportSpotInstancePrices  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (qr(\.metal), region=>'us-east-2', profile=>'fmc');
    ok $a->report eq <<END;
  CPUs by price

  10 instances types found on 2019-12-24 at 22:53:26

  Cheapest Instance Type: m5.metal
  Price Per Cpu hour    : 6.65      in millidollars per hour

     Column         Description
  1  Instance_Type  Instance type name
  2  Price          Price in millidollars per hour
  3  CPUs           Number of Cpus
  4  Price_per_CPU  The price per CPU in millidollars per hour

      Instance_Type  Price  CPUs  Price_per_CPU
   1  m5.metal         638    96           6.65
   2  r5.metal         668    96           6.97
   3  r5d.metal        668    96           6.97
   4  m5d.metal        826    96           8.61
   5  c5d.metal        912    96           9.50
   6  c5.metal        1037    96          10.81
   7  c5n.metal        912    72          12.67
   8  i3.metal        1497    72          20.80
   9  z1d.metal       1339    48          27.90
  10  i3en.metal      3254    96          33.90
  END


=head2 awsEc2RequestSpotInstances($count, $instanceType, $ami, $price, $securityGroup, $key, %options)

Request spot instances as long as they can be started within the next minute. Return a list of spot instance request ids one for each instance requested.

     Parameter       Description
  1  $count          Number of instances
  2  $instanceType   Instance type
  3  $ami            AMI
  4  $price          Price in dollars per hour
  5  $securityGroup  Security group
  6  $key            Key name
  7  %options        Options.

B<Example:>



    my $r = awsEc2RequestSpotInstances  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (2, q(t2.micro), "ami-xxx", 0.01, q(xxx), q(yyy),
      region=>'us-east-2', profile=>'fmc');


=head2 awsEc2DescribeSpotInstances(%options)

Return a hash {spot instance request => spot instance details} describing the status of active spot instances.

     Parameter  Description
  1  %options   Options.

B<Example:>



    my $r = awsEc2DescribeSpotInstances(region => q(us-east-2), profile=>q(fmc));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsR53a($zone, $server, $ip, %options)

Create/Update a B<A> L<Domain Name System|https://en.wikipedia.org/wiki/Domain_Name_System> record for the specified server.

     Parameter  Description
  1  $zone      Zone id from R53
  2  $server    Fully qualified domain name
  3  $ip        Ip address
  4  %options   AWS CLI global options

B<Example:>


    ok saveAwsIp(q(0.0.0.0)) eq awsIp;
    ok saveAwsIp(q(example.org));
    ok saveAwsDomain(q(example.org));

    ok awsR53a   (q(XXXXX), q(www.example.org), q(22.12.232.1));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok awsR53aaaa(q(XXXXX), q(www.example.org), q([1232:1232:1232:1232:1232:1232:1232:1232:]));


=head2 awsR53aaaa($zone, $server, $ip, %options)

Create/Update a B<AAAA> L<Domain Name System|https://en.wikipedia.org/wiki/Domain_Name_System> record for the specified server.

     Parameter  Description
  1  $zone      Zone id from R53
  2  $server    Fully qualified domain name
  3  $ip        Ip6 address
  4  %options   AWS CLI global options

B<Example:>


    ok saveAwsIp(q(0.0.0.0)) eq awsIp;
    ok saveAwsIp(q(example.org));
    ok saveAwsDomain(q(example.org));
    ok awsR53a   (q(XXXXX), q(www.example.org), q(22.12.232.1));

    ok awsR53aaaa(q(XXXXX), q(www.example.org), q([1232:1232:1232:1232:1232:1232:1232:1232:]));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsEc2Tag($resource, $name, $value, %options)

Tag an elastic compute resource with the supplied tags.

     Parameter  Description
  1  $resource  Resource
  2  $name      Tag name
  3  $value     Tag value
  4  %options   Options.

B<Example:>



    awsEc2Tag  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ("i-xxxx", Name=>q(Conversion), region => q(us-east-2), profile=>q(fmc));


=head2 confirmHasCommandLineCommand($cmd)

Check that the specified b<$cmd> is present on the current system.  Use $ENV{PATH} to add folders containing commands as necessary.

     Parameter  Description
  1  $cmd       Command to check for

B<Example:>


    awsCurrentIp;

    confirmHasCommandLineCommand(q(find));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 numberOfCpus($scale)

Number of cpus scaled by an optional factor - but only if you have nproc. If you do not have nproc but do have a convenient way for determining the number of cpus on your system please let me know.

     Parameter  Description
  1  $scale     Scale factor

B<Example:>



  ok numberOfCpus(8) >= 8, 'ddd';                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 ipAddressViaArp($hostName)

Get the ip address of a server on the local network by hostname via arp

     Parameter  Description
  1  $hostName  Host name

B<Example:>



    ipAddressViaArp(q(secarias));                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 parseS3BucketAndFolderName($name)

Parse an L<S3|https://aws.amazon.com/s3/> bucket/folder name into a bucket and a folder name removing any initial s3://.

     Parameter  Description
  1  $name      Bucket/folder name

B<Example:>


  if (1)

   {is_deeply [parseS3BucketAndFolderName(q(s3://bbbb/ffff/dddd/))], [qw(bbbb ffff/dddd/)], q(iii);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseS3BucketAndFolderName(q(s3://bbbb/))],           [qw(bbbb), q()];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseS3BucketAndFolderName(q(     bbbb/))],           [qw(bbbb), q()];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseS3BucketAndFolderName(q(     bbbb))],            [qw(bbbb), q()];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   }


=head2 saveCodeToS3($saveCodeEvery, $folder, $zipFileName, $bucket, $S3Parms)

Save source code every B<$saveCodeEvery> seconds by zipping folder B<$folder> to zip file B<$zipFileName> then saving this zip file in the specified L<S3|https://aws.amazon.com/s3/> B<$bucket> using any additional L<S3|https://aws.amazon.com/s3/> parameters in B<$S3Parms>.

     Parameter       Description
  1  $saveCodeEvery  Save every seconds
  2  $folder         Folder to save
  3  $zipFileName    Zip file name
  4  $bucket         Bucket/key
  5  $S3Parms        Additional S3 parameters like profile or region as a string

B<Example:>



    saveCodeToS3(1200, q(.), q(projectName), q(bucket/folder), q(--quiet));         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 addCertificate($file)

Add a certificate to the current ssh session.

     Parameter  Description
  1  $file      File containing certificate

B<Example:>



    addCertificate(fpf(qw(.ssh cert)));                                             # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 hostName()

The name of the host we are running on.


B<Example:>



    hostName;                                                                       # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 userId($user)

Get or confirm the userid we are currently running under.

     Parameter  Description
  1  $user      Userid to confirm

B<Example:>



    userId;                                                                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsTranslateText($string, $language, $cacheFolder, $Options)

Translate B<$text> from English to a specified B<$language> using AWS Translate with the specified global B<$options> and return the translated string.  Translations are cached in the specified B<$cacheFolder> for reuse where feasible.

     Parameter     Description
  1  $string       String to translate
  2  $language     Language code
  3  $cacheFolder  Cache folder
  4  $Options      Aws global options string

B<Example:>



    ok awsTranslateText("Hello", "it", ".translations/") eq q(Ciao);                # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 AWS parallel

Parallel computing across multiple instances running on L<Amazon Web Services|http://aws.amazon.com>.

=head2 onAws()

Returns 1 if we are on AWS else return 0.


B<Example:>



    ok  onAws;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok !onAwsSecondary;
    ok  onAwsPrimary;


=head2 onAwsPrimary()

Return 1 if we are on L<Amazon Web Services|http://aws.amazon.com> and we are on the primary session instance as defined by L<awsParallelPrimaryInstanceId>, return 0 if we are on a secondary session instance, else return B<undef> if we are not on L<Amazon Web Services|http://aws.amazon.com>.


B<Example:>


    ok  onAws;
    ok !onAwsSecondary;

    ok  onAwsPrimary;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 onAwsSecondary()

Return 1 if we are on L<Amazon Web Services|http://aws.amazon.com> but we are not on the primary session instance as defined by L<awsParallelPrimaryInstanceId>, return 0 if we are on the primary session instance, else return B<undef> if we are not on L<Amazon Web Services|http://aws.amazon.com>.


B<Example:>


    ok  onAws;

    ok !onAwsSecondary;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok  onAwsPrimary;


=head2 awsParallelPrimaryInstanceId(%options)

Return the instance id of the primary instance. The primary instance is the instance at L<Amazon Web Services|http://aws.amazon.com> that we communicate with - it controls all the secondary instances that form part of the parallel session. The primary instance is located by finding the first running instance in instance Id order whose Name tag contains the word I<primary>. If no running instance has been identified as the primary instance, then the first viable instance is made the primary. The ip address of the primary is recorded in F</tmp/awsPrimaryIpAddress.data> so that it can be quickly reused by L<xxxr>, L<copyFolderToRemote>, L<mergeFolderFromRemote> etc. Returns the instanceId of the primary instance or B<undef> if no suitable instance exists.

     Parameter  Description
  1  %options   Options

B<Example:>



    ok "i-xxx" eq awsParallelPrimaryInstanceId  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (region => q(us-east-2), profile=>q(fmc));


=head2 awsParallelSpreadFolder($folder, %options)

On L<Amazon Web Services|http://aws.amazon.com>: copies a specified B<$folder> from the primary instance, see: L<awsParallelPrimaryInstanceId>, in parallel, to all the secondary instances in the session. If running locally: copies the specified folder to all L<Amazon Web Services|http://aws.amazon.com> session instances both primary and secondary.

     Parameter  Description
  1  $folder    Fully qualified folder name
  2  %options   Options

B<Example:>


    my $d = temporaryFolder;
    my ($f1, $f2) = map {fpe($d, $_, q(txt))} 1..2;
    my $files = {$f1 => "1111", $f2 => "2222"};

    writeFiles($files);

    awsParallelSpreadFolder($d);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    clearFolder($d, 3);

    awsParallelGatherFolder($d);
    my $r = readFiles($d);
    is_deeply $files, $r;
    clearFolder($d, 3);


=head2 awsParallelGatherFolder($folder, %options)

On L<Amazon Web Services|http://aws.amazon.com>: merges all the files in the specified B<$folder> on each secondary instance to the corresponding folder on the primary instance in parallel.  If running locally: merges all the files in the specified folder on each L<Amazon Web Services|http://aws.amazon.com> session instance (primary and secondary) to the corresponding folder on the local machine.  The folder merges are done in parallel which makes it impossible to rely on the order of the merges.

     Parameter  Description
  1  $folder    Fully qualified folder name
  2  %options   Options

B<Example:>


    my $d = temporaryFolder;
    my ($f1, $f2) = map {fpe($d, $_, q(txt))} 1..2;
    my $files = {$f1 => "1111", $f2 => "2222"};

    writeFiles($files);
    awsParallelSpreadFolder($d);
    clearFolder($d, 3);


    awsParallelGatherFolder($d);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $r = readFiles($d);
    is_deeply $files, $r;
    clearFolder($d, 3);


=head2 awsParallelPrimaryIpAddress(%options)

Return the IP addresses of any primary instance on L<Amazon Web Services|http://aws.amazon.com>.

     Parameter  Description
  1  %options   Options

B<Example:>



    ok awsParallelPrimaryIpAddress eq      q(3.1.4.4);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [awsParallelSecondaryIpAddresses], [qw(3.1.4.5 3.1.4.6)];

    is_deeply [awsParallelIpAddresses],  [qw(3.1.4.4 3.1.4.5 3.1.4.6)];


=head2 awsParallelSecondaryIpAddresses(%options)

Return a list containing the IP addresses of any secondary instances on L<Amazon Web Services|http://aws.amazon.com>.

     Parameter  Description
  1  %options   Options

B<Example:>


    ok awsParallelPrimaryIpAddress eq      q(3.1.4.4);


    is_deeply [awsParallelSecondaryIpAddresses], [qw(3.1.4.5 3.1.4.6)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [awsParallelIpAddresses],  [qw(3.1.4.4 3.1.4.5 3.1.4.6)];


=head2 awsParallelIpAddresses(%options)

Return the IP addresses of all the L<Amazon Web Services|http://aws.amazon.com> session instances.

     Parameter  Description
  1  %options   Options

B<Example:>


    ok awsParallelPrimaryIpAddress eq      q(3.1.4.4);

    is_deeply [awsParallelSecondaryIpAddresses], [qw(3.1.4.5 3.1.4.6)];


    is_deeply [awsParallelIpAddresses],  [qw(3.1.4.4 3.1.4.5 3.1.4.6)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 getCodeContext($sub)

Recreate the code context for a referenced sub

     Parameter  Description
  1  $sub       Sub reference

B<Example:>



  ok getCodeContext(\&getCodeContext) =~ m(use strict)ims;                          # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 awsParallelProcessFiles($userData, $parallel, $results, $files, %options)

Process files in parallel across multiple L<Amazon Web Services|http://aws.amazon.com> instances if available or in series if not.  The data located by B<$userData> is transferred from the primary instance, as determined by L<awsParallelPrimaryInstanceId>, to all the secondary instances. B<$parallel> contains a reference to a sub, parameterized by array @_ = (a copy of the user data, the name of the file to process), which will be executed upon each session instance including the primary instance to update $userData. B<$results> contains a reference to a sub, parameterized by array @_ = (the user data, an array of results returned by each execution of $parallel), that will be called on the primary instance to process the results folders from each instance once their results folders have been copied back and merged into the results folder of the primary instance. $results should update its copy of $userData with the information received from each instance. B<$files> is a reference to an array of the files to be processed: each file will be copied from the primary instance to each of the secondary instances before parallel processing starts. B<%options> contains any parameters needed to interact with L<EC2|https://aws.amazon.com/ec2/>  via the L<Amazon Web Services Command Line Interface|https://aws.amazon.com/cli/>.  The returned result is that returned by sub $results.

     Parameter  Description
  1  $userData  User data or undef
  2  $parallel  Parallel sub reference
  3  $results   Series sub reference
  4  $files     [files to process]
  5  %options   Aws cli options.

B<Example:>


    my $N = 2001;                                                                 # Number of files to process
    my $options = q(region => q(us-east-2), profile=>q(fmc));                     # Aws cli options
    my %options = eval "($options)";

    for my $dir(q(/home/phil/perl/cpan/DataTableText/lib/Data/Table/),            # Folders we will need on aws
                q(/home/phil/.aws/))
     {awsParallelSpreadFolder($dir, %options);
     }

    my $d = temporaryFolder;                                                      # Create a temporary folder
    my $resultsFile = fpe($d, qw(results data));                                  # Save results in this temporary file

    if (my $r = execPerlOnRemote(join "
",                                       # Execute some code on a server
      getCodeContext(\&awsParallelProcessFilesTestParallel),                      # Get code context of the sub we want to call.
      <<SESSIONLEADER))                                                           # Launch code on session leader
  use Data::Table::Text qw(:all);


  my \$r = awsParallelProcessFiles                                                # Process files on multiple L<Amazon Web Services|http://aws.amazon.com> instances in parallel  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   ({file=>4, time=>timeStamp},                                                   # User data
    \\\&Data::Table::Text::awsParallelProcessFilesTestParallel,                   # Reference to code to execute in parallel on each session instance
    \\\&Data::Table::Text::awsParallelProcessFilesTestResults,                    # Reference to code to execute in series to merge the results of each parallel computation
    [map {writeFile(fpe(q($d), \$_, qw(txt)), \$_)} 1..$N],                       # Files to process
    $options);                                                                    # Aws cli options as we will be running on Aws

  storeFile(q($resultsFile), \$r);                                                # Save results in a file

  SESSIONLEADER

     {copyFileFromRemote($resultsFile);                                           # Retrieve user data

      my $userData = retrieveFile($resultsFile);                                  # Recover user data
      my @i = awsParallelSecondaryIpAddresses(%options);                          # Ip addresses of secondary instances
      my @I = keys $userData->{ip}->%*;
      is_deeply [sort @i], [sort @I];                                             # Each secondary ip address was used

      ok $userData->{file}  == 4;                                                 # Prove we can pass data in and get it back
      ok $userData->{merge} == 1 + @i, 'ii';                                      # Number of merges

      my %f; my %i;                                                               # Files processed on each ip
      for   my $i(sort keys $userData->{ipFile}->%*)                              # Ip
       {for my $f(sort keys $userData->{ipFile}{$i}->%*)                          # File
         {$f{fn($f)}++;                                                           # Files processed
          $i{$i}++;                                                               # Count files on each ip
         }
       }

      is_deeply \%f, {map {$_=>1} 1..$N};                                         # Check each file was processed

      if (1)
       {my @rc; my @ra;                                                           # Range of number of files processed on each ip - computed, actually counted
        my $l = $N/@i-1;                                                          # Lower limit of number of files per IP address
        my $h = $N/@i+1;                                                          # Upper limit of number of files per IP address
        for   my $i(keys %i)
         {my $nc = $i{$i};                                                        # Number of files processed on this ip - computed
          my $na = $userData->{ip}{$i};                                           # Number of files processed on this ip - actually counted
          push @rc, ($nc >= $l and $nc <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
          push @ra, ($na >= $l and $na <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
         }
        ok @i == grep {$_} @ra;                                                   # Check each ip processed the expected number of files
        ok @i == grep {$_} @rc;
       }

      ok $userData->{files}{&fpe($d, qw(4 txt))} eq                               # Check the computed MD5 sum for the specified file
         q(a87ff679a2f3e71d9181a67b7542122c);
     }

  if (0)                                                                           # Process files in series on local machine
   {my $N = 42;
    my $d = temporaryFolder;


    my $r = awsParallelProcessFiles                                               # Process files in series on local machine  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     ({file => 4},                                                                # User data
      \&Data::Table::Text::awsParallelProcessFilesTestParallel,                   # Code to execute on each session instance including the session leader written as a string because it has to be shipped to each instance
      \&Data::Table::Text::awsParallelProcessFilesTestResults,                    # Code to execute in series on the session leader to analyze the results of the parallel runs
      [map {writeFile(fpe($d, $_, qw(txt)), $_)} 1..$N],                          # Files to process
      ());                                                                        # No Aws cli options as we are running locally

    ok $r->{file}            ==  4, 'aaa';                                        # Prove we can pass data in and get it back
    ok $r->{merge}           ==  1, 'bbb';                                        # Only one merge as we are running locally

    ok $r->{ip}{localHost}   == $N, 'ccc';                                        # Number of files processed locally
    ok keys($r->{files}->%*) == $N;                                               # Number of files processed
    ok $r->{files}{fpe($d, qw(4 txt))} eq q(a87ff679a2f3e71d9181a67b7542122c);    # Check the computed MD5 sum for the specified file

    clearFolder($d, $N+2);
   }


=head1 S3

Work with S3 as if it were a file system.

=head2 s3ListFilesAndSizes($folderOrFile, %options)

Return {file=>size} for all the files in a specified B<$folderOrFile> on S3 using the specified B<%options> if any.

     Parameter      Description
  1  $folderOrFile  Source on S3 - which will be truncated to a folder name
  2  %options       Options

B<Example:>


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply

     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);
      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3FileExists($file, %options)

Return (name, size, date, time) for a B<$file> that exists on S3 else () using the specified B<%options> if any.

     Parameter  Description
  1  $file      File on S3 - which will be truncated to a folder name
  2  %options   Options

B<Example:>


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);
      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)

     {my @r = s3FileExists($file, %options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3WriteFile($fileS3, $fileLocal, %options)

Write to a file B<$fileS3> on S3 the contents of a local file B<$fileLocal> using the specified B<%options> if any.  $fileLocal will be removed if %options contains a key cleanUp with a true value.

     Parameter   Description
  1  $fileS3     File to write to on S3
  2  $fileLocal  String to write into file
  3  %options    Options

B<Example:>


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);
      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);


      s3WriteFile($file, $f, %options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3WriteString($file, $string, %options)

Write to a B<$file> on S3 the contents of B<$string> using the specified B<%options> if any.

     Parameter  Description
  1  $file      File to write to on S3
  2  $string    String to write into file
  3  %options   Options

B<Example:>


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)

     {       s3WriteString($file, $data, %options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3ReadFile($file, $local, %options)

Read from a B<$file> on S3 and write the contents to a local file B<$local> using the specified B<%options> if any.  Any pre existing version of the local file $local will be deleted.  Returns whether the local file exists after completion of the download.

     Parameter  Description
  1  $file      File to read from on S3
  2  $local     Local file to write to
  3  %options   Options

B<Example:>


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);
      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;

      s3ReadFile ($file, $f, %options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3ReadString($file, %options)

Read from a B<$file> on S3 and return the contents as a string using specified B<%options> if any.  Any pre existing version of $local will be deleted.  Returns whether the local file exists after completion of the download.

     Parameter  Description
  1  $file      File to read from on S3
  2  %options   Options

B<Example:>


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);

      my $r = s3ReadString($file,        %options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3DownloadFolder($folder, $local, %options)

Download a specified B<$folder> on S3 to a B<$local> folder using the specified B<%options> if any.  Any existing data in the $local folder will be will be deleted if delete=>1 is specified as an option. Returns B<undef on failure> else the name of the B<$local> on success.

     Parameter  Description
  1  $folder    Folder to read from on S3
  2  $local     Local folder to write to
  3  %options   Options

B<Example:>


    my %options = (profile => q(fmc));


    s3DownloadFolder  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);
      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3ZipFolder($source, $target, %options)

Zip the specified B<$source> folder and write it to the named B<$target> file on S3.

     Parameter  Description
  1  $source    Source folder
  2  $target    Target file on S3
  3  %options   S3 options

B<Example:>



    s3ZipFolder(q(home/phil/r/), q(s3://bucket/r.zip));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);


    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);
      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head2 s3ZipFolders($map, %options)

Zip local folders and upload them to S3 in parallel.  B<$map> maps source folder names on the local machine to target folders on S3. B<%options> contains any additional L<Amazon Web Services|http://aws.amazon.com> cli options.

     Parameter  Description
  1  $map       Source folder to S3 mapping
  2  %options   S3 options

B<Example:>


    my %options = (profile => q(fmc));

    s3DownloadFolder
     (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

    s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);


    s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply
     {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
     },
     {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
       ["originals4/images/business_plan_sections.png",
        112525,
        "2019-08-13",
        "20:01:10",
       ],
      "s3://salesforce.dita/originals4/images/non-referenced.png" =>
       ["originals4/images/non-referenced.png",
        19076,
        "2019-08-20",
        "01:25:04",
       ],
     };

    my $data = q(0123456789);
    my $file = q(s3://salesforce.dita/zzz/111.txt);

    if (1)
     {       s3WriteString($file, $data, %options);
      my $r = s3ReadString($file,        %options);
      ok $r eq $data;
     }

    if (1)
     {my @r = s3FileExists($file, %options);
      ok $r[0] eq "zzz/111.txt";
      ok $r[1] ==  10;
     }

    if (1)
     {my $d = $data x 2;
      my $f = writeFile(undef, $d);

      s3WriteFile($file, $f, %options);
      unlink $f;
      s3ReadFile ($file, $f, %options);
      ok readFile($f) eq $d;
      unlink $f;
     }


=head1 GitHub

Simple interactions with L<GitHub|https://github.com/philiprbrenan> - for more complex interactions please use L<GitHub::Crud>.

=head2 downloadGitHubPublicRepo($user, $repo)

Get the contents of a public repo on GitHub and place them in a temporary folder whose name is returned to the caller or confess if no such repo exists.

     Parameter  Description
  1  $user      GitHub user
  2  $repo      GitHub repo

B<Example:>



       downloadGitHubPublicRepo(q(philiprbrenan), q(psr));                          # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 downloadGitHubPublicRepoFile($user, $repo, $file)

Get the contents of a B<$user> B<$repo> B<$file> from  a public repo on GitHub and return them as a string.

     Parameter  Description
  1  $user      GitHub user
  2  $repo      GitHub repository
  3  $file      File name in repository

B<Example:>



    ok &downloadGitHubPublicRepoFile(qw(philiprbrenan pleaseChangeDita index.html));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head1 Processes

Start processes, wait for them to terminate and retrieve their results

=head2 startProcess($sub, $pids, $maximum)

Start new processes while the number of child processes recorded in B<%$pids> is less than the specified B<$maximum>.  Use L<waitForAllStartedProcessesToFinish|/waitForAllStartedProcessesToFinish> to wait for all these processes to finish.

     Parameter  Description
  1  $sub       Sub to start
  2  $pids      Hash in which to record the process ids
  3  $maximum   Maximum number of processes to run at a time

B<Example:>


    my %pids;

    sub{startProcess {} %pids, 1; ok 1 >= keys %pids}->() for 1..8;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    waitForAllStartedProcessesToFinish(%pids);
    ok !keys(%pids)


=head2 waitForAllStartedProcessesToFinish($pids)

Wait until all the processes started by L<startProcess|/startProcess> have finished.

     Parameter  Description
  1  $pids      Hash of started process ids

B<Example:>


    my %pids;
    sub{startProcess {} %pids, 1; ok 1 >= keys %pids}->() for 1..8;

    waitForAllStartedProcessesToFinish(%pids);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok !keys(%pids)


=head2 newProcessStarter($maximumNumberOfProcesses, %options)

Create a new L<process starter|/Data::Table::Text::Starter Definition> with which to start parallel processes up to a specified B<$maximumNumberOfProcesses> maximum number of parallel processes at a time, wait for all the started processes to finish and then optionally retrieve their saved results as an array from the folder named by B<$transferArea>.

     Parameter                  Description
  1  $maximumNumberOfProcesses  Maximum number of processes to start
  2  %options                   Options

B<Example:>


  if (1)
   {my $N = 100;
    my $l = q(logFile.txt);
    unlink $l;

    my $s = newProcessStarter(4);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       $s->processingTitle   = q(Test processes);
       $s->totalToBeStarted  = $N;
       $s->processingLogFile = $l;

    for my $i(1..$N)
     {Data::Table::Text::Starter::start($s, sub{$i*$i});
     }

    is_deeply
     [sort {$a <=> $b} Data::Table::Text::Starter::finish($s)],
     [map {$_**2} 1..$N];

    ok readFile($l) =~ m(Finished $N processes for: Test processes)s;
    clearFolder($s->transferArea, 1e3);
    unlink $l;
   }


=head2 Data::Table::Text::Starter::start($starter, $sub)

Start a new process to run the specified B<$sub>.

     Parameter  Description
  1  $starter   Starter
  2  $sub       Sub to be run.

B<Example:>


  if (1)
   {my $N = 100;
    my $l = q(logFile.txt);
    unlink $l;
    my $s = newProcessStarter(4);
       $s->processingTitle   = q(Test processes);
       $s->totalToBeStarted  = $N;
       $s->processingLogFile = $l;

    for my $i(1..$N)

     {Data::Table::Text::Starter::start($s, sub{$i*$i});  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     }

    is_deeply
     [sort {$a <=> $b} Data::Table::Text::Starter::finish($s)],
     [map {$_**2} 1..$N];

    ok readFile($l) =~ m(Finished $N processes for: Test processes)s;
    clearFolder($s->transferArea, 1e3);
    unlink $l;
   }


=head2 Data::Table::Text::Starter::finish($starter)

Wait for all started processes to finish and return their results as an array.

     Parameter  Description
  1  $starter   Starter

B<Example:>


  if (1)
   {my $N = 100;
    my $l = q(logFile.txt);
    unlink $l;
    my $s = newProcessStarter(4);
       $s->processingTitle   = q(Test processes);
       $s->totalToBeStarted  = $N;
       $s->processingLogFile = $l;

    for my $i(1..$N)
     {Data::Table::Text::Starter::start($s, sub{$i*$i});
     }

    is_deeply

     [sort {$a <=> $b} Data::Table::Text::Starter::finish($s)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     [map {$_**2} 1..$N];

    ok readFile($l) =~ m(Finished $N processes for: Test processes)s;
    clearFolder($s->transferArea, 1e3);
    unlink $l;
   }


=head2 squareArray(@array)

Create a two dimensional square array from a one dimensional linear array.

     Parameter  Description
  1  @array     Array

B<Example:>



    is_deeply [squareArray @{[1..4]} ], [[1, 2], [3, 4]];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [squareArray @{[1..22]}],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

     [[1 .. 5], [6 .. 10], [11 .. 15], [16 .. 20], [21, 22]];


    is_deeply [1..$_], [deSquareArray squareArray @{[1..$_]}] for 1..22;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok $_ == countSquareArray         squareArray @{[1..$_]}  for 222;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [rectangularArray(3, 1..11)],
              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9]];

    is_deeply [rectangularArray(3, 1..12)],
              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];

    is_deeply [rectangularArray(3, 1..13)],
              [[1, 4, 7, 10, 13],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];

    is_deeply [rectangularArray2(3, 1..5)],
              [[1, 2, 3],
               [4, 5]];

    is_deeply [rectangularArray2(3, 1..6)],
              [[1, 2, 3],
               [4, 5, 6]];

    is_deeply [rectangularArray2(3, 1..7)],
              [[1, 2, 3],
               [4, 5, 6],
               [7]];


=head2 deSquareArray(@square)

Create a one dimensional array from a two dimensional array of arrays

     Parameter  Description
  1  @square    Array of arrays

B<Example:>


    is_deeply [squareArray @{[1..4]} ], [[1, 2], [3, 4]];
    is_deeply [squareArray @{[1..22]}],
     [[1 .. 5], [6 .. 10], [11 .. 15], [16 .. 20], [21, 22]];


    is_deeply [1..$_], [deSquareArray squareArray @{[1..$_]}] for 1..22;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $_ == countSquareArray         squareArray @{[1..$_]}  for 222;

    is_deeply [rectangularArray(3, 1..11)],
              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9]];

    is_deeply [rectangularArray(3, 1..12)],
              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];

    is_deeply [rectangularArray(3, 1..13)],
              [[1, 4, 7, 10, 13],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];

    is_deeply [rectangularArray2(3, 1..5)],
              [[1, 2, 3],
               [4, 5]];

    is_deeply [rectangularArray2(3, 1..6)],
              [[1, 2, 3],
               [4, 5, 6]];

    is_deeply [rectangularArray2(3, 1..7)],
              [[1, 2, 3],
               [4, 5, 6],
               [7]];


=head2 rectangularArray($first, @array)

Create a two dimensional rectangular array whose first dimension is B<$first> from a one dimensional linear array.

     Parameter  Description
  1  $first     First dimension size
  2  @array     Array

B<Example:>


    is_deeply [squareArray @{[1..4]} ], [[1, 2], [3, 4]];
    is_deeply [squareArray @{[1..22]}],
     [[1 .. 5], [6 .. 10], [11 .. 15], [16 .. 20], [21, 22]];

    is_deeply [1..$_], [deSquareArray squareArray @{[1..$_]}] for 1..22;
    ok $_ == countSquareArray         squareArray @{[1..$_]}  for 222;


    is_deeply [rectangularArray(3, 1..11)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9]];


    is_deeply [rectangularArray(3, 1..12)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];


    is_deeply [rectangularArray(3, 1..13)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              [[1, 4, 7, 10, 13],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];

    is_deeply [rectangularArray2(3, 1..5)],
              [[1, 2, 3],
               [4, 5]];

    is_deeply [rectangularArray2(3, 1..6)],
              [[1, 2, 3],
               [4, 5, 6]];

    is_deeply [rectangularArray2(3, 1..7)],
              [[1, 2, 3],
               [4, 5, 6],
               [7]];


=head2 rectangularArray2($second, @array)

Create a two dimensional rectangular array whose second dimension is B<$second> from a one dimensional linear array.

     Parameter  Description
  1  $second    Second dimension size
  2  @array     Array

B<Example:>


    is_deeply [squareArray @{[1..4]} ], [[1, 2], [3, 4]];
    is_deeply [squareArray @{[1..22]}],
     [[1 .. 5], [6 .. 10], [11 .. 15], [16 .. 20], [21, 22]];

    is_deeply [1..$_], [deSquareArray squareArray @{[1..$_]}] for 1..22;
    ok $_ == countSquareArray         squareArray @{[1..$_]}  for 222;

    is_deeply [rectangularArray(3, 1..11)],
              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9]];

    is_deeply [rectangularArray(3, 1..12)],
              [[1, 4, 7, 10],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];

    is_deeply [rectangularArray(3, 1..13)],
              [[1, 4, 7, 10, 13],
               [2, 5, 8, 11],
               [3, 6, 9, 12]];


    is_deeply [rectangularArray2(3, 1..5)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              [[1, 2, 3],
               [4, 5]];


    is_deeply [rectangularArray2(3, 1..6)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              [[1, 2, 3],
               [4, 5, 6]];


    is_deeply [rectangularArray2(3, 1..7)],  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

              [[1, 2, 3],
               [4, 5, 6],
               [7]];


=head2 callSubInParallel($sub)

Call a sub reference in parallel to avoid memory fragmentation and return its results.

     Parameter  Description
  1  $sub       Sub reference

B<Example:>


    my %a = (a=>1, b=>2);

    my %b = callSubInParallel {return %a};  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply \%a, \%b;

    my $f = temporaryFile;
    ok -e $f;

    my $a = callSubInOverlappedParallel
      sub {$a{a}++; owf($f, "Hello World")},
      sub {q(aaaa)};

    ok $a           =~ m(aaaa)i;
    ok $a{a}        == 1;
    ok readFile($f) =~ m(Hello World)i;


=head2 callSubInOverlappedParallel($child, $parent)

Call the B<$child> sub reference in parallel in a separate child process and ignore its results while calling the B<$parent> sub reference in the parent process and returning its results.

     Parameter  Description
  1  $child     Sub reference to call in child process
  2  $parent    Sub reference to call in parent process

B<Example:>


    my %a = (a=>1, b=>2);
    my %b = callSubInParallel {return %a};
    is_deeply \%a, \%b;

    my $f = temporaryFile;
    ok -e $f;


    my $a = callSubInOverlappedParallel  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      sub {$a{a}++; owf($f, "Hello World")},
      sub {q(aaaa)};

    ok $a           =~ m(aaaa)i;
    ok $a{a}        == 1;
    ok readFile($f) =~ m(Hello World)i;


=head2 runInParallel($maximumNumberOfProcesses, $parallel, $results, @array)

Process the elements of an array in parallel using a maximum of B<$maximumNumberOfProcesses> processes. sub B<&$parallel> is forked to process each array element in parallel. The results returned by the forked copies of &$parallel are presented as a single array to sub B<&$results> which is run in series. B<@array> contains the elements to be processed. Returns the result returned by &$results.

     Parameter                  Description
  1  $maximumNumberOfProcesses  Maximum number of processes
  2  $parallel                  Parallel sub
  3  $results                   Results sub
  4  @array                     Array of items to process

B<Example:>


    my @N = 1..100;
    my $N = 100;
    my $R = 0; $R += $_*$_ for 1..$N;

    ok 338350 == $R;

    ok $R == runInSquareRootParallel
       (4,
        sub {my ($p) = @_; $p * $p},
        sub {my $p = 0; $p += $_ for @_; $p},
        @{[1..$N]}
       );


    ok $R == runInParallel  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       (4,
        sub {my ($p) = @_; $p * $p},
        sub {my $p = 0; $p += $_ for @_; $p},
        @{[1..$N]}
       );


=head2 runInSquareRootParallel($maximumNumberOfProcesses, $parallel, $results, @array)

Process the elements of an array in square root parallel using a maximum of B<$maximumNumberOfProcesses> processes. sub B<&$parallel> is forked to process each block of array elements in parallel. The results returned by the forked copies of &$parallel are presented as a single array to sub B<&$results> which is run in series. B<@array> contains the elements to be processed. Returns the result returned by &$results..

     Parameter                  Description
  1  $maximumNumberOfProcesses  Maximum number of processes
  2  $parallel                  Parallel sub
  3  $results                   Results sub
  4  @array                     Array of items to process

B<Example:>


    my @N = 1..100;
    my $N = 100;
    my $R = 0; $R += $_*$_ for 1..$N;

    ok 338350 == $R;


    ok $R == runInSquareRootParallel  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       (4,
        sub {my ($p) = @_; $p * $p},
        sub {my $p = 0; $p += $_ for @_; $p},
        @{[1..$N]}
       );

    ok $R == runInParallel
       (4,
        sub {my ($p) = @_; $p * $p},
        sub {my $p = 0; $p += $_ for @_; $p},
        @{[1..$N]}
       );


=head2 packBySize($N, @sizes)

Given B<$N> buckets and a list B<@sizes> of ([size of file, name of file]...) pack the file names into buckets so that each bucket contains approximately the same number of bytes.  In general this is an NP problem.  Packing largest first into emptiest bucket produces an N**2 heuristic if the buckets are scanned linearly, or N*log(N) if a binary tree is used.  This solution is a compromise at N**3/2 which has the benefits of simple code yet good performance.  Returns ([file names ...]).

     Parameter  Description
  1  $N         Number of buckets
  2  @sizes     Sizes

B<Example:>


    my $M = 7;
    my $N = 15;

    my @b = packBySize($M, map {[$_, $_]} 1..$N);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my @B; my $B = 0;
    for my $b(@b)
     {my $n = 0;
      for(@$b)
       {$n += $_;
        $B += $_;
       }
      push @B, $n;
     }
    ok $B == $N * ($N + 1) / 2;
    is_deeply [@B], [16, 20, 16, 18, 16, 18, 16];


=head2 processSizesInParallel($parallel, $results, @sizes)

Process items of known size in parallel using (8 * the number of CPUs) processes with the process each item is assigned to depending on the size of the item so that each process is loaded with approximately the same number of bytes of data in total from the items it processes.

Each item is processed by sub B<$parallel> and the results of processing all items is processed by B<$results> where the items are taken from B<@sizes>. Each &$parallel() receives an item from @files. &$results() receives an array of all the results returned by &$parallel().

     Parameter  Description
  1  $parallel  Parallel sub
  2  $results   Results sub
  3  @sizes     Array of [size; item] to process by size

B<Example:>


    my $d = temporaryFolder;
    my @f = map {owf(fpe($d, $_, q(txt)), 'X' x ($_ ** 2 % 11))} 1..9;

    my $f = fileLargestSize(@f);
    ok fn($f) eq '3', 'aaa';

  #  my $b = folderSize($d);                                                       # Needs du
  #  ok $b > 0, 'bbb';

    my $c = processFilesInParallel(
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12);

    ok 108 == $c, 'cc11';


    my $C = processSizesInParallel  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, map {[fileSize($_), $_]} (@f) x 12;

    ok 108 == $C, 'cc2';

    my $J = processJavaFilesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12;

    ok 108 == $J, 'cc3';

    clearFolder($d, 12);


=head2 processFilesInParallel($parallel, $results, @files)

Process files in parallel using (8 * the number of CPUs) processes with the process each file is assigned to depending on the size of the file so that each process is loaded with approximately the same number of bytes of data in total from the files it processes.

Each file is processed by sub B<$parallel> and the results of processing all files is processed by B<$results> where the files are taken from B<@files>. Each B<&$parallel> receives a file from B<@files>. B<&$results> receives an array of all the results returned by B<&$parallel>.

     Parameter  Description
  1  $parallel  Parallel sub
  2  $results   Results sub
  3  @files     Array of files to process by size

B<Example:>


    my $d = temporaryFolder;
    my @f = map {owf(fpe($d, $_, q(txt)), 'X' x ($_ ** 2 % 11))} 1..9;

    my $f = fileLargestSize(@f);
    ok fn($f) eq '3', 'aaa';

  #  my $b = folderSize($d);                                                       # Needs du
  #  ok $b > 0, 'bbb';


    my $c = processFilesInParallel(  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12);

    ok 108 == $c, 'cc11';

    my $C = processSizesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, map {[fileSize($_), $_]} (@f) x 12;

    ok 108 == $C, 'cc2';

    my $J = processJavaFilesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12;

    ok 108 == $J, 'cc3';

    clearFolder($d, 12);


=head2 processJavaFilesInParallel($parallel, $results, @files)

Process java files of known size in parallel using (the number of CPUs) processes with the process each item is assigned to depending on the size of the java item so that each process is loaded with approximately the same number of bytes of data in total from the java files it processes.

Each java item is processed by sub B<$parallel> and the results of processing all java files is processed by B<$results> where the java files are taken from B<@sizes>. Each &$parallel() receives a java item from @files. &$results() receives an array of all the results returned by &$parallel().

     Parameter  Description
  1  $parallel  Parallel sub
  2  $results   Results sub
  3  @files     Array of [size; java item] to process by size

B<Example:>


    my $d = temporaryFolder;
    my @f = map {owf(fpe($d, $_, q(txt)), 'X' x ($_ ** 2 % 11))} 1..9;

    my $f = fileLargestSize(@f);
    ok fn($f) eq '3', 'aaa';

  #  my $b = folderSize($d);                                                       # Needs du
  #  ok $b > 0, 'bbb';

    my $c = processFilesInParallel(
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12);

    ok 108 == $c, 'cc11';

    my $C = processSizesInParallel
      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, map {[fileSize($_), $_]} (@f) x 12;

    ok 108 == $C, 'cc2';


    my $J = processJavaFilesInParallel  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      sub
       {my ($file) = @_;
        [&fileSize($file), $file]
       },
      sub
       {scalar @_;
       }, (@f) x 12;

    ok 108 == $J, 'cc3';

    clearFolder($d, 12);


=head2 syncFromS3InParallel($maxSize, $source, $target, $Profile, $options)

Download from L<S3|https://aws.amazon.com/s3/> by using "aws s3 sync --exclude '*' --include '...'" in parallel to sync collections of two or more files no greater then B<$maxSize> or single files greater than $maxSize from the B<$source> folder on L<S3|https://aws.amazon.com/s3/> to the local folder B<$target> using the specified B<$Profile> and B<$options> - then execute the entire command again without the --exclude and --include options in series which might now run faster due to the prior downloads.

     Parameter  Description
  1  $maxSize   The maximum collection size
  2  $source    The source folder on S3
  3  $target    The target folder locally
  4  $Profile   Aws cli profile
  5  $options   Aws cli options

B<Example:>


  if (0)

   {syncFromS3InParallel 1e5,  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(xxx/originals3/),
      q(/home/phil/xxx/),
      q(phil), q(--quiet);

    syncToS3InParallel 1e5,
      q(/home/phil/xxx/),
      q(xxx/originals3/),
      q(phil), q(--quiet);
   }


=head2 syncToS3InParallel($maxSize, $source, $target, $Profile, $options)

Upload to L<S3|https://aws.amazon.com/s3/> by using "aws s3 sync --exclude '*' --include '...'" in parallel to sync collections of two or more files no greater then B<$maxSize> or single files greater than $maxSize from the B<$source> folder locally to the target folder B<$target> on L<S3|https://aws.amazon.com/s3/> using the specified B<$Profile> and B<$options> - then execute the entire command again without the --exclude and --include options in series which might now run faster due to the prior uploads.

     Parameter  Description
  1  $maxSize   The maximum collection size
  2  $source    The target folder locally
  3  $target    The source folder on S3
  4  $Profile   Aws cli profile
  5  $options   Aws cli options

B<Example:>


  if (0)
   {syncFromS3InParallel 1e5,
      q(xxx/originals3/),
      q(/home/phil/xxx/),
      q(phil), q(--quiet);


    syncToS3InParallel 1e5,  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(/home/phil/xxx/),
      q(xxx/originals3/),
      q(phil), q(--quiet);
   }


=head2 childPids($p)

Recursively find the pids of all the sub processes of a B<$process> and all their sub processes and so on returning the specified pid and all its child pids as a list.

     Parameter  Description
  1  $p         Process

B<Example:>



    is_deeply [childPids(2702)], [2702..2705];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 newServiceIncarnation($service, $file)

Create a new service incarnation to record the start up of a new instance of a service and return the description as a L<Data::Exchange::Service Definition hash|/Data::Exchange::Service Definition>.

     Parameter  Description
  1  $service   Service name
  2  $file      Optional details file

B<Example:>


  if (1)

   {my $s = newServiceIncarnation("aaa", q(bbb.txt));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $s->check, $s;

    my $t = newServiceIncarnation("aaa", q(bbb.txt));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $t->check, $t;
    ok $t->start >= $s->start+1;
    ok !$s->check(1);
    unlink q(bbb.txt);
   }


=head2 Data::Exchange::Service::check($service, $continue)

Check that we are the current incarnation of the named service with details obtained from L<newServiceIncarnation|/newServiceIncarnation>. If the optional B<$continue> flag has been set then return the service details if this is the current service incarnation else B<undef>. Otherwise if the B<$continue> flag is false confess unless this is the current service incarnation thus bringing the earlier version of this service to an abrupt end.

     Parameter  Description
  1  $service   Current service details
  2  $continue  Return result if B<$continue> is true else confess if the service has been replaced

B<Example:>


  if (1)
   {my $s = newServiceIncarnation("aaa", q(bbb.txt));
    is_deeply $s->check, $s;
    my $t = newServiceIncarnation("aaa", q(bbb.txt));
    is_deeply $t->check, $t;
    ok $t->start >= $s->start+1;
    ok !$s->check(1);
    unlink q(bbb.txt);
   }


=head1 Conversions

Perform various conversions from STDIN to STDOUT

=head2 convertPerlToJavaScript($in, $out)

Convert Perl to Javascript

     Parameter  Description
  1  $in        Input file name or STDIN if undef
  2  $out       Output file name or STDOUT if undefined

B<Example:>


  if (1)
   {my $i = writeTempFile(<<'END');
  sub test($$)                                                                    #P A test method
   {my ($file, $data) = @_;                                                       # Parameter 1, parameter 2
    if (fullyQualifiedFile($file)) {return qq($data)}                             # File is already fully qualified
   } # test


=head1 Documentation

Extract, format and update documentation for a perl module.

=head2 parseDitaRef($ref, $File, $TopicId)

Parse a dita reference B<$ref> into its components (file name, topic id, id) . Optionally supply a base file name B<$File>> to make the the file component absolute and/or a a default the topic id B<$TopicId> to use if the topic id is not present in the reference.

     Parameter  Description
  1  $ref       Reference to parse
  2  $File      Default absolute file
  3  $TopicId   Default topic id

B<Example:>



    is_deeply [parseDitaRef(q(a#b/c))], [qw(a b c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(a#./c))], [q(a), q(), q(c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(a#/c))],  [q(a), q(), q(c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(a#c))],   [q(a), q(), q(c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(#b/c))],  [q(),  qw(b c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(#b))],    [q(),  q(), q(b)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(#./c))],  [q(),  q(), q(c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(#/c))],   [q(),  q(), q(c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply [parseDitaRef(q(#c))],    [q(),  q(), q(c)];  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 parseXmlDocType($string)

Parse an L<Xml|https://en.wikipedia.org/wiki/XML> DOCTYPE and return a hash indicating its components

     Parameter  Description
  1  $string    String containing a DOCTYPE

B<Example:>


  if (1)

   {is_deeply parseXmlDocType(<<END),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  <!DOCTYPE reference PUBLIC "-//OASIS//DTD DITA Reference//EN" "reference.dtd">
  ...
  END
     {localDtd => "reference.dtd",
      public   => 1,
      publicId => "-//OASIS//DTD DITA Reference//EN",
      root     => "reference",
     };


    is_deeply parseXmlDocType(<<END),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  ...
  <!DOCTYPE concept PUBLIC "-//OASIS//DTD DITA Task//EN" "concept.dtd" []>
  ...
  )),
  END
       {localDtd => "concept.dtd",
        public   => 1,
        publicId => "-//OASIS//DTD DITA Task//EN",
        root     => "concept",
       };
   }


=head2 reportSettings($sourceFile, $reportFile)

Report the current values of parameterless subs.

     Parameter    Description
  1  $sourceFile  Source file
  2  $reportFile  Optional report file

B<Example:>



  reportSettings($0);                                                               # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 reportAttributes($sourceFile)

Report the attributes present in a B<$sourceFile>

     Parameter    Description
  1  $sourceFile  Source file

B<Example:>


    my $d = temporaryFile;

    my $f = writeFile(undef, <<'END'.<<END2);
  #!perl -I/home/phil/perl/cpan/DataTableText/lib/
  use Data::Table::Text qw(reportAttributeSettings);
  sub attribute {1}                                                               # An attribute
  sub replaceable($)                                                              #r A replaceable method
   {


=head2 reportAttributeSettings($reportFile)

Report the current values of the attribute methods in the calling file and optionally write the report to B<$reportFile>. Return the text of the report.

     Parameter    Description
  1  $reportFile  Optional report file

B<Example:>


    my $d = temporaryFile;

    my $f = writeFile(undef, <<'END'.<<END2);
  #!perl -I/home/phil/perl/cpan/DataTableText/lib/

  use Data::Table::Text qw(reportAttributeSettings);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  sub attribute {1}                                                               # An attribute
  sub replaceable($)                                                              #r A replaceable method
   {


=head2 reportReplacableMethods($sourceFile)

Report the replaceable methods marked with #r in a B<$sourceFile>

     Parameter    Description
  1  $sourceFile  Source file

B<Example:>


    my $d = temporaryFile;

    my $f = writeFile(undef, <<'END'.<<END2);
  #!perl -I/home/phil/perl/cpan/DataTableText/lib/
  use Data::Table::Text qw(reportAttributeSettings);
  sub attribute {1}                                                               # An attribute
  sub replaceable($)                                                              #r A replaceable method
   {


  sub reportReplacableMethods($)                                                   # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

   {my ($sourceFile) = @_;                                                        # Source file
    my $s = readFile($sourceFile);
    my %s;
    for my $l(split /
/, $s)                                                     # Find the attribute subs
     {if ($l =~ m(\Asub\s*(\w+).*?#\w*r\w*\s+(.*)\Z))
       {$s{$1} = $2;
       }
     }
    \%s
   }


=head2 reportExportableMethods($sourceFile)

Report the exportable methods marked with #e in a B<$sourceFile>

     Parameter    Description
  1  $sourceFile  Source file

B<Example:>


    my $d = temporaryFile;

    my $f = writeFile(undef, <<'END'.<<END2);
  #!perl -I/home/phil/perl/cpan/DataTableText/lib/
  use Data::Table::Text qw(reportAttributeSettings);
  sub attribute {1}                                                               # An attribute
  sub replaceable($)                                                              #r A replaceable method
   {


=head2 htmlToc($replace, $html)

Generate a table of contents for some html.

     Parameter  Description
  1  $replace   Sub-string within the html to be replaced with the toc
  2  $html      String of html

B<Example:>



  ok nws(htmlToc("XXXX", <<END)), 'htmlToc'                                         # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  <h1 id="1" otherprops="1">Chapter 1</h1>
    <h2 id="11" otherprops="11">Section 1</h1>
  <h1 id="2" otherprops="2">Chapter 2</h1>
  XXXX
  END

    eq nws(<<END);
  <h1 id="1" otherprops="1">Chapter 1</h1>
    <h2 id="11" otherprops="11">Section 1</h1>
  <h1 id="2" otherprops="2">Chapter 2</h1>
  <table cellspacing=10 border=0>
  <tr><td>&nbsp;
  <tr><td align=right>1<td>&nbsp;&nbsp;&nbsp;&nbsp;<a href="#1">Chapter 1</a>
  <tr><td align=right>2<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="#11">Section 1</a>
  <tr><td>&nbsp;
  <tr><td align=right>3<td>&nbsp;&nbsp;&nbsp;&nbsp;<a href="#2">Chapter 2</a>
  </table>
  END


=head2 expandWellKnownWordsAsUrlsInHtmlFormat($string)

Expand words found in a string using the html B<a> tag to supply a definition of that word.

     Parameter  Description
  1  $string    String containing url names to expand

B<Example:>


    ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq
      q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);

    ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    ok expandWellKnownUrlsInPerlFormat(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(L<GitHub|https://github.com/philiprbrenan>);

    ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);

    ok expandWellKnownUrlsInHtmlFromPerl(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);


    is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';

    is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),
      q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';

    ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa

=begin HTML

<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>

=end   HTML


bbb
"';
  aaa L<GitHub|https://github.com/philiprbrenan> bbb
  END


=head2 expandWellKnownWordsAsUrlsInMdFormat($string)

Expand words found in a string using the md url to supply a definition of that word.

     Parameter  Description
  1  $string    String containing url names to expand

B<Example:>


    ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq
      q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);

    ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    ok expandWellKnownUrlsInPerlFormat(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(L<GitHub|https://github.com/philiprbrenan>);

    ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);

    ok expandWellKnownUrlsInHtmlFromPerl(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),
      q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';


    is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';

    ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa

=begin HTML

<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>

=end   HTML


bbb
"';
  aaa L<GitHub|https://github.com/philiprbrenan> bbb
  END


=head2 expandWellKnownUrlsInPerlFormat($string)

Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format LE<lt>url-nameE<gt> using the Perl POD syntax

     Parameter  Description
  1  $string    String containing url names to expand

B<Example:>


    ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq
      q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);

    ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);


    ok expandWellKnownUrlsInPerlFormat(q(L<GitHub|https://github.com/philiprbrenan>)) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(L<GitHub|https://github.com/philiprbrenan>);


    ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ok expandWellKnownUrlsInHtmlFromPerl(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),
      q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';

    is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),
      q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';

    ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa

=begin HTML

<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>

=end   HTML


bbb
"';
  aaa L<GitHub|https://github.com/philiprbrenan> bbb
  END


=head2 expandWellKnownUrlsInHtmlFormat($string)

Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format L[url-name] using the html B<a> tag.

     Parameter  Description
  1  $string    String containing url names to expand

B<Example:>


    ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq
      q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);


    ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    ok expandWellKnownUrlsInPerlFormat(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(L<GitHub|https://github.com/philiprbrenan>);

    ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);

    ok expandWellKnownUrlsInHtmlFromPerl(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),
      q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';

    is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),
      q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';

    ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa

=begin HTML

<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>

=end   HTML


bbb
"';
  aaa L<GitHub|https://github.com/philiprbrenan> bbb
  END


=head2 expandWellKnownUrlsInHtmlFromPerl($string)

Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format L[url-name] using the html B<a> tag.

     Parameter  Description
  1  $string    String containing url names to expand

B<Example:>


    ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq
      q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);

    ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    ok expandWellKnownUrlsInPerlFormat(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(L<GitHub|https://github.com/philiprbrenan>);

    ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);


    ok expandWellKnownUrlsInHtmlFromPerl(q(L<GitHub|https://github.com/philiprbrenan>)) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),
      q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';

    is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),
      q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';

    ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa

=begin HTML

<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>

=end   HTML


bbb
"';
  aaa L<GitHub|https://github.com/philiprbrenan> bbb
  END


=head2 expandWellKnownUrlsInPod2Html($string)

Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format =begin html format

     Parameter  Description
  1  $string    String containing url names to expand

B<Example:>


    ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq
      q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);

    ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    ok expandWellKnownUrlsInPerlFormat(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(L<GitHub|https://github.com/philiprbrenan>);

    ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);

    ok expandWellKnownUrlsInHtmlFromPerl(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),
      q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';

    is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),
      q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';


    ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa

=begin HTML

<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>

=end   HTML


bbb
"';  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  aaa L<GitHub|https://github.com/philiprbrenan> bbb
  END


=head2 expandWellKnownUrlsInDitaFormat($string)

Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format L[url-name] in the L[Dita] B<xref>format.

     Parameter  Description
  1  $string    String containing url names to expand

B<Example:>



    ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);

    ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    ok expandWellKnownUrlsInPerlFormat(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(L<GitHub|https://github.com/philiprbrenan>);

    ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);

    ok expandWellKnownUrlsInHtmlFromPerl(q(L<GitHub|https://github.com/philiprbrenan>)) eq
      q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

    is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),
      q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';

    is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),
      q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';

    ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa

=begin HTML

<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>

=end   HTML


bbb
"';
  aaa L<GitHub|https://github.com/philiprbrenan> bbb
  END


=head2 expandNewLinesInDocumentation($s)

Expand new lines in documentation, specifically
 for new line and

 for two new lines.

     Parameter  Description
  1  $s         String to be expanded

B<Example:>



  ok expandNewLinesInDocumentation(q(a

  b
  c
)) eq <<END;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a

    b
    c
  END


=head2 extractCodeBlock($comment, $file)

Extract the block of code delimited by B<$comment>, starting at qq($comment-begin), ending at qq($comment-end) from the named B<$file> else the current Perl program $0 and return it as a string or confess if this is not possible.

     Parameter  Description
  1  $comment   Comment delimiting the block of code
  2  $file      File to read from if not $0

B<Example:>



  ok extractCodeBlock(q(#CODEBLOCK), $INC{"Data/Table/Text.pm"}) eq <<'END';  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $a = 1;
    my $b = 2;
  END


=head2 updateDocumentation($perlModule)

Update the documentation for a Perl module from the comments in its source code. Comments between the lines marked with:

  #Dn title # description

and:

  #D

where n is either 1, 2 or 3 indicating the heading level of the section and the # is in column 1.

Methods are formatted as:

  sub name(signature)      #FLAGS comment describing method
   {my ($parameters) = @_; # comments for each parameter separated by commas.

FLAGS can be chosen from:

=over

=item I

method of interest to new users

=item P

private method

=item r

optionally replaceable method

=item R

required replaceable method

=item S

static method

=item X

die rather than received a returned B<undef> result

=back

Other flags will be handed to the method extractDocumentationFlags(flags to process, method name) found in the file being documented, this method should return [the additional documentation for the method, the code to implement the flag].

Text following 'Example:' in the comment (if present) will be placed after the parameters list as an example. Lines containing comments consisting of '#T'.methodName will also be aggregated and displayed as examples for that method.

Lines formatted as:

  BEGIN{*source=*target}

starting in column 1 will define a synonym for a method.

Lines formatted as:

  #C emailAddress text

will be aggregated in the acknowledgments section at the end of the documentation.

The character sequence B<\n> in the comment will be expanded to one new line, B<\m> to two new lines and B<L>B<<$_>>,B<L>B<<confess>>,B<L>B<<die>>,B<L>B<<eval>>,B<L>B<<lvalueMethod>> to links to the perl documentation.

Search for '#D1': in L<https://metacpan.org/source/PRBRENAN/Data-Table-Text-20180810/lib/Data/Table/Text.pm> to see  more examples of such documentation in action - although it is quite difficult to see as it looks just like normal comments placed in the code.

Parameters:


     Parameter    Description
  1  $perlModule  Optional file name with caller's file being the default

B<Example:>



   {my $s = updateDocumentation(<<'END' =~ s(#) (#)gsr =~ s(~) ()gsr);              # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  package Sample::Module;

  #D1 Samples                                                                      # Sample methods.

  sub sample($@)                                                                  #R Documentation for the:  sample() method.  See also L<Data::Table::Text::sample2|/Data::Table::Text::sample2>. #Tsample .
   {my ($node, @context) = @_;                                                    # Node, optional context
    1
   }

  ~BEGIN{*smpl=*sample}

  sub Data::Table::Text::sample2(\&@)                                             #PS Documentation for the sample2() method.
   {my ($sub, @context) = @_;                                                     # Sub to call, context.
    1
   }

  ok sample(undef, qw(a b c)) == 1;                                               #Tsample

  if (1)                                                                          #Tsample
   {ok sample(q(a), qw(a b c))  == 2;
    ok sample(undef, qw(a b c)) == 1;
   }

  ok sample(<<END2)) == 1;                                                        #Tsample
  sample data
  END2

    ok $s =~ m/=head2 Data::Table::Text::sample2.\$sub, \@context/;



=head1 Hash Definitions




=head2 Data::Exchange::Service Definition


Service details.




=head3 Output fields


=head4 file

The file in which the service start details is being recorded.

=head4 service

The name of the service.

=head4 start

The time this service was started time plus a minor hack to simplify testing.



=head2 Data::Table::Text::AwsEc2Price Definition


Prices of selected aws elastic compute instance types




=head3 Output fields


=head4 cheapestInstance

The instance type that has the lowest CPU cost

=head4 pricePerCpu

The cost of the cheapest CPU In millidollars per hour

=head4 report

Report showing the cost of other selected instances



=head2 Data::Table::Text::Python::Documentation Definition


Documentation extracted from Python source files




=head3 Output fields


=head4 classDefinitions

Class definitions

=head4 classFiles

Class files

=head4 comments

Comments for each def

=head4 errors

Errors encountered

=head4 parameters

Parameters for each def

=head4 tests

Tests for each def

=head4 testsCommon

Common line for tests



=head2 Data::Table::Text::Starter Definition


Process starter definition.




=head3 Input fields


=head4 processingLogFile

Optional: name of a file to which process start and end information should be appended

=head4 processingTitle

Optional: title describing the processing being performed.

=head4 totalToBeStarted

Optionally: the total number of processes to be started - if this is supplied then an estimate of the finish time for this processing is printed to the log file every time a process starts or finishes.



=head3 Output fields


=head4 autoRemoveTransferArea

If true then automatically clear the transfer area at the end of processing.

=head4 maximumNumberOfProcesses

The maximum number of processes to start in parallel at one time. If this limit is exceeded, the start of subsequent processes will be delayed until processes started earlier have finished.

=head4 pids

A hash of pids representing processes started but not yet completed.

=head4 processFinishTime

{pid} == time the process finished.

=head4 processStartTime

{pid} == time the process was started.

=head4 processingLogFileHandle

Handle for log file if a log file was supplied

=head4 resultsArray

Consolidated array of results.

=head4 startTime

Start time

=head4 transferArea

The name of the folder in which files transferring results from the child to the parent process will be stored.



=head2 TestHash Definition


Definition of a blessed hash.




=head3 Output fields


=head4 a

Definition of attribute aa.

=head4 b

Definition of attribute bb.

=head4 c

Definition of attribute cc.



=head2 Udsr Definition


Package name




=head3 Input fields


=head4 headerLength

Length of fixed header which carries the length of the following message

=head4 serverAction

Server action sub, which receives a communicator every time a client creates a new connection. If this server is going to be started by systemd  as a service with the specified L<serverName> then this is the a actual text of the code that will be installed as a CGI script and run in response to an incoming transaction in a separate process with the userid set to L<serviceUser>. It receives the text of the http request from the browser as parameter 1 and should return the text to be sent back to the browser.

=head4 serviceName

Service name for install by systemd

=head4 serviceUser

Userid for service

=head4 socketPath

Socket file



=head3 Output fields


=head4 client

Client socket and connection socket

=head4 serverPid

Server pid which can be used to kill the server via kill q(kill), $pid



=head1 Attributes


The following is a list of all the attributes in this package.  A method coded
with the same name in your package will over ride the method of the same name
in this package and thus provide your value for the attribute in place of the
default value supplied for this attribute by this package.

=head2 Replaceable Attribute List


awsEc2DescribeInstancesCache awsIpFile nameFromStringMaximumLength wwwHeader


=head2 awsEc2DescribeInstancesCache

File in which to cache latest results from describe instances to avoid being throttled


=head2 awsIpFile

File in which to save IP address of primary instance on Aws


=head2 nameFromStringMaximumLength

Maximum length of a name generated from a string


=head2 wwwHeader

Html header




=head1 Private Methods

=head2 onWindows()

Are we on windows


=head2 filePathSeparatorChar()

File path separator


=head2 denormalizeFolderName($name)

Remove any trailing folder separator from a folder name.

     Parameter  Description
  1  $name      Folder name

=head2 renormalizeFolderName($name)

Normalize a folder name by ensuring it has a single trailing directory separator.

     Parameter  Description
  1  $name      Name

=head2 prefferedFileName($name)

Normalize a file name

     Parameter  Description
  1  $name      Name

=head2 findAllFilesAndFolders($folder, $dirs)

Find all the files and folders under a folder.

     Parameter  Description
  1  $folder    Folder to start the search with
  2  $dirs      True if only folders are required

=head2 readUtf16File($file)

Read a file containing L<Unicode|https://en.wikipedia.org/wiki/Unicode> encoded in utf-16.

     Parameter  Description
  1  $file      Name of file to read

=head2 binModeAllUtf8()

Set STDOUT and STDERR to accept utf8 without complaint.


B<Example:>



    binModeAllUtf8;                                                                 # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 convertImageToJpx690($Source, $target, $Size, $Tiles)

Convert a B<$source> image to a B<$target> image in jpx format using versions of L<Imagemagick|https://www.imagemagick.org/script/index.php> version 6.9.0 and above. The size in pixels of each jpx tile may be specified by the optional B<$Size> parameter which defaults to B<256>. B<$Tiles> optionally provides an upper limit on the number of each tiles in each dimension.

     Parameter  Description
  1  $Source    Source file
  2  $target    Target folder (as multiple files will be created)
  3  $Size      Optional size of each tile - defaults to 256
  4  $Tiles     Optional limit on the number of tiles in either dimension

=head2 convertImageToJpx($Source, $target, $Size, $Tiles)

Convert a B<$source> image to a B<$target> image in jpx format. The size in pixels of each jpx tile may be specified by the optional B<$Size> parameter which defaults to B<256>. B<$Tiles> optionally provides an upper limit on the number of each tiles in each dimension.

     Parameter  Description
  1  $Source    Source file
  2  $target    Target folder (as multiple files will be created)
  3  $Size      Optional size of each tile - defaults to 256
  4  $Tiles     Optional limit in either direction on the number of tiles

B<Example:>



    convertImageToJpx(fpe(qw(a image jpg)), fpe(qw(a image jpg)), 256);             # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



=head2 setCombination(@s)

Count the elements in sets B<@s> represented as arrays of strings and/or the keys of hashes

     Parameter  Description
  1  @s         Array of arrays of strings and/or hashes

=head2 formatTableMultiLine($data, $separator)

Tabularize text that has new lines in it.

     Parameter   Description
  1  $data       Reference to an array of arrays of data to be formatted as a table
  2  $separator  Optional line separator to use instead of new line for each row.

=head2 formatTableClearUpLeft($data)

Blank identical column values up and left

     Parameter  Description
  1  $data      Array of arrays

=head2 formatTableAA($data, $title, %options)

Tabularize an array of arrays.

     Parameter  Description
  1  $data      Data to be formatted
  2  $title     Reference to an array of titles
  3  %options   Options

B<Example:>


   ok formatTable
    ([[1,1,1],[1,1,2],[1,2,2],[1,2,3]], [], clearUpLeft=>1) eq <<END;             # Clear matching columns

  1  1  1  1
  2        2
  3     2  2
  4        3
  END


=head2 formatTableHA($data, $title)

Tabularize a hash of arrays.

     Parameter  Description
  1  $data      Data to be formatted
  2  $title     Optional titles

=head2 formatTableAH($data)

Tabularize an array of hashes.

     Parameter  Description
  1  $data      Data to be formatted

=head2 formatTableHH($data)

Tabularize a hash of hashes.

     Parameter  Description
  1  $data      Data to be formatted

=head2 formatTableA($data, $title)

Tabularize an array.

     Parameter  Description
  1  $data      Data to be formatted
  2  $title     Optional title

=head2 formatTableH($data, $title)

Tabularize a hash.

     Parameter  Description
  1  $data      Data to be formatted
  2  $title     Optional title

=head2 formatTableCheckKeys()

Options available for formatting tables


=head2 reloadHashes2($d, $progress)

Ensures that all the hashes within a tower of data structures have LValue methods to get and set their current keys.

     Parameter  Description
  1  $d         Data structure
  2  $progress  Progress

=head2 showHashes2($d, $keys, $progress)

Create a map of all the keys within all the hashes within a tower of data structures.

     Parameter  Description
  1  $d         Data structure
  2  $keys      Keys found
  3  $progress  Progress

=head2 showHashes($d)

Create a map of all the keys within all the hashes within a tower of data structures.

     Parameter  Description
  1  $d         Data structure

=head2 newUdsr(@parms)

Create a communicator - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.

     Parameter  Description
  1  @parms     Attributes per L<Udsr Definition|/Udsr Definition>

=head2 awsInstanceId(%options)

Create an instance-id from the specified B<%options>

     Parameter  Description
  1  %options   Options

=head2 awsProfile(%options)

Create a profile keyword from the specified B<%options>

     Parameter  Description
  1  %options   Options

=head2 awsRegion(%options)

Create a region keyword from the specified B<%options>

     Parameter  Description
  1  %options   Options

=head2 getNumberOfCpus()

Number of cpus


=head2 saveSourceToS3($aws, $saveIntervalInSeconds)

Save source code.

     Parameter               Description
  1  $aws                    Aws target file and keywords
  2  $saveIntervalInSeconds  Save internal

=head2 awsParallelProcessFilesTestParallel($userData, $file)

Test running on L<Amazon Web Services|http://aws.amazon.com> in parallel.

     Parameter  Description
  1  $userData  User data
  2  $file      File to process.

B<Example:>


    my $N = 2001;                                                                 # Number of files to process
    my $options = q(region => q(us-east-2), profile=>q(fmc));                     # Aws cli options
    my %options = eval "($options)";

    for my $dir(q(/home/phil/perl/cpan/DataTableText/lib/Data/Table/),            # Folders we will need on aws
                q(/home/phil/.aws/))
     {awsParallelSpreadFolder($dir, %options);
     }

    my $d = temporaryFolder;                                                      # Create a temporary folder
    my $resultsFile = fpe($d, qw(results data));                                  # Save results in this temporary file

    if (my $r = execPerlOnRemote(join "
",                                       # Execute some code on a server

      getCodeContext(\&awsParallelProcessFilesTestParallel),                      # Get code context of the sub we want to call.  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      <<SESSIONLEADER))                                                           # Launch code on session leader
  use Data::Table::Text qw(:all);

  my \$r = awsParallelProcessFiles                                                # Process files on multiple L<Amazon Web Services|http://aws.amazon.com> instances in parallel
   ({file=>4, time=>timeStamp},                                                   # User data

    \\\&Data::Table::Text::awsParallelProcessFilesTestParallel,                   # Reference to code to execute in parallel on each session instance  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    \\\&Data::Table::Text::awsParallelProcessFilesTestResults,                    # Reference to code to execute in series to merge the results of each parallel computation
    [map {writeFile(fpe(q($d), \$_, qw(txt)), \$_)} 1..$N],                       # Files to process
    $options);                                                                    # Aws cli options as we will be running on Aws

  storeFile(q($resultsFile), \$r);                                                # Save results in a file

  SESSIONLEADER

     {copyFileFromRemote($resultsFile);                                           # Retrieve user data

      my $userData = retrieveFile($resultsFile);                                  # Recover user data
      my @i = awsParallelSecondaryIpAddresses(%options);                          # Ip addresses of secondary instances
      my @I = keys $userData->{ip}->%*;
      is_deeply [sort @i], [sort @I];                                             # Each secondary ip address was used

      ok $userData->{file}  == 4;                                                 # Prove we can pass data in and get it back
      ok $userData->{merge} == 1 + @i, 'ii';                                      # Number of merges

      my %f; my %i;                                                               # Files processed on each ip
      for   my $i(sort keys $userData->{ipFile}->%*)                              # Ip
       {for my $f(sort keys $userData->{ipFile}{$i}->%*)                          # File
         {$f{fn($f)}++;                                                           # Files processed
          $i{$i}++;                                                               # Count files on each ip
         }
       }

      is_deeply \%f, {map {$_=>1} 1..$N};                                         # Check each file was processed

      if (1)
       {my @rc; my @ra;                                                           # Range of number of files processed on each ip - computed, actually counted
        my $l = $N/@i-1;                                                          # Lower limit of number of files per IP address
        my $h = $N/@i+1;                                                          # Upper limit of number of files per IP address
        for   my $i(keys %i)
         {my $nc = $i{$i};                                                        # Number of files processed on this ip - computed
          my $na = $userData->{ip}{$i};                                           # Number of files processed on this ip - actually counted
          push @rc, ($nc >= $l and $nc <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
          push @ra, ($na >= $l and $na <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
         }
        ok @i == grep {$_} @ra;                                                   # Check each ip processed the expected number of files
        ok @i == grep {$_} @rc;
       }

      ok $userData->{files}{&fpe($d, qw(4 txt))} eq                               # Check the computed MD5 sum for the specified file
         q(a87ff679a2f3e71d9181a67b7542122c);
     }


=head2 awsParallelProcessFilesTestResults($userData, @results)

Test results of running on L<Amazon Web Services|http://aws.amazon.com> in parallel.

     Parameter  Description
  1  $userData  User data from primary instance instance or process
  2  @results   Results from each parallel instance or process

B<Example:>


    my $N = 2001;                                                                 # Number of files to process
    my $options = q(region => q(us-east-2), profile=>q(fmc));                     # Aws cli options
    my %options = eval "($options)";

    for my $dir(q(/home/phil/perl/cpan/DataTableText/lib/Data/Table/),            # Folders we will need on aws
                q(/home/phil/.aws/))
     {awsParallelSpreadFolder($dir, %options);
     }

    my $d = temporaryFolder;                                                      # Create a temporary folder
    my $resultsFile = fpe($d, qw(results data));                                  # Save results in this temporary file

    if (my $r = execPerlOnRemote(join "
",                                       # Execute some code on a server
      getCodeContext(\&awsParallelProcessFilesTestParallel),                      # Get code context of the sub we want to call.
      <<SESSIONLEADER))                                                           # Launch code on session leader
  use Data::Table::Text qw(:all);

  my \$r = awsParallelProcessFiles                                                # Process files on multiple L<Amazon Web Services|http://aws.amazon.com> instances in parallel
   ({file=>4, time=>timeStamp},                                                   # User data
    \\\&Data::Table::Text::awsParallelProcessFilesTestParallel,                   # Reference to code to execute in parallel on each session instance

    \\\&Data::Table::Text::awsParallelProcessFilesTestResults,                    # Reference to code to execute in series to merge the results of each parallel computation  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    [map {writeFile(fpe(q($d), \$_, qw(txt)), \$_)} 1..$N],                       # Files to process
    $options);                                                                    # Aws cli options as we will be running on Aws

  storeFile(q($resultsFile), \$r);                                                # Save results in a file

  SESSIONLEADER

     {copyFileFromRemote($resultsFile);                                           # Retrieve user data

      my $userData = retrieveFile($resultsFile);                                  # Recover user data
      my @i = awsParallelSecondaryIpAddresses(%options);                          # Ip addresses of secondary instances
      my @I = keys $userData->{ip}->%*;
      is_deeply [sort @i], [sort @I];                                             # Each secondary ip address was used

      ok $userData->{file}  == 4;                                                 # Prove we can pass data in and get it back
      ok $userData->{merge} == 1 + @i, 'ii';                                      # Number of merges

      my %f; my %i;                                                               # Files processed on each ip
      for   my $i(sort keys $userData->{ipFile}->%*)                              # Ip
       {for my $f(sort keys $userData->{ipFile}{$i}->%*)                          # File
         {$f{fn($f)}++;                                                           # Files processed
          $i{$i}++;                                                               # Count files on each ip
         }
       }

      is_deeply \%f, {map {$_=>1} 1..$N};                                         # Check each file was processed

      if (1)
       {my @rc; my @ra;                                                           # Range of number of files processed on each ip - computed, actually counted
        my $l = $N/@i-1;                                                          # Lower limit of number of files per IP address
        my $h = $N/@i+1;                                                          # Upper limit of number of files per IP address
        for   my $i(keys %i)
         {my $nc = $i{$i};                                                        # Number of files processed on this ip - computed
          my $na = $userData->{ip}{$i};                                           # Number of files processed on this ip - actually counted
          push @rc, ($nc >= $l and $nc <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
          push @ra, ($na >= $l and $na <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
         }
        ok @i == grep {$_} @ra;                                                   # Check each ip processed the expected number of files
        ok @i == grep {$_} @rc;
       }

      ok $userData->{files}{&fpe($d, qw(4 txt))} eq                               # Check the computed MD5 sum for the specified file
         q(a87ff679a2f3e71d9181a67b7542122c);
     }


=head2 s3Profile(%options)

Return an S3 profile keyword from an S3 option set

     Parameter  Description
  1  %options   Options

=head2 s3Delete(%options)

Return an S3 --delete keyword from an S3 option set

     Parameter  Description
  1  %options   Options

=head2 Data::Table::Text::Starter::logEntry($starter, $finish)

Create a log entry showing progress and eta.

     Parameter  Description
  1  $starter   Starter
  2  $finish    0 - start; 1 - finish

=head2 Data::Table::Text::Starter::averageProcessTime($starter)

Average elapsed time spent by each process

     Parameter  Description
  1  $starter   Starter

=head2 Data::Table::Text::Starter::say($starter, @message)

Write to the log file if it is available.

     Parameter  Description
  1  $starter   Starter
  2  @message   Text to write to log file.

=head2 Data::Table::Text::Starter::waitOne($starter)

Wait for at least one process to finish and consolidate its results.

     Parameter  Description
  1  $starter   Starter

=head2 countSquareArray(@square)

Count the number of elements in a square array

     Parameter  Description
  1  @square    Array of arrays

=head2 processSizesInParallelN($N, $parallel, $results, @sizes)

Process items of known size in parallel using the specified number B<$N> processes with the process each file is assigned to depending on the size of the file so that each process is loaded with approximately the same number of bytes of data in total from the files it processes.

Each file is processed by sub B<$parallel> and the results of processing all files is processed by B<$results> where the files are taken from B<@files>. Each B<&$parallel> receives a file from B<@files>. B<&$results> receives an array of all the results returned by B<&$parallel>.

     Parameter  Description
  1  $N         Number of processes
  2  $parallel  Parallel sub
  3  $results   Results sub
  4  @sizes     Array of [size; item] to process by size

=head2 wellKnownUrls()

Short names for some well known urls


=head2 reinstateWellKnown($string)

Contract references to well known Urls to their abbreviated form

     Parameter  Description
  1  $string    Source string

=head2 formatSourcePodAsHtml()

Format the L<POD|https://perldoc.perl.org/perlpod.html> in the current source file as L<HTML|https://en.wikipedia.org/wiki/HTML>.


=head2 extractTest($string)

Remove example markers from test code.

     Parameter  Description
  1  $string    String containing test line

=head2 docUserFlags($flags, $perlModule, $package, $name)

Generate documentation for a method by calling the extractDocumentationFlags method in the package being documented, passing it the flags for a method and the name of the method. The called method should return the documentation to be inserted for the named method.

     Parameter    Description
  1  $flags       Flags
  2  $perlModule  File containing documentation
  3  $package     Package containing documentation
  4  $name        Name of method to be processed

=head2 updatePerlModuleDocumentation($perlModule)

Update the documentation in a B<$perlModule> and display said documentation in a web browser.

     Parameter    Description
  1  $perlModule  File containing the code of the perl module

=head2 extractPythonDocumentationFromFiles(@sources)

Extract python documentation from the specified files

     Parameter  Description
  1  @sources   Python source files


=head1 Synonyms

B<fpd> is a synonym for L<filePathDir|/filePathDir> - Create a folder name from a list of  names.

B<fpe> is a synonym for L<filePathExt|/filePathExt> - Create a file name from a list of  names the last of which is assumed to be the extension of the file name.

B<fpf> is a synonym for L<filePath|/filePath> - Create a file name from a list of  names.

B<owf> is a synonym for L<overWriteFile|/overWriteFile> - Write to a B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

B<temporaryDirectory> is a synonym for L<temporaryFolder|/temporaryFolder> - Create a new, empty, temporary folder.



=head1 Index


1 L<absFile|/absFile> - Return the name of the given file if it a fully qualified file name else returns B<undef>.

2 L<absFromAbsPlusRel|/absFromAbsPlusRel> - Absolute file from an absolute file B<$a> plus a relative file B<$r>.

3 L<addCertificate|/addCertificate> - Add a certificate to the current ssh session.

4 L<addLValueScalarMethods|/addLValueScalarMethods> - Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> scalar methods in the current package if they do not already exist.

5 L<appendFile|/appendFile> - Append to B<$file> a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded with L<utf8|https://en.wikipedia.org/wiki/UTF-8>, creating the $file first if necessary.

6 L<arrayProduct|/arrayProduct> - Find the product of any strings that look like numbers in an array.

7 L<arraySum|/arraySum> - Find the sum of any strings that look like numbers in an array.

8 L<arrayTimes|/arrayTimes> - Multiply by B<$multiplier> each element of the array B<@a> and return as the result.

9 L<arrayToHash|/arrayToHash> - Create a hash reference from an array

10 L<asciiToHexString|/asciiToHexString> - Encode an L<Ascii|https://en.wikipedia.org/wiki/ASCII> string as a string of L<hexadecimal|https://en.wikipedia.org/wiki/Hexadecimal> digits.

11 L<assertPackageRefs|/assertPackageRefs> - Confirm that the specified references are to the specified package

12 L<assertRef|/assertRef> - Confirm that the specified references are to the package into which this routine has been exported.

13 L<awsCurrentAvailabilityZone|/awsCurrentAvailabilityZone> - Get the availability zone of the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.

14 L<awsCurrentInstanceId|/awsCurrentInstanceId> - Get the instance id of the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.

15 L<awsCurrentInstanceType|/awsCurrentInstanceType> - Get the instance type of the L<Amazon Web Services|http://aws.amazon.com> server if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.

16 L<awsCurrentIp|/awsCurrentIp> - Get the ip address of the AWS server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.

17 L<awsCurrentLinuxSpotPrices|/awsCurrentLinuxSpotPrices> - Return {instance type} = cheapest spot price in dollars per hour for the given region

18 L<awsCurrentRegion|/awsCurrentRegion> - Get the region of the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.

19 L<awsEc2CreateImage|/awsEc2CreateImage> - Create an image snap shot with the specified B<$name> of the AWS server we are currently running on if we are running on an AWS server else return false.

20 L<awsEc2DescribeImages|/awsEc2DescribeImages> - Describe images available.

21 L<awsEc2DescribeInstances|/awsEc2DescribeInstances> - Describe the L<Amazon Web Services|http://aws.amazon.com> instances running in a B<$region>.

22 L<awsEc2DescribeInstancesGetIPAddresses|/awsEc2DescribeInstancesGetIPAddresses> - Return a hash of {instanceId => public ip address} for all running instances on L<Amazon Web Services|http://aws.amazon.com> with ip addresses.

23 L<awsEc2DescribeInstanceType|/awsEc2DescribeInstanceType> - Return details of the specified instance type.

24 L<awsEc2DescribeSpotInstances|/awsEc2DescribeSpotInstances> - Return a hash {spot instance request => spot instance details} describing the status of active spot instances.

25 L<awsEc2FindImagesWithTagValue|/awsEc2FindImagesWithTagValue> - Find images with a tag that matches the specified regular expression B<$value>.

26 L<awsEc2InstanceIpAddress|/awsEc2InstanceIpAddress> - Return the IP address of a named instance on L<Amazon Web Services|http://aws.amazon.com> else return B<undef>.

27 L<awsEc2ReportSpotInstancePrices|/awsEc2ReportSpotInstancePrices> - Report the prices of all the spot instances whose type matches a regular expression B<$instanceTypeRe>.

28 L<awsEc2RequestSpotInstances|/awsEc2RequestSpotInstances> - Request spot instances as long as they can be started within the next minute.

29 L<awsEc2Tag|/awsEc2Tag> - Tag an elastic compute resource with the supplied tags.

30 L<awsExecCli|/awsExecCli> - Execute an AWs command and return its response

31 L<awsExecCliJson|/awsExecCliJson> - Execute an AWs command and decode the json so produced

32 L<awsInstanceId|/awsInstanceId> - Create an instance-id from the specified B<%options>

33 L<awsIp|/awsIp> - Get ip address of server at L<Amazon Web Services|http://aws.amazon.com>.

34 L<awsMetaData|/awsMetaData> - Get an item of meta data for the L<Amazon Web Services|http://aws.amazon.com> server we are currently running on if we are running on an L<Amazon Web Services|http://aws.amazon.com> server else return a blank string.

35 L<awsParallelGatherFolder|/awsParallelGatherFolder> - On L<Amazon Web Services|http://aws.amazon.com>: merges all the files in the specified B<$folder> on each secondary instance to the corresponding folder on the primary instance in parallel.

36 L<awsParallelIpAddresses|/awsParallelIpAddresses> - Return the IP addresses of all the L<Amazon Web Services|http://aws.amazon.com> session instances.

37 L<awsParallelPrimaryInstanceId|/awsParallelPrimaryInstanceId> - Return the instance id of the primary instance.

38 L<awsParallelPrimaryIpAddress|/awsParallelPrimaryIpAddress> - Return the IP addresses of any primary instance on L<Amazon Web Services|http://aws.amazon.com>.

39 L<awsParallelProcessFiles|/awsParallelProcessFiles> - Process files in parallel across multiple L<Amazon Web Services|http://aws.amazon.com> instances if available or in series if not.

40 L<awsParallelProcessFilesTestParallel|/awsParallelProcessFilesTestParallel> - Test running on L<Amazon Web Services|http://aws.amazon.com> in parallel.

41 L<awsParallelProcessFilesTestResults|/awsParallelProcessFilesTestResults> - Test results of running on L<Amazon Web Services|http://aws.amazon.com> in parallel.

42 L<awsParallelSecondaryIpAddresses|/awsParallelSecondaryIpAddresses> - Return a list containing the IP addresses of any secondary instances on L<Amazon Web Services|http://aws.amazon.com>.

43 L<awsParallelSpreadFolder|/awsParallelSpreadFolder> - On L<Amazon Web Services|http://aws.amazon.com>: copies a specified B<$folder> from the primary instance, see: L<awsParallelPrimaryInstanceId>, in parallel, to all the secondary instances in the session.

44 L<awsProfile|/awsProfile> - Create a profile keyword from the specified B<%options>

45 L<awsR53a|/awsR53a> - Create/Update a B<A> L<Domain Name System|https://en.wikipedia.org/wiki/Domain_Name_System> record for the specified server.

46 L<awsR53aaaa|/awsR53aaaa> - Create/Update a B<AAAA> L<Domain Name System|https://en.wikipedia.org/wiki/Domain_Name_System> record for the specified server.

47 L<awsRegion|/awsRegion> - Create a region keyword from the specified B<%options>

48 L<awsTranslateText|/awsTranslateText> - Translate B<$text> from English to a specified B<$language> using AWS Translate with the specified global B<$options> and return the translated string.

49 L<binModeAllUtf8|/binModeAllUtf8> - Set STDOUT and STDERR to accept utf8 without complaint.

50 L<boldString|/boldString> - Convert alphanumerics in a string to bold.

51 L<boldStringUndo|/boldStringUndo> - Undo alphanumerics in a string to bold.

52 L<call|/call> - Call the specified B<$sub> in a separate child process, wait for it to complete, then copy back the named B<@our> variables from the child process to the calling parent process effectively freeing any memory used during the call.

53 L<callSubInOverlappedParallel|/callSubInOverlappedParallel> - Call the B<$child> sub reference in parallel in a separate child process and ignore its results while calling the B<$parent> sub reference in the parent process and returning its results.

54 L<callSubInParallel|/callSubInParallel> - Call a sub reference in parallel to avoid memory fragmentation and return its results.

55 L<checkFile|/checkFile> - Return the name of the specified file if it exists, else confess the maximum extent of the path that does exist.

56 L<checkKeys|/checkKeys> - Check the keys in a B<hash> conform to those B<$permitted>.

57 L<childPids|/childPids> - Recursively find the pids of all the sub processes of a B<$process> and all their sub processes and so on returning the specified pid and all its child pids as a list.

58 L<chooseStringAtRandom|/chooseStringAtRandom> - Choose a string at random from the list of B<@strings> supplied.

59 L<clearFolder|/clearFolder> - Remove all the files and folders under and including the specified B<$folder> as long as the number of files to be removed is less than the specified B<$limitCount>.

60 L<cmpArrays|/cmpArrays> - Compare two arrays of strings

61 L<confirmHasCommandLineCommand|/confirmHasCommandLineCommand> - Check that the specified b<$cmd> is present on the current system.

62 L<containingFolderName|/containingFolderName> - The name of a folder containing a file

63 L<containingPowerOfTwo|/containingPowerOfTwo> - Find log two of the lowest power of two greater than or equal to a number B<$n>.

64 L<contains|/contains> - Returns the indices at which an B<$item> matches elements of the specified B<@array>.

65 L<convertDocxToFodt|/convertDocxToFodt> - Convert a I<docx> B<$inputFile> file to a I<fodt> B<$outputFile> using B<unoconv> which must not be running elsewhere at the time.

66 L<convertImageToJpx|/convertImageToJpx> - Convert a B<$source> image to a B<$target> image in jpx format.

67 L<convertImageToJpx690|/convertImageToJpx690> - Convert a B<$source> image to a B<$target> image in jpx format using versions of L<Imagemagick|https://www.imagemagick.org/script/index.php> version 6.

68 L<convertPerlToJavaScript|/convertPerlToJavaScript> - Convert Perl to Javascript

69 L<convertUnicodeToXml|/convertUnicodeToXml> - Convert a B<$string> with L<Unicode|https://en.wikipedia.org/wiki/Unicode> code points that are not directly representable in L<Ascii|https://en.wikipedia.org/wiki/ASCII> into string that replaces these code points with their representation in L<Xml|https://en.wikipedia.org/wiki/XML> making the string usable in L<Xml|https://en.wikipedia.org/wiki/XML> documents.

70 L<copyBinaryFile|/copyBinaryFile> - Copy the binary file B<$source> to a file named <%target> and return the target file name,

71 L<copyBinaryFileMd5Normalized|/copyBinaryFileMd5Normalized> - Normalize the name of the specified B<$source> file to the md5 sum of its content, retaining its current extension, while placing the original file name in a companion file if the companion file does not already exist.

72 L<copyBinaryFileMd5NormalizedCreate|/copyBinaryFileMd5NormalizedCreate> - Create a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content>, whose extension is B<$extension> and which has a companion file with the same name minus the extension  which contains the specified B<$companionContent>.

73 L<copyBinaryFileMd5NormalizedGetCompanionContent|/copyBinaryFileMd5NormalizedGetCompanionContent> - Return the original name of the specified B<$source> file after it has been normalized via L<copyBinaryFileMd5Normalized|/copyBinaryFileMd5Normalized> or L<copyBinaryFileMd5NormalizedCreate|/copyBinaryFileMd5NormalizedCreate> or return B<undef> if the corresponding companion file does not exist.

74 L<copyFile|/copyFile> - Copy the B<$source> file encoded in utf8 to the specified B<$target> file in and return $target.

75 L<copyFileFromRemote|/copyFileFromRemote> - Copy the specified B<$file> from the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

76 L<copyFileMd5Normalized|/copyFileMd5Normalized> - Normalize the name of the specified B<$source> file to the md5 sum of its content, retaining its current extension, while placing the original file name in a companion file if the companion file does not already exist.

77 L<copyFileMd5NormalizedCreate|/copyFileMd5NormalizedCreate> - Create a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content>, whose extension is B<$extension> and which has a companion file with the same name minus the extension which contains the specified B<$companionContent>.

78 L<copyFileMd5NormalizedDelete|/copyFileMd5NormalizedDelete> - Delete a normalized and its companion file

79 L<copyFileMd5NormalizedGetCompanionContent|/copyFileMd5NormalizedGetCompanionContent> - Return the content of the companion file to the specified B<$source> file after it has been normalized via L<copyFileMd5Normalized|/copyFileMd5Normalized> or L<copyFileMd5NormalizedCreate|/copyFileMd5NormalizedCreate> or return B<undef> if the corresponding companion file does not exist.

80 L<copyFileMd5NormalizedName|/copyFileMd5NormalizedName> - Name a file using the GB Standard

81 L<copyFileToFolder|/copyFileToFolder> - Copy the file named in B<$source> to the specified B<$targetFolder/> or if $targetFolder/ is in fact a file into the folder containing this file and return the target file name.

82 L<copyFileToRemote|/copyFileToRemote> - Copy the specified local B<$file> to the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

83 L<copyFolder|/copyFolder> - Copy the B<$source> folder to the B<$target> folder after clearing the $target folder.

84 L<copyFolderToRemote|/copyFolderToRemote> - Copy the specified local B<$Source> folder to the corresponding remote folder on the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

85 L<countFileExtensions|/countFileExtensions> - Return a hash which counts the file extensions in and below the folders in the specified list.

86 L<countFileTypes|/countFileTypes> - Return a hash which counts, in parallel with a maximum number of processes: B<$maximumNumberOfProcesses>, the results of applying the B<file> command to each file in and under the specified B<@folders>.

87 L<countOccurencesInString|/countOccurencesInString> - Returns the number of occurrences in B<$inString> of B<$searchFor>.

88 L<countSquareArray|/countSquareArray> - Count the number of elements in a square array

89 L<createEmptyFile|/createEmptyFile> - Create an empty file unless the file already exists and return the name of the file else confess if the file cannot be created.

90 L<currentDirectory|/currentDirectory> - Get the current working directory.

91 L<currentDirectoryAbove|/currentDirectoryAbove> - Get the path to the folder above the current working folder.

92 L<cutOutImagesInFodtFile|/cutOutImagesInFodtFile> - Cut out the images embedded in a B<fodt> file, perhaps produced via L<convertDocxToFodt|/convertDocxToFodt>, placing them in the specified folder and replacing them in the source file with:

  <image href="$imageFile" outputclass="imageType">.

93 L<Data::Exchange::Service::check|/Data::Exchange::Service::check> - Check that we are the current incarnation of the named service with details obtained from L<newServiceIncarnation|/newServiceIncarnation>.

94 L<Data::Table::Text::Starter::averageProcessTime|/Data::Table::Text::Starter::averageProcessTime> - Average elapsed time spent by each process

95 L<Data::Table::Text::Starter::finish|/Data::Table::Text::Starter::finish> - Wait for all started processes to finish and return their results as an array.

96 L<Data::Table::Text::Starter::logEntry|/Data::Table::Text::Starter::logEntry> - Create a log entry showing progress and eta.

97 L<Data::Table::Text::Starter::say|/Data::Table::Text::Starter::say> - Write to the log file if it is available.

98 L<Data::Table::Text::Starter::start|/Data::Table::Text::Starter::start> - Start a new process to run the specified B<$sub>.

99 L<Data::Table::Text::Starter::waitOne|/Data::Table::Text::Starter::waitOne> - Wait for at least one process to finish and consolidate its results.

100 L<dateStamp|/dateStamp> - Year-monthName-day

101 L<dateTimeStamp|/dateTimeStamp> - Year-monthNumber-day at hours:minute:seconds

102 L<dateTimeStampName|/dateTimeStampName> - Date time stamp without white space.

103 L<ddd|/ddd> - Dump data

104 L<decodeBase64|/decodeBase64> - Decode an L<Ascii|https://en.wikipedia.org/wiki/ASCII> B<$string> in base 64.

105 L<decodeJson|/decodeJson> - Convert a L<Json|https://en.wikipedia.org/wiki/JSON> B<$string> to a L<Perl|http://www.perl.org/> data structure.

106 L<deduplicateSequentialWordsInString|/deduplicateSequentialWordsInString> - Remove sequentially duplicate words in a string

107 L<denormalizeFolderName|/denormalizeFolderName> - Remove any trailing folder separator from a folder name.

108 L<deSquareArray|/deSquareArray> - Create a one dimensional array from a two dimensional array of arrays

109 L<detagString|/detagString> - Remove L<HTML|https://en.wikipedia.org/wiki/HTML> or L<Xml|https://en.wikipedia.org/wiki/XML> tags from a string

110 L<docUserFlags|/docUserFlags> - Generate documentation for a method by calling the extractDocumentationFlags method in the package being documented, passing it the flags for a method and the name of the method.

111 L<downloadGitHubPublicRepo|/downloadGitHubPublicRepo> - Get the contents of a public repo on GitHub and place them in a temporary folder whose name is returned to the caller or confess if no such repo exists.

112 L<downloadGitHubPublicRepoFile|/downloadGitHubPublicRepoFile> - Get the contents of a B<$user> B<$repo> B<$file> from  a public repo on GitHub and return them as a string.

113 L<dumpFile|/dumpFile> - Dump to a B<$file> the referenced data B<$structure>.

114 L<dumpFileAsJson|/dumpFileAsJson> - Dump to a B<$file> the referenced data B<$structure> represented as L<Json|https://en.wikipedia.org/wiki/JSON> string.

115 L<dumpGZipFile|/dumpGZipFile> - Write to a B<$file> a data B<$structure> through L<gzip|https://en.wikipedia.org/wiki/Gzip>.

116 L<dumpTempFile|/dumpTempFile> - Dump a data structure to a temporary file and return the name of the file created

117 L<dumpTempFileAsJson|/dumpTempFileAsJson> - Dump a data structure represented as L<Json|https://en.wikipedia.org/wiki/JSON> string to a temporary file and return the name of the file created.

118 L<enclosedReversedString|/enclosedReversedString> - Convert alphanumerics in a string to enclosed reversed alphanumerics.

119 L<enclosedReversedStringUndo|/enclosedReversedStringUndo> - Undo alphanumerics in a string to enclosed reversed alphanumerics.

120 L<enclosedString|/enclosedString> - Convert alphanumerics in a string to enclosed alphanumerics.

121 L<enclosedStringUndo|/enclosedStringUndo> - Undo alphanumerics in a string to enclosed alphanumerics.

122 L<encodeBase64|/encodeBase64> - Encode an L<Ascii|https://en.wikipedia.org/wiki/ASCII> B<$string> in base 64.

123 L<encodeJson|/encodeJson> - Convert a L<Perl|http://www.perl.org/> data B<$structure> to a L<Json|https://en.wikipedia.org/wiki/JSON> string.

124 L<evalFile|/evalFile> - Read a file containing L<Unicode|https://en.wikipedia.org/wiki/Unicode> content represented as L<utf8|https://en.wikipedia.org/wiki/UTF-8>, L<perlfunc/eval> the content, confess to any errors and then return any result with L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> methods to access each hash element.

125 L<evalFileAsJson|/evalFileAsJson> - Read a B<$file> containing L<Json|https://en.wikipedia.org/wiki/JSON> and return the corresponding L<Perl|http://www.perl.org/> data structure.

126 L<evalGZipFile|/evalGZipFile> - Read a file compressed with L<gzip|https://en.wikipedia.org/wiki/Gzip> containing L<Unicode|https://en.wikipedia.org/wiki/Unicode> content represented as L<utf8|https://en.wikipedia.org/wiki/UTF-8>, L<perlfunc/eval> the content, confess to any errors and then return any result with L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> methods to access each hash element.

127 L<execPerlOnRemote|/execPerlOnRemote> - Execute some Perl B<$code> on the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

128 L<expandNewLinesInDocumentation|/expandNewLinesInDocumentation> - Expand new lines in documentation, specifically
 for new line and

 for two new lines.

129 L<expandWellKnownUrlsInDitaFormat|/expandWellKnownUrlsInDitaFormat> - Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format L[url-name] in the L[Dita] B<xref>format.

130 L<expandWellKnownUrlsInHtmlFormat|/expandWellKnownUrlsInHtmlFormat> - Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format L[url-name] using the html B<a> tag.

131 L<expandWellKnownUrlsInHtmlFromPerl|/expandWellKnownUrlsInHtmlFromPerl> - Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format L[url-name] using the html B<a> tag.

132 L<expandWellKnownUrlsInPerlFormat|/expandWellKnownUrlsInPerlFormat> - Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format LE<lt>url-nameE<gt> using the Perl POD syntax

133 L<expandWellKnownUrlsInPod2Html|/expandWellKnownUrlsInPod2Html> - Expand short L<url|https://en.wikipedia.org/wiki/URL> names found in a string in the format =begin html format

134 L<expandWellKnownWordsAsUrlsInHtmlFormat|/expandWellKnownWordsAsUrlsInHtmlFormat> - Expand words found in a string using the html B<a> tag to supply a definition of that word.

135 L<expandWellKnownWordsAsUrlsInMdFormat|/expandWellKnownWordsAsUrlsInMdFormat> - Expand words found in a string using the md url to supply a definition of that word.

136 L<extractCodeBlock|/extractCodeBlock> - Extract the block of code delimited by B<$comment>, starting at qq($comment-begin), ending at qq($comment-end) from the named B<$file> else the current Perl program $0 and return it as a string or confess if this is not possible.

137 L<extractPythonDocumentationFromFiles|/extractPythonDocumentationFromFiles> - Extract python documentation from the specified files

138 L<extractTest|/extractTest> - Remove example markers from test code.

139 L<fe|/fe> - Get the extension of a file name.

140 L<fff|/fff> - Confess a message with a line position and a file that Geany will jump to if clicked on.

141 L<fileInWindowsFormat|/fileInWindowsFormat> - Convert a unix B<$file> name to windows format

142 L<fileLargestSize|/fileLargestSize> - Return the largest B<$file>.

143 L<fileList|/fileList> - Files that match a given search pattern interpreted by L<perlfunc/bsd_glob>.

144 L<fileMd5Sum|/fileMd5Sum> - Get the Md5 sum of the content of a B<$file>.

145 L<fileModTime|/fileModTime> - Get the modified time of a B<$file> as seconds since the epoch.

146 L<fileOutOfDate|/fileOutOfDate> - Calls the specified sub B<$make> for each source file that is missing and then again against the B<$target> file if any of the B<@source> files were missing or the $target file is older than any of the @source files or if the target does not exist.

147 L<filePath|/filePath> - Create a file name from a list of  names.

148 L<filePathDir|/filePathDir> - Create a folder name from a list of  names.

149 L<filePathExt|/filePathExt> - Create a file name from a list of  names the last of which is assumed to be the extension of the file name.

150 L<filePathSeparatorChar|/filePathSeparatorChar> - File path separator

151 L<fileSize|/fileSize> - Get the size of a B<$file> in bytes.

152 L<findAllFilesAndFolders|/findAllFilesAndFolders> - Find all the files and folders under a folder.

153 L<findDirs|/findDirs> - Find all the folders under a B<$folder> and optionally B<$filter> the selected folders with a regular expression.

154 L<findFiles|/findFiles> - Find all the files under a B<$folder> and optionally B<$filter> the selected files with a regular expression.

155 L<findFileWithExtension|/findFileWithExtension> - Find the first file that exists with a path and name of B<$file> and an extension drawn from <@ext>.

156 L<firstFileThatExists|/firstFileThatExists> - Returns the name of the first file from B<@files> that exists or B<undef> if none of the named @files exist.

157 L<firstNChars|/firstNChars> - First N characters of a string.

158 L<flattenArrayAndHashValues|/flattenArrayAndHashValues> - Flatten an array of scalars, array and hash references to make an array of scalars by flattening the array references and hash values.

159 L<fn|/fn> - Remove the path and extension from a file name.

160 L<fne|/fne> - Remove the path from a file name.

161 L<folderSize|/folderSize> - Get the size of a B<$folder> in bytes.

162 L<forEachKeyAndValue|/forEachKeyAndValue> - Iterate over a hash for each key and value

163 L<formatHtmlAndTextTables|/formatHtmlAndTextTables> - Create text and html versions of a tabular report

164 L<formatHtmlAndTextTablesWaitPids|/formatHtmlAndTextTablesWaitPids> - Wait on all table formatting pids to complete

165 L<formatHtmlTable|/formatHtmlTable> - Format an array of arrays of scalars as an html table using the  B<%options> described in L<formatTableCheckKeys>.

166 L<formatHtmlTablesIndex|/formatHtmlTablesIndex> - Create an index of html reports.

167 L<formatSourcePodAsHtml|/formatSourcePodAsHtml> - Format the L<POD|https://perldoc.perl.org/perlpod.html> in the current source file as L<HTML|https://en.wikipedia.org/wiki/HTML>.

168 L<formatString|/formatString> - Format the specified B<$string> so it can be displayed in B<$width> columns.

169 L<formatTable|/formatTable> - Format various B<$data> structures as a table with titles as specified by B<$columnTitles>: either a reference to an array of column titles or a string each line of which contains the column title as the first word with the rest of the line describing that column.

170 L<formatTableA|/formatTableA> - Tabularize an array.

171 L<formatTableAA|/formatTableAA> - Tabularize an array of arrays.

172 L<formatTableAH|/formatTableAH> - Tabularize an array of hashes.

173 L<formatTableBasic|/formatTableBasic> - Tabularize an array of arrays of text.

174 L<formatTableCheckKeys|/formatTableCheckKeys> - Options available for formatting tables

175 L<formatTableClearUpLeft|/formatTableClearUpLeft> - Blank identical column values up and left

176 L<formatTableH|/formatTableH> - Tabularize a hash.

177 L<formatTableHA|/formatTableHA> - Tabularize a hash of arrays.

178 L<formatTableHH|/formatTableHH> - Tabularize a hash of hashes.

179 L<formatTableMultiLine|/formatTableMultiLine> - Tabularize text that has new lines in it.

180 L<formattedTablesReport|/formattedTablesReport> - Report of all the reports created.

181 L<fp|/fp> - Get the path from a file name.

182 L<fpn|/fpn> - Remove the extension from a file name.

183 L<fullFileName|/fullFileName> - Full name of a file.

184 L<fullyQualifiedFile|/fullyQualifiedFile> - Check whether a B<$file> name is fully qualified or not and, optionally, whether it is fully qualified with a specified B<$prefix> or not.

185 L<fullyQualifyFile|/fullyQualifyFile> - Return the fully qualified name of a file.

186 L<genHash|/genHash> - Return a B<$bless>ed hash with the specified B<$attributes> accessible via L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> method calls.

187 L<genLValueArrayMethods|/genLValueArrayMethods> - Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> array methods in the current package.

188 L<genLValueHashMethods|/genLValueHashMethods> - Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> hash methods in the current package.

189 L<genLValueScalarMethods|/genLValueScalarMethods> - Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> scalar methods in the current package, A method whose value has not yet been set will return a new scalar with value B<undef>.

190 L<genLValueScalarMethodsWithDefaultValues|/genLValueScalarMethodsWithDefaultValues> - Generate L<lvalue method|http://perldoc.perl.org/perlsub.html#Lvalue-subroutines> scalar methods with default values in the current package.

191 L<getCodeContext|/getCodeContext> - Recreate the code context for a referenced sub

192 L<getNumberOfCpus|/getNumberOfCpus> - Number of cpus

193 L<getSubName|/getSubName> - Returns the (package, name, file, line) of a perl B<$sub> reference.

194 L<guidFromMd5|/guidFromMd5> - Create a guid from an md5 hash.

195 L<guidFromString|/guidFromString> - Create a guid representation of the L<MD5 sum|https://en.wikipedia.org/wiki/MD5> of the content of a string.

196 L<hashifyFolderStructure|/hashifyFolderStructure> - Hashify a list of file names to get the corresponding folder structure.

197 L<hexToAsciiString|/hexToAsciiString> - Decode a string of L<hexadecimal|https://en.wikipedia.org/wiki/Hexadecimal> digits as an L<Ascii|https://en.wikipedia.org/wiki/ASCII> string.

198 L<hostName|/hostName> - The name of the host we are running on.

199 L<htmlToc|/htmlToc> - Generate a table of contents for some html.

200 L<imageSize|/imageSize> - Return (width, height) of an B<$image>.

201 L<indentString|/indentString> - Indent lines contained in a string or formatted table by the specified string.

202 L<indexOfMax|/indexOfMax> - Find the index of the maximum number in a list of numbers confessing to any ill defined values.

203 L<indexOfMin|/indexOfMin> - Find the index of the minimum number in a list of numbers confessing to any ill defined values.

204 L<intersectionOfHashesAsArrays|/intersectionOfHashesAsArrays> - Form the intersection of the specified hashes B<@h> as one hash whose values are an array of corresponding values from each hash

205 L<intersectionOfHashKeys|/intersectionOfHashKeys> - Form the intersection of the keys of the specified hashes B<@h> as one hash whose keys represent the intersection.

206 L<invertHashOfHashes|/invertHashOfHashes> - Invert a hash of hashes: given {a}{b} = c return {b}{c} = c

207 L<ipAddressOfHost|/ipAddressOfHost> - Get the first ip address of the specified host via Domain Name Services

208 L<ipAddressViaArp|/ipAddressViaArp> - Get the ip address of a server on the local network by hostname via arp

209 L<isBlank|/isBlank> - Test whether a string is blank.

210 L<isFileUtf8|/isFileUtf8> - Return the file name quoted if its contents are in utf8 else return undef

211 L<isSubInPackage|/isSubInPackage> - Test whether the specified B<$package> contains the subroutine <$sub>.

212 L<javaPackage|/javaPackage> - Extract the package name from a java string or file.

213 L<javaPackageAsFileName|/javaPackageAsFileName> - Extract the package name from a java string or file and convert it to a file name.

214 L<javaScriptExports|/javaScriptExports> - Extract the Javascript functions marked for export in a file or string.

215 L<keyCount|/keyCount> - Count keys down to the specified level.

216 L<lengthOfLongestSubArray|/lengthOfLongestSubArray> - Given an array of arrays find the length of the longest sub array.

217 L<lll|/lll> - Log messages with a time stamp and originating file and line number.

218 L<loadArrayArrayFromLines|/loadArrayArrayFromLines> - Load an array of arrays from lines of text: each line is an array of words.

219 L<loadArrayFromLines|/loadArrayFromLines> - Load an array from lines of text in a string.

220 L<loadArrayHashFromLines|/loadArrayHashFromLines> - Load an array of hashes from lines of text: each line is a hash of words.

221 L<loadHash|/loadHash> - Load the specified blessed B<$hash> generated with L<genHash|/genHash> with B<%attributes>.

222 L<loadHashArrayFromLines|/loadHashArrayFromLines> - Load a hash of arrays from lines of text: the first word of each line is the key, the remaining words are the array contents.

223 L<loadHashFromLines|/loadHashFromLines> - Load a hash: first word of each line is the key and the rest is the value.

224 L<loadHashHashFromLines|/loadHashHashFromLines> - Load a hash of hashes from lines of text: the first word of each line is the key, the remaining words are the sub hash contents.

225 L<lpad|/lpad> - Left Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.

226 L<makeDieConfess|/makeDieConfess> - Force die to confess where the death occurred

227 L<makePath|/makePath> - Make the path for the specified file name or folder on the local machine.

228 L<makePathRemote|/makePathRemote> - Make the path for the specified B<$file> or folder on the L<Amazon Web Services|http://aws.amazon.com> instance whose ip address is specified by B<$ip> or returned by L<awsIp>.

229 L<matchPath|/matchPath> - Return the deepest folder that exists along a given file name path.

230 L<mathematicalBoldItalicString|/mathematicalBoldItalicString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold Italic.

231 L<mathematicalBoldItalicStringUndo|/mathematicalBoldItalicStringUndo> - Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold Italic.

232 L<mathematicalBoldString|/mathematicalBoldString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold.

233 L<mathematicalBoldStringUndo|/mathematicalBoldStringUndo> - Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Bold.

234 L<mathematicalItalicString|/mathematicalItalicString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Italic.

235 L<mathematicalMonoSpaceString|/mathematicalMonoSpaceString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical MonoSpace.

236 L<mathematicalMonoSpaceStringUndo|/mathematicalMonoSpaceStringUndo> - Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical MonoSpace.

237 L<mathematicalSansSerifBoldItalicString|/mathematicalSansSerifBoldItalicString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold Italic.

238 L<mathematicalSansSerifBoldItalicStringUndo|/mathematicalSansSerifBoldItalicStringUndo> - Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold Italic.

239 L<mathematicalSansSerifBoldString|/mathematicalSansSerifBoldString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold.

240 L<mathematicalSansSerifBoldStringUndo|/mathematicalSansSerifBoldStringUndo> - Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Bold.

241 L<mathematicalSansSerifItalicString|/mathematicalSansSerifItalicString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Italic.

242 L<mathematicalSansSerifItalicStringUndo|/mathematicalSansSerifItalicStringUndo> - Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif Italic.

243 L<mathematicalSansSerifString|/mathematicalSansSerifString> - Convert alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif.

244 L<mathematicalSansSerifStringUndo|/mathematicalSansSerifStringUndo> - Undo alphanumerics in a string to L<Unicode|https://en.wikipedia.org/wiki/Unicode> Mathematical Sans Serif.

245 L<max|/max> - Find the maximum number in a list of numbers confessing to any ill defined values.

246 L<maximumLineLength|/maximumLineLength> - Find the longest line in a B<$string>.

247 L<md5FromGuid|/md5FromGuid> - Recover an md5 sum from a guid.

248 L<mergeFolder|/mergeFolder> - Copy the B<$source> folder into the B<$target> folder retaining any existing files not replaced by copied files.

249 L<mergeFolderFromRemote|/mergeFolderFromRemote> - Merge the specified B<$Source> folder from the corresponding remote folder on the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

250 L<mergeHashesBySummingValues|/mergeHashesBySummingValues> - Merge a list of hashes B<@h> by summing their values

251 L<microSecondsSinceEpoch|/microSecondsSinceEpoch> - Micro seconds since unix epoch.

252 L<min|/min> - Find the minimum number in a list of numbers confessing to any ill defined values.

253 L<mmm|/mmm> - Log messages with a differential time in milliseconds and originating file and line number.

254 L<moveFileNoClobber|/moveFileNoClobber> - Rename the B<$source> file, which must exist, to the B<$target> file but only if the $target file does not exist already.

255 L<moveFileWithClobber|/moveFileWithClobber> - Rename the B<$source> file, which must exist, to the B<$target> file but only if the $target file does not exist already.

256 L<nameFromFolder|/nameFromFolder> - Create a name from the last folder in the path of a file name.

257 L<nameFromString|/nameFromString> - Create a readable name from an arbitrary string of text.

258 L<nameFromStringRestrictedToTitle|/nameFromStringRestrictedToTitle> - Create a readable name from a string of text that might contain a title tag - fall back to L<nameFromString|/nameFromString> if that is not possible.

259 L<newProcessStarter|/newProcessStarter> - Create a new L<process starter|/Data::Table::Text::Starter Definition> with which to start parallel processes up to a specified B<$maximumNumberOfProcesses> maximum number of parallel processes at a time, wait for all the started processes to finish and then optionally retrieve their saved results as an array from the folder named by B<$transferArea>.

260 L<newServiceIncarnation|/newServiceIncarnation> - Create a new service incarnation to record the start up of a new instance of a service and return the description as a L<Data::Exchange::Service Definition hash|/Data::Exchange::Service Definition>.

261 L<newUdsr|/newUdsr> - Create a communicator - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.

262 L<newUdsrClient|/newUdsrClient> - Create a new communications client - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.

263 L<newUdsrServer|/newUdsrServer> - Create a communications server - a means to communicate between processes on the same machine via L<Udsr::read|/Udsr::read> and L<Udsr::write|/Udsr::write>.

264 L<numberOfCpus|/numberOfCpus> - Number of cpus scaled by an optional factor - but only if you have nproc.

265 L<numberOfLinesInFile|/numberOfLinesInFile> - Return the number of lines in a file.

266 L<numberOfLinesInString|/numberOfLinesInString> - The number of lines in a string.

267 L<nws|/nws> - Normalize white space in a string to make comparisons easier.

268 L<onAws|/onAws> - Returns 1 if we are on AWS else return 0.

269 L<onAwsPrimary|/onAwsPrimary> - Return 1 if we are on L<Amazon Web Services|http://aws.amazon.com> and we are on the primary session instance as defined by L<awsParallelPrimaryInstanceId>, return 0 if we are on a secondary session instance, else return B<undef> if we are not on L<Amazon Web Services|http://aws.amazon.com>.

270 L<onAwsSecondary|/onAwsSecondary> - Return 1 if we are on L<Amazon Web Services|http://aws.amazon.com> but we are not on the primary session instance as defined by L<awsParallelPrimaryInstanceId>, return 0 if we are on the primary session instance, else return B<undef> if we are not on L<Amazon Web Services|http://aws.amazon.com>.

271 L<onWindows|/onWindows> - Are we on windows

272 L<overrideAndReabsorbMethods|/overrideAndReabsorbMethods> - Override methods down the list of B<@packages> then reabsorb any unused methods back up the list of packages so that all the packages have the same methods as the last package with methods from packages mentioned earlier overriding methods from packages mentioned later.

273 L<overrideMethods|/overrideMethods> - For each method, if it exists in package B<$from> then export it to package B<$to> replacing any existing method in B<$to>, otherwise export the method from package B<$to> to package B<$from> in order to merge the behavior of the B<$from> and B<$to> packages with respect to the named methods with duplicates resolved if favour of package B<$from>.

274 L<overWriteBinaryFile|/overWriteBinaryFile> - Write to B<$file>, after creating a path to the file with L<makePath> if necessary, the binary content in B<$string>.

275 L<overWriteFile|/overWriteFile> - Write to a B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

276 L<overWriteHtmlFile|/overWriteHtmlFile> - Write an L<HTML|https://en.wikipedia.org/wiki/HTML> file to /var/www/html and make it readable

277 L<overWritePerlCgiFile|/overWritePerlCgiFile> - Write a L<Perl|http://www.perl.org/> file to /usr/lib/cgi-bin and make it executable after checking it for syntax errors

278 L<packBySize|/packBySize> - Given B<$N> buckets and a list B<@sizes> of ([size of file, name of file].

279 L<pad|/pad> - Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.

280 L<parseCommandLineArguments|/parseCommandLineArguments> - Call the specified B<$sub> after classifying the specified array of [arguments] in B<$args> into positional and keyword parameters.

281 L<parseDitaRef|/parseDitaRef> - Parse a dita reference B<$ref> into its components (file name, topic id, id) .

282 L<parseFileName|/parseFileName> - Parse a file name into (path, name, extension) considering .

283 L<parseIntoWordsAndStrings|/parseIntoWordsAndStrings> - Parse a B<$string> into words and quoted strings.

284 L<parseS3BucketAndFolderName|/parseS3BucketAndFolderName> - Parse an L<S3|https://aws.amazon.com/s3/> bucket/folder name into a bucket and a folder name removing any initial s3://.

285 L<parseXmlDocType|/parseXmlDocType> - Parse an L<Xml|https://en.wikipedia.org/wiki/XML> DOCTYPE and return a hash indicating its components

286 L<partitionStringsOnPrefixBySize|/partitionStringsOnPrefixBySize> - Partition a hash of strings and associated sizes into partitions with either a maximum size B<$maxSize> or only one element; the hash B<%Sizes> consisting of a mapping {string=>size}; with each partition being named with the shortest string prefix that identifies just the strings in that partition.

287 L<perlPackage|/perlPackage> - Extract the package name from a perl string or file.

288 L<powerOfTwo|/powerOfTwo> - Test whether a number B<$n> is a power of two, return the power if it is else B<undef>.

289 L<ppp|/ppp> - Pad the specified B<$string> to a multiple of the specified B<$length>  with blanks or the specified padding character to a multiple of a specified length.

290 L<prefferedFileName|/prefferedFileName> - Normalize a file name

291 L<printPerlDataAsXml|/printPerlDataAsXml> - Print a Perl data structure as xml

292 L<printQw|/printQw> - Print an array of words in qw() format.

293 L<processFilesInParallel|/processFilesInParallel> - Process files in parallel using (8 * the number of CPUs) processes with the process each file is assigned to depending on the size of the file so that each process is loaded with approximately the same number of bytes of data in total from the files it processes.

294 L<processJavaFilesInParallel|/processJavaFilesInParallel> - Process java files of known size in parallel using (the number of CPUs) processes with the process each item is assigned to depending on the size of the java item so that each process is loaded with approximately the same number of bytes of data in total from the java files it processes.

295 L<processSizesInParallel|/processSizesInParallel> - Process items of known size in parallel using (8 * the number of CPUs) processes with the process each item is assigned to depending on the size of the item so that each process is loaded with approximately the same number of bytes of data in total from the items it processes.

296 L<processSizesInParallelN|/processSizesInParallelN> - Process items of known size in parallel using the specified number B<$N> processes with the process each file is assigned to depending on the size of the file so that each process is loaded with approximately the same number of bytes of data in total from the files it processes.

297 L<quoteFile|/quoteFile> - Quote a file name.

298 L<randomizeArray|/randomizeArray> - Randomize an array

299 L<readBinaryFile|/readBinaryFile> - Read a binary file on the local machine.

300 L<readFile|/readFile> - Return the content of a file residing on the local machine interpreting the content of the file as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

301 L<readFileFromRemote|/readFileFromRemote> - Copy and read a B<$file> from the remote machine whose ip address is specified by B<$ip> or returned by L<awsIp> and return the content of $file interpreted as utf8 .

302 L<readFiles|/readFiles> - Read all the files in the specified list of folders into a hash.

303 L<readGZipFile|/readGZipFile> - Read the specified file containing compressed L<Unicode|https://en.wikipedia.org/wiki/Unicode> content represented as L<utf8|https://en.wikipedia.org/wiki/UTF-8> through L<gzip|https://en.wikipedia.org/wiki/Gzip>.

304 L<readStdIn|/readStdIn> - Return the contents of STDIN and return the results as either an array or a string.

305 L<readUtf16File|/readUtf16File> - Read a file containing L<Unicode|https://en.wikipedia.org/wiki/Unicode> encoded in utf-16.

306 L<rectangularArray|/rectangularArray> - Create a two dimensional rectangular array whose first dimension is B<$first> from a one dimensional linear array.

307 L<rectangularArray2|/rectangularArray2> - Create a two dimensional rectangular array whose second dimension is B<$second> from a one dimensional linear array.

308 L<reinstateWellKnown|/reinstateWellKnown> - Contract references to well known Urls to their abbreviated form

309 L<relFromAbsAgainstAbs|/relFromAbsAgainstAbs> - Relative file from one absolute file B<$a> against another B<$b>.

310 L<reloadHashes|/reloadHashes> - Ensures that all the hashes within a tower of data structures have LValue methods to get and set their current keys.

311 L<reloadHashes2|/reloadHashes2> - Ensures that all the hashes within a tower of data structures have LValue methods to get and set their current keys.

312 L<removeDuplicatePrefixes|/removeDuplicatePrefixes> - Remove duplicated leading directory names from a file name.

313 L<removeFilePathsFromStructure|/removeFilePathsFromStructure> - Remove all file paths from a specified B<$structure> to make said $structure testable with L<Test::More/is_deeply>.

314 L<removeFilePrefix|/removeFilePrefix> - Removes a file B<$prefix> from an array of B<@files>.

315 L<renormalizeFolderName|/renormalizeFolderName> - Normalize a folder name by ensuring it has a single trailing directory separator.

316 L<replaceStringWithString|/replaceStringWithString> - Replace all instances in B<$string> of B<$source> with B<$target>

317 L<reportAttributes|/reportAttributes> - Report the attributes present in a B<$sourceFile>

318 L<reportAttributeSettings|/reportAttributeSettings> - Report the current values of the attribute methods in the calling file and optionally write the report to B<$reportFile>.

319 L<reportExportableMethods|/reportExportableMethods> - Report the exportable methods marked with #e in a B<$sourceFile>

320 L<reportReplacableMethods|/reportReplacableMethods> - Report the replaceable methods marked with #r in a B<$sourceFile>

321 L<reportSettings|/reportSettings> - Report the current values of parameterless subs.

322 L<retrieveFile|/retrieveFile> - Retrieve a B<$file> created via L<Storable|https://metacpan.org/pod/Storable>.

323 L<runInParallel|/runInParallel> - Process the elements of an array in parallel using a maximum of B<$maximumNumberOfProcesses> processes.

324 L<runInSquareRootParallel|/runInSquareRootParallel> - Process the elements of an array in square root parallel using a maximum of B<$maximumNumberOfProcesses> processes.

325 L<s3Delete|/s3Delete> - Return an S3 --delete keyword from an S3 option set

326 L<s3DownloadFolder|/s3DownloadFolder> - Download a specified B<$folder> on S3 to a B<$local> folder using the specified B<%options> if any.

327 L<s3FileExists|/s3FileExists> - Return (name, size, date, time) for a B<$file> that exists on S3 else () using the specified B<%options> if any.

328 L<s3ListFilesAndSizes|/s3ListFilesAndSizes> - Return {file=>size} for all the files in a specified B<$folderOrFile> on S3 using the specified B<%options> if any.

329 L<s3Profile|/s3Profile> - Return an S3 profile keyword from an S3 option set

330 L<s3ReadFile|/s3ReadFile> - Read from a B<$file> on S3 and write the contents to a local file B<$local> using the specified B<%options> if any.

331 L<s3ReadString|/s3ReadString> - Read from a B<$file> on S3 and return the contents as a string using specified B<%options> if any.

332 L<s3WriteFile|/s3WriteFile> - Write to a file B<$fileS3> on S3 the contents of a local file B<$fileLocal> using the specified B<%options> if any.

333 L<s3WriteString|/s3WriteString> - Write to a B<$file> on S3 the contents of B<$string> using the specified B<%options> if any.

334 L<s3ZipFolder|/s3ZipFolder> - Zip the specified B<$source> folder and write it to the named B<$target> file on S3.

335 L<s3ZipFolders|/s3ZipFolders> - Zip local folders and upload them to S3 in parallel.

336 L<saveAwsDomain|/saveAwsDomain> - Make the server at L<Amazon Web Services|http://aws.amazon.com> with the given domain name the default primary server as used by all the methods whose names end in B<r> or B<Remote>.

337 L<saveAwsIp|/saveAwsIp> - Make the server at L<Amazon Web Services|http://aws.amazon.com> with the given IP address the default primary server as used by all the methods whose names end in B<r> or B<Remote>.

338 L<saveCodeToS3|/saveCodeToS3> - Save source code every B<$saveCodeEvery> seconds by zipping folder B<$folder> to zip file B<$zipFileName> then saving this zip file in the specified L<S3|https://aws.amazon.com/s3/> B<$bucket> using any additional L<S3|https://aws.amazon.com/s3/> parameters in B<$S3Parms>.

339 L<saveSourceToS3|/saveSourceToS3> - Save source code.

340 L<searchDirectoryTreeForSubFolders|/searchDirectoryTreeForSubFolders> - Search the specified directory under the specified folder for sub folders

341 L<searchDirectoryTreesForMatchingFiles|/searchDirectoryTreesForMatchingFiles> - Search the specified directory trees for the files (not folders) that match the specified extensions.

342 L<setCombination|/setCombination> - Count the elements in sets B<@s> represented as arrays of strings and/or the keys of hashes

343 L<setFileExtension|/setFileExtension> - Given a B<$file>, change its extension to B<$extension>.

344 L<setIntersection|/setIntersection> - Intersection of sets B<@s> represented as arrays of strings and/or the keys of hashes

345 L<setIntersectionOverUnion|/setIntersectionOverUnion> - Returns the size of the intersection over the size of the union of one or more sets B<@s> represented as arrays and/or hashes

346 L<setPackageSearchOrder|/setPackageSearchOrder> - Set a package search order for methods requested in the current package via AUTOLOAD.

347 L<setPartitionOnIntersectionOverUnion|/setPartitionOnIntersectionOverUnion> - Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@sets> so that within each partition the L<setIntersectionOverUnion|/setIntersectionOverUnion> of any two sets in the partition is never less than the specified level of I<$confidence**2>

348 L<setPartitionOnIntersectionOverUnionOfHashStringSets|/setPartitionOnIntersectionOverUnionOfHashStringSets> - Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<$hashSet> represented by a hash, each hash value being a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified B<$confidence**2> and the partition entries are the hash keys of the string sets.

349 L<setPartitionOnIntersectionOverUnionOfHashStringSetsInParallel|/setPartitionOnIntersectionOverUnionOfHashStringSetsInParallel> - Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<$hashSet> represented by a hash, each hash value being a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified B<$confidence**2> and the partition entries are the hash keys of the string sets.

350 L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> - Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@sets> of words so that within each partition the L<setIntersectionOverUnion|/setIntersectionOverUnion> of any two sets of words in the partition is never less than the specified I<$confidence**2>

351 L<setPartitionOnIntersectionOverUnionOfStringSets|/setPartitionOnIntersectionOverUnionOfStringSets> - Partition, at a level of B<$confidence> between 0 and 1, a set of sets B<@strings>, each set represented by a string containing words and punctuation, each word possibly capitalized, so that within each partition the L<setPartitionOnIntersectionOverUnionOfSetsOfWords|/setPartitionOnIntersectionOverUnionOfSetsOfWords> of any two sets of words in the partition is never less than the specified I<$confidence**2>

352 L<setPermissionsForFile|/setPermissionsForFile> - Apply L<chmod|https://linux.die.net/man/1/chmod> to a B<$file> to set its B<$permissions>.

353 L<setUnion|/setUnion> - Union of sets B<@s> represented as arrays of strings and/or the keys of hashes

354 L<showGotVersusWanted|/showGotVersusWanted> - Show the difference between the wanted string and the wanted string

355 L<showHashes|/showHashes> - Create a map of all the keys within all the hashes within a tower of data structures.

356 L<showHashes2|/showHashes2> - Create a map of all the keys within all the hashes within a tower of data structures.

357 L<squareArray|/squareArray> - Create a two dimensional square array from a one dimensional linear array.

358 L<startProcess|/startProcess> - Start new processes while the number of child processes recorded in B<%$pids> is less than the specified B<$maximum>.

359 L<storeFile|/storeFile> - Store into a B<$file>, after creating a path to the file with L<makePath> if necessary, a data B<$structure> via L<Storable|https://metacpan.org/pod/Storable>.

360 L<stringMd5Sum|/stringMd5Sum> - Get the Md5 sum of a B<$string> that might contain L<utf8|https://en.wikipedia.org/wiki/UTF-8> code points.

361 L<stringsAreNotEqual|/stringsAreNotEqual> - Return the common start followed by the two non equal tails of two non equal strings or an empty list if the strings are equal.

362 L<subScriptString|/subScriptString> - Convert alphanumerics in a string to sub scripts

363 L<subScriptStringUndo|/subScriptStringUndo> - Undo alphanumerics in a string to sub scripts

364 L<sumAbsAndRel|/sumAbsAndRel> - Combine zero or more absolute and relative names of B<@files> starting at the current working folder to get an absolute file name.

365 L<summarizeColumn|/summarizeColumn> - Count the number of unique instances of each value a column in a table assumes.

366 L<superScriptString|/superScriptString> - Convert alphanumerics in a string to super scripts

367 L<superScriptStringUndo|/superScriptStringUndo> - Undo alphanumerics in a string to super scripts

368 L<swapFilePrefix|/swapFilePrefix> - Swaps the start of a B<$file> name from a B<$known> name to a B<$new> one if the file does in fact start with the $known name otherwise returns the original file name as it is.

369 L<swapFolderPrefix|/swapFolderPrefix> - Given a B<$file>, swap the folder name of the $file from B<$known> to B<$new> if the file $file starts with the $known folder name else return the $file as it is.

370 L<syncFromS3InParallel|/syncFromS3InParallel> - Download from L<S3|https://aws.amazon.com/s3/> by using "aws s3 sync --exclude '*' --include '.

371 L<syncToS3InParallel|/syncToS3InParallel> - Upload to L<S3|https://aws.amazon.com/s3/> by using "aws s3 sync --exclude '*' --include '.

372 L<temporaryFile|/temporaryFile> - Create a new, empty, temporary file.

373 L<temporaryFolder|/temporaryFolder> - Create a new, empty, temporary folder.

374 L<timeStamp|/timeStamp> - hours:minute:seconds

375 L<transitiveClosure|/transitiveClosure> - Transitive closure of a hash of hashes

376 L<trim|/trim> - Remove any white space from the front and end of a string.

377 L<Udsr::kill|/Udsr::kill> - Kill a communications server.

378 L<Udsr::read|/Udsr::read> - Read a message from the L<newUdsrServer|/newUdsrServer> or the L<newUdsrClient|/newUdsrClient>.

379 L<Udsr::webUser|/Udsr::webUser> - Create a systemd installed server that processes http requests using a specified userid.

380 L<Udsr::write|/Udsr::write> - Write a communications message to the L<newUdsrServer|/newUdsrServer> or the L<newUdsrClient|/newUdsrClient>.

381 L<unbless|/unbless> - Remove the effects of bless from a L<Perl|http://www.perl.org/> data B<$structure> enabling it to be converted to L<Json|https://en.wikipedia.org/wiki/JSON> or compared with L<Test::More::is_deeply>.

382 L<unionOfHashesAsArrays|/unionOfHashesAsArrays> - Form the union of the specified hashes B<@h> as one hash whose values are a array of corresponding values from each hash

383 L<unionOfHashKeys|/unionOfHashKeys> - Form the union of the keys of the specified hashes B<@h> as one hash whose keys represent the union.

384 L<uniqueNameFromFile|/uniqueNameFromFile> - Create a unique name from a file name and the md5 sum of its content

385 L<updateDocumentation|/updateDocumentation> - Update the documentation for a Perl module from the comments in its source code.

386 L<updatePerlModuleDocumentation|/updatePerlModuleDocumentation> - Update the documentation in a B<$perlModule> and display said documentation in a web browser.

387 L<userId|/userId> - Get or confirm the userid we are currently running under.

388 L<versionCode|/versionCode> - YYYYmmdd-HHMMSS

389 L<versionCodeDashed|/versionCodeDashed> - YYYY-mm-dd-HH:MM:SS

390 L<waitForAllStartedProcessesToFinish|/waitForAllStartedProcessesToFinish> - Wait until all the processes started by L<startProcess|/startProcess> have finished.

391 L<wellKnownUrls|/wellKnownUrls> - Short names for some well known urls

392 L<writeBinaryFile|/writeBinaryFile> - Write to a new B<$file>, after creating a path to the file with L<makePath> if necessary, the binary content in B<$string>.

393 L<writeFile|/writeFile> - Write to a new B<$file>, after creating a path to the $file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

394 L<writeFiles|/writeFiles> - Write the values of a B<$hash> reference into files identified by the key of each value using L<overWriteFile|/overWriteFile> optionally swapping the prefix of each file from B<$old> to B<$new>.

395 L<writeFileToRemote|/writeFileToRemote> - Write to a new B<$file>, after creating a path to the file with L<makePath> if necessary, a B<$string> of L<Unicode|https://en.wikipedia.org/wiki/Unicode> content encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8> then copy the $file to the remote server whose ip address is specified by B<$ip> or returned by L<awsIp>.

396 L<writeGZipFile|/writeGZipFile> - Write to a B<$file>, after creating a path to the file with L<makePath> if necessary, through L<gzip|https://en.wikipedia.org/wiki/Gzip> a B<$string> whose content is encoded as L<utf8|https://en.wikipedia.org/wiki/UTF-8>.

397 L<writeStructureTest|/writeStructureTest> - Write a test for a data B<$structure> with file names in it.

398 L<writeTempFile|/writeTempFile> - Write an array of strings as lines to a temporary file and return the file name.

399 L<wwwDecode|/wwwDecode> - Percent decode a L<url|https://en.wikipedia.org/wiki/URL> B<$string> per: https://en.

400 L<wwwEncode|/wwwEncode> - Percent encode a L<url|https://en.wikipedia.org/wiki/URL> per: https://en.

401 L<wwwGitHubAuth|/wwwGitHubAuth> - Logon as a L<GitHub|https://github.com/philiprbrenan> L<Oauth|https://en.wikipedia.org/wiki/OAuth> app per: L<https://github.

402 L<xxx|/xxx> - Execute a shell command optionally checking its response.

403 L<xxxr|/xxxr> - Execute a command B<$cmd> via bash on the server whose ip address is specified by B<$ip> or returned by L<awsIp>.

404 L<yyy|/yyy> - Execute a block of shell commands line by line after removing comments - stop if there is a non zero return code from any command.

405 L<zzz|/zzz> - Execute lines of commands after replacing new lines with && then check that the pipeline execution results in a return code of zero and that the execution results match the optional regular expression if one has been supplied; confess() to an error if either check fails.

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Data::Table::Text

=head1 Author

L<philiprbrenan@gmail.com|mailto:philiprbrenan@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

=head1 Copyright

Copyright (c) 2016-2021 Philip R Brenan.

This module is free software. It may be used, redistributed and/or modified
under the same terms as Perl itself.


=head1 Acknowledgements

Thanks to the following people for their help with this module:

=over


=item L<mim@cpan.org|mailto:mim@cpan.org>

Testing on windows


=back


=cut



# Tests and documentation

sub test
 {my $p = __PACKAGE__;
  binmode($_, ":utf8") for *STDOUT, *STDERR;
  return if eval "eof(${p}::DATA)";
  my $s = eval "join('', <${p}::DATA>)";
  $@ and die $@;
  eval $s;
  $@ and die $@;
  1
 }

test unless caller;

1;
# podDocumentation
__DATA__
use Test::More;

my $localTest = ((caller(1))[0]//'Data::Table::Text') eq "Data::Table::Text";   # Local testing mode

Test::More->builder->output("/dev/null") if $localTest;                         # Reduce number of confirmation messages during testing

if ($^O =~ m(bsd|linux)i) {plan tests    => 679}                                # Supported systems
#lsif (onWindows)         {plan tests    => 621}                                # Somewhat supported systems
else
 {plan skip_all =>qq(Not supported on: $^O);
 }

my $timeStart = time;

#goto latest;

if (1)                                                                          # Unicode to local file
 {my $z = "𝝰 𝝱 𝝲";
  my $t = temporaryFolder;
  my $f = filePathExt($t, $z, qq(data));
  unlink $f if -e $f;
  ok !-e $f;
  writeFile($f, $z);
  ok  -e $f;
  my $s = readFile($f);
  ok $s eq $z;
  ok length($s) == length($z);
  unlink $f;
  ok !-e $f;
  rmdir $t;
  ok !-d $t;
 }

if (1) {                                                                        # Key counts
  my $a = [[1..3],       {map{$_=>1} 1..3}];                                    #TkeyCount
  my $h = {a=>[1..3], b=>{map{$_=>1} 1..3}};                                    #TkeyCount
  ok keyCount(2, $a) == 6;                                                      #TkeyCount
  ok keyCount(2, $h) == 6;                                                      #TkeyCount
 }

if (1) {                                                                        #TfilePath #TfilePathDir #TfilePathExt #Tfpd #Tfpe #Tfpf
  is_deeply filePath   (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";
  is_deeply filePathDir(qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";
  is_deeply filePathDir('', qw(aaa))              , prefferedFileName "aaa/";
  is_deeply filePathDir('')                       , prefferedFileName "";
  is_deeply filePathExt(qw(aaa xxx))              , prefferedFileName "aaa.xxx";
  is_deeply filePathExt(qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";

  is_deeply fpd        (qw(/aaa bbb ccc ddd))     , prefferedFileName "/aaa/bbb/ccc/ddd/";
  is_deeply fpf        (qw(/aaa bbb ccc ddd.eee)) , prefferedFileName "/aaa/bbb/ccc/ddd.eee";
  is_deeply fpe        (qw(aaa bbb xxx))          , prefferedFileName "aaa/bbb.xxx";
 }

if (1)                                                                          #TparseFileName
 {is_deeply [parseFileName "/home/phil/test.data"], ["/home/phil/", "test", "data"];
  is_deeply [parseFileName "/home/phil/test"],      ["/home/phil/", "test"];
  is_deeply [parseFileName "phil/test.data"],       ["phil/",       "test", "data"];
  is_deeply [parseFileName "phil/test"],            ["phil/",       "test"];
  is_deeply [parseFileName "test.data"],            [undef,         "test", "data"];
  is_deeply [parseFileName "phil/"],                [qw(phil/)];
  is_deeply [parseFileName "/phil"],                [qw(/ phil)];
  is_deeply [parseFileName "/"],                    [qw(/)];
  is_deeply [parseFileName "/var/www/html/translations/"], [qw(/var/www/html/translations/)];
  is_deeply [parseFileName "a.b/c.d.e"],            [qw(a.b/ c.d e)];
  is_deeply [parseFileName "./a.b"],                [qw(./ a b)];
  is_deeply [parseFileName "./../../a.b"],          [qw(./../../ a b)];
 }

if (!onWindows) {

if (1)                                                                          # Unicode
 {use utf8;
  my $z = "𝝰 𝝱 𝝲";
  my $T = temporaryFolder;
  my $t = filePath($T, $z);
  my $f = filePathExt($t, $z, qq(data));
  unlink $f if -e $f;
  ok !-e $f;
  writeFile($f, $z);
  ok  -e $f;
  my $s = readFile($f);
  ok $s eq $z;
  ok length($s) == length($z);

  my @f = findFiles($T);
  ok $f[0] eq $f;

  unlink $f;
  ok !-e $f;
  rmdir $t;
  ok !-d $t;
  rmdir $T;
  ok !-d $T;
 }

if (1)                                                                          # Binary
 {my $z = "𝝰 𝝱 𝝲";
  my $Z = join '', map {chr($_)} 0..11;
  my $T = temporaryFolder;
  my $t = filePath($T, $z);
  my $f = filePathExt($t, $z, qq(data));
  unlink $f if -e $f;
  ok !-e $f;
  writeBinaryFile($f, $Z);
  ok  -e $f;
  my $s = readBinaryFile($f);
  ok $s eq $Z;
  ok length($s) == 12;
  unlink $f;
  ok !-e $f;
  rmdir $t;
  ok !-d $t;
  rmdir $T;
  ok !-d $T;
 }
}

if (1) {                                                                        # Check files
  my $d = filePath   (my @d = qw(a b c d));                                     #TcheckFile #TmatchPath
  my $f = filePathExt(qw(a b c d e x));                                         #TcheckFile
  my $F = filePathExt(qw(a b c e d));                                           #TcheckFile
  createEmptyFile($f);                                                          #TcheckFile
  ok matchPath($d) eq $d;                                                       #TmatchPath
  ok  eval{checkFile($d)};                                                      #TcheckFile
  ok  eval{checkFile($f)};                                                      #TcheckFile
  ok !eval {checkFile($F)};
  my @m = split m/\n/, $@;
  ok $m[1] eq prefferedFileName "a/b/c/";
  unlink $f;
  ok !-e $f;
  while(@d)                                                                     # Remove path
   {my $d = filePathDir(@d);
    rmdir $d;
    ok onWindows ? 1 : !-d $d;
    pop @d;
   }
 }

if (1)                                                                          # Clear folder
 {my $d = 'a';
  my @d = qw(a b c d);
  my @D = @d;
  while(@D)
   {my $f = filePathExt(@D, qw(test data));
    overWriteFile($f, '1');
    pop @D;
   }
  ok findFiles($d) == 4;
  eval q{clearFolder($d, 3)};
  ok $@ =~ m(\ALimit is 3, but 4 files under folder:)s;
  clearFolder($d, 4);
  ok onWindows ? 1 : !-d $d;
 }

ok formatTable                                                                  #TformatTable
 ([[qw(A    B    C    D   )],                                                   #TformatTable
   [qw(AA   BB   CC   DD  )],                                                   #TformatTable
   [qw(AAA  BBB  CCC  DDD )],                                                   #TformatTable
   [qw(AAAA BBBB CCCC DDDD)],                                                   #TformatTable
   [qw(1    22   333  4444)]], [qw(aa bb cc)]) eq <<END;   #TformatTable
   aa    bb    cc
1  A     B     C     D
2  AA    BB    CC    DD
3  AAA   BBB   CCC   DDD
4  AAAA  BBBB  CCCC  DDDD
5     1    22   333  4444
END

ok formatTable                                                                  #TformatTable
 ([[qw(1     B   C)],                                                           #TformatTable
   [qw(22    BB  CC)],                                                          #TformatTable
   [qw(333   BBB CCC)],                                                         #TformatTable
   [qw(4444  22  333)]], [qw(aa bb cc)]) eq <<END;         #TformatTable
   aa    bb   cc
1     1  B    C
2    22  BB   CC
3   333  BBB  CCC
4  4444   22  333
END

ok formatTable                                                                  #TformatTable
 ([{aa=>'A',   bb=>'B',   cc=>'C'},                                             #TformatTable
   {aa=>'AA',  bb=>'BB',  cc=>'CC'},                                            #TformatTable
   {aa=>'AAA', bb=>'BBB', cc=>'CCC'},                                           #TformatTable
   {aa=>'1',   bb=>'22',  cc=>'333'}                                            #TformatTable
   ]) eq <<END;                                                                 #TformatTable
   aa   bb   cc
1  A    B    C
2  AA   BB   CC
3  AAA  BBB  CCC
4    1   22  333
END

ok formatTable                                                                  #TformatTable
 ({''=>[qw(aa bb cc)],                                                          #TformatTable
    1=>[qw(A B C)],                                                             #TformatTable
    22=>[qw(AA BB CC)],                                                         #TformatTable
    333=>[qw(AAA BBB CCC)],                                                     #TformatTable
    4444=>[qw(1 22 333)]}) eq <<END;                                            #TformatTable
      aa   bb   cc
   1  A    B    C
  22  AA   BB   CC
 333  AAA  BBB  CCC
4444    1   22  333
END

ok formatTable                                                                  #TformatTable
 ({1=>{aa=>'A', bb=>'B', cc=>'C'},                                              #TformatTable
   22=>{aa=>'AA', bb=>'BB', cc=>'CC'},                                          #TformatTable
   333=>{aa=>'AAA', bb=>'BBB', cc=>'CCC'},                                      #TformatTable
   4444=>{aa=>'1', bb=>'22', cc=>'333'}}) eq <<END;                             #TformatTable
      aa   bb   cc
   1  A    B    C
  22  AA   BB   CC
 333  AAA  BBB  CCC
4444    1   22  333
END

ok formatTable({aa=>'A', bb=>'B', cc=>'C'}, [qw(aaaa bbbb)]) eq <<END;          #TformatTable
aaaa  bbbb
aa    A
bb    B
cc    C
END

if (1) {                                                                        # AL
  my $s = loadArrayFromLines <<END;                                             #TloadArrayFromLines
a a
b b
END
  is_deeply $s, [q(a a), q(b b)];                                               #TloadArrayFromLines
  ok formatTable($s) eq <<END;                             #TloadArrayFromLines
0  a a
1  b b
END
 }

if (1) {                                                                        # HL
  my $s = loadHashFromLines <<END;                                              #TloadHashFromLines
a 10 11 12
b 20 21 22
END
  is_deeply $s, {a => q(10 11 12), b =>q(20 21 22)};                            #TloadHashFromLines
  ok formatTable($s) eq <<END;                             #TloadHashFromLines
a  10 11 12
b  20 21 22
END
 }

if (1) {                                                                        # AAL
  my $s = loadArrayArrayFromLines <<END;                                        #TloadArrayArrayFromLines
A B C
AA BB CC
END
  is_deeply $s, [[qw(A B C)], [qw(AA BB CC)]];                                  #TloadArrayArrayFromLines
  ok formatTable($s) eq <<END;                             #TloadArrayArrayFromLines
1  A   B   C
2  AA  BB  CC
END
 }

if (1) {                                                                        # HAL
  my $s = loadHashArrayFromLines <<END;                                         #TloadHashArrayFromLines
a A B C
b AA BB CC
END
  is_deeply $s, {a =>[qw(A B C)], b => [qw(AA BB CC)] };                        #TloadHashArrayFromLines
  ok formatTable($s) eq <<END;                             #TloadHashArrayFromLines
a  A   B   C
b  AA  BB  CC
END
 }

if (1) {                                                                        # AAL
  my $s = loadArrayHashFromLines <<END;                                         #TloadArrayHashFromLines
A 1 B 2
AA 11 BB 22
END
  is_deeply $s, [{A=>1, B=>2}, {AA=>11, BB=>22}];                               #TloadArrayHashFromLines
  ok formatTable($s) eq <<END;                             #TloadArrayHashFromLines
   A  AA  B  BB
1  1      2
2     11     22
END
 }

if (1) {                                                                        # HAL
  my $s = loadHashHashFromLines <<END;                                          #TloadHashHashFromLines
a A 1 B 2
b AA 11 BB 22
END
  is_deeply $s, {a=>{A=>1, B=>2}, b=>{AA=>11, BB=>22}};                         #TloadHashHashFromLines
  ok formatTable($s) eq <<END;                             #TloadHashHashFromLines
   A  AA  B  BB
a  1      2
b     11     22
END
}

if (1) {                                                                        # Using a named package
  my $class = "Data::Table::Text::Test";
  my $a = bless{}, $class;
  genLValueScalarMethods(qq(${class}::$_)) for qw(aa bb cc);
  $a->aa = 'aa';
  ok  $a->aa eq 'aa';
  ok !$a->bb;
  ok  $a->bbX eq q();
  $a->aa = undef;
  ok !$a->aa;
 }

if (1) {                                                                        # Conditionally using a named package
  my $class = "Data::Table::Text::Test";                                        #TaddLValueScalarMethods
  my $a = bless{}, $class;                                                      #TaddLValueScalarMethods
  addLValueScalarMethods(qq(${class}::$_)) for qw(aa bb aa bb);                 #TaddLValueScalarMethods
  $a->aa = 'aa';                                                                #TaddLValueScalarMethods
  ok  $a->aa eq 'aa';                                                           #TaddLValueScalarMethods
  ok !$a->bb;                                                                   #TaddLValueScalarMethods
  ok  $a->bbX eq q();                                                           #TaddLValueScalarMethods
  $a->aa = undef;                                                               #TaddLValueScalarMethods
  ok !$a->aa;                                                                   #TaddLValueScalarMethods
 }

if (1) {                                                                        # Using the caller's package
  package Scalars;                                                              #TgenLValueScalarMethods
  my $a = bless{};                                                              #TgenLValueScalarMethods
  Data::Table::Text::genLValueScalarMethods(qw(aa bb cc));                      #TgenLValueScalarMethods
  $a->aa = 'aa';                                                                #TgenLValueScalarMethods
  Test::More::ok  $a->aa eq 'aa';                                               #TgenLValueScalarMethods
  Test::More::ok !$a->bb;                                                       #TgenLValueScalarMethods
  Test::More::ok  $a->bbX eq q();                                               #TgenLValueScalarMethods
  $a->aa = undef;                                                               #TgenLValueScalarMethods
  Test::More::ok !$a->aa;                                                       #TgenLValueScalarMethods
 }

if (1) {                                                                        # SDM
  package ScalarsWithDefaults;                                                  #TgenLValueScalarMethodsWithDefaultValues
  my $a = bless{};                                                              #TgenLValueScalarMethodsWithDefaultValues
  Data::Table::Text::genLValueScalarMethodsWithDefaultValues(qw(aa bb cc));     #TgenLValueScalarMethodsWithDefaultValues
  Test::More::ok $a->aa eq 'aa';                                                #TgenLValueScalarMethodsWithDefaultValues
 }

if (1) {                                                                        # AM
  package Arrays;                                                               #TgenLValueArrayMethods
  my $a = bless{};                                                              #TgenLValueArrayMethods
  Data::Table::Text::genLValueArrayMethods(qw(aa bb cc));                       #TgenLValueArrayMethods
  $a->aa->[1] = 'aa';                                                           #TgenLValueArrayMethods
  Test::More::ok $a->aa->[1] eq 'aa';                                           #TgenLValueArrayMethods
 }                                                                              #
                                                                                #
if (1) {                                                                        ## AM
  package Hashes;                                                               #TgenLValueHashMethods
  my $a = bless{};                                                              #TgenLValueHashMethods
  Data::Table::Text::genLValueHashMethods(qw(aa bb cc));                        #TgenLValueHashMethods
  $a->aa->{a} = 'aa';                                                           #TgenLValueHashMethods
  Test::More::ok $a->aa->{a} eq 'aa';                                           #TgenLValueHashMethods
 }

if (1) {                                                                        #TindentString
  my $t = [qw(aa bb cc)];
  my $d = [[qw(A B C)], [qw(AA BB CC)], [qw(AAA BBB CCC)],  [qw(1 22 333)]];
  my $s = indentString(formatTable($d), '  ')."\n";

  ok $s eq <<END;
  1  A    B    C
  2  AA   BB   CC
  3  AAA  BBB  CCC
  4    1   22  333
END
 }

ok trim(" a b ") eq join ' ', qw(a b);                                          #Ttrim
ok isBlank("");                                                                 #TisBlank
ok isBlank(" \n ");                                                             #TisBlank

ok  powerOfTwo(1) == 0;                                                         #TpowerOfTwo
ok  powerOfTwo(2) == 1;                                                         #TpowerOfTwo
ok !powerOfTwo(3);                                                              #TpowerOfTwo
ok  powerOfTwo(4) == 2;                                                         #TpowerOfTwo

ok  containingPowerOfTwo(1) == 0;                                               #TcontainingPowerOfTwo
ok  containingPowerOfTwo(2) == 1;                                               #TcontainingPowerOfTwo
ok  containingPowerOfTwo(3) == 2;                                               #TcontainingPowerOfTwo
ok  containingPowerOfTwo(4) == 2;                                               #TcontainingPowerOfTwo
ok  containingPowerOfTwo(5) == 3;
ok  containingPowerOfTwo(7) == 3;

if (1) {                                                                        #Tpad #Tppp #Tlpad
  is_deeply pad('abc  ', 2).'='         , "abc =";
  is_deeply pad('abc  ', 3).'='         , "abc=";
  is_deeply pad('abc  ', 4, q(.)).'='   , "abc.=";
  is_deeply pad('abc  ', 5).'='         , "abc  =";
  is_deeply pad('abc  ', 6).'='         , "abc   =";

  is_deeply  ppp(2, 'abc  ').'='        , "abc =";
  is_deeply  ppp(3, 'abc  ').'='        , "abc=";
  is_deeply  ppp(4, 'abc  ', q(.)).'='  , "abc.=";
  is_deeply  ppp(5, 'abc  ').'='        , "abc  =";
  is_deeply  ppp(6, 'abc  ').'='        , "abc   =";

  is_deeply lpad('abc  ', 2).'='        , " abc=";
  is_deeply lpad('abc  ', 3).'='        , "abc=";
  is_deeply lpad('abc  ', 4, q(.)).'='  , ".abc=";
  is_deeply lpad('abc  ', 5).'='        , "  abc=";
  is_deeply lpad('abc  ', 6).'='        , "   abc=";
 }
#ok containingFolder("/home/phil/test.data") eq "/home/phil/";
#ok containingFolder("phil/test.data")       eq "phil/";
#ok containingFolder("test.data")            eq "./";

if (1) {                                                                        #TjavaPackage #TjavaPackageAsFileName #TperlPackage
  my $j = writeFile(undef, <<END);
// Test
package com.xyz;
END
  ok javaPackage($j)           eq "com.xyz";
  ok javaPackageAsFileName($j) eq "com/xyz";
  unlink $j;

  my $p = writeFile(undef, <<END);                                              #TperlPackage
package a::b;
END
  ok perlPackage($p)           eq "a::b";                                       #TperlPackage
  unlink $p;
 }

if (0)                                                                          # Ignore windows for this test
 {ok xxx("echo aaa")       =~ /aaa/;                                            #Txxx
  ok xxx("a=bbb;echo \$a") =~ /bbb/;

  eval q{xxx "echo ccc", qr(ccc)};
  ok !$@;

  eval q{xxx "echo ddd", qr(ccc)};
  ok $@ =~ /ddd/;

  ok !yyy <<END;                                                                #Tyyy
echo aaa
echo bbb
END
 }
else
 {ok 1 for 1..5;
 }

if (1) {                                                                        #TencodeJson #TdecodeJson
  my $A = encodeJson(my $a = {a=>1,b=>2, c=>[1..2]});
  my $b = decodeJson($A);
  is_deeply $a, $b;
 }

if (1) {                                                                        #TencodeBase64 #TdecodeBase64
  my $A = encodeBase64(my $a = "Hello World" x 10);
  my $b = decodeBase64($A);
  ok $a eq $b;
 }

if (1) {                                                                        #Tmax #Tmin
  ok !max;
  ok max(1) == 1;
  ok max(1,4,2,3) == 4;

  ok min(1) == 1;
  ok min(5,4,2,3) == 2;
 }

is_deeply [1],       [contains(1,0..1)];                                        #Tcontains
is_deeply [1,3],     [contains(1, qw(0 1 0 1 0 0))];                            #Tcontains
is_deeply [0, 5],    [contains('a', qw(a b c d e a b c d e))];                  #Tcontains
is_deeply [0, 1, 5], [contains(qr(a+), qw(a baa c d e aa b c d e))];            #Tcontains

is_deeply [qw(a b)], [&removeFilePrefix(qw(a/ a/a a/b))];                       #TremoveFilePrefix
is_deeply [qw(b)],   [&removeFilePrefix("a/", "a/b")];                          #TremoveFilePrefix
ok q(a/b.c) eq removeDuplicatePrefixes("a/a/b.c");                              #TremoveDuplicatePrefixes
ok q(a/b.c) eq removeDuplicatePrefixes("a/b.c");                                #TremoveDuplicatePrefixes
ok q(b.c) eq removeDuplicatePrefixes("b.c");                                    #TremoveDuplicatePrefixes

if (0) {                                                                        #TfileOutOfDate
  my @Files = qw(a b c);
  my @files = (@Files, qw(d));
  writeFile($_, $_), sleep 1 for @Files;

  my $a = '';
  my @a = fileOutOfDate {$a .= $_} q(a), @files;
  ok $a eq 'da';
  is_deeply [@a], [qw(d a)];

  my $b = '';
  my @b = fileOutOfDate {$b .= $_} q(b), @files;
  ok $b eq 'db';
  is_deeply [@b], [qw(d b)];

  my $c = '';
  my @c = fileOutOfDate {$c .= $_} q(c), @files;
  ok $c eq 'dc';
  is_deeply [@c], [qw(d c)];

  my $d = '';
  my @d = fileOutOfDate {$d .= $_} q(d), @files;
  ok $d eq 'd';
  is_deeply [@d], [qw(d)];

  my @A = fileOutOfDate {} q(a), @Files;
  my @B = fileOutOfDate {} q(b), @Files;
  my @C = fileOutOfDate {} q(c), @Files;
  is_deeply [@A], [qw(a)];
  is_deeply [@B], [qw(b)];
  is_deeply [@C], [];
  unlink for @Files;
 }
else
 { SKIP:
   {skip "Takes too much time", 11;
   }
 }

ok convertUnicodeToXml('setenta e três') eq q(setenta e tr&#234;s);             #TconvertUnicodeToXml

ok zzz(<<END, qr(aaa\s*bbb)s);                                                  #Tzzz
echo aaa
echo bbb
END

if (1)                                                                          # Failure
 {eval q{zzz(qq(echo aaa\necho bbb\n), qr(SUCCESS)s)};
  ok $@ =~ m(Data::Table::Text::zzz)s;
 }

if (1) {                                                                        #TparseCommandLineArguments
  my $r = parseCommandLineArguments {[@_]}
   [qw( aaa bbb -c --dd --eee=EEEE -f=F), q(--gg=g g), q(--hh=h h)];
  is_deeply $r,
    [["aaa", "bbb"],
     {c=>undef, dd=>undef, eee=>"EEEE", f=>"F", gg=>"g g", hh=>"h h"},
    ];
 }

if (1)                                                                          #TparseCommandLineArguments
 {my $r = parseCommandLineArguments
   {ok 1;
    $_[1]
   }
   [qw(--aAa=AAA --bbB=BBB)], [qw(aaa bbb ccc)];
  is_deeply $r, {aaa=>'AAA', bbb=>'BBB'};
 }

if (1)
 {eval
  q{parseCommandLineArguments
     {$_[1]} [qw(aaa bbb ddd --aAa=AAA --dDd=DDD)], [qw(aaa bbb ccc)];
   };
  my $r = $@;
  ok $r =~ m(\AInvalid parameter: --dDd=DDD);
 }


if (1) {                                                                        #TsetIntersection
  is_deeply [qw(a b c)], [setIntersection[qw(e f g a b c )],[qw(a A b B c C)]];
  is_deeply [qw(e)],   [setIntersection {a=>1, b=>2, e=>3}, [qw(c d e)], qw(e)];
 }


if (1) {                                                                        #TsetUnion
  is_deeply [qw(a b c)],     [setUnion(qw(a b c a a b b b))];
  is_deeply [qw(a b c d e)], [setUnion {a=>1, b=>2, e=>3}, [qw(c d e)], qw(e)];
 }

if (1) {                                                                        #TsetIntersectionOverUnion
  my $f = setIntersectionOverUnion {a=>1, b=>2, e=>3}, [qw(c d e)], qw(e);
  ok $f > 0.199999 && $f < 0.200001;
 }

if (1) {                                                                        #TsetPartitionOnIntersectionOverUnion
  is_deeply [setPartitionOnIntersectionOverUnion
   (0.80,
     [qw(a A   b c d e)],
     [qw(a A B b c d e)],
     [qw(a A B C b c d)],
   )],
  [[["A", "B", "a".."e"],
    ["A",      "a".."e"]],
   [["A".."C", "a".."d"]],
  ];
}




if (1) {                                                                        #TsetPartitionOnIntersectionOverUnionOfSetsOfWords
is_deeply [setPartitionOnIntersectionOverUnionOfSetsOfWords
   (0.80,
     [qw(a A   b c d e)],
     [qw(a A B b c d e)],
     [qw(a A B C b c d)],
   )],
 [[["a", "A", "B", "C", "b", "c", "d"]],
  [["a", "A", "B", "b" .. "e"], ["a", "A", "b" .. "e"]],
 ];
 }

if (1) {                                                                        #TsetPartitionOnIntersectionOverUnionOfStringSets
is_deeply [setPartitionOnIntersectionOverUnionOfStringSets
   (0.80,
     q(The Emu            are seen here sometimes.),
     q(The Emu, Gnu       are seen here sometimes.),
     q(The Emu, Gnu, Colt are seen here.),
   )],
 [["The Emu, Gnu, Colt are seen here."],
  ["The Emu, Gnu       are seen here sometimes.",
   "The Emu            are seen here sometimes.",
  ]];
 }

if (1) {                                                                        #TsetPartitionOnIntersectionOverUnionOfHashStringSets
  is_deeply [setPartitionOnIntersectionOverUnionOfHashStringSets
   (0.80,
     {e  =>q(The Emu            are seen here sometimes.),
      eg =>q(The Emu, Gnu       are seen here sometimes.),
      egc=>q(The Emu, Gnu, Colt are seen here.),
     }
   )],
 [["e", "eg"], ["egc"]];
 }

ok printQw(qw(a  b  c)) eq "qw(a b c)";

if (1) {
  my $f = writeFile("zzz.data", "aaa");                                         #TfileSize
  ok -e $f;
  ok fileSize($f) == 3;                                                         #TfileSize
  unlink $f;
  ok !-e $f;
 }

if (1) {
  my $f = createEmptyFile(fpe(my $d = temporaryFolder, qw(a jpg)));             #TfindFileWithExtension
  my $F = findFileWithExtension(fpf($d, q(a)), qw(txt data jpg));               #TfindFileWithExtension
  ok -e $f;
  ok $F eq "jpg";                                                               #TfindFileWithExtension
  unlink $f;
  ok !-e $f;
  rmdir $d;
  ok !-d $d;
 }

if (1) {
  my $d = temporaryFolder;                                                      #TfirstFileThatExists
  ok $d eq firstFileThatExists("$d/$d", $d);                                    #TfirstFileThatExists
 }

if (1) {                                                                        #TassertRef
  eval q{assertRef(bless {}, q(aaa))};
  ok $@ =~ m(\AWanted reference to Data::Table::Text, but got aaa);
 }

if (1) {                                                                        #TassertPackageRefs
  eval q{assertPackageRefs(q(bbb), bless {}, q(aaa))};
  ok $@ =~ m(\AWanted reference to bbb, but got aaa);
 }

# Relative and absolute files
ok "../../../"              eq relFromAbsAgainstAbs("/",                    "/home/la/perl/bbb.pl");
ok "../../../home"          eq relFromAbsAgainstAbs("/home",                "/home/la/perl/bbb.pl");
ok "../../"                 eq relFromAbsAgainstAbs("/home/",               "/home/la/perl/bbb.pl");
ok "aaa.pl"                 eq relFromAbsAgainstAbs("/home/la/perl/aaa.pl", "/home/la/perl/bbb.pl");
ok "aaa"                    eq relFromAbsAgainstAbs("/home/la/perl/aaa",    "/home/la/perl/bbb.pl");
ok "./"                     eq relFromAbsAgainstAbs("/home/la/perl/",       "/home/la/perl/bbb.pl");
ok "aaa.pl"                 eq relFromAbsAgainstAbs("/home/la/perl/aaa.pl", "/home/la/perl/bbb");
ok "aaa"                    eq relFromAbsAgainstAbs("/home/la/perl/aaa",    "/home/la/perl/bbb");
ok "./"                     eq relFromAbsAgainstAbs("/home/la/perl/",       "/home/la/perl/bbb");
ok "../java/aaa.jv"         eq relFromAbsAgainstAbs("/home/la/java/aaa.jv", "/home/la/perl/bbb.pl");
ok "../java/aaa"            eq relFromAbsAgainstAbs("/home/la/java/aaa",    "/home/la/perl/bbb.pl");
ok "../java/"               eq relFromAbsAgainstAbs("/home/la/java/",       "/home/la/perl/bbb.pl");
ok "../../la/perl/aaa.pl"   eq relFromAbsAgainstAbs("/home/la/perl/aaa.pl", "/home/il/perl/bbb.pl");
ok "../../la/perl/aaa"      eq relFromAbsAgainstAbs("/home/la/perl/aaa",    "/home/il/perl/bbb.pl");
ok "../../la/perl/"         eq relFromAbsAgainstAbs("/home/la/perl/",       "/home/il/perl/bbb.pl");
ok "../../la/perl/aaa.pl"   eq relFromAbsAgainstAbs("/home/la/perl/aaa.pl", "/home/il/perl/bbb");
ok "../../la/perl/aaa"      eq relFromAbsAgainstAbs("/home/la/perl/aaa",    "/home/il/perl/bbb");
ok "../../la/perl/"         eq relFromAbsAgainstAbs("/home/la/perl/",       "/home/il/perl/bbb");
ok "../../la/perl/"         eq relFromAbsAgainstAbs("/home/la/perl/",       "/home/il/perl/bbb");
ok "../../la/perl/aaa"      eq relFromAbsAgainstAbs("/home/la/perl/aaa",    "/home/il/perl/");
ok "../../la/perl/"         eq relFromAbsAgainstAbs("/home/la/perl/",       "/home/il/perl/");
ok "../../la/perl/"         eq relFromAbsAgainstAbs("/home/la/perl/",       "/home/il/perl/");
ok "home/la/perl/bbb.pl"    eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/");
ok "home/la/perl/bbb.pl"    eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home");
ok "la/perl/bbb.pl"         eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/");
ok "bbb.pl"                 eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/perl/aaa.pl");  #TrelFromAbsAgainstAbs
ok "bbb.pl"                 eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/perl/aaa");
ok "bbb.pl"                 eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/perl/");
ok "bbb"                    eq relFromAbsAgainstAbs("/home/la/perl/bbb",    "/home/la/perl/aaa.pl");
ok "bbb"                    eq relFromAbsAgainstAbs("/home/la/perl/bbb",    "/home/la/perl/aaa");
ok "bbb"                    eq relFromAbsAgainstAbs("/home/la/perl/bbb",    "/home/la/perl/");
ok "../perl/bbb.pl"         eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/java/aaa.jv");  #TrelFromAbsAgainstAbs
ok "../perl/bbb.pl"         eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/java/aaa");
ok "../perl/bbb.pl"         eq relFromAbsAgainstAbs("/home/la/perl/bbb.pl", "/home/la/java/");
ok "../../il/perl/bbb.pl"   eq relFromAbsAgainstAbs("/home/il/perl/bbb.pl", "/home/la/perl/aaa.pl");
ok "../../il/perl/bbb.pl"   eq relFromAbsAgainstAbs("/home/il/perl/bbb.pl", "/home/la/perl/aaa");
ok "../../il/perl/bbb.pl"   eq relFromAbsAgainstAbs("/home/il/perl/bbb.pl", "/home/la/perl/");
ok "../../il/perl/bbb"      eq relFromAbsAgainstAbs("/home/il/perl/bbb",    "/home/la/perl/aaa.pl");
ok "../../il/perl/bbb"      eq relFromAbsAgainstAbs("/home/il/perl/bbb",    "/home/la/perl/aaa");
ok "../../il/perl/bbb"      eq relFromAbsAgainstAbs("/home/il/perl/bbb",    "/home/la/perl/");
ok "../../il/perl/bbb"      eq relFromAbsAgainstAbs("/home/il/perl/bbb",    "/home/la/perl/");
ok "../../il/perl/"         eq relFromAbsAgainstAbs("/home/il/perl/",       "/home/la/perl/aaa");
ok "../../il/perl/"         eq relFromAbsAgainstAbs("/home/il/perl/",       "/home/la/perl/");
ok "../../il/perl/"         eq relFromAbsAgainstAbs("/home/il/perl/",       "/home/la/perl/");

ok "/"                      eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "../../..");
ok "/home"                  eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "../../../home");
ok "/home/"                 eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "../..");
ok "/home/la/perl/aaa.pl"   eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "aaa.pl");
ok "/home/la/perl/aaa"      eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "aaa");
ok "/home/la/perl/"         eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "");
ok "/home/la/perl/aaa.pl"   eq absFromAbsPlusRel("/home/la/perl/bbb",      "aaa.pl");                 #TabsFromAbsPlusRel
ok "/home/la/perl/aaa"      eq absFromAbsPlusRel("/home/la/perl/bbb",      "aaa");
ok "/home/la/perl/"         eq absFromAbsPlusRel("/home/la/perl/bbb",      "");
ok "/home/la/java/aaa.jv"   eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "../java/aaa.jv");
ok "/home/la/java/aaa"      eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "../java/aaa");
ok "/home/la/java"          eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "../java");
ok "/home/la/java/"         eq absFromAbsPlusRel("/home/la/perl/bbb.pl",   "../java/");
ok "/home/la/perl/aaa.pl"   eq absFromAbsPlusRel("/home/il/perl/bbb.pl",   "../../la/perl/aaa.pl");    #TabsFromAbsPlusRel
ok "/home/la/perl/aaa"      eq absFromAbsPlusRel("/home/il/perl/bbb.pl",   "../../la/perl/aaa");
ok "/home/la/perl"          eq absFromAbsPlusRel("/home/il/perl/bbb.pl",   "../../la/perl");
ok "/home/la/perl/"         eq absFromAbsPlusRel("/home/il/perl/bbb.pl",   "../../la/perl/");
ok "/home/la/perl/aaa.pl"   eq absFromAbsPlusRel("/home/il/perl/bbb",      "../../la/perl/aaa.pl");
ok "/home/la/perl/aaa"      eq absFromAbsPlusRel("/home/il/perl/bbb",      "../../la/perl/aaa");
ok "/home/la/perl"          eq absFromAbsPlusRel("/home/il/perl/bbb",      "../../la/perl");
ok "/home/la/perl/"         eq absFromAbsPlusRel("/home/il/perl/bbb",      "../../la/perl/");
ok "/home/la/perl/aaa"      eq absFromAbsPlusRel("/home/il/perl/",         "../../la/perl/aaa");
ok "/home/la/perl"          eq absFromAbsPlusRel("/home/il/perl/",         "../../la/perl");
ok "/home/la/perl/"         eq absFromAbsPlusRel("/home/il/perl/",         "../../la/perl/");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/",                      "home/la/perl/bbb.pl");
#ok "/home/la/perl/bbb.pl"  eq absFromAbsPlusRel("/home",                  "../home/la/perl/bbb.pl");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/home/",                 "la/perl/bbb.pl");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/perl/aaa.pl",   "bbb.pl");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/perl/aaa",      "bbb.pl");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/perl/",         "bbb.pl");
ok "/home/la/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/aaa.pl",   "bbb");
ok "/home/la/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/aaa",      "bbb");
ok "/home/la/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/aaa",      "bbb");
ok "/home/la/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/",         "bbb");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/java/aaa.jv",   "../perl/bbb.pl");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/java/aaa",      "../perl/bbb.pl");
ok "/home/la/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/java/",         "../perl/bbb.pl");
ok "/home/il/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/perl/aaa.pl",   "../../il/perl/bbb.pl");
ok "/home/il/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/perl/aaa",      "../../il/perl/bbb.pl");
ok "/home/il/perl/bbb.pl"   eq absFromAbsPlusRel("/home/la/perl/",         "../../il/perl/bbb.pl");
ok "/home/il/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/aaa.pl",   "../../il/perl/bbb");
ok "/home/il/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/aaa",      "../../il/perl/bbb");
ok "/home/il/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/",         "../../il/perl/bbb");
ok "/home/il/perl/bbb"      eq absFromAbsPlusRel("/home/la/perl/",         "../../il/perl/bbb");
ok "/home/il/perl"          eq absFromAbsPlusRel("/home/la/perl/aaa",      "../../il/perl");
ok "/home/il/perl/"         eq absFromAbsPlusRel("/home/la/perl/",         "../../il/perl/");

ok "/aaa/"                  eq absFile(qw(/aaa/));                              #TabsFile
ok "/aaa/bbb/ccc/ddd.txt"   eq sumAbsAndRel(qw(/aaa/AAA/ ../bbb/bbb/BBB/ ../../ccc/ddd.txt)); #TsumAbsAndRel

ok fp (prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(a/b/);                             #Tfp
ok fpn(prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(a/b/c.d);                          #Tfpn
ok fn (prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(c.d);                              #Tfn
ok fne(prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(c.d.e);                            #Tfne
ok fe (prefferedFileName q(a/b/c.d.e))  eq prefferedFileName q(e);                                #Tfe
ok fp (prefferedFileName q(/a/b/c.d.e)) eq prefferedFileName q(/a/b/);
ok fpn(prefferedFileName q(/a/b/c.d.e)) eq prefferedFileName q(/a/b/c.d);
ok fn (prefferedFileName q(/a/b/c.d.e)) eq prefferedFileName q(c.d);
ok fne(prefferedFileName q(/a/b/c.d.e)) eq prefferedFileName q(c.d.e);
ok fe (prefferedFileName q(/a/b/c.d.e)) eq prefferedFileName q(e);

if (1) {                                                                        #Tcall
  our $a = q(1);
  our @a = qw(1);
  our %a = (a=>1);
  our $b = q(1);
  for(2..4) {
    call {$a = $_  x 1e3; $a[0] = $_ x 1e2; $a{a} = $_ x 1e1; $b = 2;} qw($a @a %a);
    ok $a    == $_ x 1e3;
    ok $a[0] == $_ x 1e2;
    ok $a{a} == $_ x 1e1;
    ok $b    == 1;
   }
 }

ok prefferedFileName (q(../a/))  eq fp prefferedFileName q(../a/b.c);
ok prefferedFileName (q(b))      eq fn prefferedFileName q(../a/b.c);
ok prefferedFileName (q(c))      eq fe prefferedFileName q(../a/b.c);

ok prefferedFileName (q(./))     eq fp prefferedFileName q(./);
ok prefferedFileName (q(../))    eq fp prefferedFileName q(../);
ok prefferedFileName (q(../../)) eq fp prefferedFileName q(../../);

if (1) {
ok q(a)                          eq fn prefferedFileName q(./a);
ok q(a)                          eq fn prefferedFileName q(../a);
ok q(a)                          eq fn prefferedFileName q(../../a);

ok q(a)                          eq fe prefferedFileName q(.a);
ok q(a)                          eq fe prefferedFileName q(./.a);
ok q(a)                          eq fe prefferedFileName q(../.a);
ok q(a)                          eq fe prefferedFileName q(../../.a);
}

if (1) {                                                                        #TwwwEncode #TwwwDecode
  ok wwwEncode(q(a  {b} <c>)) eq q(a%20%20%7bb%7d%20%3cc%3e);
  ok wwwEncode(q(../))        eq q(%2e%2e/);
  ok wwwDecode(wwwEncode $_)  eq $_ for q(a  {b} <c>), q(a  b|c),
    q(%), q(%%), q(%%.%%);
 }

is_deeply quoteFile(fpe(qw(a "b" c))), onWindows ? q("a\\\"b\".c") : q("a/\"b\".c"); #TquoteFile
is_deeply       printQw(qw(a b c)),    q(qw(a b c));                            #TprintQw

latest:;
if (1) {                                                                        #TtemporaryFolder #Tfpd #TcreateEmptyFile #TfindFiles #TfindDirs #TsearchDirectoryTreesForMatchingFiles #TsearchDirectoryTreeForSubFolders #TclearFolder #TfileList
  my $D = temporaryFolder;
  ok  -d $D;

  my $d = fpd($D, q(ddd));
  ok !-d $d;

  my @f = map {createEmptyFile(fpe($d, $_, qw(txt)))} qw(a b c);
  is_deeply [sort map {fne $_} findFiles($d, qr(txt\Z))], [qw(a.txt b.txt c.txt)];

  my @D = findDirs($D);
  my @e = ($D, $d);
  my @E = sort @e;
  is_deeply [@D], [@E];

  is_deeply [sort map {fne $_} searchDirectoryTreesForMatchingFiles($d)],
            ["a.txt", "b.txt", "c.txt"];

  is_deeply [sort map {fne $_} fileList(prefferedFileName "$d/*.txt")],
            ["a.txt", "b.txt", "c.txt"];

  ok -e $_ for @f;

  is_deeply scalar(searchDirectoryTreeForSubFolders $D), 2;

  my @g = fileList(qq($D/*/*.txt));
  ok @g == 3;

  clearFolder($D, 5);
  ok onWindows ? 1 : !-e $_ for @f;
  ok onWindows ? 1 : !-d $D;
 }

if (1) {                                                                        #TwriteFile #TreadFile #TappendFile #TwriteTempFile #ToverWriteFile
  my $f = writeFile(undef,  "aaa");
  is_deeply [readFile $f], ["aaa"];

  appendFile($f, "bbb");
  is_deeply [readFile $f], ["aaabbb"];

  my $F = writeTempFile(qw(aaa bbb));
  is_deeply [readFile $F], ["aaa\n", "bbb\n"];

  eval {writeFile($f,  q(ccc))};
  ok $@ =~ m(File already exists:)i;

  overWriteFile($F,    q(ccc));
  ok   readFile($F) eq q(ccc);

  unlink $f, $F;
 }

if (1) {
  no utf8;
  my $f = writeBinaryFile(undef, 0xff x 8);                                     #TwriteBinaryFile #TreadBinaryFile
  my $s = readBinaryFile($f);                                                   #TwriteBinaryFile #TreadBinaryFile
  ok $s eq 0xff x 8;                                                            #TwriteBinaryFile #TreadBinaryFile
  unlink $f;
 }

if (1) {                                                                        #TmakePath #TmakePathRemote #TtemporaryFolder #TtemporaryDirectory #TtemporaryFile
  my $d = fpd(my $D = temporaryDirectory, qw(a));
  my $f = fpe($d, qw(bbb txt));
  ok !-d $d;
  eval q{checkFile($f)};
  my $r = $@;
  my $q = quotemeta($D);
  ok nws($r) =~ m(Can only find.+?: $q)s;
  makePath($f);
  ok -d $d;
  ok -d $D;
  rmdir $_ for $d, $D;

  my $e = temporaryFolder;                                                      # Same as temporyDirectory
  ok -d $e;
  clearFolder($e, 2);

  my $t = temporaryFile;
  ok  -f $t;
  unlink $t;
  ok !-f $t;

  if (0)
   {makePathRemote($e);                                                         # Make a path on the remote system
   }
 }

ok nws(qq(a  b    c)) eq q(a b c);                                              #Tnws

if (0) {                                                                        # Despite eval the confess seems to be killing the process - perhaps the confess is just too big?
  eval q{checkKeys({a=>1, b=>2, d=>3}, {a=>1, b=>2, c=>3})};                    #TcheckKeys
  ok nws($@) =~ m(\AInvalid options chosen: d Permitted.+?: a 1 b 2 c 3);       #TcheckKeys
 }

if (1) {                                                                        #TformatTableBasic
  my $d = [[qw(a 1)], [qw(bb 22)], [qw(ccc 333)], [qw(dddd 4444)]];
  ok formatTableBasic($d) eq <<END, q(ftb);
a        1
bb      22
ccc    333
dddd  4444
END
  }

if (0) {                                                                        #TstartProcess #TwaitForAllStartedProcessesToFinish
  my %pids;
  sub{startProcess {} %pids, 1; ok 1 >= keys %pids}->() for 1..8;
  waitForAllStartedProcessesToFinish(%pids);
  ok !keys(%pids)
 }

if (1) {
ok dateTimeStamp     =~ m(\A\d{4}-\d\d-\d\d at \d\d:\d\d:\d\d\Z), q(dts);       #TdateTimeStamp
ok dateTimeStampName =~ m(\A_on_\d{4}_\d\d_\d\d_at_\d\d_\d\d_\d\d\Z);           #TdateTimeStampName
ok dateStamp         =~ m(\A\d{4}-\w{3}-\d\d\Z);                                #TdateStamp
ok versionCode       =~ m(\A\d{8}-\d{6}\Z);                                     #TversionCode
ok versionCodeDashed =~ m(\A\d{4}-\d\d-\d\d-\d\d:\d\d:\d\d\Z);                  #TversionCodeDashed
ok timeStamp         =~ m(\A\d\d:\d\d:\d\d\Z);                                  #TtimeStamp
ok microSecondsSinceEpoch > 47*365*24*60*60*1e6;                                #TmicroSecondsSinceEpoch
 }

if (0) {
  saveCodeToS3(1200, q(.), q(projectName), q(bucket/folder), q(--quiet));       #TsaveCodeToS3
  my ($width, $height) = imageSize(fpe(qw(a image jpg)));                       #TimageSize
  addCertificate(fpf(qw(.ssh cert)));                                           #TaddCertificate
  binModeAllUtf8;                                                               #TbinModeAllUtf8
  convertImageToJpx(fpe(qw(a image jpg)), fpe(qw(a image jpg)), 256);           #TconvertImageToJpx
  currentDirectory;                                                             #TcurrentDirectory
  currentDirectoryAbove;                                                        #TcurrentDirectoryAbove
  fullFileName(fpe(qw(a txt)));                                                 #TfullFileName
  convertDocxToFodt(fpe(qw(a docx)), fpe(qw(a fodt)));                          #TconvertDocxToFodt
  cutOutImagesInFodtFile(fpe(qw(source fodt)), fpd(qw(images)), q(image));      #TcutOutImagesInFodtFile
  userId;                                                                       #TuserId
  hostName;                                                                     #ThostName
  makeDieConfess                                                                #TmakeDieConfess
  ipAddressViaArp(q(secarias));                                                 #TipAddressViaArp
  fileMd5Sum(q(/etc/hosts));                                                    #TfileMd5Sum
  countFileExtensions(q(/home/phil/perl/));                                     #TcountFileExtensions
  countFileTypes(4, q(/home/phil/perl/));                                       #TcountFileTypes
 }

ok nws(htmlToc("XXXX", <<END)), 'htmlToc'                                       #ThtmlToc
<h1 id="1" otherprops="1">Chapter 1</h1>
  <h2 id="11" otherprops="11">Section 1</h1>
<h1 id="2" otherprops="2">Chapter 2</h1>
XXXX
END
  eq nws(<<END);                                                                #ThtmlToc
<h1 id="1" otherprops="1">Chapter 1</h1>
  <h2 id="11" otherprops="11">Section 1</h1>
<h1 id="2" otherprops="2">Chapter 2</h1>
<table cellspacing=10 border=0>
<tr><td>&nbsp;
<tr><td align=right>1<td>&nbsp;&nbsp;&nbsp;&nbsp;<a href="#1">Chapter 1</a>
<tr><td align=right>2<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="#11">Section 1</a>
<tr><td>&nbsp;
<tr><td align=right>3<td>&nbsp;&nbsp;&nbsp;&nbsp;<a href="#2">Chapter 2</a>
</table>
END

ok fileModTime($0) =~ m(\A\d+\Z)s;                                              #TfileModTime

if (1)
 {my $s = updateDocumentation(<<'END' =~ s(!) (#)gsr =~ s(~) ()gsr);            #TupdateDocumentation
package Sample::Module;

!D1 Samples                                                                      ! Sample methods.

sub sample($@)                                                                  !R Documentation for the:  sample() method.  See also L<Data::Table::Text::sample2|/Data::Table::Text::sample2>. !Tsample .
 {my ($node, @context) = @_;                                                    ! Node, optional context
  1
 }

~BEGIN{*smpl=*sample}

sub Data::Table::Text::sample2(\&@)                                             !PS Documentation for the sample2() method.
 {my ($sub, @context) = @_;                                                     ! Sub to call, context.
  1
 }

ok sample(undef, qw(a b c)) == 1;                                               !Tsample

if (1)                                                                          !Tsample
 {ok sample(q(a), qw(a b c))  == 2;
  ok sample(undef, qw(a b c)) == 1;
 }

ok sample(<<END2)) == 1;                                                        !Tsample
sample data
END2

END

  ok $s =~ m/=head2 Data::Table::Text::sample2.\$sub, \@context/;               #TupdateDocumentation
 }

if (1) {                                                                        #TdumpFile #TevalFile #TdumpFileAsJson #TevalFileAsJson #TdumpTempFile #TdumpTempFileAsJson
  my $d = [qw(aaa bbb ccc), [{aaa=>'AAA', bbb=>'BBB'}]];
  my $f = dumpFile(undef, $d);
  is_deeply evalFile($f), $d;
  is_deeply evalFile(my $F = dumpTempFile($d)), $d;
  unlink $f, $F;

  my $j = dumpFileAsJson(undef, $d);
  is_deeply evalFileAsJson($j), $d;
  is_deeply evalFileAsJson(my $J = dumpTempFileAsJson($d)), $d;
  unlink $j, $J;
 }

if (1) {                                                                        #TstoreFile #TretrieveFile
  my $f = storeFile(undef, my $d = [qw(aaa bbb ccc)]);
  my $s = retrieveFile($f);
  is_deeply $s, $d;
  unlink $f;
 }

ok 3 == maximumLineLength(<<END);                                               #TmaximumLineLength
a
bb
ccc
END

ok boldString(q(zZ)) eq q(𝘇𝗭);                                                  #TboldString
ok enclosedString(q(hello world 1234)) eq q(ⓗⓔⓛⓛⓞ ⓦⓞⓡⓛⓓ ①②③④);         #TenclosedString
ok enclosedReversedString(q(hello world 1234)) eq q(🅗🅔🅛🅛🅞 🅦🅞🅡🅛🅓 ➊➋➌➍); #TenclosedReversedString

ok superScriptString(1234567890) eq q(¹²³⁴⁵⁶⁷⁸⁹⁰);                              #TsuperScriptString
ok subScriptString(1234567890)   eq q(₁₂₃₄₅₆₇₈₉₀);                              #TsubScriptString

ok mathematicalItalicString                 (q(APPLES and ORANGES)) eq q(𝐴𝑃𝑃𝐿𝐸𝑆 𝑎𝑛𝑑 𝑂𝑅𝐴𝑁𝐺𝐸𝑆);  #TmathematicalItalicString
ok mathematicalBoldString                   (q(APPLES and ORANGES)) eq q(𝐀𝐏𝐏𝐋𝐄𝐒 𝐚𝐧𝐝 𝐎𝐑𝐀𝐍𝐆𝐄𝐒);  #TmathematicalBoldString
ok mathematicalBoldStringUndo               (q(𝐀𝐏𝐏𝐋𝐄𝐒 𝐚𝐧𝐝 𝐎𝐑𝐀𝐍𝐆𝐄𝐒)) eq q(APPLES and ORANGES);  #TmathematicalBoldStringUndo
ok mathematicalBoldItalicString             (q(APPLES and ORANGES)) eq q(𝑨𝑷𝑷𝑳𝑬𝑺 𝒂𝒏𝒅 𝑶𝑹𝑨𝑵𝑮𝑬𝑺);   #TmathematicalBoldItalicString
ok mathematicalBoldItalicStringUndo         (q(𝑨𝑷𝑷𝑳𝑬𝑺 𝒂𝒏𝒅 𝑶𝑹𝑨𝑵𝑮𝑬𝑺))  eq q(APPLES and ORANGES);  #TmathematicalBoldItalicStringUndo
ok mathematicalSansSerifString              (q(APPLES and ORANGES)) eq q(𝖠𝖯𝖯𝖫𝖤𝖲 𝖺𝗇𝖽 𝖮𝖱𝖠𝖭𝖦𝖤𝖲);   #TmathematicalSansSerifString
ok mathematicalSansSerifStringUndo          (q(𝖠𝖯𝖯𝖫𝖤𝖲 𝖺𝗇𝖽 𝖮𝖱𝖠𝖭𝖦𝖤𝖲))  eq q(APPLES and ORANGES);  #TmathematicalSansSerifStringUndo
ok mathematicalSansSerifBoldString          (q(APPLES and ORANGES)) eq q(𝗔𝗣𝗣𝗟𝗘𝗦 𝗮𝗻𝗱 𝗢𝗥𝗔𝗡𝗚𝗘𝗦);   #TmathematicalSansSerifBoldString
ok mathematicalSansSerifBoldStringUndo      (q(𝗔𝗣𝗣𝗟𝗘𝗦 𝗮𝗻𝗱 𝗢𝗥𝗔𝗡𝗚𝗘𝗦)) eq q(APPLES and ORANGES);   #TmathematicalSansSerifBoldStringUndo
ok mathematicalSansSerifItalicString        (q(APPLES and ORANGES)) eq q(𝘈𝘗𝘗𝘓𝘌𝘚 𝘢𝘯𝘥 𝘖𝘙𝘈𝘕𝘎𝘌𝘚);    #TmathematicalSansSerifItalicString
ok mathematicalSansSerifItalicStringUndo    (q(𝘈𝘗𝘗𝘓𝘌𝘚 𝘢𝘯𝘥 𝘖𝘙𝘈𝘕𝘎𝘌𝘚)) eq q(APPLES and ORANGES);    #TmathematicalSansSerifItalicStringUndo
ok mathematicalSansSerifBoldItalicString    (q(APPLES and ORANGES)) eq q(𝘼𝙋𝙋𝙇𝙀𝙎 𝙖𝙣𝙙 𝙊𝙍𝘼𝙉𝙂𝙀𝙎);   #TmathematicalSansSerifBoldItalicString
ok mathematicalSansSerifBoldItalicStringUndo(q(𝘼𝙋𝙋𝙇𝙀𝙎 𝙖𝙣𝙙 𝙊𝙍𝘼𝙉𝙂𝙀𝙎)) eq q(APPLES and ORANGES);   #TmathematicalSansSerifBoldItalicStringUndo
ok mathematicalMonoSpaceString              (q(APPLES and ORANGES)) eq q(𝙰𝙿𝙿𝙻𝙴𝚂 𝚊𝚗𝚍 𝙾𝚁𝙰𝙽𝙶𝙴𝚂);  #TmathematicalMonoSpaceString
ok mathematicalMonoSpaceStringUndo          (q(𝙰𝙿𝙿𝙻𝙴𝚂 𝚊𝚗𝚍 𝙾𝚁𝙰𝙽𝙶𝙴𝚂)) eq q(APPLES and ORANGES);  #TmathematicalMonoSpaceStringUndo


if (1)                                                                          #TboldStringUndo #TenclosedStringUndo #TenclosedReversedStringUndo #TsuperScriptStringUndo #TsubScriptStringUndo
 {my $n = 1234567890;
  ok boldStringUndo            (boldString($n))             == $n;
  ok enclosedStringUndo        (enclosedString($n))         == $n;
  ok enclosedReversedStringUndo(enclosedReversedString($n)) == $n;
  ok superScriptStringUndo     (superScriptString($n))      == $n;
  ok subScriptStringUndo       (subScriptString($n))        == $n;
 }

if (!onWindows) {

if (1) {                                                                        #TwriteGZipFile #TreadGZipFile
  my $s = '𝝰'x1e3;
  my $file = writeGZipFile(q(zzz.zip), $s);
  ok -e $file;
  my $S = readGZipFile($file);
  ok $s eq $S;
  ok length($s) == length($S);
  unlink $file;
 }

if (1) {                                                                        #TdumpGZipFile #TevalGZipFile
  my $d = [1, 2, 3=>{a=>4, b=>5}];
  my $file = dumpGZipFile(q(zzz.zip), $d);
  ok -e $file;
  my $D = evalGZipFile($file);
  is_deeply $d, $D;
  unlink $file;
 }
}

if (1)
 {my $t = formatTableBasic([["a",undef], [undef, "b\nc"]]);
  ok $t eq <<END;
a
   b
   c
END
 }

ok firstNChars(q(abc), 2) eq q(ab);                                             #TfirstNChars
ok firstNChars(q(abc), 4) eq q(abc);                                            #TfirstNChars

if (1)
 {my $t = formatTable([["a",undef], [undef, "b\nc"]], [undef, undef]);
  ok $t eq <<END;
1  a
2     b
      c
END
 }

if (1) {                                                                        #TformatTable
  my $d = temporaryFolder;
  my $f = fpe($d, qw(report txt));                                              # Create a report
  my $t = formatTable
   ([["a",undef], [undef, "b\x0ac"]],                                           # Data - please replace 0a with a new line
    [undef, "BC"],                                                              # Column titles
    file=>$f,                                                                   # Output file
    head=><<END);                                                               # Header
Sample report.

Table has NNNN rows.
END
  ok -e $f;

  ok readFile($f) eq $t;
  is_deeply nws($t), nws(<<END);
Sample report.

Table has 2 rows.

This file: ${d}report.txt

      BC
1  a
2     b
      c
END
  clearFolder($d, 2);
 }

if (1)
 {my $t = "a\nb\n";
  ok numberOfLinesInString("a\nb\n") == 2;                                      #TnumberOfLinesInString
 }

if (1) {
  my $f = writeFile(undef, "a\nb\n");                                           #TnumberOfLinesInFile
  ok numberOfLinesInFile($f) == 2;                                              #TnumberOfLinesInFile
  unlink $f;
 }

if (1) {                                                                        # Synopsis

# Print a table:

my $d =
 [[qq(a), qq(b\nbb), qq(c\ncc\nccc\n)],
  [qq(1), qq(1\n22), qq(1\n22\n333\n)],
 ];

my $t = formatTable($d, [qw(A BB CCC)]);

ok $t eq <<END;
   A  BB  CCC
1  a  b   c
      bb  cc
          ccc
2  1   1    1
      22   22
          333
END

# Print a table containing tables and make it into a report:

my $D = [[qq(See the\ntable\nopposite), $t],
         [qq(Or\nthis\none),            $t],
        ];

my $T = formatTable
 ($D,
 [qw(Description Table)],
  head=><<END);
Table of Tables.

Table has 2 rows each of which contains a table.
END

ok nws($T) eq nws(<<END);
Table of Tables.

Table has 2 rows each of which contains a table.

   Description  Table
1  See the         A  BB  CCC
   table        1  a  b   c
   opposite           bb  cc
                          ccc
                2  1   1    1
                      22   22
                          333
2  Or              A  BB  CCC
   this         1  a  b   c
   one                bb  cc
                          ccc
                2  1   1    1
                      22   22
                          333
END

# Print an array of arrays:

my $aa = formatTable
 ([[qw(A   B   C  )],
   [qw(AA  BB  CC )],
   [qw(AAA BBB CCC)],
   [qw(1   22  333)]],
   [qw (aa  bb  cc)]);

ok $aa eq <<END;
   aa   bb   cc
1  A    B    C
2  AA   BB   CC
3  AAA  BBB  CCC
4    1   22  333
END

# Print an array of hashes:

my $ah = formatTable
 ([{aa=> "A",   bb => "B",   cc => "C" },
   {aa=> "AA",  bb => "BB",  cc => "CC" },
   {aa=> "AAA", bb => "BBB", cc => "CCC" },
   {aa=> 1,     bb => 22,    cc => 333 }]);

ok $ah eq <<END;
   aa   bb   cc
1  A    B    C
2  AA   BB   CC
3  AAA  BBB  CCC
4    1   22  333
END

# Print a hash of arrays:

my $ha = formatTable
 ({""     => ["aa",  "bb",  "cc"],
   "1"    => ["A",   "B",   "C"],
   "22"   => ["AA",  "BB",  "CC"],
   "333"  => ["AAA", "BBB", "CCC"],
   "4444" => [1,      22,    333]},
   [qw(Key A B C)]
   );

ok $ha eq <<END;
Key   A    B    C
      aa   bb   cc
   1  A    B    C
  22  AA   BB   CC
 333  AAA  BBB  CCC
4444    1   22  333
END

# Print a hash of hashes:

my $hh = formatTable
 ({a    => {aa=>"A",   bb=>"B",   cc=>"C" },
   aa   => {aa=>"AA",  bb=>"BB",  cc=>"CC" },
   aaa  => {aa=>"AAA", bb=>"BBB", cc=>"CCC" },
   aaaa => {aa=>1,     bb=>22,    cc=>333 }});

ok $hh eq <<END;
      aa   bb   cc
a     A    B    C
aa    AA   BB   CC
aaa   AAA  BBB  CCC
aaaa    1   22  333
END

# Print an array of scalars:

my $a = formatTable(["a", "bb", "ccc", 4], [q(#), q(Col)]);

ok $a eq <<END;
#  Col
0  a
1  bb
2  ccc
3    4
END

# Print a hash of scalars:

my $h = formatTable({aa=>"AAAA", bb=>"BBBB", cc=>"333"}, [qw(Key Title)]);

ok $h eq <<END;
Key  Title
aa   AAAA
bb   BBBB
cc     333
END
}

if (1) {                                                                        #TstringsAreNotEqual  #TshowGotVersusWanted
  ok        !stringsAreNotEqual(q(abc), q(abc));
  ok         stringsAreNotEqual(q(abc), q(abd));
  is_deeply [stringsAreNotEqual(q(abc), q(abd))], [qw(ab c d)];
  is_deeply [stringsAreNotEqual(q(ab),  q(abd))], [q(ab), '', q(d)];
  is_deeply showGotVersusWanted("aaaa\nbbbb\ncccc\ndddd\n",
                                "aaaa\nbbbb\nccee\nffff\n"), <<END;
Comparing wanted with got failed at line: 3, character: 3
Start:
aaaa
bbbb
cc
Want ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ee
ffff

Got  ________________________________________________________________________________

cc
dddd
END
 }

if (1) {                                                                        #TgenHash #TloadHash
  my $o = genHash(q(TestHash),                                                  # Definition of a blessed hash.
      a=>q(aa),                                                                 # Definition of attribute aa.
      b=>q(bb),                                                                 # Definition of attribute bb.
     );
  ok $o->a eq q(aa);
  is_deeply $o, {a=>"aa", b=>"bb"};
  my $p = genHash(q(TestHash),
    c=>q(cc),                                                                   # Definition of attribute cc.
   );
  ok $p->c eq q(cc);
  ok $p->a =  q(aa);
  ok $p->a eq q(aa);
  is_deeply $p, {a=>"aa", c=>"cc"};

  loadHash($p, a=>11, b=>22);                                                   # Load the hash
  is_deeply $p, {a=>11, b=>22, c=>"cc"};

  my $r = eval {loadHash($p, d=>44)};                                           # Try to load the hash
  ok $@ =~ m(Cannot load attribute: d);
 }

if (!onWindows) {

if (1)                                                                          #TnewServiceIncarnation #TData::Exchange::Service::check
 {my $s = newServiceIncarnation("aaa", q(bbb.txt));
  is_deeply $s->check, $s;
  my $t = newServiceIncarnation("aaa", q(bbb.txt));
  is_deeply $t->check, $t;
  ok $t->start >= $s->start+1;
  ok !$s->check(1);
  unlink q(bbb.txt);
 }

if (1)                                                                          #TnewProcessStarter #TData::Table::Text::Starter::start #TData::Table::Text::Starter::finish
 {my $N = 100;
  my $l = q(logFile.txt);
  unlink $l;
  my $s = newProcessStarter(4);
     $s->processingTitle   = q(Test processes);
     $s->totalToBeStarted  = $N;
     $s->processingLogFile = $l;

  for my $i(1..$N)
   {Data::Table::Text::Starter::start($s, sub{$i*$i});
   }

  is_deeply
   [sort {$a <=> $b} Data::Table::Text::Starter::finish($s)],
   [map {$_**2} 1..$N];

  ok readFile($l) =~ m(Finished $N processes for: Test processes)s;
  clearFolder($s->transferArea, 1e3);
  unlink $l;
 }

}

is_deeply arrayToHash(qw(a b c)), {a=>1, b=>1, c=>1};                           #TarrayToHash

if (1)                                                                          #TreloadHashes
 {my $a = bless [bless {aaa=>42}, "AAAA"], "BBBB";
  eval {$a->[0]->aaa};
  ok $@ =~ m(\ACan.t locate object method .aaa. via package .AAAA.);
  reloadHashes($a);
  ok $a->[0]->aaa == 42;
 }

if (1)                                                                          #TreloadHashes
 {my $a = bless [bless {ccc=>42}, "CCCC"], "DDDD";
  eval {$a->[0]->ccc};
  ok $@ =~ m(\ACan.t locate object method .ccc. via package .CCCC.);
  reloadHashes($a);
  ok $a->[0]->ccc == 42;
 }

if (!onWindows) {

if (1) {                                                                        #TwriteFiles #TreadFiles #TcopyFile #TcopyFolder #TmergeFolder #TmoveFileNoClobber #TmoveFileWithClobber
  my $d = temporaryFolder;
  my $a = fpd($d, q(aaa));
  my $b = fpd($d, q(bbb));
  my $c = fpd($d, q(ccc));
  my ($a1, $a2) = map {fpe($a, $_, q(txt))} 1..2;
  my ($b1, $b2) = map {fpe($b, $_, q(txt))} 1..2;
  my $files = {$a1 => "1111", $a2 => "2222"};

  writeFiles($files);
  my $ra = readFiles($a);
  is_deeply $files, $ra;
  copyFolder($a, $b);
  my $rb = readFiles($b);
  is_deeply [sort values %$ra], [sort values %$rb];

  unlink $a2;
  mergeFolder($a, $b);
  ok -e $b1; ok  -e $b2;

  copyFolder($a, $b);
  ok -e $b1; ok !-e $b2;

  copyFile($a1, $a2);
  ok readFile($a1) eq readFile($a2);

  writeFiles($files);
  ok !moveFileNoClobber  ($a1, $a2);
  ok  moveFileWithClobber($a1, $a2);
  ok !-e $a1;
  ok readFile($a2) eq q(1111);
  ok  moveFileNoClobber  ($a2, $a1);
  ok !-e $a2;
  ok readFile($a1) eq q(1111);

  clearFolder(q(aaa), 11);
  clearFolder(q(bbb), 11);
 }

}

if (1)                                                                          #TsetPackageSearchOrder
 {if (1)
   {package AAAA;

    sub aaaa{q(AAAAaaaa)}
    sub bbbb{q(AAAAbbbb)}
    sub cccc{q(AAAAcccc)}
   }
  if (1)
   {package BBBB;

    sub aaaa{q(BBBBaaaa)}
    sub bbbb{q(BBBBbbbb)}
    sub dddd{q(BBBBdddd)}
   }
  if (1)
   {package CCCC;

    sub aaaa{q(CCCCaaaa)}
    sub dddd{q(CCCCdddd)}
    sub eeee{q(CCCCeeee)}
   }

  setPackageSearchOrder(__PACKAGE__, qw(CCCC BBBB AAAA));

  ok &aaaa eq q(CCCCaaaa);
  ok &bbbb eq q(BBBBbbbb);
  ok &cccc eq q(AAAAcccc);

  ok &aaaa eq q(CCCCaaaa);
  ok &bbbb eq q(BBBBbbbb);
  ok &cccc eq q(AAAAcccc);

  ok &dddd eq q(CCCCdddd);
  ok &eeee eq q(CCCCeeee);

  setPackageSearchOrder(__PACKAGE__, qw(AAAA BBBB CCCC));

  ok &aaaa eq q(AAAAaaaa);
  ok &bbbb eq q(AAAAbbbb);
  ok &cccc eq q(AAAAcccc);

  ok &aaaa eq q(AAAAaaaa);
  ok &bbbb eq q(AAAAbbbb);
  ok &cccc eq q(AAAAcccc);

  ok &dddd eq q(BBBBdddd);
  ok &eeee eq q(CCCCeeee);
 }

if (1)
 {my $d = bless
   {a=>1,
    b=>
     [bless({A=>1, B=>2, C=>3}, q(BBBB)),
      bless({A=>5, B=>6, C=>7}, q(BBBB)),
     ],
    c=>bless({A=>1, B=>2, C=>3}, q(CCCC)),
   }, q(AAAA);

  is_deeply showHashes($d),
   {AAAA => { a => 1, b => 1, c => 1 },
    BBBB => { A => 2, B => 2, C => 2 },
    CCCC => { A => 1, B => 1, C => 1 },
   };

  reloadHashes($d);
  ok $d->c->C == 3;
 }

ok swapFilePrefix(q(/aaa/bbb.txt), q(/aaa/), q(/AAA/)) eq q(/AAA/bbb.txt);      #TswapFilePrefix

if (1)                                                                          #ToverrideMethods #TisSubInPackage
 {sub AAAA::Call {q(AAAA)}

  sub BBBB::Call {q(BBBB)}
  sub BBBB::call {q(bbbb)}

  if (1)
   {package BBBB;
    use Test::More;
    *ok = *Test::More::ok;
    *isSubInPackage = *Data::Table::Text::isSubInPackage;
    ok  isSubInPackage(q(AAAA), q(Call));
    ok !isSubInPackage(q(AAAA), q(call));
    ok  isSubInPackage(q(BBBB), q(Call));
    ok  isSubInPackage(q(BBBB), q(call));
    ok Call eq q(BBBB);
    ok call eq q(bbbb);
    &Data::Table::Text::overrideMethods(qw(AAAA BBBB Call call));
    *isSubInPackage = *Data::Table::Text::isSubInPackage;
    ok  isSubInPackage(q(AAAA), q(Call));
    ok  isSubInPackage(q(AAAA), q(call));
    ok  isSubInPackage(q(BBBB), q(Call));
    ok  isSubInPackage(q(BBBB), q(call));
    ok Call eq q(AAAA);
    ok call eq q(bbbb);
    package AAAA;
    use Test::More;
    *ok = *Test::More::ok;
    ok  Call eq q(AAAA);
    ok &call eq q(bbbb);
   }
 }

#eval {readFile($f)};                                                           # Fails to fail in the following section on a number of operating systems
#ok $@, "readFile";

if (1)                                                                          #ToverWriteBinaryFile #TwriteBinaryFile #TcopyBinaryFile
 {vec(my $a, 0, 8) = 254;
  vec(my $b, 0, 8) = 255;
  ok dump($a) eq dump("\xFE");
  ok dump($b) eq dump("\xFF");
  ok length($a) == 1;
  ok length($b) == 1;

  my $s = $a.$a.$b.$b;
  ok length($s) == 4;

  my $f = eval {writeFile(undef, $s)};
  ok fileSize($f) == 8;

  eval {writeBinaryFile($f, $s)};
  ok $@ =~ m(Binary file already exists:)s;

  eval {overWriteBinaryFile($f, $s)};
  ok !$@;
  ok fileSize($f) == 4;

  ok $s eq eval {readBinaryFile($f)};

  copyBinaryFile($f, my $F = temporaryFile);
  ok $s eq readBinaryFile($F);
  unlink $f, $F;
 }

is_deeply [parseFileName(q(/home/phil/r/aci2/out/.ditamap))],
          ["/home/phil/r/aci2/out/", "", "ditamap"];

ok relFromAbsAgainstAbs
 ("/home/phil/r/aci2/out/audit_events.xml",
  "/home/phil/r/aci2/out/.ditamap") eq "audit_events.xml";


if (1) {                                                                        #TmergeHashesBySummingValues
  is_deeply +{a=>1, b=>2, c=>3},
    mergeHashesBySummingValues
      +{a=>1,b=>1, c=>1}, +{b=>1,c=>1}, +{c=>1};
 }

if (1) {                                                                        #TsquareArray #TdeSquareArray #TrectangularArray #TrectangularArray2
  is_deeply [squareArray @{[1..4]} ], [[1, 2], [3, 4]];
  is_deeply [squareArray @{[1..22]}],
   [[1 .. 5], [6 .. 10], [11 .. 15], [16 .. 20], [21, 22]];

  is_deeply [1..$_], [deSquareArray squareArray @{[1..$_]}] for 1..22;
  ok $_ == countSquareArray         squareArray @{[1..$_]}  for 222;

  is_deeply [rectangularArray(3, 1..11)],
            [[1, 4, 7, 10],
             [2, 5, 8, 11],
             [3, 6, 9]];

  is_deeply [rectangularArray(3, 1..12)],
            [[1, 4, 7, 10],
             [2, 5, 8, 11],
             [3, 6, 9, 12]];

  is_deeply [rectangularArray(3, 1..13)],
            [[1, 4, 7, 10, 13],
             [2, 5, 8, 11],
             [3, 6, 9, 12]];

  is_deeply [rectangularArray2(3, 1..5)],
            [[1, 2, 3],
             [4, 5]];

  is_deeply [rectangularArray2(3, 1..6)],
            [[1, 2, 3],
             [4, 5, 6]];

  is_deeply [rectangularArray2(3, 1..7)],
            [[1, 2, 3],
             [4, 5, 6],
             [7]];
 }

if (1) {                                                                        #TsummarizeColumn
  is_deeply
   [summarizeColumn([map {[$_]} qw(A B D B C D C D A C D C B B D)], 0)],
   [[5, "D"], [4, "B"], [4, "C"], [2, "A"]];

  ok nws(formatTable
   ([map {[split m//, $_]} qw(AA CB CD BC DC DD CD AD AA DC CD CC BB BB BD)],
    [qw(Col-1 Col-2)],
     summarize=>1)) eq nws(<<'END');

Summary_of_column                - Count of unique values found in each column                     Use the Geany flick capability by placing your cursor on the first word
Comma_Separated_Values_of_column - Comma separated list of the unique values found in each column  of these lines and pressing control + down arrow to see each sub report.

    Col-1  Col-2
 1  A      A
 2  C      B
 3  C      D
 4  B      C
 5  D      C
 6  D      D
 7  C      D
 8  A      D
 9  A      A
10  D      C
11  C      D
12  C      C
13  B      B
14  B      B
15  B      D

Summary_of_column_Col-1
   Count  Col-1
1      5  C
2      4  B
3      3  A
4      3  D

Comma_Separated_Values_of_column_Col-1: "A","B","C","D"

Summary_of_column_Col-2
   Count  Col-2
1      6  D
2      4  C
3      3  B
4      2  A

Comma_Separated_Values_of_column_Col-2: "A","B","C","D"
END
 }


if (0) {                                                                        #TisFileUtf8
  my $f = writeFile(undef, "aaa");
  ok isFileUtf8 $f;
 }

if (0) {                                                                        # Needs direct testing on multiple systems - takes too long for normal testing
if (1) {                                                                        #TnewUdsrServer #TnewUdsrClient #TUdsr::read #TUdsr::write #TUdsr::kill
  my $N = 20;
  my $s = newUdsrServer(serverAction=>sub
   {my ($u) = @_;
    my $r = $u->read;
    $u->write(qq(Hello from server $r));
   });

  my $p = newProcessStarter(min(100, $N));                                      # Run some clients
  for my $i(1..$N)
   {$p->start(sub
     {my $count = 0;
      for my $j(1..$N)
       {my $c = newUdsrClient;
        my $m = qq(Hello from client $i x $j);
        $c->write($m);
        my $r = $c->read;
        ++$count if $r eq qq(Hello from server $m);
       }
      [$count]
     });
   }

  my $count;
  for my $r($p->finish)                                                         # Consolidate results
   {my ($c) = @$r;
    $count += $c;
   }

  ok $count == $N*$N;                                                           # Check results and kill
  $s->kill;
 }
 }
else
 {ok 1 for 1..1;
 }

if (1) {                                                                        #TguidFromMd5 #TguidFromString #TfileMd5Sum #TstringMd5Sum #Tmd5FromGuid
  my $s = join '', 1..100;
  my $m = q(ef69caaaeea9c17120821a9eb6c7f1de);

  ok stringMd5Sum($s) eq $m;

  my $f = writeFile(undef, $s);
  ok fileMd5Sum($f) eq $m;
  unlink $f;

  ok guidFromString(join '', 1..100) eq
     q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

  ok guidFromMd5(stringMd5Sum(join('', 1..100))) eq
     q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de);

  ok md5FromGuid(q(GUID-ef69caaa-eea9-c171-2082-1a9eb6c7f1de)) eq
                      q(ef69caaaeea9c17120821a9eb6c7f1de);

  ok stringMd5Sum(q(𝝰 𝝱 𝝲)) eq q(3c2b7c31b1011998bd7e1f66fb7c024d);
}

if (1)
 {ok arraySum   (1..10) ==  55;                                                 #TarraySum
  ok arrayProduct(1..5) == 120;                                                 #TarrayProduct
  is_deeply[arrayTimes(2, 1..5)], [qw(2 4 6 8 10)];                             #TarrayTimes
 }

if (1)
 {ok indexOfMax(qw(2 3 1 2)) == 1;                                              #TindexOfMax
  ok indexOfMin(qw(2 3 1 2)) == 2;                                              #TindexOfMin
 }

if (1)
 {is_deeply [arrayTimes(2, 1..3)], [2,4,6];
 }

if (1)                                                                          #TcountOccurencesInString
 {ok countOccurencesInString(q(a<b>c<b><b>d), q(<b>)) == 3;
 }

if (1)                                                                          #TfileInWindowsFormat
 {ok fileInWindowsFormat(fpd(qw(/a b c d))) eq q(\a\b\c\d\\);
 }


if (1) {                                                                        #TformattedTablesReport
  @formatTables = ();

  for my $m(2..8)
   {formatTable([map {[$_, $_*$m]} 1..$m], [q(Single), qq(* $m)],
      title=>qq(Multiply by $m));
   }

  ok nws(formattedTablesReport) eq nws(<<END);
   Rows  Title          File
1     2  Multiply by 2
2     3  Multiply by 3
3     4  Multiply by 4
4     5  Multiply by 5
5     6  Multiply by 6
6     7  Multiply by 7
7     8  Multiply by 8
END
 }

if (1) {
  my $t = [[qw(a b c)], [1..3], [4..6]];

  ok nws(formatTable($t, [qw(A B C)], summarize=>1, csv=>q(/dev/null))) eq nws(<<END)
Summary_of_column                - Count of unique values found in each column                     Use the Geany flick capability by placing your cursor on the first word
Comma_Separated_Values_of_column - Comma separated list of the unique values found in each column  of these lines and pressing control + down arrow to see each sub report.
   A  B  C
1  a  b  c
2  1  2  3
3  4  5  6
Summary_of_column_A
   Count  A
1      1  1
2      1  4
3      1  a
Comma_Separated_Values_of_column_A: "a",1,4
Summary_of_column_B
   Count  B
1      1  2
2      1  5
3      1  b
Comma_Separated_Values_of_column_B: "b",2,5
Summary_of_column_C
   Count  C
1      1  3
2      1  6
3      1  c
Comma_Separated_Values_of_column_C: "c",3,6
END
 }

if (1) {                                                                        #TasciiToHexString #ThexToAsciiString
  ok asciiToHexString("Hello World!") eq                  "48656c6c6f20576f726c6421";
  ok                  "Hello World!"  eq hexToAsciiString("48656c6c6f20576f726c6421");
 }

if (1) {                                                                        #TswapFolderPrefix
  my $g = fpd(qw(a b c d));
  my $h = fpd(qw(a b cc dd));
  my $i = fpe($g, qw(aaa txt));

  my $j = swapFolderPrefix($i, $g, $h);
  ok $j =~ m(a/b/cc/dd/)s     unless onWindows;
  ok $j =~ m(a\\b\\cc\\dd\\)s if     onWindows;
 }

if (0) {
reportSettings($0);                                                             #TreportSettings
ddd "Hello";                                                                    #Tddd
fff __LINE__, __FILE__, "Hello world";                                          #Tfff
lll "Hello world";                                                              #Tlll
mmm "Hello world";                                                              #Tmmm
}

if (1) {
  #local $formatTableWithZwSp = 1;
  my $d = [[qw(a 1)], [qw(bb 22)], [qw(ccc 333)], [qw(dddd 4444)]];
  ok nws(formatTableBasic($d)) eq nws(<<END);
a        1
bb      22
ccc    333
dddd  4444
END
  }


if (1) {                                                                        #TnameFromString #TnameFromStringRestrictedToTitle
ok q(help) eq nameFromString(q(!@#$%^help___<>?><?>));
ok q(bm_The_skyscraper_analogy) eq nameFromString(<<END);
<bookmap id="b1">
<title>The skyscraper analogy</title>
</bookmap>
END

ok q(bm_The_skyscraper_analogy_An_exciting_tale_of_two_skyscrapers_that_meet_in_downtown_Houston)
   eq nameFromString(<<END);
<bookmap id="b1">
<title>The skyscraper analogy</title>
An exciting tale of two skyscrapers that meet in downtown Houston
<concept><html>
</bookmap>
END

ok q(bm_the_skyscraper_analogy) eq nameFromStringRestrictedToTitle(<<END);
<bookmap id="b1">
<title>The skyscraper analogy</title>
An exciting tale of two skyscrapers that meet in downtown Houston
<concept><html>
</bookmap>
END
 }

if (0) {                                                                        #TcopyFileMd5Normalized #TcopyFileMd5NormalizedGetCompanionContent #TcopyFileMd5NormalizedCreate #TcopyFileMd5NormalizedDelete
  my $dir = temporaryFolder;
  my $a = fpe($dir, qw(a a jpg));
  my $b = fpe($dir, qw(b a jpg));
  my $c = fpe($dir, qw(c a jpg));

  my $content = join '', 1..1e3;

  my $A = copyFileMd5NormalizedCreate($a, $content, q(jpg), $a);
  ok readFile($A) eq $content;
  ok $A eq copyFileMd5Normalized($A);

  my $B = copyFileMd5Normalized($A, $b);
  ok readFile($B) eq $content;
  ok $B eq copyFileMd5Normalized($B);

  my $C = copyFileMd5Normalized($B, $c);
  ok readFile($C) eq $content;
  ok $C eq copyFileMd5Normalized($C);

  ok fne($A) eq fne($_) for $B, $C;
  ok readFile($_) eq $content for $A, $B, $C;
  ok copyFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;

  ok 6 == searchDirectoryTreesForMatchingFiles($dir);
  copyFileMd5NormalizedDelete($A);
  ok 4 == searchDirectoryTreesForMatchingFiles($dir);
  copyFileMd5NormalizedDelete($B);
  ok 2 == searchDirectoryTreesForMatchingFiles($dir);
  copyFileMd5NormalizedDelete($C);
  ok 0 == searchDirectoryTreesForMatchingFiles($dir);

  clearFolder($dir, 10);
  ok 0 == searchDirectoryTreesForMatchingFiles($dir);
 }

if (0) {                                                                        #TcopyBinaryFileMd5Normalized #TcopyBinaryFileMd5NormalizedGetCompanionContent #TcopyBinaryFileMd5NormalizedCreate
  my $dir = temporaryFolder;
  my $a = fpe($dir, qw(a a jpg));
  my $b = fpe($dir, qw(b a jpg));
  my $c = fpe($dir, qw(c a jpg));

  my $content = join '', 1..1e3;

  my $A = copyBinaryFileMd5NormalizedCreate($a, $content, q(jpg), $a);
  ok readBinaryFile($A) eq $content;
  ok $A eq copyBinaryFileMd5Normalized($A);

  my $B = copyBinaryFileMd5Normalized($A, $b);
  ok readBinaryFile($B) eq $content;
  ok $B eq copyBinaryFileMd5Normalized($B);

  my $C = copyBinaryFileMd5Normalized($B, $c);
  ok readBinaryFile($C) eq $content;
  ok $C eq copyBinaryFileMd5Normalized($C);

  ok fne($A) eq fne($_) for $B, $C;
  ok readBinaryFile($_) eq $content for $A, $B, $C;
  ok copyBinaryFileMd5NormalizedGetCompanionContent($_) eq $a for $A, $B, $C;

  ok 6 == searchDirectoryTreesForMatchingFiles($dir);
  clearFolder($dir, 10);
 }

ok setFileExtension(q(.c),     q(d)) eq q(.d);                                  #TsetFileExtension
ok setFileExtension(q(b.c),    q(d)) eq q(b.d);                                 #TsetFileExtension
ok setFileExtension(q(/a/b.c), q(d)) eq q(/a/b.d);                              #TsetFileExtension

if (0) {
  ok awsTranslateText("Hello", "it", ".translations/") eq q(Ciao);              #TawsTranslateText
 }

if (1) {                                                                        #TuniqueNameFromFile
  my $f = owf(q(test.txt), join "", 1..100);
  ok uniqueNameFromFile($f) eq q(test_ef69caaaeea9c17120821a9eb6c7f1de.txt);
  unlink $f;
 }

if (1) {                                                                        #TnameFromFolder
  ok nameFromFolder(fpe(qw( a b c d e))) eq q(c);
 }

if (1) {                                                                        #TparseDitaRef
  is_deeply [parseDitaRef(q(a#b/c))], [qw(a b c)];
  is_deeply [parseDitaRef(q(a#./c))], [q(a), q(), q(c)];
  is_deeply [parseDitaRef(q(a#/c))],  [q(a), q(), q(c)];
  is_deeply [parseDitaRef(q(a#c))],   [q(a), q(), q(c)];
  is_deeply [parseDitaRef(q(#b/c))],  [q(),  qw(b c)];
  is_deeply [parseDitaRef(q(#b))],    [q(),  q(), q(b)];
  is_deeply [parseDitaRef(q(#./c))],  [q(),  q(), q(c)];
  is_deeply [parseDitaRef(q(#/c))],   [q(),  q(), q(c)];
  is_deeply [parseDitaRef(q(#c))],    [q(),  q(), q(c)];
 }

if (1) {                                                                        #TjavaScriptExports
  ok javaScriptExports(<<END) eq <<END;
function aaa()            //E
 {console.log('aaa');
 }

//EEEEEEEEE

function bbb()            //E
 {console.log('bbb');
 }
END

function aaa()            //E
 {console.log('aaa');
 }

function bbb()            //E
 {console.log('bbb');
 }
END
 }

if (0) {                                                                        #TcopyFileMd5NormalizedName
  ok copyFileMd5NormalizedName(<<END, q(txt)) eq
<p>Hello<b>World</b></p>
END
q(Hello_World_6ba23858c1b4811660896c324acac6fa.txt);
 }

if (!-e q(recursion)) {                                                         # Prevent recursion while testing
  my $recursion = createEmptyFile(q(recursion));
if (0) {                                                                        #TreportAttributes #TreportReplacableMethods #TreportExportableMethods #TreportAttributeSettings
  my $d = temporaryFile;

  my $f = writeFile(undef, <<'END'.<<END2);
#!perl -I/home/phil/perl/cpan/DataTableText/lib/
use Data::Table::Text qw(reportAttributeSettings);
sub attribute {1}                                                               # An attribute
sub replaceable($)                                                              #r A replaceable method
 {
 }
sub exportable($)                                                               #e An exportable method
 {
 }
END
reportAttributeSettings(q($d));
END2

  is_deeply reportAttributes($f),        {attribute  =>"An attribute"};
  is_deeply reportReplacableMethods($f), {replaceable=>"A replaceable method"};
  is_deeply reportExportableMethods($f), {exportable =>"An exportable method"};
  qx(perl $f);
  ok readFile($d) =~ m(attribute\s*1\s*An attribute);
  unlink $d, $f;
 }
  unlink $recursion;
 }

ok  fullyQualifiedFile(q(/a/b/c.d));                                            #TfullyQualifiedFile
ok  fullyQualifiedFile(q(/a/b/c.d), q(/a/b));                                   #TfullyQualifiedFile
ok !fullyQualifiedFile(q(/a/b/c.d), q(/a/c));                                   #TfullyQualifiedFile
ok !fullyQualifiedFile(q(c.d));                                                 #TfullyQualifiedFile

if (!onWindows) {

if (1)                                                                          #TsetPermissionsForFile
 {my $f = temporaryFile();
  setPermissionsForFile($f, q(ugo=r));
  my $a = qx(ls -la $f);
  ok $a =~ m(-r--r--r--)s;
  setPermissionsForFile($f, q(u=rwx));
  my $b = qx(ls -la $f);
  ok $b =~ m(-rwxr--r--)s;
 }

}

if (0)                                                                          #TcopyFileToRemote #TcopyFileFromRemote #TcopyFolderToRemote #TmergeFolderFromRemote
 {copyFileToRemote     (q(/home/phil/perl/cpan/aaa.txt));
  copyFileFromRemote   (q(/home/phil/perl/cpan/aaa.txt));
  copyFolderToRemote   (q(/home/phil/perl/cpan/));
  mergeFolderFromRemote(q(/home/phil/perl/cpan/));
 }

if (0)                                                                          #TfullyQualifyFile
 {ok fullyQualifyFile(q(perl/cpan)) eq q(/home/phil/perl/cpan/);
 }

if (0)                                                                          #Txxxr
 {ok xxxr q(pwd);
 }

if (0)                                                                          #TUdsr::webUser
 {my $fold = fpd(qw(/home phil zzz));                                           # Folder to contain server code
  my $name = q(test);                                                           # Service
  my $user = q(phil);                                                           # User

  my $udsr = newUdsr                                                            # Create a Udsr parameter list
   (serviceName => $name,
    serviceUser => $user,
    socketPath  => qq(/home/phil/$name.socket),
    serverAction=> <<'END'
my $user = userId;
my $list = qx(ls -l);
my $dtts = dateTimeStamp;
return <<END2;
Content-type: text/html

<h1>Hello World to you $user on $dtts!</h1>

<pre>
$list
</pre>
END2
END
   );

  Udsr::webUser($udsr, $fold);                                                  # Create and install web service interface
  my $ip = awsIp;
  say STDERR qx(curl http://$ip/cgi-bin/$name/client.pl);                       # Enable port 80 on AWS first
 }

if (!onWindows) {

if (1) {                                                                        #TrunInSquareRootParallel #TrunInParallel
  my @N = 1..100;
  my $N = 100;
  my $R = 0; $R += $_*$_ for 1..$N;

  ok 338350 == $R;

  ok $R == runInSquareRootParallel
     (4,
      sub {my ($p) = @_; $p * $p},
      sub {my $p = 0; $p += $_ for @_; $p},
      @{[1..$N]}
     );

  ok $R == runInParallel
     (4,
      sub {my ($p) = @_; $p * $p},
      sub {my $p = 0; $p += $_ for @_; $p},
      @{[1..$N]}
     );
 }

}

if (0) {                                                                        #TawsCurrentIp #TconfirmHasCommandLineCommand
  awsCurrentIp;
  confirmHasCommandLineCommand(q(find));
 }

if (1) {                                                                        #TexpandWellKnownUrlsInHtmlFormat #TexpandWellKnownUrlsInDitaFormat #TexpandWellKnownUrlsInPerlFormat #TexpandWellKnownUrlsInHtmlFromPerl #TexpandWellKnownUrlsInPod2Html #TexpandWellKnownWordsAsUrlsInHtmlFormat #TexpandWellKnownWordsAsUrlsInMdFormat
  ok expandWellKnownUrlsInDitaFormat(q(L[github])) eq
    q(<xref scope="external" format="html" href="https://github.com/philiprbrenan">GitHub</xref>);

  ok expandWellKnownUrlsInHtmlFormat(q(L[github])) eq
    q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

  ok expandWellKnownUrlsInPerlFormat(q(L<github>)) eq
    q(L<GitHub|https://github.com/philiprbrenan>);

  ok expandWellKnownUrlsInPerlFormat(q(github))    eq q(github);

  ok expandWellKnownUrlsInHtmlFromPerl(q(L<github>)) eq
    q(<a format="html" href="https://github.com/philiprbrenan">GitHub</a>);

  is_deeply expandWellKnownWordsAsUrlsInHtmlFormat(q(go to gitHub and press w[enter].)),
    q(go to <a href="https://github.com/philiprbrenan">GitHub</a> and press enter.), 'ex1';

  is_deeply expandWellKnownWordsAsUrlsInMdFormat(q(go to gitHub and press w[enter].)),
    q(go to [GitHub](https://github.com/philiprbrenan) and press enter.), 'ex2';

  ok expandWellKnownUrlsInPod2Html(<<END) eq eval '"aaa\n\n=begin HTML\n\n<aaaa format=\"html\" href=\"https://github.com/philiprbrenan\">GitHub</aaaa>\n\n=end   HTML\n\n\nbbb\n"';
aaa L<github> bbb
END
 }

if (1) {                                                                        #TformatTableAA
 ok formatTable
  ([[1,1,1],[1,1,2],[1,2,2],[1,2,3]], [], clearUpLeft=>1) eq <<END;             # Clear matching columns

1  1  1  1
2        2
3     2  2
4        3
END
 }

if ($^O =~ m(\Alinux\Z)) {
if (1) {                                                                        #TfileLargestSize #TfolderSize #TprocessFilesInParallel #TprocessSizesInParallel #TprocessJavaFilesInParallel
  my $d = temporaryFolder;
  my @f = map {owf(fpe($d, $_, q(txt)), 'X' x ($_ ** 2 % 11))} 1..9;

  my $f = fileLargestSize(@f);
  ok fn($f) eq '3', 'aaa';

#  my $b = folderSize($d);                                                       # Needs du
#  ok $b > 0, 'bbb';

  my $c = processFilesInParallel(
    sub
     {my ($file) = @_;
      [&fileSize($file), $file]
     },
    sub
     {scalar @_;
     }, (@f) x 12);

  ok 108 == $c, 'cc11';

  my $C = processSizesInParallel
    sub
     {my ($file) = @_;
      [&fileSize($file), $file]
     },
    sub
     {scalar @_;
     }, map {[fileSize($_), $_]} (@f) x 12;

  ok 108 == $C, 'cc2';

  my $J = processJavaFilesInParallel
    sub
     {my ($file) = @_;
      [&fileSize($file), $file]
     },
    sub
     {scalar @_;
     }, (@f) x 12;

  ok 108 == $J, 'cc3';

  clearFolder($d, 12);
 }
}
else
 {ok 1 for 1..4
 }

ok numberOfCpus(8) >= 8, 'ddd';                                                 #TnumberOfCpus

ok replaceStringWithString(q(abababZ), q(ab), q(c)) eq q(cccZ), 'eee';          #TreplaceStringWithString

ok formatString(<<END, 16) eq  <<END, 'fff';                                    #TformatString
Now is the time for all
good men to come to the rescue
of the ailing B<party>.
END
Now is the time
for all good men
to come to the
rescue of the
ailing 𝗽𝗮𝗿𝘁𝘆.
END

if (0) {
  ok awsMetaData(q(instance-id))    eq q(i-06a4b221b30bf7a37);                  #TawsMetaData
  ok awsCurrentIp                   eq q(31.41.59.26);                          #TawsCurrentIp
  ok awsCurrentInstanceId           eq q(i-06a4b221b30bf7a37);                  #TawsCurrentInstanceId
  ok awsCurrentRegion               eq q(us-east-2);                            #TawsCurrentRegion
  ok awsCurrentAvailabilityZone     eq q(us-east-2a);                           #TawsCurrentAvailabilityZone
     awsEc2CreateImage(q(099 Gold));                                            #TawsEc2CreateImage
     downloadGitHubPublicRepo(q(philiprbrenan), q(psr));                        #TdownloadGitHubPublicRepo
  ok awsCurrentInstanceType         eq q(r4.4xlarge);                           #TawsCurrentInstanceType
  ok overrideAndReabsorbMethods(qw(main Edit::Xml Data::Edit::Xml));            #ToverrideAndReabsorbMethods
 }

is_deeply [1..5], [flattenArrayAndHashValues([1], [[2]], {a=>3, b=>[4, [5]]})], 'ggg'; #TflattenArrayAndHashValues

if (0)                                                                          #TsyncFromS3InParallel #TsyncToS3InParallel
 {syncFromS3InParallel 1e5,
    q(xxx/originals3/),
    q(/home/phil/xxx/),
    q(phil), q(--quiet);

  syncToS3InParallel 1e5,
    q(/home/phil/xxx/),
    q(xxx/originals3/),
    q(phil), q(--quiet);
 }


ok detagString(q(<a><a href="aaaa">a </a><a/>b </a>c)) eq q(a b c), 'hhh';      #TdetagString

if (1)                                                                          #TpartitionStringsOnPrefixBySize
 {my $ps = \&partitionStringsOnPrefixBySize;

  is_deeply {&$ps(1)}, {};
  is_deeply {&$ps(1, 1=>0)},      {q()=>0};
  is_deeply {&$ps(1, 1=>1)},      {q()=>1};
  is_deeply {&$ps(1, 1=>2)},      {1=>2};
  is_deeply {&$ps(1, 1=>1,2=>1)}, {1=>1,2=>1};
  is_deeply {&$ps(2, 11=>1,12=>1, 21=>1,22=>1)}, {1=>2, 2=>2};
  is_deeply {&$ps(2, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>1)}, { 111 => 1, 112 => 1, 113 => 1, 121 => 1, 122 => 1, 123 => 1, 131 => 1, 132 => 1, 133 => 1 };

  for(3..8)
   {is_deeply {&$ps($_, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>1)}, { 11 => 3, 12 => 3, 13 => 3 };
   }

  is_deeply {&$ps(9, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>1)}, { q()=> 9};
  is_deeply {&$ps(3, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>2)}, { 11 => 3, 12 => 3, 131 => 1, 132 => 1, 133 => 2 };
  is_deeply {&$ps(4, 111=>1,112=>1,113=>1, 121=>1,122=>1,123=>1, 131=>1,132=>1,133=>2)}, { 11 => 3, 12 => 3, 13 => 4 };

 }

if (1)                                                                          #TparseS3BucketAndFolderName
 {is_deeply [parseS3BucketAndFolderName(q(s3://bbbb/ffff/dddd/))], [qw(bbbb ffff/dddd/)], q(iii);
  is_deeply [parseS3BucketAndFolderName(q(s3://bbbb/))],           [qw(bbbb), q()];
  is_deeply [parseS3BucketAndFolderName(q(     bbbb/))],           [qw(bbbb), q()];
  is_deeply [parseS3BucketAndFolderName(q(     bbbb))],            [qw(bbbb), q()];
 }

if (1)                                                                          #TunionOfHashesAsArrays #TintersectionOfHashesAsArrays
 {is_deeply  unionOfHashesAsArrays
   ({a=>1,b=>2}, {b=>1,c=>1}, {c=>2}),
    {a=>[1], b=>[2,1], c=>[undef,1,2]};

  is_deeply  intersectionOfHashesAsArrays
   ({a=>1,b=>2},{b=>1,c=>1},{b=>3,c=>2}),
    {b=>[2,1,3]};
 }

if (1)                                                                          #TunionOfHashKeys #TintersectionOfHashKeys
 {is_deeply  unionOfHashKeys
   ({a=>1,b=>2}, {b=>1,c=>1}, {c=>2}),
    {a=>1, b=>2, c=>2};

  is_deeply  intersectionOfHashKeys
   ({a=>1,b=>2},{b=>1,c=>1},{b=>3,c=>2}),
    {b=>1};
 }

if (1)                                                                          #TremoveFilePathsFromStructure #TwriteStructureTest
 {my $d = {"/home/aaa/bbb.txt"=>1, "ccc/ddd.txt"=>2, "eee.txt"=>3};
  my $D = removeFilePathsFromStructure($d);

  is_deeply removeFilePathsFromStructure($d),
   {"bbb.txt"=>1, "ddd.txt"=>2, "eee.txt"=>3};

  ok writeStructureTest($d, q($d)) eq <<'END';
  is_deeply removeFilePathsFromStructure($d),
   { "bbb.txt" => 1, "ddd.txt" => 2, "eee.txt" => 3 };
END
 }

if (1)                                                                          #TformatHtmlTable
 {my $t = formatHtmlTable
   ([
      [qw(1 a)],
      [qw(2 b)],
    ],
   title  => q(Sample html table),
   head   => q(Head NNNN rows),
   foot   => q(Footer),
   columns=> <<END,
source The source number
target The target letter
END
   );

  my $T = <<'END';
<h1>Sample html table</h1>

<p>Head 2 rows</p>

<p><table borders="0" cellpadding="10" cellspacing="5">

<tr><th><span title="The source number">source</span><th><span title="The target letter">target</span>
<tr><td>1<td>a
<tr><td>2<td>b
</table></p>

<p><pre>
source  The source number
target  The target letter

</pre></p>

<p>Footer</p>

<span class="options" style="display: none">{
  columns => "source The source number\ntarget The target letter\n",
  foot    => "Footer",
  head    => "Head NNNN rows",
  rows    => 2,
  title   => "Sample html table",
}</span>
END

  ok "$t\n" eq $T;
 }

if (!onWindows) {

if (1)                                                                          #TformatHtmlAndTextTables #TformatHtmlTablesIndex #TformatHtmlAndTextTablesWaitPids
 {my $reports = temporaryFolder;

  formatHtmlAndTextTables
   ($reports, $reports, q(/cgi-bin/getFile.pl?), q(/a/),
     [[qw(1 /a/a)],
      [qw(2 /a/b)],
     ],
   title   => q(Bad files),
   head    => q(Head NNNN rows),
   foot    => q(Footer),
   file    => q(bad.html),
   facet   => q(files), aspectColor => "red",
   columns => <<END,
source The source number
target The target letter
END
   );

  formatHtmlAndTextTables
   ($reports, $reports, q(/cgi-bin/getFile.pl?file=), q(/a/),
     [[qw(1 /a/a1)],
      [qw(2 /a/b2)],
      [qw(3 /a/b3)],
     ],
   title   => q(Good files),
   head    => q(Head NNNN rows),
   foot    => q(Footer),
   file    => q(good.html),
   facet   => q(files), aspectColor => "green",
   columns => <<END,
source The source number
target The target letter
END
   );

  formatHtmlAndTextTablesWaitPids;

  my $result = formatHtmlTablesIndex($reports, q(TITLE), q(/cgi-bin/getFile.pl?file=));
  ok $result =~ m(3.*Good files);
  ok $result =~ m(2.*Bad files);
#  ok $result =~ m(green.*>3<.*>Good files);
#  ok $result =~ m(red.*>2<.*>Bad files);

  clearFolder($reports, 11);
 }

}

if (1)                                                                          #TparseIntoWordsAndStrings
 {is_deeply
   [parseIntoWordsAndStrings(
q( aa12!    a'b   "aa !! ++ bb"  '  ',      '"'  "'"  ""   ''.))  ],
 ["aa12!", "a'b", "aa !! ++ bb", "  ", ",", '"', "'", "",  "", '.'];
 }

if (1)                                                                          #TparseXmlDocType
 {is_deeply parseXmlDocType(<<END),
<!DOCTYPE reference PUBLIC "-//OASIS//DTD DITA Reference//EN" "reference.dtd">
...
END
   {localDtd => "reference.dtd",
    public   => 1,
    publicId => "-//OASIS//DTD DITA Reference//EN",
    root     => "reference",
   };

  is_deeply parseXmlDocType(<<END),
...
<!DOCTYPE concept PUBLIC "-//OASIS//DTD DITA Task//EN" "concept.dtd" []>
...
)),
END
     {localDtd => "concept.dtd",
      public   => 1,
      publicId => "-//OASIS//DTD DITA Task//EN",
      root     => "concept",
     };
 }

if (0) {                                                                        #TdownloadGitHubPublicRepoFile
  ok &downloadGitHubPublicRepoFile(qw(philiprbrenan pleaseChangeDita index.html));
 }

if (1) {                                                                        #TcopyFileToFolder
  my $sd = temporaryFolder;
  my $td = temporaryFolder;
  my $sf = writeFile fpe($sd, qw(test data)), q(aaaa);
  my $tf = copyFileToFolder($sf, $td);
  ok readFile($tf) eq q(aaaa);
  ok fp ($tf) eq $td;
  ok fne($tf) eq q(test.data);
 }

if (1) {                                                                        #TexpandNewLinesInDocumentation
ok expandNewLinesInDocumentation(q(a\m  b\n  c\n)) eq <<END;
a

  b
  c
END
 }

if (0) {                                                                        #TchildPids
  is_deeply [childPids(2702)], [2702..2705];
 }

if (0) {                                                                        #Ts3ZipFolder
  s3ZipFolder(q(home/phil/r/), q(s3://bucket/r.zip));
 }

if (0) {                                                                        #Ts3ListFilesAndSizes #Ts3ReadFile #Ts3WriteFile #Ts3WriteString #Ts3FileExists #Ts3ReadString #Ts3DownloadFolder #Ts3ZipFolder #Ts3ZipFolders
  my %options = (profile => q(fmc));

  s3DownloadFolder
   (q(s3://bucket/folder/), q(home/phil/s3/folder/), %options, delete=>1);

  s3ZipFolder ( q(home/phil/s3/folder/) => q(s3://bucket/folder/),  %options);

  s3ZipFolders({q(home/phil/s3/folder/) => q(s3://bucket/folder/)}, %options);

  is_deeply
   {s3ListFilesAndSizes(q(s3://salesforce.dita/originals4/images), %options)
   },
   {"s3://salesforce.dita/originals4/images/business_plan_sections.png" =>
     ["originals4/images/business_plan_sections.png",
      112525,
      "2019-08-13",
      "20:01:10",
     ],
    "s3://salesforce.dita/originals4/images/non-referenced.png" =>
     ["originals4/images/non-referenced.png",
      19076,
      "2019-08-20",
      "01:25:04",
     ],
   };

  my $data = q(0123456789);
  my $file = q(s3://salesforce.dita/zzz/111.txt);

  if (1)
   {       s3WriteString($file, $data, %options);
    my $r = s3ReadString($file,        %options);
    ok $r eq $data;
   }

  if (1)
   {my @r = s3FileExists($file, %options);
    ok $r[0] eq "zzz/111.txt";
    ok $r[1] ==  10;
   }

  if (1)
   {my $d = $data x 2;
    my $f = writeFile(undef, $d);

    s3WriteFile($file, $f, %options);
    unlink $f;
    s3ReadFile ($file, $f, %options);
    ok readFile($f) eq $d;
    unlink $f;
   }
 }

if (1) {                                                                        #TdeduplicateSequentialWordsInString
  ok deduplicateSequentialWordsInString(<<END) eq qq(\(aa \[bb \-cc dd ee\n);
(aa [bb bb -cc cc dd dd dd dd ee ee ee ee
END
 }

ok q(a) eq chooseStringAtRandom(qw(a a a a));                                   #TchooseStringAtRandom
is_deeply [randomizeArray(qw(a a a a))], [qw(a a a a)];                           #TrandomizeArray

if (0) {                                                                        #TawsEc2DescribeInstances #TawsEc2DescribeInstancesGetIPAddresses
  my %options = (region => q(us-east-2), profile=>q(fmc));
  my $r = awsEc2DescribeInstances              (%options);
  my %i = awsEc2DescribeInstancesGetIPAddresses(%options);
  is_deeply \%i, { "i-068a7176ba9140057" => { "18.221.162.39" => 1 } };
 }

if (0) {                                                                        #TawsEc2DescribeInstanceType
  my $i = awsEc2DescribeInstanceType
   ("m4.large", region=>'us-east-2', profile=>'fmc');

  is_deeply $i->{VCpuInfo},
   {DefaultCores          => 1,
    DefaultThreadsPerCore => 2,
    DefaultVCpus          => 2,
    ValidCores            => [1],
    ValidThreadsPerCore   => [1, 2],
    };
 }

if (1) {                                                                        #TinvertHashOfHashes
  my $h =  {a=>{A=>q(aA), B=>q(aB)}, b=>{A=>q(bA), B=>q(bB)}};
  my $g =  {A=>{a=>q(aA), b=>q(bA)}, B=>{a=>q(aB), b=>q(bB)}};

  is_deeply invertHashOfHashes($h), $g;
  is_deeply invertHashOfHashes($g), $h;
 }

if (0) {                                                                        #TawsEc2DescribeImages
  awsEc2DescribeImages(region => q(us-east-2), profile=>q(fmc));
 }

if (0) {                                                                        #TawsCurrentLinuxSpotPrices
   awsCurrentLinuxSpotPrices(region => q(us-east-2), profile=>q(fmc));
 }

if (1) {                                                                        #ThashifyFolderStructure
  is_deeply hashifyFolderStructure(qw(/a/a/a /a/a/b /a/b/a /a/b/b)),
   {"" => {a => {a => { a => "/a/a/a", b => "/a/a/b" },
                 b => { a => "/a/b/a", b => "/a/b/b" },
                },
          },
   };
 }

if (!onWindows) {

if (1) {                                                                        #TsetPartitionOnIntersectionOverUnionOfHashStringSetsInParallel
  my $N = 8;
  my %s;
  for     my $a('a'..'z')
   {my @w;
    for   my $b('a'..'e')
     {for my $c('a'..'e')
       {push @w, qq($a$b$c);
       }
     }

    for   my $i(1..$N)
     {$s{qq($a$i)} = join ' ', @w;
     }
   }

  my $expected =
   [["a1" .. "a8"],
    ["b1" .. "b8"],
    ["c1" .. "c8"],
    ["d1" .. "d8"],
    ["e1" .. "e8"],
    ["f1" .. "f8"],
    ["g1" .. "g8"],
    ["h1" .. "h8"],
    ["i1" .. "i8"],
    ["j1" .. "j8"],
    ["k1" .. "k8"],
    ["l1" .. "l8"],
    ["m1" .. "m8"],
    ["n1" .. "n8"],
    ["o1" .. "o8"],
    ["p1" .. "p8"],
    ["q1" .. "q8"],
    ["r1" .. "r8"],
    ["s1" .. "s8"],
    ["t1" .. "t8"],
    ["u1" .. "u8"],
    ["v1" .. "v8"],
    ["w1" .. "w8"],
    ["x1" .. "x8"],
    ["y1" .. "y8"],
    ["z1" .. "z8"],
   ];

  is_deeply $expected,
   [setPartitionOnIntersectionOverUnionOfHashStringSets          (0.50, \%s)];

  my $expectedInParallel =
   ["a1 a2 a3 a4 a5 a6 a7 a8",                                                  # Same strings in multiple parallel processes
    "b1 b2 b3 b4 b5 b6 b7 b8",
    "b1 b2 b3 b4 b5 b6 b7 b8",
    "c1 c2 c3 c4 c5 c6 c7 c8",
    "d1 d2 d3 d4 d5 d6 d7 d8",
    "d1 d2 d3 d4 d5 d6 d7 d8",
    "e1 e2 e3 e4 e5 e6 e7 e8",
    "f1 f2 f3 f4 f5 f6 f7 f8",
    "f1 f2 f3 f4 f5 f6 f7 f8",
    "g1 g2 g3 g4 g5 g6 g7 g8",
    "h1 h2 h3 h4 h5 h6 h7 h8",
    "h1 h2 h3 h4 h5 h6 h7 h8",
    "i1 i2 i3 i4 i5 i6 i7 i8",
    "j1 j2 j3 j4 j5 j6 j7 j8",
    "j1 j2 j3 j4 j5 j6 j7 j8",
    "k1 k2 k3 k4 k5 k6 k7 k8",
    "l1 l2 l3 l4 l5 l6 l7 l8",
    "l1 l2 l3 l4 l5 l6 l7 l8",
    "m1 m2 m3 m4 m5 m6 m7 m8",
    "n1 n2 n3 n4 n5 n6 n7 n8",
    "n1 n2 n3 n4 n5 n6 n7 n8",
    "o1 o2 o3 o4 o5 o6 o7 o8",
    "p1 p2 p3 p4 p5 p6 p7 p8",
    "q1 q2 q3 q4 q5 q6 q7 q8",
    "q1 q2 q3 q4 q5 q6 q7 q8",
    "r1 r2 r3 r4 r5 r6 r7 r8",
    "s1 s2 s3 s4 s5 s6 s7 s8",
    "s1 s2 s3 s4 s5 s6 s7 s8",
    "t1 t2 t3 t4 t5 t6 t7 t8",
    "u1 u2 u3 u4 u5 u6 u7 u8",
    "u1 u2 u3 u4 u5 u6 u7 u8",
    "v1 v2 v3 v4 v5 v6 v7 v8",
    "w1 w2 w3 w4 w5 w6 w7 w8",
    "w1 w2 w3 w4 w5 w6 w7 w8",
    "x1 x2 x3 x4 x5 x6 x7 x8",
    "y1 y2 y3 y4 y5 y6 y7 y8",
    "y1 y2 y3 y4 y5 y6 y7 y8",
    "z1 z2 z3 z4 z5 z6 z7 z8",
   ];

  if (1)
   {my @p = setPartitionOnIntersectionOverUnionOfHashStringSetsInParallel
     (0.50, \%s);

    is_deeply $expectedInParallel, [sort map {join ' ', @$_} @p];
   }
 }

}

if (0) {                                                                        #TawsEc2ReportSpotInstancePrices
  my $a = awsEc2ReportSpotInstancePrices
   (qr(\.metal), region=>'us-east-2', profile=>'fmc');
  ok $a->report eq <<END;
CPUs by price

10 instances types found on 2019-12-24 at 22:53:26

Cheapest Instance Type: m5.metal
Price Per Cpu hour    : 6.65      in millidollars per hour

   Column         Description
1  Instance_Type  Instance type name
2  Price          Price in millidollars per hour
3  CPUs           Number of Cpus
4  Price_per_CPU  The price per CPU in millidollars per hour

    Instance_Type  Price  CPUs  Price_per_CPU
 1  m5.metal         638    96           6.65
 2  r5.metal         668    96           6.97
 3  r5d.metal        668    96           6.97
 4  m5d.metal        826    96           8.61
 5  c5d.metal        912    96           9.50
 6  c5.metal        1037    96          10.81
 7  c5n.metal        912    72          12.67
 8  i3.metal        1497    72          20.80
 9  z1d.metal       1339    48          27.90
10  i3en.metal      3254    96          33.90
END
 }

if (0) {                                                                        #TawsEc2FindImagesWithTagValue
  is_deeply
   [awsEc2FindImagesWithTagValue(qr(boot)i, region=>'us-east-2',
    profile=>'fmc')],
   ["ami-011b4273c6123ae76"];
 }

if (0) {                                                                        #TawsParallelPrimaryInstanceId
  ok "i-xxx" eq awsParallelPrimaryInstanceId
   (region => q(us-east-2), profile=>q(fmc));
 }

if (0) {                                                                        #TawsEc2RequestSpotInstances
  my $r = awsEc2RequestSpotInstances
   (2, q(t2.micro), "ami-xxx", 0.01, q(xxx), q(yyy),
    region=>'us-east-2', profile=>'fmc');
 }

if (0) {                                                                        #TawsEc2DescribeSpotInstances
  my $r = awsEc2DescribeSpotInstances(region => q(us-east-2), profile=>q(fmc));
 }

if (0) {                                                                        #TawsEc2InstanceIpAddress
  ok q(3.33.133.233) eq awsEc2InstanceIpAddress
    ("i-xxx", region => q(us-east-2), profile=>q(fmc));
 }

if (0) {                                                                        #TawsEc2Tag
  awsEc2Tag
   ("i-xxxx", Name=>q(Conversion), region => q(us-east-2), profile=>q(fmc));
 }

if (0) {                                                                        #TawsParallelPrimaryIpAddress #TawsParallelSecondaryIpAddresses #TawsParallelIpAddresses
  ok awsParallelPrimaryIpAddress eq      q(3.1.4.4);

  is_deeply [awsParallelSecondaryIpAddresses], [qw(3.1.4.5 3.1.4.6)];

  is_deeply [awsParallelIpAddresses],  [qw(3.1.4.4 3.1.4.5 3.1.4.6)];
 }

if (0) {                                                                        #TawsParallelProcessFiles  #TawsParallelProcessFilesTestParallel #TawsParallelProcessFilesTestResults # Process files on multiple L<AWS> instances in parallel.
  my $N = 2001;                                                                 # Number of files to process
  my $options = q(region => q(us-east-2), profile=>q(fmc));                     # Aws cli options
  my %options = eval "($options)";

  for my $dir(q(/home/phil/perl/cpan/DataTableText/lib/Data/Table/),            # Folders we will need on aws
              q(/home/phil/.aws/))
   {awsParallelSpreadFolder($dir, %options);
   }

  my $d = temporaryFolder;                                                      # Create a temporary folder
  my $resultsFile = fpe($d, qw(results data));                                  # Save results in this temporary file

  if (my $r = execPerlOnRemote(join "\n",                                       # Execute some code on a server
    getCodeContext(\&awsParallelProcessFilesTestParallel),                      # Get code context of the sub we want to call.
    <<SESSIONLEADER))                                                           # Launch code on session leader
use Data::Table::Text qw(:all);

my \$r = awsParallelProcessFiles                                                # Process files on multiple L<AWS> instances in parallel
 ({file=>4, time=>timeStamp},                                                   # User data
  \\\&Data::Table::Text::awsParallelProcessFilesTestParallel,                   # Reference to code to execute in parallel on each session instance
  \\\&Data::Table::Text::awsParallelProcessFilesTestResults,                    # Reference to code to execute in series to merge the results of each parallel computation
  [map {writeFile(fpe(q($d), \$_, qw(txt)), \$_)} 1..$N],                       # Files to process
  $options);                                                                    # Aws cli options as we will be running on Aws

storeFile(q($resultsFile), \$r);                                                # Save results in a file

SESSIONLEADER

   {copyFileFromRemote($resultsFile);                                           # Retrieve user data

    my $userData = retrieveFile($resultsFile);                                  # Recover user data
    my @i = awsParallelSecondaryIpAddresses(%options);                          # Ip addresses of secondary instances
    my @I = keys $userData->{ip}->%*;
    is_deeply [sort @i], [sort @I];                                             # Each secondary ip address was used

    ok $userData->{file}  == 4;                                                 # Prove we can pass data in and get it back
    ok $userData->{merge} == 1 + @i, 'ii';                                      # Number of merges

    my %f; my %i;                                                               # Files processed on each ip
    for   my $i(sort keys $userData->{ipFile}->%*)                              # Ip
     {for my $f(sort keys $userData->{ipFile}{$i}->%*)                          # File
       {$f{fn($f)}++;                                                           # Files processed
        $i{$i}++;                                                               # Count files on each ip
       }
     }

    is_deeply \%f, {map {$_=>1} 1..$N};                                         # Check each file was processed

    if (1)
     {my @rc; my @ra;                                                           # Range of number of files processed on each ip - computed, actually counted
      my $l = $N/@i-1;                                                          # Lower limit of number of files per IP address
      my $h = $N/@i+1;                                                          # Upper limit of number of files per IP address
      for   my $i(keys %i)
       {my $nc = $i{$i};                                                        # Number of files processed on this ip - computed
        my $na = $userData->{ip}{$i};                                           # Number of files processed on this ip - actually counted
        push @rc, ($nc >= $l and $nc <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
        push @ra, ($na >= $l and $na <= $h) ? 1 : 0;                            # 1 - in range, 0 - out of range
       }
      ok @i == grep {$_} @ra;                                                   # Check each ip processed the expected number of files
      ok @i == grep {$_} @rc;
     }

    ok $userData->{files}{&fpe($d, qw(4 txt))} eq                               # Check the computed MD5 sum for the specified file
       q(a87ff679a2f3e71d9181a67b7542122c);
   }
 }
else
 {ok 1 for 1..7;
 }

if (0)                                                                          #TawsParallelProcessFiles # Process files in series on local machine
 {my $N = 42;
  my $d = temporaryFolder;

  my $r = awsParallelProcessFiles                                               # Process files in series on local machine
   ({file => 4},                                                                # User data
    \&Data::Table::Text::awsParallelProcessFilesTestParallel,                   # Code to execute on each session instance including the session leader written as a string because it has to be shipped to each instance
    \&Data::Table::Text::awsParallelProcessFilesTestResults,                    # Code to execute in series on the session leader to analyze the results of the parallel runs
    [map {writeFile(fpe($d, $_, qw(txt)), $_)} 1..$N],                          # Files to process
    ());                                                                        # No Aws cli options as we are running locally

  ok $r->{file}            ==  4, 'aaa';                                        # Prove we can pass data in and get it back
  ok $r->{merge}           ==  1, 'bbb';                                        # Only one merge as we are running locally

  ok $r->{ip}{localHost}   == $N, 'ccc';                                        # Number of files processed locally
  ok keys($r->{files}->%*) == $N;                                               # Number of files processed
  ok $r->{files}{fpe($d, qw(4 txt))} eq q(a87ff679a2f3e71d9181a67b7542122c);    # Check the computed MD5 sum for the specified file

  clearFolder($d, $N+2);
 }
else
 {ok 1 for 1..5;
 }

if (1) {                                                                        #TpackBySize
  my $M = 7;
  my $N = 15;
  my @b = packBySize($M, map {[$_, $_]} 1..$N);
  my @B; my $B = 0;
  for my $b(@b)
   {my $n = 0;
    for(@$b)
     {$n += $_;
      $B += $_;
     }
    push @B, $n;
   }
  ok $B == $N * ($N + 1) / 2;
  is_deeply [@B], [16, 20, 16, 18, 16, 18, 16];
 }

is_deeply [(getSubName(\&dateTime))[0,1]], ["Data::Table::Text", "dateTime"];   #TgetSubName

if (1)                                                                          # FormatTable as csv
 {my $d = temporaryFolder;
  my $aa = formatTable
   ([[qw(A   B   C  )],
     [qw(AA  BB  CC )],
     [qw(AAA BBB CCC)],
     [qw(1   22  333)]],
     [qw (aa  bb  cc)],
    file => fpe($d, qw(report txt)));

  my $f = fpe($d, qw(report csv));
  ok readFile($f) eq <<END, 'ddd';
aa,bb,cc
"A","B","C"
"AA","BB","CC"
"AAA","BBB","CCC"
1,22,333
END
  clearFolder($d, 2);
 }

if (0) {                                                                        #TexecPerlOnRemote
  ok execPerlOnRemote(<<'END') =~ m(Hello from: t2.micro)i;
#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
use Data::Table::Text qw(:all);

say STDERR "Hello from: ", awsCurrentInstanceType;
END
 }

ok getCodeContext(\&getCodeContext) =~ m(use strict)ims;                        #TgetCodeContext

if (1) {                                                                        #TcallSubInParallel #TcallSubInOverlappedParallel
  my %a = (a=>1, b=>2);
  my %b = callSubInParallel {return %a};
  is_deeply \%a, \%b;

  my $f = temporaryFile;
  ok -e $f;

  my $a = callSubInOverlappedParallel
    sub {$a{a}++; owf($f, "Hello World")},
    sub {q(aaaa)};

  ok $a           =~ m(aaaa)i;
  ok $a{a}        == 1;
  ok readFile($f) =~ m(Hello World)i;
 }

if (0) {                                                                        #TonAws #TonAwsSecondary #TonAwsPrimary
  ok  onAws;
  ok !onAwsSecondary;
  ok  onAwsPrimary;
 }

if (1) {                                                                        #TextractCodeBlock
ok extractCodeBlock(q(#CODEBLOCK), $INC{"Data/Table/Text.pm"}) eq <<'END';
  my $a = 1;
  my $b = 2;
END
 }

if (0) {                                                                        #TawsParallelSpreadFolder #TawsParallelGatherFolder
  my $d = temporaryFolder;
  my ($f1, $f2) = map {fpe($d, $_, q(txt))} 1..2;
  my $files = {$f1 => "1111", $f2 => "2222"};

  writeFiles($files);
  awsParallelSpreadFolder($d);
  clearFolder($d, 3);

  awsParallelGatherFolder($d);
  my $r = readFiles($d);
  is_deeply $files, $r;
  clearFolder($d, 3);
 }
else
 {ok 1;
 }

if (0) {                                                                        #TreadFileFromRemote #TwriteFileToRemote

  my $f = writeFileToRemote(undef, q(aaaa));
  unlink $f;
  ok readFileFromRemote($f) eq q(aaaa);
  unlink $f;
 }
else
 {ok 1;
 }

if (0) {                                                                        #TsaveAwsIp #TawsIp #TipAddressOfHost #TsaveAwsDomain #TawsR53a #TawsR53aaaa
  ok saveAwsIp(q(0.0.0.0)) eq awsIp;
  ok saveAwsIp(q(example.org));
  ok saveAwsDomain(q(example.org));
  ok awsR53a   (q(XXXXX), q(www.example.org), q(22.12.232.1));
  ok awsR53aaaa(q(XXXXX), q(www.example.org), q([1232:1232:1232:1232:1232:1232:1232:1232:]));
 }

if (0) {                                                                        #TawsExecCli #TawsExecCliJson
  ok awsExecCli(q(aws s3 ls)) =~ m(ryffine)i;
  my $p = awsExecCliJson(q(aws ec2 describe-vpcs), region=>q(us-east-1));
  ok $p->Vpcs->[0]->VpcId =~ m(\Avpc-)i;
 }

if (0) {                                                                        #ToverWriteHtmlFile #ToverWritePerlCgiFile
  overWriteHtmlFile   (q(index.html), q(<html><h1>Hello</h1></html>));
  overWritePerlCgiFile(q(gen.pl),     q(...));
 }

ok containingFolderName(q(/a/b/c.d)) eq q(b);                                   #TcontainingFolderName

if (1)                                                                          #Tunbless
 {my $a = {};
  ok ref($a)      eq  q(HASH);
  my $b =   bless $a, q(aaaa);
  ok ref($a)      eq  q(aaaa);
  my $c = unbless $b;
  ok ref($c)      eq  q(HASH);
 }

if (0) {                                                                        #TwwwHeader #TwwwGitHubAuth
  wwwHeader;

  wwwGitHubAuth
   {my ($user, $state, $token, $scope, $type) = @_;
   }
  q(12345678901234567890), q(1234567890123456789012345678901234567890),
  q(12345678901234567890123456789012), q(12345678901234567890);
 }

if (1) {
  my $s = q(on L<gitHub> or github);
  my $t = expandWellKnownUrlsInPerlFormat($s);
  my $c = reinstateWellKnown($t);
  ok lc($s) eq lc($c);
 }

if (1) {                                                                        #TcmpArrays
  ok cmpArrays([qw(a b)],   [qw(a a)])   == +1;
  ok cmpArrays([qw(a b)],   [qw(a c)])   == -1;
  ok cmpArrays([qw(a b)],   [qw(a b a)]) == -1;
  ok cmpArrays([qw(a b a)], [qw(a b)])   == +1;
  ok cmpArrays([qw(a b)],   [qw(a b)])   ==  0;
 }

if (1)                                                                          #TtransitiveClosure
 {is_deeply transitiveClosure({a=>{b=>1, c=>2}, b=>{d=>3}, c=>{d=>4}}),
   {end => [{ b => 1, c => 1, d => 4 }, { d => 1 }],
    start => { a => 0, b => 1, c => 1 },
   };
 }

if (1)                                                                          #TlengthOfLongestSubArray
 {ok 3 == lengthOfLongestSubArray [[1..2], [1..3], [1..3], []];
 }

if (1) {                                                                        #TreadStdIn
  my $d = qq(aaaa);
  open(STDIN, "<", writeTempFile($d));
  ok qq($d\n) eq readStdIn;
 }

if (1)                                                                          #TconvertPerlToJavaScript
 {my $i = writeTempFile(<<'END');
sub test($$)                                                                    #P A test method
 {my ($file, $data) = @_;                                                       # Parameter 1, parameter 2
  if (fullyQualifiedFile($file)) {return qq($data)}                             # File is already fully qualified
 } # test
END

  convertPerlToJavaScript($i, my $o = temporaryFile);

  ok readFile($o) eq <<'END';
function test(file, data)                                                       //P A test method
 {  if (fullyQualifiedFile(file)) {return `data`}                               // File is already fully qualified
 }                                                                              // test
END

  unlink $i, $o;
 }

if (1) {                                                                        #TprintPerlDataAsXml
my $perlData = {a=>1, b=>[{c=>[3,4]}, {d=>[5,6]}, {e=>7}], f=>8};
my $xml = printPerlDataAsXml($perlData);

is_deeply $xml, trim(<<END);
<hash>
    <a>1</a>
    <b>
    <array>
                <e><hash><c><array><e>3</e><e>4</e></array></c></hash></e>
                <e><hash><d><array><e>5</e><e>6</e></array></d></hash></e>
                <e><hash><e>7</e></hash></e>
                </array>
    </b>
    <f>8</f>
    </hash>
END
 }

my %h = (a=>1, b=>2, c=>3);

my @t;

if (1) {                                                                        #TforEachKeyAndValue
  forEachKeyAndValue
   {my ($letter, $number) = @_;
    push @t,  "Letter=$letter, number=$number";
   } %h;

  is_deeply join("\n", @t, ''), <<END;
Letter=a, number=1
Letter=b, number=2
Letter=c, number=3
END
}

if ($localTest)
 {say STDERR "DTT finished in ", (time() - $timeStart), " seconds";
 }

1
