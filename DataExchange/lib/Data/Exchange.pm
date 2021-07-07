#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib
#-------------------------------------------------------------------------------
# Exchange files and update issues with your colleagues via an S3 bucket or rsync.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2018
#-------------------------------------------------------------------------------
# podDocumentation
# rsync remote requires we create the target nexus folder
# Jira has no snippets and other useful editor features. Slow because file accesses are over the internet not local.  No in editor tab so constantly transferring files and cutting and pasting between editor and browser.
# Jira editor inserts emoticons for prgramming constructs like :) or <>, the editors is poor and slow.

package Data::Exchange;
our $VERSION = q(20181002);
require v5.16;
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use POSIX qw(strftime);                                                         # http://www.cplusplus.com/reference/ctime/strftime/
use utf8;

sub debug        {1}                                                            # 0 - distribution, 1 - test, 2 - production
sub testTransport{1}                                                            # 0 - S3, 1 - rsync locally, 2 - rsync remote
sub usingAt      {0}                                                            # 0 - run from users command line, 1 - run via At if on unix
sub logFile      {q(/home/phil/zzz.txt)}                                        # Log file

sub lll(@)                                                                      # Log messages
 {my $m = join '', dateTimeStamp, " ", @_;                                      # Time stamp each message
  return unless $m =~ m(\S)s;                                                   # Blank messages
  say STDERR $m if debug;
  appendFile(logFile, dateTimeStamp." $m\n");
 }

lll "Exchange Start version $VERSION";                                          # The title of the piece

if (@ARGV)                                                                      # Process any command line arguments
 {parseCommandLineArguments
   {my ($pos, $key) = @_;
    lll "Exchange parse ", dump(\@ARGV);
    if (my $f = $$key{file})
     {lll "Evaluate: $f";
      my $p = evalFile($f);
      my $x = new(%$p);
      lll "Exchange go\n", dump($x);
      $x->go();
      exit;
     }
   } \@ARGV,
   {file=>q(The file from which to read parameters),
   }
 }

sub boldFilesWithChanges                                                        #P Bold file names with changed contents                                                    # Bold file names with changed contents
 {0
 }
sub bucket                                                                      #P Default bucket used for exchanges
 {q(ryffine.exchange)
 }

sub issueFileExtension                                                          #P Default  extension that identifies issue files
 {q(txt)
 }

sub nexus                                                                       #P Default nexus folder every-one can reach
 {return fpf(q(phil@52.15.233.83:/home/phil/perl/xxx/)) if testTransport == 2;
  fpf(q(/home/phil/perl/xxxNexus/));
 }

sub quiet                                                                       #P Quiet S3/rsync if true
 {0
 }

sub rsyncOptions                                                                #P Rsync options
 {q(-mpqrt)
 }

sub sep                                                                         #P Time section separator
 {q(At \d\d\d\d\.\d\d\.\d\d \d\d:\d\d:\d\d \d{7} from \w+);
 }

sub sendRe                                                                      #P Send command
 {qr(send\s*\Z)s;
 }

sub transport                                                                   #P Default file transferm can be q(s3) or q(rsync)
 {my $t = testTransport;
  return q(rsync) if $t;
  q(s3)
 }

sub writeLine                                                                   #P Write line
 {qq(+Write below then remove this line to send.\n\n\n);
 }

sub writeLineRe                                                                 #P Write line regular expression
 {qr(\A\+Write below then remove this line to send\.\s*)
 }

#D1 File and Issue Exchanger                                                    # Exchange files and update issues with your colleagues via an S3 bucket or rsync.

sub new(@)                                                                      # Create a new exchanger for a specified user allowing them to exchange files and issues with their colleagues via an S3 bucket or rsync - see: L<Data::Exchange Definition|/Data::Exchange Definition> for the attributes that can be set by this constructor.
 {my (@parms) = @_;                                                             # Optional parameters
  genHash(q(Data::Exchange::Details),                                           # Exchanger attributes.
    boldFilesWithChanges    => boldFilesWithChanges,                            # Bold file names with changed contents
    bucket                  => bucket,                                          # The meeting point expressed as bucket/optionalPrefix for S3 transport
    exchange                => undef,                                           # Folder containing files locally
    ext                     => issueFileExtension,                              # Extension of issue files
    interval                => q(60),                                           # Interval between exchanges
    nexus                   => nexus,                                           # The meeting point expressed as a target for rsync transport
    quiet                   => quiet,                                           # Quiet S3/rsync if true
    rsyncOptions            => rsyncOptions,                                    # Rsync options
    s3Parms                 => q(--profile fmc),                                # Additional S3 parms
    segmentsExt             => q(data),                                         # Extension of segment files
    segmentsFolder          => q(segments),                                     # Folder for file segments
    service                 => undef,                                           #P Service incarnation
    start                   => time,                                            # Time this exchanger was  started
    transport               => transport,                                       # Default file transfer
    user                    => undef,                                           # User who owns this instance
    version                 => $VERSION,                                        #P Version of this software
    immediateUpdateInterval => 5,                                               # Time between immediate updates
    @_,
   );
 }

my $stampCount = 0;                                                             # Separate otherwise identical time stamps with one second resolution

sub Data::Exchange::Details::stamp($)                                           #P Match files that have the same file name.
 {my ($x) = @_;                                                                 # Exchanger
  my $c = sprintf("%07d", ++$stampCount);                                       # Sequence number
  my $u = $x->user;                                                             # User
  return qq(At 0000.00.00 00:00:00 $c from $u\n) if debug == 1;                 # Debug stamp
  strftime(qq(At %Y.%m.%d %H:%M:%S $c from $u\n), gmtime)                       # Stamp
 }

sub newOldSep                                                                   #P The separator between new and old
 {(q(-)x80)."\n";
 }

sub cutOnNewOldSep($)                                                           #P Split a string on the new old separator.
 {my ($string) = @_;                                                            # String to split
  my $s = newOldSep
  my $t = trim($string);
  my $i = index($t, $s);
  if ($i >= 0)                                                                  # Separator found
   {return (substr($t, 0, $i), substr($t, $i+length($s)))
   }
  ($t, q());                                                                    # Otherwise everything is regarded as new
 }

my $segmentFileNumber = 0;                                                      # Segment file number for testing

sub segmentFileNumber                                                           #P Segment file into new and old saving the new in a segment file then reconstructing the original file so it is all old
 {q(_at_).sprintf("%020d", debug == 1 ? ++$segmentFileNumber
                                      : microSecondsSinceEpoch);
 }

sub Data::Exchange::Details::segmentFile($$)                                    #P Segment file into new and old saving the new in a segment file then reconstructing the original file so it is all old
 {my ($x, $file) = @_;                                                          # Exchanger, file to segment

  if (inProgressFile($file))
   {lll  "File to segment $file is in progress";
    return;
   }
  my $s = readFile($file);
  #lll "Segment file $file\n$s";
  my ($new, $old) = cutOnNewOldSep($s);
  my $send = sendRe;
  $new =~ s($send) ()s;
  #lll "Segment file $file\nnew:\n$new\nold:$old\n";

  if ($new =~ m(\S)s)                                                           # New segment is not empty
   {my $t = segmentFileNumber;
    my $f = fpe($x->localFolder, $x->segmentsFolder, fn($file).$t, qw(data));
    my $h = $x->stamp;
    writeFile($f, "$h\n$new");                                                  # Segment
    owf($file, newOldSep.$h.$new.$old);                                         # Reconstruct old file
    #lll "Created segment file $f from $file";
   }
  else
   {lll "Nothing new in file $file";
   }
 }

sub replaceWriteLine($$)                                                        #P Replace the write line in a file
 {my ($file, $line) = @_;                                                       # File, new line
  my $w = writeLineRe;                                                          # Write line
  my $s = my $S = readFile($file);                                              # Read file
     $s =~ s($w) ($line\n)s;                                                    # Change write line to add message and release
  unless($s eq $S)                                                              # Rewrite file if its content have been changed
   {owf($file, $s);                                                             # Rewrite file
    updateLastModifiedTime($file);                                              # Update modification time
   }
 }

sub Data::Exchange::Details::findIssueFiles($)                                  #P Find all the files that are issues
 {my ($x) = @_;                                                                 # Exchanger
  searchDirectoryTreesForMatchingFiles
   ($x->exchange,
    $x->ext);
 }

sub Data::Exchange::Details::findUserIssueFiles($)                              #P Find all the files that are issues for this user
 {my ($x) = @_;                                                                 # Exchanger
  searchDirectoryTreesForMatchingFiles
   ($x->localFolder,
    $x->ext);
 }

sub Data::Exchange::Details::findSegmentFiles( )                                #P Find all the files that are segments
 {my ($x) = @_;                                                                 # Exchanger
  searchDirectoryTreesForMatchingFiles
   ($x->exchange,
    $x->segmentsExt);
 }

sub Data::Exchange::Details::listSegmentFiles($$)                               #P List all the segment files
 {my ($x) = @_;                                                                 # Exchanger

  my @f = searchDirectoryTreesForMatchingFiles                                  # Find files to match
   ($x->exchange,
    $x->segmentsExt);

  my @files;
  for my $file(@f)                                                              # Match files
   {my $t = readFile($file);                                                    # Read file
    push @files, [$file, $t];
   }

  {+{map {$$_[0]=>$$_[1]} @files}}
 }

sub Data::Exchange::Details::listIssueFiles($)                                  #P List user files
 {my ($x) = @_;                                                                 # Exchanger
  my @files;

  my @f = searchDirectoryTreesForMatchingFiles                                  # Find files to match
   ($x->exchange,
    $x->ext);

  for my $file(@f)                                                              # Match files
   {my $t = readFile($file);                                                    # Read file
    push @files, [$file, $t];
   }

  {+{map {$$_[0]=>$$_[1]} @files}}
 }

sub Data::Exchange::Details::listFiles($)                                       #P List all files
 {my ($x, $title) = @_;                                                         # Exchanger, title
  my $l = {issues=>[$x->listIssueFiles], segments=>[$x->listSegmentFiles]};
  my $s = dump($l);
  $s =~ s(\s*(\[|\]|\{|\})\s*) ($1)gs;
  $s =~ s(,(\]|\})) ($1)gs;
$title or confess;
  say STDERR "ListFiles: $title\n$s;";
  $l
 }

sub Data::Exchange::Details::findIssues($)                                      #P Find all the issues
 {my ($x) = @_;                                                                 # Exchanger
  my %i;
  for my $file(removeFilePrefix $x->exchange,  $x->findSegmentFiles)
   {my $f = fn $file;
       $f =~ s(_at_.*\Z) ()s;
    push @{$i{$f}}, $file;
   }
  \%i;
 }

sub inProgressFile($)                                                           #P Check whether a file is a work in progress depending on whether it has a plus sign in column 1 of line 1 or not.
 {my ($file) = @_;                                                              # File
  return 0 unless -e $file;
  my $n = newOldSep;
  my $s = readFile($file);
  my ($new, $old) = cutOnNewOldSep($s);
  my $send = sendRe;
  return 0 if length($new) == 0 or $new =~ m($send)s;                           # User has not modified the segment or has explicitly sent the segment
  1
 }

sub Data::Exchange::Details::assembleFile($$@)                                  #P Assemble the segment files supplied to recreate the target
 {my ($x, $target, @source) = @_;                                               # Exchanger, target file, source files.

  lll "Assemble file $target from ", join " ", @source;

  my @f = sort
   {my $f = fn $a;
    my $F = fn $b;
    $F cmp $f
   } @source;

  my @s;
  for my $file(@f)
   {my $f = fpf($x->exchange, $file);
    push @s, readFile($f);
   }

  my $s = newOldSep.join qq(\n).newOldSep, @s;

  if (@s and my $t = $s[0])
   {if (my @h = $t =~ m(^hi\s+(\w+))mi)
     {my $user = $1;
      if (lc($user) eq lc($x->user))
       {$s = writeLine.$s;
       }
     }
   }

  if (-e $target)
   {my $t = readFile($target);
    if ($s ne $t)
     {if (!inProgressFile($target))
       {owf($target, $s);
        lll "Replaced file $target";
       }
      else
       {lll "Did not replaced file $target because it is in progress";
       }
     }
    else
     {lll "Did not replaced file $target because it is identical";
     }
   }
  else
   {if (!inProgressFile($target))
     {owf($target, $s);
      lll "Created file $target";
     }
   }
 }

sub Data::Exchange::Details::assemble($)                                        #P Assemble the segment files
 {my ($x) = @_;                                                                 # Exchanger, Files
  my $i = $x->findIssues;
  for my $issue(sort keys %$i)                                                  # Issues
   {my $t = fpe($x->localFolder, $issue, $x->ext);
    $x->assembleFile($t, @{$i->{$issue}});
   }
 }

sub Data::Exchange::Details::localFolder($)                                     #P The local folder containing the users own files.
 {my ($x) = @_;                                                                 # Exchanger
  fpd($x->exchange, $x->user);
 }

sub Data::Exchange::Details::segmentUserIssueFiles($)                             #P Add a time stamp to files that are no longer in progress
 {my ($x) = @_;                                                                 # Exchanger
  my $sep = sep;
  for my $file($x->findUserIssueFiles)                                          # Each issue file
   {$x->segmentFile($file);
   }
 }

sub Data::Exchange::Details::exchangeUp($)                                      #P Send one set of files.
 {my ($x) = @_;                                                                 # Exchanger
  if ($x->transport =~ m(\Arsync\Z)i)                                           # Empty folder reached by rsync
   {my $o = $x->rsyncOptions;                                                   # Rsync options
    my $s = fpd($x->exchange, $x->user);                                        # User's own files locally
    my $t = fpd($x->nexus,    $x->user);                                        # User's own files on nexus
    makePath($_) for $s, $t;
    my $c = qq(rsync $o $s $t);                                                 # Command
    qx($c 2>&1);                                                                # Execute
   }
  else                                                                          # Use S3 as default transport
   {my $s = fpd($x->localFolder);                                               # User's own files
    my $t = fpd($x->bucket, $x->user);                                          # User's own files on S3
    my $p = $x->s3Parms;                                                        # Additional parameters for s3
    my $q = $x->quiet ? q(--quiet) : q();                                       # Quiet S3
    lll my $c = qq(aws s3 sync $s s3://$t $q --delete $p);                      # Command
    lll qx($c 2>&1);                                                            # Execute
   }
 }

sub Data::Exchange::Details::exchangeDown($)                                    #P Receive one set of files.
 {my ($x) = @_;                                                                 # Exchanger
  if ($x->transport =~ m(\Arsync\Z)i)                                           # Empty folder reached by rsync
   {my $o = $x->rsyncOptions;                                                   # Rsync options
    my $u = $x->user;                                                           # User
    my $s = fpd($x->nexus);                                                     # Nexus
    my $t = fpd($x->exchange);                                                  # Local files
    makePath($_) for $s, $t;
    my $c = qq(rsync $o --exclude="*/$u/*" $s $t);                              # Command
    qx($c 2>&1);                                                                # Execute
   }
  else                                                                          # Use S3 as default transport
   {my $u = $x->user;                                                           # Receiver
    my $s = fpd($x->bucket);                                                    # User's own files on S3
    my $t = fpd($x->exchange);                                                  # All possible exchange files
    my $p = $x->s3Parms;                                                        # Additional parameters for s3
    my $q = $x->quiet && !debug ? q(--quiet) : q();                             # Quiet S3
    lll my $c = qq(aws s3 sync s3://$s $t $q --exclude "$u/*" $p);              # Command
    lll qx($c 2>&1);                                                            # Execute
   }
 }

sub Data::Exchange::Details::exchangeOneSet($)                                  #P Exchange one set of files.
 {my ($x) = @_;                                                                 # Exchanger
  makePath(fpd($x->exchange, $x->user));                                        # Otherwise no issues will be created for this user
  lll "\nExchangeOneSet";
  my $d = hostName =~ m(\A(pousadouros|secarias)\Z)s;                           # Developing ?
  my $N = $d ? 10 : $x->interval;                                               # Time to wait between file transfers
  my $n = $d ?  2 : $x->immediateUpdateInterval;                                #P Time between file checks
  $x->exchangeDown;                                                             # Receive files
  $x->assemble;                                                                 # Reassemble issue files
  for(1..$N)                                                                    # Check for file changes made by the user
   {$x->segmentUserIssueFiles;                                                  # Stamp unstamped files
    last if debug;
    sleep $n;
   }
  $x->exchangeUp;                                                               # Send files
  for(1..$N)                                                                    # Check for file changes made by the user
   {$x->segmentUserIssueFiles;                                                  # Stamp unstamped files
    last if debug;
    sleep $n;
   }
 }

sub Data::Exchange::Details::go($)                                              #P Run until forcibly stopped.
 {my ($x) = @_;                                                                 # Exchanger
  lll "Go";
  my $s = newServiceIncarnation(q(exchange).$x->user);                          # Service representing exchanger
  makePath($x->localFolder);                                                    # Make the folder for the user
  $x->service = $s->check;                                                      # Assigns service
  for(;;)                                                                       # Exchange and sleep
   {$x->service->check;                                                         # Check we should continue as the current service
    $x->exchangeOneSet;                                                         # Exchange
    sleep 1;                                                                    # Stop runaways
   }
 }

sub Data::Exchange::Details::start                                              # Start a new exchanger as a service via atq.
 {my ($x) = @_;                                                                 # Exchanger
  my $e = dumpFile(undef, $x);                                                  # Temporary file with exchange parameters
  my $p = qq(perl $0 --file=$e);                                                # Perl command to start exchanger
  my $f = writeFile(undef, $p);                                                 # Temporary file with perl command
  if ($^O =~ m(MSWin32)is or !usingAt)                                          # Run from command line
   {lll "Execute ", $p;
    say STDERR $_ for qx($p);                                                   # Execute from command line
   }
  else                                                                          # Run as atq
   {my $c = qq(at now -f $f);
    say STDERR $c;
    say STDERR $_ for qx($c);                                                   # Execute from at queue
   }
 }

#D
# podDocumentation

=pod

=encoding utf-8

=head1 Name

Exchange files and update issues with your colleagues via an S3 bucket or rsync.

=head1 Synopsis

Configure and run an exchanger:

  use Data::Exchange;
  my $x        = Data::Exchange::new();
  $x->user     = q(phil);
  $x->bucket   = q(com.appaapps.exchange);
  $x->exchange = q(/home/phil/exchange);
  $x->start;

Files that end in .txt no matter where they are located below the exchange
folder:

  $x->exchange

will be merged with files of the same name from your colleagues whose files
show up in other folders under the exchange folder, allowing you to share files
and update issues with your colleagues.

Issue files that start with a plus sign B<+> in column one of line one are
assumed to be work in progress and will be ignored until the initial plus sign
is removed.

Lines which start with defined keywords in column one have special meanings if
the occur in the first section of an issues file:

 to: user

The name of the user to which this issue is addressed otherwise B<all> users will
see copies of this issue.

=head1 Description

files and update issues with your colleagues via an S3 bucket or rsync.

The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 File and Issue Exchanger

Exchange files and update issues with your colleagues via an S3 bucket or rsync.

=head2 new(@)

Create a new exchanger for a specified user allowing them to exchange files and issues with their colleagues via an S3 bucket or rsync - see: L<Data::Exchange Definition|/Data::Exchange Definition> for the attributes that can be set by this constructor.

     Parameter  Description
  1  @parms     Optional parameters

Example:


      my $x        = Data::Exchange::𝗻𝗲𝘄();


=head2 Data::Exchange::Details::start()

Start a new exchanger as a service via atq.


Example:


      $x->start;



=head1 Hash Definitions




=head2 Data::Exchange::Details Definition


Exchanger attributes.


B<boldFilesWithChanges> - Bold file names with changed contents

B<bucket> - The meeting point expressed as bucket/optionalPrefix for S3 transport

B<exchange> - Folder containing files locally

B<ext> - Extension of issue files

B<interval> - Interval between exchanges

B<nexus> - The meeting point expressed as a target for rsync transport

B<quiet> - Quiet S3/rsync if true

B<rsyncOptions> - Rsync options

B<s3Parms> - Additional S3 parms

B<start> - Time this exchanger was  started

B<transport> - Default file transfer

B<user> - User who owns this instance



=head1 Private Methods

=head2 Data::Exchange::Details::stamp($)

Match files that have the same file name.

     Parameter  Description
  1  $x         Exchanger

=head2 replaceWriteLine($$)

Replace the write line in a file

     Parameter  Description
  1  $file      File
  2  $line      New line

=head2 cutByTime($)

Cut a single file into time sections.

     Parameter  Description
  1  $file      File

=head2 Data::Exchange::Details::findIssueFiles($)

Find all the files that are issues

     Parameter  Description
  1  $x         Exchanger

=head2 inProgressFile($)

Check whether a file is a work in progress depending on whether it has a plus sign in column 1 of line 1 or not.

     Parameter  Description
  1  $file      File

=head2 Data::Exchange::Details::findInProgressFiles($)

Find all the files that are in progress.

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::assemble($@)

Assemble the time sections in multiple files into one file.

     Parameter  Description
  1  $x         Exchanger
  2  @files     Files

=head2 Data::Exchange::Details::matchFiles($)

Match files that have the same file name.

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::listFiles($)

List user files

     Parameter  Description
  1  $x         Exchanger

=head2 boldFileName($)

Create a bold file name

     Parameter  Description
  1  $file      File name

=head2 unBoldFileName($)

Remove bolding from file name

     Parameter  Description
  1  $file      File name

=head2 Data::Exchange::Details::unBoldFiles($)

Make all issue files non bold.

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::localFolder($)

The local folder containing the users own files.

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::segmentUserIssueFiles($)

Add a time stamp to files that are no longer in progress

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::exchangeUp($)

Send one set of files.

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::exchangeDown($)

Receive one set of files.

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::exchangeOneSet($)

Exchange one set of files.

     Parameter  Description
  1  $x         Exchanger

=head2 Data::Exchange::Details::go($)

Run until forcibly stopped.

     Parameter  Description
  1  $x         Exchanger


=head1 Index


1 L<boldFileName|/boldFileName> - Create a bold file name

2 L<cutByTime|/cutByTime> - Cut a single file into time sections.

3 L<Data::Exchange::Details::assemble|/Data::Exchange::Details::assemble> - Assemble the time sections in multiple files into one file.

4 L<Data::Exchange::Details::exchangeDown|/Data::Exchange::Details::exchangeDown> - Receive one set of files.

5 L<Data::Exchange::Details::exchangeOneSet|/Data::Exchange::Details::exchangeOneSet> - Exchange one set of files.

6 L<Data::Exchange::Details::exchangeUp|/Data::Exchange::Details::exchangeUp> - Send one set of files.

7 L<Data::Exchange::Details::findInProgressFiles|/Data::Exchange::Details::findInProgressFiles> - Find all the files that are in progress.

8 L<Data::Exchange::Details::findIssueFiles|/Data::Exchange::Details::findIssueFiles> - Find all the files that are issues

9 L<Data::Exchange::Details::go|/Data::Exchange::Details::go> - Run until forcibly stopped.

10 L<Data::Exchange::Details::listFiles|/Data::Exchange::Details::listFiles> - List user files

11 L<Data::Exchange::Details::localFolder|/Data::Exchange::Details::localFolder> - The local folder containing the users own files.

12 L<Data::Exchange::Details::matchFiles|/Data::Exchange::Details::matchFiles> - Match files that have the same file name.

13 L<Data::Exchange::Details::stamp|/Data::Exchange::Details::stamp> - Match files that have the same file name.

14 L<Data::Exchange::Details::segmentUserIssueFiles|/Data::Exchange::Details::segmentUserIssueFiles> - Add a time stamp to files that are no longer in progress

15 L<Data::Exchange::Details::start|/Data::Exchange::Details::start> - Start a new exchanger as a service via atq.

16 L<Data::Exchange::Details::unBoldFiles|/Data::Exchange::Details::unBoldFiles> - Make all issue files non bold.

17 L<inProgressFile|/inProgressFile> - Check whether a file is a work in progress depending on whether it has a plus sign in column 1 of line 1 or not.

18 L<new|/new> - Create a new exchanger for a specified user allowing them to exchange files and issues with their colleagues via an S3 bucket or rsync - see: L<Data::Exchange Definition|/Data::Exchange Definition> for the attributes that can be set by this constructor.

19 L<replaceWriteLine|/replaceWriteLine> - Replace the write line in a file

20 L<unBoldFileName|/unBoldFileName> - Remove bolding from file name

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Data::Exchange

=head1 Author

L<philiprbrenan@gmail.com|mailto:philiprbrenan@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

=head1 Copyright

Copyright (c) 2016-2018 Philip R Brenan.

This module is free software. It may be used, redistributed and/or modified
under the same terms as Perl itself.

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
use Test::More tests=>15;

if (!debug)                                                                     # Full production
 {ok 1 for 1..15;
  exit;
 }
else                                                                            # Testing
 {if (hostName =~ m(\Apousadouros\Z)s)                                          # Testing locally
   {if (testTransport >= 1)                                                     # Empty folder reached by rsync
     {makePath(nexus);
      clearFolder(&nexus, 12);
     }
    elsif (testTransport == 0)                                                  # Empty folder reached by rsync
     {my $b = bucket;
      for my $u(qw(bill phil aaa))                                              # Go slow: this is a very dangerous command
       {my $c = qq(aws s3 rm s3://$b/$u/ --recursive  --profile fmc);
        say STDERR $c;
        say STDERR $_ for qx($c);
       }
     }
   }

  my $b = new();                                                                # Create exchanges for each user
     $b->user     = q(bill);
     $b->exchange = q(/home/phil/perl/xxx/exchangeBill/);

  clearFolder($b->exchange, 12);
  is_deeply $b->listFiles(q(b1)), {issues =>[{}], segments =>[{}]};

  my $p = new
   (user     => q(phil),
    exchange => q(/home/phil/perl/xxx/exchangePhil/),
   );

  clearFolder($p->exchange, 12);
  is_deeply $p->listFiles(q(p1)), {issues =>[{}], segments =>[{}]};

  if (debug == 1)
   {writeFile(my $pf = fpe($p->exchange, qw(phil 1 txt)), <<END);               # Work in progress file
Hello from Phil
END

    my $epf =
{issues   =>[{"/home/phil/perl/xxx/exchangePhil/phil/1.txt" => "Hello from Phil\n"}],
 segments =>[{}]};

    is_deeply $p->listFiles(q(p2)), $epf;

    $p->segmentUserIssueFiles;                                                  # File is in progress so it will not segment
    is_deeply $p->listFiles(q(p3)), $epf;

    owf($pf, <<END);                                                            # Release work in progress
Hello from Phil
send
END
    is_deeply $p->listFiles(q(p4)),
{issues   =>[{"/home/phil/perl/xxx/exchangePhil/phil/1.txt" => "Hello from Phil\nsend\n"}],
  segments =>[{}]};

    is_deeply [$p->findUserIssueFiles],
              ["/home/phil/perl/xxx/exchangePhil/phil/1.txt"];


    $p->segmentUserIssueFiles;

    is_deeply $p->listFiles(q(p5)),
{issues   =>[{"/home/phil/perl/xxx/exchangePhil/phil/1.txt" => "--------------------------------------------------------------------------------\nAt 0000.00.00 00:00:00 0000001 from phil\nHello from Phil\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangePhil/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};

    is_deeply $p->findIssues, { 1 => ["phil/segments/1_at_00000000000000000001.data"]};

    $p->assemble;
    is_deeply $p->listFiles(q(p6)),
{issues   =>[{"/home/phil/perl/xxx/exchangePhil/phil/1.txt" => "--------------------------------------------------------------------------------\nAt 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangePhil/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};

    owf($pf, <<END);                                                            # Add new segment
Hello from Phil Again!
END
    $p->exchangeOneSet;
    is_deeply $p->listFiles(q(p7)),
{issues   =>[{"/home/phil/perl/xxx/exchangePhil/phil/1.txt" => "Hello from Phil Again!\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangePhil/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};

    $b->exchangeOneSet;                                                         # Exchange with another user
    is_deeply $b->listFiles(q(b2)),
{issues   =>[{"/home/phil/perl/xxx/exchangeBill/bill/1.txt" => "--------------------------------------------------------------------------------\nAt 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n",
                  "/home/phil/perl/xxx/exchangeBill/phil/1.txt" => "Hello from Phil Again!\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangeBill/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};

    my $bf = fpe($b->exchange, qw(bill 1 txt));                                 # Send a message back again
    owf($bf, q(Hello from Bill));                                               # Change first line to add message and release
    $b->exchangeOneSet;                                                  # Exchange with another user
    is_deeply $b->listFiles(q(b3)),
{issues   =>[{"/home/phil/perl/xxx/exchangeBill/bill/1.txt" => "--------------------------------------------------------------------------------\nAt 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n",
                  "/home/phil/perl/xxx/exchangeBill/phil/1.txt" => "Hello from Phil Again!\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangeBill/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};

    $p->exchangeOneSet;
    is_deeply $p->listFiles(q(p8)),
{issues   =>[{"/home/phil/perl/xxx/exchangePhil/bill/1.txt" => "--------------------------------------------------------------------------------\nAt 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n",
                  "/home/phil/perl/xxx/exchangePhil/phil/1.txt" => "Hello from Phil Again!\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangePhil/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};

    owf($bf, qq(Hi Phil,\nHello from Bill to Phil));
    $b->exchangeOneSet;                                                         # Exchange with another user
    is_deeply $b->listFiles(q(b4)),
{issues   =>[{"/home/phil/perl/xxx/exchangeBill/bill/1.txt" => "--------------------------------------------------------------------------------\nAt 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n",
                  "/home/phil/perl/xxx/exchangeBill/phil/1.txt" => "Hello from Phil Again!\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangeBill/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};

    $p->exchangeOneSet;
    is_deeply $p->listFiles(q(p9)),
{issues   =>[{"/home/phil/perl/xxx/exchangePhil/bill/1.txt" => "--------------------------------------------------------------------------------\nAt 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n",
                  "/home/phil/perl/xxx/exchangePhil/phil/1.txt" => "Hello from Phil Again!\n"}],
  segments =>[{"/home/phil/perl/xxx/exchangePhil/phil/segments/1_at_00000000000000000001.data" => "At 0000.00.00 00:00:00 0000001 from phil\n\nHello from Phil\n"}]};
   }
  elsif (debug == 2)
   {ok 1 for 1..13;

    my $x        = Data::Exchange::new();                                       #Tnew
    $x->user     = hostName =~ m(\Apousadouros\Z)s ? q(phil) : q(bill);
    $x->exchange = q(/home/phil/perl/xxx/exchange/);
    $x->start;                                                                  #TData::Exchange::Details::start
   }
 }
