#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/ -I/home/phil/perl/cpan/GitHubCrud/lib/ -I/home/phil/perl/cpan/DataEditXml/lib/ -I/home/phil/perl/cpan/DataEditXmlXref/lib/ -I/home/phil/perl/cpan/DitaGBStandard/lib/ -I/home/phil/perl/cpan/FlipFlop/lib/ -I/home/phil/perl/cpan/DataEditXmlToDita/lib/ -I/home/phil/perl/cpan/DitaPCD/lib/
#-------------------------------------------------------------------------------
# Data::Edit::Xml::To::Dita - Convert multiple Xml documents in parallel to Dita
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2019
#-------------------------------------------------------------------------------
# podDocumentation
=pod

Retain existing reports until next step are available thus avoid the gap

Save latest version of test.pcd and test.xml in S3 and recover at next boot

Has to be run in two steps due to memory requirements, see: # Maximum reach on first run

=cut

package Data::Edit::Xml::To::Dita;
our $VERSION = 20190824;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump);
use Data::Edit::Xml;
use Data::Edit::Xml::Lint;
use Data::Edit::Xml::Xref;
use Data::Table::Text qw(:all);
use Dita::GB::Standard qw(:all);
use Dita::PCD qw(:all !pleaseChangeDita);
use Flip::Flop;
use GitHub::Crud;
use Scalar::Util qw(blessed);
use Time::HiRes qw(time);
use utf8;

#D1 Convert Xml to the Dita standard.                                           # Convert Xml to the Dita standard.

sub changeBadXrefToPh {0}                                                       #I Change xrefs being placed in M3 by L<Data::Edit::Xml::Xref> to B<ph>.
sub classificationMaps{0}                                                       # Create classification maps if true
sub clearCount        {&develop ? 1e4 : 1e6}                                    # Limit on number of files to clear from each output folder.
sub client            {q()}                                                     # The name of the client
sub conversion        {$ENV{xrefConversion}||&conversionName}                   #I Conversion name
sub conversionStep    {1}                                                       #I Names each step in a multi-step conversion.
sub convert           {1}                                                       # Convert documents to Dita if true.
sub debug             {0}                                                       # Debug if true.
sub deguidize         {0}                                                       # 0 - normal processing, 1 - replace guids in hrefs with their target files to deguidize dita references. Given href g1#g2/id convert g1 to a file name by locating the topic with topicId g2.
sub ditaXrefs         {0}                                                       # Convert xref hrefs expressed as just ids to Dita format - useful in non Dita to Dita conversions for example: docBook
sub docSet            {$ENV{xrefDocSet}//q()}                                   #I Select set of documents to convert.
sub download          {$ENV{xrefDownload}//&develop ? 0 : 1}                    # Download from S3 if true.
sub downloadSize      {1e8}                                                     # Minimum size a folder must have to qualify for a parallel download
sub editOutputFiles   {0}                                                       # 1 - edit output files with xref information available if at 100% lint, 2 - edit output files regardless of Lint results, 0 - do not edit output files
sub exchange          {0}                                                       # 1 - upload to S3 Exchange if at 100% lint, 2 - upload to S3 Exchange regardless, 0 - no upload to S3 Exchange.
sub exchangeItems     {q()}                                                     # The items to be uploaded to the exchange folder: d - downloads, i - in, p - perl, o - out, t - topic trees. Reports are uploaded by default
sub extendedNames     {0}                                                       # Expected number of output topics or B<undef> if unknown
sub fixBadRefs        {0}                                                       # Mask bad references using M3: the Monroe Masking Method if true
sub fixDitaRefs       {q()}                                                     # Fix references in a corpus of L<Dita> documents that have been converted to the L<GBStandard> by naming the targets folder.
sub fixFailingFiles   {0}                                                       # Fix failing files in the L<testFails|/testFails> folder if this attribute is true
sub fixRelocatedRefs  {1}                                                       # Fix references to (re|un)located files that adhere to the GB standard.
sub fixXrefsByTitle   {0}                                                       # Fix failing xrefs by looking for the unique topic with a title that matches the text of the xref.
sub getFileUrl        {q(/cgi-bin/uiSelfServiceXref/client.pl?getFile=)}        #I A url to retrieve a specified file from the server running xref used in generating html reports. The complete url is obtained by appending the fully qualified file name to this value.
sub hits              {Flip::Flop::hits(0)}                                     # 1 - track hits so we can see which transformations are actually being used - normally off to avoid the overhead
sub indexWords        {0}                                                       # Have L<Xref> index words to topics if true and save them in the named file
sub lint              {1}                                                       # Lint output xml
sub makeXml           {&download}                                               # Convert downloaded documents to xml in the L<in> folder if true
sub maximumProcesses  {numberOfCpus(4)}                                         # Maximum number of conversion processes to run in parallel.
sub notify            {0}                                                       # 1 - Broadcast results of conversion if at 100% lint, 2 - broadcast regardless of error count.
sub numberOfFiles     {undef}                                                   # Expected number of output files
sub oxygenProjects    {0}                                                       # Create oxygen project files if true
sub pleaseChangeDita  {$ENV{xrefPleaseChangeDita} // 0}                         #I 0 - no PCD processing, 1 checkout PCD processing with lots of reporting in the log, 2 - optimized PCD processing, higher performance, fewer messages
sub printTopicTrees   {1}                                                       # 1 - print the parse tree before cutting out the topics
sub removeLint        {1}                                                       # 1 - Remove lint comments after renaming files back to input files as far as possible
sub renameToInput     {0}                                                       # 1 - rename output files to input file names where possible if at 100% lint, 2 - rename file to match input files regardless
sub restructure       {0}                                                       # 1 - Restructure results of conversion if at 100% lint, 2 - restructure regardless of error count.
sub restructurePhases {1}                                                       # Number of restructuring phases to run
sub showPhaseTimes    {1}                                                       # Show phase times it true
sub testMode          {0}                                                       # 1 - run development tests, 2- run standalone tests, 0 run production documents - this should be replaced with additional source urns recognized like file: or folder so that we gain orthogonality
sub titleOnly         {0}                                                       # Use only the title of topics to create GB Standard file names otherwise use the following text as well if the title is too short
sub upload            {&develop ? 0 : 1}                                        # Upload to S3 Bucket if true and the conversion is at 100%, 2 - upload to S3 Bucket regardless, 0 - no upload to S3 Bucket.
sub uploadSize        {downloadSize}                                            # Minimum size a folder must have to qualify for a parallel download
sub useZippedDownLoads{1}                                                       # 1 - a zipped copy of the files downloaded from L<S3> is reloaded into L<S3> and reused on subsequent runs to speed up future downloads from L<S3>.  Switch this off for just one run if you change the input files on L<S3> to force a refresh of the intermediate zip file.
sub version           {q()}                                                     # Description of this run as printed in notification message and title
sub xref              {1}                                                       # 1 - Xref output xml if at 100% lint, 2 - Xref regardless of lint.
sub xrefAddNavTitles  {1}                                                       # Add navtitles to bookmap entries if true
sub xrefAllowUniquePartialMatches{1}                                            # Allow partial matching - i.e ignore the stuff to the right of the # in a reference if doing so produces a unique result
sub xrefDeleteUnusedIds{1}                                                      # Have Xref delete any unused ids if true
sub xrefMatchTopics   {0}                                                       # Either 0 for no topic matching or the percentage confidence level from 0 to 100 percent for topic matching by vocabulary used

sub line              {$ENV{xrefLine}||q()}                                     #I For www driven conversions: create sub folders allowing multiple conversions to coexist. For terminal conversions use a flatter file structure as it is easier to navigate.

sub catalog           {q(/home/phil/r/dita/dita-ot-3.1/catalog-dita.xml)}       # Dita catalog to be used for linting.
sub develop           {-e q(/home/ubuntu/) ? 0 : 1}                             # Production run if this file folder is detected otherwise development.
sub ditaBin           {fpf(qw(/home phil r dita dita-ot-3.1 bin dita))}         # Location of Dita tool
sub downloads         {fpd(&home,    qw(download), line)}                       # Downloads folder with line indication
sub errorLogFile      {fpe(&perl,    q(eee).line, q(txt))}                      # Error log file.
sub exchangeHome      {fpd(qw(/home phil x aws))}                               # Home of exchange folder
sub fails             {fpd(&reports, qw(fails))}                                # Copies of failing documents in a separate folder to speed up downloading.
sub hitsFolder        {fpd(&home,    qw(hits), line)}                           # Folder containing at method hits by process id
sub home              {confess q(Please override the home attribute!")}         # Home folder containing all the other folders.
sub imageCache        {fpd(home,     qw(imageCache))}                           # Converted images are cached here to speed things up
sub in                {fpd(&home,    qw(in), line)}                             # Input documents folder.
sub inputExt          {qw(.htm .html .dita .ditamap .xml)}                      # Extension of input files.
sub indexWordsFolder  {fpd(home,     q(words), line)}                           # Folder in which to save indexes from words to topics and topics to words
sub out               {fpd(&home,    qw(out), line)}                            # Converted documents output folder.
sub outExtTopic       {q(dita)}                                                 # Preferred output extension for a topic
sub outExtMap         {q(ditamap)}                                              # Preferred output extension for a map
sub parseFailed       {fpd(&home,    qw(parseFailed), line)}                    # Folder for details of xml parse failures
sub pcdFile           {$ENV{xrefPcdFile} // fpd(&perl, qw(pcdFile.pcd))}        # Source of PCD files
sub perl              {fpd(&home,    qw(perl))}                                 # Perl folder.
sub pidFile           {$ENV{xrefPidFile} // fpe(&perl, q(pid), line, q(data))}  # File containing the pid for the current conversion process
sub process           {fpd(&home,    qw(process))}                              # Process data folder used to communicate results between processes.
sub renamed           {fpd(home,     qw(renamed), line)}                        # Renamed output folder
sub reports           {fpd(&home,    qw(reports), line)}                        # Reports folder.
sub reportsRenamed    {fpd(&reports, qw(renamed), line)}                        # Reports folder for file renaming
sub runLogFile        {fpe(&perl,    qw(zzz), line, q(txt))}                    # Run log file.
sub s3Exchange        {fpd(qw(exchange.ryffine users aws), client, line)}       # Exchange folder on S3
sub s3Profile         {q(fmc)}                                                  # Aws cli profile keyword value if any.
sub s3Parms           {q(--quiet --delete)}                                     # Additional S3 parameters for uploads and downloads.
sub s3ParmsDown       {s3Parms}                                                 # Additional S3 parameters for downloads.
sub s3ParmsUp         {s3Parms}                                                 # Additional S3 parameters for uploads.
sub sourceFromDefault {q()}                                                     # A default source of input to make testing easier
sub sourceFrom        {$ENV{xrefSourceFrom} || sourceFromDefault}               #I Source of input, either github:userid/repository or s3://bucketName/folderName or file://local file name
sub sources           {fpd(&home,    qw(sources), line)}                        # Duplicates the out file structure - each file there-in shows us the source file it came from
sub summaryFile       {fpe(reports,  qw(summary txt))}                          # Summary report file.
sub targets           {fpd(&home,    qw(targets), line)}                        # Duplicates the in file structure - each file there-in shows us where the original file went
sub tests             {fpd(&home,    qw(tests/in))}                             # Folder containing test input files received from test developer at L<testExchangeIn|/testExchangeIn>
sub testExpected      {fpd(&home,    qw(tests/expected))}                       # Folder containing test results expected.
sub testExchangeIn    {undef}                                                   # Exchange folder in which to receive tests so that test writers can disarrange their exchange folders as they please without disrupting testing at this end.
sub testExchangeOut   {undef}                                                   # Exchange folder to publish tests results in so test writers can see the results in at L<testResults|/testResults>
sub testResults       {fpd(&home,    qw(tests/results))}                        # Folder containing actual test results locally, copied to: L<testExchangeOut|/testExchangeOut>
sub testStandAlone    {fpd(&home,    qw(tests/standalone/active))}              # Folder containing standalone tests which is used instead of regression tests if content is present
sub testFails         {fpd(&home,    qw(fails))}                                # Folder containing failing files to be fixed by reprocessing them but only if fixFailingFiles is true
sub testFails2        {fpd(&home,    qw(fails2))}                               # Folder containing files still unfixed by the current set of fixes
sub topicTrees        {fpd(&home,    qw(topicTrees), line)}                     # Folder to contain printed topic trees if requested by printTopicTrees
sub user              {q(phil)}                                                 # Aws userid
sub www               {fpd(qw(/var www html))}                                  # Web server folder - general
sub wwwFolder         {fpd(www, q(gh))}                                         # Web server folder - conversion
sub zipOnWwwFile      {fpe(wwwFolder, qw(results zip))}                         # Conversion results on web server
#ub zoomTopicsFolder  {fpd(&home, qw(zoom), dsf)}                               # 2019.09.17 perhaps not a good idea ? the output topics here after upload so that they are accessible to "Dynamic Zoom" on the web server

my $startTime = time;                                                           # Start time.
my $endTime;                                                                    # End time value.
my $runTime;                                                                    # Run time value.

sub startTime         {$startTime}                                              # Start time of run in seconds since the epoch.
sub endTime           {$endTime}                                                # End time of run in seconds since the epoch.
sub runTime           {$runTime}                                                # Elapsed run time in seconds.

our $standAlone;                                                                # When true we are operating in stand alone mode to test documents from testStandalone in isolation
our $projects    = {};                                                          # Projects == documents to convert.
our $lintResults;                                                               # Lint results.
our $lintReport;                                                                # Lint report if available.
our $xrefResults;                                                               # Results from Xref

sub s3ProfileValue                                                              #P S3 profile keyword.
 {return '' unless my $p = s3Profile;
  qq( --profile $p)
 }

sub conversionName                                                              #P Conversion name.
 {return q(Free ).&client.q( from SDL) if deguidize;                            # Must be escape from SDL if we are deguidizing
  my   @w = q(Convert).(&client ? q( ).&client : q()).q( to Dita);
  push @w,  q(docSet: ).docSet         if docSet;
  push @w,  q(version: ").version.q(") if version;
  push @w,  sourceFrom                 if sourceFrom;
  join ', ', @w;
 }

sub urlName($)                                                                  #P Given a file name in L<www> return its url on the current server or confess if the file is not in L<www>
 {my ($f) = @_;                                                                 # File
  my $i = awsCurrentIp;
  my $u = swapFilePrefix($f, www, qq(http://$i/));
  $f eq $u and confess "File is not in www:\n$f";
  $u
 }

sub replacePid                                                                  #P Stop any existing conversion and make this conversion the current one
 {my $d = {};                                                                   # Processing status
  if (-e pidFile)
   {if ($d = evalFile(pidFile))                                                 # Take over existing pid file perhaps produced by uiSelfService
     {if (my $pid = $d->{pid})                                                  # Kill existing processes
       {if (my @pids = childPids($pid))
         {lll "Kill process: $pid";
          my $c = qq(kill ).join ' ', sort {$a <=> $b} @pids;
          lll qq($c);
          xxx qq($c);
         }
       }
     }
   }
  $d = {%$d, started=>time, status=>q(running), sourceFrom=>sourceFrom,
        pid=>$$};
  dumpFile(pidFile, $d);
 }

sub removePid                                                                   #P Remove the pid file as the conversion has completed
 {my $d = {};                                                                   # Processing status
     $d = evalFile(pidFile) if -e pidFile;

  $runTime = time - $startTime;

  dumpFile(pidFile, {%$d, finished => time, status => q(finished),              # Update status
    runTime => $runTime});
 }

sub formatTables($$%)                                                           # Create both html and text version of each report - html is better in a browser, text is easier on a server
 {my ($data, %options) = @_;                                                    # Cross referencer, table to be formatted, options

  cluck "No file for table"    unless $options{file};                           # Check for required options
  cluck "No columns for table" unless $options{columns};
  cluck "No title for table"   unless $options{title};

  my $prefixes = [&in, &out, &reports,  fp(&pcdFile)];                           # Choose file prefixes to be clickable

  formatHtmlAndTextTables(&reports, &reports, &getFileUrl, $prefixes,           # Format table in both text and htnl
    $data, %options);
 }

my %atHits;                                                                     # Track hits via the at method which is more stable than line number
my $startProcess = -1;                                                          # The process leader

sub setAtHits                                                                   #P Set hit tracking
 {if (hits)                                                                     # Only when hit tracking is enabled
   {*dexAt = *Data::Edit::Xml::at;                                              # Existing at method
    $startProcess = $$;
    sub at                                                                      # Replacement at method
     {if (my $r = &dexAt(@_))                                                   # Normal at succeeded
       {$atHits{join '_', @_[1..$#_]}++;                                        # Record at context
        return $r;                                                              # Return result
       }
      undef                                                                     # At failed
     }

    *Data::Edit::Xml::at = *at;                                                 # Reassign at method so that the hits can be tracked
    Flip::Flop::hits();                                                         # Reset hits to avoid overhead
   }
 }

sub analyzeHits()                                                               #P Analyze the hits to find "at" calls that always fail so we can consider them for removal
 {my %hits;                                                                     # Hits encountered
  for my $file(searchDirectoryTreesForMatchingFiles(hitsFolder))                # Hits in each process
   {my $hits = evalFile($file);                                                 # Load hits for a process
    $hits{$_} += $$hits{$_} for keys %$hits;                                    # Accumulate hits from each process
   }
  my @s = readFile($0);                                                         # Read source file looking for "at" calls
  my @miss; my @hits;                                                           # At that never hit. At that hit.
  for my $i(keys @s)                                                            # Each line by number
   {my $line = $s[$i];                                                          # Line content
    if ($line =~ m(\w+((_\w+)+))s)                                              # Look for at call
     {my $at = substr($1, 1);                                                   # Remove leading underscore
      if (my $n = $hits{$at})                                                   # Number of hits
       {push @hits, [$i+1, $n, $line];                                          # Record hit
       }
      else
       {push @miss, [$i+1, $line];                                              # Record miss
       }
     }
   }
  formatTables([@miss],                                                         # Report misses
    columns => <<END,
Line The line containing the "at" call that failed to hit
At   The text of the "at" call
END
     title  => q(Calls to "at" that missed.),
     head   => qq(Found NNNN lines in $0 containing "at" that missed on DDDD),
     file   => fpe(qw(misses txt)),
   );

  formatTables([@hits],                                                         # Report hits
    columns => <<END,
Line  The line containing the "at" call that succeeded
Count The number of times this "at" succeeded
At    The text of the "at" call
END
     title  => q(Calls to "at" that succeeded.),
     head   => qq(Found NNNN lines in $0 containing "at" that succeeded on DDDD),
     file   => fpe(qw(hits txt)),
   );
 }

END                                                                             # Dump hits by process at the end
 {dumpFile(fpe(hitsFolder, $$, q(data)), \%atHits) if hits and keys %atHits;
  if ($startProcess == $$)                                                      # Analyze the hits if we are the main process
   {analyzeHits;                                                                # Analyze hits
    clearFolder(hitsFolder, clearCount);                                        # Rather than letting the files accumulate
   }
 }

#D1 Methods                                                                     # Methods defined in this package.

#sub ddd(@)                                                                      #P Log development messages
# {my (@m) = @_;                                                                 # Messages
#  goto &lll if debug;
# }

sub eee(@)                                                                      #P Log error messages
 {my (@m) = @_;                                                                 # Messages
  my $m = join ' ', dateTimeStamp, "Error: ", @_;                               # Time stamp each message
  my ($p, $f, $l) = caller();
  $m .= " at $f line $l\n";

  appendFile(errorLogFile, ($m =~ s(\s+) ( )gsr).qq(\n));
  say STDERR $m;
 }

sub sourceFromGitHub                                                            #P Source is on GitHub
 {sourceFrom and sourceFrom =~ m(\Ahttps?://github.com)is
 }

sub sourceFromS3                                                                #P Source is on S3
 {sourceFrom and sourceFrom =~ m(\As3:)is;
 }

sub sourceFromFile                                                              #P Source is a local file
 {my $s = sourceFrom;
  return $s =~ s(\Afile:) ()gsr if $s and $s =~ m(\Afile:)is;
  undef
 }

sub s3InputFolder                                                               #r S3 input folder
 {return sourceFrom if sourceFromS3;
  undef
 }

sub s3OutputFolder                                                              #r S3 output folder
 {return sourceFrom &&
         sourceFrom =~ s(originals) (results)r =~ s(Originals) (Results)r
    if sourceFromS3;
  undef
 }

sub s3ExchangeFolder                                                            #P S3 exchange folder
 {q(s3://).fpd(s3Exchange, docSet ? docSet : ());                               # With docSet indication
 }

sub copyToAws                                                                   #P Copy to aws
 {my $aws = awsIp;                                                              # Get ip address of aws
  lll "Upload ".conversion." to AWS at $aws";                                   # Title

  my $perl = fpd(perl);
  my $user = user;
  owf(fpe(perl, qw(batchOnAws pl)), <<END2);                                    # Create at command file
at now <<END
perl ${perl}convert.pl 2>${perl}zzz.txt
END
END2

  my @foldersToUpload =                                                         # Sync folders
   (perl,
    fp(catalog),
    q(/home/phil/perl/cpan));

  for my $f(@foldersToUpload)
   {my $c = qq(rsync -mpqrt --del $f/ phil\@$aws:$f);
    lll qq($c);
    lll qx($c);
   }

  if (1)                                                                        # Run command on aws
   {my $c = qq(ssh $user\@$aws "bash $perl/batchOnAws.sh");
    lll qq($c);
    lll qx($c);
   }

  Flip::Flop::action();
  lll "Done!";
 }

sub getFromAws                                                                  #P Get results from Aws
 {my $home  = home;                                                             # Home folder
  my $aws   = awsIp;                                                            # Get ip address of aws
  lll "Download ".conversion." to AWS at $aws";                                 # Title

  my $user  = user;
  for my $dir(out, reports)
   {my $c   = qq(rsync -mpqrt --del $user\@$aws:$dir $dir);
    lll qq($c);
    lll qx($c);
   }

  Flip::Flop::action();
  lll "Done!";
 }

sub pleaseSee                                                                   #P AWS command to see results
 {my $success = $lintReport && $lintReport->totalErrors == 0;
  my $p = qq(Please see:);
  my @p;                                                                        # Where to see the results

  if (sourceFromFile || sourceFromGitHub)                                       # Input from GitHub so output to web server
   {#push @p, urlName(zipOnWwwFile);                                            # Shows the ip address which is probably not what we want
   }
  else                                                                          # Output in S3
   {if (&upload   and &s3OutputFolder and  $success || &upload   == 2)
     {push @p, qq(aws s3 sync ).fpd(s3OutputFolder, line()//q());
     }

    if (&exchange and &s3Exchange     and !$success || &exchange == 2)          # Output in exchange
     {push @p, s3Exchange;
     }
   }

  return join ' ', $p, join " or ", @p if @p;
  q()
 }

sub pleaseSeeReport                                                             #P Report please see
 {say STDERR pleaseSee;
 }

sub mifToXml($)                                                                 #P Convert Mif to Xml
 {my ($inputFile) = @_;                                                         # File containing mif
  lll "Convert mif file: $inputFile";

  my @xml;                                                                      # Generated Xml
  my @stack;                                                                    # Bracket stack

  my $string = readFile($inputFile);                                            # Read mif source
  my @lines = split /\n/, $string;                                              # Lines of input

  for my $i(keys @lines)                                                        # Fix strings spread across several lines
   {my $line = $lines[$i];
    if ($line =~ m(\A\s*<\w+\s*`)s and $line !~ m('>\s*\Z))                     # Non terminated string line
     {for my $j($i+1..$#lines)                                                  # Following lines
       {my $l = $lines[$j];
        if ($l !~ m('\s*\Z))                                                    # Not terminated
         {$lines[$i] .= $l;
          $lines[$j] = qq(# Moved to line $i);
         }
        else                                                                    # Terminated, pull in closing > from next line
         {$lines[$i]  .= $l.q(>);
          $lines[$j]   = qq(# Moved to line $i);
          $lines[$j+1] = qq(# Moved to line $i);
          last;
         }
       }
     }
   }

  my @image;
  my $image = undef;                                                            # In an image when set
  for my $i(keys @lines)
   {my $line = $lines[$i];
    if ($line =~ m(\A<Book ))                                                   # Ignore Book tag
     {next;
     }
    if (!$image)                                                                # Not in an image
     {if ($line =~ m(\A=(DIB|EMF|FrameVector|OLE2|TIFF|WMF))s)                  # Image types
       {$image = $1;
        next;
       }
      if ($line =~ m(\A<MIFFile)s)                                              # Skip file format
       {next;
       }
      if ($line =~ m(\A\s*<(\w+)\s*(.*?)>\s*\Z))                                # Entity all on one line
       {my ($tag, $Value) = ($1, $2);
        if ($Value =~ m(\A`(.*)'\Z)s)
         {$Value = $1;
         }
        my $value = Data::Edit::Xml::replaceSpecialChars($Value);
        $line = qq(<$tag value="$value"/>);
       }
      elsif ($line =~ m(\A\s*<(\w+)\s*\Z))                                      # Opening entity
       {push @stack, $1;
        $line = qq(<$1>);
       }
      elsif ($line =~ m(\A(\s*)>\s*#\s*end\s*of\s*(\w+)\s*\Z))                  # Closing entity
       {if (@stack)
         {my $w = pop @stack;
          if ($2 eq $w)
           {$line = $1.q(</).$w.q(>);
           }
          else
           {confess "Stack mismatch, expected $w, got $2 at line $i:\n$line\n".dump(\@stack);
           }
         }
        else
         {confess "Stack empty, but got $2 at line $i";
         }
       }
      elsif ($line =~ m(\A(\s*)>\s*\Z))                                         # Closing entity not specified
       {if (@stack)
         {my $w = pop @stack;
          $line = $1.q(</).$w.q(>);
         }
        else
         {confess "Stack empty, but got $2 at line $i";
         }
       }
      if ($line =~ m(\A#))                                                      # Remove comment lines
       {$line = '';
       }
      push @xml, $line;
     }
    else                                                                        # Process an image
     {if ($line =~ m(\A=))                                                      # End of an image
       {shift @image;                                                           # Remove initial &%v
        my $content = join '', @image;
        my @content = split //, $content =~ s(\r) ()gsr;                        # Remove spaces in image
        my $binary = '';                                                        # Binary content of image
        my $state = 0;                                                          # 0 - in character mode, 1 in hex mode
        for(my $j = 0; $j < @content; ++$j)
         {my $c = $content[$j];
          if ($state == 0)
           {if ($c eq qq(\\))
             {my $d = $content[$j+1];
              if ($d eq q(x))                                                   # Convert to x
               {$state = 1; ++$j;
               }
              elsif ($d eq qq(\\))                                              # A real back slash
               {$binary .= qq(\\); ++$j;
               }
              elsif ($d =~ m([rn]))                                             # r|n
               {++$j;
               }
              else
               {fff $i, $inputFile, "Backslash followed by $d not n|r|x|\\";
               }
             }
            else
             {$binary .= $c;
             }
           }
          else
           {my $d = $content[$j+1];
            if ($c eq q(\\))
             {if ($d eq q(x))
               {$state = 0; ++$j;
               }
              else
               {confess "Backslash followed by $d not x";
               }
             }
            else
             {$binary .= chr hex qq($c$d);
              ++$j;
             }
           }
         }

        my $i = gbBinaryStandardCreateFile                                      # Return the name of a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content> and whose accompanying B<.imageDef> file contains the specified B<$name>.  Such a file can be copied multiple times by L<gbBinaryStandardCopyFile|/gbBinaryStandardCopyFile> regardless of the other files in the target folders while retaining the original name information.
         ($inputFile, $binary, lc($image), qq($inputFile at line $i));

        push @xml, qq(<image href="$i"/>);                                      # Xml to image

        if ($line =~ m(\A=EndInset)s)                                           # End of images
         {$image = undef;                                                       # Finished image cutting out
         }
        else                                                                    # Another image
         {$image = lc $line =~ s(\A=|\n|\r) ()gr;                               # Image type
         }
        @image = ();
       }
      else
       {push @image, substr($line, 1);
       }
     }
   }

  my $text = join "\n", qq(<mif>\n), @xml, qq(</mif>\n);                        # Write results - pretty printed if possible
  my $out  = fpe(fpn($inputFile), q(xml));
  if (my $x = eval {Data::Edit::Xml::new($text)})                               # Parse xml
   {owf($out, -p $x);                                                           # Indented
   }
  else
   {owf($out, $text);                                                           # Output as xml and return file
   }

  $out
 } # mifToXml

sub convertImageToSvg($)                                                        #P Convert a graphics file to svg
 {my ($file) = @_;                                                              # File to convert
  return if $file =~ m((ole2)\Z)s;                                              # Add file types we cannot process correctly here to prevent them from being converted on each run

  my $svg = setFileExtension($file, q(svg));                                    # Converted image name

  my $cached = fpe(imageCache, fn($file), q(svg));                              # Cached image conversion - cannot use GB yet because the file will be called for by name

  if (-e $cached)                                                               # Reuse cached version
   {copyBinaryFile($cached, $svg);
   }
  else                                                                          # Convert and cache the image.
   {my $e = fe($file);
    my $c = qq(unoconv  -f svg -o "$svg" "$file");                              # Libre office
       $c = qq(pdftocairo -svg "$file"  "$svg") if $e =~ m(pdf)is;              # Pdf
       $c = qq(convert         "$file"  "$svg") if $e =~ m(tiff)is;             # Tiff
       $c = qq(inkscape -e "$svg" "$file")      if $e =~ m(emf)is;              # Inkscape

    lll qx($c);                                                                 # Convert

    if (-e $svg)                                                                # Check results
     {copyBinaryFile($svg, $cached);
     }
    else
     {lll "Unable to convert image file to svg:\n$file";                        # Need to extend converter to new image type
     }
   }
 }

sub downloadBefore                                                              #r Perform just before downloads start
 {
 }

sub downloadAfter                                                               #r Perform just after downloads finish
 {
 }

sub downloadFiles                                                               #r Download documents from S3 or GH to the L<downloads|/downloads> folder.
 {if (&download)                                                                # Download if requested
   {lll "Download phase started";

    &downloadBefore;

    makePath(downloads);                                                        # Make folder as aws cli will not

    if (my $f = sourceFromFile)                                                 # Convert local source file
     {clearFolder(downloads, clearCount);                                       # Clear last run
      if (-f $f)
       {copyFileToFolder($f, downloads);                                        # Copy file
       }
      else                                                                      # Log non existence of source file but carry on regardless
       {lll "Source file does not exist:\n$f";
       }
     }

    elsif (sourceFromGitHub)                                                    # Get source files from GitHub
     {clearFolder(downloads, clearCount);                                       # Clear last run as unzip will not
      my $r = sourceFrom;                                                       # Url of repo on GitHub
      my $s = fpe($r, qw(archive master zip));                                  # Url to GitHub to retrieve zipped repository
      my $t = makePath fpe downloads, qw(gh zip);                               # Download target
      lll "Download from $r";
      my $R = xxx(qq(wget -O $t $s), qr(200 OK));                               # Run download
      $R =~ m(ERROR 404: Not Found)s || !-e $t || fileSize($t) < 1e1 and        # Make sure we got a zip file
         confess "No such user/repo on GitHub or repo too small:\n$r\n$R\n";
     }

    elsif (my $s = s3InputFolder)                                               # Get source files from S3 - no need to clear as aws s3 sync will
     {my %s3o = (profile => s3Profile, cleanUp => 1);                           # S3 options
      my $zip = fpe($s =~ s(/\Z) ()r, q(zip));                                  # Zip file on S3 - remove the trailing / if any on input folder to obtain  zip file name
      my $Zip = fpe(&downloads, qw(s3 zip));                                    # Local copy of zip file
      my $dwn = downloads;

      if (useZippedDownLoads and s3FileExists($zip, %s3o))                      # Check for the zipped version of the corpus first as it is much faster to download
       {lll "Download previously created zip file: $zip to $Zip";
        clearFolder(&downloads, 1e7);
        s3ReadFile($zip, $Zip, %s3o);
        if (-e $Zip)                                                            # Unzip the downloaded zip file
         {my $c = qq(unzip -d $dwn -qn $Zip);
          lll $c;
          unless(xxx $c)                                                        # Remove zip file if there were no problems with the unzip
           {unlink $Zip;
           }
         }
       }
      else                                                                      # Download in parallel if no zipped version of the corpus
       {lll "Download in parallel from $s to", downloads;
        syncFromS3InParallel downloadSize, $s, downloads, s3Profile, q(--delete); # Download from S3 in parallel
        unless(fork)                                                            # Create and upload a zipped version to speed up future downloads
         {my $z = fpe(temporaryFile, q(zip));                                   # Local zip file
          my $c = qq(cd $dwn; zip -qr $z .);                                    # Zip command
          lll $c;
          xxx $c;
          s3WriteFile($zip, $z, %s3o);                                          # Upload to S3
          lll "Zipped version of downloads uploaded to:\n$zip";
          unlink $z;                                                            # Remove temporary zip file
          exit;
         }
       }
     }

#   else                                                                        # Get source files from S3 - no need to clear as aws s3 sync will
#    {my $s = s3InputFolder;                                                    # Download from S3
#     my $t = downloads;                                                        # Download target
#     my $p = s3ParmsDown.s3ProfileValue;                                       # Cli parameters
#     lll "Download in series from $s to $t";
#     xxx qq(aws s3 sync $s $t $p);                                             # Run download
#    }

# Improve by using files command to determine what kind of archives we have, then keep unzipping with indicated unzipper until all such files and their contents have been unzipped
    if (0)                                                                      # Enable as soon as word is incorporated as SalesForce has 165 docx files to process.
     {for my $f(searchDirectoryTreesForMatchingFiles(downloads, qw(.doc .docx)))# Convert doc files
       {my $t = setFileExtension($f, q(odt));                                   # Set extension
        my $c = qq(unoconv -f odt -o "$t" "$f");                                # Command to convert to odt ignoring weird message
        lll $c;
        xxx($c, qr(line=188));                                                  # Execute command with error check
       }
     }

    if (1)                                                                      # Locate archives and rename to zip
     {my @files = searchDirectoryTreesForMatchingFiles(downloads);              # Rename archive files to zip so they get unzipped
      my $files = @files;
      lll "Search downloads for zip files amongst the $files files present";
      processFilesInParallel
        sub
         {my ($f) = @_;                                                         # Downloaded file
          if ($f and -e $f and qx(file "$f") =~ m(Zip archive data)is)          # Check for archive
           {return if $f =~ m((zip|7z|jar)\Z)s;                                 # Extension is already correct
            my $t = setFileExtension($f, q(zip));                               # Correct extension

            if (-e $t)                                                          # They might also have a zip file already in place with the same name - believe me it has happened.
             {if (fileMd5Sum($f) eq fileMd5Sum($t))                             # Confirm that the zip file duplicates the file with the same name but without a .zip extension
               {lll "Unlink duplicate zip file:\n$f";
                unlink $f;                                                      # Safe as the two files are the same
               }
              else                                                              # Name already taken by a different file
               {confess "I want to rename (1) to (2) but (2) already exists and is different from (1)\n$f\n$t\n";
               }
             }
            else
             {lll "Rename archive file to zip:\n$f\n$t";
              rename $f, $t;
             }
           }
         },
        sub {}, @files;
     }

    if (1)                                                                      # Some people put zip files inside zip files so we have to unzip until there are no more zip files to unzip
     {my %unzipped;                                                             # Unzipped files
      my $newUnzips = 0;
      for my $pass(1..3)                                                        # Let us hope that even the most ingenious customer will not zip file up to this depth!
       {#lll "Unzip pass $pass";
        for my $zip(searchDirectoryTreesForMatchingFiles(downloads, qw(.zip .7z))) # Unzip files
         {next if $unzipped{$zip}++;                                            # Skip if already unzipped
          $newUnzips++;

          lll "Unzip $zip";                                                     # Unzip
          my $d = fp $zip;                                                      # Unzip target folder
          my $D = fpd $d, fn $zip;                                              # Folder in downloads into which to unzip the file as we do not know the internal file structure
          makePath($D);                                                         # Make the path as zip will not

          my $c = qq(cd "$d"; unzip -d "$D" -qn "$zip"; rm -r "${D}__MACOSX" 2>/dev/null;);
             $c = qq(cd "$d"; 7z e "$zip" -y -o"$D") if fe($zip) =~ m(7z\Z)s;   # 7z command
          #lll $c;
          xxx $c;                                                               # Run unzip
         }
       }
     }

    if (my @mif = searchDirectoryTreesForMatchingFiles(downloads, q(.mif)))     # Locate and convert any mif files
     {my $n = @mif;                                                             # Number of mif files
      lll "Convert mif files of numerosity $n\n";                               # Title
      processFilesInParallel                                                    # Process mif files in parallel
        sub                                                                     # Convert each mif file to xml
         {my ($mif) = @_;                                                       # Mif file
          my $f = mifToXml($mif);                                               # Convert mif to xml
          eval {Data::Edit::Xml::new($f)};                                      # Trial parse
          if ($@)                                                               # Report errors
           {confess "Unable to parse file:\n$mif\n$f\n$@\n";                    # Reason for failure
           }
          1                                                                     # Return success from multiverse to universe
         }, undef, @mif;                                                        # Finish parallel conversions
     }

    if (1)                                                                      # Convert images to svg where possible - run in parallel
     {my @images = searchDirectoryTreesForMatchingFiles(downloads,
                  qw(.dib .emf .ole2 .pdf .tiff .wmf .vsd .vsdx));
      my $images = @images;
      lll "Convert images of numerosity $images";
      for my $image(@images)                                                    # Convert images in series as unoconv cannot be used in parallel - but we do cache image conversions so this runs faster in subsequent runs.
       {convertImageToSvg($image);
       }
     }

    if (1)                                                                      # Report file extensions
     {my $d = downloads;
      my $e = countFileExtensions($d);
      formatTables([map {[$$e{$_}, $_]} sort keys %$e],
        columns   => <<END,
Count      Number of times this extension appears
Extension  The extension in question
END
        title     => q(File extensions in downloads folder),
        head      => <<END,
NNNN file extensions found on DDDD in folder: $d
END
        summarize => 1,
        file      => fpe(qw(count file_extensions_in_downloads txt)));
     }

    &downloadAfter;
    Flip::Flop::download();                                                     # Downloads as completed
    lll "Download phase completed";

   }
  else
   {#ddd "Download from S3 not requested";
   }
 }

sub spelling($$)                                                                #r Fix spelling in source string
 {my ($s, $file) = @_;                                                          # Source string, file being processed
  $s
 }

sub spellingOut($)                                                              #r Fix spelling in output string
 {my ($s) = @_;                                                                 # Output string
  $s
 }

sub makeOneXmlFile                                                              #P Convert one file to utf8 and return undef if successful else the name of the document in error
 {my ($source) = @_;                                                            # File to convert

  #ddd "Convert one file to utf8: $source";                                     # Title

  my $target = swapFilePrefix($source, downloads, in);                          # Target file from source file
  makePath($target);

  my $fileType = sub                                                            # File type
   {my $c = qx(file "$source") // q(unknown file type);                         # Decode contents using B<file>

    return q(htmlAscii)   if $c =~ m(HTML document, ASCII text)s;               # Html
    return q(htmlUtf8)    if $c =~ m(HTML document, UTF-8 Unicode text)s;
    return q(htmlIso8859) if $c =~ m(HTML document, ISO-8859 text)s;
    return q(htmlNonIso)  if $c =~ m(HTML document, Non-ISO extended-ASCII)s;

    return q(xmlUtf8)     if $c =~ m(XML 1.0 document text)s;                   # Xml

    return q(ASCII)       if $c =~ m(ASCII text)s;                              # Something to be converted to Xml
    return q(ISO_8859-16) if $c =~ m(ISO-8859 text);
    return q(UTF8)        if $c =~ m(UTF-8 Unicode .*text)s;
    return q(UTF16)       if $c =~ m(UTF-16 Unicode text)s;

    my $t = readBinaryFile($source);                                            # Search unknown file for clues as to its content
    return q(UTF16)       if $t =~ m(\Aencoding="UTF-16"\Z);
    confess "\nUnknown file type $c\n\n";
   }->();

  if (isFileUtf8($source))                                                      # Copy file directly if already in utf8
   {if ($source =~ m(\.html?\Z)s and $fileType =~ m(\Ahtml)s)                   # Dita xml gets reported as html by file so further restrict the definition of what might be html
     {xxx qq(hxnormalize -x < "$source" > "$target"), qr(\A\s*\Z);              # Normalize html to xml
     }
    else
     {copyFile($source, $target);                                               # Direct copy
     }
   }
  else                                                                          # Convert file to utf8
   {if ($fileType =~ m(\Ahtml(Ascii|Utf8)))
     {xxx qq(hxnormalize -x < "$source" > "$target"), qr(\A\s*\Z);              # Normalize html to xml
     }
    elsif ($fileType =~ m(\Ahtml(Iso8859|NonIso)))                              # Normalize ISO8859 html to xml
     {xxx qq(hxnormalize -x < "$source" | iconv -c -f ISO_8859-16 -t UTF8 -o "$target" -), qr(\A\s*\Z);
     }
    else
     {xxx qq(iconv -c -f $fileType -t UTF8 -o "$target" "$source");             # Silently discard any unconvertible characters with -c !
     }
   }

  if (-e $target)                                                               # Preprocess source file
   {my $Text = readFile($target);
    my $text = $Text =~ s(encoding="[^"]+") (encoding="UTF-8")r;                # Write encoding if necessary
    $text    = spelling $text, $target;                                         # Fix spelling
    owf($target, $text) unless $text eq $Text;
    return undef
   }

  $source;
 }

sub makeXmlFiles                                                                #P Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.
 {if (makeXml)
   {clearFolder(in, clearCount);

    my @d = searchDirectoryTreesForMatchingFiles(downloads, inputExt);          # Files downloaded
    my $n = @d;
    lll "No input documents to convert to xml" unless $n;                       # Mention the non existence of documents to convert if relevant to the current proceedings
    lll "Make xml from input documents of numerosity $n" if $n;

    my @results = processFilesInParallel sub {makeOneXmlFile(@_)}, undef, @d;   # Convert files in parallel

    if (my @failed = grep {$_} @results)                                        # Consolidate results -  list of conversions that failed
     {my $t = formatTableBasic([[qw(File)], map {[$_]} @failed]);
      eee "The following source files could not be made into xml files:\n", $t;
     }
    else
     {Flip::Flop::unicode();
     }
   }
  else
   {#ddd "makeXmlFiles:- not requested";
   }
 } # makeXmlFiles

sub projectCount()                                                              #P Number of projects.
 {scalar keys %$projects
 }

sub chooseIDGroup($)                                                            #r Return the id group for a project - files with the same id group share the same set of id attributes.
 {my ($project) = @_;                                                           # Project
  q(all);
 }

sub newProject($;$)                                                             #P Project details including at a minimum the name of the project and its source file.
 {my ($source, $n) = @_;                                                        # Source file, project number

  confess "Source file does not exist:\n$source\n" unless -e $source;
  my $name = stringMd5Sum($source);                                             # Use md5 sum of content as project name

  my $p = genHash(q(Project),                                                   # Project definition
    convertTime => undef,                                                       # Number of seconds taken to convert the document
    empty       => undef,                                                       # Mark projects that are empty so they can be removed if desirable
    idGroup     => undef,                                                       # Projects with the same id group share id attributes.
    lintFailed  => undef,                                                       # Lint failed
    name        => $name,                                                       # Name of project
    number      => $n // projectCount + 1,                                      # Number of project
    parseFailed => undef,                                                       # Parse of source file failed
    parseError  => undef,                                                       # Parse error description
    pcd         => undef,                                                       # Please Change Dita execution statsitics
    source      => $source,                                                     # Input file
    sourceSize  => fileSize($source),                                           # Size of input file
    targets     => undef,                                                       # Where the items cut out of this topic wind up
    test        => undef,                                                       # Test projects write their results unlinted to testResults
   );

  $p->idGroup = chooseIDGroup($p);                                              # Choose the id group for the project
  $projects->{$name} = $p;                                                      # Save project definition
 }

sub findProjectFromSource($)                                                    #P Locate a project by its source file
 {my ($source) = @_;                                                            # Full file name
  my @p;
  my $file = swapFilePrefix($source, in);
  for my $p(values %$projects)
   {push @p, $p if swapFilePrefix($p->source, in) eq $file;
   }

  return $p[0] if @p == 1;                                                      # Found the matching project
  undef                                                                         # No such unique project
 }

sub findProjectWithLargestSource                                                #P Locate the project with the largest input source file
 {my $l;                                                                        # Project with largest input source file size
  for my $p(values %$projects)                                                  # Each project
   {$l = $p if !$l or $l->size < $p->size;                                      # Compare size
   }
  $l                                                                            # Largest project found
 }

my %failingFiles;

sub loadFailingFiles                                                            #P Find source of each failing file while developing
 {if (&develop)                                                                 # Development only
   {my $fff = fpd(&exchangeHome, &client, docSet, qw(reports fails));           # Failing files folder
    for my $file(searchDirectoryTreesForMatchingFiles($fff))
     {if (my $l = Data::Edit::Xml::Lint::read($file))
       {$failingFiles{$l->inputFile}++;
       }
     }

    my $pff = fpd(&exchangeHome, &client, docSet, qw(parseFailed));             # Files that failed to parse
    for my $file(searchDirectoryTreesForMatchingFiles($pff))                    # Each file that failed to parse
     {my $s = readFile($file);                                                  # File text
         $s =~ s(Source file is:.*\Z) ()s;                                      # Remove failure reason
      my $f = owf(fpf(&in, fne $file), $s);                                     # Place in in for reprocessing
     }
   }
 }

sub selectFileForProcessing($$;$)                                               #r Select an input file for processing
 {my ($file, $number, $failed) = @_;                                            # Full file name, project number, known to have failed on last AWS run if true
  $file
 }

sub loadProjects                                                                #P Locate documents to convert from folder L<in|/in>.
 {$projects = {};                                                               # Reset projects
  if (testMode eq q(1) or ref(testMode))                                        # Local tests
   {lll "Run local tests";                                                      # Title

    for my $file(searchDirectoryTreesForMatchingFiles(testExchangeIn, inputExt))# Copy in new tests
     {my $fileName = fn $file;
      if ($fileName =~ m(b\Z)s)                                                 # Before file
       {my $project = $fileName =~ s(b\Z) ()gsr;
        my $in = fpe(tests, $project, q(dita));                                 # Input test file name
        if (!-e $in)                                                            # Copy to local input tests folder if not already present
         {warn "Added before file for $project as:\n$in\n";
          copyFile($file, $in);
         }
       }
      elsif ($fileName =~ m(a\Z)s)                                              # After file
       {my $project = $fileName =~ s(a\Z) ()gsr;
        my $in = fpe(testExpected, $project, q(dita));                          # Expected file name
        if (!-e $in)                                                            # Copy to local expected folder if not already present
         {warn "Added after file for project $project as:\n$in\n";
          copyFile($file, $in);
         }
       }
      else                                                                      # Complain about an unexpected file
       {#warn "Ignored unexpected test file:\n$file\n";
       }
     }

    for my $file(searchDirectoryTreesForMatchingFiles(tests, inputExt))         # Load tests
     {if (testMode eq q(1) or Data::Edit::Xml::atPositionMatch($file, testMode))# Filter tests if requested
       {my $p = newProject($file);
        $p->test = 1;
       }
     }

    clearFolder($_, 1e3)                                                        # Clear results folders
      for out, parseFailed, reports, sources, targets, testResults;
   }
  elsif (testMode eq q(2))                                                      # Standalone tests
   {if (my @files = searchDirectoryTreesForMatchingFiles(testStandAlone, inputExt))
     {$projects = {};                                                           # Remove any regression tests as they will only confuse the issue
      $standAlone = @files;
      warn "Entered standalone mode with $standAlone files in folder: ",
            testStandAlone;
      for my $file(@files)                                                      # Potential test documents from Phil
       {my $project = fn $file;
        if (!$$projects{$project})
         {newProject($file);
          warn "Added test project $project source $file";
         }
       }
     }
   }
  else                                                                          # Production documents
   {loadFailingFiles if testMode == 3;
    my @files = searchDirectoryTreesForMatchingFiles(in, inputExt);

    my @selectedFiles;                                                          # Selected files
    my %fileNumber;                                                             # File->number
    for my $i(keys @files)
     {my $file = $files[$i];
      next unless selectFileForProcessing($file, $i+1, $failingFiles{$file});
      push @selectedFiles, $file;
      $fileNumber{$file} = $i;
     }

    if (@selectedFiles)                                                         # Name each project uniquely via its md5 sum
     {processFilesInParallel
        sub
         {my ($file) = @_;                                                      # Create projects
          newProject $file, $fileNumber{$file};
         },
        sub
         {my (@projects) = @_;                                                  # Consolidate projects
          reloadHashes(\@projects);
          for my $project(@projects)
           {$$projects{$project->name} = $project;
           }
         }, @selectedFiles;
     }
    else                                                                        # No input files selected
     {lll "No input files selected from: ".in."\n";
     }

    if (&develop and keys(%$projects) > 64 and 0)                               # Check project count
     {my $n = keys %$projects;
      my $m = qq(Too many projects selected for processing, count: $n\n);
      lll $m, dump($projects);
      confess $m;
     }
   }

  my @sizes = sort {($$b[0] <=> $$a[0])}
              map {[$_->sourceSize//0, $_->source]}
              values %$projects;

  formatTable([@sizes], <<END,                                                  # Report projects by size so we can detect potentially long running ones
  Size   Source file size
  Source Source file
END
      title     => q(Source files by size),
      head      => <<END,
NNNN source files to process on DDDD listed in order of descending size
END
      file      => fpe(reports, qw(lists source_files_by_size txt)));
 }

sub formatXml($)                                                                #r Format xml
 {my ($x) = @_;                                                                 # Parse tree
  $x->ditaPrettyPrintWithHeaders;
 }

sub ditaConceptHeader                                                           #P Dita concept header
 {<<END =~ s(\s*\Z) ()gsr;                                                      # Header for a concept
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE concept PUBLIC "-//OASIS//DTD DITA Task//EN" "concept.dtd" []>
END
 }

sub parseFile($)                                                                #P Parse a file
 {my ($file) = @_;                                                              # File
  my $e = fpe(parseFailed, $$, q(xml));                                         # Errors file
  my $x = eval {Data::Edit::Xml::new($file, errorsFile=>$e)};                   # Parse the source file

  if ($@)                                                                       # Report parse failure
   {eee join "\n", "Failed to parse file, errors in file:", $file, $e;          # Failure message
    return undef;
   }

  $x                                                                            # Return parse tree
 }

sub parseProject($)                                                             #r Parse a project.  Originally the results were cached for later reuse but it takes on only a few milliseconds and the cached results slowed the AMI start up.
 {my ($project) = @_;                                                           # Project
  my $s = $project->source;                                                     # Source file
  my $x = eval {Data::Edit::Xml::new($s)};                                      # Parse the source file
  return $x if $x;                                                              # Source file parsed successfully

  my $e = $Data::Edit::Xml::lastParseError;                                     # Parse error
  lll "Parse failure on file:\n$s\n$e\n";                                       # Report failure
  my $t = swapFilePrefix($s, &in, &parseFailed);                                # Copy file to parseFailed
  copyFile($s, $t);                                                             # Copy file

  $project->parseError  = $e;                                                   # Record parse error
  $project->parseFailed = 1;                                                    # Mark project as failing to parse

  if (1)                                                                        # Lint the existing input file and place it in the output folder to be reported on by the Dexl step.
   {my $l            = Data::Edit::Xml::Lint::new;                              # Linter
       $l->catalog   = &catalog;                                                # Catalog
       $l->file      = swapFilePrefix($s, &in, &out);                           # Output file
       $l->inputFile = $s;                                                      # Add source file information
       $l->source    = readFile($s);                                            # Source xml
       $l->lint;                                                                # Lint the file - which we know has errors so it will be reported as in error.
   }

  undef                                                                         # Project failed to parse
 }

sub createTopicId($$)                                                           #r Create the topic id for a new topic
 {my ($project, $x) = @_;                                                       # Project, parse tree

  $x->createGuidId;                                                             # Create an md5 topic id for the topic saving any existing one as a label. The new topic id is dependent solely on the content of the topic and so maintains the GB standard.
 }

sub createTopicIdIfNeeded($$)                                                   #r Create a topic id for a new topic if one has not already been supplied
 {my ($project, $x) = @_;                                                       # Project, parse tree
  return if $x->id;                                                             # We already have a topic id
  $x->createGuidId;                                                             # Create an id for the topic saving any existing one as a label
 }

sub createTopicIdIfNeededFromBaseFileName($$)                                   #r Create a topic id for a new topic if one has not already been supplied using the base file name of the input file.  This breaks the GB Standard - which requires that the topic filename be chosen consistently solely from the content of the file.  The only thing thing that can be said in defence is that in most cases the requester of this method, SalesForce, had already used the file base name as the topic id, this method then is used to fill in the blanks on those rare occasions that they failed to follow their own standard.
 {my ($project, $x) = @_;                                                       # Project, parse tree
  #cluck "Deprecated as it violates the GB Standard";
  return if $x->id;                                                             # We already have a topic id
  $x->id = nameFromString fn $project->source;                                  # Create the topic id
 }

sub chooseTopicFileName($$$$)                                                   #r Choose the name of the file to hold the new topic from the title of the topic - the Monroe Title Method - the ideal and normal solution.
 {my ($project, $x, $source, $extension) = @_;                                  # Project, parse tree, source, extension
  gbStandardFileName($source, $extension);                                      # GB Standard file name
 }

sub chooseTopicFileNameUsingTopicId($$$$)                                       #r Choose the name of the file to hold the new topic - this version derives the topic name from the topic id which does not quite violate the GB standard but does lose some of the additional benefits derived from using the GB Standard in conjunction with the Monroe Title Method.
 {my ($project, $x, $source, $extension) = @_;                                  # Project, parse tree, source, extension

  if (my $i = $x->id)                                                           # Topic id
   {unless($i =~ m(\AGUID)i)                                                    # Topic id not a guid
     {my $file = gbStandardFileName($source, $extension, g=>nameFromString($i));    # GB Standard file name with G overriden as long as it is not a guid as that would be pointless in the extreme
      return $file;
     }
   }

  gbStandardFileName($source, $extension);                                      # GB Standard file name without G overriden
 }

sub chooseTopicFileNameFromSourceFile($$$$)                                     #r Choose the name of the file to hold the new topic - this version derives the topic file name from input source file name, assuming therefore, that there will be no file flattening or topic sub section cutting out.
 {my ($project, $x, $source, $extension) = @_;                                  # Project, parse tree, source, extension
  my $s = $project->source;
  my $f = swapFilePrefix($s, &in);                                              # LintTopic() will put the file into the output folder
  $f
 }

sub lintTopic($$;$)                                                             #P Lint a topic and return the lint details
 {my ($project, $x, $title) = @_;                                               # Project, parse tree, optional title of lint

  $x->id = $x->tag unless $x->id;                                               # Create a topic id for the topic if an id has not already been supplied.  Topic ids appear in Dita references but are superfluous as only the file and id components are needed to actually address an element.

# Might fail if no header is known for the tag
  my $source = spellingOut formatXml $x;                                        # Pretty print topic

  my $extension = $x->tag =~ m(map\Z)s ? &outExtMap : &outExtTopic;             # Choose output extension

  my $file = chooseTopicFileName($project, $x, $source, $extension);            # Choose topic file name

  my $l = Data::Edit::Xml::Lint::new;                                           # Linter
     $l->catalog   = &catalog;                                                  # Catalog
     $l->ditaType  = -t $x;                                                     # Topic type
     $l->file      = fpf(&out, $file);                                          # Output file
     $l->guid      = $x->id;                                                    # Guid
     $l->inputFile = $project->source;                                          # Add source file information
     $l->labels    = $x;                                                        # Add label information to the output file so when all the files are written they can be retargeted by Data::Edit::Xml::Lint
     $l->project   = $project->idGroup;                                         # Group files into Id scopes
     $l->title     = $title if $title;                                          # Optional title
     $l->source    = $source;                                                   # Source from parse tree

  if ($project->test)                                                           # Write test results unlinted
   {my $f = fpf(&testResults, fne($project->source));
    if (!-e $f)
     {$l->file = owf($f, $source);
     }
   }
  else
   {$l->lint;                                                                   # Lint the results
   }

  $project->lintFailed += $l->errors if $l->errors;                             # An output file derived from this source file failed to lint

  $l
 }

sub createTarget($$$$)                                                          #P Lint a book map
 {my ($source, $target, $sourceDocType, $targetType) = @_;                      # Source file, target file produced from source file, source document type, target type: bookmap|image|topic

  -e $source or confess "No such file:\n$source";

  my $sourceFolder = sub                                                        # Source folder
   {for my $w(in, downloads)                                                    # Possible locations from whence came this file
     {return $w if index($source, $w) == 0;                                     # File came from this source folder
     }
    confess "Unknown source folder for file:\n$source";                         # The file does not come from a known folder
   }->();

  my $sourceFile = swapFilePrefix($source, $sourceFolder, targets);             # File with the same base name as source but in targets folder
  my $targetFile = swapFilePrefix($target, &out,          sources);             # File with the same base name as target but in sources folder

  $targetType =~ m(\A(bookmap|image|topic)\Z)s or confess                       # Check target type
   "Invalid target type: $targetType";

  if    ($targetType eq q(image))                                               # Check source folder is appropriate
   {$sourceFolder eq &downloads or $sourceFolder eq &in or confess
     "Wrong source folder for image file:\n$source\n$sourceFolder";
   }
  else
   {$sourceFolder eq in or confess
     "Wrong source folder for topic or bookmap file:\n$source\n$sourceFolder";
   }

  dumpFile $targetFile, genHash(q(TargetToSource),                              # Write details of the source file that created this target file
    source        => $source,
    target        => $target,
    sourceDocType => $sourceDocType,
    targetType    => $targetType,
   );

  my $r = genHash(q(SourceToTarget),                                            # Details of the target file created from the source file
    source        => $source,
    target        => $target,
    sourceDocType => $sourceDocType,
    targetType    => $targetType,
   );

  dumpFile $sourceFile, $r;                                                     # Save details in targets/ folder

  $r                                                                            # Return source to target mapping
 }

sub lintBookMap($$$$)                                                           #P Lint a book map
 {my ($project, $x, $bookMap, $title) = @_;                                     # Project, parse tree of source, book map parse tree, title of lint

  if ($bookMap->at_bookmap)
   {$_->change_chapter_topicref for @$bookMap;                                  # Change outermost topics refs to chapters
   }

  my $lint = lintTopic($project, $bookMap, $title);                             # Lint book map

  $project->targets = createTarget                                              # Source file to target bookmap details
   ($project->source,
    $lint->file,
    $x->tag,
    q(bookmap));

  $lint                                                                         # Return lint result
 }

sub cleanUpCutOutTopic($$)                                                      #r Clean up a topic once it has been cut out and its output file has been assigned
 {my ($project, $x) = @_;                                                       # Project, parse
 }

sub cleanUpBookMap($$)                                                          #r Clean up a book map once all its topics have been cut out and its output file has been assigned
 {my ($project, $x) = @_;                                                       # Project, parse
 }

sub topicIsEssentiallyEmpty($)                                                  #P Return B<1> if the topic is essentially empty else B<undef>.
 {my ($file) = @_;                                                              # File to check
  if (fileSize($file) < 2e3)                                                    # Only consider small files
   {if (my $x = parseFile($file))                                               # Parse the source
     {my $c = $x->countNonEmptyTags                                             # Count tags with content
       (qw(concept conbody reference refbody task taskbody));
      return 1 unless keys %$c;                                                 # Return empty unless there is content
     }
   }
  0                                                                             # Topic has content
 }

my %imageFiles;                                                                 # Cache the possible image files

sub findImage($$)                                                               #P Find an image that has been misplaced
 {my ($source, $image) = @_;                                                    # Source of images, image to search for

  if (!$imageFiles{$source})                                                    # Possible images from this source
   {$imageFiles{$source} = [searchDirectoryTreesForMatchingFiles($source)];
   }

  my @imageFiles = $imageFiles{$source}->@*;

  my $L; my $I;                                                                 # Best match length so far, best target that had that match
  my $i = reverse $image;                                                       # Image name reversed
  for my $file(@imageFiles)                                                     # Possible targets
   {if (my @e = stringsAreNotEqual($i, reverse $file))                          # Find common prefix of reversed file names
     {if (my $c = $e[0])                                                        # Common prefix
       {my $l = length($c);                                                     # Length of common prefix
        if (!$L or $L < $l)                                                     # Longer length
         {$L = $l; $I = $file;                                                  # Best match so far
         }
       }
     }
   }

  return $I if $I and fne($I) eq fne($image);                                   # Check that at least the lowest level matches
  $image                                                                        # No improvement possible
 }

sub standardDitaCleanUpDefault($$)                                              #P Clean up some items that always need to be done in Dita topics
 {my ($project, $x) = @_;                                                       # Project, parse

  $x->ditaObviousChanges;                                                       # Safe because preceded by convertDocument and followed by cleanUpCutoutTopic

  $x->by(sub                                                                    # External xref
   {my ($o, $p) = @_;
    if ($o->at_xref)
     {if (my $h = $o->href)
       {if ($h =~ m(\Ahttps?://|\Awww\.|\.com\Z|\.org\Z|\Amailto:)s)
         {$o->set(scope=>q(external), format=>q(html));
         }
       }
      $o->wrapWith_p if $p and -t $p =~ m(body\Z)s;                             # Buffer xref from parent body
     }
    elsif ($o->at_table)                                                        # Fix tables
     {$o->fixTable;
      $o->wrapWith_p if $o->at_table_entry;                                     # Resolve nested tables by wrapping the table in a p
     }

    if ($o->at_p_p)                                                             # Break out p under p
     {$o->breakOutChild;
     }

    $o->tag =~ s([^-0-9a-z]) ()gis;                                             # Remove any punctuation in tag names
    if (my $attr = $o->attributes)                                              # Remove any punctuation in attribute names
     {my %attr = %$attr;
      if (my @a = grep {m(\A[^0-9a-z]*\Z)is} keys %attr)                        # Attributes with punctuation in them
       {for my $a(@a)
         {my $A = $a =~ s([^0-9a-z]) ()gisr;                                    # Attribute minus punctuation
          if (!defind($attr{$A}))
           {$attr{$A} = delete $attr{$a};
           }
         }
        $o->attributes = \%attr;                                                # Modified attributes
       }
     }

    if ($o->at_image)                                                           # Copy images across
     {if (my $h = $o->href)                                                     # Image name
       {for my $source(&downloads, &in)                                         # Possible image locations
          {my $i = sub
           {return $h if fullyQualifiedFile($h, $source);                       # Href is fully qualified to downloads/
            return swapFilePrefix($h, &in, $source)                             # Href is fully qualified to in/
              if $source ne &in and fullyQualifiedFile($h, &in);
            return $h if fullyQualifiedFile($h);                                # Href is fully qualified to somewhere else

            my $f = absFromAbsPlusRel($project->source, $h);                    # Relative image source in input
            return  swapFilePrefix($f, &in, $source);                           # Image source in downloads
           }->();
          my $f = -e $i ? $i : findImage($source, $i);                          # The right image often gets delivered in the wrong place
          if (-e $f)                                                            # Copy file across if it exists - Xref will report if it is missing
           {my $t = gbBinaryStandardCopyFile($f, &out);                         # Copy image into position - this might happen multiple times but so what - to compute is cheap!
            $o->href = fne($t);                                                 # Xref will take care of the path
            createTarget($f, $t, q(image), q(image));                           # Image to image mapping
            last;                                                               # Found a candidate for the image
           }
         }
       }
     }

   });
 } # standardDitaCleanUpDefault

sub standardDitaCleanUp($$)                                                     #r Clean up some items that always need to be done in Dita topics
 {my ($project, $x) = @_;                                                       # Project, parse
  standardDitaCleanUpDefault($project, $x);                                     # Default standard clean up
 } # standardDitaCleanUp

sub couldBeCutOut($)                                                            #P Return true if this node can be cut out
 {my ($node) = @_;                                                              # Node to test
  $node->at(qr(\A(concept|glossentry|reference|task)\Z))                        # Topics we can cut on
 }

my $compiledPcdBlocks;                                                          # Compiled pcd blocks

sub compilePcds                                                                 # Compile pcd files
 {if (pleaseChangeDita and pcdFile and -f pcdFile)
   {$compiledPcdBlocks = compilePcdFile(&pcdFile);
    my $N = @$compiledPcdBlocks;
    lll "Compiled $N Pcds successfully with no errors";
   }
 }

sub applyPcds($$)                                                               # Apply pcd if requested
 {my ($p, $x) = @_;                                                             # Project, parse tree
  if (my $l = pleaseChangeDita)
   {if ($compiledPcdBlocks)
     {if ($l =~ m(\A1\Z))
       {$p->pcd = transformDitaWithPcd($p->source, $x, $compiledPcdBlocks);     # Record PCD statistics from checkout in project
       }
      else
       {$p->pcd = transformDitaWithPcdOptimized                                 # Optimized PCD processing provides less tracing
         ($p->source, $x, $compiledPcdBlocks);
       }
     }
   }
 }

sub cutOutTopics($$)                                                            #P Cut out the topics in a document assuming that they are nested within the parse tree and create a bookmap from the residue if it is not already a bookmap
 {my ($project, $x) = @_;                                                       # Project == document to cut, parse tree.
  my $title = fn $project->source;                                              # Default title for residual book map
  my $topicRef;                                                                 # Topic refs tree
  my $topicPath;                                                                # Path of latest topic

  standardDitaCleanUp($project, $x);                                            # Standard dita clean up

  if (printTopicTrees)                                                          # Print tree before cutting out begins if requested
   {my $f = swapFilePrefix($project->source, in, topicTrees);
    owf($f, -p $x);
   }

  $x->by(sub                                                                    # Cut out each topic converting the residue to a bookmap if it is not already a bookmap
   {my ($o, $p)  = @_;
    if (couldBeCutOut($o))                                                      # Cut out topics
     {$topicRef  = $o->wrapWith(qq(topicref));                                  # Wrap inner concepts in topicref

      $o->downToDie(sub                                                         # Nest topic references
       {my ($r)  = @_;
        if ($r->at(qr(\A(appendix|chapter|mapref|topicref)\Z)))
         {$topicRef->putLastCut($r);                                            # Move topics and their sub topics out of section into containing topic
          die "Topmost topic reached";                                          # Finished with this enclosing topicref
         }
       });

      $o->cut;                                                                  # Cut out referenced section
      my $O = $o->renew;                                                        # Renew the parse tree to eliminate corner cases involving white space
      cleanUpCutOutTopic($project, $O);                                         # Clean up renewed topic
      applyPcds($project, $O);                                                  # Apply PCDs

      my $Title = $O->go_title;                                                 # Get the title of the piece
      my $title = $Title ? $Title->stringText : q();                            # Title content
      my $lint  = lintTopic($project, $O, $title);                              # Lint topic

      $topicRef->setAttr(navtitle=>$title, href=>fne($lint->file));             # Set navtitle and href for topicref knowing it conforms to the GB Standard
      $topicPath = $lint->file;
     }
   });

  if ($topicRef)                                                                # Create book map from topicRefs  that have content
   {$topicRef->cut;                                                             # Cut out the topic tree so we can print the notices if any

    if (!$topicRef->isAllBlankText)                                             # Topicref has content or overridden by single topic bookmaps
     {my $notices = sub                                                         # The notices are any remaining text
       {return q() if couldBeCutOut($x)  or                                     # Cut out everything
                      $x->isADitaMap($x) or                                     # We are processing a map so there is no need for notices
                      $x->isAllBlankText;                                       # Notice content would be all blank so there is no need for notices
        my $c = $x->wrapWith_conbody_wrapWith_concept;                          # Put the notices in a concept
        $c->putFirstAsTree(<<END);
<title>$title</title>
END
        my $l = lintTopic($project, $c, 'Notices for: '.$title);                # Lint the notices
        fne($l->file)                                                           # File containing notices concept
       }->();

      my $bookMap = $x->isADitaMap && !$x->isAllBlankText ? $x :                # Create a bookmap unless we have been left with a convincing bookmap
        $x->ditaSampleBookMap
         (chapters=>$topicRef, title=>$title, notices=>$notices);

      cleanUpBookMap($project, $bookMap);                                       # Clean up book map
      lintBookMap($project, $x, $bookMap, $title);                              # Lint bookmap
     }
    else                                                                        # Not creating a book map for a single item
     {$project->targets =                                                       # Source file to target file details
        createTarget($project->source, $topicPath, $x->tag, q(topic));
     }
   }
  else                                                                          # No recognized structure so we will just create a bookmap so we get errors that we can see
   {my $bookMap = $x->isADitaMap ? $x : $x->wrapWith_bookmap;                   # Either its a bookmap or its something that we might be able to improve into being a bookmap
    cleanUpBookMap($project, $bookMap);                                         # Clean up book map
    lintBookMap($project, $x, $bookMap, $title);                                # Lint bookmap
   }
 } # cutOutTopics

sub convertDocument($$)                                                         #r Convert one document.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
  $x->wrapDown_conbody_concept unless couldBeCutOut($x) or $x->isADitaMap;      # Turn it into a concept if not already a recognizable type
  $x->ditaObviousChanges;
  $x                                                                            # Return parse tree
 }

sub convertOneProjectUsingPcdsBefore($$)                                        #r Preprocess a project before applying Pcds
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
 }

sub convertOneProjectUsingPcdsAfter($$)                                         #r Post process a project after applying Pcds
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
 }

sub convertOneProjectUsingPcds($$)                                              #P Convert a project using PCDs (and nothing else)
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.

# applyPcds($project, $x);                                                      # Apply Pcds
# lintTopic($project, $x);                                                      # Save and lint results

  convertOneProjectUsingPcdsBefore($project, $x);                               # Preprocess before Pcds
  applyPcds                       ($project, $x);                               # Apply Pcds
  convertOneProjectUsingPcdsAfter ($project, $x);                               # Post process after Pcds
  my $lint = lintTopic($project, $x);                                           # Save and lint results
  $project->targets =                                                           # Source file to target file details
    createTarget($project->source, $lint->file, $x->tag, q(topic));
 }

sub convertOneProjectBefore($$)                                                 #r Run just before convert one project.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
 }

sub convertOneProjectAfter($$)                                                  #r Run just after convert one project.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
 }

sub convertOneProject($$)                                                       #r Convert one project.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.

  my $y = convertDocument($project, $x);                                        # Convert document optionally returning a new parse tree
  $x = $y if ref($y) and ref($y) eq ref($x);                                    # Continue with new parse tree if one provided

  cutOutTopics($project, $x);                                                   # Cut out topics
 }

sub formatRunTime($)                                                            #P Format seconds to two decimal places
 {my ($delta) = @_;                                                             # Time in seconds
  sprintf("%4.2f", $delta)
 }

sub convertProject($)                                                           #P Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.
 {my ($project) = @_;                                                           # Project == document to convert
  my $startTime = time;                                                         # Time the conversion
  #ddd "convertProject           $$:", $project->source;

  my $x = parseProject $project;                                                # Reload parse into this process
  return $project unless $x;                                                    # Parse failed

  convertOneProjectBefore($project, $x);                                        # Just before converting one document
  convertOneProject      ($project, $x);                                        # Convert one document
  convertOneProjectAfter ($project, $x);                                        # Just after converting one document

  my $Delta = $project->convertTime = time - $startTime;                        # Time the conversion took
  my $delta = formatRunTime($Delta);

  #ddd "Convert project finished in time: $delta seconds in process: $$:",
  #  $project->source;

  $project                                                                      # Conversion succeeded for project
 } # convertProject

sub lintResultsInitial                                                          #r Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.
 {my @actions;

  for my $a(qw(restructure oxygenProjects editOutputFiles))                     # Processing dependent on initial lint
   {push @actions, $a if eval q(&).$a;
   }

  if (@actions)                                                                 # Only needed if optional processing dependent on lint has been enabled
   {my $a = join ', ', @actions;
    lll "Lint conversion initial results for: $a";
    if (my $report = $lintReport = Data::Edit::Xml::Lint::report
     (out, qr(\.(dita|ditamap|xml)\Z)))
     {say STDERR $report->print;
     }
   }
 } # lintResultsInitial

sub lintResultsFinal                                                            #r Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.
 {if (lint)                                                                     # Only if lint requested
   {lll "Lint conversion final results";

    if (my $report = $lintReport = Data::Edit::Xml::Lint::report                # Lint report
     (out, qr(\.(dita|ditamap|xml)\Z)))
     {my $r = $report->print;
      say STDERR $r;                                                            # Write lint results
      owf(summaryFile, $r);

      if (my $fails = $report->failingFiles)                                    # Copy failing files into their own folder for easy access s
       {if (@$fails)
         {for my $file(@$fails)
           {my $f = $file->[2];                                                 # Failing file name
            my $F = fpf(fails, fne $f);                                         # Shorten file name path so we can find the file easily
            copyFile($f, $F);
           }
         }
       }

      $lintResults = join "\n\n", ($r =~ s(\n.*\Z) ()sr);                       # Lint results summary, used to create GitHub notification after upload is complete
     }
    else
     {lll "No Lint report available";;
     }
   }
  else
   {#ddd "Lint report not requested";
   }
 } # lintResultsFinal

sub copyLogFiles                                                                #P Copy log files to reports/ so they get uploaded too
 {for my $source(errorLogFile, runLogFile)
   {my $target = swapFilePrefix($source, perl, reports);
    copyBinaryFile($source, $target) if -e $source;
   }
 }

sub beforeUploadToS3                                                            #r Copy additional files into position before upload to s3
 {
 }

sub actionAfterLint($$)                                                         #P Determine whether a post lint action should happen or be suppressed
 {my ($action, $name) = @_;                                                     # Action attribute, action name

  my $n = pad($name, 32);                                                       # Align messages

  my $actionAfterLint = sub                                                     # Determine whether a post lint action should happen or be suppressed
   {return 0 unless $lintReport;                                                # Lint report required
    return 2 if $action =~ m(\A2\Z)i;                                           # Action is always required
    return 1 if $action =~ m(\A1\Z)i && $lintReport->totalErrors == 0;          # Action should proceed at 100% lint
    0                                                                           # Action should be supressed
   }->();

  if (!$lintReport)                                                             # No lint yet
   {lll "$n $action no lint results";
    return 0;                                                                   # Fail action
   }
  elsif ($actionAfterLint)                                                      # Action required
   {return 1;                                                                   # Proceed with action
   }
  elsif ($action)                                                               # Reason suppressed
   {lll "$n $action suppressed by lint results or lack thereof";
    return 0;                                                                   # Fail action
   }
  else                                                                          # Not requested
   {lll "$n not requested";
    return 0;                                                                   # Fail action
   }
 }

sub xrefMap                                                                     #P Run Xref quickly to map results
 {lll "Xref map started";
  my $x = Data::Edit::Xml::Xref::xref
   (inputFolder               => &out,
    reports                   => fpd(&reports, q(xrefMap)),
   );
  lll "Xref map finished";
  $x
 }

sub xrefFix                                                                     #P Run Xref to fix references
 {my @fixes;

  for my $f(qw(addNavTitles allowUniquePartialMatches changeBadXrefToPh
      deguidize deleteUnusedIds fixBadRefs fixDitaRefs fixRelocatedRefs
      fixXrefsByTitle))
   {push @fixes, $f if eval q(&).$f;
   }

  if (@fixes and actionAfterLint(&xref, q(Xref fix)))                           # Only run if fixes have been requested
   {lll "Xref fix started to:", @fixes;
    my $x = Data::Edit::Xml::Xref::xref                                         # Check and perhaps fix any cross references
     (addNavTitles              => xrefAddNavTitles,
      allowUniquePartialMatches => xrefAllowUniquePartialMatches,
      changeBadXrefToPh         => changeBadXrefToPh,
      deguidize                 => &deguidize,
      deleteUnusedIds           => &xrefDeleteUnusedIds,
      fixBadRefs                => &fixBadRefs,
      fixDitaRefs               => &fixDitaRefs ? targets : undef,              # Location of targets if we are going to fix dita refs in the output corpus that were valid in the input corpus
      fixRelocatedRefs          => &fixRelocatedRefs,                           # Fix references to relocated files that adhere to the GB Standard.
      fixXrefsByTitle           => &fixXrefsByTitle,                            # Fix xref by unique title
      inputFolder               => &out,
      fixedFolder               => &out,
      reports                   => fpd(&reports, q(xrefFix)),
     );
    lll "Xref fix results : ", $x->statusLine;
    return $x;
   }
  undef
 } # xrefFix

sub xrefResults                                                                 #P Run Xref to check results
 {#lll "AAAA ", dump(&xref, $lintReport);
  if (actionAfterLint(&xref, q(Xref results)))
   {$xrefResults = undef;                                                       # Free any existing xref before creating a new one
    my $x = $xrefResults = Data::Edit::Xml::Xref::xref
     (html                      => &reports,
      indexWordsFolder          => &indexWordsFolder,
      indexWords                => &indexWords,
      inputFolder               => &out,
      matchTopics               => (&xrefMatchTopics||0)/100,
      reports                   => fpd(&reports, q(xref)),
     );
    lll "Xref results     : ", $x->statusLine;
    return $x;
   }
  undef
 } # xrefResults

sub renameFilesToInputAfterLint                                                 # Rename the output so it matches the input as closely as possible
 {lll "Rename of output files back to input file names started";
  my %topicExtensions = map {s/\.//; $_ => 1} inputExt;                         # Hash of input extensions for topics
  my $C = my $M = 0;                                                            # Number of collisions. Number of files mapped
  my @refsUpdated; my @refsRetained;                                            # Report references updated as a result of renaming, references not updated

  my %GBWouldGo;                                                                # Where each GB would go if there were no collisions
  my %images; my %topics;                                                       # GB files which are images or topics

  for my $f(searchDirectoryTreesForMatchingFiles(&sources))                     # Target to Source mapping
   {my $m = evalFile($f);                                                       # Load target to source mapping
    my $g = $m->target;                                                         # GB file
    my $s = $m->source;                                                         # Source file
    my $i = $m->targetType =~ m(image)i ? 1 : 0;                                # Mapping an image
    my $t = swapFilePrefix($m->source, $i ? &downloads : &in, &renamed);        # Rename to original source file
    $GBWouldGo{$g}{$t}++;                                                       # Where each file should go
    if ($i) {$images{$g}++} else {$topics{$g}++}                                # Image or topic
   }

  my %collisions = invertHashOfHashes(\%GBWouldGo)->%*;                         # Potential Collisions

  my %GBWillGo;                                                                 # Where in fact each GB file will go

  if (1)                                                                        # Identify where each GB file should appear in the renamed folders
   {my @report;                                                                 # Report mapping of GB names to original names.
    for my $target(sort keys %collisions)                                       # Actual collisions
     {my @g = sort keys $collisions{$target}->%*;                               # Results files derived from one input file
      my $t = swapFilePrefix($target, &renamed);                                # Short target file name

      for my $g(@g)                                                             # Each GB file
       {my $G = fne $g;                                                         # Base GB name
        my $n = q(** ).scalar(@g).q( **);                                       # Collision count
        if (@g == 1)                                                            # Unique
         {$GBWillGo{$g} = $target;
          $n = q();
          ++$M;
         }
        else                                                                    # Collision
         {$GBWillGo{$g} = fpf(fp($target), fne($g));                            # Place along side target separated by GB name
          ++$C; ++$M;
         }
        push @report, [$n, $t, fne $g];                                         # Report renamed or not
       }
     }

    formatTable([@report], <<END,
Dups           The ** number ** of collisions or blank if there are no collisions
Target_file    The final name of a file
GB_File        The GB file we are trying to assign a final name to
END
      title     => q(Mapping of GB file names to final names),
      head      => <<END,
Mapped $M files with $C potential collisions in the renamed corpus on DDDD

This report shows how the files uniquely named via the GB standard are mapped
backed to the original file names where possible without creating file name
collisions.

Where possible files are renamed back to their original base name while
retaining their new path in the renamed folders.  Where such renaming is
not possible due to name collisions, the GB standard name is used as the base
name.  The GB standard names are very easy to pick out using grep:

  grep -inr -P '_[0-9a-f]{32}' .

In the case where two identical input source files with different names get
flattened down to one target output file there will be two possible names
source names the target file could use.  The actual name chosen will be at
random - it depends on which of the parallel processes processing the source
files updated the target to source mapping last.

END
      clearUpLeft => 1,
      file        => fpe(&reportsRenamed, qw(collisions txt)));
   }

  if (%images)                                                                  # Copy images into position
   {lll "Rename images and topics started";

    for my $g(sort keys %images)                                                # Each image
     {my $t = $GBWillGo{$g};                                                    # Target file name
      copyBinaryFile($g, $t);                                                   # Copy image into position
     }

    my $n = keys %images;
    lll "Rename images finished, $n images copied";
   }
  else
   {lll "No images present to be renamed";
   }

  if (%topics)                                                                  # Fix references to renamed files
   {lll "Rename topics and fix references started";

    for my $g(sort keys %topics)                                                # Each topic
     {my $target = $GBWillGo{$g};                                               # Target of the GB file
      my $source = readFile($g);                                                # Source of GB file

      my $r = sub                                                               # Fix a reference to a GB file
       {my ($reference) = @_;                                                   # Referring file, referenced file
        my $f = fpf(&out, $reference);                                          # The reference should be to a file in the output folder
        return undef unless -e $f;                                              # No such file so we leave the reference alone

        if    (my $T = $GBWillGo{$f})                                           # Target of GB file
         {my $t = relFromAbsAgainstAbs($T, $target);
          push @refsUpdated, [$t, $reference, $target, $g];
          return qq(="$t);                                                      # Updated topic reference
         }
        else                                                                    # Reference that cannot be fixed
         {push @refsRetained, [$reference, $g];
          return q(=").$reference;
         }
       };

      $source =~ s(="([^" ]*_[0-9a-f]{32}\.([a-z0-9]+))) ($r->($1))egs;         # Replace references to gb files

      $source =~ s(<!--linted:.*\Z) ()s if &removeLint;                         # Remove lint comments if requested
      writeFile($target, $source);                                              # Update topic
     }

    lll "Rename topics and fix references finished";
   }

  if (1)                                                                        # Renaming statistics
   {my @r;
    push @r, [q(Files mapped),        $M                     ],
             [q(Would go),            scalar keys %GBWouldGo ],
             [q(Will go),             scalar keys %GBWillGo  ],
             [q(Collisions),          $C                     ],
             [q(References updated),  scalar @refsUpdated    ],
             [q(References retained), scalar @refsRetained   ];

    lll "Renaming summary:\n", formatTableBasic(\@r);
    warn "$C files cannot be renamed to their original names" if $C;

    formatTable([@r], <<END,
Action Action taken
Count  Number of files the action applies to
END
    title     => q(Renaming summary),
    head      => <<END,
Renamed $M files with $C colissions on DDDD
END
    summarize => 1,
    file      => fpe(reports, qw(summary_of_renaming_actions txt)));
   }

  formatTable([@refsUpdated], <<END,
Resolved       The resolved reference
Reference      The reference resolved
Renamed_File   The renamed file the reference occurs in
GB_File        The GB file containing the reference
END
    title     => q(References resolved in renamed corpus),
    head      => <<END,
NNNN references in flattened corpus were resolved in the renamed corpus on DDDD
END
    file      => fpe(&reportsRenamed, qw(references_updated txt)));

  formatTable([@refsRetained], <<END,
Reference      The reference we wished to resolve
GB_File        The GB file containing the reference
END
    title     => q(References not resolved in renamed corpus),
    head      => <<END,
NNNN references in flattened corpus were not resolved in renamed corpus on DDDD
END
    file      => fpe(&reportsRenamed, qw(references_retained txt)));
 } # renameFilesToInputAfterLint

sub renameFilesToInput                                                          #P Rename files to match input
 {if (actionAfterLint(&renameToInput, q(Rename files to input)))
   {&renameFilesToInputAfterLint
   }
 } # renameFilesToInput

sub zipToS3InParallel                                                           #r Upload folders to S3
 {my ($source) = @_;                                                            # Source folder to be zipped, target on S3

  my $z = fpe(temporaryFile, q(zip));                                           # Zip file
  my $r = qx(cd $source; zip -qr $z .);                                         # Zip folder
  my $s = swapFilePrefix $source, home;                                         # Source folder minus home
  my @s = reverse split m(/), $s;                                               # Source folder minus home components
  my $t = fpe(s3OutputFolder, @s, q(zip));                                      # Target zip file

  s3WriteFile($t, $z, profile=>s3Profile);                                      # Copy zip file into position
 }

sub uploadFoldersToS3                                                           #r Upload folders to S3
 {my ($processStarter, $s3, @folders) = @_;                                     # Process starter, S3 folder suffix, folders to upload

  for my $dir(@folders)
   {next unless -d $dir;
    $processStarter->start(sub
     {zipToS3InParallel $dir;                                                   # Zip upload to S3 in parallel
     });
   }
 }

sub uploadToS3($)                                                               #P Copy entire home folder to S3
 {my ($processStarter) = @_;                                                    # Process starter

  beforeUploadToS3;                                                             # Copy in additional files if required

  if (s3OutputFolder)                                                           # Upload to output area if requested
   {if (actionAfterLint upload, "Upload to S3")                                 # Upload to S3 if requested
     {uploadFoldersToS3($processStarter, s3OutputFolder,                        # Folders to upload
       reports, out, perl, parseFailed, renamed, targets);
     }
   }
  else
   {eee q(Upload to S3 requested but S3OutputFolder not set);
   }
 } # uploadToS3

sub uploadToExchange                                                            #P Copy entire home folder to Exchange
 {my ($processStarter) = @_;                                                    # Process starter

  if (actionAfterLint exchange, "Upload to Exchange")
   {if (s3ExchangeFolder)
     {my @d;                                                                    # Folders to upload
      my $e = exchangeItems||'';
      push @d, downloads    if $e =~ m(d)i;
      push @d, in           if $e =~ m(i)i;
      push @d, perl         if $e =~ m(p)i;
      push @d, out          if $e =~ m(o)i;
      push @d, reports;
      push @d, topicTrees   if $e =~ m(t)i;
      push @d, parseFailed;
      push @d, targets;

      for my $dir(@d)                                                           # Upload requested folders
       {if (-d $dir)
         {my $target = swapFilePrefix($dir, home, s3ExchangeFolder);
          $processStarter->start(sub
           {syncToS3InParallel uploadSize, $dir, $target, s3Profile, s3ParmsUp; # Upload to S3 in parallel
           });
         }
       }
     }
   }
 } # uploadToExchange

sub uploadToExchange22                                                            #P Copy entire home folder to Exchange
 {my ($processStarter) = @_;                                                    # Process starter
  my $h = home;
  my $p = s3ParmsUp.s3ProfileValue;

  if (actionAfterLint exchange, "Upload to Exchange")
   {if (s3ExchangeFolder)
     {my @d;                                                                    # Folders to upload
      my $e = exchangeItems||'';
      push @d, downloads    if $e =~ m(d)i;
      push @d, in           if $e =~ m(i)i;
      push @d, perl         if $e =~ m(p)i;
      push @d, out          if $e =~ m(o)i;
      push @d, reports;
      push @d, topicTrees   if $e =~ m(t)i;
      push @d, parseFailed;
      push @d, targets;

      for my $dir(@d)                                                           # Upload requested folders
       {if (-d $dir)
         {my $target = swapFilePrefix($dir, home, s3ExchangeFolder);
          $dir       = pad($dir,    16);
          $target    = pad($target, 16);
          my $c      = qq(aws s3 sync $dir $target $p);
          lll $c;
          $processStarter->start(sub {xxx $c, qr()});
         }
        else
         {#ddd "Unable to upload directory as it does not exist:\n=$dir=";
         }
       }
     }
   }
 } # uploadToExchange

sub uploadResults                                                               #P Upload results
 {copyLogFiles;                                                                 # Copy log files into position

  if (s3OutputFolder)                                                           # Input was from S3 so results are returned there
   {lll "Upload results to S3 started";
    my $p = newProcessStarter(maximumProcesses);                                # Upload each folder in parallel
    uploadToS3($p);
    uploadToExchange($p);
    $p->finish();
    lll "Upload results to S3 finished";
   }

  elsif (!&develop and upload and sourceFromGitHub)                             # Input was from GitHub so results are placed in a zip file accessible to the web server - but only if we are on AWS
   {my $h = home;
    my $z = zipOnWwwFile;
    my $f = join ' ', map {swapFilePrefix($_, &home)} grep {-d $_}              # Available folders
            &downloads, &in, &out, &reports, &targets,
            &topicTrees;

    my $Z = fp $z;

    xxx(qq(sudo mkdir -p $Z; sudo touch $z; sudo chmod ugo=rw $z;).             # Create zip file
        qq(cd $h; zip -qr - $f > $z; sudo chmod ugo=r $z),
        qr(\A\s*\Z));
   }
 } # uploadResults

#sub saveZoomTopics                                                              #P Copy output files to the zoom folder - useful to keep the latest version in out so that they can be accessed via reports
# {my $s = &out;
#  my $t = zoomTopicsFolder;
#  makePath($t);
#  lll "Copy files to zoom folder $t";
#  clearFolder(zoomTopicsFolder, clearCount);
#  qx(cp -r "$s" "$t");
# } # saveZoomTopics


my @failedTests;                                                                # Failing tests
my @passedTests;                                                                # Passed tests
my @availableTests;                                                             # Available Tests

sub runTests                                                                    #P Run tests by comparing files in folder L<out|/out> with corresponding files in L<testResults|/testResults>.
 {if (develop and testMode == 1)                                                # Run tests if developing
   {&checkResults;
    my $F = join " ", @failedTests;
    my $f = @failedTests;
    my $p = @passedTests;
    my $a = @availableTests;

    $f + $p != $a and confess "Passing tests: $p plus failing tests: $f".
     " not equal to tests available: $a";

    if ($f)
     {confess "Failed tests $f tests out $F";
     }
    else
     {lll "Passed all tests";
     }
   }
 }

sub normalizeXml($)                                                             #P Remove document processor tags
 {my ($string) = @_;                                                            # Text to normalize
  $string =~ s(<[!?][^>]*>)  ()gs;                                              # Headers and comments
  $string =~ s( (props|id)="[^"]*") ()gs;
  $string =~ s( xtrf="[^"]*") ()gs;                                             # Remove xtrf attribute as it often contains meta data
  $string
 }

sub testResult($$$)                                                             #P Evaluate the results of a test
 {my ($test, $got, $expected) = @_;                                             # Test name, what we got, what we expected result
  my $g = normalizeXml($got);
  my $e = normalizeXml($expected);

  if ($e !~ m(\S)s)                                                             # Blank test file
   {confess "Expected results for test $test is all blank";
   }

  my %g = map {trim($_)=>1} split /\n/, $g;                                     # Remove lines that match
  my %e = map {trim($_)=>1} split /\n/, $e;

  for my $k(keys(%g), keys(%e))
   {if ($e{$k} and $g{$k})
     {delete $e{$k}; delete $g{$k};
   } }

  if (!keys(%g) and !keys(%e))                                                  # Compare got with expected
   {push @passedTests, $test;
    say STDERR "ok $test";
    return 1;
   }
  else                                                                          # Not as expected - the results are  not printed in sequential order as one might reasonably expect.
   {push @failedTests, $test;
    my $e = join "\n", sort keys %e;
    my $g = join "\n", sort keys %g;
    cluck "Got/expected in test $test:\n".
          "Got:\n$g\nExpected:\n$e\n";
    return 0;
   }
 }

sub checkResults                                                                #P Check test results
 {for my $file(searchDirectoryTreesForMatchingFiles(tests, &inputExt))
   {my $test = fn $file;
    my $expected = fpe(testExpected, $test, q(dita));
    my $got      = fpe(testResults,  $test, q(dita));
    if (-e $got)
     {if (!-e $expected)
       {lll "Created expected results for $test\n$expected\n";
        copyFile($got, $expected);                                              # Create expected test results
       }
      else
       {push @availableTests, $test;
        my $g = readFile($got);
        my $e = readFile($expected);
        if (!testResult($test, $g, $e))
         {#owf($expected, $g);                                                  # Force all test results to be up to date if necessary
         }
       }
     }
    else
     {cluck "No test output for $test";
     }
   }
 }

sub reportProjectsThatFailedToLint                                              #P Report projects that failed to parse
 {#ddd "Report projects that failed to lint";

  my @lintFailed;                                                               # Projects that failed to lint
  for my $p(sort keys %$projects)                                               # Each project
   {my $project = $projects->{$p};

    if (my $f = $project->lintFailed)                                           # Report projects that failed to lint
     {my $s = $project->source;
      my $t = qx(file "$s") =~ s(\A.*?:\s*) ()gsr;
      push @lintFailed, [$f, $s, $t];
     }
   }

  formatTables([sort {$$a[1] cmp $$b[1]} @lintFailed],
    columns => <<END,
Count   Number of lint errors in total in files derived from this file
Source  Source file
Type    File type
END
    title   => <<END,
Input files producing lint errors
END
    head    => <<END,
NNNN projects with lint errors on DDDD.

END
    file    => my $f = fpe(qw(bad lint txt)));

  dumpFile(fpe(reports, qw(lint data)), \@lintFailed);                          # Machine readable version

  if (my $n = @lintFailed)                                                      # Report parse failures summary
   {lll "$n projects failed to lint see:\n$f"
   }
 }

sub reportProjectsThatFailedToParse                                             #P Report projects that failed to parse
 {#ddd "Report projects that failed to parse";

  my @parseFailed;                                                              # Projects that failed to parse
  for my $p(sort keys %$projects)                                               # Each project
   {my $project = $projects->{$p};

    if (my $f = $project->parseFailed)                                          # Report projects that failed to parse
     {my $e = $project->parseError;
      my $s = $project->source;
      my $t = qx(file "$s") =~ s(\A.*?:\s*) ()gsr;
      push @parseFailed, [$s, $e, $t];
     }
   }
#  delete $$projects{$$_[0]} for @parseFailed;                                   # Remove projects that failed to parse

  formatTables([sort {$$a[0] cmp $$b[0]} @parseFailed],
    columns => <<END,
Source  Source file
Errors  Error listing
Type    File type
END
    title   => <<END,
Input files that failed to parse
END
    head    => <<END,
NNNN projects failed to parse on DDDD.

END
    file    => my $f = fpe(qw(bad failedToParse txt)));

  dumpFile(fpe(reports, qw(failedToParse data)), \@parseFailed);                # Machine readable version

  if (my $n = @parseFailed)                                                     # Report parse failures summary
   {lll "$n projects failed to parse, see:\n$f"
   }
 }

sub reportProjectConversionTimes                                                #P Report project conversion times
 {#ddd "Report project conversion times";

  my @T;                                                                        # Times
  for my $p(sort keys %$projects)                                               # Each project
   {my $project = $projects->{$p};
    if (my $d = $project->convertTime)                                          # Conversion time
     {push @T, [$d, $project->source];
     }
   }

  my @t = sort {$$b[0] <=> $$a[0]} @T;                                          # Sort with longest run times first

  formatTables(\@t,
    columns => <<END,                                                           # Report convert times as a table
Time    Time in seconds
Source  Source file
END
    title   => <<END,
Conversion times
END
    head    => <<END,
Converted NNNN projects failed to parse on DDDD.

END
    file    => fpe(qw(timing convertTimes txt)));                               # Report file
 }
=pod
   {failed     => { "test.pcd" => { 1 => 2, 4 => 5, 7 => 8 } },
    failedFile => {
                  "test.pcd" => { 1 => { "" => 2 }, 4 => { "" => 5 }, 7 => { "" => 8 } },
                  },
    passed     => { "test.pcd" => { 1 => 1, 4 => 1, 7 => 1 } },
    passedFile => {
                  "test.pcd" => { 1 => { "" => 1 }, 4 => { "" => 1 }, 7 => { "" => 1 } },
                  },
   };
=cut
sub reportPcdStatistics                                                         #P Report statistics from Please Change Dita execution
 {return unless $compiledPcdBlocks;
  lll "Report pcd statistics";

  my %D; my %H;                                                                 # PCD {file}{line} = description.  Hit rate for each pcd block
  for my $p(@$compiledPcdBlocks)                                                # Each block
   {my ($f, $l) = ($$p[0][2], $$p[0][1]);                                       # File, line of PCD block
    $D {$f}{$l} =  $$p[0][0];                                                   # Description of block in file at line
    $H {$f}{$l} =      0;                                                       # Hits on block
   }

  my %P;  my %F;                                                                # Passes, fails
  my %PF; my %FF;                                                               # Passes, fails by file
  for my $p(sort keys %$projects)                                               # Each project
   {my $project = $projects->{$p};
    if (my $s = $project->pcd)                                                  # Pcd statistics available
     {if (my $f = $s->{failed})                                                 # Fails - find the greatest advance made
       {for     my $df(sort keys %$f)
         {for   my $dl(sort keys %{$$f{$df}})
           {my $best = $F {$df}{$dl};
            my $curr = $$f{$df}{$dl};
            if (!$best or $best < $curr)
             {$F{$df}{$dl} = $curr;
             }
           }
         }
       }
      if (my $p = $s->{passed})                                                 # Passes - sum so we know which pcds are being used
       {for     my $df(sort keys %$p)
         {for   my $dl(sort keys %{$$p{$df}})
           {       $P{$df}{$dl} += $$p{$df}{$dl};
            delete $F{$df}{$dl};                                                # Succeeded
           }
         }
       }
      if (my $p = $s->{passedFile})                                             # Passes by file
       {for     my $df(sort keys %$p)
         {for   my $dl(sort keys %{$$p{$df}})
           {for my $if(sort keys %{$$p{$df}{$dl}})
             {       $PF{$df}{$dl}{$if} += $$p{$df}{$dl}{$if};
              delete $FF{$df}{$dl}{$if};                                        # Succeeded
             }
           }
         }
       }
     }
   }

  my @P;                                                                        # Passes
  for   my $f(sort keys %P)                                                     # Each pcd description block
   {for my $l(sort keys %{$P{$f}})                                              # Each pcd description line
     {my $c = $P{$f}{$l};
      push @P, [$c, $D{$f}{$l}, $l, $c, $f];
     }
   }

  my @PF;                                                                       # Passes by file - the only statistics reported by the optimized PCD transformer
  for     my $f(sort keys %PF)                                                  # Each pcd description block
   {for   my $l(sort keys %{$PF{$f}})                                           # Each pcd description line
     {for my $i(sort keys %{$PF{$f}{$l}})                                       # Each file to which the pcd was applied
       {my $c = $PF{$f}{$l}{$i};
        push @PF, [$c, $D{$f}{$l}, $i, $l, $f];
        $H{$f}{$l}++;
       }
     }
   }

  my @H;                                                                        # Where the PCD hits occurred and where they did not
  for     my $f(sort keys %H)                                                   # Each pcd file
   {for   my $l(sort keys %{$H{$f}})                                            # Each pcd description line
     {my $c = $H{$f}{$l};
      push @H, [$c, ($c?q():q(***)),$l, $D{$f}{$l}, $f];                        # Count PCD hits for each description in each pcd file
     }
   }

  my @F;                                                                        # Fails
  for   my $f(sort keys %F)                                                     # Each pcd description block
   {for my $i(sort keys %{$F{$f}})                                              # Each pcd description line
     {my $c = $F{$f}{$i};
      push @F, [$c, $D{$f}{$i}, $i, $f];
     }
   }

  formatTables(\@P,                                                             # Passes
    columns => <<END,
Count       Number of times this PCD block succeeded == passed
Description The description of the PCD block
Line_Number The line number of the PCD block in the PCD File (next)
PCD_File    The file containing the PCD block
END
    title   => <<END,
PCD Blocks that have succeeded
END
    head    => <<END,
NNNN PCD Blocks succeeded on DDDD.
END
    file    => fpe(qw(pcd passes txt)));

  formatTables(\@PF,                                                            # Passes by file
    columns => <<END,
Count       Number of times this PCD block succeeded == passed
Description The description of the PCD block
Input_File  The input file to which the PCD was applied
Line_Number The line number of the PCD block in the PCD File (next)
PCD_File    The file containing the PCD block
END
    title   => <<END,
PCD Blocks that have succeeded by input file processed
END
    head    => <<END,
NNNN PCD Blocks succeeded on DDDD.
END
    file    => fpe(qw(pcd passes_by_file txt)));

  formatTables([sort{$$a[3] cmp $$b[3]} @H],                                    # Hits by Pcd block
    columns => <<END,
Count       Number of files this PCD block succeeded in
Zero        *** if the PCD had no hits
Line_Number The line number of the PCD block in the PCD File (next)
Description The description of the PCD block
PCD_File    The file containing the PCD block
END
    title   => <<END,
The number of files in which each PCD block suceeded
END
    head    => <<END,
PCD blocks succeeded in NNNN files on DDDD
END
    file    => fpe(qw(pcdHits txt)));

  formatTables(\@F,                                                             # Fails
    columns => <<END,
Advance     The greatest line number at which this PCD failed
Description The description of the PCD block
Line_Number The line number of the PCD block in the PCD File (next)
PCD_File    The file containing the PCD block
END
    title   => <<END,
PCDs which started but did not succeed
END
    head    => <<END,
NNNN PCD Blocks almost succeeded on DDDD.
END
    file    => fpe(qw(pcd fails txt)));
 }

sub reportSourceMapToTargets                                                    #P Report where the source files went - done in Xref at: lists source_to_targets
 {#ddd "Report source files mapped to targets";
  my @files = searchDirectoryTreesForMatchingFiles(targets);                    # Files in targets folder
  my $files = @files;
  my @r;

  for my $file(@files)                                                          # Each target file
   {my $m = evalFile($file);                                                    # Source to target details

    push @r, [$m->sourceDocType, $m->source, $m->targetType, $m->target];

    if ($m->targetType =~ m(map\Z))                                             # The target might be a map because a map was input or because one was generated by cutting up a topic.  The generated map will have the correct file names in it. The copied map will have the original file names in it - Xref will fix them by looking for their true targets elsewhere in the targets folder.
     {my $x = parseFile($m->target);
      $x->by(sub
       {my ($o) = @_;
        if ($o->at(qr(\A(chapter|topicref)\Z)))
         {my $targetFile =
           push @r, [q(), q(),  q(), $o->href, $o->attrX_navtitle];
         }
       });
     }
   }

  my $r = @r;

  formatTables(\@r,
    columns   => <<END,
DocType DocType tag of source document
Source  Source input file
Type    Whether the target file represents a bookmap, image or topicTarget
Target  Target file
Title   The title of the target file
END
    title     => q(Source to Target mapping),
    head      => <<END,
$files source files mapped to $r target files on DDDD.
END
    summarize => 1,
    file      => fpe(qw(lists sourceToTargetMapping txt)));
 }

sub convertSelectedProjects                                                     #P Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.
 {my @p = sort keys %$projects;                                                 # Projects
  my $p = @p;

  lll "Convert selected xml projects of numerosity $p";

  my %projectsBySource = map {$_->source => $_} values %$projects;              # Source files to projects

  if (@p)                                                                       # Convert each project in paralle
   {processFilesInParallel
      sub                                                                       # Process element
       {my ($file) = @_;
        &convertProject($projectsBySource{$file})
       },
      sub                                                                       # Consolidate results
       {my @results = @_;                                                       # Results
        reloadHashes(\@results);                                                # Recreate attribute methods
        my %convert = %$projects;                                               # Projects to convert
        for my $project(@results)                                               # Each result
         {if (my $projectName = $project->{name})                               # Converted project name
           {if (my $p = $$projects{$projectName})                               # Find project
             {$$projects{$projectName} = $project;                              # Consolidate information gathered
              delete $convert{$projectName};                                    # Mark project as converted
             }
            else                                                                # Confess to invalid project
             {lll "Unknown converted project", $projectName, $project->source;
             }
           }
          else                                                                  # Confess to invalid project
           {lll "No name for project ", $project->source;
           }
         }

        if (my @f = sort keys %convert)                                         # Confess to projects that failed to convert
         {formatTables(
           [map {[$convert{$_}->source]} @f],
            columns => <<END,
Source Source file that failed to convert
END
            title   => qq(Projects that field to convert),
            head    => qq(NNNN of $p source files failed to convert on DDDD),
            msg     => 1,
            file    => fpe(qw(bad sourceFileConversions txt)));
         }

####TEST####        reportProjectsThatFailedToLint;                                         # Report projects that failed to lint - needs to be in parallel
        reportProjectsThatFailedToParse;                                        # Report projects that failed to parse
        reportSourceMapToTargets;                                               # Report where each source file went
        reportProjectConversionTimes;                                           # Report project conversion times
        reportPcdStatistics;                                                    # Report statistics from Please Change Dita execution
        lll "Convert selected xml projects of numerosity $p completed";
       }, sort keys %projectsBySource;
   }
  else
   {eee "No xml projects selected for conversion";                              # No projects selected
   }
 } # convertSelectedProjects

sub beforeLoadProjects                                                          #r Run just before we choose which files to convert
 {
 }

sub beforeConvertProjects($)                                                    #r Run just before project conversion starts
 {my ($projects) = @_;                                                          # Projects in the conversion
 }

sub afterConvertProjects($)                                                     #r Runs after all projects have been converted
 {my ($projects) = @_;                                                          # Projects in the conversion
 }

sub fixDitaXrefHrefs                                                            #P Fix single word xref href attributes so that they are in dita format - these tend to originate in non dita xml.
 {if (ditaXrefs)                                                                # Fix if requested
   {lll "Convert xref hrefs to Dita format";
    Data::Edit::Xml::Lint::fixDitaXrefHrefs(maximumProcesses, out, inputExt);
   }
  else
   {#ddd "Convert xref hrefs to Dita format not requested";
   }
 }

sub reportProgramAttributeSettings                                              #P Report the attribute settings
 {my $f = fpe(&reports, qw(parameterSettings txt));                             # Report settings file
  reportAttributeSettings($f);                                                  # Report settings
 }

sub reportDownloadExtensions                                                    #P Report extensions of downloaded files
 {my $e = countFileExtensions(&downloads);                                      # Count file extensions
  my @e = map {[$$e{$_}, $_]} sort keys %$e;                                    # Sort by extension

  formatTables([@e],                                                            # Report extensions
    columns => <<END,
Count Number of files with the following extension
Ext   File extension
END
    title   => q(Extensions present in downloads folder),
    head    => qq(Found NNNN extensions on DDDD),
    file    => fpe(qw(lists downloadExtensions txt)),
   );
 }

sub createClassificationMaps                                                    # Create classification maps if requested
 {if (&classificationMaps)
   {lll "Create classification maps";
    Data::Edit::Xml::Xref::xref
     (inputFolder        => &in,
      reports            => fpd(&reports, q(in)),
      classificationMaps => &classificationMaps,
      subjectSchemeMap   => fpe(&reports, qw(maps subjectScheme ditamap)),
     );
   }
  else
   {lll "Create classification maps not requested";
   }
 }

sub convertProjects                                                             #P Convert the selected documents.
 {if (convert)                                                                  # Convert the documents if requested.
   {for my $f(out, parseFailed, process, renamed, reports,                      # Clear output folders
              sources, targets, topicTrees)
     {clearFolder($f, clearCount)
     }

    lll "Report attributes"                 if showPhaseTimes;
    reportProgramAttributeSettings;                                             # Report attribute settings
    lll "Report download extensions"        if showPhaseTimes;
    reportDownloadExtensions;                                                   # Report extensions of downloaded files
    lll "Before load projects"              if showPhaseTimes;
    beforeLoadProjects;                                                         # Before load projects
    lll "Before create classification maps" if showPhaseTimes;                  # Create classification maps
    createClassificationMaps;
    lll "Load projects"                     if showPhaseTimes;
    loadProjects;                                                               # Projects to run
    lll "Before convert projects"           if showPhaseTimes;
    beforeConvertProjects($projects);
    lll "Convert selected projects"         if showPhaseTimes;
    convertSelectedProjects;                                                    # Convert selected projects
    lll "After convert projects"            if showPhaseTimes;
    afterConvertProjects($projects);
    fixDitaXrefHrefs;                                                           # Fix Dita xref href attributes
    Flip::Flop::convert();                                                      # Reset conversion flip flop
    confess "Exiting because we are in stand alone mode" if $standAlone;        # Stop testing at this point to look at results
   }
  else
   {#ddd "Convert documents not requested";
   }
 } # convertProjects

sub restructureOneDocument($$$$)                                                #r Restructure one document
 {my ($phase, $lint, $xref, $x) = @_;                                           # Phase, lint results, xref results, parse tree
 } # restructureOneDocument

sub restructureCleanUp($$@)                                                     #r Cleanup after each restructuring phase
 {my ($phase, $xref, @cleanUps) = @_;                                           # Phase, Xref details, cleanup requests
 } # restructureCleanUp

sub restructureOneFile($$)                                                      #P Restructure one output file
 {my ($phase, $file) = @_;                                                      # Phase, file to restructure
  my $lint   = Data::Edit::Xml::Lint::read($file);
  my $x      = Data::Edit::Xml::new($lint->source);
  my $source = $x->ditaPrettyPrintWithHeaders;

  my $result = &restructureOneDocument($phase, $lint, $xrefResults, $x);        # Restructure

  my $Source = $x->ditaPrettyPrintWithHeaders;

  if ($Source ne $source)                                                       # Write out modified source
   {$lint->catalog = &catalog;
    $lint->source  = $Source;
    $lint->lint;                                                                # NB: we reuse the existing file name to avoid disrupting the links created by Xref even though this potentially invalidates the GB standard.
    if ($lint->errors)                                                          # Check for lint errors
     {lll "Lint errors in restructured file:", $lint->file;
     }
   }

  $result
 } # restructureOneFile

sub restructureResultsFiles                                                     #P Restructure output folders based on results from Lint and Xref. Another run with Xref will almost certainly be required to fix references to restructured files.
 {my %projects;                                                                 # Details of each file processed

  xrefResults;                                                                  # Get Xref results for restructuring

  my $phases = restructurePhases;                                               # Number of restructuring phases
  for my $phase(1..restructurePhases)                                           # Performed specified number of restructuring phases
   {my @files = searchDirectoryTreesForMatchingFiles                            # Reload files on each phase as there might be changes to the file structure given the name of this sub
     (&out, &outExtMap, &outExtTopic);

    my $files = @files;
    lll "Restructure output files of numerosity $files, phase $phase/$phases";

    processFilesInParallel                                                      # Process files in parallel
      sub                                                                       # Restructure one file
       {my ($file) = @_;
        &restructureOneFile($phase, $file)
       },
      sub                                                                       # Clean up results
       {my @results = @_;
        my $xrefMap = xrefMap;                                                  # Use Xref to map results so far
        restructureCleanUp($phase, $xrefMap, @results);                         # Cleanup after the restructuring
       }, @files;

    my $report = Data::Edit::Xml::Lint::report                                  # Lint report
     (out, qr(\.(dita|ditamap|xml)\Z));

    if (my $n = $report->totalErrors)                                           # Check for lint errors after restructuring
     {lll "Lint errors after restructuring: $n errors";
      owf(fpe(reports, qw(lint_after_restructuring txt)), $report->print);
     }
    else                                                                        # No lint errors after restructuring
     {lll "No lint errors after restructuring phase: $phase.";
     }
   }
 } # restructureResultsFiles

sub restructureBefore                                                           #r Processing before restructuring of each individual file starts
 {
 } # restructureBefore

sub restructureAfter                                                            #r Processing after restructuring of each individual file has finished
 {
 } # restructureBefore

sub restructureResults                                                          #P Restructure output folders based on results from Lint and Xre
 {if (actionAfterLint restructure, "Restructure documents")                     # Restructure if requested
   {restructureBefore;
    restructureResultsFiles;
    restructureAfter;
   }
 } # restructureResults

sub oxygenProjectFileMetaData                                                   #r Meta data for the oxygen project files
 {q(<meta/>)
 } # oxygenProjectFileMetaData

our %targetFolderStructure;                                                      # Target folder structure used to create oxygen project files

sub createOxygenProjectFile($$)                                                 # Create an Oxygen project file for the specified bookmap
 {my ($xref, $bm) = @_;                                                         # Xref, Bookmap

  my @mapRefs = ($bm);                                                          # Include this map in the project file

  my $extractHrefs = sub                                                        # Extract some hrefs from the xref
   {my ($field) = @_;                                                           # Field to extract
    my %hash;

    for     my $bm(@mapRefs)                                                    # Initial plus referenced bookamps
     {for   my $file(sort keys %{$xref->topicsReferencedFromBookMaps->{$bm}})   # Topics from referenced bookmaps
       {for my $href(sort keys %{$xref->{$field}->{$file}})                     # Href
         {if (my ($t) = parseDitaRef($href, $file))                             # Fully qualified target
           {$hash{$t}++;                                                        # Referenced file
           }
         }
       }
     }
    %hash
   };

  my $extractHrefsAndWrapWithField = sub                                        # Extract some hrefs from the xref and wrap them with file name relative to the specified absolute file
   {my ($field, $xpr) = @_;                                                     # Field to extract, oxygen project file
    my %hash = &$extractHrefs($field, $xpr);

    my @f;                                                                      # File names
    for my $file(sort keys %hash)
     {my $r = relFromAbsAgainstAbs $file, $xpr;
      push @f, qq(<file name="$r"/>);
     }

    join "\n", @f;
   };

  my $extractTopicsNotConrefs = sub                                             # Extract non conreffed topicrefs
   {my ($xpr) = @_;                                                             # Project file being built
    my %conRefs = &$extractHrefs(q(conRefs), $xpr);

    my @f; my %s;                                                               # File names, files already seen
    for   my $bm(@mapRefs)                                                      # Each bookmap in this xpr
     {for my $file(sort keys $xref->topicsReferencedFromBookMaps->{$bm}->%*)    # Field to extract
       {if (my $docType = $xref->docType->{$file})                              # Include files only once even if they are referenced multiple times
         {if ($docType !~ m(map\Z)s and !$s{$file}++)                           # Include files only once even if they are referenced multiple times
           {if (!$conRefs{$file})
             {my $t = detagString($xref->title->{$file} // $file);
              my $r = relFromAbsAgainstAbs $file, $xpr;
              push @f, <<END
<folder name="$t">
  <file name="$r"/>
</folder>
END
             }
           }
         }
       }
     }

    join "\n", sort @f;
   };

  my $formatTargetsFolder = sub                                                 # Recreate the targets folder structure so that they can get from their old folder structure to the new flattened files
   {my ($xpr) = @_;                                                             # Oxygen project file

    my @r;
    my $r; $r = sub                                                             # Target files for each file in the bookmaps in this project file
     {my ($files) = @_;
      for my $f(sort keys %$files)
       {if (ref($$files{$f}))
         {push @r, qq(<folder name="$f">);
          &$r($$files{$f});
          push @r, qq(</folder>);
         }
        else
         {my $F = $$files{$f};                                                  # Find first containing bookmap
          for my $bm(@mapRefs)
           {if ($xref->topicsReferencedFromBookMaps->{$bm}{$F} or
                $xref->imagesReferencedFromBookMaps->{$bm}{$F})
             {my $t = detagString($xref->title->{$F} // $f);
              my $r = relFromAbsAgainstAbs $F, $xpr;
              push @r, <<END;
<folder name="$f" navTitle="$t">
  <file name="$r"/>
</folder>
END
              last;
             }
           }
         }
       }
     };

    &$r(\%targetFolderStructure);

    join "\n", @r;
   };

  my $mapRefsFrom; $mapRefsFrom = sub                                           # Parse referenced bookmaps recursively
   {my ($bookMap, $href) = @_;                                                  # Bookmap, href in bookmap which refers to this bookmap

    my $bm = absFromAbsPlusRel($bookMap, $href);                                # Referenced bookmap
    push @mapRefs, $bm;                                                         # Save referenced bookmap

    if (my $x = eval {Data::Edit::Xml::new($bm)})                               # Parse bookmap
     {$x->by(sub                                                                # Traverse referenced bookmap
       {my ($o, $p, $q) = @_;
        if ($o->at_mapref)                                                      # Map reference in referenced bookmap
         {if (my $h = $o->href)
           {&$mapRefsFrom($bm, $h);
           }
         }
        elsif ($o->at_topicref)                                                 # Map referenced via topic ref in referenced bookmap
         {if ($o->attrX_format =~ m(\Aditamap\Z)i)
           {if (my $h = $o->href)
             {&$mapRefsFrom($bm, $h);
             }
           }
         }
       });
     }
   };

  my $x = Data::Edit::Xml::new($bm);                                            # Parse initial bookmap

  my $title;                                                                    # Title from initial bookmap

  $x->by(sub                                                                    # Traverse initial bookmap
   {my ($o, $p, $q) = @_;
    if    ($o->at_title)
     {$title = $o->stringText;
     }
    elsif ($o->at_mapref)                                                       # Map ref in initial bookmap
     {if (my $h = $o->href)
       {&$mapRefsFrom($bm, $h, 1);
       }
     }
    elsif ($o->at_topicref)                                                     # Map referenced via a topic reference in the initial bookmap
     {if ($o->attrX_format =~ m(\Aditamap\Z)i)
       {if (my $h = $o->href)
         {&$mapRefsFrom($bm, $h, 1);
         }
       }
     }
   });

  $title //= q();                                                               # Default title

  my $base = fn $bm;                                                            # Remove MD5 sum to create an acceptable name but arbitrary name for the project tree
     $base =~ s(_[a-f0-9]{32}\Z) ()i;

  my $baseExt = fne $bm;
  my $name = fpe($base, q(xpr));

  my $mapRefs = join "\n",
    map
     {q(<file name=").relFromAbsAgainstAbs($_, $bm).q("/>)
     } @mapRefs;

  my $out = setFileExtension($bm, q(xpr));                                      # Oxygen Project File name

  my $resources = &$extractHrefsAndWrapWithField(q(conRefs), $out);
  my $images    = &$extractHrefsAndWrapWithField(q(images),  $out);
  my $topics    = &$extractTopicsNotConrefs                 ($out);
  my $targets   = &$formatTargetsFolder                     ($out);

  my $metaData  = &develop ? q() :  &oxygenProjectFileMetaData;                 # Add meta data if we are doing this for real otherwise ignore it as it is bulky and gets in the way

  my $X = eval {$x->new(<<END)};
<?xml version="1.0" encoding="UTF-8"?>
<project version="21.1">
    $metaData
    <projectTree name="$name">
        <folder masterFiles="true" name="Master Files">
           $mapRefs
        </folder>
        <folder name="$title">
          <file name="$baseExt"/>
        </folder>
        <folder name="Images">
          $images
        </folder>
        <folder name="Resources">
          $resources
        </folder>
        <folder name="Topics">
          $topics
        </folder>
        <folder name="CCX Content Repo (All)">
          <folder path="../out"/>
        </folder>
        <folder name="New files from old files">
          $targets
        </folder>
    </projectTree>
</project>
END

  $X->go_projectTree->last->by(sub                                              # Cut out empty folders under the last folder otherwise we get lots of empty folders that contain no files because the files in them were not relevant to this bookmap
   {my ($f) = @_;
    $f->cutIfEmpty_folder;
   });

  if ($@)
   {cluck "Unable to parse generated oxygen project file:\n$@"
   }
  else
   {my $text = Data::Edit::Xml::xmlHeader -p $X;

    owf($out, $text);
   }
 } # createOxygenProjectFile

sub createOxygenProjectMapFiles                                                 # Create Oxygen project files from Xref results
 {lll "create Oxygen Project files";

  for my $f(searchDirectoryTreesForMatchingFiles &targets)                      # Map target folder structure
   {my $file = swapFilePrefix($f, &targets);
    my @f = split m(/), $file;
    my $s = join '', map {q({).dump($_).q(})} @f;
    my $t = evalFile($f);
    my $c = "\$targetFolderStructure$s = " . dump($t->target);                  # Load targets
    eval $c;
    confess $@ if $@;
   }

  my $xref = Data::Edit::Xml::Xref::xref                                        # Xref again to pick up the improvements made by the previous Xref. This approach has the substantial benefit of allowing us to run this phase in isolation.
   (inputFolder => &out,
    reports     => fpd(reports, q(oxygenProjectFiles)),
    );

  if (develop)                                                                  # Run in series for testing otherwise things get very confusing
   {for my $bm(sort keys %{$xref->exteriorMaps})                                # Oxygen project files only required for exterior book maps. Classification maps are automatically excluded because they are referred to by the map they are classifying.
     {createOxygenProjectFile($xref, $bm);
     }
   }
  else                                                                          # Run in parallel on AWS otherwise it takes forever
   {processFilesInParallel
    sub
     {my ($bm) = @_;
      createOxygenProjectFile($xref, $bm)
     },
    sub
     {
     }, sort keys %{$xref->exteriorMaps};
   }
 } # createOxygenProjectMapFiles

sub createOxygenProjectFiles                                                    # Create Oxygen project files from Xref results
 {if (actionAfterLint oxygenProjects, "Create Oxygen project file")             # Create oxygen project file if we are 100% lint
   {createOxygenProjectMapFiles
   }
 } # createOxygenProjectFiles

sub editOneOutputFile($)                                                        #r Edit one output file
 {my ($file) = @_;                                                              # File to clean up
  $file                                                                         # Return data to be passed to L<editOutputFilesCleanUp>
 } # editOneOutputFile

sub editOutputFilesCleanUp(@)                                                   #r Clean up edited output files
 {my (@files) = @_;                                                             # Results from each invocationof L<editOneOutputFile>
 } # editOutputFilesCleanUp

sub editAllOutputFiles                                                          # Edit output files
 {if (actionAfterLint editOutputFiles, "Edit output files with Xref info")      # Possibly dependent on Lint results
   {my @files = searchDirectoryTreesForMatchingFiles
     (&out, &outExtMap, &outExtTopic);

    processFilesInParallel                                                      # Edit each file in parallel
      sub
       {my ($file) = @_;
        my $S = readFile($file);
        my $s = editOneOutputFile($S);
        if ($s ne $S)
         {owf($file, $s);
         }
       },
      sub
       {&editOutputFilesCleanUp(@_);
       }, @files;
   }
 } # editAllOutputFiles

sub notifyUsers                                                                 #r Notify users of results
 {my $pass = $lintReport ? $lintReport->passRatePercent : 0;
  my $ffs  = $lintReport ? @{$lintReport->failingFiles} : 0;
  my $ff   = $ffs ? qq( and $ffs failing files) : q();

  my $title = conversion." ".version." completed with $pass % success$ff";      # Title
  my $body  = boldStringUndo join "",
    ($lintResults//q()), "\n\n",                                                # Body
    q(http://www.ryffine.com);

  if (actionAfterLint notify, "Notify users")                                   # Create issue if requested
   {eval {GitHub::Crud::createIssueFromSavedToken                               # Create issue
     ("philiprbrenan", "notifications", $title, $body)};
   }

  if (!develop)                                                                 # Occasionally takes a long time then fails - why?
   {eval {GitHub::Crud::writeFileUsingSavedToken                                # Update latest conversion status on github
     ("philiprbrenan", "notifications", q(README.md), <<END)};
# $title

$body
END
   }
 }

our $costToCaller = 0;                                                          # Cost of run

sub costToCaller                                                                #r Cost of run
 {return $costToCaller if &develop or $costToCaller;                            # Return cached value

  my $bytes     = folderSize(&in) // 0;                                         # Number of bytes of input
  $costToCaller = ($bytes - ($bytes % 100)) / 100;                              # Cost in dollars at 1 dollar per megabyte

  my $client    = nameFromString(&client||q(Ryffine));                          # Client requesting conversion
  my $time      = CORE::time;                                                   # Time of run to act as key
  my $version   = nameFromString(&version);                                     # Version
  my $dt        = dateTimeStamp;                                                # Date of run

  my $file = dumpFile(fpe(reports, qw(costToCaller data)),                      # Write cost report
   {Cost     => $costToCaller,
    Currency => "US dollars rounded down to nearest dollar at one dollar per megabyte of source",
    Date     => $dt,
    Client   => $client,
    Version  => $version,
   });

  my $cmd = qq(aws s3 cp "$file" ).                                             # Save cost report in separate bucket by time
            qq("s3://exchange.ryffine/billable/$client/$time" ).
            qq( --profile fmc --quiet);
  #lll qq($cmd);                                                                # Show command
  unless(fork)
   {xxx $cmd, qr();                                                             # Execute command in parallel ignoring results
    exit;
   }
 }

sub replaceableMethods                                                          #P Replaceable methods
 {qw(
afterConvertProjects
beforeConvertProjects
beforeLoadProjects
beforeUploadToS3
chooseIDGroup
chooseTopicFileName
chooseTopicFileNameUsingTopicId
chooseTopicFileNameFromSourceFile
cleanUpBookMap
cleanUpCutOutTopic
convertDocument
convertOneProjectAfter convertOneProjectBefore convertOneProject
convertOneProjectUsingPcdsBefore convertOneProjectUsingPcdsAfter
convertXmlToDita
costToCaller
createOxygenProjectFiles
createTopicId
createTopicIdIfNeeded
createTopicIdIfNeededFromBaseFileName
downloadAfter downloadBefore downloadFiles
editOneOutputFile
editOutputFilesCleanUp
formatXml
lintResultsInitial
lintResultsFinal
notifyUsers
oxygenProjectFileMetaData
parseProject
restructureAfter
restructureBefore
restructureCleanUp
restructureOneDocument
s3InputFolder
s3OutputFolder
saveCode
selectFileForProcessing
spelling
spellingOut
standardDitaCleanUp
uploadFoldersToS3
)
 }

if (0)                                                                          # Format replaceable methods
 {lll "Replaceable methods in $0\n", join "\n",
   (sort keys %{reportReplacableMethods($0)}),
   '';
  exit;
 }

sub attributeMethods                                                            #P Attribute methods
 {qw(
catalog
changeBadXrefToPh
classificationMaps
clearCount
client
conversion
conversionStep
convert
debug
deguidize
develop
ditaBin
ditaXrefs
docSet
download
downloads
downloadSize
editOutputFiles
endTime
errorLogFile
exchange
exchangeHome
exchangeItems
extendedNames
fails
fixBadRefs
fixDitaRefs
fixFailingFiles
fixRelocatedRefs
fixXrefsByTitle
hits
hitsFolder
home
imageCache
in
inputExt
indexWords
indexWordsFolder
line
lint
makeXml
maximumProcesses
notify
numberOfFiles
oxygenProjects
out
outExtMap
outExtTopic
parseFailed
pcdFile
perl
pidFile
pleaseChangeDita
printTopicTrees
process
removeLint
renameToInput
reports
restructure
restructurePhases
runLogFile
runTime
s3Exchange
s3Parms
s3ParmsDown
s3ParmsUp
s3Profile
showPhaseTimes
sourceFrom
sourceFromDefault
startTime
summaryFile
targets
testExchangeIn
testExchangeOut
testExpected
testFails
testFails2
testMode
testResults
testStandAlone
tests
titleOnly
topicTrees
upload
user
useZippedDownLoads
version
www
wwwFolder
xref
xrefAddNavTitles
xrefAllowUniquePartialMatches
xrefDeleteUnusedIds
xrefMatchTopics
zipOnWwwFile
)
 }
#gbbfStandard

if (0)                                                                          # Format replaceable attributes
 {lll "Replaceable attributes in $0\n", join "\n",
   (sort keys %{reportAttributes($0)}), '';
  exit;
 }

my $overrideMethods;                                                            # Merge packages only once

sub overrideableMethods                                                         #P Methods that can be overridden
 {(replaceableMethods, attributeMethods)
 }

sub overrideMethods(;$)                                                         #P Merge packages
 {my ($package) = @_;                                                           # Name of package to be merged defaulting to that of the caller.
  my ($p) = caller();                                                           # Default package if none supplied
  $package //= $p;                                                              # Supply default package if none supplied
  return if $overrideMethods++;                                                 # Merge packages only once
  Data::Table::Text::overrideMethods $package, __PACKAGE__, overrideableMethods # Override methods in the standard way
 }

sub saveCode                                                                    #r Save code if developing
 {my $cpan   = q(/home/phil/perl/cpan/);
  my $parms  = q( --only-show-errors --profile fmc --region eu-west-1);
  my $folder = q(ryffine/code/perl/);

  if (develop)
   {saveCodeToS3(1200, &perl, client,  $folder, $parms);
    saveCodeToS3(7200, $cpan, q(cpan), $folder, $parms);
   }
 }

sub checkParameters                                                             #P Check parameters for obvious failures
 {my $h = home;
  $h =~ m(\A/.*/\Z)s or confess "home must start and end with / but got: $h";
  $h =~ m(//)s      and confess "home contains // see: $h";

  catalog or cluck "catalog attribute\n";
  if (!develop)
   {download and !sourceFrom and confess "sourceFrom attribute required";
    client or cluck "client attribute\n";
   }
 }

sub convertXmlToDita                                                            #r Perform all the conversion projects. This routine is not really replaceable but using the replace mechanism is a convenient way to export it.
 {my ($package) = caller;

  binModeAllUtf8;                                                               # So we can print bold characters from unicode

  lll conversion;                                                               # Title of run

  unlink errorLogFile;                                                          # Clear log
  replacePid;                                                                   # Make this conversion the current conversion

  my @times;                                                                    # Time taken by each phase

  for my $phase(q(saveCode),                                                    # Execute conversion phases
                q(reportProgramAttributeSettings),
                q(checkParameters),
                q(setAtHits),
                q(compilePcds),
                q(downloadFiles),
                q(makeXmlFiles),
                q(convertProjects),                                             # Maximum reach on first run
                q(lintResultsInitial),
                q(restructureResults),
                q(createOxygenProjectFiles),
                q(editAllOutputFiles),
                q(lintResultsFinal),
                q(xrefFix),
                q(xrefResults),
                q(renameFilesToInput),
                q(runTests),
                q(costToCaller),
                q(uploadResults),
#               q(saveZoomTopics),
                q(notifyUsers))
   {lll " " x 40, pad("Start phase: $phase", 32) if showPhaseTimes;
    my $startTime = time;

    if (1)
     {no strict;
      &{$phase};
      cluck $@ if $@;
     }

    push @times, [$phase, my $delta = sprintf("%12.4f", time - $startTime)];
    lll " " x 40, pad("$delta $phase", 32)." (time in seconds for phase)" if showPhaseTimes;
   }

  formatTables([@times],                                                        # Phase times
    columns => <<END,
Phase Convert Xml to Dita processing phase
Time_In_Seconds  Time in seconds taken by this processing phase
END
    title   => qq(Processing phases elapsed times in seconds),
    head    => <<END,
Convert Xml to Dita NNNN processing phases took the following times in seconds on DDDD
END
    file    => fpe(q(timing), qw(convertXmlToDita_phases txt)));                # Write phase times

  formatHtmlTablesIndex(&reports, &conversion, &getFileUrl, 3);                 # Create an index of html files for use as an initial page of Xref results

  removePid;                                                                    # Conversion is complete

  if (my $delta = formatRunTime($runTime))                                      # Print run time
   {lll conversion, "\nFinished in $delta seconds with ",
        maximumProcesses, " processes.";
   }

  print STDERR $lintReport->summary if $lintReport;                             # Restate lint summary

  pleaseSeeReport;                                                              # Location of results

  genHash(q(Data::Edit::Xml::To::Dita::Results),                                # Conversion results
    lint => $lintReport,                                                        # Return the lint report
    xref => $xrefResults,                                                       # Return the xref results
   );
 }

#D0

#-------------------------------------------------------------------------------
# Export - eeee
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA          = qw(Exporter);
@EXPORT       = qw();

@EXPORT_OK   = qw(
$projects $lintResults $lintReport $xrefResults
ddd
eee
lll
);

%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

# podDocumentation

=pod

=encoding utf-8

=head1 Name

Data::Edit::Xml::To::Dita - Convert multiple Xml documents in parallel to Dita.

=head1 Synopsis

A framework for converting multiple Xml documents in parallel to Dita:

  use Data::Edit::Xml::To::Dita;

  sub convertDocument($$)
   {my ($project, $x) = @_;                   # use sumAbsRel to get default home

    $x->by(sub
     {my ($c) = @_;
      if ($c->at_conbody)
       {$c->putFirst($c->new(<<END));
<p>Hello world!</p>
END
       }
     });
   }

  Data::Edit::Xml::To::Dita::createSampleInputFiles;
  Data::Edit::Xml::To::Dita::convertXmlToDita;

Evaluate the results of the conversion by reading the summary file in the
B<reports/> folder:

  use Data::Table::Text qw(fpe readFile);

  if (lint) # Lint report if available
   {my $s = readFile(&summaryFile);
    $s =~ s(\s+on.*) ()ig;
    my $S = <<END;

  Summary of passing and failing projects

  100 % success. Projects: 0+1=1.  Files: 0+1=1. Errors: 0,0

  CompressedErrorMessagesByCount (at the end of this file):        0

  FailingFiles   :         0
  PassingFiles   :         1

  FailingProjects:         0
  PassingProjects:         1


  FailingProjects:         0
     #  Percent   Pass  Fail  Total  Project
                                               # use sumAbsRel to get default home


  PassingProjects:         1
     #   Files  Project
     1       1  1


  DocumentTypes: 1

  Document  Count
  concept       1


  100 % success. Projects: 0+1=1.  Files: 0+1=1. Errors: 0,0

  END

    ok $s eq $S;
   }

See the converted files in the B<out/> folder:

  if (1) # Converted file
   {my $s = nwsc(readFile(fpe(&out, qw(hello_world dita))));
    my $S = nwsc(<<END);

  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE concept PUBLIC "-//OASIS//DTD DITA Concept//EN" "concept.dtd" []>
  <concept id="c1">
    <title id="title">Hello World</title>
    <conbody>
      <p>Hello world!</p>
    </conbody>
  </concept>
  END

    ok $S eq $s;
   }

=head1 Description

Convert multiple Xml documents in parallel to Dita.


Version 20190721.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Convert Xml to the Dita standard.

Convert Xml to the Dita standard.

=head1 Methods

Methods defined in this package.

=head2 spelling($$)

Fix spelling in source string

     Parameter  Description
  1  $s         Source string
  2  $file      File being processed

B<Example:>


  sub 𝘀𝗽𝗲𝗹𝗹𝗶𝗻𝗴($$)
   {my ($s, $file) = @_;                                                          # Source string, file being processed
    $s
   }


You can provide you own implementation of this method in your calling package
via:

  sub spelling {...}

if you wish to override the default processing supplied by this method.



=head2 spellingOut($)

Fix spelling in output string

     Parameter  Description
  1  $s         Output string

B<Example:>


  sub 𝘀𝗽𝗲𝗹𝗹𝗶𝗻𝗴𝗢𝘂𝘁($)
   {my ($s) = @_;                                                                 # Output string
    $s
   }


You can provide you own implementation of this method in your calling package
via:

  sub spellingOut {...}

if you wish to override the default processing supplied by this method.



=head2 chooseIDGroup($)

Return the id group for a project - files with the same id group share the same set of id attributes.

     Parameter  Description
  1  $project   Project

B<Example:>


  sub 𝗰𝗵𝗼𝗼𝘀𝗲𝗜𝗗𝗚𝗿𝗼𝘂𝗽($)
   {my ($project) = @_;                                                           # Project
    q(all);
   }


You can provide you own implementation of this method in your calling package
via:

  sub chooseIDGroup {...}

if you wish to override the default processing supplied by this method.



=head2 selectFileForProcessing($$$)

Select an input file for processing

     Parameter  Description
  1  $file      Full file name
  2  $number    Project number
  3  $failed    Known to have failed on last AWS run if true

B<Example:>


  sub 𝘀𝗲𝗹𝗲𝗰𝘁𝗙𝗶𝗹𝗲𝗙𝗼𝗿𝗣𝗿𝗼𝗰𝗲𝘀𝘀𝗶𝗻𝗴($$;$)
   {my ($file, $number, $failed) = @_;                                            # Full file name, project number, known to have failed on last AWS run if true
    $file
   }


You can provide you own implementation of this method in your calling package
via:

  sub selectFileForProcessing {...}

if you wish to override the default processing supplied by this method.



=head2 formatXml($)

Format xml

     Parameter  Description
  1  $x         Parse tree

B<Example:>


  sub 𝗳𝗼𝗿𝗺𝗮𝘁𝗫𝗺𝗹($)
   {my ($x) = @_;                                                                 # Parse tree
    $x->prettyStringDitaHeaders;
   }


You can provide you own implementation of this method in your calling package
via:

  sub formatXml {...}

if you wish to override the default processing supplied by this method.



=head2 parseProject($)

Parse a project.  Originally the results were cached for later reuse but it takes on only a few milliseconds and the cached results slow the AMI start up.

     Parameter  Description
  1  $project   Project

B<Example:>


  sub 𝗽𝗮𝗿𝘀𝗲𝗣𝗿𝗼𝗷𝗲𝗰𝘁($)
   {my ($project) = @_;                                                           # Project
    parseFile $project->source                                                    # Parse the source
   }


You can provide you own implementation of this method in your calling package
via:

  sub parseProject {...}

if you wish to override the default processing supplied by this method.



=head2 cleanUpCutOutTopic($$)

Clean up a topic once it has been cut out and its output file has been assigned

     Parameter  Description
  1  $project   Project
  2  $x         Parse

B<Example:>


  sub 𝗰𝗹𝗲𝗮𝗻𝗨𝗽𝗖𝘂𝘁𝗢𝘂𝘁𝗧𝗼𝗽𝗶𝗰($$)
   {my ($project, $x) = @_;                                                       # Project, parse
   }


You can provide you own implementation of this method in your calling package
via:

  sub cleanUpCutOutTopic {...}

if you wish to override the default processing supplied by this method.



=head2 cleanUpBookMap($$)

Clean up a book map once all its topics have been cut out and its output file has been assigned

     Parameter  Description
  1  $project   Project
  2  $x         Parse

B<Example:>


  sub 𝗰𝗹𝗲𝗮𝗻𝗨𝗽𝗕𝗼𝗼𝗸𝗠𝗮𝗽($$)
   {my ($project, $x) = @_;                                                       # Project, parse
   }


You can provide you own implementation of this method in your calling package
via:

  sub cleanUpBookMap {...}

if you wish to override the default processing supplied by this method.



=head2 standardDitaCleanUp($$)

Clean up some items that always need to be done in Dita topics

     Parameter  Description
  1  $project   Project
  2  $x         Parse

B<Example:>


  sub 𝘀𝘁𝗮𝗻𝗱𝗮𝗿𝗱𝗗𝗶𝘁𝗮𝗖𝗹𝗲𝗮𝗻𝗨𝗽($$)
   {my ($project, $x) = @_;                                                       # Project, parse
    standardDitaCleanUpDefault($project, $x);                                     # Default standard clean up
   } # 𝘀𝘁𝗮𝗻𝗱𝗮𝗿𝗱𝗗𝗶𝘁𝗮𝗖𝗹𝗲𝗮𝗻𝗨𝗽


You can provide you own implementation of this method in your calling package
via:

  sub standardDitaCleanUp {...}

if you wish to override the default processing supplied by this method.



=head2 convertDocument($$)

Convert one document.

     Parameter  Description
  1  $project   Project == document to convert
  2  $x         Parse tree.

B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗗𝗼𝗰𝘂𝗺𝗲𝗻𝘁($$)
   {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
    $x->wrapDown_conbody_concept unless couldBeCutOut($x) or $x->isADitaMap;      # Turn it into a concept if not already a recognizable type
    $x->ditaObviousChanges;
    $x                                                                            # Return parse tree
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertDocument {...}

if you wish to override the default processing supplied by this method.



=head2 lintResults()

Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.


B<Example:>


  sub 𝗹𝗶𝗻𝘁𝗥𝗲𝘀𝘂𝗹𝘁𝘀
   {lintResultsDefault;                                                           # Only if lint requested
   }


You can provide you own implementation of this method in your calling package
via:

  sub lintResults {...}

if you wish to override the default processing supplied by this method.



=head2 beforeUploadToS3()

Copy additional files into position before upload to s3


B<Example:>


  sub 𝗯𝗲𝗳𝗼𝗿𝗲𝗨𝗽𝗹𝗼𝗮𝗱𝗧𝗼𝗦𝟯
   {
   }


You can provide you own implementation of this method in your calling package
via:

  sub beforeUploadToS3 {...}

if you wish to override the default processing supplied by this method.



=head2 uploadFoldersToS3()

Upload folders to S3


B<Example:>


  sub 𝘂𝗽𝗹𝗼𝗮𝗱𝗙𝗼𝗹𝗱𝗲𝗿𝘀𝗧𝗼𝗦𝟯
   {my ($processStarter) = @_;                                                    # Process starter

    my $p = s3Parms.s3ProfileValue;

    for my $dir(reports, out, perl, parseFailed, targets)
     {next unless -d $dir;
      my $target = swapFolderPrefix($dir, home, s3OutputFolder);
      my $c = qq(aws s3 sync $dir $target $p);
      lll $c;
      $processStarter->start(sub
       {say STDERR qx($c);
       });
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub uploadFoldersToS3 {...}

if you wish to override the default processing supplied by this method.



=head2 beforeConvertProjects($)

Run just before project conversion starts

     Parameter  Description
  1  $projects  Projects in the conversion

B<Example:>


  sub 𝗯𝗲𝗳𝗼𝗿𝗲𝗖𝗼𝗻𝘃𝗲𝗿𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀($)
   {my ($projects) = @_;                                                          # Projects in the conversion
   }


You can provide you own implementation of this method in your calling package
via:

  sub beforeConvertProjects {...}

if you wish to override the default processing supplied by this method.



=head2 afterConvertProjects($)

Run just after project conversion starts

     Parameter  Description
  1  $projects  Projects in the conversion

B<Example:>


  sub 𝗮𝗳𝘁𝗲𝗿𝗖𝗼𝗻𝘃𝗲𝗿𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀($)
   {my ($projects) = @_;                                                          # Projects in the conversion
   }


You can provide you own implementation of this method in your calling package
via:

  sub afterConvertProjects {...}

if you wish to override the default processing supplied by this method.



=head2 restructureOneDocument($$$$)

Restructure one document

     Parameter  Description
  1  $phase     Phase
  2  $lint      Lint results
  3  $xref      Xref results
  4  $x         Parse tree

B<Example:>


  sub 𝗿𝗲𝘀𝘁𝗿𝘂𝗰𝘁𝘂𝗿𝗲𝗢𝗻𝗲𝗗𝗼𝗰𝘂𝗺𝗲𝗻𝘁($$$$)
   {my ($phase, $lint, $xref, $x) = @_;                                           # Phase, lint results, xref results, parse tree
   } # 𝗿𝗲𝘀𝘁𝗿𝘂𝗰𝘁𝘂𝗿𝗲𝗢𝗻𝗲𝗗𝗼𝗰𝘂𝗺𝗲𝗻𝘁


You can provide you own implementation of this method in your calling package
via:

  sub restructureOneDocument {...}

if you wish to override the default processing supplied by this method.



=head2 restructureCleanUp($$@)

Cleanup after each restructuring phase

     Parameter  Description
  1  $phase     Phase
  2  $xref      Xref details
  3  @cleanUps  Cleanup requests

B<Example:>


  sub 𝗿𝗲𝘀𝘁𝗿𝘂𝗰𝘁𝘂𝗿𝗲𝗖𝗹𝗲𝗮𝗻𝗨𝗽($$@)
   {my ($phase, $xref, @cleanUps) = @_;                                           # Phase, Xref details, cleanup requests
   } # 𝗿𝗲𝘀𝘁𝗿𝘂𝗰𝘁𝘂𝗿𝗲𝗖𝗹𝗲𝗮𝗻𝗨𝗽


You can provide you own implementation of this method in your calling package
via:

  sub restructureCleanUp {...}

if you wish to override the default processing supplied by this method.



=head2 editOneOutputFile($)

Edit one output file

     Parameter  Description
  1  $file      File to clean up

B<Example:>


  sub 𝗲𝗱𝗶𝘁𝗢𝗻𝗲𝗢𝘂𝘁𝗽𝘂𝘁𝗙𝗶𝗹𝗲($)
   {my ($file) = @_;                                                              # File to clean up
    $file                                                                         # Return data to be passed to L<editOutputFilesCleanUp>
   } # 𝗲𝗱𝗶𝘁𝗢𝗻𝗲𝗢𝘂𝘁𝗽𝘂𝘁𝗙𝗶𝗹𝗲


You can provide you own implementation of this method in your calling package
via:

  sub editOneOutputFile {...}

if you wish to override the default processing supplied by this method.



=head2 editOutputFilesCleanUp(@)

Clean up edited output files

     Parameter  Description
  1  @files     Results from each invocationof L<editOneOutputFile>

B<Example:>


  sub 𝗲𝗱𝗶𝘁𝗢𝘂𝘁𝗽𝘂𝘁𝗙𝗶𝗹𝗲𝘀𝗖𝗹𝗲𝗮𝗻𝗨𝗽(@)
   {my (@files) = @_;                                                             # Results from each invocationof L<editOneOutputFile>
   } # 𝗲𝗱𝗶𝘁𝗢𝘂𝘁𝗽𝘂𝘁𝗙𝗶𝗹𝗲𝘀𝗖𝗹𝗲𝗮𝗻𝗨𝗽


You can provide you own implementation of this method in your calling package
via:

  sub editOutputFilesCleanUp {...}

if you wish to override the default processing supplied by this method.



=head2 editAllOutputFiles()

Edit output files


=head2 saveCode()

Save code if developing


B<Example:>


  sub 𝘀𝗮𝘃𝗲𝗖𝗼𝗱𝗲
   {if (develop)
     {saveCodeToS3(1200, &perl, client, q(ryffine/code/perl/),
             q(--only-show-errors --profile fmc --region eu-west-1));
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub saveCode {...}

if you wish to override the default processing supplied by this method.



=head2 convertXmlToDita()

Perform all the conversion projects. This routine is not really replaceable but using the replace mechanism is a convenient way to export it.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗫𝗺𝗹𝗧𝗼𝗗𝗶𝘁𝗮
   {my ($package) = caller;
    lll conversion;                                                               # Title of run

    unlink errorLogFile;                                                          # Clear log
    replacePid;                                                                   # Make this conversion the current conversion

    for my $phase(q(saveCode),                                                    # Execute conversion phases
                  q(reportProgramAttributeSettings),
                  q(checkParameters),
                  q(setAtHits),
                  q(downloadFiles),
                  q(makeXmlFile),
                  q(convertProjects),
                  q(lintResults),
                  q(restructureResults),
                  q(editAllOutputFiles),
                  q(runTests),
                  q(convertDitaToHtml),
                  q(uploadResults),
                  q(notifyUsers))
     {no strict;
      #lll "Phase: ", $phase;
      &{$phase};
     }

    $endTime = time;                                                              # Run time statistics
    $runTime = int($endTime - $startTime);
    lll conversion, "finished in $runTime seconds";                               # Print run time

    removePid;                                                                    # Conversion is complete
    $lintReport;                                                                  # Return the lint report
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertXmlToDita {...}

if you wish to override the default processing supplied by this method.




=head2 Project Definition


Project definition




=head3 Output fields


B<convertTime> - Number of seconds taken to convert the document

B<empty> - Mark projects that are empty so they can be removed if desirable

B<idGroup> - Projects with the same id group share id attributes.

B<name> - Name of project

B<number> - Number of project

B<parseFailed> - Parse of source file failed

B<source> - Input file

B<sourceSize> - Size of input file

B<targets> - Where the items cut out of this topic wind up

B<test> - Test projects write their results unlinted to testResults



=head1 Attributes


The following is a list of all the attributes in this package.  A method coded
with the same name in your package will over ride the method of the same name
in this package and thus provide your value for the attribute in place of the
default value supplied for this attribute by this package.

=head2 Replaceable Attribute List


catalog changeBadXrefToPh clearCount client conversion convert debug deguidize develop ditaBin ditaXrefs docSet download downloads editOutputFiles endTime errorLogFile exchange exchangeHome exchangeItems extendedNames fails fixBadRefs fixDitaRefs fixFailingFiles fixRelocatedRefs fixXrefsByTitle hits hitsFolder home imageCache in inputExt lint makeXml maximumProcesses mimajen notify numberOfFiles out outExtMap outExtTopic parseFailed perl pidFile printTopicTrees process publications publish reports restructure restructurePhases runLogFile runTime s3Exchange s3Parms s3Profile sourceFrom startTime summaryFile targets testExchangeIn testExchangeOut testExpected testFails testFails2 testMode testResults testStandAlone tests titleOnly topicTrees upload user version www wwwFolder xref xrefAddNavTitles xrefAllowUniquePartialMatches xrefMatchTopics zipOnWwwFile


=head2 catalog

Dita catalog to be used for linting.


=head2 changeBadXrefToPh

Change xrefs being placed in M3 by L<Data::Edit::Xml::Xref> to B<ph>.


=head2 clearCount

Limit on number of files to clear from each output folder.


=head2 client

The name of the client


=head2 conversion

Conversion name


=head2 convert

Convert documents to Dita if true.


=head2 debug

Debug if true.


=head2 deguidize

0 - normal processing, 1 - replace guids in hrefs with their target files to deguidize dita references. Given href g1#g2/id convert g1 to a file name by locating the topic with topicId g2.


=head2 develop

Production run if this file folder is detected otherwise development.


=head2 ditaBin

Location of Dita tool


=head2 ditaXrefs

Convert xref hrefs expressed as just ids to Dita format - useful in non Dita to Dita conversions for example: docBook


=head2 docSet

Select set of documents to convert.


=head2 download

Download from S3 if true.


=head2 downloads

Downloads folder.


=head2 editOutputFiles

1 - edit output files with xref information available if at 100% lint, 2 - edit output files regardless of Lint results, 0 - do not edit output files


=head2 endTime

End time of run in seconds since the epoch.


=head2 errorLogFile

Error log file.


=head2 exchange

1 - upload to S3 Exchange if at 100% lint, 2 - upload to S3 Exchange regardless, 0 - no upload to S3 Exchange.


=head2 exchangeHome

Home of exchange folder


=head2 exchangeItems

The items to be uploaded to the exchange folder: d - downloads, i - in, p - perl, o - out, t - topic trees. Reports are uploaded by default


=head2 extendedNames

Expected number of output topics or B<undef> if unknown


=head2 fails

Copies of failing documents in a separate folder to speed up downloading.


=head2 fixBadRefs

Mask bad references using M3: the Monroe Masking Method if true


=head2 fixDitaRefs

Fix references in a corpus of L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> documents that have been converted to the L<GB Standard|http://metacpan.org/pod/Dita::GB::Standard>.


=head2 fixFailingFiles

Fix failing files in the L<testFails|/testFails> folder if this attribute is true


=head2 fixRelocatedRefs

Fix references to (re|un)located files that adhere to the GB standard.


=head2 fixXrefsByTitle

Fix failing xrefs by looking for the unique topic with a title that matches the text of the xref.


=head2 hits

1 - track hits so we can see which transformations are actually being used - normally off to avoid the overhead


=head2 hitsFolder

Folder containing at method hits by process id


=head2 home

Home folder containing all the other folders.


=head2 imageCache

Converted images are cached here to speed things up


=head2 in

Input documents folder.


=head2 inputExt

Extension of input files.


=head2 lint

Lint output xml


=head2 makeXml

Convert downloaded documents to xml in the L<in> folder if true


=head2 maximumProcesses

Maximum number of conversion processes to run in parallel.


=head2 mimajen

1- Copy files to web, 0 - suppress


=head2 notify

1 - Broadcast results of conversion if at 100% lint, 2 - broadcast regardless of error count.


=head2 numberOfFiles

Expected number of output files


=head2 out

Converted documents output folder.


=head2 outExtMap

Preferred output extension for a map


=head2 outExtTopic

Preferred output extension for a topic


=head2 parseFailed

Folder for details of xml parse failures


=head2 perl

Perl folder.


=head2 pidFile

File containing the pid for the current conversion process


=head2 printTopicTrees

1 - print the parse tree before cutting out the topics


=head2 process

Process data folder used to communicate results between processes.


=head2 publications

Publications folder if we use Dita OT to convert to Html


=head2 publish

Convert Dita to h : Html, p : pdf  via DITA-OT if at 100% lint,  Convert regardless if uppercase: H or P.


=head2 reports

Reports folder.


=head2 restructure

1 - Restructure results of conversion if at 100% lint, 2 - restructure regardless of error count.


=head2 restructurePhases

Number of restructuring phases to run


=head2 runLogFile

Run log file.


=head2 runTime

Elapsed run time in seconds.


=head2 s3Exchange

Exchange folder on S3


=head2 s3Parms

Additional S3 parameters for uploads and downloads.


=head2 s3Profile

Aws cli profile keyword value if any.


=head2 sourceFrom

Source of input, either github:userid/repository or s3://bucketName/folderName


=head2 startTime

Start time of run in seconds since the epoch.


=head2 summaryFile

Summary report file.


=head2 targets

Duplicates the in file structure - each file there-in shows us where the original file went


=head2 testExchangeIn

Exchange folder in which to receive tests so that test writers can disarrange their exchange folders as they please without disrupting testing at this end.


=head2 testExchangeOut

Exchange folder to publish tests results in so test writers can see the results in at L<testResults|/testResults>


=head2 testExpected

Folder containing test results expected.


=head2 testFails

Folder containing failing files to be fixed by reprocessing them but only if fixFailingFiles is true


=head2 testFails2

Folder containing files still unfixed by the current set of fixes


=head2 testMode

1 - run development tests, 2- run standalone tests, 0 run production documents


=head2 testResults

Folder containing actual test results locally, copied to: L<testExchangeOut|/testExchangeOut>


=head2 testStandAlone

Folder containing standalone tests which is used instead of regression tests if content is present


=head2 tests

Folder containing test input files received from test developer at L<testExchangeIn|/testExchangeIn>


=head2 titleOnly

Use only the title of topics to create GB Standard file names otherwise use the following text as well if the title is too short


=head2 topicTrees

Folder to contain printed topic trees if requested by printTopicTrees


=head2 upload

Upload to S3 Bucket if true and the conversion is at 100%, 2 - upload to S3 Bucket regardless, 0 - no upload to S3 Bucket.


=head2 user

Aws userid


=head2 version

Description of this run as printed in notification message and title


=head2 www

Web server folder - general


=head2 wwwFolder

Web server folder - conversion


=head2 xref

Xref output xml.


=head2 xrefAddNavTitles

Add navtitles to bookmap entries if true


=head2 xrefAllowUniquePartialMatches

Allow partial matching - i.e ignore the stuff to the right of the # in a reference if doing so produces a unique result


=head2 xrefMatchTopics

Either 0 for no topic matching or the percentage confidence level for topic matching


=head2 zipOnWwwFile

Conversion results on web server




=head1 Optional Replace Methods

The following is a list of all the optionally replaceable methods in this
package.  A method coded with the same name in your package will over ride the
method of the same name in this package providing your preferred processing for
the replaced method in place of the default processing supplied by this
package. If you do not supply such an over riding method, the existing method
in this package will be used instead.

=head2 Replaceable Method List


afterConvertProjects beforeConvertProjects beforeUploadToS3 chooseIDGroup cleanUpBookMap cleanUpCutOutTopic convertDocument convertXmlToDita editOneOutputFile editOutputFilesCleanUp formatXml lintResults parseProject restructureCleanUp restructureOneDocument saveCode selectFileForProcessing spelling spellingOut standardDitaCleanUp uploadFoldersToS3




=head1 Private Methods

=head2 getHome()

Compute home directory once.


=head2 s3ProfileValue()

S3 profile keyword.


=head2 conversionName()

Conversion name.


=head2 urlName($)

Given a file name in L<www> return its url on the current server or confess if the file is not in L<www>

     Parameter  Description
  1  $f         File

=head2 replacePid()

Stop any existing conversion and make this conversion the current one


=head2 removePid()

Remove the pid file as the conversion has completed


=head2 setAtHits()

Set hit tracking


=head2 analyzeHits()

Analyze the hits to find "at" calls that always fail so we can consider them for removal


=head2 ddd(@)

Log development messages

     Parameter  Description
  1  @m         Messages

=head2 eee(@)

Log error messages

     Parameter  Description
  1  @m         Messages

=head2 sourceFromGitHub()

Source is on GitHub


=head2 sourceFromS3()

Source is on S3


=head2 s3InputFolder()

S3 input folder


=head2 s3OutputFolder()

S3 output folder


=head2 s3ExchangeFolder()

S3 exchange folder


=head2 copyToAws()

Copy to aws


=head2 getFromAws()

Get results from Aws


=head2 pleaseSee()

AWS command to see results


=head2 mifToXml($)

Convert Mif to Xml

     Parameter   Description
  1  $inputFile  File containing mif

=head2 convertImageToSvg($)

Convert a graphics file to svg

     Parameter  Description
  1  $file      File to convert

=head2 downloadFiles()

Download documents from S3 or GH to the L<downloads|/downloads> folder.


=head2 makeOneXmlFile()

Convert one file to utf8 and return undef if successful else the name of the document in error


=head2 makeXmlFile()

Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.


=head2 projectCount()

Number of projects.


=head2 newProject($)

Project details including at a minimum the name of the project and its source file.

     Parameter  Description
  1  $source    Source file

=head2 findProjectFromSource($)

Locate a project by its source file

     Parameter  Description
  1  $source    Full file name

=head2 findProjectWithLargestSource()

Locate the project with the largest input source file


=head2 loadFailingFiles()

Find source of each failing file while developing


=head2 loadProjects()

Locate documents to convert from folder L<in|/in>.


=head2 parseFile($)

Parse a file

     Parameter  Description
  1  $file      File

=head2 lintTopic($$$)

Lint a topic and return the lint details

     Parameter  Description
  1  $project   Project
  2  $x         Parse tree
  3  $title     Optional title of lint

=head2 createTarget($$$$)

Lint a book map

     Parameter       Description
  1  $source         Source file
  2  $target         Target file produced from source file
  3  $sourceDocType  Source document type
  4  $targetType     Target type: bookmap|image|topic

=head2 lintBookMap($$$$)

Lint a book map

     Parameter  Description
  1  $project   Project
  2  $x         Parse tree of source
  3  $bookMap   Book map parse tree
  4  $title     Title of lint

=head2 topicIsEssentiallyEmpty($)

Return B<1> if the topic is essentially empty else B<undef>.

     Parameter  Description
  1  $file      File to check

=head2 findImage($)

Find an image that has been misplaced

     Parameter  Description
  1  $image     Image to locate

=head2 standardDitaCleanUpDefault($$)

Clean up some items that always need to be done in Dita topics

     Parameter  Description
  1  $project   Project
  2  $x         Parse

=head2 couldBeCutOut($)

Return true if this node can be cut out

     Parameter  Description
  1  $node      Node to test

=head2 cutOutTopics($$)

Cut out the topics in a document assuming that they are nested within the parse tree and create a bookmap from the residue if it is not already a bookmap

     Parameter  Description
  1  $project   Project == document to cut
  2  $x         Parse tree.

=head2 convertProject($)

Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.

     Parameter  Description
  1  $project   Project == document to convert

=head2 xrefResults()

Run Xref to fix check results


=head2 lintResultsDefault()

Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.


=head2 copyLogFiles()

Copy log files to reports/ so they get uploaded too


=head2 actionAfterLintRequired($)

Determine whether a post lint action should happen or be suppressed

     Parameter  Description
  1  $action    Action attribute

=head2 actionAfterLint($$)

Determine whether a post lint action should happen or be suppressed

     Parameter  Description
  1  $action    Action attribute
  2  $name      Action name

=head2 uploadToS3($)

Copy entire home folder to S3

     Parameter        Description
  1  $processStarter  Process starter

=head2 uploadToExchange()

Copy entire home folder to Exchange


=head2 uploadResults()

Upload results


=head2 bookMapPublicationFolder($)

Folder for html obtained by converting bookmap in supplied file

     Parameter  Description
  1  $bookMap   Bookmap file

=head2 convertBookMapToHtml($)

Publish bookmaps on web server

     Parameter  Description
  1  $bookMap   Bookmap

=head2 convertBookMapsToHtml()

Publish bookmaps on web server


=head2 convertDitaToHtml()

Publish bookmaps on web server


=head2 runTests()

Run tests by comparing files in folder L<out|/out> with corresponding files in L<testResults|/testResults>.


=head2 normalizeXml($)

Remove document processor tags

     Parameter  Description
  1  $string    Text to normalize

=head2 testResult($$$)

Evaluate the results of a test

     Parameter  Description
  1  $test      Test name
  2  $got       What we got
  3  $expected  What we expected result

=head2 checkResults()

Check test results


=head2 reportProjectsThatFailedToParse()

Report projects that failed to parse


=head2 reportProjectConversionTimes()

Report project conversion times


=head2 reportSourceMapToTargets()

Report where the source files went - done in Xref at: lists source_to_targets


=head2 convertSelectedProjects()

Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.


=head2 fixDitaXrefHrefs()

Fix single word xref href attributes so that they are in dita format - these tend to originate in non dita xml.


=head2 reportProgramAttributeSettings()

Report the attribute settings


=head2 reportDownloadExtensions()

Report extensions of downloaded files


=head2 convertProjects()

Convert the selected documents.


=head2 restructureOneFile($$)

Restructure one output file

     Parameter  Description
  1  $phase     Phase
  2  $file      File to restructure

=head2 restructureResultsFiles()

Restructure output folders based on results from Lint and Xre


=head2 restructureResults()

Restructure output folders based on results from Lint and Xre


=head2 notifyUsers()

Notify users of results


=head2 replaceableMethods()

Replaceable methods


=head2 attributeMethods()

Attribute methods


=head2 overrideMethods($)

Merge packages

     Parameter  Description
  1  $package   Name of package to be merged defaulting to that of the caller.

=head2 checkParameters()

Check parameters for obvious failures



=head1 Index


1 L<actionAfterLint|/actionAfterLint> - Determine whether a post lint action should happen or be suppressed

2 L<actionAfterLintRequired|/actionAfterLintRequired> - Determine whether a post lint action should happen or be suppressed

3 L<afterConvertProjects|/afterConvertProjects> - Run just after project conversion starts

4 L<analyzeHits|/analyzeHits> - Analyze the hits to find "at" calls that always fail so we can consider them for removal

5 L<attributeMethods|/attributeMethods> - Attribute methods

6 L<beforeConvertProjects|/beforeConvertProjects> - Run just before project conversion starts

7 L<beforeUploadToS3|/beforeUploadToS3> - Copy additional files into position before upload to s3

8 L<bookMapPublicationFolder|/bookMapPublicationFolder> - Folder for html obtained by converting bookmap in supplied file

9 L<checkParameters|/checkParameters> - Check parameters for obvious failures

10 L<checkResults|/checkResults> - Check test results

11 L<chooseIDGroup|/chooseIDGroup> - Return the id group for a project - files with the same id group share the same set of id attributes.

12 L<cleanUpBookMap|/cleanUpBookMap> - Clean up a book map once all its topics have been cut out and its output file has been assigned

13 L<cleanUpCutOutTopic|/cleanUpCutOutTopic> - Clean up a topic once it has been cut out and its output file has been assigned

14 L<conversionName|/conversionName> - Conversion name.

15 L<convertBookMapsToHtml|/convertBookMapsToHtml> - Publish bookmaps on web server

16 L<convertBookMapToHtml|/convertBookMapToHtml> - Publish bookmaps on web server

17 L<convertDitaToHtml|/convertDitaToHtml> - Publish bookmaps on web server

18 L<convertDocument|/convertDocument> - Convert one document.

19 L<convertImageToSvg|/convertImageToSvg> - Convert a graphics file to svg

20 L<convertProject|/convertProject> - Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.

21 L<convertProjects|/convertProjects> - Convert the selected documents.

22 L<convertSelectedProjects|/convertSelectedProjects> - Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.

23 L<convertXmlToDita|/convertXmlToDita> - Perform all the conversion projects.

24 L<copyLogFiles|/copyLogFiles> - Copy log files to reports/ so they get uploaded too

25 L<copyToAws|/copyToAws> - Copy to aws

26 L<couldBeCutOut|/couldBeCutOut> - Return true if this node can be cut out

27 L<createTarget|/createTarget> - Lint a book map

28 L<cutOutTopics|/cutOutTopics> - Cut out the topics in a document assuming that they are nested within the parse tree and create a bookmap from the residue if it is not already a bookmap

29 L<ddd|/ddd> - Log development messages

30 L<editAllOutputFiles|/editAllOutputFiles> - Edit output files

31 L<editOneOutputFile|/editOneOutputFile> - Edit one output file

32 L<editOutputFilesCleanUp|/editOutputFilesCleanUp> - Clean up edited output files

33 L<eee|/eee> - Log error messages

34 L<findImage|/findImage> - Find an image that has been misplaced

35 L<findProjectFromSource|/findProjectFromSource> - Locate a project by its source file

36 L<findProjectWithLargestSource|/findProjectWithLargestSource> - Locate the project with the largest input source file

37 L<fixDitaXrefHrefs|/fixDitaXrefHrefs> - Fix single word xref href attributes so that they are in dita format - these tend to originate in non dita xml.

38 L<formatXml|/formatXml> - Format xml

39 L<getFromAws|/getFromAws> - Get results from Aws

40 L<getHome|/getHome> - Compute home directory once.

41 L<downloadFiles|/downloadFiles> - Download documents from S3 or GH to the L<downloads|/downloads> folder.

42 L<lintBookMap|/lintBookMap> - Lint a book map

43 L<lintResults|/lintResults> - Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.

44 L<lintResultsDefault|/lintResultsDefault> - Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.

45 L<lintTopic|/lintTopic> - Lint a topic and return the lint details

46 L<loadFailingFiles|/loadFailingFiles> - Find source of each failing file while developing

47 L<loadProjects|/loadProjects> - Locate documents to convert from folder L<in|/in>.

48 L<makeOneXmlFile|/makeOneXmlFile> - Convert one file to utf8 and return undef if successful else the name of the document in error

49 L<makeXmlFile|/makeXmlFile> - Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.

50 L<mifToXml|/mifToXml> - Convert Mif to Xml

51 L<newProject|/newProject> - Project details including at a minimum the name of the project and its source file.

52 L<normalizeXml|/normalizeXml> - Remove document processor tags

53 L<notifyUsers|/notifyUsers> - Notify users of results

54 L<overrideMethods|/overrideMethods> - Merge packages

55 L<parseFile|/parseFile> - Parse a file

56 L<parseProject|/parseProject> - Parse a project.

57 L<pleaseSee|/pleaseSee> - AWS command to see results

58 L<projectCount|/projectCount> - Number of projects.

59 L<removePid|/removePid> - Remove the pid file as the conversion has completed

60 L<replaceableMethods|/replaceableMethods> - Replaceable methods

61 L<replacePid|/replacePid> - Stop any existing conversion and make this conversion the current one

62 L<reportDownloadExtensions|/reportDownloadExtensions> - Report extensions of downloaded files

63 L<reportProgramAttributeSettings|/reportProgramAttributeSettings> - Report the attribute settings

64 L<reportProjectConversionTimes|/reportProjectConversionTimes> - Report project conversion times

65 L<reportProjectsThatFailedToParse|/reportProjectsThatFailedToParse> - Report projects that failed to parse

66 L<reportSourceMapToTargets|/reportSourceMapToTargets> - Report where the source files went - done in Xref at: lists source_to_targets

67 L<restructureCleanUp|/restructureCleanUp> - Cleanup after each restructuring phase

68 L<restructureOneDocument|/restructureOneDocument> - Restructure one document

69 L<restructureOneFile|/restructureOneFile> - Restructure one output file

70 L<restructureResults|/restructureResults> - Restructure output folders based on results from Lint and Xre

71 L<restructureResultsFiles|/restructureResultsFiles> - Restructure output folders based on results from Lint and Xre

72 L<runTests|/runTests> - Run tests by comparing files in folder L<out|/out> with corresponding files in L<testResults|/testResults>.

73 L<s3ExchangeFolder|/s3ExchangeFolder> - S3 exchange folder

74 L<s3InputFolder|/s3InputFolder> - S3 input folder

75 L<s3OutputFolder|/s3OutputFolder> - S3 output folder

76 L<s3ProfileValue|/s3ProfileValue> - S3 profile keyword.

77 L<saveCode|/saveCode> - Save code if developing

78 L<selectFileForProcessing|/selectFileForProcessing> - Select an input file for processing

79 L<setAtHits|/setAtHits> - Set hit tracking

80 L<sourceFromGitHub|/sourceFromGitHub> - Source is on GitHub

81 L<sourceFromS3|/sourceFromS3> - Source is on S3

82 L<spelling|/spelling> - Fix spelling in source string

83 L<spellingOut|/spellingOut> - Fix spelling in output string

84 L<standardDitaCleanUp|/standardDitaCleanUp> - Clean up some items that always need to be done in Dita topics

85 L<standardDitaCleanUpDefault|/standardDitaCleanUpDefault> - Clean up some items that always need to be done in Dita topics

86 L<testResult|/testResult> - Evaluate the results of a test

87 L<topicIsEssentiallyEmpty|/topicIsEssentiallyEmpty> - Return B<1> if the topic is essentially empty else B<undef>.

88 L<uploadFoldersToS3|/uploadFoldersToS3> - Upload folders to S3

89 L<uploadResults|/uploadResults> - Upload results

90 L<uploadToExchange|/uploadToExchange> - Copy entire home folder to Exchange

91 L<uploadToS3|/uploadToS3> - Copy entire home folder to S3

92 L<urlName|/urlName> - Given a file name in L<www> return its url on the current server or confess if the file is not in L<www>

93 L<xrefResults|/xrefResults> - Run Xref to fix check results

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Data::Edit::Xml::To::Dita

=head1 Author

L<philiprbrenan@gmail.com|mailto:philiprbrenan@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

=head1 Copyright

Copyright (c) 2016-2019 Philip R Brenan.

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
qx(perl /home/phil/perl/cpan/DataEditXmlToDita/test.pl);
