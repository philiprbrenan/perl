#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/ -I/home/phil/perl/cpan/GitHubCrud/lib/ -I/home/phil/perl/cpan/DataEditXmlXref/lib/
#-------------------------------------------------------------------------------
# Data::Edit::Xml::To::Dita - Convert multiple Xml documents in parallel to Dita
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2019.01.19 23:01:21
#-------------------------------------------------------------------------------
# podDocumentation
# Fix report settings so it shows the total settings not just caller

=pod

2019.03.01 19:38:50 Use Xref to change image names from absolute names to relative names - we can use this for all internal hrefs

Used by Marvell mif to dita conversion
No xml transformations are allowed in this file except those required to cut out topics from book maps.
Md5Sum assigned names allows us to:
  bypass the gather stage to speeds things up
  flatten folder structure for easy references

=cut

package Data::Edit::Xml::To::DitaVa;
our $VERSION = 20190315;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump);
use Data::Edit::Xml;
use Data::Edit::Xml::Lint;
use Data::Edit::Xml::Xref;
use Data::Table::Text qw(:all !lll);
use Flip::Flop;
use GitHub::Crud;
use Scalar::Util qw(blessed);
use Time::HiRes qw(time);
use utf8;

#D1 Convert Xml to the Dita standard.                                           # Convert Xml to the Dita standard.

sub client         {undef}                                                      # The name of the client
sub conversion     {undef}                                                      # Conversion name.
sub convert        {1}                                                          # Convert documents to dita if true.
sub cuttingOut     {2}                                                          # 1 - assume that each input document will create one output topic, 2 - assume each input documents will have topics nested in side each other which must be cut out to produce multiple output documents.
sub debug          {1}                                                          # Debug if true.
sub docSet         {1}                                                          # Select set of documents to convert.
sub download       {!&develop}                                                  # Download from S3 if true.
sub exchange       {1}                                                          # 1 - upload to S3 Exchange if true and the conversion fails, 2 - upload to S3 Exchange regardless, 0 - no upload to S3 Exchange.
sub exchangeItems  {q(r)}                                                       # The items to be uploaded to the exchange folder: p - perl, o - out, r - reports
sub extendedNames  {0}                                                          # 0 - derive names solely from titles, 1 - consider text beyond the title when constructing file names
sub flattenImages  {1}                                                          # 0 - do not flatten images, 1 - flatten images.
sub lint           {1}                                                          # Lint output xml
sub mimajen        {1}                                                          # 1- Copy files to web, 0 - suppress
sub notify         {1}                                                          # 1 - Broadcast results of conversion if at 100%, 2 - broadcast regardless of error count.
sub publish        {1}                                                          # 1 - convert Dita to Html and publish via DITA-OT if conversion is 100% successful,  2 - publish regardless
sub testMode       {&develop ? 1 : 0}                                           # 1 - run development tests, 2- run standalone tests, 0 run production documents
sub unicode        {download}                                                   # Convert to utf8 if true.
sub upload         {&develop ? 0 : 1}                                           # 1 - upload to S3 Bucket if true and the conversion is at 100%, 2 - upload to S3 Bucket regardless, 0 - no upload to S3 Bucket.
sub xref           {1}                                                          # Xref output xml.
sub debugXrefTimes {0}                                                          # Debug Xref phase times if true
sub fixFailingFiles{0}                                                          # Fix failing files in the L<testFails|/testFails> folder if this attribute is true

sub catalog        {q(/home/phil/r/dita/dita-ot-3.1/catalog-dita.xml)}          # Dita catalog to be used for linting.
sub clearCount     {&develop ? 1e4 : 1e6}                                       # Limit on number of files to clear from each output folder.
sub develop        {-e q(/home/ubuntu/) ? 0 : 1}                                # Production run if this file folder is detected otherwise development.
sub ditaBin        {fpf(qw(/home phil r dita dita-ot-3.1 bin dita))}            # Location of Dita tool
sub downloads      {fpd(&home,    qw(download))}                                # Downloads folder.
sub errorLogFile   {fpe(&perl,    qw(eee txt))}                                 # Error log file.
sub fails          {fpd(&reports, qw(fails))}                                   # Copies of failing documents in a seperate folder to speed up downloading.
sub gathered       {fpd(&home,    qw(gathered))}                                # Folder containing saved parse trees after initial parse and information gathering.
sub exchangeHome   {fpd(qw(/home phil))}                                        # Home of exchange folder
sub home           {&getHome}                                                   # Home folder containing all the other folders.
#ub images         {fpd(home,     qw(images))}                                  # Input images folder.
sub imageCache     {fpd(home,     qw(imageCache))}                              # Converted images are cached here to speed things up
sub in             {fpd(&home,    qw(in))}                                      # Input documents folder.
sub inputExt       {qw(.xml .dita .ditamap)}                                    # Extension of input files.
sub logFile        {fpe(&perl,    qw(lll txt))}                                 # Log file.
sub logStderr      {fpe(&perl,    qw(zzz txt))}                                 # Log file containing output to STDERR.
sub out            {fpd(&home,    qw(out))}                                     # Converted documents output folder.
sub outExtTopic    {q(dita)}                                                    # Preferred output extension for a topic
sub outExtMap      {q(ditamap)}                                                 # Preferred output extension for a map
#ub outImages      {fpd(&home,    qw(out images))}                              # Output images.
sub parseCache     {fpd(&home,    qw(parseCache))}                              # Cached parse trees.
sub perl           {fpd(&home,    qw(perl))}                                    # Perl folder.
sub process        {fpd(&home,    qw(process))}                                 # Process data folder used to communicate results between processes.
sub publications   {fpd(&www,     qw(publications), client)}                    # Publications folder on web server for client
sub reports        {fpd(&home,    qw(reports))}                                 # Reports folder.
sub s3Bucket       {q(s3Bucket)}                                                # Bucket on S3 holding documents to convert and the converted results.
sub s3FolderIn     {q(originals).docSet}                                        # Folder on S3 containing original documents.
sub s3FolderUp     {q(results).docSet}                                          # Folder on S3 containing results of conversion.
sub s3Exchange     {q(exchange.ryffine/users/aws/avaya)}                        # Exchange folder on S3
sub s3Profile      {undef}                                                      # Aws cli profile keyword value if any.
sub s3Parms        {q(--quiet --delete)}                                        # Additional S3 parameters for uploads and downloads.
sub summaryFile    {fpe(reports, qw(summary txt))}                              # Summary report file.
sub tests          {fpd(&home, qw(tests/in))}                                   # Folder containing test input files received from test developer at L<testExchangeIn|/testExchangeIn>
sub testExpected   {fpd(&home, qw(tests/expected))}                             # Folder containing test results expected.
sub testExchangeIn {undef}                                                      # Exchange folder to receive tests in so that test writers can send testsand have them copied to L<tests|/test>
sub testExchangeOut{undef}                                                      # Exchange folder to publish tests results in so test writers can see the results in at L<testResults|/testResults>
sub testResults    {fpd(&home, qw(tests/results))}                              # Folder containing actual test results locally, copied to: L<testExchangeOut|/testExchangeOut>
sub testStandAlone {fpd(&home, qw(tests/standalone/active))}                    # Folder containing standalone tests which is used instead of regression tests if content is present
sub testFails      {fpd(&home, q(fails))}                                       # Folder containing failing files to be fixed by reprocessing them but only if fixFailingFiles is true
sub testFails2     {fpd(&home, q(fails2))}                                      # Folder containing files still unfixed by the current set of fixes
sub www            {fpd(qw(/var www html))}                                     # Web server folder

my $startTime      = time;                                                      # Start time.
my $endTime;                                                                    # End time value.
my $runTime;                                                                    # Run time value.
sub startTime      {$startTime}                                                 # Start time of run in seconds since the epoch.
sub endTime        {$endTime}                                                   # End time of run in seconds since the epoch.
sub runTime        {$runTime}                                                   # Elapsed run time in seconds.

sub maximumNumberOfProcesses    {develop ?  2 : 256}                            # Maximum number of conversion processes to run in parallel.
sub maximumNumberOfLinkProcesses{develop ?  2 : 256}                            # Maximum number of Lint::relint link processes - as they come later in the run fork burns more memory and we often run out of memory here.

sub maximumfileFromStringLength {50}                                            # Maximum amount of title to use in constructing output file names.
sub maximumNumberOfFilesToClear{develop ? 1e3 : 1e6}                            # Maximum number of files to clear.

our $standAlone;                                                                # When true we are operating in stand alone mode to test documents from testStandalone in isolation
our $projects    = {};                                                          # Projects == documents to convert.
our $project;                                                                   # Visible, thus loggable.
our $lintResults = q(No lint results yet!);                                     # Lint results.
our $lintReport;                                                                # Lint report if available.
#our %fileToTopicId;                                                             # {short file name} = topicId
#our %labelToFile;                                                               # {label or id}{short names of files containing label or id}
#our %labelToId;                                                                 # {label or id}{short file name}{ids of nodes containing label or id}

my $home;

sub getHome                                                                     #P Compute home directory once.
 {return $home if $home;
  my $c = currentDirectory;
  return $home = $c if $c =~ m(\A/home/phil/perl/cpan/)s;
  $home = fpd(Data::Table::Text::sumAbsAndRel($c, "../"));
 }

sub s3ProfileValue                                                              #r S3 profile keyword.
 {return '' unless my $p = s3Profile;
  qq( --profile $p)
 }

#D1 Methods                                                                     # Methods defined in this package.

sub lll(@)                                                                      # Log messages including the project name if available. This method is not merged as we need to retain its prototype.
 {my (@m) = @_;                                                                 # Messages
  return unless (join '', @_) =~ m(\S)s;

  my $m = join '', timeStamp, " $$ ", @_;                                       # Time stamp each message
  if ($project)
   {$m .= " in project $project";
   }
  my ($p, $f, $l) = caller();
  $m .= " at $f line $l\n" if develop;

  say STDERR $m;

  appendFile(logFile, ($m =~ s(\s+) ( )gsr).qq(\n));                            # Copy the message to the local log file
  $m                                                                            # Return message produced
 }

sub ddd(@)                                                                      # Log development messages
 {my (@m) = @_;                                                                 # Messages
  goto &lll if debug;
 }

sub eee(@)                                                                      # Log error messages
 {my (@m) = @_;                                                                 # Messages
  my $m = join '', dateTimeStamp, " $$ Error: ", @_;                            # Time stamp each message
  if ($project)
   {$m .= " in project $project";
   }
  my ($p, $f, $l) = caller();
  $m .= " at $f line $l\n";

  appendFile(errorLogFile, ($m =~ s(\s+) ( )gsr).qq(\n));
  say STDERR $m;
 }

sub s3InputFolder                                                               #r S3 input folder
 {q(s3://).fpd(s3Bucket, s3FolderIn);
 }

sub s3OutputFolder                                                              #r S3 output folder
 {q(s3://).fpd(s3Bucket, s3FolderUp);
 }

sub s3ExchangeFolder                                                            #r S3 output folder
 {q(s3://).s3Exchange;
 }

sub pleaseSee($$)                                                               #P AWS command to see results
 {my ($lint, $xref) = @_;
  my $success = $lintReport && $lintReport->totalErrors == 0;
  my $w = fpe(q(http://).awsIp, q(publications), client, qw(index html));
  my $p = qq(Please see $w or:);
  my $s = &upload   && &s3OutputFolder &&  $success||&upload   == 2 ? qq(aws s3 sync ).s3OutputFolder : q();
  my $x = &exchange && &s3Exchange     && !$success||&exchange == 2 ? s3Exchange                      : q();
  return qq($p $s or $x) if $s and $x;
  return qq($p $s)       if $s;
  return qq($p $x)       if        $x;
  q()
 }

sub mifToXml($)                                                                 # Convert Mif to Xml
 {my ($inputFile) = @_;                                                         # File containing mif
  lll "Convert mif file: $inputFile";

  my @xml;                                                                      # Generated Xml
  my @stack;                                                                    # Bracket stack

  my $string = readFile($inputFile);                                            # Read mif source
  my @lines = split /\n/, $string;                                              # Lines of input

  for my $i(keys @lines)                                                        # Fix strings spread across several lines
   {my $line = $lines[$i];
    if ($line =~ m(\A\s*<\w+\s*`)s and $line !~ m('>\s*\Z))                     # Unterminated string line
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

        my $i = copyBinaryFileMd5NormalizedCreate                               # Return the name of a file in the specified B<$folder> whose name is constructed from the md5 sum of the specified B<$content>, whose content is B<$content> and whose accompanying B<.imageDef> file contains the specified B<$name>.  Such a file can be copied multiple times by L<copyBinaryFileMd5Normalized|/copyBinaryFileMd5Normalized> regardless of the other files in the target folders while retaining the original name information.
         ($inputFile, $binary, lc($image), qq(at $inputFile line $i));

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

sub convertImageToSvg($)                                                        # Convert a graphics file to svg
 {my ($file) = @_;                                                              # File to convert
  return if $file =~ m((ole2)\Z)s;                                              # Add file types we cannot process correctly here to prevent them from being converted on each run

  my $svg = setFileExtension($file, q(svg));                                    # Converted image name

  my $cached = fpe(imageCache, fn($file), q(svg));                              # Cached image conversion - cannot use GB yet because the file will be called for by name

  if (-e $cached)                                                               # Reuse cached version
   {copyBinaryFile($cached, $svg);
   }
  else                                                                          # Convert and cache the image.
   {my $e = fe($file);
    my $c = qq(unoconv  -f svg -o "$svg" "$file");
       $c = qq(pdftocairo -svg "$file"  "$svg") if $e =~ m(pdf)is;
       $c = qq(convert         "$file"  "$svg") if $e =~ m(tiff)is;
       $c = qq(inkscape -e "$svg" "$file")      if $e =~ m(emf)is;
    lll qx($c);
    if (-e $svg)
     {copyBinaryFile($svg, $cached);
     }
    else
     {lll "Unable to convert image file to svg:\n$svg";
     }
   }
 }

sub downloadFromS3                                                              #r Download documents from S3 to the L<downloads|/downloads> folder.
 {if (&download)                                                                # Download if requested
   {lll "Download from S3";
    clearFolder(downloads, clearCount);
    makePath(downloads);
    my $s = s3InputFolder;
    my $t = downloads;
    my $p = s3Parms.s3ProfileValue;
    xxx qq(aws s3 sync $s $t $p);

    for my $f(searchDirectoryTreesForMatchingFiles(downloads, qw(.odp .odt)))   # Rename odp and odt files to zip so they get unzipped
     {my $target = setFileExtension($f, q(zip));
      rename $f, $target;
     }

    if (1)                                                                      # Some people put zip files inside zip files so we have to unzip until there are no moe zip files to unzip
     {my %unzipped;                                                             # Unzipped files
      for my $pass(1..3)                                                        # Let us hope that even the most ingenious customer will not zip file up to this depth!
       {lll "Unzip pass $pass";
         for my $zip(searchDirectoryTreesForMatchingFiles(downloads, q(.zip)))  # Unzip files
         {next if $unzipped{$zip}++;
          my $d = fp $zip;
          my $D = fpd($d, fn $zip);                                             # Folder in downloads into which to unzip the file as we do not know the internal file structure
          makePath($D);
          my $c = qq(cd "$d"; unzip -d "$D" -q "$zip"; rm -r "${D}__MACOSX" 2>/dev/null;);
          lll $c;
          xxx $c;
         }
       }
     }

    for my $zip(searchDirectoryTreesForMatchingFiles(downloads, q(.7z)))        # Unzip 7z files
     {my $d = downloads;
      my $D = fpd($d, fn $zip);                                                 # Folder in downloads into which to unzip the file as we do not know the internal file structure
      makePath($D);
      my $c = qq(cd "$d"; 7z e "$zip" -y -o"$D");
      lll $c;
      xxx $c;
     }

    if (1)                                                                      # Convert any mif files to xml as part of converting any downloaded files into something parseable
     {my $ps = newProcessStarter(maximumNumberOfProcesses);                     # Process starter
      for my $mif(searchDirectoryTreesForMatchingFiles(downloads, q(.mif)))     # Convert each mif file to xml
       {$ps->start(sub
         {my $f = mifToXml($mif);                                               # Convert to xml
          eval {Data::Edit::Xml::new($f)};                                      # Trial parse
          if ($@)                                                               # Report errors
           {confess "Unable to parse file:\n$mif\n$f\n$@\n";
           }
          1
         });
       }
      $ps->finish;                                                              # Finish conversions
     }

    if (1)                                                                      # Convert images to svg where possible - run in parallel
     {my @images = searchDirectoryTreesForMatchingFiles(downloads,
                  qw(.dib .emf .ole2 .pdf .tiff .wmf .vsd .vsdx));
      my $images = @images;
      lll "Found images of numerosity $images";
      for my $image(@images)                                                    # Convert images
       {convertImageToSvg($image);
       }
     }

    Flip::Flop::download();                                                     # Downloads as completed
   }
  else
   {ddd "Download from S3 not requested";
   }
 }

sub spelling($)                                                                 #r Fix spelling in source string
 {my ($s) = @_;                                                                 # Source string
  $s
 }

sub deTagString($)                                                              # Remove any xml tags from a string
 {my ($string) = @_;                                                            # String to detag
  $string =~ s(<[^>]*?>) ()gr
 }

sub convertOneFileToUTF8                                                        #P Convert one file to utf8 and return undef if successful else the name of the document in error
 {my ($source) = @_;                                                            # File to convert
  ddd "Convert one file: $source";
  my $d = downloads;
  my $i = in;
  my $target = $source =~ s(\A$d) ($i)r;                                        # Target
  makePath($target);

  if (isFileUtf8($source))                                                      # Copy file directly if already utf8
   {copyFile($source, $target);
   }
  else                                                                          # Convert file to utf8
   {my $fileType = sub
     {my $c = qx(file "$source") // q(unknown file type);
      return q(ASCII) if $c =~ m(ASCII text)s;
      return q(UTF8)  if $c =~ m(UTF-8 Unicode .*text)s;
      return q(UTF16) if $c =~ m(UTF-16 Unicode text)s;
      my $file = readBinaryFile($source);
      return q(UTF16) if $source =~ m(\Aencoding="UTF-16"\Z);
      confess "\nUnknown file type $c\n\n";
     }->();

    xxx qq(iconv -c -f $fileType -t UTF8 -o "$target" "$source");               # Silently discard any unconvertible characters with -c !
   }

  if (-e $target)                                                               # Preprocess source file
   {my $Text = readFile($target);
    my $text = $Text =~ s(encoding="[^"]+") (encoding="UTF-8")r;                # Write encoding if necessary
    $text    = spelling($text);                                                 # Check/fix spelling
    owf($target, $text) unless $text eq $Text;
    return undef
   }

  $source;
 }

sub convertToUTF8                                                               #r Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.
 {if (unicode)
   {clearFolder(in, maximumNumberOfFilesToClear);

    my @d = searchDirectoryTreesForMatchingFiles(downloads, inputExt);
    my $n = @d;
    lll "Unicode conversion $n ",
        "xml documents to convert from folder: ", downloads;

    my $ps = newProcessStarter(maximumNumberOfProcesses);                       # Process starter
       $ps->processingTitle   = q(Convert documents to uft8);
       $ps->totalToBeStarted  = $n;
       $ps->processingLogFile = fpe(reports, qw(log convertUtf8 txt));

    for my $d(@d)                                                               # Convert projects
     {$ps->start(sub
       {[convertOneFileToUTF8($d)]
       });
     }
    if (my @results = $ps->finish)                                              # Consolidate results
     {my @failed;                                                               # Projects that failed to convert
      for my $r(@results)                                                       # Results
       {my ($source) = @$r;                                                     # Each result
        if ($source)                                                            # A failing file
         {push @failed, $source;                                                # Report failures
         }
       }
      if (@failed)                                                              # Confess to projects that failed to covert
       {my $t = formatTableBasic([[qw(File)], map {[$_]} @failed]);
        eee "The following source files failed to convert:\n", $t;
       }
      else
       {lll "Unicode conversion - converted all $n documents";
        Flip::Flop::unicode();
       }
     }
   }
  else
   {ddd "Unicode conversion not requested";
   }
 }

sub projectCount()                                                              #r Number of projects to process.
 {scalar keys %$projects
 }

sub chooseIDGroup($)                                                            #r Return the id group for a project - files with the same id group share the same set of id attributes.
 {my ($project) = @_;                                                           # Project
  q(all);
 }

sub chooseNameFromString($)                                                     #r Choose a name from a string
 {my ($string) = @_;                                                            # String
  return namefromString($string) if &extendedNames;
  nameFromStringRestrictedToTitle($string);
 }

sub Project($$)                                                                 #r PPPP Project details including at a minimum the name of the project and its source file.
 {my ($name, $source) = @_;                                                     # Project name, source file

  confess "No name for project\n"          unless $name;
  confess "No source for project: $name\n" unless $source;
  if (my $q = $$projects{$name})
   {my $Q = $q->source;
    confess "Duplicate project: $name\n$source\n$Q\n";
   }
  confess "Source file does not exist:\n$source\n" unless -e $source;

  my $p = genHash(q(Project),                                                   # Project definition
    idGroup    => undef,                                                        # Projects awuth the same id group share id attributes.
#   lints      => [],                                                           # Lints associated with this project
    name       => chooseNameFromString($name),                                  # Name of project minus any weird characters
    number     => projectCount + 1,                                             # Number of project
    parseFailed=> undef,                                                        # Parse of source file failed
    source     => $source,                                                      # Input file
   );

  $p->idGroup = chooseIDGroup($p);                                              # Choose the id group for the project
  $projects->{$p->name} = $p;                                                   # Save project definition
 }

sub chooseProjectName($)                                                        #r Create a project name for each file to convert
 {my ($file) = @_;                                                              # Full file name
  chooseNameFromString($file);
#  fn $file                                                                     # Short file name
 }

sub findProjectFromSource($)                                                    #r Locate a project by its source file
 {my ($source) = @_;                                                            # Full file name
  my @p;
  my $file = swapFilePrefix($source, in);
  for my $p(values %$projects)
   {push @p, $p if swapFilePrefix($p->source, in) eq $file;
   }

  return $p[0] if @p == 1;                                                      # Found the matching project
  undef                                                                         # No such unique project
 }

sub selectFileForProcessing($$)                                                 #r Select an input file for processing
 {my ($file, $number) = @_;                                                     # Full file name, project number
  $file
 }

sub loadProjects                                                                #r Locate documents to convert from folder L<in|/in>.
 {if (testMode == 1)                                                            # Local tests
   {for my $file(searchDirectoryTreesForMatchingFiles(testExchangeIn, inputExt))# Potential test documents from Mim
     {my $fileName = fn $file;
      if ($fileName =~ m(b\Z)s)                                                 # Before file
       {my $project = $fileName =~ s(b\Z) ()gsr;
        my $in = fpe(tests, $project, q(dita));                                 # Input test file name
        if (!-e $in)                                                            # Copy to local input tests folder if not already present
         {warn "Added before file for $project as:\n$in\n";
          copyFile($file, $in);
         }
        Project($project, $in);
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
       {warn "Ignored unexpected test file:\n$file\n";
       }
     }
   }
  elsif (testMode == 2)                                                         # Standalone tests
   {if (my @files = searchDirectoryTreesForMatchingFiles(testStandAlone, inputExt))
     {$projects = {};                                                           # Remove any regression tests as they will only confuse the issue
      $standAlone = @files;
      warn "Entered standalone mode with $standAlone files in folder: ",
            testStandAlone;
      for my $file(@files)                                                      # Potential test documents from Phil
       {my $project = fn $file;
        if (!$$projects{$project})
         {Project($project, $file);
          warn "Added test project $project source $file";
         }
       }
     }
   }
  else                                                                          # Production documents
   {my @files = searchDirectoryTreesForMatchingFiles(in, inputExt);
    for my $i(keys @files)
     {my $file = $files[$i];
      next unless selectFileForProcessing($file, $i+1);
      my $name  = chooseProjectName $file;
      Project($name, $file);
     }
   }
 }

sub Project::by($$$)                                                            # Process parse tree with checks to confirm features
 {my ($project, $x, $sub) = @_;                                                 # Project, node, sub
  $x->by($sub);
 }

sub formatXml($)                                                                #r Format xml
 {my ($x) = @_;                                                                 # Parse tree
  $x->prettyStringDitaHeaders;
 }

sub parseCacheFile($)                                                           #P Name of the file in which to cache parse trees
 {my ($project) = @_;                                                           # Project
  my $s = $project->source;
  my $m = fileMd5Sum($s);
  fpe(parseCache, $m, q(data));
 }

sub parseProject($)                                                             #P Parse a project.
 {my ($project) = @_;                                                           # Project
  my $projectName = $project->name;

  my $c = parseCacheFile($project);                                             # Cache parse file name

  if (!&develop and -e $c)                                                      # Reuse cached parse if available on aws
   {return retrieveFile($c);
   }

  my $x = eval {Data::Edit::Xml::new($project->source)};                        # Parse the source
  if ($@)
   {$project->parseFailed = 1;
    eee "Parse of project: $projectName failed\n", $project->source, "\n$@";
    return undef;
   }

  storeFile($c, $x);                                                            # Cache parse

  $x
 }

sub fileFromString($$)                                                          #r Convert a string into a file name in the context of a parse tree to provide an md5 sum and a root tag
 {my ($x, $string) = @_;                                                        # Parse tree, string
  defined($string) or confess "No string to convert to a file name";
  my $s = firstNChars                                                           # String with junk removed
   (chooseNameFromString($string), maximumfileFromStringLength);
  my $f = join q(_),                                                            # File name
    substr($x->tag, 0, 1),                                                      # Topic indicator
    firstNChars
     (chooseNameFromString($string), maximumfileFromStringLength),              # String with junk removed
    fileMd5Sum -p $x;                                                           # Guid from parse content
  $f =~ s(_+) (_)gs;                                                            # Collapse multiple _
  $f
 }

sub lintTopic($$$$)                                                             #r Lint a topic and return the lint details
 {my ($project, $x, $extension, $title) = @_;                                   # Project, parse tree, extension of output file, title of lint

  my $source = -p $x;
  my $file = copyFileMd5NormalizedCreate                                        # Standard name recorded in the lint details
   (&out, $source, $extension, $project->source, titleOnly=>!extendedNames);    # Not entirely satisfactory

  my $l = Data::Edit::Xml::Lint::new;                                           # Linter
     $l->catalog  = &catalog;                                                   # Catalog
     $l->ditaType = -t $x;                                                      # Topic type
     $l->file     = $file;                                                      # Output file
     $l->guid     = $x->id;                                                     # Guid
     $l->inputFile= $project->source;                                           # Add source file information
     $l->labels   = $x;                                                         # Add label information to the output file so when all the files are written they can be retargeted by Data::Edit::Xml::Lint
     $l->project  = $project->idGroup;                                          # Group files into Id scopes
     $l->title    = $title;                                                     # Title
     $l->source = formatXml($x);                                                # Source from parse tree
     $l->lintNOP;                                                               # Lint the results

  $l
 }

sub relintTopic($$;$)                                                           #r Relint a topic and return the lint details
 {my ($file, $x, $newFile) = @_;                                                # File to relint, new parse tree, optional new file else overwrite existing file

  my $l = Data::Edit::Xml::Lint::read($file);                                   # Linter
     $l->file   = $newFile // $file;                                            # New file or existing file
     $l->source = formatXml($x);                                                # Source from parse tree
     $l->lintNOP;                                                               # Lint the results

  $l
 }

sub cleanUpCutOutTopic($$)                                                      #r Clean up a topic once it has been cut out and its output file has been assigned
 {my ($project, $x) = @_;                                                       # Project, parse
 }

sub cleanUpBookMap($$)                                                          #r Clean up a book map once all its topics have been cut out and its output file has been assigned
 {my ($project, $x) = @_;                                                       # Project, parse
 }

#sub gbStandardBookMapFileName($)                                                #r Compute the GB standard file name for a bookmap
# {my ($project) = @_;
#  my $Name = nameFromString(swapFilePrefix($project->source, in));              # Reduce file name for the text component
#  my $name = firstNChars($Name, 128);                                           # Reduce file name length if necessary
#  my $md5  = fileMd5Sum($project->source);                                      # Md5 sum of source for digest
#  fpe(qq(bookmap_${name}_$md5), qq(ditamap))                                    # In summa
# }

sub cutOutTopics1($$)                                                           #r Write the parse tree as a topic
 {my ($project, $x) = @_;                                                       # Project == document to cut, parse tree.
  my $Title = $x->go_title;                                                     # Get the title of the piece
  my $title = $Title ? $Title->stringText : q();                                # Title content

  cleanUpCutOutTopic($project, $x);                                             # Clean up the topic
  lintTopic($project, $x, &outExtTopic, $title);                                # Lint the topic
 }

sub cutOutTopics2($$)                                                           #r Cut out the topics in a document assuming that they are nested within the parse tree.
 {my ($project, $x) = @_;                                                       # Project == document to cut, parse tree.
  my $topicRef;                                                                 # Topic refs tree
  my $topicPath;                                                                # Path of latest topic

  $x->by(sub                                                                    # Cut out each concept
   {my ($o, $p)  = @_;
    if ($o->at(qr(\A(concept|reference|task)\Z)))                               # Cut out topics
     {$topicRef  = $o->wrapWith(qq(topicref));                                  # Wrap inner concepts in topicref
      $o->by(sub                                                                # Nest topic references
       {my ($r)  = @_;
        if ($r->at(qr(\A(appendix|chapter|mapref|topicref)\Z), qr(body\Z|\Asection\Z)))
         {$topicRef->putLastCut($r);                                            # Move topics out of section into containing topic
         }
       });

#     $fileToTopicId{$file} = $o->id;                                           # Record topic id

      $o->cut;                                                                  # Cut out referenced section
      cleanUpCutOutTopic($project, $o);

      my $Title = $o->go_title;                                                 # Get the title of the piece
      my $title = $Title ? $Title->stringText : q();                            # Title content
      my $lint  = lintTopic($project, $o, &outExtTopic, $title);                # Lint topic

      $topicRef->setAttr(navtitle=>$title, href=>fne($lint->file));             # Set navtitle and href for topicref
      $topicPath = $lint->file;
#     $o->by(sub                                                                # Save ids and labels  - almost certainly not needed
#      {my ($q) = @_;
#       if (my $i = $q->id)
#        {for my $label($i, $q->getLabels)                                      # Each label and id
#          {$labelToFile{$label}{$file}++;                                      # Tells us the files containing an id or label
#           $labelToId  {$label}{$file}{$i}++;                                  # Tells us the id of nodes with a file that contain a label or an id
#          }
#        }
#      });
     }
   });

  if ($topicRef)                                                                # Create bookmap from topicRefs
   {#my $Title = swapFilePrefix($project->source, &in);                          # Title for book map
#   my $title = removeDuplicatePrefixes($Title) =~ s(_xml_) (_)gsr;             # Remove duplicate leading file components and well known file extensions
    my $title = fn $project->source;                                            # MARVM57 - should really factor this out

#   my $notices = relFromAbsAgainstAbs($topicPath, fpd(&out, $name));           # Notices file relative to bookmap
    $_->change_chapter for $topicRef->c_topicref;                               # Outermost topic refs become chapters

    my $bookMap = $topicRef->ditaSampleBookMap                                  # Bookmap xml
     (chapters=>$topicRef, title=>$title, notices=>fne($topicPath));

    $bookMap->go_topicref->unwrap;                                              # Unwrap the outermost topicref which is: Notices, to leave the remaining chapters/topicrefs
    cleanUpBookMap  ($project, $bookMap);                                       # Clean up book map
    lintTopic($project, $bookMap, &outExtMap, $title);                          # Lint bookmap
   }
 } # cutOutTopics2

sub cutOutTopics($$)                                                            #r Cut out the topics in a document depending on the cutting out mode
 {my ($project, $x) = @_;                                                       # Project == document to cut, parse tree.
  if (cuttingOut == 1)
   {&cutOutTopics1(@_);
   }
  else
   {&cutOutTopics2(@_);
   }
 }

sub convertDocument($$)                                                         #r Convert one document.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
  lll "convertDocument: ", $project->source;
  $x->wrapDown_conbody_concept;
  $x->createGuidId;
  $x->ditaObviousChanges;
 }

#my @testResultsFiles;                                                           # Files containing test results

sub saveTestResults($$)                                                         #r Convert one document.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
  return unless &develop;
  my $s = -p $x;
  my $before = $project->source;                                                # Before file
  my $after  = $before =~ s(b\.) (a.)sr;                                        # After file
  for my $dir(testResults, testExchangeOut)                                     # Locations where we want to see results
   {next unless $dir;
    my $out = fpe($dir, $project->name, q(dita));                               # Results file
    owf($out, $s);                                                              # Write results
   }
#  my $result
#  push @testResultsFiles, [$result, $out, $after, $before];
 }

#sub reportTestResults                                                           #r Report test results
# {return unless &develop;
#
#  formatTable(\@testResultsFiles, <<END,
#Failure   Reason why the test failed
#Out       Results we  got
#Expected  Results we expected
#In        Input file to test
#END
#  title=>qq(Test summary),
#  head=>qq(Ran NNNN tests on DDDD),
#  file=>fpe(&reports, qw(tests txt)));
# }

sub convertProject($)                                                           #r Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.
 {my ($project) = @_;                                                           # Project == document to convert
  my $projectName = $project->name;
  ddd "Convert ", $project->number, "/", projectCount, " $projectName";         # Title of each conversion

  my $x = parseProject $project;                                                # Reload parse into this process

  my $y = convertDocument($project, $x);                                        # Convert document optionally returning a new parse tree
  $x = $y if ref($y) and ref($y) eq ref($x);                                    # Continue with new parse tree if one provided
  cutOutTopics   ($project, $x);                                                # Cut out topics
# convertXrefsToDitaFormat($project);                                           # Fix xrefs if requested
  saveTestResults($project, $x);                                                # Save any test results
  $project                                                                      # Conversion succeeded for project
 }

sub lintResults                                                                 #r Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.
 {#return if &develop;
  if (lint)                                                                     # Only if lint requested
   {lll "Lint results";
    clearFolder(reports, clearCount);                                           # Clear prior run

    lll "  Start Xref";
    my $xref = xref ? Data::Edit::Xml::Xref::xref                               # Check any cross references
     (inputFolder              => &out,
      reports                  => &reports,
      debugTimes               =>  debugXrefTimes,
      maximumNumberOfProcesses =>  &maximumNumberOfProcesses,
     ) : undef;

    lll "  Start Lint Summary";
    if (my $report = $lintReport = Data::Edit::Xml::Lint::report
     (out, qr(\.(dita|ditamap|xml)\Z)))
     {my $d = dateTimeStamp;
      my $p = pleaseSee($report, $xref);
      my $r = $report->print;
      my $x = sub                                                               # Include xref results
       {return q() unless $xref;
        $xref->statusLine // q();
       }->();

      my $s = <<END;                                                            # rrrr - write summary.txt
Summary of passing and failing projects on $d.\t\tVersion: $VERSION

$r

$x

$p
END
      say STDERR $s;
      writeFile(summaryFile, $s);
      Flip::Flop::lint();

      if (my $fails = $report->failingFiles)                                    # Copy failing files into their own folder  for easy access s
       {if (@$fails)
         {for my $file(@$fails)
           {my $f = $file->[2];                                                 # Failing file name
            my $F = fpf(fails, fne $f);                                         # Shorten file name path so we can find the file easily
            copyFile($f, $F);
           }
         }
       }

      $lintResults = join "\n\n", ($r =~ s(\n.*\Z) ()sr), $x, $p;               # Lint results summary, used to create GitHub notification after upload is complete
     }
    else
     {return lll "No Lint report available";;
     }
   }
  else
   {return ddd "Lint report not requested";
   }
 }

sub copyLogFiles                                                                #r Copy log files to reports/ so they get uploaded too
 {for my $source(errorLogFile, logFile, logStderr)
   {my $target = swapFilePrefix($source, perl, reports);
    copyBinaryFile($source, $target) if -e $source;
   }
 }


sub chunkFile($)                                                                #P Chunk a file name to make it more readable
 {my ($file) = @_;                                                              # File to chunck
  my $f = swapFilePrefix($file, home);                                          # Remove common prefix
  join " <big><b>/</b></big> ", split m(/), $f;                                 # Chunk the file name to make it more readable
 }

sub copyFilesToWeb2                                                             # Copy files into position so that they can be web served
 {return if develop;
  my $client        = client;
  my $reports       = reports;
  my $www           = www;
  my $wwwClient     = publications;
  my $searchFolder  = qq(<input style="display: none;" type="text" name="folder" value="$client">);
  my $date          = dateTimeStamp;
  my $aws           = awsIp;
  my $http          = "http://$aws/";                                           # Http address
  my $mimEdit       = "http://$aws/cgi-bin/fileManager.pl?";                    # Mim editor address

  push my @html, <<END;                                                         # Create html
<head>
  <meta charset="UTF-8">
  <style>

\@font-face
 {font-family: dvs
  src        : url(http://$aws/woff/DejaVuSans.woff)
 }

*
 {font-family: dvs
 }

.even {
  background-color: #ddffdd;
}

.odd {
  background-color: #ffdddd;
}
</style>

</head>
<body>

<body>
<table>

<tr>
<td>Run on: $date

<td><form action="/cgi-bin/fileManager.pl">
Grep: <input type="text" name="grep"> <input type="submit" value="Submit">$searchFolder
</form>

<td><form action="/cgi-bin/fileManager.pl">
Find: <input type="text" name="search" value=""> <input type="submit" value="Submit">$searchFolder
</form>

<td>F1 to switch help on and off

</table>

<div id="help" style="display : none;  position: fixed;
  z-index : 1;
  left    : 0;
  top     : 0;
  width   : 100%;
  height  : 100%;
  overflow: auto;
  background-color: #ffffff;
">
<p>Please send me your proposed help text and I will install it on this page!
</div>

<script>
document.onkeydown = function(e)                                                // Intercept help key and display help
 {if (e.key == 'F1')
   {const h = document.getElementById('help');
    if (h.style.display == "none") h.style.display = "block";
    else                           h.style.display = "none";

    e.preventDefault();
    e.stopImmediatePropagation();
   }
 }
</script>

<table border=1 cellspacing=5 cellpadding=5 style="width: 100%; version: 20190315-111">
END
  xxx qq(sudo rm -r $wwwClient; sudo mkdir -p $wwwClient);

  my @files =                                                                   # File list
    grep {!-d $_}
    grep {!m(/\.)}                                                              # Dot files
    searchDirectoryTreesForMatchingFiles(perl, reports, out);

  my @sorted  = ((grep { m($reports.*summary.txt)} @files),                     # Put reports first so they are to hand
                 (grep { m($reports)} @files),
                 (grep {!m($reports)} @files),
                  searchDirectoryTreesForMatchingFiles(downloads, q(.xml)));    # Xml files

  my %files;                                                                    # {existing file name} = GB standard name - this allows all the web served files to be placed in one folder which makes referencing them much easier
  my %action;                                                                   # {existing file name} = action
  for my $file(@sorted)                                                         # List files
   {my ($action, $ext) = sub
     {my $e = fe $file;
      return (q(skip), q())   unless $e;                                        # Accompanying files
      return (q(edit),  $e)   if $e =~ m((dita|ditamap|xml)\Z)is;               # Edit Dita files
      return qw(pre     html) if $e =~ m(txt\Z)is;                              # Copy txt files with pre wrapped around them
      return (q(copy),  $e)   if $e =~ m((csv|html|svg)\Z)is;                   # Copy csv, html and svg as they are.
      return (q(image), $e);                                                    # Make as an image
     }->();
    next unless $ext;                                                           # Skip companion files
    $files {$file} = setFileExtension(uniqueNameFromFile($file), $ext) if $ext; # Target file name for each file with possible new extension
    $action{$file} = $action;
   }

  my $tempFile = temporaryFile;
  my $count    = 0;
  for my $file(@sorted)                                                         # Edit files to link to other files and copy to web server area so that when reruns are occurring we can still see the content
   {my $relFile = $files{$file};                                                # File relative to index.html
    next unless $relFile;                                                       # Keep the special sort order
    my $target  = fpf($wwwClient, $relFile);                                    # Web file relative to web server home
    my $action  = $action{$file};                                               # Wrap text files with pre to make them into html

    if ($action eq q(say))                                                      # Skip companion files
     {next;
     }
    elsif ($action eq q(pre))                                                   # Wrap text files with pre to make them into html
     {my $s = eval {readFile($file)};                                           # Read source
      if ($@)
       {lll "Cannot read file:\$file\n$@";
        next;
       }
      my $S = $s =~ s(<) (&lt;)gsr =~ s(>) (&gt;)gsr;                           # Replace angle brackets

      my $T = sub                                                               # Convert fully qualified names in tables to anchors
       {my ($s) = @_;
        my @s = split /\x{200b}/, $s;                                           # Split on zero width space
        for my $s(@s)
         {if ($s =~ m(\A(.*?)\s*\Z)s)
           {if (my $f = $files{$1})
             {$s = qq(<a href="$f">$s</a>);
             }
           }
         }
        join q(), @s;
       }->($S);

      my $H = sub                                                               # Expand hrefs
       {my ($text) = @_;
        my @hrefs = split m((?= href="[^"]+)), $text;
        for my $h(@hrefs)
         {if ($h =~ m(\A href="([^"]+)"(.*)\Z)s)
           {my $hrefFile = $1;
            my $rest     = $2;
            my $F = absFromAbsPlusRel($file, $hrefFile);
            if (my $f = $files{$F})
             {$h = qq( <a href="$f">$hrefFile</a> $rest);
             }
           }
         }
        join q(), @hrefs;
       }->($T);

      owf($tempFile, <<END);
<html>
<meta charset="UTF-8">
<pre>
$H
</pre>
</html>
END
      my $cmd = qq(sudo cp "$tempFile" "$target");
      lll $cmd;
      lll qx($cmd);
     }
    else                                                                        # Not a text file so copy directly
     {my $cmd = qq(sudo cp "$file" "$target");
      lll $cmd;
      lll qx($cmd);
     }

    my $publish = sub                                                           # Link to html version of bookmap
     {if ($file =~ m(.ditamap\Z)s)
       {my $p = bookMapPublicationFolder($file);
        my $f = fn $file;
        return qq(<td><a href="$f/index.html"><b>html</b></a>);
       }
      return q(<td>);
     }->();


    if (1)                                                                      # Generate html table row for file
     {++$count;
      my $bg          = qw(even odd)[$count % 2];                               # Row back ground colour
      my $row         = sprintf("%04d", $count);
      my $size        = fileSize($file);                                        # Size of file
      my $original    = copyBinaryFileMd5NormalizedGetCompanionContent($file);  # Original file name
      my $chunkedFile = chunkFile($original // $file);                          # Easier to format file name

      push @html, qq(<tr class="$bg"><td align="right">$row).                   # Row number
                  qq(<td align="right">$size).                                  # File size
                  $publish.                                                     # Html after conversion by Dita OT
                  qq(<td>);

      if ($action    =~ m(image|svg))                                           # Image
       {push @html, qq(<img src="$relFile">$chunkedFile);
       }
      elsif ($action eq q(edit))                                                # Edit
       {push @html, <<END;
<a href="${mimEdit}mim=$relFile&folder=$client">$chunkedFile</a>
END
       }
      else                                                                      # Browse
       {push @html, qq(<a href="$relFile">$chunkedFile</a>);
       }
     }
   }
  push @html, <<END;
</table>
</body>
END

  if (1)                                                                        # Create index
   {my $target = fpe($wwwClient, qw(index html));
    my $source = owf($tempFile, join "\n", @html);
    xxx qq(sudo cp $source $target; sudo chmod -R ugo+r $wwwClient), qr();
   }
  unlink $tempFile;
 }

sub copyFilesToWeb                                                              #r Copy files into position so that they can be web served
 {if (mimajen)
   {lll "Copy file to server for viewing with mimajen";
    copyFilesToWeb2;
   }
  else
   {ddd "Upload to S3 not requested";
   }
 }

sub beforeUploadToS3                                                            #r Copy additional files into position before upload to s3
 {
 }

sub uploadToS3                                                                  #r Copy entire home folder to S3
 {beforeUploadToS3;                                                             # Copy in additional files if required
  my $upload = sub                                                              # Decode upload requirements
   {return undef unless upload;
    return 2 if upload == 2;
    if ($lintReport)
     {return 1 if $lintReport->totalErrors == 0;
     }
    undef
   }->();

  if ($upload)
   {lll "Upload to S3";
    my $p = s3Parms.s3ProfileValue;

    if (s3OutputFolder)                                                         # Upload to output area if requested
     {for my $dir(reports, out, perl)
       {my $target = swapFolderPrefix($dir, home, s3OutputFolder);
        my $c = qq(aws s3 sync $dir $target $p);
        lll $c;
        print STDERR $_ for qx($c);
        lll '';
       }
     }
    else
     {eee q(Upload to S3 requested but S3OutputFolder not set);
     }
    Flip::Flop::uploadToS3();                                                   # Reset upload flip flop
   }
  elsif (upload)
   {lll "Upload to S3 suppressed";
   }
  else
   {ddd "Upload to S3 not requested";
   }
 }

sub uploadToExchange                                                            #r Copy entire home folder to Exchange
 {my $h = home;
  my $p = s3Parms.s3ProfileValue;

  my $exchange = sub                                                            # Decode upload requirements
   {return undef unless exchange;
    return 2 if exchange == 2;
    if ($lintReport)
     {return 1 if $lintReport->totalErrors != 0;
     }
    undef
   }->();

  if (1)                                                                        # Clear exchange folder regardless
   {my $target = s3ExchangeFolder;
    my $q = $p =~ s(--delete) ()gsr;
    xxx qq(aws s3 rm $target $q);
    }

  if ($exchange)
   {lll "Upload to Exchange";

    if (s3ExchangeFolder)
     {my @d;                                                                    # Folders to upload
      push @d, perl    if exchangeItems =~ m(p|\A\Z)i;
      push @d, out     if exchangeItems =~ m(o|\A\Z)i;
      push @d, reports if exchangeItems =~ m(r|\A\Z)i;

      for my $dir(@d)                                                           # Just reports now that we have the reports/fails/ folder.
       {my $target = swapFilePrefix($dir, home, s3ExchangeFolder);
        xxx qq(aws s3 sync $dir $target $p);
       }
     }
    else
     {eee q(Upload to Exchange requested but S3ExchangeFolder not set);
     }
    Flip::Flop::uploadToExchange();                                             # Reset upload flip flop
   }
  elsif (exchange)
   {lll "Upload to S3 Exchange suppressed";
   }
  else
   {ddd "Upload to Exchange not requested";
   }
 } # uploadToExchange

sub bookMapPublicationFolder($)                                                 #r Folder for html obtained by converting bookmap in supplied file
 {my ($bookMap) = @_;                                                           # Bookmap file
  fpd(publications, fn $bookMap)
 }

sub convertBookMapToHtml($)                                                     #r Publish bookmaps on web server
 {my ($bookMap) = @_;
  my $d = ditaBin;
  my $o = fpd(publications, fn $bookMap);

  yyy(<<END);                                                                   # Publish bookmap
sudo mkdir -p $o
sudo rm -r $o
sudo $d -input=$bookMap -format=html5 -output $o
END

  if (1)                                                                        # DitaOT puts our files in the wrong folder!!!  Fix this problem...
   {my @files = searchDirectoryTreesForMatchingFiles($o);
    for my $source(@files)
     {next if -d $source;
      my $target = fpf($o, fne $source);
      if (!-e $target)
       {my $c = qq(sudo mv $source $o);
        lll qx($c);
       }
     }
   }
 }

sub convertBookMapsToHtml                                                       #r Publish bookmaps on web server
 {my $h = home;

  my $ps = newProcessStarter(maximumNumberOfProcesses);                         # Process starter
  for my $bm(searchDirectoryTreesForMatchingFiles(out, q(.ditamap)))            # Convert each bookmap to html
   {$ps->start(sub
     {convertBookMapToHtml($bm);                                                # Convert to html
      1
     });
   }

  $ps->finish;                                                                  # Finish publication
 }

sub convertDitaToHtml                                                           #r Publish bookmaps on web server
 {my $h = home;

  my $publish = sub                                                             # Decode publication requirements
   {return undef unless publish;
    return 2 if publish == 2;
    if ($lintReport)
     {return 1 if $lintReport->totalErrors == 0;
     }
    undef
   }->();

  if ($publish)                                                                 # Publish
   {lll "Convert Dita to HTML and publish";
    convertBookMapsToHtml;
    Flip::Flop::publish();                                                      # Reset publish flip flop
   }
  elsif (publish)
   {lll "Publish suppressed";
   }
  else
   {ddd "Publish not requested";
   }
 } # convertDitaToHtml

my @failedTests;                                                                # Failing tests
my @passedTests;                                                                # Passed tests
my @availableTests;                                                             # Available Tests

sub runTests                                                                    #r Run tests by comparing files in folder L<out|/out> with corresponding files in L<testResults|/testResults>.
 {if (develop)                                                                  # Run tests if developing
   {&checkResults;
    my $F = join " ", @failedTests;
    my $f = @failedTests;
    my $p = @passedTests;
    my $a = @availableTests;
    say STDERR "Failed tests: $F" if @failedTests;
    $p + $f == $a or warn "Passing plus failing tests".
     " not equal to tests available: $p + $f != $a";
    my $X = exchangeHome;
#   my $x = testExchangeOut =~ s($X) ()gsr;
#   say STDERR "Tests: $p+$f == $a pass+fail==avail in $x";
    say STDERR "Tests: $p+$f == $a pass+fail==avail";
   }
 }

sub nwsc($)                                                                     #r Normalize white space and remove comments
 {my ($string) = @_;                                                            # Text to normalize
  $string =~ s(<\?.*?\?>)  ()gs;
  $string =~ s(<!--.*?-->) ()gs;
  $string =~ s(<!DOCTYPE.+?>)  ()gs;
  $string =~ s( (props|id)="[^"]*") ()gs;
  $string =~ s( xtrf="[^"]*") ()gs;                                             # Remove xtrf attribute as it often contains meta data
  nws($string);
 }

sub testResult($$$)                                                             #r Evaluate the results of a test
 {my ($test, $got, $expected) = @_;                                             # Test name, what we got, what we expected result
  my $g = nwsc($got);
  my $e = nwsc($expected);

  if ($e !~ m(\S)s)                                                             # Blank test file
   {confess "Expected results for test $test is all blank";
   }

  if ($g eq $e)                                                                 # Compare got with expected and pass
   {push @passedTests, $test;
    return 1;
   }
  else                                                                          # Not as expected
   {push @failedTests, $test;
    my @g = grep {!/\A\s*(<!|<\?)/} split /\n/, $got;
    my @e = grep {!/\A\s*(<!|<\?)/} split /\n/, $expected;
    shift @g, shift @e while @g and @e and nwsc($g[0]) eq nwsc($e[0]);
    cluck "Got/expected in test $test:\n".
          "Got:\n". $g[0].
          "\nExpected:\n". $e[0]. "\n";
    return 0;
   }
 }

sub checkResults                                                                #r Check test results
 {for my $file(searchDirectoryTreesForMatchingFiles(tests))
   {my $test = fn $file;
    my $expected = fpe(testExpected, $test, q(dita));
    my $got      = fpe(testResults,  $test, q(dita));
    if (-e $got)
     {if (!-e $expected)
       {warn "Updated expected results for $test\n$expected\n";
       }
      else
       {push @availableTests, $test;
        my $g = readFile($got);
        my $e = readFile($expected);
        testResult($test, $g, $e);
       }
     }
    else
     {warn "No test output for $test";
     }
   }
 }

sub reportProjectsThatFailedToParse                                             #r Report projects that failed to parse
 {ddd "Report projects that failed to parse";

  my @parseFailed;                                                              # Projects that failed to parse
  for my $p(sort keys %$projects)                                               # Each project
   {my $project = $projects->{$p};

    if ($project->parseFailed)                                                  # Report projects that failed to parse
     {push @parseFailed, [$project->name, $project->source];
     }
   }
  delete $$projects{$$_[0]} for @parseFailed;                                   # Remove projects that failed to parse

  formatTable(\@parseFailed, [qw(Project Source)],
    head=><<END,
NNNN projects failed to parse on DDDD.

END
    file=>my $parseFailedReportFile = fpe(reports, qw(failedToParse txt)));

  if (my $n = @parseFailed)                                                     # Report parse failures summary
   {eee "$n projects failed to parse, see:\n$parseFailedReportFile"
   }
 }

sub convertSelectedProjects                                                     #r Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.
 {my $ps = newProcessStarter(maximumNumberOfProcesses);                         # Process starter

  my @p = sort keys %$projects;                                                 # Projects
  my $p = @p;
  lll "Convert selected projects of numerosity $p";

  for $project(@p)                                                              # Convert projects
   {$ps->start(sub{&convertProject($projects->{$project})});                    # Convert each project in a separate process
   }

  if (my @results = $ps->finish)                                                # Consolidate results
   {reloadHashes(\@results);                                                    # Recreate attribute methods
    my %convert = %$projects;                                                   # Projects to convert
    for my $project(@results)                                                   # Each result
     {my $projectName = $project->name;                                         # Converted project name
      if (my $p = $$projects{$projectName})                                     # Find project
       {$$projects{$projectName} = $project;                                    # Consolidate information gathered
        delete $convert{$projectName};                                          # Mark project as converted
       }
      else                                                                      # Confess to invalid project
       {confess "Unknown converted project $projectName";
       }
     }

    if (my @f = sort keys %convert)                                             # Confess to projects that failed to convert
     {formatTable(
       [map {$convert{$_}->source} @f],
       [q(), q(Source File)],
        head=>qq(NNNN of $p source files failed to convert on DDDD),
        msg =>1,
        file=>fpe(reports, qw(bad sourceFileConversions txt)));
     }
    else
     {lll "Successfully converted selected projects of numerosity $p";
     }

    reportProjectsThatFailedToParse;                                            # Report projects that failed to parse
   }
  else
   {lll "No projects selected for conversion";
   }
 }

sub beforeConvertProjects                                                       #r Run just before project conversion starts
 {
 }

sub afterConvertProjects                                                        #r Run just after project conversion starts
 {
 }

sub convertProjects                                                             #r Convert the selected documents.
 {if (convert)                                                                  # Convert the documents if requested.
   {lll "Convert documents";
    clearFolder($_, clearCount) for out, process;                               # Clear output folders
    loadProjects;                                                               # Projects to run
    beforeConvertProjects;
    convertSelectedProjects;                                                    # Convert selected projects
    afterConvertProjects;
    Data::Edit::Xml::Lint::fixDitaXrefHrefs                                     # Fix Dita xref href attributes
     (maximumNumberOfLinkProcesses, out, inputExt);
    Flip::Flop::convert();                                                      # Reset conversion flip flop
    confess "Exiting because we are in stand alone mode" if $standAlone;        # Stop testing at this point to look at results
   }
  else
   {ddd "Convert documents not requested";
   }
 }

sub notifyUsers                                                                 #r Notify users of results
 {if (notify)                                                                   # Convert the documents if requested.
   {ddd "Notify users";
    if ($lintReport)
     {my $pass = $lintReport->passRatePercent;
      if (notify == 1 && $lintReport->totalErrors == 0 or notify == 2)          # Notify of results if at 100% or all notifications requested
       {GitHub::Crud::createIssueFromSavedToken
         ("philiprbrenan", "notifications",
          conversion." completed with $pass % success",
          $lintResults. "\n\n".
          q(http://www.ryffine.com));
        Flip::Flop::notify();
       }
     }
   }
  else
   {ddd "Notify not requested";
   }
 }

sub replaceableMethods                                                          #P Replaceable methods
 {qw(Project
  afterConvertProjects
  beforeConvertProjects
  beforeUploadToS3
  bookMapPublicationFolder
  checkResults
  chooseIDGroup
  chooseNameFromString
  chooseProjectName
  cleanUpBookMap
  cleanUpCutOutTopic
  convertBookMapToHtml
  convertBookMapsToHtml
  convertDitaToHtml
  convertDocument
  convertProject
  convertProjects
  convertSelectedProjects
  convertToUTF8
  convertXmlToDita
  copyFilesToWeb
  copyLogFiles
  cutOutTopics
  cutOutTopics1
  cutOutTopics2
  downloadFromS3
  fileFromString
  findProjectFromSource
  formatXml
  lintResults
  lintTopic
  loadProjects
  notifyUsers
  nwsc
  projectCount
  relintTopic
  reportProjectsThatFailedToParse
  runTests
  s3ExchangeFolder
  s3InputFolder
  s3OutputFolder
  saveTestResults
  selectFileForProcessing
  spelling
  testResult
  uploadToExchange
  uploadToS3)
 }

if (0)                                                                          # Format replaceable methods
 {lll "Replaceable methods in $0\n", join "\n",
   (sort keys %{reportReplacableMethods($0)}),
   '';
  exit;
 }


sub attributeMethods                                                            #P Attribute methods
 {qw(catalog clearCount client conversion convert
  cuttingOut debug debugXrefTimes develop
  ditaBin docSet download downloads endTime errorLogFile exchange exchangeHome
  exchangeItems extendedNames
  fails fixFailingFiles flattenImages gathered home imageCache in
  inputExt lint logFile logStderr maximumNumberOfFilesToClear
  maximumNumberOfLinkProcesses
  maximumNumberOfProcesses maximumfileFromStringLength notify out
  outExtMap outExtTopic  parseCache
  perl process publications publish reports runTime s3Bucket s3Exchange
  s3FolderIn s3FolderUp s3Parms s3Profile startTime summaryFile testExchangeIn
  testExchangeOut testExpected testFails testFails2 testMode testResults
  testStandAlone tests unicode upload www)
 }

if (0)                                                                          # Format replaceable attributes
 {lll "Replaceable attributes in $0", join "\n",
    sort keys %{reportAttributes($0)};
  exit;
 }

my $overrideMethods;                                                            # Merge packages only once

sub overrideMethods(;$)                                                         #P Merge packages
 {my ($package) = @_;                                                           # Name of package to be merged defaulting to that of the caller.
  my ($p) = caller();                                                           # Default package if none supplied
  $package //= $p;                                                              # Supply default package if none supplied
  return if $overrideMethods++;                                                 # Merge packages only once
  Data::Table::Text::overrideMethods($package, __PACKAGE__,
    replaceableMethods, attributeMethods);
 }

sub convertXmlToDita                                                            #r Perform all the conversion projects.
 {my ($package) = caller;

  unlink errorLogFile;                                                          # Clear log

  reportSettings($0, fpe(reports, qw(parameterSettings txt)));                  # Report settings

  for my $phase(qw(downloadFromS3 convertToUTF8 convertProjects),               # Execute conversion phases
                qw(lintResults runTests copyLogFiles copyFilesToWeb),
                qw(uploadToS3 uploadToExchange convertDitaToHtml notifyUsers))
   {no strict;
    lll "Phase: ", $phase;
    &{$phase};
   }

  $endTime = time;                                                              # Run time statistics
  $runTime = $endTime - $startTime;
  say STDERR $lintResults if $lintReport;                                       # Avoid line number info on helpful statement
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
$projects $project $lintResults $lintReport %fileToTopicId %labelToFile %labelToId
ddd
eee
lll
overrideMethods
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


Version 20190112.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Convert Xml to the Dita standard.

Convert Xml to the Dita standard.

=head1 Methods

Methods defined in this package.

=head2 lll(@)

Log messages including the project name if available. This method is not merged as we need to retain its prototype.

     Parameter  Description
  1  @m         Messages

=head2 ddd(@)

Log development messages

     Parameter  Description
  1  @m         Messages

=head2 eee(@)

Log error messages

     Parameter  Description
  1  @m         Messages

=head2 downloadFromS3()

Download documents from S3 to the L<downloads|/downloads> folder.


B<Example:>


  sub 𝗱𝗼𝘄𝗻𝗹𝗼𝗮𝗱𝗙𝗿𝗼𝗺𝗦𝟯
   {if (&download)                                                                # Download if requested
     {lll "Download from S3";
      clearFolder(downloads, clearCount);
      makePath(downloads);
      my $s = s3InputFolder;
      my $t = downloads;
      my $p = s3Parms.s3ProfileValue;
      my $c = qq(aws s3 sync $s $t $p);
      lll $c;
      lll $_ for qx($c);
      Flip::Flop::download();
     }
    else
     {ddd "Download from S3 not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub downloadFromS3 {...}

if you wish to override the default processing supplied by this method.



=head2 spelling($)

Fix spelling in source string

     Parameter  Description
  1  $s         Source string

B<Example:>


  sub 𝘀𝗽𝗲𝗹𝗹𝗶𝗻𝗴($)
   {my ($s) = @_;                                                                 # Source string
    $s
   }


You can provide you own implementation of this method in your calling package
via:

  sub spelling {...}

if you wish to override the default processing supplied by this method.



=head2 deTagString($)

Remove any xml tags from a string

     Parameter  Description
  1  $string    String to detag

=head2 convertToUTF8()

Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗧𝗼𝗨𝗧𝗙𝟴
   {if (unicode)
     {clearFolder(in, maximumNumberOfFilesToClear);

      my @d = searchDirectoryTreesForMatchingFiles(downloads, inputExt);
      my $n = @d;
      lll "Unicode conversion $n ",
          "xml documents to convert from folder: ", downloads;

      my $ps = newProcessStarter(maximumNumberOfProcesses);                       # Process starter
         $ps->processingTitle   = q(Convert documents to uft8);
         $ps->totalToBeStarted  = $n;
         $ps->processingLogFile = fpe(reports, qw(log convertUtf8 txt));

      for my $d(@d)                                                               # Convert projects
       {$ps->start(sub
         {[convertOneFileToUTF8($d)]
         });
       }
      if (my @results = $ps->finish)                                              # Consolidate results
       {my @failed;                                                               # Projects that failed to convert
        for my $r(@results)                                                       # Results
         {my ($source) = @$r;                                                     # Each result
          if ($source)                                                            # A failing file
           {push @failed, $source;                                                # Report failures
           }
         }
        if (@failed)                                                              # Confess to projects that failed to covert
         {my $t = formatTableBasic([[qw(File)], map {[$_]} @failed]);
          eee "The following source files failed to convert:
", $t;
         }
        else
         {lll "Unicode conversion - converted all $n documents";
          Flip::Flop::unicode();
         }
       }
     }
    else
     {ddd "Unicode conversion not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertToUTF8 {...}

if you wish to override the default processing supplied by this method.



=head2 projectCount()

Number of projects to process.


B<Example:>


  sub 𝗽𝗿𝗼𝗷𝗲𝗰𝘁𝗖𝗼𝘂𝗻𝘁()
   {scalar keys %$projects
   }


You can provide you own implementation of this method in your calling package
via:

  sub projectCount {...}

if you wish to override the default processing supplied by this method.



=head2 Project()

Project details including at a minimum the name of the project and its source file.


B<Example:>


  sub 𝗣𝗿𝗼𝗷𝗲𝗰𝘁
   {my ($name, $source) = @_;                                                     # 𝗣𝗿𝗼𝗷𝗲𝗰𝘁 name, source file

    confess "No name for project
"          unless $name;
    confess "No source for project: $name
" unless $source;
    if (my $q = $$projects{$name})
     {my $Q = $q->source;
      confess "Duplicate project: $name
$source
$Q
";
     }
    confess "Source file does not exist:
$source
" unless -e $source;

    my $p = genHash(q(𝗣𝗿𝗼𝗷𝗲𝗰𝘁),                                                   # 𝗣𝗿𝗼𝗷𝗲𝗰𝘁 definition
      name       => $name,                                                        # Name of project
      number     => projectCount + 1,                                             # Number of project
      parseFailed=> undef,                                                        # Parse of source file failed
      source     => $source,                                                      # Input file
     );

    $projects->{$p->name} = $p;                                                   # Save project definition
   }


You can provide you own implementation of this method in your calling package
via:

  sub Project {...}

if you wish to override the default processing supplied by this method.



=head2 chooseProjectName($)

Create a project name for each file to convert

     Parameter  Description
  1  $file      Full file name

B<Example:>


  sub 𝗰𝗵𝗼𝗼𝘀𝗲𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝗡𝗮𝗺𝗲($)
   {my ($file) = @_;                                                              # Full file name
    fn $file                                                                      # Short file name
   }


You can provide you own implementation of this method in your calling package
via:

  sub chooseProjectName {...}

if you wish to override the default processing supplied by this method.



=head2 useLastFolderAsProjectName($)

Create a project name from the last folder the file is in.

     Parameter  Description
  1  $file      Full file name

B<Example:>


  sub 𝘂𝘀𝗲𝗟𝗮𝘀𝘁𝗙𝗼𝗹𝗱𝗲𝗿𝗔𝘀𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝗡𝗮𝗺𝗲($)
   {my ($file) = @_;                                                              # Full file name
    my @f = split m(/), fp $file;                                                 # Folder names
    $f[-1]                                                                        # Last folder name
   }


You can provide you own implementation of this method in your calling package
via:

  sub useLastFolderAsProjectName {...}

if you wish to override the default processing supplied by this method.



=head2 useLastFolderPlusFileAsProjectName($)

Create a project name from the last folder the file is in plus the short file name

     Parameter  Description
  1  $file      Full file name

B<Example:>


  sub 𝘂𝘀𝗲𝗟𝗮𝘀𝘁𝗙𝗼𝗹𝗱𝗲𝗿𝗣𝗹𝘂𝘀𝗙𝗶𝗹𝗲𝗔𝘀𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝗡𝗮𝗺𝗲($)
   {my ($file) = @_;                                                              # Full file name
    my @f = split m(/), fp $file;                                                 # Folder names
    $f[-1].q(_).fn($file)                                                         # Last folder name plus short file name
   }


You can provide you own implementation of this method in your calling package
via:

  sub useLastFolderPlusFileAsProjectName {...}

if you wish to override the default processing supplied by this method.



=head2 selectProjectForProcessing($)

Select a project for processing.

     Parameter  Description
  1  $file      Full file name

B<Example:>


  sub 𝘀𝗲𝗹𝗲𝗰𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝗙𝗼𝗿𝗣𝗿𝗼𝗰𝗲𝘀𝘀𝗶𝗻𝗴($)
   {my ($file) = @_;                                                              # Full file name
    $file
   }


You can provide you own implementation of this method in your calling package
via:

  sub selectProjectForProcessing {...}

if you wish to override the default processing supplied by this method.



=head2 loadProjects()

Locate documents to convert from folder L<in|/in>.


B<Example:>


  sub 𝗹𝗼𝗮𝗱𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀
   {my @p = searchDirectoryTreesForMatchingFiles(in,    inputExt);                # Production documents
    my @t = searchDirectoryTreesForMatchingFiles(tests, inputExt);                # Test documents
    if (testDocuments)                                                            # Locate documents to be tested
     {my %t = map {$_=>1} testDocuments;
      for my $file(@p, @t)                                                        # Favor production over test because test is easier to run in bulk
       {my $name = chooseProjectName $file;

        next unless $t{$name};                                                    # Skip unless name matches
        next if $projects->{$name};                                               # Skip if we already have a document to test
        Project($name, $file);
       }
     }
    else                                                                          # Production documents
     {for my $file(develop ? @t : @p)
       {next unless selectProjectForProcessing($file);
        my $name  = chooseProjectName $file;
        Project($name, $file);
       }
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub loadProjects {...}

if you wish to override the default processing supplied by this method.



=head2 Project::by($$$)

Process parse tree with checks to confirm features

     Parameter  Description
  1  $project   Project
  2  $x         Node
  3  $sub       Sub

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



=head2 fileFromTitle($$)

File name from title

     Parameter  Description
  1  $x         Parse tree
  2  $title     Title

B<Example:>


  sub 𝗳𝗶𝗹𝗲𝗙𝗿𝗼𝗺𝗧𝗶𝘁𝗹𝗲($$)
   {my ($x, $title) = @_;                                                         # Parse tree, title
    my $f = join q(_), substr($x->tag, 0, 1),                                     # Topic indicator
                       $title =~ s([^0-9a-z]+) (_)igsr,                           # Title with junk removed
                       md5FromGuid($x->id);                                       # Guid
    $f =~ s(_+) (_)gs;                                                            # Collapse multiple _
    $f
   }


You can provide you own implementation of this method in your calling package
via:

  sub fileFromTitle {...}

if you wish to override the default processing supplied by this method.



=head2 lintTopic($$$$)

Lint a topic within a project

     Parameter  Description
  1  $project   Project
  2  $x         Parse
  3  $file      Output file
  4  $title     Title

B<Example:>


  sub 𝗹𝗶𝗻𝘁𝗧𝗼𝗽𝗶𝗰($$$$)
   {my ($project, $x, $file, $title) = @_;                                        # Project, parse, output file, title

    my $l = Data::Edit::Xml::Lint::new;                                           # Linter
       $l->catalog  = &catalog;                                                   # Catalog
       $l->ditaType = -t $x;                                                      # Topic type
       $l->file     = $file;                                                      # Output file
       $l->guid     = $x->id;                                                     # Guid
       $l->labels   = $x;                                                         # Add label information to the output file so when all the files are written they can be retargeted by Data::Edit::Xml::Lint
       $l->project  = $project->name;                                             # Project name
       $l->source   = $x->prettyStringDitaHeaders;                                # Source
       $l->title    = $title;                                                     # Title
       $l->lintNOP;                                                               # Lint topic

    $l
   }


You can provide you own implementation of this method in your calling package
via:

  sub lintTopic {...}

if you wish to override the default processing supplied by this method.



=head2 cutOutTopics($$)

Cut out the topics in a document.

     Parameter  Description
  1  $project   Project == document to cut
  2  $x         Parse tree.

B<Example:>


  sub 𝗰𝘂𝘁𝗢𝘂𝘁𝗧𝗼𝗽𝗶𝗰𝘀($$)
   {my ($project, $x) = @_;                                                       # Project == document to cut, parse tree.

    my $topicRef;                                                                 # Topic refs tree
    my $topicPath;                                                                # Path of latest topic

    $x->by(sub                                                                    # Cut out each concept
     {my ($o, $p)  = @_;
      if ($o->at_concept)
       {my $title  = $o->go_title->stringContent;                                 # Get the title of the piece
        my $file   = fileFromTitle($o, $title);                                   # File name from title and guid
        $topicPath = fpe(&out, $file, qq(dita));                                  # Full file name for topic file

        $topicRef = $o->wrapWith(qq(topicref));                                   # Wrap inner concepts in topicref
        $topicRef->putLastCut($_) for $o->go_conbody->c(qq(topicref));            # Move topics out of section into containing topic
        $topicRef->setAttr(navtitle=>$title, href=>fpe(q(../), $file, q(dita)));  # Set navtitle and href for topicref

        $o->cut;                                                                  # Cut out referenced section
        lintTopic($project, $o, $topicPath, $title);
       }
     });

    if ($topicRef)                                                                # Create bookmap from topicRefs
     {my $title = q(Bookmap for: ).$project->name;                                # Title for book map
      my $notices = relFromAbsAgainstAbs($topicPath, fpd(&out, $project->name));  # Notices file relative to bookmap

      $_->change_chapter for $topicRef->c_topicref;                               # Outermost topic refs become cheoters

      my $bookMap = $topicRef->ditaSampleBookMap                                  # Bookmap xml
       (chapters=>$topicRef, title=>$title, notices=>$notices);

      $bookMap->go_topicref->unwrap;                                              # Unwrap the outermost topicref which is: Notices, to leave the remaining chapters/topicrefs

      my $file = fileFromTitle($bookMap, $project->name);                         # File name for bookmap
      my $path = fpe(&out, $project->name, $file, qq(ditamap));                   # Full file name for bookmap file
      lintTopic($project, $bookMap, $path, $title);                               # Lint bookmap
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub cutOutTopics {...}

if you wish to override the default processing supplied by this method.



=head2 convertDocument($$)

Convert one document.

     Parameter  Description
  1  $project   Project == document to convert
  2  $x         Parse tree.

B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗗𝗼𝗰𝘂𝗺𝗲𝗻𝘁($$)
   {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertDocument {...}

if you wish to override the default processing supplied by this method.



=head2 convertProject($)

Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.

     Parameter  Description
  1  $project   Project == document to convert

B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁($)
   {my ($project) = @_;                                                           # Project == document to convert
    my $projectName = $project->name;
    ddd "Convert ", $project->number, "/", projectCount, " $projectName";         # Title of each conversion

    my $x = parseProject $project;                                                # Reload parse into this process

    convertDocument($project, $x);                                                # Convert document
    cutOutTopics   ($project, $x);                                                # Cut out topics

    $project                                                                      # Conversion succeeded for project
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertProject {...}

if you wish to override the default processing supplied by this method.



=head2 formatDitaRefs()

Format references per the Dita standard - requires that lint is specified to create the label information in each output file


B<Example:>


  sub 𝗳𝗼𝗿𝗺𝗮𝘁𝗗𝗶𝘁𝗮𝗥𝗲𝗳𝘀
   {if (ditaRefs)
     {lll "Convert references to Dita standard";
     }
    else
     {ddd "Convert references to Dita standard not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub formatDitaRefs {...}

if you wish to override the default processing supplied by this method.



=head2 lintResults()

Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.


B<Example:>


  sub 𝗹𝗶𝗻𝘁𝗥𝗲𝘀𝘂𝗹𝘁𝘀
   {if (lint)                                                                     # Only if lint requested
     {lll "Lint results";
      clearFolder(reports, clearCount);                                           # Clear prior run

      formatDitaRefs;                                                             # Format references in data format

      lll "  Start Xref";
      my $xref = xref ? Data::Edit::Xml::Xref::xref                               # Check any cross references
       (inputFolder=>&out, reports=>&reports,
        maximumNumberOfProcesses=>&maximumNumberOfProcesses) : undef;

      lll "  Start Lint Summary";
      if (my $report = Data::Edit::Xml::Lint::report
       (out, qr(\.(dita|ditamap|xml)\Z)))
       {my $d = dateTimeStamp;
        my $p = pleaseSee;
        my $r = $report->print;
        my $x =                                                                   # Include xref results
        ˢ{return q() unless $xref;
          $xref->statusLine // q();
         };

        my $s = <<END;                                                            # rrrr - write summary.txt
  Summary of passing and failing projects on $d.\t\tVersion: $VERSION

  $r

  $x

  $p
  END
        say STDERR $s;
        writeFile(summaryFile, $s);
        Flip::Flop::lint();

        if (my $fails = $report->failingFiles)                                    # Copy failing files into their own folder  for easy access s
         {if (@$fails)
           {for my $file(@$fails)
             {my $f = $file->[2];                                                 # Failing file name
              my $F = fpf(fails, fne $f);                                         # Shorten file name path so we can find the file easily
              copyFile($f, $F);
             }
           }
         }

        $𝗹𝗶𝗻𝘁𝗥𝗲𝘀𝘂𝗹𝘁𝘀 = join "
", $VERSION,                                       # Run results to create issue after upload is complete
         ($r =~ s(
.*\Z) ()sr), $x, $p;
       }
      else
       {return lll "No Lint report available";;
       }
     }
    else
     {return ddd "Lint report not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub lintResults {...}

if you wish to override the default processing supplied by this method.



=head2 copyLogFiles()

Copy log files to reports/ so they get uploaded too


B<Example:>


  sub 𝗰𝗼𝗽𝘆𝗟𝗼𝗴𝗙𝗶𝗹𝗲𝘀
   {for my $source(errorLogFile, logFile, logStderr)
     {my $target = swapFilePrefix($source, perl, reports);
      copyBinaryFile($source, $target) if -e $source;
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub copyLogFiles {...}

if you wish to override the default processing supplied by this method.



=head2 uploadToS3()

Copy entire home folder to S3


B<Example:>


  sub 𝘂𝗽𝗹𝗼𝗮𝗱𝗧𝗼𝗦𝟯
   {if (upload)
     {lll "Upload to S3";
      my $p = s3Parms.s3ProfileValue;

      if (s3OutputFolder)                                                         # Upload to output area if requested
       {for my $dir(reports, out, perl)
         {my $target = swapFilePrefix($dir, home, s3OutputFolder);
          my $c = qq(aws s3 sync $dir $target $p);
          lll $c;
          print STDERR $_ for qx($c);
          lll '';
         }
       }
      else
       {eee q(Upload to S3 requested but S3OutputFolder not set);
       }
      Flip::Flop::𝘂𝗽𝗹𝗼𝗮𝗱𝗧𝗼𝗦𝟯();                                                   # Reset upload flip flop
     }
    else
     {ddd "Upload to S3 not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub uploadToS3 {...}

if you wish to override the default processing supplied by this method.



=head2 uploadToExchange()

Copy entire home folder to Exchange


B<Example:>


  sub 𝘂𝗽𝗹𝗼𝗮𝗱𝗧𝗼𝗘𝘅𝗰𝗵𝗮𝗻𝗴𝗲
   {if (exchange)
     {lll "Upload to Exchange";
      my $h = home;
      my $p = s3Parms.s3ProfileValue;

      if (s3ExchangeFolder)
       {for my $dir(reports)                                                      # Just reports now that we have the reports/fails/ folder.
         {my $target = swapFilePrefix($dir, home, s3ExchangeFolder);
          my $c = qq(aws s3 sync $dir $target $p);
          lll $c;
          print STDERR $_ for qx($c);
          lll '';
         }
       }
      else
       {eee q(Upload to Exchange requested but S3ExchangeFolder not set);
       }
      Flip::Flop::𝘂𝗽𝗹𝗼𝗮𝗱𝗧𝗼𝗘𝘅𝗰𝗵𝗮𝗻𝗴𝗲();                                             # Reset upload flip flop
     }
    else
     {ddd "Upload to Exchange not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub uploadToExchange {...}

if you wish to override the default processing supplied by this method.



=head2 runTests()

Run tests by comparing files in folder L<out|/out> with corresponding files in L<testResults|/testResults>.


B<Example:>


  sub 𝗿𝘂𝗻𝗧𝗲𝘀𝘁𝘀
   {if (develop)                                                                  # Run tests if developing
     {&checkResults;
      my $F = join " ", @failedTests;
      my $f = @failedTests;
      my $p = @passedTests;
      my $a = @availableTests;
      say STDERR "Failed tests: $F" if @failedTests;
      $p + $f == $a or warn "Passing plus failing tests".
       " not equal to tests available: $p + $f != $a";
      say STDERR "Tests: $p+$f == $a pass+fail==avail";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub runTests {...}

if you wish to override the default processing supplied by this method.



=head2 nwsc($)

Normalize white space and remove comments

     Parameter  Description
  1  $string    Text to normalize

B<Example:>


  sub 𝗻𝘄𝘀𝗰($)
   {my ($string) = @_;                                                            # Text to normalize
    $string =~ s(<\?.*?\?>)  ()gs;
    $string =~ s(<!--.*?-->) ()gs;
    $string =~ s(<!DOCTYPE.+?>)  ()gs;
    $string =~ s( (props|id)="[^"]*") ()gs;
    nws($string);
   }


You can provide you own implementation of this method in your calling package
via:

  sub nwsc {...}

if you wish to override the default processing supplied by this method.



=head2 testResult($$$)

Evaluate the results of a test

     Parameter  Description
  1  $file      File
  2  $got       What we got
  3  $expected  What we expected result

B<Example:>


  sub 𝘁𝗲𝘀𝘁𝗥𝗲𝘀𝘂𝗹𝘁($$$)
   {my ($file, $got, $expected) = @_;                                             # File, what we got, what we expected result
    my $f = fpe(tests, $file, q(dita));                                           # Actual result
    my $g = nwsc($got);
    my $e = nwsc($expected);

    if ($e !~ m(\S)s)                                                             # Blank test file
     {confess "Test $file is all blank";
     }

    if ($g eq $e)                                                                 # Compare got with expected and pass
     {push @passedTests, $file;
      return 1;
     }
    else                                                                          # Not as expected
     {push @failedTests, $file;
      my @g = grep {!/\A\s*(<!|<\?)/} split /
/, readFile($f);
      my @e = grep {!/\A\s*(<!|<\?)/} split /
/, $expected;
      shift @g, shift @e while @g and @e and nwsc($g[0]) eq nwsc($e[0]);
      cluck "Got/expected in test $file:
".
            "Got:
". $g[0].
            "
Expected:
". $e[0]. "
";
      return 0;
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub testResult {...}

if you wish to override the default processing supplied by this method.



=head2 checkResults()

Send results to S3 from folder L<out|/out>.


B<Example:>


  sub 𝗰𝗵𝗲𝗰𝗸𝗥𝗲𝘀𝘂𝗹𝘁𝘀
   {for my $expected(searchDirectoryTreesForMatchingFiles(testResults))
     {my $got  = swapFilePrefix($expected, testResults, out);
      my $test = fn $expected;
      push @availableTests, $test;
      if (-e $got)
       {testResult($test, readFile($got), readFile($expected));
       }
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub checkResults {...}

if you wish to override the default processing supplied by this method.



=head2 reportProjectsThatFailedToParse()

Report projects that failed to parse


B<Example:>


  sub 𝗿𝗲𝗽𝗼𝗿𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀𝗧𝗵𝗮𝘁𝗙𝗮𝗶𝗹𝗲𝗱𝗧𝗼𝗣𝗮𝗿𝘀𝗲
   {ddd "Report projects that failed to parse";

    my @parseFailed;                                                              # Projects that failed to parse
    for my $p(sort keys %$projects)                                               # Each project
     {my $project = $projects->{$p};

      if ($project->parseFailed)                                                  # Report projects that failed to parse
       {push @parseFailed, [$project->name, $project->source];
       }
     }
    delete $$projects{$$_[0]} for @parseFailed;                                   # Remove projects that failed to parse

    formatTable(\@parseFailed, [qw(Project Source)],
      head=><<END,
  NNNN projects failed to parse on DDDD.

  END
      file=>my $parseFailedReportFile = fpe(reports, qw(failedToParse txt)));

    if (my $n = @parseFailed)                                                     # Report parse failures summary
     {eee "$n projects failed to parse, see:
$parseFailedReportFile"
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub reportProjectsThatFailedToParse {...}

if you wish to override the default processing supplied by this method.



=head2 convertSelectedProjects()

Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗦𝗲𝗹𝗲𝗰𝘁𝗲𝗱𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀
   {my $ps = newProcessStarter(maximumNumberOfProcesses);                         # Process starter

    my @p = sort keys %$projects;                                                 # Projects
    my $p = @p;
    lll "Convert selected projects of numerosity $p";

    for $project(@p)                                                              # Convert projects
     {$ps->start(sub{&convertProject($projects->{$project})});                    # Convert each project in a separate process
     }

    if (my @results = $ps->finish)                                                # Consolidate results
     {reloadHashes(\@results);                                                    # Recreate attribute methods
      my %convert = %$projects;                                                   # Projects to convert
      for my $project(@results)                                                   # Each result
       {my $projectName = $project->name;                                         # Converted project name
        if (my $p = $$projects{$projectName})                                     # Find project
         {$$projects{$projectName} = $project;                                    # Consolidate information gathered
          delete $convert{$projectName};                                          # Mark project as converted
         }
        else                                                                      # Confess to invalid project
         {confess "Unknown converted project $projectName";
         }
       }

      if (my @f = sort keys %convert)                                             # Confess to projects that failed to convert
       {formatTable(
         [map {$convert{$_}->source} @f],
         [q(), q(Source File)],
          head=>qq(NNNN of $p source files failed to convert on DDDD),
          msg =>1,
          file=>fpe(reports, qw(bad sourceFileConversions txt)));
       }

      reportProjectsThatFailedToParse;                                            # Report projects that failed to parse
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertSelectedProjects {...}

if you wish to override the default processing supplied by this method.



=head2 convertProjects()

Convert the selected documents.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀
   {if (convert)                                                                  # Convert the documents if requested.
     {lll "Convert documents";
      clearFolder($_, clearCount) for out, process;                               # Clear output folders
      loadProjects;                                                               # Projects to run
      convertSelectedProjects                                                     # Convert selected projects
      Flip::Flop::convert();                                                      # Reset conversion flip flop
     }
    else
     {ddd "Convert documents not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertProjects {...}

if you wish to override the default processing supplied by this method.



=head2 convertXmlToDita()

Perform all the conversion projects.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗫𝗺𝗹𝗧𝗼𝗗𝗶𝘁𝗮
   {my ($package) = caller;

    unlink errorLogFile;

    for my $phase(qw(downloadFromS3 convertToUTF8 convertProjects
                     lintResults runTests copyLogFiles
                     uploadToS3 uploadToExchange))
     {no strict;
      ddd "Phase: ", $phase;
      &{$phase};
     }

    $endTime = time;                                                              # Run time statistics
    $runTime = $endTime - $startTime;
    say STDERR $lintResults;                                                      # Avoid line number info on helpful statement
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertXmlToDita {...}

if you wish to override the default processing supplied by this method.




=head1 Hash Definitions




=head2 Project Definition


Project definition


B<name> - Name of project

B<number> - Number of project

B<parseFailed> - Parse of source file failed

B<source> - Input file



=head2 𝗣𝗿𝗼𝗷𝗲𝗰𝘁 Definition


𝗣𝗿𝗼𝗷𝗲𝗰𝘁 definition


B<name> - Name of project

B<number> - Number of project

B<parseFailed> - Parse of source file failed

B<source> - Input file



=head1 Attributes


The following is a list of all the attributes in this package.  A method coded
with the same name in your package will over ride the method of the same name
in this package and thus provide your value for the attribute in place of the
default value supplied for this attribute by this package.

=head2 Replaceable Attribute List


catalog clearCount convert debug develop ditaRefs download downloads endTime errorLogFile exchange fails gathered home images in inputExt lint logFile logStderr maximumFileFromTitleLength maximumNumberOfFilesToClear maximumNumberOfLinkProcesses maximumNumberOfProcesses out outImages parseCache perl process reports runTime s3Bucket s3Exchange s3FolderIn s3FolderUp s3Parms s3Profile startTime summaryFile testDocuments testResults tests unicode upload xref


=head2 catalog

Dita catalog to be used for linting.


=head2 clearCount

Limit on number of files to clear from each output folder.


=head2 convert

Convert documents to dita if true.


=head2 debug

Debug if true


=head2 develop

Production run if this file folder is detected otherwise development.


=head2 ditaRefs

Format hrefs in the Dita format


=head2 download

Download from S3 if true.


=head2 downloads

Downloads folder.


=head2 endTime

End time of run in seconds since the epoch.


=head2 errorLogFile

Error log file


=head2 exchange

Upload to S3 Exchange if true.


=head2 fails

Copies of failing documents in a seperate folder to speed up downloading


=head2 gathered

Folder containing saved parse trees after initial parse and information gathering.


=head2 home

Home folder containing all the other folders


=head2 images

Images folder


=head2 in

Input documents folder.


=head2 inputExt

Extension of input files.


=head2 lint

Lint output xml


=head2 logFile

Log file.


=head2 logStderr

Log file containing output to STDERR.


=head2 maximumFileFromTitleLength

Maximum amount of title to use in constructing output file names.


=head2 maximumNumberOfFilesToClear

Maximum number of files to clear


=head2 maximumNumberOfLinkProcesses

Maximum number of Lint::relint link processes - as they come later in the run fork burns more memory and we often run out of memory here


=head2 maximumNumberOfProcesses

Maximum number of conversion processes to run in parallel.


=head2 out

Converted documents output folder.


=head2 outImages

Output images


=head2 parseCache

Cached parse trees


=head2 perl

Perl folder


=head2 process

Process data folder used to communicate results between processes.


=head2 reports

Reports folder.


=head2 runTime

Elapsed run time in seconds.


=head2 s3Bucket

Bucket on S3 holding documents to convert and the converted results.


=head2 s3Exchange

Results area in exchange folder if results desired in exchange folder as well


=head2 s3FolderIn

Folder on S3 containing original documents.


=head2 s3FolderUp

Folder on S3 containing results of conversion.


=head2 s3Parms

Additional S3 parameters for uploads and downloads.


=head2 s3Profile

Aws cli profile keyword value if any


=head2 startTime

Start time of run in seconds since the epoch.


=head2 summaryFile

Summary report file.


=head2 testDocuments

List of production documents to test in development or () for normal testing locally or normal production if on Aws.


=head2 testResults

Folder containing test results expected.


=head2 tests

Folder containing test files.


=head2 unicode

Convert to utf8 if true.


=head2 upload

Upload to S3 if true.


=head2 xref

Xref output xml




=head1 Optional Replace Methods

The following is a list of all the optionally replaceable methods in this
package.  A method coded with the same name in your package will over ride the
method of the same name in this package providing your preferred processing for
the replaced method in place of the default processing supplied by this
package. If you do not supply such an over riding method, the existing method
in this package will be used instead.

=head2 Replaceable Method List


Project checkResults chooseProjectName convertDocument convertProject convertProjects convertSelectedProjects convertToUTF8 convertXmlToDita copyLogFiles cutOutTopics downloadFromS3 fileFromTitle formatDitaRefs formatXml lintResults lintTopic loadProjects nwsc projectCount reportProjectsThatFailedToParse runTests selectProjectForProcessing spelling testResult uploadToExchange uploadToS3 useLastFolderAsProjectName useLastFolderPlusFileAsProjectName




=head1 Private Methods

=head2 getHome()

Compute home directory once


=head2 s3ProfileValue()

S3 profile keyword


=head2 s3InputFolder()

S3 input folder


=head2 s3OutputFolder()

S3 output folder


=head2 s3ExchangeFolder()

S3 output folder


=head2 pleaseSee()

AWS command to see results


=head2 convertOneFileToUTF8()

Convert one file to utf8 and return undef if successful else the name of the document in error


=head2 parseCacheFile($)

Name of the file in which to cache parse trees

     Parameter  Description
  1  $project   Project

=head2 parseProject($)

Parse a project.

     Parameter  Description
  1  $project   Project

=head2 replaceableMethods()

Replaceable methods


=head2 attributeMethods()

Attribute methods


=head2 overrideMethods($)

Merge packages

     Parameter  Description
  1  $package   Name of package to be merged defaulting to that of the caller.


=head1 Index


1 L<attributeMethods|/attributeMethods> - Attribute methods

2 L<checkResults|/checkResults> - Send results to S3 from folder L<out|/out>.

3 L<chooseProjectName|/chooseProjectName> - Create a project name for each file to convert

4 L<convertDocument|/convertDocument> - Convert one document.

5 L<convertOneFileToUTF8|/convertOneFileToUTF8> - Convert one file to utf8 and return undef if successful else the name of the document in error

6 L<convertProject|/convertProject> - Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.

7 L<convertProjects|/convertProjects> - Convert the selected documents.

8 L<convertSelectedProjects|/convertSelectedProjects> - Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.

9 L<convertToUTF8|/convertToUTF8> - Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.

10 L<convertXmlToDita|/convertXmlToDita> - Perform all the conversion projects.

11 L<copyLogFiles|/copyLogFiles> - Copy log files to reports/ so they get uploaded too

12 L<cutOutTopics|/cutOutTopics> - Cut out the topics in a document.

13 L<ddd|/ddd> - Log development messages

14 L<deTagString|/deTagString> - Remove any xml tags from a string

15 L<downloadFromS3|/downloadFromS3> - Download documents from S3 to the L<downloads|/downloads> folder.

16 L<eee|/eee> - Log error messages

17 L<fileFromTitle|/fileFromTitle> - File name from title

18 L<formatDitaRefs|/formatDitaRefs> - Format references per the Dita standard - requires that lint is specified to create the label information in each output file

19 L<formatXml|/formatXml> - Format xml

20 L<getHome|/getHome> - Compute home directory once

21 L<lintResults|/lintResults> - Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.

22 L<lintTopic|/lintTopic> - Lint a topic within a project

23 L<lll|/lll> - Log messages including the project name if available.

24 L<loadProjects|/loadProjects> - Locate documents to convert from folder L<in|/in>.

25 L<nwsc|/nwsc> - Normalize white space and remove comments

26 L<overrideMethods|/overrideMethods> - Merge packages

27 L<parseCacheFile|/parseCacheFile> - Name of the file in which to cache parse trees

28 L<parseProject|/parseProject> - Parse a project.

29 L<pleaseSee|/pleaseSee> - AWS command to see results

30 L<Project|/Project> - Project details including at a minimum the name of the project and its source file.

31 L<Project::by|/Project::by> - Process parse tree with checks to confirm features

32 L<projectCount|/projectCount> - Number of projects to process.

33 L<replaceableMethods|/replaceableMethods> - Replaceable methods

34 L<reportProjectsThatFailedToParse|/reportProjectsThatFailedToParse> - Report projects that failed to parse

35 L<runTests|/runTests> - Run tests by comparing files in folder L<out|/out> with corresponding files in L<testResults|/testResults>.

36 L<s3ExchangeFolder|/s3ExchangeFolder> - S3 output folder

37 L<s3InputFolder|/s3InputFolder> - S3 input folder

38 L<s3OutputFolder|/s3OutputFolder> - S3 output folder

39 L<s3ProfileValue|/s3ProfileValue> - S3 profile keyword

40 L<selectProjectForProcessing|/selectProjectForProcessing> - Select a project for processing.

41 L<spelling|/spelling> - Fix spelling in source string

42 L<testResult|/testResult> - Evaluate the results of a test

43 L<uploadToExchange|/uploadToExchange> - Copy entire home folder to Exchange

44 L<uploadToS3|/uploadToS3> - Copy entire home folder to S3

45 L<useLastFolderAsProjectName|/useLastFolderAsProjectName> - Create a project name from the last folder the file is in.

46 L<useLastFolderPlusFileAsProjectName|/useLastFolderPlusFileAsProjectName> - Create a project name from the last folder the file is in plus the short file name

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Data::Edit::Xml::To::DitaVa

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
Test::More->builder->output("/dev/null")                                        # Reduce number of confirmation messages during testing
  if ((caller(1))[0]//'Data::Table::Text') eq "Data::Table::Text";

use Test::More tests=>1;

ok 1;
