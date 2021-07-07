#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/ -I/home/phil/perl/cpan/DataEditXmlXref/lib
#-------------------------------------------------------------------------------
# Data::Edit::Xml::To::Dita - Convert multiple Xml documents in parallel to Dita
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2018
#-------------------------------------------------------------------------------
# podDocumentation
# Add docSet

package Data::Edit::Xml::To::Dita;
our $VERSION = 20190112;
use v5.20;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump);
use Data::Edit::Xml;
use Data::Edit::Xml::Lint;
use Data::Edit::Xml::Xref;
use Data::Table::Text qw(:all !lll);
use Flip::Flop;
use Scalar::Util qw(blessed);
use utf8;

#D1 Convert Xml to the Dita standard.                                           # Convert Xml to the Dita standard.

sub testDocuments  {qw()}                                                       # List of production documents to test in development or () for normal testing locally or normal production if on Aws.

sub upload         {!&develop}                                                  # Upload to S3 if true.
sub exchange       {!&develop}                                                  # Upload to S3 Exchange if true.
sub exchangeItems  {q(r)}                                                       # The items to be uploaded to the exchange folder: p - perl, o - out, r - reports
sub download       {!&develop}                                                  # Download from S3 if true.
sub unicode        {1}                                                          # Convert to utf8 if true.
sub convert        {1}                                                          # Convert documents to dita if true.
sub lint           {1}                                                          # Lint output xml
sub lineNumbers    {1}                                                          # Whether to include line number information in parsed xml
sub xref           {1}                                                          # Xref output xml
sub ditaRefs       {1}                                                          # Format hrefs in the Dita format
sub debug          {0}                                                          # Debug if true
sub debugXrefTimes {0}                                                          # Debug Xref phase times if true
sub addNavTitles   {1}                                                          # Add nav titles to topicrefs if true
sub writeResidue   {1}                                                          # Write the remainder after sections have been cut out
sub keepFileNames  {0}                                                          # Keep existing file names if set

sub catalog        {q(/home/phil/r/dita/dita-ot-3.1/catalog-dita.xml)}          # Dita catalog to be used for linting.
sub clearCount     {&develop ? 1e3 : 1e6}                                       # Limit on number of files to clear from each output folder.
sub develop        {-e q(/home/ubuntu/) ? 0 : 1}                                # Production run if this file folder is detected otherwise development.
sub downloads      {fpd(&home,    qw(download))}                                # Downloads folder.
sub errorLogFile   {fpe(&perl,    qw(eee txt))}                                 # Error log file
sub fails          {fpd(&reports, qw(fails))}                                   # Copies of failing documents in a seperate folder to speed up downloading
sub gathered       {fpd(&home,    qw(gathered))}                                # Folder containing saved parse trees after initial parse and information gathering.
sub home           {&getHome}                                                   # Home folder containing all the other folders
sub images         {fpd(home,     qw(images))}                                  # Images folder
sub in             {fpd(&home,    qw(in))}                                      # Input documents folder.
sub inputExt       {qw(.xml .dita .ditamap)}                                    # Extension of input files.
sub logFile        {fpe(&perl,    qw(lll txt))}                                 # Log file.
sub logStderr      {fpe(&perl,    qw(zzz txt))}                                 # Log file containing output to STDERR.
sub out            {fpd(&home,    qw(out))}                                     # Converted documents output folder.
sub out2           {fpd(&home,    qw(out2))}                                    # Renamed/flattened topics as produced by Xref via the flattenedFolder option
sub outImages      {fpd(&home,    qw(out images))}                              # Output images
#sub parseCache     {fpd(&home,    qw(parseCache))}                             # Cached parse trees - less effective than expected because they slow aws spot start up and occupy a lot of space.
sub perl           {fpd(&home,    qw(perl))}                                    # Perl folder
sub process        {fpd(&home,    qw(process))}                                 # Process data folder used to communicate results between processes.
sub reports        {fpd(&home,    qw(reports))}                                 # Reports folder.
sub s3Bucket       {q(s3Bucket)}                                                # Bucket on S3 holding documents to convert and the converted results.
sub s3FolderIn     {q(originals)}                                               # Folder on S3 containing original documents.
sub s3FolderUp     {q(results)}                                                 # Folder on S3 containing results of conversion.
sub s3Exchange     {undef}                                                      # Results area in exchange folder if results desired in exchange folder as well
sub s3Profile      {undef}                                                      # Aws cli profile keyword value if any
sub s3Parms        {q(--quiet --delete)}                                        # Additional S3 parameters for uploads and downloads.
sub summaryFile    {fpe(reports, qw(summary txt))}                              # Summary report file.
sub testResults    {fpd(&home, qw(testResults))}                                # Folder containing test results expected.
sub tests          {in}                                                         # Folder containing test files.

my $startTime      = time;                                                      # Start time.
my $endTime;                                                                    # End time value.
my $runTime;                                                                    # Run time value.
sub startTime      {$startTime}                                                 # Start time of run in seconds since the epoch.
sub endTime        {$endTime}                                                   # End time of run in seconds since the epoch.
sub runTime        {$runTime}                                                   # Elapsed run time in seconds.

sub maximumNumberOfProcesses    {develop ?  4 : 256}                            # Maximum number of conversion processes to run in parallel.
sub maximumNumberOfLinkProcesses{develop ?  2 :  16}                            # Maximum number of Lint::relint link processes - as they come later in the run fork burns more memory and we often run out of memory here

sub maximumFileFromTitleLength {50}                                             # Maximum amount of title to use in constructing output file names.
sub maximumNumberOfFilesToClear{develop ? 1e3 : 1e6}                            # Maximum number of files to clear

our $projects    = {};                                                          # Projects == documents to convert
our $project;                                                                   # Visible, thus loggable
our $lintResults = q(No lint results yet!);                                     # Lint results

my $home;
sub getHome                                                                     #P Compute home directory once
 {return $home if $home;
  my $c = currentDirectory;
  return $home = $c if $c =~ m(\A/home/phil/perl/cpan/)s;
  $home = fpd(Data::Table::Text::sumAbsAndRel($c, "../"));
 }

sub s3ProfileValue                                                              #P S3 profile keyword
 {return '' unless my $p = s3Profile;
  qq( --profile $p)
 }

#D2 Methods                                                                     # Methods defined in this package.

sub lll(@)                                                                      #r Log messages including the project name if available. This method is not merged as we need to retain its prototype.
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

sub Project::lll(@)                                                             #r Log messages including the project name if available
 {goto &lll;
 }

sub ddd(@)                                                                      # Log development messages
 {goto &lll if debug;
 }

sub eee(@)                                                                      #r Log error messages
 {my (@m) = @_;                                                                 # Messages
  my $m = join '', timeStamp, " $$ Error: ", @_;                                # Time stamp each message
  if ($project)
   {$m .= " in project $project";
   }
  my ($p, $f, $l) = caller();
  $m .= " at $f line $l\n";

  appendFile(errorLogFile, ($m =~ s(\s+) ( )gsr).qq(\n));
  say STDERR $m;
 }

sub s3InputFolder                                                               #P S3 input folder
 {q(s3://).fpd(s3Bucket, s3FolderIn);
 }

sub s3OutputFolder                                                              #P S3 output folder
 {q(s3://).fpd(s3Bucket, s3FolderUp);
 }

sub s3ExchangeFolder                                                            #P S3 output folder
 {q(s3://).fpd(s3Exchange);
 }

sub pleaseSee                                                                   #P AWS command to see results
 {my $p = qq(Please see:);
  my $s = &upload   && &s3OutputFolder ? qq(aws s3 sync ).&s3OutputFolder : q();
  my $x = &exchange && &s3Exchange     ? &s3Exchange                      : q();
  return qq($p $s or $x) if &upload and &exchange;
  return qq($p $s)       if &upload;
  return qq($p $x)       if             &exchange;
  q()
 }

sub downloadFromS3                                                              #r Download documents from S3 to the L<downloads|/downloads> folder.
 {if (&download)                                                                 # Download if requested
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
   {lll "Download from S3 not requested";
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
# lll "Convert one file: $source";
  my $d = downloads;
  my $i = in;
  my $target = $source =~ s(\A$d) ($i)r;                                        # Target
  makePath($target);

  if (isFileUtf8($source))                                                      # Copy file directly if already utf8
   {copyFile($source, $target);
   }
  else                                                                          # Convert file to utf8
   {my $fileType = sub
     {my $c = qx(file "$source");
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
   {lll "Unicode conversion not requested";
   }
 }

sub projectCount()                                                              #r Number of projects to process.
 {scalar keys %$projects
 }

sub Project                                                                     #r Project details including at a minimum the name of the project and its source file.
 {my ($name, $source) = @_;                                                     # Project name, source file

  confess "No name for project\n"          unless $name;
  confess "No source for project: $name\n" unless $source;
  if (my $q = $$projects{$name})
   {my $Q = $q->source;
    confess "Duplicate project: $name\n$source\n$Q\n";
   }
  confess "Source file does not exist:\n$source\n" unless -e $source;

  my $p = genHash(q(Project),                                                   # Project definition
    author     => undef,                                                        # Author fo this document
    id         => undef,                                                        # Id attribute value from outermost tag
    isMap      => undef,                                                        # Map
    name       => $name,                                                        # Name of project
    number     => projectCount + 1,                                             # Number of project
    outputFiles=> undef,                                                        # {proposed output file name} = globally unique output file name
    outputFile => $name,                                                        # Output file is the name fo the project by default
    parseFailed=> undef,                                                        # Parse of source file failed
    source     => $source,                                                      # Input file
    tag        => undef,                                                        # Name of outermost tag
    title      => undef,                                                        # Title for project
    topicId    => undef,                                                        # Topic id for project - collected during gather
   );

  $projects->{$p->name} = $p;                                                   # Save project definition
 }

sub chooseProjectName($)                                                        #r Create a project name for each file to convert
 {my ($file) = @_;                                                              # Full file name
  fn $file                                                                      # Short file name
 }

sub useLastFolderAsProjectName($)                                               #r Create a project name from the last folder the file is in.
 {my ($file) = @_;                                                              # Full file name
  my @f = split m(/), fp $file;                                                 # Folder names
  $f[-1]                                                                        # Last folder name
 }

sub useLastFolderPlusFileAsProjectName($)                                       #r Create a project name from the last folder the file is in plus the short file name
 {my ($file) = @_;                                                              # Full file name
  my @f = split m(/), fp $file;                                                 # Folder names
  $f[-1].q(_).fn($file)                                                         # Last folder name plus short file name
 }

sub selectProjectForProcessing($)                                               #r Select a project for processing.
 {my ($file) = @_;                                                              # Full file name
  $file
 }

sub loadProjects                                                                #r Locate documents to convert from folder L<in|/in>.
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
  else                                                                          # Choose documents in bulk
   {for my $file(develop ? @t : @p)
     {next unless selectProjectForProcessing($file);
      my $name  = chooseProjectName $file;
      Project($name, $file);
     }
   }
 }

sub Project::by($$$)                                                            # Process parse tree with checks to confirm features
 {my ($project, $x, $sub) = @_;                                                 # Project, node, sub
  $x->by($sub);
 }

sub Project::tableColumns2222($$)                                               # Set column details for a table
 {my ($project, $table) = @_;
  $table->at_table or confess "Not at a table ". -A $table;
  my @g;                                                                        # Groups in table
  $table->by(sub                                                                # Set number of columns in  each group
   {my ($r, $hb, $g, $t) = @_;
    if ($r->at_row_tbody_tgroup_table or $r->at_row_thead_tgroup_table)
     {push @g, $g if $t == $table;
     }
   });

  for my $g(@g)                                                                 # Set number of columns in  each group
   {for my $hb(grep {$_->at_thead or $_->at_tbody} @$g)
     {for my $r($hb->c_row)
       {my @e = $r->c_entry;
        my $e = @e;
        $g->set(cols=>$e);
       }
     }
   }

  for my $g(@g)                                                                 # Add colspec
   {my $n = $g->attr_cols;
    for(reverse 1..$n)
     {$g->putFirst($g->newTag(q(colspec), colname=>$_));
     }
   }

  for my $g(@g)                                                                 # Colspan
   {my $n = $g->attr_cols;
    for my $hb(grep {$_->at_thead or $_->at_tbody} @$g)
     {for my $r($hb->c_row)
       {my $col = 0;
        for my $e($r->c_entry)
         {++$col;
          if (my $span = $e->attr_colspan)
           {$e->deleteAttrs_colspan;
            if ($span ne q(1))
             {$e->set(namest=>q(c).$col, nameend=>q(c).($col+$span-1));
             }
           }
          if (my $span = $e->attrX_rowspan)                                     # Rowspan
           {$e->deleteAttrs_rowspan;
            if ($span ne q(1) and $span =~ m(\A\d+\Z)s)
             {$e->set(morerows=>($span-1));
             }
           }
         }
       }
     }
   }
 }

sub formatXml($)                                                                #r Format xml
 {my ($x) = @_;                                                                 # Parse tree
  my $s = $x->prettyStringDitaHeaders;
  spelling($s);
 }

sub writeAndLintTopic($$$)                                                      #r Write and lint a topic
 {my ($project, $x, $out) = @_;                                                 # Project, parse tree, output folder

  my $title = sub
   {my $t = $x->go_title;
    return $t->stringText if $t;
    q();
   }->();

  my $ext = sub
   {my $t = $x->go_title;
    return q(ditamap) if $x->tag =~ m(map)is;
    q(dita);
   }->();

  my $file = copyFileMd5NormalizedCreate
   ($out, &formatXml($x), $ext, <<END);
WriteAndLintTopic
END

  $x->createGuidId unless $x->id;

  my $l = Data::Edit::Xml::Lint::new();                                         # Write and lint topic
     $l->author    = $project->author;                                          # Author of topic
     $l->project   = $project->name;                                            # Project name
     $l->catalog   = &catalog;                                                  # Catalog
     $l->ditaType  = $x->tag;                                                   # Dita type
     $l->guid      = $x->id;                                                    # Topic guid
     $l->file      = $file;                                                     # File to write to
     $l->inputFile = $project->source;                                          # Input file
     $l->labels    = $x;                                                        # Show labels
     $l->source    = formatXml($x);                                             # Format source and add headers
     $l->title     = $title;                                                    # Title
     $l->lintNOP;                                                               # Lint
 }

sub stringToFileName($)                                                         #r Convert a title string to a file name
 {my ($string) = @_;                                                            # String
  $string =~ s(\.\.\.)        (_)gs;
  $string =~ s([%@#*?“'"|,.]) (_)gs;
  $string =~ s([&\+~\/\\:=])  (-)gs;
  $string =~ s([<\[])         (\x28)gs;
  $string =~ s([>\]])         (\x29)gs;
  $string =~ s(\s+)           (_)gs;

  $string =~ s(<[^>]*?>)      ()g;                                              # Single line because tags do not cross lines.
  $string =~ s(-+)            (_)g;                                             # Otherwise Lint complains because it needs -- for comments
  $string =~ s([^a-zA-Z0-9\\/.+_-]) ()g;                                        # Remove any other extraneous characters

  my $r = lc firstNChars $string, maximumFileFromTitleLength;
  $r
 }

#sub parseCacheFile($)                                                           #P Name of the file in which to cache parse trees
# {my ($project) = @_;                                                           # Project
#  my $s = $project->source;
#  my $m = fileMd5Sum($s);
#  fpe(parseCache, $m, q(data));
# }

sub parse($)                                                                    #P Parse a project.
 {my ($project) = @_;                                                           # Project
  my $projectName = $project->name;

#  my $c = parseCacheFile($project);                                             # Cache parse file name
#
#  if (-e $c)                                                                    # Reuse cached parse if available
#   {return retrieveFile($c);
#   }
#
#  my $s = readFile($project->source);
##    $s =~ s(]]\s+>) (]]>)gs;                                                   # Disagreement about the end of CDATA tag
  my $x = eval {Data::Edit::Xml::new($project->source)};                        # Parse the source
  if ($@)
   {$project->parseFailed = 1;
    eee "Parse of project: $projectName failed\n", $project->source, "\n$@";
    return undef;
   }

# storeFile($c, $x);                                                            # Cache parse

  $x
 }

sub gatheredFile($)                                                             #r Save file for parse tree after initial parse and gather
 {my ($project) = @_;                                                           # Project == document to convert
  fpe(gathered, $project->number, q(data))
 }

sub gatherOneProject($$)                                                        #r Allow the caller to process the parse tree during gather
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree
 }

sub gatherProject($)                                                            #r Gather some information from each project
 {my ($project) = @_;                                                           # Project == document to convert
  my $projectName = $project->name;
  ddd "Gather ", $project->number, "/", projectCount, " $projectName";          # Title of each gather

  if (my $x = parse($project))                                                  # Parse file
   {$project->isMap   = $x->tag =~ m(map\Z)is;                                  # Map file
    $project->topicId = $x->id;                                                 # Topic Id
    $project->tag     = $x->tag;                                                # Outermost tag

    gatherOneProject($project, $x);                                             # Allow the caller to process the parse tree during gather

    $x->by(sub                                                                  # Locate title and hence output file
     {my ($t) = @_;
      if ($t->at_title)
       {my $T = $project->title = $t->stringText;                               # Title without interior tags
        $project->outputFile = stringToFileName($T);
       }
     }) unless keepFileNames;                                                   # Keep existing file names if requested rather than converting to some variant of the GB standard.

    storeFile gatheredFile($project), $x;                                       # Save parse tree - separately because it is large.
   }
  $project                                                                      # Gathered information about a project
 }

sub assignOutputFiles                                                           #r Add deduplicating numbers to output files names that would otherwise be the same.
 {my %outputFileNames;                                                          # Output file names
  my @badTags;                                                                  # Files not converted because they have an unexpected tag type
  my %dupId;                                                                    # Projects with duplicate topic ids
  my @dupId;                                                                    # Projects with duplicate topic ids report
  my @noId;                                                                     # Projects with no topic id

  for my $p(sort keys %$projects)                                               # The normal case where we want to improve the input file names.
   {my $project = $projects->{$p};

    eval {Data::Edit::Xml::topicTypeAndBody($project->tag)};                    # Check the outermost tag is acceptable
    if ($@)
     {push @badTags, [$project->tag, $p, $project->source];
      next;
     }

    if (my $o = $project->outputFile)
     {if (my $n = $outputFileNames{$o}++)
       {$project->outputFile .= q(_).$n;
       }
     }
    else
     {eee "No output file for project source:\n", $project->source, "\n";
     }

    if (my $i = $project->topicId)                                              # Check topic ids
     {if (my $p = $dupId{$i})
       {push @dupId,
         [$i, $p->name, $project->name, $p->source, $project->source];
       }
      else
       {$dupId{$i} = $project;
       }
     }
    else
     {push @noId, [$project->name, $project->source];
     }
   }

  delete $$projects{$$_[1]} for @badTags;                                       # Remove projects with tags we cannot handle

  formatTable(\@noId, [qw(Project Source)],
    head=><<END,
NNNN projects have no topic ids on DDDD.

END
    file=>fpe(reports, qw(duplicateIdsInProjects txt)));

  formatTable(\@dupId, [qw(Id Project1 Project2 Source1 Source2)],
    head=><<END,
NNNN ids are defined in more than one project on DDDD.

This report lists ids that are defined in more then one source document
END
    file=>fpe(reports, qw(duplicateIdsInProjects txt)));

  formatTable(\@badTags, <<END,
Root_Tag    Root tag considered bad
Project     Project containing the bad root tag
Source_File Sourcev file containing bad root tag
END
    head=><<END,
NNNN projects removed because they had bad tags on the root node on DDDD.

END
    file=>fpe(reports, qw(badTags txt)));
 }

sub fullOutputFileName($)                                                       #r Full output file name for a project when there is one output file per input file
 {my ($project) = @_;                                                           # Project == document to convert, parse tree.
  fpe(out, $project->outputFile, q(dita));                                      # File to write to
 }

sub convertDocument($$)                                                         #r Convert one document.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.
 }

sub convertProject($)                                                           #r Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.
 {my ($project) = @_;                                                           # Project == document to convert
  my $projectName = $project->name;
  ddd "Convert Project ", $project->number, "/", projectCount, " $projectName"; # Title of each conversion

  my $x = retrieveFile(gatheredFile $project);                                  # Reload parse into this process

  $project->author = sub                                                        # Locate author name
   {my $a;
    $x->by(sub
     {if ($_->at_author)
      {$a = $_->stringText unless $a;
      }
    });
    $a // $project->author // "Anonymous";                                      # Return some sort of author name
   }->();

  convertDocument($project, $x);                                                # Convert document

  if (writeResidue)                                                             # Write the remainder
   {my $extension = sub                                                         # Extension for output file
     {return q(ditamap) if $x->tag =~ m(\A(book)?map\Z)s;
      q(dita);
     }->();

    my $title = sub                                                             # Title for SDL file map
     {if (my $t = $x->go_title)
       {my $title = $t->stringText;
        return $title;
       }
      undef;
     }->();

    my $o = fpe(out, $project->outputFile, $extension);                         # File to write to
    my $s = eval {formatXml($x)}          ;                                     # Will die if the root tag is not a standard Dita topic tag

    if (lint and $s)                                                            # Lint llll
     {my $l = Data::Edit::Xml::Lint::new();                                     # Write and lint topic
      $l->author    = $project->author;                                         # Author of topic
      $l->project   = $project->name;                                           # Project name
      $l->catalog   = catalog;                                                  # Catalog
      $l->ditaType  = $x->tag;                                                  # Dita type
      $l->guid      = $x->id;                                                   # Topic guid
      $l->file      = $o;                                                       # File to write to
      $l->inputFile = $project->source;                                         # Input file
      $l->labels    = $x;                                                       # Show labels
      $l->source    = spelling($s);                                             # Format source and add headers
      $l->title     = $title if $title;                                         # Add title if present
      $l->lintNOP;                                                              # Lint
     }
    else                                                                        # Write without lint because we could not assign headers. Xref will report on this as there are no headers.
     {my $f = $project->outputFile;
      writeFile($o, -p $x);
     }
   }

  $project                                                                      # Conversion succeeded for project
 } # convertProject

sub formatDitaRefs                                                              #r Format references per the Dita standard - requires that lint is specified to create the label information in each output file
 {#ddd "Convert references to Dita standard";
 }

sub lintResults                                                                 #r Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.
 {if (lint)                                                                     # Only if lint requested
   {lll "Lint results";
    clearFolder(reports, clearCount);                                           # Clear prior run

    formatDitaRefs if ditaRefs;                                                 # Format references in data format
    lll "  Start Xref";
    my $xref = xref ? Data::Edit::Xml::Xref::xref                               # Check any cross references
     (inputFolder              => &out,
      reports                  => &reports,
      debugTimes               => debugXrefTimes,
      maximumNumberOfProcesses => &maximumNumberOfProcesses,
      addNavTitles             => addNavTitles,
      out2 ? (flattenFolder    => out2) : (),
     ) : undef;

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

      $lintResults = join "\n", ($r =~ s(\n.*\Z) ()sr), $x, $p;                 # Run results to create issue after upload is complete
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

sub uploadToS3                                                                  #r Copy entire home folder to S3
 {if (upload)
   {lll "Upload to S3";
    my $p = s3Parms.s3ProfileValue;

    if (s3OutputFolder)                                                         # Upload to output area if requested
     {my @out = (out2 ? out2 : out);
      for my $dir(reports, out, @out, perl)
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
    Flip::Flop::uploadToS3();                                                   # Reset upload flip flop
   }
  else
   {ddd "Upload to S3 not requested";
   }
 }

sub uploadToExchange                                                            #r Copy entire home folder to Exchange
 {if (exchange)
   {lll "Upload to Exchange";
    my $h = home;
    my $p = s3Parms.s3ProfileValue;

    if (s3ExchangeFolder)
     {my @d;                                                                    # Folders to upload
      push @d, perl    if exchangeItems =~ m(p|\A\Z)i;
      push @d, out     if exchangeItems =~ m(o|\A\Z)i;
      push @d, reports if exchangeItems =~ m(r|\A\Z)i;

      for my $dir(@d)                                                           # Just reports now that we have the reports/fails/ folder.
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
    Flip::Flop::uploadToExchange();                                             # Reset upload flip flop
   }
  else
   {ddd "Upload to Exchange not requested";
   }
 }

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
    say STDERR "Tests: $p+$f == $a pass+fail==avail";
   }
 }

sub nwsc($)                                                                     #r Normalize white space and remove comments
 {my ($string) = @_;                                                            # Text to normalize
  $string =~ s(<\?.*?\?>)  ()gs;
  $string =~ s(<!--.*?-->) ()gs;
  $string =~ s(<!DOCTYPE.+?>)  ()gs;
  $string =~ s( (props|id)="[^"]*") ()gs;
  nws($string);
 }

sub testResult($$$)                                                             #r Evaluate the results of a test
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
    my @g = grep {!/\A\s*(<!|<\?)/} split /\n/, readFile($f);
    my @e = grep {!/\A\s*(<!|<\?)/} split /\n/, $expected;
    shift @g, shift @e while @g and @e and nwsc($g[0]) eq nwsc($e[0]);
    cluck "Got/expected in test $file:\n".
          "Got:\n". $g[0].
          "\nExpected:\n". $e[0]. "\n";
    return 0;
   }
 }

sub checkResults                                                                #r Send results to S3 from folder L<out|/out>.
 {for my $expected(searchDirectoryTreesForMatchingFiles(testResults))
   {my $got  = swapFilePrefix($expected, testResults, out);
    my $test = fn $expected;
    push @availableTests, $test;
    if (-e $got)
     {testResult($test, readFile($got), readFile($expected));
     }
   }
 }

sub gatherSelectedProjects                                                      #r Gather information from the selected project by reading their source files held in the L<in|/in>.
 {lll "Gather selected projects";
  my $ps = newProcessStarter(maximumNumberOfProcesses, process);                # Process starter

  my @p = sort keys %$projects;                                                 # Projects
  my $p = @p;

  for $project(@p)                                                              # Gather information from each project
   {$ps->start(sub{gatherProject($projects->{$project})});
   }

  if (my @results = $ps->finish)                                                # Consolidate results
   {reloadHashes(\@results);                                                    # Recreate attribute methods
    my %gather = %$projects;                                                    # Projects to gather
    for my $project(@results)                                                   # Each result
     {my $projectName = $project->name;                                         # Project name
      if (my $p = $$projects{$projectName})                                     # Find project
       {$$projects{$projectName} = $project;                                    # Consolidate information gathered
        delete $gather{$projectName};                                           # Mark project as gathered
       }
      else                                                                      # Confess to invalid project
       {confess "Unknown gathered project $projectName";
       }
     }
    if (my @f = sort keys %gather)                                              # Confess to projects that failed to gather
     {formatTable(
       [map {$gather{$_}->source} @_],
       [q(), q(Source File)],
        head=>qq(NNNN of $p source files failed to gather on DDDD),
        file=>fpe(reports, qw(bad sourceFileConversions txt)));
     }
   }

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
  lll "Convert selected projects, of numerosity $p";

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
    clearFolder($_, clearCount) for out, (out2 ? out2 : ()), process;           # Clear output folders
    loadProjects;                                                               # Projects to run
    gatherSelectedProjects;                                                     # Gather information about each project
    assignOutputFiles;                                                          # Assign output file names
    beforeConvertProjects;
    convertSelectedProjects;                                                    # Convert selected projects
    afterConvertProjects;
    Flip::Flop::convert();                                                      # Reset conversion flip flop
   }
  else
   {lll "Convert documents not requested";
   }
 }

sub sampleConcept()
 {<<END
<concept id="c1">
  <title id="title">Unknown</title>
  <conbody/>
</concept>
END
 }

sub sampleBookmap()
 {<<END
<bookmap>
  <booktitle>
    <mainbooktitle/>
    <booktitlealt/>
    <booktitlealt/>
  </booktitle>
  <bookmeta>
    <shortdesc/>
    <author/>
    <source/>
    <category/>
    <prodinfo>
      <prodname product=""/>
      <vrmlist>
        <vrm version=""/>
      </vrmlist>
      <prognum/>
      <brand/>
    </prodinfo>
    <bookrights>
      <copyrfirst>
        <year/>
      </copyrfirst>
      <bookowner/>
    </bookrights>
  </bookmeta>
 <frontmatter>
  <notices/>
  <booklists>
  <toc/>
  </booklists>
  <preface/>
</frontmatter>
<chapter/>
<appendices>
    <appendix/>
</appendices>
<reltable>
    <relheader>
        <relcolspec/>
        <relcolspec/>
    </relheader>
    <relrow>
        <relcell/>
        <relcell/>
    </relrow>
    <relrow>
        <relcell/>
        <relcell/>
    </relrow>
</reltable>
</bookmap>
END
 }

sub replaceableMethods                                                          #P Replaceable methods
 {qw(Project  assignOutputFiles checkResults
chooseProjectName convertDocument
convertProject beforeConvertProjects convertProjects afterConvertProjects
convertSelectedProjects convertToUTF8 convertXmlToDita deTagString
copyLogFiles
downloadFromS3
formatXml formatDitaRefs fullOutputFileName gatherProject
gatherSelectedProjects gatheredFile lintResults loadProjects
numberOutputFiles nwsc outImages parse projectCount runTests
selectProjectForProcessing
spelling stringToFileName
testResult uploadToExchange uploadToS3
useLastFolderAsProjectName useLastFolderPlusFileAsProjectName
writeAndLintTopic);
 }
# parseCacheFile
sub attributeMethods                                                            #P Attribute methods
 {qw(addNavTitles
catalog clearCount convert
debugX debugXrefTimes develop ditaRefs download downloads
endTime gathered home images in inputExt keepFileNames
lineNumbers lint logFile logStderr
maximumNumberOfProcesses
maximumFileFromTitleLength out out2 parseCache perl process reports runTime
s3Bucket s3Exchange s3FolderIn s3FolderUp
s3Parms s3Profile startTime summaryFile
testDocuments testResults
tests unicode upload writeResidue xref)
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
  lll "Convert xml to Dita";
  unlink errorLogFile;

  for my $phase(qw(downloadFromS3 convertToUTF8
                   beforeConvertProjects convertProjects afterConvertProjects
                   lintResults runTests copyLogFiles
                   uploadToS3 uploadToExchange))                                # We might fullOutputFileName before linting
   {no strict;
    lll "Phase: ", $phase;
    &{$phase};
   }

  $endTime = time;                                                              # Run time statistics
  $runTime = $endTime - $startTime;
  say STDERR $lintResults;                                                      # Avoid line number info on helpful statement
 }

sub createSampleInputFiles                                                      #P Create sample input files for testing. The attribute B<inputFolder> supplies the name of the folder in which to create the sample files.
 {my ($p) = caller();                                                           # Default package if none supplied

  my $f = fpe(downloads, qw(1 dita));
  owf($f, <<END);
<concept id="c1">
  <title id="title">Hello World</title>
  <conbody/>
</concept>
END
  owf(fpe(downloads, qw(2 dita)), <<END);
<concept id="c2">
  <title id="title">Good Bye</title>
  <conbody/>
</concept>
END
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
$projects $project $lintResults
catalog
checkResults
clearCount
convert
convertDocument
convertProject
beforeConvertProjects
convertProjects
afterConvertProjects
convertSelectedProjects
convertToUTF8
copyLogFiles
ddd
develop
download
downloadFromS3
downloads
eee
endTime
formatXml
gathered
gatheredFile
gatherProject
gatherSelectedProjects
home
in
inputExt
lint
lintResults
lll
loadProjects
logFile
maximumNumberOfProcesses
maximumFileFromTitleLength
numberOutputFiles
nwsc
out
overrideMethods
parse
process
Project
projectCount
reports
runTests
runTime
s3Bucket
s3FolderIn
s3FolderUp
s3Parms
startTime
stringToFileName
summaryFile
testDocuments
testResult
testResults
tests
unicode
exchange
upload
uploadToS3
uploadToExchange
useLastFolderAsProjectName
useLastFolderPlusFileAsProjectName
);
# parseCache
# parseCacheFile

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


Version 20181101.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Convert Xml to the Dita standard.

Convert Xml to the Dita standard.

=head2 Methods

Methods defined in this package.

=head3 lll(@)

Log messages including the project name if available

     Parameter  Description
  1  @m         Messages

B<Example:>


  sub 𝗹𝗹𝗹(@)
   {my (@m) = @_;                                                                 # Messages
    my $m = join '', dateTimeStamp, " ", @_;                                      # Time stamp each message
       $m =~ s(\s+) ( )gs;
    if ($project)
     {$m .= " in project $project";
     }
    my ($p, $f, $l) = caller();
    $m .= " at $f line $l
";

    say STDERR $m;
   }


You can provide you own implementation of this method in your calling package
via:

  sub lll {...}

if you wish to override the default processing supplied by this method.



=head3 downloadFromS3()

Download documents from S3 to the L<downloads|/downloads> folder.


B<Example:>


  sub 𝗱𝗼𝘄𝗻𝗹𝗼𝗮𝗱𝗙𝗿𝗼𝗺𝗦𝟯
   {if (download)                                                                 # Download if requested
     {lll "Download from S3";
      clearFolder(downloads, clearCount);
      makePath(downloads);
      my $b = s3Bucket;
      my $d = downloads;
      my $f = s3FolderIn;
      my $p = s3Parms;
      my $c = qq(aws s3 sync s3://$b/$f $d $p);
      xxx $c;
      Flip::Flop::download();
     }
    else
     {lll "Download from S3 not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub downloadFromS3 {...}

if you wish to override the default processing supplied by this method.



=head3 convertToUTF8()

Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗧𝗼𝗨𝗧𝗙𝟴
   {if (unicode)
     {clearFolder(in, maximumNumberOfFilesToClear);

      my $n = 0;
      my @d = searchDirectoryTreesForMatchingFiles(downloads, inputExt);
      my $N = @d;
      lll "Unicode conversion $N xml documents to convert from folder: ", downloads;
      for my $s(@d)
       {my $d = downloads;
        my $i = in;
        my $t = $s =~ s(\A$d) ($i)r;
        makePath($t);

        my $fileType = sub
         {my $c = qx(file "$s");
          return q(ASCII) if $c =~ m(ASCII text)s;
          return q(UTF8)  if $c =~ m(UTF-8 Unicode .*text)s;
          return q(UTF16) if $c =~ m(UTF-16 Unicode text)s;
          my $s = readBinaryFile($s);
          return q(UTF16) if $s =~ m(\Aencoding="UTF-16"\Z);
          confess "
Unknown file type $c

";
         }->();

        xxx qq(iconv -f $fileType -t UTF8 -o "$t" "$s");

        if (1)
         {my $s = readFile($t);
          my $S = $s =~ s(encoding="[^"]+") (encoding="UTF-8")r;
          owf($t, $S) unless $S eq $s;
          ++$n;
         }
       }
      if ($N != $n)
       {lll "Unicode conversion - converted $n documents BUT EXPECTED $N";
       }
      else
       {lll "Unicode conversion - converted all $n documents";
       }
      Flip::Flop::unicode();
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertToUTF8 {...}

if you wish to override the default processing supplied by this method.



=head3 projectCount()

Number of projects to process.


B<Example:>


  sub 𝗽𝗿𝗼𝗷𝗲𝗰𝘁𝗖𝗼𝘂𝗻𝘁()
   {scalar keys %$projects
   }


You can provide you own implementation of this method in your calling package
via:

  sub projectCount {...}

if you wish to override the default processing supplied by this method.



=head3 Project()

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
      id         => undef,                                                        # Id attribute value from outermost tag
      isMap      => undef,                                                        # Map
      name       => $name,                                                        # Name of project
      number     => projectCount + 1,                                             # Number of project
      outputFile => undef,                                                        # Output file
      parseFailed=> undef,                                                        # Parse of source file failed
      source     => $source,                                                      # Input file
      tag        => undef,                                                        # Name of outermost tag
      title      => undef,                                                        # Title for project
      topicId    => undef,                                                        # Topic id for project - collected during gather
     );

    $projects->{$p->name} = $p;                                                   # Save project definition
   }


You can provide you own implementation of this method in your calling package
via:

  sub Project {...}

if you wish to override the default processing supplied by this method.



=head3 loadProjects()

Locate documents to convert from folder L<in|/in>.


B<Example:>


  sub 𝗹𝗼𝗮𝗱𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀
   {my @p = searchDirectoryTreesForMatchingFiles(in,    inputExt);                # Production documents
    my @t = searchDirectoryTreesForMatchingFiles(tests, inputExt);                # Test documents
    if (my %t = map {$_=>1} testDocuments)                                        # Locate documents to be tested
     {for my $file(@p, @t)                                                        # Favor production over test because test is easier to run in bulk
       {my $name = fn $file;
        next unless $t{$name};                                                    # Skip unless name matches
        next if $projects->{$name};                                               # Skip if we already have a document to test
        Project($name, $file);
       }
     }
    else                                                                          # Choose documents in bulk
     {for my $file(develop ? @t : @p, inputExt)
       {my $name  = fn $file;
        Project($name, $file);
       }
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub loadProjects {...}

if you wish to override the default processing supplied by this method.



=head3 Project::by($$$)

Process parse tree with checks to confirm features

     Parameter  Description
  1  $project   Project
  2  $x         Node
  3  $sub       Sub

=head3 Project::formatXml($$)

Output file for a document

     Parameter  Description
  1  $project   Project
  2  $x         Parse tree

=head3 stringToFileName($)

Convert a title string to a file name

     Parameter  Description
  1  $string    String

B<Example:>


  sub 𝘀𝘁𝗿𝗶𝗻𝗴𝗧𝗼𝗙𝗶𝗹𝗲𝗡𝗮𝗺𝗲($)
   {my ($string) = @_;                                                            # String
    $string =~ s(\.\.\.)        (_)gs;
    $string =~ s([%@#*?“'"|,.]) (_)gs;
    $string =~ s([&\+~\/\\:=])  (-)gs;
    $string =~ s([<\[])         (28)gs;
    $string =~ s([>\]])         (29)gs;
    $string =~ s(\s+)           (_)gs;

    my $r = lc firstNChars $string, maximumFileFromTitleLength;
    $r
   }


You can provide you own implementation of this method in your calling package
via:

  sub stringToFileName {...}

if you wish to override the default processing supplied by this method.



=head3 gatheredFile($)

Save file for parse tree after initial parse and gather

     Parameter  Description
  1  $project   Project == document to convert

B<Example:>


  sub 𝗴𝗮𝘁𝗵𝗲𝗿𝗲𝗱𝗙𝗶𝗹𝗲($)
   {my ($project) = @_;                                                           # Project == document to convert
    fpe(gathered, $project->number, q(data))
   }


You can provide you own implementation of this method in your calling package
via:

  sub gatheredFile {...}

if you wish to override the default processing supplied by this method.



=head3 gatherProject($)

Gather some information from each project

     Parameter  Description
  1  $project   Project == document to convert

B<Example:>


  sub 𝗴𝗮𝘁𝗵𝗲𝗿𝗣𝗿𝗼𝗷𝗲𝗰𝘁($)
   {my ($project) = @_;                                                           # Project == document to convert
    my $projectName = $project->name;
    lll "Gather ", $project->number, "/", projectCount, " $projectName";          # Title of each gather

    if (my $x = $project->parse)                                                  # Parse file
     {$project->isMap   = $x->tag =~ m(map\Z)is;                                  # Map file
      $project->topicId = $x->id;                                                 # Topic Id
      $project->tag     = $x->tag;                                                # Outermost tag

      $x->by(sub                                                                  # Locate title and hence output file
       {my ($t) = @_;
        if ($t->at_title)
         {my $T = $project->title = $t->stringContent;
          $project->outputFile = stringToFileName($T);
         }
       });

      storeFile gatheredFile($project), $x;                                       # Save parse tree - separately because it is large.
     }
    $project                                                                      # Gathered information about a project
   }


You can provide you own implementation of this method in your calling package
via:

  sub gatherProject {...}

if you wish to override the default processing supplied by this method.



=head3 assignOutputFiles()

Add deduplicating numbers to output files names that would otherwise be the same.


B<Example:>


  sub 𝗮𝘀𝘀𝗶𝗴𝗻𝗢𝘂𝘁𝗽𝘂𝘁𝗙𝗶𝗹𝗲𝘀
   {my %o;
    my @badTags;                                                                  # Files not converted because they have an unexpected tag type
    my @parseFailed;                                                              # Projects that failed to parse
    my %dupId;                                                                    # Projects with duplicate topic ids
    my @dupId;                                                                    # Projects with duplicate topic ids report
    my @noId;                                                                     # Projects with no topic id

    for my $p(sort keys %$projects)
     {my $project = $projects->{$p};

      if ($project->parseFailed)                                                  # Report projects that failed to parse
       {push @parseFailed, [$project->name, $project->source];
       }
      else
       {eval {Data::Edit::Xml::topicTypeAndBody($project->tag)};                  # Check the outermost tag is acceptable
        if ($@)
         {push @badTags, [$project->tag, $p, $project->source];
          next;
         }

        if (my $o = $project->outputFile)
         {if (my $n = $o{$o}++)
           {$project->outputFile .= q(_).$n;
           }
         }
        else
         {confess "No output file for project source:
", $project->source, "
";
         }

        if (my $i = $project->topicId)                                            # Check topic ids
         {if (my $p = $dupId{$i})
           {push @dupId,
             [$i, $p->name, $project->name, $p->source, $project->source];
           }
          else
           {$dupId{$i} = $project;
           }
         }
        else
         {push @noId, [$project->name, $project->source];
         }
       }
     }

    delete $$projects{$$_[0]} for @parseFailed;                                   # Remove projects that failed to parse
    delete $$projects{$$_[1]} for @badTags;                                       # Remove projects with tags we cannot handle

    formatTable(\@noId, [qw(Project Source)],
      head=><<END,
  NNNN projects have no topic ids on DDDD.

  END
      file=>fpe(reports, qw(duplicateIdsInProjects txt)));

    formatTable(\@dupId, [qw(Id Project1 Project2 Source1 Source2)],
      head=><<END,
  NNNN ids are defined in more than one project on DDDD.

  This report lists ids that are defined in more then one source document
  END
      file=>fpe(reports, qw(duplicateIdsInProjects txt)));

    formatTable(\@parseFailed, [qw(Project Source)],
      head=><<END,
  NNNN projects failed to parse on DDDD.

  END
      file=>fpe(reports, qw(failedToParse txt)));
   }


You can provide you own implementation of this method in your calling package
via:

  sub assignOutputFiles {...}

if you wish to override the default processing supplied by this method.



=head3 convertDocument($$)

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



=head3 convertProject($)

Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.

     Parameter  Description
  1  $project   Project == document to convert

B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁($)
   {my ($project) = @_;                                                           # Project == document to convert
    my $projectName = $project->name;
    lll "Convert ", $project->number, "/", projectCount, " $projectName";         # Title of each conversion

    my $x = retrieveFile(gatheredFile $project);                                  # Reload parse into this process

    convertDocument($project, $x);                                                # Convert document

    my $o = fpe(out, $project->outputFile, q(dita));                              # File to write to

    if (lint)                                                                     # Lint
     {my $l = Data::Edit::Xml::Lint::new();                                       # Write and lint topic
      $l->project = $project->name;                                               # Project name
      $l->catalog = catalog;                                                      # Catalog
      $l->file    = $o;                                                           # File to write to
      $l->source  = $project->formatXml($x);                                      # Format source and add headers
      $l->lintNOP;                                                                # Lint
     }
    else                                                                          # Write without lint
     {my $f = $project->outputFile;
      writeFile($o, $project->formatXml($x));
     }

    $project                                                                      # Conversion succeeded for project
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertProject {...}

if you wish to override the default processing supplied by this method.



=head3 lintResults()

Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.


B<Example:>


  sub 𝗹𝗶𝗻𝘁𝗥𝗲𝘀𝘂𝗹𝘁𝘀
   {if (lint)                                                                     # Only if lint requested
     {lll "Lint results";
      clearFolder(reports, clearCount);                                           # Clear prior run

      lll "  Start Xref";
      my $xref = Data::Edit::Xml::Xref::xref                                      # Check any cross references
       (inputFolder=>&out, reports=>&reports,
        maximumNumberOfProcesses=>&maximumNumberOfProcesses);

      lll "  Start Lint Summary";
      if (my $report = Data::Edit::Xml::Lint::report
       (out, qr(\.(dita|ditamap|xml)\Z)))
       {my $r = $report->print;
        my $d = dateTimeStamp;
        my $h = home;
        my $b = s3Bucket;
        my $B = $b && upload ?
          qq(

Please see: aws s3 sync s3://$b ?

) :
          qq();
        my $x =                                                                   # Include xref results
        ˢ{my $s = $xref->statusLine;
          return "

$s"  if $s;
          q()
         };

        my $s = <<END;                                                            # rrrr
  Summary of passing and failing projects on $d.\t\tVersion: $VERSION$B

  $r
  $x
  END
        say STDERR $s;
        writeFile(summaryFile, $s);
        Flip::Flop::lint();
       }
      else
       {lll "No Lint report available";
       }
     }
    else
     {lll "Lint report not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub lintResults {...}

if you wish to override the default processing supplied by this method.



=head3 uploadToS3()

Send results to S3 from folder L<out|/out>.


B<Example:>


  sub 𝘂𝗽𝗹𝗼𝗮𝗱𝗧𝗼𝗦𝟯
   {if (upload)
     {lll "Upload to S3";
      my $h = home;
      my $b = s3Bucket;
      my $f = s3FolderUp;
      my $p = s3Parms;
      my $c = qq(aws s3 sync $h s3://$b/$f $p);
      say STDERR $c;
      print STDERR $_ for qx($c);
      say STDERR qq(Please see:  aws s3 sync s3://$b/$f ?);
      Flip::Flop::𝘂𝗽𝗹𝗼𝗮𝗱𝗧𝗼𝗦𝟯();                                                   # Reset upload flip flop
     }
    else
     {lll "Upload to S3 not requested";
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub uploadToS3 {...}

if you wish to override the default processing supplied by this method.



=head3 runTests()

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



=head3 nwsc($)

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



=head3 testResult($$$)

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



=head3 checkResults()

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



=head3 gatherSelectedProjects()

Gather information from the selected project by reading their source files held in the L<in|/in>.


B<Example:>


  sub 𝗴𝗮𝘁𝗵𝗲𝗿𝗦𝗲𝗹𝗲𝗰𝘁𝗲𝗱𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀
   {lll "Gather selected projects";
    my $ps = newProcessStarter(maximumNumberOfProcesses, process);                # Process starter

    for(sort keys %$projects)                                                     # Gather information from each project
     {$ps->start(sub{gatherProject($projects->{$_})});
     }

    if (my @results = $ps->finish)                                                # Consolidate results
     {reloadHashes(\@results);                                                    # Recreate attribute methods
      my %togather = %$projects;
      for my $project(@results)                                                   # Each result
       {my $projectName = $project->name;                                         # Project name
        if (my $p = $$projects{$projectName})                                     # Find project
         {$$projects{$projectName} = $project;                                    # Consolidate information gathered
          delete $togather{$projectName};                                         # Mark project as gathered
         }
        else                                                                      # Confess to invalid project
         {confess "Unknown gathered project $projectName";
         }
       }
      if (my @f = sort keys %togather)                                            # Confess to projects that failed to gather
       {confess "The following projects failed to gather:
", join (' ', @f);
       }
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub gatherSelectedProjects {...}

if you wish to override the default processing supplied by this method.



=head3 convertSelectedProjects()

Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗦𝗲𝗹𝗲𝗰𝘁𝗲𝗱𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀
   {lll "Converted selected projects";
    my $ps = newProcessStarter(maximumNumberOfProcesses, process);                # Process starter

    for $project(sort keys %$projects)                                            # Convert projects
     {$ps->start(sub{convertProject($projects->{$project})});                     # Convert each project in a separate process
     }

    if (my @results = $ps->finish)                                                # Consolidate results
     {reloadHashes(\@results);                                                    # Recreate attribute methods
      my %toConvert = %$projects;
      for my $project(@results)                                                   # Each result
       {my $projectName = $project->name;                                         # Converted project name
        if (my $p = $$projects{$projectName})                                     # Find project
         {$$projects{$projectName} = $project;                                    # Consolidate information gathered
          delete $toConvert{$projectName};                                        # Mark project as converted
         }
        else                                                                      # Confess to invalid project
         {confess "Unknown converted project $projectName";
         }
       }
      if (my @f = sort keys %toConvert)                                           # Confess to projects that failed to convert
       {confess "The following projects failed to convert:
", join (' ', @f);
       }
     }
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertSelectedProjects {...}

if you wish to override the default processing supplied by this method.



=head3 convertProjects()

Convert the selected documents.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗣𝗿𝗼𝗷𝗲𝗰𝘁𝘀
   {if (convert)                                                                  # Convert the documents if requested.
     {lll "Convert documents";
      clearFolder($_, clearCount) for out, process;                               # Clear output folders
      loadProjects;                                                               # Projects to run
      gatherSelectedProjects;                                                     # Gather information about each project
      assignOutputFiles;                                                          # Assign output file names
      my @r = convertSelectedProjects                                             # Convert selected projects
      Flip::Flop::convert();                                                      # Reset conversion flip flop
      return @r;                                                                  # Return results of conversions
     }
    else
     {lll "Convert documents not requested";
     }

    ()
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertProjects {...}

if you wish to override the default processing supplied by this method.



=head3 convertXmlToDita()

Perform all the conversion projects.


B<Example:>


  sub 𝗰𝗼𝗻𝘃𝗲𝗿𝘁𝗫𝗺𝗹𝗧𝗼𝗗𝗶𝘁𝗮
   {my ($package) = caller;

  # mergePackages($package);

    for my $phase(qw(downloadFromS3 convertToUTF8 convertProjects
                     lintResults runTests uploadToS3))
     {no strict;
  #   lll "Phase: ", $phase;
      &{$phase};
     }

    $endTime = time;                                                              # Run time statistics
    $runTime = $endTime - $startTime;
   }


You can provide you own implementation of this method in your calling package
via:

  sub convertXmlToDita {...}

if you wish to override the default processing supplied by this method.




=head1 Hash Definitions




=head2 Project Definition


Project definition


B<id> - Id attribute value from outermost tag

B<isMap> - Map

B<name> - Name of project

B<number> - Number of project

B<outputFile> - Output file

B<parseFailed> - Parse of source file failed

B<source> - Input file

B<tag> - Name of outermost tag

B<title> - Title for project

B<topicId> - Topic id for project - collected during gather



=head2 𝗣𝗿𝗼𝗷𝗲𝗰𝘁 Definition


𝗣𝗿𝗼𝗷𝗲𝗰𝘁 definition


B<id> - Id attribute value from outermost tag

B<isMap> - Map

B<name> - Name of project

B<number> - Number of project

B<outputFile> - Output file

B<parseFailed> - Parse of source file failed

B<source> - Input file

B<tag> - Name of outermost tag

B<title> - Title for project

B<topicId> - Topic id for project - collected during gather



=head1 Attributes


The following is a list of all the attributes in this package.  A method coded
with the same name in your package will over ride the method of the same name
in this package and thus provide your value for the attribute in place of the
default value supplied for this attribute by this package.

=head2 Replaceable Attribute List


catalog clearCount convert devShm devShmOrHome develop download downloads endTime gathered home in inputExt lint maximumFileFromTitleLength maximumNumberOfFilesToClear maximumNumberOfProcesses out parseCache process reports runTime s3Bucket s3FolderIn s3FolderUp s3Parms startTime summaryFile testDocuments testResults tests unicode upload


=head2 catalog

Dita catalog to be used for linting.


=head2 clearCount

Limit on number of files to clear from each output folder.


=head2 convert

Convert documents to dita if true.


=head2 devShm

Shared memory folder for output files.


=head2 devShmOrHome

Shared memory folder or home folder.


=head2 develop

Production run if this file folder is detected otherwise development.


=head2 download

Download from S3 if true.


=head2 downloads

Downloads folder.


=head2 endTime

End time of run in seconds since the epoch.


=head2 gathered

Folder containing saved parse trees after initial parse and information gathering.


=head2 home

Home folder containing all the other folders


=head2 in

Input documents folder.


=head2 inputExt

Extension of input files.


=head2 lint

Lint output xml if true or write directly if false.


=head2 maximumFileFromTitleLength

Maximum amount of title to use in constructing output file names.


=head2 maximumNumberOfFilesToClear

Maximum number of files to clear


=head2 maximumNumberOfProcesses

Maximum number of processes to run in parallel.


=head2 out

Converted documents output folder.


=head2 parseCache

Cached parse trees


=head2 process

Process data folder used to communicate results between processes.


=head2 reports

Reports folder.


=head2 runTime

Elapsed run time in seconds.


=head2 s3Bucket

Bucket on S3 holding documents to convert and the converted results.


=head2 s3FolderIn

Folder on S3 containing original documents.


=head2 s3FolderUp

Folder on S3 containing results of conversion.


=head2 s3Parms

Additional S3 parameters for uploads and downloads.


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




=head1 Optional Replace Methods

The following is a list of all the optionally replaceable methods in this
package.  A method coded with the same name in your package will over ride the
method of the same name in this package providing your preferred processing for
the replaced method in place of the default processing supplied by this
package. If you do not supply such an over riding method, the existing method
in this package will be used instead.

=head2 Replaceable Method List


Project assignOutputFiles checkResults convertDocument convertProject convertProjects convertSelectedProjects convertToUTF8 convertXmlToDita downloadFromS3 gatherProject gatherSelectedProjects gatheredFile lintResults lll loadProjects nwsc projectCount runTests stringToFileName testResult uploadToS3




=head1 Private Methods

=head2 getHome()

Compute home directory once


=head2 Project::parseCacheFile($)

Name of the file in which to cache parse trees

     Parameter  Description
  1  $project   Project

=head2 Project::parse($)

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

=head2 createSampleInputFiles()

Create sample input files for testing. The attribute B<inputFolder> supplies the name of the folder in which to create the sample files.



=head1 Index


1 L<assignOutputFiles|/assignOutputFiles> - Add deduplicating numbers to output files names that would otherwise be the same.

2 L<attributeMethods|/attributeMethods> - Attribute methods

3 L<checkResults|/checkResults> - Send results to S3 from folder L<out|/out>.

4 L<convertDocument|/convertDocument> - Convert one document.

5 L<convertProject|/convertProject> - Convert one document held in folder L<in|/in> into topic files held in L<out|/out>.

6 L<convertProjects|/convertProjects> - Convert the selected documents.

7 L<convertSelectedProjects|/convertSelectedProjects> - Convert the selected documents by reading their source in L<in|/in>, converting them and writing the resulting topics to L<out|/out>.

8 L<convertToUTF8|/convertToUTF8> - Convert the encoding of documents in L<downloads|/downloads> to utf8 equivalents in folder L<in|/in>.

9 L<convertXmlToDita|/convertXmlToDita> - Perform all the conversion projects.

10 L<createSampleInputFiles|/createSampleInputFiles> - Create sample input files for testing.

11 L<downloadFromS3|/downloadFromS3> - Download documents from S3 to the L<downloads|/downloads> folder.

12 L<gatheredFile|/gatheredFile> - Save file for parse tree after initial parse and gather

13 L<gatherProject|/gatherProject> - Gather some information from each project

14 L<gatherSelectedProjects|/gatherSelectedProjects> - Gather information from the selected project by reading their source files held in the L<in|/in>.

15 L<getHome|/getHome> - Compute home directory once

16 L<lintResults|/lintResults> - Lint results held in folder L<out|/out>and write reports to folder L<reports|/reports>.

17 L<lll|/lll> - Log messages including the project name if available

18 L<loadProjects|/loadProjects> - Locate documents to convert from folder L<in|/in>.

19 L<nwsc|/nwsc> - Normalize white space and remove comments

20 L<overrideMethods|/overrideMethods> - Merge packages

21 L<Project|/Project> - Project details including at a minimum the name of the project and its source file.

22 L<Project::by|/Project::by> - Process parse tree with checks to confirm features

23 L<Project::formatXml|/Project::formatXml> - Output file for a document

24 L<Project::parse|/Project::parse> - Parse a project.

25 L<Project::parseCacheFile|/Project::parseCacheFile> - Name of the file in which to cache parse trees

26 L<projectCount|/projectCount> - Number of projects to process.

27 L<replaceableMethods|/replaceableMethods> - Replaceable methods

28 L<runTests|/runTests> - Run tests by comparing files in folder L<out|/out> with corresponding files in L<testResults|/testResults>.

29 L<stringToFileName|/stringToFileName> - Convert a title string to a file name

30 L<testResult|/testResult> - Evaluate the results of a test

31 L<uploadToS3|/uploadToS3> - Send results to S3 from folder L<out|/out>.

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Data::Edit::Xml::To::Dita

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
