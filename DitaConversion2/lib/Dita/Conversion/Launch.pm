#!/usr/bin/perl -I/home/phil/perl/cpan/DitaConversion/lib
#-------------------------------------------------------------------------------
# Dita::Conversion::Launch - Launch conversion of xml files to Dita in parallel
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2018
#-------------------------------------------------------------------------------

package Dita::Conversion::Launch;
our $VERSION = "20180601";
require v5.16;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump);
use Data::Edit::Xml;
use Data::Edit::Xml::Lint;
use Data::Table::Text qw(:all);
use Dita::Conversion;
use Storable;
use utf8;

sub preprocessStep {q(preprocess)}                                              # Constants
sub parseStep      {q(parse)}
sub convertStep    {q(convert)}

my %projects;                                                                   # Prevent projects with duplicates names

#1 Methods                                                                      # Xml to  L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> conversion utilities.

sub new                                                                         #S Create a project to describe the conversion of each source file containing xml representing documentation into one or more L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> topics. L<Selected projects|/setSelection> can then be processed in parallel to speed up processing. L<Selected changes|/setChangeReasons> made by each conversion can then be L<consolidated|/writeConsolidatedChanges> into a single report.
 {my $launch = bless {@_};
  ref($launch->projects) &&
  ref($launch->projects) =~ m(\ADita::Conversion::Projects\Z) or
    confess "projects=>Dita::Conversion::Projects required";
  !$launch->preprocess or ref($launch->preprocess) =~ m(CODE) or
    confess preprocessStep."=>sub required";

  ref($launch->convert) or confess convertStep."=>specification required";      # Conversion specification
  $launch->save //= q(save);                                                    # Default save folder

  $launch->stepsByNumber =                                                      # Common steps
   [sub                                                                         # Preprocess
     {my ($launch, $projectName) = @_;
      my $project = $launch->projects->{$projectName};
      $project or confess "No such project: $projectName";
#     say STDERR join " ", "Step: ", $project->name, preprocessStep;
      $project->source = readFile($project->sourceFile);                        # Read source

      my $R = $project->preprocessedSource = sub
       {if (my $p = $launch->preprocess->($launch, $project, preprocessStep))   # Preprocess with supplied preprocessor
         {return $p;
         }
        $project->source                                                        # Reuse source if no preprocessor supplied
       }->();
      $launch->saveProject($projectName, preprocessStep);
      $R                                                                        # Result of preprocess step
     },
    sub                                                                         # Parse
     {my ($launch, $projectName) = @_;
      my $project = $launch->projects->{$projectName};
      $project or confess "No such project: $projectName";
#     say STDERR join " ", "Step: ", $project->name, parseStep;
      my $R = $project->parse =
        Data::Edit::Xml::new($project->preprocessedSource);
      $launch->saveProject($projectName, parseStep);
      $R                                                                        # Result of parse
     },
    ];

  $launch->stepNumberByName = {&preprocessStep=>0, &parseStep=>1};              # Common step names to step number
  $launch->stepNameByNumber = [preprocessStep,    parseStep];                   # Common step numbers to step name

  if (my $r = ref $launch->convert)                                             # Single or staged conversion
   {if ($r =~ m(CODE))                                                          # Single step conversion
     {push @{$launch->stepsByNumber},
       sub                                                                      # Add conversion step
         {my ($launch, $projectName) = @_;
          my $project = $launch->projects->{$projectName};
          $project or confess "No such project: $projectName";
#         say STDERR join " ", "Step: ", $project->name, q(convert);
          my $R = $launch->convert->($project, convertStep);
          $launch->saveProject($projectName, convertStep);
          $R                                                                    # Return result of convertion
         };
      push @{$launch->stepNameByNumber}, convertStep;                           # Step number to step name
      $launch->stepNumberByName->{&convertStep} = 2;                            # Step name to step number
     }
    elsif ($r =~ m(ARRAY))                                                      # Multiple step conversion
     {my @convert = @{$launch->convert};
      for my $stage(@convert)
       {if (ref($stage) and ref($stage) =~ m(ARRAY))
         {@$stage >=  2 or
           confess "name=>sub required for stage, got: ".dump($stage);
          my ($name, $sub) = @$stage;
          ref($name) and
            confess "Scalar required for stage name, got: ". dump($name);
          $name eq preprocessStep and
            confess "Cannot use preprocess for a ".
              "step name as it is used to reference the preprocess stage";
          $name eq parseStep and
            confess "Cannot use parse for a ".
              "step name as it is used to reference the parse stage";
          ref($sub) && ref($sub) =~ m(\ACODE\Z) or
            confess "Code required for stage $name, got: ". dump($sub);
         }
        else
         {confess "[conversion stage name => conversion sub] required, got: ".
                   dump($stage);
         }
       }

      for my $convert(@convert)                                                 # Each conversion step
       {my ($stepName, $sub) = @$convert;                                       # Step name, processing sub
        my $stepNumber = $launch->stepNumberByName->{$stepName} =
          @{$launch->stepsByNumber};                                            # Conversion step name to number
        push @{$launch->stepNameByNumber}, $stepName;                           # Conversion step number to name
        push @{$launch->stepsByNumber},
          sub                                                                   # Conversion steps
           {my ($launch, $projectName) = @_;
            ref($launch)  =~ m(Launch)s or confess;
           !ref($projectName) or confess;
#           say STDERR join " ", "Step: ", convertStep, $projectName, $stepName;
            my $R = $sub->($launch->projects->{$projectName}, $stepName);
            $launch->saveProject($projectName, $stepName);
            $R                                                                  # Return result of convertion
           };
       }
     }
    else
     {confess "Either convert=>conversion sub or ".
              " convert=>[[conversion stage name => conversion sub], ...]".
              " required, got: ".dump($launch->convert);
     }
   }

  return $launch;                                                               # Launch specification
 }

sub launch($$)                                                                  # Launch  the conversion of several projects in parallel
 {my ($launch, $title) = @_;                                                    # Launch specification, title
  my $projects = $launch->projects;
  my $mp       = $launch->maximumNumberOfProcesses // 8;

  if (my $out = $launch->out)                                                   # Clear the output area if present
   {my $limit = $launch->outFileLimit // 32;
    clearFolder($out, $limit);
   }

  $launch->log(q(Launch), $title, $launch->restart);

  if ($mp == 1)                                                                 # Process sequentially
   {for my $projectName(sort keys %$projects)                                   # Convert matching projects
     {$launch->launchProject($projectName);
     }
   }
  else                                                                          # Process in parallel
   {my $count = 0;
    for my $projectName(sort keys %$projects)                                   # Convert matching projects
     {wait if ++$count > $mp;                                                   # Wait for processes to finish
      $launch->launchProject($projectName), exit unless fork;                   # Convert in parallel
     }
    for(;;) {last if wait == -1}                                                # Wait for conversions to complete
   }

  if (my $out = $launch->out)                                                   # Clear the output area if present
   {Data::Edit::Xml::Lint::waitAllProcesses;                                    # Wait for lints to finish
    my $report = &Data::Edit::Xml::Lint::report($out, qr((dita|ditamap|xml)\Z));# Report results
    if (ref($report))
     {say STDERR $report->print;
      if (my $reportFile = $launch->reportResults)                              # Report lint results
       {writeFile($reportFile, $report->print);
       }
     }
   }
 }

#1 Attributes                                                                   # Use these attributes to configure Xml to configure a launch.

genLValueScalarMethods(q(changes));                                             # Changes to be recorded in the parse tree: undef for none else a selecting regular expression or a hash whose keys are the names of the selected projects or a specific change number.
genLValueScalarMethods(q(convert));                                             # Either a sub to perform the conversion, or an array of [name=>sub] to perform a series of conversions steps, where the results of each step is saved to L<the save directory|/save> so that conversion can be L<restarted|/ restart> at any point. Each conversion sub is passed the definition of the project to be converted with the parse tree already constructed. If the single step option is chosen then its step name is implicitly B<convert>.  The step name B<preprocess> refers to the L<preprocess|/preprocess> action.
genLValueScalarMethods(q(logMessages));                                         # [[timestamp, events]...]
genLValueScalarMethods(q(matching));                                            # Projects to be converted in this launch: undef for all projects or a string for a named project or an array of project names or a hash whose keys are the selected project names or a regular expression to select projects by name.
genLValueScalarMethods(q(maximumNumberOfProcesses));                            # Maximum number of processes to run in parallel
genLValueScalarMethods(q(out));                                                 # File output area for converted topics.  This area will be cleared at the start of each launch and reported on after the launch of this attribute is present.
genLValueScalarMethods(q(outFileLimit));                                        # Limit on the number of files to be cleared from the L<out|/out> folder at the start of each launch.
genLValueScalarMethods(q(projects));                                            # {projectName} = project description for all projects available
genLValueScalarMethods(q(preprocess));                                          # An optional sub to preprocess the source xml string contained in L<sourceFile|/sourceFile> before it is parsed.  The source xml is presented in B<$_>. The sub should return the transformed string.
genLValueScalarMethods(q(restart));                                             # Restart from the named step or from "preprocess"
genLValueScalarMethods(q(reportResults));                                       # Report results of linting the files produced during this conversion in this file
genLValueScalarMethods(q(save));                                                # Temporary files will be stored in this folder
genLValueScalarMethods(q(stepNameByNumber));                                    # Get the name of a step from its name
genLValueScalarMethods(q(stepNumberByName));                                    # Get the number of a step from its name
genLValueScalarMethods(q(stepsByNumber));                                       # Array of steps to be performed

sub stepSaveFile($$$)                                                           #P Save file for a project and a step
 {my ($launch, $projectName, $step) = @_;                                       # Launch specification, project, step name
  ref($launch)  =~ m(Launch)s or confess;
  !ref($projectName) or confess;
  fpe($launch->save, $projectName, $step, q(data))
 }

sub saveProject($$$)                                                            #P Save project at a particular step
 {my ($launch, $projectName, $step) = @_;                                       # Launch specification, project, step
  ref($launch)  =~ m(Launch)s or confess;
  !ref($projectName) or confess;
  my $file = $launch->stepSaveFile($projectName, $step);
  makePath $file;
  my $project = $launch->projects->{$projectName};
  store $project, $file;
  $project->stepsProcessed->{$step}++;                                          # Step processed
  $launch->log("save", $projectName, $step);                                    # Log progress
 }

sub loadProject($$$)                                                            #P Load a project at a particular step
 {my ($launch, $projectName, $stepNumber) = @_;                                 # Launch specification, project, step to reload
  ref($launch)  =~ m(Launch)s or confess;
  !ref($projectName) or confess;
  my $step = $launch->stepNameByNumber->[$stepNumber];
  my $file = $launch->stepSaveFile($projectName, $step);
  -e $file or confess "No such file:\n$file";
  my $p    = retrieve $file;
  $p or confess "Unable to retrieve project save file:\n$file";
  $launch->projects->{$projectName} = $p;
  $launch->log("load", $projectName, $step);                                    # Log progress
 }

sub launchProject($$)                                                           #P Convert a single project in a seperate process
 {my ($launch, $projectName) = @_;                                              # Launch specification, project to be processed
  ref($launch)  =~ m(Launch)s or confess;
  !ref($projectName) or confess;
  my $projects = $launch->projects;                                             # Projects
  my $project  = $projects->{$projectName};                                     # Project
  $project or confess "No such project: $projectName";
  my $sourceFile = $project->sourceFile;
  my $steps    = $launch->stepsByNumber;                                        # Steps
  my $mp       = $launch->maximumNumberOfProcesses // 8;
  my $restart  = $launch->restart;                                              # Restart specification
  my $count    = 0;
  $project->stepsProcessed = {};                                                # Steps processed
  my @steps    = @$steps;                                                       # All the steps for a complete project

  if ($restart)                                                                 # Remove steps we can skip in a restart because we have a restart file for that step
   {my $requested = $launch->stepNumberByName->{$restart};                      # Requested restart step number
    defined($requested) or confess "No such step: $restart, choose from: ".     # Missing step
      dump([sort keys %{$launch->stepNumberByName}]);
    my $actual;                                                                 # Actual restart step number
    for my $step(1..$requested)                                                 # Each possible restart step
     {my $stepName = $launch->stepNameByNumber->[$step-1];                      # Step name from step number
      my $save = $launch->stepSaveFile($projectName, $stepName);                # Save file
      last unless -e $save;                                                     # Have to start at the preceeding step if the required start file is missing for this step
      last if fileOutOfDate sub{1}, $save, $sourceFile;                         # Have to start at the preceeding step if the source file is newer than the start file
      $actual = $step-1;                                                        # Step we are going to restart at
      shift @steps;                                                             # Consider step as done
     }
    if (defined $actual)                                                        # Overlay saved project
     {$launch->loadProject($projectName, $actual);
     }
   }

  for my $step(@steps)                                                          # Each step in the conversion
   {$step->($launch, $projectName);
   }
 } # launchProject

sub log                                                                         #P Log a message during a launch
 {my ($launch, @message) = @_;                                                  # Launch, messages
  push @{$launch->logMessages}, [dateTimeStamp, @message];
 }

sub logRM                                                                       #P Remove log timestamps
 {my ($launch) = @_;
  shift @$_ for @{$launch->logMessages};
  return $launch->logMessages;
 }

#-------------------------------------------------------------------------------
# Export
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA          = qw(Exporter);
@EXPORT_OK    = qw(
);
%EXPORT_TAGS  = (all=>[@EXPORT, @EXPORT_OK]);

# podDocumentation

=pod

=encoding utf-8

=head1 Name

Dita::Conversion::Launch - Launch parallel processes to convert Xml files to
L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html>.

=head1 Synopsis

=head1 Description

Xml to L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> conversion utilities

The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.

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
 }

test unless caller;

1;
# podDocumentation
__DATA__
#Test::More->builder->output("/dev/null");                                      # Show only errors during testing - but this must be commented out for production
use Dita::Conversion::Projects;
use warnings FATAL=>qw(all);
use strict;
use Test::More tests=>2;

my $N = 1;                                                                      # Number of test files per launch

makePath(my $inDir  = q(in));                                                   # Create and clear folders
makePath(my $outDir = q(out));
clearFolder($inDir, 20);

sub catalogName  :lvalue                                                        # The Dita Xml catalog to use to validate topics. Assign the file name of the catalog on your computer to this lvalue method.
 {q(/home/phil/r/ge/dita/org.oasis-open.dita.v1_3/dtd/technicalContent/catalog.xml)
 }

sub catalogNameBM:lvalue                                                        # The Dita Xml catalog to use to validate bookmaps. Assign the file name of the catalog on your computer to this lvalue method.
 {q(/home/phil/dita/plugins/org.oasis-open.dita.v1_3/dtd/bookmap/catalog.xml)
 }

map {writeFile(fpe($inDir, $_, q(xml)), <<END)} 1..$N;                          # Create test files
<concept id="c$_">
  <title>Title</title>
  <conbody>
    <para>pppp</para>
  </conbody>
</concept>
END

my $projects = Dita::Conversion::Projects::loadFromFolder($inDir);              # Define project set

my $convertProject = sub                                                        # Convert a project
 {my ($project) = @_;
  my $x = $project->parse;

  $x->by(sub
   {my ($o) = @_;
    if ($o->at(qw(para)))
     {$o->crc(111);
      $o->change(q(p));
     }
    $project->lintTopic                                                         # Create topic and lint it
     (fpe($outDir, $project->name, q(xml)),
      $x->ditaTopic,                                                            # In Dita::Conversion
      catalogName);
   });
 };
sleep 1;                                                                        # Age the files a little so they appear older than the restart files

if (1)                                                                          # Single step conversion
 {my $l = Dita::Conversion::Launch::new
   (projects                 => $projects, preprocess=>sub{$_},
    convert                  => $convertProject,
    out                      => $outDir,
    maximumNumberOfProcesses => 1,
   );

  $l->launch("Start");
  $l->launch("Restart ",  $l->restart  = q(preprocess));
  $l->launch("Restart ",  $l->restart  = q(parse));
  $l->launch("Restart ",  $l->restart  = q(convert));

  is_deeply $l->logRM, [
  ["Launch", "Start", undef],
  ["save", 1, "preprocess"],
  ["save", 1, "parse"],
  ["save", 1, "convert"],
  ["Launch", "Restart ", "preprocess"],
  ["save", 1, "preprocess"],
  ["save", 1, "parse"],
  ["save", 1, "convert"],
  ["Launch", "Restart ", "parse"],
  ["load", 1, "preprocess"],
  ["save", 1, "parse"],
  ["save", 1, "convert"],
  ["Launch", "Restart ", "convert"],
  ["load", 1, "parse"],
  ["save", 1, "convert"],
];
 }

if (1)                                                                          # Multi-step conversion
 {my $l = Dita::Conversion::Launch::new
   (projects                 => $projects, preprocess=>sub{$_},
    convert                  =>
     [[aaa=>$convertProject],
      [bbb=>$convertProject],
     ],
    out                      => $outDir,
    maximumNumberOfProcesses => 1,
   );

  $l->launch;

  $l->launch("Restart ",  $l->restart  = q(preprocess));
  $l->launch("Restart ",  $l->restart  = q(parse));
  $l->launch("Restart ",  $l->restart  = q(aaa));
  $l->launch("Restart ",  $l->restart  = q(bbb));

  sleep 2;                                                                      # Touch and age files to force redo
  say STDERR qx(touch $inDir/1.xml);
  $l->launch("Restart after touch ",  $l->restart = q(bbb));

  is_deeply $l->logRM, [
  ["Launch", undef, undef],
  ["save", 1, "preprocess"],
  ["save", 1, "parse"],
  ["save", 1, "aaa"],
  ["save", 1, "bbb"],
  ["Launch", "Restart ", "preprocess"],
  ["save", 1, "preprocess"],
  ["save", 1, "parse"],
  ["save", 1, "aaa"],
  ["save", 1, "bbb"],
  ["Launch", "Restart ", "parse"],
  ["load", 1, "preprocess"],
  ["save", 1, "parse"],
  ["save", 1, "aaa"],
  ["save", 1, "bbb"],
  ["Launch", "Restart ", "aaa"],
  ["load", 1, "parse"],
  ["save", 1, "aaa"],
  ["save", 1, "bbb"],
  ["Launch", "Restart ", "bbb"],
  ["load", 1, "aaa"],
  ["save", 1, "bbb"],
  ["Launch", "Restart after touch ", "bbb"],
  ["save", 1, "preprocess"],
  ["save", 1, "parse"],
  ["save", 1, "aaa"],
  ["save", 1, "bbb"],
];
 }
