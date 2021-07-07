#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/ -I/home/phil/perl/cpan/DataEditXmlLint/lib/
#-------------------------------------------------------------------------------
# Dita::Project - A framework for converting Xml documents to the Dita standard.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2018
#-------------------------------------------------------------------------------
# podDocumentation
# Project should point  back to conversion
# Methods to create output files and report files.

package Dita::Conversion;
our $VERSION = "20180729";
require v5.16;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump);
use Data::Edit::Xml;
use Data::Edit::Xml::Lint;
use Data::Table::Text qw(:all);
use Storable;
use utf8;

#D1 Construction                                                                # Construct and perform the conversion of a number of Xml documents to dita in parallel.

sub projectClass                                                                #P Class for project details
 {q(Dita::Conversion::Project)
 }

sub inOption                                                                    #P Input folder option.
 {my ($conversion) = @_;
  return $conversion->in if $conversion->can(q(in));
  fpd($conversion->home, q(in))
 }

sub outOption                                                                    #P Input folder option.
 {my ($conversion) = @_;
  return $conversion->in if $conversion->can(q(out));
  fpd($conversion->home, q(out))
 }

sub reportsOption                                                                    #P Input folder option.
 {my ($conversion) = @_;
  return $conversion->reports if $conversion->can(q(reports));
  fpd($conversion->home, q(reports))
 }

sub maximumNumberOfProcessesToStartOption                                                                    #P Input folder option.
 {my ($conversion) = @_;
  return $conversion->maximumNumberOfProcessesToStart
    if $conversion->can(q(maximumNumberOfProcessesToStartOption));
  8
 }

sub convert                                                                     #S Perform the conversion of a number of Xml documents to dita in parallel.
 {my ($conversion) = @_;

  Data::Edit::Xml::Lint::clear(outOption, reportsOption);                       # Clear prior run

  $conversion->defineProjects                                                   # Define the projects associated with this conversion
   (searchDirectoryTreesForMatchingFiles($c->inOption));

  if (1)                                                                        # Number projects
   {my $N = 0;
    for my $p(sort keys %{$c->projects})
     {my $project = $c->projects->{$p};
      $project->number = ++$N;
     }
   }

  if (1)                                                                        # Convert each project in parallel
   {my %pids;
    for my $p(sort keys %{$c->projects})                                        # Convert each project
     {my $project = $c->projects->{$p};
      startProcess
       {$c->convertProject($project);                                           # Convert project
        Data::Edit::Xml::Lint::waitAllProcesses;                                # Wait for any lints to complete
       } %pids,
        &maximumNumberOfProcessesToStartOption;
     }
    waitForAllStartedProcessesToFinish(%pids);
   }


  if (my $out = $c->outOption)                                                  # Report results
   {if (-d $out)                                                                # Check there is some output
     {if (my $report = eval {Data::Edit::Xml::Lint::report($out)})              # Report results
       {my $r = $c->report = $report->print;
        writeFile(fpe(&reportsOption, qw(summary text)), $r);
       }
     }
   }

  if (defined &postProcess)                                                     # Perform post processing
   {$c->postProcess;
   }

  $c
 }

sub project($$$)                                                                # Define a project.  A project represents the conversion of one input file. The definition of a project can be extended by defining L<additional project attributes|/additionalProjectAttributes>.
 {my ($conversion, $name, $file) = @_;                                          # Conversion,  project name, project source file
  $conversion->projects->{$name} =                                              # Create a project description
    bless{name=>$name, source=>$file}, projectClass;
 }

sub linter($$)                                                                  # Create a L<Data::Edit::Xml::Linter> to validate an output topic file created during the conversion of a project.
 {my ($conversion, $project) = @_;                                              # Conversion, project
  my $l       = Data::Edit::Xml::Lint::new();
  $l->project = $project->name;
  $l->catalog = $conversion->catalog;
  $l
 }

sub createSampleInputFiles                                                      #P Create sample input files for testing.
 {my $in = q(in);
  for(1..8)
   {writeFile(fpe($in, $_, q(xml)), <<END);
<concept id="c$_">
  <title>Concept $_</title>
  <conbody id="$_"/>
</concept>
END
   }
 }

#D
#createSampleInputFiles; exit;                                                   # Create the sample input files if they are not already there

#-------------------------------------------------------------------------------
# Export
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA          = qw(Exporter);
@EXPORT_OK    = qw(
convert
);
%EXPORT_TAGS  = (all=>[@EXPORT, @EXPORT_OK]);

# podDocumentation

=pod

=encoding utf-8

=head1 Name

Dita::Project - A framework for converting Xml documents to the L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html>
standard.

=head1 Synopsis

Dita::Project - A framework for converting Xml documents to the L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html>
standard.

  use Dita::Conversion;

=head1 Description

A framework for converting Xml documents to the L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html>

The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Construction

Construct and perform the conversion of a number of Xml documents to dita in parallel.

=head2 convert()

Perform the conversion of a number of Xml documents to dita in parallel.


Example:


  convert;                                                                        # Perform the conversion


This is a static method and so should be invoked as:

  Dita::Conversion::convert


=head2 project($$$)

Define a project.  A project represents the conversion of one input file. The definition of a project can be extended by defining L<additional project attributes|/additionalProjectAttributes>.

     Parameter    Description
  1  $conversion  Conversion
  2  $name        Project name
  3  $file        Project source file

Example:


     {$conversion->project(fn($file), $file);                                     # Create a project for each input file


=head2 linter($$)

Create a L<Data::Edit::Xml::Linter> to validate an output topic file created during the conversion of a project.

     Parameter    Description
  1  $conversion  Conversion
  2  $project     Project

Example:


    if (my $l = $conversion->linter($project))                                    # Lint the results of the conversion.


=head1 Usage

Usage example: the code in this section when aggregated together forms a complete example.

=head2 additionalProjectAttributes()

Define additional project attributes and their meanings.  This allows the user to attach more information to the definition of a project for use in L<convertProject|/convertProject>.


Example:


  sub additionalProjectAttributes
   {{author=>q(Name of the author of the document),
     title =>q(Title of the document),
    }
   }


You can provide an implementation of this method as B<Dita::Conversion::additionalProjectAttributes> if you wish to override the default processing.

=head2 in()

Source files folder. Each project is associated with one input file contained in this folder.


Example:


  sub in
   {q(in)
   }


You can provide an implementation of this method as B<Dita::Conversion::in> if you wish to override the default processing.

=head2 out()

Converted files. Output topic files created by the conversion will be placed in this folder.


Example:


  sub out
   {q(out)
   }


You can provide an implementation of this method as B<Dita::Conversion::out> if you wish to override the default processing.

=head2 reports()

Reports describing the conversion will be placed in this folder.


Example:


  sub reports
   {q(reports)
   }


You can provide an implementation of this method as B<Dita::Conversion::reports> if you wish to override the default processing.

=head2 catalog()

Lint created topics with DTDs in this catalog.


Example:


  sub catalog
   {q(/home/phil/dita/plugins/org.oasis-open.dita.v1_3/dtd/technicalContent/catalog.xml)
   }


You must supply an implementation of this method as B<Dita::Conversion::catalog>.

=head2 defineProjects($@)

Define projects. A project must be created for each file in L<in|/in> to be converted giving at a minimum the (much shorter) name of the project associated with the input file.  However, you may add any of the additional attributes describing a project defined using L<additionalProjectattributes|/additionalProjectattributes> to extend this definition is you wish.

     Parameter    Description
  1  $conversion  Conversion
  2  @file        Files in folder named by in()

Example:


  sub defineProjects($@)
   {my ($conversion, @file) = @_;                                                 # Conversion, files in folder named by in()
    for my $file(@file)                                                           # Each input file
     {$conversion->project(fn($file), $file);                                     # Create a project for each input file
     }
   }


You must supply an implementation of this method as B<Dita::Conversion::defineProjects>.

=head2 convertProject($$)

Convert a project. Each project conversion converts one L<input|/in> file into zero or more topic files.

     Parameter    Description
  1  $conversion  Conversion
  2  $project     Project description

Example:


  sub convertProject($$)
   {my ($conversion, $project) = @_;                                              # Conversion, project description

    my $x = Data::Edit::Xml::new($project->source);                               # Parse source file
    $x->by(sub                                                                    # Convert source
     {my ($o) = @_;
      if ($o->at_conbody and $o->idX % 2 == 1)
       {$o->change_taskbody;
       }
     });

    if (my $l = $conversion->linter($project))                                    # Lint the results of the conversion.
     {$l->file   = fpe(out, $project->name, q(dita));
      $l->source = $x->prettyStringDitaHeaders;
      $l->lint;
     }
   }


You must supply an implementation of this method as B<Dita::Conversion::convertProject>.

=head2 postProcess()

Actions to perform after the conversion of each project such as reusing files or targeting links.


Example:


  sub postProcess
   {my ($conversion) = @_;                                                        # Conversion
    say STDERR $conversion->report;
    my $r = $conversion->report;
       $r =~ s(\d\d\d\d-\d\d-\d\d at \d\d:\d\d:\d\d) ()gs;
       $r =~ s(CompressedErrorMessagesByCount:.+\Z)  ()gs;
       $r = nws($r);
    ok $r eq &data;
    #say STDERR " {q($r)";
   }


You must supply an implementation of this method as B<Dita::Conversion::postProcess>.


=head1 Private Methods

=head2 projectClass()

Class for project details


=head2 inOption()

Input folder option.


=head2 outOption()

Output folder option.


=head2 reportsOption()

Reports folder option.


=head2 createSampleInputFiles()

Create sample input files.



=head1 Index


1 L<additionalProjectAttributes|/additionalProjectAttributes>

2 L<catalog|/catalog>

3 L<convert|/convert>

4 L<convertProject|/convertProject>

5 L<createSampleInputFiles|/createSampleInputFiles>

6 L<defineProjects|/defineProjects>

7 L<in|/in>

8 L<inOption|/inOption>

9 L<linter|/linter>

10 L<out|/out>

11 L<outOption|/outOption>

12 L<postProcess|/postProcess>

13 L<project|/project>

14 L<projectClass|/projectClass>

15 L<reports|/reports>

16 L<reportsOption|/reportsOption>

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Dita::Conversion

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
 }

test unless caller;

1;
# podDocumentation
__DATA__
#1 Usage                                                                        # Usage example: the code in this section when aggregated together forms a complete example.
package Dita::Conversion::Test;
use warnings FATAL=>qw(all);
use strict;
use Data::Edit::Xml;
use Dita::Conversion;
use Test::More tests=>1;

Test::More->builder->output("/dev/null")                                        # Show only errors during testing
  if ((caller(1))[0]//'Dita::Conversion') eq "Dita::Conversion";

sub additionalProjectAttributes                                                 #r Define additional project attributes and their meanings.  This allows the user to attach more information to the definition of a project for use in L<convertProject|/convertProject>.
 {{author=>q(Name of the author of the document),
   title =>q(Title of the document),
  }
 }

sub home                                                                        #R Hone folder for conversion.
 {q(./)
 }

sub catalog                                                                     #R Lint created topics with DTDs in this catalog.
 {q(/home/phil/dita/plugins/org.oasis-open.dita.v1_3/dtd/technicalContent/catalog.xml)
 }

sub maximumNumberOfProcessesToStart                                             #r Maximum number of processes to start.
 {8
 }

sub defineProjects($@)                                                          #R Define projects. A project must be created for each file in L<in|/in> to be converted giving at a minimum the (much shorter) name of the project associated with the input file.  However, you may add any of the additional attributes describing a project defined using L<additionalProjectattributes|/additionalProjectattributes> to extend this definition is you wish.
 {my ($conversion, @file) = @_;                                                 # Conversion, files in folder named by in()
  for my $file(@file)                                                           # Each input file
   {$conversion->project(fn($file), $file);                                     # Create a project for each input file #Tproject
   }
 }

sub convertProject($$)                                                          #R Convert a project. Each project conversion converts one L<input|/in> file into zero or more topic files.
 {my ($conversion, $project) = @_;                                              # Conversion, project description

  my $x = Data::Edit::Xml::new($project->source);                               # Parse source file
  $x->by(sub                                                                    # Convert source
   {my ($o) = @_;
    if ($o->at_conbody and $o->idX % 2 == 1)
     {$o->change_taskbody;
     }
   });

  if (my $l = $conversion->linter($project))                                    # Lint the results of the conversion. #Tlinter
   {$l->file   = fpe(outOption, $project->name, q(dita));
    $l->source = $x->prettyStringDitaHeaders;
    $l->lint;
   }
 }

convert(bless {});                                                                        # Perform the conversion #Tconvert

sub postProcess                                                                 #R Actions to perform after the conversion of each project such as reusing files or targeting links.
 {my ($conversion) = @_;                                                        # Conversion
  if (my $r = $conversion->report)
   {$r =~ s(\d\d\d\d-\d\d-\d\d at \d\d:\d\d:\d\d) ()gs;
    $r =~ s(CompressedErrorMessagesByCount:.+\Z)  ()gs;
    $r = nws($r);
    ok $r eq &testReport;
   }
  else
   {confess "No report available";
   }
 }

sub testReport                                                                  # The summary line from Data::Edit::Xml::Lint
 {q(50 % success. Projects: 4+4=8. Files: 4+4=8. Errors: 3,12 On CompressedErrorMessagesByCount (at the end of this file): 3 FailingFiles : 4 PassingFiles : 4 FailingProjects: 4 PassingProjects: 4 FailingProjects: 4 # Percent Pass Fail Total Project 1 0.0000 0 1 1 1 2 0.0000 0 1 1 3 3 0.0000 0 1 1 5 4 0.0000 0 1 1 7 PassingProjects: 4 # Files Project 1 1 2 2 1 4 3 1 6 4 1 8 FailingFiles: 4 Files that failed to pass lint by number of compressed errors # Errors Project File 1 3 1 ./out/1.dita 2 3 3 ./out/3.dita 3 3 5 ./out/5.dita 4 3 7 ./out/7.dita)
 }
