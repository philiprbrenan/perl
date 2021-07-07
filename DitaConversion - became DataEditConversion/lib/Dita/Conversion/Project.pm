#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Dita::Conversion:Project - Convert an xml file to Dita topics
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2018
#-------------------------------------------------------------------------------

package Dita::Conversion::Project;
our $VERSION = "20180602";
require v5.16;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump);
use Data::Edit::Xml::Lint;
use Data::Table::Text qw(:all);
use utf8;

#1 Methods                                                                      # Xml to  L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> conversion utilities.

my %projects;                                                                   # Prevent duplicate namea and number projects

sub new                                                                         #S Create a project to describe the conversion of a source file containing xml representing documentation into one or more L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> topics.
 {my $p = bless{@_};
  $p->number = keys %projects;
  my $n = $p->name;
  my $s = $p->sourceFile;

  confess "No name for project\n"            unless $n;
  confess "Duplicate project: $n\n"          if $projects{$n};
  confess "No source file for project: $n\n" unless $s;
  confess "Source file does not exist: $s\n" unless -e $s;

  $projects{$n} = $p;
 }

genLValueScalarMethods(q(name));                                                # Name of project.
genLValueScalarMethods(q(number));                                              # Number of the project.
genLValueScalarMethods(q(parse));                                               # Parse of the preprocessed source xml.
genLValueScalarMethods(q(preprocessedSource));                                  # Source xml as a string after preprocessing but before parsing.
genLValueScalarMethods(q(sourceFile));                                          # Input file containing the source xml.
genLValueScalarMethods(q(source));                                              # Source xml as a string.
genLValueScalarMethods(q(stepsProcessed));                                      # Hash of steps processed during a launch
genLValueScalarMethods(q(title));                                               # Title of the project.

sub lintTopic($$$$)                                                             # Write and lint topic recording the source file in the linted xml
 {my ($project, $file, $source, $catalog) = @_;                                 # Project, file to write to, source, DTD catalog file name
  my $l = Data::Edit::Xml::Lint::new();                                         # Write and lint topic
  $l->project = $project->name;
  $l->file    = $file;
  $l->source  = $source;
  $l->catalog = $catalog;
  $l->lint;
 }

1;
