#!/usr/bin/perl -I/home/phil/perl/cpan/DitaConversion/lib
#-------------------------------------------------------------------------------
# Dita::Conversion:Projects - Specify xml files to be converted to Dita
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2018
#-------------------------------------------------------------------------------

package Dita::Conversion::Projects;
our $VERSION = "20180601";
require v5.16;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess);
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use Dita::Conversion::Project;
use utf8;

#1 Methods                                                                      # Xml to  L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> conversion utilities.

sub loadFromFolder($)                                                           #S Create a project for each dita/ditamap/doc/xml file in and below the specified folder and return the projects created
 {my ($dir) = @_;                                                               # Folder of files
  my @f = searchDirectoryTreesForMatchingFiles($dir, qw(dita ditamap doc xml));
  my $p = {};                                                                   # Project hash
  for my $file(@f)
   {my (undef, $name, undef) = parseFileName($file);                            # Project name from file name
    $p->{$name} =                                                               # Create project definition
      Dita::Conversion::Project::new(name=>$name, sourceFile=>$file);
   }
  bless $p, "Dita::Conversion::Projects";                                       # Return hash of projects
 }
1;
__DATA__
sub matching($$)                                                                # Return a set of porjects that match a selection specification
 {my ($projects, $matching) = @_;                                               # Projects, selection
  my $selected = bless {};
  for my $project(values %$projects)
   {my $n = $project->name;                                                     # Project name
    my $s = $matching;                                                          # Projects selected for launching
    my $r = ref $s;
    $selected->{$n} = $project if sub
     {if ($r and $r =~ m(Regexp))                                                 # Projects selected by regular expression
       {return $n =~ m($s);
       }
      elsif ($r and $r =~ m(HASH))                                                # Projects selected by hash
       {return $s->{$n}
       }
      elsif ($r)                                                                  # Named project
       {return $r eq $n
       }
      1                                                                           # All projects  by default
     }->();
   }
  $selected
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

Dita::Project - Xml to L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> conversion utilities

=head1 Synopsis

A collection of methods useful for converting Xml representing documentation to
the
L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html>
standard.

  use Dita::Project;
  use Data::Table::Text qw(nws writeFile);
  use Test::More qw(no_plan);

  my $source = writeFile(q(a.xml), <<END);                                      # Create a source file containing xml
  <section>
    <title>How to do something</title>
    <para>Do the following:</para>
    <ol>
      <li>Read the book</li>
      <li>Apply the ideas</li>
      <li>consider the results</li>
    </ol>
  </section>
  END

  my $project = Dita::Project::new(name=>q(docbook), source=>$source);          # Create a project from the source file

  $project->parseSource;                                                        # Create an xml parse tree for the project

  $project->contextFreeConversionsFromDocBookToDita;                            # Do the obvious conversions to Dita

  $project->parse x= sub                                                        # Task specific edits
   {$project->listToSteps($_)                      if $_->at(qw(ol));           # Change list to steps
    $_->change(q(context))                         if $_->at(qw(p));            # Paragraph to context
    $_->next->wrapTo($_->lastSibling, q(taskbody)) if $_->at(qw(title));        # Wrap content in task body
    $_->change(q(task))->id = "t1"                 if $_->at(qw(section));      # Make the section a task
   };

   ok -p $project->parse eq <<END;                                              # Print the resulting parse tree
  <task id="t1">
    <title>How to do something</title>
    <taskbody>
      <context>Do the following:</context>
      <steps>
        <step>
          <cmd>Read the book</cmd>
        </step>
        <step>
          <cmd>Apply the ideas</cmd>
        </step>
        <step>
          <cmd>consider the results</cmd>
        </step>
      </steps>
    </taskbody>
  </task>
  END

  if (qx(xmllint -version) =~ m(\Axmllint: using libxml version)s)              # Lint if xmllint is available
   {$project->lintTopic(q(task.dita));                                          # Save and lint the topic to a file
    Dita::Project::waitLints;                                                   # Wait for lint to finish

    my $report = Dita::Project::reportLints(q(summary.txt), qw(. dita));        # Create lint report by examining all completed lints

    ok nws($report->print =~ s(on ....-..-.. at ..:..:..) ()gsr) eq nws(<<END); # Check lint report after normalizing whitespace and date

     100 % success converting 1 projects containing 1 xml files

     Passes: 1   Fails: 0    Errors: 0

     ProjectStatistics
       #  Percent   Pass  Fail  Total  Project
       1 100.0000      1     0      1  docbook

    END
   }

=head1 Description

Xml to L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> conversion utilities

The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Methods

Xml to  L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> conversion utilities.

=head2 new()

Create a project to describe the conversion of one source file containing xml representing documentation into one or more L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> topics.


Example:


  my $p = new(name=>q(a), source=>fpe($dir, 1, q(xml)));

  ok $p->source eq qq($dir/1.xml);


This is a static method and so should be invoked as:

  Dita::Project::new


=head2 name :lvalue

Name of project.


=head2 number :lvalue

Number of the project.


=head2 parse :lvalue

Parse of the project.


=head2 source :lvalue

Input file containing the source xml.


=head2 title :lvalue

Title of the project.


=head2 loadFolder($)

Create a project for each dita/ditamap/doc/xml file in and below the specified folder.

     Parameter  Description
  1  $dir       Folder

Example:


  map {writeFile(fpe($dir, $_, q(xml)), <<END)} 1..9;
  <concept id="c$_">
    <title>Title</title>
    <conbody>
      <para>pppp</para>
    </conbody>
  </concept>
  END

  loadFolder($dir);

  ok projects         == 9;

  ok project(1)->name == 1;

  is_deeply [matching(qr(1|2))],

  [bless({ name => 1, number => 0, source =>qq($dir/1.xml)}, "Dita::Project"),

  bless({ name => 2, number => 1, source =>qq($dir/2.xml)}, "Dita::Project"),


This is a static method and so should be invoked as:

  Dita::Project::loadFolder


=head2 projects()

List all the projects defined.


Example:


  map {writeFile(fpe($dir, $_, q(xml)), <<END)} 1..9;
  <concept id="c$_">
    <title>Title</title>
    <conbody>
      <para>pppp</para>
    </conbody>
  </concept>
  END

  loadFolder($dir);

  ok projects         == 9;


=head2 project($)

Details of a specific project.

     Parameter  Description
  1  $name      Name of the project

Example:


  map {writeFile(fpe($dir, $_, q(xml)), <<END)} 1..9;
  <concept id="c$_">
    <title>Title</title>
    <conbody>
      <para>pppp</para>
    </conbody>
  </concept>
  END

  loadFolder($dir);

  ok projects         == 9;

  ok project(1)->name == 1;


=head2 matching($)

List of projects whose names match the specified regular expression.

     Parameter  Description
  1  $re        Regular expression to select projects by name

Example:


  map {writeFile(fpe($dir, $_, q(xml)), <<END)} 1..9;
  <concept id="c$_">
    <title>Title</title>
    <conbody>
      <para>pppp</para>
    </conbody>
  </concept>
  END

  loadFolder($dir);

  ok projects         == 9;

  is_deeply [matching(qr(1|2))],

  [bless({ name => 1, number => 0, source =>qq($dir/1.xml)}, "Dita::Project"),

  bless({ name => 2, number => 1, source =>qq($dir/2.xml)}, "Dita::Project"),


=head2 parseSource($)

Parse a project from its source file and the resulting parse tree.

     Parameter  Description
  1  $project   Project

Example:


  ok -p $p->parseSource eq <<END;
  <concept id="c1">
    <title>Title</title>
    <conbody>
      <para>pppp</para>
    </conbody>
  </concept>
  END


=head2 topicTypeAndBody($$)

Topic type and corresponding body.

     Parameter  Description
  1  $project   Project
  2  $type      Type - concept; bookmap etc

Example:


  is_deeply [project(2)->topicTypeAndBody(q(concept))],

  ["concept", "Concept", "conbody"];


This is a static method and so should be invoked as:

  Dita::Project::topicTypeAndBody


=head2 xmlHeaders($$$)

Add xml headers for each document type to a string of xml.

     Parameter  Description
  1  $project   Project
  2  $type      Type - concept; bookmap etc
  3  $string    String of xml

Example:


  ok project(1)->xmlHeaders(qw(concept <a/>)) eq <<END;
  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE concept PUBLIC "-//OASIS//DTD DITA Concept//EN" "concept.dtd" []>
  <a/>
  END


=head2 catalogName()

The Dita Xml catalog to use to validate topics. Assign the file name of the catalog on your computer to this lvalue method.


=head2 catalogNameBM()

The Dita Xml catalog to use to validate bookmaps. Assign the file name of the catalog on your computer to this lvalue method.


=head2 lintTopic($$$)

Lint a topic by writing the optional xml to a file and invoking xmllint on the file.  If the xml is not specified the current parse tree is printed and so used as the source of the xml.

     Parameter  Description
  1  $project   Project
  2  $file      File
  3  $string    Optional string of xml else the current parse tree will be printed

Example:


  project(1)->lintTopic(fpe($dir, qw(out dita)),

  project(1)->xmlHeaders(qw(concept), <<END));
  <concept id="c1">
    <title>Title</title>
    <conbody>
      <p>ppp</p>
    </conbody>
  </concept>
  END

  waitLints;


=head2 lintBookMap($$$)

Lint a bookmap by writing the optional xml to a file and invoking xmllint on the file.  If the xml is not specified the current parse tree is printed and so used as the source of the xml.

     Parameter  Description
  1  $project   Project
  2  $file      File
  3  $string    Optional string of xml else the current parse tree will be printed

Example:


  project(1)->lintBookMap(fpe($dir, qw(bookmap dita)),

  project(1)->xmlHeaders(qw(bookmap), <<END));
  <bookmap>
    <booktitle>
      <mainbooktitle>Title</mainbooktitle>
    </booktitle>
    <chapter href="1.dita" navtitle="Project 1"/>
  </bookmap>
  END


=head2 waitLints()

Wait for all lints to finish.


Example:


  project(1)->lintTopic(fpe($dir, qw(out dita)),

  project(1)->xmlHeaders(qw(concept), <<END));
  <concept id="c1">
    <title>Title</title>
    <conbody>
      <p>ppp</p>
    </conbody>
  </concept>
  END

  project(1)->lintBookMap(fpe($dir, qw(bookmap dita)),

  project(1)->xmlHeaders(qw(bookmap), <<END));
  <bookmap>
    <booktitle>
      <mainbooktitle>Title</mainbooktitle>
    </booktitle>
    <chapter href="1.dita" navtitle="Project 1"/>
  </bookmap>
  END

  waitLints;


This is a static method and so should be invoked as:

  Dita::Project::waitLints


=head2 reportLints($@)

Report results of lints over selected folders and file extensions.

     Parameter                      Description
  1  $reportFile                    File to write report to
  2  @folderNamesAndFileExtensions  Folder names and file extensions to report over

Example:


  project(1)->lintTopic(fpe($dir, qw(out dita)),

  project(1)->xmlHeaders(qw(concept), <<END));
  <concept id="c1">
    <title>Title</title>
    <conbody>
      <p>ppp</p>
    </conbody>
  </concept>
  END

  project(1)->lintBookMap(fpe($dir, qw(bookmap dita)),

  project(1)->xmlHeaders(qw(bookmap), <<END));
  <bookmap>
    <booktitle>
      <mainbooktitle>Title</mainbooktitle>
    </booktitle>
    <chapter href="1.dita" navtitle="Project 1"/>
  </bookmap>
  END

  waitLints;


This is a static method and so should be invoked as:

  Dita::Project::reportLints


=head2 requiredCleanUpComment($$)

Required cleanup comment.

     Parameter  Description
  1  $id        Clean up id
  2  $string    Comment as a string

Example:


  ok requiredCleanUpComment(1, q(aaa)) eq  nws(<<END);
  <required-cleanup id="1">aaa</required-cleanup>
  END


This is a static method and so should be invoked as:

  Dita::Project::requiredCleanUpComment


=head2 requiredCleanUp($$)

Replace a node with a required cleanup node around the text of the replaced node.

     Parameter  Description
  1  $project   Project
  2  $node      Parse tree node

Example:


  ok nws(<<END) eq nws(-p $p->requiredCleanUp(Data::Edit::Xml::new(<<END2)));
  <required-cleanup id="a">&lt;a&gt;
    &lt;b&gt;bbb
    &lt;/b&gt;
  &lt;/a&gt;
  </required-cleanup>
  END


=head2 uniqueCounter($)

The next unique number in a set of counters.

     Parameter  Description
  1  $set       Counter set name

Example:


  ok uniqueCounter(q(a)) eq q(a1);

  ok uniqueCounter(q(a)) eq q(a2);

  ok uniqueCounter(q(b)) eq q(b1);


This is a static method and so should be invoked as:

  Dita::Project::uniqueCounter


=head2 titleToFileName($$$)

Create a unique file name from a title.

     Parameter  Description
  1  $project   Project
  2  $title     Title
  3  $ext       Desired extension - defaults to .dita

Example:


  ok project(1)->titleToFileName("title") eq q(title.dita);


=head2 convertListToSteps($$$)

Change ol/ul to steps.

     Parameter  Description
  1  $project   Project
  2  $o         List node in parse tree
  3  $s         "step" or "substep"

=head2 listToSteps($$$)

Change B<ol/ul> to B<steps>.

     Parameter  Description
  1  $project   Project
  2  $o         List node in parse tree
  3  $s         "step" or "substep"

Example:


  ok -p project(1)->listToSteps(Data::Edit::Xml::new(<<FIN)) eq <<END;
  <ol>
    <li>command 1</li>
    <li>command 2</li>
  </ol>
  FIN
  <steps>
    <step>
      <cmd>command 1</cmd>
    </step>
    <step>
      <cmd>command 2</cmd>
    </step>
  </steps>
  END


=head2 stepsToList($$)

Change B<steps> to B<ol>.

     Parameter  Description
  1  $project   Project
  2  $o         Steps node in parse tree

Example:


  ok -p project(1)->stepsToList(Data::Edit::Xml::new(<<FIN)) eq <<END;
  <steps>
    <step><cmd>command 1</cmd></step>
    <step><cmd>command 2</cmd></step>
  </steps>
  FIN
  <ol>
    <li>command 1</li>
    <li>command 2</li>
  </ol>
  END


=head2 contextFreeConversionsFromDocBookToDita($)

Make obvious changes to the parse tree of a DocBook file to make it look more like L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html>.

     Parameter  Description
  1  $project   Project

Example:


  ok -p $p->parseSource eq <<END;
  <concept id="c1">
    <title>Title</title>
    <conbody>
      <para>pppp</para>
    </conbody>
  </concept>
  END

  $p->parseSource;

  ok -p $p->contextFreeConversionsFromDocBookToDita eq <<END;
  <concept id="c1">
    <title>Title</title>
    <conbody>
      <p>pppp</p>
    </conbody>
  </concept>
  END



=head1 Index


1 L<catalogName|/catalogName>

2 L<catalogNameBM|/catalogNameBM>

3 L<contextFreeConversionsFromDocBookToDita|/contextFreeConversionsFromDocBookToDita>

4 L<convertListToSteps|/convertListToSteps>

5 L<lintBookMap|/lintBookMap>

6 L<lintTopic|/lintTopic>

7 L<listToSteps|/listToSteps>

8 L<loadFolder|/loadFolder>

9 L<matching|/matching>

10 L<name|/name>

11 L<new|/new>

12 L<number|/number>

13 L<parse|/parse>

14 L<parseSource|/parseSource>

15 L<project|/project>

16 L<projects|/projects>

17 L<reportLints|/reportLints>

18 L<requiredCleanUp|/requiredCleanUp>

19 L<requiredCleanUpComment|/requiredCleanUpComment>

20 L<source|/source>

21 L<stepsToList|/stepsToList>

22 L<title|/title>

23 L<titleToFileName|/titleToFileName>

24 L<topicTypeAndBody|/topicTypeAndBody>

25 L<uniqueCounter|/uniqueCounter>

26 L<waitLints|/waitLints>

27 L<xmlHeaders|/xmlHeaders>

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read, use,
modify and install.

Standard L<Module::Build> process for building and installing modules:

  perl Build.PL
  ./Build
  ./Build test
  ./Build install

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
#Test::More->builder->output("/dev/null");                                      # Show only errors during testing - but this must be commented out for production
use warnings FATAL=>qw(all);
use strict;
use Test::More tests=>3;

makePath(my $dir = q(yyy));
clearFolder($dir, 20);
map {writeFile(fpe($dir, $_, q(xml)), <<END)} 1..9;                             #TloadFolder #Tprojects #Tproject #Tmatching
<a/>
END

my $p = Dita::Conversion::Projects::loadFromFolder($dir);                                                       #TloadFolder #Tprojects #Tproject #Tmatching

is_deeply $p, {
  1 => bless({ name => 1, number => 0, source => "yyy/1.xml" }, "Dita::Conversion::Project"),
  2 => bless({ name => 2, number => 1, source => "yyy/2.xml" }, "Dita::Conversion::Project"),
  3 => bless({ name => 3, number => 2, source => "yyy/3.xml" }, "Dita::Conversion::Project"),
  4 => bless({ name => 4, number => 3, source => "yyy/4.xml" }, "Dita::Conversion::Project"),
  5 => bless({ name => 5, number => 4, source => "yyy/5.xml" }, "Dita::Conversion::Project"),
  6 => bless({ name => 6, number => 5, source => "yyy/6.xml" }, "Dita::Conversion::Project"),
  7 => bless({ name => 7, number => 6, source => "yyy/7.xml" }, "Dita::Conversion::Project"),
  8 => bless({ name => 8, number => 7, source => "yyy/8.xml" }, "Dita::Conversion::Project"),
  9 => bless({ name => 9, number => 8, source => "yyy/9.xml" }, "Dita::Conversion::Project"),
 };

is_deeply $p->matching({1=>1, 2=>1}), {
  1 => bless({ name => 1, number => 0, source => "yyy/1.xml" }, "Dita::Conversion::Project"),
  2 => bless({ name => 2, number => 1, source => "yyy/2.xml" }, "Dita::Conversion::Project"),
 };

clearFolder($dir, 20);
rmdir $dir;

ok !-d $dir;
