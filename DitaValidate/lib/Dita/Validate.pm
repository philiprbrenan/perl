#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/ -I/home/phil/perl/cpan/DitaValidate/lib/
#-------------------------------------------------------------------------------
# Check that the next XML tag is valid under L<dita>
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc, 2018-2020
#-------------------------------------------------------------------------------
# podDocumentation
package Dita::Validate;
our $VERSION = 20200424;
use v5.26;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use Dita::Validate::SSV;

my $ssv   = genHash q(DitaValidateSSVssv),   Dita::Validate::SSV::ssv->%*;
my $codes = genHash q(DitaValidateSSVcodes), Dita::Validate::SSV::codes->%*;

#D1 Methods                                                                     # Check which tags are permissible or required next in a sequence of xml tags expected to conform to the dita standard without resorting to either the L<DitaOT> or L<xmllint>.

sub ditaTags                                                                    #S List of dita tags which have DTD definitions
 {sort keys $ssv->all->%*
 }

sub ditaTag($)                                                                  #S Whether a tag is a dita tag
 {my ($tag) = @_;                                                               # Tag
  $ssv->all->{$tag}
 }

sub ditaEmptyTag($)                                                             #S Whether a tag is always empty tag
 {my ($tag) = @_;                                                               # Tag
  $ssv->mustBeEmpty->{$tag}
 }

sub ditaCanBeEmptyTag($)                                                        #S Whether a tag can be empty
 {my ($tag) = @_;                                                               # Tag
  $ssv->canBeEmpty->{$tag}
 }

sub ditaOpenTag($)                                                              #S Whether a tag is an open tag
 {my ($tag) = @_;                                                               # Tag
  $tag =~ m(\Adraft-comment)i or $ssv->open->{$tag}                             # Known open tags
 }

sub ditaStartTag($)                                                             #S Whether a tag can start a topic
 {my ($tag) = @_;                                                               # Tag
  $ssv->start->{$tag}
 }

sub children($)                                                                 #S {tag} = 1 : children of the specified parent
 {my ($parent) = @_;                                                            # The name of the tag to test
  $ssv->children->{$parent}
 }                                                                              # True if this tag can be empty

sub childOf($$)                                                                 #S Whether the specified tag is a child of the specified parent
 {my ($tag, $parent) = @_;                                                      # The name of the tag to test, the possible parent
  $ssv->children->{$parent}{$tag}
 }

sub descendants($)                                                              #S {descendant} = 1 : descendants that can appear under a parent tag.
 {my ($parent) = @_;                                                            # Parent tag
  if (defined(my $t = $ssv->subTags->{start}{$parent}))                         # Index of table of reachable children
   {return $ssv->subTags->{end}[$t]                                             # Presence of child in table of reachable children
   }
  undef
 }

sub descendantOf($$)                                                            #S Whether the child can be reached by a path starting at the parent
 {my ($child, $parent) = @_;                                                    # Child, parent
  if (defined(my $t = $ssv->subTags->{start}{$parent}))                         # Index of table of reachable children
   {return $ssv->subTags->{end}[$t]{$child}                                     # Presence of child in table of reachable children
   }
  undef
 }                                                                              # True if this tag can be empty

sub tagsNext($$)                                                                #S [tag ...] that may follow after B<$tag> under B<$parent> else L<undef> if $tag cannot appear under $parent.
 {my ($tag, $parent) = @_;                                                      # The name of the child tag, the name of a parent tag
  my $t = $ssv->next->{$parent}{$tag};                                          # Transition table
  defined($t) ? $ssv->possibilities->[$t] : undef
 }

sub tagNext($$$)                                                                #SI True if the B<$test> tag may follow B<$tag> under B<$parent>, else L<undef>.
 {my ($test, $tag, $parent) = @_;                                               # Tag to test, current tag, parent tag
  my $p = tagsNext($tag, $parent);                                              # Possibilities
  $p and $$p{$test}
 }

sub tagsPrev($$)                                                                #S [tag ...] that may appear before B<$tag> under B<$parent> else L<undef> if $tag cannot appear under $parent.
 {my ($tag, $parent) = @_;                                                      # Child tag, parent tag
  my %p;
  for my $t(sort keys $ssv->children->%*)                                       # Possible children of this parent
   {$p{$t}++ if tagNext($tag, $t, $parent);                                     # Transitioning on this tag produces the desired target
   }
  keys %p ? [sort keys %p] : undef
 }

sub tagPrev($$$)                                                                #SI True if the B<$prev> tag may appear before B<$tag> under B<$parent>, else L<undef>.
 {my ($test, $tag, $parent) = @_;                                               # Tag to test, current tag, parent tag
  my $p = tagsPrev($tag, $parent);                                              # Possibilities
  $p and scalar(grep{$_ eq $test} @$p) == 1
 }

sub tagsFirst($)                                                                #S [tag ...] that can be first under B<$parent>.
 {my ($parent) = @_;                                                            # Parent tag
  my $t = $ssv->first->{$parent};
  defined($t) ? $ssv->possibilities->[$t] : undef
 }

sub tagFirst($$)                                                                #S True if B<$test> may be first under B<$parent> else L<undef>.
 {my ($test, $parent) = @_;                                                     # Test tag, parent tag
  my $p = $ssv->first->{$parent};                                               # Possibilities table
  defined($p) and $ssv->possibilities->[$p]->{$test}
 }

sub tagsLast($)                                                                 #S [tag ...] that can be last under B<$parent>.
 {my ($parent) = @_;                                                            # Parent tag
  my $t = $ssv->last->{$parent};
  defined($t) ? [sort keys %$t] : undef
 }

sub tagLast($$)                                                                 #S True if B<$test> may be last under B<$parent> else L<undef>.
 {my ($test, $parent) = @_;                                                     # Test tag, parent tag
  my $p = tagsLast($parent);                                                    # Possibilities
  $p and scalar(grep{$_ eq $test} @$p) == 1
 }

sub tagCanBeEmpty($)                                                            #S Whether a tag can be empty or not.
 {my ($parent) = @_;                                                            # The name of the tag to test
  $ssv->canBeEmpty->{$parent}
 }                                                                              # True if this tag can be empty

sub tagsThatFitBetween($$$)                                                     #S [tag, B<fitAfter>, B<fitBefore>] that will fit after the B<$prev> tag and before the B<$next> tag under B<$parent>. If $prev is L<undef> then the first tags permissible under $parent will have B<fitAfter> set to true. If $next is L<undef> then last tags permissible under $parent will have B<fitBefore> set to true.
 {my ($prev, $next, $parent) = @_;                                              # Previous tag, next tag, parent tag
  my %t = map{$_=>[]} keys children($parent)->%*;                               # Children of parent

  if (my $p = $prev ? tagsNext($prev, $parent) : tagsFirst($parent))
   {map {$t{$_}[0] = 1} keys %$p
   }

  if (my $p = $next ? tagsPrev($next, $parent) : tagsLast($parent))
   {$t{$_}[1] = 1 for @$p
   }

  [map{[$_, $t{$_}->@* ]} sort keys %t]
 } # tagsThatFitBetween

sub tagMustBeFirst($)                                                           #S The name of the one tag that must be present first under B<$parent> or B<undef> if there is no such unique tag.
 {my ($parent) = @_;                                                            # The name of the parent tag
  my $f = $ssv->first->{$parent};
  if (defined $f)
   {my $t = $ssv->possibilities->[$f];
    if (keys %$t == 1)
     {my ($n) = keys %$t;
      return $n                                                                 # This tag must come next because it is the only possibility and the previous tag cannot be last
     }
   }
  undef
 }

sub tagMustBeNext($$)                                                           #S The name of the one tag that must follow the specified B<$tag> under B<$parent> or B<undef> if there is no such tag.
 {my ($tag, $parent) = @_;                                                      # Child tag, parent tag
  if (!$ssv->last->{$parent}{$tag})                                             # This tag is not final so a following tag is required
   {my $n = $ssv->next->{$parent}{$tag};
    if (defined $n)
     {my $t = $ssv->possibilities->[$n];
      if (keys %$t == 1)
       {my ($n) = keys %$t;
        return $n                                                               # This tag must come next because it is the only possibility and the previous tag cannot be last
       }
     }
   }
  undef
 }

sub tagMustBeLast($)                                                            #S The name of the one tag that must be present last under B<$parent> or B<undef> if there is no such unique tag.
 {my ($parent) = @_;                                                            # The name of the parent tag
  my $f = $ssv->last->{$parent};
  if (defined $f)
   {my @t = keys %$f;
    return $t[0] if @t == 1
   }
  undef
 }

sub pathsToDescendantFromParent($;$)                                            #S [[tag, ...], ...] paths to the specified B<$descendant> from the B<$parent>. If $parent is B<undef> then all teh L<dita> starttags are tried.
 {my ($descendant, $parent) = @_;                                               # Descendant tag, parent tag
  my @paths;                                                                    # Path to descendant from parent

  my sub check($@)                                                              # Check paths from current level
   {my ($parent, @context) = @_;                                                # Current parent, current context of parent
    return if ditaOpenTag($parent);                                             # These tags can include anything so they are not included to prevent them from including everything else automatically

    if (my $c = children($parent))                                              # Children we could explore next
     {for my $tag(sort keys %$c)                                                # Each possible child
       {if ($tag eq $descendant)                                                # Save path if it terminates on the specified descendant
         {push @paths, join ' ', $tag, $parent, @context;
         }
        elsif (descendantOf($descendant, $tag))                                 # Possible to descend from tag to descendant
         {my sub directChildOfAncestor                                          # Whether the parent could have been a direct child of any ancestor node other than the first one - if it is then it is pointless expanding it at this lower level when it will be (or was) expanded at the upper level.
           {for my $ancestor(@context)
             {return 1 if childOf($tag, $ancestor);
             }
            undef
           };

          if (!directChildOfAncestor)                                           # Could not have explored this tag from an upper level
           {__SUB__->($tag, $parent, @context);
           }
         }
       }
     }
   };

  if ($parent)                                                                  # Using supplied parent
   {check $parent                                                               # Start at the parent
   }
  else                                                                          # Any start tag as a parent
   {for my $start(sort keys $ssv->start->%*)
     {check $start;
     }
   }


  return [sort @paths];                                                         # Paths in alphabetical order
# [sort {cmpArrays($a, $b)} @paths]                                             # Paths in alphabetical order
 }

sub descendantsThatFitBetween($$$)                                              #S {tag} = 1:  descendants that could fit at some level after the B<$prev> tag and before the B<$next> tag under B<$parent>. If $prev is L<undef> then the first tags permissible under $parent will have B<fitAfter> set to true. If $next is L<undef> then last tags permissible under $parent will have B<fitBefore> set to true.
 {my ($prev, $next, $parent) = @_;                                              # Previous tag, next tag, parent tag
  my $tags = tagsThatFitBetween($prev, $next, $parent);                         # All the possible tags under the parent

  my %descendants;
  for my $t(map {$$_[0]} @$tags)                                                # All descendants of tags that could come between
   {next if ditaOpenTag $t;
    $descendants{$_}++ for sort keys descendants($t)->%*
   }

  \%descendants                                                                 # Return descendants
 } # descendantsThatFitBetween

sub pathsFromParentToChild($$)                                                  #S [[tag, ...], ...] ways under parent that reach the specified child or if no child is specified to reach a child that can be last.
 {my ($parent, $child) = @_;                                                    # Parent, child tag
  my %seen;                                                                     # Children (alpha) and expansion tables (numeric) already seen
  my @paths;                                                                    # Paths under the parent that reach the specified child

  my sub check($@)
   {my ($tag, @path) = @_;                                                      # Tag, path to tag
#   next if ditaOpenTag $tag;                                                   # Does this make any difference?
    if (!$seen{$tag}++)
     {if ($tag eq $child)                                                       # Save path if it terminates on the specified child
       {push @paths, [reverse @_]
       }
      else
       {my $n = $ssv->next->{$parent}{$tag};
        if (defined($n) and !$seen{$n}++)                                       # Have not expanded this possibility before
         {map{__SUB__->($_, @_)} sort keys $ssv->possibilities->[$n]->%*;       # Check children in expansion
          delete $seen{$n}
         }
       }
      delete $seen{$tag}
     }
   };

  if (defined(my $f = $ssv->first->{$parent}))                                  # Start on parent
   {check($_) for sort keys $ssv->possibilities->[$f]->%*                       # Tags that can come first under a parent
   }

  [sort {join(' ', @$a) cmp join(' ', @$b)} @paths]                             # Paths in alphabetical order
 }

sub pathsUnderParent($)                                                         #S [[tag, ...], ...] ways under parent to reach an end tag
 {my ($parent) = @_;                                                            # Parent
  return undef if ditaOpenTag      ($parent);                                   # No  chain possible
  return []    if ditaCanBeEmptyTag($parent);                                   # Any chain possible

  return [qw(year month day)] if $parent =~ m(\A(completed|started)\Z);         # The few cases where multiple paths are possible under a parent
  return [qw(year month day)] if $parent =~ m(\A(completed|started)\Z);
  return [qw(data)]           if $parent =~ m(\Adata-about\Z);
  return [qw(steps)]          if $parent =~ m(\Aremedy\Z);

  if (my $f = tagsFirst($parent))                                               # Single tag path
   {my @path;
    for my $tag(sort keys %$f)
     {push @path, [$tag] if tagLast($tag, $parent) and $tag !~ m(\APCDATA\Z)i;
     }
    confess "More than one path" if @path > 1;
    return $path[0] if @path;
   }

  my %seen;                                                                     # Children (alpha) and expansion tables (numeric) already seen
  my @paths;                                                                    # Paths under the parent that reach the specified child

  my sub check($@)
   {my ($tag, @path) = @_;                                                      # Tag, path to tag
    if (!$seen{$tag}++)
     {if (tagLast($tag, $parent))                                               # Save path if it terminates on the specified child
       {push @paths, [reverse @_];
       }
      else
       {my $n = $ssv->next->{$parent}{$tag};
        if (defined($n) and !$seen{$n}++)                                       # Have not expanded this possibility before
         {map{__SUB__->($_, @_)} sort keys $ssv->possibilities->[$n]->%*;       # Check children in expansion
          delete $seen{$n}
         }
       }
      delete $seen{$tag}
     }
   };

  if (defined(my $f = $ssv->first->{$parent}))                                  # Start on parent
   {check($_) for sort keys $ssv->possibilities->[$f]->%*                       # Tags that can come first under a parent
   }

  my %paths;
  for my $path(@paths)                                                          # Compress each path and remove duplicates
   {shift @$path while @$path > 1 and tagFirst $$path[1],  $parent;             # Remove iterated first tags
    pop   @$path while @$path > 1 and tagLast  $$path[-2], $parent;             # Remove iterated last tags
    s/\APCDATA\Z/text/ for @$path;                                              # Map PCDATA to text
    for   my $i(0..@$path-3)
     {next unless $$path[$i];
      for my $j($i+2..@$path-1)
       {next unless $$path[$j];
        if (tagNext($$path[$j], $$path[$i], $parent))
         {$$path[$j] = undef
         }
       }
     }
    my @short = grep {defined} @$path;
    $paths{join ' ', @short} = [@short];
   }

  confess "More than one path" if keys %paths > 1;
  my ($path) = keys %paths;
  return $paths{$path} if $path;
  undef
 } # pathsUnderParent

sub initialChildChainUnderParent($)                                             #S tags, ... required under an empty parent
 {my ($parent) = @_;                                                            # Parent
  my $skipRe   = qr(\A(ANY|EMPTY)\Z);                                           # Skip these catagories
  my @chain;                                                                    # Children

  my $first  = tagsFirst($parent);                                              # Tags that can come first
  return () unless $first and keys %$first == 1;                                # Just one first child
  my ($prev) = keys %$first;
  return () if $prev =~ m($skipRe);

  push @chain, $prev;

  while(my $next = tagsNext($chain[-1], $parent))                               # Tags that can follow next
   {my %next = $next->%*;
    delete $next{$_} for @chain;                                                # Remove tags seem before
    return @chain unless keys %next == 1;                                       # Unambiguous continuance
    push @chain, keys %next                                                     # He's only got one
   }

  @chain                                                                        # Initial child chain
 }

sub tabularizeArraysOfText($)                                                   # Tabularize an array of arrays of text.
 {my ($data) = @_;                                                              # Reference to an array of arrays of data to be formatted as a table.
  ref($data) =~ /array/i or                                                     # Must be an array
    confess "Array reference required not:\n".dump($data)."\n";
  my @width;                                                                    # Maximum width of each column

  for   my $row(@$data)                                                         # Each row
   {for my $col(0..$#$row)                                                      # Each column index
     {my $text  = $row->[$col] // '';                                           # Text of current line
      my $a  = $width[$col] // 0;                                               # Maximum length of data so far
      my $b  = length($text);                                                   # Length of longest line in current item
      $width[$col] = ($a > $b ? $a : $b);                                       # Update maximum length
     }
   }

  my @text;                                                                     # Formatted data
  for my $row(@$data)
   {my @row;
    for my $col(0..$#$row)
     {my $m = $width[$col];                                                     # Maximum width
      my $i = $row->[$col]//'';                                                 # Current item
      if ($i !~ /\A\s*[-+]?\s*(\d|[,])+(\.\d+)?([Ee]\s*[-+]?\s*\d+)?\s*\Z/)     # Not a number - left justify
       {push @row, substr($i.(' 'x$m), 0, $m)."  ";
       }
      else                                                                      # Number - right justify
       {push @row, substr((' 'x$m).$i, -$m)."  ";
       }
     }
    push @text, \@row
   }

  \@text
 } #tabularizeArraysOfText

sub resultingStructureIsArrayOfArrays($)                                        #P Record an expected structure as JSON
 {my ($perl) = @_;                                                              # Data as Perl

  my $t = tabularizeArraysOfText($perl);                                        # Format array of arrays as a table

#  my @t = map {join '', q([), (join ', ',                                       # Each row with truth and lalse
#            map {s/\b1\b/true/ =~ s(\b0\b) (false)gsr} @$_), q(])} @$t;
#
  my @t;

  for my $r(@$t)
   {push @t, q([).(join ', ', map {dump($_)} @$r).q(]);
   }

  my $r = join '', q([), (join ",\n", @t), q(]);
     $r =~ s(  ") ("  )gs for 1..10;
     $r =~ s( ")  (" )gs;
     $r =~ s(" 1")  (true)gs;



  my $file = q(/home/phil/perl/z/zzzJson.txt);
  owf($file, $r);                                                               # Save json
  $perl                                                                         # Return perl
 }

#D0
#-------------------------------------------------------------------------------
# Export - eeee
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

# containingFolder

@ISA          = qw(Exporter);
@EXPORT       = qw();
@EXPORT_OK    = qw();

%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

# podDocumentation

=pod

=encoding utf-8

=head1 Name

Dita::Validate - Check that the next L<xml> tag is valid under L<dita>.

=head1 Synopsis

Check which tags are permissible or required next in a sequence of L<xml> tags
expected to conform to the L<dita> standard.

For example, a B<concept> must always start with a B<title>:

  use Dita::Validate;

  ok  Dita::Validate::tagMustBeFirst(q(concept)) eq q(title);

The tags that can follow a B<title> tag in a B<concept> are:

  is_deeply Dita::Validate::tagsNext(q(title), q(concept)),
   ["abstract",
    "conbody",
    "concept",
    "prolog",
    "related-links",
    "shortdesc",
    "titlealts",
    "topic",
   ];

=head1 Description

Check that the next L<Xml|https://en.wikipedia.org/wiki/XML> tag is valid under L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html>.


Version 20200414.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Immediately useful methods

These methods are the ones most likely to be of immediate use to anyone using
this module for the first time:


L<tagNext($test, $tag, $parent)|/tagNext($test, $tag, $parent)>

True if the B<$test> tag may follow B<$tag> under B<$parent>, else L<undef|https://perldoc.perl.org/functions/undef.html>.

L<tagPrev($test, $tag, $parent)|/tagPrev($test, $tag, $parent)>

True if the B<$prev> tag may appear before B<$tag> under B<$parent>, else L<undef|https://perldoc.perl.org/functions/undef.html>.




=head1 Methods

Check which tags are permissible or required next in a sequence of xml tags expected to conform to the dita standard without resorting to either the L<DITA Open ToolKit|http://www.dita-ot.org/download> or L<Xml Lint|http://xmlsoft.org/xmllint.html>.

=head2 ditaTag($tag)

Whether a tag is an open tag

     Parameter  Description
  1  $tag       Tag

B<Example:>


    ok  ditaEmptyTag(q(anchor));
    ok !ditaEmptyTag(q(p));
    ok  ditaOpenTag (q(draft-comment));
    ok  ditaOpenTag (q(unknown));
    ok  ditaStartTag(q(task));
    ok  𝗱𝗶𝘁𝗮𝗧𝗮𝗴     (q(task));

    ok  childOf('step', 'steps');
    ok !childOf('p',    'steps');

    is_deeply children(q(steps)),
     {"data"=>1, "data-about"=>1, "sort-as"=>1, "step"=>3, "stepsection"=>2};


This is a static method and so should either be imported or invoked as:

  Dita::Validate::ditaTag


=head2 ditaEmptyTag($tag)

Whether a tag is an empty tag

     Parameter  Description
  1  $tag       Tag

B<Example:>


    ok  𝗱𝗶𝘁𝗮𝗘𝗺𝗽𝘁𝘆𝗧𝗮𝗴(q(anchor));
    ok !𝗱𝗶𝘁𝗮𝗘𝗺𝗽𝘁𝘆𝗧𝗮𝗴(q(p));
    ok  ditaOpenTag (q(draft-comment));
    ok  ditaOpenTag (q(unknown));
    ok  ditaStartTag(q(task));
    ok  ditaTag     (q(task));

    ok  childOf('step', 'steps');
    ok !childOf('p',    'steps');

    is_deeply children(q(steps)),
     {"data"=>1, "data-about"=>1, "sort-as"=>1, "step"=>3, "stepsection"=>2};


This is a static method and so should either be imported or invoked as:

  Dita::Validate::ditaEmptyTag


=head2 ditaOpenTag($tag)

Whether a tag is an open tag

     Parameter  Description
  1  $tag       Tag

B<Example:>


    ok  ditaEmptyTag(q(anchor));
    ok !ditaEmptyTag(q(p));
    ok  𝗱𝗶𝘁𝗮𝗢𝗽𝗲𝗻𝗧𝗮𝗴 (q(draft-comment));
    ok  𝗱𝗶𝘁𝗮𝗢𝗽𝗲𝗻𝗧𝗮𝗴 (q(unknown));
    ok  ditaStartTag(q(task));
    ok  ditaTag     (q(task));

    ok  childOf('step', 'steps');
    ok !childOf('p',    'steps');

    is_deeply children(q(steps)),
     {"data"=>1, "data-about"=>1, "sort-as"=>1, "step"=>3, "stepsection"=>2};


This is a static method and so should either be imported or invoked as:

  Dita::Validate::ditaOpenTag


=head2 ditaStartTag($tag)

Whether a tag can start a topic

     Parameter  Description
  1  $tag       Tag

B<Example:>


    ok  ditaEmptyTag(q(anchor));
    ok !ditaEmptyTag(q(p));
    ok  ditaOpenTag (q(draft-comment));
    ok  ditaOpenTag (q(unknown));
    ok  𝗱𝗶𝘁𝗮𝗦𝘁𝗮𝗿𝘁𝗧𝗮𝗴(q(task));
    ok  ditaTag     (q(task));

    ok  childOf('step', 'steps');
    ok !childOf('p',    'steps');

    is_deeply children(q(steps)),
     {"data"=>1, "data-about"=>1, "sort-as"=>1, "step"=>3, "stepsection"=>2};


This is a static method and so should either be imported or invoked as:

  Dita::Validate::ditaStartTag


=head2 children($parent)

{tag} = 1 : children of the specified parent

     Parameter  Description
  1  $parent    The name of the tag to test

This is a static method and so should either be imported or invoked as:

  Dita::Validate::children


=head2 childOf($tag, $parent)

Whether the specified tag is a child of the specified parent

     Parameter  Description
  1  $tag       The name of the tag to test
  2  $parent    The possible parent

This is a static method and so should either be imported or invoked as:

  Dita::Validate::childOf


=head2 descendants($parent)

{descendant} = 1 : descendants that can appear under a parent tag.

     Parameter  Description
  1  $parent    Parent tag

B<Example:>


  if (1)
   {ok 𝗱𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝘀('concept')->{text} == 457;
    ok 197 == scalar keys 𝗱𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝘀('concept')->%*;
    ok 221 == scalar keys 𝗱𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝘀('task')->%*;
    ok 286 == scalar keys 𝗱𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝘀('bookmap')->%*;
    is_deeply 𝗱𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝘀('tm'), { PCDATA => 7, text => 7, tm => 4 };

    is_deeply pathsToDescendantFromParent('step', 'concept'), [];

    is_deeply pathsToDescendantFromParent('li', 'xref'),
     ["li ol desc xref",
      "li ol fn ph xref",
      "li ul desc xref",
      "li ul fn ph xref",
     ];

    is_deeply pathsToDescendantFromParent('steps', 'task'),
     ["steps taskbody task"];

    is_deeply pathsToDescendantFromParent('step', 'taskbody'),
      ["step steps taskbody",
       "step steps-unordered taskbody"];

    is_deeply pathsToDescendantFromParent('author', 'bookmap'),
     ["author bookmeta bookmap",
      "author topicmeta anchorref backmatter bookmap",
      "author topicmeta anchorref frontmatter bookmap",
      "author topicmeta appendices bookmap",
      "author topicmeta appendix bookmap",
      "author topicmeta chapter bookmap",
      "author topicmeta draftintro frontmatter bookmap",
      "author topicmeta glossarylist booklists backmatter bookmap",
      "author topicmeta glossarylist booklists frontmatter bookmap",
      "author topicmeta keydef backmatter bookmap",
      "author topicmeta keydef frontmatter bookmap",
      "author topicmeta mapref backmatter bookmap",
      "author topicmeta mapref frontmatter bookmap",
      "author topicmeta notices backmatter bookmap",
      "author topicmeta notices frontmatter bookmap",
      "author topicmeta part bookmap",
      "author topicmeta preface frontmatter bookmap",
      "author topicmeta reltable bookmap",
      "author topicmeta topicgroup backmatter bookmap",
      "author topicmeta topicgroup frontmatter bookmap",
      "author topicmeta topichead backmatter bookmap",
      "author topicmeta topichead frontmatter bookmap",
      "author topicmeta topicref backmatter bookmap",
      "author topicmeta topicref frontmatter bookmap",
      "author topicmeta topicset backmatter bookmap",
      "author topicmeta topicset frontmatter bookmap",
      "author topicmeta topicsetref backmatter bookmap",
      "author topicmeta topicsetref frontmatter bookmap"];

    is_deeply pathsToDescendantFromParent('dd', 'conbody'),
     ["dd dlentry dl conbody"];

    is_deeply pathsFromParentToChild('step', 'info'),
     [["cmd", "info"]];

    is_deeply tagsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers'),
      [["contactnumber", 1, 1]];

    is_deeply [sort keys descendantsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers')->%*],
     ["ANY", "EMPTY", "PCDATA", "abbreviated-form", "alt", "apiname", "area", "b",
      "boolean", "cite", "cmdname", "codeblock", "codeph", "coderef", "colspec",
      "consequence", "coords", "data", "data-about", "dd", "ddhd", "delim", "desc",
      "div", "dl", "dlentry", "dlhead", "draft-comment", "dt", "dthd", "entry",
      "fig", "figgroup", "filepath", "fn", "foreign", "fragment", "fragref",
      "groupchoice", "groupcomp", "groupseq", "hazardstatement", "hazardsymbol",
      "howtoavoid", "i", "image", "imagemap", "index-base", "index-see",
      "index-see-also", "index-sort-as", "indexterm", "indextermref", "itemgroup",
      "keyword", "kwd", "li", "line-through", "lines", "longdescref",
      "longquoteref", "lq", "markupname", "menucascade", "messagepanel",
      "msgblock", "msgnum", "msgph", "note", "numcharref", "object", "ol", "oper",
      "option", "overline", "p", "param", "parameterentity", "parml", "parmname",
      "pd", "ph", "plentry", "pre", "pt", "q", "repsep", "required-cleanup", "row",
      "screen", "sep", "shape", "shortcut", "simpletable", "sl", "sli", "sort-as",
      "state", "stentry", "sthead", "strow", "sub", "sup", "synblk", "synnote",
      "synnoteref", "synph", "syntaxdiagram", "systemoutput", "table", "tbody",
      "term", "text", "textentity", "tgroup", "thead", "title", "tm", "tt",
      "typeofhazard", "u", "uicontrol", "ul", "unknown", "userinput", "var",
      "varname", "wintitle", "xmlatt", "xmlelement", "xmlnsname", "xmlpi", "xref"];
   }


This is a static method and so should either be imported or invoked as:

  Dita::Validate::descendants


=head2 descendantOf($child, $parent)

Whether the child can be reached by a path starting at the parent

     Parameter  Description
  1  $child     Child
  2  $parent    Parent

This is a static method and so should either be imported or invoked as:

  Dita::Validate::descendantOf


=head2 tagsNext($tag, $parent)

[tag ...] that may follow after B<$tag> under B<$parent> else L<undef|https://perldoc.perl.org/functions/undef.html> if $tag cannot appear under $parent.

     Parameter  Description
  1  $tag       The name of the child tag
  2  $parent    The name of a parent tag

B<Example:>


    is_deeply [sort keys 𝘁𝗮𝗴𝘀𝗡𝗲𝘅𝘁(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok tagNext(q(conbody), q(title), q(concept));

    is_deeply tagsPrev(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok tagPrev(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply tagsLast(q(ol)), ["li"];

    ok tagFirst("title", q(concept));
    ok tagLast ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagsNext


=head2 tagNext($test, $tag, $parent)

True if the B<$test> tag may follow B<$tag> under B<$parent>, else L<undef|https://perldoc.perl.org/functions/undef.html>.

     Parameter  Description
  1  $test      Tag to test
  2  $tag       Current tag
  3  $parent    Parent tag

B<Example:>


    is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok 𝘁𝗮𝗴𝗡𝗲𝘅𝘁(q(conbody), q(title), q(concept));

    is_deeply tagsPrev(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok tagPrev(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply tagsLast(q(ol)), ["li"];

    ok tagFirst("title", q(concept));
    ok tagLast ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagNext


=head2 tagsPrev($tag, $parent)

[tag ...] that may appear before B<$tag> under B<$parent> else L<undef|https://perldoc.perl.org/functions/undef.html> if $tag cannot appear under $parent.

     Parameter  Description
  1  $tag       Child tag
  2  $parent    Parent tag

B<Example:>


    is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok tagNext(q(conbody), q(title), q(concept));

    is_deeply 𝘁𝗮𝗴𝘀𝗣𝗿𝗲𝘃(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok tagPrev(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply tagsLast(q(ol)), ["li"];

    ok tagFirst("title", q(concept));
    ok tagLast ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagsPrev


=head2 tagPrev($test, $tag, $parent)

True if the B<$prev> tag may appear before B<$tag> under B<$parent>, else L<undef|https://perldoc.perl.org/functions/undef.html>.

     Parameter  Description
  1  $test      Tag to test
  2  $tag       Current tag
  3  $parent    Parent tag

B<Example:>


    is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok tagNext(q(conbody), q(title), q(concept));

    is_deeply tagsPrev(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok 𝘁𝗮𝗴𝗣𝗿𝗲𝘃(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply tagsLast(q(ol)), ["li"];

    ok tagFirst("title", q(concept));
    ok tagLast ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagPrev


=head2 tagsFirst($parent)

[tag ...] that can be first under B<$parent>.

     Parameter  Description
  1  $parent    Parent tag

B<Example:>


    is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok tagNext(q(conbody), q(title), q(concept));

    is_deeply tagsPrev(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok tagPrev(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys 𝘁𝗮𝗴𝘀𝗙𝗶𝗿𝘀𝘁(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply tagsLast(q(ol)), ["li"];

    ok tagFirst("title", q(concept));
    ok tagLast ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagsFirst


=head2 tagFirst($test, $parent)

True if B<$test> may be first under B<$parent> else L<undef|https://perldoc.perl.org/functions/undef.html>.

     Parameter  Description
  1  $test      Test tag
  2  $parent    Parent tag

B<Example:>


    is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok tagNext(q(conbody), q(title), q(concept));

    is_deeply tagsPrev(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok tagPrev(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply tagsLast(q(ol)), ["li"];

    ok 𝘁𝗮𝗴𝗙𝗶𝗿𝘀𝘁("title", q(concept));
    ok tagLast ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagFirst


=head2 tagsLast($parent)

[tag ...] that can be last under B<$parent>.

     Parameter  Description
  1  $parent    Parent tag

B<Example:>


    is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok tagNext(q(conbody), q(title), q(concept));

    is_deeply tagsPrev(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok tagPrev(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply 𝘁𝗮𝗴𝘀𝗟𝗮𝘀𝘁(q(ol)), ["li"];

    ok tagFirst("title", q(concept));
    ok tagLast ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagsLast


=head2 tagLast($test, $parent)

True if B<$test> may be last under B<$parent> else L<undef|https://perldoc.perl.org/functions/undef.html>.

     Parameter  Description
  1  $test      Test tag
  2  $parent    Parent tag

B<Example:>


    is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
     ["abstract",
      "conbody",
      "concept",
      "prolog",
      "related-links",
      "shortdesc",
      "titlealts",
     ];

    ok tagNext(q(conbody), q(title), q(concept));

    is_deeply tagsPrev(q(conbody), q(concept)),
     ["abstract", "prolog", "shortdesc", "title", "titlealts"];

    ok tagPrev(q(prolog), q(conbody), q(concept));

    is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

    is_deeply tagsLast(q(ol)), ["li"];

    ok tagFirst("title", q(concept));
    ok 𝘁𝗮𝗴𝗟𝗮𝘀𝘁 ("step",  q(steps));


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagLast


=head2 tagCanBeEmpty($parent)

Whether a tag can be empty or not.

     Parameter  Description
  1  $parent    The name of the tag to test

B<Example:>


    ok !𝘁𝗮𝗴𝗖𝗮𝗻𝗕𝗲𝗘𝗺𝗽𝘁𝘆(q(concept));
    ok  𝘁𝗮𝗴𝗖𝗮𝗻𝗕𝗲𝗘𝗺𝗽𝘁𝘆(q(text));

    ok  tagMustBeNext (q(stepsection), q(steps)) eq q(step);
    ok !tagMustBeNext (q(table), q(conbody));
    ok  tagMustBeFirst(q(concept))               eq q(title);
    ok  tagMustBeLast (q(steps))                 eq q(step);

    ok  descendantOf(q(entry), q(table))   ; #######
    ok  descendantOf(q(steps), q(taskbody)); #######
    ok !descendantOf(q(steps), q(concept));  #######
    ok !descendantOf(q(info),  q(conbody));  #######


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagCanBeEmpty


=head2 tagsThatFitBetween($prev, $next, $parent)

[tag, B<fitAfter>, B<fitBefore>] that will fit after the B<$prev> tag and before the B<$next> tag under B<$parent>. If $prev is L<undef|https://perldoc.perl.org/functions/undef.html> then the first tags permissible under $parent will have B<fitAfter> set to true. If $next is L<undef|https://perldoc.perl.org/functions/undef.html> then last tags permissible under $parent will have B<fitBefore> set to true.

     Parameter  Description
  1  $prev      Previous tag
  2  $next      Next tag
  3  $parent    Parent tag

B<Example:>


     is_deeply 𝘁𝗮𝗴𝘀𝗧𝗵𝗮𝘁𝗙𝗶𝘁𝗕𝗲𝘁𝘄𝗲𝗲𝗻(undef, undef, q(steps)),
     [["data", 1],
      ["data-about", 1],
      ["sort-as", 1],
      ["step", 1, 1],
      ["stepsection", 1],
     ];

    is_deeply 𝘁𝗮𝗴𝘀𝗧𝗵𝗮𝘁𝗙𝗶𝘁𝗕𝗲𝘁𝘄𝗲𝗲𝗻(q(p), q(p), q(conbody)),
     [["codeblock", 1, 1],
      ["conbodydiv", 1],
      ["data", 1, 1],
      ["data-about", 1, 1],
      ["div", 1, 1],
      ["dl", 1, 1],
      ["draft-comment", 1, 1],
      ["equation-block", 1, 1],
      ["equation-figure", 1, 1],
      ["example", 1],
      ["fig", 1, 1],
      ["foreign", 1, 1],
      ["hazardstatement", 1, 1],
      ["image", 1, 1],
      ["imagemap", 1, 1],
      ["lines", 1, 1],
      ["lq", 1, 1],
      ["mathml-d-foreign", 1],
      ["msgblock", 1, 1],
      ["note", 1, 1],
      ["object", 1, 1],
      ["ol", 1, 1],
      ["p", 1, 1],
      ["parml", 1, 1],
      ["pre", 1, 1],
      ["required-cleanup", 1, 1],
      ["screen", 1, 1],
      ["section", 1],
      ["simpletable", 1, 1],
      ["sl", 1, 1],
      ["sort-as", 1, 1],
      ["svg-d-foreign", 1],
      ["syntaxdiagram", 1, 1],
      ["table", 1, 1],
      ["ul", 1, 1],
      ["unknown", 1, 1]]


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagsThatFitBetween


=head2 tagMustBeFirst($parent)

The name of the one tag that must be present first under B<$parent> or B<undef> if there is no such unique tag.

     Parameter  Description
  1  $parent    The name of the parent tag

B<Example:>


    ok !tagCanBeEmpty(q(concept));
    ok  tagCanBeEmpty(q(text));

    ok  tagMustBeNext (q(stepsection), q(steps)) eq q(step);
    ok !tagMustBeNext (q(table), q(conbody));
    ok  𝘁𝗮𝗴𝗠𝘂𝘀𝘁𝗕𝗲𝗙𝗶𝗿𝘀𝘁(q(concept))               eq q(title);
    ok  tagMustBeLast (q(steps))                 eq q(step);

    ok  descendantOf(q(entry), q(table))   ; #######
    ok  descendantOf(q(steps), q(taskbody)); #######
    ok !descendantOf(q(steps), q(concept));  #######
    ok !descendantOf(q(info),  q(conbody));  #######


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagMustBeFirst


=head2 tagMustBeNext($tag, $parent)

The name of the one tag that must follow the specified B<$tag> under B<$parent> or B<undef> if there is no such tag.

     Parameter  Description
  1  $tag       Child tag
  2  $parent    Parent tag

B<Example:>


    ok !tagCanBeEmpty(q(concept));
    ok  tagCanBeEmpty(q(text));

    ok  𝘁𝗮𝗴𝗠𝘂𝘀𝘁𝗕𝗲𝗡𝗲𝘅𝘁 (q(stepsection), q(steps)) eq q(step);
    ok !𝘁𝗮𝗴𝗠𝘂𝘀𝘁𝗕𝗲𝗡𝗲𝘅𝘁 (q(table), q(conbody));
    ok  tagMustBeFirst(q(concept))               eq q(title);
    ok  tagMustBeLast (q(steps))                 eq q(step);

    ok  descendantOf(q(entry), q(table))   ; #######
    ok  descendantOf(q(steps), q(taskbody)); #######
    ok !descendantOf(q(steps), q(concept));  #######
    ok !descendantOf(q(info),  q(conbody));  #######


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagMustBeNext


=head2 tagMustBeLast($parent)

The name of the one tag that must be present last under B<$parent> or B<undef> if there is no such unique tag.

     Parameter  Description
  1  $parent    The name of the parent tag

B<Example:>


    ok !tagCanBeEmpty(q(concept));
    ok  tagCanBeEmpty(q(text));

    ok  tagMustBeNext (q(stepsection), q(steps)) eq q(step);
    ok !tagMustBeNext (q(table), q(conbody));
    ok  tagMustBeFirst(q(concept))               eq q(title);
    ok  𝘁𝗮𝗴𝗠𝘂𝘀𝘁𝗕𝗲𝗟𝗮𝘀𝘁 (q(steps))                 eq q(step);

    ok  descendantOf(q(entry), q(table))   ; #######
    ok  descendantOf(q(steps), q(taskbody)); #######
    ok !descendantOf(q(steps), q(concept));  #######
    ok !descendantOf(q(info),  q(conbody));  #######


This is a static method and so should either be imported or invoked as:

  Dita::Validate::tagMustBeLast


=head2 pathsToDescendantFromParent($descendant, $parent)

[[tag, ...], ...] paths to the specified B<$descendant> from the B<$parent>. If $parent is B<undef> then all teh L<Dita|http://docs.oasis-open.org/dita/dita/v1.3/os/part2-tech-content/dita-v1.3-os-part2-tech-content.html> starttags are tried.

     Parameter    Description
  1  $descendant  Descendant tag
  2  $parent      Parent tag

B<Example:>


  if (1)
   {ok descendants('concept')->{text} == 457;
    ok 197 == scalar keys descendants('concept')->%*;
    ok 221 == scalar keys descendants('task')->%*;
    ok 286 == scalar keys descendants('bookmap')->%*;
    is_deeply descendants('tm'), { PCDATA => 7, text => 7, tm => 4 };

    is_deeply 𝗽𝗮𝘁𝗵𝘀𝗧𝗼𝗗𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝗙𝗿𝗼𝗺𝗣𝗮𝗿𝗲𝗻𝘁('step', 'concept'), [];

    is_deeply 𝗽𝗮𝘁𝗵𝘀𝗧𝗼𝗗𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝗙𝗿𝗼𝗺𝗣𝗮𝗿𝗲𝗻𝘁('li', 'xref'),
     ["li ol desc xref",
      "li ol fn ph xref",
      "li ul desc xref",
      "li ul fn ph xref",
     ];

    is_deeply 𝗽𝗮𝘁𝗵𝘀𝗧𝗼𝗗𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝗙𝗿𝗼𝗺𝗣𝗮𝗿𝗲𝗻𝘁('steps', 'task'),
     ["steps taskbody task"];

    is_deeply 𝗽𝗮𝘁𝗵𝘀𝗧𝗼𝗗𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝗙𝗿𝗼𝗺𝗣𝗮𝗿𝗲𝗻𝘁('step', 'taskbody'),
      ["step steps taskbody",
       "step steps-unordered taskbody"];

    is_deeply 𝗽𝗮𝘁𝗵𝘀𝗧𝗼𝗗𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝗙𝗿𝗼𝗺𝗣𝗮𝗿𝗲𝗻𝘁('author', 'bookmap'),
     ["author bookmeta bookmap",
      "author topicmeta anchorref backmatter bookmap",
      "author topicmeta anchorref frontmatter bookmap",
      "author topicmeta appendices bookmap",
      "author topicmeta appendix bookmap",
      "author topicmeta chapter bookmap",
      "author topicmeta draftintro frontmatter bookmap",
      "author topicmeta glossarylist booklists backmatter bookmap",
      "author topicmeta glossarylist booklists frontmatter bookmap",
      "author topicmeta keydef backmatter bookmap",
      "author topicmeta keydef frontmatter bookmap",
      "author topicmeta mapref backmatter bookmap",
      "author topicmeta mapref frontmatter bookmap",
      "author topicmeta notices backmatter bookmap",
      "author topicmeta notices frontmatter bookmap",
      "author topicmeta part bookmap",
      "author topicmeta preface frontmatter bookmap",
      "author topicmeta reltable bookmap",
      "author topicmeta topicgroup backmatter bookmap",
      "author topicmeta topicgroup frontmatter bookmap",
      "author topicmeta topichead backmatter bookmap",
      "author topicmeta topichead frontmatter bookmap",
      "author topicmeta topicref backmatter bookmap",
      "author topicmeta topicref frontmatter bookmap",
      "author topicmeta topicset backmatter bookmap",
      "author topicmeta topicset frontmatter bookmap",
      "author topicmeta topicsetref backmatter bookmap",
      "author topicmeta topicsetref frontmatter bookmap"];

    is_deeply 𝗽𝗮𝘁𝗵𝘀𝗧𝗼𝗗𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝗙𝗿𝗼𝗺𝗣𝗮𝗿𝗲𝗻𝘁('dd', 'conbody'),
     ["dd dlentry dl conbody"];

    is_deeply pathsFromParentToChild('step', 'info'),
     [["cmd", "info"]];

    is_deeply tagsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers'),
      [["contactnumber", 1, 1]];

    is_deeply [sort keys descendantsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers')->%*],
     ["ANY", "EMPTY", "PCDATA", "abbreviated-form", "alt", "apiname", "area", "b",
      "boolean", "cite", "cmdname", "codeblock", "codeph", "coderef", "colspec",
      "consequence", "coords", "data", "data-about", "dd", "ddhd", "delim", "desc",
      "div", "dl", "dlentry", "dlhead", "draft-comment", "dt", "dthd", "entry",
      "fig", "figgroup", "filepath", "fn", "foreign", "fragment", "fragref",
      "groupchoice", "groupcomp", "groupseq", "hazardstatement", "hazardsymbol",
      "howtoavoid", "i", "image", "imagemap", "index-base", "index-see",
      "index-see-also", "index-sort-as", "indexterm", "indextermref", "itemgroup",
      "keyword", "kwd", "li", "line-through", "lines", "longdescref",
      "longquoteref", "lq", "markupname", "menucascade", "messagepanel",
      "msgblock", "msgnum", "msgph", "note", "numcharref", "object", "ol", "oper",
      "option", "overline", "p", "param", "parameterentity", "parml", "parmname",
      "pd", "ph", "plentry", "pre", "pt", "q", "repsep", "required-cleanup", "row",
      "screen", "sep", "shape", "shortcut", "simpletable", "sl", "sli", "sort-as",
      "state", "stentry", "sthead", "strow", "sub", "sup", "synblk", "synnote",
      "synnoteref", "synph", "syntaxdiagram", "systemoutput", "table", "tbody",
      "term", "text", "textentity", "tgroup", "thead", "title", "tm", "tt",
      "typeofhazard", "u", "uicontrol", "ul", "unknown", "userinput", "var",
      "varname", "wintitle", "xmlatt", "xmlelement", "xmlnsname", "xmlpi", "xref"];
   }


This is a static method and so should either be imported or invoked as:

  Dita::Validate::pathsToDescendantFromParent


=head2 descendantsThatFitBetween($prev, $next, $parent)

{tag} = 1:  descendants that could fit at some level after the B<$prev> tag and before the B<$next> tag under B<$parent>. If $prev is L<undef|https://perldoc.perl.org/functions/undef.html> then the first tags permissible under $parent will have B<fitAfter> set to true. If $next is L<undef|https://perldoc.perl.org/functions/undef.html> then last tags permissible under $parent will have B<fitBefore> set to true.

     Parameter  Description
  1  $prev      Previous tag
  2  $next      Next tag
  3  $parent    Parent tag

B<Example:>


  if (1)
   {ok descendants('concept')->{text} == 457;
    ok 197 == scalar keys descendants('concept')->%*;
    ok 221 == scalar keys descendants('task')->%*;
    ok 286 == scalar keys descendants('bookmap')->%*;
    is_deeply descendants('tm'), { PCDATA => 7, text => 7, tm => 4 };

    is_deeply pathsToDescendantFromParent('step', 'concept'), [];

    is_deeply pathsToDescendantFromParent('li', 'xref'),
     ["li ol desc xref",
      "li ol fn ph xref",
      "li ul desc xref",
      "li ul fn ph xref",
     ];

    is_deeply pathsToDescendantFromParent('steps', 'task'),
     ["steps taskbody task"];

    is_deeply pathsToDescendantFromParent('step', 'taskbody'),
      ["step steps taskbody",
       "step steps-unordered taskbody"];

    is_deeply pathsToDescendantFromParent('author', 'bookmap'),
     ["author bookmeta bookmap",
      "author topicmeta anchorref backmatter bookmap",
      "author topicmeta anchorref frontmatter bookmap",
      "author topicmeta appendices bookmap",
      "author topicmeta appendix bookmap",
      "author topicmeta chapter bookmap",
      "author topicmeta draftintro frontmatter bookmap",
      "author topicmeta glossarylist booklists backmatter bookmap",
      "author topicmeta glossarylist booklists frontmatter bookmap",
      "author topicmeta keydef backmatter bookmap",
      "author topicmeta keydef frontmatter bookmap",
      "author topicmeta mapref backmatter bookmap",
      "author topicmeta mapref frontmatter bookmap",
      "author topicmeta notices backmatter bookmap",
      "author topicmeta notices frontmatter bookmap",
      "author topicmeta part bookmap",
      "author topicmeta preface frontmatter bookmap",
      "author topicmeta reltable bookmap",
      "author topicmeta topicgroup backmatter bookmap",
      "author topicmeta topicgroup frontmatter bookmap",
      "author topicmeta topichead backmatter bookmap",
      "author topicmeta topichead frontmatter bookmap",
      "author topicmeta topicref backmatter bookmap",
      "author topicmeta topicref frontmatter bookmap",
      "author topicmeta topicset backmatter bookmap",
      "author topicmeta topicset frontmatter bookmap",
      "author topicmeta topicsetref backmatter bookmap",
      "author topicmeta topicsetref frontmatter bookmap"];

    is_deeply pathsToDescendantFromParent('dd', 'conbody'),
     ["dd dlentry dl conbody"];

    is_deeply pathsFromParentToChild('step', 'info'),
     [["cmd", "info"]];

    is_deeply tagsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers'),
      [["contactnumber", 1, 1]];

    is_deeply [sort keys 𝗱𝗲𝘀𝗰𝗲𝗻𝗱𝗮𝗻𝘁𝘀𝗧𝗵𝗮𝘁𝗙𝗶𝘁𝗕𝗲𝘁𝘄𝗲𝗲𝗻('contactnumber', 'contactnumber', 'contactnumbers')->%*],
     ["ANY", "EMPTY", "PCDATA", "abbreviated-form", "alt", "apiname", "area", "b",
      "boolean", "cite", "cmdname", "codeblock", "codeph", "coderef", "colspec",
      "consequence", "coords", "data", "data-about", "dd", "ddhd", "delim", "desc",
      "div", "dl", "dlentry", "dlhead", "draft-comment", "dt", "dthd", "entry",
      "fig", "figgroup", "filepath", "fn", "foreign", "fragment", "fragref",
      "groupchoice", "groupcomp", "groupseq", "hazardstatement", "hazardsymbol",
      "howtoavoid", "i", "image", "imagemap", "index-base", "index-see",
      "index-see-also", "index-sort-as", "indexterm", "indextermref", "itemgroup",
      "keyword", "kwd", "li", "line-through", "lines", "longdescref",
      "longquoteref", "lq", "markupname", "menucascade", "messagepanel",
      "msgblock", "msgnum", "msgph", "note", "numcharref", "object", "ol", "oper",
      "option", "overline", "p", "param", "parameterentity", "parml", "parmname",
      "pd", "ph", "plentry", "pre", "pt", "q", "repsep", "required-cleanup", "row",
      "screen", "sep", "shape", "shortcut", "simpletable", "sl", "sli", "sort-as",
      "state", "stentry", "sthead", "strow", "sub", "sup", "synblk", "synnote",
      "synnoteref", "synph", "syntaxdiagram", "systemoutput", "table", "tbody",
      "term", "text", "textentity", "tgroup", "thead", "title", "tm", "tt",
      "typeofhazard", "u", "uicontrol", "ul", "unknown", "userinput", "var",
      "varname", "wintitle", "xmlatt", "xmlelement", "xmlnsname", "xmlpi", "xref"];
   }


This is a static method and so should either be imported or invoked as:

  Dita::Validate::descendantsThatFitBetween


=head2 pathsFromParentToChild($parent, $child)

[[tag, ...], ...] ways under parent that reach the specified child.

     Parameter  Description
  1  $parent    Parent
  2  $child     Child tag

B<Example:>


  if (1)
   {ok descendants('concept')->{text} == 457;
    ok 197 == scalar keys descendants('concept')->%*;
    ok 221 == scalar keys descendants('task')->%*;
    ok 286 == scalar keys descendants('bookmap')->%*;
    is_deeply descendants('tm'), { PCDATA => 7, text => 7, tm => 4 };

    is_deeply pathsToDescendantFromParent('step', 'concept'), [];

    is_deeply pathsToDescendantFromParent('li', 'xref'),
     ["li ol desc xref",
      "li ol fn ph xref",
      "li ul desc xref",
      "li ul fn ph xref",
     ];

    is_deeply pathsToDescendantFromParent('steps', 'task'),
     ["steps taskbody task"];

    is_deeply pathsToDescendantFromParent('step', 'taskbody'),
      ["step steps taskbody",
       "step steps-unordered taskbody"];

    is_deeply pathsToDescendantFromParent('author', 'bookmap'),
     ["author bookmeta bookmap",
      "author topicmeta anchorref backmatter bookmap",
      "author topicmeta anchorref frontmatter bookmap",
      "author topicmeta appendices bookmap",
      "author topicmeta appendix bookmap",
      "author topicmeta chapter bookmap",
      "author topicmeta draftintro frontmatter bookmap",
      "author topicmeta glossarylist booklists backmatter bookmap",
      "author topicmeta glossarylist booklists frontmatter bookmap",
      "author topicmeta keydef backmatter bookmap",
      "author topicmeta keydef frontmatter bookmap",
      "author topicmeta mapref backmatter bookmap",
      "author topicmeta mapref frontmatter bookmap",
      "author topicmeta notices backmatter bookmap",
      "author topicmeta notices frontmatter bookmap",
      "author topicmeta part bookmap",
      "author topicmeta preface frontmatter bookmap",
      "author topicmeta reltable bookmap",
      "author topicmeta topicgroup backmatter bookmap",
      "author topicmeta topicgroup frontmatter bookmap",
      "author topicmeta topichead backmatter bookmap",
      "author topicmeta topichead frontmatter bookmap",
      "author topicmeta topicref backmatter bookmap",
      "author topicmeta topicref frontmatter bookmap",
      "author topicmeta topicset backmatter bookmap",
      "author topicmeta topicset frontmatter bookmap",
      "author topicmeta topicsetref backmatter bookmap",
      "author topicmeta topicsetref frontmatter bookmap"];

    is_deeply pathsToDescendantFromParent('dd', 'conbody'),
     ["dd dlentry dl conbody"];

    is_deeply 𝗽𝗮𝘁𝗵𝘀𝗙𝗿𝗼𝗺𝗣𝗮𝗿𝗲𝗻𝘁𝗧𝗼𝗖𝗵𝗶𝗹𝗱('step', 'info'),
     [["cmd", "info"]];

    is_deeply tagsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers'),
      [["contactnumber", 1, 1]];

    is_deeply [sort keys descendantsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers')->%*],
     ["ANY", "EMPTY", "PCDATA", "abbreviated-form", "alt", "apiname", "area", "b",
      "boolean", "cite", "cmdname", "codeblock", "codeph", "coderef", "colspec",
      "consequence", "coords", "data", "data-about", "dd", "ddhd", "delim", "desc",
      "div", "dl", "dlentry", "dlhead", "draft-comment", "dt", "dthd", "entry",
      "fig", "figgroup", "filepath", "fn", "foreign", "fragment", "fragref",
      "groupchoice", "groupcomp", "groupseq", "hazardstatement", "hazardsymbol",
      "howtoavoid", "i", "image", "imagemap", "index-base", "index-see",
      "index-see-also", "index-sort-as", "indexterm", "indextermref", "itemgroup",
      "keyword", "kwd", "li", "line-through", "lines", "longdescref",
      "longquoteref", "lq", "markupname", "menucascade", "messagepanel",
      "msgblock", "msgnum", "msgph", "note", "numcharref", "object", "ol", "oper",
      "option", "overline", "p", "param", "parameterentity", "parml", "parmname",
      "pd", "ph", "plentry", "pre", "pt", "q", "repsep", "required-cleanup", "row",
      "screen", "sep", "shape", "shortcut", "simpletable", "sl", "sli", "sort-as",
      "state", "stentry", "sthead", "strow", "sub", "sup", "synblk", "synnote",
      "synnoteref", "synph", "syntaxdiagram", "systemoutput", "table", "tbody",
      "term", "text", "textentity", "tgroup", "thead", "title", "tm", "tt",
      "typeofhazard", "u", "uicontrol", "ul", "unknown", "userinput", "var",
      "varname", "wintitle", "xmlatt", "xmlelement", "xmlnsname", "xmlpi", "xref"];
   }


This is a static method and so should either be imported or invoked as:

  Dita::Validate::pathsFromParentToChild



=head1 Index


1 L<childOf|/childOf> - Whether the specified tag is a child of the specified parent

2 L<children|/children> - {tag} = 1 : children of the specified parent

3 L<descendantOf|/descendantOf> - Whether the child can be reached by a path starting at the parent

4 L<descendants|/descendants> - {descendant} = 1 : descendants that can appear under a parent tag.

5 L<descendantsThatFitBetween|/descendantsThatFitBetween> - {tag} = 1:  descendants that could fit at some level after the B<$prev> tag and before the B<$next> tag under B<$parent>.

6 L<ditaEmptyTag|/ditaEmptyTag> - Whether a tag is an empty tag

7 L<ditaOpenTag|/ditaOpenTag> - Whether a tag is an open tag

8 L<ditaStartTag|/ditaStartTag> - Whether a tag can start a topic

9 L<ditaTag|/ditaTag> - Whether a tag is an open tag

10 L<pathsFromParentToChild|/pathsFromParentToChild> - [[tag, .

11 L<pathsToDescendantFromParent|/pathsToDescendantFromParent> - [[tag, .

12 L<tagCanBeEmpty|/tagCanBeEmpty> - Whether a tag can be empty or not.

13 L<tagFirst|/tagFirst> - True if B<$test> may be first under B<$parent> else L<undef|https://perldoc.perl.org/functions/undef.html>.

14 L<tagLast|/tagLast> - True if B<$test> may be last under B<$parent> else L<undef|https://perldoc.perl.org/functions/undef.html>.

15 L<tagMustBeFirst|/tagMustBeFirst> - The name of the one tag that must be present first under B<$parent> or B<undef> if there is no such unique tag.

16 L<tagMustBeLast|/tagMustBeLast> - The name of the one tag that must be present last under B<$parent> or B<undef> if there is no such unique tag.

17 L<tagMustBeNext|/tagMustBeNext> - The name of the one tag that must follow the specified B<$tag> under B<$parent> or B<undef> if there is no such tag.

18 L<tagNext|/tagNext> - True if the B<$test> tag may follow B<$tag> under B<$parent>, else L<undef|https://perldoc.perl.org/functions/undef.html>.

19 L<tagPrev|/tagPrev> - True if the B<$prev> tag may appear before B<$tag> under B<$parent>, else L<undef|https://perldoc.perl.org/functions/undef.html>.

20 L<tagsFirst|/tagsFirst> - [tag .

21 L<tagsLast|/tagsLast> - [tag .

22 L<tagsNext|/tagsNext> - [tag .

23 L<tagsPrev|/tagsPrev> - [tag .

24 L<tagsThatFitBetween|/tagsThatFitBetween> - [tag, B<fitAfter>, B<fitBefore>] that will fit after the B<$prev> tag and before the B<$next> tag under B<$parent>.

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Dita::Validate

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
use warnings FATAL=>qw(all);
use strict;
use Test::More qw(no_plan);

#goto latest;

if (1) {                                                                        #TditaEmptyTag #TditaCanBeEmptyTag #TditaEmptyTags #TditaOpenTag #TditaOpenTags #TditaStartTags #TditaStartTag #TditaTag #TchildOf #Tchildren
  ok  ditaEmptyTag(q(anchor));
  ok  ditaCanBeEmptyTag(q(dlhead));
  ok !ditaCanBeEmptyTag(q(ul));
  ok !ditaEmptyTag(q(p));
  ok  ditaOpenTag (q(draft-comment));
  ok  ditaOpenTag (q(unknown));
  ok  ditaStartTag(q(task));
  ok  ditaTag     (q(task));
  ok !ditaTag     (q(a));
  ok  380 == scalar [ditaTags]->@*;

  ok  childOf('step', 'steps');
  ok !childOf('p',    'steps');

  is_deeply children(q(steps)),
   {"data"=>1, "data-about"=>1, "sort-as"=>1, "step"=>3, "stepsection"=>2};
 }


if (1) {                                                                        #TtagsNext #TtagNext #TtagsPrev #TtagPrev #TtagsFirst #TtagsLast #TtagFirst #TtagLast
  is_deeply [sort keys tagsNext(q(title), q(concept))->%*],
   ["abstract",
    "conbody",
    "concept",
    "prolog",
    "related-links",
    "shortdesc",
    "titlealts",
   ];

  ok tagNext(q(conbody), q(title), q(concept));

  is_deeply tagsPrev(q(conbody), q(concept)),
   ["abstract", "prolog", "shortdesc", "title", "titlealts"];

  ok tagPrev(q(prolog), q(conbody), q(concept));

  is_deeply [sort keys tagsFirst(q(table))->%*], ["desc", "tgroup", "title"];

  is_deeply tagsLast(q(ol)), ["li"];

  ok tagFirst("title", q(concept));
  ok tagLast ("title", q(concept));
  ok tagLast ("step",  q(steps));
 }

if (1) {                                                                        #TtagMustBeFirst #TtagMustBeLast #TtagMustBeNext #TtagCanBeEmpty
  ok !tagCanBeEmpty(q(concept));
  ok  tagCanBeEmpty(q(text));

  ok  tagMustBeNext (q(stepsection), q(steps)) eq q(step);
  ok !tagMustBeNext (q(table), q(conbody));
  ok  tagMustBeFirst(q(concept))               eq q(title);
  ok  tagMustBeLast (q(steps))                 eq q(step);

  ok  descendantOf(q(entry), q(table));
  ok  descendantOf(q(steps), q(taskbody));
  ok !descendantOf(q(steps), q(concept));
  ok !descendantOf(q(info),  q(conbody));
 }

if (1) {                                                                        #TtagsThatFitBetween
   is_deeply tagsThatFitBetween(undef, undef, q(steps)),
     resultingStructureIsArrayOfArrays(
   [["data", 1],
    ["data-about", 1],
    ["sort-as", 1],
    ["step", 1, 1],
    ["stepsection", 1],
   ]);

  is_deeply tagsThatFitBetween(q(p), q(p), q(conbody)),
     resultingStructureIsArrayOfArrays(
   [["codeblock", 1, 1],
    ["conbodydiv", 1],
    ["data", 1, 1],
    ["data-about", 1, 1],
    ["div", 1, 1],
    ["dl", 1, 1],
    ["draft-comment", 1, 1],
    ["equation-block", 1, 1],
    ["equation-figure", 1, 1],
    ["example", 1],
    ["fig", 1, 1],
    ["foreign", 1, 1],
    ["hazardstatement", 1, 1],
    ["image", 1, 1],
    ["imagemap", 1, 1],
    ["lines", 1, 1],
    ["lq", 1, 1],
    ["mathml-d-foreign", 1],
    ["msgblock", 1, 1],
    ["note", 1, 1],
    ["object", 1, 1],
    ["ol", 1, 1],
    ["p", 1, 1],
    ["parml", 1, 1],
    ["pre", 1, 1],
    ["required-cleanup", 1, 1],
    ["screen", 1, 1],
    ["section", 1],
    ["simpletable", 1, 1],
    ["sl", 1, 1],
    ["sort-as", 1, 1],
    ["svg-d-foreign", 1],
    ["syntaxdiagram", 1, 1],
    ["table", 1, 1],
    ["ul", 1, 1],
    ["unknown", 1, 1]]);
 }

if (1)                                                                          #Tdescendants #TpathsToDescendantFromParent #TpathsFromParentToChild #TdescendantsThatFitBetween
 {ok descendants('concept')->{text} == 457;
  ok 197 == scalar keys descendants('concept')->%*;
  ok 221 == scalar keys descendants('task')->%*;
  ok 286 == scalar keys descendants('bookmap')->%*;
  is_deeply descendants('tm'), { PCDATA => 7, text => 7, tm => 4 };

  is_deeply pathsToDescendantFromParent('step', 'concept'), [];

  is_deeply pathsToDescendantFromParent('li', 'xref'),
   ["li ol desc xref",
    "li ol fn ph xref",
    "li ul desc xref",
    "li ul fn ph xref",
   ];

  is_deeply pathsToDescendantFromParent('steps', 'task'),
   ["steps taskbody task"];

  is_deeply pathsToDescendantFromParent('step', 'taskbody'),
    ["step steps taskbody",
     "step steps-unordered taskbody"];

  is_deeply pathsToDescendantFromParent('author', 'bookmap'),
   ["author bookmeta bookmap",
    "author topicmeta anchorref backmatter bookmap",
    "author topicmeta anchorref frontmatter bookmap",
    "author topicmeta appendices bookmap",
    "author topicmeta appendix bookmap",
    "author topicmeta chapter bookmap",
    "author topicmeta draftintro frontmatter bookmap",
    "author topicmeta glossarylist booklists backmatter bookmap",
    "author topicmeta glossarylist booklists frontmatter bookmap",
    "author topicmeta keydef backmatter bookmap",
    "author topicmeta keydef frontmatter bookmap",
    "author topicmeta mapref backmatter bookmap",
    "author topicmeta mapref frontmatter bookmap",
    "author topicmeta notices backmatter bookmap",
    "author topicmeta notices frontmatter bookmap",
    "author topicmeta part bookmap",
    "author topicmeta preface frontmatter bookmap",
    "author topicmeta reltable bookmap",
    "author topicmeta topicgroup backmatter bookmap",
    "author topicmeta topicgroup frontmatter bookmap",
    "author topicmeta topichead backmatter bookmap",
    "author topicmeta topichead frontmatter bookmap",
    "author topicmeta topicref backmatter bookmap",
    "author topicmeta topicref frontmatter bookmap",
    "author topicmeta topicset backmatter bookmap",
    "author topicmeta topicset frontmatter bookmap",
    "author topicmeta topicsetref backmatter bookmap",
    "author topicmeta topicsetref frontmatter bookmap"];

  is_deeply pathsToDescendantFromParent('dd', 'conbody'),
   ["dd dlentry dl conbody"];

  is_deeply pathsFromParentToChild('step', 'info'),
   [["cmd", "info"],
    ["hazardstatement", "cmd", "info"],
    ["note", "cmd", "info"]];

  is_deeply tagsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers'),
   [["contactnumber", 1, 1]];

  is_deeply [sort keys descendantsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers')->%*],
   ["ANY", "EMPTY", "PCDATA", "abbreviated-form", "alt", "apiname", "area", "b",
    "boolean", "cite", "cmdname", "codeblock", "codeph", "coderef", "colspec",
    "consequence", "coords", "data", "data-about", "dd", "ddhd", "delim", "desc",
    "div", "dl", "dlentry", "dlhead", "draft-comment", "dt", "dthd", "entry",
    "fig", "figgroup", "filepath", "fn", "foreign", "fragment", "fragref",
    "groupchoice", "groupcomp", "groupseq", "hazardstatement", "hazardsymbol",
    "howtoavoid", "i", "image", "imagemap", "index-base", "index-see",
    "index-see-also", "index-sort-as", "indexterm", "indextermref", "itemgroup",
    "keyword", "kwd", "li", "line-through", "lines", "longdescref",
    "longquoteref", "lq", "markupname", "menucascade", "messagepanel",
    "msgblock", "msgnum", "msgph", "note", "numcharref", "object", "ol", "oper",
    "option", "overline", "p", "param", "parameterentity", "parml", "parmname",
    "pd", "ph", "plentry", "pre", "pt", "q", "repsep", "required-cleanup", "row",
    "screen", "sep", "shape", "shortcut", "simpletable", "sl", "sli", "sort-as",
    "state", "stentry", "sthead", "strow", "sub", "sup", "synblk", "synnote",
    "synnoteref", "synph", "syntaxdiagram", "systemoutput", "table", "tbody",
    "term", "text", "textentity", "tgroup", "thead", "title", "tm", "tt",
    "typeofhazard", "u", "uicontrol", "ul", "unknown", "userinput", "var",
    "varname", "wintitle", "xmlatt", "xmlelement", "xmlnsname", "xmlpi", "xref"];

  is_deeply [sort keys descendantsThatFitBetween('contactnumber', 'contactnumber', 'contactnumbers')->%*],
   ["ANY", "EMPTY", "PCDATA", "abbreviated-form", "alt", "apiname", "area", "b",
    "boolean", "cite", "cmdname", "codeblock", "codeph", "coderef", "colspec",
    "consequence", "coords", "data", "data-about", "dd", "ddhd", "delim", "desc",
    "div", "dl", "dlentry", "dlhead", "draft-comment", "dt", "dthd", "entry",
    "fig", "figgroup", "filepath", "fn", "foreign", "fragment", "fragref",
    "groupchoice", "groupcomp", "groupseq", "hazardstatement", "hazardsymbol",
    "howtoavoid", "i", "image", "imagemap", "index-base", "index-see",
    "index-see-also", "index-sort-as", "indexterm", "indextermref", "itemgroup",
    "keyword", "kwd", "li", "line-through", "lines", "longdescref",
    "longquoteref", "lq", "markupname", "menucascade", "messagepanel",
    "msgblock", "msgnum", "msgph", "note", "numcharref", "object", "ol", "oper",
    "option", "overline", "p", "param", "parameterentity", "parml", "parmname",
    "pd", "ph", "plentry", "pre", "pt", "q", "repsep", "required-cleanup", "row",
    "screen", "sep", "shape", "shortcut", "simpletable", "sl", "sli", "sort-as",
    "state", "stentry", "sthead", "strow", "sub", "sup", "synblk", "synnote",
    "synnoteref", "synph", "syntaxdiagram", "systemoutput", "table", "tbody",
    "term", "text", "textentity", "tgroup", "thead", "title", "tm", "tt",
    "typeofhazard", "u", "uicontrol", "ul", "unknown", "userinput", "var",
    "varname", "wintitle", "xmlatt", "xmlelement", "xmlnsname", "xmlpi", "xref"];

  is_deeply [initialChildChainUnderParent "area"],            ["shape", "coords", "xref"];
  is_deeply [initialChildChainUnderParent "concept"],         ["title"];
  is_deeply [initialChildChainUnderParent "dlentry"],         ["dt", "dd"];
  is_deeply [initialChildChainUnderParent "hazardstatement"], ["messagepanel", "hazardsymbol"];

#  for my $tag(ditaTags)
#   {if (my @c = initialChildChainUnderParent($tag))
#     {say STDERR dump [$tag, \@c];
#     }
#   }

  is_deeply pathsUnderParent('step'), ["cmd"];
  is_deeply pathsUnderParent('ol'),   ["li"];
  is_deeply pathsUnderParent('row'),  ["entry"];

  is_deeply pathsUnderParent("area"),         ["shape", "coords", "xref"];
  is_deeply pathsUnderParent("chhead"),       ["choptionhd", "chdeschd"];
  is_deeply pathsUnderParent("chrow"),        ["choption", "chdesc"];
  is_deeply pathsUnderParent("copyright"),    ["copyryear", "copyrholder"];
  is_deeply pathsUnderParent("dlentry"),      ["dt", "dd"];
  is_deeply pathsUnderParent("imagemap"),     ["image", "area"];
  is_deeply pathsUnderParent("messagepanel"), ["typeofhazard", "howtoavoid"];
  is_deeply pathsUnderParent("plentry"),      ["pt", "pd"];


  if (1)                                                                        # No parent demands a sequence of more than one tag
   {my $n; my $o;
    for my $tag(ditaTags)
     {if (my $c = pathsUnderParent($tag))
       {++$n if @$c > 1;
        say STDERR "$tag: ", dump(@$c) if @$c > 1;
        #say STDERR ++$o, " ", $tag if @$c == 1;
       }
     }
    ok $n == 10;
   }
 }
