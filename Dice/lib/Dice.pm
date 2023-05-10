#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Throw several dice and analyze the results
# Philip R Brenan at gmail dot com, 2022
#-------------------------------------------------------------------------------
# podDocumentation
package Dice;
our $VERSION = "20221018";
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Table::Text qw(:all);

#D1 Dice                                                                        # Throw several dice and analyze the results.

sub new($;$)                                                                    # Create a new dice ready to throw.
 {my ($dice, $dots) = @_;                                                       # The number of dice to throw, optional number of dots on the dice
  scalar(@_) =~ m(\A[12]\Z) or confess "One or two parameters required\n";
  $dots > 0                 or confess "Number of dots must be one or more\n";

  genHash(__PACKAGE__,                                                          # Dice object
    throws   => 0,                                                              # Number of throws
    number   => $dice,                                                          # Number of dice
    dots     => $dots // 6,                                                     # Number of dots on dice
    result   => undef,                                                          # Last throw result
    analysis => undef,                                                          # Analysis of last throw
   );
 }

sub throw($)                                                                    # Perform a throw of the dice.
 {my ($dice) = @_;                                                              # Dice thrower
  @_ == 1 or confess "One parameter required\n";

  my @t;                                                                        # The results of the throw
  my $d = $dice->dots;                                                          # Dots on the dice
  my $n = $dice->number;                                                        # Number if dice

  $dice->result = [];
  push $dice->result->@*, int(rand($d)+1) for 1..$n;                                 # Throws
  $dice->throws++;                                                              # Increment number of throws
  $dice                                                                         # Chain
 }

sub fix($@)                                                                     # Perform a fixed throw of the dice - useful for testing because we use a random number generator to perform the actual throw.
 {my ($dice, @values) = @_;                                                     # Dice thrower, values of dice
  @_ >= 1 or confess "At least one parameter required\n";

  my $n = $dice->number;                                                        # Number if dice
  @values == $n or confess "$n throws required, you gave ".scalar(@values)."\n";# check we were handed the right number of dice values

  my $d = $dice->dots;                                                          # Number of dots
  for my $i(keys @values)                                                       # Validate supplied value of each throw
   {my $v = $values[$i];
    if ($v < 1 or $v > $d)                                                      # Out of range
     {confess "Invalid throw $i. Value $v is out of range, should be 1 to $d\n";
     }
   }
  $dice->result = [@values];                                                    # Save fixed results
  $dice->throws++;                                                              # Increment number of throws
  $dice                                                                         # Chain
 }

sub numberToWord($)                                                             #P Convert a number in the range 0..10 to a word else return the number as is
 {my ($number) = @_;                                                            # Number
  @_ == 1 or confess "One parameter required\n";

  my %n = qw(0 zero 1 one 2 two 3 three 4 four 5 five 6 six 7 seven 8 eight 9 nine 10 ten);
  $n{$number} // $number;                                                       #  Represent number as a word if possible
 }

sub analyze($)                                                                  # Analyze the result of the last throw. Indicate whether several dice show the same value and on the length of the longest consecutive sequence.
 {my ($dice) = @_;                                                              # Dice thrower
  @_ == 1 or confess "One parameter required\n";

  my @m;                                                                        # Things we would like to tell the user about their dice throw

  if (1)                                                                        # Check for several of a kind
   {my %c;                                                                      # Counts
    $c{$_}++  for $dice->result->@*;                                            # Find dice showing the same value

    my ($h) = sort {$$b[0] <=> $$a[0]} map {[$c{$_}, $_]} keys %c;              # Sort dice so that highest are first
    my ($count, $score) = @$h;                                                  # Highest count, corresponding score

    push @m, ucfirst numberToWord($count)." of a kind" if $count > 1;           # More than one dice had the same value
   }

  if (1)                                                                        # Check for longest sequence
   {my @s = sort {$a <=> $b} $dice->result->@*;                                 # Ordered by value
    my $longestStart = 0; my $longestEnd = 0;                                   # Longest sequence:  so far, start of sequence, end of sequence
    my $currentStart = 0; my $currentEnd = 0;                                   # Current sequence: start of current sequence, end of current sequence
    for my $i(1..$#s)                                                           # Each throw in value order
     {my $s = $s[$i];
      if ($s == $s[$i-1] + 1)                                                   # The sequence continues
       {$currentEnd = $i;
        if ($currentEnd - $currentStart > $longestEnd - $longestStart)          # A new longest sequence
         {$longestStart = $currentStart; $longestEnd = $currentEnd;
         }
       }
      else                                                                      # The end of the sequence
       {$currentStart = $i;
        $currentEnd   = $i;
       }
     }

    if ($longestEnd > $longestStart)                                            # Remark on the longest sequence if it is interesting
     {my $n = numberToWord $longestEnd - $longestStart + 1;                     # Start of longest sequence
      my $a = numberToWord $s[$longestStart];                                   # Start of longest sequence
      my $b = numberToWord $s[$longestEnd];                                     # End of longest sequence
      push @m, ucfirst "$n in a row from $a to $b";                             # Details of sequence
     }

    if (@m == 0)                                                                # Remark on the fact that nothing much happened
     {push @m, "Nothing interesting occurred";
     }
   }

  $dice->analysis = join "\n", @m, '';                                          # Assemble verdict with a new line on the end
  $dice                                                                         # Chain
 }

#d
#-------------------------------------------------------------------------------
# Export - eeee
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA          = qw(Exporter);
@EXPORT       = qw(new throw fix analyze);
@EXPORT_OK    = qw();
%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

# podDocumentation
=pod

=encoding utf-8

=head1 Name

Dice - Throw some dice and analyze the results

=head1 Synopsis

Dice - Throw a specified number of dice with a specified number of dots on them
and analyze the results looking for the most dice showing the same value and
for the longest consecutive sequence.

=head1 Description

Throw some dice and analyze the results


Version "20221018".


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Dice

Throw several dice and analyze the results.

=head2 new($dice, $dots)

Create a new dice ready to throw.

     Parameter  Description
  1  $dice      The number of dice to throw
  2  $dots      Optional number of dots on the dice

B<Example:>



    my $d = Dice::new(10, 6);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       $d->fix(qw(4 2 6 4 3 6 1 4 4 6));
       $d->analyze;
    is_deeply $d->analysis, <<END;
  Four of a kind
  Four in a row from one to four
  END
  }

  if (1) {

    my $d = Dice::new(10, 6);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       $d->throw();
    ok $d->result->@* == 10;
  }



=head2 throw($dice)

Perform a throw of the dice.

     Parameter  Description
  1  $dice      Dice thrower

B<Example:>


    my $d = Dice::new(10, 6);

       $d->throw();  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok $d->result->@* == 10;
  }



=head2 fix($dice, @values)

Perform a fixed throw of the dice - useful for testing because we use a random number generator to perform the actual throw.

     Parameter  Description
  1  $dice      Dice thrower
  2  @values    Values of dice

B<Example:>


    my $d = Dice::new(10, 6);

       $d->fix(qw(4 2 6 4 3 6 1 4 4 6));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

       $d->analyze;
    is_deeply $d->analysis, <<END;
  Four of a kind
  Four in a row from one to four
  END
  }

  if (1) {
    my $d = Dice::new(10, 6);
       $d->throw();
    ok $d->result->@* == 10;
  }



=head2 analyze($dice)

Analyze the result of the last throw. Indicate whether several dice show the same value and on the length of the longest consecutive sequence.

     Parameter  Description
  1  $dice      Dice thrower

B<Example:>


    my $d = Dice::new(10, 6);
       $d->fix(qw(4 2 6 4 3 6 1 4 4 6));

       $d->analyze;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    is_deeply $d->analysis, <<END;
  Four of a kind
  Four in a row from one to four
  END
  }

  if (1) {
    my $d = Dice::new(10, 6);
       $d->throw();
    ok $d->result->@* == 10;
  }




=head1 Hash Definitions




=head2 Dice Definition


Dice object




=head3 Output fields


=head4 analysis

Analysis of last throw

=head4 dots

Number of dots on dice

=head4 number

Number of dice

=head4 result

Last throw result

=head4 throws

Number of throws



=head1 Private Methods

=head2 numberToWord($number)

Convert a number in the range 0..10 to a word else return the number as is

     Parameter  Description
  1  $number    Number


=head1 Index


1 L<analyze|/analyze> - Analyze the result of the last throw.

2 L<fix|/fix> - Perform a fixed throw of the dice - useful for testing because we use a random number generator to perform the actual throw.

3 L<new|/new> - Create a new dice ready to throw.

4 L<numberToWord|/numberToWord> - Convert a number in the range 0.

5 L<throw|/throw> - Perform a throw of the dice.

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Dice

=head1 Author

L<philiprbrenan@gmail.com|mailto:philiprbrenan@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

=head1 Copyright

Copyright (c) 2016-2021 Philip R Brenan.

This module is free software. It may be used, redistributed and/or modified
under the same terms as Perl itself.

=cut

1;

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
use Time::HiRes qw(time);
use Test::More;

my $d = Dice::new(10, 6);                                                       # 10 dice with 6 dots

eval {$d->fix(qw(4))};
ok $@ =~ m"10 throws required, you gave 1";

eval {$d->fix(qw(4 2 6 4 3 6 1 4 4 9))};
ok $@ =~ m"Invalid throw 9. Value 9 is out of range, should be 1 to 6";

if (1) {                                                                        #Tthrows
  $d->throw;
  is_deeply $d->throws, 1;
}

if (1) {                                                                        #Tnew #Tfix #Tanalyze
  my $d = Dice::new(10, 6);
     $d->fix(qw(4 2 6 4 3 6 1 4 4 6));
     $d->analyze;
  is_deeply $d->analysis, <<END;
Four of a kind
Four in a row from one to four
END
}

if (1) {                                                                        #Tthrow
  my $d = Dice::new(10, 6);
     $d->throw();
  ok $d->result->@* == 10;
}

done_testing;
