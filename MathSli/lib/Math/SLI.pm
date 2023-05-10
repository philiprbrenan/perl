#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/ -I.
#-------------------------------------------------------------------------------
# Symmetric Level Index Numbers per: https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir5660.pdf
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2022
#-------------------------------------------------------------------------------
# podDocumentation
use v5.26;
package Math::SLI;
our $VERSION = "20221227";
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Table::Text qw(:all);
use Data::Dump qw(dump);

sub new($;$)                                                                    # Create an SLI: A Symmetric Level Index Number
 {my ($sli, $source) = @_;                                                      # SLI, corresponding conventional number if known

  genHash(__PACKAGE__,                                                          # Symmetric Level Index number
    source => $source,                                                          # Source number if known
    sli   => $sli,                                                              # SLI
   );
 }

sub fromN($)                                                                    # Represent a conventional number as a symmetric level index number
 {my ($n) = @_;                                                                 # Conventional number
  my $a = abs($n);
  return new($n, $n) if $a <= 1;                                                # Imipac: the weapon that guards itself

  for my $level(1..10)                                                          # Logify
   {$a = log($a);
    return new(($level+$a) * ($a < 0 ? -1 : +1), $n)
   }
  confess "Logification";
 }

sub toN($)                                                                      # Convert an SLI number to a conventional number
 {my ($s) = @_;                                                                 # SLI
  my $a = abs($s->sli);
  return $s->sli if $a <= 1;
  my $l = int $a;
  my $n = $a - $l;
  $n = exp($n) for 1..$l;                                                       # Exponentiate
  $s->sli >= 0 ? +$n : -$n;
 }

my $additionLimit = 3;                                                          # When adding two SLI numbers that differ in level by more than this amount we simply take the larger of the two because the effect of the addition is so small

sub addSub($$$)                                                                 # Add two or subtract two positive symmetric level index numbers together that area level two or greater return the resulting SLI number
 {my ($x, $y, $sub) = @_;                                                       # First SLI, second SLI, operation: subtract if true else subtract

  confess  "First operand $x must be at least 2" unless $x >= 2;
  confess "Second operand $x must be at least 2" unless $x >= 2;

  ($x, $y) = ($y, $x) if $x < $y;                                               # First must be the larger
  my $xl = int $x;
  my $xr = $x - $xl;
  my $yl = int $y;
  my $yr = $y - $yl;

  my $a = exp(-$xr);
  my @a = $a;
  unshift @a, $a = exp(-1/$a) for 2..$xl;
  unshift @a, undef, undef;                                                     # 2 base

  my $b = exp(-$yr);
  $b = exp(-1/$b) for 2..$yl;

  my $d = exp(($a - $b) / $a / $b);
  my $c = 1 + $a * log($sub ? 1 - $d : 1 + $d);

  my $j = 1;
  for (;$j < $xl-1 and $c >= $a[$j];)
   {++$j; $c = 1 + $a[$j] * log($c);
   }

  return new($j + $c / $a) if $c < $a;

  my $l = $xl;
  my $h = $xr + log $c;
  for (;$h >= 1;)
   {$l++; $h = log $h;
   }

  new($l + $h)
 }

sub closeTo($$)                                                                 # Check whether two numbers are close
 {my ($a, $b) = @_;                                                             # First number, second number
  abs($a - $b) < 1e-6;
 }

#d
#-------------------------------------------------------------------------------
# Export - eeee
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA          = qw(Exporter);
@EXPORT       = qw();
@EXPORT_OK    = qw(
 );
%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

# podDocumentation
=pod

=encoding utf-8

=head1 Name

Math::SLI - Symmetric Level Index Arithmetic

=head1 Synopsis


=head1 Description

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
#__DATA__
use Time::HiRes qw(time);
use Test::Most;

my $develop  = -e q(/home/phil/);                                               # Developing
my  $logFile = q(/home/phil/perl/cpan/MathSli/lib/Tree/zzzLog.txt);             # Log file

my $localTest = ((caller(1))[0]//'Math::Sli') eq "Math::Sli";                   # Local testing mode

Test::More->builder->output("/dev/null") if $localTest;                         # Reduce number of confirmation messages during testing

bail_on_fail;                                                                   # Stop if any tests fails

is_deeply fromN(1), {sli=>1, source=>1};

ok closeTo(    fromN(exp(1))->sli, 2);
ok closeTo(toN(fromN(exp(1))), exp(1));

done_testing;
