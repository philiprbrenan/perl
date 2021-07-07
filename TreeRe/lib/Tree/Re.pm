#!/usr/bin/perl -I/home/phil/perl/cpan/TreeOps/lib/ -I/home/phil/perl/cpan/DataDFA/lib/
#-------------------------------------------------------------------------------
# Regular expression as a parse tree.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2020
#-------------------------------------------------------------------------------
# podDocumentation
package Tree::Re;
our $VERSION = 20200627;
require v5.26;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess);
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use Data::DFA;
use Tree::Ops;
use feature qw(say current_sub);

my $logFile = q(/home/phil/z/z/z/zzz.txt);                                      # Log printed results if developing

#D1 Build                                                                       # Create a tree.

sub grammarFromDtd($)                                                           # Create a parse tree from a L<XML> L<DTD> element definition
 {my ($definition) = @_;                                                        # DTD Element Definition

  my sub convert($$)                                                            #P Create a parse tree to represent the regular expression.
   {my ($parent, $node) = @_;                                                   # Parent parse tree element, current element of RE parse tree
    if (ref($node))                                                             # Expression
     {my ($o, @p)   = @$node;
      $parent->putLast(my $n = Tree::Ops::new $o);
      __SUB__->($n, $_) for @p;                                                 # Add sub expressions
     }
    else                                                                        # Element
     {$parent->putLast (Tree::Ops::new $node);
     }
    $parent
   }

  my sub collapseTree($)                                                        #P Collapse out degenerate structures
   {my ($t) = @_;                                                               # Parse tree to collapse

    $t->by(sub                                                                  # Load modifiable parse tree
     {my ($o) = @_;                                                             # Improve parse tree
      my @c = $o->context;

      if (@c and $c[0]->key eq 'choice')                                        # Check choices are unique
       {my %c;                                                                  # Choices seen so far
        for my $c($o->children->@*)                                             # Each choice
         {my $b = $c->brackets;                                                 # String representation of choice
          $c->cut if $c{$b}++;                                                  # Remove choice if already seen
         }
        $c[0]->unwrap if $c[0]->children->@* == 1;                              # Unwrap single choices
       }

      if (@c > 1)                                                               # Unwrap nested sequences and choices
       {my ($p, $q) = map {$_->key} @c;

        if ($p eq 'sequence' && $q eq 'sequence' or                             # Remove degenerate forms
            $p eq 'choice'   && $q eq 'choice')
         {$o->unwrap;
         }
       }

      if (my $p = $o->singleChildOfParent)                                      # Remove degenerate forms with one child
       {my ($O, $P) = map {$_->key} $o, $p;
        if      ($O eq 'choice')     {$o->unwrap}                               # Unwrap single choices
        elsif   ($O eq 'oneOrMore')
         {if    ($P eq 'choice')     {$p->unwrap}
          elsif ($P eq 'oneOrMore')  {$p->unwrap}
          elsif ($P eq 'optional')   {$p->unwrap; $o->key = 'zeroOrMore'}
          elsif ($P eq 'sequence')   {$p->unwrap}
          elsif ($P eq 'zeroOrMore') {$o->unwrap}
         }
        elsif   ($O eq 'optional')
         {if    ($P eq 'choice')     {}
          elsif ($P eq 'oneOrMore')  {$o->unwrap; $p->key = 'zeroOrMore'}
          elsif ($P eq 'optional')   {$p->unwrap}
          elsif ($P eq 'sequence')   {$p->unwrap}
          elsif ($P eq 'zeroOrMore') {$o->unwrap}
         }
        elsif   ($O eq 'sequence')   {$o->unwrap}
        elsif   ($O eq 'zeroOrMore' and $P !~ 'choice|element') {$p->unwrap}
       }
     });
    $t
   }

  my $re   = Data::DFA::parseDtdElementAST $definition;                         # Parse DTD definition
  my $dfa  = Data::DFA::fromExpr $re;                                           # Create DFA
  confess "Not univalent" unless $dfa->univalent;                               # Confirm that the DFA is univalent

  my $tree = Tree::Ops::new(q(re));                                             # Create a parse tree
  convert      $tree, $re;                                                      # Convert the RE to a parse tree
  collapseTree $tree;                                                           # Remove redundant layers from the parse tree
  $tree
 } # grammarFromDtd

sub parse($$)                                                                   # Use a grammar to parse an array of symbols
 {my ($grammar, $symbols) = @_;                                                 # Grammar, symbols

  my %terminals;                                                                # Index of terminals in the grammar
  $terminals{$_->key} = $_ for $grammar->leaves;                                # Each terminal

  my $parse = $grammar->transcribe;                                             # Create initial the parse tree

  my $p;                                                                        # Initial parse position
  for my $symbol(@$symbols)                                                     # Each symbol
   {if (my $q = $terminals{$symbol})                                            # Leaf in parse tree corresponding to this symbol
     {if (defined($p) and $q == $p || $q->before($p))                           # Repeat
       {my $a = $p->mostRecentCommonAncestor($q);                               # Could not repeat lower
        my @c = $a->context;                                                    # Context of common ancestor
        for my $c(@c)                                                           # Look for first repetition
         {if ($c->key =~ m(OrMore)i)                                            # First repetition
           {my $C = $c->{transcribedTo};                                        # Where the repetition point was mapped to
            my $d = $C->putLast($c->dup);                                       # Repeat
            $d->unwrap;                                                         # Unwrap the secondary point of repetition
            last;
           }
         }
       }
      $p = $q;                                                                  # Next position in parse
     }
    else
     {confess "Cannot locate symbol $symbol";
     }
   }
  owf($logFile, $parse->print) if -e $logFile;
  $parse
 } # parse

#D1 Data Structures                                                             # Data structures use by this package.

#D0
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

Tree::Re - Regular expression for a DTD as a parse tree.

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
__DATA__
use warnings FATAL=>qw(all);
use strict;
require v5.26;
use Test::More tests=>44;

makeDieConfess;

#goto latestTest;

is_deeply grammarFromDtd(q/(((c*)+)*)/)                         ->brackets, q/re(zeroOrMore(element(c)))/;
is_deeply grammarFromDtd(q/(a, (b | b), ((((((c|d)*)+)*)+)?)?)/)->brackets, q/re(sequence(element(a)element(b)zeroOrMore(choice(element(c)element(d)))))/;
is_deeply grammarFromDtd(q/(a, (b|c|d), (e, f)+, g?)/)          ->brackets, q/re(sequence(element(a)choice(element(b)element(c)element(d))oneOrMore(element(e)element(f))optional(element(g))))/;

if (1)                                                                          #
 {my $g = grammarFromDtd(q/(a+, b+)/);
  is_deeply parse($g, [qw(a b b)])->print, <<END;
Key            Value
re
  sequence
    oneOrMore
      element
        a
    oneOrMore
      element
        b
      element
        b
END
 }

if (1)                                                                          #
 {my $g = grammarFromDtd(q/(a+, b?, c+)/);
  is_deeply parse($g, [qw(a a b c c)])->print, <<END;
Key            Value
re
  sequence
    oneOrMore
      element
        a
      element
        a
    optional
      element
        b
    oneOrMore
      element
        c
      element
        c
END
 }

if (1)                                                                          #
 {my $g = grammarFromDtd(q/(a+, (b|c), d+)/);
  is_deeply $g->brackets, q/re(sequence(oneOrMore(element(a))choice(element(b)element(c))oneOrMore(element(d))))/;
  is_deeply parse($g, [qw(a a c d d)])->print, <<END;
Key            Value
re
  sequence
    oneOrMore
      element
        a
      element
        a
    choice
      element
        b
      element
        c
    oneOrMore
      element
        d
      element
        d
END
 }

if (1)                                                                          #
 {my $g = grammarFromDtd(q/((a, b)+, (c, d)+)/);
  is_deeply parse($g, [qw(a b a b c d c d)])->print, <<END;
Key            Value
re
  sequence
    oneOrMore
      element
        a
      element
        b
      element
        a
      element
        b
    oneOrMore
      element
        c
      element
        d
      element
        c
      element
        d
END
 }

exit;

ok  validateSymbols(q/(a)/,     [qw(a)]);
ok !validateSymbols(q/(a)/,     [qw(b)]);
ok  validateSymbols(q/(a, b)/,  [qw(a b)]);
ok !validateSymbols(q/(a, b)/,  [qw(a)]);
ok !validateSymbols(q/(a, b)/,  [qw(b)]);
ok  validateSymbols(q/(a | b)/, [qw(a)]);
ok  validateSymbols(q/(a | b)/, [qw(b)]);
ok !validateSymbols(q/(a | b)/, [qw(c)]);
ok !validateSymbols(q/(a | b)/, [qw(a b)]);

#ok  validateSymbols(q/(a*)/,    [qw()]);
ok  validateSymbols(q/(a*)/,    [qw(a)]);
ok  validateSymbols(q/(a*)/,    [qw(a a)]);
#ok !validateSymbols(q/(a*)/,    [qw(b a)]);

ok !validateSymbols(q/(a+)/,    [qw()]);
ok  validateSymbols(q/(a+)/,    [qw(a)]);
ok  validateSymbols(q/(a+)/,    [qw(a a)]);
ok !validateSymbols(q/(a+)/,    [qw(b a)]);

ok  validateSymbols(q/(a?)/,    [qw()]);
ok  validateSymbols(q/(a?)/,    [qw(a)]);
ok !validateSymbols(q/(a?)/,    [qw(a a)]);
ok !validateSymbols(q/(a?)/,    [qw(b a)]);

ok !validateSymbols(q/(a, b?)/,  [qw()]);
ok  validateSymbols(q/(a, b?)/,  [qw(a)]);
ok  validateSymbols(q/(a, b?)/,  [qw(a b)]);
ok !validateSymbols(q/(a, b?)/,  [qw(a b b)]);
ok !validateSymbols(q/(a, b?)/,  [qw(b a)]);

ok !validateSymbols(q/(a, b+)/,  [qw()]);
ok !validateSymbols(q/(a, b+)/,  [qw(a)]);
ok  validateSymbols(q/(a, b+)/,  [qw(a b)]);
ok  validateSymbols(q/(a, b+)/,  [qw(a b b)]);
ok !validateSymbols(q/(a, b+)/,  [qw(a b a)]);

ok !validateSymbols(q/(a, b*)/,  [qw()]);
#ok  validateSymbols(q/(a, b*)/,  [qw(a)]);
#ok  validateSymbols(q/(a, b*)/,  [qw(a b)]);
#ok  validateSymbols(q/(a, b*)/,  [qw(a b b)]);
#ok !validateSymbols(q/(a, b*)/,  [qw(a b a)]);

ok !validateSymbols(q/(a, (b, (c, d?)*)+)/,  [qw()]);
ok !validateSymbols(q/(a, (b, (c, d?)*)+)/,  [qw(a)]);
ok  validateSymbols(q/(a, (b, (c, d?)*)+)/,  [qw(a b)]);
ok  validateSymbols(q/(a, (b, (c, d?)*)+)/,  [qw(a b b)]);
ok  validateSymbols(q/(a, (b, (c, d?)*)+)/,  [qw(a b c)]);
ok !validateSymbols(q/(a, (b, (c, d?)*)+)/,  [qw(a b b c)]);
ok  validateSymbols(q/(a, (b, (c, d?)*)+)/,  [qw(a b c d)]);

ok !validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw()]);
ok  validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw(a)]);
ok !validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw(a b)]);
ok !validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw(a b b)]);
ok  validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw(a b c)]);
ok !validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw(a b b c)]);
ok  validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw(a b c c)]);
ok  validateSymbols(q/(a, (b, (c, d?)+)*)/,  [qw(a b c d)]);

ok !validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw()]);
ok  validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a)]);
ok !validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a b)]);
ok !validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a b b)]);
ok !validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a b c)]);
ok  validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a b d)]);
ok !validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a b b c)]);
ok !validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a b c c)]);
ok  validateSymbols(q/(a, (b, (c?, d)+)*)/,  [qw(a b c d)]);

#done_testing;
#   owf(q(/home/phil/z/z/z/zzz.txt), $dfa->dumpAsJson);
