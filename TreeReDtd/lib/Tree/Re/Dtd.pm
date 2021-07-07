#!/usr/bin/perl -I/home/phil/perl/cpan/TreeOps/lib/ -I/home/phil/perl/cpan/DataDFA/lib/
#-------------------------------------------------------------------------------
# Regular expression for a DTD as a parse tree.
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

#D1 Build                                                                       # Create a tree.

sub parseTreeFromDtd($)                                                         # Create a parse tree from a L<XML> L<DTD> element definition
 {my ($string) = @_;                                                            # String representation of DTD Element definition

  my $re = unbless Data::DFA::parseDtdElementAST($string);                      # Parse DTD definition

  my sub newNode($)                                                             # Create a new parse tree node
   {my ($type) = @_;                                                            # Type

    return Tree::Ops::new                                                       # New parse tree node
      genHash(q(TreeReDtdNode),                                                 # A node in the parse tree of the DTD re
        type  => $type,                                                         # Node type: choice, element, oneOrMore, optional, sequence, zeroOrMore
        first => {},                                                            # The symbols that can start this node
        next  => {},                                                            # The next node for each symbol
        final => undef,                                                         # Whether this node is final or not
        value => undef,                                                         # The symbol represented by this node if it is an element node
       );
   }

  sub TreeReDtdNode::canBlock($)                                                #P Check whether a node can block or not
   {my ($n) = @_;                                                               # Node
    $n->type =~ m(\A(choice|element|oneOrMore|sequence)\Z)                      # Blocking nodes
   }

  sub TreeReDtdNode::canBeFirst($)                                              #P Check that a node can be first
   {my ($n) = @_;                                                               # Node
    return 1 if $n->parent and $n->parent->user->type eq q(choice);
    for my $s($n->siblingsBefore)
     {return 0 if $s->canBlock;
     }
    1
   }

  sub TreeReDtdNode::canBeLast($)                                               #P Check that a node can be last
   {my ($n) = @_;                                                               # Node
    for my $s($n->siblingsAfter)
     {return 0 if $s->canBlock;
     }
    1
   }

  sub Tree::Ops::pp($)                                                          # Print a parse tree
   {my ($tree) = @_;                                                            # Parse tree to print
    $tree->brackets(sub
     {my ($n) = @_;
      $n->type eq 'element' ? $n->value : $n->type;                             # Either the node type or its value for elements
     }, ',');
   }

  my sub convert($$)                                                            #P Create a modifiable parse tree from the RE parse tree
   {my ($parent, $node) = @_;
    if (ref($node))
     {my ($o, @p)   = @$node;
      my $n = newNode $o;
      $parent->putLast($n);
      __SUB__->($n, $_) for @p;
     }
    else
     {$parent->user->value = $node;
     }
   }

  my $t = newNode(q(re));                                                       # Create modifiable parse tree
  convert($t, $re);

  $t->by(sub                                                                    # Load modifiable parse tree
   {my ($o) = @_;                                                               # Improve parse tree
    my @c = $o->context;

    if (@c and $c[0]->user->type eq 'choice')                                   # Check choices are unique
     {my %c;                                                                    # Choices seen so far
      for my $c($o->children->@*)                                               # Each choice
       {my $b = $c->pp;                                                         # String representation of choice
        $c->cut if $c{$b}++;                                                    # Remove choice if already seen
       }
      $c[0]->unwrap if $c[0]->children->@* == 1;                                # Unwrap single choices
     }

    if (@c > 1)                                                                 # Unwrap nested sequences and choices
     {my ($p, $q) = map {$_->user->type} @c;

      if ($p eq 'sequence' && $q eq 'sequence' or                               # Remove degenerate forms
          $p eq 'choice'   && $q eq 'choice')
       {$o->unwrap;
       }
     }

    if (my $p = $o->singleChildOfParent)                                        # Remove degenerate forms with one child
     {my ($O, $P) = map {$_->user->type} $o, $p;
      if      ($O eq 'choice')     {$o->unwrap}                                 # Unwrap single choices
      elsif   ($O eq 'oneOrMore')
       {if    ($P eq 'choice')     {$p->unwrap}
        elsif ($P eq 'oneOrMore')  {$p->unwrap}
        elsif ($P eq 'optional')   {$p->unwrap; $o->user->type = 'zeroOrMore'}
        elsif ($P eq 'sequence')   {$p->unwrap}
        elsif ($P eq 'zeroOrMore') {$o->unwrap}
       }
      elsif   ($O eq 'optional')
       {if    ($P eq 'choice')     {}
        elsif ($P eq 'oneOrMore')  {$o->unwrap; $p->user->type = 'zeroOrMore'}
        elsif ($P eq 'optional')   {$p->unwrap}
        elsif ($P eq 'sequence')   {$p->unwrap}
        elsif ($P eq 'zeroOrMore') {$o->unwrap}
       }
      elsif   ($O eq 'sequence')   {$o->unwrap unless $O eq q(element)}
      elsif   ($O eq 'zeroOrMore' and $P !~ 'choice|element') {$p->unwrap}
     }
   });

  $t->by(sub                                                                    # Map first and next elements
   {my ($node) = @_;                                                            # Element of modifiable parse tree
    my $user = $node->user;
    my $type = $user->type;
    if ($element eq a(element))
     {my $value   = $element->value
      my @context = $node->context;
      for my $context(@context)                                                 # First at each level of context
       {if ($context->user->canBeFirst)
         {for my $sibling($context->siblingsBefore)
           {confess "Disagreement on source of $value" if $sibling->user->first->{$value};
            $sibling->user->first->{$value} = $node;
           }
         }
       }
      for my $context(@context)                                                 # Last == next at each level of context
        if ($context->user->canBeLast)
         {for my $sibling($context->siblingsAfter)
           {confess "Disagreement on target of $value" if $sibling->user->next->{$value};
            $sibling->user->next->{$value} = $node;
           }
         }
       }
      for my $context(@context)                                                 # Last == next at each level of context
        if ($context->user->canBeLast)
         {for my $sibling($context->siblingsAfter)
           {confess "Disagreement on target of $value" if $sibling->user->next->{$value};
            $sibling->user->next->{$value} = $node;
           }
         }
       }
     }
   });

  $t
 } # parseTreeFromDtd

sub validateSymbols($$;$)                                                       # Validate a stream of symbols against a parse tree representation of a regular expression
 {my ($re, $symbols, $errors) = @_;                                             # Parse tree representation of a regular expression, array of symbols
  my $r = parseTreeFromDtd($re);
  my @p = [$r->children->[0], 0, undef];                                        # Node to process next, number of times processed, virtual if true
  my @s = @$symbols;
  my $error;

  my sub onElement
   {@p and $p[-1][0]->user eq 'element'
   }

  my sub onChoice
   {@p and $p[-1][0]->user eq 'choice'
   }

  my sub onSequence
   {@p and $p[-1][0]->user eq 'sequence'
   }

  my sub onOneOrMore
   {@p and $p[-1][0]->user eq 'oneOrMore'
   }

  my sub onOptional
   {@p and $p[-1][0]->user eq 'optional'
   }

  my sub onZeroOrMore
   {@p and $p[-1][0]->user eq 'zeroOrMore'
   }

  my sub underChoice
   {@p > 1 and $p[-2][0]->user eq 'choice'
   }

  my sub underOneOrMore
   {@p > 1 and $p[-2][0]->user eq 'oneOrMore'
   }

  my sub underOptional
   {@p > 1 and $p[-2][0]->user eq 'optional'
   }

  my sub isLast
   {@p and $p[-1][0]->isLast;
   }

  my sub matches
   {
say STDERR "YYYY", dump($p[-1][0]->children->[0]->user, $s[0]) if @p and @s;

     $error = !@p || !@s || $p[-1][0]->children->[0]->user ne $s[0] ? __LINE__ : undef;
    if (!$error)
     {shift @s unless underOptional and $p[-2][1];
      $p[-2]->[1]++ if underOneOrMore or underOptional;                         # Choice has been satisfied
     }
 #   else {confess}
   }
  my $goFirst;
  my $goNext;
  my $goUpNextFirst;

  $goFirst = sub                                                                # Before processing a node
   {if (!$error)
     {if    (onElement)
       {matches;
       }
      elsif (onChoice)
       {if (@p)
         {for my $c($p[-1][0]->children->@*)
           {push @p, [$c, 0, 1];
            $error = undef;
            __SUB__->();
            return unless $error;
            pop @p;
           }
          $error = __LINE__;
         }
       }
      elsif (onZeroOrMore or onOptional)
       {if (@p)
         {my $c = $p[-1][0]->children->[0];
          push @p, [$c, 0, 1];
          __SUB__->();
say STDERR "ZZZZ ", scalar(@p), " ", $p[-1][0]->user, " ", dump($error, @s, scalar@p);
          if ($error)
           {$error = undef;
            if (isLast)
             {pop @p;
              &$goNext;
             }
            else
             {#confess "CCCC";
             }
           }
         }
       }
      else
       {if   (!@s)
         {if (onSequence or onOneOrMore)
           {$error = __LINE__;
           }
         }
        elsif (@p)
         {push @p, [$p[-1][0]->children->[0], 0, 1];
          __SUB__->();
         }
       }
     }
   };

  $goUpNextFirst = sub
#  {if (isLast)
   {while(@p and isLast)
     {pop @p;
     }
    &$goNext;
    &$goFirst;
   };

  $goNext = sub
   {say STDERR "HHHH ", dump($error, isLast, scalar@p);
    if (!$error)
     {if (isLast)
       {if (onOneOrMore)
         {&$goFirst;
          $error = undef if @p and $p[-1][1];
         }
        elsif (onZeroOrMore)
         {&$goFirst;
          $error = undef;
         }
        elsif (onElement)
         {matches;
         }

        &$goUpNextFirst
       }
      elsif (underChoice)
       {pop @p; pop @p;
        if (isLast)
         {&$goUpNextFirst
         }
        else
         {&$goFirst
         }
       }
      elsif (@p)                                                                # Step to next parse tree element
       {my $q = pop @p;
        push @p, [$q->[0]->next, 0];
        &$goFirst
       }
     }
    else
     {
       #confess "AAAA";
     }
#  confess "BBBB";
   };

  my $finish = sub
   {while(@p and !$error)
     {if (isLast)
       {if (onOneOrMore)
         {$error = __LINE__ if !$p[-1][1];
         }
        pop @p unless $error;
       }
      elsif (underChoice)
       {pop @p; pop @p;
       }
      else
       {&$goUpNextFirst;
       }
     }
   };

  say STDERR "AAAA ", dump($error, \@p);
  say STDERR "BBBB ", dump(\@s);
  &$goFirst;
  say STDERR "CCCC ", dump($error, \@p);
  say STDERR "DDDD ", dump(\@s);
  while(@p and @s and !$error)
   {&$goNext;
    say STDERR "EEEE ", dump($error, \@p);
    say STDERR "FFFF ", dump(\@s);
   }
  say STDERR "GGGG ", dump($error, \@p, \@s);
  &$finish();

  if    (!@p and !@s) {say STDERR "Success ", dump($error)}
  elsif (         @s) {say STDERR "Remaining symbols are:", dump(\@s)}
  elsif ( @p)         {say STDERR "Incomplete input, ", scalar(@p)}
  else                {say STDERR "Error at:", dump(\@s)}

  return !@p && !@s && !$error;

 } # validateSymbols

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
#ok  validateSymbols(q/(a, b*)/,  [qw(a)]); exit;

is_deeply parseTreeFromDtd(q/(((c*)+)*)/)                         ->pp, q/re(zeroOrMore(c))/;
is_deeply parseTreeFromDtd(q/(a, (b | b), ((((((c|d)*)+)*)+)?)?)/)->pp, q/re(sequence(a,b,zeroOrMore(choice(c,d))))/;
is_deeply parseTreeFromDtd(q/(a, (b|c|d), (e, f)+, g?)/)          ->pp, q/re(sequence(a,choice(b,c,d),oneOrMore(e,f),optional(g)))/;
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
