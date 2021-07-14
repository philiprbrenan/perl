#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
#-------------------------------------------------------------------------------
# Create a parse tree from an array of terms representing an expression.
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2021
#-------------------------------------------------------------------------------
package Tree::Term;
use v5.26;
our $VERSION = 20210714;                                                        # Version
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess cluck);
use Data::Dump qw(dump ddx pp);
use Data::Table::Text qw(:all);
use feature qw(say state current_sub);

#D1 Parse                                                                       # Create a parse tree from an array of terms representing an expression.

sub new($@)                                                                     #P New term.
 {my ($operator, @operands) = @_;                                               # Operator, operands.

  my $t = genHash(__PACKAGE__,                                                  # Description of a term in the expression.
     operands => @operands ? [@operands] : undef,                               # Operands to which the operator will be applied.
     operator => $operator,                                                     # Operator to be applied to one or more operands.
     up       => undef,                                                         # Parent term if this is a sub term.
   );

  $_->up = $t for grep {ref $_} @operands;                                      # Link to parent if possible

  $t
 }

sub LexicalCode($$$)                                                            #P Lexical code definition
 {my ($letter, $next, $name) = @_;                                              # Letter used to refer to the lexical item, letters of items that can follow this lexical item, descriptive name of lexical item
  genHash(q(Tree::Term::LexicalCode),                                           # Lexical item codes.
    letter => $letter,                                                          # Letter code used to refer to the lexical item.
    next  => $next,                                                             # Letters codes of items that can follow this lexical item.
    name  => $name,                                                             # Descriptive name of lexical item.
   );
 }

my $LexicalCodes = genHash(q(Tree::Term::Codes),                                # Lexical item codes.
  a => LexicalCode('a', 'bpv',   'assignment operator'),                        # Infix operator with priority 2 binding right to left typically used in an assignment.
  b => LexicalCode('b', 'bBpsv', 'opening parenthesis'),                        # Opening parenthesis.
  B => LexicalCode('B', 'aBdqs', 'closing parenthesis'),                        # Closing parenthesis.
  d => LexicalCode('d', 'abpv',  'dyadic operator'),                            # Infix operator with priority 3 binding left to right typically used in arithmetic.
  p => LexicalCode('p', 'bpv',   'prefix operator'),                            # Monadic prefix operator.
  q => LexicalCode('q', 'aBdqs', 'suffix operator'),                            # Monadic suffix operator.
  s => LexicalCode('s', 'bBpsv', 'semi-colon'),                                 # Infix operator with priority 1 binding left to right typically used to separate statements.
  t => LexicalCode('t', 'aBdqs', 'term'),                                       # A term in the expression.
  v => LexicalCode('v', 'aBdqs', 'variable'),                                   # A variable in the expression.
 );

my $first = 'bpsv';                                                             # First element
my $last  = 'Bqsv';                                                             # Last element

sub LexicalStructure()                                                          # Return the lexical codes and their relationships in a data structure so this information can be used in other contexts.
 {genHash(q(Tree::Term::LexicalStructure),                                      # Lexical item codes.
    codes => $LexicalCodes,                                                     # Code describing each lexical item
    first => $first,                                                            # Lexical items we can start with
    last  => $last,                                                             # Lexical items we can end with
   );
 }

sub type($)                                                                     #P Type of term
 {my ($s) = @_;                                                                 # Term to test
  return 't' if ref $s;                                                         # Term on top of stack
  substr($s, 0, 1);                                                             # Something other than a term defines its type by its first letter
 }

sub expandElement($)                                                            #P Describe a lexical element
 {my ($e) = @_;                                                                 # Element to expand
  my $x = $LexicalCodes->{type $e}->name;                                       # Expansion
  "'$x': $e"
 }

sub expandCodes($)                                                              #P Expand a string of codes
 {my ($e) = @_;                                                                 # Codes to expand
  my @c = map {qq('$_')} sort map {$LexicalCodes->{$_}->name} split //, $e;     # Codes  for next possible items
  my $c = pop @c;
  my $t = join ', ', @c;
  "$t or $c"
 }

sub expected($)                                                                 #P String of next possible lexical items
 {my ($s) = @_;                                                                 # Lexical item
  my $e = expandCodes $LexicalCodes->{type $s}->next;                           # Codes for next possible items
  "Expected: $e"
 }

sub unexpected($$$)                                                             #P Complain about an unexpected element
 {my ($element, $unexpected, $position) = @_;                                   # Last good element, unexpected element, position
  my $j = $position + 1;
  my $E = expandElement $unexpected;
  my $X = expected $element;

  my sub de($)                                                                  # Extract an error message and die
   {my ($message) = @_;                                                         # Message
    $message =~ s(\n) ( )gs;
    die "$message\n";
   }

  de <<END if ref $element;
Unexpected $E following term ending at position $j.
$X.
END
  my $S = expandElement $element;
  de <<END;
Unexpected $E following $S at position $j.
$X.
END
 }

sub syntaxError(@)                                                              # Check the syntax of an expression without parsing it. Die with a helpful message if an error occurs.  The helpful message will be slightly different from that produced by L<parse> as it cannot contain information from the non existent parse tree.
 {my (@expression) = @_;                                                        # Expression to parse
  my @e = @_;

  return '' unless @e;                                                          # An empty string is valid

  my sub test($$$)                                                              # Test a transition
   {my ($current, $following, $position) = @_;                                  # Current element, following element, position
    my $n = $LexicalCodes->{type $current}->next;                               # Elements expected next
    return if index($n, type $following) > -1;                                  # Transition allowed
    unexpected $current, $following, $position - 1;                             # Complain about the unexpected element
   }

  my sub testFirst                                                              # Test first transition
   {return if index($first, type $e[0]) > -1;                                   # Transition allowed
    my $E = expandElement $e[0];
    my $C = expandCodes $first;
    die <<END;
Expression must start with $C, not $E.
END
   }

  my sub testLast($$)                                                           # Test last transition
   {my ($current, $position) = @_;                                              # Current element, position
    return if index($last, type $current) > -1;                                 # Transition allowed
    my $C = expandElement $current;
    my $E = expected $current;
    die <<END;
$E after final $C.
END
   }

  if (1)                                                                        # Test parentheses
   {my @b;
    for my $i(keys @e)                                                          # Each element
     {my $e = $e[$i];
      if (type($e) eq 'b')                                                      # Open
       {push @b, [$i, $e];
       }
      elsif (type($e) eq 'B')                                                   # Close
       {if (@b > 0)
         {my ($h, $a) = pop(@b)->@*;
          my $j = $i + 1;
          my $g = $h + 1;
          die <<END if substr($a, 1) ne substr($e, 1);                          # Close fails to match
Parenthesis mismatch between $a at position $g and $e at position $j.
END
         }
        else                                                                    # No corresponding open
         {my $j = $i + 1;
          my $E = $i ? expected($e[$i-1]) : testFirst;                          # What we might have had instead
          die <<END;
Unexpected closing parenthesis $e at position $j. $E.
END
         }
       }
     }
    if (@b > 0)                                                                 # Closing parentheses at end
     {my ($h, $a) = pop(@b)->@*;
      my $g = $h + 1;
          die <<END;
No closing parenthesis matching $a at position $g.
END
     }
   }

  if (1)                                                                        # Test transitions
   {testFirst $e[0];                                                            # First transition
    test      $e[$_-1], $e[$_], $_+1 for 1..$#e;                                # Each element beyond the first
    testLast  $e[-1], scalar @e;                                                # Final transition
   }
 }

for my $t(qw(abdps ads b B bdp bdps bpsv bst p pbsv s sb sbt v))                # Test various sets of items
 {my $c = <<'END';
sub test_XXXX($)                                                                #P Check that we have XXXX
 {my ($item) = @_;                                                              # Item to test
  !ref($item) and index('XXXX',  substr($item, 0, 1)) > -1
 }
END
       $c =~ s(XXXX) ($t)gs;
  eval $c; $@ and confess "$@\n";
 }

sub test_t($)                                                                   #P Check that we have a semi-colon
 {my ($item) = @_;                                                              # Item to test
  ref $item
 }

sub reduce($)                                                                   #P Convert the longest possible expression on top of the stack into a term
 {my ($s) = @_;                                                                 # Stack
  #lll "TTTT ", scalar(@s), "\n", dump([@s]);

  if (@$s >= 3)                                                                 # Go for term infix-operator term
   {my ($l, $d, $r) = ($$s[-3], $$s[-2], $$s[-1]);                              # Left dyad right
    if (test_t($l) and test_t($r) and test_ads($d))                             # Parse out infix operator expression
     {pop  @$s for 1..3;
      push @$s, new $d, $l, $r;
      return 1;
     }
    if (test_b($l) and test_B($r) and test_t($d))                               # Parse parenthesized term
     {pop  @$s for 1..3;
      push @$s, $d;
      return 1;
     }
   }

  if (@$s >= 2)                                                                 # Convert an empty pair of parentheses to an empty term
   {my ($l, $r) = ($$s[-2], $$s[-1]);
    if (test_b($l) and test_B($r))                                              # Empty pair of parentheses
     {pop  @$s for 1..2;
      push @$s, new 'empty1';
      return 1;
     }
    if (test_s($l) and test_B($r))                                              # Semi-colon, close implies remove unneeded semi
     {pop  @$s for 1..2;
      push @$s, $r;
      return 1;
     }
    if (test_p($l) and test_t($r))                                              # Prefix, term
     {pop  @$s for 1..2;
      push @$s, new $l, $r;
      return 1;
     }
   }

  undef                                                                         # No move made
 }

for my $t(qw(t bdp bdps bst abdps))
 {my $c = <<'END';
sub check_XXXX($$$)                                                             #P Check that the top of the stack has one of bdps
 {my ($s, $i, $e) = @_;                                                         # Stack, index of current element, current element
  return 1 if index("XXXX", type($$s[-1])) > -1;                                # Check type allowed
  unexpected $$s[-1], $e, $i;                                                   # Complain about an unexpected type
 }
END
       $c =~ s(XXXX) ($t)gs;
  eval $c; $@ and confess "$@\n";
 }

sub parse(@)                                                                    # Parse an expression.
 {my (@expression) = @_;                                                        # Expression to parse

  my $s = [];                                                                   # Stack

  for my $i(keys @expression)                                                   # Each input element
   {my $e = $expression[$i];

    if (!@$s)                                                                   # Empty stack
     {my $E = expandElement $e;
      die <<END =~ s(\n) ( )gsr =~ s(\s+\Z) (\n)gsr if !test_bpsv($e);
Expression must start with 'opening parenthesis', 'prefix
operator', 'semi-colon' or 'variable', not $E.
END
      if    (test_v($e))                                                        # Single variable
       {@$s = (new $e);
       }
      elsif (test_s($e))                                                        # Semi
       {@$s = (new('empty4'), $e);
       }
      else                                                                      # Not semi or variable
       {@$s = ($e);
       }
      next;
     }
                                                                                # Action on each lexical item
     {a => sub                                                                  # Assign
       {check_t($s, $i, $e);
        push @$s, $e;
       },

      b => sub                                                                  # Open
       {check_bdps($s, $i, $e);
        push @$s, $e;
       },

      B => sub                                                                  # Closing parenthesis
       {check_bst($s, $i, $e);
        1 while reduce $s;
        push @$s, $e;
        1 while reduce $s;
        check_bst($s, $i, $e);
       },

      d => sub                                                                  # Infix but not assign or semi-colon
       {check_t($s, $i, $e);
        push @$s, $e;
       },

      p => sub                                                                  # Prefix
       {check_bdp($s, $i, $e);
        push @$s, $e;
       },

      q => sub                                                                  # Post fix
       {check_t($s, $i, $e);
        if (ref $$s[-1])                                                        # Post fix operator applied to a term
         {my $p = pop @$s;
          push @$s, new $e, $p;
         }
       },

      s => sub                                                                  # Semi colon
       {check_bst($s, $i, $e);
        push @$s, new 'empty5' if test_sb($$s[-1]);                             # Insert an empty element between two consecutive semicolons
        1 while reduce $s;
        push @$s, $e;
       },

      v => sub                                                                  # Variable
       {check_abdps($s, $i, $e);
        push @$s, new $e;

        while(@$s >= 2 and 'p' eq type($$s[-2]))                                # Check for preceding prefix operators
         {my ($l, $r) = splice @$s, -2;
          push @$s, new $l, $r;
         }
       },
     }->{substr($e, 0, 1)}->();                                                 # Dispatch the action associated with the lexical item
   }

  pop @$s while @$s > 1 and $$s[-1] =~ m(s);                                    # Remove any trailing semi colons
  1 while reduce $s;                                                            # Final reductions

  if (@$s != 1)                                                                 # Incomplete expression
   {my $E = expected $expression[-1];
    die "Incomplete expression. $E.\n";
   }

  if (index($last,   type $expression[-1]) == -1)                               # Incomplete expression
   {my $C = expandElement $expression[-1];
    my $E = expected      $expression[-1];
    die <<END;
$E after final $C.
END
   }

  $$s[0]                                                                        # The resulting parse tree
 } # parse

#D1 Print                                                                       # Print a parse tree to make it easy to visualize its structure.

sub depth($)                                                                    #P Depth of a term in an expression.
 {my ($term) = @_;                                                              # Term
  my $d = 0;
  for(my $t = $term; $t; $t = $t->up) {++$d}
  $d
 }

sub listTerms($)                                                                #P List the terms in an expression in post order
 {my ($expression) = @_;                                                        # Root term
  my @t;                                                                        # Terms

  sub                                                                           # Recurse through terms
   {my ($e) = @_;                                                               # Term
    my $o = $e->operands;
    return unless $e;                                                           # Operator
    if (my @o = $o ? grep {ref $_} @$o : ())                                    # Operands
     {my ($p, @p) = @o;
      __SUB__->($p);                                                            # First operand
      push @t, $e;                                                              # Operator
      __SUB__->($_) for @p;                                                     # Second and subsequent operands
     }
    else                                                                        # No operands
     {push @t, $e;                                                              # Operator
     }
   } ->($expression);

  @t
 }

sub flat($@)                                                                    # Print the terms in the expression as a tree from left right to make it easier to visualize the structure of the tree.
 {my ($expression, @title) = @_;                                                # Root term, optional title
  my @t = $expression->listTerms;                                               # Terms in expression in post order
  my @s;                                                                        # Print

  my sub align                                                                  # Align the ends of the lines
   {my $L = 0;                                                                  # Longest line
    for my $s(@s)
     {my $l = length $s; $L = $l if $l > $L;
     }

    for my $i(keys @s)                                                          # Pad to longest
     {my $s = $s[$i] =~ s/\s+\Z//rs;
      my $l = length($s);
      if ($l < $L)
       {my $p = ' ' x ($L - $l);
        $s[$i] = $s . $p;
       }
     }
   };

  for my $t(@t)                                                                 # Initialize output rectangle
   {$s[$_] //= '' for 0..$t->depth;
   }

  for my $t(@t)                                                                 # Traverse tree
   {my $d = $t->depth;
    my $p = $t->operator;                                                       # Operator

    align if $p =~ m(\A(a|d|s));                                                # Shift over for some components

    $s[$d] .= " $p";                                                            # Describe operator or operand
    align unless $p =~ m(\A(p|q|v));                                            # Vertical for some components
   }

  shift @s while @s and $s[ 0] =~ m(\A\s*\Z)s;                                  # Remove leading blank lines

  for my $i(keys @s)                                                            # Clean up trailing blanks so that tests are not affected by spurious white space mismatches
   {$s[$i] =~ s/\s+\n/\n/gs;
    $s[$i] =~ s/\s+\Z//gs;
   }

  unshift @s, join(' ', @title) if @title;                                      # Add title

  join "\n", @s, '';
 }

#D
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

Tree::Term - Create a parse tree from an array of terms representing an expression.

=head1 Synopsis

The expression to L<parse> is presented as an array of words, the first letter
of each word indicates its lexical role as in:

  my @e = qw(b b p2 p1 v1 q1 q2 B d3 b p4 p3 v2 q3 q4 d4 p6 p5 v3 q5 q6 B s B s);

Where:

  a assign     - infix operator with priority 2 binding right to left
  b open       - open parenthesis
  B close      - close parenthesis
  d dyad       - infix operator with priority 3 binding left to right
  p prefix     - monadic prefix operator
  q suffix     - monadic suffix operator
  s semi-colon - infix operator with priority 1 binding left to right
  v variable   - a variable in the expression

The results of parsing the expression can be printed with L<flat> which
provides a left to right representation of the parse tree.

  is_deeply parse(@e)->flat, <<END;

      d3
   q2       d4
   q1    q4    q6
   p2    q3    q5
   p1    p4    p6
   v1    p3    p5
         v2    v3
END

=head1 Description

Create a parse tree from an array of terms representing an expression.


Version 20210714.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Parse

Create a parse tree from an array of terms representing an expression.

=head2 LexicalStructure()

Return the lexical codes and their relationships in a data structure so this information can be used in other contexts.


B<Example:>


  
  is_deeply LexicalStructure,                                                       # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  

=head2 syntaxError(@expression)

Check the syntax of an expression without parsing it. Die with a helpful message if an error occurs.  The helpful message will be slightly different from that produced by L<parse|https://en.wikipedia.org/wiki/Parsing> as it cannot contain information from the non existent parse tree.

     Parameter    Description
  1  @expression  Expression to parse

B<Example:>


  if (1)                                                                          
  
   {eval {syntaxError(qw(v1 p1))};  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ok -1 < index $@, <<END =~ s({a}) ( )gsr;
  Unexpected 'prefix operator': p1 following 'variable': v1 at position 2.
  Expected: 'assignment operator', 'closing parenthesis',
  'dyadic operator', 'semi-colon' or 'suffix operator'.
  END
   }
  

=head2 parse(@expression)

Parse an expression.

     Parameter    Description
  1  @expression  Expression to parse

B<Example:>


  
   my @e = qw(b b p2 p1 v1 q1 q2 B d3 b p4 p3 v2 q3 q4 d4 p6 p5 v3 q5 q6 B s B s);
  
  
   is_deeply parse(@e)->flat, <<END;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      d3
   q2       d4
   q1    q4    q6
   p2    q3    q5
   p1    p4    p6
   v1    p3    p5
         v2    v3
  END
  
  }
  
  ok T [qw(b b v1 B s B s)], <<END;
   v1
  END
  
  ok T [qw(v1 q1 s)], <<END;
   q1
   v1
  END
  
  ok T [qw(b b v1 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
   q6
   q5
   q4
   q3
   q2
   q1
   v1
  END
  
  ok T [qw(p1 p2 b v1 B)], <<END;
   p1
   p2
   v1
  END
  
  ok T [qw(v1 d1 p1 p2 v2)], <<END;
      d1
   v1    p1
         p2
         v2
  END
  
  ok T [qw(p1 p2 b p3 p4 b p5 p6 v1 d1 v2 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
         q6
         q5
         p1
         p2
         q4
         q3
         p3
         p4
      d1
   p5    q2
   p6    q1
   v1    v2
  END
  
  ok T [qw(p1 p2 b p3 p4 b p5 p6 v1 a1 v2 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
         q6
         q5
         p1
         p2
         q4
         q3
         p3
         p4
      a1
   p5    q2
   p6    q1
   v1    v2
  END
  
  ok T [qw(b v1 B d1 b v2 B)], <<END;
      d1
   v1    v2
  END
  
  ok T [qw(b v1 B q1 q2 d1 b v2 B)], <<END;
      d1
   q2    v2
   q1
   v1
  END
  
  ok E <<END;
  a
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'assignment operator': a.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'assignment operator': a.
  END
  
  ok E <<END;
  B
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'closing parenthesis': B.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'closing parenthesis': B.
  END
  
  ok E <<END;
  d1
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'dyadic operator': d1.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'dyadic operator': d1.
  END
  
  ok E <<END;
  p1
  Expected: 'opening parenthesis', 'prefix operator' or 'variable' after final 'prefix operator': p1.
  Expected: 'opening parenthesis', 'prefix operator' or 'variable' after final 'prefix operator': p1.
  END
  
  ok E <<END;
  q1
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'suffix operator': q1.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'suffix operator': q1.
  END
  
  ok E <<END;
  s
  
  
  END
  
  ok E <<END;
  v1
  
  
  END
  
  ok E <<END;
  b v1
  Incomplete expression. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  No closing parenthesis matching b at position 1.
  END
  
  ok E <<END;
  b v1 B B
  Unexpected 'closing parenthesis': B following 'closing parenthesis': B at position 4. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  Unexpected closing parenthesis B at position 4. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  END
  
  ok E <<END;
  v1 d1 d2 v2
  Unexpected 'dyadic operator': d2 following 'dyadic operator': d1 at position 3. Expected: 'assignment operator', 'opening parenthesis', 'prefix operator' or 'variable'.
  Unexpected 'dyadic operator': d2 following 'dyadic operator': d1 at position 3. Expected: 'assignment operator', 'opening parenthesis', 'prefix operator' or 'variable'.
  END
  
  ok E <<END;
  v1 p1
  Unexpected 'prefix operator': p1 following term ending at position 2. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  Unexpected 'prefix operator': p1 following 'variable': v1 at position 2. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  END
  
  if (1)                                                                          
   {eval {syntaxError(qw(v1 p1))};
    ok -1 < index $@, <<END =~ s({a}) ( )gsr;
  Unexpected 'prefix operator': p1 following 'variable': v1 at position 2.
  Expected: 'assignment operator', 'closing parenthesis',
  'dyadic operator', 'semi-colon' or 'suffix operator'.
  END
  

=head1 Print

Print a parse tree to make it easy to visualize its structure.

=head2 flat($expression, @title)

Print the terms in the expression as a tree from left right to make it easier to visualize the structure of the tree.

     Parameter    Description
  1  $expression  Root term
  2  @title       Optional title

B<Example:>


  
   my @e = qw(v1 a2 v3 d4 v5 s6 v8 a9 v10);
  
  
   is_deeply parse(@e)->flat, <<END;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

                  s6
      a2                a9
   v1       d4       v8    v10
         v3    v5
  END
  }
  
  ok T [qw(v1 a2 v3 s s s  v4 a5 v6 s s)], <<END;
                                         s
                              s            empty5
                     s             a5
            s          empty5   v4    v6
      a2      empty5
   v1    v3
  END
  
  ok T [qw(b B)], <<END;
   empty1
  END
  
  ok T [qw(b b B B)], <<END;
   empty1
  END
  
  ok T [qw(b b v1 B B)], <<END;
   v1
  END
  
  ok T [qw(b b v1 a2 v3 B B)], <<END;
      a2
   v1    v3
  END
  
  ok T [qw(b b v1 a2 v3 d4 v5 B B)], <<END;
      a2
   v1       d4
         v3    v5
  END
  
  ok T [qw(p1 v1)], <<END;
   p1
   v1
  END
  
  ok T [qw(p2 p1 v1)], <<END;
   p2
   p1
   v1
  END
  
  ok T [qw(v1 q1)], <<END;
   q1
   v1
  END
  
  ok T [qw(v1 q1 q2)], <<END;
   q2
   q1
   v1
  END
  
  ok T [qw(p2 p1 v1 q1 q2)], <<END;
   q2
   q1
   p2
   p1
   v1
  END
  
  ok T [qw(p2 p1 v1 q1 q2 d3 p4 p3 v2 q3 q4)], <<END;
      d3
   q2    q4
   q1    q3
   p2    p4
   p1    p3
   v1    v2
  END
  
  ok T [qw(p2 p1 v1 q1 q2 d3 p4 p3 v2 q3 q4  d4 p6 p5 v3 q5 q6 s)], <<END;
      d3
   q2       d4
   q1    q4    q6
   p2    q3    q5
   p1    p4    p6
   v1    p3    p5
         v2    v3
  END
  
  ok T [qw(b s B)], <<END;
   empty5
  END
  
  ok T [qw(b s s B)], <<END;
          s
   empty5   empty5
  END
  
  
  if (1) {                                                                        
  
   my @e = qw(b b p2 p1 v1 q1 q2 B d3 b p4 p3 v2 q3 q4 d4 p6 p5 v3 q5 q6 B s B s);
  
  
   is_deeply parse(@e)->flat, <<END;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      d3
   q2       d4
   q1    q4    q6
   p2    q3    q5
   p1    p4    p6
   v1    p3    p5
         v2    v3
  END
  
  }
  
  ok T [qw(b b v1 B s B s)], <<END;
   v1
  END
  
  ok T [qw(v1 q1 s)], <<END;
   q1
   v1
  END
  
  ok T [qw(b b v1 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
   q6
   q5
   q4
   q3
   q2
   q1
   v1
  END
  
  ok T [qw(p1 p2 b v1 B)], <<END;
   p1
   p2
   v1
  END
  
  ok T [qw(v1 d1 p1 p2 v2)], <<END;
      d1
   v1    p1
         p2
         v2
  END
  
  ok T [qw(p1 p2 b p3 p4 b p5 p6 v1 d1 v2 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
         q6
         q5
         p1
         p2
         q4
         q3
         p3
         p4
      d1
   p5    q2
   p6    q1
   v1    v2
  END
  
  ok T [qw(p1 p2 b p3 p4 b p5 p6 v1 a1 v2 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
         q6
         q5
         p1
         p2
         q4
         q3
         p3
         p4
      a1
   p5    q2
   p6    q1
   v1    v2
  END
  
  ok T [qw(b v1 B d1 b v2 B)], <<END;
      d1
   v1    v2
  END
  
  ok T [qw(b v1 B q1 q2 d1 b v2 B)], <<END;
      d1
   q2    v2
   q1
   v1
  END
  
  ok E <<END;
  a
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'assignment operator': a.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'assignment operator': a.
  END
  
  ok E <<END;
  B
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'closing parenthesis': B.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'closing parenthesis': B.
  END
  
  ok E <<END;
  d1
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'dyadic operator': d1.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'dyadic operator': d1.
  END
  
  ok E <<END;
  p1
  Expected: 'opening parenthesis', 'prefix operator' or 'variable' after final 'prefix operator': p1.
  Expected: 'opening parenthesis', 'prefix operator' or 'variable' after final 'prefix operator': p1.
  END
  
  ok E <<END;
  q1
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'suffix operator': q1.
  Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'suffix operator': q1.
  END
  
  ok E <<END;
  s
  
  
  END
  
  ok E <<END;
  v1
  
  
  END
  
  ok E <<END;
  b v1
  Incomplete expression. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  No closing parenthesis matching b at position 1.
  END
  
  ok E <<END;
  b v1 B B
  Unexpected 'closing parenthesis': B following 'closing parenthesis': B at position 4. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  Unexpected closing parenthesis B at position 4. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  END
  
  ok E <<END;
  v1 d1 d2 v2
  Unexpected 'dyadic operator': d2 following 'dyadic operator': d1 at position 3. Expected: 'assignment operator', 'opening parenthesis', 'prefix operator' or 'variable'.
  Unexpected 'dyadic operator': d2 following 'dyadic operator': d1 at position 3. Expected: 'assignment operator', 'opening parenthesis', 'prefix operator' or 'variable'.
  END
  
  ok E <<END;
  v1 p1
  Unexpected 'prefix operator': p1 following term ending at position 2. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  Unexpected 'prefix operator': p1 following 'variable': v1 at position 2. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
  END
  
  if (1)                                                                          
   {eval {syntaxError(qw(v1 p1))};
    ok -1 < index $@, <<END =~ s({a}) ( )gsr;
  Unexpected 'prefix operator': p1 following 'variable': v1 at position 2.
  Expected: 'assignment operator', 'closing parenthesis',
  'dyadic operator', 'semi-colon' or 'suffix operator'.
  END
  


=head1 Hash Definitions




=head2 Tree::Term Definition


Description of a term in the expression.




=head3 Output fields


=head4 operands

Operands to which the operator will be applied.

=head4 operator

Operator to be applied to one or more operands.

=head4 up

Parent term if this is a sub term.



=head2 Tree::Term::Codes Definition


Lexical item codes.




=head3 Output fields


=head4 B

Closing parenthesis.

=head4 a

Infix operator with priority 2 binding right to left typically used in an assignment.

=head4 b

Opening parenthesis.

=head4 d

Infix operator with priority 3 binding left to right typically used in arithmetic.

=head4 p

Monadic prefix operator.

=head4 q

Monadic suffix operator.

=head4 s

Infix operator with priority 1 binding left to right typically used to separate statements.

=head4 t

A term in the expression.

=head4 v

A variable in the expression.



=head2 Tree::Term::LexicalCode Definition


Lexical item codes.




=head3 Output fields


=head4 letter

Letter code used to refer to the lexical item.

=head4 name

Descriptive name of lexical item.

=head4 next

Letters codes of items that can follow this lexical item.



=head2 Tree::Term::LexicalStructure Definition


Lexical item codes.




=head3 Output fields


=head4 codes

Code describing each lexical item

=head4 first

Lexical items we can start with

=head4 last

Lexical items we can end with



=head1 Private Methods

=head2 new($operator, @operands)

New term.

     Parameter  Description
  1  $operator  Operator
  2  @operands  Operands.

=head2 LexicalCode($letter, $next, $name)

Lexical code definition

     Parameter  Description
  1  $letter    Letter used to refer to the lexical item
  2  $next      Letters of items that can follow this lexical item
  3  $name      Descriptive name of lexical item

=head2 type($s)

Type of term

     Parameter  Description
  1  $s         Term to test

=head2 expandElement($e)

Describe a lexical element

     Parameter  Description
  1  $e         Element to expand

=head2 expandCodes($e)

Expand a string of codes

     Parameter  Description
  1  $e         Codes to expand

=head2 expected($s)

String of next possible lexical items

     Parameter  Description
  1  $s         Lexical item

=head2 unexpected($element, $unexpected, $position)

Complain about an unexpected element

     Parameter    Description
  1  $element     Last good element
  2  $unexpected  Unexpected element
  3  $position    Position

=head2 test_XXXX($item)

Check that we have XXXX

     Parameter  Description
  1  $item      Item to test

=head2 test_t($item)

Check that we have a semi-colon

     Parameter  Description
  1  $item      Item to test

=head2 reduce($s)

Convert the longest possible expression on top of the stack into a term

     Parameter  Description
  1  $s         Stack

=head2 check_XXXX($s, $i, $e)

Check that the top of the stack has one of bdps

     Parameter  Description
  1  $s         Stack
  2  $i         Index of current element
  3  $e         Current element

=head2 depth($term)

Depth of a term in an expression.

     Parameter  Description
  1  $term      Term

=head2 listTerms($expression)

List the terms in an expression in post order

     Parameter    Description
  1  $expression  Root term


=head1 Index


1 L<check_XXXX|/check_XXXX> - Check that the top of the stack has one of bdps

2 L<depth|/depth> - Depth of a term in an expression.

3 L<expandCodes|/expandCodes> - Expand a string of codes

4 L<expandElement|/expandElement> - Describe a lexical element

5 L<expected|/expected> - String of next possible lexical items

6 L<flat|/flat> - Print the terms in the expression as a tree from left right to make it easier to visualize the structure of the tree.

7 L<LexicalCode|/LexicalCode> - Lexical code definition

8 L<LexicalStructure|/LexicalStructure> - Return the lexical codes and their relationships in a data structure so this information can be used in other contexts.

9 L<listTerms|/listTerms> - List the terms in an expression in post order

10 L<new|/new> - New term.

11 L<parse|/parse> - Parse an expression.

12 L<reduce|/reduce> - Convert the longest possible expression on top of the stack into a term

13 L<syntaxError|/syntaxError> - Check the syntax of an expression without parsing it.

14 L<test_t|/test_t> - Check that we have a semi-colon

15 L<test_XXXX|/test_XXXX> - Check that we have XXXX

16 L<type|/type> - Type of term

17 L<unexpected|/unexpected> - Complain about an unexpected element

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Tree::Term

=head1 Author

L<philiprbrenan@gmail.com|mailto:philiprbrenan@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

=head1 Copyright

Copyright (c) 2016-2021 Philip R Brenan.

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
#__DATA__
use Time::HiRes qw(time);
use Test::More;

my $develop   = -e q(/home/phil/);                                              # Developing
my $log       = q(/home/phil/perl/cpan/TreeTerm/lib/Tree/zzz.txt);              # Log file
my $localTest = ((caller(1))[0]//'Tree::Term') eq "Tree::Term";                 # Local testing mode

Test::More->builder->output("/dev/null") if $localTest;                         # Reduce number of confirmation messages during testing

if ($^O =~ m(bsd|linux)i)                                                       # Supported systems
 {plan tests => 50
 }
else
 {plan skip_all =>qq(Not supported on: $^O);
 }

sub T                                                                           #P Test a parse
 {my ($expression, $expected) = @_;                                             # Expression, expected result
  syntaxError @$expression;                                                     # Syntax check without creating parse tree
  my $g = parse(@$expression)->flat;
  my $r = $g eq $expected;
  owf($log, $g) if -e $log;                                                     # Save result if testing
  confess "Failed test" unless $r;
  $r
 }

sub E($)                                                                        #P Test a parse error
 {my ($text) = @_;
  my ($test, $parse, $syntax) = split /\n/,  $text;                             # Parse test description

  my @e = split /\s+/, $test;
  my $e = 0;
  eval {parse       @e}; ++$e unless index($@, $parse)  > -1; my $a = $@ // '';
  eval {syntaxError @e}; ++$e unless index($@, $syntax) > -1; my $b = $@ // '';
  if ($e)
   {owf($log, "$a$b") if -e $log;                                               # Save result if testing
    confess;
   }
  !$e
 }

my $startTime = time;

eval {goto latest};

ok T [qw(v1)], <<END;
 v1
END

ok T [qw(s)], <<END;
 empty4
END

ok T [qw(s s)], <<END;
        s
 empty4   empty5
END

ok T [qw(v1 d2 v3)], <<END;
    d2
 v1    v3
END

ok T [qw(v1 a2 v3)], <<END;
    a2
 v1    v3
END

ok T [qw(v1 a2 v3 d4 v5)], <<END;
    a2
 v1       d4
       v3    v5
END

if (1) {                                                                        #Tflat

 my @e = qw(v1 a2 v3 d4 v5 s6 v8 a9 v10);

 is_deeply parse(@e)->flat, <<END;
                s6
    a2                a9
 v1       d4       v8    v10
       v3    v5
END
}

ok T [qw(v1 a2 v3 s s s  v4 a5 v6 s s)], <<END;
                                       s
                            s            empty5
                   s             a5
          s          empty5   v4    v6
    a2      empty5
 v1    v3
END

ok T [qw(b B)], <<END;
 empty1
END

ok T [qw(b b B B)], <<END;
 empty1
END

ok T [qw(b b v1 B B)], <<END;
 v1
END

ok T [qw(b b v1 a2 v3 B B)], <<END;
    a2
 v1    v3
END

ok T [qw(b b v1 a2 v3 d4 v5 B B)], <<END;
    a2
 v1       d4
       v3    v5
END

ok T [qw(p1 v1)], <<END;
 p1
 v1
END

ok T [qw(p2 p1 v1)], <<END;
 p2
 p1
 v1
END

ok T [qw(v1 q1)], <<END;
 q1
 v1
END

ok T [qw(v1 q1 q2)], <<END;
 q2
 q1
 v1
END

ok T [qw(p2 p1 v1 q1 q2)], <<END;
 q2
 q1
 p2
 p1
 v1
END

ok T [qw(p2 p1 v1 q1 q2 d3 p4 p3 v2 q3 q4)], <<END;
    d3
 q2    q4
 q1    q3
 p2    p4
 p1    p3
 v1    v2
END

ok T [qw(p2 p1 v1 q1 q2 d3 p4 p3 v2 q3 q4  d4 p6 p5 v3 q5 q6 s)], <<END;
    d3
 q2       d4
 q1    q4    q6
 p2    q3    q5
 p1    p4    p6
 v1    p3    p5
       v2    v3
END

ok T [qw(b s B)], <<END;
 empty5
END

ok T [qw(b s s B)], <<END;
        s
 empty5   empty5
END


if (1) {                                                                        #Tparse

 my @e = qw(b b p2 p1 v1 q1 q2 B d3 b p4 p3 v2 q3 q4 d4 p6 p5 v3 q5 q6 B s B s);

 is_deeply parse(@e)->flat, <<END;
    d3
 q2       d4
 q1    q4    q6
 p2    q3    q5
 p1    p4    p6
 v1    p3    p5
       v2    v3
END

}

ok T [qw(b b v1 B s B s)], <<END;
 v1
END

ok T [qw(v1 q1 s)], <<END;
 q1
 v1
END

ok T [qw(b b v1 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
 q6
 q5
 q4
 q3
 q2
 q1
 v1
END

ok T [qw(p1 p2 b v1 B)], <<END;
 p1
 p2
 v1
END

ok T [qw(v1 d1 p1 p2 v2)], <<END;
    d1
 v1    p1
       p2
       v2
END

ok T [qw(p1 p2 b p3 p4 b p5 p6 v1 d1 v2 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
       q6
       q5
       p1
       p2
       q4
       q3
       p3
       p4
    d1
 p5    q2
 p6    q1
 v1    v2
END

ok T [qw(p1 p2 b p3 p4 b p5 p6 v1 a1 v2 q1 q2 B q3 q4 s B q5 q6  s)], <<END;
       q6
       q5
       p1
       p2
       q4
       q3
       p3
       p4
    a1
 p5    q2
 p6    q1
 v1    v2
END

ok T [qw(b v1 B d1 b v2 B)], <<END;
    d1
 v1    v2
END

ok T [qw(b v1 B q1 q2 d1 b v2 B)], <<END;
    d1
 q2    v2
 q1
 v1
END

ok E <<END;
a
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'assignment operator': a.
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'assignment operator': a.
END

ok E <<END;
B
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'closing parenthesis': B.
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'closing parenthesis': B.
END

ok E <<END;
d1
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'dyadic operator': d1.
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'dyadic operator': d1.
END

ok E <<END;
p1
Expected: 'opening parenthesis', 'prefix operator' or 'variable' after final 'prefix operator': p1.
Expected: 'opening parenthesis', 'prefix operator' or 'variable' after final 'prefix operator': p1.
END

ok E <<END;
q1
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'suffix operator': q1.
Expression must start with 'opening parenthesis', 'prefix operator', 'semi-colon' or 'variable', not 'suffix operator': q1.
END

ok E <<END;
s


END

ok E <<END;
v1


END

ok E <<END;
b v1
Incomplete expression. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
No closing parenthesis matching b at position 1.
END

ok E <<END;
b v1 B B
Unexpected 'closing parenthesis': B following 'closing parenthesis': B at position 4. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
Unexpected closing parenthesis B at position 4. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
END

ok E <<END;
v1 d1 d2 v2
Unexpected 'dyadic operator': d2 following 'dyadic operator': d1 at position 3. Expected: 'assignment operator', 'opening parenthesis', 'prefix operator' or 'variable'.
Unexpected 'dyadic operator': d2 following 'dyadic operator': d1 at position 3. Expected: 'assignment operator', 'opening parenthesis', 'prefix operator' or 'variable'.
END

ok E <<END;
v1 p1
Unexpected 'prefix operator': p1 following term ending at position 2. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
Unexpected 'prefix operator': p1 following 'variable': v1 at position 2. Expected: 'assignment operator', 'closing parenthesis', 'dyadic operator', 'semi-colon' or 'suffix operator'.
END

if (1)                                                                          #TsyntaxError
 {eval {syntaxError(qw(v1 p1))};
  ok -1 < index $@, <<END =~ s(\x{a}) ( )gsr;
Unexpected 'prefix operator': p1 following 'variable': v1 at position 2.
Expected: 'assignment operator', 'closing parenthesis',
'dyadic operator', 'semi-colon' or 'suffix operator'.
END
 }

ok T [qw(v1 s)], <<END;
 v1
END

ok T [qw(v1 s s)], <<END;
    s
 v1   empty5
END

ok T [qw(v1 s b s B)], <<END;
    s
 v1   empty5
END

ok T [qw(v1 s b b s s B B)], <<END;
    s
 v1          s
      empty5   empty5
END

ok T [qw(b v1 s B s s)], <<END;
    s
 v1   empty5
END

is_deeply LexicalStructure,                                                     #TLexicalStructure
bless({
  codes => bless({
             a => bless({ letter => "a", name => "assignment operator", next => "bpv" },   "Tree::Term::LexicalCode"),
             b => bless({ letter => "b", name => "opening parenthesis", next => "bBpsv" }, "Tree::Term::LexicalCode"),
             B => bless({ letter => "B", name => "closing parenthesis", next => "aBdqs" }, "Tree::Term::LexicalCode"),
             d => bless({ letter => "d", name => "dyadic operator",     next => "abpv" },  "Tree::Term::LexicalCode"),
             p => bless({ letter => "p", name => "prefix operator",     next => "bpv" },   "Tree::Term::LexicalCode"),
             q => bless({ letter => "q", name => "suffix operator",     next => "aBdqs" }, "Tree::Term::LexicalCode"),
             s => bless({ letter => "s", name => "semi-colon",          next => "bBpsv" }, "Tree::Term::LexicalCode"),
             t => bless({ letter => "t", name => "term",                next => "aBdqs" }, "Tree::Term::LexicalCode"),
             v => bless({ letter => "v", name => "variable",            next => "aBdqs" }, "Tree::Term::LexicalCode"),
           }, "Tree::Term::Codes"),
  first => "bpsv",
  last  => "Bqsv",
}, "Tree::Term::LexicalStructure");

lll "Finished in", sprintf("%7.4f", time - $startTime), "seconds";
