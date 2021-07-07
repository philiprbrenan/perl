#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Create a choice tree with a minimal number of fixed size choices for a list of
# items ordered by popularity,
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc, 2018
#-------------------------------------------------------------------------------
#podDocumentation

package Data::Choice::Tree;
our $VERSION = '20180204';
use v5.8.0;
use warnings FATAL => qw(all);
use Carp qw(confess);
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use utf8;
use strict;

sub Item(@)                                                                     ## Item definition
 {package Data::Choice::Tree::Item;
  my $item = bless {promoted=>0, @_};
  return $item;
 }

if (1)                                                                          # Item methods
 {package Data::Choice::Tree::Item;
  use Data::Table::Text qw(:all);
  genLValueHashMethods  (qw(genre));                                            # Hash of the genres for each item
  genLValueScalarMethods(qw(source));                                           # The source of the item
  genLValueScalarMethods(qw(name));                                             # The name of the item
  genLValueScalarMethods(qw(promoted));                                         # The item has been promoted to a higher slot
  genLValueScalarMethods(qw(popularity));                                       # The popularity of the item - the most popular items are used to fill any empty choice slots
 }

sub ChoiceSet(@)                                                                ## Set of choices to be offered in one display
 {package Data::Choice::Tree::ChoiceSet;
  bless {@_};
 }

if (1)                                                                          # Item methods
 {package Data::Choice::Tree::ChoiceSet;
  use Data::Table::Text qw(:all);
  genLValueHashMethods (qw(choices));                                           # The choices in the choice set
  genLValueArrayMethods(qw(items));                                             # The items already in the choice set
  genLValueArrayMethods(qw(promoted));                                          # The items promoted into the choice set
 }

sub countGenres(@)                                                              ## Count the number of instances of each genre present in a list of items
 {my (@items) = @_;                                                             # Items to check
  my $g;                                                                        # Genre counts
  for my $item(@items)                                                          # Each  item
   {$g->{$_}++ for keys %{$item->genre};                                        # Genres this item occurs in
   }
  $g                                                                            # Genre counts
 }

sub findMaximalGenre(@)                                                         ## Find a maximal genre present in a list if items
 {my (@items) = @_;                                                             # Items to check
  my $g = findGenreCounts(@items);                                              # Genre counts
  return (sort {$b->[1] <=> $a->[1]} map {[$_=> $$g{$_}]} keys %$g)[0][0]       # Return a most frequently occurring remaining genre if possible
    if keys %$g;
  undef                                                                         # No maximal genre
 }

sub Data::Choice::Tree::ChoiceSet::divide($@)                                   ## Divide items by the most common genres
 {my ($choiceSet, $N, @items) = @_;                                             # Empty choice set to fold division, number of elements in a choice set, items to divide into this choice set
  while(@items)                                                                 # Try to divide the supplied items
   {my $m = findMaximalGenre(@items);                                           # One of the most frequently occuring genres

    confess "More genres needed for these items:\n".                            # No genres left but items still to be divided
      formatTable([sort map {$_->name} @items], [qw(Name)])
      unless $m;

    my @m =  grep { $_->genre->{$m}} @items;                                    # Set of remaining items that are in the chosen genre
    @items = grep {!$_->genre->{$m}} @items;                                    # Set of remaining items that are not in the chosen genre

    delete $_->genre->{$m} for @m;                                              # Remove the chosen genre from items that are in the chosen genre

    if (my @c = sort keys %{$choiceSet->choices})                               # Check for  overflow
     {
       if (@c >= $N)                                                  # Complain that the choice set is not possible
       {my $s  = "Divide these items amongst these genres or add new genres:\n";
           $s .= formatTable([@c],                         [qw(Genres)]);
           $s .= formatTable([sort map {$_->name} @items], [qw(Items)]);
        confess $s
       }
     }

    if (@m > $N)                                                                # Further divide a choice that is still too big
     {$choiceSet->choices->{$m} = ChoiceSet()->divide($N, @m);
     }
    elsif (@m)                                                                  # Accept items without further division
     {$choiceSet->choices->{$m} = ChoiceSet(items=>[@m]);
     }
   }
  $choiceSet
 }

sub Data::Choice::Tree::ChoiceSet::itemsNotPromoted($)                          ## Apps found in and below the specified choice set that have not been promoted
 {my ($choiceSet) = @_;                                                         # ChoiceSet
  my $choices = $choiceSet->choices;                                            # Choices in this choice set
  my $items    = $choiceSet->items;                                             # Apps in this choice set
  my @items;                                                                    # Apps in and under this choice set
  for my $a(@{$choiceSet->items})                                               # Apps in this choice set
   {push @items, $a unless $a->promoted;                                        # Apps in this choice set that have not been promoted
   }
  for my $c(sort keys %$choices)                                                # Choices in this set
   {my $choice = $choices->{$c};                                                # Choice
    push @items, $choice->itemsNotPromoted;                                     # Apps in this choice that have not been promoted
   }
  sort {$b->popularity <=> $a->popularity} @items;                              # Sort by popularity
 }

sub Data::Choice::Tree::ChoiceSet::promote($$)                                  # Promote items to fill empty slots
 {my ($choiceSet, $N) = @_;                                                     # ChoiceSet, number of elements in a choice set, choice set tree, array of items
  my $choices = $choiceSet->choices;                                            # Choices in this choice set
  my @a;
  for my $c(sort keys %$choices)                                                # Find the items not promoted below the choices in this choice set
   {push @a, $choices->{$c}->itemsNotPromoted;
   }
  for my $a(@a)                                                                 # Fill slots in this choice set with items that have not been promoted
   {last if keys(%$choices) + scalar @{$choiceSet->items} +                     # Finished if the choice set is full
                              scalar @{$choiceSet->promoted} >= $N;
    push @{$choiceSet->promoted}, $a;                                           # Promote an item
    $a->promoted++;
   }
  for my $c(sort keys %$choices)                                                # Fill slots below this choice set
   {$choices->{$c}->promote($N);
   }
 }

sub Data::Choice::Tree::ChoiceSet::cutZeroes($)                                 ## Remove choice sets with no remaining elements
 {my ($choiceSet) = @_;                                                         # ChoiceSet
  my $choices = $choiceSet->choices;                                            # Choices in this choice set
  for my $c(sort keys %$choices)
   {my $choice = $choices->{$c};                                                # Each choice
    my @a = $choice->itemsNotPromoted;
    if (@a == 0 and !@{$choice->promoted})                                      # No items in or below this choice set
     {delete $choices->{$c};                                                    # Delete choice set
     }
    elsif (@a)                                                                  # Try contained choice sets
     {$choice->cutZeroes;
     }
   }
 }

sub Data::Choice::Tree::ChoiceSet::unwrapSingles($$)                            # Unwrap choice sets that contain just one item
 {my ($choiceSet, $N) = @_;                                                     # ChoiceSet, number of elements in a choice set
  my $choices = $choiceSet->choices;                                            # Choices in this choice set
  for my $c(sort keys %$choices)
   {my $choice = $choices->{$c};                                                # Each choice in this choice set
    my @a = ($choice->itemsNotPromoted, @{$choice->promoted});                  # Apps in this choice set
    if (@a == 1)                                                                # Just one item in this choice set
     {for my $a(@a)
       {push @{$choiceSet->promoted}, $a;                                       # Promote item
        $a->promoted++;
       }
      delete $choices->{$c};                                                    # Remove choice set as it is now empty
     }
    else                                                                        # Unwrap choice sets that contain a single item
     {$choices->{$c}->unwrapSingles($N);
     }
   }
 }

#1 Create and Print                                                             # Create a choice tree and print it

sub new($@)                                                                     # Create a choice tree by dividing the list of items by the most frequently occurring genres
 {my ($MaxChoice, @Items) = @_;                                                 # Maximum number of elements in a choice set, list of items
  my $N = $MaxChoice;

  my @items = map                                                               # Parse the items supplied
   {Item
     (name=>$_->[0], genre=>{map {$_=>1} @{$_->[1]}}, popularity=>$_->[2],
      source=>$_)}
    @Items;

  my $tree = ChoiceSet()->divide($N, @items);                                   # Divide the items into choice sets
  $tree->promote($N);                                                           # Promote most popular items into empty slots woith the most popular going to the highest position
  $tree->cutZeroes;                                                             # Remove empty choices
  $tree->unwrapSingles($N);                                                     # Unwrap choices with just one item in them

  $tree
 }

sub Data::Choice::Tree::ChoiceSet::dump($;$)                                    # Dump the choice set
 {my ($choiceSet, $depth) = @_;                                                 # ChoiceSet, depth on tree
  $depth //= 0;                                                                 # Depth in tree
  my $choices = $choiceSet->choices;                                            # Choices in this choice set
  my @out;                                                                      # Output
  for my $c(sort keys %$choices)
   {my $choice = $choices->{$c};                                                # Each choice in the choice set
    my @a = $choice->itemsNotPromoted;                                          # Apps not promoted
    push @out, "  " x $depth, "choice:   ", $c, " [", scalar(@a), "] ",         # Choice details
               join(" ", sort map {$_->name} @a), "\n";
    push @out, $choice->dump($depth+1);                                         # Choices in contained choice sets
   }
  for my $a(@{$choiceSet->items})                                               # Items in this choice set
   {push @out, "  " x $depth, "item:      ", $a->name, " ", $a->promoted,
               " ", $a->popularity, "\n";
   }
  for my $a(@{$choiceSet->promoted})                                            # Items that have been promoted to this choice set
   {push @out, "  " x $depth, "promoted: ", $a->name, " ", $a->promoted,
               " ", $a->popularity, "\n";
   }
  @out
 }

sub Data::Choice::Tree::ChoiceSet::print($;$)                                   # Print the choice set as the user will see it
 {my ($choiceSet, $depth) = @_;                                                 # ChoiceSet, depth on tree
  $depth //= 0;                                                                 # Depth in tree
  my $choices = $choiceSet->choices;                                            # Choices in this choice set
  my @out;                                                                      # Output
  for my $c(sort keys %$choices)
   {my $choice = $choices->{$c};                                                # Each choice in the choice set
    push @out, "  " x $depth, "choice: ", $c, "\n";                             # Choice
    push @out, $choice->print($depth+1);                                        # Choices in contained choice sets
   }
  for my $a(@{$choiceSet->items}, @{$choiceSet->promoted})                      # Items in choice set
   {push @out, "  " x $depth, "item:    ", $a->name, "\n";
   }
  @out
 }

sub genreOverlap($$@)                                                           # Fractional overlap of two genres
 {my ($g, $G, @items) = @_;                                                     # First genre, Second genre, items to consider
  my ($o, $m, $n, $N) = (0,0,0,0);
  for(@items)
   {my $c = $_->genre->{$g};
    my $C = $_->genre->{$G};
    if    ($c and $C) {++$o}
    elsif ($c)        {++$n}
    elsif ($C)        {++$N}
   }
  return 0 unless $o;
  my $overlap = $o / ($n+$N+$o);
  if ($overlap > 0.5)
   {say STDERR "AAAA overlap $o between $g($n) and $G($N)";
   }
 }
#podDocumentation

=pod

=encoding utf-8

=head1 Name

Data::Choice::Tree - Select an item from a list using a minimal number of fixed
size choices

=head1 Synopsis

Given an ordered list, each of whose items belongs to one or more genres,
produce a tree of minimum height that allows each item to be chosen with a
minimum number of choices with no more than a specified number of options in
each choice.

=head1 Example

Create a file by writing some of data to it, read the file to get its contents,
delete the file, then try to read the deleted file again:

 use Data::Choice::Tree;

  my $g = Data::Choice::Tree::from(4,
   [[qw(Science Biology)], q(Flowers of the Himalayas)],
   [[qw(Science Biology)], q(Penguids of the Antarctic)],
     $g->userid              = "...";
     $g->repository          = "test";
     $g->gitFile             = "test.html";
     $g->personalAccessToken = "...";

  say STDERR
     "Write : ", dump($g->write(join '-', 1..9)),
   "\nRead 1: ", dump($g->read),
   "\nDelete: ", dump($g->delete),
   "\nRead 2: ", dump($g->read);

Produces:

 Write : 'created';
 Read 1: "1-2-3-4-5-6-7-8-9"
 Delete: 1
 Read 2: undef

=head1 Description

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
use warnings FATAL=>qw(all);
use strict;
use Test::More tests=>1;

binModeAllUtf8;

if (0)
 {my @items =
   ([q(1a), [qw(1 A a)], 100],
    [q(1b), [qw(1 A b)], 110],
    [q(1c), [qw(1 A c)], 121],
    [q(1d), [qw(1 B d)], 110],
    [q(1e), [qw(1 B e)], 120],
    [q(1f), [qw(1 B f)], 102],

    [q(2a), [qw(2 A a)], 121],
    [q(2b), [qw(2 A b)], 111],
    [q(2c), [qw(2 A c)], 102],
    [q(2d), [qw(2 B d)], 112],
    [q(2e), [qw(2 B e)], 110],
   );

  my $tree = new(4, @items);
  if (1)
   {my $s = join '', $tree->dump;
say STDERR $s;
    my $t = <<END;
choice:   1 [2] 1d 1f
  choice:   B [2] 1d 1f
    item:      1d 0 110
    item:      1e 1 120
    item:      1f 0 102
  promoted: 1b 1 110
  promoted: 1a 1 100
choice:   2 [2] 2d 2e
  choice:   B [2] 2d 2e
    item:      2d 0 112
    item:      2e 0 110
  promoted: 2a 1 121
  promoted: 2b 1 111
  promoted: 2c 1 102
promoted: 1c 1 121
promoted: 1e 1 120
END
    ok nws($s) eq nws($t) or STDERR->say("AAAA ", dump([$s, $t]));
   }

  if (1)
   {my $s = join '', $tree->print;
    my $t = <<END;
choice: A
  choice: 2
    item:    2a
    item:    2b
    item:    2c
  item:    1b
  item:    1a
choice: B
  choice: 2
    item:    2d
    item:    2e
  item:    1e
  item:    1d
  item:    1f
item:    1c
item:    2a
END
    ok nws($s) eq nws($t) or STDERR->say("AAAA ", dump([$s, $t]));
   }
  say STDERR $tree->print;
 }

if ()
 {my @items = &exampleItems;
  my $tree = new(4, @items);
  my $s = join '', $tree->dump;
  my $t = <<END;
END

  say STDERR $tree->dump;
 }


if (0)                                                                          # Examine overlap
 {my @items =
    map {Item(name=>$$_[0], genre=>{map{$_=>1} @{$$_[1]}})} &exampleItems;
  my $genres = countGenres @items;
  for   my $g(sort keys %$genres)
   {for my $G(sort keys %$genres)
     {next if $g le $G;
      genreOverlap($g, $G, @items);
     }
   }
 }

sub exampleItems
 {my @items = map
   {my ($title, $genres) = /\A(.{52})(.*)\Z/;
    my @genres = split /\s+/, trim($genres);
    [trim($title), [@genres]]
   } split /\n/, &exampleSource;
  $items[$_][2] = @items - $_ for keys @items;
  @items
 }

sub exampleSource{<<END}
A Day at the Fun Fair                                   English  Rhymes FunFair
A Day at the Fun Fair 1                                 English  Rhymes FunFair
A Day at the Fun Fair 2                                 English  Rhymes FunFair
A Day at the Fun Fair 3                                 English  Rhymes FunFair
Addition                                                Maths Arithmetic
Addition 1                                              Maths Arithmetic
Adjectives                                              English  Adjectives
Adjectives 1                                            English  Adjectives
Adjectives 2                                            English  Adjectives
Adjectives 3                                            English  Adjectives
Amazing Apes                                            Animals Biology Apes
Amazing Apes 1                                          Animals Biology Apes
Animal Eyes                                             Animals Biology Eyes
Animal Eyes 1                                           Animals Biology Eyes
Animal Eyes 2                                           Animals Biology Eyes
Animals Yawning                                         Animals Biology Behaviour
Animals Yawning 1                                       Animals Biology Behaviour
Animals Yawning 2                                       Animals Biology Behaviour
Another Day, Another Sunset                             Music
Another Day, Another Sunset 1                           Music
Arches National Park USA                                Geography USA Parks
Arches National Park USA 1                              Geography USA Parks
Baby Animals                                            Animals  Baby
Baby Animals 1                                          Animals  Baby
Baby Animals 2                                          Animals  Baby
Bathroom                                                English  Home Bath
Bathroom 1                                              English  Home Bath
Bathroom 2                                              English  Home Bath
Bathroom 3                                              English  Home Bath
Boots and Shoes                                         English  Clothing
Boots and Shoes 1                                       English  Clothing
Boots and Shoes 2                                       English  Clothing
Bridges                                                 Technology Engineering Civil
Bridges 1                                               Technology Engineering Civil
British Royal Family                                    People
British Royal Family 1                                  People
Cars Cranes Trucks Trains                               English  Technology Transport Engineering
Cars Cranes Trucks Trains 1                             English  Technology Transport Engineering
Cars Cranes Trucks Trains 2                             English  Technology Transport Engineering
Cars Cranes Trucks Trains in French                     French   Technology Transport Engineering
Cars Cranes Trucks Trains in French 1                   French   Technology Transport Engineering
Cars Cranes Trucks Trains in French 2                   French   Technology Transport Engineering
Cars Cranes Trucks Trains in German                     German   Technology Transport Engineering
Cars Cranes Trucks Trains in German 1                   German   Technology Transport Engineering
Cars Cranes Trucks Trains in German 2                   German   Technology Transport Engineering
China                                                   Geography East
China 1                                                 Geography East
China 2                                                 Geography East
Chocolate                                               Food
Chocolate 1                                             Food
Chocolate 2                                             Food
Christmas                                               English  Holidays
Christmas 1                                             English  Holidays
Christmas 2                                             English  Holidays
Cliffs, Stacks and Monoliths                            Geography Geology
Cliffs, Stacks and Monoliths 1                          Geography Geology
Cliffs, Stacks and Monoliths 2                          Geography Geology
Collective Nouns in English                             English  Collections
Collective Nouns in English 1                           English  Collections
Collective Nouns in German                              German   Collections
Collective Nouns in German 1                            German   Collections
Common Word Endings                                     English  Suffixes
Common Word Endings 1                                   English  Suffixes
Common Word Endings 2                                   English  Suffixes
Common Word Endings 3                                   English  Numbers
Count Action Toys 1-10                                  Numbers
Count Action Toys 1-10 1                                Numbers
Count Animals 1-10                                      Numbers
Count Animals 1-10 1                                    Numbers
Count Chocolates 1-20                                   Numbers
Count Chocolates 1-20 1                                 Numbers
Count Chocolates 1-20 2                                 Numbers
Count Dogs 1-10                                         Numbers
Count Dogs 1-10 1                                       Numbers
Count Dolls 1-20                                        Numbers
Count Dolls 1-20 1                                      Numbers
Count Dolls 1-20 2                                      Numbers
Count Euros                                             Maths
Count Euros 1                                           Maths
Count Euros 2                                           Maths
Count Flowers 1-10                                      Numbers
Count Flowers 1-10 1                                    Numbers
Count Flowers in German                                 Numbers
Count Flowers in German 1                               Numbers
Count Soft Toys 1-10                                    Numbers
Count Soft Toys 1-10 1                                  Numbers
Count Soft Toys 1-10 2                                  Numbers
Count Teddy Bears 1-20                                  Numbers
Count Teddy Bears 1-20 1                                Numbers
Count Toy Dogs 1-20                                     Numbers
Count Toy Dogs 1-20 1                                   Numbers
Days of the Week in English                             English  Time
Days of the Week in English 1                           English  Time
Days of the Week in German                              German   Time
Days of the Week in German 1                            German   Time
Death Valley USA                                        Geography World
Death Valley USA 1                                      Geography World
Easy Animal Alphabet                                    English Alphabet Animals
Easy Animal Alphabet 1                                  English Alphabet Animals
Easy Animal Alphabet 2                                  English Alphabet Animals
Easy English Alphabet                                   English Alphabet General
Easy English Alphabet 1                                 English Alphabet General
Easy English Alphabet 2                                 English Alphabet General
Easy English Alphabet 3                                 English Alphabet General
Easy Flower Alphabet                                    English Alphabet Flowers
Easy Flower Alphabet 1                                  English Alphabet Flowers
Easy Flower Alphabet 2                                  English Alphabet Flowers
Easy Flower Alphabet 3                                  English Alphabet Flowers
Easy Soft Toy Alphabet                                  English Alphabet Toys
Easy Soft Toy Alphabet 1                                English Alphabet Toys
Easy Soft Toy Alphabet 2                                English Alphabet Toys
Easy Toy Transport Alphabet                             English Alphabet Toys Transport
Easy Toy Transport Alphabet 1                           English Alphabet Toys Transport
Easy Transport Alphabet                                 English Alphabet Transport
Easy Transport Alphabet 1                               English Alphabet Transport
Easy Transport Alphabet 2                               English Alphabet Transport
Easy Transport Alphabet 3                               English Alphabet Transport
Easy Weather Vane Alphabet                              English Alphabet Transport
Easy Weather Vane Alphabet 1                            English Alphabet Transport
Famous City Landmarks                                   Geography Cities World
Famous City Landmarks 1                                 Geography Cities World
Famous City Landmarks 2                                 Geography Cities World
Famous City Landmarks 3                                 Geography Cities World
Famous London Landmarks                                 Geography Cities London
Famous London Landmarks 1                               Geography Cities London
Famous London Landmarks 2                               Geography Cities London
Famous London Landmarks 3                               Geography Cities London
Fish of the Coral Reef                                  Animals Fish Coral
Fish of the Coral Reef 1                                Animals Fish Coral
Fish of the Coral Reef 2                                Animals Fish Coral
Five-Letter Words                                       English  Words 5
Five-Letter Words 1                                     English  Words 5
Five-Letter Words 2                                     English  Words 5
Five-Letter Words 3                                     English  Words 5
Four-Letter Words                                       English  Words 4
Four-Letter Words 1                                     English  Words 4
Four-Letter Words 2                                     English  Words 4
Four-Letter Words 3                                     English  Words 4
Fractions                                               Maths Arithmetic Fractions
Fractions 1                                             Maths Arithmetic Fractions
Fruits and Berries                                      Food Biology Agriculture
Fruits and Berries 1                                    Food Biology Agriculture
Fruits and Berries 2                                    Food Biology Agriculture
Garden Flowers                                          Flowers Biology
Garden Flowers 1                                        Flowers Biology
Garden Flowers 2                                        Flowers Biology
Geography Man-Made Features                             Geography ManMade
Geography Man-Made Features 1                           Geography ManMade
Geography Man-Made Features 2                           Geography ManMade
Geography Man-Made Features in German                   Geography ManMade German
Geography Man-Made Features in German 1                 Geography ManMade German
Geography Man-Made Features in German 2                 Geography ManMade German
Geography Natural Features                              Geography Natural
Geography Natural Features 1                            Geography Natural
Geography Natural Features 2                            Geography Natural
Geometric Shapes                                        Maths Geometery
Geometric Shapes 1                                      Maths Geometery
Geometric Shapes 2                                      Maths Geometery
Glacier National Park, USA                              Geography World Parks
Glacier National Park, USA 1                            Geography World Parks
Great Olympians                                         Sport History
Great Olympians 1                                       Sport History
Great Olympians 2                                       Sport History
Great Olympians 3                                       Sport History
Great Sports Cars                                       TopTen Technology Sport Cars
Great Sports Cars 1                                     TopTen Technology Sport Cars
Great Sports Cars 2                                     TopTen Technology Sport Cars
Great Sports Cars 3                                     TopTen Technology Sport Cars
Greatest Common Divisor                                 Maths Arithmetic Division
Greatest Common Divisor 1                               Maths Arithmetic Division
House                                                   English  Home Rhymes
House 1                                                 English  Home Rhymes
House 2                                                 English  Home Rhymes
How Heavy Am I?                                         Science Biology Numbers
How Heavy Am I? 1                                       Science Biology Numbers
How Heavy Am I? 2                                       Science Biology Numbers
How Much Power Do I Use?                                Science Physics Numbers
How Much Power Do I Use? 1                              Science Physics Numbers
India                                                   Geography Country East
India 1                                                 Geography Country East
India 2                                                 Geography Country East
Jets and Planes                                         English  Technology Planes
Jets and Planes 1                                       English  Technology Planes
Jets and Planes 2                                       English  Technology Planes
Lots of Animals                                         English  Biology Numbers
Lots of Animals 1                                       English  Biology Numbers
Lots of Animals 2                                       English  Biology Numbers
Man On The Moon                                         Technology Lunar
Man On The Moon 1                                       Technology Lunar
Man On The Moon 2                                       Technology Lunar
Marine Aquarium Fish                                    Animals Fish Marine
Marine Aquarium Fish 1                                  Animals Fish Marine
Marine Aquarium Fish 2                                  Animals Fish Marine
Mathematical Functions                                  Maths Calculus
Mathematical Functions 1                                Maths Calculus
Mathematical Functions 2                                Maths Calculus
Mathematical Functions 3                                Maths Calculus
Model Puppy Dog Breeds 1                                Animals Models
Model Puppy Dog Breeds 1 1                              Animals Models
Model Puppy Dog Breeds 2                                Animals Models
Model Puppy Dog Breeds 2 1                              Animals Models
Months of the Year in English                           English  Date
Months of the Year in English 1                         English  Date
Months of the Year in German                            German   Date
Months of the Year in German 1                          German   Date
National Parks USA                                      Geography World Parks
National Parks USA 1                                    Geography World Parks
National Parks USA 2                                    Geography World Parks
Nice Birds                                              Animals Birds
Nice Birds 1                                            Animals Birds
Nice Birds 2                                            Animals Birds
Noses                                                   Animals Biology Body
Noses 1                                                 Animals Biology Body
Noses 2                                                 Animals Biology Body
Organic Molecules                                       Science Chemistry
Organic Molecules 1                                     Science Chemistry
Organic Molecules 2                                     Science Chemistry
Organic Molecules 3                                     Science Chemistry
Parts of the Body                                       English Body Biology Human
Parts of the Body 1                                     English Body Biology Human
Parts of the Body 2                                     English Body Biology Human
Paws And Claws                                          Animals  Biology Body
Paws And Claws 1                                        Animals  Biology Body
Paws And Claws 2                                        Animals  Biology Body
Penguins of Antarctica                                  Animals  Biology Penguins
Penguins of Antarctica 1                                Animals  Biology Penguins
Plus or Minus, More or Less                             Maths Arithmetic
Plus or Minus, More or Less 1                           Maths Arithmetic
Plus or Minus, More or Less 2                           Maths Arithmetic
Puppies and Dogs                                        Animals Dogs
Puppies and Dogs 1                                      Animals Dogs
Puppies and Dogs 2                                      Animals Dogs
Puppies and Dogs 3                                      Animals Dogs
Rhyming Kitchen in English                              English  Home Rhymes
Rhyming Kitchen in English 1                            English  Home Rhymes
Rhyming Kitchen in English 2                            English  Home Rhymes
Rhyming Kitchen in English 3                            English  Home Rhymes
Ride through the Gap of Dunloe in South-West Ireland    Geography Ireland
Ride through the Gap of Dunloe in South-West Ireland 1  Geography Ireland
Sharks                                                  Animals Biology Fish Sharks
Sharks 1                                                Animals Biology Fish Sharks
Sharks 2                                                Animals Biology Fish Sharks
Sharks In German                                        German  Biology Fish Sharks
Sharks In German 1                                      German  Biology Fish Sharks
Sharks In German 2                                      German  Biology Fish Sharks
Signs of the Zodiac                                     People Horoscopes
Signs of the Zodiac 1                                   People Horoscopes
Stars of Bollywood                                      People Movies
Stars of Bollywood 1                                    People Movies
Stars of Bollywood 2                                    People Movies
Subtraction                                             Maths Arithmetic
Subtraction 1                                           Maths Arithmetic
Sun, Moons and Planets                                  Science Astronomy
Sun, Moons and Planets 1                                Science Astronomy
Sun, Moons and Planets 2                                Science Astronomy
Super Birds                                             Animals Biology Birds
Super Birds 1                                           Animals Biology Birds
Super Fish                                              Animals Biology Fish
Super Fish 1                                            Animals Biology Fish
Super Mammals                                           Animals Biology Mammals
Super Mammals 1                                         Animals Biology Mammals
Teddy Bears In Action                                   English Verbs
Teddy Bears In Action 1                                 English Verbs
Teddy Bears In Action In German                         German Verbs
Teddy Bears In Action In German 1                       German Verbs
Teddy Bears In Action In German 2                       German Verbs
Tell the Time                                           English Time
Tell the Time 1                                         English Time
Tell the Time Two                                       English Time
Tell the Time Two 1                                     English Time
Tell the Time in German                                 German  Time
Tell the Time in German 1                               German  Time
Test Cricketers of Australia                            Sport   Cricket Australia
Test Cricketers of Australia 1                          Sport   Cricket Australia
Test Cricketers of Australia 2                          Sport   Cricket Australia
Test Cricketers of India                                Sport   Cricket India
Test Cricketers of India 1                              Sport   Cricket India
Test Cricketers of India 2                              Sport   Cricket India
The Four Seasons in English                             English Time Calendar
The Four Seasons in English 1                           English Time Calendar
The Greek Alphabet                                      Maths Greek Alphabet
The Greek Alphabet 1                                    Maths Greek Alphabet
The Irregular Past in English                           English Verbs Past
The Irregular Past in English 1                         English Verbs Past
The Irregular Past in English 2                         English Verbs Past
The Irregular Past in English 3                         English Verbs Past
The Lake District                                       Geography England Parks
The Lake District 1                                     Geography England Parks
The Lake District 2                                     Geography England Parks
The Numbers from 1 to 100 in English                    English Numbers
The Numbers from 1 to 100 in English 1                  English Numbers
The Numbers from 1 to 100 in French                     French Numbers
The Numbers from 1 to 100 in French 1                   French Numbers
The Numbers from 1 to 100 in German                     German Numbers
The Numbers from 1 to 100 in German 1                   German Numbers
Three-Letter Words                                      English Words 3
Three-Letter Words 1                                    English Words 3
Three-Letter Words 2                                    English Words 3
Three-Letter Words 3                                    English  Words 3
To Be Or Not To Be                                      Shakespeare
To Be Or Not To Be 1                                    Shakespeare
Top 10 Cereals                                          Food Cereals
Top 10 Cereals 1                                        Food Cereals
Top 10 Fast Motor Bikes                                 TopTen MotorBikes
Top 10 Fast Motor Bikes 1                               TopTen MotorBikes
Top 10 Highest Mountains                                Geography Nepal Parks
Top 10 Highest Mountains 1                              Geography Nepal Parks
Top 10 Highest Mountains 2                              Geography Nepal Parks
Top 10 Tallest Asia Pacific Roller Coasters             TopTen RollerCoasters Asia
Top 10 Tallest Asia Pacific Roller Coasters 1           TopTen RollerCoasters Asia
Top 10 Tallest Asia Pacific Roller Coasters 2           TopTen RollerCoasters Asia
Top 10 Tallest European Roller Coasters                 TopTen RollerCoasters Europe
Top 10 Tallest European Roller Coasters 1               TopTen RollerCoasters Europe
Top 10 Tallest European Roller Coasters 2               TopTen RollerCoasters Europe
Top 10 Tallest Ferris Wheels                            TopTen RollerCoasters Ferris
Top 10 Tallest Ferris Wheels 1                          TopTen RollerCoasters Ferris
Top 10 Tallest Ferris Wheels 2                          TopTen RollerCoasters Ferris
Top 10 Tallest North American Roller Coasters           TopTen RollerCoasters USA
Top 10 Tallest North American Roller Coasters 1         TopTen RollerCoasters USA
Top 10 Tallest North American Roller Coasters 2         TopTen RollerCoasters USA
Top 10 Tallest Roller Coasters                          TopTen RollerCoasters World
Top 10 Tallest Roller Coasters 1                        TopTen RollerCoasters World
Top 10 Tallest Roller Coasters 2                        TopTen RollerCoasters World
Top 10 Tallest Towers                                   TopTen Towers
Top 10 Tallest Towers 1                                 TopTen Towers
Top 20 Cats and Kittens                                 Animals Cats
Top 20 Cats and Kittens 1                               Animals Cats
Top 20 Horse Breeds                                     Animals Horses
Top 20 Horse Breeds 1                                   Animals Horses
Top 50 Dog Breeds                                       Animals Dogs
Top 50 Dog Breeds 1                                     Animals Dogs
Top 50 Dog Breeds 2                                     Animals Dogs
Town and Country Buildings                              English Buildings
Town and Country Buildings 1                            English Buildings
Town and Country Buildings 2                            English Buildings
Trace Numbers 0 to 9                                    Numbers Digits English
Trace Numbers 0 to 9 1                                  Numbers Digits English
Trace Numbers 0 to 9 in Cantonese                       Numbers Digits Cantonese
Trace Numbers 0 to 9 in Cantonese 1                     Numbers Digits Cantonese
Trace Numbers 0 to 9 in French                          Numbers Digits French
Trace Numbers 0 to 9 in French 1                        Numbers Digits French
Trace Numbers 0 to 9 in German                          Numbers Digits German
Trace Numbers 0 to 9 in German 1                        Numbers Digits German
Trace Numbers 0 to 9 in Mandarin                        Numbers Digits Mandarin
Trace Numbers 0 to 9 in Mandarin 1                      Numbers Digits Mandarin
Trees                                                   Trees
Trees 1                                                 Trees
Trees 2                                                 Trees
Tugs and Tankers, Ships and Boats                       Technology Boats
Tugs and Tankers, Ships and Boats 1                     Technology Boats
Tugs and Tankers, Ships and Boats 2                     Technology Boats
Waterfalls                                              Geography Waterfalls
Waterfalls 1                                            Geography Waterfalls
Waterfalls 2                                            Geography Waterfalls
Wild Animals                                            Animals Wild
Wild Animals 1                                          Animals Wild
Wild Animals 2                                          Animals Wild
Wild Animals in German                                  German  Animals Wild
Wild Animals in German 1                                German  Animals Wild
Wild Animals in German 2                                German  Animals Wild
Wild Animals of Antarctica                              Animals Wild Antarctica
Wild Animals of Antarctica 1                            Animals Wild Antarctica
Wild Animals of Antarctica 2                            Animals Wild Antarctica
Wild Animals of the Arctic                              Animals Wild Artic
Wild Animals of the Arctic 1                            Animals Wild Artic
Wild Animals of the Arctic 2                            Animals Wild Artic
Wild Flowers                                            Flowers Wild
Wild Flowers 1                                          Flowers Wild
Wild Flowers 2                                          Flowers Wild
Wild Flowers in German                                  German  Wild
Wild Flowers in German 1                                German  Wild
Wild Flowers in German 2                                German  Wild
Wild Flowers of the Alps                                Flowers Wild Alps
Wild Flowers of the Alps 1                              Flowers Wild Alps
Wild Flowers of the Alps 2                              Flowers Wild Alps
World of Colour                                         English  Colours
World of Colour 1                                       English  Colours
World of Colour 2                                       English  Colours
Yosemite National Park, USA                             Geography Parks USA
Yosemite National Park, USA 1                           Geography Parks USA
END
