#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib
#-------------------------------------------------------------------------------
# Preprocess ◁, ◀, ▷ and ▶ as operators in ANSI-C.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2020
#-------------------------------------------------------------------------------
# podDocumentation
package Preprocess::Ops;
our $VERSION = 20201117;
use warnings FATAL => qw(all);
use strict;
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all !trim);
use feature qw(say current_sub);
use utf8;

my $logC = q(/home/phil/c/z/z/zzz.c);                                           # Log to this file if present
my $logH = q(/home/phil/c/z/z/zzz.h);

binModeAllUtf8;                                                                 # Helps with debugging unicode characters

#D1 Preprocess                                                                  # Preprocess ◁, ◀, ▷ and ▶ as operators in ANSI-C.

sub trimComment($)                                                              #P Remove trailing white space and comment
 {my ($s) = @_;                                                                 # String
  $s =~ s(\s*//.*\n) ()r;
 }

sub method($)                                                                   #P Check whether a line of C code defines a method, returning (return, name, flags, comment) if it is, else ()
 {my ($line) = @_;                                                              # Line of C code
  return () if $line =~ m(test.*//T\S);                                         # Tests are never methods
  if ($line =~ m(\Astatic\s*(.*?)((?:\w|\$)+)\s+//(\w*)\s*(.*)\Z))              # Static function is always a method
   {return ($1, $2, $3, $4)
   }
  if ($line =~ m(\A(.*?)(new(?:\w|\$)+)\s+//(\w*)\s*(.*)\Z))                    # Constructor is always a method
   {return ($1, $2, $3, $4);
   }
  ()
  }

sub structure($)                                                                #P Check whether a line of C code defines a structure, returning (name, flags, comment) if it is, else ()
 {my ($line) = @_;                                                              # Line of C code

  if ($line =~ m(\A(typedef\s+)?struct\s+((?:\w|\$)+)\s*//(w*)\s*(.*)\Z))       # struct name, comment start, flags, comment
   {return ($2, $3, $4)
   }
  ()
  }

sub mapCode($)                                                                  #P Find the structures and methods defined in a file
 {my ($file) = @_;                                                              # Input file

  my %methods;                                                                  # Method descriptions
  my %structures;                                                               # Structures defined

  my @code = readFile($file);                                                   # Read input file
  for my $line(@code)                                                           # Index of each line
   {next if $line =~ m(\A//);                                                   # Ignore comment lines

    my ($return, $name, $flags, $comment) = method($line);                      # Parse function return, name, description comment
    if ($name)
     {$methods{$name}++                                                         # Return type
     }
    else
     {my ($name, $flags, $comment) = structure($line);                          # Parse structure definition
      if ($name)
       {$structures{$name}++
       }
     }
   }

  genHash(q(PreprocessOpsMap),                                                  # Methods and structures in the C file being preprocessed
    methods    => \%methods,                                                    # Methods.
    structures => \%structures,                                                 # Structure definitions.
   );
 }

sub printData($$)                                                               #P Print statement
 {my ($lineNumber, $line) = @_;                                                 # Code line number, code line

  my ($command, @w) = split m/\s+/, $line;                                      # Parse print line
  my @f;
  for my $w(@w)                                                                 # Each variable to be printed
   {push @f, join ' ', $w, "=", $w =~ m((\A|\.|\->)[i-n]) ? "%lu" : "%s";
   }
  my $f = join " ",  @f;
  my $w = join ", ", @w;
  my $l = $lineNumber + 1;
  qq(fprintf(stderr, "Line $l: $f\\n", $w);\n);
 }

sub duplicateFunction($$$)                                                      #P Duplicate the previous function with the specified changes applied
 {my ($lineNumber, $inputFile, $code) = @_;                                     # Line number of line being expanded, file containing line being expanded, lines of code
  if ($$code[$lineNumber] =~ m(\A(duplicate)\s+))                               # Parse duplicate statement: the words after are comma separated lists of regular expressions that change the text of the preceding function
   {my ($command, @changes) = split /\s+/, $$code[$lineNumber];
    my @c;
    for(my $i = $lineNumber - 1; $i >= 0; --$i)                                 # Text of preceding function to duplicate
     {unshift @c, my $c = $$code[$i];
      last if $c =~ m(\Astatic);                                                # Back to the previous static that started the function definition
     }
    my @r;                                                                      # Resulting code
    for my $change(@changes)                                                    # Apply changes
     {my @C;                                                                    # Code after each change
      for my $c(@c)                                                             # Each change
       {local $_ = $c;
        for my $r(split/,/, $change)                                            # Each re in the change
         {eval $r;
          confess "Cannot make change: $r in: $change\n$@\n" if $@;
         }
        push @C, $_;                                                            # Save accumulated changes
       }

      my $l = $lineNumber + 2;                                                  # Save duplicate code with accumulated changes
      push @r, join '', @C;
      push @r, qq(#line $l "$inputFile"\n);
     }

    my $r = join '', @r;                                                        # Changed code
    return $r;
   }

  confess $$code[$lineNumber]," is not a 'duplicate' command";
 }

sub includeFile($$$$$)                                                          #P Expand include files so that we can pull in code and structures from other files in the includes folder.
 {my ($lineNumber, $inputFile, $cFile, $hFile, $code) = @_;                     # Line number of line being expanded, file containing line being expanded, output C file, output H file, line of code
  if ($code =~ m(\A(include)\s+))                                               # Parse preprocessor statement
   {my ($command, $relFile, @items) = split /\s+/, $code;
    my %items = map {$_=>1} @items;
    my $file = sumAbsAndRel($inputFile, $relFile);
    -e $file or confess "Cannot find include file: $file\n";

    my @code = readFile($file);
#   my $map  = mapCode($inputFile);

    for(my $i = 0; $i < @code; ++$i)                                            # Expand duplicate commands
     {if ($code[$i] =~ m(\Aduplicate ))                                         # Duplicate the previous function with changes
       {$code[$i] = duplicateFunction($i, $inputFile, \@code);
       }
     }

    my @c;
    for(my $i = 0; $i < @code; ++$i)                                            # Expand exports/include commands in included file
     {my  $c = $code[$i];                                                       # With    trailing comment
      my  $d = $c =~ s(//.*\Z) ()gsr;                                           # Without trailing comment
      if ($c =~ m(\Ainclude))                                                   # Expand include files so that we can pull in code and structures from other files in the includes folder.
       {push @c, &includeFile($i, $file, $cFile, $hFile, $d);
       }
      elsif ($c =~ m(\Aexports\s))                                              # Add exports from included package if named in the include list
       {my ($command, $name, @exports) = split m/\s+/, $d;                      # Export command, list name, exports in list
        if ($items{qq(:$name)})                                                 # Requested this list
         {for my $e(@exports)                                                   # Add exports unless they have been excluded
           {$items{$e} ++ unless $items{qq(!$e)};
           }
         }
       }
      elsif (method($c) or structure($c))                                       # Method or structure definition
       {if ($c =~ m((\S+)\s*//))                                                # Method or structure name
         {my $item = $1;
          if ($command =~ m(include)      &&  $items            {$item})        # Include specifies the exact name of the thing we want
           {push @c, join ' ', "#line", $i+2, qq("$file"), "\n";
            my @l;
            for(; $i < @code; ++$i)
             {push @l, $code[$i];
              last if $code[$i] =~ m(\A })
             }
            if (@l)                                                             # Save included struct or method
             {$l[0] =~ s(//) (//I);                                             # Mark as included
#             $l[0] =~ s/\Astatic /static __attribute__ ((unused)) /;           # Mark included methods as potentially unused
              push @c, @l;
             }
           }
         }
       }
     }
    my $l = $lineNumber + 2;                                                    # Adjust line numbers to reflect unexpanded source
    return join '', @c, qq(#line $l "$inputFile" // CC\n);
#   return join '', @c;
   }
  confess "Unable to parse include statement:\n$code";
 } # includeFile

sub c($$$;$)                                                                    # Preprocess ▷ and ▶ as method dispatch operators in ANSI-C.
 {my ($inputFile, $cFile, $hFile, $column) = @_;                                # Input file, C output file, H output file, optional start column for comments (80)

  my $baseFile      = fn $inputFile;                                            # The base name of the file
  my($shortBaseFile)= split /_/, $baseFile;                                     # Base name of the file preceding first underscore
  my $packageName   = ucfirst $baseFile;                                        # The package name which is used to replace $
  my $commentColumn = ($column // 80) - 1;                                      # Column in which to start comments

  my %methods;                                                                  # Method descriptions
  my %structures;                                                               # Structures defined
  my %structureParameters;                                                      # Structures used as parameters
  my %testsFound;                                                               # Tests found
  my %testsNeeded;                                                              # Tests needed
  my @forwards;                                                                 # Forward declarations of functions used as methods
  my @code = readFile($inputFile);                                              # Read code
  my %exports;                                                                  # Export statements encountered

  for my $i(keys @code)                                                         # Execute preprocessor commands found in the source
   {my $c = $code[$i];
    if    ($c =~ m(\A(include)\s+))                                             # Expand include files so that we can pull in code and structures from other files in the includes folder.
     {$code[$i] = includeFile($i, $inputFile, $cFile, $hFile, $c);
     }
    elsif ($c =~ m(\Aduplicate ))                                               # Duplicate the previous function with changes
     {$code[$i] = duplicateFunction($i, $inputFile, \@code);
     }
    elsif ($c =~ m(\A(exports)\s+))                                             # Skip export commands in open source
     {$exports{$c} = $i+1;
      $code[$i] = "\n";
     }
    elsif ($c =~ m(\Aprint))                                                    # Expand print statements
     {$code[$i] = printData($i, $c);
     }
   }

  @code = map {"$_\n"} split /\n/, join '', @code;                              # Resplit code plus any additions into lines

  my sub expand($)                                                              # Expand $ and @
   {$_[0] =~ s(\$\$) ($baseFile)gs;                                             # $$ is base file name with first char lower cased
    $_[0] =~ s(\$)   ($packageName)gs;                                          # $  is base file name with first character uppercased
    $_[0] =~ s(\@)   (${shortBaseFile}_)gs;                                     # @ is  translated to short base file name followed by underscore to support Gnome naming conventions
   }

  expand($_) for @code;                                                         # Replace $ with package name.

  if (1)                                                                        # Parse source code
   {my %duplicates; my @duplicates;                                             # Duplication check for first parameter plus short method name
    for my $i(keys @code)                                                       # Index of each line
     {my $line = $code[$i];
      next if $line =~ m(\A//);                                                 # Ignore comment lines

      my ($return, $name, $flags, $comment) = method($line);                    # Parse function return, name, description comment
      if ($name)
       {$methods{$name}{return}  = $return;                                     # Return type
        $methods{$name}{flags}   = {map {$_=>1} split //, $flags};              # Flags after comment start
        $methods{$name}{comment} = $comment;                                    # Comment
       ($methods{$name}{name})   = split /_/, $name;                            # Short name as used after call operator
        push @forwards, join ' ', trimComment($line);                           # Save function definition for forward declaration

        if ($line !~ m(\(void\)))                                               # Parse parameter definitions if there are any
         {for $i($i+1..$#code)                                                  # Parameter definitions
           {$line = $code[$i];
            if ($line    =~ m(\A\s*[(]?(.*?)\s*(\w+)[,)]\s*//\s*(.*)\Z))        # Variable: Get type, parameter name, comment
             {push $methods{$name}{parameters}->@*, [$1, $2, $3];
             }
            elsif ($line =~ m(\A\s*(.*?)\s*\(\*(\s*(const)?\s*\w+)\)\s*(.*?)[,\)]\s*//\s*(.*)\Z)) # Function: Get type, parameter name, comment
             {push $methods{$name}{parameters}->@*, ["$1 (*$2) $4", $2, $5];
             }
            elsif ($line =~ m(\A\s*\.\.\.\)\s*//\s*(.*)\Z))                     # Variadic list
             {push $methods{$name}{parameters}->@*, ["", "...", $1];
             }

            push @forwards, trimComment($line);                                 # Save function definition for forward declaration
            last if $line =~ m(\)\s*//);                                        # End of parameter list
           }
         }

        $forwards[-1] .= ';';                                                   # Terminate forward declaration
        if (my $o = $methods{$name}{structure} = $methods{$name}{parameters}[0][0])   # Structure parameter
         {$o =~ s((\A|\s+)const\s+) ()g;                                        # Remove const from structure name
          $o =~ s([*])              ()g;                                        # Remove pointer from structure name
          $o = nws($o);                                                         # Normalize white space
          $structureParameters{$o}{$name}++;                                    # Record methods in each structure
          if (my $d = $duplicates{"$name$o"})                                   # Check for duplicate
           {push @duplicates, [$name, $o, $i, $d];                              # Record duplicate
           }
          $duplicates{"$name$o"} = $i;
         }
       }
      if (1)
       {my ($name, $flags, $comment) = structure($line);                        # Parse structure definition
        if ($name)
         {$structures{$name} = genHash(q(PreprocessOpsStruct),                  # Structure declaration
            name    => $name,                                                   # Name of structure
            flags   => $flags,                                                  # Flags for structure
            comment => $comment);                                               # Comment for structure
         }
       }
     }
    if (@duplicates)                                                            # Print duplicates
     {for my $i(keys @code)                                                     # Index of each line
       {my $line = $code[$i];
        say STDERR sprintf("%06d  %s\n", $i, $line);
       }
      confess join "\n", "Duplicates:", dump(\@duplicates);
     }
    if (1)                                                                      # Locate tests for each method
     {my %m;                                                                    # Methods that need tests
      for my $m(sort keys %methods)
       {my $flags = $methods{$m}{flags};                                        # Flags for method
        next if $$flags{I} or $$flags{P};                                       # Ignore private methods marked with P and included methods marked with I
        $testsNeeded{$methods{$m}{name}}++;
       }

      for my $l(@code)                                                          # Each code line
       {my @t = $l =~ m((//T\w+))g;                                             # Tests located
        for my $t(@t)                                                           # Each test marker //T...
         {my $n = $t =~ s(\A//T) ()r;                                           # Remove //T
          delete $testsNeeded{$n};                                              # Test found for this method
          $testsFound{$n}++;                                                    # The tests we have found
         }
       }

      if (keys %testsNeeded)                                                    # Report methods that need tests
       {lll "The following methods need tests:\n", join "\n", sort keys %testsNeeded;
       }
     }
   }

  if (1)                                                                        # Write prototypes
   {my @h;                                                                      # Generated code
    for my $struct(sort keys %structures)                                       # Each structure regardless of whether it is actually used in a method
     {push @h, "struct ProtoTypes_$struct {";                                   # Start structure
      for my $m(sort keys $structureParameters{$struct}->%*)                    # Methods in structure
       {my $method = $methods{$m};                                              # Method
        my $s  = join '', '  ', $$method{return}, ' (*',  $$method{name}, ')('; # Start signature
        my $t  = join ' ', pad($s, $commentColumn), '//', $$method{comment};
        push @h, $t;
        my @p = $$method{parameters}->@*;                                       # Parameters for method
        for my $i(keys @p)                                                      # Each parameter
         {my ($return, $name, $comment) = $p[$i]->@*;

          my $cc      = $commentColumn;                                         # Comment column
          my $comma   = $i == $#p ? ');' : ',';                                 # Comma as separator
          my $Comment = "// $comment";                                          # Format comment
          my $off     = " " x 4;
          if ($return =~ m(\(*\s*(const)?\s*\w+\)))                             # Function parameter
           {push @h, join ' ', pad(qq($off$return$comma), $cc), $Comment;
           }
          else                                                                  # Variable parameter
           {push @h, join ' ', pad(qq($off$return $name$comma), $cc), $Comment;
           }
         }
       }
      push @h, join '', " } const ProtoTypes_", $struct, ' =';                  # Load prototype
      my $j =  join(', ', sort keys $structureParameters{$struct}->%*);         # List of functions
      push @h, join '', "{$j};";
      push @h, <<END;                                                           # Add a constructor for each structure
$struct new$struct($struct allocator) {return allocator;}
END
     }
    owf($hFile, join "\n", @forwards, @h, '');
   }

  if (1)                                                                        # Preprocess here documents in C code
   {my $state;                                                                  # True if we are in a here document
    for(my $i = 0; $i < @code; ++$i)                                            # Each start line
     {next if $code[$i] =~ m(\A//◉);                                            # Skip previously scanned here document
      if ($code[$i] =~ s((\A.*\S\s*)◉(.*)\Z) ($1))                              # Start of here document
       {my ($open, $close) = ($1, $2);                                          # open and close of here doc expression
        my @h;                                                                  # Lines of here document
        my $j = $i + 1;                                                         # Scan forwards
        for my $k($j..$#code)                                                   # Skip previously extracted here documents
         {last unless $code[$j = $k] =~ m(\A//◉);
         }
        for $j($j..$#code)                                                      # Scan forwards
         {my $c = $code[$j];
          if ($code[$j] =~ m(\A◉))                                              # Close
           {  $code[$j] = q(//).$code[$j];                                      # Comment close out
              my $h = join ' ', @h;                                             # Equivalent  string for here document
              $code[$i] = qq($open$h$close\n);                                  # Rebuild here doc line with one string
            last;
           }
          else                                                                  # One line of here document
           {my    $c = $code[$j];                                               # Remove new line
            chomp $c;                                                           # Remove new line
                  $c =~ s(") (\\\")g;                                           # Protect quotes
            push @h, qq("$c\\n");                                               # Save line
            $code[$j] = q(//◉).$code[$j];                                       # Comment line out

           }
         }
       }
     }
   }

  if (1)                                                                        # Preprocess input C file
   {my $e = q([a-z0-9𝗮-𝘇¹²³⁴⁵⁶⁷⁸⁹⁰₁₂₃₄₅₆₇₈₉₀ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ\$_>.*[\]-]); # Elements of a name
    for my $c(@code)                                                            # Source code lines
     {$c =~ s{($e+)\s*◀\s*(.*?);}       {typeof($2) $1 = $2;}gis;               # Variable creation
      $c =~ s{($e+)\s*◁\s*(.*?);} {const typeof($2) $1 = $2;}gis;               # Constant creation

      $c =~ s{new\s*(\w+\s*)\(([^:)]*:[^)]*)\)}                                 # Constructor with named arguments in parenthesis based on: https://gcc.gnu.org/onlinedocs/gcc-10.2.0/gcc/Designated-Inits.html#Designated-Inits
             {new$1(({struct $1 t = {$2, proto: &ProtoTypes_$1}; t;}))}gs;

      $c =~ s{new\s*(\w+\s*)(\(\))?([,;])}                                      # Constructor followed by [,;] calls for default constructor.
             {new$1(({struct $1 t = {proto: &ProtoTypes_$1};   t;}))$3}gs;

      $c =~ s{($e+)\s*▶\s*(\w+)\s*\(} {$1->proto->$2($1, }gis;                  # Method call with arguments from pointer
      $c =~ s{($e+)\s*▶\s*(\w+)}      {$1->proto->$2($1)}gis;                   # Method call with no arguments from pointer
      $c =~ s{($e+)\s*▷\s*(\w+)\s*\(} {$1.proto->$2(&$1, }gis;                  # Method call with arguments
      $c =~ s{($e+)\s*▷\s*(\w+)}      {$1.proto->$2(&$1)}gis;                   # Method call with no arguments

      if (1)                                                                    # String compare and assignment
       {$c =~ s{($e+)\s* !≈ \s*([^;)]*)} { strcmp($1, $2)}gisx;                 # String not equal
        $c =~ s{($e+)\s* ≈≈ \s*([^;)]*)} {!strcmp($1, $2)}gisx;                 # String equal
        $c =~ s{($e+)\s* ≋  \s*([^;)]*)} {char $1\[$2\]}gisx;                   # Create string
        $c =~ s{($e+)\s* ≈  \s*([^;)]*)} { strcpy($1, $2)}gisx;                 # String copy
        $c =~ s{($e+)\s* \+≈\s*([^;)]*)} { stpcpy($1, $2)}gisx;                 # String concatenation
        $c =~ s{($e+)\s* ∼  \s*([^;)]*)} { strstr($1, $2)}gisx;                 # Find second string in first string and return a pointer to it
       }

      $c =~ s{[☑✓✔✅]([^;]*)} {assert($1)}gis;                                   # Tick becomes assert
      $c =~ s{[☒✕❎✗✘✖❌]([^;]*)} {assertNot($1)}gis;                             # Cross becomes assert not

      if (1)                                                                    # Memory compare and assignment
       {my $s = 'source'x3; my $t = 'target'x3;                                 # Unlikely variable names
        $c =~ s{($e+)\s*!≞\s*([^;)]*)} {({typeof($1) $t = $1, $s = $2;  memcmp((void *)&$t, (void *)&$s, sizeof($1));})}gis; # Memory not equal
        $c =~ s{($e+)\s*≞≞\s*([^;)]*)} {({typeof($1) $t = $1, $s = $2; !memcmp((void *)&$t, (void *)&$s, sizeof($1));})}gis; # Memory equal
        $c =~ s{($e+)\s*≞\s*0\s*}      {memset((void *)&$1, 0,                                           sizeof($1))}gis;    # Clear memory if equal zero
        $c =~ s{($e+)\s*≞\s*([^;]*)}  {({typeof($1) $s = $2; memcpy((void *)&$1, (void *)&$s, sizeof($1));})}gis;            # Memory equals becomes memcpy if copying something not zero
       }

      if (1)                                                                    # Memory compare and assignment where one side of the equation (black) is a pointer to void
       {my $s = 'source'x3; my $t = 'target'x3;                                 # Unlikely variable names           # Type set by left hand, right hand is void *
        $c =~ s{($e+)\s*!◧\s*([^;)]*)} {({typeof($2) $s = $2;  memcmp((void *)$1,  (void *)&$s, sizeof($2));})}gis; # Memory not equal
        $c =~ s{($e+)\s*◧◧\s*([^;)]*)} {({typeof($2) $s = $2; !memcmp((void *)$1,  (void *)&$s, sizeof($2));})}gis; # Memory equal
        $c =~ s{($e+)\s*◧\s*([^;]*)}   {({typeof($2) $s = $2;  memcpy((void *)$1,  (void *)&$s, sizeof($2));})}gis; # Copy memory to make equal
                                                                                                                    # Type set by right hand size
        $c =~ s{($e+)\s*!◨\s*([^;)]*)} {({                     memcmp((void *)&$1, (void *)$2, sizeof($1));})}gis;  # Memory not equal
        $c =~ s{($e+)\s*◨◨\s*([^;)]*)} {({                    !memcmp((void *)&$1, (void *)$2, sizeof($1));})}gis;  # Memory equal
        $c =~ s{($e+)\s*◨\s*0\s*}      {                       memset((void *)&$1, 0,          sizeof($1))}gis;     # Clear memory if equal zero
        $c =~ s{($e+)\s*◨\s*([^;]*)}   {                       memcpy((void *)&$1, (void *)$2, sizeof($1))}gis;     # Memory equals becomes memcpy if copying something not zero
       }

      $c =~ s( +\Z) ()gs;                                                       # Remove trailing spaces at line ends
     }
   }

  if (1)                                                                        # Report export requests for methods that are missing
   {my @m;
    for my $x(sort keys %exports)
     {my ($command, $list, @e) = split /\s+/, $x;

      for my $e(@e)
       {expand($e);
        next unless $e =~ m(\A[a-z])i;
        push @m, [$exports{$x}, $e] unless $methods{$e} or $structures{$e};
       }
     }
    if (keys @m)
     {say STDERR formatTable(\@m, <<END,
Line   Line on which the missing method was exported
Export Method requested but missing
END
      title => q(Missing exports));
     }
   }

  owf($cFile, qq(#line 1 "$inputFile"\n).join('', @code));                      # Output C file

  genHash(q(PreprocessOpsParse),                                                # Structure of the C program being preprocessed
    methods             => \%methods,                                           # Methods.
    structures          => \%structures,                                        # Structure definitions.
    structureParameters => \%structureParameters,                               # Structures used as parameters
    testsFound          => \%testsFound,                                        # Tests found
    testsNeeded         => \%testsNeeded)                                       # Tests still needed
 }

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

Preprocess::Ops - Preprocess ◁, ◀, ▷ and ▶ as operators in ANSI-C.

=head1 Synopsis

See the final lines of:
L<https://github.com/philiprbrenan/C/blob/master/c/z/arenaTree/arenaTree.c>
for working examples of the following operators.

=head2 Method dispatch operators: ▷ and ▶

Preprocess ▷ and ▶ as method dispatch by translating:

  p = node ▷ key("a");

to:

  p = node . proto->key(&node, "a");

and:

  p = node ▶ key("a");

to:

  p = node -> proto->key(node, "a");

=head2 Constant and variable creation operators: ◁ and ◀

Preprocess instances of ◁ as a constant creation operator:

  c ◁ sfc("cba");

to get:

  const typeof(sfc("cba")) c = sfc("cba");

Preprocess instances of ◀ as a variable creation operator:

  d ◀ sfc("cba");

to get:

  typeof(sfc("cba")) c = sfc("cba");

which, in effect, produces:

  const char c = sfc("cba");
        char d = sfc("cba");

in the context of:

 char sfc(char *s) {return *s;}

 int main(void) {
   c ◁ sfc("cba");
   d ◀ sfc("cba");
   assert(c == 'c');
 }

=head2 Here documents via ◉

Preprocess instances of ◉ as starting a L<here|https://www.perldoc.pl/perlop#Quote-and-Quote-like-Operators> document:

  char *c = ◉;
a
  b
◉

to get:

  char *a =
"a\n"
"  b\n"
;

=head2 Using ✓ or ✔ for assert(...);

Convert instances of B<✓> or B<✔> as in:

  ✔ a == 1;

to:

  assert(a == 1);

to make B<assert> function calls more prominent in tests.

Conversely B<✗> as in:

    a ◁ 0;
  ✗ a;

converts to:

  const int a = 0;
  assert(!a);

=head2 Using ≞ for memcpy(...);

Convert instances of B<≞> as in:

  Colour c;
  c ≞ makeColour(0,1,0,1);

to:

  Colour c;
  memcpy(
   ((void *)&c,
   ({typeof(c) s = makeColour(0,1,0,1); (void *)&s;}),
   sizeof(c));

to allow piece-wise initialization of structures in memory with constant elements.

The special case of initialization with a constant zero:

  c ≞ 0;

produces:

  memset(c, 0, sizeof(c));

=head2 Using ≞≞ for !memcmp(...)

Convert instances of B<≞≞> and  B<!≞> as in:

  typedef struct
   {int a;
    int b;
   } S;
  S t,   s;  s.a = 1; s.b = 2;
    t  ≞ s;  // A
  ✓ s ≞≞ t;  // B
    t  ≞ 0;  // C
  ✓ s !≞ t;  // D

to get:

  // A
  ({typeof( t)  sourcesourcesource = s;
    memcpy(&t, &sourcesourcesource,
    sizeof( t));
  });

  // B
  assert(
   ({typeof(s) targettargettarget = s,
               sourcesourcesource = t;
     !memcmp(&targettargettarget, &sourcesourcesource, sizeof(s));
    }));

  // C
  memset(&t, 0, sizeof(t));

  // D
  assert(
   ({typeof(s)targettargettarget = s, sourcesourcesource = t;
     memcmp( &targettargettarget,    &sourcesourcesource,
     sizeof(s));
   }));

to allow compact comparisons of structures in memory.

=head2 Using ≈≈ for !strcmp(...);

Convert instances of B<≈≈> and  B<!≈> as in:

 {  a ≋ 12; b ≋ 12; c ≋ 12;
    a ≈ "aaaa";
    b ≈ "bbbb";
    A ◁ c +≈ a; B ◁ A +≈ b;
    C ◁ c ∼  b;

  ✓ a ≈≈ "aaaa";  // AAAA
  ✓ b ≈≈ "bbbb";
  ✓ b !≈ a;       // BBBB
  ✓ c ≈≈ "aaaabbbb";
  ✓ A ≈≈ b;
  ✓ B ≈≈ "";
  ✓ C ≈≈ b;

to get:

  assert(!strcmp(a, "aaaa"); // AAAA

  assert( strcmp(b, "bbbb"); // BBBB

to create readable string equality operations.

=head3 Using ≋ to declare strings.

Strings can be declared using the B<≋> operator to supply a length, so that:

  N ◁ 12;
  a ≋ N+1;

becomes:

  char a[13];

via:

  const int N = 12;
  char    a[N + 1];

=head3 Using +≈ to concatenate strings.

Strings can be concatenated with the B<+≈> operator, so that:

  c ≋ 12;
  a ◁ c +≈ "aaaa";
  b ◁ a +≈ "aaaa";
  ✓ c ≈≈ "aaaabbbb";

becomes:

  char c[12];
  char *a = stpcpy(c, "aaaa");
  char *b = stpcpy(a, "bbbb");
  assert(!strcmp(c,   "aaaabbbb");

=head3 Using ∼ to search strings.

Strings can be searched using the B<∼> operator to supply a search string as in:

    C ◁ c ∼ b;

which becomes:

  char *C = strstr(a, b);

=head2 Replacing $ with the base file name.

Occurrences of the B<$> character are replaced by the base name of the file
containing the source with the first letter capitalized, so that:

  typedef struct $Node {...} $Node;

in a file called B<tree.c> becomes:

  typedef struct TreeNode {...} TreeNode;

=head2 new operator

Occurrences of:

  new XXX

are replaced by:

  newXXX(({struct XXX t = {proto: &ProtoTypes_XXX}; t;}))

Occurrences of:

  new XXX(a:1)

are replaced by:

  newXXX(({struct XXX t = {a:1, proto: &ProtoTypes_XXX}; t;}))

The prototype vectors are generated by examining all the methods defined in the
B<c> file.  The prototype vectors are written to the specified B<h> file which
should be included in the B<c> file for use via the ▷ and ▶ operators.

=head2 Marking tests with //T

B<//T> immediately followed by the name of a method up to its first B<_> (if
any or the end of the name otherwise) marks a function as testing all the
methods that start with that name:

  void test10()                        //Tsystem //TprintsAs
   {  a ◁ make$FromString("uname");
      a ▷ system;
    ✓ a ▷ containsString("Linux");
    ✓ a ▷ printsAs(◉);
  Linux
  ◉
      a ▷ free;
   }

Function B<test10> is marked as testing both B<system_string> and
B<printsAs_stringBuffer_string>.  Functions that are declared B<static> but
have no associated tests are listed in the preprocessor output as in:

  The following methods need tests:
    parseXmlFromString

after preprocessing a file called xml.c containing:

  static $Parse parse$FromString_$Parse_StringBuffer
   (StringBuffer string)
   {return make$ParseFromString(0, string, strlen(string));
   }

with no test function marked with B<//TparseXmlFromString>.

=head2 Preprocessor commands

=head3 duplicate

The B<duplicate> command generates the previous function with the changes
indicated in the words following the command as in:

  static char * key_$Node                                                       // Get the key for a node
   (const $Node n)                                                              // Node
   {return n.key;
   }
  duplicate s/key/data/g

which adds the following code to the current output file:

  static char * data_$Node                                                      // Get the data for a node
   (const $Node n)                                                              // Node
   {return n.data;
   }

=head3 exports

The B<exports> command provides a name for or a collection of functions that
can be B<include>d in generated output files, for instance:

  exports aaa new$Node key_$Node

creates a new set of exports called B<aaa> which contains the two functions
mentioned. As these names have B<$> in them they will be expanded with the base
name of the file into which they are being copied.

=head3 include

The B<include> command copies the named function, structures, and exported
collections from the specified file into the current output file. For instance:

  include ../arenaTree.c :arena !key_$Node data_$Node

reads the relative file B<../arenaTree.c> and copies in all the structures
mentioned in collection B<arena> except for B<key_$node> as well as copying the
explicitly mentioned function B<data_$Node>.

=head2 Public Version Control on GitHub

Please submit changes as pull requests on
L<https://github.com/philiprbrenan/PreprocessOps>

=head1 Description

Preprocess ◁, ◀, ▷ and ▶ as operators in ANSI-C.


Version 20201117.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Preprocess

Preprocess ◁, ◀, ▷ and ▶ as operators in ANSI-C.

=head2 c($inputFile, $cFile, $hFile, $column)

Preprocess ▷ and ▶ as method dispatch operators in ANSI-C.

     Parameter   Description
  1  $inputFile  Input file
  2  $cFile      C output file
  3  $hFile      H output file
  4  $column     Optional start column for comments (80)

B<Example:>


  if (88) {
    my $d  = q(zzz);
    my $ds = fpd($d,  qw(source));
    my $dd = fpd($d,  qw(derived));

    my $sc = fpe($ds, qw(node c));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $dc = fpe($dd, qw(node c));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    my $dh = fpe($dd, qw(node h));

    owf($sc, <<END);
  #include <stdio.h>

  typedef struct Node                                                             // Node
   {const struct ProtoTypes_Node *proto;
    int data;
   } Node;

  #include "node.h"

  static Node by                                                                  // New from node * number
   (const Node * n,                                                               // Node
    const int    i)                                                               // Multiplier
   {return new Node(data: i * n->data);
   }

  static void dump                                                                // Dump a node to stdout
   (const Node * n)                                                               // Node to dump
   {printf("data=%d\
", n->data);
   }

  int main(void)                                                                  //TnewNode //Tdump //Tby
   {a ◁ new Node(data: 6);
    b ◁ a ▷ by(7);
    b ▷ dump;
    return 0;
   }
  END


    my $r = c($sc, $dc, $dh);                                                     # Preprocess source c to get derived c  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $c = qq((cd $dd; gcc node.c -o a; ./a));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply scalar(qx($c)), "data=42
";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    is_deeply readCFile($dc), <<'END';                                            # Generated base.c  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


  #line 1 "node.c"  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  #include <stdio.h>

  typedef struct Node                                                             // Node
   {const struct ProtoTypes_Node *proto;
    int data;
   } Node;

  #include "node.h"

  static Node by                                                                  // New from node * number
   (const Node * n,                                                               // Node
    const int    i)                                                               // Multiplier
   {return newNode(({struct Node t = {data: i * n->data, proto: &ProtoTypes_Node}; t;}));
   }

  static void dump                                                                // Dump a node to stdout
   (const Node * n)                                                               // Node to dump
   {printf("data=%d
", n->data);
   }

  int main(void)                                                                  //TnewNode //Tdump //Tby
   {const typeof(newNode(({struct Node t = {data: 6, proto: &ProtoTypes_Node}; t;}))) a = newNode(({struct Node t = {data: 6, proto: &ProtoTypes_Node}; t;}));
    const typeof(a.proto->by(&a, 7)) b = a.proto->by(&a, 7);
    b.proto->dump(&b);
    return 0;
   }
  END

    is_deeply readCFile($dh), <<END;                                              # Generated include file
  static Node by
   (const Node * n,
    const int    i);
  static void dump
   (const Node * n);
  int main(void);
  struct ProtoTypes_Node {
    Node  (*by)(                                                                  // New from node * number
      const Node * n,                                                             // Node
      const int i);                                                               // Multiplier
    void  (*dump)(                                                                // Dump a node to stdout
      const Node * n);                                                            // Node to dump
   } const ProtoTypes_Node =
  {by, dump};
  Node newNode(Node allocator) {return allocator;}
  END

    clearFolder($d, 10);
   }

  if (36) {
    my $d = q(zzz);

    my $c = owf(fpe($d, qw(source c)), <<'END');  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  #include <assert.h>
  #include <stdio.h>
  int main(void)
   {a ◁ ◉;
  a
    b
  ◉
    ✓ a[0] == 'a';
    printf("%s", a);
   }
  END

    my $h = fpe($d, qw(source  h));

    my $g = fpe($d, qw(derived c));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    my $r = c($c, $g, $h);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲



    is_deeply scalar(qx(cd $d; gcc derived.c -o a; ./a)), <<END, 'aaaa';  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  a
    b
  END

    is_deeply readCFile($g), <<'END', 'bbbb';

  #line 1 "source.c"  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  #include <assert.h>
  #include <stdio.h>
  int main(void)
   {const typeof("a
" "  b
") a = "a
" "  b
";
  //◉a
  //◉  b
  //◉
    assert( a[0] == 'a');
    printf("%s", a);
   }
  END
    clearFolder($d, 10);
   }

  if (26) {
    my $d = q(zzz);

    my $c = owf(fpe($d, qw(source c)), <<'END');  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  #include <assert.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  int main(void)
   {typedef struct
     {int a;
      int b;
     } S;
    S s; s.a = 1; s.b = 2;
    S t;
      t ≞ s;  ✓ s ≞≞ t;
      t ≞ 0;  ✓ s !≞ t;

    S *ps = &s, *pt = &t;

     t ≞ 0; ✓ t.a == 0;   ✓ t.b == 0;    ✓ pt !◧ s;
    pt ◧ s; ✓ t.a == s.a; ✓ t.b == s.b;  ✓ pt ◧◧ s;

     s ≞ 0; ✓ s.a == 0;   ✓ s.b == 0;    ✓ t !◨ ps;
    ps ◧ t; ✓ s.a == t.a; ✓ s.b == t.b;  ✓ t ◨◨ ps;
    printf(◉);
  success
  ◉
   }

  if (26) {
    my $d = q(zzz);

    my $c = owf(fpe($d, qw(source c)), <<'END');  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  #include <assert.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  int main(void)

   {  a ≋ 12; b ≋ 12; c ≋ 12;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

      a ≈ "aaaa";
      b ≈ "bbbb";

      A ◁ c +≈ a; B ◁ A +≈ b;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


      C ◁  c ∼ b;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    ✓ a ≈≈ "aaaa";
    ✓ b ≈≈ "bbbb";
    ✓ b !≈ a;

    ✓ c ≈≈ "aaaabbbb";  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

    ✓ A ≈≈ b;
    ✓ B ≈≈ "";
    ✓ C ≈≈ b;

    printf(◉);
  success
  ◉
   }
  END

    my $h = fpe($d, qw(source  h));

    my $g = fpe($d, qw(derived c));  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    my $r = c($c, $g, $h);  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲


    is_deeply scalar(qx(cd $d; gcc -g -Wall derived.c -o a; ./a)), <<END;  # 𝗘𝘅𝗮𝗺𝗽𝗹𝗲

  success
  END
  # clearFolder($d, 10);
   }



=head2 PreprocessOpsMap Definition


Methods and structures in the C file being preprocessed




=head3 Output fields


=head4 methods

Methods.

=head4 structures

Structure definitions.



=head2 PreprocessOpsParse Definition


Structure of the C program being preprocessed




=head3 Output fields


=head4 methods

Methods.

=head4 structureParameters

Structures used as parameters

=head4 structures

Structure definitions.

=head4 testsFound

Tests found

=head4 testsNeeded

Tests still needed



=head2 PreprocessOpsStruct Definition


Structure declaration




=head3 Output fields


=head4 comment

Comment for structure

=head4 flags

Flags for structure

=head4 methods

Methods.

=head4 name

Name of structure

=head4 structureParameters

Structures used as parameters

=head4 structures

Structure definitions.

=head4 testsFound

Tests found

=head4 testsNeeded

Tests still needed



=head1 Private Methods

=head2 trimComment($s)

Remove trailing white space and comment

     Parameter  Description
  1  $s         String

=head2 method($line)

Check whether a line of C code defines a method, returning (return, name, flags, comment) if it is, else ()

     Parameter  Description
  1  $line      Line of C code

=head2 structure($line)

Check whether a line of C code defines a structure, returning (name, flags, comment) if it is, else ()

     Parameter  Description
  1  $line      Line of C code

=head2 mapCode($file)

Find the structures and methods defined in a file

     Parameter  Description
  1  $file      Input file

=head2 printData($lineNumber, $line)

Print statement

     Parameter    Description
  1  $lineNumber  Code line number
  2  $line        Code line

=head2 duplicateFunction($lineNumber, $inputFile, $code)

Duplicate the previous function with the specified changes applied

     Parameter    Description
  1  $lineNumber  Line number of line being expanded
  2  $inputFile   File containing line being expanded
  3  $code        Lines of code

=head2 includeFile($lineNumber, $inputFile, $cFile, $hFile, $code)

Expand include files so that we can pull in code and structures from other files in the includes folder.

     Parameter    Description
  1  $lineNumber  Line number of line being expanded
  2  $inputFile   File containing line being expanded
  3  $cFile       Output C file
  4  $hFile       Output H file
  5  $code        Line of code


=head1 Index


1 L<c|/c> - Preprocess ▷ and ▶ as method dispatch operators in ANSI-C.

2 L<duplicateFunction|/duplicateFunction> - Duplicate the previous function with the specified changes applied

3 L<includeFile|/includeFile> - Expand include files so that we can pull in code and structures from other files in the includes folder.

4 L<mapCode|/mapCode> - Find the structures and methods defined in a file

5 L<method|/method> - Check whether a line of C code defines a method, returning (return, name, flags, comment) if it is, else ()

6 L<printData|/printData> - Print statement

7 L<structure|/structure> - Check whether a line of C code defines a structure, returning (name, flags, comment) if it is, else ()

8 L<trimComment|/trimComment> - Remove trailing white space and comment

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install Preprocess::Ops

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
require v5.26;
use Time::HiRes qw(time);
use Test::More;

my $hasgcc = confirmHasCommandLineCommand(q(gcc));

if ($^O =~ m(bsd|linux)i and $hasgcc)                                           # Only these operating systems are supported
 {plan tests => 7
 }
else
 {plan skip_all => 'Not supported'
 }

my $startTime = time();
my $localTest = ((caller(1))[0]//'Preprocess::Ops') eq "Preprocess::Ops";       # Local testing mode
Test::More->builder->output("/dev/null") if $localTest;                         # Suppress output in local testing mode

sub readCFile($)                                                                # Remove #line number statements from a C file
 {my ($file) = @_;                                                              # C file name
  my $c = readFile($file);
  $c =~ s(#line (\d+) ".*/(\w+).c") (#line $1 "$2.c");
  $c =~ s(\A\s+) ()s;
  $c =~ s(\s+\Z) ()s;
  "$c\n"
 }

if (88) {                                                                       #Tc
  my $d  = q(zzz);
  my $ds = fpd($d,  qw(source));
  my $dd = fpd($d,  qw(derived));
  my $sc = fpe($ds, qw(node c));
  my $dc = fpe($dd, qw(node c));
  my $dh = fpe($dd, qw(node h));

  owf($sc, <<END);
#include <stdio.h>

typedef struct Node                                                             // Node
 {const struct ProtoTypes_Node *proto;
  int data;
 } Node;

#include "node.h"

static Node by                                                                  // New from node * number
 (const Node * n,                                                               // Node
  const int    i)                                                               // Multiplier
 {return new Node(data: i * n->data);
 }

static void dump                                                                // Dump a node to stdout
 (const Node * n)                                                               // Node to dump
 {printf("data=%d\\n", n->data);
 }

int main(void)                                                                  //TnewNode //Tdump //Tby
 {a ◁ new Node(data: 6);
  b ◁ a ▷ by(7);
  b ▷ dump;
  return 0;
 }
END

  my $r = c($sc, $dc, $dh);                                                     # Preprocess source c to get derived c
  my $c = qq((cd $dd; gcc node.c -o a; ./a));
  is_deeply scalar(qx($c)), "data=42\n";

  is_deeply readCFile($dc), <<'END';                                            # Generated base.c
#line 1 "node.c"
#include <stdio.h>

typedef struct Node                                                             // Node
 {const struct ProtoTypes_Node *proto;
  int data;
 } Node;

#include "node.h"

static Node by                                                                  // New from node * number
 (const Node * n,                                                               // Node
  const int    i)                                                               // Multiplier
 {return newNode(({struct Node t = {data: i * n->data, proto: &ProtoTypes_Node}; t;}));
 }

static void dump                                                                // Dump a node to stdout
 (const Node * n)                                                               // Node to dump
 {printf("data=%d\n", n->data);
 }

int main(void)                                                                  //TnewNode //Tdump //Tby
 {const typeof(newNode(({struct Node t = {data: 6, proto: &ProtoTypes_Node}; t;}))) a = newNode(({struct Node t = {data: 6, proto: &ProtoTypes_Node}; t;}));
  const typeof(a.proto->by(&a, 7)) b = a.proto->by(&a, 7);
  b.proto->dump(&b);
  return 0;
 }
END

  is_deeply readCFile($dh), <<END;                                              # Generated include file
static Node by
 (const Node * n,
  const int    i);
static void dump
 (const Node * n);
int main(void);
struct ProtoTypes_Node {
  Node  (*by)(                                                                  // New from node * number
    const Node * n,                                                             // Node
    const int i);                                                               // Multiplier
  void  (*dump)(                                                                // Dump a node to stdout
    const Node * n);                                                            // Node to dump
 } const ProtoTypes_Node =
{by, dump};
Node newNode(Node allocator) {return allocator;}
END

  clearFolder($d, 10);
 }

if (36) {                                                                       #Tc
  my $d = q(zzz);
  my $c = owf(fpe($d, qw(source c)), <<'END');
#include <assert.h>
#include <stdio.h>
int main(void)
 {a ◁ ◉;
a
  b
◉
  ✓ a[0] == 'a';
  printf("%s", a);
 }
END

  my $h = fpe($d, qw(source  h));
  my $g = fpe($d, qw(derived c));

  my $r = c($c, $g, $h);

  is_deeply scalar(qx(cd $d; gcc derived.c -o a; ./a)), <<END, 'aaaa';
a
  b
END

  is_deeply readCFile($g), <<'END', 'bbbb';
#line 1 "source.c"
#include <assert.h>
#include <stdio.h>
int main(void)
 {const typeof("a\n" "  b\n") a = "a\n" "  b\n";
//◉a
//◉  b
//◉
  assert( a[0] == 'a');
  printf("%s", a);
 }
END
  clearFolder($d, 10);
 }

if (26) {                                                                       #Tc
  my $d = q(zzz);
  my $c = owf(fpe($d, qw(source c)), <<'END');
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(void)
 {typedef struct
   {int a;
    int b;
   } S;
  S s; s.a = 1; s.b = 2;
  S t;
    t ≞ s;  ✓ s ≞≞ t;
    t ≞ 0;  ✓ s !≞ t;

  S *ps = &s, *pt = &t;

   t ≞ 0; ✓ t.a == 0;   ✓ t.b == 0;    ✓ pt !◧ s;
  pt ◧ s; ✓ t.a == s.a; ✓ t.b == s.b;  ✓ pt ◧◧ s;

   s ≞ 0; ✓ s.a == 0;   ✓ s.b == 0;    ✓ t !◨ ps;
  ps ◧ t; ✓ s.a == t.a; ✓ s.b == t.b;  ✓ t ◨◨ ps;
  printf(◉);
success
◉
 }
END

  my $h = fpe($d, qw(source  h));
  my $g = fpe($d, qw(derived c));
  my $r = c($c, $g, $h);
  is_deeply scalar(qx(cd $d; gcc -g -Wall derived.c -o a; ./a)), <<END;
success
END
  clearFolder($d, 10);
 }

if (26) {                                                                       #Tc
  my $d = q(zzz);
  my $c = owf(fpe($d, qw(source c)), <<'END');
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(void)
 {  a ≋ 12; b ≋ 12; c ≋ 12;
    a ≈ "aaaa";
    b ≈ "bbbb";
    A ◁ c +≈ a; B ◁ A +≈ b;
    C ◁  c ∼ b;

  ✓ a ≈≈ "aaaa";
  ✓ b ≈≈ "bbbb";
  ✓ b !≈ a;
  ✓ c ≈≈ "aaaabbbb";
  ✓ A ≈≈ b;
  ✓ B ≈≈ "";
  ✓ C ≈≈ b;

  printf(◉);
success
◉
 }
END

  my $h = fpe($d, qw(source  h));
  my $g = fpe($d, qw(derived c));
  my $r = c($c, $g, $h);
  is_deeply scalar(qx(cd $d; gcc -g -Wall derived.c -o a; ./a)), <<END;
success
END
  clearFolder($d, 10);
 }

done_testing;
# owf($logC, readCFile($dc));
# owf($logH, readCFile($dh));

if ($localTest)
 {say "TO finished in ", (time() - $startTime), " seconds";
 }

1;
