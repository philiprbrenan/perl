#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Layout data in a vec string via machine code instructions
# Philip R Brenan at gmail dot com, Appa Apps Ltd, 2017
#-------------------------------------------------------------------------------

package Data::Layout::Machine;

=pod

⨁     Add - replace the first register with the sum of all the named registers
=   Constant - load a constant into the named register
🡻       Decrement the contents of the named register if its contents are greater than zero
🡹       Increment the contents of the named register
?   If - execute code depending on the comparison of the contents of two registers, which in conjunction with Load and Store give us a Turing Machine
%   Module - replace the contents of the first register with the remainder when the contents of the first register are divided by the contents of the second register
🢀       Load some bits of memory into a register
🢂       Store the contents of a register into memory
𝗱      Dump the state of the machine
𝗽      Push a copy of all the registers on the registers stack
𝗿       Return - copy the named registers to the previous layer of registers and remove the current layer
𝘅      Execute a sequence of instructions
𝘄     While - loop while the contents of a register are not zero
𝘃      Vector - call action indexed by a register
𝘇      If zero - execute code depending on whether a register is zero or not

=cut
use v5.16.0;
use warnings FATAL => qw(all);
use strict;
use Data::Table::Text qw(:all);
use Data::Layout::BuddySystem;
use Carp;
use utf8;
use Data::Dump qw(dump);
our $VERSION = 2017.106;

if (0)                                                                          # Save to S3:- this will not work, unless you're me, or you happen, to know the key
 {my $z = 'DataLayoutMachine.zip';
  print for qx(zip $z $0 && aws s3 cp $z s3://AppaAppsSourceVersions/$z && rm $z);
 }

#1 Methods
sub new                                                                         # Create a new machine
 {return bless {allocations=>Data::Layout::BuddySystem->new};
 }

sub allocFree   :lvalue {my $m; $_[0]{allocFree}   //= $m}                      ## The allocation status of the memory of machine
sub allocations         {       $_[0]{allocations}}                             ## The layout of the memory used by the machine
sub memory      :lvalue {my $m; $_[0]{memory}      //= $m}                      ## The memory of the machine
sub registers   :lvalue {       $_[0]{registers}   //= [{}]}                    ## Registers stack created during the execution of code by the machine
sub steps       :lvalue {my $s; $_[0]{steps}       //= $s}                      ## The number of steps executed by the machine

sub exec($$@)                                                                   # Execute some code on the machine
 {my ($machine, $code, %options) = @_;                                          # Machine, array of instructions, execution options

  checkKeys(\%options, loadHashFromLines(<<END));                               # Check optional parameters
allocFree Check that memory is allocated on read/write
debug     Print each instruction before executing it
steps     The maximum number of steps to execute
END
  my $allocFree = $options{allocFree};
  my $debug     = $options{debug};
  my $maxSteps  = $options{steps};

  my @calls = ([$code, 0]);                                                     # Call stack

  my $operationCodes = {                                                        # Instruction definitions go here so we can can see the machine
qw(⨁)=>sub                                                                      # Add - replace the first register with the sum of all the named registers
 {my (@registers) = @_;                                                         # Registers to sum to create the content of the first register
  return unless @_ > 1;                                                         # No op and one op do nothing as there is nothing to add in these cases
  my $s = 0;
     $s += $machine->registers->[-1]{$_} for @_;
           $machine->registers->[-1]{$_[0]} = $s;
 },

qw(=)=>sub                                                                      # Constant - load a constant into the named register
 {my ($register, $constant) = @_;                                               # (register,constant)
  $machine->registers->[-1]{$register} = $constant;
 },

qw(🡻)=>sub                                                                      # Decrement the contents of the named register if its contents are greater than zero
 {my ($register) = @_;                                                          # Register whose contents are to be decremented
  my $n = $machine->registers->[-1]{$register};                                 # Current value of register
          $machine->registers->[-1]{$register} = $n - 1 if $n > 0               # Decrement the current value of the named register
 },

qw(🡹 )=>sub                                                                     # Increment the contents of the named register
 {my ($register) = @_;                                                          # Register whose contents are to be incremented
  $machine->registers->[-1]{$register}++;                                           # Increment the current value of the named register
 },

qw(?)=>sub                                                                      # If - execute code depending on the comparison of the contents of two registers, which in conjunction with Load and Store give us a Turing Machine
 {my ($register1, $register2, @code) = @_;                                      # Register1, Register2, one to three actions
  my $x = $machine->registers->[-1]{$register1};                                # Get index from register1
  my $y = $machine->registers->[-1]{$register2};                                # Get index from register2
  my $c = $x <=> $y;                                                            # Compare registers
  my $a = sub                                                                   # Action to take based on comparison result
   {my ($i) = @_;                                                               # Action to select
    ++$calls[-1][1];                                                            # Next instruction after 'if' once 'if' has been executed
    push @calls, [$code[$i], -1];                                               # Action to execute depending on register comparison
   };
  &$a(0) if $c == -1 and $code[0];                                              # Execute first action if first register is smaller than second register
  &$a(1) if $c ==  0 and $code[1];                                              # Execute second action if registers are equal
  &$a(2) if $c == +1 and $code[2];                                              # Execute third action if first register is greater than second register
 },

qw(%)=>sub                                                                      # Module - replace the contents of the first register with the remainder when the contents of the first register are divided by the contents of the second register
 {my ($register1, $register2) = @_;                                             # Register to be remaindered, register containing modulus
  $machine->registers->[-1]{$register1} %=                                      # Modulus
  $machine->registers->[-1]{$register2};
 },

qw(🢀)=>sub                                                                      # Load some bits of memory into a register
 {my ($register1, $register2, $width) = @_;                                     # Register to load, register containing location in memory, log2 width of memory area
  my $address = $machine->registers->[-1]{$register2};                          # Address to load from
  defined($address) or confess "No address contained in $register2";            # Check address
  $machine->allocatedMemoryCheck($address, $width, qw(read)) if $allocFree;     # Check allocation status of memory to be read if requested
  $machine->registers->[-1]{$register1} =                                       # Load specified data
    vec($machine->memory, $address>>$width, 1<<$width);                         # Position and length in vec format
 },

qw(🢂)=>sub                                                                      # Store the contents of a register into memory
 {my ($register1, $register2, $width) = @_;                                     # Register to save, register containing location in memory, log2 width of memory area
  my $save = $machine->registers->[-1]{$register1};                             # Data to save
  defined($save) or confess "Cannot save an undefined value into memory";       # Data must be defined
  my $address = $machine->registers->[-1]{$register2};                          # Address to save to
  defined($address) or confess "No address contained in $register2";            # Check address
  $machine->allocatedMemoryCheck($address, $width, qw(write)) if $allocFree;    # Check allocation status of memory to be written if requested
  vec($machine->memory, $address>>$width, 1<<$width) = $save;                   # Position and length in vec format
 },

qw(𝗮)=>sub                                                                      # Allocate memory
 {my ($register1, $register2) = @_;                                             # Register that will hold allocation address, register containing the log2 amount of memory required
  my $n = $machine->registers->[-1]{$register2};                                # Log2 allocation size
  my $a = $machine->allocations->alloc($n);                                     # Allocate requested amount
  $machine->allocatedMemoryMark($a, $n, 1) if $allocFree;                       # Debug memory use if requested
  $machine->registers->[-1]{$register1} = $a;                                   # Save allocation bit address in register
 },

qw(𝗱)=>sub                                                                      # Dump the state of the machine
 {my ($title) = @_;                                                             # Title of dump
  say STDERR "$title\n  ",  dump($machine),
   "\nMemory    :", $machine->memoryAsBitString,
   "\nAlloc Free:", $machine->allocFreeAsBitString;
 },

qw(𝗳)=>sub                                                                      # Free memory
 {my ($register) = @_;                                                          # Register holding address of allocation to be freed
  my $a = $machine->registers->[-1]{$register};                                 # Address of allocation to be freed
  defined($a) or confess "Invalid address to be freed in $register";            # Check we have an address
  my $n = $machine->allocations->sizeAddress($a);                               # Log2 allocation size of area to be freed
  defined($a) or confess "Invalid address to be freed: $a";                     # Check we have a valid address
  $machine->allocations->free($a);                                              # Free allocation at address
  $machine->allocatedMemoryMark($a, $n, 0) if $allocFree;                       # Debug memory use if requested
 },

qw(𝗽)=>sub                                                                      # Push a copy of all the registers on the registers stack
 {my %r = %{$machine->registers->[-1]};                                         # Make a copy of the current set of registers
  push @{$machine->registers}, \%r;                                             # Save the copy on the register stack
 },

qw(𝗿)=>sub                                                                      # Return - copy the named registers to the previous layer of registers and remove the current layer
 {my (@registers) = @_;                                                         # Registers to return to the previous layer
  my $r = $machine->registers;
  if ($r and @$r > 1)
   {for(@_)                                                                     # Return the named registers to thelayer below
     {$machine->registers->[-2]{$_} =  $machine->registers->[-1]{$_}
     }
    pop @$r                                                                     # Remove current layer of registers
   }
 },

qw(𝘅)=>sub                                                                      # Execute a sequence of instructions
 {my ($code) = @_;                                                              # Array of instructions to execute
  ++$calls[-1][1];                                                              # Next instruction beyond call to which we will return
  push @calls, [$code, -1]                                                      # New code to execute from the start
 },

qw(𝘄)=>sub                                                                      # While - loop while the contents of a register are not zero
 {my ($register, $code) = @_;                                                   # Register, code to execute while register contents are not zero
  my $while = $machine->registers->[-1]{$register};                             # Register contents
  push @calls, [[@$code, [qw(🡻), $register]], -1] if $while                     # Execute code if register is not zero and then decrement register
 },

qw(𝘃)=>sub                                                                      # Vector - call method indexed by a register
 {my ($register, @code) = @_;                                                   # Register, array of methods
  if (my $index = $machine->registers->[-1]{$register})                         # Get index from register
   {if (my $action = $code[$index-1])                                           # Choose method to be called
     {++$calls[-1][1];                                                          # Next instruction to which we will return after the called method has executed
      push @calls, [$code[$index-1], -1];                                       # Vector to code
     }
   }
 },

qw(𝘇)=>sub                                                                      # If zero - execute code depending on whether a register is zero or not
 {my ($register, @code) = @_;                                                   # Register, one or two actions
  my $x = $machine->registers->[-1]{$register};                                 # Get contents of register
  my $a = sub                                                                   # Action to take based on comparison result
   {my ($i) = @_;                                                               # Action to select
    ++$calls[-1][1];                                                            # Next instruction after 'if' once 'if' has been executed
    push @calls, [$code[$i], -1];                                               # Action to execute depending on register comparison
   };
  &$a(0) if !$x and $code[0];                                                   # Execute first action if register is zero
  &$a(1) if  $x and $code[1];                                                   # Execute second action if register is non zero
 },

};

  while(@calls)                                                                 # Execute the code
   {my $top = $calls[-1];                                                       # Action being executed is always at the top of the call stack
    my ($code, $position) = @$top;                                              # Action to execute, position in action
    if ($position >= @$code)                                                    # Fallen off the end of the current action
     {pop @calls;                                                               # Return to previous action
     }
    else
     {my $instruction = $code->[$position];                                     # Instruction
      my ($operationCode, @rest) = @$instruction;                               # (operation code, operands*) - next instruction to execute
      my $steps = ++$machine->steps;                                            # Number of instruction steps executed including no-ops
      return if $maxSteps and $steps > $maxSteps;                               # Stop if a maximum number of instructions to execute was supplied and we have exceeded this value
      next unless $operationCode;                                               # No operation specified
      my $sub = $operationCodes->{$operationCode};                              # Locate code to perform operation
      $sub or confess "Invalid instruction $operationCode";                     # Bad operation code
      say STDERR $steps, '  ' x scalar(@calls), $operationCode if $debug;       # Print debug data
      &$sub(@rest);                                                             # Execute method associated with operation code
      ++$calls[-1][1]                                                           # Next instruction
     }
   }
 }

sub get($$)                                                                     # Get the current contents of a register
 {my ($machine, $register) = @_;                                                # Machine, name of register
  $machine->registers->[-1]{$register}                                          # Register contents
 }

sub allocatedMemoryCheck($$$$)                                                  # Check that an area of memory has in fact been allocated
 {my ($machine, $address, $width, $action) = @_;                                # Machine, start bit address, number of bits of memory to check, action being performed so we can wrote a meaningful error message
  my $m = $machine->allocFree;                                                  # Memory allocation status of machine
  $m or confess "No memory allocated";                                          # Check that memory has been allocated
  for(0..1<<$width)                                                             # Each bit in the area of memory specified
   {my $a = $address + $_;
    vec($m, $a, 1) or confess "Attempt to $action unallocated memory at bit $a";   # Check that the bit is allocated
   }
 }

sub allocatedMemoryMark($$$$)                                                   # Mark an area of memory as being  allocated
 {my ($machine, $address, $width, $allocated) = @_;                             # Machine, start bit address, number of bits of memory to mark, allocated not free
  my $m = $machine->allocFree;                                                  # Memory allocation status of machine
  for(0..1<<$width)                                                             # Each bit in the area of memory specified
   {my $a = $address + $_;
    vec($m, $a, 1) = $allocated;                                                # Mark the bit as allocated or free
   }
  $machine->allocFree = $m;                                                     # Save memory allocation status of machine
 }

sub memoryAsBitString($)                                                        # Dump the memory of a machine as a string of bits with low bits left
 {my ($machine) = @_;                                                           # Machine
  my $m = $machine->memory;
  return '' unless $m;                                                          # No memory
  unpack("b*", $m)                                                              # Memory as bit string left to right / low to high
 }

sub allocFreeAsBitString($)                                                     # Dump the alloc free state of memory of a machine as a string of bits with low bits left
 {my ($machine) = @_;                                                           # Machine
  my $m = $machine->allocFree;
  return '' unless $m;                                                          # No memory
  unpack("b*", $m)                                                              # Memory as bit string left to right / low to high
 }

# Test
#sub test{eval join('', <Data::Layout::Machine::DATA>) or die $@}

#test unless caller;

# Documentation
#extractDocumentation() unless caller;                                          # Extract the documentation

1;

=encoding utf-8

=head1 Name

 Data::Layout::Machine - Execute data as machine code

=head1 Synopsis

=head1 Description

=head1 Installation

This module is written in 100% Pure Perl and is thus easy to read, modify and
install.

Standard Module::Build process for building and installing modules:

  perl Build.PL
  ./Build
  ./Build test
  ./Build install

=head1 Author

philiprbrenan@gmail.com

http://www.appaapps.com

=head1 Copyright

Copyright (c) 2017 Philip R Brenan.

This module is free software. It may be used, redistributed and/or modified
under the same terms as Perl itself.

=cut

__DATA__
use utf8;
use Test::More tests=>40;

if (1)                                                                          # 1 + 1 == 2
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= a 1)],                                                                    # a = 1
[qw(⨁ a a)],                                                                    # a = a + a
  ]);
  ok $m->get(qw(a)) == 2;                                                       # a == 2
  ok $m->steps == 2;                                                            # 2 steps in total
 }

if (1)                                                                          # Save/load
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= v 1)],                                                                    # v = 1
[qw(⨁ v v)],                                                                    # v = v + v
[qw(= a 0)],                                                                    # a = 0
[qw(🢂 v a 3)],                                                                  # (3,3)=v
[qw(🢀 r a 3)]]);                                                                # r=(3,3)
  ok $m->get(qw(r)) == 2;                                                       # r == 2
  ok $m->steps == 5;                                                            # 5 steps
 }

if (1)                                                                          # Call a procedure
 {my $m = Data::Layout::Machine::new();
  my $dv = [[qw(⨁ v v)]];                                                       # Double the contents of register v
  $m->exec([
[qw(= v 1)],                                                                    # v = 1
[𝘅=>$dv],                                                                       # Execute double register v
]);                                                                             # v == r=(3,3)
  ok $m->get(qw(v)) == 2;
  ok $m->steps == 3;                                                            # Execution took 6 steps, 1 step more because of the execute instruction
 }

if (1)                                                                          # While
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= i 10)],                                                                   # $i  = 10
[qw(= n  0)],                                                                   # $n  =  0
[qw(𝘄 i), [[qw(⨁ n i)]]],                                                       # $n +=  i while $i
]);
  ok $m->get(qw(i)) ==  0;
  ok $m->get(qw(n)) == 55;
  ok $m->steps      == 33;                                                      # Execution took 6 steps, 1 step more because of the execute instruction
 }

if (1)                                                                          # If - sum of even numbers from 1 to 10
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= i 12)],                                                                   # $i = 10
[qw(= n  0)],                                                                   # $n =  0
[qw(𝘄 i),                                                                       # while $i
 [
  [qw(= j 0)],                                                                  #   $j  = 0
  [qw(⨁ j i)],                                                                  #   $j += $i => $j == $i
  [qw(= 0 0)],                                                                  #   $z  = 0
  [qw(= 2 2)],                                                                  #   $2 = 2
  [qw(% j 2)],                                                                  #   $j %= $2 => $j == $i % 2

  [qw(? j 0), undef,                                                            # do nothing if $i % 2 != 0
    [[qw(⨁ n i)]],                                                              # $n += $i   if $i % 2 == 0  => n sums the even numbers
  ]
 ]
]]);
  ok $m->steps      == 105;
  ok $m->get(qw(0)) ==   0;                                                     # 0
  ok $m->get(qw(2)) ==   2;                                                     # 2
  ok $m->get(qw(j)) ==   1;                                                     # Last value of 'i' test is 1
  ok $m->get(qw(n)) ==  42;                                                     # 2+4+6+8+10+12 == 30
 }

if (1)                                                                          # Vector
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= x 2)],                                                                    # x = 2
[qw(𝘃 x), [[qw(= x 11)]], [[qw(= x 22)]], [[qw(= x 33)]]]]);                    # x = 22 as we vectored to action 2
  ok $m->steps      ==  3;
  ok $m->get(qw(x)) == 22;
 }

if (1)                                                                          # If zero
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= x 0)],
[qw(𝘇 x), [[qw(= x 1)]], [[qw(= x 2)]]],
[qw(𝘇 x), [[qw(= y 1)]], [[qw(= y 2)]]]], allocFree=>1);
  ok $m->steps      == 5;
  ok $m->get(qw(x)) == 1;
  ok $m->get(qw(y)) == 2;
 }

if (1)                                                                          # Push/return
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= x 1)],
[qw(= y 1)],
[qw(= z 1)],
[qw(𝗽)],
[qw(= x 2)],
[qw(= y 2)],
[qw(= z 2)],
[qw(𝗽)],
[qw(= x 3)],
[qw(= y 3)],
[qw(= z 3)],
[qw(𝗿      z)],
[qw(𝗿  y z)],
], allocFree=>1);
  ok $m->steps == 13;
  ok $m->get(qw(x)) == 1;
  ok $m->get(qw(y)) == 2;
  ok $m->get(qw(z)) == 3;
 }

if (1)                                                                          # Alloc
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= x 0)],
[qw(𝗮 𝘅 x)],
[qw(= 𝕩 1)],
[qw(🢂 𝕩 𝘅 0)],

[qw(= y 1)],
[qw(𝗮 𝘆 y)],
[qw(= 𝕪 3)],
[qw(🢂 𝕪 𝘆 1)],

[qw(= z 2)],
[qw(𝗮 𝘇 z)],
[qw(= 𝕫 7)],
[qw(🢂 𝕫 𝘇 2)],
], allocFree=>1);
  ok $m->steps      == 12;
  ok $m->get(qw(𝘅)) == 0;
  ok $m->get(qw(𝘆)) == 2;
  ok $m->get(qw(𝘇)) == 4;
  ok $m->memoryAsBitString eq '10111110';
 }

if (1)                                                                          # Debug memory - read from unallocated memory
 {my $m = Data::Layout::Machine::new();
  eval {$m->exec([
[qw(= x 1)],
[qw(🢀 a x 1)],
], allocFree=>1)};
  ok $@ =~ /No memory allocated/i;
  ok $m->steps == 2;
 }

if (1)                                                                          # Debug memory - write to unallocated memory
 {my $m = Data::Layout::Machine::new();
  eval {$m->exec([
[qw(= x 1)],
[qw(🢂 x x 1)],
], allocFree=>1)};
  ok $@ =~ /No memory allocated/i;
  ok $m->steps      == 2;
 }

if (1)                                                                          # Debug memory - use allocated memory
 {my $m = Data::Layout::Machine::new();
  $m->exec([
[qw(= x 1)],
[qw(𝗮 𝘅 x)],
[qw(🢂 x 𝘅 1)],
], allocFree=>1);
  ok $m->steps      == 3;
  ok $m->memoryAsBitString eq '10000000';
 }

if (1)                                                                          # Debug memory - read from freed memory
 {my $m = Data::Layout::Machine::new();
  eval {$m->exec([
[qw(= a 1)],                                                                    # Allocation a
[qw(𝗮 𝗮 a)],
[qw(🢂 a 𝗮  1)],

[qw(= b 2)],                                                                    # Allocation b
[qw(𝗮 𝗯 b)],
[qw(🢂 b 𝗯  2)],
[qw(𝗳 𝗮)],                                                                      # Free allocation a
[qw(= z 0)],
[qw(🢀 z 𝗮  1)],                                                                 # Write to freed memory
], allocFree=>1)};

  ok $@ =~ /Attempt to read unallocated memory at bit 0/;
  ok $m->steps      == 9;
  ok $m->memoryAsBitString eq '10000100';
 }

if (1)                                                                          # Debug memory - write to freed memory
 {my $m = Data::Layout::Machine::new();
  eval {$m->exec([
[qw(= a 1)],                                                                    # Allocation a
[qw(𝗮 𝗮 a)],
[qw(🢂 a 𝗮  1)],

[qw(= b 2)],                                                                    # Allocation b
[qw(𝗮 𝗯 b)],
[qw(🢂 b 𝗯  2)],
[qw(𝗳 𝗮)],                                                                      # Free allocation a
[qw(= z 0)],
[qw(🢂 z 𝗮  1)],                                                                 # Write to freed memory
], allocFree=>1)};
  ok $@ =~ /Attempt to write unallocated memory at bit 0/;
  ok $m->steps      == 9;
  ok $m->memoryAsBitString eq '10000100';
 }

sub dm
 {my ($m) = @_;
  say STDERR $m->memoryAsBitString;
 }

1;
