#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Set a switch in your script to zero after a run with the switch set to one.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc, 2016-2017
#-------------------------------------------------------------------------------

package Flip::Flop;
our $VERSION = 20181024;
use v5.8.0;
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Table::Text qw(:all);

my @flipflops;                                                                  # Flip::Flops encountered
my $startProcess = $$;                                                          # Starting process

sub AUTOLOAD(@)                                                                 # Any method will do
 {push @flipflops, $Flip::Flop::AUTOLOAD unless @_;                             # No parameters: flop switch, with parameters: flip switch to $[0]
  $_[0]
 }

END
 {if ($startProcess eq $$)                                                      # Reset the flip flops once in the starting process
   {#unless($?)                                                                 # Clean run? Not always effective.
     {my $S = my $s = readFile($0);                                             # Read source
      for my $program(@flipflops)                                               # Each flip flop
       {my $f = "$program(0)";                                                  # Regular expression to set the switch to zero
        if ($s !~ m/$f/s)                                                       # Reset the switch if it is not already zero
         {my $F = "$program\\(\\d+\\)";                                         # Regular expression to find the switches
          $s =~ s($F)($f)gs;                                                    # Reset switch
         }
       }
      overWriteFile($0, $s) if $s ne $S;                                        # Update source file if any switches were reset
     }
   }
 }

# podDocumentation

=pod

=encoding utf-8

=head1 Name

Flip::Flop - Set a switch in your script to zero after a run with the switch
set to one.

=head1 Synopsis

Some where near the top of your program you might have some variables
(illustrated below by B<uploadToCloud>) that select the actions the code is to
perform on the next run from your IDE:

 my $doUpload = Flip::Flop::uploadToCloud(1);

 ...

 if ($doUpload)
  {...
   Flip::Flop::uploadToCloud();
  }

If the upload succeeds, your program source code will be modified to read:

  my $doUpload = Flip::Flop::uploadToCloud(0);

so that the next time you run your program from your IDE this lengthy operation
will not be performed unless you explicitly re-request it.

If the run does not succeed the switch will be left unchanged. The switch will
only be reset if your program requests the reset.

You can have as many such switches as desired.

If your program L<forks|/fork>, then only the process in which Perl was started
will update the Flip::Flop switches.

This capability will only be useful to you if you are using an editor that
detects changes made independently to the file currently being edited. If you
do use such an editor this technique is surprisingly useful for simplifying,
standardizing, streamlining and supporting your edit/run cycle.

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read, use,
modify and install.

Standard Module::Build process for building and installing modules:

  sudo cpan install Flip::Flop

=head1 Author

L<philiprbrenan\@gmail.com|mailto:philiprbrenan\@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

=head1 Copyright

Copyright (c) 2016-2018 Philip R Brenan.

This module is free software. It may be used, redistributed and/or modified
under the same terms as Perl itself.

END

=cut

1;
# podDocumentation
