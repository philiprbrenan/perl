#!/usr/bin/perl
#-------------------------------------------------------------------------------
# DejaVu Sans font as a data uri.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc, 2016-2018
#-------------------------------------------------------------------------------
# podDocumentation
# Get DejaVu fonts from GitHub as sfd file.  Convert to woff using font forge.

package DejaVu::Sans;
our $VERSION = 20190317;
use v5.20;
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess);
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);

#D1 Construction                                                                # Create the DejaVu font as a data url usable by @fontface in a web browser

sub woff                                                                        #IS Return DejaVu font as a data url usable by @fontface in a web browser
 {my ($fileNameOrString, @options) = @_;                                        # Optional file name or string from which to construct the parse tree, hash of other options.
 }

sub create                                                                      #P The name of the tag to be used to represent text - this tag must not also be used as a command tag otherwise the parser will L<confess>.
 {my $s = readBinaryFile(q(/home/phil/Downloads/DejaVuSans.woff));

  owf(q(/home/phil/z/z/z/out.xml), encodeBase64($s));
 }

# podDocumentation
=pod

=encoding utf-8

=head1 Name

Data::Edit::Xml - Edit data held in the XML format.

=head1 Synopsis

=head1 Description

Edit data held in the XML format.


Version 20190317.


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.



=head1 Immediately useful methods

These methods are the ones most likely to be of immediate use to anyone using
this module for the first time:


L<woff|/woff>

Return DejaVu font as a data url usable by @fontface in a web browser




=head1 Construction

Create the DejaVu font as a data url usable by @fontface in a web browser

=head2 woff()

Return DejaVu font as a data url usable by @fontface in a web browser


This is a static method and so should be invoked as:

  DejaVu::Sans::woff



=head1 Private Methods

=head2 create()

The name of the tag to be used to represent text - this tag must not also be used as a command tag otherwise the parser will L<confess|http://perldoc.perl.org/Carp.html#SYNOPSIS/>.



=head1 Index


1 L<create|/create> - The name of the tag to be used to represent text - this tag must not also be used as a command tag otherwise the parser will L<confess|http://perldoc.perl.org/Carp.html#SYNOPSIS/>.

2 L<woff|/woff> - Return DejaVu font as a data url usable by @fontface in a web browser

=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install DejaVu::Sans

=head1 Author

L<philiprbrenan@gmail.com|mailto:philiprbrenan@gmail.com>

L<http://www.appaapps.com|http://www.appaapps.com>

=head1 Copyright

Copyright (c) 2016-2018 Philip R Brenan.

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
use Test::More tests=>1;
use warnings FATAL=>qw(all);
use strict;
use Data::Table::Text qw(:all);

Test::More->builder->output("/dev/null")                                        # Show only errors during testing
  if ((caller(1))[0]//'DejaVu::Sans') eq "DejaVu::Sans";

create;

1
