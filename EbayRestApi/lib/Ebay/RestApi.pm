#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Make calls to the Ebay REST API.
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc, 2022
#-------------------------------------------------------------------------------
package Ebay::RestApi;
use v5.26;
our $VERSION = 20220505;                                                        # Version
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess);
use Data::Table::Text qw(:all);
use Data::Dump qw(dump);
use utf8;

#D1 Oauth2 token                                                                # Get an OAuth2 token which which to make calls to the api

sub new(%)                                                                      # New Ebay API Requestor
 {my (%options) = @_;                                                           # Api Options
  genHash(__PACKAGE__,
    authorization => $options{authorization},                                   # The shared secret used to communicate with ebay
    token         => $options{token        },                                   # The token returned by Ebay that grants access to the API.
    useSandBox    => $options{sandBox      },                                   # Use the sand box if true
   )
 }

sub getToken()                                                                  # Get a token from Ebay
 {my ($e) = @_;                                                                 # Ebay api requestors
  my $a = $e->authorization;
  my $c = <<"END" =~ s(\n) ( )gsr;
curl -sS -X POST 'https://api.sandbox.ebay.com/identity/v1/oauth2/token'
  -H 'Cache-Control: no-cache',
  -H 'Accept: application/json',
  -H 'Content-Type: application/x-www-form-urlencoded'
  -H 'Authorization: Basic $a'
  -d 'grant_type=client_credentials&scope=https%3A%2F%2Fapi.ebay.com%2Foauth%2Fapi_scope https:%3A%2F%2api.ebay.com%2oauth%2api_scope%2sell.account'
END
  my @c = qx($c);
  say STDERR join "\n", @c;
 }

#-------------------------------------------------------------------------------
# Export - eeee
#-------------------------------------------------------------------------------

use Exporter qw(import);

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA          = qw(Exporter);
@EXPORT       = qw();
@EXPORT_OK    = qw();
%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

#D0
# podDocumentation

=pod

=encoding utf-8

=head1 Name

Ebay::RestApi - Make calls to the Ebay REST API.


=head1 Description

Make calls to the Ebay REST API.

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
use Test::More;

my $timeStart = time;

eval {goto latest};

sub sandBox                                                                     # Create a sandbox ebay descriptor
 {new authorization => "BookTrol-BTIntern-SBX-90f1ff760-63ad8e12",
      useSandBox    => 1,
 }

say STDERR dump sandBox->getToken;

done_testing;                                                                   # Finished
if (((caller(1))[0]//'Ebay::RestApi') eq "Ebay::RestApi")
 {say STDERR "ERA finished in ", (time() - $timeStart), " seconds";
 }

1
