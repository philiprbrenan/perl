#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib  -I/home/phil/perl/cpan/JavaDoc/lib -I/home/phil/perl/cpan/DitaPCD/lib/ -I/home/phil/perl/cpan/DataEditXml/lib/ -I/home/phil/perl/cpan/GitHubCrud/lib/  -I/home/phil/perl/cpan/DataDFA/lib/ -I/home/phil/perl/cpan/DataNFA/lib/ -I//home/phil/perl/cpan/PreprocessOps/lib/
#-------------------------------------------------------------------------------
# Make with Perl
# Philip R Brenan at gmail dot com, Appa Apps Ltd, 2017
#-------------------------------------------------------------------------------
use v5.26;
package MakeWithPerl;
our $VERSION = "20210601";
use warnings FATAL => qw(all);
use strict;
use Carp qw(confess);
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use Getopt::Long;
use Time::HiRes qw(time);
use utf8;

sub mwpl {qq(makeWithPerlLocally.pl)}                                           # Make with Perl locally

my $javaHome;                                                                   # Location of java files
my $cIncludes;                                                                  # C includes folder
my $compile;                                                                    # Compile
my $coverage;                                                                   # Get coverage of code
my $doc;                                                                        # Documentation
my $gccVersion;                                                                 # Alternate version of gcc is set.  Example: --gccVersion gcc-10
my $htmlToPdf;                                                                  # Convert html to pdf
my $run;                                                                        # Run
my $search;                                                                     # Search for a local file to make the specified file
my $showHtml;                                                                   # Show html in opera
my $upload;                                                                     # Upload files
my $valgrind;                                                                   # Check C memory usage
my $xmlCatalog;                                                                 # Verify xml

sub makeWithPerl {                                                              # Make a file
GetOptions(
  'cIncludes=s' =>\$cIncludes,
  'compile'     =>\$compile,
  'coverage'    =>\$coverage,
  'doc'         =>\$doc,
  'gccVersion=s'=>\$gccVersion,
  'htmlToPdf'   =>\$htmlToPdf,
  'javaHome=s'  =>\$javaHome,
  'run'         =>\$run,
  'search!'     =>\$search,
  'showHtml!'   =>\$showHtml,
  'valgrind'    =>\$valgrind,
  'upload'      =>\$upload,
  'xmlCatalog=s'=>\$xmlCatalog,
 );

my $file = shift @ARGV // $0;                                                   # File to process

$cIncludes //= "-I/home/phil/c/ -I.";                                           # Includes used in C files if not already set

unless($file)                                                                   # Confirm we have a file
 {confess "Use %f to specify the file to process";
 }

if (! -e $file)                                                                 # No such file
 {confess "No such file:\n$file"
 }

if ($search)                                                                    # Upload files to GitHub or run some other action defined in the containing folder hierarchy unless search is forbidden
 {my @d = split m{/}, $file;                                                    # Split file name
  pop @d;
  while(@d)                                                                     # Look for a folder that contains a push command
   {for my $n(qw(pushToGitHub upload package))
     {my $u = "/".fpe(@d, $n, q(pl));
      if (-e $u)
       {say STDERR $u;
        qx(perl $u);
        exit;
       }
     }
    pop @d;
   }
  confess "Unable to find pushToGitHub in folders down to $file";
 }

if ($doc)                                                                       # Documentation
 {if ($file =~ m((pl|pm)\Z)s)                                                   # Document perl
   {say STDERR "Document perl $file";
    updatePerlModuleDocumentation($file);
   }
  elsif ($file =~ m((java)\Z)s)                                                 # Document java
   {say STDERR "Document java $file";

    my %files;
    for(findFiles($javaHome))
     {next if m/Test\.java\Z/ or m(/java/z/);                                   # Exclude test files and /java/ sub folders
      $files{$_}++ if /\.java\Z/
     }
    confess;
    #my $j = Java::Doc::new;
    #$j->source = [sort keys %files];
    #$j->target = my $f = filePathExt($javaHome, qw(documentation html));
    #$j->indent = 20;
    #$j->colors = [map {"#$_"} qw(ccFFFF FFccFF FFFFcc CCccFF FFCCcc ccFFCC)];
    #$j->html;  69.221.225.129
   }
  else
   {confess "Unable to document file $file";
   }
  exit
 }

if (-e mwpl and $run)                                                           # Make with Perl locally
 {my $p = join ' ', @ARGV;
  my $c = mwpl;
  print STDERR qx(perl -CSDA $c $p);
  exit;
 }

if ($file =~ m(/bt/files/booktrolls/.*\.(p[lm]|cgi)\Z))                         # Perl on booktrolls
 {my $b  = q(phil@booktrolls.com);
  my $f1 = q(/home/phil/zzz1.txt);
  my $f2 = q(/home/phil/zzz2.txt);
  my $p  = $file =~ s(/bt/files/booktrolls/) (/booktrolls/)sr;
  my $o  = q(-I/home/phil/booktrolls/lib/);
  my $k  = qq(1>$f1 2>$f2);

  if ($compile)                                                                 # Syntax check perl
   {my $c = qq(ssh -4 $b 'perl $o -cw $p $k');
    say STDERR qq($c);
    say STDERR qx($c);
   }
  else                                                                          # Run perl
   {my $c = qq(ssh -4 $b 'perl $o     $p $k');
    say STDERR qq($c);
    say STDERR qx($c);
   }
# print STDERR qx(rsync $b:$f1 $f1; rsync $b:$f2 $f2; cat $f1 $f2);
  if (1)
   {my $c = qq(rsync $b:/home/phil/zzz[12].txt /home/phil/ && cat /home/phil/zzz[12].txt);
    say STDERR qx($c);
   }
  exit;
 }

if ($file =~ m(\.(p[lm]|cgi)\Z))                                                # Perl
 {if ($compile)                                                                 # Syntax check perl
   {print STDERR qx(perl -CSDA -cw "$file");
   }
  elsif ($run)                                                                  # Run perl
   {if ($file =~ m(.cgi\Z)s)                                                    # Run from web server
     {&cgiPerl($file);
     }
    else                                                                        # Run from command line
     {say STDERR qq(perl -CSDA -w  "$file");
      print STDERR qx(perl -CSDA -w  "$file");
     }
   }
  elsif ($doc)                                                                  # Document perl
   {say STDERR "Document perl $file";
    updatePerlModuleDocumentation($file);
   }
  exit;
 }

if ($file =~ m(\.(txt|htm)\Z))                                                  # Html
 {my $s = expandWellKnownUrlsInHtmlFormat
          expandWellKnownWordsAsUrlsInHtmlFormat
          join "", includeFiles $file;                                          # Expand any include files
  my $o = setFileExtension $file, q(html);                                      # Output file
  my $f = owf $o, $s;

  if ($htmlToPdf)                                                               # Convert html to pdf if requested
   {my $p = setFileExtension($file, q(pdf));
    say STDERR qx(wkhtmltopdf  --enable-local-file-access $f $p);
   }
  elsif ($showHtml//1)                                                          # Show html in opera
   {my $c = qq(timeout 3m opera $o);
    say STDERR qq($c);
    say STDERR qx($c);
   }
  exit;
 }

if ($file =~ m(\.(dita|ditamap|xml)\Z))                                         # Process xml
 {my $source = readFile($file);
  my $C = $xmlCatalog;
  my $c = qq(xmllint --noent --noout "$file" && echo "Parses OK!" && export XML_CATALOG_FILES=$C && xmllint --noent --noout --valid - < "$file" && echo Valid);
  say STDERR $c;
  say STDERR qx($c);
  exit;
 }

if ($file =~ m(\.asm\Z))                                                        # Process assembler
 {my $o = setFileExtension $file, q(o);
  my $e = setFileExtension $file;
  my $l = setFileExtension $file, q(txt);
  my $c = qq(nasm -f elf64 -g -l $l -o $o $file);
  if ($compile)
   {say STDERR $c;
    say STDERR qx($c; cat $l);
   }
  else
   {$c = "$c; ld -o $e $o; $e";
    say STDERR $c;
    say STDERR qx($c);
   }
  exit;
 }

sub profile($)                                                                  # Add profile lines
 {my ($file) = @_;                                                              # Parameters
  my @lines  = readFile($file);                                                 # Source file
  my $ext    = fe $file;                                                        # File extension shows type
  my $re;
     $re = qr(final static int..lined = null;) if $ext =~ m(java);              # Profile requested from Java
     $re = qr(int lined\[LINES\] = \{\};)      if $ext =~ m(c);                 # Profile requested from C

  my $p = 0;
  for my $i(keys @lines)                                                        # Check for profile if supplied
   {$p = $i if $lines[$i] =~ m($re);
   }

  if ($p)                                                                       # Profile requested
   {my @lined;
    for my $l(keys @lines)                                                      # Profiler
     {my $l1 = $l + 1;
      if ($lines[$l] =~ m(\A//p))                                               # Capture profile statistics
       {  $lines[$l] = "line[$l1]++;\n";
        push @lined, $l1;
       }
     }
    my $lined = join '', '{', join(', ', @lined), '}';                          # Missed lines
    if ($ext =~ m(java)i)
     {$lines[$p] =~ s(null) ($lined);
     }
    else
     {$lines[$p] =~ s(\{\}) ($lined);
     }
    return owf(fpe(fpn($file)."2", fe($file)), join '', @lines);                # Similar file name
   }
  $file
 }

sub runTests($$)                                                                # Run some tests embedded in a source file
 {my ($file, $command) = @_;                                                    # Source file, compile && run command
  my  @tests = split m(\n(?=//test\s+))i, readFile($file);                      # Split on tests
  shift @tests;

  if (@tests > 0)                                                               # Tests present
   {my $passed = 0; my $failed = 0;
    my ($compile, $run) = split /&&/, $command, 2;                              # Commands to compile and execute java
    print qx($compile);

    my $start = time();

    for my $testi(keys @tests)
     {my $test  = $tests[$testi];
      my $testI = $testi+1;
      my $line  = $test =~ s(\n.*) ()isr;
      my $title = $line =~ s(//TEST\s+) ()isr =~ s(\-{2,}\w+) ()gsr;

      if ($line =~ m(--skip)i)
       {say STDERR sprintf "%4d Skip requested               %s", $testI, $title;
        next;
       }

      my ($input, @expected) = split /\n\-{4,}\n/, $test;                       # Each test can possibly have multiple answers

      $expected[-1] =~ s/\s*\*\//\n/s;
      for my $i(keys @expected)
       {$expected[$i] =~ s(\A\s+) ()s;
        $expected[$i] =~ s(\s+\Z) (\n)s;
       }
      $input        =~ s(\A.*?/\*\n) ()s;

      my $in    = writeTempFile("$input\n");
      my $out   = temporaryFile;

      my $start = time;
      print STDERR qx($run $line --expected="$expected[0]" <$in >$out);         # Run program
      if ($? != 0)
       {my $c = $? >> 8;
        lll "Exiting on non zero return code $c";
        exit $c;
       }
      my $time  = time - $start;
      my $got   = readFile($out);
      unlink $in, $out;

      my $matches = 0;                                                          # Number of tests matched
      for my $expected(@expected)
       {if ($got eq $expected)                                                  # Evaluate test
         {++$passed;
          say STDERR sprintf "%4d passed in %8.4f seconds ++ %s", $testI, $time, $title;
          ++$matches;
          last;
         }
       }
      if ($matches == 0)                                                        # Nothing matched
       {for my $expected(@expected)
         {say STDERR "FAILED:\n$got\nVersus:\n$expected"; ++$failed;
          #say STDERR "In  : ", dump($input);
          say STDERR "Got : ", dump($got);
          say STDERR "Want: ", dump($expected);
         #last unless $line =~ m(--continue)i;
         }
        say STDERR sprintf "%4d failed in %8.4f seconds -- %s", $testI, $time, $title;
       }

      if ($line =~ m(--stop)i)
       {say STDERR sprintf "%4d Stop requested               %s", $testI, $title;
        return;
       }
     }
    if ($failed)
     {say STDERR sprintf "%4d FAILED, %4d passed in %8.4f seconds", $failed, $passed, time - $start;
     }
    else
     {say STDERR sprintf "ALL %4d  passed in %8.4f seconds",                 $passed, time - $start;
     }
   }
  else
   {print STDERR qx($command);
   }
 }

if ($file =~ m(\.cp*\Z))                                                        # GCC
 {my $mix = "sde-mix-out.txt";                                                  # Mix performance file produced by Intel emulator
  unlink $mix;
  my $cp = join ' ', map {split /\s+/} grep {!/\A#/} split /\n/, <<END;         # Compiler options
-fopenmp
-finput-charset=UTF-8 -fmax-errors=7 -rdynamic
-Wall -Wextra -Wno-unused-function
$cIncludes
-I.
END
#-O3
  my $source   = readFile($file);                                               # Check source for specific capabilities needed
  my $avx512   = $source =~ m'//sde';                                           # Avx512 instructions
  my $valgrind = $source =~ m(//valgrind)i;                                     # Request Valgrind
  my $optimize = $source =~ m(//optimize)i;                                     # Request optimization
  $cp .= " -mavx512f" if $avx512;
  $cp .= $optimize ? " -O3 " : " -O0 -g3 -rdynamic ";

# -pg for gprof executable gmon.out
  my $gcc = $gccVersion // 'gcc';                                               # Gcc version 10
  if ($compile)
   {my $cmd = qq($gcc $cp -c "$file" -o /dev/null);                             # Syntax check
    say STDERR $cmd;
    print STDERR $_ for qx($cmd);
   }
  else
   {my $e = $file =~ s(\.cp?p?\Z) ()gsr;                                        # Execute
    my $o = fpe($e, q(o));                                                      # Object file
    unlink $e, $o;
    my $E = $avx512 ? "sde -mix -- $e" : $e;

    my $f = profile($file);
    my $l = q(-lm);

    my  $c = $valgrind ?                                                        # Compile and run
        qq($gcc $cp -o "$e" "$f" && valgrind --leak-check=full --leak-resolution=high --show-leak-kinds=definite  --track-origins=yes $E 2>&1)
       :qq($gcc $cp -o "$e" "$f" $l && timeout 100s $E);
    say STDERR $c;
    runTests($file, $c);
    unlink $o;

    if (-e $mix)
     {my $s = readFile($mix);
      if ($s =~ m(\*total\s+(\d+)))
       {say '// ', numberWithCommas($1)." instructions executed";
       }
     }
   }
  exit;
 }

if ($file =~ m(\.rkt\Z))                                                        # Racket
 {my $c = qq(racket -f "$file");
  say STDERR $c;
  print STDERR qx($c);
  say STDERR q();
  exit;
 }

if ($file =~ m(\.js\Z))                                                         # Javascript
 {if ($compile)
   {say STDERR "Compile javascript $file";
    print STDERR qx(nodejs -c "$file");                                         # Syntax check javascript
   }
  else
   {my $c = qq(nodejs  --max_old_space_size=4096  "$file");                     # Run javascript
    say STDERR $c;
    print STDERR qx($c);
    say STDERR q();
   }
  exit;
 }

if ($file =~ m(\.sh\Z))                                                         # Bash script
 {if ($compile)
   {say STDERR "Test bash $file";
    print STDERR qx(bash -x "$file");                                           # Debug bash
   }
  else
   {print STDERR qx(bash "$file");                                              # Bash
   }
  exit;
 }

if ($file =~ m(\.adblog\Z))                                                     # Android log
 {my $adb = q(/home/phil/android/sdk/platform-tools/adb);
  my $c = qq($adb -e logcat "*:W" -d > $file && $adb -e logcat -c);
  say STDERR "Android log\n$c";
  print STDERR qx($c);
  exit;
 }

if ($file =~ m(\.java\Z))                                                       # Java
 {my  $name   = fn $file;                                                       # Parse file name
  !$javaHome and confess "Specify --javaHome keyword to specify the folder where class files are to go.";
  my $package = &getPackageNameFromFile($file);                                 # Get package name
  my $cp      = fpd($javaHome, qw(Classes));                                    # Folder containing java classes
  if ($compile)                                                                 # Compile
   {my $c = "javac -g -d $cp -cp $cp -Xlint -Xdiags:verbose $file -Xmaxerrs 9";# Syntax check Java
    say STDERR $c;
    print STDERR qx($c);
   }
  else                                                                          # Compile and run
   {my $class = $package ? "$package.$name" : $name;                            # Class location
    my $p = join ' ', @ARGV;                                                    # Collect the remaining parameters and pass them to the java application
    my $f = profile($file);
#   my $c = "javac -g -d $cp -cp $cp $file && java -ea -cp $cp $class $p";      # Run java
    my $c = "javac -g -d $cp -cp $cp $f && java -ea -cp $cp $class $p";         # Run java
    say STDERR $c;

    runTests($file, $c);
#   unlink $f;
   }
  &removeClasses;
  exit;
 }

if ($file =~ m(\.py\Z))                                                         # Python
 {if ($compile)                                                                 # Syntax check
   {print STDERR qx(python3 -m py_compile "$file");
   }
  elsif ($run)                                                                  # Run
   {print STDERR qx(python3 "$file");
   }
  elsif ($doc)                                                                  # Document
   {say STDERR "Document perl $file";
    updatePerlModuleDocumentation($file);
   }
  exit;
 }

if ($file =~ m(\.(vala)\Z))                                                     # Vala
 {my $lib = "--pkg gtk+-3.0";                                                   # Libraries
   if ($compile)                                                                # Syntax check
   {print STDERR qx(valac -c "$file" $lib);
   }
  elsif ($run)                                                                  # Run
   {print STDERR qx(vala "$file" $lib);
   }
  elsif ($doc)                                                                  # Document
   {say STDERR "Document perl $file";
    updatePerlModuleDocumentation($file);
   }
  exit;
 }

sub removeClasses
 {unlink for fileList("*.class")
 }

sub getPackageNameFromFile($)                                                   # Get package name from java file
 {my ($file) = @_;                                                              # File to read
  my $s = readFile($file);
  my ($p) = $s =~ m/package\s+(\S+)\s*;/;
  $p
 }

sub cgiPerl($)                                                                  # Run perl on web server
 {my ($file) = @_;                                                              # File to read

  my $r = qx(perl -CSDA -cw "$file" 2>&1);
  if ($r !~ m(syntax OK))
   {say STDERR $r;
   }
  else
   {my $base = fne $file;
    my $target = fpf(q(/usr/lib/cgi-bin), $base);
    lll qx(echo 121212 | sudo -S cp $file $target);
    lll qx(echo 121212 | sudo chmod ugo+rx $target);
    lll qx(opera http://localhost/cgi-bin/$base &);
   }
 }
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

MakeWithPerl - Make with Perl

=head1 Synopsis

Integrated development environment for Geany or similar editor for compiling
running and documenting programs written in a number of languages.

=head2 Installation:

  sudo cpan install MakeWithPerl

=head2 Operation

Configure Geany as described at
L<README.md|https://github.com/philiprbrenan/MakeWithPerl>.

=head1 Description

Make with Perl


Version "20210533".


The following sections describe the methods in each functional area of this
module.  For an alphabetic listing of all methods by name see L<Index|/Index>.




=head1 Index


=head1 Installation

This module is written in 100% Pure Perl and, thus, it is easy to read,
comprehend, use, modify and install via B<cpan>:

  sudo cpan install MakeWithPerl

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
__DATA__
use Test::Most;
use Time::HiRes qw(time);

bail_on_fail;

my $develop   = -e q(/home/phil/);                                              # Developing
my $localTest = ((caller(1))[0]//'MakeWithPerl') eq "MakeWithPerl";             # Local testing mode

Test::More->builder->output("/dev/null") if $localTest;                         # Reduce number of confirmation messages during testing

if ($^O =~ m(bsd|linux|darwin)i and $^V and $^V ge v5.26)                       # Supported systems
 {plan tests => 1;
 }
else
 {plan skip_all => qq(Not supported on: $^O);
 }

my $f = owf("zzz.pl", <<END);
#!/usr/bin/perl
say STDOUT 'Hello World';
END
my $c = qq($^X -Ilib -M"MakeWithPerl" -e"MakeWithPerl::makeWithPerl" -- --run $f 2>&1);
my $r = qx($c);
unlink $f;

ok $r =~ m(Hello World);

1;
