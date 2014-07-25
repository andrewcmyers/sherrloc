#!/usr/bin/perl

use Cwd;
my $dirname = getcwd;
my $ghc_modified = "$dirname/../../../../GHC-7.8.2-modified/ghc-7.8.2/inplace/bin";
my $ghcopt = "-fno-code";
my $dumpopt = "-ddump-tc-trace";
my $ghc = "ghc";
my $translator = "$dirname/../translate.pl";
my $diagnostic = "$dirname/../../../sherrloc";
my $sherrlocopt = "-c -v";
my $mlfile;
my $outfile = "data";

use strict;
use warnings;
use POSIX qw(strftime);

use CGI qw(:standard);
use Term::ANSIColor;

package Location;

sub new {
  my $type = shift;
  my $self = {
    ln_st => shift,
    col_st => shift,
    ln_ed => shift,
    col_ed => shift
  };
  bless $self, $type;
  return $self;
}

sub print {
  my ($self) = @_;
  if ($self->{ln_st} == $self->{ln_ed}) {
    printf( "Location: %d,%d-%d\n", $self->{ln_st}, $self->{col_st}, $self->{col_ed} );
  }
  else {
    printf( "Location: %d,%d-%d,%d\n", $self->{ln_st}, $self->{col_st}, $self->{ln_ed}, $self->{col_ed} );
  }
}

sub contains {
  my ($self, $other) = @_;
  return (($self->{ln_st} < $other->{ln_st} || 
		  ($self->{ln_st} == $other->{ln_st} && $self->{col_st} <= $other->{col_st})) 
       && ($other->{ln_ed} < $self->{ln_ed} || 
	          ($other->{ln_ed} == $self->{ln_ed} && $other->{col_ed} <= $self->{col_ed})));
}

#and here is the test
package main;

sub parse {
  my @locs = ();
  my $str = shift;
  while ( $str =~ m/(\d+),(\d+)-(\d+),(\d+)/g ) {
    my $loc = new Location($1, $2, $3, $4);
    push(@locs, $loc);
  }
  while ( $str =~ m/(\d+),(\d+)-(\d+)/g ) {
    my $loc = new Location($1, $2, $1, $3);
    push(@locs, $loc);
  }
  if ( $str =~ /java.lang.OutOfMemoryError/ ) {
    return @locs;
  }
  elsif ((scalar @locs) == 0) {
    print "parse error";
    print $str;
    exit 0;
  }
  return @locs;
}

my $top_rank_size;
my %total_size;
$total_size{'SHErrLoc'} = 0;
$total_size{'GHC'} = 0;

# read the diagnostic location results
sub diagnoseLocations {
  $mlfile = shift;
  # avoid to generate a trace if it's already there
  my ($prefix) = $mlfile =~ m/(.+)\.hs$/;
  if (-e ($prefix.".con")) {
  }
  else {
    `$ghc_modified/ghc-stage1 $dumpopt $ghcopt $mlfile >/dev/null 2>$prefix.trace`;
    `perl $translator $prefix.trace`;
  }
  my $str = `$diagnostic $sherrlocopt $prefix.con 2>&1`;
  for(split /^/, $str) {
    if (/^top_rank_size:/) {
      ($top_rank_size) = $_ =~ m/^top_rank_size: (.+)/;
      $total_size{'SHErrLoc'} += $top_rank_size;
    }
  }
  return $str; 
}

# read the location results from unmodified GHC
sub ghcLocations {
  $mlfile = shift;
  my ($prefix) = $mlfile =~ m/(.+)\.hs$/;
  `$ghc $ghcopt $mlfile >/dev/null 2>$prefix.ghc`;
  open GHC_REPORT, '<', "$prefix.ghc" or die "file not found!";
  my $str = "";
  my ($line, $column);
  for(<GHC_REPORT>) {
    if (/^$prefix.hs:(\d+):(\d+):/) {
        ($line, $column) = ($1, $2);
        $str .= "$line,$column-$column ";
    }
  }
  $total_size{'GHC'} += 1;
  return $str; 
}

# read the manually labeled cause location
sub causeLocations {
  my $mlfile = shift;
  open my $fh, '<', $mlfile or die "file not found!";
  my $last_line;
  while(<$fh>) {
    $last_line = $_ if eof;
  }
  close $fh;
  return $last_line; 
}

my %succ_counter;
my %fail_counter;
$succ_counter{'SHErrLoc'} = 0;
$succ_counter{'GHC'} = 0;
$fail_counter{'SHErrLoc'} = 0;
$fail_counter{'GHC'} = 0;

sub print_ok {
  my $name = shift;
  my $size = shift;
  $succ_counter{$name} ++;
  print colored("\tok size=$size", 'green');
}

sub print_fail {
  my $name = shift;
  $fail_counter{$name} ++;
  print colored("\tfail ($fail_counter{$name})", 'red');
}

sub print_safe {
  print colored("\ttype safe", 'yellow'), "\n";
}

sub print_parsing {
  print colored("\tparsing error", 'yellow'), "\n";
}


sub print_unsound {
  print colored("\tSHErrLoc is unsound", 'yellow'), "\n";
}

# open my $fh, '<', $Name or die "file not found!";
my $prettylen = 27;
my $topdir = "fp0203";
my @groups= <$topdir/*>;

# remove cmo files, in case the last run fails
#foreach my $file (@files) {
#    if ($file =~ /.cmo$/) {
#       unlink ($file);
#    }
#}

# clean up all cmo files.
if (($#ARGV == 0) && ($ARGV[0] eq "clean")) {
  foreach my $file (@groups) {
      if ($file =~ /\.trace$/ or $file =~ /\.con$/) {
          unlink ($file);
      }
  }
  exit 0;
}

open OUT, ">$outfile" or die "oped failed : $outfile\n";
OUT->autoflush(1);

my $filename_length = 10; # for pretty print
# iterate to the deepest level of directory

foreach my $group (@groups) {
  if (-d $group) {
      next if $group eq "." or $group eq "..";
      my @subdirs = <$group/*>;
      foreach my $subdir (@subdirs) {
        chdir $subdir;
        my @files = <*>;
        my $file;
        foreach my $subfile (@files) {
            if ($subfile =~ /(.*)\.out$/) {
                $file = $1;
                last;
            }
        }
        open IN, "<$file";
        my $noOra = 0;
        while (<IN>) {
                if (/No oracle given./) {
                        $noOra = 1;
                }
        }
        close IN;
        # skip the files without oracle 
        if ($noOra) {
                next;
        }
        my ($prefix) = $file =~ m/(.+)\.hs$/;
        print (substr $file, 0, $prettylen);
        print ":";
        print ' ' x ($filename_length-length($file));
        my $cause = causeLocations($file);
        if ($cause =~ m/Type safe in Haskell/) {
	    print_safe();
	    next;
 	}
        if ($cause =~ m/Parsing error/) {
	    print_parsing();
	    next;
 	}
        my @loc1 = parse ($cause);
        
        # test the correctness of SHErrLoc 
        my $toolret = diagnoseLocations($file);
	if ($toolret =~ m/No errors were found/) {
	    print_unsound();
	    next;
	}
        my @loc2 = parse ($toolret);
        my $succ = 0;
L1:     for my $loc1 (@loc1) {
          for my $loc2 (@loc2) {
            if ($loc1->contains ($loc2)) {
              print_ok('SHErrLoc', $top_rank_size);
              $succ = 1;
              last L1;
            }
          }
        }
        if ($succ == 0) {
          # remove cmo files
          print_fail('SHErrLoc');
        }
        else {
	  print OUT $file."\n";
          print OUT $toolret;
  	} 

        # test the correctness of GHC
        my $ghcret = ghcLocations($file);
        my @loc3 = parse ($ghcret);
        $succ = 0;
L1:     for my $loc1 (@loc1) {
          for my $loc3 (@loc3) {
            if ($loc1->contains ($loc3)) {
              print_ok('GHC', 1); # GHC only return one suggestion per error
              $succ = 1;
              last L1;
            }
          }
        }
        if ($succ == 0) {
          # remove cmo files
          print_fail('GHC');
        }
        print "\n";
        chdir "../../..";
    }
  }
}

# remove cmo files
#@files = <*>;
#foreach my $file (@files) {
#    if ($file =~ /\.cmo$/) {
#        unlink ($file);
#    }
#}

for my $name ('SHErrLoc', 'GHC') {
    print OUT "$name:\n";
    print OUT (($succ_counter{$name}+$fail_counter{$name}) . " programs evaluated. " . ($fail_counter{$name}) . " of them fails.\n");
    print OUT "Average top rank size is: " . ($total_size{$name}/($succ_counter{$name}+$fail_counter{$name})) . "\n";
}
close OUT;

=comment
my @loc1 = parse ("(* 9,1-12 5,14-18 *)");
my @loc2 = parse ("(* 5,15-18 *)");

for my $loc1 (@loc1) {
  for my $loc2 (@loc2) {
    if ($loc1->contains ($loc2)) {
      print "correct\n";
    }
  }
}

print TEMP "<H4> OCaml error message:</H4><br>";

my $result =  `$ezyocaml/ocamlc $mlfile 2>&1`;
print TEMP $result;

open (DIAG_RESULT, "<error.html");
while (<DIAG_RESULT>) {
  print TEMP;
} 

print TEMP end_html();
close (TEMP);
close (DIAG_RESULT);
=cut
