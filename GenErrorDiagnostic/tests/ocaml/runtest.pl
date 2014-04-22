#!/usr/bin/perl

use Cwd;
my $dirname = getcwd;
my $ezyocaml = "$dirname/../../../easyocaml-modified/ocaml-3.10.2/bin/";
my $diagnostic = "$dirname/../../sherrloc";
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
    cleanup();
    return @locs;
  }
  elsif ((scalar @locs) == 0) {
    print "parse error";
    print $str;
    cleanup();
    exit 0;
  }
  return @locs;
}

my $top_rank_size;
my $total_size = 0;

# read the diagnostic location results
sub diagnoseLocations {
  $mlfile = shift;
  `$ezyocaml/ecamlc $mlfile >/dev/null 2>&1`;
  my $str = `$diagnostic -e -v error.con 2>&1`;
  unlink ("error.con");
  for(split /^/, $str) {
    if (/^top_rank_size:/) {
      ($top_rank_size) = $_ =~ m/^top_rank_size: (.+)/;
      $total_size += $top_rank_size;
    }
  }
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

my $succ_counter = 0;
my $fail_counter = 0;

sub print_ok {
  $succ_counter ++;
  print colored("ok", 'green'), "\n";
}

sub print_fail {
  $fail_counter ++;
  print colored("fail ($fail_counter)", 'red'), "\n";
  cleanup();
}

my $prefix;

sub cleanup () {
  unlink ($prefix.".cmo");
}

# open my $fh, '<', $Name or die "file not found!";
my $prettylen = 27;
my @files = <*>;

# remove cmo files, in case the last run fails
#foreach my $file (@files) {
#    if ($file =~ /.cmo$/) {
#       unlink ($file);
#    }
#}

# clean up all cmo files.
if (($#ARGV == 0) && ($ARGV[0] eq "clean")) {
  foreach my $dir (@files) {
    if (-d $dir) {
      opendir my $dh, $dir or die "$0: opendir: $!";
      while (defined(my $file = readdir $dh)) {
        if ($file =~ /\.cmo$/) {
          unlink ($dir."/".$file);
        }
      }
    }
  }
  exit 0;
}

open OUT, ">$outfile" or die "oped failed : $outfile\n";
OUT->autoflush(1);

foreach my $dir (@files) {
  if (-d $dir) {
    opendir my $dh, $dir or die "$0: opendir: $!";
    while (defined(my $file = readdir $dh)) {
      $file = $dir."/".$file;
      if ($file =~ /\.ml$/) {
        # ignore tested files
        ($prefix) = $file =~ m/(.+)\.ml$/;
        print (substr $file, 0, $prettylen);
        print ": ";
        if (-e ($prefix.".cmo")) {
          print_ok();
        }
        else{
          my $cause = causeLocations($file);
          my @loc1 = parse ($cause);
          my $toolret = diagnoseLocations($file);
          my @loc2 = parse ($toolret);
          my $succ = 0;
L1:       for my $loc1 (@loc1) {
            for my $loc2 (@loc2) {
              if ($loc1->contains ($loc2)) {
                print "$top_rank_size "; 
                print_ok();
                $succ = 1;
                last L1;
              }
            }
          }
          if ($succ == 0) {
            # remove cmo files
            print_fail();
          }
          else {
		print OUT $file."\n";
                print OUT $toolret;
          }
        }
      }
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

print OUT (($succ_counter+$fail_counter) . " programs evaluated. " . ($fail_counter) . " of them fails.\n");
print OUT "Average top rank size is: " . ($total_size/($succ_counter+$fail_counter)) . "\n";
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
