#!/usr/bin/perl

my $datafile = "data";
#system ("./collect_data.pl");

# read in group size
open DATA , "<" . $datafile         or die $!;
open OUT  , ">" . "runtime.dat" or die $!;
print "Generating data ...\n";

my $pnum_total=0;
my $rnum_total=0;
my $p_max=0.0;
my $r_max=0.0;
my %freq_p = (1=>0,2=>0,3=>0,4=>0,5=>0,6=>0);
my %freq_r = (1=>0,2=>0,3=>0,4=>0,5=>0,6=>0);

sub by_number {$a <=> $b};

sub by_range {
  if ($_[1]<0.01) {$_[0]{1} ++;}
  elsif ($_[1]<0.1) {$_[0]{2} ++;}
  elsif ($_[1]<1) {$_[0]{3} ++;}
  elsif ($_[1]<10) {$_[0]{4} ++;}
  elsif ($_[1]<100) {$_[0]{5} ++;}
  elsif ($_[1]<1000) {$_[0]{6} ++;}
}

while (<DATA>) {
#    if (/^graph_size: /) { 
#        @size= m/^graph_size: (.*)$/;
#        printf OUT ("\n%d\t", @size);
#    }
    if (/^LOC: /) {
        @size= m/^LOC: (.*)$/;
        printf OUT ("\n%d\t", @size);
    }
    if (/^path_finding time: /) { 
        @ptime= m/^path_finding time: (.*)$/;
        by_range(\%freq_p,@ptime[0]/1000.0);
        $pnum_total ++;
        if ($pmax < @ptime[0]/1000.0) {
            $pmax = @ptime[0]/1000.0;
        }
        printf OUT ("%f\t", @ptime[0]/1000.0);
    }
    if (/^expansion_time: /) { 
         @etime= m/^expansion_time: (.*)$/;
         printf OUT ("%f\t", @etime[0]/1000.0);
    }

    if (/^ranking_time: /) { 
        @rtime= m/^ranking_time: (.*)$/;
        by_range(\%freq_r,@rtime[0]/1000.0);
        $rnum_total ++;
        if ($rmax < @rtime[0]/1000.0) {
            $rmax = @rtime[0]/1000.0;
        }
        printf OUT ("%f", @rtime[0]/1000.0);
    }
} 

foreach $key (sort by_number keys %freq_p) {
    printf ("%d %f\n", $key, ($freq_p{$key}/$pnum_total));
}

foreach $key (sort by_number keys %freq_r) {
    printf ("%d %f\n", $key, ($freq_r{$key}/$rnum_total));
}

print ("Max path building time: ".$pmax."\n");
print ("Max ranking time: ".$rmax."\n");

close OUT;

print "All data generated!\n";
# call gnuplot
print "Generating graph.\n";
system "gnuplot runtime.plot";
