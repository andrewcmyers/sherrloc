#!/usr/bin/perl

my $datafile = "data";
#system ("./collect_data.pl");

# read in group size
open DATA , "<" . $datafile         or die $!;
open OUT  , ">" . "size.dat" or die $!;
print "Generating data ...\n";

my $num_total=0;
my %freq = (1=>0,2=>0,3=>0,4=>0,5=>0,6=>0);

sub by_number {$a <=> $b};

while (<DATA>) {
    if (/^top_rank_size: /) { 
        @size= m/^top_rank_size: (.*)$/;
        if (exists $freq{@size[0]}) {
            $freq{@size[0]} ++;
        }
        else {
            $freq{@size[0]} = 1;
        }
        $num_total ++;
    }
} 

foreach $key (sort by_number keys %freq) {
    printf OUT ("%d %f\n", $key, ($freq{$key}/$num_total));
}

close OUT;

print "All data generated!\n";
# call gnuplot
# print "Generating graph.\n";
#system "gnuplot plotperformance.gnu";
#system "gnuplot plotsize.gnu";
