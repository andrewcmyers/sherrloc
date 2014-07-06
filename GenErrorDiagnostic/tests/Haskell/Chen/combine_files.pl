#!/usr/bin/perl

print "Start combining (annotated) separate files for all examples\n";

my $examples = "AllExamples.hs";	

open (FILE, ">$examples") or die "$examples does not exist!";

my $count = 1;
while ($count <= 121) {
        open (OUT, "<p$count.hs") or die "p$count.hs does not exist!";

	while (<OUT>) {
		$line = $_;
		if ($line !~ "module Example where" and $line !~ "import Data.Char") {
			print FILE $line;
		}
                else {
                        # eat the empty line
                        $line = <OUT>;
                }
	}	
	close OUT;
	$count ++;
}

close FILE;

print "done!\n";
