#!/usr/bin/perl

print "Start generating separate files for all examples\n";

my $examples = "Examples.hs";	

open (FILE, "<$examples") or die "$examples does not exist!";

my $count = 0;
my $content="";
while (<FILE>) {
	if (/-- Problem:/) {
		if ($count != 0) {
			open (OUT, ">p$count.hs");
			print OUT "module Example where\n\n";
			print OUT "import Data.Char\n\n";
			print OUT $content;
			close OUT;
			$content = "";
			$count ++;
		}
		else {
			$content = ""; 
			$count ++;
		}
	}	
	$content .= $_;
}

open (OUT, ">p$count.hs");
print OUT "module Example where\n\n";
print OUT "import Data.Char\n\n";
print OUT $content;
close OUT;

print "done!\n";
