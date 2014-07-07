#!/usr/bin/perl

print "Start generating separate files for all examples\n";

my $examples = "AllExamples.hs";	
my $classexamples = "Class.hs";	

sub separate_one_file {
	$filename = shift;
	$prefix = shift;
	my $count = 0;
	open (FILE, "<$filename") or die "$filename does not exist!";
	my $content="";
	while (<FILE>) {
		if (/-- Problem:/) {
			if ($count != 0) {
				open (OUT, ">$prefix$count.hs");
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
	open (OUT, ">$prefix$count.hs");
	print OUT "module Example where\n\n";
	print OUT "import Data.Char\n\n";
	print OUT $content;
	$count ++;
	close OUT;
}

separate_one_file ($examples, "p");
separate_one_file ($classexamples, "s");

print "done!\n";
