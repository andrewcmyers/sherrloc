#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$ghchome="../../../../GHC-7.8.2-modified/ghc-7.8.2/inplace/bin/";
$ghc = $ghchome."ghc-stage1";
$opt="-ddump-tc-trace -fno-code -c";
$haskell98 = "";

opendir(DIR, ".") or die $!;

while (my $file = readdir(DIR)) {
	# skip other files
	if ($file =~ /(.*)\.hs/) {
		`$ghc $opt $1.hs $haskell98 2> $1.trace`;
		`perl ../translate.pl $1.trace`;
	}
}

print "done!\n";
