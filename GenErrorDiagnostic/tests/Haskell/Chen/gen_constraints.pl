#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$ghchome="/home/zhdf/workspace/GHC-7.8.2-modified/ghc-7.8.2/bin/";
$ghc = $ghchome."ghc";
$opt="-ddump-tc-trace -c -fno-code";

opendir(DIR, ".") or die $!;

while (my $file = readdir(DIR)) {
	# skip other files
	if ($file =~ /p(.*)\.hs/) {
		`$ghc $opt p$1.hs 2> p$1.trace`;
		`perl ../translate.pl p$1.trace`;
	}
}

print "done!\n";
