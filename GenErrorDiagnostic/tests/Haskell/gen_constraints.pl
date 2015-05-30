#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$ghchome="../../../GHC-7.8.2-modified/ghc-7.8.2/inplace/bin/";
$ghc = $ghchome."ghc-stage1";
$opt="-ddump-tc-trace -fno-code -c";

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs

# the following files with util files in reversion 3141
@progs = (
	"family",
        "family1",
	"overlapping",
	"unsoundness",
	"polymorphism",
	"p4",
        "signature",
        "signature_poly",
        "Implication",
        "runningexample"
);

foreach (@progs) {
    `$ghc $opt $_.hs 2> $_.trace`;
    `perl translate.pl $_.trace`;
}

print "done!\n";
