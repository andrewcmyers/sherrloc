#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$ghchome="/home/zhdf/workspace/GHC-7.8.2-modified/ghc-7.8.2/bin/";
$ghc = $ghchome."ghc";
$opt="-ddump-tc-trace";

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs

# the following files with util files in reversion 3141
@progs = (
	"p1",
	"p2",
	"p3",
	"p4",
	"p5",
);

foreach (@progs) {
    `$ghc $opt $_.hs 2> $_.trace`;
    `perl translate.pl $_.trace`;
}

print "done!\n";
