#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$ghchome="../../../../../ghc-modified/ghc-7.8.2/inplace/bin/";
$ghc = $ghchome."ghc-stage1";
$opt="-ddump-tc-trace -fno-code -c";

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs

opendir(DIR, ".") or die $!;

while (my $file = readdir(DIR)) {
    if ($file =~ /(.*).hs/) {
        print $1."\n";
        `$ghc $opt $1.hs 2> $1.trace`;
        `perl ../../translate.pl $1.trace`;
    }
}

print "done!\n";
