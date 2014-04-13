#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$civihome="/home/zhdf/APL/Jif/civitas";

#$cp = ".:$civihome/sig-classes:$civihome/classes";
$cp = ".:$civihome/lib/jife.jar:$civihome/lib/jifelib.jar:$civihome/lib/jifert.jar:$civihome/lib/jifesig.jar";
$dist_dir = "constraints/";

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs

# the following files with util files in reversion 3141
@names = (
	"CapabilityMix",
);

#if (-e "snapp") { system ("rm -fr snapp")}

# first, compile the Civitas project
system ("./compile_civitas.sh");

foreach (@names) {
    system ("jifc -diagnostic -noserial -nonrobust -trusted-providers -fail-on-exception -sourcepath $civihome/jif-src:$civihome/sig-src -classpath ".$cp." source/".$_.".jif");
    for ($i=1; $i<50; $i++) {
        $name = "error".$i.".con";
        if (-e $name) {
            system ("mv $name $dist_dir".$_."_$i.con");
        }
    }
}

print "done!\n"
