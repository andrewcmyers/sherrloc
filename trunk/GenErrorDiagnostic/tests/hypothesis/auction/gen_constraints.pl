#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$cp = ".";
$dist_dir = $ARGV[0];

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs
@names = (
	"AirlineAgent1",
	"AirlineAgent2",
	"User1",
	"AirlineExample1",
	"AirlineExample2",
	"AirlineExample3",
);

# clean
system ("rm *.java *.class");

system ("jifc AirlineServer.jif IAirlineServer.jif Offer.jif UserChoice.jif");

foreach (@names) {
    system ("jifc -diagnostic -nonrobust -fail-on-exception -cp ".$cp." ".$_.".jif");
    for ($i=1; $i<50; $i++) {
        $name = "error".$i.".con";
        if (-e $name) {
            system ("mv ".$name." ".$dist_dir.$_.".con");
        }
    }
}

print "done!\n"
