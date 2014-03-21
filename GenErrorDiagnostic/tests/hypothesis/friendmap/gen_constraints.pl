#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$fmhome="/home/zhdf/APL/Fabric/examples/friendmap/classes";
$cp = ".:$fmhome/fabric";
$dist_dir = $ARGV[0];

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs
@names = (
	"Location1",
	"Snapp1",
	"Snapp2",
	"FriendMap1",
	"FriendMap2",
	"FriendMap3",
	"FriendMap4",
	"FriendMap5",
	"FriendMap6",
	"FriendMap7" ,
	"FriendMap8" ,
	"FriendMap9" ,
	"FriendMap10" ,
	"FriendMap11" ,
	"FriendMap12" ,
	"FriendMap13" ,
	"FriendMap14" ,
	"FriendMap15" ,
	"FriendMap16" ,
	"FriendMap17" ,
	"Box1", 
	"Box2",
	"Box3",
	"Box4",
	"MapImage1" ,
	"MapImage2" ,
	"MapImage3" ,
	"MapImage4" ,
	"MapImage5" ,	
	"MapImage6" ,	
	"MapServer1" ,	
);

if (-e "friendmap") { system ("rm -fr friendmap")}
if (-e "mapserv") { system ("rm -fr mapserv")}
if (-e "snapp") { system ("rm -fr snapp")}

foreach (@names) {
    system ("fabc -diagnostic -nonrobust -trusted-providers -fail-on-exception -addsigcp $fmhome/fab-sig -addbootcp $fmhome/fab-sig-impl -classpath ".$cp." ".$_.".fab");
    for ($i=1; $i<50; $i++) {
        $name = "error".$i.".con";
        if (-e $name) {
            system ("mv ".$name." ".$dist_dir.$_.".con");
        }
    }
}

print "done!\n"
