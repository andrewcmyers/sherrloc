#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$fmhome="/home/zhdf/APL/Fabric/examples/friendmap/classes";
$fabrichome="/home/zhdf/APL/Fabric/";

$cp = ".:$fmhome/fabric";
$dist_dir = "$ARGV[0]/constraints";

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs

# the following files with util files in reversion 3141
@names3141 = (
	"FriendMap3103",
	"FriendMap3104",
	"FriendMap3105",
	"FriendMap3107",
	"FriendMap3108",
	"FriendMap3109",
	"FriendMap3110",
	"FriendMap3111",
	"FriendMap3112",
	"FriendMap3113",
	"FriendMap3114",
	"FriendMap3115",
	"FriendMap3116",
	"FriendMap3117",
	"FriendMap3120",
	"FriendMap3122",
	"FriendMap3141",
	"FriendMap3142",
);

@names3143 = (
	"FriendMap3143",
	"FriendMap3144",
);

@names3193 = (
	"FriendMap3151",
	"FriendMap3167",
	"FriendMap3192",
	"FriendMap3193",

);

if (-e "friendmap") { system ("rm -fr friendmap")}
if (-e "mapserv") { system ("rm -fr mapserv")}
if (-e "snapp") { system ("rm -fr snapp")}

# first, get a snapshot of FriendMap application at reversion 3141
system ("./compile_fm.sh", "3193");

foreach (@names3193) {
    system ("fabc -diagnostic -e -nonrobust -trusted-providers -fail-on-exception -addsigcp $fmhome/fab-sig -addbootcp $fmhome/fab-sig-impl -classpath ".$cp." source/".$_.".fab");
    for ($i=1; $i<50; $i++) {
        $name = "error".$i.".con";
        if (-e $name) {
            system ("mv $name $dist_dir".$_."_$i.con");
        }
    }
}

print "done!\n"
