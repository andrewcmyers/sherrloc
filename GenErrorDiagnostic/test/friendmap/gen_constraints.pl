#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$fmhome="/home/zhdf/APL/Fabric/examples/friendmap/classes";
$cp = ".:$fmhome/fabric";
$dist_dir = $ARGV[0];

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs

# the following files with util files in reversion 3122
@names3122 = (
	#"FriendMap3103",
	#"FriendMap3104",
	#"FriendMap3105",
	#"FriendMap3107",
	#"FriendMap3108",
	#"FriendMap3109",
	#"FriendMap3110",
	#"FriendMap3111",
	#"FriendMap3112",
	#"FriendMap3113",
	#"FriendMap3114",
	#"FriendMap3115",
	#"FriendMap3116",
	"FriendMap3117",
	#"FriendMap3120",
	#"FriendMap3122",
);

@names3141 = (
	#"FriendMap3141",
	#"FriendMap3142",
);

@names3143 = (
	#"FriendMap3143",
	#"FriendMap3144",

);

@names3151 = (
	#"FriendMap3151",
	#"FriendMap3167",
	#"FriendMap3192",
	#"FriendMap3193",
	#"FriendMap3194",

);

if (-e "friendmap") { system ("rm -fr friendmap")}
if (-e "mapserv") { system ("rm -fr mapserv")}
if (-e "snapp") { system ("rm -fr snapp")}

# first, get a snapshot of FriendMap application at reversion 3122
#system ("./compile_fm.sh", "3141");

foreach (@names3122) {
    system ("fabc -diagnostic -nonrobust -trusted-providers -fail-on-exception -addsigcp $fmhome/fab-sig -addbootcp $fmhome/fab-sig-impl -classpath ".$cp." ".$_.".fab");
    for ($i=1; $i<50; $i++) {
        $name = "error".$i.".con";
        if (-e $name) {
            system ("mv $name $dist_dir".$_."_$i.con");
        }
    }
}

print "done!\n"
