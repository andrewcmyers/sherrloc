#!/usr/bin/perl

print "Start generating constraints for all examples\n";

$jpmailhome="/home/zhdf/APL/Jif/jif/examples/jpmail-full-0.3.0/";
$cp = ".:$jpmailhome/classes:$jpmailhome/jifcrypto/jars/bcprov-jdk14-131.jar";
$opt = "-diagnostic -addsigcp $jpmailhome/sig-classes -classpath $cp";
$dist_dir = "constraints/";

# the map from file name to generated graph name
# this is required since one file may generate 
# multiple graphs

# the following files with util files in reversion 3141
@names = (
	#"AESClosure",
	#"DESClosure",
	#"RSAClosure",
	#"MD5Closure",
	#"TripleDESClosure",
	#"RSA", # secure after removing all downgrades
	
	"MailReaderCrypto",
	#"DeclassMsgBodyClosure",
	#"DeclassStringClosure",
);

#if (-e "snapp") { system ("rm -fr snapp")}

# first, compile the JPMail project
#system ("./compile_civitas.sh");

foreach (@names) {
    system ("jifc $opt source/".$_.".jif");
    for ($i=1; $i<50; $i++) {
        $name = "error".$i.".con";
        if (-e $name) {
            system ("mv $name $dist_dir".$_."_$i.con");
        }
    }
}

print "done!\n"
