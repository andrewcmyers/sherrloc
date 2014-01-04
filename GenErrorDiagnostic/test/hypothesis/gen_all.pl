#!/usr/bin/perl
use Cwd;

my $cwd = cwd();
my $subname = "gen_constraints.pl";
my $dist_dir = "../constraints/";


opendir DIR, $cwd or die "open failed : $!\n";

for(readdir DIR)
{ 
	if ($_ eq "auction" or $_ eq "battleship" or $_ eq "friendmap" or $_ eq "social")
	{	
		chdir "$_";
		system "./$subname $dist_dir\n";
        	chdir "..";	
	}
}

closedir DIR or die "close failed : $!\n";
