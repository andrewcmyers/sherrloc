#!/usr/bin/perl

use strict;
use warnings;
use POSIX qw(strftime);

use CGI qw(:standard);
use Digest::MD5 qw(md5_hex);

my $root = '/home/zhangdf/public_html';
my $source = "program";

my $helpfulness = param('helpfulness');
my $comparison = param('comparison');
my $errorloc = param('errorloc');
my $comments = param('comments');

chdir "$root/temp";
my ($sec, $min, $hr, $day, $mon, $year) = localtime;
my $timestamp = sprintf("%04d%02d%02d%02d%02d%02d", 
       1900 + $year, $mon+1, $day, $hr, $min, $sec);
system ("cp", "$source", "feedback/"."$timestamp");
open (PROGRAM, ">>feedback/$timestamp");

print PROGRAM "(*\n";
print PROGRAM "helpfulness: $helpfulness\n";
print PROGRAM "comparison: $comparison\n";
print PROGRAM "errorloc: $errorloc\n";
print PROGRAM "comments: $comments\n";
print PROGRAM "*)";
close (PROGRAM);

print header(-charset=>'utf-8');

print start_html(
    -title=>'Thank you',
    -style=>{'src'=>'style.css'},

    -BGCOLOR=>'white',
);

print "Thanks very much for your feedback<br>";

print "<a href='http://apl.cs.cornell.edu/~zhangdf/diagnostic'>Try more?</a>";

print end_html();
# print redirect('http://apl.cs.cornell.edu/~zhangdf/temp/error.html');

