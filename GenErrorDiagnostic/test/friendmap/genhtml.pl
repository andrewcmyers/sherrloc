#!/usr/bin/perl

use Cwd;
my $path = Cwd::abs_path();
my $dirname = getcwd;
my $diagnostic = "$dirname/../../diagnostic";
my $cp = "../tests/:.";
my $html = "temp.html";
my $consfile;

open (TEMP, ">$html");

use strict;
use warnings;
use POSIX qw(strftime);

use CGI qw(:standard);

if ( @ARGV > 0 )
{
  $consfile = $ARGV[0];
}
else
{
  print "No arguments!\n";
  exit;
}

system ($diagnostic, "-c", "-i", $consfile.".fab", $consfile."_1.con");

print TEMP start_html(
    -head=>meta({-http_equiv => 'Content-Type',-content => 'text/html',-charset=>'utf-8'}),
    -title=>'Error Diagnostic Report',
    -style=>{'src'=>'style.css'},

    -BGCOLOR=>'white',

    -script=>[ {-code=>"function windowTitle()
                       {
                         if (location.href.indexOf(\'is-external=true\') == -1) {
                            parent.document.title=\"report\";
                         }
                       }" },
        
               {-src=>'errors.js'},

               {-src=>'colorize.js'},],
);

open (DIAG_RESULT, "<error.html");
while (<DIAG_RESULT>) {
    print TEMP;
} 

print TEMP end_html();
close (TEMP);
close (DIAG_RESULT);

system ("firefox", $html);
