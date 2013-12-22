#!/usr/bin/perl

use Cwd qw();
my $path = Cwd::abs_path();
my $bin = "$path/../../../easyocaml-modified/ocaml-3.10.2/bin";
my $diagnostic = "$path/../../diagnostic";
my $html = "temp.html";
my $mlfile;

open (TEMP, ">$html");

use strict;
use warnings;
use POSIX qw(strftime);

use CGI qw(:standard);

if ( @ARGV > 0 )
{
  $mlfile = $ARGV[0];
}
else
{
  print "No arguments!\n";
  exit;
}

system ("$bin/ecamlc", $mlfile);
system ($diagnostic, "-c", "-s", "-i", $mlfile, "error.con");

print TEMP start_html(
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

print TEMP "<H4> OCaml error message:</H4><br>";

my $result =  `$bin/ocamlc $mlfile 2>&1`;
print TEMP $result;

open (DIAG_RESULT, "<error.html");
while (<DIAG_RESULT>) {
    print TEMP;
} 

print TEMP end_html();
close (TEMP);
close (DIAG_RESULT);

system ("firefox", $html);
