#!/usr/bin/perl

use strict;
use warnings;
use POSIX qw(strftime);

use CGI qw(:standard);
use Digest::MD5 qw(md5_hex);

$CGI::POST_MAX = 1024 * 2000;

my $root = '/home/zhangdf/public_html';
my $ocamlbin = '/home/zhangdf/diagnostic/easyocaml-modified/ocaml-3.10.2/bin';
my $diagbin = '/home/zhangdf/diagnostic/GenErrorDiagnostic';
my $ecaml= $ocamlbin."/ecamlc";
my $ocaml= $ocamlbin."/ocamlc";
my $diagnostic= $diagbin."/diagnostic";
my $source = "program.ml";
my $cons_file = "error.con";

chdir "$root/temp";
open (PROGRAM, ">$source");

print header(-charset=>'utf-8');\
print start_html(
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

my $upload_filename = param("prog_file");

if ($upload_filename) {
    my $upload_file = upload("prog_file");
    while (<$upload_file>) {
        print PROGRAM;
    }
}
else {
    print PROGRAM param('prog');
}
close (PROGRAM);
my $md5 = md5_hex (param('prog'));
system ("cp", "$source", "archive/"."$source"."$md5"); 

system ("rm", "$cons_file");

my $result =  `$ocaml -c $source 2>&1`;

if(!$result) {
    print "<H2><BR>OCaml Compiler Report</H2>";
    print "<H3>The program passed type checking. No errors were found.</H3>";
    print end_html();
    exit 0;
}

my $inter_proc = param("inter");

if ($inter_proc eq 'no') {
    system ("$ecaml", "$source");
}
else {
    system ("$ecaml", "-inter", "$source");
}

if (-e $cons_file) {
    system ("$diagnostic", "-c", "-s", "-f", "-i", "$source", "-o", "error.html", "$cons_file", ">/dev/null");
    print "<H2><BR>OCaml Compiler Report</H2>";
    print "<HR>\n";
    print "<pre>\n";
    print $result;
    print "</pre>\n";

    open (DIAG_RESULT, "error.html");
    while (<DIAG_RESULT>) {
        print;
    } 
    close (DIAG_RESULT);
}
else {
    print "<H2><BR>OCaml Compiler Report</H2>";
    print "<HR>\n";
    print "<pre>\n";
    print $result;
    print "</pre>\n";

    print "<pre class=\"code\">\n";
    open (PROGRAM, "$source");
    while (<PROGRAM>) {
        print $. . '. ';
        print;
    } 
    close (PROGRAM);
    print "\n</pre>";
}

print end_html();
# print redirect('http://apl.cs.cornell.edu/~zhangdf/temp/error.html');

