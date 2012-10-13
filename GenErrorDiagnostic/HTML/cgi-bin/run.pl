#!/usr/bin/perl

use strict;
use warnings;
use POSIX qw(strftime);

use CGI qw(:standard);
use Digest::MD5 qw(md5_hex);

my $root = '/home/zhangdf/public_html';
my $ocamlbin = '/home/zhangdf/diagnostic/easyocaml-modified/ocaml-3.10.2/bin';
my $diagbin = '/home/zhangdf/diagnostic/GenErrorDiagnostic';
my $ecaml= $ocamlbin."/ecaml";
my $diagnostic= $diagbin."/diagnostic";
my $source = "program";

chdir "$root/temp";
open (PROGRAM, ">$source");
# print header(-charset=>'utf-8');
# print start_html();
# print start_html(
#     -title=>'Error Diagnostic Report',
#     -style=>{'src'=>'style.css'},
# 
#     -BGCOLOR=>'white',
# 
#     -script=>{-type=>'JAVASCRIPT',
#               -code=>'function windowTitle()\n',
#                      '{\n' ,
#     	             '\tif (location.href.indexOf(\'is-external=true\') == -1) {\n' ,
#                      '\t\tparent.document.title=\"report\";\n' ,
#                      '\t}\n' ,
# 		     '}\n' },
# 
#     -script=>{-type=>'JAVASCRIPT',
#               -src=>'/error.js'},
#     -script=>{-type=>'JAVASCRIPT',
#               -src=>'/colorize.js'},
# );
print PROGRAM param('prog');
close (PROGRAM);
my $md5 = md5_hex (param('prog'));
system ("cp", "$source", "archive/"."$source"."$md5"); 
my $result =  `$ecaml $source`;
if ($result) {
    print header(-charset=>'utf-8');\
    print start_html(
        -title=>'Error Diagnostic Report',
        -style=>{'src'=>'style.css'},
    
        -BGCOLOR=>'white',
    
        -script=>{-type=>'JAVASCRIPT',
                  -code=>'function windowTitle()\n',
                         '{\n' ,
        	             '\tif (location.href.indexOf(\'is-external=true\') == -1) {\n' ,
                         '\t\tparent.document.title=\"report\";\n' ,
                         '\t}\n' ,
   	     '}\n' },
   
        -script=>{-type=>'JAVASCRIPT',
                  -src=>'/error.js'},
        -script=>{-type=>'JAVASCRIPT',
                  -src=>'/colorize.js'},
    );
    print $result;
    print "<pre class=\"code\">\n";
    print param('prog');
    print "\n</pre>";
    print end_html();
}
else {
    system ("$diagnostic", "-c", "-s", "-i", "$source", "-o", "error.html", "error.con", ">/dev/null");
    system ("chmod", "a+r", "error.html");
    print redirect('http://apl.cs.cornell.edu/~zhangdf/temp/error.html');
}
