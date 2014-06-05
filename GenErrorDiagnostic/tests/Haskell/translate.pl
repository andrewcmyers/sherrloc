#!/usr/bin/perl
use strict;

package PlainCt;

sub new {
	my $type = shift;
	my $ct = shift;
	my ($left, $right, $rel);

	if ($ct =~ /(.*) ~ (.*)/) {
		$left = $1;
		$right = $2;
		$rel = "==";
	}	
	elsif ($ct =~ /(.*) (.*)/) {	# type class constraints
		$left = $2;
		$right = $1;
		$rel = "<=";
	}
	else {
		die "Cannot translate constraint $ct";
	}
	
	my $self = {
		left => $left,
		right => $right,
		rel => $rel 
	};

	bless $self, $type;
	return $self;
}

# translate a plain GCH constraint element to the SHErrLoc syntax
sub to_sherrloc_ele {
	my $ct = shift;
	# [x] to (list x)
	$ct =~ s/\[(.*)\]/(list $1)/g;
	return $ct;
}

sub print {
	my ($self) = @_;
	my $ret = (to_sherrloc_ele ($self->{left}));
	$ret .= " $self->{rel} ";
	$ret .= (to_sherrloc_ele ($self->{right}));
	return $ret;
}

sub print_wLoc {
	my ($self, $loc) = @_;
	my $ret = (to_sherrloc_ele ($self->{left}));
	$ret .= "[\"\":$loc]";
	$ret .= " $self->{rel} ";
	$ret .= (to_sherrloc_ele ($self->{right}));
	$ret .= "[\"\":$loc]";
	return $ret;
}

package Constraint;

sub new {
	my $type = shift;
	my $ct = new PlainCt(shift);
	my $desc = shift;
	my $loc = shift;
	my $hypo = shift;
	my @hypolist = ();

	foreach (@{$hypo}) {
		push (@hypolist, $_);
	}	


	# translate location format
	if ($loc =~ /(.*):(.*):(.*)/) {
		my ($line, $col, $file) = ($2, $3, $1);
		if ($col !~ /-/) {
			$col = "$col-$col";
		}
		$loc = "$line,$col\@$file";
	}
	else {
		die "Cannot translate location $loc";
	}

	my $self = {
		ct => $ct,
		desc => $desc,      # description
		loc => $loc,        # location in source code
		hypo => \@hypolist   # hypothesis
	};

	bless $self, $type;
	return $self;
}


sub print {
	my ($self) = @_;
        my $line = $self->{ct}->print_wLoc($self->{loc}); 
	$line .= " {";
	foreach (@{$self->{hypo}}) {
		$line .= ($_)->print().";";
	}	
	$line .= "};[\"$self->{desc}\":$self->{loc}]\n";

	return $line;
}

package main;

sub syntax_error {
	die "Unsupported constraint syntax: $_[0]";
}

my $tracefile;          # GHC trace file
my @constraints = ();    # collected constraints
my @assumptions = ();   # collected assumptions
my $prev_ct;            # a constraint may occupy multiple lines

# get an input trace file from GHC
if ( @ARGV > 0 )
{
  $tracefile = $ARGV[0];
}
else
{
  print "Please provide a trace file from GHC by using the option -ddump-tc-trace\n";
  exit;
}

open (TRACE, "<$tracefile") or die "$tracefile does not exist!";
my ($outfile) = $tracefile =~ /(.*)?\./;
open (OUT, ">$outfile.con") or die "Cannot open output file $outfile.con!";

# translate one plain GHC constraint (potentially incomplete)
sub trans_ct {
	my $line = shift;
	chomp $line;
	$line =~ s/^\s+//;
	if ($line eq "") { # avoid empty constraints
		return;
	}
	if ($line !~ /[W]/) {
		$line = $prev_ct." ".$line;
	}
	# remove useless annotations, and attach assumptions
	if ($line =~ m/:: (.*) \[\" (.*) \" (.*)\] \(CNonCanonical\)/) {
		print "  Ct: $line\n";
		# need to make sure pass in an array reference, instead of an array
		push (@constraints, new Constraint($1, $2, $3, \@assumptions));
	}
	$prev_ct = $line;
}

# translate one assumption
sub trans_as {
	my $line = shift;
	chomp $line;
	$line =~ s/^\s+//;
	print "  As: $line\n";

	#remove useless annotations
	if ($line =~ m/:: (.*)/) {
		push (@assumptions, new PlainCt($1));
	}
	else {
		die "Cannot translate assumption: $line\n";
	}
}

sub trans_WC;
sub trans_implic;

# only need to translate wc_flat, and wc_impl
sub trans_WC {
	my $line = shift;
	my $tail;
	my $end = 0;

	#print "(WC) parsing $line";

	if ($line =~ /Implic \{(.*)/) {
		$tail = trans_implic $1;
	}
	else {
		# stripe the last }s if it indicates an end 
		if ($line =~ m/([^\}]*)(\}.*)/) { # match the first occurrence of }
			$line = $1;
			$tail = $2;
		}
		if ($line =~ /wc_flat = (.*)/) {
			trans_ct $1;
		}
		else {
			trans_ct $line;
		}
	}

	if ($tail =~ m/^\}(.*)/) {
		$end = 1;
		$tail = $1;
	}	

	if (not $end) {
		my $nextline = <TRACE>;
		$tail = trans_WC $nextline;
	}

	return $tail;
}

# only need to translate Skolems, Given and Wanted
sub trans_implic {
	my $line = shift;
	my $end = 0;
	my $tail;
	my @old_as = @assumptions;

	#print "(imp) parsing $line";

	if ($line =~ /WC \{(.*)/) {
		$tail = trans_WC $1;
	}
	else {
		# stripe the last }s if it indicates an end 
		if ($line =~ m/([^\}]*)(\}.*)/) { # match the first occurrence of }
			$line = $1;
			$tail = $2;
		}
		if ($line =~ /Skolems = (.*)/) {
			my $skolems = $1;
			print "  Skolems: $skolems\n";
		}
		elsif ($line =~ /Given = (.*)/) {
			trans_as $1;
		}
	}

	if ($tail =~ m/^\}(.*)/) {
		$end = 1;
		$tail = $1;
	}	
	
	if (not $end) {
		my $nextline = <TRACE>;
		$tail = trans_implic $nextline;
	}
	# pop out local assumptions
	@assumptions = @old_as;
	return $tail;
}

while (<TRACE>) {

	# get the original constraints that caused errors
	if (/^originalCts/) {
		my $next = <TRACE>;
		if ($next =~ /fvars =  \[(.*)\](.*)/) {
			my $vars = $1;
			if ($2 !~ /wanted =/) {
				$next = <TRACE>;
			}
			@constraints = ();
			@assumptions = ();
			print "Translating wanted constraints:\n";
			trans_WC $next;
			# output translated constraints
			print "Translated SHErrLoc constraints:\n";
			if ($vars =~ /\((.*)\)/) {
				my @vars = split(',', $1);
				foreach (@vars) {
					print OUT ("VARIABLE $_\n");
				}
			}
			print OUT "\n";
			foreach (@constraints) {
				print OUT ($_->print());
			}
		}
		else {
			syntax_error $next;
		}
	}
}

close TRACE;
close OUT;

system ("cat", "$outfile.con");
