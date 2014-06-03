#!/usr/bin/perl


sub syntax_error {
	die "Unsupported constraint syntax: $_[0]";
}

sub parse {
	my $temp;
	print ;
	if (/WC {/) {
		$header = "WC";
		$temp = m/WC {(.*)/;
		print "WC { $temp\n";
		trans_WC $temp;
	}
	else {
		syntax_error $_;
	}
}

my $tracefile;
my @constaints = ();
my @assumptions = ();
my $temp_str; # a constraint may occupy multiple lines

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

open (TRACE, "<$tracefile");

# translate to SHErrLoc syntax
sub to_sherrloc_ct {
	$ct = shift;

	if ($ct =~ /~/) {	# equality constraints
		$ct =~ s/~/==/g; 	# ~ to ==
	}
	else { 	# type class constraints
		$ct =~ m/(.*) (.*)/;
		$ct = $2." <= ".$1;
	}
	# [x] to (list x)
	$ct =~ s/\[(.*)\]/(list $1)/g;
	return $ct;
}

my $prev_ct;

# translate one plain GHC constraint (potentially incomplete)
sub trans_ct {
	my $line = shift;
	chomp $line;
	$line =~ s/^\s+//;
	if ($line eq "") { # avoid empty constraints
		return;
	}
	if ($line =~ /[W]/) {
		print "  Ct: $line\n";
	}
	else {
		$line = $prev_ct." ".$line;
		print "  Ct: $line (append)\n";
	}
	# remove useless annotations, and attach assumptions
	if ($line =~ m/:: (.*) \(CNonCanonical\)/) {
		$line = to_sherrloc_ct ($1);
		$line .= " {";
		foreach (@assumptions) {
			$line .= to_sherrloc_ct ($_).";";
		}
		$line .= "}";
		push (@constraints, $line);
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
		push (@assumptions, $1);
	}
	else {
		die "Unrecognized assumption: $line\n";
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

# output the constraints into SHErrLoc syntax
sub output_sherrloc {
}

while (<TRACE>) {

	# get the original constraints that caused errors
	if (/^originalCts/) {
		my $next = <TRACE>;
		if ($next =~ /WC \{/) {
			@constraints = ();
			@assumptions = ();
			print "Translating wanted constraints:\n";
			trans_WC $next;
			# output translated constraints
			print "Translated SHErrLoc constraints:\n";
			foreach (@constraints) {
				print $_.";\n";
			}
		}
		else {
			syntax_error $next;
		}
		print "\n";
	}
}
