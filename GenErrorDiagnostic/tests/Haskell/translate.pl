#!/usr/bin/perl
use strict;

my @synonyms = ();      # collected synonyms

package Synonym;

sub new {
	my $type = shift;
	my $left = shift;
        my $right = shift;

        my @left_words = $left =~ /(\S+)/g;
        my $name = shift @left_words;

	my $self = {
		name => $name,
                replacement => $right,
		paras => \@left_words
	};

	bless $self, $type;
}	

# translate synonyms in a constraint
sub transCtr {
	my ($self) = shift; 
	my $ctr = shift;

        while ($ctr =~ m/(.*)\b$self->{name}\b(.*)/) {
                my $head = $1;
                my $rest = $2; 
                my $to_replace = $self->{replacement};
                my $word;
                #chew on parameters, and substitute them in the replacement 
                foreach (@{$self->{paras}}) {
                        ($word, $rest) = ($rest =~ m/(\(.+\)|\S+)(.*)/); #extract the next word
                        $to_replace =~ s/\b$_\b/$word/g;
                }
                $ctr = $head.$to_replace.$rest; #match whole word only
        }
        return $ctr;
}

package PlainCt;

sub new {
	my $type = shift;
	my $ct = shift;
	my ($left, $right, $rel);
	$ct =~ s/\R//g; # remove line breaks
	
	if ($ct =~ /(.*) ~ (.*)/) {
		$left = $1;
		$right = $2;
		$rel = "==";
	}	
	else {
		my @tys = ();
		my $paracount = 0;
		my $parsed = "";
		foreach my $byte (split //, $ct) {
			if ($byte eq '(') { $paracount ++;}
			elsif ($byte eq ')') { $paracount --;}
			if ($paracount eq 0 and $byte eq ' ') {
				if ($parsed =~ /.+/) {
					push (@tys, $parsed);
				}
				$parsed = "";
			}
			else {
				$parsed .= $byte;
			}
		}
		if ($parsed =~ /.+/) {	push (@tys, $parsed);}

		#@tys = split(' ', $ct);
		if (scalar @tys > 1) {  # type class constraints: class ty1 ty2 .. tyn
			my $right_name = shift @tys; 	# class name
			# for multi-parameter type classes, we need an constructor to collect elements
			if (scalar @tys > 1) {
				$left = "cons_".(scalar @tys);
                                $right = $right_name; #"cons_".(scalar @tys);
                                my $count = 1;
				foreach (@tys) {
					$left .= " $_";     # rewrite to (cons_n ty1 ty2 .. tyn) <= class
					#$right .= " $right_name$count";
                                        $count++;
				}
			}
			else {
				$left = shift @tys;
                                $right = $right_name;
			}
			$rel = "<=";
		}
		else {
			die "Cannot translate constraint $ct";
		}
	}

        # GHC produces constraints like Integer -> x ~ Integer -> y,
        # where the structure is not informative at all. We remove
        # this source
	if ($left =~ /Integer -> ([^)]*)/) {
		my $temp = $1;
		if ($right =~ /Integer -> ([^)]*)/) {
			$left = "($temp)";
			$right = "($1)";
		}
	}
	
	my $self = {
		left => $left,
		right => $right,
		rel => $rel 
	};

	bless $self, $type;
	return $self;
}

# translate a potentially nested list in the form of [...] to list (...)
sub transList {
	my $str = shift;
	if ($str =~ m/([^\[]*)\[(.*)/) { # match first [
		my $pre = $1;
		my $parsed = transList ($2); # potentially nested lists
	 	# match the first ]
		$parsed =~ m/([^\]]*)\](.*)/;
		return ("$pre(list ($1))$2");	
	}
	else {
		return $str;
	}
}

# translate a plain GCH constraint element to the SHErrLoc syntax
sub to_sherrloc_ele {
	my $ct = shift;
	$ct = transList $ct; # translate lists
        foreach (@synonyms) {
                $ct = $_->transCtr($ct);
        }
	$ct =~ s/String/(list Char)/g;
	$ct =~ s/\(\)/EMPTY/g;
	return "($ct)";
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

package Axiom;

sub new {
	my $type = shift;
	my $qvars = shift;
	my $premise = shift; 
	my $conclusion = shift; 
	my @qvarlist = ();
	my @premiselist = ();
	my @conclusionlist = ();

	foreach (@{$qvars}) {
		push (@qvarlist, $_);
	}

	foreach (@{$premise}) {
		push (@premiselist, $_);
	}	

	foreach (@{$conclusion}) {
		push (@conclusionlist, $_);
	}	

	my $self = {
		qvars => \@qvarlist,   # quantified vars
		premise => \@premiselist,
		conclusion => \@conclusionlist
	};

	bless $self, $type;
	return $self;
}	

sub print {
	my ($self) = @_;
	my $line;
	if (scalar @{$self->{premise}} != 0 or scalar @{$self->{qvars}} != 0) {
		$line .= "axiom ";
	}
	if (scalar @{$self->{qvars}} != 0) {
		$line .= join(' , ', @{$self->{qvars}});
		$line .= " . ";
	}

	$line .= join('; ', map {($_)->print()} @{$self->{premise}});
	if (scalar @{$self->{premise}} != 0) {
		$line .= ";";
		$line .= " => ";
	}
	$line .= join('; ', map {($_)->print()} @{$self->{conclusion}});
	if (scalar @{$self->{conclusion}} != 0) {
		$line .= ";";
	}
	return $line;
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
	elsif ($loc =~ /(.*):\((.*),(.*)\)-\((.*),(.*)\)/) {
		$loc = "$2,$3-$4,$5\@$1";
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
my @constraints = ();   # collected constraints
my @assumptions = ();   # collected assumptions
my @axioms = ();        # collected axioms
#my @functions = ();     # collected functions
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
	if ($line !~ /\[W\]/) {
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
sub trans_AXIOM;

# only need to translate wc_flat, and wc_impl
sub trans_WC {
	my $line = shift;
	my $tail;
	my $end = 0;

	#print "(WC) parsing $line\n";

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
		elsif ($line =~ /wc_insol = (.*)/) {
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

sub _transAxiom {
	my $ct = shift;
	# print "@@@@ translating $ct\n";
	$ct =~ s/\R//g; # remove line breaks
	$ct =~ s/\(->\)/arrow/g; # replace (->) by "arrow"
	$ct =~ s/\(,\)/pair/g;   # replace (,) by "pair"
	my @qvars = ($ct =~ /[( ]([a-z])[ )]/g);
	# remove duplicated declarations
	my %vhash = ();
	foreach (@qvars) {
		$vhash{$_} = 1;
	}
	@qvars = keys %vhash;
	my @premise = ();
	my @conclusion = ();
	if (scalar @qvars == 0 and $ct !~ /=>/) {
		# trivial axiom
		return new PlainCt($ct);
	}
	else {
		# parse an axiom
		if (($ct =~ /(.*)=>(.*)/)) {
			my $lhs = $1;
			$ct = $2;

			if ($lhs =~ m/\((.*)\)/) {
				$lhs = $1;
			}	
			if ($lhs =~ /,/) {
				foreach (split (',', $lhs)) {
					push (@premise, new PlainCt($_));
				}
			}
			else {
				push (@premise, new PlainCt($lhs));
			}
		}
		# assume there is only one conclusion
		@conclusion = new PlainCt($ct);
		my $axiom = new Axiom(\@qvars, \@premise, \@conclusion);
		# print "---> ".($axiom)->print()."\n";
		return new Axiom(\@qvars, \@premise, \@conclusion);
	}
}

# translate axioms
sub trans_AXIOM {
	my $line = shift;
	my $ct = "";
	while ($line !~ /^End .*InstEnvs \}/) {
		$line =~ s/(.*instance) //g;
		$line =~ s/(\[overlap ok\])//g;
                
		if ($line =~ /(.*) -- Defined /) {
			$ct .= $1;
			if ($ct !~ /GHC\.Generics/ and $ct !~ /Data\.Type\.Equality/ ) {
				push (@axioms, _transAxiom($ct));
			}
			$ct = "";
		}
		else {	# collect the constraint
			$ct .= $line;
		}
		$line = <TRACE>;
	}
}

@axioms = ();
while (<TRACE>) {
        # collect synonyms 
        if (/^  type (.*) = (.*)/) {
                push (@synonyms, new Synonym($1, PlainCt::to_sherrloc_ele($2)));
                #push (@functions, "@synstr[0] $size");
        }
	
        # translate axioms
	if (/^InstEnvs \(.*\) \{/ or /^famInstEnvs \(Internal\) \{/) { # only handle internal type families for now
		my $next = <TRACE>;
		trans_AXIOM $next;
	}

	# get the original constraints that caused errors
	if (/^originalCts/) {
		my $next = <TRACE>;
		if ($next =~ /fvars =  (.*)/) {
			my $vars = $1;
			$next = <TRACE>;
			while ($next !~ /wanted =/) {
				$vars .= " $next";
				$next = <TRACE>;
			}
			@constraints = ();
			@assumptions = ();
			print "Translating wanted constraints:\n";
			trans_WC $next;
			# output translated constraints
			print "Translated SHErrLoc constraints:\n";
			my @vars = $vars =~ /\([^,]*, ([^,]*)\)/g;
			foreach (@vars) {
				print OUT ("VARIABLE $_\n");
			}

			print OUT "\n\n%%\n";
			foreach (@axioms) {
				print OUT $_->print().";\n";
			}

			print OUT "\n\n%%\n";
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
