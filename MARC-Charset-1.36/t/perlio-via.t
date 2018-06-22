#!/usr/bin/perl

#==============================================================================================================================================================================
# Legal license: This software is free software and may be distributed under the same terms as Perl itself.
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Purpose: Tests MARC-8 PerlIO :via(MARC::Charset) layer
# Usage: perl -Ilib t/perlio-via.t --debug=10
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Change history:
# 
# 19 Jun 2018	Jean-François Allard	Creation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# TODO:
#==============================================================================================================================================================================

use strict;
use warnings;
use MARC::Charset;			# For its PerlIO layer + quoteString()
use Getopt::Long;			# For program options
use Pod::Usage;
require Devel::Peek;		# For debug purpose
use Fcntl;					# For test purpose
use Test::More tests => 7;
#use Test::More 'no_plan';	# Temporarily use this line, rather than the one above, after having added many new tests

my $options;				# Program options

test();

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Runs all tests
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub test {
	# Initializations
	
	parseOptions();						# Parse program options
	
	# Configure MARC::Charset module
	MARC::Charset->ignore_errors(1);	# ignore MARC-8 encoding errors
	MARC::Charset::setCallbacks(getDebugLevel => \& getDebugLevel);	# Gets the current debug level 0..10
	
	# Calls to _test() convert MARC-8 to Unicode, then back to MARC-8
	
	_test(<<END);						# 2 MARC-8 lines
		\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBC\xBD\xC0\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8
		\xE0e\xE1e\xE2e\xE3e\xE4e\xE5e\xE6e\xE7e\xE8e\xE9e\xEAe\xEDe\xEEe\xEFe\xF0e\xF1e\xF2e\xF3e\xF4e\xF5e\xF6e\xF7e\xF8e\xF9e\xFEe\r
END
	
	_test("\xEBe\xECe");				# "Combining double inverted breve" diacritic, above two "e" letters
	_test("\xFAe\xFBe");				# "Combining double tilde" diacritic, above two "e" letters
	
	test7();							# Test +>:raw:via(MARC::Charset), TELL() and SEEK()
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Tests PerlIO :via(MARC::Charset) layer
# Converts MARC-8 to Unicode, then back to MARC-8
# Tests without and with PerlIO :crlf layer
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub _test {
	my ($marc8Initial) = @_;
	my $marc8Layer = ":raw:via(MARC::Charset)";	# PerlIO MARC-8 layer
	_test2($marc8Initial, $marc8Layer);
	_test2($marc8Initial, "$marc8Layer:crlf");	# Also test with CR+LF <=> LF conversions
}
	
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Tests PerlIO MARC-8 layer
# Converts MARC-8 to Unicode, then back to MARC-8
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub _test2 {
	my ($marc8Initial, $marc8Layer) = @_;
	
	# Initialization:
	# - Create a pipe
	# Step 1: Write MARC-8 to pipe, and read Unicode from pipe
	# - Configure the pipe read handle to PerlIO :via(MARC::Charset) layer
	# - Configure the pipe write handle to transparent PerlIO layer
	# - Write MARC-8 to the pipe
	# - Read Unicode from the pipe
	# Step 2: Write Unicode to pipe, and read MARC-8 from pipe
	# - Configure the pipe read handle to transparent PerlIO layer
	# - Configure the pipe write handle to PerlIO :via(MARC::Charset) layer
	# - Write Unicode to the pipe
	# - Read MARC-8 from the pipe
	# Step 3: Compare the MARC-8 content, written at step 1, with the one read at step 2.
	
	# Initializations:
	
	# Create a pipe
	
	pipe(my ($pipeReadHandle, $pipeWriteHandle)) || die("Cannot create pipe: $!");
	debug("pipe($pipeReadHandle, $pipeWriteHandle)");
	
	# TODO: Why not. Are they really useful?
#	$pipeReadHandle->autoflush(1);
#	$pipeWriteHandle->autoflush(1);
	
#	# TODO: Set mode to read from pipe without blocking
#	debug("Before $pipeReadHandle->blocking(0)");
#	defined($pipeReadHandle->blocking(0)) || die("$pipeReadHandle->blocking(0): $!");	# Program exits without displaying the die() info! Do not know why.
#	debug("After $pipeReadHandle->blocking(0)");
	
	# PerlIO layers
	
	#debug("PerlIO layers $marc8Layer");
	
	my $transparentLayer = ":raw:unix";	# Transparent PerlIO layer. :unix is necessary, else would read back MARC-8 with UTF-8 flag set!
	
	# The last line must end with a newline, for $read->() to do not block forever on trying to read it
	
	if ($marc8Initial !~ m{\n\z}) {
		debug("Note: Adding a newline to initial MARC-8");
		$marc8Initial .= "\n";
	}
	
	# Insert an \r before \n in $marc8Initial for test with :crlf layer at the end of $marc8Layer
	$marc8Initial =~ s{\n}{\r\n}g if $marc8Layer =~ m{:crlf\z};
	
	# Number of lines in $marc8Initial
	# This variable is used by subroutine $read->(), to read exactly $nbLines lines from the pipe,
	# i.e. all lines of $marc8Initial, without blocking forever on waiting to read, from the pipe, a line that will never exist.
	my $nbLines = $marc8Initial =~ tr[\n][\n];
	
#	# Number of lines in $marc8Initial
#	# The use of "() = " is a trick, that forces a list context for m{\n.}g pattern matches, to count occurrences.
#	# The pattern has "." to exclude a newline at end of file, to do not count the last line, whatever it has a newline or not.
#	# The last line is then taken into account by the " + 1".
#	# This variable is used by subroutine $read->(), to read exactly $nbLines lines from the pipe,
#	# i.e. all lines of $marc8Initial, without blocking forever on waiting to read, from the pipe, a line that will never exist.
#	my $nbLines = (() = $marc8Initial =~ m{\n.}g) + 1;
	
	# Step 1: Write MARC-8 to pipe, and read Unicode from pipe
	
	setLayers($pipeWriteHandle, $transparentLayer);		# Configure the pipe write handle to transparent PerlIO layer
	setLayers($pipeReadHandle, $marc8Layer);			# Configure the pipe read handle to PerlIO :via(MARC::Charset) layer
	_write($pipeWriteHandle, $marc8Initial);			# Write MARC-8 to the pipe
	my $unicode = _read($pipeReadHandle, $nbLines);		# Read Unicode from the pipe
	
	# Step 2: Write Unicode to pipe, and read MARC-8 from pipe
	
	setLayers($pipeWriteHandle, $marc8Layer);			# Configure the pipe write handle to PerlIO :via(MARC::Charset) layer
	setLayers($pipeReadHandle, $transparentLayer);		# Configure the pipe read handle to transparent PerlIO layer
	_write($pipeWriteHandle, $unicode);					# Write Unicode to the pipe
	my $marc8Read = _read($pipeReadHandle, $nbLines);	# Read MARC-8 from the pipe
	
	# Step 3: Compare the MARC-8 content, written at step 1, with the one read at step 2.
	# Display test result
	
	#debug("Initial MARC-8:       ", quoteString($marc8Initial));
	Devel::Peek::Dump($marc8Initial) if getDebugLevel() >= 10;
	#debug("MARC-8, after codec:  ", quoteString($marc8Read));
	Devel::Peek::Dump($marc8Read) if getDebugLevel() >= 10;
	debug("Intermediate Unicode: ", quoteString($unicode));
	my $testName = "Convert MARC-8 " . quoteString($marc8Initial) . " to Unicode, then back to MARC-8, using $marc8Layer layer";
	is(quoteString($marc8Read), quoteString($marc8Initial), $testName);	# Output test result
	displayLine() if getDebugLevel();					# Output a newline, before next test, for readability
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Sets a pipe handle PerlIO layers
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub setLayers {
	my ($pipeHandle, $layers) = @_;
	debug(qq(binmode($pipeHandle, "$layers")));
	binmode($pipeHandle, $layers) || die("Cannot set pipe writer PerlIO layers to $layers: $!");
	
	# Debug info: pipe handle PerlIO encoding
	debug(getPerlioLayers($pipeHandle));
	debug(getPerlioLayers($pipeHandle, 1));
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Reads $nbLines lines from a pipe read handler
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub _read {
	my ($pipeReadHandle, $nbLines) = @_;
	#local $/;				# Read all lines. Do not use. It would block forever. $pipeReadHandle->blocking(0) fails.
	my $lines;
	
	for (1 .. $nbLines) {	# read exactly $nbLines from the pipe
		debug("<$pipeReadHandle>");
		my $line = <$pipeReadHandle>;
		defined($line) || die("Cannot read from pipe: $!");
		debug("<$pipeReadHandle> = ", quoteString($line), ")");
		$lines .= $line;
	}
	
	debug("Read ", quoteString($lines));
	return $lines;
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Writes lines to a pipe write handler
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub _write {
	my ($pipeWriteHandle, $lines) = @_;
	debug("$pipeWriteHandle->printflush(", quoteString($lines), ")");
	$pipeWriteHandle->printflush($lines) || die("Cannot write to the pipe: $!");
	debug("Wrote to $pipeWriteHandle");
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Gets PerlIO layers
# $getPerlioLayers->(GLOB(0x3f989d8), 1) =
#     unix(args = undef, flags = 0x1201200), crlf(args = undef, flags = 0x401200), via(args = undef, flags = 0x409200), crlf(args = undef, flags = 0x40d200)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub getPerlioLayers {
	my ($fh, $details) = @_;
	my @layers = PerlIO::get_layers($fh, details => $details);
	my @realLayers;
	
	for (my $i = 0; $i < @layers; $i += 2 + ($details ? 1 : 0)) {
		my $realLayer;
		
		if ($details) {
			my ($layer, $args, $flags) = @layers[$i .. $i+2];
			$args = "undef" unless defined($args);
			$flags = sprintf('0x%x', $flags);
			$realLayer = "$layer(args = $args, flags = $flags)";
		}
		else {
			my ($layer, $flag) = @layers[$i .. $i+1];
			$realLayer = "$layer(flag = " . ($flag || "undef") . ")";
		}
		
		push(@realLayers, $realLayer);
	}
	
	return "PerlIO::get_layers($fh" . (defined($details) ? ", details => $details" : "") . ") = " . join(", ", @realLayers);
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Tests +>:raw:via(MARC::Charset), TELL() and SEEK()
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub test7 {
	open(my $fh, "+>:raw:via(MARC::Charset)", "testMarc8.dummy");	# MARC::Charset::PUSHED("MARC::Charset", "w+", undef)
	$fh->print(my $line1 = "e\x{360}e\n");		# Write line 1 to the file
	$fh->print(my $line2 = "e\x{361}e\n");		# Write line 2 to the file
	my $ok;										# Test result
	
	if ($fh->tell() == 10) {					# Check current reading/writing position
		$fh->seek(0, Fcntl::SEEK_SET);			# Rewind to the beginning of file
		
		if (<$fh> eq $line1) {					# Check line 1
			if (<$fh> eq $line2) {				# Check line 2
				$fh->seek(5, Fcntl::SEEK_SET);	# Rewind to the beginning of line 2
				$fh->print($line1);				# Overwrite line 2 with $line1
				$fh->seek(5, Fcntl::SEEK_SET);	# Rewind to the beginning of line 2
				$ok = <$fh> eq $line1;			# Check line 2
			}
		}
	}
	
	ok($ok, "open(..., '+>:raw:via(MARC::Charset)', ...), tell() and seek()");	# Display test result
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Quotes a string to be printable
# For debug and test purpose
# MARC::Charset::quoteString should not be importable
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub quoteString {
	MARC::Charset::quoteString(@_);
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Outputs a text string
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub displayLine {
	print(@_, "\n");
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Gets the current debug level 0..10
# For debug purpose
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub getDebugLevel {
	return $options->{debug};
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Outputs a debug info on STDERR
# For debug purpose
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub debug {
	my @caller = caller(1);
	warn(@_, " at ", $caller[1], " line ", $caller[2], "\n") if getDebugLevel();
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Parses options from @ARGV.
# Modifies $options and @ARGV.
# See below for options.
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sub parseOptions {
	$options = {		# Reset $options to default value
		debug	=> 0,	# $options->{verbose} should always be an integer (never undefined)
	};
	
	Getopt::Long::GetOptions(
		$options,
		"debug:1",		# Outputs debug info on stderr. Optional level 1..10 allows debug level Info. Default level is 1.
		"help",			# Outputs an online brief help message
		"man",			# Outputs the online manual page
	) ||
	Pod::Usage::pod2usage(2);
	
	Pod::Usage::pod2usage(-exitval => 1) if $options->{help};				# Help
	Pod::Usage::pod2usage(-exitval => 0, -verbose => 2) if $options->{man};	# Man
	return $options;
}

__END__

=head1 NAME

perlio-via.t - Tests PerlIO :via(MARC::Charset) layer

=head1 SYNOPSIS

perlio-via.t [options]

=head1 OPTIONS

=over 8

=item B<--verbose>[=I<level>]

Outputs debug info on stderr. Optional level 1..10 allows debug level Info. Default level is 1.

=item B<--help>

Outputs an online brief help message.

=item B<--man>

Outputs the online manual page.

=back

=head1 DESCRIPTION

=cut