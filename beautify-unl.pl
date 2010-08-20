#!/usr/bin/perl -w #-d
#
# beautify-unl.pl - Output delimited columns in nice, even columns.

# Author: Jacob Salomon
#         jakesalomon@yahoo.com

# Options:
#   -d delimiter    The input delimiter that marks the columns.
#                   Default: | (Vertical bar)
#                   To specify a blank delimiter, use -db
#   -D delimiter    (Not implemented yet) The delimiter to use
#                   for output columns
# Parameters:
#   The input file[s].  Default: stdin
#
# Output is to stdout, using the same delimiter in the input
#
# Note that the original purpose of this utility was to straighten out
# the columns of an unloaded query in an Informix environment. As such,
# the default column delimiter is the | and each line does end in a
# delimiter, with nothing to follow.  Hence, the split() function will
# think it has parsed one more column that it actually has found.
#----------------------------------------------------------------------
# Author: $Author: Jake $
# Date:   $Date: 2010/08/04 05:30:16 $
# Header information:
# $Id: beautify-unl.pl,v 1.5 2010/08/04 05:30:16 Jake Exp $
# Log messages:
# $Log: beautify-unl.pl,v $
# Revision 1.5  2010/08/04 05:30:16  Jake
# Minor fix to better conform with Perl object-method conventions
#
# Revision 1.4  2010/07/27 06:13:39  Jake
# Changed the way it calls the UNLbeautifier constructor, since I have
# changed the consructor as well.
#
# Revision 1.3  2010/07/25 23:59:31  Jake
# Coded for -h option (though not for --help)
#
# Revision 1.2  2010/07/25 20:54:12  Jake
# (Just adding my full name and email address to the file.
#  RCS on Cygwin had no way tpo obtain that)
#
# Revision 1.1  2010/07/25 20:47:09  Jake
# Initial revision
#
#----------------------------------------------------------------------

# Release History:
# Release 1.0 2010-??-??
#   Rewrote in Perl from the original shell script
#
use strict;
#use carp;
use warnings;
#use diagnostics;

use Getopt::Std;

use UNLbeautifier;

use Data::Dumper;

my $program_name = $0;

my %opt_values;         # Hash for the option values the user specified
                        # on the command line
my $opt_string = "hd:D:"; # Specification of delimiters
my $def_in_delim = '|'; # Default input delimiter
#print Data::Dumper->Dump([\%main::], ["main"]), "\n==============\n";
#print Data::Dumper->Dump([\%UNLbeautifier::], ["UNLbeautifier"]), "\n";

#
# Begin parsing the command line.  (For now, simple enough)
#
my $white = '\s+';      # White-space pattern (routine)

my $ok_opts = getopts($opt_string, \%opt_values);
if (! $ok_opts)
{
  Usage();
  die "Please check your options list.";
};

# OK, options parsed successfully. What are they?
# OK, simple enough, optional input and output column delimiters
#
if (defined($opt_values{h}))
{ # Just asked for help. Give it and go away
  Usage();
  exit(0);
}
my $in_delim  = defined($opt_values{d})
              ? $opt_values{d} : $def_in_delim;

my $out_delim = defined($opt_values{D})
              ? $opt_values{D} : $in_delim;

my $p_file = UNLbeautifier->new();  # Create the object and
$p_file->set_in_delim($in_delim);   # set the input/output delimiters
$p_file->set_out_delim($out_delim);

my $in_line = "";                   # Line buffer
my $tally = -1;                     # Mainly for debugging
while ($in_line = <>)
{
  $tally = $p_file + $in_line;      # Throw the line to the beautifier
}
# All done: Print it out
#
$p_file->print();

#
sub HELP_MESSAGE
{
  my ($f_handle, $opt_pkg_name, $opt_pkg_version, $opt_string)
   = @_;    # (Not using these arguments at this time. -- JS)
  Usage();
}

sub Usage
{
  print <<EOT
Usage:
  $program_name --help
  $program_name [-d delimiter] [-D delimiter] [File] [File ...]

  --help: This help text

  -d: The 1-character delimiter of columns in the input file
      Default: The vertical bar (|).
      To specify a space as input delimiter, use -db

  -D: The 1-character delimiter to be used to separate the output
      columns. Default: The same as the input delimiter

  Files: The names of the input files, of course!
      Default: stdin

  All output is to stdout.
EOT
}
