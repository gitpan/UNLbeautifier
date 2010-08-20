#!/usr/bin/perl -w #-d
#
# UNLbeautifier.pm - Package to straighten the columns of delimited
#                    text by padding with spaces so that the delimiters
#                    are alligned.

# Author: Jacob Salomon
#         jakesalomon@yahoo.com

#----------------------------------------------------------------------
# Author: $Author: Jake $
# Date:   $Date: 2010/08/04 05:30:45 $
# Header information:
# $Id: UNLbeautifier.pm,v 1.7 2010/08/04 05:30:45 Jake Exp $
# Log messages:
# $Log: UNLbeautifier.pm,v $
# Revision 1.7  2010/08/04 05:30:45  Jake
# Added new() method to the _util sub-class. Since it is now a true
# object, I also had to:
# o Add a line to each utility function to get the class reference
#   parameter out of the way.
# o Change each function call to an object->method() call.
#
# Revision 1.6  2010/07/29 12:28:43  Jake
# Removed the UNLbeautifier_line:: qualifier from the new() method of
# package UNLbeautifier_line
#
# Revision 1.5  2010/07/28 03:52:55  Jake
# Completed modification to accomadate passing array reference to the +
# operator.
# Completed a decent (IMO) pod.
#
# Revision 1.4  2010/07/27 23:42:09  Jake
# Added array processing in addition to line processing.
# Also started a pod.
#
# Revision 1.3  2010/07/27 06:14:27  Jake
# Changes the constructor to eschew the delimiter parameters.
# Instead, new methods to set the inout and output delimiters
#
# Revision 1.2  2010/07/25 20:54:12  Jake
# (Just adding my full name and email address to the file.
#  RCS on Cygwin had no way tpo obtain that)
#
# Revision 1.1  2010/07/25 20:48:44  Jake
# Initial revision
#
#----------------------------------------------------------------------

# Note: Historically, this is an outgrowth of beautify-unl.sh, a shell
# script targeted for Informix UNLOAD file format.  This is still the
# driving force in this package.  Hence, if a delimiter is escaped by a
# \ in the data, this will be recognized here.  Similarly, an escaped
# escape character will be left unmolested, as it still needs to be
# readable for loading.
#
use strict;
#use carp;
use warnings;
#use diagnostics;

# Patterns I will use to determine the data type of column data:
#
my $white = '\s+';      # White-space pattern (routine)
my $int_pattern = '-?^\d+$';        # Integer pattern, signed
my $dec_pattern = '-?^\d+\.\d*$';   # Decimal Number pattern, signed
my $util;               # Will be a reference to _util object
#
package UNLbeautifier;
  use overload
    '+'  => \&UNL_add_line,
    '<<' => \&UNL_add_parsed_line;

sub new
{ # Create the object and parse the input/out delimiters as well
  #
  my $class = shift(@_);
  my $self = {};        # (Just a reference to an anonynmous hash)
  bless ($self, $class);

  # Some object initialization
  #
  $self->{in_delim}  = '|';     # Default delimiter for unl files
  $self->{in_split}  = '\|';    # Escape it, since | is a metacharacter
  $self->{out_delim} = '|';     # Reasonable for out to mimic in
  $self->{n_lines} = 0;         # No lines parsed yet
  $self->{max_width}[0]    = 0; # Member arrays for column width
  $self->{max_decimals}[0] = 0; # comparisons.  THis is decimal places
  $self->{max_wholes}[0]   = 0; #  Whole parts of decimal numbers

  $self->{has_end_delim}   = 0; # Assume no delimiter at end of line
                                # Will likely revise this flag 
  $util = UNLbeautifier::_util->new();  # Create utility pseudo-object
  return $self;
}
#
# Method: set_in_delim(): Sets the inpput delimiter to something other
# than the default | character.  It also messes with the output
# delimiter, since the default output delimiter is the same as the
# input delimter.  If the user decides otherwise, he can call
# set_out_delim() to accomplish that.
#
# Parameters:
# o (Implicit) Reference to the UNLbeautifier object
# o The input delimiter character.
#
sub set_in_delim
{
  my $self = shift(@_);     #(which moves the delimiter into $_[0]
  my $rval = 1;             # Optimistic start

  return $rval if (! defined($_[0]));   # Forgot delim; use default
                                        # Otherwise, analyze input
  $self->{in_split} = $self->{in_delim} =  shift(@_);   # Get it first
                            # If all is OK, {in_split} is correct delim

  if (length($self->{in_delim}) > 1)
  {                                 # If delimiter came is as string
    my $c = $self->{in_delim};      # tell user only first char is used
    my $d = substr($self->{in_delim}, 0, 1);
    warn "Using only first character ($d) of input delimiter $c\n";
    $self->{in_split} = $self->{in_delim} = $d;
  }

  # White-space delimiter requires special treatment
  #
  if ($self->{in_delim} eq "b") {$self->{in_split} = $white;}

  # Problem: The in-delimiter might be a metacharacter, like '$'
  # So the actual splitting pattern must be escaped.
  #
  $self->{in_split} = ($util->matches_meta($self->{in_delim}))
                    ? quotemeta($self->{in_delim})
                    : $self->{in_delim};
  
  # Now that we have decided how to split the input line, set the
  # default column separator for the output lines: Same is for input.
  #
  $self->set_out_delim($self->{in_split});
  return $rval;
}
#
# Method: UNLbeautifier::set_out_delim(): Sets the output delimiter
# Always called from method set_in_delim; may be called by user
#
# Parameters:
# o (Implicit) Reference to the UNLbeautifier object
# o The output delimiter character.
#
sub set_out_delim
{
  my $self = shift(@_);     #(which moves the delimiter into $_[0]
  my $rval = 1;

   if (! defined($_[0]))    # Forgot delim; Program bug
  {die "Program error: method called with undefined output delimiter\n"}
   
  $self->{out_delim} =  shift(@_);

  # If the output delimiter is an escaped character, [ likely when
  # called from set_in_delim() ] remove the escape and just use the
  # character
  #
  $self->{out_delim} =~ s/^\\(.)$/$1/;

  if ($self->{out_delim} eq "$white") # White-space pattern is a string
  { $self->{out_delim} = ' ';}      # just set it to a blank

  # If, after all that filtering, the output delimiter is still more
  # than 1 character, user may have made a worisome call. Fix for that
  #
  if (length($self->{in_delim}) > 1)
  {                                 # If delimiter came is as string
    my $c = $self->{out_delim};     # tell user only first char is used
    my $d = substr($self->{out_delim}, 0, 1);
    warn "Using only first character ($d) of output delimiter $c\n";
    $self->{out_delim} = $d;
  }

  return $rval;
}

# Method: UNLbeautifier::set_has_end_delim()
# Sets a flag to inticate I want a terminating delimiter on each line,
# as befits a proper .unl file.
#
# Parameter: (Implicit) Ref to a UNLbeautifier object (ie parsed file)
#
sub set_has_end_delim
{
  my $self = $_[0];
  $self->{has_end_delim} = 1;
}
#
# Method + to add a raw line into the parsed-file list
#
sub UNL_add_line
{
  my $self     = shift(@_); # Ref to the UNLbeautifier object
  my $one_line = shift(@_); # Get the actual line string to be appended
  
  # Parse the line and calculate basic info about it.
  #
  my $p_line = UNLbeautifier::Line->new($one_line, $self);

  my $ok = $self << $p_line;     # Integrate parsed line to line list

  $self->{n_lines}++;   # Tally up line count
  return $self->{n_lines};
}
#
#
# Method << to add a parsed line into the parsed file list
# Parameters:
# o (Implicit) reference to the UNLbeautifier file object
# o Reference to a parsed line object
#
sub UNL_add_parsed_line
{
  my ($self, $pline) = @_;
  my $cur_line = $self->{n_lines};  # Slot number to get the line
  my $n_cols = $pline->ncolumns();  # Column count for looping
  $self->{parsed_line}[$cur_line] = $pline; # Store line reference
                                            # Line is now integrated

  if ($pline->{has_delims})
  { $self->check_col_widths($cur_line); }
  # If no delimiter, I don't give a hoot about column width.

  return 1;         # Return success code
}
#
# Method: check_col_widths()
# For an already parsed line, run down trhe columns to set the width of
# the widest value in that column for whole file.
#
# Parameters:
# o (Implicit) Reference to the parsed file object
# o Row (or line) number
#
sub check_col_widths
{
  my ($pfile, $row) = @_;   # (Using $pfile instead of $self. Why?)

  # For each column, compare its width against the widest so far.
  # Similar check for decimal places if it has decimal places
  #
  my $col_wid   = 0;    # Column width
  my $col_whole = 0;    # Width of integer or integer part of a decimal
  my $col_dec   = 0;    # Width of decimal part of a float
  my $row_ref = $pfile->{parsed_line}[$row];    # Neater access to cols
   
  # For cleaner access to columns of the line, use this reference:
  #
  my $split_ref = $row_ref->{split_line};

  for (my $lc = 0; $lc < $row_ref->{columns}; $lc++)
  { # First make sure there is a column width to compare;
    # If not, start it with a zero width.
    #
    if (! defined($pfile->{max_width}[$lc]))
      { $pfile->{max_width}[$lc] = 0; }
    if (! defined($pfile->{max_wholes}[$lc]))
      { $pfile->{max_wholes}[$lc] = 0; }
    if (! defined($pfile->{max_decimals}[$lc]))
      { $pfile->{max_decimals}[$lc] = 0; }

    # Check for widest column. This is counted different ways for
    # string, integer and decimal.  Start by checking integer pattern
    #
    if ($split_ref->[$lc] =~ $int_pattern)
    {
      if ( ($col_wid = length($split_ref->[$lc]))
                     > $pfile->{max_width}[$lc])
      { # We have a new largest width for this column
        # as wll as a widest whole-number part for this column
        #
        $pfile->{max_width}[$lc]  = $col_wid;   # New widest width
        $pfile->{max_wholes}[$lc] = $col_wid;   # Widest whole number
      }
    }
#
    # Check for decimal/float pattern
    #
    elsif ($split_ref->[$lc] =~ $dec_pattern)
    { # If decimal, check for most decimal places and whole numbers
      #
      my $whole_part = 0; my $decimal_part = 0 ;
      ($whole_part, $decimal_part) = split('\.', $split_ref->[$lc]);

      if ( ($col_whole = length($whole_part))
                       > $pfile->{max_wholes}[$lc])
      { $pfile->{max_wholes}[$lc] = $col_whole; }   # New widest whole

      if ( ( $col_dec = length($decimal_part))
                      > $pfile->{max_decimals}[$lc])
      {  $pfile->{max_decimals}[$lc] = $col_dec;}   # New widest decimal

      # Width of widest decimal, so far, is
      # width of widest whole part + width of widest decimal part
      # + 1 for the decimal point.
      #
      my $new_dec_width = $pfile->{max_wholes}[$lc]
                        + $pfile->{max_decimals}[$lc]
                        + 1;  # What is total width of these maxima?
      if ($new_dec_width > $pfile->{max_width}[$lc])
      { $pfile->{max_width}[$lc] = $new_dec_width; }
    }
    else
    { # Neither decimal nor integer be: Must be a string
      # Much simpler width calculation - Just one simple comparison
      #
      if ( ($col_wid = length($split_ref->[$lc]))
                     > $pfile->{max_width}[$lc])
      { $pfile->{max_width}[$lc] = $col_wid; } # New widest this column
    }
  
  }
}
#
# Method print() - Print the beautified output
# Implicit parameter: [Reference to] the completely parsed file
sub print
{
  my $self = shift;
  my $lc;           # My usual loop counter

  for ($lc = 0; $lc < $self->{n_lines}; $lc++)
  {
    my $cur_col;
    my $cur_p_line = $self->{parsed_line}[$lc]; # ->Line object
    my $split_ref = $cur_p_line->{split_line};  # -> Array of cols
    if (! $cur_p_line->{has_delims})    # If line has no delimiters
    {
      printf ("%s\n", $split_ref->[0]); # Just print the line as is
      next;                             # and go the next parsed line
    }
    # Still here: then line has delimiters (majority of cases)
    #
    for ($cur_col = 0; $cur_col < $cur_p_line->{columns}; $cur_col++)
    {  # One column per round in this loop
      if ($cur_p_line->{type}[$cur_col] eq "s")
      {
        printf("%-*s%s",
               $self->{max_width}[$cur_col],
               $split_ref->[$cur_col],
               $self->{out_delim});
      }
      else
      { # Else, it is a numeric type - either d or f. I won't even look
        # at that but at the widest column and most decimal places
        #
        if ($self->{max_decimals}[$cur_col] == 0)
        { # No row had any decimal places in this column.  Format
          # intger at widest width with [default] right justification
          #
          printf("%*d%s",
                 $self->{max_width}[$cur_col],
                 $split_ref->[$cur_col],
                 $self->{out_delim});
        }
        else
        { # If even 1 row had decimal places in this column, format
          # this column accordingly for all rows.
          #
          printf("%*.*f%s",
                 $self->{max_width}[$cur_col],
                 $self->{max_decimals}[$cur_col],
                 $split_ref->[$cur_col],
                 $self->{out_delim});
        }
      }
    }   # End loop for one row
    printf("\n");   # Line feed after finishing line printout 
  }     # End loop for whole set of parsed lines
}   # End method print()
#
# package UNLbeautifier::Line:
# "Private" class used by class UNLbeautifier.  That class operates on
# a whole file.  UNLbeautifier_line operates on a single line structure.
#
package UNLbeautifier::Line;

# Constructor for 1 line-object.  Parameters:
# - The class (implicit)
# - The line (scalar) OR a reference to an array of scalars.
#   The scalar is more likely to be passed if the client is working
#   with ..unl data; the array reference is more likely if client is
#   fetching database data an passing it to this method.
# - A reference to the UNLbeautifier object to which this line belongs
#
sub new
{
  my $class  = shift(@_);   # (Implicitly passed class name)
  my $one_line =  shift(@_);
  my $p_file = shift(@_);   # The UNLbeautifier object reference
  my $self = {};            # Create new object
  bless ($self, $class);    # of this class
  $self->{split_line}[0] = "";  # Just to establish this field as array

  my ($in_delim, $in_split) # Just get local copies of delimiters
    = ($p_file->{in_delim}, $p_file->{in_split});

  if (ref($one_line) eq "ARRAY")    # If I received an array reference
  {                                 # copy the array into line object
    @{$self->{split_line}} = @{$one_line};          #  and set the
    $self->{columns} = $#{$self->{split_line}} + 1; # column count
    $self->{has_delims} = 1;        # Already separated - as good as
                                    # delimited.
  }
  else                      # Assume I got a scalar - a line
  {                         # More work: Split, check, repair, etc..
    chomp($one_line);
    $one_line =~ s/^\s+//;  # Trim leading spaces
    $one_line =~ s/\s+$//;  # Trim trailing spaces
    
    # If line has no delimiters, it is a blob-dump line, not to be
    # counted like a reguler UNL line.
    #
    $self->{has_delims} = 0;        # Initially assume line had no
    if ($one_line =~ $in_split)     # delims, but if I find one,
      {$self->{has_delims} = 1;}    # correct the assumption ASAP
  
    if ($self->{has_delims})
    {
      # Split the line but keep trailing null fields.
      #
      @{$self->{split_line}} = split($in_split, $one_line, -1);
      $util->repair_esc_delims(\@{$self->{split_line}}, $in_delim);
                                    # That is, undo overzealous splits
 # 
      # Now, is there a trailing delimiter in the original line? In a
      # .unl file, that is the last character of the line; there is no
      # field past that.  However, the split() function does not know
      # that and creates a bogus, null last field. I need to drop that
      # myself.
      # Also, if even one line has a final delimiter, flag whole file to
      # making sure there is one on every output line.
      #
      if (substr($one_line, (length($one_line) -1)) eq $in_delim)
      {
        $p_file->{has_end_delim} = 1;   # OK if this is set repeatedly
        pop @{$self->{split_line}};     # Lose the bogus last element
      }
      $self->{columns} = $#{$self->{split_line}} + 1;   # Column count
    }
    else  # If line has no delimiters
    {
      $self->{split_line}[0] = $one_line;   # Copy the line unparsed
      $self->{columns} = 1;                 # Exactly 1 column
    }  
  } # End of line-splitting code

  # Regardless of whether I got the split record or had to split it
  # myself, ttidy up fields by trimming leading & trailing spaces
  # Then track the size & formats of each field
  #
  for (my $nfield = 0;
       $nfield <= $#{$self->{split_line}};
       $nfield++)
  { 
    $self->{split_line}[$nfield] =~ s/^\s+//;    # Trim leading
    $self->{split_line}[$nfield] =~ s/\s+$//;    # Trim trailing

    # Now for the data types:
    # %d for integer
    # %f for decimal (float)
    # %s for anything else
    #
    if    ($self->{split_line}[$nfield] =~ $int_pattern)
      { $self->{type}[$nfield] = "d";}
    elsif ($self->{split_line}[$nfield] =~ $dec_pattern)
      { $self->{type}[$nfield] = "f"; }
    else
      {$self->{type}[$nfield] = "s";}

  }
  return $self;
}
#
sub ncolumns { my $self = shift(@_); return $self->{columns}; }

#
package UNLbeautifier::_util;

# Token constructor so that functions can be called like methods
#
sub new {my $self = {}; bless($self, $_[0]) ; return $self; }

# matches_meta(): Function to test if the given delimiter character is
#                 a known metacharacter.
# Returns 1 if it does match, 0 if it does not.
#
sub matches_meta
{
  shift(@_);                    # Don't need object reference; lose it
  my $delim_char = shift(@_);   # Get the parameter into a private var

  my $rval = 0;                 # Return value - Assume not a meta

  my $metachars = '|()[]{}^$*+?.';  # This is the list of metacharacters

  my $meta_length = length($metachars); # Loop limit

  for (my $lc = 0; $lc < $meta_length; $lc++)
  {
    if ($delim_char eq substr($metachars, $lc, 1)) {$rval = 1; last;}
  }

  return $rval;
}
#
# repair_esc_delims() - Scan up the array to look for columns that end
#   with an escape cahracter (\); this indicates that a delimiter was
#   intended to be part fo the scring and we hsould not have split it
#   up there.  We need to put back the delimiter and recombine the
#   split column with the following column.  The last column, the first
#   one I will check, cannot be recombined, of course.
#
# Parameters: (for now)
# - An array reference.
# - The delimiter to put back. 
#
sub repair_esc_delims
{
  shift(@_);                    # Don't need object reference; lose it
  my ($listref, $delim_p) = @_;

  for (my $lc = $#{$listref}; $lc >= 0; $lc--)
  {
    my $col_copy = $listref->[$lc]; # Copy to make code more readable
    my $col_length = length($col_copy) -1;  # O, length off by 1..

    # If column does not end in escape character(s), fuggeddaboudit!
    #
    next if ($col_copy !~ m/\\+$/);

    # AHA! Column does end in an escape. It may have been escaping a
    # delimiter in the original line.  Or it may itself be an escaped
    # escape.  How can I tell?  An odd number of \ clusters at end of
    # colum indicate an escape delimiter, requiring repair.  An even
    # number indicates escaped escape character.  Not my jurisdicion.
    #
    my $esc_count = $util->count_escapes($col_copy);
    next if (($esc_count % 2) == 0);    # Even number of esc; no problem

    # Odd number of escapes - need to effect repair of improper split.
    # o Put back the wrongly removed delimiter
    # o If this is not the last column in the array, append the succee-
    #   ding column to this one while splicing that succeeding column
    #   from the array.
    #
    $listref->[$lc] .= $delim_p;    # Putting back the delimiter
    if ($lc < $#{$listref})         # Cant splice after last element
    {
      $listref->[$lc] .=  splice(@{$listref}, $lc+1, 1);
    }
  } # End FOR (my $lc = $#{$listref}; $lc >= 0; $lc--)
}
#
# Function count_escapes: Counts contiguous escape characters at the
# end of the given string.
#
# Parameter:
# o The string
#
# Returns:
# o The number of consecutive escapes at end.
#
sub count_escapes
{
  shift(@_);                    # Don't need object reference; lose it
  my $instr = shift @_;

  my $len = length($instr) -1;
  my $lc = $len;        # Loop counter to start high, work down
  my $count = 0;        # Good place for a counter to start

  while (substr($instr, $lc--, 1) eq "\\") {($count++);}

  return $count;
}
#
1;  # End of all package definitions in this module
__END__
#

=pod

=head1 NAME

Package: UNLbeautifier::

    Produces neat columns from delimited data or from arrays of
    retrieved data, as from a database fetch

=head2 Author

=for text

Please send comments (and corrections to Perl issues) to:

Jacob Salomon

jakesalomon@yahoo.com

=head2 Date

$Date: 2010/08/04 05:30:45 $


=head1 Synopsys

  use UNLbeautifier;
  my $b_file = new UNLbeautifier;
  $b_file->set_in_delim(',');   # Tells $b_file that the input file
                                # will have it columns delimited by
                                # the comma. Default: '|' (AKA Pipe)
  $b_file->set_out_delim('^');  # Request to have the output columns
                                # delimited by a caret.
                                # Default: Same as the input delimiter
  
  $b_file->UNL_add_line($line); # Splits a line and places the columns
  $nl = $b_file + $line;        # in an array within the $b_file object
                                # Note the overloaded +; Prefer this.
  $b_file->UNL_add_line(\@list); # This method/+ also accepts an array
  $nl = $b_file + \@list;        # reference instead of a scalar string

  $b_file->print();             # Prints the beautified columns to
                                # stdout

Note that we are passing the *reference* to an array to +, not the array
itself.

=head2 What this package does

=for text

The purpose of this package is to make it easy to format neat columns
from data with unequal sized data.  This is the kind of data that come
from an unload utility available in most database prducts.  e.g. The
UNLOAD command in Informix's SQL or the dbexport utility in that same
product. The data comes out in uneven columns, delimited by some
character not normally found in text data.  Again, in the case of
Informix, this is normally the vertical var '|' AKA "Pipe".  The data
thus displayes is suitable for reloading but wholly unsuitable for
eyeballing, since it is difficult to follow the uneven columns down the
sheet. It is just not as easy to read as a spreadsheet.

UNLbeautifier to the rescue! If send your database output through this
package, the result will be as neat as a spreadhseet, with all data in
each column appropriately justified in a space as wide as the widest
data in that column.  But what do I mean by "appropriately justified"?

=over 4

=item - String data is left justified

=item - Integer (and other numeric) data is right justfied

=item - Decimal numbers are 0-padded so that the decimal alligns

=back

Note that if there is even one decimal number eg. - 123.4567 - in a
column of integers, all numbers in that column will be right-justified
with 4 decimal places when they print.

(At this time, the package recognizes hex data as string.)

There are two primary ways to send data to the package:

=over 4

=item - Send ASCII delimited lines of unloaded data through the package

=item - Send fetched rows of data, via a reference, through the package

=back

For the first method, there is a utility already in place:
beautify-unl.pl, available from the IIUG software repository.  It is
unlikely that an application program written in Perl would normally be

dealing with data in this format.  More likely, the application will be
fetching data, a row at a time, to send through, something like this:

=for code

while (@row_values = $curs_tabinfo->fetchrow_array)
{
  my $nl = $b_file + \@row_values;
}

=for text

That is, as you fetch a row of data into an array via a database cursor,
send [a refernce to] the row through the pakage by "adding" it to the
beautified file (AKA BNLbeautifier::) object.

When you run out of data, print the neat columns using the class
method:  $b_file->print().  At this time, only stdout is supported.
In a future release, a provision will be made for sending to an already
open file handle, a named flat file (passing the path), or a pipe to
another program.

=cut
