#!/usr/bin/perl -w
#
# flac2mp3.pl
#
# Version 0.2.5
#
# Converts a directory full of flac files into a corresponding
# directory of mp3 files
#
# Robin Bowes <robin@robinbowes.com>
#
# Revision History:
#
# v0.2.5
#  - Added better handling of complex frames, e.g. Comments
#  - use File::Find::Rule instead of custom recursive sub
#  - Moved command-line options into hash
#  - fix up SRC track number as well as DEST 
# v0.2.4
#  - Handle extended characters better (accents, etc.) [thanks Dan Sully]
#  - Don't reset timestamp of destination file
#  - Moved flags into a hash structures (%Options, %pflags}
#  - Other code restructuring
# v0.2.3
#  - Added --force option to force conversion
#  - files/directories now processed alphabetically
#  - Now handles Comments correctly (Comments are complex frames)
#  - Tidied up code with perltidy (http://perltidy.sf.net)
# v0.2.2
#  - Bug-fix: timestamp comparison not quite right
#  - Be more robust when converting files
#    (thanks Darren Warner <darren@dazwin.com>)
# v0.2.1
#  - Bug-fix: omitted File::Path module include
# v0.2.0
#  - Only create directories if files are found in them
#  - Make output less cluttered (only display filenames, not whole path)
#  - Changed command-line options.
#  - Major overhaul of tag handling. Now using MP3::Tag module
#    to write tags to mp3 files. Allows tags to be read separately
# v0.1.4
#  - Fix for files with multiple periods in filename, e.g. "01 - Back In
#    The U.S.S.R..flac" would be converted as "Back In The U.mp3"
#  - Fix for timestamp comparison (got it the right way round this
#    time!)
# v0.1.3
#  - added --quiet option to flac and lame commands
#  - only run conversion if dest file doesn't exist or if src file is
#    newer than dest file
#  - set modification time of dest file to same as src file
#  - check exit value of conversion command
#  - fixed problem with certain characters in file/directory name
#  - added rudimentary SIGINT handling
# v0.1.2
#  - Fixed filename quoting
# v0.1.1
#  - Changes to filename quoting
# v0.1.0
#  - Initial version
#
# To-do:
#  - Extend to apply all legal tags in FLAC field (esp. Composer, Conductor, etc.)
#  - Still falls over with certain characters in filename, e.g.
#    Rock `N' Roll Suicide.flac
#  - Clean up filepaths (check for double // in dirnames)
#  - Clean up and rationalise program output, e.g. produce a sensible
#    level of output by default unless a --quiet switch is used
#  - Investigate using Audio::FLAC::Decode and Audio::MPEG instead of
#    flac and lame programs used from system call
#  - Write OO Audio file objects that know how to decode/encode/get
#    tags/set tags for themselves - should open up the way to allow for
#    transcoding between additional formats.
#  - Write README file and bundle flac2mp3.pl into a tarball
#  - Write "Usage" method to document command-line switches
#  - google for a standard perl script template include things like
#    options, pod, GPL statement, how to set program version, etc. etc.
#  - Use Carp module
#  - Use Shell::Quote module

use strict;
use Audio::FLAC::Header;
use Data::Dumper;
use File::Basename;
use File::Find::Rule;
use File::Path;
use File::Spec;
use File::stat;
use Getopt::Long;
use MP3::Tag;

our $flaccmd = "/usr/bin/flac";
our $lamecmd = "/usr/bin/lame";

our @flacargs = qw (
  --decode
  --stdout
  --silent
);

# Should I build this array dynamically from command-line options
our @lameargs = qw (
  --preset standard
  --replaygain-accurate
  --quiet
);

# FLAC/MP3 tag/frame mapping
# Flac: 	ALBUM  ARTIST  TITLE  DATE  GENRE  TRACKNUMBER  COMMENT
# ID3v2:	ALBUM  ARTIST  TITLE  YEAR  GENRE  TRACK        COMMENT
# Frame:	TALB   TPE1    TIT2   TYER  TCON   TRCK         COMM

# hash mapping FLAC tag names to MP3 frames
our %MP3frames = (
    'ALBUM'       => 'TALB',
    'ARTIST'      => 'TPE1',
    'COMMENT'     => 'COMM',
    'DATE'        => 'TYER',
    'GENRE'       => 'TCON',
    'TITLE'       => 'TIT2',
    'TRACKNUMBER' => 'TRCK',
);

# Hash telling us which key to use if a complex frame hash is encountered
# For example, the COMM frame is complex and returns a hash with the
# following keys (with example values):
#   'Language'	    => 'ENG'
#   'Description'   => 'Short Text'
#   'Text'	    => 'This is the actual comment field'
#
# In this case, we want to grab the content of the 'Text' key.
our %Complex_Frame_Keys = (
    'COMM'	  => 'Text',
);

our %Options;

# Catch interupts (SIGINT)
$SIG{INT} = \&INT_Handler;

GetOptions( \%Options, "quiet!", "debug!", "tagsonly!", "force!" );

# info flag is the inverse of --quiet
$Options{info} = !$Options{quiet};

package main;

# Turn off output buffering (makes debugging easier)
$| = 1;

# Do I need to set the default value of any options?
# Or does GetOptions handle it?
# If I do, what's the "best" way to do it?

my ( $srcdirroot, $destdirroot ) = @ARGV;

showusage() if ( !defined $srcdirroot || !defined $destdirroot );

die "Source directory not found: $srcdirroot\n"
  unless -d $srcdirroot;

# count all flac files in srcdir
# Display a progress report after each file, e.g. Processed 367/4394 files
# Possibly do some timing and add a Estimated Time Remaining
# Will need to only count files that are going to be processed.
# Hmmm could get complicated.

$::Options{info} && msg("Processing directory: $srcdirroot\n");

my @flac_files = File::Find::Rule->file()
        ->name( '*.flac' )
        ->in( $srcdirroot );

$::Options{info} && msg("$#flac_files flac files found. Sorting...");

@flac_files = sort @flac_files;

$::Options{info} && msg("done.\n");

foreach my $srcfilename (@flac_files) {

    # get the directory containing the file
    my $srcRelPath = File::Spec->abs2rel( $srcfilename, $srcdirroot );
    my $destPath = File::Spec->rel2abs( $srcRelPath, $destdirroot );

    my ( $fbase, $destdir, $fext ) = fileparse( $destPath, '\.flac$' );
    my $destfilename = $destdir . $fbase . ".mp3";

    # Create the destination directory if it doesn't already exist
    mkpath($destdir)
      or die "Can't create directory $destdir\n"
      unless -d $destdir;

    convert_file( $srcfilename, $destfilename );
}

1;

sub showusage {
    print <<"EOT";
Usage: $0 [--quiet] [--debug] [--tagsonly] [--force] <flacdir> <mp3dir>
    --quiet         Disable informational output to stdout
    --debug         Enable debugging output. For developers only!
    --tagsonly      Don't do any transcoding - just update tags
    --force         Force transcoding and tag update even if not required
EOT
    exit 0;
}

sub msg {
    my $msg = shift;
    print "$msg";
}

sub convert_file {
    my ( $srcfilename, $destfilename ) = @_;

    # To do:
    #   Compare tags even if src and dest file have same timestamp
    #   Use command-line switches to override default behaviour

    # get srcfile timestamp
    my $srcstat = stat($srcfilename);
    my $deststat;

    $::Options{debug} && msg("srcfile: $srcfilename\n");
    $::Options{debug} && msg("destfile: $destfilename\n");

    # create object to access flac tags
    my $srcfile = Audio::FLAC::Header->new($srcfilename);

    # Get tags from flac file
    my $srcframes = $srcfile->tags();

    $::Options{debug} && print "Tags from source file:\n" . Dumper $srcframes;

    # hash to hold tags that will be updated
    my %changedframes;

    # weed out tags not valid in destfile
    foreach my $frame ( keys %$srcframes ) {
        if ( $MP3frames{$frame} ) {
            $changedframes{$frame} = $srcframes->{$frame};
        }
    }
    
    # Fix up TRACKNUMBER
    my $srcTrackNum = $changedframes{'TRACKNUMBER'} * 1;
    if ($srcTrackNum < 10) {
	$changedframes{'TRACKNUMBER'} = sprintf( "%02u", $srcTrackNum );
    }

    if ( $::Options{debug} ) {
        print "Tags we know how to deal with from source file:\n";
        print Dumper \%changedframes;
    }

    # Initialise file processing flags
    my %pflags = (
        exists    => 0,
        tags      => 0,
        timestamp => 1
    );

    # if destfile already exists
    if ( -e $destfilename ) {

        $pflags{exists} = 1;

        $::Options{debug} && msg("destfile exists: $destfilename\n");

        # get destfile timestamp
        $deststat = stat($destfilename);

        my $srcmodtime  = scalar $srcstat->mtime;
        my $destmodtime = scalar $deststat->mtime;

        if ( $::Options{debug} ) {
            print("srcfile mtime:  $srcmodtime\n");
            print("destfile mtime: $destmodtime\n");
        }

        # General approach:
        #   Don't process the file if srcfile timestamp is earlier than destfile
        #   or tags are different
        #
        # First check timestamps and set flag
        if ( $srcmodtime <= $destmodtime ) {
            $pflags{timestamp} = 0;
        }

        # If the source file os not newer than dest file
        if ( !$pflags{timestamp} ) {

            $Options{debug} && msg("Comparing tags\n");

            # Compare tags; build hash of changed tags;
            # if hash empty, process the file

            my $mp3 = MP3::Tag->new($destfilename);

            my @tags = $mp3->get_tags;

            $Options{debug} && print Dumper @tags;

            # If an ID3v2 tag is found
            my $ID3v2 = $mp3->{"ID3v2"};
            if ( defined $ID3v2 ) {

                $Options{debug} && msg("ID3v2 tag found\n");

                # loop over all valid destfile frames
                foreach my $frame ( keys %MP3frames ) {

                    $::Options{debug} && msg("frame is $frame\n");

                # To do: Check the frame is valid
		# Specifically, make sure the GENRE is one of the standard ID3 tags
                    my $method = $MP3frames{$frame};

                    $::Options{debug} && msg("method is $method\n");

                    # Check for tag in destfile
                    my ( $destframe, @info ) = $ID3v2->get_frame($method);
                    $destframe = '' if ( !defined $destframe );
		    
                    $::Options{debug}
                      && print Dumper $destframe, @info;
		    
		    my $dest_text;

		    # check for complex frame (e.g. Comments)
		    if (ref $destframe) {
			my $cfname = $Complex_Frame_Keys{$method};
			$dest_text = $$destframe{$cfname};
		    } else {
			$dest_text = $destframe
		    }

		    # Fix up TRACKNUMBER
                    if ( $frame eq "TRACKNUMBER" ) {
                        if ( $destframe < 10 ) {
                            $dest_text = sprintf( "%02u", $destframe );
                        }
                    }

                    # get tag from srcfile
                    my $srcframe = utf8toLatin1( $changedframes{$frame} );
                    $srcframe = '' if ( !defined $srcframe );

                    $::Options{debug} && msg("srcframe value: $srcframe\n");
                    $::Options{debug} && msg("destframe value: $dest_text\n");

                    # If set the flag if any frame is different
                    if ( $dest_text ne $srcframe ) {
                        $pflags{tags} = 1;
                    }
                }
            }
        }
    }

    if ( $::Options{debug} ) {
        msg("pf_exists:    $pflags{exists}\n");
        msg("pf_tags:      $pflags{tags}\n");
        msg("pf_timestamp: $pflags{timestamp}\n");
    }

    if ( $::Options{debug} ) {
        print "Tags to be written if tags need updating\n";
        print Dumper \%changedframes;
    }

    if (   !$pflags{exists}
        || $pflags{timestamp}
        || $pflags{tags}
        || $::Options{force} )
    {
        $::Options{info} && msg("Processing \"$srcfilename\"\n");

        if (
            $::Options{force}
            || ( !$::Options{tagsonly}
                && ( !$pflags{exists} || ( $pflags{exists} && !$pflags{tags} ) )
            )
          )
        {

            # Building command used to convert file (tagging done afterwards)
            # Needs some work on quoting filenames containing special characters
            my $quotedsrc       = $srcfilename;
            my $quoteddest      = $destfilename;
            my $convert_command =
                "$flaccmd @flacargs \"$quotedsrc\""
              . "| $lamecmd @lameargs - \"$quoteddest\"";

            $::Options{debug} && msg("$convert_command\n");

            # Convert the file
            my $exit_value = system($convert_command);

            $::Options{debug}
              && msg("Exit value from convert command: $exit_value\n");

            if ($exit_value) {
                msg("$convert_command failed with exit code $exit_value\n");

                # delete the destfile if it exists
                unlink $destfilename;

                # should check exit status of this command

                exit($exit_value);
            }

            # the destfile now exists!
            $pflags{exists} = 1;
        }

        # Write the tags to the converted file
        if (   $pflags{exists} && ( $pflags{tags} || $pflags{timestamp} )
            || $::Options{force} )
        {

            my $mp3 = MP3::Tag->new($destfilename);

            # Remove any existing tags
            $mp3->{ID3v2}->remove_tag if exists $mp3->{ID3v2};

            # Create a new tag
            $mp3->new_tag("ID3v2");

            foreach my $frame ( keys %changedframes ) {

                $::Options{debug} && msg("changedframe is $frame\n");

             # To do: Check the frame is valid
             # Specifically, make sure the GENRE is one of the standard ID3 tags
                my $method = $MP3frames{$frame};

                $::Options{debug} && msg("method is $method\n");

                # Convert utf8 string to Latin1 charset
                my $framestring = utf8toLatin1( $changedframes{$frame} );

                $::Options{debug} && msg("Setting $frame = $framestring\n");

                # COMM is a Complex frame so needs to be treated differently.
                if ( $method eq "COMM" ) {
                    $mp3->{"ID3v2"}
                      ->add_frame( $method, 'ENG', 'Short text', $framestring );
                }
                else {
                    $mp3->{"ID3v2"}->add_frame( $method, $framestring );
                }
            }

            $mp3->{ID3v2}->write_tag;

            $mp3->close();

     # should optionally reset the destfile timestamp to the same as the srcfile
     # utime $srcstat->mtime, $srcstat->mtime, $destfilename;
        }
    }
}

sub INT_Handler {
    my $signame = shift;
    die "Exited with SIG$signame\n";
}

sub utf8toLatin1 {
    my $data = shift;

    # Don't run the substitution on an empty string
    if ($data) {
        $data =~
          s/([\xC0-\xDF])([\x80-\xBF])/chr(ord($1)<<6&0xC0|ord($2)&0x3F)/eg;
        $data =~ s/[\xE2][\x80][\x99]/'/g;
    }

    return $data;
}

# vim:set softtabstop=4:
# vim:set shiftwidth=4:

__END__
