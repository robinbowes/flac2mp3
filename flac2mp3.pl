#!/usr/bin/perl -w
#
# flac2mp3.pl
#
# Version 0.2.4
#
# Converts a directory full of flac files into a corresponding
# directory of mp3 files
#
# Robin Bowes <robin@robinbowes.com>
#
# Revision History:
#
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
use File::Find;
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

# Build this array dynamically from command-line options
# Add --quiet option by default?
our @lameargs = qw (
  --preset standard
  --replaygain-accurate
  --quiet
);

# FLAC/MP3 tag/frame mapping
# Flac: 	ALBUM  ARTIST  TITLE  DATE  GENRE  TRACKNUMBER  COMMENT
# ID3v2:	ALBUM  ARTIST  TITLE  YEAR  GENRE  TRACK        COMMENT
# Frame: TALB   TPE1    TIT2   TYER  TCON   TRCK         COMM

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

our %Options;

#$flag_info, $flag_debug, $flag_tagsonly, $flag_force;

# Catch interupts (SIGINT)
$SIG{INT} = \&INT_Handler;

GetOptions( \%Options, "quiet!", "debug!", "tagsonly!", "force!" );

#    "quiet!"    => \$flag_info,
#    "debug!"    => \$flag_debug,
#    "tagsonly!" => \$flag_tagsonly,
#    "force!"    => \$flag_force
#);

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
# Will need to only count files that are going to be processed. Hmmm could get complicated.

find_files( $srcdirroot, $destdirroot, $srcdirroot );

1;

sub find_files {
    my ( $srcdirroot, $destdirroot, $srcdir ) = @_;

    $::Options{info} && msg("Processing directory: $srcdir\n");

    # remove the src root directory from the beginning
    # of the current directory to give the additional
    # path information to be added to the destination root.
    # Try to create the new destination directory.
    ( my $extra_path = $srcdir ) =~ s/^\Q$srcdirroot\E//;
    my $destdir = $destdirroot . $extra_path;

    # get all directory entries
    opendir( SRCDIR, $srcdir ) or die "Couldn't open directory $srcdir\n";
    my @direntries = readdir(SRCDIR)
      or die "Couldn't read directory entries for directory $srcdir\n";
    closedir(SRCDIR);

    # get all target files within the present directory
    my @target_files = map { $_->[1] }    # extract pathnames
      map { [ $_, "$srcdir/$_" ] }        # form (name, path)
      sort                                # sort the entries (does this work?)
      grep { /\.flac$/ }                  # just flac files
      @direntries;

    # get all subdirs of the present directory
    my @subdirs = map { $_->[1] }         # extract pathnames
      grep { -d $_->[1] }                 # only directories
      map { [ $_, "$srcdir/$_" ] }        # form (name, path)
      sort                                # sort the entries (does this work?)
      grep { !/^\.\.?$/ }                 # not . or ..
      @direntries;

    # Create the destination directory if it doesn't already exist
    mkpath($destdir)
      or die "Can't create directory $destdir\n"
      unless -d $destdir;

    # process all files found in this directory
    foreach my $srcfilename (@target_files) {
        if ( $::Options{debug} ) {
            msg("target_files: ");
            print Dumper @target_files;
            $::Options{debug} && msg("srcfilename: $srcfilename\n");
            $::Options{debug} && msg("destdir: $destdir\n");
        }
        convert_file( $srcfilename, $destdir );

        # The following construct generates silly warnings so I've removed it
        # Suggested by Darren Warner <darren@dazwin.com>
        # eval convert_file($srcfilename, $destdir);
        # warn($@) if $@;
    }

    # process any subdirs of present directory
    foreach my $srcsubdir (@subdirs) {
        &find_files( $srcdirroot, $destdirroot, $srcsubdir );
    }
}

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
    my ( $srcfilename, $destdir ) = @_;

    # To do:
    #   Compare tags even if src and dest file have same timestamp
    #   Use command-line switches to override default behaviour

    # get srcfile timestamp
    my $srcstat = stat($srcfilename);
    my $deststat;

    my $srcRelPath = File::Spec->abs2rel( $srcfilename, $srcdirroot );

    my $destPath = File::Spec->rel2abs( $srcRelPath, $destdirroot );

    my ( $fbase, $fdir, $fext ) = fileparse( $destPath, '\.flac$' );
    my $destfilename = $fdir . $fbase . ".mp3";

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

    if ( $::Options{debug} ) {
        print "Tags we know how to deal with from source file:\n";
        print Dumper \%changedframes;
    }

    # File Processing flags
    my %pflags = (
       exists => 0,
       tags   => 0,
       timestamp => 1
    );

    $::Options{debug} && msg("destfilename: $destfilename\n");

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

        $::Options{debug} && msg("pf_timestamp: $pflags{timestamp}\n");

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
                    if (!defined $destframe) {
                    		$destframe = '';
                    	}

                    $::Options{debug} 
                      && print Dumper $destframe, @info; #msg("destframe: $destframe\ninfo: @info\n");

                    if ( $::Options{debug} ) {
                        my $framedata = $ID3v2->what_data( "", $method );
                        if ( ref $framedata ) {
                            if ( $#$framedata == 0 ) {
                                msg("$method is a text frame\n");
                            }
                            else {
                                msg("$method is a complex frame\n");
                            }
                        }
                        else {
                            msg("$method is an other frame\n");
                        }
                    }

          # Bug: AudioFile::Info returns track as numeric. FLAC returns a string
          # 3 <> 03 so the track tag will always appear modified
                        if ( $frame eq "TRACKNUMBER" ) {
                            if ( $destframe < 10 ) {
                                $destframe = sprintf( "%02u", $destframe );
                            }
                        }

                    # get tag from srcfile
                    my $srcframe = utf8toLatin1( $changedframes{$frame} );

                    $srcframe = '' if ( !defined $srcframe );

                    $::Options{debug} && msg("srcframe value: $srcframe\n");
                    $::Options{debug} && msg("destframe value: $destframe\n");

                    # If set the flag if any frame is different
                    if ( $destframe ne $srcframe ) {
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


    if ( !$pflags{exists} || $pflags{timestamp} || $pflags{tags} || $::Options{force} ) {
        $::Options{info} && msg("Processing \"$fbase$fext\"\n");

        if (
            $::Options{force}
            || ( !$::Options{tagsonly}
                && ( !$pflags{exists} || ( $pflags{exists} && !$pflags{tags} ) ) )
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
                print("$convert_command failed with exit code $exit_value\n");

                # delete the destfile if it exists
                unlink $destfilename;

                # should check exit status of this command

                exit($exit_value);
            }

            # the destfile now exists!
            $pflags{exists} = 1;
        }

        # Write the tags to the converted file
        if ( $pflags{exists} && ( $pflags{tags} || $pflags{timestamp} ) || $::Options{force} )
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
    	$data =~ s/([\xC0-\xDF])([\x80-\xBF])/chr(ord($1)<<6&0xC0|ord($2)&0x3F)/eg;
	    $data =~ s/[\xE2][\x80][\x99]/'/g;
	  }

    return $data;
}

# vim:set tabstop=3:

__END__
