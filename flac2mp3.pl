#!/usr/bin/env perl
#
# $Id$
#
# Converts a directory full of flac files into a corresponding
# directory of mp3 files
#
# Robin Bowes <robin@robinbowes.com>
#
# Release History:
#  - See changelog.txt

use warnings;
use strict;
use FindBin;
use lib "$FindBin::Bin/lib";
use Audio::FLAC::Header;
use Carp;
use Data::Dumper;
use File::Basename;
use File::Find::Rule;
use File::Path;
use File::Spec;
use File::Temp qw/ cleanup /;
use File::Which;
use Getopt::Long;
use MP3::Tag;
use Proc::ParallelLoop;
use Scalar::Util qw/ looks_like_number /;

# ------- User-config options start here --------
# Assume flac and lame programs are in the path.
# If not, put full path to programs here.
#
# On Windows:
#  * If you specify a path, you must include the ".exe" extension
#  * Long filenames are OK, e.g. c:/Program Files/flac/flac.exe
#  * You can use "/" or "\\" or even "\" as path separator, e.g.:
#      c:\windows\system32\flac.exe
#    or
#      c:/windows/system32/flac.exe
#    or
#      c:\\windows\\system32\\flac.exe
our $flaccmd = 'flac';
our $lamecmd = 'lame';

# Modify lame options if required
our @lameargs = qw (
    --noreplaygain
    --vbr-new
    -V 2
    -h
    --nohist
    --quiet
);

our $num_cores = 1;

# -------- User-config options end here ---------

our @flacargs = qw (
    --decode
    --stdout
    --silent
);

# hash mapping FLAC tag names to MP3 frames
our %MP3frames = (
    'ALBUM' => 'TALB',

    #    'ALBUMARTIST'             => 'TPE2',
    'ARTIST'                  => 'TPE1',
    'BAND'                    => 'TPE2',
    'BPM'                     => 'TBPM',
    'COMMENT'                 => 'COMM',
    'COMPILATION'             => 'TCMP',
    'COMPOSER'                => 'TCOM',
    'CONDUCTOR'               => 'TPE3',
    'DATE'                    => 'TYER',
    'DISCNUMBER'              => 'TPOS',
    'GENRE'                   => 'TCON',
    'ISRC'                    => 'TSRC',
    'LYRICIST'                => 'TEXT',
    'PUBLISHER'               => 'TPUB',
    'TITLE'                   => 'TIT2',
    'TRACKNUMBER'             => 'TRCK',
    'MUSICBRAINZ_ALBUMID'     => 'TXXX',
    'MUSICBRAINZ_ALBUMSTATUS' => 'TXXX',
    'MUSICBRAINZ_ALBUMTYPE'   => 'TXXX',
    'MUSICBRAINZ_ARTISTID'    => 'TXXX',
    'MUSICBRAINZ_SORTNAME'    => 'TXXX',
    'MUSICBRAINZ_TRACKID'     => 'UFID',
    'MUSICBRAINZ_TRMID'       => 'TXXX',
    'MD5'                     => 'TXXX',

    'REPLAYGAIN_TRACK_PEAK' => 'TXXX',
    'REPLAYGAIN_TRACK_GAIN' => 'TXXX',
    'REPLAYGAIN_ALBUM_PEAK' => 'TXXX',
    'REPLAYGAIN_ALBUM_GAIN' => 'TXXX',
);

our %MP3frametexts = (
    'COMMENT'                   => 'Short Text',
    'MD5'                       => 'MD5',
    'MUSICBRAINZ_ALBUMARTISTID' => 'MusicBrainz Album Artist Id',
    'MUSICBRAINZ_ALBUMID'       => 'MusicBrainz Album Id',
    'MUSICBRAINZ_ALBUMSTATUS'   => 'MusicBrainz Album Status',
    'MUSICBRAINZ_ALBUMTYPE'     => 'MusicBrainz Album Type',
    'MUSICBRAINZ_ARTISTID'      => 'MusicBrainz Artist Id',
    'MUSICBRAINZ_SORTNAME'      => 'MusicBrainz Sortname',
    'MUSICBRAINZ_TRACKID'       => 'MusicBrainz Trackid',
    'MUSICBRAINZ_TRMID'         => 'MusicBrainz TRM Id',
    'REPLAYGAIN_TRACK_PEAK'     => 'REPLAYGAIN_TRACK_PEAK',
    'REPLAYGAIN_TRACK_GAIN'     => 'REPLAYGAIN_TRACK_GAIN',
    'REPLAYGAIN_ALBUM_PEAK'     => 'REPLAYGAIN_ALBUM_PEAK',
    'REPLAYGAIN_ALBUM_GAIN'     => 'REPLAYGAIN_ALBUM_GAIN',
);

# Hash telling us which key to use if a complex frame hash is encountered
# For example, the COMM frame is complex and returns a hash with the
# following keys (with example values):
#   'Language'      => 'ENG'
#   'Description'   => 'Short Text'
#   'Text'      => 'This is the actual comment field'
#
# In this case, we want to use the "Description" to check if this is the
# correct frame.
# We always grab the "Text" for the frame data.
our %Complex_Frame_Keys
    = ( 'COMM' => 'Description', 'TXXX' => 'Description', 'UFID' => '_Data' );

our %Options;

# Catch interupts (SIGINT)
$SIG{INT} = \&INT_Handler;

GetOptions(
    \%Options,   "quiet!",  "tagdiff",   "debug!",
    "tagsonly!", "force!",  "usage",     "help",
    "version",   "pretend", "skipfile!", "skipfilename=s",
    "processes=i"
);

# info flag is the inverse of --quiet
$::Options{info} = !$::Options{quiet};

if ( !exists $::Options{skipfilename} ) {
    $::Options{skipfilename} = 'flac2mp3.ignore';
}

if ( !exists $::Options{skipfile} ) {
    $::Options{skipfile} = 1;
}

if ( !exists $::Options{processes} ) {

    # if no user-option specified, use 1 as a default
    $::Options{processes} = 1;
}

package main;

# Turn off output buffering (makes debugging easier)
$| = 1;

# Do I need to set the default value of any options?
# Or does GetOptions handle it?
# If I do, what's the "best" way to do it?

my ( $srcdirroot, $destdirroot ) = @ARGV;

showversion() if ( $::Options{version} );
showhelp()    if ( $::Options{help} );
showusage()
    if ( !defined $srcdirroot
    or !defined $destdirroot
    or $::Options{processes} < 1
    or $::Options{usage} );

my $pretendString = '';
$pretendString = '** Pretending ** '
    if $::Options{pretend};

# Check flac and lame are found
# First see if the specified command is executable.
# If not, look in path
foreach my $cmd ( $flaccmd, $lamecmd ) {
    my $cmdpath;
    if ( -x $cmd ) {
        $cmdpath = $cmd;
    }
    else {
        $cmdpath = which($cmd);
    }
    croak "$cmd not found" unless $cmdpath;
    $::Options{info} && msg("Using $cmd from: $cmdpath");
}

# Convert directories to absolute paths
$srcdirroot  = File::Spec->rel2abs($srcdirroot);
$destdirroot = File::Spec->rel2abs($destdirroot);

die "Source directory not found: $srcdirroot\n"
    unless -d $srcdirroot;

# count all flac files in srcdir
# Display a progress report after each file, e.g. Processed 367/4394 files
# Possibly do some timing and add a Estimated Time Remaining
# Will need to only count files that are going to be processed.
# Hmmm could get complicated.

# Change directory into srcdirroot
chdir $srcdirroot;

$::Options{info}
    && msg( $pretendString . "Processing directory: $srcdirroot" );

# Now look for files in the current directory
# (following symlinks)

my @flac_files;
my $flac_list
    = File::Find::Rule->extras( { follow => 1 } )->name(qr/\.flac$/i);
if ( $::Options{skipfile} ) {
    my $skip_list = File::Find::Rule->directory->exec(
        sub {
            my ( $fname, $fpath, $frpath ) = @_;
            ### FIXME - need to use a platform neutral way to do this join
            if ( -f "$frpath/$::Options{skipfilename}" ) {
                return 1;
            }
            else {
                return 0;
            }
        }
    )->prune->discard;
    @flac_files
        = sort File::Find::Rule->or( $skip_list, $flac_list )->in('.');
}
else {

    @flac_files = sort $flac_list ->in('.');
}

$::Options{debug} && msg( Dumper(@flac_files) );

if ( $::Options{info} ) {
    my $file_count = @flac_files;  # array in scalar context returns no. items
    my $files_word = 'file';
    if ( $file_count > 1 ) {
        $files_word .= 's';
    }
    msg("$file_count flac $files_word found.\n");
    msg("Using $::Options{processes} transcoding processes.\n");
}

# Get directories from destdirroot and put in an array
my ( $dstroot_volume, $dstroot_dirpath, $dstroot_file )
    = File::Spec->splitpath( $destdirroot, 1 );
my @dstroot_dirs = File::Spec->splitdir($dstroot_dirpath);

# use parallel processing to launch multiple transcoding processes
pareach [@flac_files], sub {
    my $src_file = shift;

    # Get directories in src file and put in an array
    my ( $src_volume, $src_dirpath, $src_filename )
        = File::Spec->splitpath($src_file);
    my @src_dirs = File::Spec->splitdir($src_dirpath);

    # Join together dst_root and src_dirs
    my @dst_dirs;
    push @dst_dirs, @dstroot_dirs;
    push @dst_dirs, @src_dirs;

    # Join all the dst_dirs back together again
    my $dst_dirpath = File::Spec->catdir(@dst_dirs);

    # Get the basename of the src file
    my ( $src_base, $src_dir, $src_ext )
        = fileparse( $src_filename, qr{\.flac} );

    # Now join it all together to get the complete path of the dest_file
    my $dst_file = File::Spec->catpath( $dstroot_volume, $dst_dirpath,
        $src_base . '.mp3' );
    my $dst_dir = File::Spec->catpath( $dstroot_volume, $dst_dirpath, '' );

    convert_file( $src_file, $dst_dir, $dst_file );
}, { Max_Workers => $::Options{processes} };

1;

sub showusage {
    print <<"EOT";
Usage: $0 [--pretend] [--quiet] [--debug] [--tagsonly] [--force] [--tagdiff] [--noskipfile] [--skipfilename=<filename>] <flacdir> <mp3dir>
    --pretend       Don't actually do anything
    --quiet         Disable informational output to stdout
    --debug         Enable debugging output. For developers only!
    --tagsonly      Don't do any transcoding - just update tags
    --force         Force transcoding and tag update even if not required
    --tagdiff	    Print source/dest tag values if different
    --noskipfile    Ignore any skip files
    --skipfilename  Specify the name of the skip file [default: flac2mp3.ignore]
    --processes=n   Launch n parallel transcoding processes. Use with multi-core CPUs.
EOT
    exit 0;
}

sub msg {
    my $msg = shift;
    print "$msg\n";
}

sub convert_file {
    my ( $srcfilename, $dst_dir, $destfilename ) = @_;

    $::Options{debug} && msg("srcfile: '$srcfilename'");
    $::Options{debug} && msg("destfile: '$destfilename'");

    # create object to access flac tags
    my $srcfile = Audio::FLAC::Header->new($srcfilename);

    # get tags from flac file
    my $srcframes = $srcfile->tags();

    $::Options{debug} && msg "Tags from source file:\n" . Dumper $srcframes;

    # hash to hold tags that will be updated
    my %frames_to_update;

    # weed out tags not valid in destfile
    foreach my $frame ( keys %$srcframes ) {
        if ( $MP3frames{$frame} ) {

            # Multiple comments with the same name are returned as an array
            # Check for that here and convert the array to a null-separated
            # list to be compatible with mp3 tags
            my $src_frame_type = ref( $srcframes->{$frame} );

            # Check for normal string
            if ( !$src_frame_type ) {
                $frames_to_update{$frame}
                    = fixUpFrame( $srcframes->{$frame} );
            }
            else {
                if ( $src_frame_type eq 'ARRAY' ) {

                    # Fixup each value individually
                    map { $_ = fixUpFrame($_) } @{ $srcframes->{$frame} };

                    # join all values in null-separated list
                    $frames_to_update{$frame}
                        = join( "\000", @{ $srcframes->{$frame} } );
                }
                else {
                    carp "Unexpected source frame data type returned";
                }
            }
        }
    }

    # get MD5 checksdum from flac file and add to frames_to_update hash
    $frames_to_update{'MD5'} = $srcfile->info('MD5CHECKSUM');

    # Fix up TRACKNUMBER
    if ( $frames_to_update{'TRACKNUMBER'} ) {
        my $fixeduptracknumber
            = fixUpTrackNumber( $frames_to_update{'TRACKNUMBER'},
            $srcfilename );
        if ( $fixeduptracknumber ne $frames_to_update{'TRACKNUMBER'} ) {
            $frames_to_update{'TRACKNUMBER'} = $fixeduptracknumber;
        }
    }

    if ( $::Options{debug} ) {
        msg("Tags we know how to deal with from source file:");
        msg( Dumper \%frames_to_update );

    }

    # Initialise file processing flags
    my %pflags = (
        exists => 0,    # assume file doesn't exist
        md5    => 1,    # and the md5 checksum doesn't match
        tags   => 0,    # and the tags match (this will be set if tags
                        # don't match
    );

    # if destfile already exists
    if ( -e $destfilename ) {

        $pflags{exists} = 1;
        $::Options{debug} && msg("destfile exists: '$destfilename'");

  # General approach:
  #   Transcode the file if destfile md5 tag is different than the srcfile md5
  #   Update the tags if tags are different

        # Get tags from dst file and compare
        $::Options{debug} && msg("Comparing tags");

        # Compare tags; build hash of changed tags;

        my $mp3  = MP3::Tag->new($destfilename);
        my @tags = $mp3->get_tags;
        $::Options{debug} && msg( Dumper @tags );
        my $ID3v2 = $mp3->{"ID3v2"};

        # If an ID3v2 tag is found
        if ( defined $ID3v2 ) {

            $::Options{debug} && msg("ID3v2 tag found");

            # loop over all valid destfile frames
            foreach my $frame ( keys %MP3frames ) {

                $::Options{debug} && msg("frame is '$frame'");

           # To do: Check the frame is valid
           # Specifically, make sure the GENRE is one of the standard ID3 tags
                my $method = $MP3frames{$frame};

                $::Options{debug} && msg("method is '$method'");

           # Check for tag in destfile
           # 'intact' option makes sure that any embedded '\0' are not mangled
           # This is needed now we can handle multiple tags of the same type
                my ( $tagname, @info )
                    = $ID3v2->get_frames( $method, 'intact' );

                $::Options{debug}
                    && msg( "values from id3v2 tags:\n" . Dumper \$tagname,
                    \@info );

                my $dest_text = '';

                # check for complex frame (e.g. Comments)
            TAGLOOP:
                foreach my $tag_info (@info) {
                    if ( ref($tag_info) ) {
                        my $cfname = $MP3frametexts{$frame};
                        my $cfkey  = $Complex_Frame_Keys{$method};

                        if ( $$tag_info{$cfkey} eq $cfname ) {
                            $dest_text = $$tag_info{'Text'};
                            if ( $frame eq 'MD5' ) {
                                $pflags{md5} = (
                                    $frames_to_update{'MD5'} ne $dest_text );

                                if ( $::Options{debug} ) {
                                    msg("\$pflags{md5} is "
                                            . (
                                            $pflags{md5} ? 'set' : 'not set'
                                            )
                                    );

                                }
                            }
                            last TAGLOOP;
                        }
                    }
                    else {
                        $dest_text = $tag_info;
                    }
                }

                $::Options{debug}
                    && msg( "\$dest_text: " . Dumper $dest_text );

                # Fix up TRACKNUMBER
                if ( $frame eq 'TRACKNUMBER' ) {
                    my $fixeduptracknumber
                        = fixUpTrackNumber( $dest_text, $destfilename );
                    if ( $fixeduptracknumber ne $dest_text ) {
                        $dest_text = $fixeduptracknumber;
                    }
                }

                # get tag from srcfile
                my $srcframe = $frames_to_update{$frame};
                $srcframe = '' if ( !defined $srcframe );

                # Strip trailing spaces from src frame value
                # $srcframe =~ s/ *$//;

                # If set the flag if any frame is different
                if ( $dest_text ne $srcframe ) {
                    $pflags{tags} = 1;
                    if ( $::Options{tagdiff} ) {
                        msg("frame: '$frame'");
                        msg("srcframe value: '$srcframe'");
                        msg("destframe value: '$dest_text'");
                    }

                }
            }
        }
        else {

            # no ID2V2 object found so set the flag so the tags get written
            $pflags{tags} = 1;
        }
    }

    if ( $::Options{debug} ) {
        msg("pf_exists:    $pflags{exists}");
        msg("pf_tags:      $pflags{tags}");
        msg("pf_md5:       $pflags{md5}\n");
    }

    if ( $::Options{debug} ) {
        msg("Tags to be written if tags need updating\n");
        msg( Dumper \%frames_to_update );
    }

    # Building command used to convert file (tagging done afterwards)
    # Needs some work on quoting filenames containing special characters
    my $quotedsrc  = $srcfilename;
    my $quoteddest = $destfilename;

    if ( ( !$pflags{exists} || $pflags{md5} || $::Options{force} )
        && !$::Options{tagsonly} )
    {

        $::Options{info}
            && msg( $pretendString . "Transcoding \"$quotedsrc\"" );

        # Transcode to a temp file in the destdir.
        # Rename the file if the conversion completes sucessfully
        # This avoids leaving incomplete files in the destdir
        # If we're "pretending", don't create a File::Temp object
        my $tmpfilename;
        my $tmpfh;
        if ( $::Options{pretend} ) {
            $tmpfilename = $quoteddest;
        }
        else {

            # Create the destination directory if it
            # doesn't already exist
            mkpath($dst_dir)
                or die "Can't create directory $dst_dir\n"
                unless -d $dst_dir;
            $tmpfh = new File::Temp(
                UNLINK => 1,
                DIR    => $dst_dir,
                SUFFIX => '.tmp'
            );
            $tmpfilename = $tmpfh->filename;
        }

        my $convert_command = "\"$flaccmd\" @flacargs \"$quotedsrc\""
            . "| \"$lamecmd\" @lameargs - \"$tmpfilename\"";

        $::Options{debug} && msg("$convert_command");

        # Convert the file (unless we're pretending}
        my $exit_value;
        if ( !$::Options{pretend} ) {
            $exit_value = system($convert_command);
        }
        else {
            $exit_value = 0;
        }

        $::Options{debug}
            && msg("Exit value from convert command: $exit_value");

        if ($exit_value) {
            msg("$convert_command failed with exit code $exit_value");

            # delete the destfile if it exists
            unlink $destfilename;

            # should check exit status of this command

            exit($exit_value);
        }

        if ( !$::Options{pretend} ) {

            # If we get here, assume the conversion has succeeded
            $tmpfh->unlink_on_destroy(0);
            $tmpfh->close;
            croak "Failed to rename '$tmpfilename' to '$destfilename' $!"
                unless rename( $tmpfilename, $destfilename );

            # the destfile now exists!
            $pflags{exists} = 1;

            # and the tags need writing
            $pflags{tags} = 1;
        }
    }

    if ( $::Options{debug} ) {
        msg("pf_exists:    $pflags{exists}");
        msg("pf_tags:      $pflags{tags}");
        msg( "\$::Options{pretend}:   "
                . ( $::Options{pretend} ? 'set' : 'not set' ) );
    }

    # Write the tags
    if ($pflags{exists}
        && (   $pflags{tags}
            || $::Options{force} )
        )
    {

        $::Options{info}
            && msg( $pretendString . "Writing tags to \"$quoteddest\"" );

        if ( !$::Options{pretend} ) {
            my $mp3 = MP3::Tag->new($destfilename);

            # Remove any existing tags
            $mp3->{ID3v2}->remove_tag if exists $mp3->{ID3v2};

            # Create a new tag
            $mp3->new_tag("ID3v2");

            foreach my $frame ( keys %frames_to_update ) {

                $::Options{debug}
                    && msg("changedframe is '$frame'");

           # To do: Check the frame is valid
           # Specifically, make sure the GENRE is one of the standard ID3 tags
                my $method = $MP3frames{$frame};

                $::Options{debug} && msg("method is $method");

                # Convert utf8 string to Latin1 charset
                my $framestring = $frames_to_update{$frame};

                # Only add the frame if framestring is not empty
                if ( $framestring ne '' ) {
                    $::Options{debug}
                        && msg("Setting $frame = '$framestring'");

                    # COMM, TXX, and UFID are Complex frames that must be
                    # treated differently.
                    if ( $method eq "COMM" ) {
                        $mp3->{"ID3v2"}
                            ->add_frame( $method, 'ENG', 'Short Text',
                            $framestring );
                    }
                    elsif ( $method eq "TXXX" ) {
                        my $frametext = $MP3frametexts{$frame};
                        $frametext = $frame
                            if ( !( defined($frametext) ) );
                        $mp3->{"ID3v2"}->add_frame( $method, 0, $frametext,
                            $framestring );
                    }
                    elsif ( $method eq 'UFID' ) {
                        my $frametext = $MP3frametexts{$frame};
                        $mp3->{'ID3v2'}
                            ->add_frame( $method, $framestring, $frametext );
                    }
                    else {
                        $mp3->{"ID3v2"}->add_frame( $method, $framestring );
                    }
                }
            }

            $mp3->{ID3v2}->write_tag;

            $mp3->close();

# should we optionally reset the destfile timestamp to the same as the srcfile
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
        $data
            =~ s/([\xC0-\xDF])([\x80-\xBF])/chr(ord($1)<<6&0xC0|ord($2)&0x3F)/eg;
        $data =~ s/[\xE2][\x80][\x99]/'/g;
    }

    return $data;
}

sub fixUpFrame {
    my ($frameValue) = @_;
    $frameValue = utf8toLatin1($frameValue);
    $frameValue =~ s/ +$//;
    return $frameValue;
}

sub fixUpTrackNumber {
    my ( $trackNum, $filename ) = @_;

    # Check TRACKNUMBER tag is not empty
    if ($trackNum) {

        # Check TRACKNUMBER tag is numeric
        if ( looks_like_number($trackNum) ) {
            $trackNum = sprintf( "%02u", $trackNum );
        }
        else {
            $::Options{info}
                && msg("TRACKNUMBER not numeric in $filename");
        }
    }
    return $trackNum;
}

# vim:set softtabstop=4:
# vim:set shiftwidth=4:

__END__
