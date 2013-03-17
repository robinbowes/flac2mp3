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
use Carp;

use FindBin;
use lib "$FindBin::Bin/lib";

use Audio::FLAC::Header;
use Data::Dumper;
use Encode;
use File::Basename;
use File::Copy;
use File::Find::Rule;
use File::Path;
use File::Spec;
use File::Temp qw/ cleanup /;
use File::Which;
use Getopt::Long;
use MP3::Tag;
use Parallel::Forkmanager;
use Scalar::Util qw/ looks_like_number /;
use FreezeThaw qw/ cmpStr /;
use Digest::MD5;

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

my ($flaccmd, $lamecmd);
if ($^O eq 'MSWin32' or $^O eq 'MSWin64') {
	# This is an example of typical Windows filepaths. 
	# Change them if necessary:
	$flaccmd = q|C:\Program Files (x86)\FLAC\flac.exe|;
	$lamecmd = q|C:\Program Files\LAME\lame.exe|;
}
else {
	$flaccmd = 'flac';
	$lamecmd = 'lame';
}

# Modify lame options if required
my @lameargs = qw (
    --noreplaygain
    -V 2
    -h
    --nohist
    --quiet
);

# Use one process by default
my $NUM_PROCESSES_DEFAULT = 1;

# -------- User-config options end here ---------

# use Id3 v2.3.0 tag separator by default
my $TAG_SEPARATOR_DEFAULT = '/';

my @flacargs = qw (
    --decode
    --stdout
    --silent
);

# hash mapping FLAC tag names to MP3 frames
my %MP3frames = (
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
    'PIC'                     => 'APIC',

    'REPLAYGAIN_TRACK_PEAK' => 'TXXX',
    'REPLAYGAIN_TRACK_GAIN' => 'TXXX',
    'REPLAYGAIN_ALBUM_PEAK' => 'TXXX',
    'REPLAYGAIN_ALBUM_GAIN' => 'TXXX',
);

my %MP3frametexts = (
    'COMMENT'                   => '',
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
my %Complex_Frame_Keys = ( 'COMM' => 'Description', 'TXXX' => 'Description', 'UFID' => '_Data' );

# Catch interupts (SIGINT)
$SIG{INT} = \&INT_Handler;

# Set default options
my %Options = (
    skipfilename => 'flac2mp3.ignore',
    skipfile     => 1,
    processes    => $NUM_PROCESSES_DEFAULT,
    tagseparator => $TAG_SEPARATOR_DEFAULT
);

GetOptions(
    \%Options,     "quiet!",         "tagdiff", "debug!",  "tagsonly!", "force!",
    "usage",       "help",           "version", "pretend", "skipfile!", "skipfilename=s",
    "processes=i", "tagseparator=s", "lameargs=s", "copyfiles", "delete"
);

# info flag is the inverse of --quiet
$Options{info} = !$Options{quiet};

# Turn off output buffering (makes debugging easier)
$| = 1;

my ( $source_root, $target_root ) = @ARGV;

showversion() if ( $Options{version} );
showhelp()    if ( $Options{help} );
showusage()
    if ( !defined $source_root
    or !defined $target_root
    or $Options{processes} < 1
    or $Options{usage} );

@lameargs = $Options{lameargs}
    if $Options{lameargs};

my $pretendString = '';
$pretendString = '** Pretending ** '
    if $Options{pretend};

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
    msg_info("Using $cmd from: $cmdpath");
}

# Turn off unsyncing, due to broken implementation in
# some software (such as Logitech Media Server)
MP3::Tag->config(id3v23_unsync => 0);

# Convert directories to absolute paths
$source_root  = File::Spec->rel2abs($source_root);
$target_root = File::Spec->rel2abs($target_root);

die "Source directory not found: $source_root\n"
    unless -d $source_root;

# count all flac files in source_dir
# Display a progress report after each file, e.g. Processed 367/4394 files
# Possibly do some timing and add a Estimated Time Remaining
# Will need to only count files that are going to be processed.
# Hmmm could get complicated.

msg_info( $pretendString . "Processing directory: $source_root" );

# Now look for files in the source dir
# (following symlinks)
my %flac_mp3_files = get_all_paths('name', '.flac', $source_root, $target_root, '.mp3');
my @flac_files = sort keys { %flac_mp3_files };
	
my $file_count = scalar @flac_files;
msg_info( "Found $file_count flac file" . ( $file_count > 1 ? 's'  : '' . "\n" ) );

# If allowed, delete surplus files and folders from target directory, keeping 
# it in perfect sync with (i.e. a mirror of) the source directory.  
delete_excess_files_from_dest($source_root, $target_root) if ( $Options{delete} ) ;


# use parallel processing to launch multiple transcoding processes
msg_info("Using $Options{processes} transcoding processes.\n");
my $pm = new Parallel::ForkManager($Options{processes});
foreach my $src_file (@flac_files) {
	$pm->start and next; # Forks and returns the pid for the child
	path_and_conversion($src_file);	
	$pm->finish; # Terminates the child process
}
$pm->wait_all_children;


# If allowed, copy non-flac files to destination dirs
copy_non_flacs($source_root, $target_root) if ( $Options{copyfiles} );

1;
# ------------ Main program ends here --------------------------------------


# ------------ Subroutines start here --------------------------------------

sub delete_excess_files_from_dest {
	my ($source_root, $target_root) = @_;
	
	# Generate (source => target) hashes for the files found using 
	# each of the following combinations of root dirs and file suffixes 
	my %existing_target_mp3_files = get_all_paths('name', '.mp3', $target_root, $target_root, '.mp3');
	my %existing_source_mp3_files = get_all_paths('name', '.mp3', $source_root, $target_root, '.mp3');
	my %non_flac_files = get_all_paths('not_name', '.flac', $source_root, $target_root, '');
	my %existing_target_non_mp3_files = get_all_paths('not_name', '.mp3', $target_root, $target_root, '');
	
	# 1. calculate what files to expect in directory after finished transcoding and copying
	my @expected_transcoded_mp3s = keys { reverse %flac_mp3_files }; # expected mp3 files in target from transcoded flac files
	my @expected_copied_files = keys { reverse %non_flac_files }; # expected files in target copied from non-flac files in source
	my @expected_files = uniq(@expected_transcoded_mp3s, @expected_copied_files); # Join the arrays and remove any duplicates 
	
	# 2. check what files are actually present
	my @actual_mp3s = keys { reverse %existing_target_mp3_files }; # actual existing mp3 files in target
	my @actual_non_mp3s = keys { reverse %existing_target_non_mp3_files }; # existing non-mp3 files in target
	my @actual_files = (@actual_mp3s, @actual_non_mp3s); # Join the arrays (being mutually exclusive, there is no overlap) 
	
	# 3. determine which files to remove from target directory tree
	my @files_to_remove = single_difference(\@expected_files, \@actual_files);
	
	# 4. determine which subdirectories to remove from target directory tree
	my @expected_subdirs_in_target = get_all_dirs($source_root,$target_root);
	my @actual_subdirs_in_target = get_all_dirs($target_root,$target_root);
	my @dirs_to_remove = single_difference(\@expected_subdirs_in_target, \@actual_subdirs_in_target);
	
	# 5. carry out the deletions 
	foreach my $file (@files_to_remove) {
		$Options{pretend} || unlink $file or die "Unable to delete $file: $!";
		msg_info($pretendString . "Deleted \"$file\"");
	}
	foreach my $dir (reverse sort @dirs_to_remove) {
		$Options{pretend} || File::Path->remove_tree($dir) or die "Unable to delete directory $dir: $!";
		msg_info($pretendString . "Deleted directory \"$dir\"");
	}
}

# Return all unique elements of input array @_
sub uniq { 
	return sort keys %{{ map { $_ => 1 } @_ }} 
};

# Acccept two arrays @A and @B as argument, return elements in @B that aren't in @A.
sub single_difference { 
	my ($A, $B) = @_;

	# build lookup table
	my %seen = ();
	my @bonly = ();
	@seen{@$A} = (1) x @$A;
	foreach my $item (@$B) {
		push(@bonly, $item) unless $seen{$item};
	}
	return sort @bonly;
}

sub get_all_dirs {
	my ($root, $new_root) = @_;
	#	we supply no suffix, so we search for directories (not files):
	my @orig_dirs = @{ find_files_or_dirs($root) };

	my @dirs = ();
	foreach my $dir (@orig_dirs) {
		# strip source root dir from path...
		my $rel_path = File::Spec->abs2rel( $dir, $root );
		# then replace it with target root dir
		push @dirs, File::Spec->rel2abs( $rel_path, $new_root );
	}
	return sort @dirs;
}

sub get_all_paths {
	my ($rule, $suffix, $root, $new_root, $new_suffix) = @_;
	my @orig_files = @{ find_files_or_dirs($root, $rule, $suffix) };

	# Even if $root = $new_root, we need to do the following operations 
	# to get a consistent path format (otherwise problematic in e.g. MS Win) 
	# that is suitable for later string comparison: 
	my %paths = ();
	foreach my $src (@orig_files) {
		# Strip source root dir from file path
		my $rel_path = File::Spec->abs2rel( $src, $root );
		# ... then replace it with target root dir and change file suffix.
		($paths{$src} = File::Spec->rel2abs( $rel_path, $new_root ) ) =~ s{$suffix$}{$new_suffix}xmsi;
	}
	return %paths
}

sub find_files_or_dirs {
	my $path = shift;
	my $rule = shift;
    my $suffix = shift;
    
	# If a matching rule and file suffix is defined we are looking for files, 
	# otherwise we are looking for directories.
    my $found_list;
	if (defined $rule && defined $suffix) {
		$found_list = File::Find::Rule->file()->extras( { follow => 1 } )->$rule(qr{$suffix$}xmsi)
	}
	else {
		$found_list = File::Find::Rule->directory->extras( { follow => 1 } )
	};
	
	# skip any directories where a "skipfile" is found  
    my @found;
	if ( $Options{skipfile} && ($path eq $source_root) ) {
        my $skip_list = File::Find::Rule->directory->exec(
            sub {
                my ( $fname, $fpath, $frpath ) = @_;
                if ( -f File::Spec->catdir( $frpath, $Options{skipfilename} ) ) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
        )->prune->discard;
        @found = sort File::Find::Rule->or( $skip_list, $found_list )->in($path);
    }
    else {
        @found = sort $found_list ->in($path);		
    }
return \@found;
}

sub copy_non_flacs {
	my ($source_root, $target_root) = @_;
	
	my %non_flac_files = get_all_paths('not_name', '.flac', $source_root, $target_root, '');
    my @non_flac_files = keys %non_flac_files; 
    my $non_flac_file_count = scalar @non_flac_files;
    msg_info( "Found $non_flac_file_count non-flac file" . 
		( $non_flac_file_count != 1 ? 's'  : '' . "\n" ) );

    # Copy non-flac files from source to dest directories
    my $t0 = time;
    my $cntr_all = 0;
    my $cntr_copied = 0;
    foreach my $src_file (@non_flac_files) {
		my $dst_file = $non_flac_files{$src_file};
		# Flag which determines if file should be copied:
		my $do_copy = 1;
		# Don't copy file if it already exists in dest directory and 
		# has identical md5 to the source file   	
		if (-e $dst_file) {
			my $src_md5 = get_md5_of_non_flac_file($src_file);
			my $dst_md5 = get_md5_of_non_flac_file($dst_file);
			if ($src_md5 eq $dst_md5) {
				$do_copy = 0; # Don't copy if equal md5
			};
		}
		else {
			# Create the destination directory if it
			# doesn't already exist
			(undef, my $dst_dir) = 
				File::Basename::fileparse($dst_file); # retrieve directory name
			unless ( $Options{pretend} || -d $dst_dir ) {
				mkpath($dst_dir) or die "Can't create directory $dst_dir\n";
			}
		};
		if ( $do_copy ) {
			unless ( $Options{pretend} ) { 
				copy($src_file,$dst_file) || die("Can't copy this FILE: $src_file !");
			}
			$cntr_copied ++;
		};
		$cntr_all ++;
		# Show the progress every second
		if ( $Options{info} && 
			( ((time - $t0) >= 1) || ($cntr_all==$non_flac_file_count) ) ) {
			$t0 = time;
			print("\r" . $pretendString . $cntr_copied . 
				" non-flac files of " . $cntr_all ." were copied to dest directories.");
		};
    };
    msg_info("\n");	# double line feed
};

sub get_md5_of_non_flac_file {
    my $file = shift;
    open(FILE, $file) or die "Can't open '$file': $!";
    binmode(FILE);
    my $md5_code = Digest::MD5->new->addfile(*FILE)->hexdigest;
    close FILE;
    return $md5_code;
};

sub path_and_conversion{
    my $source = shift;
	my $target = $flac_mp3_files{$source};

    $Options{debug} && msg("source: '$source'");
    $Options{debug} && msg("target: '$target'");

    # Step 1: get tags from flac file
    my $source_tags = read_flac_tags($source);

    # Step 2: hash to hold tags that will be updated
    my $tags_to_update = preprocess_flac_tags( $source_tags );

    # Step 3: Initialise file processing flags
    my ($pflags) = examine_destfile_tags( $target, $tags_to_update );


	if ( ( !$$pflags{exists} || $$pflags{md5} || $Options{force} )
        && !$Options{tagsonly} ) {

		# Step 4: Transcode the file based on the processing flags
		transcode_file( $source, $target, $pflags );
	};

    # Step 5: Write the tags based on the processing flags
	write_tags( $target, $tags_to_update, $pflags );
};

sub showusage {
    print <<"EOT";
Usage: $0 [--pretend] [--quiet] [--debug] [--tagsonly] [--force] [--tagdiff] [--noskipfile] [--skipfilename=<filename>] [--lameargs='parameter-list'] <flacdir> <mp3dir>

    --pretend        Don't actually do anything
    --quiet          Disable informational output to stdout
    --debug          Enable debugging output. For developers only!
    --tagsonly       Don't do any transcoding - just update tags
    --force          Force transcoding and tag update even if not required
    --tagdiff	     Print source/dest tag values if different
    --lameargs='s'   specify parameter(string) to be passed to the LAME Encoder
                     Default: "--noreplaygain -V 2 -h --nohist --quiet"
    --noskipfile     Ignore any skip files
    --skipfilename   Specify the name of the skip file.
                     Default: flac2mp3.ignore
    --processes=n    Launch n parallel transcoding processes (does not work on Windows platform)
                     Use with multi-core CPUs.
                     Default: 1
    --tagseparator=s Use "s" as the separator to join multiple instances of the
                     same tag.
                     Default: "/"
    --copyfiles      Copy non-flac files to dest directories
    --delete         Delete surplus files and directories in destination, keeping in sync with source dir
EOT
    exit 0;
}

sub msg { 
	print "@_\n"
}

sub msg_info {
	# display only if "--quiet" option is not in use
	$Options{info} && msg(@_)
}

sub read_flac_tags {
    my $source = shift;

    # create object to access flac tags
    my $source_header = Audio::FLAC::Header->new($source);

    # get tags from flac file
    my $source_tags = $source_header->tags();

    # convert all tagnames to upper case
    %$source_tags = map { uc $_ => $source_tags->{$_} } keys %$source_tags;
    $Options{debug} && msg "Tags from source file:\n" . Dumper $source_tags;

    # get MD5 checksdum from flac file and add to srcframes hash
    $source_tags->{'MD5'} = $source_header->info('MD5CHECKSUM');
	
	# if present, add album art to srcframes hash:
	# get picture data from flac file and
	# proceed if a picture metadata block is found (i.e. a valid ref was returned)
	if ( ref(my $allsrcpictures = $source_header->picture('all')) ) {
		$source_tags->{'PIC'} = $allsrcpictures;
	};

    return $source_tags;
}

sub preprocess_flac_tags {
    my $source_tags   = shift;
    my %tags_to_update;

    # weed out tags not valid in destfile
    foreach my $frame ( keys %$source_tags ) {
        if ( $MP3frames{$frame} ) {

            # Multiple comments with the same name are returned as an array
            # Check for that here and convert the array to a null-separated
            # list to be compatible with mp3 tags
            my $src_tag_type = ref( $source_tags->{$frame} );

            # Check for normal string
            if ( !$src_tag_type ) {
                $tags_to_update{$frame} = fixUpFrame( $source_tags->{$frame} );
            }
            else {
	            if ( $frame eq 'PIC' ) {
		            foreach my $pic ( @{$source_tags->{'PIC'}} ) {
						$$pic{'description'} = fixUpFrame($$pic{'description'}); # convert from UTF-8 to latin1
					};
                	$tags_to_update{$frame} = $source_tags->{$frame};
            	}
                elsif ( $src_tag_type eq 'ARRAY' ) {

                    # Fixup each value individually
                    map { $_ = fixUpFrame($_) } @{ $source_tags->{$frame} };

                    # join all values, separated by the tagseparator string
                    $tags_to_update{$frame} = join( $Options{tagseparator}, @{ $source_tags->{$frame} } );
                }
                else {
                    carp "Unexpected source frame data type returned";
                }
            }
        }
    }

    # Fix up TRACKNUMBER
    if ( $tags_to_update{'TRACKNUMBER'} ) {
        my $fixeduptracknumber = fixUpTrackNumber( $tags_to_update{'TRACKNUMBER'} );
        if ( $fixeduptracknumber ne $tags_to_update{'TRACKNUMBER'} ) {
            $tags_to_update{'TRACKNUMBER'} = $fixeduptracknumber;
        }
    }

    if ( $Options{debug} ) {
        msg("Tags we know how to deal with from source file:");
        msg( Dumper \%tags_to_update );

    }

    return \%tags_to_update;
}

sub examine_destfile_tags {
    my $destfilename     = shift;
    my $frames_ref       = shift;
    my %frames_to_update = %$frames_ref;    # this is only to minimize changes

    # Initialise file processing flags
    my %pflags = (
        exists => 0,                        # assume file doesn't exist
        md5    => 1,                        # and the md5 checksum doesn't match
        tags   => 0,                        # and the tags match (this will be set if tags
                                            # don't match
    );

    # if destfile already exists
    if ( -e $destfilename ) {

        $pflags{exists} = 1;
        $Options{debug} && msg("destfile exists: '$destfilename'");

        # General approach:
        #   Transcode the file if destfile md5 tag is different than the srcfile md5
        #   Update the tags if tags are different

        # Get tags from dst file and compare
        $Options{debug} && msg("Comparing tags");

        # Compare tags; build hash of changed tags;

        my $mp3  = MP3::Tag->new($destfilename);
        my @tags = $mp3->get_tags;
        $Options{debug} && msg( Dumper @tags );
        my $ID3v2 = $mp3->{"ID3v2"};

        # If an ID3v2 tag is found
        if ( defined $ID3v2 ) {

            $Options{debug} && msg("ID3v2 tag found");

            # loop over all valid destfile frames
            foreach my $frame ( keys %MP3frames ) {

                $Options{debug} && msg("frame is '$frame'");

                # To do: Check the frame is valid
                # Specifically, make sure the GENRE is one of the standard ID3 tags
                my $method = $MP3frames{$frame};

                $Options{debug} && msg("method is '$method'");

                # Check for tag in destfile
                # 'intact' option makes sure that any embedded '\0' are not mangled
                # This is needed now we can handle multiple tags of the same type
                my ( $tagname, @info ) = $ID3v2->get_frames( $method, 'intact' );

                $Options{debug}
                    && msg( "values from id3v2 tags:\n" . Dumper \$tagname, \@info );

                # Compare album art
                if ( $frame eq 'PIC' ) {
					$pflags{tags} 
						= compare_src_dest_picture_data($frames_to_update{'PIC'},\@info,$destfilename);
					next; # don't do any more processing on the picture frame
				};

                my $dest_text = '';

                # check for complex frame (e.g. Comments)
            TAGLOOP:
                foreach my $tag_info (@info) {
                    if ( ref($tag_info) ) {
                        my $cfname = $MP3frametexts{$frame} || '';    # we may not know $frame
                        my $cfkey = $Complex_Frame_Keys{$method};

                        if ( $$tag_info{$cfkey} eq $cfname ) {
                            $dest_text = $$tag_info{'Text'};
                            if ( $frame eq 'MD5' ) {
                                $pflags{md5} = ( $frames_to_update{'MD5'} ne $dest_text );

                                if ( $Options{debug} ) {
                                    msg( "\$pflags{md5} is " . ( $pflags{md5} ? 'set' : 'not set' ) );

                                }
                            }
                            last TAGLOOP;
                        }
                    }
                    else {
                        $dest_text = $tag_info;
                    }
                }

                $Options{debug}
                    && msg( "\$dest_text: " . Dumper $dest_text );

                # Fix up TRACKNUMBER
                if ( $frame eq 'TRACKNUMBER' ) {
                    my $fixeduptracknumber = fixUpTrackNumber( $dest_text );
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
                    if ( $Options{tagdiff} ) {
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

    if ( $Options{debug} ) {
        msg("pf_exists:    $pflags{exists}");
        msg("pf_tags:      $pflags{tags}");
        msg("pf_md5:       $pflags{md5}\n");
    }

    if ( $Options{debug} ) {
        msg("Tags to be written if tags need updating\n");
        msg( Dumper \%frames_to_update );
    }

    return \%pflags;
}

sub transcode_file {
    my $source     = shift;
    my $target     = shift;
    my $pflags_ref = shift;
    my %pflags     = %$pflags_ref;    # this is only to minimize changes

	# Transcode to a temp file in the destdir.
	# Rename the file if the conversion completes sucessfully
	# This avoids leaving incomplete files in the destdir
	# If we're "pretending", don't create a File::Temp object
	my $tmpfilename;
	my $tmpfh;
	if ( $Options{pretend} ) {
		$tmpfilename = $target;
	}
	else {
		# retrieve destination directory name
		(undef, my $dst_dir) = File::Basename::fileparse($target);

		# Create the destination directory if it
		# doesn't already exist
		unless (-d $dst_dir) {
			# If necessary, allow a second check. Don't die just because the
			# dir was created by another child (race condition):
			mkpath($dst_dir) or (-d $dst_dir)
				or die "Can't create directory $dst_dir\n";
		};
		$tmpfh = new File::Temp(
			UNLINK => 1,
			DIR    => $dst_dir,
			SUFFIX => '.tmp'
		);
		$tmpfilename = $tmpfh->filename;
	}
	msg_info( $pretendString . "Transcoding    \"$source\"" );

	my $convert_command = "\"$flaccmd\" @flacargs \"$source\"" . "| \"$lamecmd\" @lameargs - \"$tmpfilename\"";

	$Options{debug} && msg("transcode: $convert_command");

	# Convert the file (unless we're pretending}
	my $exit_value;
	if ( !$Options{pretend} ) {
		$exit_value = system($convert_command);
	}
	else {
		$exit_value = 0;
	}

	$Options{debug}
		&& msg("Exit value from convert command: $exit_value");

	if ($exit_value) {
		msg("$convert_command failed with exit code $exit_value");

		# delete the destfile if it exists
		unlink $tmpfilename;

		# should check exit status of this command

		exit($exit_value);
	}

	if ( !$Options{pretend} ) {

		# If we get here, assume the conversion has succeeded
		$tmpfh->unlink_on_destroy(0);
		$tmpfh->close;
		croak "Failed to rename '$tmpfilename' to '$target' $!"
			unless rename( $tmpfilename, $target );

		# the destfile now exists!
		$pflags{exists} = 1;

		# and the tags need writing
		$pflags{tags} = 1;
	}


    if ( $Options{debug} ) {
        msg("pf_exists:    $pflags{exists}");
        msg("pf_tags:      $pflags{tags}");
        msg( "\$Options{pretend}:   " . ( $Options{pretend} ? 'set' : 'not set' ) );
	}

    %$pflags_ref = %pflags;    # this is only to minimize changes
}

sub write_tags {
    my $destfilename     = shift;
    my $frames_ref       = shift;
    my $pflags_ref       = shift;
    my %frames_to_update = %$frames_ref;    # this is only to minimize changes
    my %pflags           = %$pflags_ref;    # this is only to minimize changes

    # Write the tags
    if ( $pflags{exists} && ( $pflags{tags} || $Options{force} ) ) {

        msg_info( $pretendString . "Writing tags to \"$destfilename\"" );

        if ( !$Options{pretend} ) {
            my $mp3 = MP3::Tag->new($destfilename);

            # Remove any existing tags
            $mp3->{ID3v2}->remove_tag if exists $mp3->{ID3v2};

            # Create a new tag
            $mp3->new_tag("ID3v2");

            foreach my $frame ( keys %frames_to_update ) {

                $Options{debug}
                    && msg("changedframe is '$frame'");

                # To do: Check the frame is valid
                # Specifically, make sure the GENRE is one of the standard ID3 tags
                my $method = $MP3frames{$frame};

                $Options{debug} && msg("method is $method");

                if ( $method eq "APIC" ) {
					# Add the source picture data to APIC frames in the dest file			     
					$mp3 = picsToAPICframes($mp3, $frames_to_update{$frame});
					next; # avoid more processing of this complex tag, jump to next
         		};

                my $framestring = $frames_to_update{$frame};

                # Only add the frame if framestring is not empty
                if ( $framestring ne '' ) {
                    $Options{debug}
                        && msg("Setting $frame = '$framestring'");

                    # COMM, TXX, and UFID are Complex frames that must be
                    # treated differently.
                    my $frametext = $MP3frametexts{$frame};
                    if ( $method eq "COMM" ) {
                        $mp3->{"ID3v2"}->add_frame( $method, 'ENG', $frametext, $framestring );
                    }
                    elsif ( $method eq "TXXX" ) {
                        $frametext = $frame
                            if ( !( defined($frametext) ) );
                        $mp3->{"ID3v2"}->add_frame( $method, 0, $frametext, $framestring );
                    }
                    elsif ( $method eq 'UFID' ) {
                        $mp3->{'ID3v2'}->add_frame( $method, $framestring, $frametext );
                    }
                    else {
                        $mp3->{"ID3v2"}->add_frame( $method, $framestring );
                    }
                }
            }

            $mp3->{ID3v2}->write_tag or die("Couldn't write the ID3v2 tag to $destfilename!\n");

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

sub fixUpFrame {
    my ($frameValue) = @_;
    $frameValue = decode("utf8", $frameValue);
    $frameValue =~ s/ +$//;
    return $frameValue;
}

sub fixUpTrackNumber {
    my $trackNum = shift;

    # Check TRACKNUMBER tag is not empty
    if ($trackNum) {

        # Check TRACKNUMBER tag is numeric
        if ( looks_like_number($trackNum) ) {
            $trackNum = sprintf( "%02u", $trackNum );
        }
        else {
			msg_info('TRACKNUMBER not numeric');
        }
    }
    return $trackNum;
}

sub compare_src_dest_picture_data {	
	my ($allsrcpictures, $alldestpictures, $destfilename) = @_;

	# Create temporary MP3 id3v2 tag      
	my $mp3_tmp_pic = MP3::Tag->new($destfilename);
	$mp3_tmp_pic->new_tag("ID3v2");
		
	# Write APIC frames to temporary tag 
	$mp3_tmp_pic = picsToAPICframes($mp3_tmp_pic, $allsrcpictures);
	
	# Read back the APIC frames in a format which allows direct 
	# comparison with destination file data
	( my $tagname, my @alltmppictures ) =
		$mp3_tmp_pic->{"ID3v2"}->get_frames( "APIC", 'intact' );

	# Set 'tags don't match' flag to 1 if embedded picture data differs 
	# between source and destination files. Use cmpStr from FreezeThaw for this:
	my $pics_dont_match = cmpStr([@alltmppictures],[@$alldestpictures]);
	($Options{debug} || $Options{tagdiff}) && ($pics_dont_match) 
		&&	msg("Source and destination picture data NOT equal, " 
		. "will rewrite destination APIC frames.");		

	return $pics_dont_match
}

sub picsToAPICframes {
	# Write pictures to supplied mp3 tag
	my ($mp3_object, $allpics) = @_;
	if ($allpics)  {
		foreach my $thisPic ( @$allpics ) {
			my $imdata 	= $thisPic->{imageData};
			my $pictype = $thisPic->{pictureType};
			my @APICheader 
				= (0, $$thisPic{mimeType}, chr($pictype), $$thisPic{description});
			$mp3_object
				->{"ID3v2"}->add_frame("APIC", @APICheader, $imdata);
		};
	};
	return $mp3_object;
}

# vim:set softtabstop=4:
# vim:set shiftwidth=4:

__END__