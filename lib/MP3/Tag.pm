package MP3::Tag;

# Copyright (c) 2000-2004 Thomas Geffert.  All rights reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the Artistic License, distributed
# with Perl.

################
#
# provides a general interface for different modules, which can read tags
#
# at the moment MP3::Tag works with MP3::Tag::ID3v1 and MP3::Tag::ID3v2

use strict;
use File::Spec;

{
  package MP3::Tag::__hasparent;
  sub parent_ok {
    my $self = shift;
    $self->{parent} and $self->{parent}->proxy_ok;
  }
  sub get_config {
    my $self = shift;
    return $MP3::Tag::config{shift()} unless $self->parent_ok;
    return $self->{parent}->get_config(@_);
  }
}

use MP3::Tag::ID3v1;
use MP3::Tag::ID3v2;
use MP3::Tag::File;
use MP3::Tag::Inf;
use MP3::Tag::CDDB_File;
use MP3::Tag::ParseData;
use MP3::Tag::LastResort;

use vars qw/$VERSION %config/;
%config = ( autoinfo	=> [qw(ParseData ID3v2 ID3v1 CDDB_File Inf filename LastResort)],
	    cddb_files	=> [qw(audio.cddb cddb.out cddb.in)],
	    v2title	=> [qw(TIT1 TIT2 TIT3)],
	    extension	=> ['\.(?!\d+\b)\w{1,4}$'],
	    parse_data	=> [],
	    parse_split	=> ["\n"],
	    parse_filename_ignore_case => [1],
	    parse_filename_merge_dots => [1],
	    parse_join	=> ['; '],
	    year_is_timestamp	=> [1],
	    comment_remove_date	=> [0],
	    id3v2_frame_empty_ok	=> [0],
	    parse_minmatch => [0],
	  );

$VERSION="0.97";

=pod

=head1 NAME

MP3::Tag - Module for reading tags of MP3 audio files

=head1 SYNOPSIS

  use MP3::Tag;

  $mp3 = MP3::Tag->new($filename);

  # get some information about the file in the easiest way
  ($title, $track, $artist, $album, $comment, $year, $genre) = $mp3->autoinfo();
  $comment = $mp3->comment();

  # or have a closer look on the tags

  # scan file for existing tags
  $mp3->get_tags;

  if (exists $mp3->{ID3v1}) {
      # read some information from the tag
      $id3v1 = $mp3->{ID3v1};  # $id3v1 is only a shortcut for $mp3->{ID3v1}
      print $id3v1->title;
      
      # change the tag contents
      $id3v1->all("Song","Artist","Album",2001,"Comment",10,"Top 40");
      $id3v1->write_tag;
  }

  if (exists $mp3->{ID3v2}) {
      # read some information from the tag
      ($name, $info) = $mp3->{ID3v2}->get_frame("TIT2");
      # delete the tag completely from the file
      $mp3->{ID3v2}->remove_tag;
  } else {
      # create a new tag
      $mp3->new_tag("ID3v2");
      $mp3->{ID3v2}->add_frame("TALB", "Album title");
      $mp3->{ID3v2}->write_tag;
  }

  $mp3->close();

=head1 AUTHORS

Thomas Geffert, thg@users.sourceforge.net
Ilya Zakharevich, ilyaz@cpan.org

=head1 DESCRIPTION

Tag is a wrapper module to read different tags of mp3 files. 
It provides an easy way to access the functions of seperate modules
which do the handling of reading/writing the tags itself.

At the moment MP3::Tag::ID3v1 and MP3::Tag::ID3v2 are supported for read
and write; MP3::Tag::Inf, MP3::Tag::CDDB_File, MP3::Tag::File, MP3::Tag::LastResort are
supported for read access (the information obtained by parsing CDDB files,
F<.inf> file and the filename).

=over 4

=item new()

 $mp3 = MP3::Tag->new($filename);

Creates a mp3-object, which can be used to retrieve/set
different tags.

=cut

sub new {
    my $class = shift;
    my $filename = shift;
    my $mp3data;
    my $self = {};
    bless $self, $class;
    my $proxy = MP3::Tag::__proxy->new($self);
    if (-f $filename) {
	$mp3data = MP3::Tag::File->new_with_parent($filename, $proxy);
    }
    # later it should hopefully possible to support also http/ftp sources
    # with a MP3::Tag::Net module or something like that
    if ($mp3data) {
	%$self = (filename=>$mp3data,
		  ofilename => $filename,
		  abs_filename => File::Spec->rel2abs($filename),
		  __proxy => $proxy);
	return $self;
    }
    return undef;
}

{ # Proxy class: to have only one place where to weaken/localize the reference
  # $obj->[0] must be settable to the handle (not needed if weakening succeeds)
  package MP3::Tag::__proxy;
  use vars qw/$AUTOLOAD/;

  sub new {
    my ($class, $handle) = (shift,shift);
    my $self = bless [$handle], $class;
    return bless [], $class
	unless eval {require Scalar::Util; Scalar::Util::weaken($self->[0])};
    $self;
  }
  sub DESTROY {}
  sub proxy_ok { shift->[0] }
  sub AUTOLOAD {
    my $self = shift;
    die "local_proxy not initialized" unless $self->[0];
    (my $meth = $AUTOLOAD) =~ s/.*:://;
    my $smeth = $self->[0]->can($meth);
    die "proxy can't find the method $meth" unless $smeth;
    unshift @_, $self->[0];
    goto &$smeth;
  }
}

sub proxy_ok { 1 }		# We can always be a proxy to ourselves... ;-)

=pod

=item get_tags()

  [old name: getTags() . The old name is still available, but its use is not advised]

  @tags = $mp3->get_tags;

Checks which tags can be found in the mp3-object. It returns
a list @tags which contains strings identifying the found tags, like
"ID3v1", "ID3v2", "Inf", or "CDDB_File" (the last but one if the F<.inf>
information file with the same basename as MP3 file is found).

Each found tag can then be accessed with $mp3->{tagname} , where tagname is
a string returned by get_tags ;

Use the information found in L<MP3::Tag::ID3v1>, L<MP3::Tag::ID3v2> and
L<MP3::Tag::Inf>, L<MP3::Tag::CDDB_File> to see what you can do with the tags.

=cut 

################ tag subs

sub get_tags {
    my $self = shift;
    return @{$self->{gottags}} if exists $self->{gottags};
    my (@IDs, $id);

    # Will not create a reference loop
    local $self->{__proxy}[0] = $self unless $self->{__proxy}[0] or $ENV{MP3TAG_TEST_WEAKEN};
    for $id (qw(ParseData ID3v2 ID3v1 Inf CDDB_File LastResort)) {
	my $ref = "MP3::Tag::$id"->new_with_parent($self->{filename}, $self->{__proxy});
	next unless defined $ref;
	$self->{$id} = $ref;
	push @IDs, $id;
    }
    $self->{gottags} = [@IDs];
    return @IDs;
}

sub _get_tag {
    my $self = shift;
    $self->{shift()};
}

# keep old name for a while
*getTags = \&get_tags;

=pod

=item new_tag()

  [old name: newTag() . The old name is still available, but its use is not advised]

  $tag = $mp3->new_tag($tagname);

Creates a new tag of the given type $tagname. You
can access it then with $mp3->{$tagname}. At the
moment ID3v1 and ID3v2 are supported as tagname.

Returns an tag-object: $mp3->{$tagname}.

=cut

sub new_tag {
    my $self = shift;
    my $whichTag = shift;
    if ($whichTag =~ /1/) {
	$self->{ID3v1}= MP3::Tag::ID3v1->new($self->{filename},1);
	return $self->{ID3v1};
    } elsif ($whichTag =~ /2/) {
	$self->{ID3v2}= MP3::Tag::ID3v2->new($self->{filename},1);
	return $self->{ID3v2};
    }
}

# keep old name for a while
*newTag = \&new_tag;

#only as a shortcut to {filename}->close to explicitly close a file

=pod

=item close()

  $mp3->close;

You can use close() to explicitly close a file. Normally this is done
automatically by the module, so that you do not need to do this.

=cut

sub close {
    my $self=shift;
    $self->{filename}->close;
}

=pod

=item genres()

  $allgenres = $mp3->genres;
  $genreName = $mp3->genres($genreID);
  $genreID   = $mp3->genres($genreName);

Returns a list of all genres (reference to an array), or the according 
name or id to a given id or name.

This function is only a shortcut to MP3::Tag::ID3v1->genres.

This can be also called as MP3::Tag->genres;

=cut

sub genres {
  # returns all genres, or if a parameter is given, the according genre
  my $self=shift;
  return MP3::Tag::ID3v1::genres(shift);
}

=pod

=item autoinfo()

  ($title, $track, $artist, $album, $comment, $year, $genre) = $mp3->autoinfo();
  $info_hashref = $mp3->autoinfo();

autoinfo() returns information about the title, track number,
artist, album name, the file comment, the year and genre.  It can get this
information from an ID3v1-tag, an ID3v2-tag, from CDDB file, from F<.inf>-file,
and from the filename itself.

It will as default first try to find a ID3v2-tag to get this
information. If this cannot be found it tries to find a ID3v1-tag, then
to read an CDDB file, an F<.inf>-file, and
if these are not present either, it will use the filename to retrieve
the title, track number, artist, album name.  The comment, year and genre
are found differently, via the C<comment>, C<year> and C<genre> methods.

You can change the order of lookup with the config() command.

autoinfo() returns an array with the information or a hashref. The hash
has four keys 'title', 'track', 'artist' and 'album' where the information is
stored.  If comment, year or genre are found, the hash will have keys
'comment' and/or 'year' and/or 'genre' too.

If an optional argument C<'from'> is given, the returned values (title,
track number, artist, album name, the file comment, the year and genre) are
array references with the first element being the value, the second the
tag (C<ID3v2> or C<ID3v1> or C<Inf> or C<CDDB_File> or C<filename>) from which
it is taken.

(Deprecated name 'song' can be used instead of 'title' as well.)

=cut

sub autoinfo() {
    my ($self, $from) = (shift, shift);
    my (@out, %out);

    for my $elt ( qw( title track artist album comment year genre ) ) {
	my $out = $self->$elt($from);
	if (wantarray) {
	    push @out, $out;
	} elsif (defined $out and length $out) {
	    $out{$elt} = $out;
	}
    }
    $out{song} = $out{title} if exists $out{title};

    return wantarray ? @out : \%out;
}

=item comment()

  $comment = $mp3->comment();		# empty string unless found

comment() returns comment information. It can get this information from an
ID3v1-tag, or an ID3v2-tag (from C<COMM> frame with empty <short> field),
CDDB file (from C<EXTD> or C<EXTT> fields), or F<.inf>-file (from
C<Trackcomment> field).

It will as default first try to find a ID3v2-tag to get this
information. If no comment is found there, it tries to find it in a ID3v1-tag,
if none present, will try CDDB file, then F<.inf>-file.  It returns an empty string if
no comment is found.

You can change the order of this with the config() command.

If an optional argument C<'from'> is given, returns an array reference with
the first element being the value, the second the tag (ID3v2 or ID3v1) from
which the value is taken.

=cut

=item year()

  $year = $mp3->year();		# empty string unless found

year() returns the year information. It can get this information from an
ID3v2-tag, or ID3v1-tag, or F<.inf>-file, or filename.

It will as default first try to find a ID3v2-tag to get this
information. If no year is found there, it tries to find it in a ID3v1-tag,
if none present, will try CDDB file, then F<.inf>-file,
then by parsing the file name. It returns an empty string if no year is found.

You can change the order of this with the config() command.

If an optional argument C<'from'> is given, returns an array reference with
the first element being the value, the second the tag (ID3v2 or ID3v1 or
filename) from which the value is taken.

=item comment_collection(), comment_track(), title_track(). author_collection()

access the corresponding fields returned by parse() method of CDDB_File.

=cut

my %ignore_0length = qw(ID3v1 1 CDDB_File 1 Inf 1);

sub auto_field($;$) {
    my ($self, $elt, $from) = (shift, shift, shift);
    local $self->{__proxy}[0] = $self unless $self->{__proxy}[0] or $ENV{MP3TAG_TEST_WEAKEN};

    my $parts = $self->get_config($elt) || $self->get_config('autoinfo');
    $self->get_tags;

    foreach my $part (@$parts) {
	next unless exists $self->{$part};
	next unless defined (my $out = $self->{$part}->$elt());
	# Ignore 0-length answers from ID3v1, CDDB_File, and Inf
	next if not length $out and $ignore_0length{$part};	# Return empty...
	return [$out, $part] if $from;
	return $out;
    }
    return '';
}

for my $elt ( qw( title track artist album comment year genre ) ) {
  no strict 'refs';
  *$elt = sub (;$) {
    my $self = shift;
    my $translate = ($self->get_config("translate_$elt") || [])->[0] || sub {$_[1]};
    return &$translate($self, $self->auto_field($elt, @_));
  }
}

for my $elt ( qw( comment_collection comment_track title_track artist_collection ) ) {
  no strict 'refs';
  my ($tr) = ($elt =~ /^(\w+)_/);
  *$elt = sub (;$) {
    my $self = shift;
    local $self->{__proxy}[0] = $self unless $self->{__proxy}[0] or $ENV{MP3TAG_TEST_WEAKEN};
    $self->get_tags;
    return unless exists $self->{CDDB_File};
    my $translate = ($self->get_config("translate_$tr") || [])->[0] || sub {$_[1]};
    return &$translate( $self, $self->{CDDB_File}->parse($elt) );
  }
}

=item genre()

  $genre = $mp3->genre();		# empty string unless found

genre() returns the genre string. It can get this information from an
ID3v2-tag or ID3v1-tag.

It will as default first try to find a ID3v2-tag to get this
information. If no genre is found there, it tries to find it in a ID3v1-tag,
if none present, will try F<.inf>-file,
It returns an empty string if no genre is found.

You can change the order of this with the config() command.

If an optional argument C<'from'> is given, returns an array reference with
the first element being the value, the second the tag (ID3v2 or ID3v1 or
filename) from which the value is taken.


=item config

  MP3::Tag->config(item => value1, value2...);	# Set options globally
  $mp3->config(item => value1, value2...);	# Set object options

When object options are first time set or get, the global options are
propagated into object options.  (So if global options are changed later, these
changes are not inherited.)

Possible items are:

=over

=item autoinfo

Configure the order in which ID3v1-, ID3v2-tag and filename are used
by autoinfo.  Options can be "ID3v1", "ID3v2", "CDDB_File", "Inf", "filename".
The order
in which they are given to config also sets the order how they are
used by autoinfo. If an option is not present, it will not be used
by autoinfo (and other auto-methods if the specific overriding config
command were not issued).

  $mp3->config("autoinfo","ID3v1","ID3v2","filename");

sets the order to check first ID3v1, then ID3v2 and at last the
Filename

  $mp3->config("autoinfo","ID3v1","filename","ID3v2");

sets the order to check first ID3v1, then the Filename and last
ID3v2. As the filename will be always present ID3v2 will here
never be checked.

  $mp3->config("autoinfo","ID3v1","ID3v2");

sets the order to check first ID3v1, then ID3v2. The filename will
never be used.

=item title artist album year comment track genre

Configure the order in which ID3v1- and ID3v2-tag are used
by the corresponding methods (e.g., comment()).  Options can be
"ID3v1", "ID3v2", "Inf", "CDDB_File", "filename". The order
in which they are given to config also sets the order how they are
used by comment(). If an option is not present, then C<autoinfo> option
will be used instead.

=item  extension

regular expression to match the file extension (including the dot).  The
default is to match 1..4 letter extensions which are not numbers.

=item parse_data

the data used by L<MP3::Tag::ParseData> handler; each option is an array
reference of the form C<[$flag, $string, $pattern1, ...]>.  All the options
are processed in the following way: patterns are matched against $string
until one of them succeeds; the information obtained from later options takes
precedence over the information obtained from earlier ones.

=item  parse_split

The regular expression to split the data when parsing with C<n> or C<l> flags.

=item  parse_filename_ignore_case

If true (default), calling parse() and parse_rex() with match-filename
escapes (such as C<%=D>) matches case-insensitively.

=item  parse_filename_merge_dots

If true (default), calling parse() and parse_rex() with match-filename
escapes (such as C<%=D>) does not distinguish a dot and many consequent
dots.

=item  parse_join

string to put between multiple occurences of a tag in a parse pattern;
defaults to C<'; '>.  E.g., parsing C<'1988-1992, Homer (LP)'> with pattern
C<'%c, %a (%c)'> results in comment set to C<'1988-1992; LP'> with the
default value of C<parse_join>.

=item  v2title

Configure the elements of ID3v2-tag which are used by ID3v2::title().
Options can be "TIT1", "TIT2", "TIT3"; the present values are combined.
If an option is not present, it will not be used by ID3v2::title().

=item  cddb_files

List of files to look for in the directory of MP3 file to get CDDB info.

=item  year_is_timestamp

If TRUE (default) parse() will match complicated timestamps against C<%y>;
for example, C<2001-10-23--30,2002-02-28> is a range from 23rd to 30th of
October 2001, I<and> 28th of February of 2002.  According to ISO, C<--> can
be replaced by C</> as well.  For convenience, the leading 0 can be omited
from the fields which ISO requires to be 2-digit.

=item  comment_remove_date

When extracting the date from comment fields, remove the recognized portion
even if it is human readable (e.g., C<Recorded on 2014-3-23>) if TRUE.
Current default: FALSE.

=item  id3v2_frame_empty_ok

When setting the individual id3v2 frames via ParseData, do not
remove the frames set to an empty string.  Default 0 (empty means 'remove').

=item  translate_*

A subroutine used to munch a field C<*> (out of C<title track artist album comment year genre>)
Takes two arguments: the MP3::Tag object, and the current value of the field.

The second argument may also have the form C<[value, handler]>, where C<handler>
is the string indentifying the handler which returned the value.

=item id3v2_missing_fatal

If TRUE, interpolating ID3v2 frames (e.g., by C<%{TCOM}>) when
the ID3v2 tags is missing is a fatal error.  If false (default), in such cases
interpolation results in an empty string.

=item parse_minmatch

may be 0, 1, or a list of C<%>-escapes (matching any string) which should
matched non-greedily by parse() and friends.  E.g., parsing 
C<'Adagio - Andante - Piano Sonata'> via C<'%t - %l'> gives different results
for the settings 0 and 1; note that greediness of C<%l> does not matter,
thus the value of 1 is equivalent for the value of C<t> for this particular
pattern.

=item *

Later there will be probably more things to configure.

=over

=cut

my $conf_rex;

sub config {
    my ($self, $item, @options) = @_;
    $item = lc $item;
    my $config = ref $self ? ($self->{config} ||= {%config}) : \%config;
    my @known = qw(autoinfo title artist album year comment track genre
		   v2title cddb_files force_interpolate parse_data parse_split
		   parse_join parse_filename_ignore_case
		   parse_filename_merge_dots year_is_timestamp
		   comment_remove_date extension id3v2_missing_fatal
		   parse_minmatch);
    my @tr = map "translate_$_", qw( title track artist album comment year genre );
    $conf_rex = '^(' . join('|', @known, @tr) . ')$' unless $conf_rex;

    if ($item =~ /^(force)$/) {
	return $config->{$item} = {@options};
    } elsif ($item !~ $conf_rex) {
	warn "MP3::Tag::config(): Unknown option '$item' found; known options: @known @tr\n";
	return;
    }

    $config->{$item} = \@options;
}

=item get_config

  $opt = $mp3->get_config("item");

When object options are first time set or get, the global options are
propagated into object options.  (So if global options are changed later, these
changes are not inherited.)

=cut

sub get_config ($$) {
    my ($self, $item) = @_;
    my $config = ref $self ? ($self->{config} ||= {%config}) : \%config;
    $config->{lc $item};
}

=item pure_filetags

  $data = $mp3->pure_filetags()->autoinfo;

Configures $mp3 to not read anything except the pure ID3v2 or ID3v1 tags, and
do not postprocess them.  Returns the object reference itself to simplify
chaining of method calls.

=cut

sub pure_filetags ($) {
    my $self = shift;
    for my $c (qw(autoinfo title artist album year comment track genre)) {
	$self->config($c,"ID3v2","ID3v1");
    }
    $self->config('comment_remove_date', 0);
    for my $k (%{$self->{config}}) {
	delete $self->{config}->{$k} if $k =~ /^translate_/;
    }
    return $self;
}

=item get_user

  $data = $mp3->get_user($n);	# n-th piece of user scratch space

Queries an entry in a scratch array ($n=3 corresponds to C<%{U3}>).

=item set_user

  $mp3->set_user($n, $data);	# n-th piece of user scratch space

Sets an entry in a scratch array ($n=3 corresponds to C<%{U3}>).

=cut

sub get_user ($$) {
    my ($self, $item) = @_;
    unless ($self->{userdata}) {
        local $self->{__proxy}[0] = $self unless $self->{__proxy}[0] or $ENV{MP3TAG_TEST_WEAKEN};
	$self->{ParseData}->parse('track');	# Populate the hash if possible
	$self->{userdata} ||= [];
    }
    return unless defined (my $d = $self->{userdata}[$item]);
    $d;
}

sub set_user ($$$) {
    my ($self, $item, $val) = @_;
    $self->{userdata} ||= [];
    $self->{userdata}[$item] = $val;
}

=item set_id3v2_frame

  $mp3->set_id3v2_frame($name, @values);

When called with only $name as the argument, removes the specified
frame (if it existed).  Otherwise sets the frame passing the specified
@values to the add_frame() function of MP3::Tag::ID3v2.

=cut

# With two elements, removes frame
sub set_id3v2_frame ($$;@) {
    my ($self, $item) = (shift, shift);
    $self->get_tags;
    return if not @_ and not exists $self->{ID3v2};
    $self->new_tag("ID3v2") unless exists $self->{ID3v2};
    $self->{ID3v2}->remove_frame($item)
      if defined $self->{ID3v2}->get_frame($item);
    return unless @_;
    return $self->{ID3v2}->add_frame($item, @_);
}

=item get_id3v2_frames

  ($descr, @frames) = $mp3->get_id3v2_frames($fname);

Returns the specified frame(s); has the same API as
L<MP3::Tag::ID3v2::get_frames>, but also returns undef if no ID3v2
tag is present.

=cut

sub get_id3v2_frames ($$;$) {
    my ($self) = (shift);
    $self->get_tags;
    return if not exists $self->{ID3v2};
    $self->{ID3v2}->get_frames(@_);
}

=item is_id3v2_modified

  $frame = $mp3->is_id3v2_modified();

Returns TRUE if ID3v2 tag exists and was modified after creation.

=cut

sub is_id3v2_modified ($$;@) {
    my ($self) = (shift);
    return if not exists $self->{ID3v2};
    $self->{ID3v2}->is_modified();
}

=item select_id3v2_frame

  $frame = $mp3->select_id3v2_frame($fname, $descrs, $langs [, $VALUE]);

Returns the specified frame(s); has the same API as
L<MP3::Tag::ID3v2::frame_select> (args are frame name, list of wanted
Descriptors, list of wanted Languages, and possibly the new contents - with
C<undef> meaning deletion).  For read-only access it returns C<undef> if no
ID3v2 tag is present.

=item have_id3v2_frame

  $have_it = $mp3->have_id3v2_frame($fname, $descrs, $langs);

Returns TRUE the specified frame(s) exist; has the same API as
L<MP3::Tag::ID3v2::frame_have> (args are frame name, list of wanted
Descriptors, list of wanted Languages).

=item get_id3v2_frame_ids

  $h = $mp3->get_id3v2_frame_ids();
  print "  $_ => $h{$_}" for keys %$h;

Returns a hash reference with the short names of ID3v2 frames present
in the tag as keys (and long description of the meaning as values), or
FALSE if no ID3v2 tag is present.  See
L<MP3::Tags::ID3v2::get_frame_ids> for details.

=cut

sub select_id3v2_frame ($$;@) {
    my ($self) = (shift);
    $self->get_tags;
    if (not exists $self->{ID3v2}) {
	return if @_ <= 3 or not defined $_[3];	# Read access, or deletion
	$self->new_tag("ID3v2");
    }
    $self->{ID3v2}->frame_select(@_);
}

sub have_id3v2_frame ($$;@) {
    my ($self) = (shift);
    $self->get_tags;
    return if not exists $self->{ID3v2};
    $self->{ID3v2}->frame_have(@_);
}

sub get_id3v2_frame_ids ($$) {
    my ($self) = (shift);
    $self->get_tags;
    return if not exists $self->{ID3v2};
    $self->{ID3v2}->get_frame_ids(@_);
}

=item interpolate

  $string = $mp3->interpolate($pattern)

interpolates C<%>-escapes in $pattern using the information from $mp3 tags.
The syntax of escapes is similar to this of sprintf():

  % [ [FLAGS] MINWIDTH] [.MAXWIDTH] ESCAPE

The only recognized FLAGS are C<-> (to denote left-alignment inside MINWIDTH-
wide field), C<' '> (SPACE), and C<0> (denoting the fill character to use), as
well as an arbitrary character in parentheses (which becomes the fill
character).  MINWIDTH and MAXWIDTH should be numbers.

The one-letter ESCAPEs are replaced by

		% => literal '%'
		t => title
		a => artist
		l => album
		y => year
		g => genre
		c => comment
		n => track
		f => filename without the directory path
		F => filename with the directory path
		D => the directory path of the filename
		E => file extension
		e => file extension without the leading dot
		A => absolute filename without extension
		B => filename without the directory part and extension
		N => filename as originally given without extension

		v	mpeg_version
		L	mpeg_layer_roman
		r	bitrate_kbps
		q	frequency_kHz
		Q	frequency_Hz
		S	total_secs_int
		m	total_mins
		s	leftover_secs
		C	is_copyrighted_YN
		p	frames_padded_YN
		o	channel_mode
		u	frames


Additionally, ESCAPE can be a string (with all backslashes and curlies escaped)
enclosed in curly braces C<{}>.  The interpretation is the following:

=over 4

=item *

C<d>I<NUMBER> is replaced by I<NUMBER>-th component of the directory name (with
0 corresponding to the last component).

=item *

C<U>I<NUMBER> is replaced by I<NUMBER>-th component of the user scratch
array.

=item *

C<D>I<NUMBER> is replaced by the directory name with NUMBER components stripped.

=item *

Names of ID3v2 frames are replaced by their text values (empty for missing
frames).

=item *

If string starts with C<FNAME:>: if frame FNAME does not exists, the escape
is ignored; otherwise the rest of the string is reinterpreted (after stripping
backslashes from backslashes and curlies).

=item *

String starting with C<!FNAME:> are treated similarly with inverted test.

=item *

String starting with I<LETTER>C<:> or C<!>I<LETTER>C<:> are treated similarly
to ID3v2 conditionals, but the condition is that the corresponding escape
expands to non-empty string.

=item *

Strings C<aC>, C<tT>, C<cC>, C<cT> are replaced by the collection artist,
track title, collection comment, and track comment as obtained from
CDDB_File.

=item *

Strings C<ID3v1> and C<ID3v2> are replaced by the whole ID3v1/2 tag.

=item *

Strings of the form C<FRAM(list,of,languages)[description]'> are
replaced by the first FRAM frame with the descriptor "description" in
the specified comma-separated list of languages.  Instead of a
language (ID3v2 uses lowercase 3-char ISO-639-2 language notations) one can use
a string of the form C<#Number>; e.g., C<#4> means 4th FRAM frame, or
FRAM04.  Empty string for the language means any language.)  Works as
a condition for conditional interpolation too.

Any one of the list of languages and the disription can be omitted;
this means that either the frame FRAM has no language or descriptor
associated, or no restriction should be applied.

Unknown language should be denoted as C<XXX> (in uppercase!).  The language
match is case-insensitive.

=over

The default for the fill character is SPACE.  Fill character should preceed
C<-> if both are given.  Example:

   Title: %(/)-12.12t%{TIT3:; TIT3 is %\{TIT3\}}%{!TIT3:. No TIT3 is present}

will result in

   Title: TITLE///////; TIT3 is Op. 16

if title is C<TITLE>, and TIT3 is C<Op. 16>, and

   Title: TITLE///////. No TIT3 is present

if title is C<TITLE>, but TIT3 is not present.

  Fat content: %{COMM(eng,fra,fre,rus,)[FatContent]}

will print the comment field with I<Description> C<FatContent>
prefering the description in English to one in French, Russian, or any
other language (in this order).  (I do not know which one of
terminology/bibliography codes for Frech is used, so for safety
include both.)

=cut

my %trans = qw(	t	title
		a	artist
		l	album
		y	year
		g	genre
		c	comment
		n	track
		E	filename_extension
		e	filename_extension_nodot
		A	abs_filename_noextension
		B	filename_nodir_noextension
		N	filename_noextension
		f	filename_nodir
		D	dirname
		F	abs_filename
		aC	artist_collection
		tT	title_track
		cC	comment_collection
		cT	comment_track

		v	mpeg_version
		L	mpeg_layer_roman
		?	is_stereo
		?	is_vbr
		r	bitrate_kbps
		q	frequency_kHz
		Q	frequency_Hz
		?	size_bytes
		S	total_secs_int
		m	total_mins
		s	leftover_secs
		?	leftover_msec
		?	time_mm_ss
		C	is_copyrighted_YN
		p	frames_padded_YN
		o	channel_mode
		u	frames
		?	frame_len
		?	vbr_scale
  );

# Different:	%v is without trailing 0s, %q has fractional part,
#		%e, %E are for the extension,
#		%r is a number instead of 'Variable', %u is one less...
# Missing:
#	%b      Number of corrupt audio frames (integer)
#	%e      Emphasis (string)
#	%E      CRC Error protection (string)
#	%O      Original material flag (string)

sub interpolate {
    my ($self, $pattern) = @_;
    $self->get_tags();
    my $res = "";
    my $ids;

    while ($pattern =~ s/^([^%]+)|^%(?:(?:\((.)\)|([^-.1-9%a-zA-Z]))?(-)?(\d+))?(?:\.(\d+))?([talygcnfFeEABDNvLrqQSmsCpou{%])//s) {
	$res .= $1, next if defined $1;
	my ($fill, $left, $minwidth, $maxwidth, $what)
	    = ((defined $2 ? $2 : $3), $4, $5, $6, $7);
	my $str;
	if ($what eq '{' and $pattern =~ s/^([dD])(\d+)}//) {	# Directory
	    if ($1 eq 'd') {
		$str = $self->dir_component($2);
	    } else {
		$str = $self->dirname($2);
	    }
	} elsif ($what eq '{' and $pattern =~ s/^U(\d+)}//) {	# User data
	    $str = $self->get_user($1);
	} elsif ($what eq '{' and $pattern =~ s/^(aC|tT|c[TC])}//) {
	    my $meth = $trans{$1};
	    $str = $self->$meth();
	} elsif ($what eq '{' and $pattern =~ s/^(!)?([talygcnfFeEABD]):((?:[^\\{}]|\\[\\{}])*)}//) {
	    my $neg = $1;
	    my $have = length($self->interpolate("%$2"));
	    next unless $1 ? !$have : $have;
	    ($str = $3) =~ s/\\([\\{}])/$1/g;
	    $str = $self->interpolate($str);
	} elsif ($what eq '{' and $pattern =~ s/^ID3v1}//) {
	    return '' unless $self->{ID3v1};
	    $str = $self->{ID3v1}->as_bin;
	} elsif ($what eq '{') {	# id3v2 stuff
	    $pattern =~ s/^((?:[^\\{}]|\\[\\{}])*)}// or die "Mismatched {} in pattern `$pattern'";
	    $what = $1;
	    unless ($self->{ID3v2} or $what =~ /^!/) {
		die "No ID3v2 present" if $self->get_config('id3v2_missing_fatal');
		return '';
	    }
	    if ($what eq 'ID3v2') {
		return '' unless $self->{ID3v2};
		$str = $self->{ID3v2}->as_bin;
	    } elsif ($what =~ /^\w{4}(?:\d{2,})?$/) {
		(undef, $str) = $self->get_id3v2_frames($what);
		$str = $str->{_Data} if $str and ref $str and exists $str->{_Data};
	    } elsif ($what =~ /^(\w{4})(?:\(([^)]*)\))?(?:\[([^]]*)\])?$/) {
		my $langs = defined $2 ? [split /,/, $2, -1] : '';
		my ($fname, $shorts) = ($1, $3);
		$str = $self->select_id3v2_frame($fname, $shorts, $langs);
	    } elsif ($what =~ /^(!)?(\w{4}(?:\d{2,})?):(.*)/s) {
		$ids = $self->get_id3v2_frame_ids || ''
		    unless defined $ids; # Cache the value
		my $have = $ids && exists $ids->{$2};
		next unless $1 ? !$have : $have;
		($str = $3) =~ s/\\([\\{}])/$1/g;
		$str = $self->interpolate($str);
	    } elsif ($what =~ /^(!)?(\w{4})(?:\(([^)]*)\))?(?:\[([^]]*)\])?:(.*)$/s) {
		my $langs = defined $3 ? [split /,/, $3, -1] : undef;
		my ($fname, $shorts) = ($2, $4);
		my $have = $self->have_id3v2_frame($fname, $shorts, $langs);
		next unless $1 ? !$have : $have;
		($str = $5) =~ s/\\([\\{}])/$1/g;
		$str = $self->interpolate($str);
	    } else {
		die "unknown escape `$what'";
	    }
	} elsif ($what eq '%') {
	    $str = '%';
	} else {
	    my $meth = $trans{$what};
	    $str = $self->$meth;
	}
	$str = '' unless defined $str;
	$str = substr $str, 0, $maxwidth if defined $maxwidth;
	if (defined $minwidth) {
	  $fill = ' ' unless defined $fill;
	  if ($left) {
	    $str .= $fill x ($minwidth - length $str);
	  } else {
	    $str = $fill x ($minwidth - length $str) . $str;
	  }
	}
	$res .= $str;
    }
    die "Can't parse `$pattern' during interpolation" if length $pattern;
    return $res;
}

=item parse_rex($pattern, $string)

Parse $string according to the regular expression $pattern with C<%>-escapes
C<%%, %a, %t, %l, %y, %g, %c, %n, %e, %E>.  The meaning of escapes is the same
as for L<interpolate>.  Also supported are escapes C<%=a, %=t, %=l, %=y, %=g, %=c,
%=n, %=e, %=E, %=A, %=B, %=D, %=f, %=F, %=N, %={WHATEVER}>; they match substrings which are
I<actual> values of
artist/title/etc (C<%=n> also matches leading 0s; actual file-name matches
ignore the difference between C</> and C<\>, between one and multiple
consequent dots (if configuration variable C<parse_filename_merge_dots> is true (default))
and are case-insensitive if configuration variable C<parse_filename_ignore_case>
is true (default);
moreover, <%n>, <%y>, <%=n>, <%=y> will not match if the string-to-match
is adjacent to a digit).  Returns false on failure, a hash reference with
parsed fields otherwise.  The escapes C<%{UE<lt>numberE<gt>}> and escapes
of the forms C<%{ABCD}>, C<%{ABCDE<lt>numberE<gt>}> match any string,
and corresponds to the hash key inside braces; here C<ABCD> is a 4-letter
word possibly followed by 2-digit number (as in names of ID3v2 tags), or
what can be put in C<'%{FRAM(lang,list)[description]}'>.

  $res = $mp3->parse_rex(qr<^%a - %t\.\w{1,4}$>, $mp3->filename_nodir) or die;
  $author = $res->{author};

2-digit numbers are allowed for the track number (the leading 0 is stripped);
4-digit years in the range 1000..2999 are allowed for year.  Alternatively, if
option year_is_timestamp is TRUE (default), year may be a range of timestamps
in the format understood by ID3v2 method year() (see L<MP3::Tag::ID3v2/"year">).

Currently the regular expressions with capturing parens are not supported.

=item parse_rex_prepare($pattern)

Returns a data structure which later can be used by parse_rex_match().
These two are equivalent:

  $mp3->parse_rex($pattern, $data);
  $mp3->parse_rex_match($mp3->parse_rex_prepare($pattern), $data);

This call constitutes the "slow part" of the parse_rex() call; it makes sense to
factor out this step if the parse_rex() with the same $pattern is called
against multiple $data.

=item parse_rex_match($prepared, $data)

Matches $data against a data structure returned by parse_rex_prepare().
These two are equivalent:

  $mp3->parse_rex($pattern, $data);
  $mp3->parse_rex_match($mp3->parse_rex_prepare($pattern), $data);

=cut

sub _rex_protect_filename {
    my ($self, $filename, $what) = (shift, quotemeta shift, shift);
    $filename =~ s,\\[\\/],[\\\\/],g;	# \ and / are interchangeable + backslashitis
    if ($self->get_config('parse_filename_merge_dots')->[0]) {
	# HPFS doesn't distinguish x..y and x.y
	$filename =~ s(\\\.+)(\\.+)g;
	$filename =~ s($)(\\.*) if $what =~ /[ABN]/;
    }
    my $case = $self->get_config('parse_filename_ignore_case')->[0];
    return $filename unless $case;
    return "(?i:$filename)";
}

sub _parse_rex_anything ($$) {
    my $c = shift->get_config('parse_minmatch');
    my $min = $c->[0];
    if ($min and $min ne '1') {
	my $field = shift;
	$min = grep $_ eq $field, @$c;
    }
    return $min ? '(.*?)' : '(.*)';
}

sub _parse_rex_microinterpolate {	# $self->idem($code, $groups, $ecount)
    my ($self, $code, $groups) = (shift, shift, shift);
    return '%' if $code eq '%';
    # In these two, allow setting to '' too...
    push(@$groups, $code), return '((?<!\d)\d{1,2}(?!\d)|\A\Z)' if $code eq 'n';
    (push @$groups, $code), return '((?<!\d)[12]\d{3}(?:(?:--|[-:/T\0,])\d(?:|\d|\d\d\d))*(?!\d)|\A\Z)'
	if $code eq 'y' and ($self->get_config('year_is_timestamp'))->[0];
    (push @$groups, $code), return '((?<!\d)[12]\d{3}(?!\d)|\A\Z)'
	if $code eq 'y';
    (push @$groups, $code), return $self->_parse_rex_anything($code)
	if $code =~ /^[talgc]$/;
    $_[0]++, return $self->_rex_protect_filename($self->interpolate("%$1"), $1)
	if $code =~ /^=([ABDfFN]|{d\d+})$/;
    $_[0]++, return quotemeta($self->interpolate("%$1"))
	if $code =~ /^=([talgceE]|{.*})$/;
    $_[0]++, return '(?<!\d)0*' . quotemeta($self->track) . '(?!\d)'
	if $code eq '=n';
    $_[0]++, return '(?<!\d)' . quotemeta($self->year) . '(?!\d)'
	if $code eq '=y';
    (push @$groups, $1), return $self->_parse_rex_anything()
	if $code =~ /^{(U\d+|\w{4}(\d\d+|(?:\([^\)]*\))?(?:\[[^\]]*\])?)?)}$/;
    # What remains is extension
    my $e = $self->get_config('extension')->[0];
    (push @$groups, $code), return "($e)" if $code eq 'E';
    (push @$groups, $code), return "(?<=(?=(?:$e)\$)\\.)(.*)" if $code eq 'e';
    # Check whether '=' was omitted, as in %f
    $code =~ /^=/ or
      eval {my ($a, $b); $self->_parse_rex_microinterpolate("=$code", $a, $b)}
	and die "escape `%$code' can't be parsed; did you forget to put `='?";
    die "unknown escape `%$code'";
}

sub parse_rex_prepare {
    my ($self, $pattern) = @_;
    my ($codes, $exact) = ([], 0);
    my $o = $pattern;
    $pattern =~ s<%(=?{(?:[^\\{}]|\\[\\{}])*}|{U\d+}|=?.)>
		 ( $self->_parse_rex_microinterpolate($1, $codes, $exact) )seg;
    my @tags = map { length == 1 ? $trans{$_} : $_ } @$codes;
    return [$o, $pattern, \@tags, $exact];
}

sub parse_rex_match {	# pattern = [Original, Interpolated, Fields, NumExact]
    my ($self, $pattern, $data) = @_;
    return unless @{$pattern->[2]} or $pattern->[3];
    my @vals = ($data =~ /$pattern->[1]()/s) or return;	# At least 1 group
    my $cv = @vals - 1;
    die "Unsupported %-regular expression `$pattern->[0]' (catching parens? Got $cv vals) (converted to `$pattern->[1]')"
	unless $cv == @{$pattern->[2]};
    my ($c, %h) = 0;
    for my $k ( @{$pattern->[2]} ) {
	$h{$k} ||= [];
	push @{ $h{$k} }, $vals[$c++];	# Support multiple occurences
    }
    my $j = $self->get_config('parse_join')->[0];
    for $c (keys %h) {
	$h{$c} = join $j, grep length, @{ $h{$c} };
    }
    $h{track} =~ s/^0+(?=\d)// if exists $h{track};
    return \%h;
}

sub parse_rex {
    my ($self, $pattern, $data) = @_;
    $self->parse_rex_match($self->parse_rex_prepare($pattern), $data);
}

=item parse($pattern, $string)

Parse $string according to the string $pattern with C<%>-escapes C<%%, %a, %t,
%l, %y, %g, %c, %n, %e, %E>.  The meaning of escapes is the same as for L<interpolate>. See L<"parse_rex($pattern, $string)"> for more details.
Returns false on failure, a hash reference with parsed fields otherwise.

  $res = $mp3->parse("%a - %t.mp3", $mp3->filename_nodir) or die;
  $author = $res->{author};

2-digit numbers are allowed for the track number; 4-digit years in the range
1000..2999 are allowed for year.

=item parse_prepare($pattern)

Returns a data structure which later can be used by parse_rex_match().
This is a counterpart of parse_rex_prepare() used with non-regular-expression
patterns.  These two are equivalent:

  $mp3->parse($pattern, $data);
  $mp3->parse_rex_match($mp3->parse_prepare($pattern), $data);

This call constitutes the "slow part" of the parse() call; it makes sense to
factor out this step if the parse() with the same $pattern is called
against multiple $data.

=cut

#my %unquote = ('\\%' => '%', '\\%\\=' => '%=');
sub __unquote ($) { (my $k = shift) =~ s/\\(\W)/$1/g; $k }

sub parse_prepare {
    my ($self, $pattern) = @_;
    $pattern = "^\Q$pattern\E\$";
    # unquote %. and %=. and %={WHATEVER} and %{WHATEVER}
    $pattern =~ s<(\\%(?:\\=)?(\w|\\{(?:\w|\\[^\w\\{}]|\\\\\\[\\{}])*\\}|\\\W))>
		 ( __unquote($1) )ge;
    # $pattern =~ s/(\\%(?:\\=)?)(\w|\\(\W))/$unquote{$1}$+/g;
    return $self->parse_rex_prepare($pattern);
}

sub parse {
    my ($self, $pattern, $data) = @_;
    $self->parse_rex_match($self->parse_prepare($pattern), $data);
}

=item filename()

=item abs_filename()

=item filename_nodir()

=item filename_noextension()

=item filename_nodir_noextension()

=item abs_filename_noextension()

=item dirname([$strip_levels])

=item filename_extension()

=item filename_extension_nodot()

=item dir_component([$level])

  $filename = $mp3->filename();
  $abs_filename = $mp3->abs_filename();
  $filename_nodir = $mp3->filename_nodir();
  $abs_dirname = $mp3->dirname();
  $abs_dirname = $mp3->dirname(0);
  $abs_parentdir = $mp3->dirname(1);
  $last_dir_component = $mp3->dir_component(0);

Return the name of the audio file: either as given to the new() method, or
absolute, or directory-less, or originally given without extension, or
directory-less without extension, or
absolute without extension, or the directory part of the fullname only, or
filename extension (with dot included, or not).

The extension is calculated using the config() value C<extension>.

The dirname() method takes an optional argument: the number of directory
components to strip; the C<dir_component($level)> method returns one
component of the directory (to get the last use 0 as $level; this is the
default if no $level is specified).

=cut

sub filename {
    shift->{ofilename}
}

sub filename_noextension {
    my $self = shift;
    my $f = $self->{ofilename};
    my $ext_re = $self->get_config('extension')->[0];
    $f =~ s/$ext_re//;
    return $f;
}

sub abs_filename {
    shift->{abs_filename}
}

sub filename_nodir {
    require File::Basename;
    return scalar File::Basename::fileparse(shift->filename, "");
}

sub dirname {
    require File::Basename;
    my ($self, $l) = (shift, shift);
    my $p = $l ? $self->dirname($l - 1) : $self->abs_filename;
    return File::Basename::dirname($p);
}

sub dir_component {
    require File::Basename;
    my ($self, $l) = (shift, shift);
    return scalar File::Basename::fileparse($self->dirname($l), "");
}

sub filename_extension {
    my $self = shift;
    my $f = $self->filename_nodir;
    my $ext_re = $self->get_config('extension')->[0];
    $f =~ /($ext_re)/ or return '';
    return $1;
}

sub filename_nodir_noextension {
    my $self = shift;
    my $f = $self->filename_nodir;
    my $ext_re = $self->get_config('extension')->[0];
    $f =~ s/$ext_re//;
    return $f;
}

sub abs_filename_noextension {
    my $self = shift;
    my $f = $self->abs_filename;
    my $ext_re = $self->get_config('extension')->[0];
    $f =~ s/$ext_re//;
    return $f;
}

sub filename_extension_nodot {
    my $self = shift;
    my $e = $self->filename_extension;
    $e =~ s/^\.//;
    return $e;
}

=item mpeg_version()

=item mpeg_layer()

=item mpeg_layer_roman()

=item is_stereo()

=item is_vbr()

=item bitrate_kbps()

=item frequency_Hz()

=item frequency_kHz()

=item size_bytes()

=item total_secs()

=item total_secs_int()

=item total_mins()

=item leftover_secs()

=item leftover_msec()

=item time_mm_ss()

=item is_copyrighted()

=item is_copyrighted_YN()

=item frames_padded()

=item frames_padded_YN()

=item channel_mode_int()

=item frames()

=item frame_len()

=item vbr_scale()

These methods return the information about the contents of the MP3 file.
Useing these methods requires that the module L<MP3::Info|MP3::Info>
is installed.  Since these calls are redirectoed to the module
L<MP3::Info|MP3::Info>, the returned info is subject to the same restrictions
as the method get_mp3info() of this module; in particular, the information
about the frame number and frame length is only approximate

vbr_scale() is from the VBR header; total_secs() is not necessarily an integer,
but total_secs_int() is;
time_mm_ss() has format C<MM:SS>; the C<*_YN> flavors return the value as a
string Yes or No; mpeg_layer_roman() returns the value as a roman numeral;
channel_mode() takes values in C<'stereo', 'joint stereo', 'dual channel', 'mono'>.

=cut

my %mp3info = qw(
  mpeg_version		VERSION
  mpeg_layer		LAYER
  is_stereo		STEREO
  is_vbr		VBR
  bitrate_kbps		BITRATE
  frequency_kHz		FREQUENCY
  size_bytes		SIZE
  total_secs		SECS
  total_mins		MM
  leftover_secs		SS
  leftover_msec		MS
  time_mm_ss		TIME
  is_copyrighted	COPYRIGHT
  frames_padded		PADDING
  channel_mode_int	MODE
  frames		FRAMES
  frame_len		FRAME_LENGTH
  vbr_scale		VBR_SCALE
);

for my $elt (keys %mp3info) {
  no strict 'refs';
  my $k = $mp3info{$elt};
  *$elt = sub (;$) {
    require MP3::Info;
    $MP3::Info::try_harder = 1;
    my $self = shift;
    my $info = MP3::Info::get_mp3info($self->abs_filename);
    die "Didn't get valid data from MP3::Info for `".($self->abs_filename)."': $@"
      unless defined $info;
    $info->{$k}
  }
}

sub frequency_Hz ($) {
  1000 * (shift->frequency_kHz);
}

sub mpeg_layer_roman	{ 'I' x (shift->mpeg_layer) }
sub total_secs_int	{ int (shift->total_secs) }
sub frames_padded_YN	{ shift->frames_padded() ? 'Yes' : 'No' }
sub is_copyrighted_YN	{ shift->is_copyrighted() ? 'Yes' : 'No' }

my @channel_modes = ('stereo', 'joint stereo', 'dual channel', 'mono');
sub channel_mode	{ $channel_modes[shift->channel_mode_int] }

=item update_tags( [ $data ] )

  $mp3 = MP3::Tag->new($filename);
  $mp3->update_tags();			# Fetches the info, and updates tags

This method updates ID3v1 and ID3v2 tags (the latter only if needed) with
the the information about title, artist, album, year, comment, track,
genre from the hash reference $data.  The format of $data is the same as
one returned from autoinfo() (with or without the optional argument 'from').
The fields which are marked as coming from ID3v1 or ID3v2 tags are not updated
when written to the same tag.

If $data is not defined or missing, C<autoinfo('from')> is called to obtain
the data.  Returns the object reference itself to simplify chaining of method
calls.

=cut

sub update_tags {
    my ($mp3, $data) = (shift, shift);

    $mp3->get_tags;
    $data = $mp3->autoinfo('from') unless defined $data;

    $mp3->new_tag("ID3v1") unless exists $mp3->{ID3v1};
    my $elt;
    for $elt (qw/title artist album year comment track genre/) {
	my $d = $data->{$elt};
	next unless defined $d;
	$d = [$d, ''] unless ref $d;
        $mp3->{ID3v1}->$elt( $d->[0] ) if $d->[1] ne 'ID3v1';
    }				# Skip what is already there...
    $mp3->{ID3v1}->write_tag;

    return $mp3 if $mp3->{ID3v1}->fits_tag($data) and not exists $mp3->{ID3v2};

    $mp3->new_tag("ID3v2") unless exists $mp3->{ID3v2};
    for $elt (qw/title artist album year comment track genre/) {
	my $d = $data->{$elt};
	next unless defined $d;
	$d = [$d, ''] unless ref $d;
        $mp3->{ID3v2}->$elt( $d->[0] ) if $d->[1] ne 'ID3v2';
    }				# Skip what is already there...
    # $mp3->{ID3v2}->comment($data->{comment}->[0]);
    $mp3->{ID3v2}->write_tag;
    return $mp3;
}

sub DESTROY {
    my $self=shift;
    if (exists $self->{filename} and defined $self->{filename}) {
	$self->{filename}->close;
    }
}

1;

=pod

=head1 EXAMPLE SCRIPTS

Some example scripts come with this module:

=over

=item mp3info2

perform command line manipulation of audio tags (and more!);

=item audio_rename

rename audio files according to associated tags (and more!);

=item type_mp3_dir

write LaTeX files suitable for CD covers and normal-size sheet
descriptions of hierarchy of audio files;

=item mp3_total_time

Calculate total duration of audio files;

=item eat_wav_mp3_header

remove WAV headers from MP3 files in WAV containers.

=back

(Last two do not use these modules!)

Some more examples:

  # Convert from one (non-standard-conforming!) encoding to another
  perl -MMP3::Tag -MEncode -wle '
    my @fields = qw(artist album title comment);
    for my $f (@ARGV) {
      print $f;
      my $t = MP3::Tag->new($f) or die;
      $t->update_tags(
	{ map { $_ => encode "cp1251", decode "koi8-r", $t->$_() }, @fields }
      );
    }' list_of_audio_files

=head1 SEE ALSO

L<MP3::Tag::ID3v1>, L<MP3::Tag::ID3v2>, L<MP3::Tag::File>,
L<MP3::Tag::ParseData>, L<MP3::Tag::Inf>, L<MP3::Tag::CDDB_File>.

=head1 COPYRIGHT

Copyright (c) 2000-2004 Thomas Geffert, Ilya Zakharevich.  All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the terms of the Artistic License, distributed
with Perl.

=cut

