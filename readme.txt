About flac2mp3
==============

flac2mp3 is a perl script that will search for flac files within
a directory hierarchy and convert them all to mp3 format, creating a
matching directory structure in the process.

I wrote it as I have a large collection of flac files but need to
convert them to mp3 format for use with my iPod.

There are a few programs that can do basic file format conversion
but I found that it was hard to detect which files were new in my
flac collection and to convert just those files. I also find I update
the metadata in my flac files fairly often (when I spot mistakes, etc.)
and needed  a way to update just the tags rather than running the whole
conversion process again.

flac2mp3 can do this.

It can take a directory structure like this:

lossless
  |
  +--Coldplay
  |    |
  |    +--Parachutes
  |         |
  |         +-- 01 - Don't Panic.flac
  |             02 - Shiver.flac
  +--The Chameleons
       |
       +Script of the Bridge
          |
          +-- 01 - Don't Fall.flac
              02 - Here Today.flac

And produce a directory structure like this:

lossy
  |
  +--Coldplay
  |    |
  |    +--Parachutes
  |         |
  |         +-- 01 - Don't Panic.mp3
  |             02 - Shiver.mp3
  +--The Chameleons
       |
       +Script of the Bridge
          |
          +-- 01 - Don't Fall.mp3
              02 - Here Today.mp3

The command to do this is:

  flac2mp3.pl /path/to/lossless /path/to/lossy

Now, suppose I notice that I've spelled "coldplay" wrongly. I simply
use a tag editor to correct the flac files then run flac2mp3 again
to update the tags in the mp3 files:

  flac2mp3.pl /path/to/lossless /path/to/lossy

flac2mp3 will detect that just the tags have changed in the flac files
and update the mp3 files without re-transcoding. 

Command-line options can be seen by typing "flac2mp3.pl" with no options:

Usage: ./flac2mp3.pl [--pretend] [--quiet] [--debug] [--tagsonly] [--force]
[--tagdiff] <flacdir> <mp3dir>
    --pretend       Don't actually do anything
    --quiet         Disable informational output to stdout
    --debug         Enable debugging output. For developers only!
    --tagsonly      Don't do any transcoding - just update tags
    --force         Force transcoding and tag update even if not required
    --tagdiff       Print source/dest tag values if different

Installation
============

As of v0.2.6 all non-standard perl modules are supplied in the archive.
Installation should be as simple as extracting the archive into a 
directory of your choice.

For example, on linux:

 # cd ~/bin
 # tar zxvf flac2mp3-0.3.0.tar.gz

On Windows, you can use Winzip or other utility to unzip the file and
extract the archive.

Mailing lists
=============

The following mailing lists are available:

List address: flac2mp3-announce@robinbowes.com
Description:  Read-only list for announcements
To subscribe: flac2mp3-announce-subscribe@robinbowes.com

List address: flac2mp3-devel@robinbowes.com
Description:  Developer discussion
To subscribe: flac2mp3-devel-subscribe@robinbowes.com

List address: flac2mp3-general@robinbowes.com
Description:  General user discussion
To subscribe: flac2mp3-general-subscribe@robinbowes.com

Please report any issues to robin-flac2mp3@robinbowes.com

Robin Bowes
July 2007
