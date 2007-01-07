#!/bin/env perl

use warnings;
use strict;

# utility to create a release

# Steps:
#
# Pre-release work:
# ================
#  - Update change log
#  - tag release in subversion
#    svn copy trunk tags/RELEASE-x.y.z
#
# Building a release
# ==================
#  - export release tag into temp dir
#    svn export \
        http://robinbowes.com/svn/flac2mp3/tags/RELEASE-x.y.z \
        flac2mp3-x.y.z
#  - remove some dirs from the export so they aren't packaged
        rm -rf flac2mp3-x.y.z/patches
#  - create tarball and gzip
        tar cvzf flac2mp3-x.y.z.tar.gz flac2mp3-x.y.z
#  - create zip file (for windows users)
        zip -r9 flac2mp3-x.y.z.zip flac2mp3-x.y.z
#  - copy to download directory
