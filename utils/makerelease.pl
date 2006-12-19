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
#
# Building a release
# ==================
#  - export release tag into temp dir
#  - create tarball and gzip
#  - copy to download directory
