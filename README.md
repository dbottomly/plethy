plethy
======

Analysis of whole body plethysmography data in R.

INSTALLATION

A stable version is best obtained through Bioconductor:

source("http://bioconductor.org/biocLite.R")

biocLite("plethy")

NOTE: Currently plethy is part of Bioconductor 2.13 which is the development branch so Bioconductor requires
R-3.01 and the useDevel=TRUE option before installation.

Any new features, refactoring, or bug fixes will be applied here first so those interesting in the latest
experimental version can install from here using the devtools package after first installing the dependencies:

source("http://bioconductor.org/biocLite.R")

biocLite(c("RSQLite", "batch", "IRanges", "reshape2", "Streamer"))

install_github(username="dbottomly", repo="plethy", ref="master")

CONTRIBUTING

Contributions are encouraged through the standard fork/pull procedures.  Feel free to send me an email with any 
questions.

BUG REPORTS, GENERAL QUESTIONS AND FEATURE REQUESTS

The Bioconductor mailing list is the best place to report bugs, ask questions or request features.


Package Maintainer:

Dan Bottomly

bottomly@ohsu.edu
