\name{plethy-package}
\alias{plethy-package}
\alias{plethy}
\alias{buxco.sample.data.path}
\alias{sample.db.path}
\docType{package}
\title{
R framework for exploration and analysis of respirometry data

}
\description{
This package provides the infrastructure and tools to import, query
        and perform basic analysis of whole body plethysmography and metabolism
        data.  Currently support is limited to data derived from Buxco respirometry 
	instruments as exported by their FinePointe software.
}
\details{
\tabular{ll}{
Package: \tab plethy\cr
Version: \tab 0.99.4\cr
Date: \tab 2013-4-26\cr
Imports: \tab Streamer, DBI, RSQLite, methods, IRanges, reshape2, batch\cr
Depends: \tab R (>= 3.0.0)\cr
Suggests: \tab RUnit, BiocGenerics\cr
License: \tab GPL-3\cr
biocViews: \tab DataImport, biocViews, Infastructure,
DataRepresentation,TimeCourse\cr
Collate: \tab BuxcoDB.R RetList.R buxco_db_v2.R dep_parser.R utilities.R\cr
Packaged: \tab 2013-08-01 21:02:18 UTC; bottomly\cr
Built: \tab R 3.0.1; ; 2013-08-01 21:02:29 UTC; unix\cr
}

Index:
\preformatted{
BuxcoDB-class           Class '"BuxcoDB"'
dbImport                Import data into a BuxcoDB database
parse.buxco             Functions for parsing Buxco respirometry data
plethy-package          R framework for exploration and analysis of
                        respirometry data
buxco.sample.data.path  Path to a sample Buxco output file
sample.db.path          Path to a sample BuxcoDB database
}

Further information is available in the following vignettes:
\tabular{ll}{
\code{plethy} \tab plethy (source, pdf)\cr
}

}
\author{
Daniel Bottomly, Marty Ferris

Maintainer: Daniel Bottomly <bottomly@ohsu.edu>
}
\references{
www.buxco.com
}

\keyword{ package }
\examples{
tmp_db <- tempfile()

bux.db <- parse.buxco(file.name=buxco.sample.data.path(), db.name = tmp_db)

head(retrieveData(bux.db))

bux.db <- makeBuxcoDB(tmp_db)

samples(bux.db)
variables(bux.db)

}
