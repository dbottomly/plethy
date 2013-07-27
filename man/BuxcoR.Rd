\name{BuxcoR-package}
\alias{BuxcoR-package}
\alias{BuxcoR}
\alias{sample.db.path}
\alias{buxco.sample.data.path}
\docType{package}
\title{
R framework for exploration and analysis of Buxco respirometry data
}
\description{
This package provides the infrastructure and tools to import, query and perform basic analysis of whole body plethysmography and metabolism data derived from Buxco respirometry instruments.
}
\details{
Index:
\preformatted{
BuxcoDB-class           Main class providing access to the BuxcoDB SQLite databases
parse.buxco             Function for parsing Buxco respirometry data
dbImport                Function facilitating adding to and merging BuxcoDB databases
sample.db.path          Path to a sample BuxcoDB database
buxco.sample.data.path  Path to a sample Buxco output file
}

Further information is available in the following vignettes:
\tabular{ll}{
\code{BuxcoR} \tab BuxcoR (source)\cr
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

bux.db <- parse.buxco(file.name=buxco.sample.data.path(), db.name = "example_bux.db")

head(retrieveData(bux.db))

bux.db <- makeBuxcoDB("example_bux.db")

samples(bux.db)
variables(bux.db)

file.remove("example_bux.db")
}
