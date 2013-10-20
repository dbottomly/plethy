\name{Buxco file Parsers}
\alias{parse.buxco}
\alias{parse.buxco.basic}
\title{
Functions for parsing Buxco respirometry data
}
\description{
A typical Buxco respirometry experiment involves collecting repeated measures on both acclimation and experimental data related to metabolism and respiration.  The \code{parse.buxco} function creates a local database representation of a given file to facilitate fast retrieval and ultimately analysis.  The user should only use \code{parse.buxco} with \code{parse.buxco.basic} mainly used for testing purposes.  

}
\usage{
parse.buxco(file.name = NULL, table.delim = "Table", burn.in.lines = c("Measurement", "Create measurement", "Waiting for", "Site Acknowledgement Changed"), chunk.size = 500, db.name = "bux_test.db", max.run.time.minutes = 60, overwrite = TRUE, verbose=TRUE)
parse.buxco.basic(file.name=NULL, table.delim="Table", burn.in.lines=c("Measurement", "Create measurement", "Waiting for", "Site Acknowledgement Changed"))
}
\arguments{
  \item{file.name}{
A path to the Buxco CSV file.  See vignette for further description of the required file format.
}
  \item{table.delim}{
A character vector of length one containing the pattern used to divide the Buxco file into tables.
}
  \item{burn.in.lines}{
A character vector containing the patterns used to divided each Buxco table into readings for different animals.
}
  \item{chunk.size}{
The number of lines that should be read in at a given time, more lines results in more memory consumption and quicker
parsing speed.
}
  \item{db.name}{
The file name of the local database to create.
}
  \item{max.run.time.minutes}{
The maximum time in minutes that a acclimation or experimental run should take.  A warning will be given if this is
exceeded and the data will be treated as if there were seperate runs.
}
  \item{overwrite}{
A logical value specifying whether the local database specified in \code{db.name} should be overwritten if exists.
}
\item{verbose}{
A logical value specifying if additional information should be printed as parsing progresses.
}
}
\details{
The \code{parse.buxco} function reads in the specified file in chunks.  It uses the lines specified in \code{burn.in.lines} to determine whether a 'break' has been reached.  Each break signifies that a series of readings for several animals has been completed and so only upon reaching a break is measurement data written to the database for the completed measurement sets.  Because of this, there will always be some memory overhead in proportion to the number of readings in each series irrespective of \code{chunk.size}.  To access the database in R, use the convienience method \code{retrieveData}.
}
\value{
The \code{parse.buxco} function returns a \code{BuxcoDB} object.
The \code{parse.buxco.basic} function returns a \code{data.frame}.
}
\references{
http://www.buxco.com/
}
\author{
Daniel Bottomly 
}
\note{
\code{parse.buxco.basic} should not be used directly as it is extremely memory intensive as it parses the entire file
at once and returns a \code{data.frame} result.  
}

\seealso{
\code{\linkS4class{BuxcoDB}}, \code{\link{retrieveData}}
}
\examples{

bux.db <- parse.buxco(file.name=buxco.sample.data.path(), db.name =tempfile())

head(retrieveData(bux.db))

}

\keyword{utilities}
