\name{Additional annotation queries}
\alias{break.type.query}
\alias{day.infer.query}

\title{
Builtin queries to add additional annotation to a BuxcoDB database
}
\description{
These functions take BuxcoDB objects as input and return an SQL query (in the SQLite dialect) which results in the creation of a table containing additional annotation that is not parsed from the Buxco CSV file.  These annotations are meant to be categorical labels such as whether a datapoint was part of an acclimation or experimental run which may not necessarily be encoded directly in the Buxco CSV file.  A user can also specify functions in this form to add custom annotations to the database.  Typically the end user does not need to call these functions directly, instead they are supplied to the \code{code} argument of the \code{addAnnotation} method.  This method then calls the function internally in the process of generating the annotation table.

Currently implemented queries are:

\code{day.infer.query}: Computes the number of days past the first observed timestamp for a given sample. This day assignment should be compared to the experimental timepoint potentially recorded in the 'Phase' element of the Buxco CSV file.  This value will exist as the 'Rec_Exp_date' column of the \code{data.frame} returned by \code{retrieveData}. 

\code{break.type.query}: Labels each datapoint as belonging to one of several categories.  Typically each datapoint would be part of the 'ACC' or 'EXP' groups corresponding to acclimation or experimental readings.  The assignment of these categories is based on observations regarding the current breakpoint number relative to the number of breakpoints observed for a given sample and given day.  Typically the acclimation readings are reported first followed by the experimental readings.  Additionally, 'UNK' or 'ERR' indicate deviations from the expected sample-breakpoint relationships.  Specifically, 'UNK' refers to the case where only one breakpoint was observed for that animal and day so the category is unknown. The presence of 'ERR' categories indicate potential issues with how the data was parsed and should be reported to the package author for investigation. NOTE: Currently, \code{day.infer.query} needs to be run prior to this query as it draws upon the computed date as opposed to the labeled date.

  
}
\usage{
break.type.query(obj)
day.infer.query(obj)
}
\arguments{
  \item{obj}{
A \code{BuxcoDB} object.
}
}
\value{
A character string representing an SQL query.
}
\references{
http://www.sqlite.org/
}
\author{
Daniel Bottomly
}

\seealso{
\code{\linkS4class{BuxcoDB}}
}
\examples{	

samp.file <- sample.db.path()
new.file <- file.path(tempdir(), basename(samp.file))

stopifnot(file.copy(samp.file, new.file, overwrite=TRUE))

bux.db <- makeBuxcoDB(new.file)

head(retrieveData(bux.db))

#query used to compute experiment day relative to the initial timepoint.
day.infer.query(bux.db)

addAnnotation(bux.db, query=day.infer.query)

head(retrieveData(bux.db))

}

\keyword{utilities}
