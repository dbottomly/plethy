\name{dbImport}
\alias{dbImport}

\title{
Import data into a BuxcoDB database
}
\description{
The main purpose of this function is to add data originally retrieved from the \code{retrieveData} method into a new or existing \code{BuxcoDB} database.  This will most frequently be useful in the context of a merging procedure, however it also can facilitate data sharing and/or communication between seperate DBMS systems. 
}
\usage{
dbImport(bux.db = NULL, bux.dta, db.name = "merge_test_1.db", debug = FALSE)
}

\arguments{
  \item{bux.db}{
Either NULL or a \code{BuxcoDB} object
}
  \item{bux.dta}{
A \code{data.frame} consistent with the database structure of the \code{BuxcoDB} database, most easily created from a call to \code{retrieveData}.
}
  \item{db.name}{
Path to the new SQLite database to create.
}
  \item{debug}{
Logical value indicating whether the function should be more verbose.
}
}
\details{
If only \code{bux.dta} is supplied and not bux.db, then a new database will be created and populated at \code{db.name} from its contents. If both \code{db.name} and \code{bux.dta} are
supplied then the \code{data.frame} will be loaded into the existing database.
}
\value{
	A \code{BuxcoDB} object pointing to the newly created database.
}

\author{
Daniel Bottomly
}

\seealso{
\code{\linkS4class{BuxcoDB}}, \code{\link{retrieveData}}
}
\examples{
bux.db <- makeBuxcoDB(sample.db.path())

samp.1 <- retrieveData(bux.db, samples="8034x13140_5")
	
test.db <- "test_db.db"
	
if (file.exists(test.db))
{
	file.remove(test.db)
}
	
#create a new database from the output
db.1 <- dbImport(bux.db=NULL, bux.dta=samp.1, db.name=test.db)
	
samples(db.1)
	
test.db.2 <- "test_db_2.db"
	
if (file.exists(test.db.2))
{
	file.remove(test.db.2)
}

samp.2 <- retrieveData(bux.db, samples="8034x13140_11")
	
db.2 <- dbImport(bux.db=db.1, bux.dta=samp.2, db.name=test.db.2)
	
samples(db.2)
	
file.remove(test.db.2)
file.remove(test.db)
}


\keyword{utilities}

