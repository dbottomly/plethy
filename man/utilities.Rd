\name{Utility functions}
\alias{add.labels.by.sample}
\alias{get.err.breaks}
\alias{adjust.labels}

\title{
Utility functions to assist with QA/QC and analysis of plethysmography data
}
\description{
After creation of a database, often additional data needs to be added or modified.  These functions assist with the common tasks that occur when working with Buxco whole body plethysmography data such as adding labels based on the 
sample IDs in the case of \code{add.labels.by.sample} or modifying labels that have previously been added in the case of \code{adjust.labels}.  The \code{get.err.breaks} function produces a summary of the samples and timepoints that
have the specified value for the 'Break_type_label' column (such as 'ERR' or 'UNK') and whether they are close to the expected value for either and experimental or acclimation run.  This can occur if there was only an experimental run 
for some samples or if other anomalies occured.  The user can then inspect these new labels wihtin the data.frame, modify them manually if necessary and use the data.frame as input to the \code{adjust.labels} function which replaces 
the original labels and moves the original labels to another column for future reference.
}
\usage{
add.labels.by.sample(bux.db, sample.labels)
get.err.breaks(bux.db, max.exp.count=150, max.acc.count=900, vary.perc=.1, label.val="ERR")
adjust.labels(bux.db, err.breaks.dta)
}

\arguments{
\item{bux.db}{
An object of class \code{BuxcoDB}
}
  \item{sample.labels}{
A data.frame with a column named 'samples' with values corresponding to the sample names in the database. The other columns will be add to the annotation table and any sample not included in the data.frame will have their labels set to NULL.
}

\item{err.breaks.dta}{
	A data.frame produced by \code{get.err.breaks} function.
}

\item{max.exp.count}{
	The maximum number of records expected for the experimental run.
}

\item{max.acc.count}{
	The maximum value of records expected for the acclimation run.
}

\item{vary.perc}{
	The size of a percent decrease relative to the maximum experimental or acclimation run tolerated and still allow assignment to that category.  Needs to be a value between 0 and 1.
}

\item{label.val}{
	A single character string observed in the Break_type_labels column of the annotation table (cannot be 'ACC' or 'EXP').
}

}


\value{
	\code{add.labels.by.sample} and \code{adjust.labels} modify tables in the SQLite database pointed to in the \code{BuxcoDB} object so nothing is returned.
	\code{get.err.breaks} returns a data.frame summarizing the samples and timepoints with a given \code{label.var}.
}

\author{
Daniel Bottomly
}
\seealso{
\code{\link{parse.buxco}},\code{\linkS4class{BuxcoDB}} 
}
\examples{
	
##set up a test dataset using internal functions
##should label sample_1 as ACC and EXP and samples 2 and 3 as UNK
##sample_3 should be too divergent from the expected 150 rows, so 
##the inferred labels should remain 'UNK'
	
samples=c(NA, "sample_1", NA, "sample_1", "sample_2", "sample_3")
count = c(NA,900, NA,150, 150, 110)
measure_break = c(FALSE, FALSE, TRUE, FALSE, FALSE,FALSE)
table_break = c(TRUE, rep(FALSE, length(samples)-1))
phase = rep("D1", length(samples))
    
err.dta <- data.frame(samples=samples, count=count, measure_break=measure_break, table_break=table_break, phase=phase, stringsAsFactors=FALSE)
    
sim.bux.lines <- plethy:::generate.sample.buxco(err.dta)
    
temp.file <- tempfile()
temp.db.file <- tempfile()
write(sim.bux.lines, file=temp.file)
test.bux.db <- parse.buxco(file.name=temp.file, db.name=temp.db.file, chunk.size=10000)
addAnnotation(test.bux.db, query=day.infer.query, index=FALSE)
addAnnotation(test.bux.db, query=break.type.query, index=TRUE)
	
##get a summary of this
	
unk.summary <- get.err.breaks(test.bux.db, label.val="UNK")
table(unk.summary$Sample_Name, unk.summary$inferred_labs)
	
##use the summary to change the Break_type_label column in the annotation table
	
head(retrieveData(test.bux.db))
	
adjust.labels(test.bux.db, unk.summary)
	
head(retrieveData(test.bux.db))
	
##additional annotations can be added to the database based on sample ID
	
sample.labels <- data.frame(samples=c("sample_1","sample_3"), response_type=c("high", "low"),stringsAsFactors=FALSE)
	
add.labels.by.sample(test.bux.db, sample.labels)
	
final.dta <- retrieveData(test.bux.db)
	
head(final.dta)
	
##should be 'high' for sample_1 and 'low' for sample_3 with NAs for sample_2

table(final.dta$Sample_Name, final.dta$response_type, useNA="ifany")

}
\keyword{Utilities}
