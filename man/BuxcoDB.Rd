\name{BuxcoDB-class}
\Rdversion{1.1}
\docType{class}
\alias{BuxcoDB-class}
\alias{BuxcoDB}
\alias{addAnnotation,BuxcoDB-method}
\alias{addAnnotation}
\alias{annoTable,BuxcoDB-method}
\alias{annoTable}
\alias{annoCols,BuxcoDB-method}
\alias{annoCols}
\alias{annoLevels,BuxcoDB-method}
\alias{annoLevels}
\alias{dbName,BuxcoDB-method}
\alias{dbName}
\alias{retrieveData,BuxcoDB-method}
\alias{retrieveData}
\alias{samples,BuxcoDB-method}
\alias{samples}
\alias{tables,BuxcoDB-method}
\alias{tables}
\alias{variables,BuxcoDB-method}
\alias{variables}
\alias{makeBuxcoDB}
\alias{summaryMeasures}
\alias{summaryMeasures,BuxcoDB-method}
\alias{retrieveMatrix,BuxcoDB-method}
\alias{retrieveMatrix}
\alias{tsplot}
\alias{tsplot,BuxcoDB-method}
\alias{mvtsplot}
\alias{mvtsplot,BuxcoDB-method}

\title{Class \code{"BuxcoDB"}}
\description{
This is the main class of the plethy package.  Each object of this class simply holds the name of the database as well as the name(s) of any additional tables added through addAnnotation.
}
\section{Objects from the Class}{
Objects should be created by calls of the form \code{makeBuxcoDb(db.name=NULL, annotation.table="Additional_labels")}.
}
\section{Slots}{
  \describe{
    \item{\code{db.name}:}{Object of class \code{"character"} Stores the path to the database.}
    \item{\code{annotation.table}:}{Object of class \code{"character"} Stores the name of the additional annotation table in the database to be created if \code{addAnnotation} is called. }
  }
}
\section{Methods}{
  \describe{
    \item{addAnnotation}{\code{signature(obj = "BuxcoDB")}: Carry out an additional query to populate columns in a new or existing table.  Each query should be specified by a function taking a BuxcoDB object as an argument and returning valid SQL.  Additionally if the \code{index} argument is set to TRUE, indexes will be placed on the columns in the table.  See the vignette for an example of how this function should be used in practice.}
    \item{annoTable}{\code{signature(obj = "BuxcoDB")}: Retrieve the name of the table where the additional annotation is to be stored.  This table will not exist until the \code{addAnnotation} method is called first.}
    \item{annoCols}{\code{signature(obj = "BuxcoDB")}: Returns a vector of the column names in the additional annotation table.}
    \item{annoLevels}{\code{signature(obj = "BuxcoDB")}: Returns a list of the same length as the number of annotation columns in the additional annotation table with each element containing the unique values for each column.}
    \item{dbName}{\code{signature(obj = "BuxcoDB")}: Retrieve the path to the plethy database}
    \item{retrieveData}{\code{signature(obj = "BuxcoDB")}: With no arguments, this method will retrieve all available data in the database as a \code{data.frame}.  Specifying single or a vector of values to one or more of \code{samples}, \code{variables}, \code{tables}, \code{phase} or one of the addtional annotation names specified using \code{addAnnotation} will retrieve a subset of Buxco data.  Note that the valid variable (column) names for the annotation table can be found through the \code{annoCols} method and the valid variables for each variable name can be found through \code{annoLevels}.}
    \item{retrieveMatrix}{\code{signature{obj = "BuxcoDB"}}: This method first uses 'retrieveData' to retrieve the specified subset of data and then summarizes it into a matrix or array by first applying the specified 'summary.func'.  The default form of the array is Samples x Timepoint x Variable though this can be changed by supplying a different formula.}
    \item{samples}{\code{signature(obj = "BuxcoDB")}: Returns a vector of the unique samples in the database}
    \item{tables}{\code{signature(obj = "BuxcoDB")}: Returns a vector of the unique tables in the database}
    \item{variables}{\code{signature(obj = "BuxcoDB")}: Returns a vector of the unique Buxco measurement variables in the database}
    \item{tsplot}{\code{signature(obj = "BuxcoDB")}: Produces a line plot of the subset of the subset of the data as specified in 'retrieveData' after summarizing each sample for each timepoint by the function specified in 'summary.func'.  Colors and a legend can be added for a single experimental variable specified in 'exp.factor'.  The x,y and legend labels can be modified using the 'xlab', 'ylab' and 'legend.name' arguments respecitively.}
    \item{mvtsplot}{\code{signature(obj = "BuxcoDB")}: Produces a multivariate timeseries plot adapted from the function in the 'mvtsplot' CRAN package.  By default it will produce a plot containing a heatmap like image for each sample centered and scaled by row along with a boxplot depicting the overall distribution of the variable specified in 'plot.value'.  At the bottom of the plot, a line plot shows the median trend over time. The data can also optionally be grouped as either an 'inner.group' or an 'outer.group'.  The 'inner.group.name' defaults at the sample name though it can be used to specify a group in its own right.  The 'outer.group.name' subdivides the plot visualy and the medians are computed over each group seperately.  Colors of the 'outer.group' can be changed by supplying a color vector to 'outer.col' named by the levels of 'outer.group.name'.  The colors used in the heatmap image can be influenced by supplying a different palette from RColorBrewer to 'colorbrewer.pal'. Note that the data is first summarized by sample and day using the function supplied to 'summary.func'.}
	\item{summaryMeasures}{\code{signature(obj = "BuxcoDB"), summary.type=c("time.to.max.response", "max.response", "auc.response", "mean.response"), sample.summary.func=function(x) data.frame(Value=mean(x$Value)), samples=NULL, variables=NULL, tables=NULL, Break_type_label="EXP", day.summary.column="Days"}: Returns a data.frame containing a summaries of the variables for each sample with respect to the main time element specified in 'day.summary.column' after first summarizing each variable and timepoint for each sample by 'sample.summary.func'.  The data can be subsetted ahead of time using the samples, variables, tables and Break_type_label arguments.  Note this assumes that \code{break.type.query} was previously run using \code{addAnnotation}.}
	 }
}

\author{
Daniel Bottomly
}

\seealso{
\code{\link{parse.buxco}}
}
\examples{

samp.file <- sample.db.path()
new.file <- file.path(tempdir(), basename(samp.file))

stopifnot(file.copy(samp.file, new.file, overwrite=TRUE))

bux.db <- makeBuxcoDB(new.file)

show(bux.db)

head(retrieveData(bux.db))

annoCols(bux.db)

annoLevels(bux.db)

dbName(bux.db)

samples(bux.db)

tables(bux.db)

variables(bux.db)

addAnnotation(bux.db, query=day.infer.query, index=FALSE)

annoCols(bux.db)
annoLevels(bux.db)

addAnnotation(bux.db, query=break.type.query, index=TRUE)

annoCols(bux.db)
annoLevels(bux.db)

head(retrieveData(bux.db))

retrieveMatrix(bux.db)[1:5,1,1:5]

}
\keyword{classes}
