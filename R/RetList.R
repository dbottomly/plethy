setClass("CurDbTabs", representation(variable="data.frame", table="data.frame"),
         prototype=prototype(variable=data.frame(Variable_ID=integer(0), Variable_Name=character(0)),
                             table=data.frame(Bux_table_ID=integer(0), Bux_table_Name=character(0))),
         contains=character())

setClass("RetList", representation(all.breaks="data.frame", collect.header="character",
                                   samp.tab="data.frame", cur.db.tabs="CurDbTabs", break.at.end="logical",
                                   cur.table="character", max.run.time.mins="numeric"),
                    prototype=prototype(all.breaks=data.frame(), collect.header=character(0), samp.tab=data.frame(),
                                        cur.db.tabs=new("CurDbTabs"), break.at.end=FALSE, cur.table=character(0), max.run.time.mins=60),
                    contains=character())

#mainly for the unit tests
basicRetList <- function()
{
         return(new("RetList"))
}

#was originally this
#ret.list <- list(all.breaks=data.frame(), collect.header=character(), samp.tab=data.frame(),
#                    cur.db.tabs=list(Variable=data.frame(Variable_ID=integer(0), Variable_Name=character(0)),
#                    Table=data.frame(Table_ID=integer(0), Table_Name=character(0))), break.at.end=FALSE)

setGeneric("breakData", def=function(obj,...) standardGeneric("breakData"))
setMethod("breakData", signature("RetList"), function(obj)
          {
                return(obj@all.breaks)
          })

setGeneric("breakData<-", def=function(obj,value) standardGeneric("breakData<-"))
setReplaceMethod("breakData", signature("RetList"), function(obj, value)
                 {
                    slot(obj, "all.breaks") <- value
                    validObject(obj)
                    return(obj)
                 })

setGeneric("collectHeader", def=function(obj,...) standardGeneric("collectHeader"))
setMethod("collectHeader", signature("RetList"), function(obj)
          {
                return(obj@collect.header)
          })

setGeneric("collectHeader<-", def=function(obj, value) standardGeneric("collectHeader<-"))
setReplaceMethod("collectHeader", signature("RetList"), function(obj, value)
                 {
                    slot(obj, "collect.header") <- value
                    validObject(obj)
                    return(obj)
                 })

setGeneric("curTable", def=function(obj, ...) standardGeneric("curTable"))
setMethod("curTable", signature("RetList"), function(obj)
          {
                return(obj@cur.table)
          })

setGeneric("curTable<-", def=function(obj,value) standardGeneric("curTable<-"))
setReplaceMethod("curTable", signature("RetList"), function(obj, value)
                 {
                    slot(obj, "cur.table") <- value
                    validObject(obj)
                    return(obj)
                 })

setGeneric("sampTab", def=function(obj,...) standardGeneric("sampTab"))
setMethod("sampTab", signature("RetList"), function(obj)
          {
                return(obj@samp.tab)
          })

setGeneric("sampTab<-", def=function(obj,value) standardGeneric("sampTab<-"))
setReplaceMethod("sampTab", signature("RetList"), function(obj, value)
                 {
                    slot(obj, "samp.tab") <- value
                    validObject(obj)
                    return(obj)
                 })

setGeneric("breakAtEnd", def=function(obj,...) standardGeneric("breakAtEnd"))
setMethod("breakAtEnd", signature("RetList"), function(obj)
          {
                return(obj@break.at.end)
          })

setGeneric("breakAtEnd<-", def=function(obj,value) standardGeneric("breakAtEnd<-"))
setReplaceMethod("breakAtEnd", signature("RetList"), function(obj, value)
                 {
                    slot(obj, "break.at.end") <- value
                    validObject(obj)
                    return(obj)
                 })

setGeneric("variable", def=function(obj,...) standardGeneric("variable"))
setMethod("variable", signature("RetList"), function(obj)
          {
                return(obj@cur.db.tabs@variable)
          })

setGeneric("variable<-", def=function(obj,value) standardGeneric("variable<-"))
setReplaceMethod("variable", signature("RetList"), function(obj, value)
                 {
                    slot(slot(obj, "cur.db.tabs"), "variable") <- value
                    validObject(obj)
                    return(obj)
                 })

setGeneric("buxTable", def=function(obj,...) standardGeneric("buxTable"))
setMethod("buxTable", signature("RetList"), function(obj)
          {
                return(obj@cur.db.tabs@table)
          })

setGeneric("buxTable<-", def=function(obj,value) standardGeneric("buxTable<-"))
setReplaceMethod("buxTable", signature("RetList"), function(obj, value)
                 {
                    slot(slot(obj, "cur.db.tabs"), "table") <- value
                    validObject(obj)
                    return(obj)
                 })
                 
setGeneric("maxTime", def=function(obj,...) standardGeneric("maxTime"))
setMethod("maxTime", signature("RetList"), function(obj)
          {
                return(obj@max.run.time.mins)
          })

setGeneric("maxTime<-", def=function(obj,value) standardGeneric("maxTime<-"))
setReplaceMethod("maxTime", signature("RetList"), function(obj, value)
                 {
                    slot(obj, "max.run.time.mins") <- value
                    validObject(obj)
                    return(obj)
                 })
