#basic class that points to the database and allows easier manipulations
setClass("BuxcoDB", representation(db.name="character", annotation.table="character"), prototype=prototype(db.name=character(0), annotation.table="Additional_labels"), contains=character())

makeBuxcoDB <- function(db.name=NULL, annotation.table="Additional_labels")
{
    if (missing(db.name) || is.null(db.name))
    {
        stop("ERROR: Need to supply a valid file name for db.name")
    }
    else if (! file.exists(db.name))
    {
        stop(paste("ERROR:", db.name, "does not exist"))
    }
    
    if (is.character(annotation.table) == FALSE || length(annotation.table) != 1)
    {
        stop("ERROR annotation.table needs to be a character vector of length 1")
    }
    
    return(new("BuxcoDB", db.name=db.name, annotation.table=annotation.table))
}

setMethod("show", signature("BuxcoDB"), function(object)
        {
            message("BuxcoDB object")
            message(paste("Database:", object@db.name))
            message(paste("Annotation Table:", object@annotation.table))
        })

setGeneric("summaryMeasures", def=function(obj,...) standardGeneric("summaryMeasures"))
setMethod("summaryMeasures", signature("BuxcoDB"), function(obj, summary.type=c("time.to.max.response", "max.response", "auc.response", "mean.response"), sample.summary.func=function(x) data.frame(Value=mean(x$Value)), samples=NULL, variables=NULL, tables=NULL, Break_type_label="EXP", day.summary.column="Days")
          {
                summaries <- match.arg(summary.type, several.ok=TRUE)
                
                if (is.function(sample.summary.func) == FALSE)
                {
                    stop("ERROR: sample.summary.func needs to be a valid function")
                }
                
                ret.dta <- retrieveData(obj, samples=samples, variables=variables, tables=tables, Break_type_label=Break_type_label)
                
                if (day.summary.column %in% names(ret.dta) == FALSE || any(is.na(as.numeric(ret.dta[,day.summary.column]))))
                {
                    stop("ERROR: day.summary.column needs to be a valid name in the database and be coercible to numeric values")
                }
                
                if ("Break_type_label" %in% names(ret.dta) == FALSE)
                {
                    stop("ERROR: Break_type_label needs to be part of the returned values for ret.dta")
                }
                
                if (any(Break_type_label %in% ret.dta$Break_type_label) == FALSE)
                {
                    stop("ERROR: At least one type element of Break_type_label needs to exist in the current output")
                }
                
                sum.days <- ddply(ret.dta, c("Variable_Name", "Sample_Name", day.summary.column), .fun=sample.summary.func)
                #a hack because ddply can't find the functions if they are supplied as characters...
                
                ret.dta <- data.frame(Variable_Name=character(0), Sample_Name=character(), stringsAsFactors=FALSE)
                
                for (i in summaries)
                {
                    summary.func <- get(i)
                    
                    temp.dta <- ddply(sum.days, c("Variable_Name", "Sample_Name"), .fun=summary.func, day.name=day.summary.column)
                    temp.dta$Variable_Name <- as.character(temp.dta$Variable_Name)
                    temp.dta$Sample_Name <- as.character(temp.dta$Sample_Name)
                    
                    ret.dta <- merge(ret.dta, temp.dta, by=c("Variable_Name", "Sample_Name"), all=TRUE, incomparables=NA, sort=FALSE)
                }
                
                return(ret.dta)
                
          })

setGeneric("annoTable", def=function(obj,...) standardGeneric("annoTable"))
setMethod("annoTable", signature("BuxcoDB"), function(obj)
          {
                return(obj@annotation.table)
          })

setGeneric("annoCols", def=function(obj,...) standardGeneric("annoCols"))
setMethod("annoCols", signature("BuxcoDB"), function(obj)
          {
                db.con <- dbConnect(SQLite(), dbName(obj))
                
                if (annoTable(obj) %in% dbListTables(db.con) == FALSE)
                {
                    return(character(0))
                }
                else
                {
                    #modified this 9-03-2013 to deal with the case of columns added by user that had _ID, really only deal with the case of Break_Chunk_ID as the ID col...
                    test.query <- dbListFields(db.con, annoTable(obj))
                    dbDisconnect(db.con)
                    id.col <- test.query[test.query == "Break_Chunk_ID"]
                    stopifnot(length(id.col) == 1)
                    lo.cols <- setdiff(test.query, id.col)
                    return(lo.cols)
                }
          })

setGeneric("annoLevels", def=function(obj,...) standardGeneric("annoLevels"))
setMethod("annoLevels", signature("BuxcoDB"), function(obj)
          {
                db.con <- dbConnect(SQLite(), dbName(obj))
                use.cols <- annoCols(obj)
                
                if (length(use.cols) == 0)
                {
                    return(character(0))
                }
                else
                {
                    ret.list <- lapply(use.cols, function(x)
                       {
                            dbGetQuery(db.con, paste("SELECT DISTINCT (", x,") FROM", annoTable(obj)))[,1]
                       })
                
                    names(ret.list) <- use.cols
                    dbDisconnect(db.con)
                    
                    return(ret.list)
                }
          })

setGeneric("dbName", def=function(obj,...) standardGeneric("dbName"))
setMethod("dbName", signature("BuxcoDB"), function(obj)
          {
                return(obj@db.name)
          })

setGeneric("samples", def=function(obj,...) standardGeneric("samples"))
setMethod("samples", signature("BuxcoDB"), function(obj)
          {
                get.simple.single.col.query(db.name=dbName(obj), var.name="Sample", col.suffix="_Name")
          })

setGeneric("variables", def=function(obj,...) standardGeneric("variables"))
setMethod("variables", signature("BuxcoDB"), function(obj)
          {
                get.simple.single.col.query(db.name=dbName(obj), var.name="Variable", col.suffix="_Name")
          })

setGeneric("tables", def=function(obj,...) standardGeneric("tables"))
setMethod("tables", signature("BuxcoDB"), function(obj)
          {
                get.simple.single.col.query(db.name=dbName(obj), var.name="Bux_table", col.suffix="_Name")
          })

setGeneric("retrieveData", def=function(obj,...) standardGeneric("retrieveData"))
setMethod("retrieveData", signature("BuxcoDB"), function(obj, samples=NULL, variables=NULL, tables=NULL,phase=NULL,debug=FALSE, ...)
          {
            
            supplied.args <- ls()
            
            db.con <- dbConnect(SQLite(), dbName(obj))
            
            #modified this on 1-22-2013, added column="P_Time" and break=list(table="Chunk_Time", column="Break_number") to make sure these make
            #it to the results
            table.map <- list(data=list(table="Data", column="Value"),
                              timepoint=list(table="Timepoint", column="P_Time"),
                              chunk.time=list(table="Chunk_Time", column="Break_sec_start"),
                              samples=list(table="Sample", column="Sample_Name"),
                              variables=list(table="Variable", column="Variable_Name"),
                              tables=list(table="Bux_table", column="Bux_table_Name"),
                              phase=list(table="Chunk_Time", column="Rec_Exp_date"),
                              break.num=list(table="Chunk_Time", column="Break_number"))
            
            #if additional table are present
            if (annoTable(obj) %in% dbListTables(db.con))
            {
                anno.tab.args <- list(...)
                if (length(anno.tab.args) > 0 && (is.null(names(anno.tab.args)) == TRUE || all(names(anno.tab.args) %in% annoCols(obj)) == FALSE))
                {
                    stop("ERROR: Need to supply named arguments (arg.name=c(1:10)) corresponding to columns of the annotation table, use annoCols(obj)")
                }
                
                for (column in annoCols(obj))
                {
                    table.map[[column]] <- list(table=annoTable(obj), column=column, value=anno.tab.args[[column]])
                }
                
            }
            
            supplied.args <- supplied.args[supplied.args %in% c("obj", "debug") == FALSE]
            
            for (i in supplied.args)
            {
                arg.vals <- get(i)
                
                table.map[[i]]$value <- arg.vals
            }
            
            query.res <- execute.query.map(db.con=db.con, query.map=table.map, debug=debug)
            
            dbDisconnect(db.con)
            
            #enforce kind of a rough ordering of the columns mainly for asthetics--sample is first, value is last
            
            if (all(c("Value", "Sample_Name") %in% colnames(query.res)))
            {
                lo.names <- setdiff(colnames(query.res), c("Value", "Sample_Name"))
                
                new.order <- c("Sample_Name", lo.names, "Value")
                
                query.res <- query.res[,new.order]
            }
            
            return(query.res)
            
          })

setGeneric("addAnnotation", def=function(obj,...) standardGeneric("addAnnotation"))
setMethod("addAnnotation", signature("BuxcoDB"), function(obj, query=NULL, index=FALSE, id.col.regex="_ID", debug=FALSE)
          {
                if (missing(query) || is.null(query) || is.function(query) == FALSE)
                {
                    stop("ERROR: Need to supply a function which takes a BuxcoDB object to the query argument")    
                }
                
                if (length(index) != 1 || is.logical(index) == FALSE)
                {
                    stop("ERROR: index needs to be a logical value")
                }
                
                if (length(id.col.regex) != 1 || is.character(id.col.regex) == FALSE)
                {
                    stop("ERROR: id.col.regex needs to be a character string")
                }
                
                if (length(debug) != 1 || is.logical(debug) == FALSE)
                {
                    stop("ERROR: debug needs to be a logical value")
                }
                
                db.con <- dbConnect(SQLite(), dbName(obj))
          
                cur.tables <- dbListTables(db.con)
                use.query <- query(obj)
                if(annoTable(obj) %in% cur.tables)
                {
                    temp.tab.1 <- paste(annoTable(obj), "temp1", sep="_")
                    temp.tab.2 <- paste(annoTable(obj), "temp2", sep="_")
                    
                    query.list <- c(paste("CREATE TEMPORARY TABLE", temp.tab.1,"AS SELECT * FROM", annoTable(obj)),
                                    paste("CREATE TEMPORARY TABLE", temp.tab.2, " AS", use.query), paste("DROP TABLE", annoTable(obj)),
                                       paste("CREATE TABLE ", annoTable(obj), "AS SELECT * FROM", temp.tab.1, "NATURAL JOIN", temp.tab.2),
                                       paste("DROP TABLE", temp.tab.1), paste("DROP TABLE", temp.tab.2))
                    
                }
                else
                {
                    #otherwise just create the table directly
                    query.list <- paste("CREATE TABLE", annoTable(obj), "AS", use.query)
                }
                final.query <- paste(query.list, sep=";")
                if (debug==TRUE)
                {
                    message(final.query)
                }
                else
                {
                    for (i in query.list)
                    {
                        stopifnot(is.null(dbGetQuery(db.con, i)))
                    }
                    
                }
                
                if (index==TRUE)
                {
                    make.annotation.indexes(db.con, annoTable(obj))
                }
                
                dbDisconnect(db.con)
          })
          
make.annotation.indexes <- function(db.con, anno.table)
{
    test.query <- dbGetQuery(db.con, paste("SELECT * FROM", anno.table, "limit 5"))
                    
    id.col <- names(test.query)[grep("_ID", names(test.query))]
    stopifnot(length(id.col) == 1)
    lo.cols <- setdiff(names(test.query), id.col)
    
    index.query <- paste("CREATE INDEX IF NOT EXISTS",paste(anno.table,"_", id.col, "_ind", sep=""),"ON",anno.table,"(",id.col,")")
    dbGetQuery(db.con, index.query)
    
    if (length(lo.cols) > 1)
    {
        perms <- expand.grid(rep(list(lo.cols), length(lo.cols)))
        use.perms <- apply(perms, 1, function(x) sum(duplicated(x)) == 0) 
        perms <- perms[use.perms,]
        
        for (i in 1:nrow(perms))
        {
            paste.rows <- paste(unlist(perms[i,]), collapse=", ")
            var.query <- paste("CREATE INDEX IF NOT EXISTS",paste(anno.table,"_ind_",i,sep=""),"ON",anno.table,"(",paste.rows,")")
            dbGetQuery(db.con, var.query)
        }
    }
    
}

dbImport <- function(bux.db=NULL, bux.dta, db.name="merge_test_1.db", debug=FALSE)
{
    if (missing(bux.db) == FALSE && is.null(bux.db) == FALSE && class(bux.db) != "BuxcoDB")
    {
        stop("ERROR: bux.db needs to be a BuxcoDB object or not specified at all")
    }
    else if (missing(bux.db) == FALSE && is.null(bux.db) == FALSE && class(bux.db) == "BuxcoDB")
    {
        file.copy(from=dbName(bux.db), to=db.name)
    }
    
    if (is.data.frame(bux.dta) == FALSE || nrow(bux.dta) < 1)
    {
        stop("ERROR: bux.dta needs to be a dataframe containing at least one row")
    }
    else if (validate.dta(bux.db, bux.dta) == FALSE)
    {
        stop("ERROR: bux.dta needs to have the same columns as bux.db, compare bux.dta with retrieveData(bux.db)")
    }
    
    if (is.character(db.name) == FALSE || length(db.name) != 1)
    {
        stop("ERROR: db.name needs to be a character string of the path to a new database")
    }
    
    #first create the simple tables, Sample, Bux_table, Variable, Timepoint
    #then do chunk_time followed by additional labels and data at the end
    
    #to do this first define a list containing definitions to create database tables:
        
    schema.list <- list(Sample=list(primary.key="Sample_ID", foreign.keys=NULL, record.vars="Sample_Name"),
                        Bux_table=list(primary.key="Bux_table_ID", foreign.keys=NULL, record.vars="Bux_table_Name"),
                        Variable=list(primary.key="Variable_ID", foreign.keys=NULL, record.vars="Variable_Name"),
                        Timepoint=list(primary.key="Timepoint_ID", foreign.keys=NULL, record.vars="P_Time"),
                        Chunk_Time=list(primary.key="Break_Chunk_ID", foreign.keys=c("Sample_ID", "Time_ID", "Bux_table_ID", "Variable_ID", "Break_number"),
                            record.vars=c("Break_sec_start", "Rec_Exp_date")),
                        Data=list(primary.key="Data_ID", foreign.keys=c("Time_ID", "Variable_ID", "Sample_ID", "Bux_table_ID"), record.vars="Value"))
    
    db.con <- dbConnect(SQLite(), db.name)
    
    db.tables <- dbListTables(db.con)
    
    if (length(setdiff(names(schema.list), db.tables)) == length(schema.list))
    {
        create.tables(db.con)
        
        db.tables <- dbListTables(db.con)
    }
    
    for (i in names(schema.list))
    {
        if (debug == TRUE) message(paste("Starting table", i))
        cur.schema <- schema.list[[i]]
        
        if (i %in% db.tables == FALSE)
        {
            stop(paste("ERROR: table", i, "not found in database"))
        }
        
        if (is.null(cur.schema$foreign.key))
        {
            rev.query <- db.insert.autoinc(db.con=db.con, table.name=i, col.name=cur.schema$record.vars, values=unique(bux.dta[,cur.schema$record.vars]),
                                            return.query.type="reverse", debug=debug)
                                        
            bux.dta <- merge(bux.dta, rev.query, all=TRUE, incomparables=NA, sort=FALSE)
        }
        else
        {
            relevant.cols <- c(cur.schema$foreign.keys, cur.schema$record.vars)
            temp.dta <- bux.dta[,relevant.cols]
            temp.dta <- temp.dta[!duplicated(temp.dta),]
            
            use.sql <- paste("INSERT INTO", i, "(", paste(relevant.cols, collapse=",") ,")","VALUES (", paste(paste("$", relevant.cols, sep=""), collapse=",") ,")")
            
            if (debug==TRUE) message(use.sql)
            
            #find the previous max primary key if applicable
            prev.max.primary <- as.numeric(dbGetQuery(db.con, paste("SELECT MAX(",cur.schema$primary.key,") FROM", i))[,1])
            
            if (is.na(prev.max.primary)) prev.max.primary <- 0
            
            dbBeginTransaction(db.con)
            dbGetPreparedQuery(db.con, use.sql, bind.data = temp.dta)
            dbCommit(db.con)
            bux.dta <- merge(bux.dta, dbGetQuery(db.con, paste("SELECT * FROM", i, "WHERE", cur.schema$primary.key, ">", prev.max.primary)), all=TRUE, incomparables=NA, sort=FALSE)
            
        }
        
    }
    
    if (debug==TRUE) message("Starting annotation portion")
    
    #figure out if additional annotation is present
        
    annot.cols <- setdiff(colnames(bux.dta), unique(as.character(unlist(schema.list))))
    
    if (length(annot.cols) > 0)
    {
        #does the annotation table exist?
        
        annot.tab <- setdiff(db.tables, c(names(schema.list), "sqlite_sequence"))
        
        temp.dta <- bux.dta[,c(schema.list$Chunk_Time$primary.key, annot.cols)]
        temp.dta <- temp.dta[!duplicated(temp.dta),]
        
        #if it doesn't exist
        if (length(annot.tab) == 0)
        {
            #just to get the default additional annotation table name specified via the prototype
            temp.obj <- new("BuxcoDB")
            
            dbWriteTable(db.con, annoTable(temp.obj), temp.dta, row.names=FALSE)
            
            make.annotation.indexes(db.con, anno.table=annoTable(temp.obj))
            
            cur.annot.table <- annoTable(temp.obj)
        }
        else
        {
            cur.annot.tab.cols <- dbListFields(db.con, annot.tab)
            
            if (all(annot.cols %in% cur.annot.tab.cols) == FALSE)
            {
                stop("ERROR: annotation columns are discordant between bux.dta and bux.db")
            }
            
            use.sql <- paste("INSERT INTO", annot.tab, "(", paste(colnames(temp.dta), collapse=",") ,")","VALUES (", paste(paste("$", colnames(temp.dta), sep=""), collapse=",") ,")")
            dbBeginTransaction(db.con)
            dbGetPreparedQuery(db.con, use.sql, bind.data = temp.dta)
            dbCommit(db.con)
            
            cur.annot.table <- annot.tab
        }
    }
    else
    {
    	cur.annot.table <- "Additional_labels"
    }
    
    dbDisconnect(db.con)
    
    return(makeBuxcoDB(db.name=db.name, annotation.table=cur.annot.table))
}

#incomplete for now
validate.dta <- function(bux.db, bux.dta)
{
    return(TRUE)
}
