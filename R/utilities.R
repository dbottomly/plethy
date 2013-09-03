#maybe make more general eventually and allow adding labels by more than just sample
add.labels.by.sample <- function(bux.db, sample.labels)
{
    if (class(bux.db) != "BuxcoDB")
    {
        stop("ERROR: bux.db needs to be an object of class BuxcoDB")
    }
    
    if (is.data.frame(sample.labels) == FALSE || is.null(names(sample.labels)) || any(names(sample.labels) == "samples") == FALSE)
    {
        stop("ERROR: sample.labels needs to be a data.frame with a 'samples' column")
    }
    
    if (all(sample.labels$samples %in% samples(bux.db)) == FALSE)
    {
        diff.samps <- setdiff(sample.labels$sample, samples(bux.db))
        stop(paste("ERROR: Invalid samples specified:", paste(diff.samps, collapse=",")))
    }
    
    if (any(is.na(sample.labels)))
    {
        stop("ERROR: There cannot be NAs in sample.labels")
    }
    
    if (ncol(sample.labels) == 1)
    {
        stop("ERROR: There are no label columns specified")
    }
    
    bux.con <- dbConnect(SQLite(), dbName(bux.db))
    
    #write the data to a temporary table
    
    if ("temp_table" %in% dbListTables(bux.con))
    {
        warning("Table 'temp_table' already exists dropping...")
        dbGetQuery(bux.con, "DROP TABLE temp_table")
    }
    
    label.cols <- setdiff(names(sample.labels), "samples")
    
    #make sure these label columns don't clash with existing columns
    
    all.db.cols <- unlist(lapply(dbListTables(bux.con), function(x)
           {
                dbListFields(bux.con, x)
           }))
    
    if (any(label.cols %in% all.db.cols))
    {
        dbDisconnect(bux.con)
        stop(paste("ERROR: Column(s):", paste(intersect(all.db.cols, label.cols), collapse=","), "already found in database"))
    }
    
    label.types <- sapply(label.cols, function(x)
           {
            col.classes <- class(sample.labels[,x])
            if (is.numeric(col.classes))
            {
                return("NUM")
            }
            else if (is.character(col.classes))
            {
                return("TEXT")
            }
            else if (is.integer(col.classes))
            {
                return("INT")
            }
            else
            {
                stop(paste("ERROR: Unexpected datatype for column:", x))
            }
           })
    
    all.labels <- paste(paste(label.cols, label.types), collapse=",")
    
    dbGetQuery(bux.con, paste("CREATE TEMPORARY TABLE temp_table (Sample_Name TEXT, ", all.labels,")"))
    
    ins.query <- paste("INSERT INTO temp_table VALUES (:Sample_Name, ",paste(paste0(":", label.cols), collapse=","),")")
    
    names(sample.labels)[names(sample.labels) == "samples"] <- "Sample_Name"
    
    dbBeginTransaction(bux.con)
    dbGetPreparedQuery(bux.con, ins.query, sample.labels)
    dbCommit(bux.con)

    #perform the merging step
    
    dbGetQuery(bux.con, paste("CREATE TABLE temp_table_2 AS SELECT ",paste(c(dbListFields(bux.con, annoTable(bux.db)), label.cols), collapse=",")," FROM Sample NATURAL LEFT OUTER JOIN temp_table NATURAL JOIN Chunk_Time NATURAL JOIN", annoTable(bux.db)))
    
    dbGetQuery(bux.con, paste("DROP TABLE", annoTable(bux.db)))
    
    dbGetQuery(bux.con, paste("CREATE TABLE", annoTable(bux.db), "AS SELECT * FROM temp_table_2"))
    
    dbGetQuery(bux.con, paste("DROP TABLE temp_table"))
    dbGetQuery(bux.con, paste("DROP TABLE temp_table_2"))
    
    invisible(dbDisconnect(bux.con))
}

adjust.labels <- function(bux.db, err.breaks.dta)
{
    if (class(bux.db) != "BuxcoDB")
    {
        stop("ERROR: bux.db needs to be an object of class BuxcoDB")
    }
    
    if (is.data.frame(err.breaks.dta) == FALSE || all(colnames(err.breaks.dta) %in% c("Sample_Name", "Variable_Name", "Rec_Exp_date", "Break_number", "Break_type_label", "num_entries", "inferred_labs")) == FALSE)
    {
        stop("ERROR: The provided err.breaks.dta is not a data.frame with appropriate columns (please use the 'get.err.breaks' function)")
    }
    
    temp.table <- "temp_table"
    
    bux.con <- dbConnect(SQLite(), dbName(bux.db))
    
    #check to see if this has been done before and stop if it has for now...
    
    if(any(dbListFields(bux.con, annoTable(bux.db)) == "Break_type_label_orig"))
    {
        dbDisconnect(bux.con)
        stop("ERROR: It appears that this operation has been performed before, performing it multiple times is currently not supported")
    }
    
    dbGetQuery(bux.con, "CREATE TEMPORARY TABLE temp_table (Sample_Name TEXT, Variable_Name TEXT, Rec_Exp_date TEXT, Break_number INT, inferred_labs TEXT)")
    
    ins.query <- paste("INSERT INTO temp_table VALUES (:Sample_Name, :Variable_Name, :Rec_Exp_date, :Break_number, :inferred_labs)")
    
    dbBeginTransaction(bux.con)
    dbGetPreparedQuery(bux.con, ins.query, err.breaks.dta)
    dbCommit(bux.con)

    use.cols <- dbListFields(bux.con, annoTable(bux.db))
    
    which.btl <- which(use.cols == "Break_type_label")
    
    if (length(which.btl) != 1)
    {
        stop("ERROR: Cannot find Break_type_label in the list of columns")
    }
    
    use.cols[which.btl] <- paste(use.cols[which.btl], " AS Break_type_label_orig")
    
    dbGetQuery(bux.con, paste("CREATE TEMPORARY TABLE temp_table_2 AS SELECT", paste(use.cols, collapse=","), ", IFNULL(inferred_labs, Break_type_label) AS Break_type_label FROM ",annoTable(bux.db),"NATURAL JOIN Chunk_Time NATURAL JOIN Sample NATURAL JOIN Variable NATURAL LEFT OUTER JOIN temp_table"))
    
    #check to make sure the new version is the same size as the old version
    
    same.len <- unlist(as.numeric(dbGetQuery(bux.con, "SELECT COUNT(*) from temp_table_2;"))) == unlist(as.numeric(dbGetQuery(bux.con, paste("SELECT COUNT(*) from", annoTable(bux.db)))))
    
    if (! same.len)
    {
        stop("ERROR: The new database is not of equal length to the old database")
    }
    
    #now drop the old annotTable and replace it with the temporary one...
    
    dbGetQuery(bux.con, paste("DROP TABLE", annoTable(bux.db)))
    
    dbGetQuery(bux.con, paste("CREATE TABLE", annoTable(bux.db), "AS SELECT * FROM temp_table_2"))
    
    dbGetQuery(bux.con, paste("DROP TABLE temp_table"))
    dbGetQuery(bux.con, paste("DROP TABLE temp_table_2"))
    
    invisible(dbDisconnect(bux.con))
}

get.err.breaks <- function(bux.db, max.exp.count=150, max.acc.count=900, vary.perc=.1, label.val="ERR")
{
    if (class(bux.db) != "BuxcoDB")
    {
        stop("ERROR: bux.db needs to be an object of class BuxcoDB")
    }
    
    if (length(max.exp.count) != 1 || is.numeric(max.exp.count) == FALSE || max.exp.count < 1)
    {
        stop("ERROR: max.exp.count needs to be a single numeric value > 0")
    }
    
    if (length(max.acc.count) != 1 || is.numeric(max.acc.count) == FALSE || max.acc.count < 1)
    {
        stop("ERROR: max.acc.count needs to be a single numeric value > 0")
    }
    
    if (length(vary.perc) != 1 || is.numeric(vary.perc) == FALSE || vary.perc <= 0 || vary.perc >= 1)
    {
        stop("ERROR: vary.perc needs to be a single numeric value between 0 and 1")
    }
    
    if (is.character(label.val) == FALSE || length(label.val) > 1 || label.val %in% c("ACC", "EXP") || label.val %in% annoLevels(bux.db)$Break_type_label == FALSE)
    {
        stop("ERROR: label.val needs to be a single character and in the Break_type_label column of the annotation table (other than 'ACC' or 'EXP')")
    }
    
    db.con <- dbConnect(SQLite(), dbName(bux.db))
    
    break.count.dta <- dbGetQuery(db.con, paste0("SELECT Sample_Name, Variable_Name, Rec_Exp_date, Break_number, Break_type_label, COUNT(Time_ID) AS num_entries from Chunk_Time NATURAL JOIN Additional_labels NATURAL JOIN Variable NATURAL JOIN Sample WHERE Break_type_label = '",label.val,"' GROUP BY Sample_Name, Variable_Name, Rec_Exp_date, Break_number, Break_type_label"))
    
    dbDisconnect(db.con)
    
    #also attempt to determine if the breaks should be labeled otherwise
    
    which.exp <- break.count.dta$num_entries > (max.exp.count - (max.exp.count*vary.perc)) & break.count.dta$num_entries <= max.exp.count
    which.acc <- break.count.dta$num_entries > (max.acc.count - (max.acc.count*vary.perc)) & break.count.dta$num_entries <= max.acc.count
    
    break.count.dta$inferred_labs <- break.count.dta$Break_type_label
    break.count.dta$inferred_labs[which.exp] <- "EXP"
    break.count.dta$inferred_labs[which.acc] <- "ACC"
    
    return(break.count.dta)
}

basic.table.headers <- list("WBPth"=c("f","TVb","MVb","Penh","PAU","Rpef","Comp","PIFb","PEFb","Ti","Te","EF50","Tr","Tbody","Tc","RH","Rinx"))

generate.sample.buxco <- function(break.dta, table.line="Table", table.var.list=basic.table.headers, measure.lines=c("Measurement", "Create measurement", "Waiting for", "Site Acknowledgement Changed"))
{
    baseline.time <- Sys.time()
    
    sim.vec <- as.character(unlist(lapply(split(break.dta, cumsum(break.dta$table_break)), function(x)
           {
                #for each table
                unlist(lapply(split(x, cumsum(x$measure_break)), function(y)
                       {
                            #for each output chunk
                            unlist(lapply(1:nrow(y), function(z)
                                   {
                                        if (y$table_break[z])
                                        {
                                            #need to allow more than one table...
                                            return(c(paste(table.line, names(table.var.list)[1]), paste0(paste(c("Time","Subject","Phase","Recording", table.var.list[[1]]), collapse=","),",")))
                                        }
                                        else if (y$measure_break[z])
                                        {
                                            #need to add in a time stamp, value followed by the appropriate amount of ','
                                            return(paste(format(baseline.time, "%m/%d/%Y %I:%M:%S %p"), measure.lines, paste(rep(",", 4+length(table.var.list[[1]])-2), collapse=""),sep=","))
                                        }
                                        else
                                        {
                                            #generate baseline timestamp
                                            return(sapply(1:y$count[z], function(w)
                                                   {
                                                        dta.vec <- abs(rnorm(length(basic.table.headers[[1]])))
                                                        baseline.time <<- baseline.time+2
                                                        return(paste0(paste(c(format(baseline.time, "%m/%d/%Y %I:%M:%S %p"), y$samples[z], y$phase[z], "original", dta.vec), collapse=","),","))
                                                   }, USE.NAMES=FALSE))
                                        }
                                   }))
                       }))
           })))
}

find.bux.breaks <- function(filename, burn.in.lines=c("Measurement", "Create measurement", "Waiting for", "Site Acknowledgement Changed"), table.delim = "Table")
{
    temp <- readLines(filename)
    
    tables.pos <- plethy:::multi.grep(patterns=table.delim, str.vec=temp)
    table.ranges <- plethy:::find.break.ranges.integer(tables.pos, length(temp))
    cur.header <- character()
    
    table.ranges <- table.ranges[table.ranges$width > 1,]
    
    all.dta <- do.call("rbind", lapply(1:nrow(table.ranges), function(x)
           {
                tab.temp <- temp[table.ranges$start[x]:table.ranges$end[x]]
                break.pos <- plethy:::multi.grep(patterns=burn.in.lines, str.vec=tab.temp)
                break.ranges <- plethy:::find.break.ranges.integer(break.pos, length(tab.temp))
                    
                cbind(Table_num=x, do.call("rbind", lapply(1:nrow(break.ranges), function(y)
                {
                    cur.break <- tab.temp[break.ranges$start[y]:break.ranges$end[y]]
                    if(y == 1)
                    {
                        cur.table <- plethy:::csv.to.table(cur.break, header=TRUE)
                        cur.header <<- names(cur.table)
                    }
                    else
                    {
                        cur.table <- plethy:::csv.to.table(cur.break, header=FALSE)
                        names(cur.table) <- cur.header
                    }
                    
                    cur.table <- plethy:::fix.time(cur.table)
                    
                    split.tab <- split(cur.table, list(cur.table$Subject, cur.table$Phase), drop=TRUE)
                    
                    cbind(Break.num=y, do.call("rbind", lapply(split.tab, function(z)
                           {
                                if (is.na(min(z$posix.time)))
                                {
                                    browser()
                                }
                                ret.dta <- data.frame(Sample=unique(as.character(z$Subject)), Phase=unique(as.character(z$Phase)), Size=nrow(z), min.time=min(z$posix.time), max.time=max(z$posix.time))
                           })))
                    
                })))
           }))
    
    return(all.dta)
}

#basic sanity checks to ensure the parsing of the Buxco file seemed to be performed correctly
#emits warnings if the resulting database looks outside of parameters and returns the queries as an invisble list of data.frames if the acutal output needs to be inspected.
proc.sanity <- function(bux.db, max.exp.time=300, max.acc.time=1800, max.exp.count=150, max.acc.count=900)
{
    db.con <- dbConnect(SQLite(), dbName(bux.db))
    
    time.dta <- dbGetQuery(db.con, "SELECT Break_type_label, MIN(Break_sec_start) AS min_seconds, MAX(Break_sec_start) AS max_seconds from Additional_labels NATURAL JOIN Chunk_Time GROUP BY Break_type_label;")
    
    looks.good <- TRUE
    
    if (time.dta$max_seconds[time.dta$Break_type_label == "ACC"] > max.acc.time)
    {
        warning("Max ACC values outside of max.acc.time parameter")
    }
    
    if (time.dta$max_seconds[time.dta$Break_type_label == "EXP"] > max.exp.time)
    {
        warning("Max EXP values outside of specified max.exp.time parameter")
    }
    
    count.dta <- dbGetQuery(db.con, "SELECT Sample_Name, Variable_Name, Days, Break_type_label, COUNT(Time_ID) AS num_entries from Chunk_Time NATURAL JOIN Additional_labels NATURAL JOIN Variable NATURAL JOIN Sample GROUP BY Sample_Name, Variable_Name, Days, Break_type_label")

    
    if (any(count.dta$num_entries[count.dta$Break_type_label == "ACC"] > max.acc.count))
    {
        warning("Number of ACC values outside of specified max.acc.count parameter")
    }
    
    if (any(count.dta$num_entries[count.dta$Break_type_label == "EXP"] > max.exp.count))
    {
        warning("Number of EXP values outside of specified max.exp.count parameter")
    }
    
    if (all(annoLevels(bux.db)$Break_type_label %in% c("ACC", "EXP")) == FALSE)
    {
        warning("Break_type_labels other than ACC or EXP found")
    }
    
    dbDisconnect(db.con)
    
    invisible(list(time=time.dta, count=count.dta))
}

#assuming Sample_Name column looks like: 15156x1566  f99, subtracts Sample_Name and adds the columns:
#ID as RIX.ID_Mating
#RIX.ID
#Mating
fix.sample.ids <- function(bux.dta)
{
    non.samp.name.col <- setdiff(names(bux.dta), "Sample_Name")
    
    split.samp.names <- strsplit(bux.dta$Sample_Name, "\\s+")
    
    bux.dta$Mating <- sapply(split.samp.names, "[", 1)
    bux.dta$RIX_ID <- sub("f", "", sapply(split.samp.names, "[", 2))
    bux.dta$ID <- paste(bux.dta$Mating, bux.dta$RIX_ID, sep="_")
    
    return(bux.dta[,c("ID", "RIX_ID", "Mating", non.samp.name.col)])
}

