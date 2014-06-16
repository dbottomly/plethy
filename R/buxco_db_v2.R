
###TODO: Issues with unit tests for db.insert.autoinc and examine.table.lines indicate possible unexpected behavior with data.frames having factors as opposed to characters, need to check
##TODO: Update data loading portion of db.insert.autoinc

#Parses Buxco data in the following manner:
#Assumes that the strings specified in burn.in.lines delimit the sets (chunks) of measurements in the following form:

#table.delim table name
#header line
#acclimation (ACC) for animal 1
#delim
#acclimation (ACC) for animal 2
#experimental readings (EXP) for animal 1
#delim
#experimental readings (EXP for animal 2
#acclimation (ACC) for animal 3

sample.db.path <- function()
{
    system.file(file.path("extdata", "BuxcoR_sample.db"), package = "plethy")
}

buxco.sample.data.path <- function()
{
    system.file(file.path("extdata","BuxcoR_sample.csv"), package = "plethy")
}


#Note that table.delim and burn.in.lines can be partial matching (see multi.grep below).

#A function that records the actual experimental date based on the POSIXlt coded timestamps for each measurement.
day.infer.query <- function(obj)
{
    min.time.query <- "(SELECT Sample_ID, MIN(STRFTIME('%J', P_Time)) AS Min_time FROM Timepoint JOIN Chunk_Time USING (Time_ID) GROUP BY Sample_ID)"
    overall.query <- paste("SELECT Break_Chunk_ID, ROUND(STRFTIME('%J', P_Time) - Min_time) AS Days FROM Timepoint JOIN Chunk_Time USING (Time_ID) JOIN", min.time.query, " USING (Sample_ID)")
    return(overall.query)
}

#A function that labels measurement series for each sample and timepoint and based on the ordering of the breaks
break.type.query <- function(obj)
{
    table.with.date <- annoTable(obj)
    overall.case <- "CASE ((Break_number - Min_break) || Distinct_breaks) WHEN '02' THEN 'ACC' WHEN '12' THEN 'EXP' WHEN '01' THEN 'UNK' ELSE 'ERR' END"
    min.cat.query <- paste("(SELECT Sample_ID, Days, Bux_Table_ID, MIN(Break_number) AS Min_break, COUNT(DISTINCT(Break_number)) AS Distinct_breaks  FROM Chunk_Time JOIN ",table.with.date," USING (Break_Chunk_ID) GROUP BY Sample_ID, Days, Bux_table_ID)")
    overall.query <- paste("SELECT Break_Chunk_ID,", overall.case ,"AS Break_type_label FROM Chunk_Time JOIN ",table.with.date," USING (Break_Chunk_ID) JOIN", min.cat.query, " USING (Sample_ID, Days, Bux_table_ID)")
    return(overall.query)
}

#A function that labels groups of samples based on their (first) timepoint assuming that that would indicate whether the animals were part of the same group of animals run at once. 
chamber.inference.query <- function(obj)
{
    return(c("CREATE TEMPORARY TABLE min_time AS SELECT DISTINCT MIN(Time_ID) AS Min_time FROM Chunk_Time GROUP BY Sample_ID,Rec_Exp_date",
            "CREATE TEMPORARY TABLE samp_sum AS SELECT Sample_ID, Rec_Exp_date, MIN(Time_ID) AS Min_time FROM Chunk_Time GROUP BY Sample_ID, Rec_Exp_date",
           "SELECT Break_Chunk_ID, min_time.ROWID AS Chamber_group_ID FROM samp_sum JOIN min_time USING (Min_time) JOIN Chunk_Time USING (Sample_ID, Rec_Exp_date)"))
}

#A simple utility to ensure that the 'query.map' supplied to execute.query.map appears to be valid before attempting to process it.
validate.query.map <- function(query.map)
{
    all(sapply(query.map, function(x)
            {
                if (sum(names(x) == "table") != 1)
                {
                    return(FALSE)
                }
                else if (any(names(x) == "value") == TRUE && any(names(x) == "column") == FALSE)
                {
                    return(FALSE)
                }
                else if(length(x) > 3 || length(x) == 0)
                {
                    return(FALSE)
                }
                else
                {
                    return(TRUE)
                }
            }))
}

#a query.map is a list of lists named by keywords which represent tables.  Each internal list has a 'table' elment and potentially'column' and 'value' elements.
#If a column element is present it is used as a constraint for the result based on the the vector in 'value' if present.  The resulting query is returned.
#No tests written for this function as it will be tested through the public interface 'retrieveData'.
execute.query.map <- function(db.con, query.map, debug=FALSE)
{
    if (class(db.con) != "SQLiteConnection")
    {
        stop("ERROR: db.con needs to be an object of class SQLiteConnection")
    }
    
    if(validate.query.map(query.map) == FALSE)
    {
        stop("ERROR: Invalid query.map supplied")
    }

    query.tables <- sapply(query.map, "[[", "table")
    base.from <- paste(unique(query.tables), collapse=" NATURAL JOIN ")
    
    #where column doesn't exist (set to NULL or missing) don't return any column but still join the table
    use.select <- sapply(query.map, function(x) is.null(x$column) == FALSE)
    query.map <- query.map[use.select]
    
    query.columns <- sapply(query.map, "[[", "column")
    base.select <- paste("SELECT", paste(query.columns, collapse=", "), "FROM")
    
    #where value doesn't exist (set to NULL or missing) don't place a constraint on the query
    use.where <- sapply(query.map, function(x) is.null(x$value) == FALSE)
    query.map <- query.map[use.where]
    
    if (length(query.map) == 0)
    {
        base.where <- ""
    }
    else
    {
        base.where <- paste("WHERE", paste(sapply(query.map, function(x)
                                                  {    
                                                        if (is.true.character(as.character(x$value)))
                                                        {
                                                            x$value <- paste("'",x$value, "'", sep="")
                                                        }
                                                        
                                                        if (length(x$value) == 1)
                                                        {
                                                            paste(x$column, "=", x$value)
                                                        }
                                                        else if (length(x$value) > 1)
                                                        {
                                                            paste(x$column, "in (", paste(x$value, collapse=","), ")")
                                                        }
                                                        else
                                                        {
                                                            stop("ERROR: Unexpected number of values")
                                                        }
                                                  }), collapse=" AND "))
    }
    
    final.query <- paste(base.select, base.from, base.where)
    if (debug == TRUE) message(final.query)
    return(dbGetQuery(db.con, final.query))
}

#A simple function to carry out a single query on a column specified by var.name appended to col.suffix from the var.name table.
#Assuming that such a column exists.  The resulting column is returned.
get.simple.single.col.query <- function(db.name, var.name, col.suffix="_Name")
{

    if (is.character(var.name) == FALSE || length(var.name) != 1 || file.exists(db.name) == FALSE)
    {
        stop("ERROR: db.name needs to be a valid file name")    
    }
    
    if (is.character(var.name) == FALSE || length(var.name) != 1)
    {
        stop("ERROR: var.name needs to be a character vector of length 1")    
    }
    
    if (is.character(col.suffix) == FALSE || length(col.suffix) != 1)
    {
        stop("ERROR: col.suffix needs to be a character vector of length 1")    
    }
    
    db.con <- dbConnect(SQLite(), db.name)

    col.name <- paste(var.name, col.suffix, sep="")
    query <- paste("SELECT", col.name ,"FROM", var.name)
    
    query.res <- dbGetQuery(db.con, query)[,col.name]
    
    dbDisconnect(db.con)
    
    return(query.res)
}

#this function parses a Buxco output file and creates a local SQLite database as defined in create.tables()
#table.delim and burn.in.lines define how the lines of the file are seperated into categories: first seperated by table and then by the lines indicating transition
#from one phase measurement phase to another (i.e. acclimation to experimental measurements)
parse.buxco <- function(file.name=NULL, table.delim="Table", burn.in.lines=c("Measurement", "Create measurement", "Waiting for",
                        "Site Acknowledgement Changed"), chunk.size=500,db.name="bux_test.db", max.run.time.minutes=60, overwrite=TRUE, verbose=TRUE)
{
    if (missing(file.name) || is.null(file.name))
    {
        stop("ERROR: Please supply a non-NULL argument for file.name")
    }
    else if (! file.exists(file.name))
    {
        stop(paste("ERROR:", file.name, " does not exist"))
    }
    
    if (is.logical(overwrite) == TRUE && overwrite==TRUE)
    {
        if (file.exists(db.name))
        {
            file.remove(db.name)
        }
        
        db.con <- dbConnect(SQLite(), db.name)
        create.tables(db.con)
    }
    else if (is.logical(overwrite) == TRUE && overwrite==FALSE)
    {
        db.con <- dbConnect(SQLite(), db.name)
    }
    else
    {
        stop("ERROR: Unexpected value for overwrite")
    }
    
    ret.list <- new("RetList", max.run.time.mins=max.run.time.minutes)
    
    bux.prod <- ReadLinesProducer(con=file.name, n=chunk.size)
    
    cur.chunk <- 1
    
    if (verbose == TRUE) message(paste("Processing", file.name, "in chunks of", chunk.size))
    
    while(length(bux.lines <- yield(bux.prod)))
    {
        if (verbose == TRUE && cur.chunk %% 1 == 0)
        {
            message(paste("Starting chunk", cur.chunk))
        }
        
        ret.list <- examine.table.lines(bux.lines=bux.lines, table.delim=table.delim, burn.in.lines=burn.in.lines, ret.list=ret.list, db.con=db.con, verbose=verbose)
    
        cur.chunk <- cur.chunk + 1
    }
    
    close(bux.prod)
    
    #write the remaining contents of samp.tab here...
    
    if (verbose == TRUE) message("Reached the end of the file, writing remaining data")
    write.sample.db(db.con=db.con, dta.tab=sampTab(ret.list), ret.list=ret.list, verbose=verbose)
    
    dbDisconnect(db.con)
    
    return(makeBuxcoDB(db.name=db.name, annotation.table="Additional_labels"))
}

#takes the read in bux.lines and searches them for presence of the string defined in table.delim
#It parses each table seperately using the values specified in burn.in.lines and expects a maximum of two tables
#ret.list is a RetList object
#db.con is a DBIConnection object to a SQLite database
#verbose is a boolean
examine.table.lines <- function(bux.lines, table.delim, burn.in.lines, ret.list, db.con, verbose)
{
    if (is.character(bux.lines) == FALSE || length(bux.lines) < 1)
    {
        stop("ERROR: bux.lines needs to be a character vector with greater than 1 element")
    }

    if (is.character(table.delim) == FALSE || length(table.delim) != 1)
    {
        stop("ERROR: table.delim needs to be a character vector of length 1")   
    }
    
    if (is.character(burn.in.lines) ==FALSE || length(burn.in.lines) < 1)
    {
        stop("ERROR: burn.in.lines needs to be a character vector with at least one element")
    }
    
    if (class(db.con) != "SQLiteConnection")
    {
        stop("ERROR: db.con needs to be an object of class SQLiteConnection")
    }
    
    if (class(ret.list) != "RetList")
    {
        stop("ERROR: ret.list needs to an object of class RetList")
    }
    
    if (is.logical(verbose) == FALSE || length(verbose) != 1)
    {
        stop("ERROR: verbose needs to be a logical vector of length 1")
    }

    #first see if there is a start to a new table
    table.points <- grep(table.delim, bux.lines)
    
    if (length(table.points) > 0)
    {
        #ok, you found at least one table
        #split the lines into tables based on a line corresponding to: table.delim table name
        table.break.dta <- find.break.ranges.integer(as.integer(table.points), length(bux.lines))
        table.break.name <- sub(paste(table.delim, "\\s+", sep=""), "", bux.lines[table.points])
        buxTable(ret.list) <- db.insert.autoinc(db.con, table.name="Bux_table", col.name="Bux_table_Name", values=table.break.name,
                                                        return.query.type="all")
        
        if (nrow(table.break.dta) == 1)
        {
            curTable(ret.list) <- table.break.name
            table.lines <- bux.lines[table.break.dta$start[1]:table.break.dta$end[1]]
            
        }
        else if (nrow(table.break.dta) == 2)
        {
            #if there is more than one table then both need to finish the previous table and start the new one
            
            ###modification 3-27-2013 to deal with empty tables at the begining
            
            if (table.break.dta$width[1] > 1)
            {
                ret.list <- table.lines.to.dta(table.lines=bux.lines[table.break.dta$start[1]:table.break.dta$end[1]], ret.list=ret.list, db.con=db.con, tab.name=curTable(ret.list),
                                           burn.in.lines=burn.in.lines)
                ret.list <- write.sample.breaks(db.con=db.con, ret.list=ret.list, verbose=verbose)
            
                curTable(ret.list) <- table.break.name
                table.lines <- bux.lines[table.break.dta$start[2]:table.break.dta$end[2]]
            }
            else
            {
                curTable(ret.list) <- table.break.name[2]
                table.lines <- bux.lines[table.break.dta$start[2]:table.break.dta$end[2]]
            }
        }
        else
        {
            stop("ERROR: Unexpected number of rows for table.break.dta")
        }
        
        collectHeader(ret.list) <- character()
        
    }
    else
    {
        table.lines <- bux.lines
    }
    
    ###modification 3-27-2013 to deal with empty tables at the end
    if (length(table.lines) > 1)
    {
        ret.list <- table.lines.to.dta(table.lines=table.lines, ret.list=ret.list, db.con=db.con, tab.name=curTable(ret.list), burn.in.lines=burn.in.lines)
        ret.list <- write.sample.breaks(db.con=db.con, ret.list=ret.list, verbose=verbose)
    }
    
    return(ret.list)
}

#takes a database connect and a RetList object and writes data for each sample (if fully contained in the RetList object) to the database
#otherwise it simply returns the same RetList object that was supplied to it.
#It determines whether the data for a given sample is complete by looking for a change in the break.num column and writes all the data for the complete samples
#until the last break is reached.  At that point, the sample data is set to the data retrieved from the last break point.
#db.con is a DBIConnect object
#ret.list is a RetList object
#verbose is a boolean
write.sample.breaks <- function(db.con, ret.list, verbose)
{
    if (class(db.con) != "SQLiteConnection")
    {
        stop("ERROR: db.con needs to be an object of class SQLiteConnection")
    }
    
    if (class(ret.list) != "RetList")
    {
        stop("ERROR: ret.list needs to an object of class RetList")
    }
    
    if (is.logical(verbose) == FALSE || length(verbose) != 1)
    {
        stop("ERROR: verbose needs to be a logical vector of length 1")
    }
    
    #for a given sample, is there a breakpoint change?
    if (length(unique(sampTab(ret.list)$break.num)) > 1)
    {
        if (verbose == TRUE) message("Reached breakpoint change")
        
        split.samps <- split(sampTab(ret.list), sampTab(ret.list)$break.num)
        for (i in 1:(length(split.samps)-1))
        {
            if (verbose == TRUE) message(paste("Processing breakpoint", names(split.samps)[i]))
            write.sample.db(db.con=db.con, dta.tab=split.samps[[i]], ret.list=ret.list, verbose=verbose)
        }
        
        sampTab(ret.list) <- split.samps[[length(split.samps)]]
    }
    
    return(ret.list)
}

#takes in the raw input lines for each chunk of the buxco output file, populates the Variable database table and adds the relevant sample data to ret.list
#table.lines is a character vector of length greater than 0 containing CSV seperated elements.
#ret.list is a RetList object
#db.con is a DBIConnection object to the specified SQLite database
#tab.name is a character string containing the current table name
#NOTE: This function is not included in the unit tests as it largely just calls the a set of helper functions which do the bulk of the work.
table.lines.to.dta <- function(table.lines, ret.list, db.con, tab.name, burn.in.lines)
{
    if (class(ret.list) != "RetList")
    {
        stop("ERROR: ret.list needs to an object of class RetList")
    }
    
    if (class(db.con) != "SQLiteConnection")
    {
        stop("ERROR: db.con needs to be an object of class SQLiteConnection")
    }
    
    if (is.character(tab.name) == FALSE || length(tab.name) != 1)
    {
        stop("ERROR: tab.name needs to be a character vector of length one")
    }
    
    if (length(collectHeader(ret.list)) == 0)
    {
        tab <- csv.to.table(table.lines, header=TRUE)
        collectHeader(ret.list) <- colnames(tab)
        variable(ret.list) <- db.insert.autoinc(db.con, table.name="Variable", col.name="Variable_Name",
                                                  values=setdiff(collectHeader(ret.list), c("Time","Subject","Phase","Recording")),
                                                  return.query.type="all")
    }
    else
    {
        tab <- csv.to.table(table.lines, header=FALSE)
        colnames(tab) <- collectHeader(ret.list)
    }
    
    tab$tab.name <- tab.name
    
    break.dta.list <- make.break.dta(tab, burn.in.lines)
    
    break.dta <- correct.breaks(break.dta.list$break.dta, ret.list, burn.in.lines, tab.name)
    
    ret.list <- add.breaks.to.tab(tab, break.dta, ret.list)
    
    #breaks at the end affect the next batch instead of current, so this has to be done at the end.
    breakAtEnd(ret.list) <- break.dta.list$break.at.end
    
    return(ret.list)
}

#add a column to a given data.frame with the time formated in a more portable way
fix.time <- function(dta)
{
    if (is.data.frame(dta) == FALSE || "Time" %in% names(dta) == FALSE)
    {
        stop("ERROR: dta needs to be a data.frame with a column named 'Time'")
    }
    
    dta$posix.time <- as.character(strptime(dta$Time, format="%m/%d/%Y %I:%M:%S %p"))
    
    return(dta)
}

#annotate tab with the break numbers added to break.dta in correct.breaks that the data is associated with and add it to the RetList object supplied in ret.list
#tab is a data.frame as generated in 'csv.to.table'
#break.dta is a data.frame as generated in 'make.break.dta'
#ret.list is a RetList object
add.breaks.to.tab <- function(tab, break.dta, ret.list)
{
    if (is.data.frame(tab) == FALSE || nrow(tab) < 1 || all("Subject" %in% names(tab)) == FALSE)
    {
        stop("ERROR: tab needs to be a data.frame with greater than 1 row with at least a Subject column")
    }
    
    if (is.data.frame(break.dta) == FALSE || nrow(break.dta) < 1 || all(c("start", "end", "width", "break.num") %in% names(break.dta))==FALSE)
    {
        stop("ERROR: break.dta needs to be a data.frame with at least one row containing 'start', 'end', 'width' and 'break.num' columns")
    }
    
    if (class(ret.list) != "RetList")
    {
        stop("ERROR: ret.list is not of class RetList")
    }
    
    if (nrow(variable(ret.list)) == 0)
    {
        stop("ERROR: Need to specify at least one variable in ret.list")
    }
    
    common.vars <- intersect(variable(ret.list)$Variable_Name, names(tab))
    
    if (length(common.vars) == 0)
    {
        stop("ERROR: No variables shared between the current chunk and the variable table")
    }
    
    tab$break.num <- 0
    
    for (i in 1:nrow(break.dta))
    {
        tab$break.num[break.dta$start[i]:break.dta$end[i]] <- break.dta$break.num[i]
    }
    
    tab <- tab[tab$break.num != 0,]
    
    unique.anim.break <- tab[,c("Subject", "break.num")]
    unique.anim.break <- unique.anim.break[!duplicated(unique.anim.break),]
    
    break.dta <- merge(break.dta, unique.anim.break, by="break.num", all=TRUE, incomparables=NA, sort=FALSE)
    
    breakData(ret.list) <- aggregate.breaks(breakData(ret.list), break.dta) 
    
    sampTab(ret.list) <- rbind(sampTab(ret.list), melt(tab, measure.vars=common.vars))
    
    return(ret.list)
}

#adds in or correct the break.num element of break.dta based on whether there is a break observed in the current chunk as well as when a new table is found
#and the break occurs at the very end of the previous break.dta as determined in make.break.dta.
#break.dta is a data.frame generated from make.break.dta which contains start, end and width elements
#ret.list is a RetList object
#burn.in.lines is a character vector containing the lines delimiting sections of buxco data
#tab.name is a character string containing the name of the current table
###NOTE: There is a possible issue here if for some reason not all of the burn.in.lines were found.  The breaks might be incorrectly labeled in this case.
correct.breaks <- function(break.dta, ret.list, burn.in.lines, tab.name)
{
    if (is.data.frame(break.dta) == FALSE || nrow(break.dta) < 1 || all(names(break.dta) %in% c("start", "end", "width")) == FALSE)
    {
        stop("ERROR: break.dta needs to be a data.frame with at least one row and contain columns denoting start, end and width")
    }
    
    if (class(ret.list) != "RetList")
    {
        stop("ERROR: ret.list needs to be a valid object of class RetList")
    }
    
    if (is.character(burn.in.lines) ==FALSE || length(burn.in.lines) < 1)
    {
        stop("ERROR: burn.in.lines needs to be a character vector with at least one element")
    }
    
    if (is.character(tab.name) == FALSE || length(tab.name) != 1)
    {
        stop("ERROR: tab.name needs to be a character vector of length 1")
    }
    
    #if (all(sampTab(ret.list)$Subject == "1--4" & sampTab(ret.list)$Phase == "Day1"))
    #{
    #    browser()
    #}
    
    #Covers a corner case where the next batch has a break at the very beginning or at the very end of the previous section
    #Also if the table changes, count it as a new break
    #They shouldn't occur together as the lines fed to this function occur after the removal of the table switching lines
    if ((break.dta$start[1] == length(burn.in.lines)) || breakAtEnd(ret.list) || ((tab.name %in% sampTab(ret.list)$tab.name == FALSE) && (nrow(sampTab(ret.list)) > 0)))
    {
        break.cor <- 1
    }
    else
    {
        break.cor <- 0
    }
    
    if (nrow(breakData(ret.list)) == 0)
    {
        break.dta$break.num <- 1:nrow(break.dta) + break.cor
    }
    else
    {
        break.dta$break.num <- (max(breakData(ret.list)$break.num)):(max(breakData(ret.list)$break.num)+nrow(break.dta)-1) + break.cor
    }
    
    return(break.dta)
}

#given tab, a data.frame representing buxco data lines parsed from 'table.lines.to.dta'
#and burn.in.lines which are the lines delimiting each output section create a data.frame and boolean
#describing the sections present in the data and whether they occur at the end respectively.
make.break.dta <- function(tab, burn.in.lines)
{
    if (is.data.frame(tab) == FALSE || nrow(tab) < 1)
    {
        stop("ERROR: tab should be a data.frame with at least one row.")
    }
    
    if (is.character(burn.in.lines) ==FALSE || length(burn.in.lines) < 1)
    {
        stop("ERROR: burn.in.lines needs to be a character vector with at least one element")
    }
    
    #if (any(tab$Subject == "1--4" & tab$Phase == "Day1"))
    #{
    #    browser()
    #}
    
    burn.in.line.points <- multi.grep(tab$Subject, burn.in.lines)
    break.dta <- find.break.ranges.integer(as.integer(burn.in.line.points), nrow(tab))
    
    if (length(burn.in.line.points) > 0 && any(burn.in.line.points == nrow(tab)))
    {
        break.at.end <- TRUE
    }
    else
    {
        break.at.end <- FALSE
    }
    
    return(list(break.dta=break.dta, break.at.end=break.at.end))
}

#given a data.frame, 'dta.tab' with the following columns:
#Subject--subject name
#Time--contains the Buxco time representation for the run
#tab.name--Buxco table name (WBPth and Metabolism as of this implementation)
#variable--Buxco variable names
#break.num--The number of sample chunks (as the machine writes in chunks for a given sample or two)
#Phase--Experimental label, often the experimental day of a timecourse
#value--The values for the Buxco variables
#and, ret.list, a RetList object containing a CurDbTabs object with the following data.frames:
#Table--a simple data.frame containing table names and their unique IDs
#   Bux_table_Name and Bux_table_ID respectively
#Variable--a simple data.frame containing variable names and their unique IDs
#   Variable_Name and Variable_ID repectively
#write the data to the connection 'db.con'
#in addition, verbose is a boolean indicating whether additional messages should be sent
write.sample.db <- function(db.con, dta.tab, ret.list, verbose)
{
    dta.tab <- fix.time(dta.tab)

    #further seperate by sample, come up with seconds from start of set using the posix.time and write to the database
    split.breaks <- split(dta.tab, dta.tab$Subject)
    
    for (j in 1:length(split.breaks))
    {
        if (verbose ==TRUE) message(paste("Starting sample", names(split.breaks)[j]))
        cur.samp.break <- split.breaks[[j]]
        
        if (length(unique(cur.samp.break$break.num)) != 1)
        {
            stop("ERROR: Expected only a single break for a given sample")
        }
        
        #do an initial check to see if the timestamps are greater than a user specified number of hours apart, if so compute seconds from start seperately
            #for each chunk with a warning.
        
        cur.samp.break$sec.from.start <- as.integer(difftime(time1=as.POSIXlt(cur.samp.break$posix.time), time2=min(as.POSIXlt(cur.samp.break$posix.time)), units="secs"))
        
        cur.samp.break$sec.from.start <- sanity.check.time(cur.samp.break$sec.from.start, maxTime(ret.list)*60, names(split.breaks)[j], unique(cur.samp.break$break.num))
        
        cur.samp.tab <- db.insert.autoinc(db.con, table.name="Sample", col.name="Sample_Name", values=names(split.breaks)[j], return.query.type="reverse")
        cur.time.tab <- db.insert.autoinc (db.con, table.name="Timepoint", col.name="P_Time", values=as.character(unique(cur.samp.break$posix.time)), return.query.type="reverse")
        
        cur.samp.break <- get.tab.ids(cur.samp.break, list(list(merge.name="tab.name", merge.dta=buxTable(ret.list)), list(merge.name="Subject", merge.dta=cur.samp.tab),
                                            list(merge.name="posix.time", merge.dta=cur.time.tab), list(merge.name="variable", merge.dta=variable(ret.list))),
                                           merge.dta.column.pat=c("_Name", "_Name", "P_Time", "_Name"))
        
        #Data table first
        cur.samp.break$null.val <- NA_integer_
        data.tab <- cur.samp.break[,c("null.val", "Time_ID", "Variable_ID", "Sample_ID", "Bux_table_ID", "value")]
        names(data.tab)[c(1, ncol(data.tab))] <- c("Data_ID", "Value")
        
        data.query <- paste("INSERT INTO Data VALUES (", paste(paste0(":", names(data.tab)), collapse=","), ")")
        
        dbBeginTransaction(db.con)
        dbGetPreparedQuery(db.con, data.query, bind.data = data.tab)
	dbCommit(db.con)
        
        #dbWriteTable(conn=db.con, name="Data", value=data.tab, row.names = FALSE, overwrite = FALSE, append = TRUE)
        
        #Chunk_Time last
        chunk.tab <- cur.samp.break[,c("null.val", "Sample_ID", "Time_ID", "Bux_table_ID", "Variable_ID", "break.num", "sec.from.start", "Phase")]
        names(chunk.tab) <- c("Break_Chunk_ID", "Sample_ID", "Time_ID", "Bux_table_ID", "Variable_ID", "Break_number", "Break_sec_start", "Rec_Exp_date")
        
        chunk.query <- paste("INSERT INTO Chunk_Time VALUES (", paste(paste0(":", names(chunk.tab)), collapse=","), ")")
        
        dbBeginTransaction(db.con)
        dbGetPreparedQuery(db.con, chunk.query, bind.data = chunk.tab)
	dbCommit(db.con)
        
        #dbWriteTable(conn=db.con, name="Chunk_Time", value=chunk.tab, row.names=FALSE, overwrite=FALSE, append=TRUE)
        
        if (verbose ==TRUE) message("Sample written")
    }
}

#recursive helper function which checks whether the time intervals make sense with reguards to the expected time intervals and corrects them
#if necessary, with a warning.
sanity.check.time <- function(sec.vec, max.sec, sample, break.num)
{
    which.gt <- sec.vec > max.sec
    
    if (sum(which.gt) > 0)
    {
        warning(paste("Found (",sum(which.gt),"/",length(which.gt),") timepoints greater than maximum expected time length for sample", sample, ", break.num:",break.num,", computing relative to new minimum"))
        sec.vec[which.gt] <- sec.vec[which.gt] - min(sec.vec[which.gt])
        return(sanity.check.time(sec.vec, max.sec, sample, break.num))
    }
    else
    {
        return(sec.vec)
    }
}

#a function that facilitates merging of multiple data.frames
#use.dta is the initial data.frame to be merged with the data.frames in merge.list
#merge.list is a list of lists containing two elements: merge.name and merge.dta
#   merge.name is the name in use.dta that should be used for merging
#   merge.dta is the corresponding data.frame to be merged
#merge.dta.column.pat is the pattern used to find the name in merge.dta for merging
get.tab.ids <- function(use.dta, merge.list, merge.dta.column.pat="_Name")
{
    if(is.character(merge.dta.column.pat) == FALSE || (length(merge.dta.column.pat) != 1 && length(merge.dta.column.pat) != length(merge.list)))
    {
        stop("ERROR: merge.dta.column.pat needs to either be a character vector of length 1 or be equal to the length of merge.list")
    }
    
    if (length(merge.dta.column.pat) == 1)
    {
        merge.dta.column.pat <- rep(merge.dta.column.pat, length(merge.list))
    }
    
    if (is.data.frame(use.dta) == FALSE || nrow(use.dta) < 1)
    {
        stop("ERROR: use.dta needs to be a data.frame of at least one row")
    }
    
    if (is.list(merge.list) == FALSE || length(merge.list) < 1)
    {
        stop("ERROR: merge.list needs to be a list of lists with at least one element")
    }
    
    for (i in 1:length(merge.list))
    {
        cur.merge <- merge.list[[i]]
        
        if (is.list(cur.merge) == FALSE || all(names(cur.merge) %in% c("merge.name", "merge.dta")) == FALSE)
        {
            stop("ERROR: each element of cur.merge needs to be a list containing elements named 'merge.name' and 'merge.dta'")
        }
        
        merge.dta.name <- names(cur.merge$merge.dta)[grep(merge.dta.column.pat[i], names(cur.merge$merge.dta))]
        
        if (length(merge.dta.name) != 1)
        {
            stop("ERROR: the pattern specified in merge.dta.column.pat did not uniquely identify an element of merge.dta")
        }
        
        use.dta <- merge(use.dta, cur.merge$merge.dta, by.x=cur.merge$merge.name, by.y=merge.dta.name, all.x=TRUE, all.y=FALSE, incomparables=NULL, sort=FALSE)
    }
    
    return(use.dta)
}

#insert character strings into a database allowing for the generation of primary keys through autoincrementation
#reverse indicates that the same query should be used to retrieve the data after they have been inserted into the db
#all indicates that all values in the db should be retrieved
#none, well indicates that nothing (empty data.frame) should be returned
db.insert.autoinc <- function(db.con, table.name, col.name, values, return.query.type=c("reverse", "all", "none"),debug=FALSE)
{
    if (class(db.con) != "SQLiteConnection")
    {
        stop("ERROR: db.con needs to be of class SQLiteConnection")
    }
    
    if (is.character(table.name) == FALSE || length(table.name) != 1)
    {
        stop("table.name needs to be a single string value")
    }
    
    if (is.character(col.name) == FALSE || length(col.name) != 1)
    {
        stop("col.name needs to be a single string value")
    }
    
    if (is.character(values) == FALSE || length(values) < 1)
    {
        stop("values needs to be a vector of strings")
    }
    
    return.query.type <- match.arg(return.query.type, several.ok=FALSE)
    stopifnot(length(col.name) == 1)
    
    ins.dta <- data.frame(prim_key=NA_integer_, values, stringsAsFactors=FALSE)
    names(ins.dta) <- c("prim_key", col.name)
    
    query <- paste0("INSERT OR IGNORE INTO ", table.name, " VALUES (:prim_key, :",col.name,")")
    dbBeginTransaction(db.con)
    dbGetPreparedQuery(db.con, query, bind.data = ins.dta)
    dbCommit(db.con)
    
    if(return.query.type == "reverse")
    {
        rev.query <- paste("SELECT * FROM", table.name, "WHERE", col.name, "IN", "(", paste(paste("'", values, "'", sep=""), collapse=","),")")
        if(debug==TRUE) message(rev.query)
        query.res <- dbGetQuery(db.con, rev.query)
        #make sure the returned query is in proper order
        return(query.res[order(query.res[,1]),])
    }
    else if (return.query.type == "all")
    {
        all.query <- paste("SELECT * FROM", table.name)
        if(debug==TRUE) message(all.query)
        query.res <- dbGetQuery(db.con, all.query)
        return(query.res[order(query.res[,1]),])
    }
    else if (return.query.type == "none")
    {
        return(data.frame())   
    }
}

create.tables <- function(db.con)
{
    sub.tab <- "CREATE TABLE IF NOT EXISTS Sample (Sample_ID INTEGER CONSTRAINT Samp_pk PRIMARY KEY AUTOINCREMENT,
                    Sample_Name TEXT CONSTRAINT Samp_name UNIQUE)"
    var.tab <- "CREATE TABLE IF NOT EXISTS Variable (Variable_ID INTEGER CONSTRAINT Variable_pk PRIMARY KEY AUTOINCREMENT,
                    Variable_Name TEXT CONSTRAINT Var_name UNIQUE)"
    table.tab <- "CREATE TABLE IF NOT EXISTS Bux_table (Bux_table_ID INTEGER CONSTRAINT Bux_table_pk PRIMARY KEY AUTOINCREMENT,
                    Bux_table_Name TEXT CONSTRAINT Tab_name UNIQUE)"
    time.tab <- "CREATE TABLE IF NOT EXISTS Timepoint (Time_ID INTEGER CONSTRAINT Time_pk PRIMARY KEY AUTOINCREMENT,
                    P_Time TEXT CONSTRAINT Time_name UNIQUE)"
    data.tab <- "CREATE TABLE IF NOT EXISTS Data (Data_ID INTEGER CONSTRAINT Data_pk PRIMARY KEY AUTOINCREMENT,
                    Time_ID INTEGER, Variable_ID INTEGER, Sample_ID INTEGER, Bux_table_ID INTEGER, Value DOUBLE)"
    break.tab <- "CREATE TABLE IF NOT EXISTS Chunk_Time (Break_Chunk_ID INTEGER CONSTRAINT Break_Chunk_pk PRIMARY KEY AUTOINCREMENT, Sample_ID INTEGER,
                    Time_ID INTEGER, Bux_table_ID INTEGER, Variable_ID INTEGER, Break_number INTEGER, Break_sec_start INTEGER, Rec_Exp_date TEXT)"
    
    data.ind <- "CREATE INDEX Data_comp_ind ON Data (Sample_ID, Time_ID, Bux_table_ID, Variable_ID)"
    chunk.t.ind <- "CREATE INDEX Chunk_Time_comp_ind ON Chunk_Time (Sample_ID, Time_ID, Bux_table_ID, Variable_ID)"
    
    query.list <- list(sub.tab, var.tab, table.tab, time.tab, data.tab, break.tab, data.ind, chunk.t.ind)
    
    for (i in 1:length(query.list))
    {
        stopifnot(is.null(dbGetQuery(db.con, query.list[[i]])))
    }

}

aggregate.breaks <- function(all.breaks, break.dta)
{
    return(rbind(all.breaks, break.dta))
}

#simply iterate over grep patterns and concatenate the result returning an integer vector of positions
multi.grep <- function(str.vec, patterns)
{
    if (is.character(str.vec) == FALSE || length(str.vec) < 1)
    {
        stop("ERROR: str.vec needs to contain a character vector of positive length")
    }
    
    if (is.character(patterns) == FALSE || length(patterns) < 1)
    {
        stop("ERROR: patterns needs to contain a character vector of positive length")
    }
    
    grep.list <- lapply(patterns, function(x)
    {
        grep(x, str.vec, perl=TRUE)  
    })
    
    return(do.call("c", grep.list))
}

#Takes a vector of integers as input (grep.res) as well as the total length of the original query (query.len) and returns a data.frame
#representation of the starts, ends and widths corresponding to the locations in grep.res
find.break.ranges.integer <- function(grep.res, query.len)
{
    if (is.integer(grep.res) == FALSE || any(grep.res < 1))
    {
        stop("ERROR: grep.res needs to be an integer vector containing non-zero elements")
    }
    
    if (is.integer(query.len) == FALSE || length(query.len) != 1 || query.len < 1)
    {
        stop("ERROR: query.len needs to contain a single positive integer")
    }
    
    break.widths <- Rle(1:query.len %in% grep.res)
    return(as.data.frame(IRanges(break.widths == FALSE)))
}

#simple function that takes in a character vector with data values seperated by columns  (csv.vec) and returns a table.  It will use
#the first set of values as a header if header=TRUE, otherwise the R standard of 'V 'pasted to the column number.
csv.to.table <- function(csv.vec, header=TRUE)
{
    if (is.character(csv.vec) == FALSE || length(csv.vec) < 1)
    {
        stop("ERROR: csv.vec needs to be a character vector of positive length")
    }
    
    if (header == TRUE && length(csv.vec) < 2)
    {
        stop("ERROR: csv.vec needs to have greater than 1 element if header = TRUE")
    }
    else if (header == FALSE && length(csv.vec) < 1)
    {
        stop("ERROR: csv.vec needs to at least 1 element if header = FALSE")
    }
    
    if (is.logical(header) == FALSE || length(header) != 1)
    {
        stop("ERROR: header needs to be a logical vector of length 1")
    }
    
    split.lines <- strsplit(csv.vec, ",")
    
    list.len <- sapply(split.lines, length)
    
    if (all(list.len == list.len[1]) == FALSE)
    {
        stop("ERROR: Found table vectors of unequal length")
    }
    
    if (header == TRUE)
    {
        header <- split.lines[[1]]
        split.lines <- split.lines[-1]
    }
    else
    {
        header <- paste("V", 1:length(split.lines[[1]]), sep="")
    }
    
    line.table <- data.frame(do.call("rbind", split.lines), stringsAsFactors=FALSE)
    
    colnames(line.table) <- header
    
    return(line.table)
}

is.true.character <- function(values)
{
    if (is.character(values) == FALSE || length(values) < 1)
    {
        stop("ERROR: Need to supply a character vector of positive length")
    }
    
    conv.to.num <- suppressWarnings(as.numeric(values))

    if (all(is.na(conv.to.num) == TRUE))
    {
        return(TRUE)
    }
    else if (all(is.na(conv.to.num) == FALSE))
    {
        return(FALSE)
    }
    else
    {
        stop("ERROR: Unexpected value for conv.to.num")
    }
}
