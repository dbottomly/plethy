
#use.dta needs to be a data.frame with the columns Sample_Name and Days
#main is the title of the plot
#outer.group.name is the name of the column in use.dta which indicates the outermost group  (i.e. the group which is above the inner group in the hierarchy) or NULL
#inner.group.name is the name of the column in use.dta which indicates the inner-most group (or only group).
#outer.cols is a named character vector containing the colors to use for each level of the outer group
#colorbrewer.pal needs a string indicating a Rcolorbrewer palatte.
#plot.value is the column name of use.dta corresponding to the (numeric) data to be displayed on the heatmap

#code adapted from the mtvsplot CRAN package: http://www.biostat.jhsph.edu/~rpeng/RR/mvtsplot/

#outer.cols=c(Flu="black", SARS="brown", Mock="blue")
mvtsplot.data.frame <- function(use.dta, plot.value="Penh",main=plot.value, outer.group.name=NULL, inner.group.name=NULL, outer.cols=NULL, colorbrewer.pal="PRGn")
{
    
    if (is.data.frame(use.dta) == FALSE || ncol(use.dta) < 3 || all(c('Sample_Name', 'Days') %in% colnames(use.dta)) == FALSE)
    {
        stop("ERROR: use.dta needs to be a data.frame with at least 3 columns, two of which need to be named Sample_Name and Days")
    }
    
    if (length(plot.value) != 1 || is.character(plot.value) == FALSE || plot.value %in% colnames(use.dta) == FALSE || is.numeric(use.dta[,plot.value]) == FALSE)
    {
        stop("ERROR: plot.value needs to be a character vector of length 1 corresponding to a numeric column in use.dta")
    }
    
    if (missing(outer.group.name) || is.null(outer.group.name))
    {
        use.dta$temp_outer <- factor("temp")
        outer.group.name <- "temp_outer"
    }
    else if (length(outer.group.name) != 1 || is.character(outer.group.name) == FALSE || outer.group.name %in% colnames(use.dta) == FALSE || (is.character(use.dta[,outer.group.name]) == FALSE && is.factor(use.dta[,outer.group.name]) == FALSE))
    {
        stop("ERROR: outer.group.name needs to a character vector of length one corresponding to a character or factor column in use.dta")
    }
    else if (is.character(use.dta[,outer.group.name]))
    {
        use.dta[,outer.group.name] <- factor(use.dta[,outer.group.name])
    }
    
    if (missing(inner.group.name) || is.null(inner.group.name))
    {
        use.dta$temp_inner <- factor("temp")
        inner.group.name <- "Sample_Name"
    }
    if (length(inner.group.name) != 1 || is.character(inner.group.name) == FALSE || inner.group.name %in% colnames(use.dta) == FALSE || (is.character(use.dta[,inner.group.name]) == FALSE && is.factor(use.dta[,inner.group.name]) == FALSE))
    {
        stop("ERROR: inner.group.name needs to a character vector of length one corresponding to a character or factor column in use.dta")
    }
    else if (is.character(use.dta[,inner.group.name]))
    {
        use.dta[,inner.group.name] <- factor(use.dta[,inner.group.name])
    }
    
    if (outer.group.name == "temp_outer")
    {
        outer.cols <- c(temp="black")
    }
    else if ((length(outer.cols) != nlevels(use.dta[,outer.group.name]) || is.null(names(outer.cols)) || all(names(outer.cols) %in% levels(use.dta[,outer.group.name])) == FALSE || all(outer.cols %in% colors() == FALSE)))
    {
        stop("ERROR: outer.cols needs to be a named character vector corresponding to levels in the outer.group.name column containing the names of colors")
    }
    
    if (is.character(colorbrewer.pal) == FALSE || length(colorbrewer.pal) != 1 || colorbrewer.pal %in% rownames(brewer.pal.info) == FALSE)
    {
        stop("ERROR: colorbrewer.pal needs to be a single valid RColorBrewer palette.")
    }
    
    #pal.list <- rep(colorbrewer.pal, nlevels(use.dta[,outer.group.name]))
    #names(pal.list) <- levels(use.dta[,outer.group.name])
    
    mean.mat <- acast(formula=Days~Sample_Name, data=use.dta, value.var=plot.value)
    
    sample.cross <- use.dta[,c("Sample_Name", inner.group.name, outer.group.name)]
    sample.cross <- sample.cross[!duplicated(sample.cross),]
    rownames(sample.cross) <- as.character(sample.cross$Sample_Name)
    
    sample.cross <- sample.cross[colnames(mean.mat),]
    
    clear.ord <- do.call("order", sample.cross[,c(outer.group.name, inner.group.name)])
    
    mean.mat <- mean.mat[,clear.ord]
    
    sample.cross <- sample.cross[clear.ord,]
    
    outer.group <- sample.cross[,outer.group.name]
    inner.group <- sample.cross[,inner.group.name]
    
    cx <- t(scale(t(mean.mat)))
    
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(las = 1, cex.axis = 0.6)
    cn <- colnames(cx)
    nc <- ncol(cx)
    
    utime <- 1:nrow(cx)
    
    use.layout.list <- lapply(seq(from=1, length.out=nlevels(outer.group), by=2), function(x)
           {
                return (c(rep(x, 6), x+1))
           })
    
    use.layout <- do.call("rbind", use.layout.list)
    
    if (nrow(use.layout) == 1)
    {
        use.layout <- rbind(use.layout, use.layout, use.layout)
    }
    
    use.layout <- rbind(use.layout, c(rep((nlevels(outer.group)*2)+1, 6), (nlevels(outer.group)*2)+2))
  
    layout(use.layout)
    
    colm.list <- lapply(levels(outer.group), function(x)
                        {
                            apply(mean.mat[,outer.group == x], 2, boxplot, plot=F)
                        })
    
    names(colm.list) <- levels(outer.group)
    
    right.xlim <- range(do.call("cbind", lapply(unlist(colm.list, recursive=F), "[", "stats")), na.rm = TRUE)
   
    #side2 <- max(0.55, max(strwidth(cn, "inches")) + 0.1)
    side2 <- max(strwidth(cn, "inches")) + .5
    
    use.heat.colors <- brewer.pal(4, colorbrewer.pal)
    
    for (i in levels(outer.group))
    {
        #c(bottom, left, top, right)
        
        bottom <- .05
        
        if (i == levels(outer.group)[1])
        {
            top <- .1
        }else
        {
            top <- 0
        }
        
        par(mai = c(bottom, side2, top, 0.05))
        cur.cx <- cx[,outer.group == i]
        image(utime, seq_len(sum(outer.group == i)), cur.cx, col = use.heat.colors, xlim = range(utime),
            xaxt = "n", yaxt = "n", ylab = "", xlab = "")
        #axis(2, at = seq_len(nc), cn)
        
        usrpar <- par("usr")
        par(usr = c(usrpar[1:2], 0, 1))
        
        cur.inner <- inner.group[outer.group == i, drop=T]
        
        group.bounds <- lapply(levels(cur.inner), function(x) end(IRanges(Rle(cur.inner == x))))
        names(group.bounds) <- unique(cur.inner)
        group.lines <- sort(as.numeric(unlist(group.bounds)))
        last.line <- group.lines[length(group.lines)]
        group.lines <- group.lines[-length(group.lines)]/length(cur.inner)
        
        if (length(group.lines) > 0)
        {
            abline(h = group.lines, lwd = 1, col = 1)
        
            for (j in names(group.bounds)){
                for (k in group.bounds[[j]]){
                    dist.val <- k/(length(cur.inner))
                    half.dist <- dist.val - min(abs(dist.val - group.lines[group.lines != dist.val]))/2
                    mtext(text=j, side=2, at=half.dist, las=1, cex=.5, line=.5, col=outer.cols[i])
                }
                
            }
            
            mtext(text=i, side=2, las=0, line=.5+(ceiling(max(nchar(cn))/2)), col=outer.cols[i])
        }
        
        colm <- colm.list[[i]]
        
        par(mai = c(bottom, .05, top, .1))
        
        if (i == levels(outer.group)[nlevels(outer.group)])
        {
            plot(1:length(colm), type = "n", ylab = "", yaxt = "n",
                xlab = "", xlim = right.xlim)
        }else
        {
            plot(1:length(colm), type = "n", ylab = "", yaxt = "n", xaxt = "n",
                xlab = "", xlim = right.xlim)
        }
       
        usrpar <- par("usr")
        par(usr = c(usrpar[1:2], 0, 1))
        
        ypos <- (1:length(colm) - 1/2)/length(colm)
        
        for (cur_el in 1:length(colm))
        {
            bxp(colm[[cur_el]], horizontal=T, at=ypos[cur_el],add=T, boxwex=.1, xaxt="n", yaxt="n")
            #temporary...
            #mtext(text=names(colm)[cur_el], side=2, at=ypos[cur_el], cex=.5)
        }
        
        abline(h = group.lines, lwd = 1, col = 1)
    }
    
    all.meds <- sapply(levels(outer.group), function(x)
                       {
                            apply(mean.mat[,outer.group == x], 1, median, na.rm = TRUE)
                       })
    
    bottom.ylim <- range(all.meds, na.rm = TRUE)
    
    par(mai = c(0.4, side2, 0.05, 0.05))
    plot(utime, all.meds[,1], type = "l", ylim = bottom.ylim, xaxt = "n", 
        xlab = "", ylab = "Median", col=outer.cols[colnames(all.meds)[1]])
    
    if (ncol(all.meds) > 1)
    {
        for (i in 2:ncol(all.meds))
        {
            lines(all.meds[,i], type="l", col=outer.cols[colnames(all.meds)[i]])
        }

    }
    
    par(usr = c(c(0,nrow(all.meds)) , par("usr")[3:4]))
    Axis(at=1:nrow(all.meds), labels=rownames(all.meds), side = 1)
    
    #as the above margins are too large, mainly as a placeholder...
    par(mai = c(0.05, 0.05, 0.1, 0.1))
    plot(0, 0, xlab = "", ylab = "", axes = FALSE, type = "n")
    #maybe make a legend here
    #text(0, 0, main)
    legend("center", legend=rev(c("Low", "", "", "High")), fill=rev(use.heat.colors), title=main,y.intersp = .75)
    
}

#functions to be used in conjunction with the summaryMeasures method
time.to.max.response <- function(x, day.name)
{
    return(data.frame(time.to.max.response=as.numeric(as.character(x[which.max(x$Value),day.name]))))
}

max.response <- function(x, day.name)
{
    return(data.frame(max.response=max(x$Value)))
}

auc.response <- function(x, day.name)
{
    x[,day.name] <- as.numeric(x[,day.name])
    ord.days <- x[order(x[,day.name], decreasing=FALSE),]
    return(data.frame(auc.response=(1/2)*sum(sapply(1:(nrow(ord.days)-1), function(y) (ord.days[,day.name][y+1]-ord.days[,day.name][y])*(ord.days$Value[y] + ord.days$Value[y+1])))))
}

mean.response <- function(x, day.name)
{
    return(data.frame(mean.response=mean(x$Value)))
}

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
    #should be a check to make sure all the phase annotations are valid here: but need to expose that as a method: else if (any(names(sample.labels) == "phase") && )
    
    if (any(is.na(sample.labels)))
    {
        stop("ERROR: There cannot be NAs in sample.labels")
    }
    
    if ((ncol(sample.labels) == 1) || (ncol(sample.labels) == 2 && any(names(sample.labels) == "phase")))
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
    
    label.cols <- setdiff(names(sample.labels), c("samples", "phase"))
    
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
    
    if (any(names(sample.labels) == "phase"))
    {
        basic.col <- c("Sample_Name", "Rec_Exp_date")
    }
    else
    {
        basic.col <- "Sample_Name"
    }
    
    dbGetQuery(bux.con, paste("CREATE TEMPORARY TABLE temp_table (", paste(paste(basic.col, "TEXT"), collapse=",") ,", ", all.labels,")"))
    
    ins.query <- paste("INSERT INTO temp_table VALUES (",paste(paste0(":", basic.col), collapse=",") ,", ",paste(paste0(":", label.cols), collapse=","),")")
    
    names(sample.labels)[names(sample.labels) == "samples"] <- "Sample_Name"
    names(sample.labels)[names(sample.labels) == "phase"] <- "Rec_Exp_date"
    
    dbBegin(bux.con)
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
    
    dbBegin(bux.con)
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
    if (class(bux.db) != "BuxcoDB")
    {
        stop("ERROR: bux.db needs to be a 'BuxcoDB class'")
    }
    
    if ((is.numeric(max.exp.time) && is.numeric(max.acc.time)) == F)
    {
        stop("max.exp.time and max.acc.time need to be numeric values")
    }
    
    if ((is.numeric(max.exp.count) && is.numeric(max.acc.count)) ==F)
    {
        stop("max.exp.count and max.acc.count need to be numeric")
    }
    
    db.con <- dbConnect(SQLite(), dbName(bux.db))
    
    time.dta <- dbGetQuery(db.con, paste("SELECT Break_type_label, MIN(Break_sec_start) AS min_seconds, MAX(Break_sec_start) AS max_seconds from", annoTable(bux.db), "NATURAL JOIN Chunk_Time GROUP BY Break_type_label;"))
    
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

