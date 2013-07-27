parse.buxco.basic <- function(file.name=NULL, table.delim="Table", burn.in.lines=c("Measurement", "Create measurement", "Waiting for",
                                                                                                         "Site Acknowledgement Changed"))
{
    bux.lines <- readLines(file.name)
    
    #split the lines into tables based on a line corresponding to: table.delim table name
    #The goal is to find all of the locations of the above line using grep and then create a vector indicating the table names for each entry or "" for the
    #actual table line itself.  Based on this annotation, the bux.lines character vector is split into corresponding pieces and the "" piece removed.
    table.points <- grep(table.delim, bux.lines)
    table.break.dta <- find.break.ranges.integer(table.points, length(bux.lines))
    table.break.dta$names <- sub(paste(table.delim, "\\s+", sep=""), "", bux.lines[table.points])
    
    table.break.split <- vector(mode="character", length=length(bux.lines)) 
    
    for (i in 1:nrow(table.break.dta))
    {
        table.break.split[table.break.dta$start[i]:table.break.dta$end[i]] <- table.break.dta$names[i]
    }
    
    split.table.range <- split(bux.lines, table.break.split)
    
    #remove the header lines
    split.table.range <- split.table.range[names(split.table.range) != ""]
    
    #Here we parse each table seperately and follow a similar paradigm as above where each line is checked for a set of values delimiting each section of
    #output from the machine as in burn.in.lines.  Again a vector tab.break.split is created showing the boundaries of each burn.in.lines set.  In this case
    #we make tab.break.split a ordered factor vector so that we can test whether a given Subject and Phase belongs to several sets and which set comes before
    #the other.  The data is split by Subject and Phase and the resulting data is assigned to ACC or EXP category if possible based on the values in tab.break.split.
    #also note the creation of a POSIXlt time object from the Time column.  This object enables times to be compared and basic operations performed on them.  Here
    #we check to make sure the ACC categories always precede the EXP categories and provide a new column (time.diff) consisting of the elapsed time in seconds relative to the
    #start of the ACC or EXP measurements.
    
    bux.sum.list <- lapply(1:length(split.table.range), function(x)
           {
                tab <- csv.to.table(split.table.range[[x]])
                
                burn.in.line.points <- multi.grep(tab$Subject, burn.in.lines)
                break.dta <- find.break.ranges.integer(burn.in.line.points, nrow(tab))
                break.dta$names <- paste("break", 1:nrow(break.dta), sep="_")
                
                tab.break.split <- vector(mode="character", length=nrow(tab)) 
    
                for (i in 1:nrow(break.dta))
                {
                    tab.break.split[break.dta$start[i]:break.dta$end[i]] <- break.dta$names[i]
                }
                
                tab$break.split <- tab.break.split
                tab <- tab[tab$break.split != "",]
                tab$posix.time <- as.character(strptime(tab$Time, format="%m/%d/%Y %I:%M:%S %p"))
                
                tab$break.split <- factor(tab$break.split, levels=break.dta$names, labels=break.dta$names, ordered=TRUE)
                
                split.tab <- split(tab, list(tab$Phase, tab$Subject))
                
                anim.list <- lapply(split.tab, function(y)
                       {
                            if (nrow(y) == 0)
                            {
                                ret.dta <- data.frame()
                            }
                            else
                            {
                                if (length(unique(y$break.split)) == 2)
                                {
                                    all.dta <- y
                                    all.dta$type <- ifelse(all.dta$break.split == min(all.dta$break.split), "ACC", "EXP")
                                    
                                    #conversion back from string to POSIXlt seems to be pretty much optional but will do it to be safe...
                                    acc.times <- as.POSIXlt(all.dta$posix.time[all.dta$type == "ACC"])
                                    exp.times <- as.POSIXlt(all.dta$posix.time[all.dta$type == "EXP"])
                                    
                                    if (max(acc.times) > min(exp.times))
                                    {
                                        stop(paste("ERROR: max ACC time is > min EXP time for", unique(y$Subject), "phase", unique(y$Phase)))
                                    }
                                    
                                    all.dta$time.diff <- as.numeric(ifelse(all.dta$type == "ACC", difftime(all.dta$posix.time, min(acc.times), units="secs"),
                                           difftime(all.dta$posix.time, min(exp.times), units="secs")))
                                }
                                else if (length(unique(y$break.split)) == 1)
                                {
                                    warning(paste("Only one break for:", unique(y$Subject), "phase", unique(y$Phase), "assuming EXP measurement only"))
                                    all.dta <- y
                                    all.dta$type <- "EXP"
                                    
                                    all.dta$time.diff <- as.numeric(difftime(as.POSIXlt(all.dta$posix.time), min(as.POSIXlt(all.dta$posix.time)), units="secs"))
                                }
                                else
                                {
                                    stop(paste("ERROR: Unexpected number of breaks for", unique(y$Subject), "phase", unique(y$Phase)))
                                }
                                
                                all.dta <- all.dta[,-which(colnames(all.dta) == "break.split")]
                                
                                ret.dta <- melt(all.dta, id.vars=c("Time", "posix.time", "Subject", "Phase", "Recording", "type", "time.diff"))
                            }
                            
                            return(ret.dta)
                       })
                
                anim.dta <- data.frame(do.call("rbind", anim.list))
                anim.dta$table.name <- names(split.table.range)[x]
                
                return(anim.dta)
           })
    
    bux.dta <- data.frame(do.call("rbind", bux.sum.list))
    
    return(bux.dta)
    
}