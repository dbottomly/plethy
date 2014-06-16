require(RSQLite)
require(reshape2)


fix.time <- plethy:::fix.time
csv.to.table <- plethy:::csv.to.table
correct.breaks <- plethy:::correct.breaks
db.insert.autoinc <- plethy:::db.insert.autoinc
find.break.ranges.integer <- plethy:::find.break.ranges.integer
get.simple.single.col.query <- plethy:::get.simple.single.col.query
get.tab.ids <- plethy:::get.tab.ids
is.true.character <- plethy:::is.true.character
multi.grep <- plethy:::multi.grep
make.break.dta <- plethy:::make.break.dta
write.sample.db <- plethy:::write.sample.db
add.breaks.to.tab <- plethy:::add.breaks.to.tab
examine.table.lines <- plethy:::examine.table.lines
sanity.check.time <- plethy:::sanity.check.time

test.summaryMeasures <- function()
{   
    samples=rep(c(NA, "sample_1", NA, "sample_2"), 4)
    count = rep(c(NA,150, NA,150), 4)
    measure_break = c(c(FALSE, FALSE, TRUE, FALSE), rep(c(TRUE,FALSE, TRUE, FALSE), 3))
    table_break = c(TRUE, rep(FALSE, length(samples)-1))
    phase = c(rep(1, 4), rep(2, 4), rep(3,4), rep(4, 4))
    
    test.dta <- data.frame(samples=samples, count=count, measure_break=measure_break, table_break=table_break, phase=phase, stringsAsFactors=FALSE)
    
    sim.bux.lines <- plethy:::generate.sample.buxco(test.dta)
    
    temp.file <- tempfile()
    temp.db.file <- tempfile()
    write(sim.bux.lines, file=temp.file)
    bux.db.1 <- parse.buxco(file.name=temp.file, db.name=temp.db.file, chunk.size=10000)
    addAnnotation(bux.db.1, query=day.infer.query, index=FALSE)
    addAnnotation(bux.db.1, query=break.type.query, index=TRUE)
    
    summary.type=c("time.to.max.response", "max.response", "auc.response", "mean.response")
    
    sums <- summaryMeasures(bux.db.1, summary.type=summary.type, sample.summary.func=function(x) data.frame(Value=mean(x$Value)), samples=NULL, variables=NULL, tables=NULL, Break_type_label="ERR", day.summary.column="Rec_Exp_date")
    
    checkTrue(length(intersect(colnames(sums),c("Variable_Name", "Sample_Name", summary.type))) == length(union(colnames(sums),c("Variable_Name", "Sample_Name", summary.type))))
    
    all.dta <- retrieveData(bux.db.1)
    
    day.mean.dta <- melt(with(all.dta, tapply(Value, list(Variable_Name, Sample_Name, Rec_Exp_date), mean)))
    names(day.mean.dta) <- c("Variable_Name", "Sample_Name", "Rec_Exp_date", "Value")
    
    sums <- sums[do.call("order", sums[,1:2]),]
    
    mean.response <- melt(with(day.mean.dta, tapply(Value, list(Variable_Name, Sample_Name), mean)))
    mean.response <- mean.response[do.call("order", mean.response[,1:2]),]
    checkEquals(mean.response$value, sums$mean.response)
    
    time.to.max.response <- melt(as.table(by(day.mean.dta, day.mean.dta[,c("Variable_Name", "Sample_Name")], function(x) x$Rec_Exp_date[x$Value == max(x$Value)])))
    time.to.max.response <- time.to.max.response[do.call("order", time.to.max.response[,1:2]),]
    checkEquals(time.to.max.response$value, sums$time.to.max.response)
    
    max.response <- melt(with(day.mean.dta, tapply(Value, list(Variable_Name, Sample_Name), max)))
    max.response <- max.response[do.call("order", max.response[,c(1:2)]),]
    checkEquals(max.response$value, sums$max.response)
    
    #here first check the private function, then use it to check the sanity of sums
    
    #from Matthews et al. 1990 BMJ
    
    test.dta <- data.frame(Time=c(0,5,10,15,20,30,40,60,75,90,120), Value=c(0,8.3,21.6,33.9,35.5,47.2,38.3,20.5,13.3,0,0))
    
    checkTrue(as.numeric(plethy:::auc.response(test.dta, "Time")) == 2190)#close to the 2191 mentioned in the paper, maybe rounding errors...
    
    auc.response <- melt(as.table(by(day.mean.dta, day.mean.dta[,c("Variable_Name", "Sample_Name")], function(x) as.numeric(plethy:::auc.response(x, "Rec_Exp_date")))))
    auc.response <- auc.response[do.call("order", auc.response[,1:2]),]
    checkEquals(auc.response$value, sums$auc.response)
    
    checkException(summaryMeasures(bux.db.1, Break_type_label="ERR", day.summary.column="Days_2"))
    checkException(summaryMeasures(bux.db.1, Break_type_label="EXP", day.summary.column="Rec_Exp_date"))
    
}

test.examine.table.lines <- function()
{
    #tests for empty tables at the begining
    
    if (file.exists("unit_test_extabl.db"))
    {
	file.remove("unit_test_extabl.db")
    }
	
    test.con <- dbConnect("SQLite", "unit_test_extabl.db")
    
    plethy:::create.tables(test.con)
    
    ret.list <- new("RetList", max.run.time.mins=60)
    
    bux.lines.test.1 <- readLines(buxco.sample.data.path(), n=158)
    bux.lines.test.1 <- append(c("Table Metabolism", "Time,Subject,Phase,Recording,O2ref,O2out,CO2ref,CO2out,Flow,MR,VO2,VCO2,RQ"), bux.lines.test.1)
    
    ret.list <- examine.table.lines(bux.lines=bux.lines.test.1, table.delim="Table", burn.in.lines=c("Measurement", "Create measurement", "Waiting for","Site Acknowledgement Changed"),
			ret.list=ret.list, db.con=test.con, verbose=FALSE)
    
    checkTrue(ret.list@cur.table == "WBPth")
    
    #only read in the experimental run and header, not the acclimation line
    exp.csv.tab <- plethy:::csv.to.table(bux.lines.test.1[c(4, 11:length(bux.lines.test.1))], header=TRUE)
    melt.exp.csv.tab <- melt(exp.csv.tab, id.vars=c("Time", "Subject", "Phase", "Recording"))
    
    ret.list@samp.tab <- ret.list@samp.tab[,-which(names(ret.list@samp.tab) %in% c("break.num", "tab.name"))]
    
    rownames(ret.list@samp.tab) <- NULL
    
    checkEquals(melt.exp.csv.tab, ret.list@samp.tab)
    
    checkTrue(all(ret.list@samp.tab$tab.name == "WBPth"))
    
    #did the acclimation sample get written to db?
    
    acc.query.count <- dbGetQuery(test.con, "SELECT COUNT(*) FROM Data NATURAL JOIN Timepoint NATURAL JOIN Chunk_Time NATURAL JOIN Sample NATURAL JOIN Variable NATURAL JOIN Bux_table")
    
    checkTrue(acc.query.count[,1] == 17)
    
    dbDisconnect(test.con)
    file.remove("unit_test_extabl.db")
    
    #tests for empty tables at the end, below simulates the final chunk being processed before completion
	
    test.con <- dbConnect("SQLite", "unit_test_extabl.db")
    
    plethy:::create.tables(test.con)
    
    ret.list <- new("RetList", max.run.time.mins=60)
    ret.list@cur.table <- "WBPth"
    ret.list@collect.header <- strsplit(readLines(buxco.sample.data.path(), n=2)[2], ",")[[1]]
    var.names <- ret.list@collect.header[5:length(ret.list@collect.header)]
    ret.list@cur.db.tabs@variable <- data.frame(Variable_ID=1:length(var.names), Variable_Name=var.names, stringsAsFactors=FALSE)
    
    bux.lines.test.2 <- readLines(buxco.sample.data.path(), n=158)
    bux.lines.test.2 <- append(bux.lines.test.2[-c(1:9)], c("Table Metabolism", "Time,Subject,Phase,Recording,O2ref,O2out,CO2ref,CO2out,Flow,MR,VO2,VCO2,RQ"))
    
    ret.list <- examine.table.lines(bux.lines=bux.lines.test.2, table.delim="Table", burn.in.lines=c("Measurement", "Create measurement", "Waiting for","Site Acknowledgement Changed"),
			ret.list=ret.list, db.con=test.con, verbose=FALSE)
    
    exp.csv.tab <- plethy:::csv.to.table(bux.lines.test.2[1:(length(bux.lines.test.2)-2)], header=FALSE)
    colnames(exp.csv.tab) <- strsplit(readLines(buxco.sample.data.path(), n=2)[2], ",")[[1]]
    melt.exp.csv.tab <- melt(exp.csv.tab, id.vars=c("Time", "Subject", "Phase", "Recording"))
    
    checkTrue(all(ret.list@samp.tab$tab.name == "WBPth"))
    
    ret.list@samp.tab <- ret.list@samp.tab[,-which(names(ret.list@samp.tab) %in% c("break.num", "tab.name"))]
    
    checkEquals(melt.exp.csv.tab, ret.list@samp.tab)
    checkTrue(ret.list@cur.table == "Metabolism")
    
    dbDisconnect(test.con)
    file.remove("unit_test_extabl.db")
}

test.dbImport <- function()
{
    #can simulate two such databases instead of relying on the hardcoded paths
    
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
    bux.db.1 <- parse.buxco(file.name=temp.file, db.name=temp.db.file, chunk.size=10000)
    addAnnotation(bux.db.1, query=day.infer.query, index=FALSE)
    addAnnotation(bux.db.1, query=break.type.query, index=TRUE)
    
    samples=c(NA, "sample_4", NA, "sample_4", "sample_5", NA, "sample_5")
    count = c(NA,900, NA,150, 900, NA, 150)
    measure_break = c(FALSE, FALSE, TRUE, FALSE, FALSE,TRUE, FALSE)
    table_break = c(TRUE, rep(FALSE, length(samples)-1))
    phase = rep("D1", length(samples))
    
    err.dta <- data.frame(samples=samples, count=count, measure_break=measure_break, table_break=table_break, phase=phase, stringsAsFactors=FALSE)
    
    sim.bux.lines <- plethy:::generate.sample.buxco(err.dta)
    
    temp.file <- tempfile()
    temp.db.file <- tempfile()
    write(sim.bux.lines, file=temp.file)
    bux.db.2 <- parse.buxco(file.name=temp.file, db.name=temp.db.file, chunk.size=10000)
    addAnnotation(bux.db.2, query=day.infer.query, index=FALSE)
    addAnnotation(bux.db.2, query=break.type.query, index=TRUE)
    
    #need to test the new retrieveData code
    bux.2.dta <- retrieveData(bux.db.2)
    
    #create a new database from the data.frame of an old one
    bux.db.3 <- dbImport(bux.db=NULL, bux.dta=bux.2.dta, db.name=tempfile(), debug=FALSE)
    
    bux.2.dta.test <- retrieveData(bux.db.3)
    checkTrue(compare.bux.dta(dta.1=bux.2.dta, dta.2=bux.2.dta.test))
    
    #next test whether new data can be successfully added to an existing database
    bux.db.4 <- dbImport(bux.db=bux.db.1, bux.dta=bux.2.dta, db.name=tempfile(), debug=FALSE)
    
    bux.2.dta.test <- retrieveData(bux.db.4, samples = unique(bux.2.dta$Sample_Name))
    checkTrue(compare.bux.dta(dta.1=bux.2.dta, dta.2=bux.2.dta.test))
    
    #then again with the first buxco dataset
    
    bux.1.dta <- retrieveData(bux.db.1)
    bux.1.dta.test <- retrieveData(bux.db.4, samples = unique(bux.1.dta$Sample_Name))
    checkTrue(compare.bux.dta(dta.1=bux.1.dta, dta.2=bux.1.dta.test))
}

compare.bux.dta <- function(dta.1, dta.2)
{
    comp.list <- list(dta.1, dta.2)
    
    res.list <- lapply(comp.list, function(x)
        {
            for(i in 1:ncol(x))
            {
                    x[,i] <- as.character(x[,i])
            }
        
            x <- x[with(x, order(Value, Sample_Name, Variable_Name, Bux_table_Name, Rec_Exp_date, P_Time, Break_sec_start, Break_number, Days, Break_type_label)),]
            
            rownames(x) <- 1:nrow(x)
            
            return(x)
        })
    
    return(isTRUE(all.equal(res.list[[1]], res.list[[2]])))
}

test.annoCols <- function()
{
	bux.db <- makeBuxcoDB(sample.db.path())
	
	#should give an empty vector as the annotation tables does not exist yet
	anno.col.fail <- annoCols(bux.db)
	
	checkTrue(length(anno.col.fail) == 0)
	
	local.samp.db <- file.path(getwd(), basename(sample.db.path()))
	checkTrue(file.copy(from=sample.db.path(), to=local.samp.db))
	
	bux.db <- makeBuxcoDB(local.samp.db)
	
	addAnnotation(bux.db, query=day.infer.query)
	addAnnotation(bux.db, query=break.type.query)
	
	#added this case to deal with bug occuring when a column as _ID was added
	
	addAnnotation(bux.db, query=function(x) paste("SELECT Break_Chunk_ID, 10 AS test_ID FROM", annoTable(x)))
	
	anno.col.succ <- annoCols(bux.db)
	
	checkIdentical(anno.col.succ, c("Days", "Break_type_label", "test_ID"))
	
	checkTrue(file.remove(local.samp.db))
}

test.annoLevels <- function()
{
	bux.db <- makeBuxcoDB(sample.db.path())
	
	#should give an empty vector as the annotation tables does not exist yet
	anno.lev.fail <- annoLevels(bux.db)
	
	checkTrue(length(anno.lev.fail) == 0)
	
	local.samp.db <- file.path(getwd(), basename(sample.db.path()))
	checkTrue(file.copy(from=sample.db.path(), to=local.samp.db))
	
	checkTrue(file.exists(local.samp.db))
	
	bux.db <- makeBuxcoDB(local.samp.db)
	
	addAnnotation(bux.db, query=day.infer.query)
	addAnnotation(bux.db, query=break.type.query)
	
	anno.lev.succ <- annoLevels(bux.db)
	
	checkIdentical(anno.lev.succ, list(Days=0, Break_type_label=c("ACC", "EXP")))
	
	checkTrue(file.remove(local.samp.db))
}

natural.join.tables <- function(db.con)
{
	all.tab <- data.frame()
	
	for (i in dbListTables(db.con))
	{
	    if (i != "sqlite_sequence")
	    {
		temp.tab <- dbReadTable(db.con, i, stringsAsFactors=FALSE)
		if (nrow(all.tab) == 0)
		{
			all.tab <- temp.tab
		}
		else
		{
			all.tab <- merge(all.tab, temp.tab)
		}
	    }
		
	}
	
	return(all.tab)
}

#Left off here, modifying unit tests for use with example data...
test.retrieveData <- function()
{
	local.samp.db <- file.path(getwd(), basename(sample.db.path()))
	checkTrue(file.copy(from=sample.db.path(), to=local.samp.db))
	
	checkTrue(file.exists(local.samp.db))
	
	bux.db <- makeBuxcoDB(local.samp.db)
	
	db.con <- dbConnect("SQLite", dbName(bux.db))
	
	all.tab <- natural.join.tables(db.con)
	
	all.dta <- retrieveData(bux.db)
	
	sub.all.tab <- all.tab[,colnames(all.dta)]
	
	all.dta <- all.dta[do.call("order", all.dta),]
	rownames(all.dta) <- NULL
	sub.all.tab <- sub.all.tab[do.call("order", sub.all.tab),]
	rownames(sub.all.tab) <- NULL
	
	checkEquals(all.dta,sub.all.tab)
	
	test.samp.1 <- samples(bux.db)[1]
	
	samp.dta.1 <- retrieveData(bux.db, samples=test.samp.1)
	samp.dta.1 <- samp.dta.1[do.call("order", samp.dta.1),]
	rownames(samp.dta.1) <- NULL
	
	sub.all.tab.samp.1 <- sub.all.tab[sub.all.tab$Sample_Name == test.samp.1,]
	sub.all.tab.samp.1 <- sub.all.tab.samp.1[do.call("order", sub.all.tab.samp.1),]
	rownames(sub.all.tab.samp.1) <- NULL
	
	checkEquals(sub.all.tab.samp.1, samp.dta.1)
	
	test.samp.2 <- samples(bux.db)[2]
	
	samp.dta.2 <- retrieveData(bux.db, samples=c(test.samp.1, test.samp.2))
	samp.dta.2 <- samp.dta.2[do.call("order", samp.dta.2),]
	rownames(samp.dta.2) <- NULL
	
	sub.all.tab.samp.2 <- sub.all.tab[sub.all.tab$Sample_Name %in% c(test.samp.1, test.samp.2),]
	sub.all.tab.samp.2 <- sub.all.tab.samp.2[do.call("order", sub.all.tab.samp.2),]
	rownames(sub.all.tab.samp.2) <- NULL
	
	checkEquals(samp.dta.2, sub.all.tab.samp.2)
	
	test.vars <- variables(bux.db)[1:2]
	
	samp.var.dta <- retrieveData(bux.db, samples=c(test.samp.1, test.samp.2), variables=test.vars)
	samp.var.dta <- samp.var.dta[do.call("order", samp.var.dta),]
	rownames(samp.var.dta) <- NULL
	
	sub.all.tab.samp.var <- sub.all.tab[sub.all.tab$Sample_Name %in% c(test.samp.1, test.samp.2) & sub.all.tab$Variable_Name %in% test.vars,]
	sub.all.tab.samp.var <- sub.all.tab.samp.var[do.call("order", sub.all.tab.samp.var),]
	rownames(sub.all.tab.samp.var) <- NULL
	
	checkEquals(samp.var.dta, sub.all.tab.samp.var)
	
	#now do the same after adding the default annotations
	
	addAnnotation(bux.db, query=day.infer.query)
	addAnnotation(bux.db, query=break.type.query)
	
	all.tab.annot <- natural.join.tables(db.con)
	
	all.dta.annot <- retrieveData(bux.db)
	
	sub.all.tab.annot <- all.tab.annot[,colnames(all.dta.annot)]
	
	all.dta.annot <- all.dta.annot[do.call("order", all.dta.annot),]
	rownames(all.dta.annot) <- NULL
	
	sub.all.tab.annot <- sub.all.tab.annot[do.call("order", sub.all.tab.annot),]
	rownames(sub.all.tab.annot) <- NULL
	
	checkEquals(all.dta.annot, sub.all.tab.annot)
	
	test.annot.lab <- annoLevels(bux.db)$Break_type_label[1]
	
	samp.var.annot.dta <- retrieveData(bux.db, samples=c(test.samp.1, test.samp.2), variables=test.vars, Break_type_label=test.annot.lab)
	samp.var.annot.dta <- samp.var.annot.dta[do.call("order", samp.var.annot.dta),]
	rownames(samp.var.annot.dta) <- NULL
	
	samp.var.annot.tab <- sub.all.tab.annot[with(sub.all.tab.annot, Sample_Name %in% c(test.samp.1, test.samp.2) & Variable_Name %in% test.vars & Break_type_label == test.annot.lab),]
	samp.var.annot.tab <- samp.var.annot.tab[do.call("order", samp.var.annot.tab),]
	rownames(samp.var.annot.tab) <- NULL
	
	checkEquals(samp.var.annot.dta, samp.var.annot.tab)
	
	dta.fail.1 <- retrieveData(bux.db, samples="cat")
	checkTrue(nrow(dta.fail.1) == 0)
	
	dbDisconnect(db.con)
	file.remove(local.samp.db)
}

test.addAnnotation <- function()
{
	local.samp.db <- file.path(getwd(), basename(sample.db.path()))
	
	checkTrue(file.copy(from=sample.db.path(), to=local.samp.db))

	bux.db <- makeBuxcoDB(local.samp.db)
	
	addAnnotation(bux.db, query=day.infer.query)
	
	db.con <- dbConnect("SQLite", dbName(bux.db))
	
	checkTrue(annoTable(bux.db) %in% dbListTables(db.con))
	
	labeled.days <- dbGetQuery(db.con, paste("select * from", annoTable(bux.db)))
	
	break.time <- dbGetQuery(db.con, "select * from Chunk_Time natural join Timepoint")

	min.time.samp <- tapply(break.time$P_Time, break.time$Sample_ID, min)
	
	break.time$min.time <- min.time.samp[as.character(break.time$Sample_ID)]

	break.time$inf.days <- as.numeric(round(difftime(break.time$P_Time, break.time$min.time, units="days")))

	break.time.merge <- merge(break.time, labeled.days, by="Break_Chunk_ID", sort=FALSE, incomparables=NULL)

	checkIdentical(break.time.merge$inf.days, break.time.merge$Days)
	
	#just a quick check here
	
	addAnnotation(bux.db, query=break.type.query)
	
	all.annots <- dbGetQuery(db.con, paste("select * from", annoTable(bux.db)))
	
	checkTrue(all(names(all.annots) == c("Break_Chunk_ID", "Days", "Break_type_label")))
	
	checkTrue(nrow(all.annots) == nrow(labeled.days))
	
	dbDisconnect(db.con)
	
	file.remove(local.samp.db)
	
	#make sure having both set to index=TRUE mode doesn't break
	
	checkTrue(file.copy(from=sample.db.path(), to=local.samp.db))
	
	bux.db <- makeBuxcoDB(local.samp.db)
	
	addAnnotation(bux.db, query=day.infer.query, index=TRUE)
	addAnnotation(bux.db, query=break.type.query, index=TRUE)
	
	db.con <- dbConnect("SQLite", dbName(bux.db))
	all.annots.ind <- dbGetQuery(db.con, paste("select * from", annoTable(bux.db)))
	
	checkTrue(all(names(all.annots.ind) == c("Break_Chunk_ID", "Days", "Break_type_label")))
	checkTrue(nrow(all.annots.ind) == nrow(labeled.days))
	
	dbDisconnect(db.con)
	
	file.remove(local.samp.db)
}

test.get.simple.single.col.query <- function()
{
	test.con <- dbConnect("SQLite", "test.db")
	
	dbWriteTable(test.con, "letts", data.frame(index=1:10, letts_Name=letters[1:10]), row.names=FALSE)

	dbDisconnect(test.con)

	col.res <- get.simple.single.col.query(db.name="test.db", var.name="letts", col.suffix="_Name")
	
	checkIdentical(col.res, letters[1:10])
	
	file.remove("test.db")
}

test.parse.buxco <- function()
{
	
	test.bux.file <- buxco.sample.data.path()
	
	bux.db <- parse.buxco(file.name=test.bux.file, db.name=tempfile(), verbose=FALSE)

	test <- retrieveData(bux.db)
	
	comp.test <- test[,c("Value", "Sample_Name", "Variable_Name", "Bux_table_Name", "Rec_Exp_date", "P_Time", "Break_sec_start")]
	
	old.test <- parse.buxco.basic(file.name=test.bux.file)
	
	comp.old.test <- old.test[,c("value", "Subject", "variable", "table.name", "Phase", "posix.time", "time.diff")]
	
	names(comp.old.test) <- c("Value", "Sample_Name", "Variable_Name", "Bux_table_Name", "Rec_Exp_date", "P_Time", "Break_sec_start")
	
	for(i in 1:ncol(comp.old.test))
	{
		comp.old.test[,i] <- as.character(comp.old.test[,i])
	}
	
	for(i in 1:ncol(comp.test))
	{
		comp.test[,i] <- as.character(comp.test[,i])
	}
	
	#changed this on 5-02-2013
	
	comp.old.test <- comp.old.test[do.call("order", comp.old.test),]
	
	comp.test <- comp.test[do.call("order", comp.test),]
	
	rownames(comp.old.test) <- 1:nrow(comp.old.test)
	rownames(comp.test) <- 1:nrow(comp.test)
	
	checkEquals(comp.old.test, comp.test)
}

test.fix.time <- function()
{
	bux.lines <- readLines(buxco.sample.data.path(), n=158)
	bux.lines <- bux.lines[-c(1, 3:9)]
	
	bux.tab <- csv.to.table(csv.vec=bux.lines, header=TRUE)
	
	exp.samp.tab <- data.frame(melt(bux.tab, id.vars=c("Time", "Subject", "Phase", "Recording")), break.num=1, stringsAsFactors=FALSE)
	
	exp.samp.tab <- fix.time(exp.samp.tab)
	
	checkTrue("posix.time" %in% names(exp.samp.tab))
	
	#just to make sure the result seems like a POSIXlt object
	checkTrue(all(class(as.POSIXlt(exp.samp.tab$posix.time)) == c("POSIXlt","POSIXt")))
}

test.add.breaks.to.tab <- function()
{
	bux.lines <- readLines(buxco.sample.data.path(), n=158)
	bux.lines <- bux.lines[-c(1, 3:9)]
	
	bux.tab <- csv.to.table(csv.vec=bux.lines, header=TRUE)
	
	burn.in.lines <- c("Measurement", "Create measurement", "Waiting for","Site Acknowledgement Changed")
	
	basic.result <- make.break.dta(tab=bux.tab, burn.in.lines=burn.in.lines)
	
	break.dta <- basic.result$break.dta
	break.dta$break.num <- 1
	
	ret.list <- plethy:::basicRetList()
	
	plethy:::variable(ret.list) <- data.frame(Variable_ID=1:(ncol(bux.tab)-5 +1), Variable_Name=colnames(bux.tab)[5:ncol(bux.tab)], stringsAsFactors=FALSE)
	
	#try a basic one
	
	exp.samp.tab <- data.frame(melt(bux.tab, id.vars=c("Time", "Subject", "Phase", "Recording")), break.num=1, stringsAsFactors=FALSE)
	exp.break.dta <- data.frame(break.dta[,c("break.num", "start", "end", "width")], Subject="8034x13140_5", stringsAsFactors=FALSE)
	
	new.ret.list <- add.breaks.to.tab(tab=bux.tab, break.dta=break.dta, ret.list=ret.list)
	
	checkTrue(class(new.ret.list) == "RetList")
	checkEquals(plethy:::breakData(new.ret.list), exp.break.dta)
	checkEquals(exp.samp.tab[,names(plethy:::sampTab(new.ret.list))], plethy:::sampTab(new.ret.list))
	
	#slightly more complicated one with two breaks
	
	break.dta <- data.frame(start=c(1, 75), end=c(74, 149), width=c(74, 75), break.num=c(2, 3))
	exp.break.dta <- data.frame(break.dta[,c("break.num", "start", "end", "width")], Subject="8034x13140_5", stringsAsFactors=FALSE)
	exp.samp.tab <- data.frame(melt(bux.tab, id.vars=c("Time", "Subject", "Phase", "Recording")), break.num=c(rep(2, 74), rep(3, 75)), stringsAsFactors=FALSE)

	new.ret.list <- add.breaks.to.tab(tab=bux.tab, break.dta=break.dta, ret.list=ret.list)
	
	checkTrue(class(new.ret.list) == "RetList")
	checkEquals(plethy:::breakData(new.ret.list), exp.break.dta)
	checkEquals(exp.samp.tab[,names(plethy:::sampTab(new.ret.list))], plethy:::sampTab(new.ret.list))
}

test.correct.breaks <- function()
{
	tab.name <- "WBPth"
	
	burn.in.lines <- c("Measurement", "Create measurement", "Waiting for","Site Acknowledgement Changed")
	
	ret.list <- plethy:::basicRetList()
	
	#no break first chunk
	
	exp.dta <- data.frame(start=1, end=99, width=99, break.num=1)
	break.dta <- data.frame(start=1, end=99, width=99)
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)
	
	#break at start, first chunk
	
	exp.dta <- data.frame(start=4, end=99, width=96, break.num=2)
	break.dta <- data.frame(start=4, end=99, width=96)
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)

	#break in middle first chunk

	exp.dta <- data.frame(start=c(1, 41), end=c(40, 99), width=c(40, 59), break.num=c(1, 2))
	break.dta <- data.frame(start=c(1, 41), end=c(40, 99), width=c(40, 59))
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)
	
	#break at end first chunk

	plethy:::breakAtEnd(ret.list) <- TRUE
	
	exp.dta <- data.frame(start=1, end=99, width=99, break.num=2)
	break.dta <- data.frame(start=1, end=99, width=99)
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)

	#no break end not first chunk

	plethy:::breakAtEnd(ret.list) <- FALSE
	plethy:::breakData(ret.list) <- data.frame(start=1, end=95, width=95, break.num=2)
	
	exp.dta <- data.frame(start=1, end=99, width=99, break.num=2)
	break.dta <- data.frame(start=1, end=99, width=99)
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)

	#break at start not first chunk

	exp.dta <- data.frame(start=4, end=99, width=96, break.num=3)
	break.dta <- data.frame(start=4, end=99, width=96)
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)

	#break in middle not first chunk

	exp.dta <- data.frame(start=c(1, 41), end=c(40, 99), width=c(40, 59), break.num=c(2, 3))
	break.dta <- data.frame(start=c(1, 41), end=c(40, 99), width=c(40, 59))
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)

	#break at end not first chunk

	plethy:::breakAtEnd(ret.list) <- TRUE
	
	exp.dta <- data.frame(start=1, end=99, width=99, break.num=3)
	break.dta <- data.frame(start=1, end=99, width=99)
	check.dta <- correct.breaks(break.dta=break.dta, ret.list=ret.list, burn.in.lines=burn.in.lines, tab.name=tab.name)
	checkEquals(check.dta, exp.dta)
	
}

test.make.break.dta <- function()
{
	bux.lines <- readLines(buxco.sample.data.path(), n=158)
	bux.lines <- bux.lines[-c(1, 3:9)]
	
	bux.tab <- csv.to.table(csv.vec=bux.lines, header=TRUE)
	
	burn.in.lines <- c("Measurement", "Create measurement", "Waiting for","Site Acknowledgement Changed")
	
	basic.result <- make.break.dta(tab=bux.tab, burn.in.lines=burn.in.lines)
	
	checkTrue(is.list(basic.result) && names(basic.result) == c("break.dta", "break.at.end"))
	checkTrue(basic.result$break.at.end == FALSE)
	checkEquals(basic.result$break.dta, data.frame(start=1, end=149, width=149, stringsAsFactors=FALSE))
	
	bux.tab$Subject[(nrow(bux.tab)-3):nrow(bux.tab)] <- burn.in.lines
	
	bae.result <- make.break.dta(tab=bux.tab, burn.in.lines=burn.in.lines)
	
	checkTrue(is.list(bae.result) && names(bae.result) == c("break.dta", "break.at.end"))
	checkTrue(bae.result$break.at.end == TRUE)
	checkEquals(bae.result$break.dta, data.frame(start=1, end=145, width=145, stringsAsFactors=FALSE))
	
	bux.tab$Subject[50:53] <- burn.in.lines
	
	bim.ae.result <- make.break.dta(tab=bux.tab, burn.in.lines=burn.in.lines)
	
	checkTrue(is.list(bim.ae.result) && names(bim.ae.result) == c("break.dta", "break.at.end"))
	checkTrue(bim.ae.result$break.at.end == TRUE)
	checkEquals(bim.ae.result$break.dta, data.frame(start=c(1,54), end=c(49,145), width=c(49,92), stringsAsFactors=FALSE))
	
	bux.tab <- bux.tab[-c(52:53, 146:148),]
	
	bim.ae.single.result <- make.break.dta(tab=bux.tab, burn.in.lines=burn.in.lines)
	
	checkTrue(is.list(bim.ae.single.result) && names(bim.ae.single.result) == c("break.dta", "break.at.end"))
	checkTrue(bim.ae.single.result$break.at.end == TRUE)
	checkEquals(bim.ae.single.result$break.dta, data.frame(start=c(1,52), end=c(49,143), width=c(49,92), stringsAsFactors=FALSE))
	
	bux.tab <- bux.tab[-nrow(bux.tab),]
	
	bim.result <- make.break.dta(tab=bux.tab, burn.in.lines=burn.in.lines)
	
	checkTrue(is.list(bim.result) && names(bim.result) == c("break.dta", "break.at.end"))
	checkTrue(bim.result$break.at.end == FALSE)
	checkEquals(bim.result$break.dta, data.frame(start=c(1,52), end=c(49,143), width=c(49,92), stringsAsFactors=FALSE))

}

#left off here 5-2-2013
test.write.sample.db <- function()
{
	if (file.exists("unit_test_writedb.db"))
	{
		file.remove("unit_test_writedb.db")
	}
	
	test.con <- dbConnect("SQLite", "unit_test_writedb.db")
	
	sub.tab <- "CREATE TABLE IF NOT EXISTS Sample (Sample_ID INTEGER CONSTRAINT Samp_pk PRIMARY KEY AUTOINCREMENT,
                    Sample_Name TEXT CONSTRAINT Samp_name UNIQUE)"
	time.tab <- "CREATE TABLE IF NOT EXISTS Timepoint (Time_ID INTEGER CONSTRAINT Time_pk PRIMARY KEY AUTOINCREMENT,
                    P_Time TEXT CONSTRAINT Time_name UNIQUE)"
	data.tab <- "CREATE TABLE IF NOT EXISTS Data (Data_ID INTEGER CONSTRAINT Data_pk PRIMARY KEY AUTOINCREMENT,
                    Time_ID INTEGER, Variable_ID INTEGER, Sample_ID INTEGER, Bux_table_ID INTEGER, Value DOUBLE)"
	break.tab <- "CREATE TABLE IF NOT EXISTS Chunk_Time (Break_Chunk_ID INTEGER CONSTRAINT Break_Chunk_pk PRIMARY KEY AUTOINCREMENT, Sample_ID INTEGER,
                    Time_ID INTEGER, Bux_table_ID INTEGER, Variable_ID INTEGER, Break_number INTEGER, Break_sec_start INTEGER, Rec_Exp_date TEXT)"
	
	for(i in list(sub.tab, time.tab, data.tab, break.tab))
	{
		checkTrue(is.null(dbGetQuery(test.con, i)))
	}
	
	subj.1 <- data.frame(Subject="907_L_f", Time=c("9/24/2012 3:48:21 PM", "9/24/2012 3:48:23 PM", "9/24/2012 3:48:25 PM", "9/24/2012 3:48:27 PM"),
			     tab.name="WBPth", variable=c("var_1", "var_2", "var_3", "var_4"), value=c(13.5, 19, 97, 2), Phase=c("Day1", "Day1", "Day1", "Day1"), break.num=rep(1, 4), stringsAsFactors=FALSE)
	
	subj.2 <- data.frame(Subject="907_R_f", Time=c("9/25/2012 5:18:16 PM", "9/25/2012 5:18:18 PM", "9/25/2012 5:18:20 PM", "9/25/2012 5:18:22 PM"),
			     tab.name="WBPth", variable=c("var_1", "var_2", "var_3", "var_4"), value=c(20, 99, 348, 50), Phase=c("Day1", "Day1", "Day1", "Day1"), break.num=rep(1,4), stringsAsFactors=FALSE)
	
	test.dta.tab <- rbind(subj.1, subj.2)
	
	tab.db <- data.frame(Bux_table_ID=1:2, Bux_table_Name=c("WBPth", "Metabolism"), stringsAsFactors=FALSE)
	var.db <- data.frame(Variable_ID=1:5, Variable_Name=paste("var", 1:5, sep="_"), stringsAsFactors=FALSE)
	
	#need to make me a RetList object instead...

	ret.list <- plethy:::basicRetList()
	
	plethy:::variable(ret.list) <- var.db
	plethy:::buxTable(ret.list) <- tab.db
	
	write.sample.db(db.con=test.con, dta.tab=test.dta.tab, ret.list=ret.list, verbose=FALSE)
	
	#now make sure the results make sense with respect to the inputs

	checkTrue(all(c("Chunk_Time", "Data", "Sample","Timepoint") %in% dbListTables(test.con)))
	
	samp <- dbReadTable(test.con, "Sample")
	checkTrue(all(samp$Sample_Name %in% test.dta.tab$Subject) && nrow(samp) == length(unique(test.dta.tab$Subject)))
	
	test.dta.tab$posix.time <- as.character(strptime(test.dta.tab$Time, format="%m/%d/%Y %I:%M:%S %p"))
	
	timep <- dbReadTable(test.con, "Timepoint")
	checkTrue(all(timep$P_Time %in% test.dta.tab$posix.time) && nrow(timep) == length(unique(test.dta.tab$posix.time)))
	
	chunk.time <- dbReadTable(test.con, "Chunk_Time")
	chunk.recon <- get.tab.ids(chunk.time, list(list(merge.name="Sample_ID", merge.dta=samp), list(merge.name="Time_ID", merge.dta=timep),
					list(merge.name="Bux_table_ID", merge.dta=tab.db), list(merge.name="Variable_ID", merge.dta=var.db)),
                                           merge.dta.column.pat=c("Sample_ID", "Time_ID", "Bux_table_ID","Variable_ID"))
	
	test.dta.tab$Break_sec_start <- ifelse(test.dta.tab$Subject == "907_L_f", as.integer(difftime(time1=as.POSIXlt(test.dta.tab$posix.time),
												      time2=min(as.POSIXlt(test.dta.tab$posix.time[test.dta.tab$Subject=="907_L_f"])),
												      units="secs")),
										as.integer(difftime(time1=as.POSIXlt(test.dta.tab$posix.time),
												      time2=min(as.POSIXlt(test.dta.tab$posix.time[test.dta.tab$Subject=="907_R_f"])),
												      units="secs")))
	sub.chunk.recon <- chunk.recon[,c("Sample_Name", "P_Time", "Bux_table_Name", "Variable_Name", "Rec_Exp_date", "Break_number", "Break_sec_start")]
	
	sub.test.dta.tab <- test.dta.tab[,c("Subject", "posix.time", "tab.name", "variable", "Phase", "break.num", "Break_sec_start")]
	
	names(sub.test.dta.tab) <- c("Sample_Name", "P_Time", "Bux_table_Name", "Variable_Name", "Rec_Exp_date", "Break_number", "Break_sec_start")
	
	sub.test.dta.tab$P_Time <- as.POSIXlt(sub.test.dta.tab$P_Time)
	sub.chunk.recon$P_Time <- as.POSIXlt(sub.chunk.recon$P_Time)
	
	sub.test.dta.tab <- sub.test.dta.tab[order(sub.test.dta.tab$Break_sec_start),]
	
	checkEquals(sub.chunk.recon, sub.test.dta.tab, check.attributes=FALSE)
	
	dta <- dbReadTable(test.con, "Data")
	
	dta.recon <- get.tab.ids(dta, list(list(merge.name="Sample_ID", merge.dta=samp), list(merge.name="Time_ID", merge.dta=timep),
					list(merge.name="Bux_table_ID", merge.dta=tab.db), list(merge.name="Variable_ID", merge.dta=var.db)),
                                           merge.dta.column.pat=c("Sample_ID", "Time_ID", "Bux_table_ID","Variable_ID"))
	
	dta.recon <- dta.recon[,c("Value", "Sample_Name", "P_Time", "Bux_table_Name", "Variable_Name")]
	
	sub.test.dta.tab <- test.dta.tab[,c("value", "Subject","posix.time", "tab.name", "variable")]
	names(sub.test.dta.tab) <- c("Value", "Sample_Name", "P_Time", "Bux_table_Name", "Variable_Name")
	
	dta.recon <- dta.recon[order(dta.recon$Value),]
	sub.test.dta.tab <- sub.test.dta.tab[order(sub.test.dta.tab$Value),]
	
	checkEquals(dta.recon, sub.test.dta.tab,check.attributes=FALSE)
	
	dbDisconnect(test.con)
	file.remove("unit_test_writedb.db")
}

test.get.tab.ids <- function()
{
	test.samps <- c("907_L_f", "907_B_f", "907_N_f", "907_R_f", "906_R_m", "906_L_m", "908_N_s", "908_R_s", "908_L_s", "908_B_s", "906_N_m")
	test.dta <- data.frame(samp.ind=1:length(test.samps), samp.name=test.samps, stringsAsFactors=FALSE)
	
	test.dta.1 <- data.frame(samp.val=test.samps, rand.col.1=paste("temp", 1:length(test.samps), sep="_"), stringsAsFactors=FALSE)
	test.dta.2 <- data.frame(samp.val=test.samps, rand.col.2=paste("temp2", 1:length(test.samps), sep="_"), stringsAsFactors=FALSE)

	test.run <- get.tab.ids(use.dta=test.dta, merge.list=list(list(merge.name="samp.name", merge.dta=test.dta.1),
								  list(merge.name="samp.name", merge.dta=test.dta.2)),
				merge.dta.column.pat="samp.val")

	checkTrue(is.data.frame(test.run) && nrow(test.run) == nrow(test.dta))
	checkTrue(all(names(test.run) %in% c("samp.name", "samp.ind", "rand.col.1", "rand.col.2")))
	checkTrue(all(test.run$samp.name == test.dta$samp.name & test.run$samp.ind == test.dta$samp.ind & test.run$rand.col.1 == test.dta.1$rand.col.1 &
		      test.run$rand.col.2 == test.dta.1$rand.col.2))
	
	checkException(get.tab.ids(use.dta=test.dta, merge.list=list(list(merge.name="samp.narm", merge.dta=test.dta.1),
								  list(merge.name="samp.name", merge.dta=test.dta.2)),
				merge.dta.column.pat="samp.val"))
	
	checkException(get.tab.ids(use.dta=test.dta, merge.list=list(list(merge="samp.name", merge.dta=test.dta.1),
								  list(merge.name="samp.name", merge.dta=test.dta.2)),
				merge.dta.column.pat="samp.val"))
}

test.db.insert.autoincrement <- function()
{
	#make a test database
	
	if (file.exists("unit_test_auto_inc.db"))
	{
		file.remove("unit_test_auto_inc.db")
	}
	
	test.con <- dbConnect("SQLite", "unit_test_auto_inc.db")
	
	sub.tab <- "CREATE TABLE IF NOT EXISTS Sample (Sample_ID INTEGER CONSTRAINT Samp_pk PRIMARY KEY AUTOINCREMENT,
                    Sample_Name TEXT CONSTRAINT Samp_name UNIQUE)"
	
	stopifnot(is.null(dbGetQuery(test.con, sub.tab)))
	
	test.samps <- c("907_L_f", "907_B_f", "907_N_f", "907_R_f", "906_R_m", "906_L_m", "908_N_s", "908_R_s", "908_L_s", "908_B_s", "906_N_m")

	ret.dta <- db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name", values=test.samps, return.query.type="reverse",debug=FALSE)

	checkTrue(all(ret.dta$Sample_Name == test.samps))

	#here also test return.query.type = "all"
	test.samps.plus <- c(test.samps, paste("samp", 1:1000, sep=""))

	ret.dta <- db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name", values=test.samps.plus, return.query.type="all",debug=FALSE)
	
	checkTrue(all(ret.dta$Sample_Name == test.samps.plus))
	
	#Note that the original data was not duplicated here

	diff.test.samps <- paste("samp", 3000:3050, sep="")
	
	ret.dta <- db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name", values=diff.test.samps, return.query.type="reverse",debug=FALSE)

	checkTrue(all(ret.dta$Sample_Name == diff.test.samps))

	diff.test.samps.2 <- paste("samp", 5000:5050, sep="")

	ret.dta <- db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name", values=diff.test.samps.2, return.query.type="all",debug=FALSE)

	checkTrue(all(ret.dta$Sample_Name == c(test.samps.plus, diff.test.samps,diff.test.samps.2)))
	checkTrue(all(ret.dta$SampleID == 1:nrow(ret.dta)))
	
	ret.dta <- db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name", values=diff.test.samps.2, return.query.type="none", debug=FALSE)
	
	checkTrue(nrow(ret.dta) == 0)
	
	checkException(db.insert.autoinc(db.con=test.con, table.name="Sample4", col.name="Sample_Name", values=diff.test.samps.2,
						    return.query.type="none", debug=FALSE))
	
	#this was removed as db.insert.autoinc no longer directly checks columns, probably an oversight and will fix in the future...
	#checkException(db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name4", values=diff.test.samps.2,
	#					    return.query.type="none",debug=FALSE))
	
	checkException(db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name", values=character(),
						    return.query.type="none",debug=FALSE))
	
	checkException(db.insert.autoinc(db.con=test.con, table.name=character(), col.name="Sample_Name", values=diff.test.samps.2,
						    return.query.type="none",debug=FALSE))
	
	checkException(db.insert.autoinc(db.con=test.con, table.name=character(), col.name=character(), values=diff.test.samps.2,
						    return.query.type="none",debug=FALSE))
	
	checkException(db.insert.autoinc(db.con=test.con, table.name="Sample", col.name="Sample_Name", values=diff.test.samps.2,
						    return.query.type="kitten",debug=FALSE))
	
	dbDisconnect(test.con)
	file.remove("unit_test_auto_inc.db")
}

test.multi.grep <- function()
{
	checkTrue(all(multi.grep(str.vec=c("test", "cheetah", "horse"), patterns=c("hor", "test")) == c(3,1)))
	checkException(multi.grep(str.vec=1:10, patterns=c("hor", "test")))
	checkException(multi.grep(str.vec=c("test", "cheetah", "horse"), patterns=1:10))
	checkException(multi.grep(str.vec=1:10,patterns=1:10))
}

test.find.break.ranges.integer <- function()
{
	basic.res <- find.break.ranges.integer(as.integer(c(1,5,7)), 10L)
	checkTrue(is.data.frame(basic.res) && nrow(basic.res) == 3)
	checkTrue(all(names(basic.res) == c("start", "end", "width")))
	checkTrue(nrow(find.break.ranges.integer(1L, 10L)) == 1)
	checkException(find.break.ranges.integer(c(-1L, 4L, 5L), 10L))
	checkException(find.break.ranges.integer(0L, 10L))
	checkException(find.break.ranges.integer(10L, 0L))
}

test.csv.to.table <- function()
{
	checkException(csv.to.table("rat,dog,pig", header=TRUE))
	checkException(csv.to.table(character(), header=TRUE))
	checkException(csv.to.table(character(), header=FALSE))

	dta <- csv.to.table(c("rat,dog,pig", "dan,cat,man"), header=TRUE)
	checkTrue(is.data.frame(dta) && nrow(dta) == 1 && colnames(dta) == c("rat", "dog", "pig"))
	dta.2 <- csv.to.table("rat,dog,pig", header=FALSE)
	checkTrue(is.data.frame(dta.2) && nrow(dta.2) == 1 && colnames(dta.2) == paste("V", 1:3, sep=""))

	for (i in c(TRUE, FALSE))
	{
		checkException(csv.to.table(c("rat,dog", "dan,cat,man"), header=i), msg=i)
        	checkException(csv.to.table(c("rat,dog,pig", "cat,man"), header=i), msg=i)
	}
}

test.is.true.character <- function()
{
	checkTrue(is.true.character("dan"))
	checkTrue(is.true.character(c("dan", "cat")))
	checkTrue(is.true.character("10")==FALSE)
	checkTrue(is.true.character("10.476")==FALSE)
	checkException(is.true.character(character()))
	checkException(is.true.character(factor(10,15)))
	checkException(is.true.character(c("dan",15)))
}

test.sanity.check.time <- function()
{
    sec.vec.1 <- seq(1, 300, by=2)
    
    test.sec.vec.1 <- sanity.check.time(sec.vec.1, 3600, "sample1", 10)
    
    checkIdentical(sec.vec.1, test.sec.vec.1)
    
    sec.vec.2 <- c(seq(1, 300, by=2), seq(3601, 3900, by=2))
    
    test.sec.vec.2 <- suppressWarnings(sanity.check.time(sec.vec.2, 3600, "sample1", 10))
    
    checkIdentical(c(sec.vec.1, seq(0, 298, by=2)), test.sec.vec.2)
}

##tests for the utility functions, first make some sample data

#test both get.err.breaks and test.adjust.labels at the same time...
test.get.err.breaks <- function()
{
    samples=c(NA, "sample_1", NA, "sample_1", "sample_2", NA, "sample_3", "sample_2", NA, "sample_3", "sample_4",NA, "sample_4", "sample_2", NA, "sample_2", "sample_5")
    count = c(NA,900, NA,150, 1, NA, 900, 28,NA, 150, 900, NA, 150, 900, NA, 150, 900)
    measure_break = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,FALSE, FALSE,TRUE,FALSE, FALSE, TRUE,FALSE, FALSE, TRUE, FALSE, FALSE)
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
    
    test.err.breaks <- get.err.breaks(test.bux.db, max.exp.count=150, max.acc.count=900, vary.perc=.1, label.val="ERR")
    
    checkTrue(all(test.err.breaks$Sample_Name == "sample_2"))
    checkTrue(all(test.err.breaks$Break_number[test.err.breaks$inferred_labs == "ERR"] %in% c(2,3)))
    
    checkTrue(all(test.err.breaks$Break_number[test.err.breaks$inferred_labs == "ACC"] == 5))
    checkTrue(all(test.err.breaks$Break_number[test.err.breaks$inferred_labs == "EXP"] == 6))
    
    checkTrue(all(table(test.err.breaks$Variable_Name) == sum(is.na(err.dta$samples) == FALSE & err.dta$samples == "sample_2")))
    
    orig.test <- retrieveData(test.bux.db)
    
    test.err.breaks <- get.err.breaks(test.bux.db, max.exp.count=150, max.acc.count=900, vary.perc=.1, label.val="ERR")
    
    adjust.labels(test.bux.db, test.err.breaks)
    
    test <- retrieveData(test.bux.db)
    
    checkTrue(all(test$Break_type_label_orig[test$Sample_Name == "sample_2"] == "ERR"))
    checkTrue(all(test$Break_number[test$Break_type_label == "ERR"] %in% c(2,3)))
    checkTrue(all(test$Break_number[test$inferred_labs == "ACC"] == 5))
    checkTrue(all(test$Break_number[test$inferred_labs == "EXP"] == 6))
    
    sub.test <- test[,-which(names(test) == "Break_type_label")]
    names(sub.test)[names(sub.test) == "Break_type_label_orig"] <- "Break_type_label"
    checkEquals(orig.test, sub.test)
    
    checkException(get.err.breaks(test.bux.db, max.exp.count=150, max.acc.count=900, vary.perc=.1, label.val="HELLO"))
    checkException(get.err.breaks(test.bux.db, max.exp.count=150, max.acc.count=900, vary.perc=.1, label.val="ACC"))
}

test.add.labels.by.sample <- function()
{   
    samp.file <- sample.db.path()
    new.file <- file.path(tempdir(), basename(samp.file))

    stopifnot(file.copy(samp.file, new.file, overwrite=TRUE))

    test.bux.db <- makeBuxcoDB(new.file)

    addAnnotation(test.bux.db, query=day.infer.query, index=FALSE)
    addAnnotation(test.bux.db, query=break.type.query, index=TRUE)
    
    test.orig <- retrieveData(test.bux.db)
    
    sample.labels <- data.frame(samples=c("8034x13140_1", "8034x13140_10", "8034x13140_4", "8034x13140_11"), inf_status=c("sars", "sars", "flu", "flu"), stringsAsFactors=FALSE)
    
    add.labels.by.sample(test.bux.db, sample.labels)

    test <- retrieveData(test.bux.db)
    
    for (i in 1:nrow(sample.labels))
    {
        checkTrue(sum(test$Sample_Name == sample.labels$samples[i] & test$inf_status == sample.labels$inf_status[i]) == sum(test$Sample_Name == sample.labels$samples[i]))
    }
    
    #check that all of the labels for sample_4 are NAs
    
    checkTrue(all(is.na(test$inf_status[test$Sample_Name %in% setdiff(samples(test.bux.db), sample.labels$samples)])))
    
    #Also double check against test.orig above
   
    test.orig <- test.orig[do.call("order", test.orig),]
    rownames(test.orig) <- NULL
    new.test <- test[,-which(names(test) == "inf_status")]
    new.test <- new.test[do.call("order", new.test),]
    rownames(new.test) <- NULL
   
    checkEquals(test.orig, new.test)
    
    #also check that you can add info by phase as well...
    
    samples=c(NA, "sample_1", NA, "sample_1", "sample_2", NA, "sample_3", "sample_2", NA, "sample_3", "sample_1",NA, "sample_1", "sample_2", NA, "sample_2", "sample_3", NA, "sample_3")
    count = c(NA,900, NA,150, 900, NA, 900, 150,NA, 150, 900,NA, 150, 900, NA, 150, 900, NA, 150)
    measure_break = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,FALSE, FALSE,TRUE,FALSE, FALSE, TRUE,FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
    table_break = c(TRUE, rep(FALSE, length(samples)-1))
    phase = c(rep("D1", 10), rep("D2", 9))
   
    err.dta <- data.frame(samples=samples, count=count, measure_break=measure_break, table_break=table_break, phase=phase, stringsAsFactors=FALSE)
    
    sim.bux.lines <- plethy:::generate.sample.buxco(err.dta)
    
    temp.file <- tempfile()
    temp.db.file <- tempfile()
    write(sim.bux.lines, file=temp.file)
    test.bux.db <- parse.buxco(file.name=temp.file, db.name=temp.db.file, chunk.size=10000)
    addAnnotation(test.bux.db, query=day.infer.query, index=FALSE)
    addAnnotation(test.bux.db, query=break.type.query, index=FALSE)
    
    sample.labels <- data.frame(samples=c("sample_1", "sample_3", "sample_1"), phase=c("D1", "D1", "D2"), important_col=1:3, stringsAsFactors=FALSE)
    
    add.labels.by.sample(test.bux.db, sample.labels)
    
    test <- retrieveData(test.bux.db)
    
    checkTrue(all(test$important_col[test$Rec_Exp_date == "D1" & test$Sample_Name == "sample_1"] == 1))
    checkTrue(all(test$important_col[test$Rec_Exp_date == "D2" & test$Sample_Name == "sample_1"] == 3))
    checkTrue(all(test$important_col[test$Rec_Exp_date == "D1" & test$Sample_Name == "sample_3"] == 2))
    
    num.nas <- nrow(test) - sum((test$Rec_Exp_date== "D1" & test$Sample_Name == "sample_1") | (test$Rec_Exp_date == "D2" & test$Sample_Name == "sample_1") | (test$Rec_Exp_date == "D1" & test$Sample_Name == "sample_3"))
    checkTrue(sum(is.na(test$important_col)) == num.nas)
    
    diff.cols <- setdiff(colnames(test), colnames(new.test)) 
    
    checkTrue(length(diff.cols) == 1 && diff.cols == "important_col")
}
