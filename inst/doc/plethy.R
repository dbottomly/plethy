### R code from vignette source 'plethy.Rnw'

###################################################
### code chunk number 1: plethy.Rnw:28-29
###################################################
stopifnot(require(plethy))


###################################################
### code chunk number 2: plethy.Rnw:32-38
###################################################
file.name <- buxco.sample.data.path()
chunk.size <- 500
db.name <-  file.path(tempdir(), "bux_test.db")

parse.buxco(file.name=file.name, chunk.size=chunk.size, db.name=db.name, verbose=FALSE)



###################################################
### code chunk number 3: plethy.Rnw:42-49
###################################################

bux.db <- makeBuxcoDB(db.name=db.name)

samples(bux.db)
variables(bux.db)
tables(bux.db)



###################################################
### code chunk number 4: plethy.Rnw:53-58
###################################################

data.1 <- retrieveData(bux.db, samples="8034x13140_1", variables="Penh")

head(data.1)



###################################################
### code chunk number 5: plethy.Rnw:62-69
###################################################

data.2 <- retrieveData(bux.db, samples=c("8034x13140_1", "8034x13140_10"), variables=c("Penh", "f"))

head(data.2)

table(data.1$Sample_Name, data.1$Variable_Name)



###################################################
### code chunk number 6: plethy.Rnw:73-77
###################################################

addAnnotation(bux.db, query=day.infer.query, index=FALSE)
addAnnotation(bux.db, query=break.type.query, index=TRUE)



###################################################
### code chunk number 7: plethy.Rnw:81-85
###################################################

annoCols(bux.db)
annoLevels(bux.db)



###################################################
### code chunk number 8: plethy.Rnw:89-93
###################################################
data.3 <- retrieveData(bux.db, samples="8034x13140_2", variables="Penh")

with(data.3, table(Days, Break_type_label))



###################################################
### code chunk number 9: plethy.Rnw:97-106
###################################################
data.4 <- retrieveData(bux.db, samples="8034x13140_2", variables="Penh", Days = 0)

with(data.4, table(Days, Break_type_label))

data.5 <- retrieveData(bux.db, samples="8034x13140_2", variables="Penh", Days = 0, 
	Break_type_label = 'EXP')

with(data.5, table(Days, Break_type_label))



###################################################
### code chunk number 10: plethy.Rnw:111-118
###################################################

exp.penh <- retrieveData(bux.db, variables="Penh", Break_type_label = 'EXP')

head(exp.penh)

boxplot(Value~Sample_Name, data=exp.penh)



###################################################
### code chunk number 11: plethy.Rnw:121-125
###################################################

plot(Value~Break_sec_start, data=exp.penh, subset=Sample_Name=="8034x13140_5", type="l", 
	xlab="Seconds past start")



