# AFGR June 3 2016
# I am going to prepare a csv with the information found below 
#     (N)  (%Female) (mean(sd)-Age)
# Go1
# Go2
# MGI

# The first thing I need to do is prepare the Go1 Row
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
go1.n <- nrow(mergedQAP)
go1.female <- length(which(mergedQAP$sex==2))/go1.n
go1.mean.age <- mean(mergedQAP$ageAtGo1Scan/12)
go1.sd.age <- sd(mergedQAP$ageAtGo1Scan/12)
go1.output <- cbind(c('PNC'), go1.n, go1.female, go1.mean.age, go1.sd.age)

# Now do the validation data sets
# start with MGI
source('/home/adrose/qapQA/scripts/loadMgiData.R')
mergedQAP <- rbind(mergedQAP.pitt, mergedQAP.penn)
mgi.n <- nrow(mergedQAP)
mgi.female <- length(which(mergedQAP$Gender==2))/mgi.n
mgi.mean.age <- mean(mergedQAP$age)
mgi.sd.age <- sd(mergedQAP$age)
mgi.output <- cbind(c('MGI'), mgi.n, mgi.female, mgi.mean.age, mgi.sd.age)

# Now do GO2
mergedQAP <- mergedQAP.go2
manualQAData <- read.csv("/home/analysis/redcap_data/201507/n1601_go1_datarel_073015.csv")
mergedQAP <- merge(mergedQAP, manualQAData, by='bblid')
go2.n <- nrow(mergedQAP)
go2.female <- length(which(mergedQAP$sex==2))/go2.n
go2.mean.age <- mean(mergedQAP$ageAtGo2Scan/12)
go2.sd.age <- sd(mergedQAP$ageAtGo2Scan/12)
go2.output <- cbind(c('Go2'), go2.n, go2.female, go2.mean.age, go2.sd.age)

#output <- as.data.frame(rbind(go1.output, mgi.output, go2.output))
output <- as.data.frame(rbind(go1.output, mgi.output))
colnames(output) <- c('Study', 'N', '% Female', 'Age Mean', 'Age SD')
write.csv(output, './table1QAPPaper.csv', quote=F, row.names=F)
