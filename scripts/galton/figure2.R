# AFGR June 4 2016
# This R script is going to be used to produce figure 2 for the QAP paper
# It is going to produce 4 bar graphs in a 2 x 2 matrix
# The bar graphs are going to have the rating bin distribution for each of the QAP 
# data sets The final bar graph will have the final break down 

## Load Data 
## Load the Go1 data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')

## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
detachAllPackages()
install_load('ggplot2','scales', 'irr')

## Now prep the Go1 figure
dataQaDf <- as.data.frame(table(round(mergedQAP$rawAverageRating, digits=2)))

# Now save the vector of the rating in a sepoerate variable for later
go1.data.qulaity <- mergedQAP$rawAverageRating

## Now find the ICC... somehow
agreeMatrix.go1 <- cbind(mergedQAP$ratingJB, mergedQAP$ratingKS, mergedQAP$ratingLV)
perc.agree.go1 <- agree(agreeMatrix.go1)$value
kendall.go1 <- kendall(agreeMatrix.go1, correct=T)$value


## Now plot the Go1 figure
go1BG <- ggplot(dataQaDf, aes(x=Var1, y=Freq, fill=Var1)) + 
                geom_bar(stat='identity') + 
                labs(title='N of Average Image Quality for PNC', x='Average Quality Rating', y='# Images') + 
                geom_text(data=dataQaDf,aes(x=Var1,y=Freq,label=Freq),vjust=0) +
                theme_bw() +
                theme(legend.position="none") +
                annotate("text", label=paste("% Agreement =", print(round(perc.agree.go1))),
                x = 2, y=1000) + 
                annotate("text", label=paste("Kendall W =", print(round(kendall.go1, digits=2))),
                x = 2, y=800)                 




## Now do MGI
source('/home/adrose/qapQA/scripts/loadMgiData.R')
detachAllPackages()
install_load('ggplot2','scales', 'irr')
mergedQAP <- rbind(mergedQAP.penn, mergedQAP.pitt)

## Prep the data for the MGI figure
dataQaDf <- as.data.frame(table(round(mergedQAP$rawAverageRating, digits=2)))

## Now find the ICC... somehow
agreeMatrix.mgi <- cbind(mergedQAP$ratingJB, mergedQAP$ratingKS, mergedQAP$ratingLV)
perc.agree.mgi <- agree(agreeMatrix.mgi)$value
kendall.mgi <- kendall(agreeMatrix.mgi, correct=T)$value



## Now plot the MGI figure
mgiBG <- ggplot(dataQaDf, aes(x=Var1, y=Freq, fill=Var1)) + 
                geom_bar(stat='identity') + 
                labs(title='N of Average Image Quality for MGI', x='Average Quality Rating', y='# Images') + 
                geom_text(data=dataQaDf,aes(x=Var1,y=Freq,label=Freq),vjust=0) + 
                theme_bw() +
                theme(legend.position="none") + 
                annotate("text", label=paste("% Agreement =", print(round(perc.agree.mgi))),
                x = 2, y=300) + 
                annotate("text", label=paste("Kendall W =", print(round(kendall.mgi, digits=2))),
                x = 2, y=250) 

## Now do the Go2 data
mergedQAP <- mergedQAP.go2

## Now prep the go2 data
dataQaDf[1,2] <- 1
dataQaDf[2,2] <- 0
dataQaDf[3,2] <- 0
dataQaDf[4,2] <- 25
dataQaDf[5,2] <- 14
dataQaDf[6,2] <- 27
dataQaDf[7,2] <- 315

## Now find the ICC... somehow
agreeMatrix.go2 <- cbind(mergedQAP$ratingJB, mergedQAP$ratingKS, mergedQAP$ratingLV)
perc.agree.go2 <- agree(agreeMatrix.go2)$value
kendall.go2 <- kendall(agreeMatrix.go2, correct=T)$value


## Now plot the Go2 figure
go2BG <- ggplot(dataQaDf, aes(x=Var1, y=Freq, fill=Var1)) + 
                geom_bar(stat='identity') + 
                labs(title='N of Average Image Quality for PNC 2', x='Average Quality Rating', y='# Images') + 
                geom_text(data=dataQaDf,aes(x=Var1,y=Freq,label=Freq),vjust=0) + 
                theme_bw() +
                theme(legend.position="none") +
                annotate("text", label=paste("% Agreement =", print(round(perc.agree.go2))),
                x = 2, y=275) + 
                annotate("text", label=paste("Kendall W =", print(round(kendall.go2, digits=2))),
                x = 2, y=225)


## Now do all data sets
all.data.quality <- append(go1.data.qulaity, mergedQAP.pitt$rawAverageRating)
all.data.quality <- append(all.data.quality, mergedQAP.penn$rawAverageRating)
all.data.quality <- append(all.data.quality, mergedQAP.go2$rawAverageRating)
all.data.quality <- round(all.data.quality, digits=2)


dataQaDf <- as.data.frame(table(all.data.quality))

# Now combine all of the manual ratings
agreeMatrix.all <- rbind(agreeMatrix.go1, agreeMatrix.mgi, agreeMatrix.go2)
perc.agree.all <- agree(agreeMatrix.all)$value
kendall.all <- kendall(agreeMatrix.all, correct=T)$value

## Now plot the Go2 figure
allBG <- ggplot(dataQaDf, aes(x=all.data.quality, y=Freq, fill=all.data.quality)) + 
                geom_bar(stat='identity') + 
                labs(title='N of Average Image Quality for All Images', x='Average Quality Rating', y='# Images') + 
                geom_text(data=dataQaDf,aes(x=all.data.quality,y=Freq,label=Freq),vjust=0) + 
                theme_bw() +
                theme(legend.position="none") +
                annotate("text", label=paste("% Agreement =", print(round(perc.agree.all))),
                x = 2, y=1500) + 
                annotate("text", label=paste("Kendall W =", print(round(kendall.all, digits=2))),
                x = 2, y=1300)



pdf('figure2QAPPaper.pdf', width=16, height=8)
#multiplot(go1BG, mgiBG, go2BG, allBG, cols=2)
multiplot(go1BG, mgiBG, cols=2)
dev.off()
