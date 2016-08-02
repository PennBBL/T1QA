## Load the data
source('/home/adrose/qapQA/scripts/loadGo1Data.R')
detachAllPackages()
set.seed(16)

## Load Library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('glmnet', 'bootstrap', 'psych', 'caret', 'doMC')

# Declare any functions
rmFat <- function(dataFrameToTrim, betaValues, cutOffValue){
  # First create a row of Boolieans by finding which vairbales were selected %100
  # of the time
  bar <- betaValues[,-1]
  boo.vals.tmp <- rowSums(abs(sign(apply(bar, 2, function(x) as.numeric(as.character(x))))))
  boo.vals <- rep("FALSE", length(boo.vals.tmp))
  #boo.vals[which(boo.vals.tmp<cutOffValue)] <- 'FALSE'
  boo.vals[which(boo.vals.tmp>=cutOffValue)] <- 'TRUE'
  
  # Now return the data frame
  cog.data <- dataFrameToTrim[,1]
  imag.data <- dataFrameToTrim[,2:dim(dataFrameToTrim)[2]]

  # Return the output
  data.frame.to.return <- cbind(cog.data, imag.data[,which(boo.vals=='TRUE')])
  return(data.frame.to.return)
}
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(modm,x){cbind(1,x)%*%modm$coef}



## Get the data in a system we want it 
mergedQAP$zeroVsNotZero <- mergedQAP$averageRating
mergedQAP$zeroVsNotZero[mergedQAP$zeroVsNotZero >=1] <- 1
folds <- createFolds(mergedQAP$zeroVsNotZero, k=3, list=T, returnTrain=T)
raw.lme.data <- merge(isolatedVars, manualQAData2, by='bblid')
raw.lme.data[,2:32] <- scale(raw.lme.data[,2:32], center=T, scale=T)
raw.lme.data$averageRating.x <- as.numeric(as.character(raw.lme.data$averageRating.x))
raw.lme.data$averageRating.x[raw.lme.data$averageRating.x>1] <- 1
index <- unlist(folds[1])
trainingData <- raw.lme.data[index,]




enet.alphas <- seq(0,1,by=0.05)
enet.optparam <- matrix(nrow=21,ncol=3)
colnames(enet.optparam)<-c("Alpha","Lambda","CVM")
count=1
C=length(train)-1
train <- trainingData

for (a in enet.alphas){
        # Change this column val
	enet.alphas.cv<-cv.glmnet(as.matrix(train[,2:32]),train$averageRating.x,alpha=a, standardize=FALSE,nfolds=10, family="binomial", type.measure="class")
	enet.optparam[count,]<-c(a,enet.alphas.cv$lambda.min,enet.alphas.cv$cvm[enet.alphas.cv$lambda==enet.alphas.cv$lambda.min])
	count=count+1}


# Find the optimal alpha
optval<-enet.optparam[which(enet.optparam[,3]==min(enet.optparam[,3])),]

# Now run the first model
mod <- glmnet(as.matrix(train[,2:32]), train$averageRating.x, standardize=FALSE, alpha=optval[1],lambda=optval[2], family="binomial")

res <- matrix(0, 31, 51)
res[,1:2] <- cbind(colnames(train[,2:32]),as.numeric(mod$beta))


for (i in 3:51) {

count=1

for (a in enet.alphas){
	enet.alphas.cv<-cv.glmnet(as.matrix(train[,2:32]),train$averageRating.x,alpha=a, standardize=FALSE,nfolds=3, family="binomial", type.measure="class")
	enet.optparam[count,]<-c(a,enet.alphas.cv$lambda.min,enet.alphas.cv$cvm[enet.alphas.cv$lambda==enet.alphas.cv$lambda.min])
	count=count+1}

optval<-enet.optparam[which(enet.optparam[,3]==min(enet.optparam[,3])),]

mod <- glmnet(as.matrix(train[,2:32]), train$averageRating.x, standardize=FALSE, alpha=optval[1],lambda=optval[2], family="binomial")
res[,i] <- as.numeric(mod$beta)
}


y <- train$averageRating.x
Xm <- rmFat(as.matrix(train[,1:32]), res, 33)
Xm <- Xm[,-1]

foo <- as.data.frame(cbind(y, Xm))
for(i in seq(2,28)){foo[,i] <- as.numeric(as.character(foo[,i]))}


modm <- glm(foo$y ~ . ,data = foo, family="binomial", control=glm.control(maxit=100000000, trace=TRUE))
p <- dim(Xm)[2]
n <- dim(Xm)[1]
