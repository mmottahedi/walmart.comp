setwd("~/cwd/walmart.comp")

#----------Load Package----------------------------
library(data.table)
library(doMC)
library(doParallel)
library(lattice)
library(randomForest)

cl <- makeCluster(4)
registerDoParallel(cores = 3)

#------------------Read Data-----------------------

Test <- read.csv("./data/Test.csv")
Train <- read.csv("./data/Train.csv")

#-----------------error calculation ---------------------------------------------
RMSLE <- function(pred,act){
        if (length(pred) == length(act)){
                return( sqrt( (1/length(pred)) * sum(   (log(pred+1) - log(act+1))^2  ) ) )
        }else return("not equal length")}

#-----------------Learn----------------------------
ttrain <- sample(1:nrow(Train),nrow(Train)*0.2)
#---------------------------------------RF tuning------------------------------------
# tune.m <- tuneRF(Train[ttrain,-c(4,5)],as.numeric(Train[ttrain,4]),ntreeTry = 15,stepFactor=2, improve=0.05,trace=T,plot=T,doBest=F)
# --------------------------------------RF-------------------------------------------

rf <- randomForest(units~.-date,data=Train[ttrain,],ntree =30 ,maxnodes=20,ncores=4)


pred.test <- predict(rf,Train[-ttrain,])
error <- RMSLE(rep(0,length(Train$units[-ttrain])),Train$units[-ttrain])
print(error)




#submission
pred <- predict(rf,Test)
submission <- data.frame(id=paste(Test$store_nbr,Test$item_nbr,Test$date,sep="_"),units=rep(0,length(pred)))
write.csv(submission,paste("submission",Sys.time(),"csv",sep="."),quote=F,row.names=F)



