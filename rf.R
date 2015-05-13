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

test <- read.csv("./data/Test.csv")
Train <- read.csv("./data/Train.csv")

#-----------------Learn----------------------------

#---------------------------------------RF tuning------------------------------------
tune.m <- tuneRF(Train[ttrain,-],as.numeric(Train[,4]),ntreeTry = 20,
                 stepFactor=2, improve=0.05,trace=T,plot=T,doBest=F)
#--------------------------------------RF-------------------------------------------

rf <- randomForest(units~.-date,data=Train[ttrain,],ntree = 20,maxnodes=20,ncores=4)


pred.test <- predict(rf,Train[-ttrain,])

error.table <- table(pred.test,Train$units[-ttrain])
plot(pred.test,Train$units[-ttrain])


RMSLE <- function(pred,act){
        if (length(x) == length(y)){

                return( sqrt( (1/length(pred)) * sum(   (log10(pred+1) - log10(act+1))^2  ) ) )


        }else return("not equal length")
}

#submission
pred <- predict(rf,Test)

submission <- data.frame(id=paste(test$store_nbr,test$item_nbr,test$date,sep="_"),units=pred)

write.csv(submission,"submission_may_12.csv",quote=F,row.names=F)



