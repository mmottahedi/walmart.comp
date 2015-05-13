setwd("~/cwd/walmart.comp")

#----------Load Package----------------------------
# library(data.table)
# library(doMC)
# library(doParallel)
# library(lattice)
# library(randomForest)
library(xgboost)
library(FeatureHashing)

#registerDoParallel(cores = 4)

#------------------Read Data-----------------------
Train <- read.csv("./data/Train.csv",)
Train < as.matrix.data.frame(Train)
dtrain <- xgb.DMatrix(data=Train)



Test <- read.csv("./data/Test.csv")


dtrain <- xgb.DMatrix(data=as.matrix(Train))
#-----------------error calculation ---------------------------------------------
RMSLE <- function(pred,act){
        if (length(pred) == length(act)){
                return( sqrt( (1/length(pred)) * sum(   (log(pred+1) - log(act+1))^2  ) ) )
        }else return("not equal length")}

#---------------------------------------------------------------------------------------------







