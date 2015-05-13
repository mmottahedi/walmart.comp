setwd("~/cwd/walmart.comp")

#-----------------------load packages----------------------------------------
library(data.table)
library(doMC)
library(doParallel)
library(lattice)
library(randomForest)

#----------------setup parallel----------------------------------------------
cl <- makeCluster(4)
registerDoParallel(cores = 3)

#------------------------Load data-------------------------------------------
weather <- read.csv("./data/weather.csv",na.strings="M",stringsAsFactors=F)
train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")
key <-read.csv("./data/key.csv")

event.list2 <- read.csv("./data/event.list.csv")
test.event.list2 <- read.csv("./data/test.event.list.csv")

train <- data.table(train)
weather <- data.table(weather)
weather$preciptotal <- as.numeric(weather$preciptotal)
weather$snowfall <- as.numeric(weather$snowfall)
weather$date <- as.Date(weather$date)
train$date <- as.Date(train$date)

#---------------------Find the events---------------------------------------

snow.event <- weather[snowfall >= 2 ]
rain.event <- weather[preciptotal >= 1]
event <- weather[snowfall >= 2 | preciptotal >=1]


#----------------add station number to data set-----------------------------
t1 <- system.time(
        for(i in 1:45) {

                train[train$store_nbr==i,"station_nbr"] <- key$station_nbr[i]

        }
)
print(t1)


for(i in 1:45) {

        test[test$store_nbr==i,"station_nbr"] <- key$station_nbr[i]

}

event.list <- data.frame(index=1:dim(train)[1])

for (i in 1:45){

        event.list[paste("store_",i,sep="")] <- train$date == event$date & event$station_nbr == key$station_nbr[i]

}


test.event.list <- data.frame(index=1:dim(test)[1])
for (i in 1:45){

        test.event.list[paste("store_",i,sep="")] <- test$date == event$date & event$station_nbr == key$station_nbr[i]

}

#--------------------SET +- 3 DAYS -----------------------------------------
event.list <- as.matrix(event.list)
event.list2 <- as.matrix(event.list)

for(i in 2:dim(event.list)[2]) {
        print(i)

        for (j in 1:dim(event.list)[1]) {
                # print(j)

                if (event.list[j,i]==T) {
                        print("event")
                        event.list2[j-3:j+3,i] <- T
                }

        }

}

test.event.list <- as.matrix(test.event.list)
test.event.list2 <- as.matrix(test.event.list)

for(i in 2:dim(test.event.list)[2]) {
        print(i)

        for (j in 2:dim(test.event.list)[1]) {
                print(j)

                if (test.event.list[j,i]==T) {
                        print("event")
                        test.event.list2[j-3:j+3,i] <- T
                }

        }

}


#---------------------MAKE LEARNING DATASET-----------------------

Test <- cbind(test,test.event.list2[,-1])

Train <- cbind(train,event.list2[,-1])




#event.vec <- apply(event.list[ttrain,],1,"|")
Train <- as.matrix(Train)
Train[,4] <- as.numeric(Train[,4])
Train[,1] <- as.Date(Train[,1])
