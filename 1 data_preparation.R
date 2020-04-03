setwd("C:/Final year/Competitions/AmEx")
#Libraries
library(data.table)
library(ggplot2)
library(plyr)
library(xgboost)
library(rattle)
library(rpart)
library(RColorBrewer)
library(e1071)
library(clusterSim)
#Seed & Read
set.seed(100)
train <- fread("train.csv")
test <- fread("test.csv")
test_all <- fread("test_all.csv")
answer <- fread("StockUp_IITKGP_240.csv")
#best <- fread("Double2.csv")
train2 <- copy(train)
test2 <- copy(test)
train_cool <- train[train$party_voted_past==train$actual_vote]
train_hot <- na.omit(train[train$party_voted_past!=train$actual_vote])
#train <- copy(train_cool)
#train <- copy(train_hot)


#Features for location

#CITIES INFO 1
city_old <- function(a){
  city_list <- unique(a$mvar32)
  tot_pop <- c()
  for(i in city_list){
    tot_pop <- c(tot_pop,length(which(a$mvar32==i)))
  }
  #tot_pop
  while(TRUE){
    inp <- readline(prompt = "Enter ")
    if(inp==1){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar32 == i & a$party_voted_past == "Centaur"))/tot_pop[which(city_list == i)]))
      }
      city <<- city_list
      city1 <<- percent
    }
    else if(inp==2){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar32 == i & a$party_voted_past == "Ebony"))/tot_pop[which(city_list == i)]))
      }
      city2 <<- percent
    }
    else if(inp==3){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar32 == i & a$party_voted_past == "Tokugawa"))/tot_pop[which(city_list == i)]))
      }
      city3 <<- percent
    }
    else if(inp==4){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar32 == i & a$party_voted_past == "Odyssey"))/tot_pop[which(city_list == i)]))
      }
      city4 <<- percent
    }
    else if(inp==5){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar32 == i & a$party_voted_past == "Cosmos"))/tot_pop[which(city_list == i)]))
      }
      city5 <<- percent
    }
    else if(inp==6){
      break
    }
  }
}
library(reshape)
city_old(train)
city_list <- t(rbind(city,city1,city2,city3,city4,city5))
city_list <- as.data.table(city_list)
city_list$city1 <- as.numeric(city_list$city1)
city_list$city2 <- as.numeric(city_list$city2)
city_list$city3 <- as.numeric(city_list$city3)
city_list$city4 <- as.numeric(city_list$city4)
city_list$city5 <- as.numeric(city_list$city5)
city_list$mvar32 <- city_list$city
city_list[,city:=NULL]

setkey(train,mvar32)
setkey(city_list,mvar32)
train <- merge(train,city_list)
city_old(test)
city_list <- t(rbind(city,city1,city2,city3,city4,city5))
city_list <- as.data.table(city_list)
city_list$city1 <- as.numeric(city_list$city1)
city_list$city2 <- as.numeric(city_list$city2)
city_list$city3 <- as.numeric(city_list$city3)
city_list$city4 <- as.numeric(city_list$city4)
city_list$city5 <- as.numeric(city_list$city5)
city_list$mvar32 <- city_list$city
city_list[,city:=NULL]

setkey(test,mvar32)
setkey(city_list,mvar32)
test <- merge(test,city_list)


#PREPROCESSING
train <- na.omit(train)
train_id <- train$citizen_id
test[is.na(test)] <- 0
test$mvar30[test$mvar30=="0"]<- "Degree"
test$mvar30[test$mvar30==""]<- "Diploma"
x <- subset(train, select=-actual_vote)
w <- subset(test)
x <- rbind(test,x)
x <- as.data.table(x)
x <-x[,colnames(x)[c(1)]:= NULL,with=F]
xx <- as.data.table(c(x$party_voted_past,train$actual_vote))
xx$V1 <- as.numeric(as.factor(xx$V1))
xx$V1 <- xx$V1 -1
x$party_voted_past <- xx$V1[1:nrow(x)]
y <- xx$V1[81307:(nrow(xx))]
x$mvar27 <- as.numeric(as.factor(x$mvar27))
x$mvar30 <- as.numeric(as.factor(x$mvar30))
xx <- as.data.table(c(x$mvar32,x$mvar33))
xx$V1 <- as.numeric(as.factor(xx$V1))
x$mvar32 <- xx$V1[1:nrow(x)]
x$mvar33 <- xx$V1[(nrow(x)+1):(2*nrow(x))]
testing <- x[1:21207]
x <- x[21208:nrow(x)]

#Feature Generation
x$var1 <- as.integer((20*x$mvar1+10*x$mvar6+5*x$mvar11+4*x$mvar16+15*x$mvar21))
x$var2 <- as.integer((20*x$mvar2+10*x$mvar7+5*x$mvar12+4*x$mvar17+15*x$mvar22))
x$var3 <- as.integer((20*x$mvar3+10*x$mvar8+5*x$mvar13+4*x$mvar18+15*x$mvar23))
x$var4 <- as.integer((20*x$mvar4+10*x$mvar9+5*x$mvar14+4*x$mvar19+15*x$mvar24))
x$var5 <- as.integer((20*x$mvar5+10*x$mvar10+5*x$mvar15+4*x$mvar20+15*x$mvar25))
x$var6 <- as.numeric((x$var1)/(1+x$var1+x$var2+x$var3+x$var4+x$var5))
x$var7 <- as.numeric((x$var2)/(1+x$var1+x$var2+x$var3+x$var4+x$var5))
x$var8 <- as.numeric((x$var3)/(1+x$var1+x$var2+x$var3+x$var4+x$var5))
x$var9 <- as.numeric((x$var4)/(1+x$var1+x$var2+x$var3+x$var4+x$var5))
x$var10 <- as.numeric((x$var5)/(1+x$var1+x$var2+x$var3+x$var4+x$var5))
x$var11 <- 0.05^(1+x$var6)
x$var12 <- 0.05^(1+x$var7)
x$var13 <- 0.05^(1+x$var8)
x$var14 <- 0.05^(1+x$var9)
x$var15 <- 0.05^(1+x$var10)
testing$var1 <- as.integer((20*testing$mvar1+10*testing$mvar6+5*testing$mvar11+4*testing$mvar16+15*testing$mvar21))
testing$var2 <- as.integer((20*testing$mvar2+10*testing$mvar7+5*testing$mvar12+4*testing$mvar17+15*testing$mvar22))
testing$var3 <- as.integer((20*testing$mvar3+10*testing$mvar8+5*testing$mvar13+4*testing$mvar18+15*testing$mvar23))
testing$var4 <- as.integer((20*testing$mvar4+10*testing$mvar9+5*testing$mvar14+4*testing$mvar19+15*testing$mvar24))
testing$var5 <- as.integer((20*testing$mvar5+10*testing$mvar10+5*testing$mvar15+4*testing$mvar20+15*testing$mvar25))
testing$var6 <- as.numeric((testing$var1)/(1+testing$var1+testing$var2+testing$var3+testing$var4+testing$var5))
testing$var7 <- as.numeric((testing$var2)/(1+testing$var1+testing$var2+testing$var3+testing$var4+testing$var5))
testing$var8 <- as.numeric((testing$var3)/(1+testing$var1+testing$var2+testing$var3+testing$var4+testing$var5))
testing$var9 <- as.numeric((testing$var4)/(1+testing$var1+testing$var2+testing$var3+testing$var4+testing$var5))
testing$var10 <- as.numeric((testing$var5)/(1+testing$var1+testing$var2+testing$var3+testing$var4+testing$var5))
testing$var11 <- 0.05^(1+testing$var6)
testing$var12 <- 0.05^(1+testing$var7)
testing$var13 <- 0.05^(1+testing$var8)
testing$var14 <- 0.05^(1+testing$var9)
testing$var15 <- 0.05^(1+testing$var10)
x$target <- train$actual_vote
x$varr <- x$var1+x$var2+x$var3+x$var4+x$var5
testing$varr <- testing$var1+testing$var2+testing$var3+testing$var4+testing$var5


x$mvar100 <- copy(x$mvar1)
x$mvar200 <- copy(x$mvar2)
x$mvar300 <- copy(x$mvar3)
x$mvar400 <- copy(x$mvar4)
x$mvar500 <- copy(x$mvar5)


testing$mvar100 <- copy(testing$mvar1)
testing$mvar200 <- copy(testing$mvar2)
testing$mvar300 <- copy(testing$mvar3)
testing$mvar400 <- copy(testing$mvar4)
testing$mvar500 <- copy(testing$mvar5)

#NEW_FEATURES
x$mvar100[x$party_voted_past==0] <- x$mvar1[x$party_voted_past==0]+1.6*mean(x$mvar1[x$party_voted_past==0])
x$mvar100[x$party_voted_past==1] <- x$mvar1[x$party_voted_past==1]+1.6*mean(x$mvar1[x$party_voted_past==1])
x$mvar100[x$party_voted_past==2] <- x$mvar1[x$party_voted_past==2]+1.6*mean(x$mvar1[x$party_voted_past==2])
x$mvar100[x$party_voted_past==3] <- x$mvar1[x$party_voted_past==3]+1.6*mean(x$mvar1[x$party_voted_past==3])
x$mvar100[x$party_voted_past==4] <- x$mvar1[x$party_voted_past==4]+1.6*mean(x$mvar1[x$party_voted_past==4])

x$mvar200[x$party_voted_past==0] <- x$mvar2[x$party_voted_past==0]+1.6*mean(x$mvar2[x$party_voted_past==0])
x$mvar200[x$party_voted_past==1] <- x$mvar2[x$party_voted_past==1]+1.6*mean(x$mvar2[x$party_voted_past==1])
x$mvar200[x$party_voted_past==2] <- x$mvar2[x$party_voted_past==2]+1.6*mean(x$mvar2[x$party_voted_past==2])
x$mvar200[x$party_voted_past==3] <- x$mvar2[x$party_voted_past==3]+1.6*mean(x$mvar2[x$party_voted_past==3])
x$mvar200[x$party_voted_past==4] <- x$mvar2[x$party_voted_past==4]+1.6*mean(x$mvar2[x$party_voted_past==4])

x$mvar300[x$party_voted_past==0] <- x$mvar3[x$party_voted_past==0]+0.88*mean(x$mvar3[x$party_voted_past==0])
x$mvar300[x$party_voted_past==1] <- x$mvar3[x$party_voted_past==1]+0.88*mean(x$mvar3[x$party_voted_past==1])
x$mvar300[x$party_voted_past==2] <- x$mvar3[x$party_voted_past==2]+0.88*mean(x$mvar3[x$party_voted_past==2])
x$mvar300[x$party_voted_past==3] <- x$mvar3[x$party_voted_past==3]+0.88*mean(x$mvar3[x$party_voted_past==3])
x$mvar300[x$party_voted_past==4] <- x$mvar3[x$party_voted_past==4]+0.88*mean(x$mvar3[x$party_voted_past==4])

x$mvar400[x$party_voted_past==0] <- x$mvar4[x$party_voted_past==0]+1.4*mean(x$mvar4[x$party_voted_past==0])
x$mvar400[x$party_voted_past==1] <- x$mvar4[x$party_voted_past==1]+1.4*mean(x$mvar4[x$party_voted_past==1])
x$mvar400[x$party_voted_past==2] <- x$mvar4[x$party_voted_past==2]+1.4*mean(x$mvar4[x$party_voted_past==2])
x$mvar400[x$party_voted_past==3] <- x$mvar4[x$party_voted_past==3]+1.4*mean(x$mvar4[x$party_voted_past==3])
x$mvar400[x$party_voted_past==4] <- x$mvar4[x$party_voted_past==4]+1.4*mean(x$mvar4[x$party_voted_past==4])


x$mvar500[x$party_voted_past==0] <- x$mvar5[x$party_voted_past==0]+1.1*mean(x$mvar5[x$party_voted_past==0])
x$mvar500[x$party_voted_past==1] <- x$mvar5[x$party_voted_past==1]+1.1*mean(x$mvar5[x$party_voted_past==1])
x$mvar500[x$party_voted_past==2] <- x$mvar5[x$party_voted_past==2]+1.1*mean(x$mvar5[x$party_voted_past==2])
x$mvar500[x$party_voted_past==3] <- x$mvar5[x$party_voted_past==3]+1.1*mean(x$mvar5[x$party_voted_past==3])
x$mvar500[x$party_voted_past==4] <- x$mvar5[x$party_voted_past==4]+1.1*mean(x$mvar5[x$party_voted_past==4])


#NEW_FEATURES
testing$mvar100[testing$party_voted_past==0] <- testing$mvar1[testing$party_voted_past==0]+1.6*mean(testing$mvar1[testing$party_voted_past==0])
testing$mvar100[testing$party_voted_past==1] <- testing$mvar1[testing$party_voted_past==1]+1.6*mean(testing$mvar1[testing$party_voted_past==1])
testing$mvar100[testing$party_voted_past==2] <- testing$mvar1[testing$party_voted_past==2]+1.6*mean(testing$mvar1[testing$party_voted_past==2])
testing$mvar100[testing$party_voted_past==3] <- testing$mvar1[testing$party_voted_past==3]+1.6*mean(testing$mvar1[testing$party_voted_past==3])
testing$mvar100[testing$party_voted_past==4] <- testing$mvar1[testing$party_voted_past==4]+1.6*mean(testing$mvar1[testing$party_voted_past==4])

testing$mvar200[testing$party_voted_past==0] <- testing$mvar2[testing$party_voted_past==0]+1.6*mean(testing$mvar2[testing$party_voted_past==0])
testing$mvar200[testing$party_voted_past==1] <- testing$mvar2[testing$party_voted_past==1]+1.6*mean(testing$mvar2[testing$party_voted_past==1])
testing$mvar200[testing$party_voted_past==2] <- testing$mvar2[testing$party_voted_past==2]+1.6*mean(testing$mvar2[testing$party_voted_past==2])
testing$mvar200[testing$party_voted_past==3] <- testing$mvar2[testing$party_voted_past==3]+1.6*mean(testing$mvar2[testing$party_voted_past==3])
testing$mvar200[testing$party_voted_past==4] <- testing$mvar2[testing$party_voted_past==4]+1.6*mean(testing$mvar2[testing$party_voted_past==4])

testing$mvar300[testing$party_voted_past==0] <- testing$mvar3[testing$party_voted_past==0]+0.88*mean(testing$mvar3[testing$party_voted_past==0])
testing$mvar300[testing$party_voted_past==1] <- testing$mvar3[testing$party_voted_past==1]+0.88*mean(testing$mvar3[testing$party_voted_past==1])
testing$mvar300[testing$party_voted_past==2] <- testing$mvar3[testing$party_voted_past==2]+0.88*mean(testing$mvar3[testing$party_voted_past==2])
testing$mvar300[testing$party_voted_past==3] <- testing$mvar3[testing$party_voted_past==3]+0.88*mean(testing$mvar3[testing$party_voted_past==3])
testing$mvar300[testing$party_voted_past==4] <- testing$mvar3[testing$party_voted_past==4]+0.88*mean(testing$mvar3[testing$party_voted_past==4])

testing$mvar400[testing$party_voted_past==0] <- testing$mvar4[testing$party_voted_past==0]+1.4*mean(testing$mvar4[testing$party_voted_past==0])
testing$mvar400[testing$party_voted_past==1] <- testing$mvar4[testing$party_voted_past==1]+1.4*mean(testing$mvar4[testing$party_voted_past==1])
testing$mvar400[testing$party_voted_past==2] <- testing$mvar4[testing$party_voted_past==2]+1.4*mean(testing$mvar4[testing$party_voted_past==2])
testing$mvar400[testing$party_voted_past==3] <- testing$mvar4[testing$party_voted_past==3]+1.4*mean(testing$mvar4[testing$party_voted_past==3])
testing$mvar400[testing$party_voted_past==4] <- testing$mvar4[testing$party_voted_past==4]+1.4*mean(testing$mvar4[testing$party_voted_past==4])


testing$mvar500[testing$party_voted_past==0] <- testing$mvar5[testing$party_voted_past==0]+1.1*mean(testing$mvar5[testing$party_voted_past==0])
testing$mvar500[testing$party_voted_past==1] <- testing$mvar5[testing$party_voted_past==1]+1.1*mean(testing$mvar5[testing$party_voted_past==1])
testing$mvar500[testing$party_voted_past==2] <- testing$mvar5[testing$party_voted_past==2]+1.1*mean(testing$mvar5[testing$party_voted_past==2])
testing$mvar500[testing$party_voted_past==3] <- testing$mvar5[testing$party_voted_past==3]+1.1*mean(testing$mvar5[testing$party_voted_past==3])
testing$mvar500[testing$party_voted_past==4] <- testing$mvar5[testing$party_voted_past==4]+1.1*mean(testing$mvar5[testing$party_voted_past==4])

#Removing Outliers
x$mvar100[x$mvar100 > 29.5] = 29.5
x$mvar200[x$mvar200 > 30.4] = 30.37554
x$mvar300[x$mvar300 > 23] = 22.9
x$mvar400[x$mvar400 > 30.4] = 30.35
x$mvar500[x$mvar500 > 26.4] = 26.35
