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
train2 <- copy(train)
test2 <- copy(test)

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
#xx$V1 <- xx$V1 -1
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
x2 <- copy(x)
testing2<- copy(testing)
x2[,c(35:44,50,51):=NULL]
testing2[,c(35:44,50):=NULL]

#Data Analysis
prob_xgboost <- fread("prob_xgboost.csv")
prob_indiv_xgb <- fread("Prediction1.csv")
prob_indiv_multinom <- fread("Prediction11.csv")
prob_multinom <- fread("prob_multinom.csv")
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
probability_table<-0.5*prob_xgboost+0.5*prob_multinom
probability_table2<-0.65*prob_indiv_xgb+0.35*prob_indiv_multinom
p <- as.data.frame(probability_table)
p2 <- as.data.frame(probability_table2)
p[, "max1"] <- apply(p[, 1:5], 1, function(x)x[maxn(1)(x)])
p[, "pos1"] <- apply(p[, 1:5], 1, maxn(1))
p[, "max2"] <- apply(p[, 1:5], 1, function(x)x[maxn(2)(x)])
p[, "pos2"] <- apply(p[, 1:5], 1, maxn(2))
p[, "max3"] <- apply(p[, 1:5], 1, function(x)x[maxn(3)(x)])
p[, "pos3"] <- apply(p[, 1:5], 1, maxn(3))
p[, "max_1"] <- apply(p2[, 1:5], 1, function(x)x[maxn(1)(x)])
p[, "pos_1"] <- apply(p2[, 1:5], 1, maxn(1))
p[, "max_2"] <- apply(p2[, 1:5], 1, function(x)x[maxn(2)(x)])
p[, "pos_2"] <- apply(p2[, 1:5], 1, maxn(2))
p[, "max_3"] <- apply(p2[, 1:5], 1, function(x)x[maxn(3)(x)])
p[, "pos_3"] <- apply(p2[, 1:5], 1, maxn(3))
#BEST ANSWER YET GENERATION
p <- as.data.table(p)
p$sum2 <- p$max1 + p$max2
p$sum3 <- p$max1 + p$max2 + p$max3
p$past <- testing$party_voted_past
p$ans <- p$pos1
ans <- p$ans
ans <- as.data.table(ans)
ans$ans[ans$ans == 1 ] <- "Centaur"
ans$ans[ans$ans == 2 ] <- "Cosmos"
ans$ans[ans$ans == 3 ] <- "Ebony"
ans$ans[ans$ans == 4 ] <- "Odyssey"
ans$ans[ans$ans == 5 ] <- "Tokugawa"
test <- fread("test.csv")
sol <- test$citizen_id
sol <- as.data.table(sol)
sol$citizen_id <- sol$sol
sol[,sol:=NULL]
sol$actual_vote <- ans$ans
sol2 <- copy(sol)
write.csv(sol,"StockUp_IITKGP_148.csv",row.names = FALSE)

#CITIES INFO 2


city_old1 <- function(a){
  city_list <- unique(a$mvar33)
  tot_pop <- c()
  for(i in city_list){
    tot_pop <- c(tot_pop,length(which(a$mvar33==i)))
  }
  #tot_pop
  while(TRUE){
    inp <- readline(prompt = "Enter ")
    if(inp==1){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar33 == i & a$party_voted_past == "Centaur"))/tot_pop[which(city_list == i)]))
      }
      city <<- city_list
      city1 <<- percent
    }
    else if(inp==2){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar33 == i & a$party_voted_past == "Ebony"))/tot_pop[which(city_list == i)]))
      }
      city2 <<- percent
    }
    else if(inp==3){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar33 == i & a$party_voted_past == "Tokugawa"))/tot_pop[which(city_list == i)]))
      }
      city3 <<- percent
    }
    else if(inp==4){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar33 == i & a$party_voted_past == "Odyssey"))/tot_pop[which(city_list == i)]))
      }
      city4 <<- percent
    }
    else if(inp==5){
      percent <- c()
      for(i in city_list){
        percent <- c(percent,100*(length(which(a$mvar33 == i & a$party_voted_past == "Cosmos"))/tot_pop[which(city_list == i)]))
      }
      city5 <<- percent
    }
    else if(inp==6){
      break
    }
  }
}
library(reshape)
city_old1(train)
city_list <- t(rbind(city,city1,city2,city3,city4,city5))
city_list <- as.data.table(city_list)
city_list$city1 <- as.numeric(city_list$city1)
city_list$city2 <- as.numeric(city_list$city2)
city_list$city3 <- as.numeric(city_list$city3)
city_list$city4 <- as.numeric(city_list$city4)
city_list$city5 <- as.numeric(city_list$city5)
city_list$city1[city_list$city1 == 100] <- 0
city_list$city2[city_list$city2 == 100] <- 0
city_list$city3[city_list$city3 == 100] <- 0
city_list$city4[city_list$city4 == 100] <- 0
city_list$city5[city_list$city5 == 100] <- 0

cities_1 <- city_list$city[city_list$city1 >= 80]
cities_2 <- city_list$city[city_list$city2 >= 80]
cities_3 <- city_list$city[city_list$city3 >= 80]
cities_4 <- city_list$city[city_list$city4 >= 80]
cities_5 <- city_list$city[city_list$city5 >= 80]
#View(city_list)


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
city_list$city1[city_list$city1 == 100] <- 0
city_list$city2[city_list$city2 == 100] <- 0
city_list$city3[city_list$city3 == 100] <- 0
city_list$city4[city_list$city4 == 100] <- 0
city_list$city5[city_list$city5 == 100] <- 0

cities1 <- city_list$city[city_list$city1 >= 80]
cities2 <- city_list$city[city_list$city2 >= 80]
cities3 <- city_list$city[city_list$city3 >= 80]
cities4 <- city_list$city[city_list$city4 >= 80]
cities5 <- city_list$city[city_list$city5 >= 80]
#View(city_list)

#CHANGING ANSWERS


sol$actual_vote <- NA


sol$actual_vote[which(p$max_1 - p$max_2 >= 0.3 & (p$pos1!=p$pos_1) )] <- sol2$actual_vote[which(p$max_1 - p$max_2 >= 0.3 & (p$pos1!=p$pos_1))]

sol$actual_vote[which(p$max1 - p$max2 >= 0.35 & (p$pos1==p$pos_1) )] <- ans$ans[which(p$max1 - p$max2 >= 0.35 & (p$pos1==p$pos_1))]

sol$actual_vote[which(p$max_1 - p$max_2 >= 0.4 & (p$pos1==p$pos_1) )] <- ans$ans[which(p$max_1 - p$max_2 >= 0.4 & (p$pos1==p$pos_1))]

sol$actual_vote[which(p$max1 - p$max2 >= 0.15 & (p$pos1!=p$pos_1) )] <- ans$ans[which(p$max1 - p$max2 >= 0.15 & (p$pos1!=p$pos_1))]



z <- testing2
z$new <- (z$mvar1+z$mvar2+z$mvar3+z$mvar4+z$mvar5)/5
z$new1 <- (z$mvar2+z$mvar3+z$mvar4+z$mvar5)/4
z$new2 <- (z$mvar1+z$mvar3+z$mvar4+z$mvar5)/4
z$new3 <- (z$mvar1+z$mvar2+z$mvar4+z$mvar5)/4
z$new4 <- (z$mvar1+z$mvar2+z$mvar3+z$mvar5)/4
z$new5 <- (z$mvar1+z$mvar2+z$mvar3+z$mvar4)/4

sol$actual_vote[which(is.na(sol$actual_vote))] <- "0"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new1 >= 2)] <- "Centaur"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new2 >= 2)] <- "Ebony"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new3 >= 2)] <- "Tokugawa"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new4 >= 2.5)] <- "Odyssey"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new5 >= 2)] <- "Cosmos"

z <- testing2
z$new <- (z$mvar21+z$mvar22+z$mvar23+z$mvar24+z$mvar25)/5
z$new1 <- (z$mvar22+z$mvar23+z$mvar24+z$mvar25)/4
z$new2 <- (z$mvar21+z$mvar23+z$mvar24+z$mvar25)/4
z$new3 <- (z$mvar21+z$mvar22+z$mvar24+z$mvar25)/4
z$new4 <- (z$mvar21+z$mvar22+z$mvar23+z$mvar25)/4
z$new5 <- (z$mvar21+z$mvar22+z$mvar23+z$mvar24)/4

sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new1 >= 3)] <- "Centaur"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new2 >= 3)] <- "Ebony"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new3 >= 3)] <- "Tokugawa"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new4 >= 3.2)] <- "Odyssey"
sol$actual_vote[which(sol$actual_vote == "0" & z$new-z$new5 >= 3)] <- "Cosmos"


sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities1))] <- "Centaur"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities2))] <- "Ebony"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities3))] <- "Tokugawa"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities4))] <- "Odyssey"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities5))] <- "Cosmos"


sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities_1))] <- "Centaur"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities_2))] <- "Ebony"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities_3))] <- "Tokugawa"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities_4))] <- "Odyssey"
sol$actual_vote[which(sol$actual_vote == "0" & (test$mvar32 %in% cities_5))] <- "Cosmos"

sol$actual_vote[which(is.na(sol$actual_vote))] <- sol_no_history$actual_vote[which(is.na(sol$actual_vote))]

sol$actual_vote[sol$actual_vote == "0"] <- test$party_voted_past[sol$actual_vote=="0"] 

