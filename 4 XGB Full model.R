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
library(dplyr)

#Seed & Read
set.seed(100)
train <- fread("train.csv")
test <- fread("test.csv")
test_all <- fread("test_all.csv")
train2 <- copy(train)
test2 <- copy(test)
train_cool <- train[train$party_voted_past==train$actual_vote]
train_hot <- na.omit(train[train$party_voted_past!=train$actual_vote])

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
 #1.55 1.7
x$mvar200[x$party_voted_past==0] <- x$mvar2[x$party_voted_past==0]+1.6*mean(x$mvar2[x$party_voted_past==0])
x$mvar200[x$party_voted_past==1] <- x$mvar2[x$party_voted_past==1]+1.6*mean(x$mvar2[x$party_voted_past==1])
x$mvar200[x$party_voted_past==2] <- x$mvar2[x$party_voted_past==2]+1.6*mean(x$mvar2[x$party_voted_past==2])
x$mvar200[x$party_voted_past==3] <- x$mvar2[x$party_voted_past==3]+1.6*mean(x$mvar2[x$party_voted_past==3])
x$mvar200[x$party_voted_past==4] <- x$mvar2[x$party_voted_past==4]+1.6*mean(x$mvar2[x$party_voted_past==4])
 # 1.58 1.66
x$mvar300[x$party_voted_past==0] <- x$mvar3[x$party_voted_past==0]+0.88*mean(x$mvar3[x$party_voted_past==0])
x$mvar300[x$party_voted_past==1] <- x$mvar3[x$party_voted_past==1]+0.88*mean(x$mvar3[x$party_voted_past==1])
x$mvar300[x$party_voted_past==2] <- x$mvar3[x$party_voted_past==2]+0.88*mean(x$mvar3[x$party_voted_past==2])
x$mvar300[x$party_voted_past==3] <- x$mvar3[x$party_voted_past==3]+0.88*mean(x$mvar3[x$party_voted_past==3])
x$mvar300[x$party_voted_past==4] <- x$mvar3[x$party_voted_past==4]+0.88*mean(x$mvar3[x$party_voted_past==4])
 #1.52 1.6
x$mvar400[x$party_voted_past==0] <- x$mvar4[x$party_voted_past==0]+1.4*mean(x$mvar4[x$party_voted_past==0])
x$mvar400[x$party_voted_past==1] <- x$mvar4[x$party_voted_past==1]+1.4*mean(x$mvar4[x$party_voted_past==1])
x$mvar400[x$party_voted_past==2] <- x$mvar4[x$party_voted_past==2]+1.4*mean(x$mvar4[x$party_voted_past==2])
x$mvar400[x$party_voted_past==3] <- x$mvar4[x$party_voted_past==3]+1.4*mean(x$mvar4[x$party_voted_past==3])
x$mvar400[x$party_voted_past==4] <- x$mvar4[x$party_voted_past==4]+1.4*mean(x$mvar4[x$party_voted_past==4])
 #1.5 1.6
x$mvar500[x$party_voted_past==0] <- x$mvar5[x$party_voted_past==0]+1.1*mean(x$mvar5[x$party_voted_past==0])
x$mvar500[x$party_voted_past==1] <- x$mvar5[x$party_voted_past==1]+1.1*mean(x$mvar5[x$party_voted_past==1])
x$mvar500[x$party_voted_past==2] <- x$mvar5[x$party_voted_past==2]+1.1*mean(x$mvar5[x$party_voted_past==2])
x$mvar500[x$party_voted_past==3] <- x$mvar5[x$party_voted_past==3]+1.1*mean(x$mvar5[x$party_voted_past==3])
x$mvar500[x$party_voted_past==4] <- x$mvar5[x$party_voted_past==4]+1.1*mean(x$mvar5[x$party_voted_past==4])
 #1.45 1.5

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
 #one hot encoding
x$hot1 <- 0
x$hot2 <- 0
x$hot3 <- 0
x$hot4 <- 0
x$hot5 <- 0
x$hot1[x$mvar27 == 1] <- 1
x$hot2[x$mvar27 == 2] <- 1
x$hot3[x$mvar27 == 3] <- 1
x$hot4[x$mvar27 == 4] <- 1
x$hot5[x$mvar27 == 5] <- 1
x[,mvar27:=NULL]
testing$hot1 <- 0
testing$hot2 <- 0
testing$hot3 <- 0
testing$hot4 <- 0
testing$hot5 <- 0
testing$hot1[testing$mvar27 == 1] <- 1
testing$hot2[testing$mvar27 == 2] <- 1
testing$hot3[testing$mvar27 == 3] <- 1
testing$hot4[testing$mvar27 == 4] <- 1
testing$hot5[testing$mvar27 == 5] <- 1
testing[,mvar27:=NULL]

x$feature1 <- 0
x$feature1[x$mvar32 == x$mvar33] <- 1
testing$feature1 <- 0
testing$feature1[testing$mvar32 == testing$mvar33] <- 1


#Removing Outliers
x$mvar100[x$mvar100 > 29.5] = 29.5
x$mvar200[x$mvar200 > 30.4] = 30.37554
x$mvar300[x$mvar300 > 23] = 22.9
x$mvar400[x$mvar400 > 30.4] = 30.35
x$mvar500[x$mvar500 > 26.4] = 26.35

#Modelling
x2 <- copy(x)
testing2<- copy(testing)
x2[,c(1,33:37,43:49,52:54):=NULL]
testing2[,c(1,33:37,43:48,51:53):=NULL]
x2 <- as.matrix(x2)
testing2 <- as.matrix(testing2)



#XGBOOST
train.xg = xgb.DMatrix(x2, label=y, missing=NA)
test.xg = xgb.DMatrix(testing2, missing=NA)
param <- list(max_depth = 10,
              eta = 0.1,
              objective="multi:softprob",
              num_class=5,
              subsample = 0.9,
              min_child_weight = 800,
              colsample_bytree = 0.9,
              base_score =0.1
)
set.seed(123)
start_time <- Sys.time()
model_xgb2 <- xgb.train(param, train.xg, nthread = 16, nround = 1600,verbose = 1)
end_time <- Sys.time()
time_taken <- end_time - start_time

#Predition
prediction1<-predict(model_xgb2,testing2, missing=NA)
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
probability_table2<-as.data.table((matrix(prediction1,nrow = 21207,byrow = T)))
p <- as.data.frame(probability_table2)
p[, "max1"] <- apply(p[, 1:5], 1, function(x)x[maxn(1)(x)])
p[, "pos1"] <- apply(p[, 1:5], 1, maxn(1))
p[, "max2"] <- apply(p[, 1:5], 1, function(x)x[maxn(2)(x)])
p[, "pos2"] <- apply(p[, 1:5], 1, maxn(2))
pp <- copy(p)

#ANSWER GENERATION
p <- as.data.table(p)
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
sol_copy <- copy(sol)
write.csv(sol,"Solution_having_outliers.csv",row.names = FALSE)



#NEURAL NET
library(neuralnet)
library(nnet)
x2 <- as.data.table(x2)
testing2 <- as.data.table(testing2)
x2$target <- y
x2[,c():=NULL]
testing2[,c():=NULL]
model_net2 <-multinom(target~., data = x2, maxit = 90)
#plot(model_net2, rep = "best")
net_result <- predict(model_net2, testing2, "probs" )
prediction1 <- net_result



###########################################################################
###########################################################################
###########################################################################
#To get ensemble of xgb and neural net multinom, rerun from line 210 to 231
probability_table2 <- probability_table2 + prediction1#####################
###########################################################################
###########################################################################
###########################################################################



ans <- prediction1
ans <- ( as.data.table(ans))
ans$ans <- as.numeric(ans$ans)
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
write.csv(sol,"Individual_Neural_Net_Multinom_Solution.csv",row.names = FALSE)



#Model3 To train for outliers
x2 <- copy(x)
testing2<- copy(testing)
x2[,c(1,33:37,43:49,52:54):=NULL]
testing2[,c(1,33:37,43:48,51:53):=NULL]
# x2[,c(12:26,34:38,44:48,53,54,49,50):=NULL]
# testing2[,c(12:26,34:38,44:48,53,52,49):=NULL]
x2$target <- y
x3 <- sample_n(x2,21000)
x2 <- x2[which(train$citizen_id %in% train_hot$citizen_id)]
x2 <- rbind(x2,x3)
yy <- x2$target
x2[,target:=NULL]
x2 <- as.matrix(x2)
testing2 <- as.matrix(testing2)

#XGBOOST
train.xg = xgb.DMatrix(x2, label=yy, missing=NA)
test.xg = xgb.DMatrix(testing2, missing=NA)
param <- list(max_depth = 10,
              eta = 0.1,
              objective="multi:softprob",
              num_class=5,
              subsample = 0.9,
              min_child_weight = 800,
              colsample_bytree = 0.9,
              base_score =0.1
)
set.seed(123)
start_time <- Sys.time()
model_xgb2 <- xgb.train(param, train.xg, nthread = 16, nround = 1600,verbose = 1)
end_time <- Sys.time()
time_taken <- end_time - start_time

prediction1<-predict(model_xgb2,testing2, missing=NA)
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
probability_table2<-as.data.table((matrix(prediction1,nrow = 21207,byrow = T)))
p <- as.data.frame(probability_table2)
p[, "max1"] <- apply(p[, 1:5], 1, function(x)x[maxn(1)(x)])
p[, "pos1"] <- apply(p[, 1:5], 1, maxn(1))
# p[, "max2"] <- apply(p[, 1:5], 1, function(x)x[maxn(2)(x)])
# p[, "pos2"] <- apply(p[, 1:5], 1, maxn(2))
# p[, "max3"] <- apply(p[, 1:5], 1, function(x)x[maxn(3)(x)])
# p[, "pos3"] <- apply(p[, 1:5], 1, maxn(3))

#ANSWER GENERATION
p <- as.data.table(p)
p$ans <- p$pos1

#Optimizers
#p$ans[(p$max1-p$max2) <= 0.04 ] <- p$past[(p$max1-p$max2) <= 0.04 ]
# p$ans[p$max1 < 0.3 & p$max1-p$max2 <= p$max2-p$max3] <- p$pos2[p$max1 < 0.3 & p$max1-p$max2 <= p$max2-p$max3]
# p$ans[p$max1 < 0.3 & p$max1-p$max2 >= p$max2-p$max3] <- p$pos3[p$max1 < 0.3 & p$max1-p$max2 >= p$max2-p$max3]
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
sol_no_history <- copy(sol)
write.csv(sol,"Outlier_Solution_working_good_on_outliers.csv",row.names = FALSE)





###########################################################################
###########################################################################
###########################################################################
#To get ensemble of Outlier Removal Model
###########################################################################
###########################################################################
###########################################################################

#sol <- fread("best_attempt_after_tuning.csv")
sol$actual_vote <- NA
sol$actual_vote[which(pp$max1 - pp$max2 >= 0.3 )] <- sol_copy$actual_vote[which(pp$max1 - pp$max2 >= 0.3 )]
sol$actual_vote[which(is.na(sol$actual_vote ))] <- sol_no_history$actual_vote[which(is.na(sol$actual_vote))]

#FINAL ANSWER
write.csv(sol,"Best_Model_Solution_Without_Outliers.csv",row.names = FALSE)


#APPENDIX :

#sol is your solution from xgboost and neural net model
#a and b are column names respectively for citizenid and actual vote
#sol_copy is copy of sol
#sol_no_history is solution from outlier prediction model


