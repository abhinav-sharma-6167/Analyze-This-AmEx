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

#Seed & Read
set.seed(100)
train <- fread("train.csv")
test <- fread("test.csv")
test_all <- fread("test_all.csv")

train2 <- copy(train)
test2 <- copy(test)



#Going all factor
#DT1
train <- na.omit(train)
train_id <- train$citizen_id
train <-train[,colnames(train)[c(1,34,35)]:= NULL,with=F]

#fit <- rpart(actual_vote ~ party_voted_past + mvar1 +mvar2 +mvar3 +mvar4 +mvar5 +mvar6 +mvar7 +mvar8 +mvar9 +mvar10 +mvar11 +mvar12 +mvar13 +mvar14+mvar15 +mvar16 +mvar17 +mvar18 +mvar19 +mvar20 +mvar21 +mvar22 +mvar23 +mvar24 +mvar25 +mvar26 +mvar27 +mvar28 + mvar29 +mvar30 +mvar31, data=train, method="class")
test[is.na(test)] <- 0
test$mvar30[test$mvar30=="0"]<- "Degree"
test$mvar30[test$mvar30==""]<- "Diploma"
#Prediction <- predict(fit, test, type = "class")


#RF


library(randomForest)
set.seed(100)
train$party_voted_past <- as.factor(train$party_voted_past)
train$actual_vote <- as.factor(train$actual_vote)
# train$mvar27 <- as.factor(train$mvar27)
# train$mvar30 <- as.factor(train$mvar30)
train <- as.data.frame(train)
train[] <- lapply(train, factor)
train <- as.data.table(train)


start_time <- Sys.time()

set.seed(123)



model_rf <- randomForest(as.factor(actual_vote) ~ .,data=train, importance=TRUE,ntree =1000, mtry=3 , do.trace=TRUE )

end_time <- Sys.time()
time_taken <- end_time - start_time



test$mvar8[test$mvar8 > 17] <- 17
test$mvar18[test$mvar18 > 15] <- 15
test$mvar19[test$mvar19 > 10] <- 10
test$var3[test$var3 > 34] <- 34

test$party_voted_past <- as.factor(test$party_voted_past)
#test$actual_vote <- as.factor(test$actual_vote)
# test$mvar27 <- as.factor(test$mvar27)
# test$mvar30 <- as.factor(test$mvar30)
test <- as.data.frame(test)
test[] <- lapply(test, factor)
test <- as.data.table(test)

test <- test[,colnames(test)[c(1,34,35)]:= NULL,with=F]


Prediction4 <- predict(model_rf, test, type = "class")




target <- train$actual_vote
train[,actual_vote:=NULL]





levels(test$party_voted_past) <- levels(train$party_voted_past)
levels(test$mvar1) <- levels(train$mvar1)
levels(test$mvar2) <- levels(train$mvar2)
levels(test$mvar3) <- levels(train$mvar3)
levels(test$mvar4) <- levels(train$mvar4)
levels(test$mvar5) <- levels(train$mvar5)
levels(test$mvar6) <- levels(train$mvar6)
levels(test$mvar7) <- levels(train$mvar7)
levels(test$mvar8) <- levels(train$mvar8)
levels(test$mvar9) <- levels(train$mvar9)
levels(test$mvar10) <- levels(train$mvar10)
levels(test$mvar11) <- levels(train$mvar11)
levels(test$mvar12) <- levels(train$mvar12)
levels(test$mvar13) <- levels(train$mvar13)
levels(test$mvar14) <- levels(train$mvar14)
levels(test$mvar15) <- levels(train$mvar15)
levels(test$mvar16) <- levels(train$mvar16)
levels(test$mvar17) <- levels(train$mvar17)
levels(test$mvar18) <- levels(train$mvar18)
levels(test$mvar19) <- levels(train$mvar19)
levels(test$mvar20) <- levels(train$mvar20)
levels(test$mvar21) <- levels(train$mvar21)
levels(test$mvar22) <- levels(train$mvar22)
levels(test$mvar23) <- levels(train$mvar23)
levels(test$mvar24) <- levels(train$mvar24)
levels(test$mvar25) <- levels(train$mvar25)
levels(test$mvar26) <- levels(train$mvar26)
levels(test$mvar27) <- levels(train$mvar27)
levels(test$mvar28) <- levels(train$mvar28)
levels(test$mvar29) <- levels(train$mvar29)
levels(test$mvar30) <- levels(train$mvar30)
levels(test$mvar31) <- levels(train$mvar31)






#SVM
x <- subset(train, select=-actual_vote)
y <- train$actual_vote
svm_model <- svm(x,y)


#Feature Generation

train$var1 <- as.integer((20*train$mvar1+10*train$mvar6+5*train$mvar11+4*train$mvar16+6*train$mvar21)/20)
train$var2 <- as.integer((20*train$mvar2+10*train$mvar7+5*train$mvar12+4*train$mvar17+6*train$mvar22)/20)
train$var3 <- as.integer((20*train$mvar3+10*train$mvar8+5*train$mvar13+4*train$mvar18+6*train$mvar23)/20)
train$var4 <- as.integer((20*train$mvar4+10*train$mvar9+5*train$mvar14+4*train$mvar19+6*train$mvar24)/20)
train$var5 <- as.integer((20*train$mvar5+10*train$mvar10+5*train$mvar15+4*train$mvar20+6*train$mvar25)/20)


#train <-train[,colnames(train)[-c(1,27:38)]:= NULL,with=F]


fit2 <- rpart(actual_vote ~., data=train, method="class")

test[is.na(test)] <- 0
test$mvar30[test$mvar30=="0"]<- "Degree"
test$mvar30[test$mvar30==""]<- "Diploma"


test$var1 <- as.integer((20*test$mvar1+10*test$mvar6+5*test$mvar11+4*test$mvar16+6*test$mvar21)/20)
test$var2 <- as.integer((20*test$mvar2+10*test$mvar7+5*test$mvar12+4*test$mvar17+6*test$mvar22)/20)
test$var3 <- as.integer((20*test$mvar3+10*test$mvar8+5*test$mvar13+4*test$mvar18+6*test$mvar23)/20)
test$var4 <- as.integer((20*test$mvar4+10*test$mvar9+5*test$mvar14+4*test$mvar19+6*test$mvar24)/20)
test$var5 <- as.integer((20*test$mvar5+10*test$mvar10+5*test$mvar15+4*test$mvar20+6*test$mvar25)/20)


test <-test[,colnames(test)[-c(2,28:33,36:40)]:= NULL,with=F]



#Prediction2 <- predict(fit2, test, type = "class")






test <- fread("test.csv")
sol <- test$citizen_id
sol <- as.data.table(sol)
sol$citizen_id <- sol$sol
sol[,sol:=NULL]
sol$actual_vote <- Prediction4
write.csv(sol,"StockUp_IITKGP_7.csv",row.names = FALSE)


#Plot DT1

library(rpart.plot)
#new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(fit)
