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




train <- na.omit(train)
train_id <- train$citizen_id

test[is.na(test)] <- 0
test$mvar30[test$mvar30=="0"]<- "Degree"
test$mvar30[test$mvar30==""]<- "Diploma"


#SVM



x <- subset(train, select=-actual_vote)
w <- subset(test)
# y <- train$actual_vote
# y<-as.factor(y)

# test$actual_vote <- "Ebony"
x <- rbind(test,x)
x <- as.data.table(x)
x <-x[,colnames(x)[c(1)]:= NULL,with=F]

xx <- as.data.table(c(x$party_voted_past,train$actual_vote))
xx$V1 <- as.numeric(as.factor(xx$V1))
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
x <- as.matrix(x)

#obj <- tune.svm(x,y, gamma =  seq(.5, .9, by = .1), cost = seq(100,1000, by = 100))


model_svm <- svm(x,y,type = "C-classification", gamma = 0.6 , cost = 400)


pred = predict(model_svm, testing)






test <- fread("test.csv")
sol <- test$citizen_id
sol <- as.data.table(sol)
sol$citizen_id <- sol$sol
sol[,sol:=NULL]
sol$actual_vote <- pred
write.csv(sol,"StockUp_IITKGP_32_svm64.csv",row.names = FALSE)





a <- as.data.table(a)
ans$ans[ans$ans == 1 ] <- "Centaur"
ans$ans[ans$ans == 2 ] <- "Cosmos"
ans$ans[ans$ans == 3 ] <- "Ebony"
ans$ans[ans$ans == 4 ] <- "Odyssey"
ans$ans[ans$ans == 5 ] <- "Tokugawa"



model_svm <- svm(x,y,type = "C-classification", gamma = 0.6 , cost = 600)


pred = predict(model_svm, testing)






test <- fread("test.csv")
sol <- test$citizen_id
sol <- as.data.table(sol)
sol$citizen_id <- sol$sol
sol[,sol:=NULL]
sol$actual_vote <- pred
write.csv(sol,"StockUp_IITKGP_32_svm66.csv",row.names = FALSE)

#KNN



