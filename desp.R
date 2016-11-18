aa <- fread("best.csv")

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



#Feature Generation

x$var1 <- as.integer((20*x$mvar1+10*x$mvar6+5*x$mvar11+4*x$mvar16+6*x$mvar21))
x$var2 <- as.integer((20*x$mvar2+10*x$mvar7+5*x$mvar12+4*x$mvar17+6*x$mvar22))
x$var3 <- as.integer((20*x$mvar3+10*x$mvar8+5*x$mvar13+4*x$mvar18+6*x$mvar23))
x$var4 <- as.integer((20*x$mvar4+10*x$mvar9+5*x$mvar14+4*x$mvar19+6*x$mvar24))
x$var5 <- as.integer((20*x$mvar5+10*x$mvar10+5*x$mvar15+4*x$mvar20+6*x$mvar25))




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

# 
# 
# table <- as.data.table(y)
# table$y1<- 0
# table$y2<- 0
# table$y3<- 0
# table$y4<- 0
# table$y5<- 0
# table$y1[table$y == 1 ] <- 1
# table$y2[table$y == 2 ] <- 1
# table$y3[table$y == 3 ] <- 1
# table$y4[table$y == 4 ] <- 1
# table$y5[table$y == 5 ] <- 1


# 
# x$var16 <- table$y1
# x$var17 <- table$y2
# x$var18 <- table$y3
# x$var19 <- table$y4
# x$var20 <- table$y5

# x$var16 <- (x$mvar33 - x$mvar32)
# x$var16[x$var16 != 0] <- 1

testing$var1 <- as.integer((20*testing$mvar1+10*testing$mvar6+5*testing$mvar11+4*testing$mvar16+6*testing$mvar21)/20)
testing$var2 <- as.integer((20*testing$mvar2+10*testing$mvar7+5*testing$mvar12+4*testing$mvar17+6*testing$mvar22)/20)
testing$var3 <- as.integer((20*testing$mvar3+10*testing$mvar8+5*testing$mvar13+4*testing$mvar18+6*testing$mvar23)/20)
testing$var4 <- as.integer((20*testing$mvar4+10*testing$mvar9+5*testing$mvar14+4*testing$mvar19+6*testing$mvar24)/20)
testing$var5 <- as.integer((20*testing$mvar5+10*testing$mvar10+5*testing$mvar15+4*testing$mvar20+6*testing$mvar25)/20)



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



train1 <- x[x$varr >= 480]
target1 <- y[x$varr >= 480]
train2 <- x[x$varr < 480 & x$varr >= 300]
target2 <- y[x$varr < 480 & x$varr >= 300]
train3 <- x[x$varr < 300]
target3 <- y[x$varr < 300]

testing$id <- test$citizen_id
test1 <- testing[testing$varr >= 28]
test2 <- testing[testing$varr < 28 & testing$varr >= 12]
test3 <- testing[testing$varr < 12]


test1_id <- test1$id
test2_id <- test2$id
test3_id <- test3$id











train1[,c(35:44,50,51):=NULL]

train2[,c(35:44,50,51):=NULL]

train3[,c(35:44,50,51):=NULL]

test1[,c(35:44,50,51):=NULL]

test2[,c(35:44,50,51):=NULL]

test3[,c(35:44,50,51):=NULL]


train1 <- as.matrix(train1)

train2 <- as.matrix(train2)

train3 <- as.matrix(train3)

test1 <- as.matrix(test1)

test2 <- as.matrix(test2)

test3 <- as.matrix(test3)

target1 <- as.matrix(target1)

target2 <- as.matrix(target2)

target3 <- as.matrix(target3)






#XGBOOST
train.xg = xgb.DMatrix(train3, label=target3, missing=NA)
test.xg = xgb.DMatrix(test3, missing=NA)






param <- list(max_depth = 12,
              eta = 0.15,
              objective="multi:softprob",
              num_class=6,
              # subsample = 0.75,
              min_child_weight = 500,
              colsample_bytree = 1,
              base_score =0
)
set.seed(123)

start_time <- Sys.time()

model_xgb2 <- xgb.train(param, train.xg, nthread = 16, nround = 1600,verbose = 1)

end_time <- Sys.time()
time_taken <- end_time - start_time




prediction3<-predict(model_xgb2,test3, missing=NA)

maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]



probability_table3<-as.data.table((matrix(prediction3,nrow = 3701,byrow = T)))
p <- as.data.frame(probability_table3)

p[, "max1"] <- apply(p[, 2:6], 1, function(x)x[maxn(1)(x)])
p[, "pos1"] <- apply(p[, 2:6], 1, maxn(1))
p[, "max2"] <- apply(p[, 2:6], 1, function(x)x[maxn(2)(x)])
p[, "pos2"] <- apply(p[, 2:6], 1, maxn(2))
p[, "max3"] <- apply(p[, 2:6], 1, function(x)x[maxn(3)(x)])
p[, "pos3"] <- apply(p[, 2:6], 1, maxn(3))

# p <- as.data.frame(probability_table)
# ans <- colnames(p)[max.col(p,ties.method="first")]
p <- as.data.table(p)
p$id <- test3_id

ans3 <- copy(p)

ans <- rbind(ans1,ans2,ans3)
p <- copy(ans)

id <- c(test1_id,test2_id,test3_id)
p$ans <- p$pos1
p$ans[p$max1 < 0.32] <- p$pos2[p$max1 < 0.32]




ans <- p$ans

ans <- as.data.table(ans)
ans$ans[ans$ans == 1 ] <- "Centaur"
ans$ans[ans$ans == 2 ] <- "Cosmos"
ans$ans[ans$ans == 3 ] <- "Ebony"
ans$ans[ans$ans == 4 ] <- "Odyssey"
ans$ans[ans$ans == 5 ] <- "Tokugawa"






test <- fread("test.csv")
sol <- id
sol <- as.data.table(sol)
sol$citizen_id <- sol$sol
sol[,sol:=NULL]
sol$actual_vote <- ans
write.csv(sol,"Triple2.csv",row.names = FALSE)






