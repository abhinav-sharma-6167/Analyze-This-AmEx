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

# 
# probability_table <- readRDS("prob.rds")
# 
# testing$var16 <- probability_table$V2
# testing$var17 <- probability_table$V3
# testing$var18 <- probability_table$V4
# testing$var19 <- probability_table$V5
# testing$var20 <- probability_table$V6
# 
# testing$var16 <- testing$mvar33 - testing$mvar32
# testing$var16[testing$var16 != 0] <- 1









x[,c(2:34,40:44):=NULL]
testing[,c(2:34,40:44):=NULL]
x <- as.matrix(x)



testing <- as.matrix(testing)


#XGBOOST
train.xg = xgb.DMatrix(x, label=y, missing=NA)
test.xg = xgb.DMatrix(testing, missing=NA)

# 
# library(Metrics)
# map3 <- function(preds, dtrain)
#   #function over prediction and xgboost object
# {
#   labels <- as.list(getinfo(dtrain,"label"))
#   #why are we getting labels??okays for mapk..but what are these labels..cant see inside test.xg
#   num.class = 6
#   #so we need to use the entire probability table for preds 
#   pred <- matrix(preds, nrow = num.class)
#   top <- t(apply(pred,2,  function(y) order(y)[num.class:(num.class-2)]-1))
#   #t is for transpose, apply for loop rowwise, function to get best 5 choices
#   top1 <- split(top, 1:NROW(top))
#   
#   map <- mapk(3, labels, top1)
#   return(list(metric = "map3", value = map))
# }


param <- list(max_depth = 10,
              eta = 0.1,
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




prediction1<-predict(model_xgb2,testing, missing=NA)

maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]



probability_table<-as.data.table((matrix(prediction1,nrow = 21207,byrow = T)))










p <- as.data.frame(probability_table)

p[, "max1"] <- apply(p[, 2:6], 1, function(x)x[maxn(1)(x)])
p[, "pos1"] <- apply(p[, 2:6], 1, maxn(1))
p[, "max2"] <- apply(p[, 2:6], 1, function(x)x[maxn(2)(x)])
p[, "pos2"] <- apply(p[, 2:6], 1, maxn(2))
p[, "max3"] <- apply(p[, 2:6], 1, function(x)x[maxn(3)(x)])
p[, "pos3"] <- apply(p[, 2:6], 1, maxn(3))

# p <- as.data.frame(probability_table)
# ans <- colnames(p)[max.col(p,ties.method="first")]
p <- as.data.table(p)


p$ans <- p$pos1
#p$ans[p$max1 < 0.4] <- p$pos2[p$max1 < 0.4]




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
sol$actual_vote <- ans
write.csv(sol,"StockUp_IITKGP_33_xgb2ice_small.csv",row.names = FALSE)












p$ans <- p$pos1
p$ans[p$max1 < 0.4] <- p$pos2[p$max1 < 0.4]




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
sol$actual_vote <- ans
write.csv(sol,"StockUp_IITKGP_33_xgb2ice2_small.csv",row.names = FALSE)



