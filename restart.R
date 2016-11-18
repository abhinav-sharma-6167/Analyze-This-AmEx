library(dplyr)
x$hot1 <- 0
x$hot2 <- 0
x$hot3 <- 0
x$hot4 <- 0
x$hot1[x$mvar30 == 1] <- 1
x$hot2[x$mvar30 == 2] <- 1
x$hot3[x$mvar30 == 3] <- 1
x$hot4[x$mvar30 == 4] <- 1
x[,mvar30:=NULL]

testing$hot1 <- 0
testing$hot2 <- 0
testing$hot3 <- 0
testing$hot4 <- 0
testing$hot1[testing$mvar30 == 1] <- 1
testing$hot2[testing$mvar30 == 2] <- 1
testing$hot3[testing$mvar30 == 3] <- 1
testing$hot4[testing$mvar30 == 4] <- 1
testing[,mvar30:=NULL]


x2 <- copy(x)
testing2<- copy(testing)
x2[,c(1,2,40:44,50:54,56,59:61):=NULL]
testing2[,c(1,2,40:44,50:55,58:60):=NULL]
x2$target <- y

x3 <- sample_n(x2,12000)
x2 <- x2[which(train$citizen_id %in% train_hot$citizen_id)]
x2 <- rbind(x2,x3)
yy <- x2$target
x2[,target:=NULL]
x2 <- as.matrix(x2)
testing2 <- as.matrix(testing2)
# 
# x2 <- data.Normalization(x2,type="n4",normalization="column")
# testing2 <- data.Normalization (testing2,type="n4",normalization="column")


# 
# x$target1 <- 0
# x$target2 <- 0
# x$target3 <- 0
# x$target4 <- 0
# x$target5 <- 0
# x$target1[y==0] <- 1
# x$target2[y==1] <- 1
# x$target3[y==2] <- 1
# x$target4[y==3] <- 1
# x$target5[y==4] <- 1

#XGBOOST
#y[which(train$citizen_id %in% train_hot$citizen_id)]
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

prediction1<-predict(model_xgb2,testing2, missing=NA)
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
probability_table2<-as.data.table((matrix(prediction1,nrow = 21207,byrow = T)))
p <- as.data.frame(probability_table2)
p[, "max1"] <- apply(p[, 1:5], 1, function(x)x[maxn(1)(x)])
p[, "pos1"] <- apply(p[, 1:5], 1, maxn(1))
p[, "max2"] <- apply(p[, 1:5], 1, function(x)x[maxn(2)(x)])
p[, "pos2"] <- apply(p[, 1:5], 1, maxn(2))
p[, "max3"] <- apply(p[, 1:5], 1, function(x)x[maxn(3)(x)])
p[, "pos3"] <- apply(p[, 1:5], 1, maxn(3))



#ANSWER GENERATION
p <- as.data.table(p)
p$sum2 <- p$max1 + p$max2
p$sum3 <- p$max1 + p$max2 + p$max3
p$past <- testing$party_voted_past

p$ans <- p$pos1


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
write.csv(sol,"StockUp_IITKGP_160.csv",row.names = FALSE)
# 
# importance_matrix <- xgb.importance(train$data@Dimnames[[2]], model = bst)
# xgb.plot.importance(importance_matrix)



#NEURAL NET
library(neuralnet)
library(nnet)
x2 <- as.data.table(x2)
testing2 <- as.data.table(testing2)
x2$target <- y
#x2 <- as.matrix(x2)
n <- names(x2)
#model_net <- neuralnet( f , data =  x2, hidden = 1, lifesign = "minimal",linear.output = FALSE, threshold = 0.25, stepmax = 1e6)
x2[,c(1):=NULL]
testing2[,c(1):=NULL]
f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))

model_net2 <-multinom(target~., data = x2, maxit = 90)
#plot(model_net2, rep = "best")
net_result <- predict(model_net2, testing2, "probs")
#net_result2 <- predict(model_net2, testing2, "raw")
prediction1 <- net_result
# prediction1 <- as.data.table(prediction1)
# prediction1$V1 <- round(prediction1$V1 )
# 
# prediction2 <- prediction(model_net)


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
write.csv(sol,"multinom1.csv",row.names = FALSE)

