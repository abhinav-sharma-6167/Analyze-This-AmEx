x[,c(35:44):=NULL]
testing[,c(35:44):=NULL]
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
write.csv(sol,"StockUp_IITKGP_26.csv",row.names = FALSE)


