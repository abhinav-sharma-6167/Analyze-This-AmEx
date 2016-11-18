#p <- fread("new_probability.csv")
p <- fread("prob_abhi.csv")
# sol <- fread("new21.csv")
sol <- fread("best_one_attempt.csv")
#sol2 <- fread("new15.csv")
sol_copy <- copy(sol)
#sol2 <- fread("StockUp_IITKGP_200.csv")
sol_no_history <- fread("outlier6.csv")



sol$b <- NA
sol$b[which(p$max1 - p$max2 >= 0.30)] <- sol_copy$b[which(p$max1 - p$max2 >= 0.30 )]
#sol$b[which(p$max1 - p$max2 < 0.25 & p$max1 - p$max2 >= 0.2)] <- sol_no_history$b[(p$max1 - p$max2 < 0.25 & p$max1 - p$max2 >= 0.2)]
sol$b[which(is.na(sol$b ))] <- sol_no_history$b[which(is.na(sol$b))]
write.csv(sol,"Solution30.csv",row.names = FALSE)
#probability_table <- p[,c(6:11):= NULL]
















prediction1<-p$prediction_matrix
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
write.csv(sol,"StockUp_IITKGP_270.csv",row.names = FALSE)

