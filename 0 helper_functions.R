predict.oneVsAll <- function(object, newX=object$info$X, ...) {
  stopifnot(class(object)=='oneVsAll')
  lapply(object$models, function(x) {
    predict(x, newX, ...)
  })
}

classify <- function(dat) {
  out <- dat/rowSums(dat)
  out$Class <- apply(dat, 1, function(x) names(dat)[which.max(x)])
  out
}

library(ada)
library(caret) 
X <- iris[,-5]
Y <- iris[,5]
myModels <- oneVsAll(X, Y, ada)
preds <- predict(myModels, X, type='probs')
preds <- data.frame(lapply(preds, function(x) x[,2])) #Make a data.frame of probs
preds <- classify(preds)
confusionMatrix(preds$Class, Y)

                           
oneVsAll <- function(X,Y,FUN,...) {
  models <- lapply(unique(Y), function(x) {
    name <- as.character(x)
    .Target <- factor(ifelse(Y==name,name,'other'), levels=c(name, 'other'))
    dat <- data.frame(.Target, X)
    model <- FUN(.Target~., data=dat, ...)
    return(model)
  })
  names(models) <- unique(Y)
  info <- list(X=X, Y=Y, classes=unique(Y))
  out <- list(models=models, info=info)
  class(out) <- 'oneVsAll'
  return(out)
}




library(MASS)
myModels <- oneVsAll(X, Y, lda)
preds <- predict(myModels, X)
preds <- data.frame(lapply(preds, function(x) x[[2]][,1])) #Make a data.frame of probs
preds <- classify(preds)
confusionMatrix(preds$Class, Y)
