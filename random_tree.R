library(MASS)
data(package="MASS")
boston<-Boston
dim(boston)
names(boston)
View(boston)
install.packages("randomForest")
require(randomForest)
set.seed(101)
train = sample(1:nrow(boston), 300)
rf.boston = randomForest(medv~., data = boston, subset = train)
rf.boston
oob.err = double(13)
test.err = double(13)
for(mtry in 1:13){
  fit = randomForest(medv~., data = boston, subset=train, mtry=mtry,
                     ntree = 350)
  oob.err[mtry] = fit$mse[350]
  pred = predict(fit, boston[-train,])
  test.err[mtry] = with(boston[-train,], mean( (medv-pred)^2 ))
}
matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red",
                                                            "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red",
                                                                "blue"))
