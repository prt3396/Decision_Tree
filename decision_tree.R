library(tree)
library(dplyr)
attach(iris)
m <- tree(Species~.,data = iris)
summary(m)
##-------------plotting the data----------------
plot(m)
text(m,pretty = 0)
##-------------splitting the data---------------
set.seed(123)
s <- sample(1:nrow(iris),0.7*nrow(iris))
train <- iris[s,]
test <- iris[-s,]
##--------------creating model for train data----
m1 <- tree(Species~.,data = train)
summary(m1)
##---------------predicting for test data--------
pre <- predict(m1,newdata = test,type = "class")
table(pre,test$Species)
42/45
##---------------pruning the tree----------------
set.seed(111)
pruning <- cv.tree(m1,FUN = prune.misclass)
plot(pruning$size,pruning$dev,type = "b")
pr_m1 <- prune.misclass(m1,best = 3)
##---------------plotting the prune tree---------
plot(pr_m1)
text(pr_m1,pretty = 0)
##----------------predicting with pruned data----
pp <- predict(pr_m1,newdata = test,type = "class")
table(pp,test$Species)
44/45
