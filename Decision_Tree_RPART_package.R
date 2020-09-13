##-------------loading the library-----------------------------------
library(rpart)
library(rpart.plot)
attach(iris)
##------------creating the model-------------------------------------
tree <- rpart(Species~.,data = train,method = "class")
rpart.plot(tree,nn=TRUE)
summary(tree)
##------------predicting model with test data------------------------
predd <- predict(object = tree,test[-5],type = "class")
##-----------confusion matrix----------------------------------------
t <- table(test$Species,predd)
library(caret)
confusionMatrix(t)
##------------pruning the tree---------------------------------------
set.seed(111)
pr <- cv.tree(tree,FUN =prune.rpart)
printcp(tree)
plotcp(tree)
ptree <- prune(tree,cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
plot(ptree,uniform = TRUE,main="pruned tree")
text(ptree,pretty = 0)
