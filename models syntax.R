############################################
#    Decision TREE
##########################################
library(party)
library(caret)
data("iris")
set.seed(12)
indexes = createDataPartition(iris$Species, p=.9, list=F)
train = iris[indexes,]
test = iris[-indexes,]
tmodel = ctree(formula=Species~ ., data= train)
print(tmodel)
plot(tmodel)
pred = predict(tmodel, test)
cm = confusionMatrix(test$Species, pred)
print(cm)
#####################################
library(rpart)
library(rattle)
fit <- rpart(Species ~ ., method = "class", data = train)
fancyRpartPlot(fit)

#############################
library(rpart)
tr <- rpart(Species ~ ., method = "class", data = train)
library(rpart.plot)
rpart.plot(tr)
rpart.plot(tr,extra=2)
#########################################
#         Random forest
#########################################
library(randomForest)
data(kyphosis)
fit <- randomForest(Kyphosis ~ Age + Number + Star, 
                    data =kyphosis, mtry=2, ntree =3)
print(fit)
plot(fit)
importance(fit)

