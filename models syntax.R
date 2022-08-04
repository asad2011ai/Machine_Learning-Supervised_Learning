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
#############################################
#         Naive Bayes
#############################################
library(e1071)
data('Titanic')

df <- as.data.frame(Titanic)
str(df)

repeating_seq <- rep.int(seq_len(nrow(df)),df$Freq)

Titanic_dataset <- df[repeating_seq,]

#Drop frequency column

Titanic_dataset$Freq = NULL
str(Titanic_dataset)
# create model
nb_model <- naiveBayes(Survived ~ ., data = Titanic_dataset)

# model result
nb_model

# prediction
nb_prediction <- predict(nb_model, Titanic_dataset)

# confusion matrix
table(nb_prediction, Titanic_dataset$Survived)

##################################################
# using naive bayes with mlr library
##################################################
install.packages('mlr')
library(mlr)

task <- makeClassifTask(data = Titanic_dataset, target = 'Survived')
# initalize classifier
select_model <- makeLearner("classif.naiveBayes")

# train mdoel
nb_mlr <- train(select_model, task)

# read the model learned
nb_mlr$learner.model

# prediction
pred_mlr <- as.data.frame(predict(nb_mlr, newdata = Titanic_dataset[,1:3]))

# confusion matrics

table(pred_mlr[,1],Titanic_dataset$Survived)

#################################################
#   naive bayes example on blench
#################################################

data(HouseVotes84, package = 'mlbench')

model <- naiveBayes(Class ~ ., data= HouseVotes84)
predict(model, HouseVotes84[1:10,])
predict(model, HouseVotes84[1:10,], type= "raw")


pred <- predict(model, HouseVotes84)
table(pred, HouseVotes84$Class)

# using lablace smoothing
model <- naiveBayes(Class ~ ., data = HouseVotes84, lablace=3)
pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)
##########################################################
#                      ANN
##########################################################

library(MASS)
?Boston
data <- Boston
str(data)
# normalization
# (x - min)/ max - min

maxv <- apply(data, 2, max)
minv <- apply(data, 2, min)
maxv

train <- as.data.frame(scale(data, center = minv, scale = maxv - minv))
str(train)

#install.packages('neuralnet')
library(neuralnet)
n <- names(train)
f <- as.formula("medv ~ rm+crim+lstat")
nn <- neuralnet(formula = f, data = train, hidden = c(10,5,2), linear.output = T)
 
plot(nn) 

######################################################
# Evaluation method
######################################################
install.packages("tidyverse")
library(tidyverse)
library(caret)

# load data
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2)

# inspect the data
sample_n(pima.data, 3)
# split
set.seed(123)
training.samples <- pima.data$diabetes %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data <- pima.data[training.samples,]
test.data <- pima.data[-training.samples,]

install.packages("MASS")
library(MASS)

# fit lda model

fit <- lda(diabetes~., data = train.data)
# make prediction 

prediction <- predict(fit, test.data)
prediction.probability <- prediction$posterior[,2]
predicted.classes <- prediction$class
observed.classes <- test.data$diabetes


accuracy <- mean(observed.classes == predicted.classes)
accuracy
error <- mean(observed.classes != predicted.classes)
error
# confusion matrix
table(observed.classes, predicted.classes)
# confusion matrix with proportion 
table(observed.classes, predicted.classes) %>%
  prop.table() %>% round(digits = 3)

# confusion matrix from caret library
confusionMatrix(predicted.classes, observed.classes, positive = "pos")

# ROC curve


library(pROC)
res.roc <- roc(observed.classes, prediction.probability)
plot.roc(res.roc, print.auc = TRUE)

# extract region of interest

roc.data <- data_frame(
  thresholds = res.roc$thresholds,
  sensitivity = res.roc$sensitivities,
  specificity = res.roc$specificities
)

# get the probability threshold for specificity =0.6

plot.roc(res.roc,print.auc = TRUE, print.thres = "best")
plot.roc(res.roc, print.thres = c(0.3,0.5,0.7))
