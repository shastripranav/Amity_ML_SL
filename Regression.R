dataset = `RELIANCE.NS.(4)`
dataset = dataset[-c(979),c(2,3,4,5)]
dataset = na.omit(dataset)

summary(dataset)
str(dataset)
dataset$Open = as.numeric(as.character(dataset$Open))
dataset$High = as.numeric(as.character(dataset$High))
dataset$Close = as.numeric(as.character(dataset$Close))
dataset$Low = as.numeric(as.character(dataset$Low))
str(dataset)
summary(dataset)


library(caTools)
set.seed(123)
split = sample.split(dataset$High,SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set =  subset(dataset, split == FALSE)

# Multi 
regressor = lm(formula = Close ~., 
               data = training_set)

y_pred = predict(regressor, newdata = test_set)

predict(regressor,newdata =  data.frame(Open = 2054 ))


library(Metrics)
error = rmse(test_set$Close,y_pred)
error
7.29358*100/2000





# Decision Tree Regression


library(rpart)
regressor = rpart(formula = Close ~., 
                  data = training_set,
                  control = rpart.control(minsplit = 1))

y_pred = predict(regressor, newdata = test_set)

error = rmse(test_set$Close,y_pred)
error*100/2000


# Random Forest Regression

library(randomForest)
set.seed(1234)
regressor =randomForest(x=dataset[-4],
                        y = dataset$Close,
                        ntree = 500)

y_pred = predict(regressor, newdata = test_set)

error = rmse(test_set$Close,y_pred)
error*100/2000

dataset[-4]
