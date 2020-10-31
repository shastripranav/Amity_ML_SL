# Logistic Regression

dataset = read.csv("Social_Network_Ads.csv")
dataset = dataset[3:5]

str(dataset)

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

str(dataset)
 
#Splitting the dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set =  subset(dataset, split == FALSE)


# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

summary(training_set)

#Logistic regression Classifier
classifier = glm(formula = Purchased~.,
                 family =binomial,
                 data = training_set)

y_pred = predict(classifier, type = 'response',newdata = test_set[-3])
y_pred = ifelse(y_pred>0.5,1,0)

cm  = table(test_set[,3],y_pred)
cm


# kNN Classifier
library(class)

y_pred = knn(train = training_set[,-3],
             test = test_set[,-3],
             cl= training_set[,3],
             k = 5,
             prob = TRUE)
  
cm  = table(test_set[,3],y_pred)
cm

## Decision Tree Regression


library(rpart)
classifier = rpart(formula = Purchased ~., 
                  data = training_set,)


y_pred = predict(classifier, type = 'class',newdata = test_set[-3])
#y_pred = ifelse(y_pred>0.5,1,0)

cm  = table(test_set[,3],y_pred)
cm


# Random Forest Classification

library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 1000)

y_pred = predict(classifier, newdata = test_set[-3])
#y_pred = ifelse(y_pred>0.5,1,0)

cm  = table(test_set[,3],y_pred)
cm
