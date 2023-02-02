#6.r
library(naivebayes)
library(caret)
data(iris)
set.seed(123)
indices = sample(1:nrow(iris), 0.8*nrow(iris))
train = iris[indices,]
test = iris[-indices,]
nb_model = naive_bayes(Species~., data = train)
pred = predict(nb_model, newdata = test)
cm = confusionMatrix(test$Species, pred)
cm