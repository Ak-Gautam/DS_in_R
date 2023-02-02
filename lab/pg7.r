#7.r
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

car_df = read.csv("/kaggle/input/car-data/car.data", sep = ',', header = FALSE)
head(car_df)
set.seed(123)
intrain = createDataPartition(y = car_df$V7, p= 0.7, list = FALSE)
train = car_df[intrain,]
test = car_df[-intrain,]
dim(train)
car_dt_model = rpart(V7 ~ ., 
                      data = train, method = "class")
#plot
prp(car_dt_model)

car_dt_predictions = predict(car_dt_model, newdata = test, type = "class")
confusionMatrix(as.factor(test$V7), car_dt_predictions)