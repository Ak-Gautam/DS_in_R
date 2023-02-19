library(tidyverse)
library(caret)
library(datarium)

# Load data
data("marketing", package = "datarium")
glimpse(marketing)

# Create histogram
ggplot(marketing, aes(sales)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Sales", y = "Count") +
  theme_classic()

# Create scatterplot with smoothed line
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  geom_smooth()

# Fit multiple linear regression model
set.seed(123)
samples <- createDataPartition(marketing$sales, p = 0.8, list = FALSE)
train <- marketing[samples,]
test <- marketing[-samples,]

model <- lm(sales ~ ., data = train)
summary(model)$coef

# Make predictions and evaluate model performance
predictions <- predict(model, newdata = test)
RMSE(predictions, test$sales)
R2(predictions, test$sales)

newdata <- data.frame(youtube=2000, facebook=1000, newspaper=1000)
predictions <- predict(model, newdata = newdata)
predictions
