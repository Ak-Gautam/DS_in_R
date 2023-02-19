library(dplyr)
library(ggplot2)
library(caret)
library(datarium)

data("marketing", package = "datarium")

# Histogram of Sales
ggplot(marketing, aes(sales)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Sales", y = "Count") +
  theme_classic()

# Scatterplot of Sales vs. Youtube
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth() +
  labs(title = "Sales vs. Youtube")

# Split data into training and test sets
set.seed(123)
samples <-  createDataPartition(marketing$sales, p = 0.6, list = FALSE)
train <- marketing[training.samples,]
test <- marketing[-training.samples,]

# Fit linear regression model and make predictions
model <- lm(sales ~ youtube, data = train)
predictions <- predict(model, newdata = test)

# Compute evaluation metrics
rmse <- RMSE(predictions, test$sales)
r2 <- R2(predictions, test$sales)

# Make predictions for new data
newdata <- data.frame(youtube = c(0, 1000))
predictions_new <- predict(model, newdata = newdata)