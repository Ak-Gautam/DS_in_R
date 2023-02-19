library(dplyr)
library(ggplot2)
library(corrplot)
​
cars_multi <- read.csv("/Cars/cars_multi.csv")
cars_price <- read.csv("/Cars/cars_price.csv")
​
# Merge cars_multi and cars_price by ID
cars <- left_join(cars_multi, cars_price, by = "ID")
​
# Sum of missing values in cars
sum(!complete.cases(cars))
​
# Basic summary of cars
summary(cars)
str(cars)
​
# Histogram of mpg
ggplot(cars, aes(mpg)) + geom_histogram(binwidth = 5) +
  labs(title = "Histogram of MPG", y = "Count") + theme_classic()
​
# Bar plot of cylinders
ggplot(cars, aes(cylinders)) + geom_bar() +
  labs(title = "Cylinders", y = "Count") + theme_classic()
​
# Box plot of displacement
boxplot(cars$displacement, main="Box Plot Displacement",
        ylab="Displacement")
​
# Count of "?" values in horsepower
count(cars[as.character(cars$horsepower) == "?",])
​
# Histogram of weight
ggplot(cars, aes(weight)) + geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Weight", y = "Count") + theme_classic()
​
# Density plot of acceleration
ggplot(cars, aes(acceleration)) + geom_density() +
  labs(title = "Density of Weight") + theme_classic()
​
# Bar plot of car models
to_Plot <- as.data.frame(table(cars$model))
colnames(to_Plot) <- c("Model", "Frequency")
ggplot(to_Plot, aes(x = Model, y = Frequency)) + geom_bar(stat = "identity") +
  labs(title = "Model") + theme_classic()
​
# Bar plot of origin
ggplot(cars, aes(origin)) + geom_bar() +
  labs(title = "Origin", y = "Count") + theme_classic()
​
# Box plot of price
cars$price <- as.integer(cars$price)
boxplot(cars$price, main="Price BoxPLot", ylab="Price")
​
# Bar plot of common price
to_Plot <- as.data.frame(table(cars$price))
colnames(to_Plot) <- c("Price", "Frequency")
ggplot(head(to_Plot[ order(-to_Plot[,2]), ]), aes(x = reorder(Price, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") + labs(title = "Common Price", x = "Price") +
  theme_classic() + coord_flip()
​
# Remove missing values from cars and drop ID column
cars <- cars %>% mutate(horsepower = as.numeric(as.character(horsepower)))
cars <- cars[complete.cases(cars), -1]
​
# Plot correlation matrix
nums <- sapply(cars, is.numeric)
correlations <- cor(cars[,nums])
corrplot(correlations, order = "hclust")