# 5.r
# Load the dplyr package
library(tidyverse)

data("iris")
iris2 = as.data.frame(scale(iris[,-5]))
kmeans_fun <- function(k) {
  kmeans(iris2, k, nstart = 50)$tot.withinss
}

ws <- sapply(2:15, kmeans_fun)
plot(2:15, ws, type = "b", main = "Elbow Method",
     xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")
iris_kmeans<-kmeans(iris2,7)
iris$clstr<-iris_kmeans$cluster
table(iris$Species,iris$clstr)
iris$clstr <- as.factor(iris$clstr)
ggplot(iris, aes(Petal.Length, Petal.Width, color = clstr)) + geom_point()