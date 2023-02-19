library(class)
library(caret)

# Load data
loan <- read.csv("/kaggle/input/credit/credit_data.csv")
loan.subset <- loan[c('Creditability', 'Age..years.', 'Sex...Marital.Status', 'Occupation', 'Account.Balance', 'Credit.Amount', 'Length.of.current.employment', 'Purpose')]

# Normalize data
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
loan.subset.n <- as.data.frame(lapply(loan.subset[,2:8], normalize))

# Split data into training and test sets
set.seed(123)
train.index <- createDataPartition(loan$Creditability, p = 0.7, list = FALSE)
train.loan <- loan.subset.n[train.index, ]
test.loan <- loan.subset.n[-train.index, ]
train.loan_labels <- loan.subset[train.index, 1]
test.loan_labels <- loan.subset[-train.index, 1]

# Perform k-NN classification and find optimal k value
k.optm <- numeric(28)
for (i in 1:28) {
  knn.mod <- knn(train = train.loan, test = test.loan, cl = train.loan_labels, k = i)
  k.optm[i] <- 100 * sum(test.loan_labels == knn.mod) / NROW(test.loan_labels)
}

# Print accuracy values and plot k-NN accuracy levels
ACC.26 <- 100 * sum(test.loan_labels == knn(train = train.loan, test = test.loan, cl = train.loan_labels, k = 26)) / NROW(test.loan_labels)
ACC.27 <- 100 * sum(test.loan_labels == knn(train = train.loan, test = test.loan, cl = train.loan_labels, k = 27)) / NROW(test.loan_labels)

cat("Accuracy (k = 26):", ACC.26, "\n")
cat("Accuracy (k = 27):", ACC.27, "\n")

plot(k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy Level")
