library(tidyverse)
library(recommenderlab)
library(reshape2)
df_data = read.csv('/kaggle/input/ecommerce-data/data.csv')
df_data$InvoiceDate = as.Date(df_data$InvoiceDate)

df_data = df_data[df_data$Quantity > 0 & df_data$UnitPrice > 0,]
df_train = as.matrix(dcast(df_data, CustomerID ~ StockCode, value.var = 'Quantity', fun.aggregate = sum, fill=0))
df_train = df_train[rowSums(df_train) > 5, colSums(df_train) > 5]
df_train = as(df_train, "realRatingMatrix")
df_train = binarize(df_train, minRating = 1)

which_train = sample(c(TRUE, FALSE), nrow(df_train), replace = TRUE, prob = c(0.8, 0.2))
x = df_train[which_train, ]
y = df_train[!which_train, ]

recc_model = Recommender(data = x, method = 'IBCF', parameter = list(method = 'Jaccard'))
recc_predicted = predict(recc_model, newdata = y, n = 5, type = "topNList")
first_user_id = names(recc_predicted@items)[1]
recc_list = as(recc_predicted, "list")[[first_user_id]]
unique(df_data[df_data$StockCode %in% recc_list, c("StockCode", "Description")])