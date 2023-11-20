###################
# Clear Memory
###################

remove(list=ls())

###################
# Load Packages
###################
library(e1071)
library(arrow)
library(caret)
library(dplyr)

# Read Data

data <- read_parquet('AMRData.parquet')[c(3, 6, 7, 12, 13)]
#data <- read_parquet('AMRData.parquet')[c(6, 7, 13)]
data <- na.omit(data)
head(data)

data$county <- as.factor(data$county)
data$order_month <- as.factor(data$order_month)
data$org_standard <- as.factor(data$org_standard)
data$species <- as.factor(data$species)
data$source <- as.factor(data$source)

str(data)

set.seed(1)
index <- createDataPartition(data$org_standard, p = 0.7, list=FALSE)
train <- data[index, ]
test <- data[-index, ]

# Naive Bayes Model

naive <- naiveBayes(org_standard~., data=train)

predict <- predict(naive, test)
predictions<-as.data.frame(predict)

con.mat<-confusionMatrix(data=as.factor(predictions$predict), reference = test$org_standard)
print(con.mat)

# Example test run

example_input <- data.frame(
  county = "New York",
  order_month = "5",
  species = "CANINE",
  source = "CYSTOCENTESIS"
)
example <- predict(naive, example_input, "raw")
example <- as.data.frame(example)

selected_columns <- example %>%
  select_if(~all(.>=0.01))

other <- c(1 - rowSums(selected_columns))
selected_columns <- cbind(selected_columns, other)
cls <- colnames(selected_columns)
selected_columns <- as.matrix(selected_columns)
pie(selected_columns, labels = cls, col = rainbow(length(cls)))







