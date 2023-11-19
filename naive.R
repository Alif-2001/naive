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

# Read Data
data <- read_parquet('AMRData.parquet')
head(data)


data <- read_parquet('AMRData.parquet')[c(3, 6, 7, 12, 13, 14)]

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


naive <- naiveBayes(org_standard~., data=train)
predict <- predict(naive, test)
predictions<-as.data.frame(predict)

con.mat<-confusionMatrix(data=as.factor(predictions$predict), reference = test$org_standard)
print(con.mat)

example_input <- c("New York", "1", "CANINE", "ABDOMINAL", "3")
example <- predict(naive, example_input, "raw")

head(example)
