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

data <- read_parquet('../AMRData.parquet')[c(6, 7, 13)]

head(data)

data$order_month <- as.factor(data$order_month)
data$org_standard <- as.factor(data$org_standard)
data$species <- as.factor(data$species)

str(data)

set.seed(1)
index <- createDataPartition(data$org_standard, p = 0.6, list=FALSE)
train <- data[index, ]
test <- data[-index, ]


naive <- naiveBayes(org_standard~., data=train)

predict <- predict(naive, test[-1])
predictions<-as.data.frame(predict)

con.mat<-confusionMatrix(data=as.factor(predictions$predict), reference = test$org_standard)
print(con.mat)
