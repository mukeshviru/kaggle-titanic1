# Load libraries
library(randomForest)

# Load the train dataset111
train_data <- read.csv("train.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))
View(train_data)
summary(train_data)

# Load the test dataset
test_data <- read.csv("test.csv",stringsAsFactors = FALSE,na.strings = c("","NA"))
View(test_data)
str(test_data)

################# Data Cleaning and Understanding ##################
# Check NA Values
sapply(train_data, function(x) sum(is.na(x)))
## There are 177 rows in Age column having NA values, 2 records in Embarked and 687 records in Cabin

sapply(test_data, function(x) sum(is.na(x)))
## There are 86 rows in Age column having NA values, 1 row in Fare column having NA value and 327 records in Cabin

## Since we will be using age/Emabrked in our prediction, we cannot remove the rows having NA.
## Hence we will be replace it with average value or maximum occurence.
# Replacing the NA values with average in train as well as test data for Age
train_data$Age[which(is.na(train_data$Age))] <- round(mean(train_data$Age,na.rm = TRUE), digits = 1)
test_data$Age[which(is.na(test_data$Age))] <- round(mean(test_data$Age,na.rm = TRUE),digits = 1)

train_data$Embarked[which(is.na(train_data$Embarked))] <- "S"
test_data$Fare[which(is.na(test_data$Fare))] <- round(mean(test_data$Fare,na.rm = TRUE),digits = 1)

# There are two records in the test data where the Parch value is 9 which is not available in the train data
# Hence those two records were replaced with 6
summary(train_data$Parch)
summary(test_data$Parch)
test_data$Parch[which(test_data$Parch==9)] <- 6

# Converting columns to numerical or factor based on their values
numericcols <- c('PassengerId','Age')
factorcols_train <- c('Pclass','Sex','SibSp','Parch','Embarked','Survived')
factorcols_test <- c('Pclass','Sex','SibSp','Parch','Embarked')

train_data[, numericcols] <- lapply(numericcols, function(x) as.numeric(train_data[, x]))
train_data[, factorcols_train] <- lapply(factorcols_train, function(x) as.factor(train_data[, x]))

test_data[,numericcols] <- lapply(numericcols, function(x) as.numeric(test_data[,x]))
test_data[,factorcols_test] <- lapply(factorcols_test, function(x) as.factor(test_data[,x]))

# Build the random forest
set.seed(71)
data.rf <- randomForest(Survived ~ Age+Pclass+Sex+SibSp+Parch+Embarked, data=train_data, proximity=FALSE,
                        ntree=500, do.trace=TRUE)
data.rf
testPred <- predict(data.rf, test_data)

submission <- data.frame(PassengerID=test_data$PassengerId,Survived=testPred)
head(submission)
write.csv(submission,file = "gender_submission.csv",row.names = FALSE)

View(submission)