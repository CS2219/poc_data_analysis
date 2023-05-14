install.packages('e1071')
install.packages('FNN')
install.packages('gmodels')
install.packages('psych')
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)

data_class <- perConvictions

#creating the output dataset
mtown_outcome <- subset(data_class, select = c(Town))

# Convert character column to factor
mtown_outcome$Town <- as.factor(mtown_outcome$Town)                            

# deleting the output column from the predictor dataset
data_class <- subset(data_class, select = -c(Town) )
                                    
str(data_class)

set.seed(1234) # set the seed to make the partition reproducible

# 75% of the sample size
smp_size <- floor(0.75 * nrow(data_class))
data_class
train_ind <- sample(seq_len(nrow(data_class)), size = smp_size)
train_ind

# creating test and training sets that contain all of the predictors
class_pred_train <- data_class[train_ind, ]
class_pred_test <- data_class[-train_ind, ]
class_pred_train

mjob_outcome_train <- mtown_outcome[train_ind, ]
mjob_outcome_test <- mtown_outcome[-train_ind, ]

mjob_outcome_train

mjob_pred_knn <- knn(train = class_pred_train, test = class_pred_test, cl = mjob_outcome_train, k=12)


# put "mjob_outcome_test" in a data frame
mjob_outcome_test <- data.frame(mjob_outcome_test)

# merge "Town_pred_knn" and "Town_outcome_test" 
class_comparison <- data.frame(mjob_pred_knn, mjob_outcome_test)

# specify column names for "class_comparison"
names(class_comparison) <- c("PredictedTown", "ObservedTown")

# inspect "class_comparison" 
head(class_comparison)
CrossTable(x = class_comparison$ObservedTown, y = class_comparison$PredictedTown, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

mjob_pred_caret <- train(class_pred_train, mjob_outcome_train, method = "knn")
mjob_pred_caret

plot(mjob_pred_caret)

predict(mjob_pred_caret, newdata = class_pred_test) 
knnPredict <- predict(mjob_pred_caret, newdata = class_pred_test) 
plot(knnPredict)

confusionMatrix(knnPredict, mjob_outcome_test$mjob_outcome_test)

confusion<-table(knnPredict, mjob_outcome_test$mjob_outcome_test)
confusion

accuracy <-function(x){sum(diag(x)/sum(rowSums(x)))*100}

accuracy(confusion)

