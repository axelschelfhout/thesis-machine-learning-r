require(data.table)
require(bit64)
require(class)
require(gmodels)
#http://www.inside-r.org/packages/cran/e1071/docs/naivebayes
require(e1071)

# Load Data
data = fread('Projects/ml-r/fakenames50k.csv')

df = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender))
colnames <- c('Age','Centimeters','Pounds','Kilograms','Gender')
colnames(df) <- colnames

# Naive Bayes Classification #
nbSet <- copy(df)

plot(as.factor(nbSet[1:100,"Centimeters"])) # See the times a length occurs in the first 100 rows.

nbTreshold = round(nrow(nbSet)*0.7)
nbSet.training <- nbSet[1:nbTreshold, ]
nbSet.test <- nbSet[(nbTreshold+1):nrow(nbSet), ]

nbModel <- naiveBayes(Gender ~ ., data=nbSet.training)
nbTestPredict <- predict(nbModel, nbSet.test[,-1])
# Shows the amount of REAL female/male and the PREDICTED (pred) female/male
table(pred=nbTestPredict, true=nbSet.test$Gender)

# See the efficiency of the prediction model.
mean(nbTestPredict==nbSet.test$Gender)

# Function to run the Naive Bayes model multiple times (on the fakeprofileset) to test the accuracy of the model.
run_nb_mulitple_times <- function(dataset, train_set_size, n) {
  fraction_correct <- rep(NA, n)
  
  for (i in 1:n) {
    
    # Add extra column (train) and set it to 1 or 0 (randomly) dividing the dataset by the 'train_set_size'
    dataset[,"train"] <- ifelse(runif(nrow(dataset))<train_set_size,1,0) 
    
    #todo # Check if the dataset isnt the same as a dataset before
    
    # Get the number of the collumn train -> we use it to remove the column our training en test set.
    trainColNum <- grep("train",names(dataset)) 
    dataset.train <- dataset[dataset$train==1,-trainColNum]
    dataset.test <- dataset[dataset$train==0,-trainColNum]
    
    # Now lets use the naiveBayes model and predict our values
    nb_model <- naiveBayes(Gender ~ ., data=dataset.train)
    nb_predict <- predict(nb_model, dataset.test[,-1])
    fraction_correct[i] <- mean(nb_predict==dataset.test$Gender)
  }
  return(fraction_correct)
}

nb_multiple_times_dataset <- copy(df)
predict_model_accuracy = run_nb_mulitple_times(nb_multiple_times_dataset, 0.7, 10)

print("Average prediction accuracy")
print(mean(predict_model_accuracy))

