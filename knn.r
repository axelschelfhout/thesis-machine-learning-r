require(data.table)
require(bit64)
require(class)
require(gmodels)

# Load Data
data = fread('Projects/ml-r/fakenames50k.csv')

df = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender))
colnames <- c('Age','Centimeters','Pounds','Kilograms','Gender')
colnames(df) <- colnames

# Add extra column (train) and set it to 1 or 0 (randomly) dividing the dataset by 70/30%
df[,"train"] <- ifelse( runif(nrow(df)) < 0.7, 1, 0)

# Split the data up for train/test data
trainColNum <- grep("train",names(df)) 
df.train <- df[df$train==1,-trainColNum]
df.test <- df[df$train==0,-trainColNum]

# All values to int
df.train <- data.matrix(df.train)
df.test <- data.matrix(df.test)

# Labels
df.train_labels <- df.train[,"Gender"]
df.test_labels <- df.test[,"Gender"]


knn_predict <- knn(train = df.train[,1:(ncol(df.train)-1)], test = df.test[,1:(ncol(df.test)-1)], cl = df.train_labels, k=5)

CrossTable(x = df.test_labels, y = knn_predict, prop.chisq = FALSE)

# Display the predicted / true in a table
table(pred=knn_predict, true=df.test_labels)

# See the efficiency of the prediction model.
mean(knn_predict==df.test_labels)


# Function to run the KNN model multiple times (on the fakeprofileset) to test the accuracy of the model.
run_knn_mulitple_times <- function(dataset, train_set_size, iterations, k) {
  fraction_correct <- rep(NA, iterations)
  
  for (i in 1:iterations) {
    
    # Add extra column (train) and set it to 1 or 0 (randomly) dividing the dataset by the 'train_set_size'
    dataset[,"train"] <- ifelse(runif(nrow(dataset))<train_set_size,1,0) 
    
    # Get the number of the collumn train -> we use it to remove the column our training en test set.
    trainColNum <- grep("train",names(dataset)) 
    dataset.train <- dataset[dataset$train==1,-trainColNum]
    dataset.test <- dataset[dataset$train==0,-trainColNum]
    
    dataset.train <- data.matrix(dataset.train)
    dataset.test <- data.matrix(dataset.test)
    
    dataset.train_labels <- dataset.train[,ncol(dataset.train)]
    dataset.test_labels <- dataset.test[,ncol(dataset.train)]
    
    dataset.train <- dataset.train[,1:(ncol(dataset.train)-1)]
    dataset.test <- dataset.test[,1:(ncol(dataset.test)-1)]
    
    # Now lets use the kNN model and predict our values
    knn_predict <- knn(train = dataset.train, test = dataset.test, cl = dataset.train_labels, k=k)
    fraction_correct[i] <- mean(knn_predict==dataset.test_labels)
    
  }
  return(fraction_correct)
}

knn_multiple_times_dataset <- copy(df)
predict_model_accuracy = run_knn_mulitple_times(knn_multiple_times_dataset, 0.7, 10, 5)
print(predict_model_accuracy)
print(mean(predict_model_accuracy))
