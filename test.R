require(data.table)
require(bit64)
# Load Data
#data = read.csv('Projects/ml-r/fakenames50k.csv')
data = fread('Projects/ml-r/fakenames50k.csv')

ages = as.numeric(data$Age)
kilos = as.numeric(data$Kilograms)
lengths = as.numeric(data$Centimeters)
pounds = as.numeric(data$Pounds)
states = data$StateFull
bloodtype = data$BloodType

# Map Genders Character values to Numeric
genders_as_string = data$Gender
data[data$Gender == 'female',] <- 1
data[data$Gender == 'male',] <- 0
genders_as_int = as.numeric(data$Gender)

genders = genders_as_string # String used in classification, int used in clustering

# 
#fullSet = data.frame(list(ages,kilos,pounds,lengths,genders,states,bloodtype))
#colnames(fullSet) <- c('Age','Kilo','Pounds','Length','Gender','State','Bloodtype')
fullSet = data.frame(list(ages,kilos,pounds,lengths,genders))
colnames(fullSet) <- c('Age','Kilo','Pounds','Length','Gender')

#
threshold = round(nrow(fullSet)*0.7)
train = fullSet[1:threshold, ]
evaluate = fullSet[(threshold+1):nrow(fullSet), ]

#
#k <- kmeans(fullSet, 5)
#plot(k$centers)
#

# Test the ggvis library
#library(ggvis)
#forggvis = fullSet[1:round(nrow(fullSet)*0.05),]
#forggvis %>% ggvis(~Kilo, ~Length, fill = ~Gender) %>% layer_points()



# KNN Classification
library(class)
set.seed(1234)

knnSet <- copy(fullSet)
threshold = round(nrow(knnSet)*0.7)
knnSet.training <- knnSet[1:threshold, 1:4]
knnSet.test <- knnSet[(threshold+1):nrow(knnSet), 1:4]

knnSet.trainLabels <- knnSet[1:threshold, 5]
knnSet.testLabels <- knnSet[(threshold+1):nrow(knnSet), 5]

prediction <- knn(train = knnSet.training, test = knnSet.test, cl = knnSet.trainLabels, k=2)

library(gmodels)
CrossTable(x = knnSet.testLabels, y = prediction, prop.chisq = FALSE)

#
# https://www.datacamp.com/community/tutorials/machine-learning-in-r
#

# -- # # -- # Logistic Regression Classification # -- # # -- # 
lrSet = copy(fullSet)

# Multinomial logistic regression
library(VGAM)
mlrFit <- vglm(Gender~., family=multinomial, data=lrSet)
summary(mlrFit)

mlrProbabilities <- predict(mlrFit, lrSet[,1:4], type="response")
mlrPredictions <- apply(mlrProbabilities, 1, which.max)
mlrPredictions[which(mlrPredictions=="1")] <- levels(lrSet$Gender)[1]
mlrPredictions[which(mlrPredictions=="2")] <- levels(lrSet$Gender)[2]
table(mlrPredictions, lrSet$Gender)

#Linear Discriminant Analysis
#LDA is a classification method that finds a linear combination of data attributes that best separate the data into classes.
library(MASS)
ldaFit <- lda(Gender~., data=lrSet)
#summary(ldaFit)
ldaPredictions <- predict(ldaFit, lrSet[,1:4])$class
table(ldaPredictions, lrSet$Gender)
# # # # #


# Naive Bayes Classification #
nbSet <- copy(fullSet)

#http://www.inside-r.org/packages/cran/e1071/docs/naivebayes
library(e1071)
plot(as.factor(nbSet[1:100,4])) # See the times a length occurs in the first 100 rows.

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

nb_multiple_times_dataset <- copy(fullSet)
predict_model_accuracy = run_nb_mulitple_times(nb_multiple_times_dataset, 0.7, 20)


#https://eight2late.wordpress.com/2015/11/06/a-gentle-introduction-to-naive-bayes-classification-using-r/





# -- # # -- # K-Means Clustering # -- # # -- #

# Determine the amount of clusters
ctrain = train[,-1]
wss <- (nrow(ctrain)-1)*sum(apply(ctrain,2,var))
for(i in 2:15) wss[i] <- sum(kmeans(ctrain, centers=i)$withinss)
plot(1:15, wss, type='b', xlab="Number of clusters", ylab="Within groups sum of squares")

#
# FIT IT
fit <- kmeans(ctrain, 5)
aggregate(ctrain, by=list(fit$cluster), FUN=mean)
ctrain <- data.frame(ctrain, fit$cluster, train$Gender)
View(ctrain)



#### **** #### #### **** #### #### **** #### #### **** #### #### **** ####

# Hierarchical Cluster Analysis
mtrxTrain <- as.matrix(train)
str(mtrxTrain)
d <- dist(mtrxTrain[1:20,1])
plot(hclust(d))
# Hierarchial Clustering sux. -> Slow and insufficient results.

#### **** #### #### **** #### #### **** #### #### **** #### #### **** ####

# Model Based Clustering
require(mclust)
mclust_data <- train[1:50,]
mclust_fit <- Mclust(mclust_data)
plot(mclust_fit) # plot results 
summary(mclust_fit) # display the best model
#


#### **** #### #### **** #### #### **** #### #### **** #### #### **** #### 





getGenderFullSet = data.frame(data$Age, data$Kilograms, data$Centimeters, data$Gender)
rownames = c('Leeftijd', 'Gewicht', 'Lengte', 'Geslacht')
colnames(getGenderFullSet) <- rownames


# 70% of the set we are using for training data
train = getGenderFullSet[1:(nrow(getGenderFullSet) * 0.7), 1:length(rownames)]
# The remaining 30% we are using for evaluation.
evaluate = getGenderFullSet[((nrow(getGenderFullSet) * 0.7) + 1):nrow(getGenderFullSet), 1:length(rownames)]





