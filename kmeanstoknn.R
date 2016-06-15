require(data.table)
require(bit64)

ptm <- proc.time()

# Data preperation #

# Load dataset
dataset = fread("Projects/ml-r/fakenames50k.csv")

# Create dataframe
df <- data.frame(list(dataset$Age,
                      dataset$Centimeters,
                      dataset$Kilograms,
                      dataset$FeetInches
                      #dataset$BloodType,
                      #dataset$Gender,
                      #dataset$TropicalZodiac
                      ))

# Define the colnames
dfColnames <- list("Age",
                   "Centimeters",
                   "Kilograms",
                   "FeetInches"
                   #"Bloodtype",
                   #"Gender",
                   #"Zodiac"
                   )

# Set the colnames to the dataframe
colnames(df) <- dfColnames

# Store a copy just to be sure
dfCopy <- copy(df)

# Split the set 20/80 to use for kmeans into kNN
df[,"kmeans"] <- ifelse( runif(nrow(df)) < 0.2, 1, 0)

# Extract the kMeans collumns
kmeansColNum <- grep("kmeans",names(df))
kmeansDf <- df[df$kmeans==1,-kmeansColNum]

# Use the rest for the kNN
knnDf <- df[df$kmeans==0,-kmeansColNum]

# Numeric the data
kmeansDf <- data.matrix(kmeansDf)
knnDf <- data.matrix(knnDf)

# kMeans #

kmeansDfScaled <- scale(kmeansDf)

# Determine optimal number of clusters
#optimal_cluster_number <- (nrow(kmeansDfScaled)-1)*sum(apply(kmeansDfScaled,2,var))
#for(i in 2:50) optimal_cluster_number[i] <- sum(kmeans(kmeansDfScaled, centers=i)$withinss)
#plot(1:50, optimal_cluster_number, type='b', xlab="Number of clusters", ylab="Within groups sum of squares")

kmeans_start <- proc.time()
# Now use the algorithm
kmeans_clustering <- kmeans(kmeansDfScaled, centers = 60)

# Save the cluster labels to a new dataframe
kmeans_clustered_data <- data.frame(kmeansDf, kmeans_clustering$cluster)

kmeans_end <- proc.time()

print("kMeans running time:")
print(kmeans_end - kmeans_start)

# kNN #
# The package class has the knn algorithm
require(class)

# Create knnTest data frame
knnTestDf <- kmeans_clustered_data

# Add extra column (train) and set it to 1 or 0 (randomly) dividing the dataset
knnTestDf[,"train"] <- ifelse(runif(nrow(knnTestDf)) < 0.7, 1, 0) 

# Get the number of the collumn train -> we use it to remove the column our training en test set.
trainColNum <- grep("train",names(knnTestDf))
knnset.train <- knnTestDf[knnTestDf$train==1,-trainColNum]
knnset.test <- knnTestDf[knnTestDf$train==0,-trainColNum]

# Create a train set from the clustered set
knnset.train.labels <- knnset.train[, "kmeans_clustering.cluster"]
knnset.train <- knnset.train[,-(ncol(knnset.train))]
# Create a test set from the clustered set
knnset.test.labels <- knnset.test[, "kmeans_clustering.cluster"]
knnset.test <- knnset.test[,-(ncol(knnset.test))]

# Use the kNN model.
knn_predict <- knn(train = knnset.train, test = knnset.test, cl = knnset.train.labels, k=5)

# Print the accuracy.
print("The accuracy of this kNN model is believed to be: ")
print(mean(knn_predict==knnset.test.labels))

knn_time_start <- proc.time()
# Predict the unlabeled data with the trained dataset and labels.
predict_new <- knn(train = knnset.train, test = knnDf, cl = knnset.train.labels, k=5)

# Add the predicted values to the dataset within a new dataframe
knn_predicted_data <- data.frame(knnDf, predict_new)
knn_time_end <- proc.time()

print("kNN running time:")
print(knn_time_end - knn_time_start)

# The elapsed time of this program.
print(proc.time() - ptm)
