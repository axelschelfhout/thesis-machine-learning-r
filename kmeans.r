require(data.table)
require(bit64)

# Load Data
data = fread('Projects/ml-r/fakenames50k.csv')
#data = fread('Projects/ml-r/fakeprofiledataset.csv')

df = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender, data$State))
colnames <- c('Age','Centimeters','Pounds','Kilograms','Gender','Provincie')
colnames(df) <- colnames

kmdf <- copy(df)
kmdf$Provincie <- NULL
#kmdf$Gender <- NULL

# Calc clusters
clusterCalcSet <- copy(kmdf)
clusterCalcSet[,"train"] <- ifelse( runif(nrow(clusterCalcSet)) < 0.7, 1, 0)
# Split the data up for train/test data
trainColNum <- grep("train",names(clusterCalcSet)) 
clustSet <- clusterCalcSet[clusterCalcSet$train==1,-trainColNum]
clustSet <- data.matrix(clusterCalcSet.train)

ctrain = clustSet[,-1]

optimal_cluster_number <- (nrow(ctrain)-1)*sum(apply(ctrain,2,var))
for(i in 2:20) optimal_cluster_number[i] <- sum(kmeans(ctrain, centers=i)$withinss)
plot(1:20, optimal_cluster_number/10000, type='b', xlab="Number of clusters", ylab="Within groups sum of squares (/10000)")



# Numeric everything
kmdf <- data.matrix(kmdf)

# KMeans
kc <- kmeans(kmdf, 6)
print(kc$center)
#
table(df$Gender, kc$cluster)
