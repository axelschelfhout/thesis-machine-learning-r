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
clustSet <- data.matrix(clustSet)

ctrain = clustSet[,-1]
unscaled <- ctrain
ctrain <- scale(ctrain)

optimal_cluster_number <- (nrow(ctrain)-1)*sum(apply(ctrain,2,var))
for(i in 2:20) optimal_cluster_number[i] <- sum(kmeans(ctrain, centers=i)$withinss)
plot(1:20, optimal_cluster_number, type='b', xlab="Number of clusters", ylab="Within groups sum of squares")

# Numeric everything
kmdf <- data.matrix(kmdf)

scale(kdmf)

# KMeans
kc <- kmeans(kmdf, 10)
print(kc$center)
#
table(df$Gender, kc$cluster)
