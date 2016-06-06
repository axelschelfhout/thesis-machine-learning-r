require(data.table)
require(bit64)
require(dbscan)

# Load Data
data = fread('Projects/ml-r/fakenames50k.csv')

df = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender, data$State))
colnames <- c('Age','Centimeters','Pounds','Kilograms','Gender','Provincie')
colnames(df) <- colnames

dbscandf <- copy(df)

# Numeric everything
dbscandf <- data.matrix(dbscandf)

# Start DBScan (Density Based Scan for finding clusters (with noise/outliers))
db_3pts <- dbscan(dbscandf, eps = 1, minPts = 5)
print(db_3pts)

db_2pts <- dbscan(dbscandf, eps = .25, minPts = 2)
print(db_2pts)


