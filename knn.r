require(data.table)
require(bit64)
require(class)
require(gmodels)

# Load Data
data = fread('Projects/ml-r/fakenames50k.csv')

#data[data$Gender == 'female',] <- 1
#data[data$Gender == 'male',] <- 0

df = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender))
colnames <- c('Age','Centimeters','Pounds','Kilograms','Gender')
colnames(df) <- colnames

# Add extra column (train) and set it to 1 or 0 (randomly) dividing the dataset by 70/30%
df[,"train"] <- ifelse( runif(nrow(df)) < 0.7, 1, 0)

# Split the data up for train/test data
trainColNum <- grep("train",names(df)) 
df.train <- df[df$train==1,-trainColNum]
df.test <- df[df$train==0,-trainColNum]

#
df.train <- data.matrix(df.train) # All values to int
df.test <- data.matrix(df.test) # All values to int

# Labels
df.train_labels <- df.train[,"Gender"]
df.test_labels <- df.test[,"Gender"]


knn_predict <- knn(train = df.train, test = df.test, cl = df.train_labels, k=5)

CrossTable(x = df.test_labels, y = knn_predict, prop.chisq = FALSE)





