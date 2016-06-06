require(data.table)
require(bit64)
require(randomForest)

data <- fread('Projects/ml-r/fakenames50k.csv') #<- heavy process
#data <- fread('Projects/ml-r/fakeprofiledataset.csv')

dfWithState = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender, data$StateFull))
colnamesWithState <- c('Age','Centimeters','Pounds','Kilograms','Gender', 'Provincie')

colnames(dfWithState) <- colnamesWithState

df = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender))
colnames <- c('Age','Centimeters','Pounds','Kilograms','Gender')

colnames(df) <- colnames

x <- 1:nrow(df)
# Set random seed to randomize the random forest
set.seed(sample(x)[1])

rfSet <- copy(df)
rfSetWithState <- copy(dfWithState)

rf <- randomForest(rfSet[,-5], rfSet[,5], ntree = 70, mtry = 4)
plot(rf, type="l", main=substitute(rf))
print(rf)

#rfWithState <- randomForest(rfSetWithState[,-6], rfSetWithState[,6])
