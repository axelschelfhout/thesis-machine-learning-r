require(data.table)
require(bit64)

# Load Data
data = fread('Projects/ml-r/fakenames50k.csv')

df = data.frame(list(data$Age, data$Centimeters, data$Pounds, data$Kilograms, data$Gender, data$State))
colnames <- c('Age','Centimeters','Pounds','Kilograms','Gender','Provincie')
colnames(df) <- colnames


kmdf <- copy(df)
kmdf$Provincie <- NULL
#kmdf$Gender <- NULL

kmdf <- data.matrix(kmdf)

kc <- kmeans(kmdf, 12)
table(df$Provincie, kc$cluster)
