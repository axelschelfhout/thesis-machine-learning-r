require(data.table)
require(bit64)
# Load Data
#data = read.csv('Projects/ml-r/fakenames50k.csv')
data = fread('Projects/ml-r/fakenames50k.csv')

#   
ages = as.numeric(data$Age)
genders = data$Gender
kilos = as.numeric(data$Kilograms)
lengths = as.numeric(data$Centimeters)
states = data$StateFull
bloodtype = data$BloodType

# 
fullSet = data.frame(list(ages,genders,kilos,lengths,states,bloodtype))
colnames(fullSet) <- c('Age','Gender','Kilo','Length','State','Bloodtype')

#
threshold = round(nrow(fullSet)*0.7)
train = fullSet[1:threshold, ]
evaluate = fullSet[(threshold+1):nrow(fullSet), ]

# K-Means Clustering
# We can't use the String. So for this test, dont use the gender collumn
ctrain = train[,-1]
wss <- (nrow(ctrain)-1)*sum(apply(ctrain,2,var))

for(i in 2:15) wss[i] <- sum(kmeans(ctrain, centers=i)$withinss)
plot(1:15, wss, type='b', xlab="Number of clusters", ylab="Within groups sum of squares")

# FIT IT
fit <- kmeans(ctrain, 4)
aggregate(ctrain, by=list(fit$cluster), FUN=mean)
ctrain <- data.frame(ctrain, fit$cluster, train$Geslacht)
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










