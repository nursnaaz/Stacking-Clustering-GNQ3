rm(list=ls(all=TRUE))
library(vegan)
setwd("~/Desktop/GNQ3 Exam/20170226_Batch25_Datasets_GNQ03")
#Read the data
mydata <- read.csv("Breast_Cancer.csv")
#Find the structure
str(mydata)
summary(mydata)
#check for any NA's
sum(is.na(mydata))

#Converting a factor to numeric
mydata$diagnosis <- as.numeric(mydata$diagnosis)

#Remove unwanted variable
mydata<- mydata[,-c(1,2)]

#Crosscheck the structure of data
str(mydata)

#Standardise the data
mydata = decostand(mydata, "range") 
summary(mydata)
#Cross check the  structure
str(mydata)

# Ward Hierarchical Clustering
d <- dist(mydata, 
          method = "euclidean") # distance matrix
d
#Plot the dedogram
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
groups

# draw dendogram with red borders around the 5 clusters------Assuming
rect.hclust(fit, k=5, border="red") 

mydata$cluster <- groups

# K-means:  Determine number of clusters
set.seed(1234)
wss <- 0
for (i in 1:15) {
        wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}

plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

#Optimal k Value is 4 from sree plot
# K-Means Cluster Analysis
set.seed(1234)
fit <- kmeans(mydata, 4) # 4 cluster solution

# get cluster means

# append cluster assignment
mydata <- data.frame(mydata, 
                     fit$cluster) 
head(mydata)




#K-means clustering
fit<-kmeans(mydata,centers=4)
fit
#With-in sum of squares in each cluster
fit$withinss
sum(fit$withinss)
#Cluster Centers
fit$centers
#To check cluster number of each row in data
fit$cluster

