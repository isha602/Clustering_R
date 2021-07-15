
#HIERARCHICAL CLUSTERING 
#1) CALCULATE THE DISTANCE B/W EACH OBSERVATION USING DIST(), dist(df, method = c("euclidean", "binary").
#2)Cluster the distances with hclust(dist, method = c("complete", "single", "average").
#3)Evaluate the hclust tree with a dendogram, principal component analysis (PCA), and/or summary statistics.
#4)Cut" the hierarchical tree into the desired number of clusters (k) or height h with cutree(hclust, k = NULL, h = NULL). 
#5)Calculate summary statistics and draw conclusions. 
#Useful summary statistics are typically membership count, and feature averages (or proportions). 
library(NbClust)
library(ISLR)
library(factoextra)
data("USArrests")
View(USArrests)
USArrests=na.omit(USArrests)
USArrests

#normalizing the continuous variables 
set.seed(42)
crime_scale=scale(USArrests)
head(crime_scale)

#calculating the distance 
dis=dist(crime_scale, method="euclidean")


#evaluating the tree
set.seed(123)
model=hclust(dis, method = "complete")
plot(model,main="using complete linkage method")

#cut the tree 
set.seed(120)
ctree=cutree(model,k=3)
table(ctree)
summary(ctree)

#####  k means clustering 
#1Determine the appropriate number of means with an elbow plot 
#2Cluster the observations using the selected number k of clusters with kmeans(df, centers = k)
#3Calculate summary statistics and draw conclusions. 

set.seed(123)
crime=sample(1:nrow(crime_scale),10)
crime

crime1=crime_scale[crime,]

crime_dis=dist(crime1,method="euclidean")
crime_dis

wss=sapply(1:crime,
           function(k){kmeans(crime_scale,k,nstart=20,iter.max = 15)$tot.withinss})
fviz_nbclust(crime_scale,kmeans,method="wss")+geom_vline(xintercept = 4,linetype=5,col="red")

k_crime=kmeans(crime_scale,centers=4,nstart=20)
k_crime
plot(crime_scale,k_crime$cluster)
data=cbind(crime_scale,clusters=k_crime$cluster)
plot(data)

##################practise 01 

##classification iris dataset k means clustering 
library(tree)
iris=iris[-5]
iris_scale=scale(iris)

set.seed(123)
iris01=sample(1:nrow(iris_scale),20)
iris1=iris_scale[iris01,]

wss=sapply(1:iris01,
           function(k){kmeans(iris_scale,k,nstart=20,iter.max = 15)$tot.withinss})
fviz_nbclust(iris_scale,kmeans,method="wss")+geom_vline(xintercept = 4,linetype=5,col="red")
set.seed(123)
iris_k=kmeans(iris_scale,centers = 3,nstart=20)
plot(iris_scale,iris_k$cluster)
irisdata=cbind(iris_scale,clusters=iris_k$cluster)
plot(irisdata)

###hierarchical clustering 

iris_dis=dist(iris_scale,method="euclidean")

set.seed(123)
iris_hclust=hclust(iris_dis,method="complete")
plot(iris_hclust)

iris_tree=cutree(iris_hclust,k=3)
table(iris_tree)
summary(iris_tree)
