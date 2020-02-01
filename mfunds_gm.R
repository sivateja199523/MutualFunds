library(readr)
library(data.table)
library(nnet)
library(caret)
library(dplyr)
#install.packages('randomForest')
library(randomForest)
#install.packages('ROCR')
library(ROCR)
library(ggplot2)
library(cluster)
library(dbscan)
library(clustertend)
library(mice)
library(tidyverse)

rawData_gm <- fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_gm_mice.csv"
                    , sep=",", header=T, 
                    strip.white = T, na.strings = c("NA","NaN","","?"))
#View(rawData_bl)
mf_bl<-rawData_gm
final_data_gm <-rawData_gm
#View(mf_bl)
#To check are there any null values
na_count <-sapply(mf_bl, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
#View(na_count)
df_bl_mf <- scale(mf_bl[,c(2:37)]) # Standardize the data
#View(df_bl_mf)
#check cluster tendency - Hopkins
#hopkins(df_bl_mf, n = nrow(df_bl_mf)-1)
#Hierarchical Clustering
d_bl <- dist(df_bl_mf, method = "euclidean") # Euclidean distance matrix.
# H.single_bl <- hclust(d_bl, method="single")
# plot(H.single_bl) # display dendogram
# H.complete_bl <- hclust(d_bl, method="complete")
# plot(H.complete_bl)
# H.average_bl <- hclust(d_bl, method="average")
# plot(H.average_bl)
# H.ward_bl <- hclust(d_bl, method="ward.D2")
# plot(H.ward_bl)
# par(mfrow=c(2,2))
# plot(H.single_bl)
# plot(H.complete_bl)
# plot(H.average_bl)
# plot(H.ward_bl)
# par(mfrow=c(1,1))
#After selecting the appropriate algorithm by visually inspecting the dendograms, we can rerun the code and obtain the desired clusters:
H.ward_bl <- hclust(d_bl, method="ward.D2")
groups_bl <- cutree(H.ward_bl, k=6) # cut tree into 6 clusters
#Then we can draw the dendogram with red borders around the 6 clusters:
#plot(H.ward_bl)
#rect.hclust(H.ward_bl, k=6, border="red")
final_data_gm$hclust <- groups_bl  #Adding hclusters to data.
#View(final_data_bl)
#Pca to get the mPints value
pca_bl <- prcomp(df_bl_mf, center = TRUE, scale. = TRUE) # Variables will be zero-centered and will have unit variance in the PCA
print(pca_bl)
plot(pca_bl, type = "l")
summary(pca_bl)
#DB scan
kNNdistplot(df_bl_mf, k =13) #k and minPts will be same, Considering PCA=12 which given 90% Cumulative Proportion
abline(h=7, col="red")
db_bl <- dbscan(df_bl_mf, eps=3.5, minPts=15)  #Need to perform pca to get the minpts, eps is from the above plot
db_bl
clusplot(df_bl_mf, db_bl$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
hullplot(df_bl_mf, db_bl)
final_data_gm$dbscan<- db_bl$cluster
dbscan_results_gm<-aggregate(final_data_gm[, 2:37], list(final_data_gm$dbscan), mean)
#K means clustering
#To identify the ideal clusters
withinssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(df_bl_mf, nc=20) 
k.means.fit_bl <- kmeans(df_bl_mf, 6)
clusplot(df_bl_mf, k.means.fit_bl$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
final_data_gm$kmeans <- k.means.fit_bl$cluster
k.means.fit_bl$size # Check the size of each cluster
k.means.fit_bl$centers # The locations of the centroids

#Internal Cluster Validation
plot(silhouette(k.means.fit_bl$cluster, d_bl)) # d is the distance
plot(silhouette(db_bl$cluster, d_bl)) # d is the distance
plot(silhouette(groups_bl, d_bl)) # d is the distance
write.table(final_data_gm, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/final_data_gm.csv", sep=",", row.names=F)
write.table(dbscan_results_gm, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/dbscan_results_gm.csv", sep=",", row.names=F)
library(fpc)
#Validation of clusters

# (k-means) within clusters
km_stats <- cluster.stats(d_bl,  k.means.fit_bl$cluster)
km_stats$within.cluster.ss
km_stats$average.between
km_stats$average.within
km_stats$clus.avg.silwidths

# DBscan within clusters 
db_stats <- cluster.stats(d_bl,  db_bl$cluster)
db_stats$within.cluster.ss
db_stats$average.between
db_stats$average.within
db_stats$clus.avg.silwidths

# Hierarchical within clusters
HC_stats <- cluster.stats(d_bl,  groups_bl)
HC_stats$within.cluster.ss
HC_stats$average.between
HC_stats$average.within
HC_stats$clus.avg.silwidths