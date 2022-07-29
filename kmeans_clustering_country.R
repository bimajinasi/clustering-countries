# CLUSTERING COUNTRY K-MEANS 

# install packages
#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")

# load libraries
library(tidyverse)  #data manipulation
library(cluster)    #clustering algorithms
library(factoextra) #clustering algorithms & visualization

# import data
df_country = read.csv("E:\\1 TESIS GO\\bima\\data-cluster_country.csv", header=T)
head(df_country)

# remove missing value
df_country = na.omit(df_country)

# remove field country
data_clust=df_country[,-1]
head(data_clust)

# set field country to index id
row.names(data_clust)=df_country[,1]
head(data_clust)

# scaling/standarizing data
#data_sc = scale(data_clust)
#data_sc

# summarize type data
str(data_clust)

# summarize statistic data
summary(data_clust)

# elbow method to find optimal number of clusters
fviz_nbclust(data_clust, kmeans, method = "wss")        

# silhouette coefficient to find optimal number of clusters
fviz_nbclust(data_clust, kmeans, method = "silhouette") 

# ==================
# K-Means clustering
# ==================
km_res = kmeans(data_clust, centers=2)
km_res

# result cluster
result_cluster_km = data.frame(data_clust, km_res$cluster)
result_cluster_km
view(result_cluster_km)

#export data
write.csv(result_cluster_km, "E:\\1 TESIS GO\\bima\\result_km_cluster_country.csv")

# sorting cluster 1,2
result_cluster_km[order(result_cluster_km$km_res.cluster),]

# calculate total cluster 1,2
data_clust%>% count(km_res$cluster)
#view(result_cluster_km)

# visualize cluster result
fviz_cluster(km_res, data=data_clust, repel = T, ellipse.type = "euclid", star.plot = T, ggtheme = theme_minimal())

# calculate mean cluster 1,2
result_cluster_km%>% mutate(cluster=final$cluster)%>%group_by(cluster)%>%summarise_all("mean")


# ===========================
# PERFORMANCE CLUSTER K-MEANS
# ===========================
# Davies-Bouldins Index
library(clusterSim)
d <-dist(data_clust)
dbi_kmean <-print(index.DB(data_clust, km_res$cluster, d, centrotypes = 'centroids'))
cat("Davies Bouldins = ",dbi_kmean$DB)

#library(clusterSim)
#dbi <- pam(result_cluster_km, 2)
#print(index.DB(result_cluster_km, dbi$clustering, centrotypes="centroids"))

# Silhouette Coefficient
library(cluster)
library(factoextra)
jarak = as.matrix(dist(data_clust))
sc <- mean(silhouette(km_res$cluster,dmatrix=jarak)[,3])
cat("Silhouette Coefficient = ", round(sc, 3))

# Calinski Harabasz
ch <- pam(result_cluster_km,2)
paste("Calinski Harabasz = ",index.G1(result_cluster_km,ch$clustering))


# Time Execution
start.time <- Sys.time()
km_res = kmeans(data_clust, centers=2)
end.time <- Sys.time()
time_exec <- end.time - start.time
cat("Time Execution = ", time_exec)
