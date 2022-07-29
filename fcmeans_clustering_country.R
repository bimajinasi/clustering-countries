# CLUSTERING COUNTRY FUZZY C-MEANS 

# load libraries
library(ppclust)
library(factoextra)
library(dplyr)
library(cluster)
library(fclust)
library(psych)
library(clusterSim)

# import data
data = read.csv("E:\\1 TESIS GO\\bima\\data-cluster_country.csv", header=T)
head(data)

# remove missing value
data = na.omit(data)

# data preparation
data_clust=data[,-1]
head(data_clust)
row.names(data_clust)=data[,1]
head(data_clust)

# check correlation pearson 
#pairs.panels(data_clust,method='pearson')

# summarize type data
str(data_clust)

# summarize statistic data
summary(data_clust)

#ELBOW FCM???
#fviz_nbclust(data_clust, kmeans, method = "wss")        # within-cluster sum of square (wss)
#fviz_nbclust(data_clust, kmeans, method = "silhouette") # silhouette

# ========================
# Fuzzy C-Means clustering
# ========================
fcm_res <- fcm(data_clust, centers=2)
fcm_res

# result cluster
result_cluster_fcm = data.frame(data_clust, fcm_res$cluster)
result_cluster_fcm

#export data
write.csv(result_cluster_fcm, "E:\\1 TESIS GO\\bima\\result_fcm_cluster_country.csv")

# sorting cluster 1,2
result_cluster_fcm[order(result_cluster_fcm$fcm_res.cluster),]

# calculate total cluster 1,2
data_clust%>% count(fcm_res$cluster)

# visualize cluster result
result_cluster_fcm2=ppclust2(fcm_res,"kmeans")
fviz_cluster(result_cluster_fcm2, data=data_clust,palette="jco",repel = T, ellipse.type = "euclid", star.plot = T, ggtheme = theme_minimal())

# =================================
# PERFORMANCE CLUSTER FUZZY C-MEANS
# =================================
result_cluster_fcm2=ppclust2(fcm_res,"fclust")

# Davies-Bouldins Index
d <-dist(data_clust)
dbi_fcm <-print(index.DB(data_clust, fcm_res$cluster, d, centrotypes = 'centroids'))
cat("Davies Bouldins = ",dbi_fcm$DB)

# Silhouete Cofficient Index
FSI=SIL.F(result_cluster_fcm2$Xca,result_cluster_fcm2$U,alpha = 1)
cat("Silhouette Cofficient Index= ", FSI)

# Calinski-Harabasz
ch<- pam(result_cluster_fcm,2)
paste("Calinski Harabasz = ",index.G1(result_cluster_fcm,ch$clustering))

# Time Execution
start.time <- Sys.time()
fcm_res <- fcm(data_clust, centers=2)
end.time <- Sys.time()
time_exec <- end.time - start.time
cat("Time Execution = ", time_exec)

fcm_res$comp.time

