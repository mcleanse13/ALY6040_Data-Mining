###
# R clustering in-class activity
###

# Loading the data set
data("USArrests")
head(USArrests)
summary(USArrests)

#1.) do some basic EDA steps to understand this dataset

#Summary of dataset, str(), head(), mean, median, mode, dim, explore relationships,
#data visualizations

#2.) Scale the data using scale() to get all features on same scale

scale(USArrests, center = TRUE, scale = TRUE)
USArrests_scaled <- as.data.frame(scale(USArrests))

#3.) Try out k-means clustering with three clusters and see how well it performs (quality)

#try it with three clusters
models <- kmeans(USArrests_scaled, centers = 3, nstart = 10)
print(models)

#4.) Visualize this clustering solution in two dimensions 

#visualize the silhouette method
require(cluster)
set.seed(42)
km_res <- kmeans(USArrests_scaled, centers = 3, nstart = 20)
sil <- silhouette(km_res$cluster, dist(USArrests_scaled))
fviz_silhouette(sil)
##   cluster size ave.sil.width
## 1       1    5          0.33
## 2       2   21          0.54


#visualize clustering in 2d
require(factoextra)
fviz_cluster(km_res, USArrests_scaled, ellipse.type = "norm")


#5.) Use nbclust to pick the optimal number of clusters between 2 and 6
# Gap statistic
set.seed(42)
fviz_nbclust(USArrests_scaled, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500) + # reduce it for lower computation time (but less precise results)
  labs(subtitle = "Gap statistic method")


nbclust_out <- NbClust(
  data = USArrests_scaled,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 6, # maximum number of clusters
  method = "kmeans"
# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 6)
# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()


#BONUS: Use hierarchical clustering with average linkage to determine the optimal number of clusters 


