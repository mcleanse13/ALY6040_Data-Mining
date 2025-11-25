library(car)

# Import data
Eurojobs <- read.csv(file = "Eurojobs.csv",sep = ",", dec = ".", header = TRUE)

head(Eurojobs) # head() is used to display only the first 6 observations

# try again with row names as countries
Eurojobs <- read.csv(
  file = "Eurojobs.csv",
  sep = ",", dec = ".", header = TRUE, row.names = 1
)

head(Eurojobs)


dim(Eurojobs) # displays the number of rows and columns
summary(Eurojobs)
str(Eurojobs)

# visualize the data
scatterplotMatrix(Eurojobs, reg.line = lm, smooth = FALSE, spread = FALSE,
                  span = 0.5, ellipse = FALSE, levels = c(.5, .9), id.n = 0,
                  diagonal = 'histogram')

#Agriculture (Agr)
#Mining (Min)
#Manufacturing (Man)
#Power supply industries (Pow)
#Construction (Con)
#Service industries (Ser)
#Finance (Fin)
#Social and personal services (Soc)
#Transport and communications (Tra)

# test out k-means with 2 clusters
model <- kmeans(Eurojobs, centers = 2)
# displays the class determined by the model for all observations:
print(model$cluster)


Eurojobs_cluster <- data.frame(Eurojobs,
                               cluster = as.factor(model$cluster))
head(Eurojobs_cluster)


# BSS and TSS are extracted from the model and stored
(BSS <- model$betweenss)

(TSS <- model$totss)

# We calculate the quality of the partition
BSS / TSS * 100


#nstarts means multiple iterations
model2 <- kmeans(Eurojobs, centers = 2, nstart = 10)
100 * model2$betweenss / model2$totss
model2$centers

#try it with three clusters
model3 <- kmeans(Eurojobs, centers = 3, nstart = 10)
BSS3 <- model3$betweenss
TSS3 <- model3$totss
BSS3 / TSS3 * 100

print(model3)

#is more clusters always better???

# load required packages
require(factoextra)
require(NbClust)

# Elbow method
fviz_nbclust(Eurojobs, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualization
  labs(subtitle = "Elbow method") # add subtitles

sean <- c(1,2,3)
sean

# Silhouette method
fviz_nbclust(Eurojobs, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


# Gap statistic
set.seed(42)
fviz_nbclust(Eurojobs, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500) + # reduce it for lower computation time (but less precise results)
  labs(subtitle = "Gap statistic method")


nbclust_out <- NbClust(
  data = Eurojobs,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans"
) # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"


## *** : The Hubert index is a graphical method of determining the number of clusters.
##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
##                 significant increase of the value of the measure i.e the significant peak in Hubert
##                 index second differences plot. 


## *** : The D index is a graphical method of determining the number of clusters. 
##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
##                 second differences plot) that corresponds to a significant increase of the value of
##                 the measure. 
##  
## ******************************************************************* 
## * Among all indices:                                                
## * 5 proposed 2 as the best number of clusters 
## * 16 proposed 3 as the best number of clusters 
## * 2 proposed 5 as the best number of clusters 
## 
##                    ***** Conclusion *****                            
##  
## * According to the majority rule, the best number of clusters is  3 
##  
##  
## *******************************************************************

# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 5)
# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()


#visualize the silhouette method
require(cluster)
set.seed(42)
km_res <- kmeans(Eurojobs, centers = 2, nstart = 20)
sil <- silhouette(km_res$cluster, dist(Eurojobs))
fviz_silhouette(sil)
##   cluster size ave.sil.width
## 1       1    5          0.33
## 2       2   21          0.54


#visualize clustering in 2d
require(factoextra)
fviz_cluster(km_res, Eurojobs, ellipse.type = "norm")



###
# HIERARCHICAL CLUSTERING
###

#generate some basic data
X <- matrix(c(2.03, 0.06, -0.64, -0.10, -0.42, -0.53, -0.36, 0.07, 1.14, 0.37),
            nrow = 5, byrow = TRUE
)


# Hierarchical clustering: single linkage
hclust <- hclust(dist(X), method = "single")


round(hclust$height, 3)

#show raw plot
plot(hclust)


#plot with suggested clusters
rect.hclust(hclust,
            k = 2, # k is used to specify the number of clusters
            border = "blue"
)


#look for biggest jump in height
barplot(hclust$height,
        names.arg = (nrow(X) - 1):1 # show the number of cluster below each bars
)



# Hierarchical clustering: complete linkage
hclust <- hclust(dist(X), method = "complete")


round(hclust$height, 3)

plot(hclust)


plot(hclust)
rect.hclust(hclust,
            k = 2, # k is used to specify the number of clusters
            border = "blue"
)


# Hierarchical clustering: average linkage
hclust <- hclust(dist(X), method = "average")


round(hclust$height, 3)

plot(hclust)


plot(hclust)
rect.hclust(hclust,
            k = 2, # k is used to specify the number of clusters
            border = "blue"
)



