tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

# In this case, we will not run the k-means algorithm again. Instead, we will will apply the k-means clustering results 
# found in the healthy brain image on the tumor vector
# So basically, what we are doing is trating the healthy vector as a training set and the tumor vector as a testing set
# & in order to do that we need to install flexclust
install.packages("flexclust")
library("flexclust")
# this package contains KCAA which stands for K centroids central analysis
# get data from the training set (healthy data set)
#function takes a while to run
KMC.kcca <- as.kcca(KMC, healthyVector)

# Now that we created the object kcca, we can cluster the pixels and the tumor vector using the predict function
tumorClusters <- predict(KMC.kcca, newdata = tumorVector)
# Now, the tumorCluster is a vector that assigns a value of 1 to 5 to each of the intensity values 
# as predicted by the k-means

# In order to create the image we need to 
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))

