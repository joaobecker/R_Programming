flowerMatrix <- as.matrix(flower)

flowerVector <- as.verctor(flowerMatrix)


# Distance matrix that calculates of the distance values between the colors (shades of grey)
distance <- dist(flowerVector, method = "euclidean")

# ward method is a minimun variance method that tries to find compact and spherical clusters
# Basicially, what it tries to do is minimize the variance within each cluster and the distance among cluster
clusterIntensity <- hclust(distance, method = "ward.D")

#Graph
plot(clusterIntensity)
# Graph separated into 3 clusters
rect.hclust(clusterIntensity, k = 3, border ="red")

# This will assing the values to 1,2,3 which correspond to each cluster created
flowerClusters <- cutree(clusterIntensity, k=3)

# This will give the mean of each cluster - we get the value of each variable with the flowerVector and correspondent cluster with flowerClusters
tapply(flowerVector, flowerClusters, mean)

# Give the image with the new clusters
image(flowerClusters, axes=FALSE)

# Black & White of the original image
image(flowerMatrix, axes=FALSE, col = grey(seq(0,1,length = 256)))