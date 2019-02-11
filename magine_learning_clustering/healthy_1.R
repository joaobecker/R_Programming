#First turn it into Matrix & then vectors
healthyMatrix <- as.matrix(healthy)
healthyVector <- as.vector(healthyMatrix)
str(healthyMatrix)

# Analyze the matrix image --> you can only analyze the image once it becomes a matrix
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length=256)))

# We cannot use heirarchical clsutering for the ditance on this image, because of the image's high resolution
# distance <- dist(healthyVector, method = "euclidean")  --> Too large, billions of variables.
# calculate how many data points
str(healthyVector)
n <- 365636
n*(n-1)/2
# The result gives us the amount of data in the image.

# So, we will need to break it down in order to be able to use it in R.
# We will use k-clustering
# 1st Step - identify the k --> amount of clusters 
# Because k is an iterative method that could take really long to converge we set a max number of iteration w/ iter.max
k <- 5
set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
# The most important piece we have to analyze in str function is the cluster vector

# We extracting the cluster information and putting into a new variable
healthyCluster <- KMC$cluster
# Usually in order to get the mean of the clusters we apply (tapply), but we can see the mean in centers in the str function
# get the mean value of the cluster 2
KMC$centers[2]
# In the str function, size shows how big each cluster is (in data points)
# The cluster 3 is the one with the largest amount of data and it is very close to 0, which means that is very dark

# Now we turn the new data into a dimension in order to visualize it. 
# we combine the amount of row and amount of rows that we want, so go back to the first Matrix.
# instead of checking the amount of rows and cols in the heatlhyMatrix, we can just call using nrow & ncol function.
dim(healthyCluster) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
# Now we visualize our dimension. Now, we will use a color scheme (rainbow). k is equal to 5, as defined earlier
image(healthyCluster, axes=FALSE, col = rainbow(k))

