# Get the table into a readable text file
movies <-  read.table("movieLens.txt", header=FALSE, sep="|", quote = "\"")

# Name the columns
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

# Exclude unwanted columns
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)

# Calculate the distance between data points
distances <- dist(movies[2:20], method = "euclidean")

# hclust meaans hierarchical clustering & ward method cares about the distance between clusters using 
# the centroid distance and also the variance in each of the clusters.
clusterMovies <- hclust(distances, method = "ward.D")

#Graphing our data
plot(clusterMovies)

# Due to thousand of variables we cant read the graph and the text is all black
# However, from the graph we can see that we have a lot of clusters, let's pick the amount of cluster we can work with
# There are three main clusters, but we dont want to have only 3 categories, so let's go with 10 clusters.
clusterGroups <- cutree (clusterMovies, k = 10)

# What this do is - It shows the action mean/average value of each cluster
# Because the eaction variable is a binary variable - 0 or 1 - by computing the average, we are computing the percentage 
# of movies in that cluster that belongs to action genre
tapply(movies$Action, clusterGroups, mean) 
tapply(movies$Romance, clusterGroups, mean)

# Analyzing a single movie/ data point
# it will give us the number of the data point and from there we can see to each cluster it belongs to
subset(movies, Title == "Men in Black (1997)")
clusterGroups[257]

# let's see all the movies in cluster 2 & then the first 10 movies in the cluster
cluster2 <- subset(movies, clusterGroups == 2)
cluster2$Title[1:10]



