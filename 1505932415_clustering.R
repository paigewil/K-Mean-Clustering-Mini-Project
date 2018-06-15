# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine2 <- scale(wine[-1])


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
	              #print(wss)
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	              #print(wss)  
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

#wssplot(df)
wssplot(wine2)

# Exercise 2:
#   * How many clusters does this method suggest?
#       -> After about 3 or 4 clusters, the change un the sum of squares doesn't
#          vary as much indicating that is the appropriate amount of clusters.
#   * Why does this method work? What's the intuition behind it?
#       -> This method works because at some point, having more clusters will not
#          significantly improve the error (sum of squares) in your clusters
#          and the slope indicates when that happens.
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

#library(NbClust)
set.seed(1234)
nc <- NbClust(wine2, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#       -> The most suggested number of clsuters by far is 3


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

#     -> Will be using k = 3

set.seed(1234) # set seed is important to reporoduce results
#fit.km <- kmeans(wine2, centers = 3, iter.max = 1000)
fit.km <- kmeans(wine2, centers = 3, nstart = 25) #using parameters the example used


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster, wine$Type)

#    1  2  3
# 1 59  3  0
# 2  0 65  0
# 3  0  3 48

# The majority of the observations for each Type were put into the same cluster 
# without much spread between the 3 clusters, indicating we have a good fit.


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clara_partition <- clara(wine2, 3)

clusplot(clara_partition)

#     -> There seems to be some overlap between two of the clusters, but in general,
#        the clusters are distinct and seem pretty good.
