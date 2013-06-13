# load required library
library(class)
library(ggplot2)

data <- iris# make data set
labels <- data$Species# store labels to test
data$Species <- NULL# remove labels

# global variables / setup
set.seed(1)
train.pct <- 0.7
size <- nrow(data)
#K <- 8 #k value to use (commented out for loop to try best K)
tests <- 10 #number of samples to test
errors <- NULL #create null vector for errors
bestk <- NULL #create data store for K, N

max.k <- 100
for (K in 1:max.k)              # perform fit for various values of k
{
## loop begins to run knn on several samples
for (n in 1:tests) {
	train.index <- sample(1:size, train.pct * size)       # random sample of records (training set)
	train.data <- data[train.index, ]       # perform train/test split
	test.data <- data[-train.index, ]       # note use of neg index...different than Python!

	train.labels <- as.factor(as.matrix(labels)[train.index, ])     # extract training set labels
	test.labels <- as.factor(as.matrix(labels)[-train.index, ])     # extract test set labels

## now we run just one KNN with K 8.
	knn.fit <- knn(train = train.data, test = test.data, cl = train.labels, k = K)
	this.err <- sum(test.labels != knn.fit) / length(test.labels)    # store gzn err
	errors <- c(errors,this.err)

}
# output the results
cat ('Running ',tests,' tests with K=', K,' gives an average standardisation error of ', round(mean(errors)*100,2), '%\n')
bestk <- rbind(bestk,c(round(mean(errors)*100,2),K))
}
print(bestk)
# I wanted to find which was the lowest to reccomend the best K value but could not work out how to do that!
