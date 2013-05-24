# load required libraries for this tutorial
library(class)
library(ggplot2)

## PREPROCESSING
data <- read.table("/Users/jeffmilewski/data_science_class_examples/homework/homework_1.txt", sep="", header=FALSE) # create copy of germantable dataframe
names(data) <- c('checkingaccount','duration', 'installmentrate', 'purpose', 'savingsaccount', 'employmentsince', 'statusandsex', 'residencesince', 'liabilities', 'age', 'otherinstallmentplans', 'existingcredits', 'property', 'telephone', 'creditamount', 'job', 'housing', 'credithistory', 'otherdebtors', 'foreignworker', 'ind1', 'ind2', 'ind3', 'ind4', 'classification')
labels <- data$classification      # store labels
data$classification <- NULL        # remove labels from feature set (note: could

## TRAIN/TEST SPLIT
# initialize random seed for consistency
# this allows our data to look the same every single time the experiment is run
set.seed(1234) 

# we want to use 70% of our data as a training set
N <- nrow(data)
train.pct <- .7

train.index <- sample(1:N, train.pct * N)       # random sample of records (training set)
train.data <- data[train.index, ]       # perform train/test split
test.data <- data[-train.index, ]       # note use of neg index...different than Python!

train.labels <- as.factor(as.matrix(labels)[train.index, ])     # extract training set labels
test.labels <- as.factor(as.matrix(labels)[-train.index, ])     # extract test set labels

## Apply the Model
# initialize results object
err.rates <- data.frame()

max.k <- 100

# perform fit for various values of k
for (k in 1:max.k) {
  knn.fit <- knn(
    train = train.data,         # training set
    test = test.data,           # test set
    cl = train.labels,          # true labels
    k = k                       # number of NN to poll
  )

  # print params and confusion matrix for each value k
  cat('\n', 'k = ', k, ', train.pct = ', train.pct, '\n', sep='')
  print(table(test.labels, knn.fit))

  # store generalation error and append to total results
  this.err <- sum(test.labels != knn.fit) / length(test.labels)
  err.rates <- rbind(err.rates, this.err)
}

## OUTPUT RESULTS
results <- data.frame(1:max.k, err.rates)   # create results summary data frame
names(results) <- c('k', 'err.rate')        # label columns of results df

# create title for results plot
title <- paste('knn results for German Credit Risk(train.pct = ', train.pct, ')', sep='')

# create results plot
results.plot <- ggplot(results, aes(x=k, y=err.rate)) + geom_point() + geom_line()
results.plot <- results.plot + ggtitle(title)

# draw results plot
results.plot