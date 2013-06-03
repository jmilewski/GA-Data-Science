# Bayes Homework Code

library('tm')
library('ggplot2')
library('e1071')
library('class')

german.data <- read.table('/Users/jeffmilewski/data_science_class_examples-master/homework/homework_1.txt')
model <- naiveBayes(V25 ~ ., data = german.data)
pred <- predict(model, german.data[1:1000,])table(pred, german.data$V25)

