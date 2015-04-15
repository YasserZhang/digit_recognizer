library(neuralnet)
train <- read.csv('train.csv')

#1make sub set train and test data
sub_train <- train[sample(2:nrow(train),100),]
sub_test <- train[sample(2:nrow(train), 1000),]

sub_train2 <- train[sample(2:nrow(train),500),]
sub_test2 <- train[sample(2:nrow(train), 1000),]

#2 expand the label vector into a matrix of ten columns,
# each of which indicates a digit. For example, the third
# column represent 2, and the value will be 1 if the original
# label is 1, otherwise 0.
# and merge the label matrix and the original train and test data

# a function expanding the vector into a matrix
# expand label column to a ten-column matrix from 0 to 9
# input the whole data
# output the label_frame
expand <- function(data){
  m = dim(data)[1]
  label = data$label
  label_matrix = matrix(0:9,m,10,byrow=T)
  for (i in 1:m){
    label_matrix[i,] = as.integer(label_matrix[i,] == label[i])
  }
  label_name = c()
  for (i in 1:10) {
    label_name[i] = paste("label", i - 1, sep = "")
  }
  label_frame = as.data.frame(label_matrix)
  names(label_frame) = label_name
  return(label_frame)
}
# merge the label matrix and the original data, excluding the original label
merge <- function(label_frame,data){
  n = dim(data)[2]
  newdata = cbind(label_frame,data[,2:n])
}
# getting the label frames for sub train and sub test
train_label_frame = expand(sub_train)
test_label_frame = expand(sub_test)
# merge to get new train and test data
new_train = merge(train_label_frame,sub_train)
new_test = merge(test_label_frame, sub_test)
# getting features
y = paste(names(train_label_frame), collapse = " + ")
x = paste(names(train[,2:dim(train)[2]]), collapse = " + ")
features = paste(y, " ~ ", x, sep = "")

#3 build a vector of neural network functions. Each function will take in
# all pixel features as input and one of label features as output.
# Putting ten outputs of each example together, predicting its label by picking out 
# the maximum value of them, whose value, if calculated correctly, is supposed 
# to be in the range of [0,1]

# attention: remember setting the parameter "linear.output" as FALSE, otherwise
# the outputs would not fall in between 0 and 1.

# train a model with one hidden layer of 30 neurons
train_model_30 <- neuralnet(features, data = new_train,hidden = 30,linear.output = FALSE)

model_result_30 <- compute(train_model_30,new_test[11:794])

a = head(model_result_30$net.result)
a = round(a)
a

a - head(sub_test$label)
sum(a* head(new_test[,1:10]))
sum(round(model_result_30$net.result) * new_test[,1:10])/1000


### second try, with more examples
# getting the label frames for sub train and sub test
train_label_frame2 = expand(sub_train2)
test_label_frame2 = expand(sub_test2)
# merge to get new train and test data
new_train2 = merge(train_label_frame2,sub_train2)
new_test2 = merge(test_label_frame2, sub_test2)
train_model2_30 <- neuralnet(features, data = new_train2,hidden = 30,linear.output = FALSE)

model_result2_30 <- compute(train_model2_30,new_test2[11:794])
sum(round(model_result2_30$net.result) * new_test2[,1:10])/1000


### third try, with 10,000 training examples
sub_train3 = train[1:10000,]
sub_test3 = train[sample(10000:nrow(train), 1000),]

# getting the label frames for sub train and sub test
train_label_frame3 = expand(sub_train3)
test_label_frame3 = expand(sub_test3)
# merge to get new train and test data
new_train3 = merge(train_label_frame3,sub_train3)
new_test3 = merge(test_label_frame3, sub_test3)

train_model3_30 <- neuralnet(features, data = new_train3,hidden = 30,linear.output = FALSE)
model_result3_30 <- compute(train_model3_30,new_test3[11:794])
sum(round(model_result3_30$net.result) * new_test3[,1:10])/1000

### using home desktop computer to run the third model, and it crunched
# for a whole day and still not yet finished it. I finally stopped the process.
# Although the computer has four-core cpu, the Rstudio only assign the process
# to one of them, which is the main reason why it did not complete the task
# even after a whole day.