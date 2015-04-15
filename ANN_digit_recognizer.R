a = matrix(1:12,3,4)
b = matrix(13:24,4,3)
a %*% b
t(a)
a .* a
a * a
c = c(1:3)
t(c)
c
d = t(c)
e = t(t(c))
a %o% a
c %o% c

train <- read.csv('train.csv')

sub_train <- train[sample(2:nrow(train),100),]
sub_test <- train[sample(2:nrow(train), 1000),]

library(neuralnet)
# preparing the features
names = colnames(train)[-match("label",colnames(train))]
names = paste(names, collapse = " + ")
features = paste("label ~ ", names)

sub_train_model <- neuralnet(features, data = sub_train)
plot(sub_train_model)

model_results <- compute(sub_train_model,sub_test[2:785])
cor(model_results$net.result,sub_test$label)

-----------------------------------------------------------

sub_train$label
label_name = c()
for (i in 1:10) {
    label_name[i] = paste("label", i - 1, sep = "")
}
c = 0:9
dim(sub_train)

label_frame = matrix(0:9,nrow(sub_train),10, byrow = T)
label = sub_train$label
for (i in 1:nrow(sub_train)){
    label_frame[i,] = as.integer(label_frame[i,] == sub_train$label[i])
}
label_frame = as.data.frame(label_frame)
names(label_frame) = label_name
sub_train_new = cbind(label_frame,sub_train)

#preparing new features
names = colnames(train)[-match("label",colnames(train))]
names = paste(names, collapse = " + ")
label_names <- paste(label_name,collapse = " + ")

features_new = paste(label_names," ~ ", names,sep = "")

sub_train_model_new <- neuralnet(features_new, data = sub_train_new)

---------------------------------------------
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

label_frame_test = expand(sub_test)
sub_test_new = cbind(label_frame_test,sub_test)
model_result_new <- compute(sub_train_model_new,sub_test_new[12:795])
head(model_result_new$net.result)
------------------------------------------------------
    > head(sub_test$label)
[1] 2 1 1 6 7 5
-------------------------------------------------------
# bias is very high, I need more neurons
sub_train_model_5by5 <- neuralnet(features_new, data = sub_train_new,hidden = c(5,5))
model_result_5by5 <- compute(sub_train_model_5by5,sub_test_new[12:795])
head(model_result_5by5$net.result)
# 5by5 seems a little better, try 30 neurons, one layer
sub_train_model_30 <- neuralnet(features_new, data = sub_train_new,hidden = c(30))
model_result_30 <- compute(sub_train_model_30,sub_test_new[12:795])
head(model_result_30$net.result)
plot(sub_train_model_5by5)
# 30by30

sub_train_model_30by30 <- neuralnet(features_new, data = sub_train_new, hidden = c(30,30), rep = 10)
model_result_30by30 <- compute(sub_train_model_30by30,sub_test_new[12:795])
head(model_result_30by30$net.result)
plot(sub_train_model_30by30)
