train_csv<-read.csv('D:/Machine Learning/blog data/blogData_train.csv', header = FALSE)
data<-as.data.frame(train_csv[1:5000,])

#performing k fold cross validation
k<- 10
data$id<- sample(1:k, nrow(train), replace = TRUE)
list<- 1:k

for(i in 1:k) {
  train<- subset(data, id %in% list[-i])
  test<- subset(data, id %in% list[i])
}


#basic features
#creating data using relevant columns to be used in creating the models
bf_x<-train[,c(51,52,53,54,55,56,57,58,59,60)]
bf_y<- train[,c(281)]


#textual features
tf_x<-train[,c(63:262)]
tf_y<- train[, c(281)]

#plot a graph to see data distribution
par(mfrow= c(4, 3))
plot(bf_x$V51, bf_y)
plot(bf_x$V52, bf_y)
plot(bf_x$V53, bf_y)
plot(bf_x$V54, bf_y)
plot(bf_x$V55, bf_y)
plot(bf_x$V56, bf_y)
plot(bf_x$V57, bf_y)
plot(bf_x$V58, bf_y)
plot(bf_x$V59, bf_y)
plot(bf_x$V60, bf_y)

#transform the data to make it more relevant
bf_x$V51<- sqrt(bf_x$V51)
bf_x$V52<- sqrt(bf_x$V52)
bf_x$V53<- sqrt(bf_x$V53)
bf_x$V54<- sqrt(bf_x$V54)
bf_x$V56<- sqrt(bf_x$V56)
bf_x$V57<- sqrt(bf_x$V57)
bf_x$V58<- sqrt(bf_x$V58)
bf_x$V59<- sqrt(bf_x$V59)



#basic features for test data
#creating data using relevant columns to be used in creating the models
bf_x_test<-test[,c(51,52,53,54,55,56,57,58,59,60)]
bf_y_test<- test[,c(281)]

#dividing up the dataset into two halfs to create the objective function
bf_x_1<- bf_x[1:2500, ]
bf_x_2<- bf_x[2501:5000, ]

a<- colSums(bf_x_1)
b<- colSums((bf_x_2))

