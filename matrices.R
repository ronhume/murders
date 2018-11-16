mnist<-read_mnist()
x<-mnist$train$images
y<-x>50 & x<205
ind<-(rowMeans(y))
mean(ind)