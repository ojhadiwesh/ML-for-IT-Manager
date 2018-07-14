#this installs the rmosek package 
install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",repos="http://download.mosek.com/R/8", configure.vars="PKG_MOSEKHOME=C:/Program Files/Mosek/8/tools/platform/win64x86 PKG_MOSEKLIB=mosek64")

#fit the train data into a linear model 
fit1<- lm(bf_y~(V51+ V52 +V53+ V54+ V56+ V57+ V58+ V59), data = bf_x)
summary(fit1)
rss<- sum(fit1$residuals)^2
anova(fit1)
rss

#fit the linear model on the test data
predicted1<- predict(fit1, bf_x_test)
#root mean square error
sum(mean(predicted1-bf_y_test)^2)
#create a model on train data
require(Rmosek)

blogmodel1<- list()
blogmodel1$sense<- "min"


t<- (bf_y_test- predicted1)
#blogmodel1$c<- c(t(bf_y-predicted1)%*%(bf_y))
blogmodel1$c<- c(t(bf_y)%*%(as.matrix(bf_x)))

blogmodel1$A<- Matrix((as.matrix(bf_x)), sparse = TRUE)
  
blc<- rep(0, nrow(bf_x))
buc<- rep(Inf, nrow(bf_x))
blogmodel1$bc<- rbind(blc, buc)


blx<- c(min(bf_x$V51), min(bf_x$V52), min(bf_x$V53), min(bf_x$V54),min(bf_x$V55), min(bf_x$V56), min(bf_x$V57), min(bf_x$V58), min(bf_x$V59), min(bf_x$V60))
bux<- c(max(bf_x$V51), max(bf_x$V52), max(bf_x$V53), max(bf_x$V54),max(bf_x$V55), max(bf_x$V56), max(bf_x$V57), max(bf_x$V58), max(bf_x$V59), max(bf_x$V60))
blogmodel1$bx<- rbind(blx, bux)
fit2<- mosek(blogmodel1)
fit2$sol

#checking error in rmosek model 
yhat<- 1.006837e-12 *bf_x$V51 -1.467584e-13*bf_x$V53 -5.542978e-13*bf_x$V54 +1.028866e-11*bf_x$V55 + 3.185862e-11*bf_x$V56 + 5.078805e-11*bf_x$V58 + 2.510361e-12*bf_x$V59 + 9.023893e-13*bf_x$V60

mse2<- sum(mean(bf_y- yhat)^2)
mse2
