### Multiple Regression HW due 9/27 ###

#Brand preference dataset

brand <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR05.txt", header=F)
names(brand) <- c("Y", "X1", "X2")

#View data
brand

#attach(brand)

#Re-format data into matrix datatypes
Y <- as.matrix(brand$Y)
Y

X <- as.matrix(cbind(1, brand$X1, brand$X2))
X

#Compute closed-form solution
b <- solve(t(X) %*% X) %*% t(X) %*% Y
b

#Compare to lm
fit <- lm(Y ~ X1 + X2, data=brand)

summary(fit)

#Grocery retailer dataset
grocery <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR09.txt", header=F)
names(grocery) <- c("Y","X1", "X2", "X3")

grocery

#Use lm first
attach(grocery)
fit_grocery <- lm(Y ~ X1 + X2 + X3, data=grocery)
summary(fit_grocery)


#Closed form solution

#re-format to matrix datatypes
Y <- as.matrix(grocery$Y)
Y

X <- as.matrix(cbind(1, grocery$X1, grocery$X2, grocery$X3))
X

beta_hat_grocery <- solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat_grocery

#again
summary(fit_grocery)
