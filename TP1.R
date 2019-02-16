set.seed(0)
mat <- matrix(0, nrow=6000, ncol=201)
for (i in 1:6000){
  mat[i,] = rnorm(201,0,1)
}
indices <- seq(1:6000)
frame <- as.data.frame(mat)
frame[1, 199]
y <- mat[,1]
x <- mat[, 2:201]
x

#Dessiner le nuage des points du premier vecteur.
plot(indices ,y, type = "l", col="red")

model <- lm(y~x, data=frame)
summary(model)
p_values = summary(model)$coefficients[,4]
count_non_zero = 0
for (i in 1:length(p_values)){
  if(p_values[i] < 0.05){
    count_non_zero = count_non_zero + 1
  }
}
#Dessiner le nuage des y estimés du premier vecteur.
fittedVlues <- fitted(model)
lines(indices,fittedVlues,col="green")
length(p_values)
count_non_zero


#Question 2
n = 1000
epsilon1 <- rnorm(1000,0,1)
epsilon2 <- rnorm(1000,0,1)
epsilon3 <- rnorm(1000,0,1)
X1 <- epsilon1
X2 <- 3*X1 +epsilon2
Y <- 2 + X1 + X2 + epsilon3
plot(X1,X2)
set.seed(3)
epsilonp1 <- rnorm(10,0,1)
epsilonp2 <- rnorm(10,0,1)
epsilonp3 <- rnorm(10,0,1)
Xp1 <- epsilonp1
Xp2 <- 3*Xp1 +epsilonp2
Yp <-  2 + Xp1 + Xp2 + epsilonp3
Yp

plot(X1,X2) 

Yp
#Modele 1:
model1 = lm(Yp~Xp1)
fitted(model1)

#Modele 2:
model2 = lm(Yp~Xp2)
fitted(model2)

#questionC
model3 =  lm(Yp~Xp1 + Xp2)
summary(model3)$coefficients
n = 10
seque <- seq(-5, 15, length=n)
Dy <- density(Yp)
plot(Dy)
mean(Yp)
((n-1)/n)*var(Yp)
polygon(Dy, col="red", border="blue") 

#questionD
"beta1 density"
vec = (   coef(model1)[2])
Vx <- seq(-5, 15, length=1000)
Vy<- dnorm(Vx, coef(model1)[2], 1)
plot(Vx,Vy)
quantiles <- quantile(y, probs=seq(0.025, 0.975))
plot(quantiles)
qnorm(0.975, coef(model1)[2], 1)
qnorm(0.125, coef(model1)[2], 1)
#beta2 density
Vx <- seq(-5, 15, length=1000)
Vy<- dnorm(Vx, coef(model2)[2], 1)
plot(Vx,Vy)
